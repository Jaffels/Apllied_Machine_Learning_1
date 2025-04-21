###############################################################################
# Drug Consumption Data Linear Models
###############################################################################

# Load required libraries
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)

# Read the cleaned dataset
drug_data <- read.csv("Data/cleaned.csv", stringsAsFactors = FALSE)

###############################################################################
# Data preparation
###############################################################################

# Define drug columns
drug_columns <- c(
  "Alcohol", "Amphet", "Amyl", "Benzos", "Caff", "Cannabis", 
  "Choc", "Coke", "Crack", "Ecstasy", "Heroin", "Ketamine", 
  "Legalh", "LSD", "Meth", "Mushrooms", "Nicotine", "Semer", "VSA"
)

# Define personality traits columns
trait_col <- c("Nscore", "Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS")

# Convert drug consumption levels to numeric values for modeling
consumption_levels <- c(
  "Never Used" = 0,
  "Used over a Decade Ago" = 1,
  "Used in Last Decade" = 2, 
  "Used in Last Year" = 3,
  "Used in Last Month" = 4,
  "Used in Last Week" = 5,
  "Used in Last Day" = 6
)

# Create a new dataframe with numeric values for drug consumption
drug_data_numeric <- drug_data
for (col in drug_columns) {
  drug_data_numeric[[col]] <- sapply(drug_data[[col]], function(x) consumption_levels[x])
}

# Convert categorical variables to factors
drug_data_numeric$Age <- factor(drug_data_numeric$Age, 
                                levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
drug_data_numeric$Gender <- factor(drug_data_numeric$Gender)
drug_data_numeric$Education <- factor(drug_data_numeric$Education)
drug_data_numeric$Country <- factor(drug_data_numeric$Country)
drug_data_numeric$Ethnicity <- factor(drug_data_numeric$Ethnicity)


###############################################################################
# Linear Models for Cannabis Consumption
###############################################################################

# Create a linear model for Cannabis consumption based on personality traits
cannabis_trait_model <- lm(Cannabis ~ Nscore + Escore + Oscore + Ascore + Cscore + Impulsive + SS, 
                           data = drug_data_numeric)

# Display model summary
print("Linear model for Cannabis based on personality traits:")
summary(cannabis_trait_model)

# Create a more comprehensive model including demographics
cannabis_full_model <- lm(Cannabis ~ Nscore + Escore + Oscore + Ascore + Cscore + Impulsive + SS +
                            Age + Gender + Education + Country, 
                          data = drug_data_numeric)

# Display full model summary
print("Linear model for Cannabis including demographics:")
summary(cannabis_full_model)

# Compare models with ANOVA
print("Comparing models:")
anova(cannabis_trait_model, cannabis_full_model)

###############################################################################
# Linear Models for Multiple Drugs
###############################################################################

# Function to build and summarize a linear model for each drug
build_drug_models <- function(drug_col) {
  # Formula with personality traits only
  trait_formula <- as.formula(paste(drug_col, "~ Nscore + Escore + Oscore + Ascore + Cscore + Impulsive + SS"))
  
  # Build model
  model <- lm(trait_formula, data = drug_data_numeric)
  
  # Extract R-squared and significant predictors
  r_squared <- summary(model)$r.squared
  adj_r_squared <- summary(model)$adj.r.squared
  
  # Get coefficients and p-values
  coefs <- summary(model)$coefficients
  sig_predictors <- rownames(coefs)[coefs[,4] < 0.05 & rownames(coefs) != "(Intercept)"]
  
  # Format significant predictors with their coefficients
  if(length(sig_predictors) > 0) {
    sig_pred_text <- paste(sig_predictors, " (", round(coefs[sig_predictors, 1], 3), ")", sep="", collapse=", ")
  } else {
    sig_pred_text <- "None"
  }
  
  return(data.frame(
    Drug = drug_col,
    R_squared = round(r_squared, 3),
    Adj_R_squared = round(adj_r_squared, 3),
    Significant_Predictors = sig_pred_text,
    stringsAsFactors = FALSE
  ))
}

# Build models for all drugs
all_drug_models <- do.call(rbind, lapply(drug_columns, build_drug_models))

# Sort by R-squared for better interpretation
all_drug_models <- all_drug_models[order(-all_drug_models$R_squared),]

# Display results
print("Summary of linear models for all drugs based on personality traits:")
print(all_drug_models)

###############################################################################
# Most influenced by personality traits - Detailed model
###############################################################################

# Find the drug most influenced by personality traits (highest R-squared)
top_drug <- all_drug_models$Drug[1]
print(paste("The drug consumption most influenced by personality traits is:", top_drug))

# Build a detailed model for the top drug
top_drug_formula <- as.formula(paste(top_drug, "~ Nscore + Escore + Oscore + Ascore + Cscore + Impulsive + SS"))
top_drug_model <- lm(top_drug_formula, data = drug_data_numeric)

# Display detailed model summary
print(paste("Detailed model for", top_drug, "consumption:"))
summary(top_drug_model)

# Visualize coefficients
coef_data <- data.frame(
  Trait = names(coef(top_drug_model))[-1],  # exclude intercept
  Coefficient = coef(top_drug_model)[-1]
)

ggplot(coef_data, aes(x = reorder(Trait, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(Coefficient, 2)), vjust = 0.5, hjust = -0.1) +
  labs(title = paste("Personality Trait Coefficients for", top_drug, "Consumption"),
       x = "Personality Trait", y = "Coefficient Value") +
  coord_flip() +
  theme_minimal()

###############################################################################
# Demographics-only model for comparison
###############################################################################

# Create a demographics-only model for Cannabis
cannabis_demo_model <- lm(Cannabis ~ Age + Gender + Education + Country + Ethnicity, 
                          data = drug_data_numeric)

# Display model summary
print("Linear model for Cannabis based on demographics only:")
summary(cannabis_demo_model)

# Compare trait-only, demographics-only, and full models
print("Comparing different model specifications:")
print(anova(cannabis_trait_model, cannabis_demo_model, cannabis_full_model))

###############################################################################
# Predicted vs Actual Values Analysis
###############################################################################

# Add predictions to the data
drug_data_numeric$predicted_cannabis <- predict(cannabis_full_model)

# Calculate prediction error
drug_data_numeric$cannabis_error <- drug_data_numeric$Cannabis - drug_data_numeric$predicted_cannabis

# Plot predicted vs actual values
ggplot(drug_data_numeric, aes(x = predicted_cannabis, y = Cannabis)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted vs Actual Cannabis Consumption",
       x = "Predicted Consumption Level", y = "Actual Consumption Level") +
  theme_minimal()

# Plot prediction errors by age group
ggplot(drug_data_numeric, aes(x = Age, y = cannabis_error)) +
  geom_boxplot() +
  labs(title = "Cannabis Consumption Prediction Errors by Age Group",
       x = "Age Group", y = "Prediction Error") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################################################
# Model Diagnostics
###############################################################################

# Diagnostic plots for the full Cannabis model
par(mfrow = c(2, 2))
plot(cannabis_full_model)

# Reset plotting parameter
par(mfrow = c(1, 1))

###############################################################################
# Cross-validation for model accuracy
###############################################################################

# Set up cross-validation
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)

# Train model with cross-validation
cv_model <- train(Cannabis ~ Nscore + Escore + Oscore + Ascore + Cscore + Impulsive + SS +
                    Age + Gender + Education + Country,
                  data = drug_data_numeric,
                  method = "lm",
                  trControl = train_control)

# Display cross-validation results
print("Cross-validation results for Cannabis model:")
print(cv_model)
print(cv_model$results)

###############################################################################
# Conclusion and summary statistics
###############################################################################

# Summary of R-squared values for all drug models
mean_r_squared <- mean(all_drug_models$R_squared)
median_r_squared <- median(all_drug_models$R_squared)

print(paste("Mean R-squared across all drug models:", round(mean_r_squared, 3)))
print(paste("Median R-squared across all drug models:", round(median_r_squared, 3)))

# Count occurrences of each trait as significant predictor
trait_counts <- sapply(trait_col, function(trait) {
  sum(grepl(trait, all_drug_models$Significant_Predictors))
})

print("Number of drug models where each trait is a significant predictor:")
print(trait_counts)
