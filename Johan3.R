# Load required libraries
library(dplyr)
library(ggplot2)
library(car)

# Read the data
model_data <- read.csv("Data/model_data.csv")

# Remove the first index column if it exists
if(names(model_data)[1] == "X") {
  model_data <- model_data[,-1]
}

# Define the predictors to use in models
predictors <- c("Age", "Gender", "Education", 
                "Nscore", "Escore", "Oscore", "Ascore", "Cscore", 
                "Impulsive", "SS")

# Function to fit Poisson GLM for a specific drug
fit_poisson_glm <- function(data, drug, predictors) {
  # Create formula
  formula_str <- paste(drug, "~", paste(predictors, collapse = " + "))
  formula <- as.formula(formula_str)
  
  # Fit Poisson GLM
  model <- glm(formula, data = data, family = poisson(link = "log"))
  
  # Return model
  return(model)
}

# Drugs to model
drugs <- c("Cannabis", "Alcohol", "Nicotine", "Coke")

# Fit models for each drug
models <- list()
for (drug in drugs) {
  if (drug %in% names(model_data)) {
    models[[drug]] <- fit_poisson_glm(model_data, drug, predictors)
    cat("Model fitted for", drug, "\n")
  } else {
    cat("Drug column", drug, "not found in dataset\n")
  }
}

# Function to create a summary table for a model
create_model_summary <- function(model) {
  # Extract model summary
  model_summary <- summary(model)
  
  # Extract coefficients and statistics
  coefs <- model_summary$coefficients
  
  # Calculate exp(Coefficients) for interpretation
  exp_coefs <- exp(coefs[, "Estimate"])
  
  # Create data frame for display
  results <- data.frame(
    Variable = rownames(coefs),
    Estimate = coefs[, "Estimate"],
    Std_Error = coefs[, "Std. Error"],
    z_value = coefs[, "z value"],
    p_value = coefs[, "Pr(>|z|)"],
    exp_Estimate = exp_coefs,
    stringsAsFactors = FALSE
  )
  
  # Add significance stars
  results$significance <- ""
  results$significance[results$p_value < 0.1] <- "."
  results$significance[results$p_value < 0.05] <- "*"
  results$significance[results$p_value < 0.01] <- "**"
  results$significance[results$p_value < 0.001] <- "***"
  
  # Add percentage change column for non-intercept terms
  results$percent_change <- NA
  for (i in 2:nrow(results)) {  # Skip intercept
    if (results$exp_Estimate[i] > 1) {
      results$percent_change[i] <- paste0("+", round((results$exp_Estimate[i] - 1) * 100, 2), "%")
    } else {
      results$percent_change[i] <- paste0("-", round((1 - results$exp_Estimate[i]) * 100, 2), "%")
    }
  }
  
  return(results)
}

# Create summary tables for all models
model_summaries <- list()
for (drug in names(models)) {
  model_summaries[[drug]] <- create_model_summary(models[[drug]])
}

# Print Cannabis model summary
print(model_summaries[["Cannabis"]])

# Compare model fit statistics
model_comparison <- data.frame(
  Drug = character(),
  AIC = numeric(),
  BIC = numeric(),
  LogLik = numeric(),
  Deviance = numeric(),
  PseudoR2 = numeric(),
  stringsAsFactors = FALSE
)

for (drug in names(models)) {
  model <- models[[drug]]
  
  # Calculate McFadden's Pseudo R²
  null_model <- glm(as.formula(paste(drug, "~ 1")), 
                    data = model_data, 
                    family = poisson(link = "log"))
  pseudo_r2 <- 1 - (logLik(model) / logLik(null_model))
  
  model_comparison <- rbind(model_comparison, data.frame(
    Drug = drug,
    AIC = AIC(model),
    BIC = BIC(model),
    LogLik = as.numeric(logLik(model)),
    Deviance = model$deviance,
    PseudoR2 = as.numeric(pseudo_r2),
    stringsAsFactors = FALSE
  ))
}

# Print model comparison
print(model_comparison)

# Check for overdispersion in Cannabis model
cannabis_model <- models[["Cannabis"]]
dispersion_param <- sum(residuals(cannabis_model, type = "pearson")^2) / cannabis_model$df.residual
cat("Dispersion parameter for Cannabis model:", round(dispersion_param, 4), "\n")

if (dispersion_param > 1.5) {
  cat("Evidence of overdispersion. Consider using a negative binomial model instead.\n")
} else {
  cat("No strong evidence of overdispersion. Poisson model appears appropriate.\n")
}

# Fit negative binomial model for comparison (if MASS package is available)
if (requireNamespace("MASS", quietly = TRUE)) {
  nb_model <- MASS::glm.nb(Cannabis ~ Age + Gender + Education + 
                             Nscore + Escore + Oscore + Ascore + Cscore + 
                             Impulsive + SS, data = model_data)
  
  # Compare AIC between Poisson and negative binomial
  cat("\nModel comparison - Cannabis:\n")
  cat("Poisson AIC:", AIC(cannabis_model), "\n")
  cat("Negative Binomial AIC:", AIC(nb_model), "\n")
  cat("Theta value in NB model:", nb_model$theta, "\n")
}

# Visualize effects of key predictors on Cannabis usage
# Use coefficients from the model to calculate predicted values

# For Age effect
age_range <- 1:6  # Age categories 18-24 through 65+
age_effect <- data.frame(
  Age = age_range,
  Predicted = exp(coef(cannabis_model)["(Intercept)"] + coef(cannabis_model)["Age"] * age_range)
)

ggplot(age_effect, aes(x = Age, y = Predicted)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  theme_minimal() +
  labs(title = "Effect of Age on Predicted Cannabis Usage",
       x = "Age Category (1=18-24, 6=65+)",
       y = "Predicted Cannabis Usage Level") +
  scale_x_continuous(breaks = 1:6)

# For Sensation Seeking effect (keeping other variables at their means)
ss_range <- seq(min(model_data$SS), max(model_data$SS), length.out = 100)
mean_values <- colMeans(model_data[, predictors[predictors != "SS"]])

ss_effect <- data.frame(
  SS = ss_range,
  Predicted = exp(coef(cannabis_model)["(Intercept)"] + 
                    coef(cannabis_model)["Age"] * mean_values["Age"] +
                    coef(cannabis_model)["Gender"] * mean_values["Gender"] +
                    coef(cannabis_model)["Education"] * mean_values["Education"] +
                    coef(cannabis_model)["Nscore"] * mean_values["Nscore"] +
                    coef(cannabis_model)["Escore"] * mean_values["Escore"] +
                    coef(cannabis_model)["Oscore"] * mean_values["Oscore"] +
                    coef(cannabis_model)["Ascore"] * mean_values["Ascore"] +
                    coef(cannabis_model)["Cscore"] * mean_values["Cscore"] +
                    coef(cannabis_model)["Impulsive"] * mean_values["Impulsive"] +
                    coef(cannabis_model)["SS"] * ss_range)
)

ggplot(ss_effect, aes(x = SS, y = Predicted)) +
  geom_line(color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Effect of Sensation Seeking on Predicted Cannabis Usage",
       x = "Sensation Seeking Score",
       y = "Predicted Cannabis Usage Level")

# Create enhanced coefficient plot for Cannabis model similar to the Drug Consumption.Rmd file
# Function to create a visually appealing coefficient plot
plot_factor_importance_enhanced <- function(model, title, color_scheme = "viridis") {
  # Extract model coefficients
  coefs <- summary(model)$coefficients
  
  # Filter out intercept and create data frame for plotting
  coef_df <- data.frame(
    Variable = rownames(coefs)[-1],  # Exclude intercept
    Estimate = coefs[-1, "Estimate"],
    StdError = coefs[-1, "Std. Error"],
    PValue = coefs[-1, "Pr(>|z|)"]
  )
  
  # Add significance markers and categories
  coef_df$Significance <- ifelse(coef_df$PValue < 0.001, "p < 0.001", 
                                 ifelse(coef_df$PValue < 0.01, "p < 0.01",
                                        ifelse(coef_df$PValue < 0.05, "p < 0.05", "Not significant")))
  
  # Sort by absolute value of estimate
  coef_df <- coef_df[order(abs(coef_df$Estimate), decreasing = TRUE), ]
  
  # Clean up variable names for display
  var_display_mapping <- c(
    "Age" = "Age",
    "Gender" = "Gender (Male=1)",
    "Education" = "Education Level",
    "Nscore" = "Neuroticism",
    "Escore" = "Extraversion",
    "Oscore" = "Openness",
    "Ascore" = "Agreeableness",
    "Cscore" = "Conscientiousness",
    "Impulsive" = "Impulsivity",
    "SS" = "Sensation Seeking"
  )
  
  # Function to clean up variable names
  clean_var_names <- function(var_name) {
    # First check exact matches in our mapping
    if (var_name %in% names(var_display_mapping)) {
      return(var_display_mapping[var_name])
    }
    
    # Handle country variables
    if (grepl("^Country", var_name)) {
      return(gsub("Country", "Country: ", var_name))
    }
    
    # Handle ethnicity variables
    if (grepl("^Ethnicity", var_name)) {
      return(gsub("Ethnicity", "Ethnicity: ", var_name))
    }
    
    return(var_name)
  }
  
  # Apply name cleaning
  coef_df$DisplayName <- sapply(coef_df$Variable, clean_var_names)
  
  # Reorder factor levels for plotting
  coef_df$DisplayName <- factor(coef_df$DisplayName, levels = rev(coef_df$DisplayName))
  
  # Define significance color palette
  if (color_scheme == "viridis") {
    sig_colors <- c("p < 0.001" = "#440154", "p < 0.01" = "#21908C", 
                    "p < 0.05" = "#5DC863", "Not significant" = "#CCCCCC")
  } else {
    sig_colors <- c("p < 0.001" = "#0072B2", "p < 0.01" = "#009E73", 
                    "p < 0.05" = "#56B4E9", "Not significant" = "#CCCCCC")
  }
  
  # Plot with enhanced aesthetics
  ggplot(coef_df, aes(x = Estimate, y = DisplayName, color = Significance)) +
    geom_point(aes(size = abs(Estimate)), alpha = 0.8) +
    geom_errorbarh(aes(xmin = Estimate - 1.96 * StdError, 
                       xmax = Estimate + 1.96 * StdError), 
                   height = 0.2, alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray", linewidth = 0.7) +
    scale_color_manual(values = sig_colors) +
    scale_size_continuous(range = c(2, 6), guide = "none") +
    labs(title = title,
         subtitle = "Estimated coefficients with 95% confidence intervals",
         x = "Effect Size (Coefficient Estimate)",
         y = "",
         color = "Statistical\nSignificance") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, color = "darkgray", hjust = 0.5),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 10),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

# Create diagnostic plots with enhanced aesthetics similar to Drug Consumption.Rmd
create_diagnostic_plots <- function(model, title, color_scheme = "blue") {
  # Extract residuals data for plotting
  model_data <- broom::augment(model)
  
  # Define color palette
  if (color_scheme == "blue") {
    point_color <- "#3182bd"
    line_color <- "#08519c"
    reference_color <- "#e41a1c"
  } else if (color_scheme == "green") {
    point_color <- "#31a354"
    line_color <- "#006d2c"
    reference_color <- "#d62728"
  } else {
    point_color <- "#756bb1"
    line_color <- "#54278f"
    reference_color <- "#e41a1c"
  }
  
  # Common theme elements
  diagnostic_theme <- theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "gray50"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
    )
  
  # 1. Residuals vs Fitted with improved aesthetics
  p1 <- ggplot(model_data, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.6, color = point_color, size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = reference_color, 
               linewidth = 0.7) +
    geom_smooth(se = TRUE, color = line_color, fill = alpha(line_color, 0.2), 
                method = "loess") +
    labs(title = "Residuals vs Fitted",
         subtitle = "Should show random scatter around the zero line",
         x = "Fitted values",
         y = "Residuals") +
    diagnostic_theme
  
  # 2. Normal Q-Q plot with improved aesthetics
  p2 <- ggplot(model_data, aes(sample = .resid)) +
    stat_qq(color = point_color, size = 1.5, alpha = 0.6) +
    stat_qq_line(color = reference_color, linewidth = 0.7) +
    labs(title = "Normal Q-Q Plot",
         subtitle = "Points should follow the diagonal line",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    diagnostic_theme
  
  # 3. Scale-Location plot with improved aesthetics
  p3 <- ggplot(model_data, aes(x = .fitted, y = sqrt(abs(.resid)))) +
    geom_point(alpha = 0.6, color = point_color, size = 1.5) +
    geom_smooth(se = TRUE, color = line_color, fill = alpha(line_color, 0.2), 
                method = "loess") +
    labs(title = "Scale-Location",
         subtitle = "Should show homogeneous variance",
         x = "Fitted values",
         y = "sqrt|Standardized Residuals|") +
    diagnostic_theme
  
  # 4. Residuals vs Leverage with improved aesthetics
  # Add Cook's distance contour lines
  p4 <- ggplot(model_data, aes(x = .hat, y = .resid)) +
    geom_point(alpha = 0.6, color = point_color, size = 1.5) +
    geom_smooth(se = TRUE, color = line_color, fill = alpha(line_color, 0.2), 
                method = "loess") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    # Add Cook's distance contours if available
    stat_contour(aes(z = .cooksd), breaks = c(0.5, 1), color = "red", 
                 linetype = "dashed", na.rm = TRUE) +
    labs(title = "Residuals vs Leverage",
         subtitle = "Identifies influential cases",
         x = "Leverage",
         y = "Standardized Residuals") +
    diagnostic_theme
  
  # Combine plots with better layout using gridExtra
  title_grob <- grid::textGrob(
    title, 
    gp = grid::gpar(fontsize = 16, fontface = "bold"),
    just = "center"
  )
  subtitle_grob <- grid::textGrob(
    "Diagnostic Plots for Poisson GLM", 
    gp = grid::gpar(fontsize = 12, col = "gray30"),
    just = "center"
  )
  
  # Arrange the title, subtitle, and plots
  combined_plot <- gridExtra::grid.arrange(
    gridExtra::arrangeGrob(title_grob, subtitle_grob, heights = c(1, 0.5), ncol = 1),
    gridExtra::arrangeGrob(p1, p2, p3, p4, ncol = 2),
    heights = c(1, 10)
  )
  
  return(combined_plot)
}

# Generate the enhanced plots
cannabis_coef_plot <- plot_factor_importance_enhanced(cannabis_model, 
                                                      "Predictors of Cannabis Usage")
print(cannabis_coef_plot)

# Generate enhanced diagnostic plots
cannabis_diagnostics <- create_diagnostic_plots(cannabis_model, 
                                                "Cannabis Usage Model Diagnostics")
print(cannabis_diagnostics)

# Check for multicollinearity
car::vif(cannabis_model)

# Function to run a more detailed analysis for the Cannabis model
analyze_cannabis_model <- function(model) {
  # Get model summary
  model_summary <- summary(model)
  
  # Print key statistics
  cat("\n=== Cannabis Model Analysis ===\n")
  cat("Number of observations:", nrow(model$model), "\n")
  cat("Null deviance:", model$null.deviance, "on", model$df.null, "degrees of freedom\n")
  cat("Residual deviance:", model$deviance, "on", model$df.residual, "degrees of freedom\n")
  cat("AIC:", model$aic, "\n")
  
  # Calculate and print McFadden's Pseudo R²
  null_model <- glm(Cannabis ~ 1, data = model_data, family = poisson(link = "log"))
  pseudo_r2 <- 1 - (logLik(model) / logLik(null_model))
  cat("McFadden's Pseudo R²:", round(as.numeric(pseudo_r2), 4), "\n")
  
  # Check for overdispersion
  dispersion <- sum(residuals(model, type = "pearson")^2) / model$df.residual
  cat("Dispersion parameter:", round(dispersion, 4), "\n")
  
  # Identify significant predictors
  sig_coefs <- coef(model_summary)[, c("Estimate", "Pr(>|z|)")]
  sig_coefs <- sig_coefs[2:nrow(sig_coefs), ]  # Remove intercept
  sig_coefs <- sig_coefs[sig_coefs[, "Pr(>|z|)"] < 0.05, ]
  sig_coefs <- sig_coefs[order(abs(sig_coefs[, "Estimate"]), decreasing = TRUE), ]
  
  cat("\nSignificant predictors (in order of effect size):\n")
  for (i in 1:nrow(sig_coefs)) {
    var_name <- rownames(sig_coefs)[i]
    estimate <- sig_coefs[i, "Estimate"]
    exp_est <- exp(estimate)
    direction <- ifelse(estimate > 0, "positive", "negative")
    
    if (exp_est > 1) {
      effect <- paste0(round((exp_est - 1) * 100, 2), "% increase")
    } else {
      effect <- paste0(round((1 - exp_est) * 100, 2), "% decrease")
    }
    
    cat(paste0(i, ". ", var_name, ": ", direction, " effect (", effect, ", p=", 
               round(sig_coefs[i, "Pr(>|z|)"], 4), ")\n"))
  }
  
  # Identify potential outliers
  pearson_resid <- residuals(model, type = "pearson")
  potential_outliers <- which(abs(pearson_resid) > 2)
  
  if (length(potential_outliers) > 0) {
    cat("\nPotential outliers (observations with |Pearson residual| > 2):\n")
    outlier_data <- data.frame(
      Observation = potential_outliers,
      Residual = pearson_resid[potential_outliers],
      Actual = model$model$Cannabis[potential_outliers],
      Predicted = fitted(model)[potential_outliers]
    )
    outlier_data <- outlier_data[order(abs(outlier_data$Residual), decreasing = TRUE), ]
    print(head(outlier_data, 5))
  }
  
  # Suggest possible model improvements
  cat("\nPossible model improvements:\n")
  if (dispersion > 1.2) {
    cat("- Consider using a negative binomial model to address overdispersion\n")
  }
  cat("- Consider interaction terms (e.g., Age × Education, Gender × SS)\n")
  cat("- Consider polynomial terms for continuous predictors if relationship is non-linear\n")
  
  return(invisible(NULL))
}

# Run detailed analysis for Cannabis model
analyze_cannabis_model(cannabis_model)

# If requested, fit models with interaction terms
cannabis_model_interactions <- glm(
  Cannabis ~ Age + Gender + Education + 
    Nscore + Escore + Oscore + Ascore + Cscore + Impulsive + SS +
    Age:Education + Gender:SS,
  data = model_data, 
  family = poisson(link = "log")
)

# Compare models with and without interactions
anova(cannabis_model, cannabis_model_interactions, test = "Chisq")
