###############################################################################
# Drug Consumption Data Analysis
###############################################################################

# Load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(knitr)
library(gridExtra)
library(tidyr)
library(naniar)

# Read the CSV file
drug_data <- read.csv("Data/drug_consumption.csv")

###############################################################################
# Define mappings and column information
###############################################################################

# Column names for the dataset
column_names <- c(
  "Index", "ID", "Age", "Gender", "Education", "Country", "Ethnicity", 
  "Nscore", "Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS", 
  "Alcohol", "Amphet", "Amyl", "Benzos", "Caff", "Cannabis", "Choc", "Coke", 
  "Crack", "Ecstasy", "Heroin", "Ketamine", "Legalh", "LSD", "Meth", 
  "Mushrooms", "Nicotine", "Semer", "VSA"
)

# Drug column names
drug_columns <- c(
  "Alcohol", "Amphet", "Amyl", "Benzos", "Caff", "Cannabis", 
  "Choc", "Coke", "Crack", "Ecstasy", "Heroin", "Ketamine", 
  "Legalh", "LSD", "Meth", "Mushrooms", "Nicotine", "Semer", "VSA"
)

# Mapping of drug consumption classes to their meanings
consumption_mapping <- c(
  "CL0" = "Never Used",
  "CL1" = "Used over a Decade Ago",
  "CL2" = "Used in Last Decade", 
  "CL3" = "Used in Last Year",
  "CL4" = "Used in Last Month",
  "CL5" = "Used in Last Week",
  "CL6" = "Used in Last Day"
)

# Map Age values to their meaning
age_mapping <- c(
  "-0.95197" = "18-24",
  "-0.07854" = "25-34",
  "0.49788" = "35-44",
  "1.09449" = "45-54",
  "1.82213" = "55-64",
  "2.59171" = "65+"
)

# Map Gender values to their meaning
gender_mapping <- c(
  "0.48246" = "Female",
  "-0.48246" = "Male"
)

# Map Education values to their meaning
education_mapping <- c(
  "-2.43591" = "Left school before 16 years",
  "-1.73790" = "Left school at 16 years",
  "-1.43719" = "Left school at 17 years",
  "-1.22751" = "Left school at 18 years",
  "-0.61113" = "Some college or university, no certificate or degree",
  "-0.05921" = "Professional certificate/diploma",
  "0.45468" = "University degree",
  "1.16365" = "Masters degree",
  "1.98437" = "Doctorate degree"
)

# Map Country values to their meaning
country_mapping <- c(
  "-0.09765" = "Australia",
  "0.24923" = "Canada",
  "-0.46841" = "New Zealand",
  "-0.28519" = "Other",
  "0.21128" = "Republic of Ireland",
  "0.96082" = "UK",
  "-0.57009" = "USA"
)

# Map Ethnicity values to their meaning
ethnicity_mapping <- c(
  "-0.50212" = "Asian",
  "-1.10702" = "Black",
  "1.90725" = "Mixed-Black/Asian",
  "0.12600" = "Mixed-White/Asian",
  "-0.22166" = "Mixed-White/Black",
  "0.11440" = "Other",
  "-0.31685" = "White"
)

###############################################################################
# Data Processing
###############################################################################

# Rename the columns 
colnames(drug_data) <- column_names

# Convert demographic columns to descriptive values
drug_data$Age <- age_mapping[as.character(drug_data$Age)]
drug_data$Gender <- gender_mapping[as.character(drug_data$Gender)]
drug_data$Education <- education_mapping[as.character(drug_data$Education)]
drug_data$Country <- country_mapping[as.character(drug_data$Country)]
drug_data$Ethnicity <- ethnicity_mapping[as.character(drug_data$Ethnicity)]

# Convert all drug consumption columns to descriptive values
for (col in drug_columns) {
  drug_data[[col]] <- consumption_mapping[as.character(drug_data[[col]])]
}

###############################################################################
# Data Cleaning - Missing values
###############################################################################

# Remove unnecessary column
drug_data <- drug_data[, -which(names(drug_data) == "ID")]

# Check for NA values in each column
na_by_column <- sapply(drug_data, function(x) sum(is.na(x)))

# Print only columns with NA values
cat("Number of missing values by columnn")
print(na_by_column[na_by_column > 0]) 


# Replace NA values with "Not Provided"
drug_data$Education[is.na(drug_data$Education)] <- "Not Provided"
drug_data$Ethnicity[is.na(drug_data$Ethnicity)] <- "Not Provided"

###############################################################################
# Data Cleaning - Looking for outliers
###############################################################################
# Define numeric columns for outlier analysis
numeric_cols <- c("Nscore", "Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS")

# Function to identify outliers using IQR method
identify_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  return(data.frame(
    min = min(x, na.rm = TRUE),
    q1 = q1,
    median = median(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    q3 = q3,
    max = max(x, na.rm = TRUE),
    iqr = iqr,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    n_outliers_below = sum(x < lower_bound, na.rm = TRUE),
    n_outliers_above = sum(x > upper_bound, na.rm = TRUE),
    total_outliers = sum(x < lower_bound | x > upper_bound, na.rm = TRUE),
    outlier_percentage = round(100 * sum(x < lower_bound | x > upper_bound, na.rm = TRUE) / length(x[!is.na(x)]), 2)
  ))
}

# Apply outlier detection to all numeric columns
outlier_summary <- data.frame()
for (col in numeric_cols) {
  result <- identify_outliers_iqr(drug_data[[col]])
  result$variable <- col
  outlier_summary <- rbind(outlier_summary, result)
}

# Display summary of outliers
# outlier_summary <- outlier_summary[, c("variable", "min", "q1", "median", "mean", "q3", "max", 
#                                        "iqr", "lower_bound", "upper_bound", 
#                                        "n_outliers_below", "n_outliers_above", 
#                                        "total_outliers", "outlier_percentage")]
# print(outlier_summary)

# Create a function to visualize outliers with boxplots
plot_outliers <- function(drug_data, columns) {
  # Explicitly use reshape2::melt to avoid namespace issues
  melted_data <- reshape2::melt(drug_data[, columns], id.vars = NULL)
  
  # Create boxplot
  ggplot(melted_data, aes(x = variable, y = value)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
    theme_minimal() +
    labs(title = "Boxplots with Outliers Highlighted",
         x = "Variable",
         y = "Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Visualize outliers
plot_outliers(drug_data, numeric_cols)

###############################################################################
# Exploratory Data Analysis
###############################################################################

# Basic summary statistics for each numerical column
# cat("\n=== Summary Statistics for Numerical Columns ===\n")
# for (col in numeric_cols) {
#   cat("\nStatistics for", col, ":\n")
#   print(summary(drug_data[[col]]))
#   cat("Standard Deviation:", sd(drug_data[[col]], na.rm = TRUE), "\n")
#   cat("Variance:", var(drug_data[[col]], na.rm = TRUE), "\n")
# }


###############################################################################
# Heatmap
###############################################################################
# Calculate the correlation matrix
cor_matrix <- cor(drug_data[numeric_cols], use = "pairwise.complete.obs")

# Convert the correlation matrix to a data frame for ggplot
cor_df <- melt(cor_matrix)
names(cor_df) <- c("Var1", "Var2", "Correlation")

# Create a ggplot2 correlation heatmap
ggplot(data = cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  coord_fixed() +
  labs(
    title = "Correlation Matrix of Personality Traits and Behavioral Measures",
    x = "",
    y = ""
  )





###############################################################################
# Gender comparison
###############################################################################

# Calculating the means of the behavioral scores
gender_results <- data.frame(
  Trait = character(),
  Female_Mean = numeric(),
  Male_Mean = numeric(),
  stringsAsFactors = FALSE
)

for (col in numeric_cols) {
  # Calculate means only
  female_mean <- mean(drug_data[drug_data$Gender == "Female", col], na.rm = TRUE)
  male_mean <- mean(drug_data[drug_data$Gender == "Male", col], na.rm = TRUE)
  
  # Add to results dataframe with only needed values
  gender_results <- rbind(gender_results, data.frame(
    Trait = col,
    Female_Mean = female_mean,
    Male_Mean = male_mean,
    stringsAsFactors = FALSE
  ))
}

# Create readable trait names
gender_results$Trait_Name <- case_when(
  gender_results$Trait == "Nscore" ~ "Neuroticism",
  gender_results$Trait == "Escore" ~ "Extraversion",
  gender_results$Trait == "Oscore" ~ "Openness",
  gender_results$Trait == "Ascore" ~ "Agreeableness",
  gender_results$Trait == "Cscore" ~ "Conscientiousness",
  gender_results$Trait == "Impulsive" ~ "Impulsivity",
  gender_results$Trait == "SS" ~ "Sensation Seeking",
  TRUE ~ gender_results$Trait
)

# Create the plot with value labels
ggplot(gender_results, aes(x = Trait_Name)) +
  # Add bars
  geom_bar(aes(y = Female_Mean, fill = "Female"), stat = "identity", position = "dodge", width = 0.7, alpha = 0.7) +
  geom_bar(aes(y = Male_Mean, fill = "Male"), stat = "identity", position = position_dodge(width = 0.7), width = 0.7, alpha = 0.7) +
  # Add value labels
  geom_text(aes(y = Female_Mean, label = sprintf("%.2f", Female_Mean)),
            position = position_dodge(width = 0.7), 
            hjust = ifelse(gender_results$Female_Mean < 0, 1.3, -0.3),  # Adjust horizontal position based on value
            size = 3) +
  geom_text(aes(y = Male_Mean, label = sprintf("%.2f", Male_Mean)),
            position = position_dodge(width = 0.7), 
            hjust = ifelse(gender_results$Male_Mean < 0, 1.3, -0.3),  # Adjust horizontal position based on value
            size = 3) +
  # Colors and formatting
  scale_fill_manual(values = c("Female" = "#FF9999", "Male" = "#6699CC"),
                    name = "Gender") +
  labs(title = "Gender Differences in Behavioral Measures",
       x = "",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "top") +
  # Fixed axis limits from -0.25 to 0.25
  scale_y_continuous(limits = c(-0.25, 0.25)) +
  coord_flip()


###############################################################################
# Education comparison
###############################################################################

# Order education levels
education_order <- c(
  "Left school before 16 years",
  "Left school at 16 years",
  "Left school at 17 years",
  "Left school at 18 years",
  "Some college or university, no certificate or degree",
  "Professional certificate/diploma",
  "University degree",
  "Masters degree",
  "Doctorate degree",
  "Not Provided"
)

# Calculate means by education level
education_means <- drug_data %>%
  group_by(Education) %>%
  summarize(
    n = n(),
    across(all_of(numeric_cols), ~mean(.x, na.rm = TRUE))
  ) %>%
  arrange(match(Education, education_order))

# Convert to long format for heatmap
education_long <- education_means %>%
  pivot_longer(
    cols = all_of(numeric_cols),
    names_to = "Trait",
    values_to = "Mean"
  ) %>%
  mutate(
    Trait_Name = case_when(
      Trait == "Nscore" ~ "Neuroticism",
      Trait == "Escore" ~ "Extraversion",
      Trait == "Oscore" ~ "Openness",
      Trait == "Ascore" ~ "Agreeableness",
      Trait == "Cscore" ~ "Conscientiousness",
      Trait == "Impulsive" ~ "Impulsivity",
      Trait == "SS" ~ "Sensation Seeking",
      TRUE ~ Trait
    )
  )

# Create a clean table view of the data
education_display <- education_means %>%
  select(-n) %>%
  rename(
    "Neuroticism" = Nscore,
    "Extraversion" = Escore,
    "Openness" = Oscore,
    "Agreeableness" = Ascore,
    "Conscientiousness" = Cscore,
    "Impulsivity" = Impulsive,
    "Sensation Seeking" = SS
  )

# Display the table with formatted cells
kableExtra::kable(education_display, 
                  caption = "Personality Traits by Education Level",
                  digits = 2,
                  longtable = TRUE,
                  booktabs = TRUE) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), 
                            font_size = 9,
                            latex_options = c("scale_down", "hold_position")) %>%
  kableExtra::column_spec(1, bold = TRUE) %>%
  kableExtra::row_spec(0, background = "#f5f5f5", bold = TRUE) %>%
  kableExtra::footnote(general = "Values represent mean scores for each trait",
                       footnote_as_chunk = TRUE)

############## Alt approach ###############
# Create a data frame for plotting
education_plot_data <- education_means %>%
  select(-n)

# Simplified visualization approach - plot means as separate bar charts  
par(mfrow = c(4, 2), mar = c(8, 4, 2, 1))  # 4 rows, 2 columns of plots

# Plot each trait separately
for (col in c("Nscore", "Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS")) {
  trait_name <- case_when(
    col == "Nscore" ~ "Neuroticism",
    col == "Escore" ~ "Extraversion",
    col == "Oscore" ~ "Openness",
    col == "Ascore" ~ "Agreeableness",
    col == "Cscore" ~ "Conscientiousness",
    col == "Impulsive" ~ "Impulsivity",
    col == "SS" ~ "Sensation Seeking",
    TRUE ~ col
  )
  
  # Create a simple barplot
  bp <- barplot(education_plot_data[[col]], 
                names.arg = rep("", nrow(education_plot_data)),  # Empty names for now
                col = ifelse(education_plot_data[[col]] > 0, "salmon", "lightblue"),
                main = trait_name,
                border = NA,
                las = 2,
                ylim = c(min(education_plot_data[[col]]) - 0.05, 
                         max(education_plot_data[[col]]) + 0.05),
                cex.main = 0.9,
                cex.axis = 0.8)
  
  # Add a reference line at y=0
  abline(h = 0, lty = 2, col = "gray")
  
  # Add abbreviated education labels
  edu_labels <- c("Before 16", "At 16", "At 17", "At 18", "Some college", 
                  "Prof cert", "University", "Masters", "Doctorate", "Not Provided")
  edu_labels <- edu_labels[1:length(bp)]  # Ensure we have the right number of labels
  
  # Add text labels at the bottom
  text(bp, par("usr")[3] - 0.02, srt = 45, adj = 1, labels = edu_labels, 
       xpd = TRUE, cex = 0.7)
}

# Add a title for the entire set of plots
mtext("Personality Traits by Education Level", side = 3, line = -1.5, 
      outer = TRUE, cex = 1.2, font = 2)

###############################################################################
# Seremon
###############################################################################

# Count Semeron users vs non-users
semeron_counts <- drug_data %>%
  group_by(Semer) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Create a nicely formatted table for the detailed counts
semeron_table <- semeron_counts %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2)) %>%
  rename(`Usage Category` = Semer) %>%
  kable(caption = "Semeron Usage Categories", 
        format = "html", 
        col.names = c("Usage Category", "Count", "Percentage (%)")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE,
                position = "center") %>%
  row_spec(0, bold = TRUE, background = "#E0E0E0")

# Display the table
print(semeron_table)


################################################################################
# Prep the data for modeling
################################################################################

# Make a copy of the dataset
model_data <- drug_data

# Remove the fake drug Semeron and index column
model_data <- model_data %>% 
  select(-c(Semer, Index))

# Map drug levels
consumption_levels <- c(
  "Never Used" = 0,
  "Used over a Decade Ago" = 1,
  "Used in Last Decade" = 2,
  "Used in Last Year" = 3,
  "Used in Last Month" = 4,
  "Used in Last Week" = 5,
  "Used in Last Day" = 6
)

# Iterate through each specified drug column
for (col_name in drug_columns) {
  if (col_name %in% names(model_data)) {
    column_values_as_char <- as.character(model_data[[col_name]])
    model_data[[col_name]] <- unname(consumption_levels[column_values_as_char])
  } 
}

# Convert gender to binary encoding
model_data$Gender <- ifelse(model_data$Gender == "Male", 1, 0)

# Change Age levels to ordinal
age_levels <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
model_data$Age <- as.integer(factor(model_data$Age, levels = age_levels))

# Change Education levels to ordinal
education_levels <- c(
  "Not Provided",
  "Left school before 16 years",
  "Left school at 16 years",
  "Left school at 17 years",
  "Left school at 18 years",
  "Some college or university, no certificate or degree",
  "Professional certificate/diploma",
  "University degree",
  "Masters degree",
  "Doctorate degree"
)
model_data$Education <- as.integer(factor(model_data$Education, levels = education_levels))

# Country - One-hot Encoding
country <- model.matrix(~ Country - 1, data = model_data)
model_data <- cbind(model_data, country)

# Ethnicity - One-hot Encoding
ethnicity <- model.matrix(~ Ethnicity - 1, data = model_data)
model_data <- cbind(model_data, ethnicity)

# Remove the original columns
model_data <- model_data %>% select(-c(Country, Ethnicity))

# Save the updated dataframe back to CSV
write.csv(model_data, "Data/model_data.csv")


