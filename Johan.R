###############################################################################
# Drug Consumption Data Analysis
###############################################################################

# Load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)

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
  "Alcohol", "Amphet", "Amy*l", "Benzos", "Caff", "Cannabis", 
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
cat("NA values by column:\n")
# Print only columns with NA values
print(na_by_column[na_by_column > 0])  
cat("\n")

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
outlier_summary <- outlier_summary[, c("variable", "min", "q1", "median", "mean", "q3", "max", 
                                       "iqr", "lower_bound", "upper_bound", 
                                       "n_outliers_below", "n_outliers_above", 
                                       "total_outliers", "outlier_percentage")]
print(outlier_summary)

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
# Generate Usage Counts (NOT INCLUDED)
###############################################################################

# Define the categories to count
categories <- unique(unlist(drug_data[drug_columns]))

# Initialize a new dataframe to store the results
results <- data.frame(ID = drug_data$ID)
for (category in categories) {
  results[[category]] <- 0
}

# Count occurrences of each category for each person
for (i in 1:nrow(drug_data)) {
  for (category in categories) {
    # Create a logical vector for matches
    matches <- sapply(drug_columns, function(col) {
      drug_data[i, col] == category
    })
    # Sum the matches
    count <- sum(matches, na.rm = TRUE)
    results[i, category] <- count
  }
}

# Merge the usage counts with the meaningful data
merged_data <- merge(drug_data, results, by = "ID", all = TRUE)

# Save the merged dataframe to a CSV file
write.csv(merged_data, "Data/merged_drug_data.csv", row.names = FALSE)
cat("Merged data saved to 'Data/merged_drug_data.csv'\n")
###############################################################################
# Generate Statistical Summaries
###############################################################################

# Drug usage counts
drug_usage_counts <- list()
for (i in 1:length(drug_columns)) {
  col <- drug_columns[i]
  usage_table <- table(drug_data[[col]])
  drug_usage_counts[[i]] <- data.frame(
    Drug = col,
    Usage = names(usage_table),
    Count = as.numeric(usage_table),
    Percentage = round(100 * as.numeric(usage_table) / nrow(drug_data), 1)
  )
}

# Combine all drug usage summaries
usage_summary <- do.call(rbind, drug_usage_counts)

# Print demographic distributions
cat("Age distribution:\n")
age_table <- table(drug_data$Age)
print(age_table)
print(round(100 * prop.table(age_table), 1))

cat("\nGender distribution:\n")
gender_table <- table(drug_data$Gender)
print(gender_table)
print(round(100 * prop.table(gender_table), 1))

cat("\nEducation distribution:\n")
education_table <- table(drug_data$Education)
print(education_table)
print(round(100 * prop.table(education_table), 1))

cat("\nCountry distribution:\n")
country_table <- table(drug_data$Country)
print(country_table)
print(round(100 * prop.table(country_table), 1))

cat("\nEthnicity distribution:\n")
ethnicity_table <- table(drug_data$Ethnicity)
print(ethnicity_table)
print(round(100 * prop.table(ethnicity_table), 1))


sample <- head(25)
write.csv(drug_data, "Data/sample.csv", row.names = FALSE) 
