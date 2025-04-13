###############################################################################
# Drug Consumption Data Analysis
###############################################################################

# Load required libraries
library(dplyr)

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

# Create a meaningful data frame with descriptive values
meaningful_data <- drug_data

# Convert demographic columns to descriptive values
meaningful_data$Age <- age_mapping[as.character(meaningful_data$Age)]
meaningful_data$Gender <- gender_mapping[as.character(meaningful_data$Gender)]
meaningful_data$Education <- education_mapping[as.character(meaningful_data$Education)]
meaningful_data$Country <- country_mapping[as.character(meaningful_data$Country)]
meaningful_data$Ethnicity <- ethnicity_mapping[as.character(meaningful_data$Ethnicity)]

# Convert all drug consumption columns to descriptive values
for (col in drug_columns) {
  meaningful_data[[col]] <- consumption_mapping[as.character(meaningful_data[[col]])]
}

###############################################################################
# Data Processing
###############################################################################



# Save the meaningful dataframe to a new CSV file
write.csv(meaningful_data, "Data/descriptive.csv", row.names = FALSE)
cat("Meaningful dataframe saved to 'Data/descriptive.csv'\n")

###############################################################################
# Generate Usage Counts (NOT INCLUDED)
###############################################################################

# Define the categories to count
categories <- unique(unlist(meaningful_data[drug_columns]))

# Initialize a new dataframe to store the results
results <- data.frame(ID = meaningful_data$ID)
for (category in categories) {
  results[[category]] <- 0
}

# Count occurrences of each category for each person
for (i in 1:nrow(meaningful_data)) {
  for (category in categories) {
    # Create a logical vector for matches
    matches <- sapply(drug_columns, function(col) {
      meaningful_data[i, col] == category
    })
    # Sum the matches
    count <- sum(matches, na.rm = TRUE)
    results[i, category] <- count
  }
}

# Merge the usage counts with the meaningful data
merged_data <- merge(meaningful_data, results, by = "ID", all = TRUE)

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
  usage_table <- table(meaningful_data[[col]])
  drug_usage_counts[[i]] <- data.frame(
    Drug = col,
    Usage = names(usage_table),
    Count = as.numeric(usage_table),
    Percentage = round(100 * as.numeric(usage_table) / nrow(meaningful_data), 1)
  )
}

# Combine all drug usage summaries
usage_summary <- do.call(rbind, drug_usage_counts)

# Print demographic distributions
cat("Age distribution:\n")
age_table <- table(meaningful_data$Age)
print(age_table)
print(round(100 * prop.table(age_table), 1))

cat("\nGender distribution:\n")
gender_table <- table(meaningful_data$Gender)
print(gender_table)
print(round(100 * prop.table(gender_table), 1))

cat("\nEducation distribution:\n")
education_table <- table(meaningful_data$Education)
print(education_table)
print(round(100 * prop.table(education_table), 1))

cat("\nCountry distribution:\n")
country_table <- table(meaningful_data$Country)
print(country_table)
print(round(100 * prop.table(country_table), 1))

cat("\nEthnicity distribution:\n")
ethnicity_table <- table(meaningful_data$Ethnicity)
print(ethnicity_table)
print(round(100 * prop.table(ethnicity_table), 1))

###############################################################################
# Save Results to Files
###############################################################################

