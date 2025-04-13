###################### First steps  ############################################
# Read the CSV file
drug_data <- read.csv("Data/drug_consumption.csv")

# Define the column mapping
column_names <- c(
  "Index", "ID", "Age", "Gender", "Education", "Country", "Ethnicity", 
  "Nscore", "Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS", 
  "Alcohol", "Amphet", "Amyl", "Benzos", "Caff", "Cannabis", "Choc", "Coke", 
  "Crack", "Ecstasy", "Heroin", "Ketamine", "Legalh", "LSD", "Meth", 
  "Mushrooms", "Nicotine", "Semer", "VSA"        
)

# Rename the columns 
colnames(drug_data) <- column_names

# Display the first few rows to verify
print(head(drug_data))

# Get basic information about the dataframe
print(str(drug_data))

# Get summary statistics
print(summary(drug_data))

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

# Map Education to catogries
edu_mapping <- c(
  "-2.43591" = "1", # Left school before 16 years
  "-1.73790" = "2", # Left school at 16 years
  "-1.43719" = "3", # Left school at 17 years
  "-1.22751" = "4", # Left school at 18 years
  "-0.61113" = "5", # Some college or university, no certificate or degree
  "-0.05921" = "6", # Professional certificate/ diploma
  "0.45468" = "7",  # University degree
  "1.16365" = "8",  # Masters degree
  "1.98437" = "9"   # Doctorate degree
)

# Create a version with descriptive values for categorical variables
drug_data_descriptive <- drug_data

# Convert drug consumption columns to factors with descriptive levels
drug_columns <- 15:33
for (i in drug_columns) {
  drug_data_descriptive[, i] <- factor(drug_data_descriptive[, i], 
                                       levels = names(consumption_mapping),
                                       labels = consumption_mapping)
}

# Convert Age to descriptive values
drug_data_descriptive$Age <- factor(as.character(drug_data_descriptive$Age),
                                    levels = names(age_mapping),
                                    labels = age_mapping)

# Convert Gender to descriptive values
drug_data_descriptive$Gender <- factor(as.character(drug_data_descriptive$Gender),
                                       levels = names(gender_mapping),
                                       labels = gender_mapping)

# Convert Education to descriptive values
drug_data_descriptive$Education <- factor(as.character(drug_data_descriptive$Education),
                                       levels = names(edu_mapping),
                                       labels = edu_mapping)

# Display the first few rows with descriptive values
print(head(drug_data_descriptive))

# Save the renamed dataframe to a new CSV file
write.csv(drug_data, "drug_consumption_renamed.csv", row.names = FALSE)
print("Renamed dataframe saved to 'drug_consumption_renamed.csv'")

# For analysis, let's see drug usage patterns
usage_summary <- lapply(drug_columns, function(i) {
  col_name <- colnames(drug_data)[i]
  table_data <- table(drug_data[, i])
  data.frame(
    Drug = col_name,
    Class = names(table_data),
    Count = as.numeric(table_data)
  )
})

# Combine all drug usage summaries
usage_df <- do.call(rbind, usage_summary)
print(usage_df)

# Age distribution
print(table(drug_data_descriptive$Age))

# Gender distribution
print(table(drug_data_descriptive$Gender))

###################Create a descriptive dataset#################################
# Read the CSV file
drug_data <- read.csv("Data/drug_consumption.csv")

# Define the column mapping
column_names <- c(
  "Index", "ID", "Age", "Gender", "Education", "Country", "Ethnicity", 
  "Nscore", "Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS", 
  "Alcohol", "Amphet", "Amyl", "Benzos", "Caff", "Cannabis", "Choc", "Coke", 
  "Crack", "Ecstasy", "Heroin", "Ketamine", "Legalh", "LSD", "Meth", 
  "Mushrooms", "Nicotine", "Semer", "VSA"        
)

# Rename the columns 
colnames(drug_data) <- column_names

# Create a dataframe with meaningful values
meaningful_data <- drug_data

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

# Convert all drug consumption columns to descriptive values
drug_columns <- 15:33
for (i in drug_columns) {
  meaningful_data[, i] <- consumption_mapping[as.character(meaningful_data[, i])]
}

# Convert Age to descriptive values
meaningful_data$Age <- age_mapping[as.character(meaningful_data$Age)]

# Convert Gender to descriptive values
meaningful_data$Gender <- gender_mapping[as.character(meaningful_data$Gender)]

# Convert Education to descriptive values
meaningful_data$Education <- education_mapping[as.character(meaningful_data$Education)]

# Convert Country to descriptive values
meaningful_data$Country <- country_mapping[as.character(meaningful_data$Country)]

# Convert Ethnicity to descriptive values
meaningful_data$Ethnicity <- ethnicity_mapping[as.character(meaningful_data$Ethnicity)]

# Display the first few rows with meaningful values
print(head(meaningful_data))

# Save the meaningful dataframe to a new CSV file
write.csv(meaningful_data, "Data/descriptive.csv", row.names = FALSE)
print("Meaningful dataframe saved to 'drug_consumption_meaningful.csv'")

# Basic statistics on the meaningful data
drug_usage_counts <- lapply(drug_columns, function(i) {
  col_name <- colnames(meaningful_data)[i]
  usage_table <- table(meaningful_data[, i])
  data.frame(
    Drug = col_name,
    Usage = names(usage_table),
    Count = as.numeric(usage_table)
  )
})

# Combine all drug usage summaries
usage_summary <- do.call(rbind, drug_usage_counts)
print(usage_summary)

# Age distribution
print(table(meaningful_data$Age))

# Gender distribution
print(table(meaningful_data$Gender))
 
# Education distribution
print(table(meaningful_data$Education))

# Country distribution
print(table(meaningful_data$Country))

# Ethnicity distribution
print(table(meaningful_data$Ethnicity))




