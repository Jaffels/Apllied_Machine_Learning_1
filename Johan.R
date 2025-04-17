###############################################################################
# Drug Consumption Data Analysis
###############################################################################

# Load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(gridExtra)


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
cat("NA values by column:\n")

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
                  digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                font_size = 11,
                full_width = TRUE) %>%
  column_spec(1, bold = TRUE, width = "15em") %>%
  row_spec(0, background = "#f5f5f5", bold = TRUE) %>%
  footnote(general = "Values represent mean scores for each trait",
           footnote_as_chunk = TRUE) %>%
  scroll_box(width = "100%", height = "400px")



###############################################################################
# Seremon
###############################################################################

# 1. Separate users into groups
separate_semeron_groups <- function(drug_data) {
  # Separate users who report Semeron use vs those who don't
  semeron_users <- drug_data[drug_data$Semer != "Never Used", ]
  non_users <- drug_data[drug_data$Semer == "Never Used", ]
  
  return(list(
    semeron_users = semeron_users,
    non_users = non_users
  ))
}

# 2. Analyze demographic differences
analyze_demographics <- function(semeron_users, non_users) {
  # Initialize comparison dataframe
  demo_comparison <- data.frame(
    Variable = character(),
    Semeron_Users_Pct = numeric(),
    Non_Users_Pct = numeric(),
    Difference = numeric()
  )
  
  # Add demographic comparisons
  for (var in c("Age", "Gender", "Education", "Country", "Ethnicity")) {
    var_users <- table(semeron_users[[var]]) / nrow(semeron_users) * 100
    var_non_users <- table(non_users[[var]]) / nrow(non_users) * 100
    
    # Create comparison for each category
    for (cat in unique(c(names(var_users), names(var_non_users)))) {
      user_pct <- if(cat %in% names(var_users)) var_users[cat] else 0
      non_user_pct <- if(cat %in% names(var_non_users)) var_non_users[cat] else 0
      
      demo_comparison <- rbind(demo_comparison, data.frame(
        Variable = paste(var, "-", cat),
        Semeron_Users_Pct = user_pct,
        Non_Users_Pct = non_user_pct,
        Difference = user_pct - non_user_pct
      ))
    }
  }
  
  # Add absolute difference for sorting
  demo_comparison$AbsDiff <- abs(demo_comparison$Difference)
  
  return(demo_comparison)
}

# 3. Analyze personality trait differences
analyze_traits <- function(semeron_users, non_users) {
  trait_comparison <- data.frame(
    Trait = character(),
    Semeron_Users_Mean = numeric(),
    Non_Users_Mean = numeric(),
    Difference = numeric(),
    T_Test_P_Value = numeric()
  )
  
  for (trait in c("Nscore", "Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS")) {
    # Calculate means
    user_mean <- mean(semeron_users[[trait]], na.rm = TRUE)
    non_user_mean <- mean(non_users[[trait]], na.rm = TRUE)
    
    # Perform t-test
    t_test_result <- t.test(semeron_users[[trait]], non_users[[trait]])
    
    # Store results
    trait_comparison <- rbind(trait_comparison, data.frame(
      Trait = trait,
      Semeron_Users_Mean = user_mean,
      Non_Users_Mean = non_user_mean,
      Difference = user_mean - non_user_mean,
      T_Test_P_Value = t_test_result$p.value
    ))
  }
  
  # Add significance indicator
  trait_comparison$Significance <- ifelse(trait_comparison$T_Test_P_Value < 0.05, "Significant", "Not Significant")
  
  return(trait_comparison)
}

# 4. Analyze drug usage patterns
analyze_drug_usage <- function(semeron_users, non_users, drug_columns) {
  drug_usage_comparison <- data.frame(
    Drug = character(),
    Usage_Category = character(),
    Semeron_Users_Pct = numeric(),
    Non_Users_Pct = numeric(),
    Difference = numeric()
  )
  
  # Exclude Semeron itself from the comparison
  compare_drugs <- drug_columns[drug_columns != "Semer"]
  
  for (drug in compare_drugs) {
    # Calculate usage distribution
    drug_users <- table(semeron_users[[drug]]) / nrow(semeron_users) * 100
    drug_non_users <- table(non_users[[drug]]) / nrow(non_users) * 100
    
    # Compare each usage category
    for (cat in unique(c(names(drug_users), names(drug_non_users)))) {
      user_pct <- if(cat %in% names(drug_users)) drug_users[cat] else 0
      non_user_pct <- if(cat %in% names(drug_non_users)) drug_non_users[cat] else 0
      
      drug_usage_comparison <- rbind(drug_usage_comparison, data.frame(
        Drug = drug,
        Usage_Category = cat,
        Semeron_Users_Pct = user_pct,
        Non_Users_Pct = non_user_pct,
        Difference = user_pct - non_user_pct
      ))
    }
  }
  
  # Tag recent use categories
  drug_usage_comparison$Recent <- ifelse(
    drug_usage_comparison$Usage_Category %in% c("Used in Last Year", "Used in Last Month", "Used in Last Week", "Used in Last Day"),
    TRUE, FALSE
  )
  
  return(drug_usage_comparison)
}

# 5. Get Semeron usage distribution
get_semeron_usage <- function(drug_data) {
  semeron_usage <- data.frame(
    Category = names(table(drug_data$Semer)),
    Count = as.numeric(table(drug_data$Semer)),
    Percentage = as.numeric(table(drug_data$Semer)) / nrow(drug_data) * 100
  )
  
  return(semeron_usage)
}

# 6. Summarize recent drug usage
summarize_recent_usage <- function(drug_usage_comparison) {
  # Filter to only recent usage categories and summarize by drug
  recent_usage <- drug_usage_comparison %>%
    filter(Recent == TRUE) %>%
    group_by(Drug) %>%
    summarize(
      Semeron_Users_Recent = sum(Semeron_Users_Pct),
      Non_Users_Recent = sum(Non_Users_Pct),
      Difference = Semeron_Users_Recent - Non_Users_Recent
    ) %>%
    arrange(desc(abs(Difference)))
  
  return(recent_usage)
}

# 7. Visualization functions

# 7.1 Create demographic comparison plot
plot_demographics <- function(demo_comparison, top_n = 15) {
  # Get top differences
  top_demo_diff <- demo_comparison[order(-demo_comparison$AbsDiff), ][1:top_n, ]
  
  # Create the plot
  demo_plot <- ggplot(top_demo_diff, aes(x = reorder(Variable, Difference), y = Difference)) +
    geom_bar(stat = "identity", aes(fill = Difference > 0)) +
    scale_fill_manual(values = c("red", "blue"), 
                      labels = c("Higher in Non-Users", "Higher in Semeron Users"),
                      name = "") +
    coord_flip() +
    labs(title = "Top Demographic Differences Between Semeron Users and Non-Users",
         subtitle = "Percentage point differences in demographic categories",
         x = "",
         y = "Percentage Point Difference") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(demo_plot)
}

# 7.2 Create personality trait plot
plot_traits <- function(trait_comparison) {
  trait_plot <- ggplot(trait_comparison, aes(x = reorder(Trait, Difference), y = Difference)) +
    geom_bar(stat = "identity", aes(fill = Significance)) +
    scale_fill_manual(values = c("gray", "darkgreen"), 
                      name = "Statistical Significance") +
    geom_text(aes(label = sprintf("p = %.3f", T_Test_P_Value)), 
              vjust = ifelse(trait_comparison$Difference > 0, -0.5, 1.5)) +
    labs(title = "Personality Trait Differences Between Semeron Users and Non-Users",
         x = "",
         y = "Mean Difference (Semeron Users - Non-Users)") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(trait_plot)
}

# 7.3 Create drug usage plot
plot_drug_usage <- function(recent_usage) {
  drug_plot <- ggplot(recent_usage, aes(x = reorder(Drug, Difference), y = Difference)) +
    geom_bar(stat = "identity", aes(fill = Difference > 0)) +
    scale_fill_manual(values = c("red4", "blue4"), 
                      labels = c("Higher in Non-Users", "Higher in Semeron Users"),
                      name = "") +
    coord_flip() +
    labs(title = "Differences in Recent Drug Use Between Semeron Users and Non-Users",
         subtitle = "Percentage point differences in 'Used in Last Year/Month/Week/Day'",
         x = "",
         y = "Percentage Point Difference") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(drug_plot)
}

# 7.4 Create Semeron usage distribution plot
plot_semeron_distribution <- function(semeron_usage) {
  semeron_plot <- ggplot(semeron_usage, aes(x = reorder(Category, -Count), y = Percentage)) +
    geom_bar(stat = "identity", fill = "darkblue") +
    geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5) +
    labs(title = "Distribution of Semeron Usage",
         x = "",
         y = "Percentage of Respondents") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(semeron_plot)
}

# 8. Main analysis function that uses all the modular functions
analyze_semeron_reporting <- function(drug_data, drug_columns) {
  # Step 1: Separate users into groups
  groups <- separate_semeron_groups(drug_data)
  semeron_users <- groups$semeron_users
  non_users <- groups$non_users
  
  # Step 2: Analyze demographics
  demo_comparison <- analyze_demographics(semeron_users, non_users)
  
  # Step 3: Analyze personality traits
  trait_comparison <- analyze_traits(semeron_users, non_users)
  
  # Step 4: Analyze drug usage patterns
  drug_usage_comparison <- analyze_drug_usage(semeron_users, non_users, drug_columns)
  
  # Step 5: Get Semeron usage distribution
  semeron_usage <- get_semeron_usage(drug_data)
  
  # Step 6: Summarize recent drug usage
  recent_usage <- summarize_recent_usage(drug_usage_comparison)
  
  # Step 7: Create visualizations
  demo_plot <- plot_demographics(demo_comparison)
  trait_plot <- plot_traits(trait_comparison)
  drug_plot <- plot_drug_usage(recent_usage)
  semeron_plot <- plot_semeron_distribution(semeron_usage)
  
  # Return results as a list
  return(list(
    demographic_comparison = demo_comparison,
    trait_comparison = trait_comparison,
    drug_usage_comparison = drug_usage_comparison,
    recent_usage_comparison = recent_usage,
    semeron_usage = semeron_usage,
    plots = list(
      demographic_plot = demo_plot,
      trait_plot = trait_plot,
      drug_plot = drug_plot,
      semeron_plot = semeron_plot
    )
  ))
}

# Function to display all plots from the analysis
display_semeron_analysis_plots <- function(analysis_results) {
  # Create a 2x2 plot grid
  grid.arrange(
    analysis_results$plots$demographic_plot,
    analysis_results$plots$trait_plot,
    analysis_results$plots$drug_plot,
    analysis_results$plots$semeron_plot,
    ncol = 2
  )
}


# Run the analysis
semeron_analysis <- analyze_semeron_reporting(drug_data, drug_columns)
 
# Display all plots
display_semeron_analysis_plots(semeron_analysis)

# Or display individual plots
semeron_analysis$plots$demographic_plot






