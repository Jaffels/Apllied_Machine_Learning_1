# Drug Consumption Linear Regression Analysis
# Author: Claude
# Date: April 25, 2025

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(car)      # For VIF analysis
library(broom)    # For tidy model output
library(knitr)    # For tables
library(kableExtra) # For enhanced tables
library(gridExtra) # For arranging multiple plots

# Read the processed dataset
model_data <- read.csv("Data/model_data.csv")

# Remove the first index column if it exists
if(names(model_data)[1] == "X") {
  model_data <- model_data[,-1]
}

#---------------------------------------------------------------
# 1. Exploratory Analysis for Linear Regression
#---------------------------------------------------------------

# Function to create a custom correlation plot with drug variables
plot_drug_correlations <- function(data, drug_name) {
  # Define better variable names for plotting
  var_names <- c(
    "Nscore" = "Neuroticism",
    "Escore" = "Extraversion", 
    "Oscore" = "Openness",
    "Ascore" = "Agreeableness",
    "Cscore" = "Conscientiousness",
    "Impulsive" = "Impulsivity",
    "SS" = "Sensation Seeking"
  )
  
  # Select personality traits and the specified drug
  vars <- c("Nscore", "Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS", drug_name)
  cor_data <- data[, vars]
  
  # Calculate correlation matrix
  cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
  
  # For a more visually appealing plot, let's use ggplot2 instead of corrplot
  cor_df <- reshape2::melt(cor_matrix)
  names(cor_df) <- c("Var1", "Var2", "Correlation")
  
  # Only keep correlations with the drug
  cor_df_drug <- cor_df[cor_df$Var2 == drug_name & cor_df$Var1 != drug_name, ]
  
  # Replace variable names with more readable ones
  cor_df_drug$Var1 <- factor(
    var_names[as.character(cor_df_drug$Var1)], 
    levels = var_names[c("SS", "Impulsive", "Oscore", "Escore", "Nscore", "Cscore", "Ascore")]
  )
  
  # Create bar chart
  ggplot(cor_df_drug, aes(x = Correlation, y = Var1, fill = Correlation)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient2(low = "#E41A1C", mid = "white", high = "#377EB8", midpoint = 0) +
    geom_text(aes(label = sprintf("%.2f", round(Correlation, 2))), 
              hjust = ifelse(cor_df_drug$Correlation < 0, 1.1, -0.1),
              color = "black", size = 3.5) +
    labs(title = paste("Personality Traits Correlated with", drug_name, "Use"),
         x = "Correlation Coefficient",
         y = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 10)
    ) +
    xlim(min(cor_df_drug$Correlation) - 0.1, max(cor_df_drug$Correlation) + 0.1)
}

# Create clean names for drugs to analyze
drug_names <- c("Cannabis", "Alcohol", "Nicotine", "Coke", "Ecstasy")

# Factor importance visualization function
plot_factor_importance <- function(model, title) {
  # Extract model coefficients
  coefs <- summary(model)$coefficients
  
  # Filter out intercept and create data frame for plotting
  coef_df <- data.frame(
    Variable = rownames(coefs)[-1],  # Exclude intercept
    Estimate = coefs[-1, "Estimate"],
    StdError = coefs[-1, "Std. Error"],
    PValue = coefs[-1, "Pr(>|t|)"]
  )
  
  # Add significance markers
  coef_df$Significant <- ifelse(coef_df$PValue < 0.05, "Yes", "No")
  
  # Sort by absolute value of estimate
  coef_df <- coef_df[order(abs(coef_df$Estimate), decreasing = TRUE), ]
  
  # Keep only top 15 predictors for visualization clarity
  if(nrow(coef_df) > 15) {
    coef_df <- coef_df[1:15, ]
  }
  
  # Reorder factor levels for plotting
  coef_df$Variable <- factor(coef_df$Variable, levels = coef_df$Variable)
  
  # Plot
  ggplot(coef_df, aes(x = Estimate, y = Variable, color = Significant)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = Estimate - 1.96 * StdError, 
                       xmax = Estimate + 1.96 * StdError), 
                   height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray") +
    scale_color_manual(values = c("Yes" = "#1F78B4", "No" = "#999999")) +
    theme_minimal() +
    labs(title = title,
         subtitle = "Estimated coefficients with 95% confidence intervals",
         x = "Coefficient Estimate",
         y = "",
         color = "Statistically\nSignificant") +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 10, color = "darkgray"),
          legend.position = "top")
}

# Function to create an even more visually appealing coefficient plot
plot_factor_importance_enhanced <- function(model, title, color_scheme = "viridis") {
  # Extract model coefficients
  coefs <- summary(model)$coefficients
  
  # Filter out intercept and create data frame for plotting
  coef_df <- data.frame(
    Variable = rownames(coefs)[-1],  # Exclude intercept
    Estimate = coefs[-1, "Estimate"],
    StdError = coefs[-1, "Std. Error"],
    PValue = coefs[-1, "Pr(>|t|)"]
  )
  
  # Add significance markers and categories
  coef_df$Significance <- ifelse(coef_df$PValue < 0.001, "p < 0.001", 
                                 ifelse(coef_df$PValue < 0.01, "p < 0.01",
                                        ifelse(coef_df$PValue < 0.05, "p < 0.05", "Not significant")))
  
  # Sort by absolute value of estimate
  coef_df <- coef_df[order(abs(coef_df$Estimate), decreasing = TRUE), ]
  
  # Keep only top 15 predictors for visualization clarity
  if(nrow(coef_df) > 15) {
    coef_df <- coef_df[1:15, ]
  }
  
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
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray", size = 0.7) +
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

#---------------------------------------------------------------
# 2. Linear Regression Modeling
#---------------------------------------------------------------

# Function to build and evaluate linear regression model
run_drug_regression <- function(data, drug_name) {
  # Formula creation - all features, but handle multicollinearity in categorical variables
  
  # For country variables, exclude one as reference (USA)
  country_vars <- grep("Country", names(data), value = TRUE)
  country_vars <- country_vars[country_vars != "CountryUSA"] # Use USA as reference
  
  # For ethnicity variables, exclude one as reference (White)
  ethnicity_vars <- grep("Ethnicity", names(data), value = TRUE)
  ethnicity_vars <- ethnicity_vars[ethnicity_vars != "EthnicityWhite"] # Use White as reference
  
  # Create formula with modified variables to avoid perfect multicollinearity
  formula_str <- paste(drug_name, "~ Age + Gender + Education + Nscore + Escore + Oscore + Ascore + Cscore + Impulsive + SS + ", 
                       paste(c(country_vars, ethnicity_vars), collapse = " + "))
  
  formula <- as.formula(formula_str)
  
  # Fit model
  model <- lm(formula, data = data)
  
  # Check VIF for multicollinearity
  # First check if the model has aliased coefficients
  alias_check <- alias(model)
  has_aliased <- length(alias_check$Complete) > 0
  
  # Only run VIF if no aliased coefficients
  if(!has_aliased) {
    vif_values <- vif(model)
    high_vif <- vif_values[vif_values > 5]
  } else {
    # If there are aliased coefficients, we can't calculate VIF
    vif_values <- "Aliased coefficients detected"
    high_vif <- "Aliased coefficients detected"
    
    # Get the names of the aliased coefficients
    aliased_names <- rownames(alias_check$Complete)
    cat("Aliased coefficients detected in model for", drug_name, ":", paste(aliased_names, collapse=", "), "\n")
  }
  
  # Optional: Step-wise selection for feature selection
  # step_model <- step(model, direction = "both")
  
  # Return model and diagnostics
  return(list(
    model = model,
    summary = summary(model),
    vif = vif_values,
    high_vif = high_vif
  ))
}

# Create a results container
results_list <- list()

# Run regression for selected drugs
for (drug in drug_names) {
  # Check if the drug exists in the dataset
  if (drug %in% names(model_data)) {
    cat("Running regression model for", drug, "\n")
    results_list[[drug]] <- run_drug_regression(model_data, drug)
  } else {
    cat("Warning: Drug", drug, "not found in dataset\n")
  }
}

#---------------------------------------------------------------
# 3. Model Comparison and Visualization
#---------------------------------------------------------------

# Function to create a summary table of model performance
create_model_summary_table <- function(results_list) {
  # Initialize empty data frame
  summary_df <- data.frame(
    Drug = character(),
    R_squared = numeric(),
    Adj_R_squared = numeric(),
    F_statistic = numeric(),
    P_value = numeric(),
    Top_positive_predictor = character(),
    Top_negative_predictor = character(),
    stringsAsFactors = FALSE
  )
  
  # Fill with results
  for (drug in names(results_list)) {
    model_summary <- results_list[[drug]]$summary
    
    # Get coefficients
    coefs <- model_summary$coefficients
    
    # Find top predictors (excluding intercept)
    coef_df <- data.frame(
      Variable = rownames(coefs)[-1],
      Estimate = coefs[-1, "Estimate"],
      P_value = coefs[-1, "Pr(>|t|)"]
    )
    
    # Get significant predictors only
    sig_coefs <- coef_df[coef_df$P_value < 0.05, ]
    
    if(nrow(sig_coefs) > 0) {
      # Get top positive and negative predictors
      top_pos <- sig_coefs[which.max(sig_coefs$Estimate), "Variable"]
      top_neg <- sig_coefs[which.min(sig_coefs$Estimate), "Variable"]
    } else {
      top_pos <- "None"
      top_neg <- "None"
    }
    
    # Add to summary
    summary_df <- rbind(summary_df, data.frame(
      Drug = drug,
      R_squared = model_summary$r.squared,
      Adj_R_squared = model_summary$adj.r.squared,
      F_statistic = model_summary$fstatistic[1],
      P_value = pf(model_summary$fstatistic[1], 
                   model_summary$fstatistic[2], 
                   model_summary$fstatistic[3], 
                   lower.tail = FALSE),
      Top_positive_predictor = top_pos,
      Top_negative_predictor = top_neg,
      stringsAsFactors = FALSE
    ))
  }
  
  return(summary_df)
}

# Create and format the summary table
model_summary_table <- create_model_summary_table(results_list)

# Format the table for nice output
formatted_summary <- model_summary_table %>%
  mutate(
    R_squared = round(R_squared, 3),
    Adj_R_squared = round(Adj_R_squared, 3),
    F_statistic = round(F_statistic, 2),
    P_value = ifelse(P_value < 0.001, "<0.001", round(P_value, 3))
  )

#---------------------------------------------------------------
# 4. Diagnostic Plots
#---------------------------------------------------------------

# Function to create diagnostic plots with enhanced aesthetics
create_diagnostic_plots <- function(model, title, color_scheme = "blue") {
  # Extract residuals data
  model_data <- augment(model)
  
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
      panel.border = element_rect(color = "gray80", fill = NA, size = 0.5)
    )
  
  # 1. Residuals vs Fitted with improved aesthetics
  p1 <- ggplot(model_data, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.6, color = point_color, size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = reference_color, size = 0.7) +
    geom_smooth(se = TRUE, color = line_color, fill = alpha(line_color, 0.2), method = "loess") +
    labs(title = "Residuals vs Fitted",
         subtitle = "Should show random scatter around the zero line",
         x = "Fitted values",
         y = "Residuals") +
    diagnostic_theme
  
  # 2. Normal Q-Q plot with improved aesthetics
  p2 <- ggplot(model_data, aes(sample = .resid)) +
    stat_qq(color = point_color, size = 1.5, alpha = 0.6) +
    stat_qq_line(color = reference_color, size = 0.7) +
    labs(title = "Normal Q-Q Plot",
         subtitle = "Points should follow the diagonal line",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    diagnostic_theme
  
  # 3. Scale-Location plot with improved aesthetics
  p3 <- ggplot(model_data, aes(x = .fitted, y = sqrt(abs(.resid)))) +
    geom_point(alpha = 0.6, color = point_color, size = 1.5) +
    geom_smooth(se = TRUE, color = line_color, fill = alpha(line_color, 0.2), method = "loess") +
    labs(title = "Scale-Location",
         subtitle = "Should show homogeneous variance",
         x = "Fitted values",
         y = "√|Standardized Residuals|") +
    diagnostic_theme
  
  # 4. Residuals vs Leverage with improved aesthetics
  # Add Cook's distance contour lines
  p4 <- ggplot(model_data, aes(x = .hat, y = .resid)) +
    geom_point(alpha = 0.6, color = point_color, size = 1.5) +
    geom_smooth(se = TRUE, color = line_color, fill = alpha(line_color, 0.2), method = "loess") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    # Add Cook's distance contours
    stat_contour(aes(z = .cooksd), breaks = c(0.5, 1), color = "red", linetype = "dashed") +
    labs(title = "Residuals vs Leverage",
         subtitle = "Identifies influential cases",
         x = "Leverage",
         y = "Standardized Residuals") +
    diagnostic_theme
  
  # Combine plots with better layout
  title_grob <- grid::textGrob(
    title, 
    gp = grid::gpar(fontsize = 16, fontface = "bold"),
    just = "center"
  )
  subtitle_grob <- grid::textGrob(
    "Diagnostic Plots for Linear Regression Model", 
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

#---------------------------------------------------------------
# 5. Generate Beautiful Output
#---------------------------------------------------------------

# Create a publication-quality regression table using kable instead of stargazer
# for more reliable operation

# Function to create clean model coefficient table
create_model_coef_table <- function(model_list, drug_names) {
  # Extract key coefficients from each model
  key_vars <- c("Age", "Gender", "Education", "Nscore", "Escore", 
                "Oscore", "Ascore", "Cscore", "Impulsive", "SS")
  
  # Create data frame for results
  result_df <- data.frame(
    Variable = c("(Intercept)", key_vars),
    stringsAsFactors = FALSE
  )
  
  # Add each model's coefficients and significance
  for (drug in drug_names) {
    if (drug %in% names(model_list)) {
      model <- model_list[[drug]]$model
      coefs <- summary(model)$coefficients
      
      # Extract coefficients and p-values
      drug_coefs <- numeric(length(result_df$Variable))
      drug_p <- numeric(length(result_df$Variable))
      
      for (i in 1:length(result_df$Variable)) {
        var_name <- result_df$Variable[i]
        if (var_name %in% rownames(coefs)) {
          drug_coefs[i] <- coefs[var_name, "Estimate"]
          drug_p[i] <- coefs[var_name, "Pr(>|t|)"]
        } else {
          drug_coefs[i] <- NA
          drug_p[i] <- NA
        }
      }
      
      # Add significance stars
      drug_sig <- ifelse(drug_p < 0.001, "***", 
                         ifelse(drug_p < 0.01, "**", 
                                ifelse(drug_p < 0.05, "*", "")))
      
      # Format coefficients with significance stars
      drug_coef_text <- ifelse(!is.na(drug_coefs), 
                               paste0(sprintf("%.3f", round(drug_coefs, 3)), drug_sig), 
                               "")
      
      # Add to result dataframe
      result_df[[drug]] <- drug_coef_text
    }
  }
  
  # Add model metrics
  metrics_rows <- data.frame(
    Variable = c("N", "R²", "Adjusted R²", "F-statistic"),
    stringsAsFactors = FALSE
  )
  
  for (drug in drug_names) {
    if (drug %in% names(model_list)) {
      model_summary <- summary(model_list[[drug]]$model)
      n <- length(model_summary$residuals)
      r2 <- model_summary$r.squared
      adj_r2 <- model_summary$adj.r.squared
      f_stat <- model_summary$fstatistic[1]
      
      metrics_rows[[drug]] <- c(
        as.character(n),
        sprintf("%.3f", round(r2, 3)),
        sprintf("%.3f", round(adj_r2, 3)),
        sprintf("%.3f", round(f_stat, 3))
      )
    }
  }
  
  # Combine results and metrics
  final_df <- rbind(result_df, metrics_rows)
  
  # Clean variable names for display
  var_display_names <- c(
    "(Intercept)" = "Intercept",
    "Age" = "Age",
    "Gender" = "Gender (Male=1)",
    "Education" = "Education Level",
    "Nscore" = "Neuroticism",
    "Escore" = "Extraversion",
    "Oscore" = "Openness",
    "Ascore" = "Agreeableness",
    "Cscore" = "Conscientiousness",
    "Impulsive" = "Impulsivity",
    "SS" = "Sensation Seeking",
    "N" = "N",
    "R²" = "R²",
    "Adjusted R²" = "Adjusted R²",
    "F-statistic" = "F-statistic"
  )
  
  final_df$Variable <- var_display_names[final_df$Variable]
  
  return(final_df)
}

# Create the comparison table for the main drugs
drug_names_for_table <- names(results_list)
if (length(drug_names_for_table) > 0) {
  model_comparison_table <- create_model_coef_table(
    results_list, 
    drug_names_for_table
  )
  
  # Display the table with kable for better formatting
  kable(model_comparison_table, 
        caption = "Linear Regression Models for Drug Usage (Usage Level 0-6)",
        align = c('l', rep('r', ncol(model_comparison_table) - 1))) %>%
    kable_styling(bootstrap_options = c("striped", "hover"), 
                  full_width = FALSE) %>%
    add_header_above(c(" " = 1, "Drug Models" = ncol(model_comparison_table) - 1)) %>%
    footnote(
      symbol = c("* p<0.05; ** p<0.01; *** p<0.001"),
      symbol_title = "Significance levels:",
      footnote_as_chunk = TRUE
    )
} else {
  cat("No valid drug models to display in table\n")
}

# Properly define the individual models for plotting
if ("Cannabis" %in% names(results_list)) {
  cannabis_model <- results_list[["Cannabis"]]$model
  cannabis_coef_plot <- plot_factor_importance_enhanced(cannabis_model, "Predictors of Cannabis Usage")
  print(cannabis_coef_plot)
}

if ("Alcohol" %in% names(results_list)) {
  alcohol_model <- results_list[["Alcohol"]]$model
  alcohol_coef_plot <- plot_factor_importance_enhanced(alcohol_model, "Predictors of Alcohol Usage")
  print(alcohol_coef_plot)
}

if ("Nicotine" %in% names(results_list)) {
  nicotine_model <- results_list[["Nicotine"]]$model
  nicotine_coef_plot <- plot_factor_importance_enhanced(nicotine_model, "Predictors of Nicotine Usage")
  print(nicotine_coef_plot)
}

# Create a table of the most significant predictors for each drug
get_significant_predictors <- function(model, drug_name) {
  # Extract model coefficients
  coefs <- summary(model)$coefficients
  
  # Filter to significant predictors (p < 0.05)
  sig_coefs <- coefs[coefs[, "Pr(>|t|)"] < 0.05, ]
  
  # Skip intercept
  sig_coefs <- sig_coefs[rownames(sig_coefs) != "(Intercept)", ]
  
  # Create data frame for results
  if(nrow(sig_coefs) > 0) {
    pred_df <- data.frame(
      Drug = drug_name,
      Predictor = rownames(sig_coefs),
      Coefficient = sig_coefs[, "Estimate"],
      StdError = sig_coefs[, "Std. Error"],
      P_Value = sig_coefs[, "Pr(>|t|)"],
      stringsAsFactors = FALSE
    )
    
    # Order by absolute coefficient value
    pred_df <- pred_df[order(abs(pred_df$Coefficient), decreasing = TRUE), ]
    
    return(pred_df)
  } else {
    return(NULL)
  }
}

# Combine significant predictors for all drugs
all_predictors_list <- lapply(names(results_list), function(drug) {
  get_significant_predictors(results_list[[drug]]$model, drug)
})

# Filter out NULL results and combine
non_null_predictors <- all_predictors_list[!sapply(all_predictors_list, is.null)]
if (length(non_null_predictors) > 0) {
  all_predictors <- do.call(rbind, non_null_predictors)
  
  # Display the top predictors table
  if(!is.null(all_predictors) && nrow(all_predictors) > 0) {
    # Format the data for nicer display
    top_predictors_table <- all_predictors %>%
      mutate(
        Coefficient = round(Coefficient, 3),
        StdError = round(StdError, 3),
        P_Value = ifelse(P_Value < 0.001, "<0.001", round(P_Value, 3))
      ) %>%
      arrange(Drug, desc(abs(Coefficient)))
    
    # Print table
    kable(top_predictors_table, caption = "Significant Predictors of Drug Usage") %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
  }
}

# If we have diagnostic plots for Cannabis model
if (exists("cannabis_model") && !is.null(cannabis_model)) {
  cannabis_diagnostics <- create_diagnostic_plots(cannabis_model, "Cannabis Usage Model Diagnostics")
  print(cannabis_diagnostics)
}

#---------------------------------------------------------------
# 6. Print summary findings to console
#---------------------------------------------------------------

# Print summary findings
cat("\n======= SUMMARY OF DRUG CONSUMPTION ANALYSIS =======\n")
cat("Linear regression models were fit for", length(results_list), "drugs\n")

if (length(results_list) > 0) {
  best_model <- model_summary_table$Drug[which.max(model_summary_table$Adj_R_squared)]
  best_r2 <- max(model_summary_table$Adj_R_squared, na.rm = TRUE)
  
  cat("The best-fitting model was for", best_model, 
      "with Adjusted R² =", round(best_r2, 3), "\n")
  
  # Print the key predictors across drugs
  if (exists("all_predictors") && !is.null(all_predictors) && nrow(all_predictors) > 0) {
    cat("\nKey personality traits predicting drug use:\n")
    personality_traits <- c("Nscore", "Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS")
    for(trait in personality_traits) {
      # Count how many drug models have this trait as significant
      trait_count <- sum(grepl(trait, all_predictors$Predictor))
      if(trait_count > 0) {
        cat("- ", trait, ": significant predictor for", trait_count, "drugs\n")
      }
    }
  } else {
    cat("\nNo significant predictors found across drugs\n")
  }
} else {
  cat("No valid drug models were created\n")
}