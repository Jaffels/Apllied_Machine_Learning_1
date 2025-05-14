# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(janitor)
library(GGally)
library(broom)
library(knitr)
library(kableExtra)

# Load the dataset
df <- read.csv("Data/model_data.csv")

#Remove the first column index X
df_cnb <- df[,-1]

# Check the structure of the dataset
View(df)

# Create a column to flag if ever used cannabis
df_cnb <- df_cnb %>%
  mutate(
    cnb_use = if_else(Cannabis == 0, 0, 1)
  )

# Check the distribution of cannabis use
df_cnb %>% tabyl(cnb_use)

# Create a bar plot to visualize the distribution of cannabis use   
ggplot(df_cnb, aes(x = factor(cnb_use))) +
  geom_bar() +
  labs(x = "Cannabis Use (0 = No, 1 = Yes)", y = "Count") +
  theme_minimal()

# Histograms (or density plots) of each trait
df_cnb %>%
  gather(trait, score, Nscore:Cscore) %>%
  ggplot(aes(x = score)) +
    geom_histogram(bins = 30, color = "white", fill = "steelblue") +
    facet_wrap(~ trait, scales = "free", ncol = 1) +       
    labs(
      title = "Distribution of Personality Traits",
      x     = "Score",
      y     = "Count"
    ) +
    theme_minimal(base_size = 14) +                        
    theme(
      strip.text = element_text(face = "bold", size = 12), 
      plot.title = element_text(face = "bold", hjust = 0.5)
    )

# Boxplots by use status
trait_labs <- c(
  Nscore = "Neuroticism",
  Escore = "Extraversion",
  Oscore = "Openness",
  Ascore = "Agreeableness",
  Cscore = "Conscientiousness"
)
df_cnb %>%
  gather(trait, score, Nscore:Cscore) %>%
  ggplot(aes(x = factor(cnb_use), y = score)) +
    geom_boxplot() +
    facet_wrap(
      ~ trait, 
      scales = "free_y", 
      ncol = 5, 
      labeller = labeller(trait = trait_labs)) +
    scale_x_discrete(labels = c("Never", "Ever")) +
    labs(x = "Marijuana Use", y = "Trait Score",
         title = "Personality Traits by Marijuana Use")

# Correlation matrix
df_cnb %>%
  select(Nscore:Cscore, cnb_use) %>%
  ggpairs(
    columns = 1:5,
    mapping = aes(color = factor(`cnb_use`), alpha = 0.7),
    upper = list(
      continuous = wrap("cor", size = 4, digits = 2)
    ),
    lower = list(
      continuous = wrap("smooth", se = FALSE, size = 0.3)
    ),
    diag = list(
      continuous = wrap("densityDiag", alpha = 0.5)
    ),
    axisLabels = "show"
  ) +
  scale_color_brewer(
    type    = "qual",
    palette = "Set1",
    name    = "Cannabis\nUse"
  ) +
  labs(
    title    = "Pairwise Relationships & Correlations of Personality Traits",
    subtitle = "Colored by Cannabis-use indicator",
    caption  = "Note: Correlation coefficients rounded to two decimals"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position   = "bottom",
    legend.title.align= 0.5,
    plot.title        = element_text(face = "bold", size = 16),
    plot.subtitle     = element_text(size = 12),
    strip.text        = element_text(face = "bold"),
    panel.grid.minor  = element_blank()
  )

model <- glm(cnb_use ~ Nscore + Escore + Oscore + Ascore + Cscore, family = binomial, data = df_cnb)
summary(model)

cnb_model <- broom::tidy(model) %>%
  mutate(
    raw_p = p.value,
    p.value = round(p.value, 3),
    p.value = ifelse(p.value < 0.001, sprintf("%.2e", raw_p), p.value),
    OR = round(exp(estimate), 2),
    lower_CI = round(exp(estimate - 1.96 * std.error), 2),
    upper_CI = round(exp(estimate + 1.96 * std.error), 2),
        term     = recode(term,
               `(Intercept)` = "Intc.",
               Nscore        = "Neuroticism",
               Escore        = "Extraversion",
               Oscore        = "Openness",
               Ascore        = "Agreeableness",
               Cscore        = "Conscientiousness"
             )
  ) %>%
  select(term, estimate, OR, lower_CI, upper_CI, p.value)

kable(cnb_model, 
      col.names = c("Term", "Estimate", "OR", "Lower 95%", "Upper 95%", "p-value"),
      digits = c( NA, 3, 2, 2, 2, NA),
      caption = "Logistic Regression (Binomial GLM) Results") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width        = FALSE,
    position          = "center",
    font_size         = 12
  ) %>%
  row_spec(0, bold = TRUE) %>%        
  column_spec(1, width = "4cm") %>%   
  column_spec(2:5, width = "2.5cm") %>%
  column_spec(6, width = "3cm")   
 