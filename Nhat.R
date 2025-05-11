# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(janitor)
library(GGally)

# Load the dataset
df <- read.csv("Data/model_data.csv")

#Remove the first column index X
df <- df[,-1]

# Check the structure of the dataset
View(df)

# Create a column to flag if ever used cannabis
df <- df %>%
  mutate(
    cnb_use = if_else(Cannabis == 0, 0, 1)
  )

# Check the distribution of cannabis use
df %>% tabyl(cnb_use)

# Create a bar plot to visualize the distribution of cannabis use   
ggplot(df, aes(x = factor(cnb_use))) +
  geom_bar() +
  labs(x = "Cannabis Use (0 = No, 1 = Yes)", y = "Count") +
  theme_minimal()

# Histograms (or density plots) of each trait
df %>%
  gather(trait, score, Nscore:Cscore) %>%
  ggplot(aes(x = score)) +
    geom_histogram(bins = 30, color = "white", fill = "steelblue") +
    facet_wrap(~ trait, scales = "free", ncol = 1) +       # <- 3 columns, 2 rows
    labs(
      title = "Distribution of Personality Traits",
      x     = "Score",
      y     = "Count"
    ) +
    theme_minimal(base_size = 14) +                        # <- larger text
    theme(
      strip.text = element_text(face = "bold", size = 12), # facet labels
      plot.title = element_text(face = "bold", hjust = 0.5)
    )

# Boxplots by use status
df %>%
  gather(trait, score, Nscore:Cscore) %>%
  ggplot(aes(x = factor(cnb_use), y = score)) +
    geom_boxplot() +
    facet_wrap(~ trait, scales = "free_y", ncol = 5) +
    scale_x_discrete(labels = c("Never", "Ever")) +
    labs(x = "Marijuana Use", y = "Trait Score",
         title = "Personality Traits by Marijuana Use")

# Correlation matrix
df %>%
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

model <- glm(cnb_use ~ Nscore + Escore + Oscore + Ascore + Cscore, family = binomial, data = df)
summary(model)













# Sensation Seeking and Ecstasy (MDMA) Use
# RQ: Does higher sensation-seeking (ImpSS) significantly increase the probability of using ecstasy in the past year?
# H0: Higher sensation-seeking (ImpSS) does not significantly increase the probability of using ecstasy in the past year.
# H1: Higher sensation-seeking (ImpSS) significantly increases the probability of using ecstasy in the past year.
# Convert the target variable to a factor
df$EcstasyUse <- as.factor(df$EcstasyUse)

# Fit a logistic regression model
model <- glm(EcstasyUse ~ ImpSS, data = df, family = binomial)

# Summarize the model
summary(model)

# Predict probabilities
df$PredictedProb <- predict(model, type = "response")

# View the updated dataframe
View(df)