# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(janitor)
library(GGally)
library(broom)
library(knitr)
library(kableExtra)
library(mgcv)


# Load the dataset
df <- read.csv("Data/model_data.csv")

#Remove the first column index X
df_cnb <- df[,-1]

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
 

# How does education level modify the age-use curve for past-year cannabis use?
# Understand if higher education delays initiation or reduces escalation.

# Load cleaned dataset
df_cl <- read.csv("Data/cleaned.csv")

# Remove the X column
if("X" %in% names(df_cl)) df_cl <- df_cl %>% select(-X)

# Classify past-year cannabis use: if use cannabis in the past year, then 1, otherwise 0 
past_year_labels <- c(
  "Used in Last Year",
  "Used in Last Month",
  "Used in Last Week",
  "Used in Last Day"
)

df_cnb_gam <- df_cl %>%
  mutate(
    cnb_use = case_when(
      Cannabis %in% past_year_labels           ~ "Yes",
      Cannabis %in% c("Never Used",
                      "Used over a Decade Ago",
                      "Used in Last Decade")  ~ "No",
      TRUE                                     ~ NA_character_
    ),
    cnb_use = factor(cnb_use, levels = c("No","Yes"))
  )
#------------------------------------------------------
trait_labs_cnb <- c(
  Nscore = "Neuroticism",
  Escore = "Extraversion",
  Oscore = "Openness",
  Ascore = "Agreeableness",
  Cscore = "Conscientiousness",
  Impulsive = "Impulsivity (BIS-11)",
  SS        = "Sensation-Seeking (ImpSS)"
)

#Correlation matrix across all numeric traits
num_vars <- df_cnb_gam %>%
  select(names(trait_labs_cnb))  # selects Nscore, Escore, ..., SS

cor_mat <- cor(num_vars, use = "pairwise.complete.obs")
library(reshape2)
cor_df <- melt(cor_mat, varnames = c("Trait1","Trait2"), value.name = "r")

ggplot(cor_df, aes(x = Trait1, y = Trait2, fill = r)) +
  geom_tile() +
  scale_fill_gradient2(
    low      = "navy",
    mid      = "white",
    high     = "firebrick",
    midpoint = 0,
    name     = "r"
  ) +
  scale_x_discrete(labels = trait_labs_cnb) +
  scale_y_discrete(labels = trait_labs_cnb) +
  labs(
    title = "Correlation Matrix: Big-Five, BIS-11 & Sensation-Seeking",
    x     = NULL,
    y     = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title  = element_text(face = "bold", hjust = 0.5)
  )

# The first thing that jumps out is a “risk-taking” cluster: people who score high on impulsivity also tend to crave sensation, and both of those traits go hand-in-hand with being open to new experiences—but often at the expense of self-discipline and cooperativeness.

# Next, there’s a “social” strand tying together extraversion and openness. Those who are outgoing also lean toward curiosity and, to a lesser extent, kindness and orderliness. Neuroticism, however, sits apart—it’s negatively linked to both sociability and conscientiousness, highlighting its role as an emotional restlessness factor.

# Finally, agreeableness and conscientiousness form a tight-knit “rule-follower” duo: cooperative, organized individuals who tend to steer clear of impulsive or thrill-seeking behaviors. Altogether, your seven measures really seem to fall into three natural themes—risk appetite, social engagement, and self-control—with anxiety (neuroticism) riding its own roller-coaster.

#-------------------------------------------------------

# Age dstribution plot of the respondents
ggplot(df_cnb_gam, aes(x = Age)) +
  geom_bar(stat = "count", 
           fill = "steelblue", 
           alpha = 0.7) +
  labs(title = "Age distribution of respondents",
       x     = "Age",
       y     = "Count") +
  theme_minimal()

#-------------------------------------------------------
# Education level distribution plot of the respondents
# Summarise into a data frame
edu_summary <- df_cnb_gam %>%
  group_by(Education) %>%
  summarise(
    n    = n(),
    prev = mean(cnb_use == "Yes") * 100
  ) %>%
  arrange(prev) %>%            
  mutate(
    prev  = round(prev, 1),
    label = paste0(n, " (", prev, "%)")
  )

#Show the same table with kable
edu_summary %>%
  select(Education, n, prev) %>%
  kable(
    col.names = c("Education Level", "Count", "Past-Year Use (%)"),
    caption   = "Cannabis Use Prevalence by Education Level",
    align     = c("l","r","r"),
    digits    = c(0,0,1)
  ) %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"))

#Plot a horizontal bar chart
ggplot(edu_summary, aes(x = prev, y = reorder(Education, prev))) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_text(aes(label = label), hjust = -0.05, size = 3) +
  scale_x_continuous(
    expand = expansion(mult = c(0, .1))
  ) +
  labs(
    x       = "Past-Year Cannabis Use (%)",
    y       = "Education Level",
    title   = "Prevalence of Past-Year Cannabis Use\nby Education Level",
    caption = "Counts in parentheses"
  ) +
  theme_minimal(base_size = 12)

#It’s striking to see how education seems to go hand-in-hand with cannabis habits: people who started college but didn’t finish top the chart at about 80% past-year use, while those who walked away before age 18 still hover around two-thirds. But as you climb the credential ladder—bachelor’s, professional diplomas, then master’s and doctorates—the rate steadily falls into the 30–45% range. Sure, some of that drop comes simply because graduate students tend to be older, but even among older adults you see lower use in the highest-degree groups. That makes you wonder: does the discipline and structure of finishing a degree actually delay trying cannabis or keep use from ramping up? A GAM that fits its own smooth age-use curve for each education level will help us untangle whether a college diploma really pushes that peak of use later and tames its rise, or if it’s mostly just reflecting who’s in which age bracket.

# define your age‐bins (adjust to your exact labels!)
age_levels <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")

df_cnb_gam <- df_cnb_gam %>%
  mutate(
    age_cat = factor(Age, levels = age_levels, ordered = TRUE),
    # numeric midpoint (as rough continuous proxy)
    age_mid = case_when(
      age_cat == "18-24" ~ 21,
      age_cat == "25-34" ~ 29.5,
      age_cat == "35-44" ~ 39.5,
      age_cat == "45-54" ~ 49.5,
      age_cat == "55-64" ~ 59.5,
      age_cat == "65+"   ~ 70,
      TRUE ~ NA_real_
    )
  )

df_cnb_gam %>%
  group_by(age_cat) %>%
  summarise(pct_use = mean(cnb_use == "Yes") * 100,
            n = n()) %>%
  ggplot(aes(x = age_cat, y = pct_use, group = 1)) +
    geom_point(aes(size = n)) +
    geom_line() +
    labs(x = "Age group", y = "Past-year cannabis use (%)",
         title = "Overall age–use curve")

# The overall age–use curve you’ve plotted shows a clear, monotonic decline in past-year cannabis use as people get older, with a few nuances worth calling out. In your sample, nearly nine in ten 18–24-year-olds report using cannabis in the past year—the highest prevalence of any age group—then it falls to around 50% in the 25–34 cohort and down to roughly 30% by age 35–44. By middle age (45–54) the prevalence has dropped further to about 25%. Interestingly, there’s a small uptick to just under 30% among 55–64-year-olds, which may hint at a returning or “late adopter” subgroup in that bracket, before plummeting to roughly 15% in those 65 and over. The size of each point (n) reminds us that the youngest and middle-aged bins are well populated—so these estimates are fairly precise—whereas the oldest bin is smaller, making that steep drop-off less certain. Overall, this pattern is consistent with both an age effect (younger adults are more likely to consume) and potential cohort or survivorship effects, and it sets a strong baseline for modeling how education might flatten or shift this curve.


#-------------------------------------------------------
# fitting GAM model

View(df)

View(df_cnb)

df_cnb <- df_cnb %>%
  mutate(
    cnb_past_year = if_else(Cannabis >= 3, 1, 0)
  )

gam_mod <- gam(
  cnb_past_year ~ Education +
    s(Age, by = Education, k = 6),
  family = binomial(link = "logit"),
  data = df_cnb,
  method = "REML"
)
# 5.2 Check model diagnostics
gam.check(gam_mod)

# 5.3 Summary of smooth terms
summary(gam_mod)
