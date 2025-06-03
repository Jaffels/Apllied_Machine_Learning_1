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
# Check model diagnostics
gam.check(gam_mod)

# Summary of smooth terms
summary(gam_mod)

ggplot(df_cnb, aes(x = Education,
                   y = cnb_past_year,
                   group = 1)) +          # ensure a single group
  geom_point(alpha = 0.5) +
  stat_smooth(
    method      = "gam",
    formula     = y ~ s(x, k = 4),     # reduce k to ≤ your # of unique education
    method.args = list(family = binomial),  # or family = binomial for logistic
    se          = TRUE
  ) +
  labs(
    title = "Past-Year Cannabis Use vs. Education (GAM, k = 9)",
    x     = "Education",
    y     = "Past-Year Cannabis Use (0/1)"
  ) +
  theme_minimal(base_size = 14)

# This GAM‐derived curve describes how the probability of past-year cannabis use (vertical axis) changes as education rises from level 1 (“Not Provided”) through level 10 (“Doctorate”).

# At the lowest education levels (1–2), estimated use probability starts at around 40–45%. As education levels switch into level 4 - 6 (left school at 17, 18 and those studying in college/university respectively), the probability climbs steadily, reaching a peak near 80% at level 6. Beyond that peak, the probability falls off sharply—by the professional certificate and bachelor’s levels (7–8) it has dropped to roughly 50–60%, and by master’s level (9) it’s down near 30–35%. Finally, the curve flattens out (and even nudges upward a bit) at the doctorate level (10), but the wide confidence ribbon there indicates greater uncertainty due to sparse observations.

# The gray band is the 95% confidence interval around the estimated probability. It is narrowest in the middle education bands (levels 3–7), where most of your data lie—so those estimates are quite precise. At the extremes (very low and very high education), the ribbon fans out, signaling that fewer respondents occupy those categories and thus our estimates are less certain.

# Taken together, this non-linear “hill-shaped” relationship shows that cannabis use probability does not simply rise or fall with education. Instead, it increases sharply through those that left school at 17, 18 and college students reflecting experimentation during teenage-age/college students and then declines among individuals with higher degrees, suggesting that the highest educational attainments are associated with lower recent use.

#--------------------------------------------------------
ggplot(df_cnb, aes(x = Age,
                   y = cnb_past_year,
                   group = 1)) +          # ensure a single group
  geom_point(alpha = 0.5) +
  stat_smooth(
    method      = "gam",
    formula     = y ~ s(x, k = 3),     # reduce k to ≤ your # of Age
    method.args = list(family = binomial),  # or family = binomial for logistic
    se          = TRUE
  ) +
  labs(
    title = "Past-Year Cannabis Use vs. Age (GAM, k = 3)",
    x     = "Age",
    y     = "Past-Year Cannabis Use (0/1)"
  ) +
  theme_minimal(base_size = 14)

# The GAM‐smoothed curve reveals a clear, non‐linear decline in the probability of past‐year cannabis use as people age. At the youngest age category (18–24), use is highest—around 80–85%. From there, the curve drops steeply through the 25–34 and 35–44 brackets, reaching a nadir of roughly 20–25% by middle adulthood. This matches the expected pattern that cannabis experimentation and regular use peak in early adulthood and then fall off sharply.

# Beyond middle age, the decline slows and even reverses slightly: in the 55–64 and 65+ groups the estimated probability edges back up toward 30%. The widening gray confidence band in those older bins reflects smaller sample sizes and greater uncertainty, but the gentle uptick suggests that a non‐negligible minority of older adults continue to report recent use.

# Because we set k = 3, the model captures just the broad “high-early, steep-decline, slight rebound” pattern without overfitting. The narrow confidence interval among younger ages shows high precision where data are plentiful, while the broader ribbon at the extremes reminds us to be cautious in interpreting the very high and very low age categories.

View(df_cnb)

df_cnb %>%
  distinct(Age)

education_levels <- c(
  "Not Provided",
  "Left school before 16",
  "Left school at 16",
  "Left school at 17",
  "Left school at 18",
  "College/University student",
  "Professional certificate/diploma",
  "University degree",
  "Masters degree",
  "Doctorate degree"
)
df_cnb <- df_cnb %>%
  mutate(
    Education = factor(
      Education,
      levels = 1:10,
      labels = education_levels
    )
  )
ggplot(df_cnb, 
       aes(x = Age, 
           y = cnb_past_year)) +   
  geom_point(alpha = 0.3) +
  stat_smooth(
    method      = "gam",
    formula     = y ~ s(x, k = 3),
    method.args = list(family = binomial),
    se          = TRUE
  ) +
  facet_wrap(~ Education, ncol = 3) +
  labs(
    title = "Past-Year Cannabis Use vs. Age, by Education Level",
    x     = "Age",
    y     = "Probability of Past-Year Use"
  ) +
  theme_minimal(base_size = 12)

# The “less‐educated” group (e.g. “Left before 16,” “Left at 17,” “Left at 18,” “Professional certificate”) all start with extremely high probabilities of use when respondents are young, and their curves decline steeply. By midlife, those groups still often have somewhat higher past‐year use than the more‐educated strata. Whereas, the highest‐education respondent group (“University degree,” “Masters,” “Doctorate”) start at a lower baseline in the youngest age bracket, decline more gradually, and by the oldest ages are clustered down near 10–25%. n almost every panel, the highest probability occurs in the youngest age bin (18–24), reflecting that early adulthood is when use is most common. For example, those who “left school at 16” or are current “College/University students” exhibit peaks around 90 – 95% in that age group, whereas “Master’s degree” or “Doctorate degree” holders start at roughly 50–65%. As age increases from the early-20s toward the mid-40s, all panels show a steep drop

# The one outlier in shape is “College/University student.” That group has a very high probability at the youngest (freshman/first‐year) ages, dips in the middle (around 35-40), then rebounds at older ages. Almost every other “education” stratum shows a decline.

# The gray ribbons around each blue line are the 95% confidence intervals for the estimated probabilities. Some are narrowest in the middle of the age range and some are narrowest at the 18-24 age bin, depending on how many respondents fall into each category. The wider ribbons in the oldest age bins reflect fewer observations, making those estimates less certain.

# Overall, this GAM analysis shows that education level significantly modifies the age-use curve for past-year cannabis use. Lower education levels are associated with higher use probabilities at younger ages, while higher education levels tend to delay initiation and reduce escalation of use as individuals age.


set.seed(123)
gam.1 <- gam(
  cnb_past_year ~ 
    Education + 
    s(Age, by = Education, k = 5),
  family = binomial(link = "logit"),
  data   = df_cnb,
)

summary(gam.1)


# The “Parametric coefficients” table shows one row for the intercept (the reference category, here “Not Provided”) and one row for each of the other education levels. The intercept row can be seen as “the starting probability of past‐year use for the ‘Not Provided’ group”, and other row tells how much higher or lower that starting probability is for each education level compared to “Not Provided.”

# (Intercept) = 0.3230 (p = 0.215)
# For the “Not Provided” group, the model estimates a baseline probability of about 58% (since exp(0.3230)/(1 + exp(0.3230)) = 0.58005). p = 0.215 is not significant.

# Left school before 16: +1.289 (p = 0.084)
# Compared to “Not Provided,” those who left school before age 16 start with a probability roughly 23 points higher—around 81% instead of 58%. The p‐value of 0.084 is just above the usual threshold of 0.05, so this is a somewhat weak signal. There is some indication that early dropouts have a higher starting chance of past‐year use, but it isn’t quite strong enough to be certain.

# Left school at 17: +0.526 (p = 0.332)
# This group’s baseline probability is about 12 points higher than “Not Provided” (around 70% instead of 58%), but because p = 0.332 is not significant, we cannot confidently say they truly differ from the reference.

# Left school at 18: +0.028 (p = 0.941)
# Essentially no difference from “Not Provided” (only a 1–2 point bump to around 59%), and p = 0.941 confirms there is no evidence of a real shift.

# College/University student: +0.704 (p = 0.0148)
# Students start with about an 18‐point higher probability than “Not Provided” (around 76% vs. 58%), and p = 0.0148 is below 0.05. In other words, being a current student is significantly associated with a higher baseline chance of past‐year use.

# Professional certificate/diploma: –0.0003 (p = 0.999)
# There is effectively no change in starting probability (stays around 58%), and p ≈ 1 shows no difference from the reference.

# University degree: –0.578 (p = 0.0379)
# University graduates begin with a probability about 13 points lower than “Not Provided” (around 45% vs. 58%). Because p = 0.0379 is below 0.05, this lower baseline is statistically significant.

# Masters degree: –1.069 (p = 0.00026)
# Master’s holders start with about a 27‐point lower probability at baseline (roughly 31% instead of 58%). The p‐value is very small, so this is a highly significant finding: master’s graduates are much less likely to report past‐year use at the reference age.

# Doctorate degree: –0.130 (p = 0.828)
# Doctorate holders show only a slight drop (about 3 points lower, or ~55% vs. 58%), and p = 0.828 indicates no significant difference from “Not Provided.”

# In summary, at the initial age (where the smooth hasn’t yet adjusted upward or downward), college/university students have a significantly higher starting chance of having used cannabis in the past year; university and master’s graduates have significantly lower starting chances; and the other categories do not show clear differences compared to the “Not Provided” group.

# For the approximate significance of the smooth terms: 
# For every education category except doctorate holders, age has a statistically significant effect on past‐year use, though the exact shape of that effect varies. Young adults in every education group exhibit dramatically higher past‐year cannabis use, yet the way that use declines with age differs depending on schooling. For example, those in “Not Provided” and “left school before 16” categories show an almost straight‐line drop from ages 18–24 onward (edf = 1.000, p < 0.01), indicating a steady decrease as they age.



gam.check(gam.1)

plot(gam.1, residuals = TRUE, pages = 1, shade = TRUE)

