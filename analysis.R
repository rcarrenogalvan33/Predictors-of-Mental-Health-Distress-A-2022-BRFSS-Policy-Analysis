############################################################
# Mental Health & Social Determinants Analysis
# Author: Rebe
# Purpose: Policy-oriented analysis of poor mental health days
# Dataset: Behavioral Risk Factor Surveillance System (BRFSS)
############################################################

############################
# 1. Load Libraries
############################
library(tidyverse)
library(effectsize)

############################
# 2. Load Data
############################
# Replace with your actual file path
mh <- read.csv("mental_health_data.csv", nrows = 10000)

############################
# 3. Variable Selection & Cleaning
############################

mh_clean <- mh %>%
  select(MENTHLTH, PHYSHLTH, INCOME3, EDUCA, EMPLOY1, GENHLTH) %>%
  mutate(
    MENTHLTH = case_when(MENTHLTH == 88 ~ 0, MENTHLTH > 30 ~ NA_real_, TRUE ~ as.numeric(MENTHLTH)),
    PHYSHLTH = case_when(PHYSHLTH == 88 ~ 0, PHYSHLTH > 30 ~ NA_real_, TRUE ~ as.numeric(PHYSHLTH))
  ) %>%
  filter(!is.na(MENTHLTH), !is.na(PHYSHLTH)) %>%
  mutate(
    # CREATE THIS FIRST so it survives the filter
    poor_menthlth_14 = factor(case_when(
      MENTHLTH < 14 ~ "<14 days",
      MENTHLTH >= 14 ~ "14+ days"
    )),
    EMPLOY1 = factor(EMPLOY1) %>%
      fct_recode("Employed"="1", "Employed"="2", "Unemployed"="3", "Unemployed"="4",
                 "Homemaker"="5", "Student"="6", "Retired"="7", "Unable to work"="8"),
    INCOME3 = factor(INCOME3) %>%
      fct_recode("<$10k"="1", "$10k-$15k"="2", "$15k-$20k"="3", "$20k-$25k"="4", 
                 "$25k-$35k"="5", "$35k-$50k"="6", "$50k-$75k"="7", "$75k+"="8"),
    GENHLTH = factor(GENHLTH) %>%
      fct_recode("Excellent"="1", "Very Good"="2", "Good"="3", "Fair"="4", "Poor"="5"),
    EDUCA = factor(EDUCA) %>%
      fct_recode("Less than HS"="1", "HS Grad"="2", "Some College"="3", "College Grad"="4")
  ) %>%
  # NUCLEAR FILTER
  filter(
    EMPLOY1 %in% c("Employed", "Unemployed", "Homemaker", "Student", "Retired", "Unable to work"),
    INCOME3 %in% c("<$10k", "$10k-$15k", "$15k-$20k", "$20k-$25k", "$25k-$35k", "$35k-$50k", "$50k-$75k", "$75k+"),
    GENHLTH %in% c("Excellent", "Very Good", "Good", "Fair", "Poor"),
    EDUCA   %in% c("Less than HS", "HS Grad", "Some College", "College Grad")
  ) %>%
  mutate(across(where(is.factor), fct_drop))
############################
# 4. Missingness Check
############################
sapply(mh_clean[, c("MENTHLTH","PHYSHLTH","INCOME3","EDUCA","EMPLOY1","GENHLTH")], 
       function(x) mean(is.na(x)))
############################
# 5. Descriptive Statistics
############################
summary(mh_clean$MENTHLTH)
summary(mh_clean$PHYSHLTH)

############################
# 6. Visualizations
############################

# Histogram: Notice we use 'mh_clean' now
ggplot(mh_clean, aes(x = MENTHLTH)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  xlim(0, 31) + 
  labs(title = "Cleaned Distribution: Poor Mental Health Days")

# Boxplot: Notice we use 'mh_clean' now
ggplot(mh_clean, aes(x = EMPLOY1, y = MENTHLTH)) +
  geom_boxplot() +
  scale_x_discrete(drop = TRUE) + # This tells ggplot to drop categories with 0 people
  coord_flip() +
  labs(title = "Mental Health by Employment (Cleaned)")
############################
# 7. Cross-tabs & Chi-square Tests
############################

# Mental health (14+ days) by employment
mh_tab_employ <- table(mh_clean$poor_menthlth_14, mh_clean$EMPLOY1)
mh_tab_employ
prop.table(mh_tab_employ, margin = 1) * 100
chisq.test(mh_tab_employ)

# Mental health (14+ days) by income
mh_tab_income <- table(mh_clean$poor_menthlth_14, mh_clean$INCOME3)
mh_tab_income
prop.table(mh_tab_income, margin = 1) * 100
chisq.test(mh_tab_income)

############################
# 8. ANOVA + Tukey + Effect Size
############################

# Mental health by income
anova_income <- aov(MENTHLTH ~ INCOME3, data = mh_clean)
summary(anova_income)
TukeyHSD(anova_income)
eta_squared(anova_income, partial = FALSE)

# Mental health by education
anova_educa <- aov(MENTHLTH ~ EDUCA, data = mh_clean)
summary(anova_educa)
TukeyHSD(anova_educa)
eta_squared(anova_educa, partial = FALSE)

# Mental health by general health
anova_genhlth <- aov(MENTHLTH ~ GENHLTH, data = mh_clean)
summary(anova_genhlth)
TukeyHSD(anova_genhlth)
eta_squared(anova_genhlth, partial = FALSE)

############################
# 9. Numeric × Numeric Analysis
############################

# Scatterplot: Change 'mh' to 'mh_clean'
ggplot(mh_clean, aes(x = PHYSHLTH, y = MENTHLTH)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Mental vs Physical Health Days")

# Correlation: Change 'mh' to 'mh_clean'
cor(mh_clean$MENTHLTH, mh_clean$PHYSHLTH, use = "complete.obs")
cor.test(mh_clean$MENTHLTH, mh_clean$PHYSHLTH)

############################
# 10. Simple Linear Regression
############################

# Regression: Change 'mh' to 'mh_clean'
lm_phys_ment <- lm(MENTHLTH ~ PHYSHLTH, data = mh_clean)
summary(lm_phys_ment)
confint(lm_phys_ment)
# Diagnostics
par(mfrow = c(2,2))
plot(lm_phys_ment)

############################
# 11. Multiple Regression (Policy Model)
############################

mh_complete <- mh_clean %>%
  drop_na(MENTHLTH, INCOME3, EDUCA, EMPLOY1, GENHLTH)

# Baseline model
m0 <- lm(MENTHLTH ~ 1, data = mh_complete)

# Incremental models
m1 <- lm(MENTHLTH ~ INCOME3 + EDUCA, data = mh_complete)
m2 <- lm(MENTHLTH ~ INCOME3 + EDUCA + EMPLOY1, data = mh_complete)
m3 <- lm(MENTHLTH ~ INCOME3 + EDUCA + EMPLOY1 + GENHLTH, data = mh_complete)

# Model summaries
summary(m1)
summary(m2)
summary(m3)

############################
# 12. Model Comparison
############################

summary(m0)$adj.r.squared
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared
summary(m3)$adj.r.squared

anova(m0, m1, m2, m3)

############################
# 13. Professional Table Export
############################
# Install if you haven't yet: install.packages("sjPlot")
library(sjPlot)

# This creates a side-by-side table of your 3 policy models
tab_model(m1, m2, m3, 
          show.ci = FALSE, 
          show.se = TRUE, 
          dv.labels = c("Model 1: SES", "Model 2: Employment", "Model 3: Health"),
          title = "Regression Analysis of Poor Mental Health Days",
          string.pred = "Predictors",
          string.est = "Estimate (B)")
############################
# End of Script
############################