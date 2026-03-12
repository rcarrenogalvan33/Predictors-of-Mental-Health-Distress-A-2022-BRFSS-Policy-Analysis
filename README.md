# Predictors-of-Mental-Health-Distress-A-2022-BRFSS-Policy-Analysis
An investigation into the social determinants of mental health using hierarchical linear regression in R. This project identifies how physical health and employment status override traditional socioeconomic indicators in predicting psychological distress.
Analyzing Social Determinants of Mental Health (BRFSS 2022)
📝 Executive Summary

This analysis investigates the relationship between socioeconomic status (SES), employment, and physical health on the frequency of poor mental health days. Using the 2022 BRFSS dataset, I developed a hierarchical linear regression model to determine which factors most significantly predict mental health distress in a post-pandemic landscape.
🛠️ The Problem

Policymakers often focus on income as the primary driver of health outcomes. However, raw data suggests that physical health and employment stability may play a more direct role. I sought to answer: Does the impact of income remain significant once we account for a person's physical health and ability to work?
🧬 Methodology & Workflow

    Data Cleaning: Handled complex BRFSS survey codes (e.g., converting "88" to 0 days, filtering "Refused" and "Don't Know" 7/9 codes).

    Exploratory Data Analysis (EDA): * Identified a polarized distribution where a significant sub-population reports 30/30 days of poor mental health.

        Visualized clear disparities in mental health medians across employment categories.

    Statistical Modeling: Built three incremental Linear Regression models to test for variable "added value" and used sjPlot for professional reporting.

📈 Key Results

    The Health Dominance: Adding General Health status to the model increased the explained variance (R2) from 3% to 14%.

    Predictor Strength: "Poor" physical health was the strongest predictor, adding +10.2 days of mental distress.

    Employment Gap: Even when controlling for income, individuals "Unable to work" reported +1.44 days of poor mental health compared to those currently employed.

📂 Repository Structure

    mental_health_analysis.R: The full cleaning and analysis pipeline.

    /plots: Exported visualizations for stakeholders.

    README.md: Project documentation and summary.

🚀 Technical Skills Demonstrated

    R Programming: Tidyverse, Data Wrangling, Factor Management.

    Statistics: Hierarchical Regression, ANOVA, Tukey HSD, Chi-Square testing.

    Communication: Data visualization and policy-oriented storytelling.
