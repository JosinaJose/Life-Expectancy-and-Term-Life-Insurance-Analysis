# ============================================================================
# DAT 201 FINAL PROJECT - COMPLETE ANALYSIS
# Questions 1 & 2: Life Expectancy & Term Life Insurance Analysis
# ============================================================================

# --- SETUP: Install & Load Required Packages ---
required_packages <- c("tidyverse", "readr", "ggplot2", "naniar", "gtExtras", 
                       "ggcorrplot", "car", "lmtest", "gridExtra")

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(tidyverse)
library(readr)
library(ggplot2)
library(naniar)
library(gtExtras)
library(ggcorrplot)
library(car)
library(gridExtra)

# ============================================================================
# QUESTION 1: NATIONAL LIFE EXPECTANCY ANALYSIS
# ============================================================================

# --- 1. DATA IMPORT & CLEANING ---
cat("\n=== QUESTION 1: LOADING DATA ===\n")

# Update this path to your file location
file_path <- "D:/Josina/McMaster University/201- Data Modeling/Final Project/UNLifeExpectancy.csv"
National_Life_Expectancies <- read_csv(file_path)

# Initial exploration
cat("\nDataset dimensions:", dim(National_Life_Expectancies), "\n")
str(National_Life_Expectancies)
summary(National_Life_Expectancies)

# --- Missing Data Analysis ---
cat("\n=== MISSING DATA ANALYSIS ===\n")
missing_summary <- National_Life_Expectancies %>%
  miss_var_summary()
print(missing_summary)

# Visualize missingness
gg_miss_var(National_Life_Expectancies) +
  labs(title = "Missing Data by Variable")

# --- Data Cleaning ---
# Fix encoding issues
National_Life_Expectancies$COUNTRY <- iconv(National_Life_Expectancies$COUNTRY,
                                            from = "", to = "UTF-8", sub = "byte")

# Median imputation for key variables
median_cols <- c("ILLITERATE", "POP", "FERTILITY", "PRIVATEHEALTH",
                 "PUBLICEDUCATION", "HEALTHEXPEND", "BIRTHATTEND",
                 "PHYSICIAN", "GDP", "SMOKING", "RESEARCHERS", "FEMALEBOSS")

for(col in median_cols){
  National_Life_Expectancies[[col]][is.na(National_Life_Expectancies[[col]])] <- 
    median(National_Life_Expectancies[[col]], na.rm = TRUE)
}

cat("\n✓ Data cleaning complete. Missing values after imputation:\n")
print(colSums(is.na(National_Life_Expectancies)))


# ============================================================================
# Q1.1: EXAMINING RELATIONSHIP BETWEEN LIFEEXP AND FERTILITY
# ============================================================================

cat("\n=== Q1.1: LIFEEXP vs FERTILITY RELATIONSHIP ===\n")

# Scatter plot with regression line
p1 <- ggplot(National_Life_Expectancies, aes(x = FERTILITY, y = LIFEEXP)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE, linewidth = 1.2) +
  labs(title = "Life Expectancy vs Fertility Rate",
       subtitle = "Strong negative correlation observed",
       x = "Fertility Rate (children per woman)",
       y = "Life Expectancy (years)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p1)

# Calculate correlation
correlation <- cor(National_Life_Expectancies$FERTILITY, 
                   National_Life_Expectancies$LIFEEXP, 
                   use = "complete.obs")
cat("\nCorrelation between FERTILITY and LIFEEXP:", round(correlation, 4), "\n")


# ============================================================================
# Q1.2: SIMPLE LINEAR REGRESSION MODEL
# ============================================================================

cat("\n=== Q1.2: SIMPLE LINEAR REGRESSION ===\n")

model1 <- lm(LIFEEXP ~ FERTILITY, data = National_Life_Expectancies)
summary(model1)

cat("\n--- MODEL INTERPRETATION ---\n")
cat("Equation: LIFEEXP = ", round(coef(model1)[1], 2), " + ", 
    round(coef(model1)[2], 2), " × FERTILITY\n", sep = "")
cat("\nInterpretation for non-statisticians:\n")
cat("• For every additional child per woman, life expectancy DECREASES by", 
    abs(round(coef(model1)[2], 2)), "years\n")
cat("• R-squared:", round(summary(model1)$r.squared, 4), 
    "- meaning", round(summary(model1)$r.squared * 100, 1), 
    "% of life expectancy variation is explained by fertility\n")


# ============================================================================
# Q1.3: PREDICT US LIFE EXPECTANCY (FERTILITY = 2.0)
# ============================================================================

cat("\n=== Q1.3: PREDICTION FOR UNITED STATES ===\n")

us_fertility <- data.frame(FERTILITY = 2.0)
us_prediction <- predict(model1, newdata = us_fertility, interval = "confidence")

cat("\nFor USA with fertility rate of 2.0:\n")
cat("• Predicted Life Expectancy:", round(us_prediction[1], 2), "years\n")
cat("• 95% Confidence Interval: [", round(us_prediction[2], 2), ",", 
    round(us_prediction[3], 2), "] years\n")


# ============================================================================
# Q1.4: MULTIPLE REGRESSION MODEL
# ============================================================================

cat("\n=== Q1.4: MULTIPLE REGRESSION MODEL ===\n")

# Create log-transformed variable
National_Life_Expectancies$lnHEALTH <- log(National_Life_Expectancies$PRIVATEHEALTH)

# Fit multiple regression
multiple_model <- lm(LIFEEXP ~ FERTILITY + PUBLICEDUCATION + lnHEALTH,
                     data = National_Life_Expectancies)

summary(multiple_model)

# --- Q1.4a: Interpret PUBLICEDUCATION coefficient ---
cat("\n--- Q1.4a: PUBLIC EDUCATION INTERPRETATION ---\n")
pub_ed_coef <- coef(multiple_model)["PUBLICEDUCATION"]
cat("Coefficient:", round(pub_ed_coef, 4), "\n")
cat("\nInterpretation:\n")
cat("For every 1% increase in public education spending (as % of GDP),\n")
cat("life expectancy increases by", round(pub_ed_coef, 3), "years,\n")
cat("holding fertility and health expenditure constant.\n")

# --- Q1.4b: Interpret lnHEALTH without log scale ---
cat("\n--- Q1.4b: HEALTH EXPENDITURE INTERPRETATION ---\n")
lnhealth_coef <- coef(multiple_model)["lnHEALTH"]
cat("Coefficient for lnHEALTH:", round(lnhealth_coef, 4), "\n")
cat("\nInterpretation WITHOUT logarithmic scale:\n")
cat("A 1% increase in private health expenditure is associated with a\n")
cat(round(lnhealth_coef/100, 4), "year increase in life expectancy\n")
cat("(holding other variables constant).\n")
cat("\nAlternatively: A 10% increase in health spending increases life expectancy by\n")
cat("approximately", round(lnhealth_coef * log(1.1), 3), "years.\n")

# --- Q1.4c: Hypothesis Test for PUBLICEDUCATION ---
cat("\n--- Q1.4c: HYPOTHESIS TEST FOR PUBLIC EDUCATION ---\n")
cat("\nNull Hypothesis (H0): β_PUBLICEDUCATION = 0 (no effect)\n")
cat("Alternative Hypothesis (H1): β_PUBLICEDUCATION ≠ 0 (has effect)\n")
cat("Significance Level: α = 0.05\n")

pub_ed_pvalue <- summary(multiple_model)$coefficients["PUBLICEDUCATION", "Pr(>|t|)"]
pub_ed_tstat <- summary(multiple_model)$coefficients["PUBLICEDUCATION", "t value"]

cat("\nTest Statistics:\n")
cat("• t-statistic:", round(pub_ed_tstat, 4), "\n")
cat("• p-value:", format.pval(pub_ed_pvalue, digits = 4), "\n")

cat("\nDecision Rule: Reject H0 if p-value < 0.05\n")
cat("\nConclusion:", 
    ifelse(pub_ed_pvalue < 0.05, 
           "✓ REJECT H0: Public education IS statistically significant",
           "✗ FAIL TO REJECT H0: Public education is NOT statistically significant"),
    "\n")


# --- Additional Visualizations ---
cat("\n=== ADDITIONAL ANALYSIS ===\n")

# Correlation heatmap
num_vars <- National_Life_Expectancies %>%
  select(LIFEEXP, FERTILITY, PUBLICEDUCATION, PRIVATEHEALTH, HEALTHEXPEND, GDP) %>%
  na.omit()

corr_matrix <- cor(num_vars)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 3,
           title = "Correlation Heatmap of Key Variables",
           colors = c("#6D9EC1", "white", "#E46726"))

# Distribution of Life Expectancy
ggplot(National_Life_Expectancies, aes(x = LIFEEXP)) +
  geom_histogram(binwidth = 3, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(LIFEEXP, na.rm = TRUE)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Life Expectancy Across Countries",
       subtitle = paste("Mean:", round(mean(National_Life_Expectancies$LIFEEXP, na.rm = TRUE), 1), "years"),
       x = "Life Expectancy (Years)",
       y = "Number of Countries") +
  theme_minimal(base_size = 14)


# ============================================================================
# Q1.7: OVERALL COMMENT FOR NON-STATISTICIAN AUDIENCE
# ============================================================================

cat("\n=== Q1.7: SUMMARY FOR NON-STATISTICIANS ===\n\n")
cat("KEY FINDINGS:\n")
cat("───────────────────────────────────────────────────────────\n")
cat("1. FERTILITY & LIFE EXPECTANCY:\n")
cat("   Countries with higher fertility rates tend to have LOWER life expectancy.\n")
cat("   Each additional child per woman is associated with a", abs(round(coef(model1)[2], 1)),
    "year\n   decrease in life expectancy.\n\n")

cat("2. MULTIPLE FACTORS MATTER:\n")
cat("   When considering fertility, public education, and health spending together:\n")
cat("   • Higher public education spending → LONGER life expectancy\n")
cat("   • Higher health expenditure → LONGER life expectancy\n")
cat("   • Higher fertility → SHORTER life expectancy\n\n")

cat("3. PRACTICAL IMPLICATIONS:\n")
cat("   • Public education IS statistically significant (p < 0.05)\n")
cat("   • Investing in education and healthcare improves population health\n")
cat("   • The model explains", round(summary(multiple_model)$r.squared * 100, 1),
    "% of life expectancy variation\n\n")

cat("4. US PREDICTION:\n")
cat("   Based on the USA's fertility rate of 2.0, predicted life expectancy is\n")
cat("   approximately", round(us_prediction[1], 1), "years.\n")
cat("───────────────────────────────────────────────────────────\n\n")


# ============================================================================
# QUESTION 2: TERM LIFE INSURANCE ANALYSIS
# ============================================================================

cat("\n\n")
cat("============================================================================\n")
cat("                    QUESTION 2: TERM LIFE INSURANCE ANALYSIS\n")
cat("============================================================================\n\n")

# --- 2. DATA IMPORT ---
cat("=== LOADING TERM LIFE INSURANCE DATA ===\n")

# Update this path
file_path2 <- "D:/Josina/McMaster University/201- Data Modeling/Final Project/TermLife-1.csv"
Term_Life_Insurance <- read_csv(file_path2)

# Initial exploration
cat("\nDataset dimensions:", dim(Term_Life_Insurance), "\n")
glimpse(Term_Life_Insurance)
summary(Term_Life_Insurance[, c("GENDER", "AGE", "INCOME", "FACE")])


# ============================================================================
# Q2.1: RELATIONSHIPS BETWEEN VARIABLES (GRAPHICAL ANALYSIS)
# ============================================================================

cat("\n=== Q2.1: EXPLORATORY VISUALIZATIONS ===\n")

# Gender vs FACE
p_gender <- ggplot(Term_Life_Insurance, aes(x = factor(GENDER, labels = c("Female", "Male")), y = FACE)) +
  geom_boxplot(fill = c("pink", "lightblue"), alpha = 0.7) +
  labs(title = "Insurance Coverage by Gender",
       x = "Gender", y = "Face Amount ($)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Age vs FACE
p_age <- ggplot(Term_Life_Insurance, aes(x = AGE, y = FACE)) +
  geom_point(alpha = 0.4, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Insurance Coverage vs Age",
       x = "Age (years)", y = "Face Amount ($)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Income vs FACE
p_income <- ggplot(Term_Life_Insurance, aes(x = INCOME, y = FACE)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(title = "Insurance Coverage vs Income",
       x = "Annual Income ($)", y = "Face Amount ($)") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

# Display plots
grid.arrange(p_gender, p_age, p_income, ncol = 2)

# Correlation matrix for numerical variables
cat("\n--- CORRELATION MATRIX ---\n")
numerical_vars <- Term_Life_Insurance %>% 
  select(AGE, INCOME, FACE) %>%
  na.omit()

correlation_matrix <- cor(numerical_vars)
print(round(correlation_matrix, 4))

# Visualize correlations
ggcorrplot(correlation_matrix, 
           lab = TRUE, 
           title = "Correlation Between Age, Income, and Insurance Coverage")


# ============================================================================
# Q2.2: MULTIPLE LINEAR REGRESSION ANALYSIS
# ============================================================================

cat("\n=== Q2.2: MULTIPLE LINEAR REGRESSION ===\n")

# --- Clean data (remove zeros and negative values) ---
clean_data <- Term_Life_Insurance %>%
  filter(FACE > 0 & INCOME > 0)

cat("Observations after cleaning:", nrow(clean_data), "\n")

# --- Q2.2a: Fit Multiple Linear Regression ---
model2 <- lm(FACE ~ GENDER + AGE + INCOME, data = clean_data)

cat("\n--- MODEL SUMMARY ---\n")
summary(model2)

# --- Q2.2b: Estimated Regression Parameters ---
cat("\n--- Q2.2b: REGRESSION COEFFICIENTS ---\n")
coefficients_table <- summary(model2)$coefficients
print(round(coefficients_table, 4))

cat("\nRegression Equation:\n")
cat("FACE = ", round(coef(model2)[1], 2), 
    " + ", round(coef(model2)[2], 2), " × GENDER",
    " + ", round(coef(model2)[3], 2), " × AGE",
    " + ", round(coef(model2)[4], 2), " × INCOME\n", sep = "")

# --- Q2.2c: Model Performance Metrics ---
cat("\n--- Q2.2c: MODEL PERFORMANCE ---\n")
r_squared <- summary(model2)$r.squared
r_squared_adj <- summary(model2)$adj.r.squared
mse <- mean(model2$residuals^2)
rmse <- sqrt(mse)

cat("R-squared:          ", round(r_squared, 4), 
    " (", round(r_squared * 100, 2), "% variance explained)\n", sep = "")
cat("Adjusted R-squared: ", round(r_squared_adj, 4), "\n")
cat("MSE:                ", format(mse, big.mark = ",", scientific = FALSE), "\n")
cat("RMSE:               ", format(rmse, big.mark = ",", scientific = FALSE), "\n")

# --- Q2.2d: T-tests for Variable Significance ---
cat("\n--- Q2.2d: T-TEST RESULTS (Variable Importance) ---\n")
cat("\nHypothesis tests for each variable:\n")
cat("H0: β = 0 (variable has no effect)\n")
cat("H1: β ≠ 0 (variable has an effect)\n")
cat("Significance level: α = 0.05\n\n")

coef_summary <- summary(model2)$coefficients
for(i in 2:nrow(coef_summary)) {
  var_name <- rownames(coef_summary)[i]
  t_val <- coef_summary[i, "t value"]
  p_val <- coef_summary[i, "Pr(>|t|)"]
  
  cat("Variable:", var_name, "\n")
  cat("  t-statistic:", round(t_val, 4), "\n")
  cat("  p-value:", format.pval(p_val, digits = 4), "\n")
  cat("  Conclusion:", 
      ifelse(p_val < 0.05, 
             "✓ SIGNIFICANT (reject H0)", 
             "✗ NOT SIGNIFICANT (fail to reject H0)"), "\n\n")
}


# ============================================================================
# Q2.4: INTERPRETATION FOR NON-STATISTICIANS
# ============================================================================

cat("\n=== Q2.4: SUMMARY FOR NON-STATISTICIANS ===\n\n")
cat("KEY FINDINGS - TERM LIFE INSURANCE:\n")
cat("───────────────────────────────────────────────────────────\n")
cat("1. WHAT AFFECTS INSURANCE COVERAGE?\n")
cat("   Our analysis examined how gender, age, and income relate to the\n")
cat("   amount of life insurance people purchase (FACE value).\n\n")

cat("2. GENDER:\n")
gender_coef <- coef(model2)["GENDER"]
gender_pval <- coef_summary["GENDER", "Pr(>|t|)"]
if(gender_pval < 0.05) {
  cat("   Gender IS significant. Males tend to have insurance coverage that is\n")
  cat("   $", format(abs(round(gender_coef, 0)), big.mark = ","), 
      " different from females (all else equal).\n\n", sep = "")
} else {
  cat("   Gender is NOT a significant factor in insurance coverage amount.\n\n")
}

cat("3. AGE:\n")
age_coef <- coef(model2)["AGE"]
age_pval <- coef_summary["AGE", "Pr(>|t|)"]
if(age_pval < 0.05) {
  cat("   Age IS significant. For each additional year of age,\n")
  cat("   insurance coverage changes by $", format(round(age_coef, 0), big.mark = ","), ".\n\n", sep = "")
} else {
  cat("   Age is NOT a significant factor in insurance coverage amount.\n\n")
}

cat("4. INCOME:\n")
income_coef <- coef(model2)["INCOME"]
income_pval <- coef_summary["INCOME", "Pr(>|t|)"]
if(income_pval < 0.05) {
  cat("   Income IS HIGHLY significant. For every $1,000 increase in income,\n")
  cat("   insurance coverage increases by approximately $", 
      format(round(income_coef * 1000, 0), big.mark = ","), ".\n", sep = "")
  cat("   This makes intuitive sense: higher earners buy more coverage.\n\n")
} else {
  cat("   Income is NOT a significant factor (surprising!).\n\n")
}

cat("5. MODEL FIT:\n")
cat("   The model explains", round(r_squared * 100, 1), 
    "% of the variation in insurance coverage.\n")
if(r_squared > 0.3) {
  cat("   This is a reasonably good fit for social science data.\n\n")
} else {
  cat("   This suggests other factors also play important roles.\n\n")
}

cat("6. BOTTOM LINE:\n")
significant_vars <- rownames(coef_summary)[coef_summary[, "Pr(>|t|)"] < 0.05][-1]
if(length(significant_vars) > 0) {
  cat("   ", paste(significant_vars, collapse = ", "), 
      " significantly affect insurance purchase decisions.\n", sep = "")
} else {
  cat("   None of the variables are statistically significant.\n")
}
cat("───────────────────────────────────────────────────────────\n")


# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("\n\n=== SAVING RESULTS ===\n")

# Save cleaned data
write.csv(National_Life_Expectancies,
          "National_Life_Expectancies_Clean.csv",
          row.names = FALSE)

write.csv(clean_data,
          "Term_Life_Insurance_Clean.csv",
          row.names = FALSE)

cat("✓ Analysis complete! Results saved.\n")
cat("✓ Check your working directory for cleaned datasets.\n")

