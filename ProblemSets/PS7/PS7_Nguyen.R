
library(modelsummary)
library(mice)


wages <- read.csv("wages.csv")

wages_clean <- wages[!is.na(wages$hgc) & !is.na(wages$tenure), ]

datasummary_skim(wages_clean, 
                 output = "wages_clean_summary.tex",
                 title = "Summary Statistics: Cleaned Wages Dataset")

# Removing missing logwage
# Remove rows where logwage is missing from the entire data frame
df1 <- wages_clean[!is.na(wages_clean$logwage), ]

# Now run the regression on the complete data frame
lm1 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, 
          data = df1)

# View results
summary(lm1)

# Replace missing logwage with mean logwage
wages_clean$logwage[is.na(wages_clean$logwage)] <- mean(wages_clean$logwage, na.rm = TRUE)

lm2 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, 
          data = wages_clean)

summary(lm2)

# Impute missing logwages as their predicted values. 

# Create a new data frame with only the complete cases for the independent variables

wages_missing <- wages_clean[is.na(wages_clean$logwage), ]

wages_missing$logwage_predicted = predict(lm1, newdata = wages_missing)
head(wages_missing)

wages_imputed = wages_clean

wages_imputed$logwage[is.na(wages_imputed$logwage)] <- wages_missing$logwage_predicted

lm3 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, 
          data = wages_imputed)
summary(lm3)

# Perform Multiple Imputation using the mice package

imputed_data <- mice(wages_clean, m = 5, method = 'norm', seed = 100)

# Fit the regression model on each imputed dataset and pool the results

fit <- with(imputed_data, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))

pooled_results <- pool(fit)

summary(pooled_results)


# Summary of all the models

modelsummary(
  list(
    "Complete Cases" = lm1,
    "Mean Imputation" = lm2,
    "Regression Imputation" = lm3,
    "Multiple Imputation (MICE)" = pooled_results
  ),
  output = "model_comparison.tex",
  title = "Comparison of Regression Models with Different Missing Data Treatments"
  )


