# Load packages -----------------------------------------------------------

library(tidyverse)

# Load data  --------------------------------------------------------------

data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)

usa_data <- data %>%
  filter(Country == "USA")

colnames(usa_data)

# Lockdown policy - stringency_index 
# Citizen Compliance - retail_and_recreation, residential
# Country demographics - elderly_population/aged_65_older, pop_area + pop_distribution_for_area, population_density, life_expectancy, poverty_type + poverty_rate_for_type, human_development_index, median_age,     
# Other economic factors - *oil_prices, *price_volatility, *economic_composition, BCI, (CCI?), CLI
# Provided economic aid - financial_disincentive_type + financial_disincentives_to_work_for_type, unemployment_benefit_type + unemployment_benefits_for_type
# Political factors - trust_in_gov, *political_stability_index, *civil_liberties
# Economic effects - stock_change

# Stimulus data 


# OLS ---------------------------------------------------------------------
fit <- lm(stock_change ~ stringency_index + 
            aged_65_older + 
            human_development_index + 
            median_age + 
            life_expectancy +
            population_density +
            extreme_poverty +
            
            , data=data)
summary(fit) # show results


# Other analyses ----------------------------------------------------------

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

# compare models
fit1 <- lm(y ~ x1 + x2 + x3 + x4, data=mydata)
fit2 <- lm(y ~ x1 + x2)
anova(fit1, fit2)

# Stepwise Regression
library(MASS)
fit <- lm(y~x1+x2+x3,data=mydata)
step <- stepAIC(fit, direction="both")
step$anova # display results

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(fit, b = 1000, type = c("lmg",
                                            "last", "first", "pratt"), rank = TRUE,
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result