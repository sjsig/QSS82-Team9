# Load packages -----------------------------------------------------------

library(tidyverse)
library( taRifx )
library(car)
library(xtable)
library(stargazer)

# Load data  --------------------------------------------------------------

data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)

data <- japply( data, which(sapply(data, typeof)=="integer"), as.double )

data <- data %>%
  filter(Date < "2021-01-01")

colnames(data)
types <- sapply(data, typeof)
types_df <- data.frame(types) %>%
  arrange(types)


# OLS ---------------------------------------------------------------------
fit <- lm(stock_change ~ stringency_index +
            median_age +
            stringency_index_m1 +
            stringency_index_m2 +
            stringency_index_m3 +
            stringency_index_m4 +
            stringency_index_m5 +
            stringency_index_m6 +
            stringency_index_m7 +
            stringency_index_m8 +
            stringency_index_m9 +
            stringency_index_m10 +
            life_expectancy +
            agriculture +
            industry +
            manufacturing +
            services +
            maj_DPI +
            frac_DPI +
            aged_65_older +
            covid_rate + 
            human_development_index +
            retail_and_recreation +
            residential +
            rural_pop +
            suburban_pop +
            urban_pop +
            one_yr_unemp_bene +
            oil_price +
            CCI +
            stimulus_spending_pct_gdp +
            liquidity_support_pct_gdp +
            health_stimulus_spending_pct_gdp +
            gdp_per_capita, data=data)

names(fit$coefficients) <- c("Intercept",
                              "Lockdown Severity",
                               "Median Age",
                               "Lockdown Severity (Day N-1)",
                               "Lockdown Severity (Day N-2)",
                               "Lockdown Severity (Day N-3)",
                               "Lockdown Severity (Day N-4)",
                               "Lockdown Severity (Day N-5)",
                               "Lockdown Severity (Day N-6)",
                               "Lockdown Severity (Day N-7)",
                               "Lockdown Severity (Day N-8)",
                               "Lockdown Severity (Day N-9)",
                               "Lockdown Severity (Day N-10)",
                               "Life Expectancy",
                               "Agriculture Sector Share of GDP",
                               "Industry Sector Share of GDP",
                               "Manufacturing Sector Share of GDP",
                               "Service Sector Share of GDP",
                               "Margin of Majority",
                               "Fractionalization Index",
                               "Share of Population Older Than 65",
                               "Total COVID-19 Cases Per Million People", 
                               "Human Development Index",
                               "Retail and Recreational Mobility",
                               "Residential Mobility",
                               "Rural Share of Population",
                               "Suburban Share of Population",
                               "Urban Share of Population",
                               "Unemployment Benefits",
                               "Oil Spot Prices",
                               "Consumer Confidence Index",
                               "Total Stimulus Spending Pct of GDP",
                               "Liquidity Support Pct of GDP",
                               "Healthcare Stimulus Spending Pct of GDP",
                               "GDP Per Capita")

summary(fit)
xtable(summary(fit))




# Lockdown policy - stringency_index 
# Citizen Compliance - retail_and_recreation, residential
# Country demographics - elderly_population/aged_65_older, pop_area + pop_distribution_for_area, population_density, life_expectancy, poverty_type + poverty_rate_for_type, human_development_index, median_age,     
# Other economic factors - *oil_prices, *price_volatility, *economic_composition, BCI, (CCI?), CLI
# Provided economic aid - financial_disincentive_type + financial_disincentives_to_work_for_type, unemployment_benefit_type + unemployment_benefits_for_type
# Political factors - trust_in_gov, *political_stability_index, *civil_liberties
# Economic effects - stock_change


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
# fit1 <- lm(y ~ x1 + x2 + x3 + x4, data=mydata)
# fit2 <- lm(y ~ x1 + x2)
# anova(fit1, fit2)

# Stepwise Regression
library(MASS)
step <- stepAIC(fit, direction="both")
step$anova # display results

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)

boot <- boot.relimp(fit, b = 10, type = c("lmg",
                                            "last", "first", "pratt"), rank = TRUE,
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result