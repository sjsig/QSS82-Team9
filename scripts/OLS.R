# Load packages -----------------------------------------------------------

library(tidyverse)
library( taRifx )
library(car)
library(xtable)
library(stargazer)
library(BBmisc)
library(dplyr)


# Load data  --------------------------------------------------------------

data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)

data <- japply( data, which(sapply(data, typeof)=="integer"), as.double )

colnames(data)
types <- sapply(data, typeof)
types_df <- data.frame(types) %>%
  arrange(types)


# OLS ---------------------------------------------------------------------
# fit <- lm(stock_change ~ stringency_ra +
#             agriculture +
#             industry +
#             manufacturing +
#             services +
#             frac_DPI +
#             aged_65_older +
#             new_cases_smoothed_per_million + 
#             new_deaths_smoothed_per_million +
#             human_development_index +
#             retail_and_recreation +
#             residential +
#             urban_pop +
#             one_yr_unemp_bene +
#             oil_price +
#             CCI +
#             stimulus_spending_pct_gdp +
#             gdp_per_capita +
#             polity +
#             hospital_beds_per_thousand +
#             population_density
#           , data=data)
# summary(fit)
# 
# names(fit$coefficients) <- c("Intercept",
#                               "Lockdown Severity",
#                                "Agriculture Sector Share of GDP",
#                                "Industry Sector Share of GDP",
#                                "Manufacturing Sector Share of GDP",
#                                "Service Sector Share of GDP",
#                                "Political Fractionalization Index",
#                                "Share of Population Older Than 65",
#                                "New COVID-19 Cases Per Million People",
#                                 "New COVID-19 Deaths Per Million People",
#                                "Human Development Index",
#                                "Retail and Recreational Mobility",
#                                "Residential Mobility",
#                                "Urban Share of Population",
#                                "One-Year Unemployment Benefits",
#                                "Oil Spot Price",
#                                "Consumer Confidence Index",
#                                "Total Stimulus Spending as % of GDP",
#                                "GDP Per Capita",
#                                 "Polity Score",
#                                 "Hospital Beds per 1000 Citizens",
#                                 "Population Density")

fit <- lm(stock_change ~ stringency_ra +
            agriculture +
            industry +
            manufacturing +
            services +
            frac_DPI +
            aged_65_older +
            new_cases_smoothed_per_million + 
            new_deaths_smoothed_per_million +
            human_development_index +
            retail_and_recreation +
            residential +
            oil_price +
            stimulus_spending_pct_gdp +
            health_spending_pct_gdp +
            new_vaccinations_smoothed_per_million +
            gdp_per_capita +
            polity +
            hospital_beds_per_thousand +
            population_density_log
          , data=data)

names(fit$coefficients) <- c("Intercept",
                             "Lockdown Severity",
                              "Agriculture Sector Share of GDP",
                              "Industry Sector Share of GDP",
                              "Manufacturing Sector Share of GDP",
                              "Service Sector Share of GDP",
                              "Political Fractionalization Index",
                              "Share of Population Older Than 65",
                              "New COVID-19 Cases Per Million People",
                               "New COVID-19 Deaths Per Million People",
                              "Human Development Index",
                              "Retail and Recreational Mobility",
                              "Residential Mobility",
                              "Oil Spot Price",
                              "Total Stimulus Spending as % of GDP",
                              "Healthcare Spending as % of GDP",
                             "New COVID-19 Vaccinations Per Million People",
                              "GDP Per Capita (Log)",
                               "Polity Score",
                               "Hospital Beds per 1000 People",
                               "Population Density (Log)")

summary(fit)
xtable(summary(fit))


# Normalized OLS Regression -----------------------------------------------

norm_data <- normalize(data, method = "standardize")

fit <- lm(stock_change ~ stringency_ra +
            agriculture +
            industry +
            manufacturing +
            services +
            frac_DPI +
            aged_65_older +
            new_cases_smoothed_per_million + 
            new_deaths_smoothed_per_million +
            human_development_index +
            retail_and_recreation +
            residential +
            oil_price +
            stimulus_spending_pct_gdp +
            health_spending_pct_gdp +
            new_vaccinations_smoothed_per_million +
            gdp_per_capita +
            polity +
            hospital_beds_per_thousand +
            population_density_log
          , data=norm_data)

names(fit$coefficients) <- c("Intercept",
                             "Lockdown Severity",
                             "Agriculture Sector Share of GDP",
                             "Industry Sector Share of GDP",
                             "Manufacturing Sector Share of GDP",
                             "Service Sector Share of GDP",
                             "Political Fractionalization Index",
                             "Share of Population Older Than 65",
                             "New COVID-19 Cases Per Million People",
                             "New COVID-19 Deaths Per Million People",
                             "Human Development Index",
                             "Retail and Recreational Mobility",
                             "Residential Mobility",
                             "Oil Spot Price",
                             "Total Stimulus Spending as % of GDP",
                             "Healthcare Spending as % of GDP",
                             "New COVID-19 Vaccinations Per Million People",
                             "GDP Per Capita (Log)",
                             "Polity Score",
                             "Hospital Beds per 1000 People",
                             "Population Density (Log)")

summary(fit)
xtable(summary(fit))



# IV - rollmean package, rolling average of stringency index of last 7 days
# Lockdown policy - stringency_index 
# Citizen Compliance - retail_and_recreation, residential
# Country demographics - elderly_population/aged_65_older, pop_area + pop_distribution_for_area, population_density, life_expectancy, poverty_type + poverty_rate_for_type, human_development_index, median_age,     
# Other economic factors - *oil_prices, *price_volatility, *economic_composition, BCI, (CCI?), CLI
# Provided economic aid - financial_disincentive_type + financial_disincentives_to_work_for_type, unemployment_benefit_type + unemployment_benefits_for_type
# Political factors - trust_in_gov, *political_stability_index, *civil_liberties
# Economic effects - stock_change


# Other analyses ----------------------------------------------------------

# coefficients(fit) # model coefficients
# confint(fit, level=0.95) # CIs for model parameters
# fitted(fit) # predicted values
# residuals(fit) # residuals
# anova(fit) # anova table
# vcov(fit) # covariance matrix for model parameters
# influence(fit) # regression diagnostics
# 
# # diagnostic plots
# layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
# plot(fit)
# 
# # compare models
# # fit1 <- lm(y ~ x1 + x2 + x3 + x4, data=mydata)
# # fit2 <- lm(y ~ x1 + x2)
# # anova(fit1, fit2)
# 
# # Stepwise Regression
library(MASS)
step_data <- data %>%
  select(stock_change, stringency_ra ,
           agriculture ,
           industry ,
           manufacturing ,
           services ,
           frac_DPI ,
           aged_65_older ,
           new_cases_smoothed_per_million ,
           new_deaths_smoothed_per_million ,
           human_development_index ,
           retail_and_recreation ,
           residential ,
           urban_pop ,
           one_yr_unemp_bene ,
           oil_price ,
           CCI ,
           stimulus_spending_pct_gdp ,
           gdp_per_capita ,
           polity ,
           hospital_beds_per_thousand ,
           population_density) %>%
  drop_na()
step_fit <- lm(stock_change ~ stringency_ra +
                 agriculture +
                 industry +
                 manufacturing +
                 services +
                 frac_DPI +
                 aged_65_older +
                 new_cases_smoothed_per_million + 
                 new_deaths_smoothed_per_million +
                 human_development_index +
                 retail_and_recreation +
                 residential +
                 urban_pop +
                 one_yr_unemp_bene +
                 oil_price +
                 CCI +
                 stimulus_spending_pct_gdp +
                 gdp_per_capita +
                 polity +
                 hospital_beds_per_thousand +
                 population_density
          , data=step_data)

step <- stepAIC(step_fit, direction="both", steps = 1000)
step$anova # display results

best_fit <- lm(stock_change ~ stringency_ra + services + polity + frac_DPI + human_development_index +
                 residential + urban_pop + oil_price + hospital_beds_per_thousand + new_cases_smoothed_per_million + retail_and_recreation + stimulus_spending_pct_gdp, data = data)
summary(best_fit)
# 
# # Calculate Relative Importance for Each Predictor
# library(relaimpo)
# calc.relimp(fit,type=c("lmg","last","first","pratt"),
#             rela=TRUE)
# 
# # Bootstrap Measures of Relative Importance (1000 samples)
# 
# boot <- boot.relimp(fit, b = 10, type = c("lmg",
#                                             "last", "first", "pratt"), rank = TRUE,
#                     diff = TRUE, rela = TRUE)
# booteval.relimp(boot) # print result
# plot(booteval.relimp(boot,sort=TRUE)) # plot result