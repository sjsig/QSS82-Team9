# File Description --------------------------------------------------------
## This file runs our primary OLS regression, a normalized regression, and a random effects model.


# Load packages -----------------------------------------------------------

library(tidyverse)
library(taRifx)
library(car)
library(xtable)
library(stargazer)
library(BBmisc)
library(lme4)
library(dplyr)


# Load full data set  --------------------------------------------------------------

data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)

data <- japply( data, which(sapply(data, typeof)=="integer"), as.double ) # convert data type to double to enable regression


# Run primary OLS regression ----------------------------------------------

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

## Clean regression coefficient names
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
                              "GDP Per Capita",
                               "Polity Score",
                               "Hospital Beds per 1000 People",
                               "Population Density (Log)")

summary(fit)
stargazer(fit)


# Run normalized OLS regression -----------------------------------------------

norm_data <- normalize(data, method = "standardize")

norm_fit <- lm(stock_change ~ stringency_ra +
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

## Clean regression coefficient names

names(norm_fit$coefficients) <- c("Intercept",
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
                             "GDP Per Capita",
                             "Polity Score",
                             "Hospital Beds per 1000 People",
                             "Population Density (Log)")

summary(norm_fit)
xtable(summary(norm_fit))


# Run random effect model -------------------------------------------------

re_fit <- lmer(stock_change ~ stringency_ra +
                 (1 | Country) + 
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
                 log(gdp_per_capita) +
                 polity +
                 hospital_beds_per_thousand +
                 population_density_log
               , data=data)

summary(re_fit)
xtable(summary(re_fit))