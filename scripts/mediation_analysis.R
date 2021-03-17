# File Description --------------------------------------------------------
## This file runs our mediation and sensitivity analyses.


# Load packages -----------------------------------------------------------

library(mediation)
library(lavaan)
library(tidyverse)
library(taRifx)
library(xtable)
library(stargazer)

# Load data ---------------------------------------------------------------

data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)

data <- japply( data, which(sapply(data, typeof)=="integer"), as.double ) # convert data type to double to enable regression

data <- data %>%
  arrange(Country, Date) %>%
  filter(!is.na(stock_change))


# Create models ------------------------------------------------------------

# Mediating model ----------------------------------------------------------

med.fit <- lm(stringency_ra ~
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

names(med.fit$coefficients) <- c("Intercept",
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

summary(med.fit)
xtable(summary(med.fit))

# Outome Model ------------------------------------------------------------

out.fit <- lm(stock_change ~ stringency_ra +
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


names(out.fit$coefficients) <- c("Intercept",
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

summary(out.fit)
xtable(summary(out.fit))


# Mediation function ------------------------------------------------------
# The mediate function. Two versions:
# ACME = average causal mediation effect. Estimated indirect effect. 
# ADE = average direct effect. 
# Indirect effect represented by (coefficient X->M (mod 1))*(coefficient M->Y(mod2))

med.out <- mediate(med.fit, out.fit, treat = "new_cases_smoothed_per_million", mediator = "stringency_ra", sims = 1000, boot = TRUE)

summary(med.out)
png("./final_plots/mediator_output.png", width=500, height=500)
plot(med.out)
dev.off()


# Sensitivity analysis. Test against possible uncontrolled confoun --------

sens <- medsens(med.out) # throws error if coefficient names are updated in regressions

png("./final_plots/sens_output_1.png", width=500, height=500)
plot(sens)
dev.off()

png("./final_plots/sens_output_2.png", width=500, height=500)
plot(sens, sens.par = "R2", r.type = "total", sign.prod = "positive")
dev.off()


# Plugging Values into Mediation Analysis ---------------------------------

covid_mean <- mean(data$new_cases_smoothed_per_million, na.rm = TRUE)
covid_median <- median(data$new_cases_smoothed_per_million, na.rm = TRUE)

lockdown_mean <- mean(data$stringency_ra, na.rm = TRUE)
lockdown_median <- median(data$stringency_ra, na.rm = TRUE)

mean_mediator_effect <- 5.957e-03 * covid_mean
median_mediator_effect <- 5.957e-03 * covid_median

mean_output_effect <- 1.756e-02 * lockdown_mean
median_output_effect <- 1.756e-02 * lockdown_median

total_mean_indirect_effect <- mean_output_effect * mean_mediator_effect
total_median_indirect_effect <- median_output_effect * median_mediator_effect




