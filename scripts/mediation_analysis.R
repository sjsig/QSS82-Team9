# Load packages -----------------------------------------------------------
# Probably most useful to do mediation analysis in...
library(mediation)
# Can also be done through...
library(lavaan) # For latent variables and structural equations models. 
library(tidyverse)
library( taRifx )

# Load data ---------------------------------------------------------------

data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)

data <- japply( data, which(sapply(data, typeof)=="integer"), as.double )


summary(data$stock_change)

data <- data %>%
  arrange(Country, Date) %>%
  filter(!is.na(stock_change))


# Create models ------------------------------------------------------------
# mediation package accepts many different model types. 

# Original model ----------------------------------------------------------


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
                             "GDP Per Capita (Log)",
                             "Polity Score",
                             "Hospital Beds per 1000 People",
                             "Population Density (Log)")






summary(med.fit)



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
                             "GDP Per Capita (Log)",
                             "Polity Score",
                             "Hospital Beds per 1000 People",
                             "Population Density (Log)")

summary(out.fit)


# Step model (modified) ---------------------------------------------------

med.fit <- lm(stringency_ra ~ services + polity + frac_DPI + human_development_index +
                 residential + urban_pop + oil_price + hospital_beds_per_thousand + new_cases_smoothed_per_million + retail_and_recreation + stimulus_spending_pct_gdp, data = data)

summary(med.fit)

out.fit <- lm(stock_change ~ stringency_ra + services + polity + frac_DPI + human_development_index +
                residential + urban_pop + oil_price + hospital_beds_per_thousand + new_cases_smoothed_per_million + retail_and_recreation + stimulus_spending_pct_gdp, data = data)

summary(out.fit)



# med.fit <- lm(stringency_ra ~ 
#                 
#                 new_cases_smoothed_per_million 
#                 
#               , data=data)
# 
# summary(med.fit)
# 
# out.fit <- lm(stock_change ~ stringency_ra +
#                 new_cases_smoothed_per_million 
#               , data=data)
# 
# summary(out.fit)


# Mediation function ------------------------------------------------------
# The mediate function. Two versions:
# ACME = average causal mediation effect. Estimated indirect effect. 
# ADE = average direct effect. 
# Indirect effect represented by (coefficient X->M (mod 1))*(coefficient M->Y(mod2))

med.out <- mediate(med.fit, out.fit, treat = "new_cases_smoothed_per_million", mediator = "stringency_ra", sims = 1000, boot = TRUE)

summary(med.out)
png("./plots/mediator_output.png", width=500, height=500)
plot(med.out)
dev.off()

#ADE crossing zero and ACME not crossing zero -> estimated indirect effect is strong - effect of x on y is totally or partially mediated
# check histograms, and if skewed consider logs of those variables and see how that runs (log of covid rates, try this out)


# Sensitivity analysis. Test against possible uncontrolled confounders. 
# Ignorability assumption. 

sens <- medsens(med.out) # throws error if coefficient names are updated in regressions

# Plot of the sensitivity analysis. 

png("./plots/sens_output_1.png", width=500, height=500)
plot(sens)
dev.off()

png("./plots/sens_output_2.png", width=500, height=500)
plot(sens, sens.par = "R2", r.type = "total", sign.prod = "positive")
dev.off()


# Plugging Values into Mediation Analysis ---------------------------------

covid_mean <- mean(data$new_cases_smoothed_per_million, na.rm = TRUE)
covid_median <- median(data$new_cases_smoothed_per_million, na.rm = TRUE)

lockdown_mean <- mean(data$stringency_ra, na.rm = TRUE)
lockdown_median <- median(data$stringency_ra, na.rm = TRUE)

mean_mediator_effect <- 0.0061308 * covid_mean
median_mediator_effect <- 0.0061308 * covid_median

mean_output_effect <- 1.746e-02 * lockdown_mean
median_output_effect <- 1.746e-02 * lockdown_median

total_mean_indirect_effect <- mean_output_effect * mean_mediator_effect
total_median_indirect_effect <- median_output_effect * median_mediator_effect




