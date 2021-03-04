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

data <- data %>%
  filter(Date < "2021-01-01")


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
                urban_pop +
                one_yr_unemp_bene +
                oil_price +
                CCI +
                stimulus_spending_pct_gdp +
                gdp_per_capita +
                polity +
                hospital_beds_per_thousand +
                population_density
              , data=data)

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
                urban_pop +
                one_yr_unemp_bene +
                oil_price +
                CCI +
                stimulus_spending_pct_gdp +
                gdp_per_capita +
                polity +
                hospital_beds_per_thousand +
                population_density
              , data=data)

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

med.out <- mediate(med.fit, out.fit, treat = "new_cases_smoothed_per_million", mediator = "stringency_ra", sims = 300, boot = TRUE)

summary(med.out)
plot(med.out)

#ADE crossing zero and ACME not crossing zero -> estimated indirect effect is strong - effect of x on y is totally or partially mediated
# check histograms, and if skewed consider logs of those variables and see how that runs (log of covid rates, try this out)


# Sensitivity analysis. Test against possible uncontrolled confounders. 
# Ignorability assumption. 

sens <- medsens(med.out) 

# Plot of the sensitivity analysis. 

plot(sens)
plot(sens, sens.par = "R2", r.type = "total", sign.prod = "positive")


