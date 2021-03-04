# Load packages -----------------------------------------------------------
# Probably most useful to do mediation analysis in...
library(mediation)
# Can also be done through...
library(lavaan) # For latent variables and structural equations models. 
library(tidyverse)

# Load data ---------------------------------------------------------------

data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)

data <- data %>%
  filter(Date < "2021-01-01")


summary(data$stock_change)

data <- data %>%
  arrange(Country, Date) %>%
  filter(!is.na(stock_change))


# Create models ------------------------------------------------------------
# mediation package accepts many different model types. 

med.fit <- lm(stringency_ra ~
                median_age +
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
                health_spending_pct_gdp +
                stimulus_spending_pct_gdp +
                liquidity_support_pct_gdp +
                health_stimulus_spending_pct_gdp +
                gdp_per_capita +
                polity
              , data=data)

summary(med.fit)

out.fit <- lm(stock_change ~ stringency_ra +
                median_age +
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
                health_spending_pct_gdp +
                stimulus_spending_pct_gdp +
                liquidity_support_pct_gdp +
                health_stimulus_spending_pct_gdp +
                gdp_per_capita +
                polity
              , data=data)

summary(out.fit)

# Mediation function ------------------------------------------------------
# The mediate function. Two versions:
# ACME = average causal mediation effect. Estimated indirect effect. 
# ADE = average direct effect. 
# Indirect effect represented by (coefficient X->M (mod 1))*(coefficient M->Y(mod2))

med.out <- mediate(med.fit, out.fit, treat = "covid_rate", mediator = "stringency_ra", sims = 300, boot = TRUE)

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


