# Load packages -----------------------------------------------------------
# Probably most useful to do mediation analysis in...
library(mediation)
# Can also be done through...
library(lavaan) # For latent variables and structural equations models. 

# Load data ---------------------------------------------------------------

data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)

summary(data$stock_change)

data <- data %>%
  arrange(Country, Date)


# Create models ------------------------------------------------------------
# mediation package accepts many different model types. 
# This example: one linear model, one probit. 

med.fit <- lm(stringency_index ~ 
                aged_65_older +
                covid_rate + 
                human_development_index +
                median_age +
                life_expectancy +
                population_density +
                retail_and_recreation +
                residential +
                urban_pop +
                rural_pop +
                suburban_pop +
                CCI +
                oil_price, data=data)

out.fit <- lm(stock_change ~ stringency_index +
                stringency_index_m1 +
                aged_65_older +
                covid_rate +
                human_development_index +
                median_age +
                life_expectancy +
                population_density +
                retail_and_recreation +
                residential +
                urban_pop +
                rural_pop +
                suburban_pop +
                CCI +
                oil_price, data=data)

# Mediation function ------------------------------------------------------
# The mediate function. Two versions:
# ACME = average causal mediation effect. Estimated indirect effect. 
# ADE = average direct effect. 
# Indirect effect represented by (coefficient X->M (mod 1))*(coefficient M->Y(mod2))

med.out <- mediate(med.fit, out.fit, treat = "covid_rate", mediator = "stringency_index", sims = 1000, boot = TRUE)

summary(med.out)
plot(med.out)


# Sensitivity analysis. Test against possible uncontrolled confounders. 
# Ignorability assumption. 

sens <- medsens(med.out)

# Plot of the sensitivity analysis. 

plot(sens)
plot(sens, sens.par = "R2", r.type = "total", sign.prod = "positive")


