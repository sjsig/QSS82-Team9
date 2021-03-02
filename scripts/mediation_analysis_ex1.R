# Mediation analysis example. R
# Prof Robert A. Cooper

# Probably most useful to do mediation analysis in...

library(mediation)

# Can also be done through...
library(lavaan) # For latent variables and structural equations models. 

## Example data. 

data("framing", package = "mediation")
glimpse(framing)

# mediation package accepts many different model types. 
# This example: one linear model, one probit. 

med.fit <- lm(emo ~ treat + age + educ + gender + income, data= framing)
out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender +income, data = framing, family = binomial("probit"))

# The mediate function. Two versions. 

# ACME = average causal mediation effect. Estimated indirect effect. 
# ADE = average direct effect. 
# Indirect effect represented by (coefficient X->M (mod 1))*(coefficient M->Y(mod2))


med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo", sims = 1000, boot = TRUE)

summary(med.out)
plot(med.out)


# Sensitivity analysis. Test against possible uncontrolled confounders. 
# Ignorability assumption. 

sens <- medsens(med.out)

# Plot of the sensitivity analysis. 

plot(sens)
plot(sens, sens.par = "R2", r.type = "total", sign.prod = "positive")


