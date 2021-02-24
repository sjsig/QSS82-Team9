install.packages("mediation")
install.packages("lavaan")

library(mediation)
library(lavaan)

stock_df <- df %>% filter(Country %in% c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA"))

med.fit <- lm(Close ~ treat + age + educ + gender + income, data = stock_df)