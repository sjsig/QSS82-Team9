# Load packages -----------------------------------------------------------

library(tidyverse)
library( taRifx )
library(car)
library(xtable)
library(stargazer)
library(extrafont)
library(broom)
library(dplyr)
library(ggplot2)
# Import Theme ------------------------------------------------------------


## Import non-system fonts (only import if you have to because ## it takes a long time and makes your computer go crazy)

font_import()

## Put fonts into data frame so that you can view new fonts you ## have access to and how R stores them

fonts <- fonttable()
fonts

## theme – to add to ggplot simply type + t9theme after plot

t9theme <- theme(text = element_text(family = "Times New Roman"), legend.title = element_blank(), panel.grid = element_line(size = .3, colour = "black"), 
                 panel.background = element_rect(fill = "white"), plot.subtitle = element_text(size = 10), plot.title = element_text(size = 16)
                 , axis.title = element_text(size = 10), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
                 panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line(linetype = "dotted"), axis.ticks = element_blank(), 
                 legend.key = element_blank())











# Create Plot -------------------------------------------------------------

results <- tidy(fit)
summary(fit)



fit_cis_95 <- confint_tidy(fit, conf.level = 0.95, func = stats::confint) %>%
  rename_all(function(x){paste(x, "_95", sep="")})

fit_cis_90 <- confint_tidy(fit, conf.level = 0.90, func = stats::confint) %>%
  rename_all(function(x){paste(x, "_90", sep="")})

results <- bind_cols(results, 
                     fit_cis_95, 
                     fit_cis_90) %>%
  rename(Variable = term,
         Coefficient = estimate,
         SE = std.error) %>%
  filter(Variable != "(Intercept)")

results <- results %>%
  filter(Variable %in% c("Lockdown Severity", "Lockdown Severity (Day N-1)", "Lockdown Severity (Day N-2)", "Lockdown Severity (Day N-3)", "Total COVID-19 Cases Per Million People",
                     "Oil Spot Prices", "Healthcare Stimulus Spending Pct of GDP"))


ggplot(results, aes(x = Variable, y = Coefficient)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = Variable, 
                 y = Coefficient)) + 
  geom_linerange(aes(x = Variable, 
                     ymin = conf.low_90,
                     ymax = conf.high_90),
                 lwd = 1) +
  geom_linerange(aes(x = Variable, 
                     ymin = conf.low_95,
                     ymax = conf.high_95),
                 lwd = 1/2) + 
  ggtitle("Confidence Intervals of Key Variables") +
  theme(plot.title = element_text(hjust = 0.5))
  coord_flip() +
  t9theme

