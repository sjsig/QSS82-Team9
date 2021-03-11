library(lme4)
library(tidyverse)

# Read Data ---------------------------------------------------------------

data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)



# Random Effects Model ----------------------------------------------------



fit_re <- lmer(stock_change ~ stringency_ra +
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
            gdp_per_capita_log +
            polity +
            hospital_beds_per_thousand +
            population_density_log
          , data=data)

names(coef(fit_re)$Country) <- c("Intercept",
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



summary(fit_re)
xtable(summary(fit_re))



# summary(data)
