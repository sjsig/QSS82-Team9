# File Description --------------------------------------------------------
## This file creates our data tables to be used within our paper and the appendix..



# Load packages -----------------------------------------------------------

library(tibble)
library(psych)
library(stargazer)
library(tidyverse)

## Loading in Data
all_data <- read_csv(file = '/Users/williamwestbrook/Documents/QSS 82/QSS 82/data/all_data.csv' )

## Creating stringency data frame
stringency <- all_data$stringency_ra
stringency <- data.frame(stringency)
colnames(stringency) <- "Government Stringency Index"

## Creating stock change data frame
stocks <- all_data$stock_change
stocks <- data.frame(stocks)
colnames(stocks) <- "Percent Change in Stock Price"

## Binding the two and entering them into stargazer
stringency_stock_table <- cbind(stringency, stocks)
stargazer(stringency_stock_table)

## Compiling country-by-country table
countrystringencydata <- all_data[,c("Country", "stringency_ra")]
countrystringencydata <- countrystringencydata %>% 
  group_by(Country) %>%
  mutate(rn = row_number()) %>%
  pivot_wider(names_from = Country, values_from = stringency_ra)
countrystringencydata <- countrystringencydata[,-1]
countrystringencydata <- as.data.frame(countrystringencydata)
stargazer(countrystringencydata)


countrystockdata <- all_data[,c("Country", "stock_change")]
countrystockdata <- countrystockdata %>% 
  group_by(Country) %>%
  mutate(rn = row_number()) %>%
  pivot_wider(names_from = Country, values_from = stock_change)
countrystockdata <- countrystockdata[,-1]
countrystockdata <- as.data.frame(countrystockdata)
stargazer(countrystockdata)


## Control Table
control_data <- all_data[, c("agriculture", 
                             "industry", 
                             "manufacturing",
                               "services", 
                               "frac_DPI",
                               "aged_65_older",
                               "new_cases_smoothed_per_million", 
                               "new_deaths_smoothed_per_million",
                               "human_development_index",
                               "retail_and_recreation",
                               "residential",
                               "oil_price",
                               "stimulus_spending_pct_gdp",
                               "health_spending_pct_gdp",
                               "new_vaccinations_smoothed_per_million",
                               "gdp_per_capita",
                               "polity",
                               "hospital_beds_per_thousand",
                               "population_density_log")]
colnames(control_data) <- c("Agriculture Sector Share of GDP",
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
control_data <- as.data.frame(control_data)
stargazer(control_data)
                              