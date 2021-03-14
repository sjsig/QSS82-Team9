## Appendix Data Viz Script

library(ggplot2)
library(tidyverse)
library(plyr)
library(grid)

all_data <- read_csv(file = '/Users/williamwestbrook/Documents/QSS 82/QSS 82/data/all_data.csv')
countrynames <- c("Australia", "Austria", "Belgium",
                      "Brazil", "Switzerland", "Germany",
                      "Spain", "Finland", "France",
                      "Great Britain", "Indonesia",
                      "India", "Ireland", "Italy",
                      "South Korea", "Lithuania", "Mexico",
                      "Netherlands", "Norway", "New Zealand",
                      "Portugal", "Sweden", "United States", "South Africa")

all_data$Country <- factor(all_data$Country)
all_data$Country <- revalue(all_data$Country, c("AUS" = "Australia", "AUT" = "Austria", "BEL" = "Belgium", "BRA" = "Brazil", "CHE" = "Switzerland", "DEU" = "Germany",
                                             "ESP" = "Spain", "FIN" = "Finland", "FRA" = "France",
                                             "GBR" = "Great Britain", "IDN" = "Indonesia",
                                             "IND" = "India", "IRL" = "Ireland", "ITA" = "Italy",
                                             "KOR" = "South Korea", "LTU" = "Lithuania", "MEX" = "Mexico",
                                             "NLD" = "Netherlands", "NOR" = "Norway", "NZL" = "New Zealand",
                                             "PRT" = "Portugal", "SWE" = "Sweden", "USA" = "United States", "ZAF" = "South Africa"))

for( k in 1:length(all_data$new_cases_smoothed_per_million)) {
  if(is.na(all_data$new_cases_smoothed_per_million[k]) == FALSE){
    if(all_data$new_cases_smoothed_per_million[k] < 0){
    all_data$new_cases_smoothed_per_million[k] = all_data$new_cases_smoothed_per_million*(-1)
    }
  }
}
tablecheck <- all_data %>%
  filter(Country == "Spain")


t9theme <- theme(text = element_text(family = "Times New Roman"), legend.title = element_blank(), panel.grid = element_line(size = .3, colour = "black"), 
                 panel.background = element_rect(fill = "white"), plot.subtitle = element_text(size = 10), plot.title = element_text(size = 16)
                 , axis.title = element_text(size = 10), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
                 panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line(linetype = "dotted"), axis.ticks = element_blank(), 
                 legend.key = element_blank(), title = element_text(), strip.background = element_rect(colour = "#1378d2", fill = "#1378d2"), 
                 strip.text = element_text(color = "white"), axis.text.x = element_text(size = 8, angle = 45, vjust = .70))

ggplot(all_data, aes(x = Date, y = new_cases_smoothed_per_million)) +
  geom_line(color = "#1378d2") +
  labs(x = "Date", y = "New COVID-19 Cases per Million People (7-Day Smoothed)", title = "COVID-19 Case Data for Nation-States in Study") +
  facet_wrap(vars(Country), nrow = 6, ncol = 4) +
  t9theme

ggplot(all_data, aes(x = Date, y = stringency_ra)) +
  geom_line(color = "#1378d2") +
  labs(x = "Date", y = "Government Stringency Index (7-Day Rolling Average)", title = "Lockdown Severity Data for Nation-States in Study") +
  facet_wrap(vars(Country), nrow = 6, ncol = 4) +
  t9theme

ggplot(all_data, aes(x = Date, y = stock_change)) +
  geom_line(color = "#1378d2") +
  labs(x = "Date", y = "Percent Change in Stock Price", title = "Stock Price Fluctuation Data for Nation-States in Study") +
  facet_wrap(vars(Country), nrow = 6, ncol = 4) +
  t9theme
