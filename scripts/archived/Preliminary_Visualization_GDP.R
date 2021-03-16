## Team Nine Preliminary Visualization (GDP)
## Due Friday, February 12th 
## Rijul Garg, Colton Wagner, Joe Signorelli, Trout Westbrook

library(ggplot2)
install.packages('tidyverse')
install.packages("broom", type = "binary")
install.packages("dbplyr", type = "binary")
install.packages("modelr", type = "binary")
install.packages("extrafont")
library(tidyverse)
library(stringr)
library(plyr)
library(readr)
library(extrafont)
library(zoo)


## reading in csv file
gdp <- read_csv("OECD_Quarterly_GDP.csv")
head(unemp)

## cleaning date column
gdp$TIME <- as.yearqtr(gsub( "Q", "", gdp$TIME))

# standardizes different EU labels pre and post-BREXIT
gdp$LOCATION <- revalue(gdp$LOCATION, c("EU27_2020"="EU"))

## create a secondary version of the data that only contains economic groupings of countries
grouped_gdp <- gdp %>%
  filter(LOCATION == c("OECD", "EU27_2020", "G-7"))

## theme creation for future use
t9theme <- theme(text = element_text(family = "mono"), legend.title = element_blank(), panel.grid = element_line(color = "grey"), 
                 panel.background = element_blank())

## overall plot and sub-grouping plot
ggplot(gdp, aes(x = TIME, y = Value, color = LOCATION)) +
  geom_line() +
  scale_x_yearqtr(format = "%Y-Q%q") +
  labs(x = "Quarter", y = "Change in Quarterly GDP (Percent)", title = "GDP Percent Change from Preceding Quarter Across Major Economic Groupings") +
  t9theme

ggplot(grouped_gdp, aes(x = TIME, y = Value, color = LOCATION)) +
  geom_line() +
  scale_x_yearqtr(format = "%Y-Q%q") +
  labs(x = "Quarter", y = "Change in Quarterly GDP (Percent)", title = "Unemployment Rate Across Major Economic Groupings") +
  t9theme

