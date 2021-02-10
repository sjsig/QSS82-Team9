## Team Nine Preliminary Visualization
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

## reading in csv file
unemp <- read_csv("OECD_Unemployment.csv")
head(unemp)

## changing csv character formatted dates to date objects
unemp$TIME <- as.Date(paste(unemp$TIME,"-01",sep=""), format = "%Y-%m-%d")

# standardizes different EU labels pre and post-BREXIT
unemp$LOCATION <- revalue(unemp$LOCATION, c("EU27_2020"="EU", "EU28"="EU"))

## create a secondary version of the data that only contains economic groupings of countries
groupedUnemp <- unemp %>%
  filter(LOCATION == c("OECD", "EU", "G-7"))

## theme creation for future use
t9theme <- theme(text = element_text(family = "mono"), legend.title = element_blank(), panel.grid = element_line(color = "grey"), 
                 panel.background = element_blank())

## overall plot and sub-grouping plot
ggplot(unemp, aes(x = TIME, y = Value, color = LOCATION)) +
  geom_line() +
  labs(x = "Point in Time (Year-Month)", y = "Unemployment Rate (Percent)") +
  t9theme

ggplot(groupedUnemp, aes(x = TIME, y = Value, color = LOCATION)) +
  geom_line() +
  labs(x = "Point in Time (Year-Month)", y = "Unemployment Rate (Percent)", title = "Unemployment Rate Across Major Economic Groupings") +
  t9theme





## unemp$TIME <- as.Date(df$TIME, “%Y-%m”)
## unemp$TIME <- as.Date(paste(unemp$TIME,"-01",sep=""), format = "%Y-%m-%d")
