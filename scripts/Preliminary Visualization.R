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
gdp <- read_csv("OECD_Quarterly_GDP.csv")

## changing csv character formatted dates to date objects
unemp$TIME <- as.Date(paste(unemp$TIME,"-01",sep=""), format = "%Y-%m-%d")

## cleaning date column
gdp$TIME <- as.yearqtr(gsub( "Q", "", gdp$TIME))

## standardizes different EU labels pre and post-BREXIT
unemp$LOCATION <- revalue(unemp$LOCATION, c("EU27_2020"="EU", "EU28"="EU"))
gdp$LOCATION <- revalue(gdp$LOCATION, c("EU27_2020"="EU"))

## create a secondary version of the data that only contains economic groupings of countries
groupedUnemp <- unemp %>%
  filter(LOCATION == c("OECD", "EU", "G-7"))

grouped_gdp <- gdp %>%
  filter(LOCATION == c("OECD", "EU", "G-7"))

## theme creation for future use - ONLY run font import if you need to take an image of a plot
font_import()
fonts <- fonttable()
fonts

t9theme <- theme(text = element_text(family = "Times New Roman"), legend.title = element_blank(), panel.grid = element_line(size = .3, colour = "black"), 
                 panel.background = element_rect(fill = "white"), plot.subtitle = element_text(size = 10), plot.title = element_text(size = 16)
                 , axis.title = element_text(size = 10), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
                  panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line(linetype = "dotted"), axis.ticks = element_blank(), 
                 legend.key = element_blank())

## overall plot and sub-grouping plot
ggplot(unemp, aes(x = TIME, y = Value, color = LOCATION)) +
  geom_line() +
  labs(x = "Date", y = "Unemployment Rate (Percent)", title = "Unemployment Rate Across OECD Countries", 
       subtitle = "Tracking unemployment in OECD countries from late 2018 through 2020") +
  t9theme

ggplot(groupedUnemp, aes(x = TIME, y = Value, color = LOCATION)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Unemployment Rate (Percent)", title = "Unemployment Rate Across Major Economic Groupings",
       subtitle = "Tracking unemployment in the G-7, EU, and OECD on a composite level from late 2018 through 2020") +
  scale_color_manual(values = c("#fbdf6c", "#d03d35", "#437ab8")) +
  scale_x_date(date_breaks = "3 months") +
  scale_y_continuous(limits = c(0, 9), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  t9theme

ggplot(gdp, aes(x = TIME, y = Value, color = LOCATION)) +
  geom_line() +
  scale_x_yearqtr(format = "%Y-Q%q") +
  labs(x = "Quarter", y = "Change in Quarterly GDP (Percent)", title = "GDP Percent Change from Preceding Quarter Across OECD Countries") +
  t9theme

ggplot(grouped_gdp, aes(x = TIME, y = Value, color = LOCATION)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#fbdf6c", "#d03d35", "#437ab8")) +
  scale_x_yearqtr(format = "%Y-Q%q") +
  labs(x = "Quarter", y = "Change in Quarterly GDP (Percent)", title = "GDP Percent Change from Preceding Quarter Across Major Economic Groupings") +
  t9theme
