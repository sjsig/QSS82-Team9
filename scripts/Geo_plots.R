# Load packages -----------------------------------------------------------

library(tidyverse)
library(car)
library(xtable)
library(stargazer)
library(broom)
library(ggplot2)
library(dplyr)
library(maps)
library(viridis)
library(countrycode)
library(gifski)
library(rworldmap)

theme_set(theme_void())

# Load data  --------------------------------------------------------------

data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)

data <- data %>%
  filter(Date < "2021-01-01")


# Build coefficients dataset---------------------------------------------------------------------

countries <- unique(data$Country)
countries <- countries[!(countries %in% c("HKG", "TUR"))]

coefficients <- data.frame()

for (country in countries){
  print(country)
  country_data <- data %>%
    filter(Country == country) %>%
    select(stock_change, stringency_ra, services, polity ,frac_DPI, human_development_index,
             residential, population_density, oil_price,hospital_beds_per_thousand, new_cases_smoothed_per_million ,retail_and_recreation, stimulus_spending_pct_gdp)
  
  for(col in colnames(country_data)){
    # print(summary(country_data[col]))
  }
  
  
  fit <- lm(stock_change ~ stringency_ra + polity + frac_DPI + human_development_index + population_density + oil_price + hospital_beds_per_thousand + new_cases_smoothed_per_million  + stimulus_spending_pct_gdp, data = country_data)

  
  coeff <- tidy(fit) %>%
    select(term, estimate) %>%
    spread(term, estimate) %>%
    mutate(region = country)
  
  coefficients <- rbind(coefficients, coeff)
  
}
  




# Plot country coefficients  ----------------------------------------------

world_map <- getMap(resolution="low") %>%
  mutate(region = countrycode(region, origin = 'country.name', destination = 'iso3c') ) 
coeff_map <- left_join(world_map,coefficients, by = "region")

coeff_list <- colnames(coefficients)
coeff_list <- coeff_list[!(coeff_list %in% c("(Intercept)", "region"))]

# for (coeff in coeff_list){
#   print(coeff)
#   plot <- ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
#     geom_polygon(aes(fill=coeff), colour = "white") +
#     scale_fill_viridis_c(option = "C", na.value = "lightgray")
#   
#   filename <- paste("./plots/",coeff,"_map.pdf", sep="")
#   ggsave(filename, plot = plot)
#     
# }


colMax <- function(data) sapply(data, max, na.rm = TRUE)
max <- colMax(coefficients[coeff_list])
max_coeff <- max(max)
colMin <- function(data) sapply(data, min, na.rm = TRUE)
min <- colMin(coefficients[coeff_list])
min_coeff <- min(min)


# stringency_ra 

ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=stringency_ra), colour = "white") +
  scale_fill_continuous( low="#CAD7EB", high="#042E6E", guide="colorbar", na.value="lightgray") +
  # scale_fill_viridis_c(option = "C", na.value = "lightgray") +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  labs(fill="Stringency Coefficient", title = "Regression Coefficient on Independent Variable by Country", subtitle = "IV is a 7-day rolling average of Oxford's government stringency index")

ggsave("./plots/stringency_ra_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/stringency_ra_map.png", width=11, height=8.5, units="in")

# covid_rate
ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=covid_rate), colour = "white") +
  scale_fill_viridis_c(option = "C", na.value = "lightgray")

ggsave("./plots/covid_rate_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/covid_rate_map.png", width=11, height=8.5, units="in")

# oil_price
ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=oil_price), colour = "white") +
  scale_fill_viridis_c(option = "C", na.value = "lightgray")

ggsave("./plots/oil_price_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/oil_price_map.png", width=11, height=8.5, units="in")

# stringency_index
ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=stringency_index), colour = "white") +
  scale_fill_viridis_c(option = "C", na.value = "lightgray", limits = range(min_coeff, max_coeff))

ggsave("./plots/stringency_index_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/stringency_index_map.png", width=11, height=8.5, units="in")

# stringency_index_m1
ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=stringency_index_m1), colour = "white") +
  scale_fill_viridis_c(option = "C", na.value = "lightgray", limits = range(min_coeff, max_coeff))

ggsave("./plots/stringency_index_m1_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/stringency_index_m1_map.png", width=11, height=8.5, units="in")

# stringency_index_m2
ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=stringency_index_m2), colour = "white") +
  scale_fill_viridis_c(option = "C", na.value = "lightgray", limits = range(min_coeff, max_coeff))

ggsave("./plots/stringency_index_m2_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/stringency_index_m2_map.png", width=11, height=8.5, units="in")

# stringency_index_m3
ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=stringency_index_m3), colour = "white") +
  scale_fill_viridis_c(option = "C", na.value = "lightgray", limits = range(min_coeff, max_coeff))

ggsave("./plots/stringency_index_m3_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/stringency_index_m3_map.png", width=11, height=8.5, units="in")

# stringency_index_m4
ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=stringency_index_m4), colour = "white") +
  scale_fill_viridis_c(option = "C", na.value = "lightgray", limits = range(min_coeff, max_coeff))

ggsave("./plots/stringency_index_m4_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/stringency_index_m4_map.png", width=11, height=8.5, units="in")

# stringency_index_m5
ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=stringency_index_m5), colour = "white") +
  scale_fill_viridis_c(option = "C", na.value = "lightgray", limits = range(min_coeff, max_coeff))

ggsave("./plots/stringency_index_m5_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/stringency_index_m5_map.png", width=11, height=8.5, units="in")

# stringency_index_m6
ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=stringency_index_m6), colour = "white") +
  scale_fill_viridis_c(option = "C", na.value = "lightgray", limits = range(min_coeff, max_coeff))

ggsave("./plots/stringency_index_m6_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/stringency_index_m6_map.png", width=11, height=8.5, units="in")

# stringency_index_m7
ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=stringency_index_m7), colour = "white") +
  scale_fill_viridis_c(option = "C", na.value = "lightgray", limits = range(min_coeff, max_coeff))

ggsave("./plots/stringency_index_m7_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/stringency_index_m7_map.png", width=11, height=8.5, units="in")

# stringency_index_m8
ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=stringency_index_m8), colour = "white") +
  scale_fill_viridis_c(option = "C", na.value = "lightgray", limits = range(min_coeff, max_coeff))

ggsave("./plots/stringency_index_m8_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/stringency_index_m8_map.png", width=11, height=8.5, units="in")

# stringency_index_m9
ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=stringency_index_m9), colour = "white") +
  scale_fill_viridis_c(option = "C", na.value = "lightgray", limits = range(min_coeff, max_coeff))

ggsave("./plots/stringency_index_m9_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/stringency_index_m9_map.png", width=11, height=8.5, units="in")

# stringency_index_m10
ggplot(coeff_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=stringency_index_m10), colour = "white") +
  scale_fill_viridis_c(option = "C", na.value = "lightgray", limits = range(min_coeff, max_coeff))

ggsave("./plots/stringency_index_m10_map.pdf", width=11, height=8.5, units="in")
ggsave("./plots/stringency_index_m10_map.png", width=11, height=8.5, units="in")



plot_files <- c("./plots/stringency_index_map.png",
                "./plots/stringency_index_m1_map.png",
                "./plots/stringency_index_m2_map.png",
                "./plots/stringency_index_m3_map.png",
                "./plots/stringency_index_m4_map.png",
                "./plots/stringency_index_m5_map.png",
                "./plots/stringency_index_m6_map.png",
                "./plots/stringency_index_m7_map.png",
                "./plots/stringency_index_m8_map.png",
                "./plots/stringency_index_m9_map.png",
                "./plots/stringency_index_m10_map.png"
                )

gifski(plot_files, gif_file = "./plots/animation.gif", width = 800, height = 600, delay = 1)

