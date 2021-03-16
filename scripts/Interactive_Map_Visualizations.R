# File Description --------------------------------------------------------
## This file runs our animations for our independent and dependent variables, lockdown severity and stock returns respectively.
## Modified code from https://gist.github.com/rafapereirabr/0d68f7ccfc3af1680c4c8353cf9ab345

# Load packages -----------------------------------------------------------

library(curl)
library(readxl)
library(data.table)
library(rworldmap)
library(ggplot2)
library(dplyr)
library(tweenr)
library(ggthemes)
library(viridis)
library(rgeos)
library(countrycode)
library(devtools)
install_github("dgrtwo/gganimate", ref = "26ec501")
library(gganimate)
library(rgdal)
library(rgeos)
if (!require(gpclib)) install.packages("gpclib", type="source")
library(gpclib)
library(maptools)
library(ggplot2)
library(dplyr)
library(zoo)
library(lazyeval)
library(tidyr)
devtools::install_github("bhaskarvk/colormap")
library(colormap)


# Lockdown Map Visualization ----------------------------------------------

# Initialize lockdown severity map ----------------------------------------------------------
wmap <- getMap(resolution="low")
wmap <- spTransform(wmap, CRS("+proj=robin")) # reproject
wmap <-   subset(wmap, !(NAME %like% "Antar")) # Remove Antarctica


# Clean data set --------------------------------------------------------

data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)


data <- data %>%
  filter(Date >= "2020-2-02" & Date < "2021-03-09") %>% # filter for relevant dates
  select(Country, Date, stringency_ra)


data <- data %>%
  rename(country_iso3c = Country)

## Rearrange data to prepare for merge
spread_data <- data %>%
  group_by(country_iso3c) %>%
  arrange(Date) %>%
  ungroup() %>%
  spread(country_iso3c, stringency_ra)

data <- spread_data %>% gather(country_iso3c, stringency_ra, AUS:ZAF) %>%
  group_by(country_iso3c) %>%
  mutate(stringency_index = ifelse(is.na(stringency_ra), na.locf(stringency_ra), stringency_ra))


# Initialize Centroids ----------------------------------------------------

centroids <- gCentroid( wmap , byid=TRUE, id = wmap@data$ISO3)
centroids <- data.frame(centroids)
setDT(centroids, keep.rownames = TRUE)[]
setnames(centroids, "rn", "country_iso3c")


# Join data to map --------------------------------------------------------

wmap_df <- fortify(wmap, region = "ISO3")
wmap_df <- left_join(wmap_df, data, by = c('id'='country_iso3c'))        # data
wmap_df <- left_join(wmap_df, centroids, by = c('id'='country_iso3c')) # centroids


# Plot map ----------------------------------------------------------------

o <- ggplot(data=wmap_df) +
  geom_polygon(aes(x = long, y = lat, group = group, fill=stringency_index, frame = Date), color="grey90") +
  scale_fill_continuous(name = "Stringency", low="#cad7eb", high="#042e6e", 
                        guide="colorbar",na.value="lightgrey") +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(title = "Government Stringency Index, Current Day:") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.05, size=25)) +
  theme(plot.caption = element_text(hjust = 0, color="gray40", size=15)) +
  coord_cartesian(xlim = c(-11807982, 14807978)) +
  theme( legend.position = c(.5, .08), 
         legend.direction = "horizontal", 
         legend.title.align = 0.5,
         title=element_text(size=30),
         legend.key.size = unit(1.3, "cm"),
         legend.title=element_text(size=15), 
         legend.text=element_text(size=13) ,
         text = element_text(family = "Times New Roman"),
         legend.key = element_blank())


# Save GIF ----------------------------------------------------------------

gg_animate(o, "lockdown_map_final.gif", title_frame =T, 
           ani.width=1600, ani.height=820, dpi=800, interval = .1)



# Stock Map Visualization -------------------------------------------------


# Initialize stock returns map ----------------------------------------------------

wmap <- getMap(resolution="low")
wmap <- spTransform(wmap, CRS("+proj=robin")) # reproject
wmap <-   subset(wmap, !(NAME %like% "Antar")) # Remove Antarctica



# Clean data set ----------------------------------------------------------

stocks <- stock_data %>%
  filter(Date >= "2020-01-01" & Date < "2021-03-09") # filter for relevant dates

stocks$Close <- as.numeric(stocks$Close) # convert to double


# Prepare data for merge by filling in NA values with lag -----------------

stocks <- stocks %>%
  group_by(Country) %>%
  rename(country_iso3c = Country) %>%
  mutate(first_stock_close = Close[which(!is.na(Close))[1]]) %>%
  mutate(change_since_first = Close - first_stock_close)


# Initialize centroids ----------------------------------------------------

centroids <- gCentroid( wmap , byid=TRUE, id = wmap@data$ISO3)
centroids <- data.frame(centroids)
setDT(centroids, keep.rownames = TRUE)[]
setnames(centroids, "rn", "country_iso3c")



# Join data to map --------------------------------------------------------

wmap_df <- fortify(wmap, region = "ISO3")
wmap_df <- left_join(wmap_df, stocks, by = c('id'='country_iso3c'))        # data
wmap_df <- left_join(wmap_df, centroids, by = c('id'='country_iso3c')) # centroids



# Plot map ----------------------------------------------------------------

o <- ggplot(data=wmap_df) +
  geom_polygon(aes(x = long, y = lat, group = group, fill=change_since_first, frame = Date), color="gray90") +
  geom_text(aes(x = x, y = y, label = round(change_since_first), frame = Date), hjust=0, vjust=0, size = 4.5) +
  scale_fill_viridis(name="Stock Change", begin = 0, end = 1, limits = c(0,1000000), na.value="gray99") +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(title = "Stock Change, ") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.05, size=25)) +
  theme(plot.caption = element_text(hjust = 0, color="gray40", size=15)) +
  coord_cartesian(xlim = c(-11807982, 14807978)) +
  theme( legend.position = c(.5, .08), 
         legend.direction = "horizontal", 
         legend.title.align = 0,
         legend.key.size = unit(1.3, "cm"),
         legend.title=element_text(size=17), 
         legend.text=element_text(size=13) )


# Save GIF ----------------------------------------------------------------

gg_animate(o, "stock_map_final.gif", title_frame =T, 
           ani.width=1600, ani.height=820, dpi=800, interval = .1)


