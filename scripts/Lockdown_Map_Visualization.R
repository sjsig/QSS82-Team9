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

# get world map
wmap <- getMap(resolution="low")

# small edits
wmap <- spTransform(wmap, CRS("+proj=robin")) # reproject
wmap <-   subset(wmap, !(NAME %like% "Antar")) # Remove Antarctica

# Clean Stock Data
data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)



data <- data %>%
  filter(Date >= "2020-01-26" & Date < "2021-01-01") %>%
  select(Country, Date, stringency_index)


data <- data %>%
  rename(country_iso3c = Country)


spread_data <- data %>%
  group_by(country_iso3c) %>%
  arrange(Date) %>%
  ungroup() %>%
  spread(country_iso3c, stringency_index)

data <- spread_data %>% gather(country_iso3c, stringency_index, ARG:ZAF) %>%
  group_by(country_iso3c) %>%
  mutate(stringency_index = ifelse(is.na(stringency_index), na.locf(stringency_index), stringency_index))

centroids <- gCentroid( wmap , byid=TRUE, id = wmap@data$ISO3)
centroids <- data.frame(centroids)
setDT(centroids, keep.rownames = TRUE)[]
setnames(centroids, "rn", "country_iso3c")

# join data to map
wmap_df <- fortify(wmap, region = "ISO3")
wmap_df <- left_join(wmap_df, data, by = c('id'='country_iso3c'))        # data
wmap_df <- left_join(wmap_df, centroids, by = c('id'='country_iso3c')) # centroids


# plot
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

# save gif
gg_animate(o, "lockdown_map.gif", title_frame =T, 
           ani.width=1600, ani.height=820, dpi=800, interval = .1)


