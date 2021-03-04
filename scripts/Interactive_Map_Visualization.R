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

# get world map
wmap <- getMap(resolution="low")

# small edits
wmap <- spTransform(wmap, CRS("+proj=robin")) # reproject
wmap <-   subset(wmap, !(NAME %like% "Antar")) # Remove Antarctica

# Clean Stock Data

stocks <- stock_data %>%
  dplyr::filter(Date >= "2020-01-01" & Date < "2021=0-01-10")

stocks$Close <- as.numeric(stocks$Close)

stocks <- stocks %>%
  group_by(Country) %>%
  rename(country_iso3c = Country) %>%
  mutate(first_stock_close = Close[which(!is.na(Close))[1]]) %>%
  mutate(change_since_first = Close - first_stock_close)

centroids <- gCentroid( wmap , byid=TRUE, id = wmap@data$ISO3)
centroids <- data.frame(centroids)
setDT(centroids, keep.rownames = TRUE)[]
setnames(centroids, "rn", "country_iso3c")

# join data to map
wmap_df <- fortify(wmap, region = "ISO3")
wmap_df <- left_join(wmap_df, stocks, by = c('id'='country_iso3c'))        # data
wmap_df <- left_join(wmap_df, centroids, by = c('id'='country_iso3c')) # centroids


# plot
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

# save gif
gg_animate(o, "output4020_old.gif", title_frame =T, 
           ani.width=1600, ani.height=820, dpi=800, interval = .1)


