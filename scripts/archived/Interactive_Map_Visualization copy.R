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

# get world map
wmap <- getMap(resolution="low")

# small edits
wmap <- spTransform(wmap, CRS("+proj=robin")) # reproject
wmap <-   subset(wmap, !(NAME %like% "Antar")) # Remove Antarctica

# Clean Stock Data
stock_data <- read.csv(file = "./data/stock_data.csv", stringsAsFactors = FALSE)



stocks <- stock_data %>%
  filter(Date >= "2020-01-01" & Date < "2021-01-01")

stocks$Close <- as.numeric(stocks$Close)

stocks <- stocks %>%
  group_by(Country) %>%
  rename(country_iso3c = Country) %>%
  mutate(first_stock_close = Close[which(!is.na(Close))[1]]) %>%
  mutate(change_since_first = ((Close - first_stock_close) / first_stock_close) * 100)

stocks <- stocks %>%
  mutate(change_since_first = ifelse(change_since_first > 10, 10, change_since_first)) %>%
  mutate(change_since_first = ifelse(change_since_first < -10, -10, change_since_first))
  

spread_stocks <- stocks %>%
  group_by(country_iso3c) %>%
  arrange(Date) %>%
  ungroup() %>%
  select(Date, country_iso3c, change_since_first) %>%
  spread(country_iso3c, change_since_first)

stocks <- spread_stocks %>% gather(country_iso3c, change_since_first, ARG:ZAF) %>%
  group_by(country_iso3c) %>%
  mutate(change_since_first = ifelse(is.na(change_since_first), na.locf(change_since_first), change_since_first))

countdf <- stocks %>% group_by(country_iso3c) %>%
  summarise(count = n())
t <- stocks %>%
  filter(country_iso3c == "CHN")

u <- stocks %>%
  filter(country_iso3c == "USA")
# 
# stocks <- subset(stocks, !ave(change_since_first, Date, FUN = function(x) any(is.na(x))))
# 
# 
# 
# test <- stocks %>%
#   group_by(Date) %>%
#   summarize(count = n())
# 
# stocks <- stocks %>% 
#   group_by(Date) %>% 
#   filter(n() >= 33)
# 
# test2 <- stocks %>%
#   group_by(Date) %>%
#   summarize(count = n())

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
  scale_fill_gradient2(name = "Percent Change in Stock Price", low="red", high="darkgreen", 
                        guide="colorbar", mid = "white",na.value="lightgrey", limits = c(-10, 10)) +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(title = "Percent Change in Closing Stock Price Since Beginning of Pandemic, Current Day:") +
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
gg_animate(o, "test_map.gif", title_frame =T, 
           ani.width=1600, ani.height=820, dpi=800, interval = .1)


