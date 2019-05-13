setwd("~/Cal/Spring2019/Stat222/sf-crime")

census_tract_2009_2017 <- read_csv("data/census_tract_2009_2017.csv",
                                              col_types = cols(GEOID=col_character()))

## pad
census_tract_2009_2017$GEOID <- str_pad(census_tract_2009_2017$GEOID, 
                                        width=11, side='left', pad='0')

clusters = unique(census_tract_2009_2017 %>% select(GEOID, cluster_id_10))

library(leaflet)
library(tigris)
sf_map <- tracts(state = "CA", county='San Francisco', cb =TRUE)

map_data = geo_join(sf_map, clusters, by_sp="GEOID", by_df="GEOID",how='left')

indx = which(map_data@data$ALAND>0)
map_data@polygons <- map_data@polygons[indx]
map_data@data <- map_data@data[indx,]

# dop next smallest areas of water...
water_size = sort((as.numeric(map_data@data$AWATER)), decreasing=TRUE)[1:3]
indx_match = which(as.numeric(map_data@data$AWATER) %in% water_size)
map_data@polygons <- map_data@polygons[-indx_match]
map_data@data <- map_data@data[-indx_match,]

pal <- colorFactor("Spectral", domain = factor(map_data@data$cluster_id_10))

leaf_map <- leaflet(map_data) %>% addProviderTiles("CartoDB.Positron") %>% 
  fitBounds(lng1=-122.47, lng2=-122.37, lat1=37.70, lat2=37.82) %>%
  addPolygons(fillColor = ~pal(factor(cluster_id_10)),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7) 
leaf_map

library(tidyverse)
mapped_data2010_2018 <- read_csv("data/crime_with_census_ids.csv")

grouped_data = mapped_data2010_2018 %>% group_by(GEOID_tract, year) %>%
  summarize(count=n()) %>% filter(!is.na(GEOID_tract) & year>2009)

ggplot(data=grouped_data) + geom_histogram(aes(x=count)) + theme_bw() +
  xlab("Yearly Crime Count by Census Tract") + ylab("Count")

ggplot(data=grouped_data) + geom_histogram(aes(x=log(count))) + theme_bw() +
  xlab("Log(Yearly Crime Count by Census Tract)") + ylab("Count")

ggplot(data=grouped_data) + geom_histogram(aes(x=sqrt(count))) + theme_bw() +
  xlab("sqrt(Yearly Crime Count by Census Tract)") + ylab("Count")

