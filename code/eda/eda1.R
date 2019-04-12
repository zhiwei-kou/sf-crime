## change of overall crime rates by census btwn 2010-2017
## change of overall crime counts by census btwn 2010-2017
## yearly crime rates and then yearly crime rate changes; distribution

census_tracts_small <- census_tract_2009_2017[,c(1,2,3,4)]

grouped_mapped_data <- mapped_data2010_2018 %>% 
  group_by(year, GEOID_tract) %>% summarize(count=n())

grouped_mapped_data <- left_join(grouped_mapped_data, census_tracts_small,
                                 by=c("GEOID_tract"="GEOID", "year"))
grouped_mapped_data <- grouped_mapped_data %>% mutate(rate=count/Estimate_Total)

library(ggplot) 

ggplot(data=grouped_mapped_data) + geom_histogram(aes(x=count)) + theme_bw()
ggplot(data=grouped_mapped_data) + geom_histogram(aes(x=log(count))) + theme_bw()

ggplot(data=grouped_mapped_data) + geom_histogram(aes(x=rate)) + theme_bw()
ggplot(data=grouped_mapped_data) + geom_histogram(aes(x=log(rate))) + theme_bw()

## function for % change for 2 y values based on start/stop
rate_of_change_basic <- function(data, start, stop){
  tmp = data %>% filter(year==start|year==stop)
  
  pct_change = function(x) (x/lag(x)-1)*100
  
  tmp2 = tmp[,-2] %>% mutate_all(funs(pct_change))
  tmp2$year  = tmp$year
  tmp2$GEOID_tract = tmp$GEOID_tract
  
  ## ggplot to keep changes
  map_changes = tmp2 %>% filter(year==stop)
  p = ggplot(data=map_changes) + geom_histogram(aes(x=rate)) + theme_bw()
  p2 = ggplot(data=map_changes) + geom_histogram(aes(x=log(rate))) + theme_bw()
  p3 = ggplot(data=map_changes) + geom_histogram(aes(x=count)) + theme_bw()
  p4 = ggplot(data=map_changes) + geom_histogram(aes(x=log(count))) + theme_bw()
  print(p)
  print(p2)
  print(p3)
  print(p4)
  return(list(change_data=map_changes, orig_data=tmp))
}

change_data = rate_of_change_basic(grouped_mapped_data[,-4], 2010, 2017)
counts = change_data$orig_data
percent_change = change_data$change_data

library(leaflet)
library(tigris)
library(rgdal)
library(tidycensus)
map_sf <- tracts("CA","San Francisco", year=2016) # hasn't changed since 2010

map_data <- geo_join(map_sf, percent_change, 
                     by_sp="GEOID", by_df="GEOID_tract",
                     how='left')

pal <- colorQuantile("Spectral", domain = log(map_data@data$rate))
pal <- colorBin("Spectral", domain = log(map_data@data$rate),
                bins=summary(log(map_data@data$rate)))

leaflet(map_data) %>% addProviderTiles("CartoDB.Positron") %>% 
  fitBounds(lng1=-122.47, lng2=-122.37, lat1=37.70, lat2=37.82) %>%
  addPolygons(fillColor = ~pal(log(rate)),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7) %>%
  addLegend(pal = pal, values = ~log(rate), opacity = 0.7, title = NULL,
            position = "bottomright")
