## change of overall crime rates by census btwn 2010-2017
## change of overall crime counts by census btwn 2010-2017
## yearly crime rates and then yearly crime rate changes; distribution

library(stringr)
census_tracts_small <- census_tract_2009_2017[,c(1,2,3,4)]
census_tracts_small$GEOID <- str_pad(census_tracts_small$GEOID, 
                                     width=11, side='left', pad='0')
grouped_mapped_data <- mapped_data2010_2018 %>% 
  group_by(year, GEOID_tract) %>% summarize(count=n())

grouped_mapped_data <- left_join(grouped_mapped_data, census_tracts_small,
                                 by=c("GEOID_tract"="GEOID", "year"))
grouped_mapped_data <- grouped_mapped_data %>% mutate(rate=count/Estimate_Total)

library(ggplot) 

ggplot(data=grouped_mapped_data) + geom_histogram(aes(x=count)) + theme_bw() + 
  xlab("Crime Count") + ylab("Frequency")
ggplot(data=grouped_mapped_data) + geom_histogram(aes(x=log(count))) + theme_bw()  + 
  xlab("Log Crime Count") + ylab("Frequency")

ggplot(data=grouped_mapped_data) + geom_histogram(aes(x=rate)) + theme_bw()  + 
  xlab("Crime Rate") + ylab("Frequency")
ggplot(data=grouped_mapped_data) + geom_histogram(aes(x=log(rate))) + theme_bw()  + 
  xlab("Log Crime Rate") + ylab("Frequency")

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

indx = which(map_data@data$ALAND>0)
map_data@polygons <- map_data@polygons[indx]
map_data@data <- map_data@data[indx,]

# dop next smallest areas of water...
water_size = sort((as.numeric(map_data@data$AWATER)), decreasing=TRUE)[1:3]
indx_match = which(as.numeric(map_data@data$AWATER) %in% water_size)
map_data@polygons <- map_data@polygons[-indx_match]
map_data@data <- map_data@data[-indx_match,]


## drop rows that don't have any land 
#map_data@data <- map_data@data[map_data@data$ALAND>0, ]

pal <- colorQuantile("Spectral", domain = log(map_data@data$rate))
pal <- colorBin("Spectral", domain = log(map_data@data$rate),
                bins=summary(log(map_data@data$rate))[1:6])

leaflet(map_data) %>% addProviderTiles("CartoDB.Positron") %>% 
  fitBounds(lng1=-122.47, lng2=-122.37, lat1=37.70, lat2=37.82) %>%
  addPolygons(fillColor = ~pal(log(rate)),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7) %>%
  addLegend(pal = pal, values = ~log(rate), 
            opacity = 0.7, title = NULL,
            position = "bottomright")

url311 <- "https://raw.githubusercontent.com/malvikarajeev/sfcrimeanalysis/master/weekly_crime_counts_with_varaibles.csv"
data311 <- read_csv(url311)

data311 %>% group_by(year, GEOID) %>% summarize(calls311=sum(N_calls_311))

data311 %>% group_by(year) %>% summarize(calls311=sum(N_calls_311))
