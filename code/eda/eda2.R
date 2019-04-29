setwd("~/Cal/Spring2019/Stat222")

source("sf-crime/code/analysis/0_load_data.R", local=TRUE)
source("code/analysis/1_prepare_for_analysis.R", local=TRUE)

rm(list=(setdiff(ls(),c('census_tract_2009_2017',
                        'crime_assualt_data',
                        'crime_burglary_data',
                        'crime_robbery_data',
                        'crime_theft_data',
                        'crime_vehicle_theft_data'))))

library(leaflet)
library(tigris)
library(rgdal)
library(tidycensus)
library(stringr)


census_tracts_small <- census_tract_2009_2017[,c(1,2,3,4)]
census_tracts_small$GEOID <- str_pad(census_tracts_small$GEOID, 
                                     width=11, side='left', pad='0')

map_sf <- tracts("CA","San Francisco", year=2016) # hasn't changed since 2010


## function for % change for 2 y values based on start/stop
rate_of_change_basic <- function(data, start, stop, crime_type){
  tmp = data %>% filter(year==start|year==stop)
  
  pct_change = function(x) (x/lag(x)-1)*100
  
  tmp2 = tmp[,-2] %>% mutate_all(funs(pct_change))
  tmp2$year  = tmp$year
  tmp2$GEOID_tract = tmp$GEOID_tract
  
  ## ggplot to keep changes
  map_changes = tmp2 %>% filter(year==stop)
  p = ggplot(data=map_changes) + geom_histogram(aes(x=rate)) + theme_bw() +
    ggtitle(paste(crime_type, "Rate", sep=": "))
  p2 = ggplot(data=map_changes) + geom_histogram(aes(x=log(rate))) + 
    theme_bw() +
    ggtitle(paste(crime_type, "Log Rate", sep=": "))
  p3 = ggplot(data=map_changes) + geom_histogram(aes(x=count)) + 
    theme_bw() +
    ggtitle(paste(crime_type, "Count", sep=": "))
  p4 = ggplot(data=map_changes) + geom_histogram(aes(x=log(count))) +
    theme_bw() +
    ggtitle(paste(crime_type, "Log Count", sep=": "))
  print(p)
  print(p2)
  print(p3)
  print(p4)
  return(list(change_data=map_changes, orig_data=tmp))
}


## apply prev code to dif data sets
create_maps <- function(crime_data, census_tracts_small, map_sf,
                        type_of_crime){
  
  grouped_mapped_data <- crime_data %>% 
    group_by(year, GEOID_tract) %>% summarize(count=sum(crime)) %>%
    filter(!is.na(GEOID_tract))
  
  grouped_mapped_data <- left_join(grouped_mapped_data, census_tracts_small,
                                   by=c("GEOID_tract"="GEOID", "year"))
  grouped_mapped_data <- grouped_mapped_data %>% 
    mutate(rate=count/Estimate_Total)
  
  change_data = rate_of_change_basic(grouped_mapped_data[,-4], 2010, 2017,
                                     type_of_crime)
  counts = change_data$orig_data
  percent_change = change_data$change_data
  
  
  ## drop certain tracts
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
  
  
  no_nas <- map_data@data$count[!is.na(map_data@data$count)]
  bins <- summary(no_nas)[1:6]
  pal <- colorBin("Spectral", domain = map_data@data$count,
                  bins=bins)
  
  leaf_map <- leaflet(map_data) %>% addProviderTiles("CartoDB.Positron") %>% 
    fitBounds(lng1=-122.47, lng2=-122.37, lat1=37.70, lat2=37.82) %>%
    addPolygons(fillColor = ~pal(count),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7) %>%
    addLegend(pal = pal, values = ~count, 
              title=type_of_crime,
              opacity = 0.7,
              position = "bottomright")
  
  return(leaf_map)
  
  
}

assault_map <- create_maps(crime_assualt_data, census_tracts_small, map_sf,
            "Assault")

burglary_map <- create_maps(crime_burglary_data, census_tracts_small, map_sf,
            "Burglary")

robbery_map <- create_maps(crime_robbery_data, census_tracts_small, map_sf,
            "Robbery")

theft_map <- create_maps(crime_theft_data, census_tracts_small, map_sf,
            "Theft")

vehicle_theft_map <- create_maps(crime_vehicle_theft_data, 
                                 census_tracts_small, map_sf,
                                 "Vehicle Theft")
