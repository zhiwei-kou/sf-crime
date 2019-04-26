setwd("~/Cal/Spring2019/Stat222/sf-crime")

library(tigris)
library(sp)

## everything the same after 2010
join_mapLook<- function(data, tract_map, block_map){
  
  data1 = data[,c("lng","lat")]
  spatial_data  <- SpatialPointsDataFrame(coordinates(data1), data)
  
  proj4string(spatial_data) <- proj4string(tract_map)
  data$GEOID_tract <-  over(spatial_data, tract_map, returnList=FALSE)$GEOID # just grab geoid
  
  proj4string(spatial_data) <- proj4string(block_map)
  data$GEOID_block <-  over(spatial_data, block_map, returnList=FALSE)$GEOID # just grab geoid
  return(data)
}

mapLookup_noyear <-  function(data){
  tract_map <- tracts(state = "CA", county='San Francisco', cb =TRUE, year = 2016)
  block_map <- block_groups(state = "CA", county='San Francisco', cb =TRUE, year = 2016)
  
    mapped_data <- join_mapLook(data, tract_map, block_map)
  return(mapped_data)
}

# download zip from https://www.sfmta.com/reports/gtfs-transit-data

transit_stops <- read_csv("data/google_transit/stops.txt",
                         col_types=cols(
                           stop_id = col_double(),
                           stop_code = col_double(),
                           stop_name = col_character(),
                           stop_desc = col_character(),
                           stop_lat = col_double(),
                           stop_lon = col_double(),
                           zone_id = col_character(),
                           stop_url = col_character()
                         ))
names(transit_stops)[5:6] <- c("lat","lng")

transit_stops <-  mapLookup_noyear(transit_stops) 

#https://data.sfgov.org/Economy-and-Community/Schools/tpp3-epx2

school_data <- read_csv("data/Schools.csv")

library(stringr)

school_data <- school_data %>%
  separate("Location 1",
           c("lat","lng"), ",")

pattern <- "CA|\n|\\(|\\)"
school_data$lat <-as.numeric(gsub(pattern, "", school_data$lat))
school_data$lng <-as.numeric(gsub(pattern, "", school_data$lng))

school_data <- mapLookup_noyear(school_data)

## group by geo_blocks

summary_transit <- transit_stops %>% filter(!is.na(GEOID_tract))  %>%
  group_by(GEOID_tract) %>%
  summarize(mta_stops=n())

## group by geo_tracts and type of schools
wide_school_data <- school_data %>% group_by(`CCSF Entity`, GEOID_tract) %>%
  summarize(count=n()) %>% spread(`CCSF Entity`, count) 

overall_school_data <- school_data %>% group_by( GEOID_tract) %>%
  summarize(count=n())

wide_school_data <- full_join(wide_school_data, overall_school_data,
                              by = "GEOID_tract")

## get schools and merge to summary transit
spatial_features_data <- full_join(summary_transit, 
                                   wide_school_data,
                                   by = "GEOID_tract"
                                   )

spatial_features_data[is.na(spatial_features_data)] <- 0


write_csv(spatial_features_data, "data/spatial_features_noyear.csv")



