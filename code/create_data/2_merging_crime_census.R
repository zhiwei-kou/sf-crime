setwd("~/Cal/Spring2019/Stat222/sf-crime")

#####
#### Author: Andrea Bonilla
#### 

source("code/create_data/1_load_crime_data.R", local=TRUE)
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

mapLookup <-  function(data){
  mapped_data <- data.frame()
  tract_map <- tracts(state = "CA", county='San Francisco', cb =TRUE, year = 2016)
  block_map <- block_groups(state = "CA", county='San Francisco', cb =TRUE, year = 2016)
  
  for(y in 2010:2018){
    print(y)
    data0 = data %>% filter(year==y) %>% drop_na(lat, lng)
    mapped_data <- rbind(mapped_data, join_mapLook(data0, tract_map, block_map))
  }
  return(mapped_data)
}

mapped_data  <-  mapLookup(data) # note block_group isnt fine

## get na'
#after2009 <- data %>% filter(year>2009 & year<2018)
#na_crime <- data %>% filter(is.na(lat)| is.na(lng)) #%>% filter(year>2009 & year<2018)
#nrow(na_crime) + nrow(mapped_data) == nrow(after2009)
#mapped_data2 <- rbind(mapped_data, na_crime)

## ISSUE for 2018 and 2019
# 16  2018   5398   5398
# 17  2019    534    534

write_csv(mapped_data, "data/crime_with_census_ids.csv")

