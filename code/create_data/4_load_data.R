#####
#### Author: Andrea Bonilla
#### 
#setwd("~/Cal/Spring2019/Stat222")

#source("sf-crime/code/create_data/1_load_crime_data.R", local=TRUE)
## data is original data

## load mapped data 
setwd("~/Cal/Spring2019/Stat222/sf-crime")
library(tidyverse)
mapped_data2010_2018 <- read_csv("data/crime_with_census_ids.csv")
## load census data
# census_block_2013_2017 <- read_csv("data/census_block_2013_2017.csv",
#                                    col_types = cols(GEOID=col_character()))
census_tract_2009_2017 <- read_csv("data/census_tract_2009_2017.csv",
                                    col_types = cols(GEOID=col_character()))

## pad
census_tract_2009_2017$GEOID <- str_pad(census_tract_2009_2017$GEOID, 
                                        width=11, side='left', pad='0')

spatial_data_no_years <- read_csv("data/spatial_features_noyear.csv")
names(spatial_data_no_years)[1] <- "GEOID"


clean_data_url <- "https://raw.githubusercontent.com/malvikarajeev/sfcrimeanalysis/master/final_clean_data.csv"
additional_data <- read_csv(clean_data_url) %>% select(GEOID, year, quarter,
                                                       N_housing, N_calls_311)
additional_data$GEOID <- str_pad(additional_data$GEOID,
                                 width=11, side='left', pad='0')
# number of geos...soem dont have any census vars
length(unique(additional_data$GEOID))

keep_geos = unique(additional_data$GEOID)


