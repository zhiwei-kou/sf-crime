#####
#### Author: Andrea Bonilla
#### 
setwd("~/Cal/Spring2019/Stat222")

source("sf-crime/code/create_data/1_load_crime_data.R", local=TRUE)
## data is original data

## load mapped data 
setwd("~/Cal/Spring2019/Stat222/sf-crime")
library(tidyverse)
mapped_data2010_2018 <- read_csv("data/crime_with_census_ids.csv")
## load census data
census_block_2013_2017 <- read_csv("data/census_block_2013_2017.csv",
                                   col_types = cols(GEOID=col_character()))
census_tract_2009_2017 <- read_csv("data/census_tract_2009_2017.csv",
                                   col_types = cols(GEOID=col_character()))

## pad
census_tract_2009_2017$GEOID <- str_pad(census_tract_2009_2017$GEOID, 
                                        width=11, side='left', pad='0')

spatial_data_no_years <- read_csv("data/spatial_features_noyear.csv")
names(spatial_data_no_years)[1] <- "GEOID"

url311 <- "https://raw.githubusercontent.com/malvikarajeev/sfcrimeanalysis/master/calls_311_2010_2011.csv"
#url311 <- "https://raw.githubusercontent.com/malvikarajeev/sfcrimeanalysis/master/weekly_crime_counts_with_varaibles.csv"
data311 <- read_csv(url311)
data311 <- data311[, which(!(names(data311) %in% c("X1")))]
names(data311)[4] <- "N_calls_311"

clean_data_url <- "https://raw.githubusercontent.com/malvikarajeev/sfcrimeanalysis/master/final_clean_data.csv"
additional_data <- read_csv(clean_data_url) %>% select(-X1) %>% select(-N)
additional_data$GEOID <- str_pad(additional_data$GEOID,
                                 width=11, side='left', pad='0')
# number of geos
length(unique(additional_data$GEOID))

# data311$quarter <- quarter(as.Date(paste(data311$year, 
#                                          str_pad(data311$week-1, 
#                                                  width = 2,
#                                                  side="left",
#                                                  pad="0"), 
#                                          1,
#                                          sep="-"), 
#                                    "%Y-%U-%u"))

# data311_quarter <- data311 %>% group_by(year, quarter, GEOID) %>% 
#   summarize(N_calls_311=sum(N_calls_311))

#data_common_metics <- data311  %>% 
#  select(-N_calls_311) #%>% select(-quarter) %>%
  # #select(-week) %>%
  # group_by(year,GEOID) %>%
  # summarise_at(vars(-group_cols()), mean)

#urlhousing <- "https://raw.githubusercontent.com/malvikarajeev/sfcrimeanalysis/master/housing_tracts.csv"
#urlgeos <- "https://raw.githubusercontent.com/malvikarajeev/sfcrimeanalysis/master/areas.csv"

# data_housing <- read_csv(urlhousing)
# data_housing <- data_housing[, which(names(data_housing) %in% c("year","quarter",
#                                                                 "N_housing","GEOID"))]
# names(data_housing)[4] <- "eviction_n"
# 
# data_housing0 <- unique(data_housing)
# quick_check = data_housing %>%
#   group_by(quarter, year, GEOID) %>%
#   summarize(count=n())
# examine = quick_check %>% filter(count>1)
# check = inner_join(data_housing, examine)
# 
# data_geo <- read_csv(urlgeos)
# data_geo <- data_geo[, which(names(data_geo) %in% c("GEOID","land","water"))]
