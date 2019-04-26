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
census_block_2013_2017 <- read_csv("data/census_block_2013_2017.csv")
census_tract_2009_2017 <- read_csv("data/census_tract_2009_2017.csv")

spatial_data_no_years <- read_csv("data/spatial_features_noyear.csv")

url311 <- "https://raw.githubusercontent.com/malvikarajeev/sfcrimeanalysis/master/weekly_crime_counts_with_varaibles.csv"
data311 <- read_csv(url311)
data311 <- data311[, which(!(names(data311) %in% c("X1","N")))]

## might need to be updated once mal has here code in