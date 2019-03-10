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
