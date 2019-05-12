### create data for analysis
crime_theft_data0 <- mapped_data2010_2018 %>% 
  filter(theft==1 & !is.na(GEOID_tract)) %>%
  group_by(year, quarter, GEOID_tract) %>%
  summarize(crime=n())

crime_robbery_data0 <- mapped_data2010_2018 %>% 
  filter(robbery==1 & !is.na(GEOID_tract)) %>%
  group_by(year, quarter, GEOID_tract) %>%
  summarize(crime=n()) 

crime_assualt_data0 <- mapped_data2010_2018 %>% 
  filter(assault==1 & !is.na(GEOID_tract))  %>%
  group_by(year, quarter, GEOID_tract) %>%
  summarize(crime=n()) 

crime_vehicle_theft_data0 <- mapped_data2010_2018 %>% 
  filter(vehicle_theft==1 & !is.na(GEOID_tract)) %>%
  group_by(year, quarter, GEOID_tract) %>%
  summarize(crime=n()) 

crime_burglary_data0 <- mapped_data2010_2018 %>% 
  filter(burglary==1 & !is.na(GEOID_tract)) %>%
  group_by(year, quarter, GEOID_tract) %>%
  summarize(crime=n()) 

## quick check
quick_check <- function(data){
  tmp = data %>% group_by(year, GEOID) %>% 
    summarize(count=n())
 return(unique(tmp$count))
}

### merge each _data df with census_tract_2009_2017, 
### data311_quarter, spatial_data_no_year
## data_geo, data_housing
merge_data <- function(data0){
  ## data with just no year
  names(data0)[3] <- "GEOID"
  ## all quarters, years, and geoid's
  completions = expand.grid(quarter=1:4,
                            year=2010:2018,
                            GEOID=unique(data0$GEOID),
                            stringsAsFactors = FALSE)
  # look = left_join(data0, census_tract_2009_2017)
  # look = left_join(look, data311_quarter)
  # look = left_join(look, spatial_data_no_years)
  # look = left_join(look, data_geo)
  # look = left_join(look, data_housing) # this is double..
  # look = left_join(look, data_common_metics)
  tmp = full_join(data0, completions)
  
  look = list(tmp, census_tract_2009_2017,
              spatial_data_no_years,
              additional_data) %>%
    reduce(left_join)
  look[is.na(look)] <- 0
  return(look)
}

crime_theft_data <- merge_data(crime_theft_data0)
crime_robbery_data <- merge_data(crime_robbery_data0)
crime_assualt_data <- merge_data(crime_assualt_data0)
crime_vehicle_theft_data <- merge_data(crime_vehicle_theft_data0)
crime_burglary_data <- merge_data(crime_burglary_data0)

assertthat::are_equal(4, quick_check(crime_theft_data))
assertthat::are_equal(4, quick_check(crime_robbery_data))
assertthat::are_equal(4, quick_check(crime_assualt_data))
assertthat::are_equal(4, quick_check(crime_vehicle_theft_data))
assertthat::are_equal(4, quick_check(crime_burglary_data))

write_csv(crime_assualt_data, 'data/crime_assualt_data.csv')
write_csv(crime_burglary_data, 'data/crime_burglary_data.csv')
write_csv(crime_robbery_data, 'data/crime_robbery_data.csv')
write_csv(crime_theft_data, 'data/crime_theft_data.csv')
write_csv(crime_vehicle_theft_data, 'data/crime_vehicle_theft_data.csv')


rm(list=(setdiff(ls(),c('crime_theft_data', 'crime_robbery_data',
                        'crime_assualt_data', 'crime_vehicle_theft_data',
                        'crime_burglary_data', 'keep_geos'))))

# write_csv(crime_theft_data, "data/crime_theft_data.csv")
# write_csv(crime_burglary_data, "data/crime_burglary_data.csv")
# write_csv(crime_robbery_data, "data/crime_robbery_data.csv")
# write_csv(crime_vehicle_theft_data, "data/crime_vehicle_theft_data.csv")
# write_csv(crime_assualt_data, "data/crime_assualt_data.csv")
