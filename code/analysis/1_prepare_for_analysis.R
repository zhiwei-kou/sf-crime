### create data for analysis
crime_theft_data <- mapped_data2010_2018 %>% 
  filter(theft==1) %>%
  group_by(year, quarter, GEOID_tract) %>%
  summarize(crime=n())

crime_robbery_data <- mapped_data2010_2018 %>% 
  filter(robbery==1) %>%
  group_by(year, quarter, GEOID_tract) %>%
  summarize(crime=n())

crime_assualt_data <- mapped_data2010_2018 %>% 
  filter(assault==1) %>%
  group_by(year, quarter, GEOID_tract) %>%
  summarize(crime=n())

crime_vehicle_theft_data <- mapped_data2010_2018 %>% 
  filter(vehicle_theft==1) %>%
  group_by(year, quarter, GEOID_tract) %>%
  summarize(crime=n())

crime_burglary_data <- mapped_data2010_2018 %>% 
  filter(burglary==1) %>%
  group_by(year, quarter, GEOID_tract) %>%
  summarize(crime=n())


### merge each _data df with census_tract_2009_2017, 
### data311_quarter, spatial_data_no_year
## data_geo, data_housing