setwd("~/Cal/Spring2019/Stat222/sf-crime")

assault_data = read_csv("data/crime_assualt_data.csv")
burglary_data = read_csv("data/crime_burglary_data.csv")
robbery_data = read_csv("data/crime_robbery_data.csv")
theft_data = read_csv("data/crime_theft_data.csv")
vehicle_theft_data = read_csv("data/crime_vehicle_theft_data.csv")

mapped_data2010_2018 <- read_csv("data/crime_with_census_ids.csv")

counts = mapped_data2010_2018 %>% group_by(year) %>% 
  summarize(total_crime=n()) %>% filter(year>2009)

counts_a = assault_data %>% group_by(year) %>% 
  summarize(assualt=sum(crime))

counts_b = burglary_data %>% group_by(year) %>% 
  summarize(burglary=sum(crime))

counts_r = robbery_data %>% group_by(year) %>% 
  summarize(robbery=sum(crime))
counts_t = theft_data %>% group_by(year) %>% 
  summarize(theft=sum(crime))

counts_vt = vehicle_theft_data %>% group_by(year) %>% 
  summarize(vehicle_theft=sum(crime))

total_counts = list(counts, counts_a, counts_b, counts_r, counts_t, counts_vt) %>%
  reduce(left_join)

write_csv(total_counts, "final_pres/total_crime_counts.csv")
