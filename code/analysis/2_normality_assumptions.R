setwd("~/Cal/Spring2019/Stat222/sf-crime")

assault_data = read_csv("data/crime_assualt_data.csv")
burglary_data = read_csv("data/crime_burglary_data.csv")
robbery_data = read_csv("data/crime_robbery_data.csv")
theft_data = read_csv("data/crime_theft_data.csv")
vehicle_theft_data = read_csv("data/crime_vehicle_theft_data.csv")

mean_sd_dif <- function(data){
  mn = mean(data$crime)
  s = sd(data$crime)
  print(mn)
  print(s)
  print(mn-s)
}

mean_sd_dif(assault_data)
mean_sd_dif(burglary_data)
mean_sd_dif(robbery_data)
mean_sd_dif(theft_data)
mean_sd_dif(vehicle_theft_data)
