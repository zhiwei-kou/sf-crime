setwd("~/Cal/Spring2019/Stat222")

source("sf-crime/code/analysis/0_load_data.R", local=TRUE)
source("code/analysis/1_prepare_for_analysis.R", local=TRUE)

rm(list=(setdiff(ls(),c('census_tract_2009_2017',
                        'crime_assualt_data',
                        'crime_burglary_data',
                        'crime_robbery_data',
                        'crime_theft_data',
                        'crime_vehicle_theft_data'))))

library(lubridate)
viz_data <- function(data, type){
  data$date = yq(paste(data$year, data$quarter))
  data = data %>% arrange(-crime)
  print(head(data[,c(1:4, 6)],20))
  print(ggplot(data=data, aes(x=date, y=crime, color=factor(GEOID))) +
          geom_line() + theme_bw()  + xlab("Date") + ylab("Crime Count") +
          ggtitle(paste("Crime Count of", type)) + guides(color=FALSE))
}

viz_data(crime_assualt_data, 'assault')
viz_data(crime_burglary_data, 'burglary')
viz_data(crime_robbery_data, 'robbery')
viz_data(crime_theft_data, 'theft')
viz_data(crime_vehicle_theft_data, 'vehicle theft')


overall_data <- function(data, type){
  data$date = yq(paste(data$year, data$quarter))
  # median crime countss??
  data_group = data %>% group_by(date) %>% summarize(crime=mean(crime),
                                                     median_crime=median(crime))
  print(ggplot(data=data_group) + geom_line(aes(x=date, y=crime)) + theme_bw()  + 
    xlab("Date") + ylab("Crime Count") +
    ggtitle(paste("Crime Count of", type)))
}

overall_data(crime_assualt_data, 'assault')
overall_data(crime_burglary_data, 'burglary')
overall_data(crime_robbery_data, 'robbery')
overall_data(crime_theft_data, 'theft')
overall_data(crime_vehicle_theft_data, 'vehicle theft')


