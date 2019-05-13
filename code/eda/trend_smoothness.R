## why chose certain ?
setwd("~/Cal/Spring2019/Stat222/sf-crime")
library(tidyverse)
mapped_data2010_2018 <- read_csv("data/crime_with_census_ids.csv")

create_counts <- function(data, ...){
  group = quos(...)
  tmp = data %>% group_by(!!!group) %>% summarize(crime=n()) %>% ungroup()
  return(tmp)
}


summarized_data <- mapped_data2010_2018 %>% 
  mutate(category=ifelse(theft==1, "theft",
                         ifelse(robbery==1,
                                "robbery",
                                ifelse(assault==1, "assault",
                                       ifelse(vehicle_theft==1,
                                              "vehicle_theft",
                                              ifelse(burglary==1,
                                                     "burglary","error")))))) %>%
  select(year, day, month, quarter, category) %>% filter(category!="error")


crime_year = create_counts(summarized_data, category, year)
crime_quarterly = create_counts(summarized_data, category, year, quarter)
crime_month = create_counts(summarized_data, category, year, month)
crime_day = create_counts(summarized_data, category, year, month, day)
library(lubridate)

crime_year = crime_year %>% mutate(date=ymd(paste(year, paste(1, 1))),
                                   crime_cs=cumsum(crime))

crime_quarterly = crime_quarterly  %>% mutate(date=yq(paste(year, quarter)),
                                   crime_cs=cumsum(crime))

crime_month = crime_month  %>% mutate(date=ymd(paste(year, paste(month, 1))),
                                   crime_cs=cumsum(crime))

crime_day = crime_day  %>% mutate(date=ymd(paste(year, paste(month, day))),
                                   crime_cs=cumsum(crime))

                                     

## maybe do cumsum on year
ggplot() + geom_line(data=crime_year, aes(x=date, y=crime), color="blue") +
  geom_line(data=crime_quarterly, aes(x=date, y=crime), color="purple") +
  geom_line(data=crime_month, aes(x=date, y=crime), color="orange") +
  geom_line(data=crime_day, aes(x=date, y=crime), color="red") +
  facet_wrap(category~.) + theme_bw()

library(forecast)
library(tseries)

create_ts <- function(data, type_of_crime, date_format, freq){
  tmp = data %>% filter(category==type_of_crime)
  if(date_format=="%Y"){
    new_ts = ts(tmp$crime, start = 2010,
                frequency = freq)
    return(new_ts)
  }
  if(date_format=="%q"){
    new_ts = ts(tmp$crime, 
                start = c(2010, 1),
                frequency = freq)
  }else{
    new_ts = ts(tmp$crime, start = c(2010, 
                                     as.numeric(format(tmp$date[1], date_format))),
                frequency = freq)
  }
  decomposed_ts = decompose(new_ts)
  title = paste("Year for", paste(type_of_crime, paste("crimes"), 
                                  paste("on", date_format)))
  print(autoplot(decomposed_ts) + theme_bw() + 
          xlab(title))
  print(decomposed_ts)
  return(new_ts)
}


categories = c("theft", "vehicle_theft", 
               "robbery", "assault", "burglary")
daily_ts = lapply(categories, create_ts, data=crime_day, 
                  date_format="%j", freq=365.25)
monthly_ts = lapply(categories, create_ts, data=crime_month,
                    date_format="%m", freq=12)
quarterly_ts = lapply(categories, create_ts, data=crime_quarterly,
                      date_format="%q", freq=4)
yearly_ts = lapply(categories, create_ts, data=crime_year,
                   date_format="%Y", freq=1)


