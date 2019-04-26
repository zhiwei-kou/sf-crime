library(tidyverse)
library(lubridate)
time_period <- function(df, start, end, by, cat_of_interest="BlueCollar"){
  breaks = ceiling((end-start)/by)
  #df$week <- week(df$Date)
  tmp  = df %>% filter(!is.na(GEOID_tract)) %>%
    #group_by( year, CatDesc, GEOID_tract) %>%
    group_by(month, year, CatDesc, GEOID_tract) %>%
    summarize(count=n())
  tmp$yeargroup  = cut(tmp$year, breaks=breaks)
  return(tmp)
}

## DO CENSUS TRACTS 127
#mapped_data <-  time_period(mapped_data2010_2018, 2010, 2018, 3)
mapped_data <- mapped_data2010_2018 %>% group_by(year, GEOID_tract) %>%
  summarize(count=n())

## LIBOR  SAID REPEAT 2018 and 2017 b/c won't change too much
census_tract_2018 <- census_tract_2009_2017  %>% filter(year==2017)
census_tract_2018$year <- 2018
census_tract_2009_2018 <- rbind(census_tract_2009_2017, census_tract_2018)

pct_change = function(x) (x/lag(x)-1)*100

## calculate % change perr column instead
census_tract_changes <- census_tract_2009_2018 %>% arrange(GEOID, year)
## infinite values with 0 values so just change to 1
census_tract_changes[census_tract_changes==0] = 1
census_tract_changes2 = census_tract_changes[,-c(2,3)] %>% group_by(GEOID) %>%
  mutate_all(funs(pct_change))
census_tract_changes2$year <- census_tract_changes$year

census_tract_changes2 = census_tract_changes2 %>% filter(year>2009)
census_tract_changes2[is.na(census_tract_changes2)] = 0


mapped_data <- left_join(mapped_data, census_tract_changes[,c(1,3,4)],
                         by=c("year", "GEOID_tract"="GEOID"))

mapped_data2 <- mapped_data %>% group_by(GEOID_tract, year) %>%
  summarize(count=sum(count))
mapped_data2 <- left_join(mapped_data2, census_tract_changes2,
                          by=c("year", "GEOID_tract"="GEOID"))

per_person = 10000 #10k
mapped_data$rate <- mapped_data$count / mapped_data$Estimate_Total * per_person
summary(mapped_data$rate)

## 2010 and 2018 changes
census_2010_2018 <- census_tract_changes %>% filter(year %in% c(2010, 2017, 2018))
census_2010_2018[census_2010_2018==0] = 1

census_2010_2018_2 = census_2010_2018[,-c(2,3)] %>% group_by(GEOID) %>%
  mutate_all(funs(pct_change))
census_2010_2018_2$year <- census_2010_2018$year

## census changes by year

regression_data <- census_2010_2018_2 %>% filter(year==2017)

mapped_data3  <- mapped_data[,1:3] %>% filter(year %in% c(2010, 2017, 2018))
# mapped_data3 <- left_join(mapped_data3, census_2010_2018[,c(1, 3,4)] ,
#                           by=c("year","GEOID_tract"="GEOID"))
#mapped_data3$rate <- mapped_data3$count / mapped_data3$Estimate_Total * per_person
mapped_data4 <- mapped_data3 %>% group_by(GEOID_tract) %>%
  mutate_all(funs(pct_change))
mapped_data4$year <- mapped_data3$year
mapped_data5 <- mapped_data4 %>% filter(year==2017) %>% drop_na()


regression_data2 <- left_join(mapped_data5, regression_data,
                              by=c("GEOID_tract"="GEOID", "year"))

m1 <- lm(count~Estimate_Total, data=regression_data2)
true_2018_data <- mapped_data4 %>% filter(year==2018)
true_2018_data2 <- left_join(true_2018_data, regression_data2[,c(1,2,4)],
                             by="GEOID_tract") %>% drop_na()
true_2018_data2$preds <- predict(m1, newdata=true_2018_data2) # b/c same census rates
true_2018_data2$dif <- true_2018_data2$preds-true_2018_data2$count
ggplot(data=true_2018_data2) +
  geom_point(aes(x=preds, y=count)) + theme_bw() +
  ylab("True Crime Change") + xlab("Predicted Crime Change")

ggplot(data=true_2018_data2) +
  geom_boxplot(aes(x=0, y=dif)) +
  theme_bw() + ylab("Count Difference Error") +
  xlab("Category")

library(sjPlot)
sjPlot::plot_model(m1)
sjPlot::tab_model(m1)
