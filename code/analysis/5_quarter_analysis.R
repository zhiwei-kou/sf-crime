
map_quarters <- function(data, title){
  
  tmp = data %>% group_by(GEOID, quarter) %>% summarize(crime=mean(crime))
  p1 <- ggplot(data=tmp) + 
    geom_density(aes(x=crime, y=..density..,
                     fill=factor(quarter)), alpha=0.5) + 
    geom_histogram(aes(x=crime, y=..density..,
                       fill=factor(quarter)), alpha=0.5) +
    theme_bw() + ggtitle(title)
  
  p2 <- ggplot(data=tmp) +  geom_density(aes(x=crime, y=..density..), 
                                   alpha=0.5) + 
    geom_histogram(aes(x=crime, y=..density..), alpha=0.5) +
    facet_wrap(quarter~.) +
    theme_bw() +  ggtitle(title)
  
  print(p1)
  print(p2)
}

map_quarters(crime_assualt_data, title="Assault")
map_quarters(crime_burglary_data, title="Burglary")
map_quarters(crime_robbery_data, title="Robbery")
map_quarters(crime_theft_data, title="Theft")
map_quarters(crime_vehicle_theft_data, title="Vehicle Theft")
