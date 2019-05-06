## Kmeans Euclidean distance for types of neigbhorhood 

cluster_data <- function(data, cols, model_type){
  data  = data %>% filter(year<2018) #  just drop for now
  tmp = data %>% ungroup() %>% select(GEOID, year, crime) %>%
    group_by(GEOID, year) %>% summarize(crime=sum(crime)) %>%
    group_by(GEOID) %>% summarize(crime=mean(crime))
  tmp_census = unique(data %>% ungroup() %>% 
                        select(one_of(c(cols,"GEOID", "year")))) %>%
    group_by(GEOID) %>% summarize_all(mean) %>% select(-year)
  tmp_all = inner_join(tmp, tmp_census)
  tmp_all_scale = scale(tmp_all[,-1]) # just scale without GEOID
  tmp_all_scale_df = as.data.frame(tmp_all_scale)
  ggplot(data=tmp_all_scale_df,
         aes(x=crime, y=Estimate_Total)) + geom_point() + theme_bw()
  clusters = kmeans(tmp_all_scale, centers=3) # trry and do 3
  tmp_all_scale_df$cluster = clusters$cluster
  tmp_all_scale_df$GEOID = tmp_all$GEOID
  
  ggplot(data=tmp_all_scale_df) + geom_point(aes(x=crime, 
                                                 y=Estimate_Total, 
                                                 color=factor(cluster))) +
    theme_bw() 
  
  map_sf <- tracts("CA","San Francisco", year=2016) # hasn't changed since 2010
  
  map_data <- geo_join(map_sf, tmp_all_scale_df, 
                       by_sp="GEOID", by_df="GEOID",
                       how='left')
  
  indx = which(map_data@data$ALAND>0)
  map_data@polygons <- map_data@polygons[indx]
  map_data@data <- map_data@data[indx,]
  
  # dop next smallest areas of water...
  water_size = sort((as.numeric(map_data@data$AWATER)), decreasing=TRUE)[1:3]
  indx_match = which(as.numeric(map_data@data$AWATER) %in% water_size)
  map_data@polygons <- map_data@polygons[-indx_match]
  map_data@data <- map_data@data[-indx_match,]
  
  
  pal <- colorBin("Spectral", domain = map_data@data$cluster,
                  bins=3)
  #pal <- colorNumeric("Spectral", domain = map_data@data$mse)
  
  p <- leaflet(map_data) %>% addProviderTiles("CartoDB.Positron") %>% 
    fitBounds(lng1=-122.47, lng2=-122.37, lat1=37.70, lat2=37.82) %>%
    addPolygons(fillColor = ~pal(map_data@data$cluster),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7) %>%
    addLegend(pal = pal, values = ~map_data@data$cluster, 
              opacity = 0.7, title = paste("Clusters", model_type),
              position = "bottomright")
  return(list(plot=p, 
              cluster_data=tmp_all_scale_df[, which(names(tmp_all_scale_df) %in% c("cluster","GEOID"))]))
  #distances = fastdist(tmp_all_scale)
}

cols= c("land", "water", "N_housing", "quarter",
        "N_calls_311", "mta_stops", "school_total",
        "Estimate_Total","prop_rented",
        "prop_male", "prop_african_american",
        "prop_under_poverty_level",
        "prop_vacant_houses" ,
        "prop_stable","racial_index" ,
        "income_index" ,"age_index"  ,
        "working_class")

results <- cluster_data(crime_burglary_data, cols, "Burglary")
cluster_info_burg <- results$cluster_data

sepcific_neighbohoods <- function(data, examine, cols){
  data$group  <- data$GEOID %in% examine
  data = data %>% filter(year<2018)
  results = cluster_data(data, c("Estimate_Total"), model_type)
  data2 = results$cluster_data
  data3 =  left_join(data, data2)
  print(data2 %>%  filter(GEOID %in%  examine))
  print(ggplot(data=data3) + geom_point(aes(x=Estimate_Total,
                                      y=crime, color=factor(group))))
  ggplot(data=data) + geom_density(aes(x=crime, fill=factor(group))) +
    theme_bw()
}

for(t in examine){
  print(summary((data3 %>% filter(GEOID==t))$Estimate_Total))
  x = data3 %>% filter(GEOID==t & Estimate_Total==0)
  print(unique(x$year))
}

examine = c("06075010500" ,"06075020402","06075026303" ,"06075980900" )

library(lubridate)
check_weird_results <- function(data, examine, type){
  tmp = data %>% filter(GEOID %in% examine)
  tmp$date = yq(paste(tmp$year,tmp$quarter))
  #tmp = tmp %>% arrange(date)
  ggplot(data=tmp, aes(x=date,
                       y=crime, color=factor(GEOID))) + 
    geom_line() + theme_bw() + xlab("Date") + ylab("Crime Count") +
    ggtitle(paste("Outliers with data", type))
}

check_weird_results(crime_assualt_data, examine, type="assault")
check_weird_results(crime_burglary_data, examine, type="burglary")

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
## merge MSE and then group by cluster to see if any different trends...