setwd("~/Cal/Spring2019/Stat222")

#source("sf-crime/code/analysis/0_load_data.R", local=TRUE)
#source("code/analysis/1_prepare_for_analysis.R", local=TRUE)

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

library(GGally)
relationships <- function(data, columns){
  tmp = data[, which(names(data) %in% c('crime',columns))]
  pm = ggpairs(tmp, upper = list(continuous = "cor", 
                                 combo = "box_no_facet", 
                                 discrete =  "facetbar", 
                                 na = "na"), 
               lower = list(continuous = "points", 
                            combo = "facethist", 
                            discrete = "facetbar", 
                            na = "na"), 
               diag = list(continuous = "densityDiag", 
                           discrete = "barDiag",
                           na = "naDiag")) + theme_bw()
  print(pm)
}

half_cols = c("land", "water", "N_housing", "quarter",
                "N_calls_311", "mta_stops", "school_total",
                "Estimate_Total")
second_cols = c("prop_rented",
                "prop_male", "prop_african_american",
                "prop_under_poverty_level",
                "prop_vacant_houses" ,
                "prop_stable","racial_index" ,
                "income_index" ,"age_index"  ,
                "working_class")
relationships(crime_assualt_data,
              columns=half_cols)
relationships(crime_assualt_data,
              columns=second_cols)

## maybe not
library(ggcorrplot)
get_corr_plot <- function(data, columns, crime_type){
  
  tmp = dplyr::select_if(data[, which(names(data) %in% c('crime',columns))], 
                         is.numeric)
  corr = round(cor(tmp), 1)
  p.mat = cor_pmat(tmp)
  crime_indx = which(colnames(corr)=="crime")
  print(corr[crime_indx,order(corr[crime_indx,], decreasing = TRUE)])
  print(ggcorrplot(corr, hc.order = TRUE, type = "lower",
             outline.col = "white", lab=TRUE, title=crime_type))#p.mat = p.mat))
}

get_top_cor_columns <- function(data, top_n=15){
  tmp = dplyr::select_if(data,is.numeric)
  corr = round(cor(tmp), 1)
  crime_indx = which(colnames(corr)=="crime")
  top_columns = corr[crime_indx,order(abs(corr[crime_indx,]), decreasing = TRUE)]
  return(names(top_columns)[1:top_n])
}

p1 <- get_corr_plot(crime_assualt_data, c("land", "water", "N_housing", "quarter",
                                    "N_calls_311", "mta_stops", "school_total",
                                    "Estimate_Total", "prop_rented",
                                    "prop_male", "prop_african_american",
                                    "prop_under_poverty_level",
                                    "prop_vacant_houses" ,
                                    "prop_stable","racial_index" ,
                                    "income_index" ,"age_index"  ,
                                    "working_class"), "Assault")

p2 <- get_corr_plot(crime_burglary_data, c("land", "water", "N_housing", "quarter",
                                    "N_calls_311", "mta_stops", "school_total",
                                    "Estimate_Total", "prop_rented",
                                    "prop_male", "prop_african_american",
                                    "prop_under_poverty_level",
                                    "prop_vacant_houses" ,
                                    "prop_stable","racial_index" ,
                                    "income_index" ,"age_index"  ,
                                    "working_class"), "Burglary")

p3 <- get_corr_plot(crime_robbery_data, c("land", "water", "N_housing", "quarter",
                                    "N_calls_311", "mta_stops", "school_total",
                                    "Estimate_Total", "prop_rented",
                                    "prop_male", "prop_african_american",
                                    "prop_under_poverty_level",
                                    "prop_vacant_houses" ,
                                    "prop_stable","racial_index" ,
                                    "income_index" ,"age_index"  ,
                                    "working_class"), "Robbery")

p4 <- get_corr_plot(crime_theft_data, c("land", "water", "N_housing", "quarter",
                                    "N_calls_311", "mta_stops", "school_total",
                                    "Estimate_Total", "prop_rented",
                                    "prop_male", "prop_african_american",
                                    "prop_under_poverty_level",
                                    "prop_vacant_houses" ,
                                    "prop_stable","racial_index" ,
                                    "income_index" ,"age_index"  ,
                                    "working_class"), "Theft")

p5 <- get_corr_plot(crime_vehicle_theft_data, c("land", "water", "N_housing", "quarter",
                                    "N_calls_311", "mta_stops", "school_total",
                                    "Estimate_Total", "prop_rented",
                                    "prop_male", "prop_african_american",
                                    "prop_under_poverty_level",
                                    "prop_vacant_houses" ,
                                    "prop_stable","racial_index" ,
                                    "income_index" ,"age_index"  ,
                                    "working_class"), "Vehicle Theft")

get_corr_plot(crime_assualt_data, get_top_cor_columns(crime_assualt_data),
              'Assault')

library(leaflet)
## visualize data from SF for 311 calls...
library(tigris)
sf_map <- tracts(state = "CA", county='San Francisco', cb =TRUE)

create_map_list <- function(data, sf_map, title="311 Calls for year"){
  map_result <- list()
  tmp = data %>% group_by(GEOID, year) %>% summarize(N_calls_311=sum(N_calls_311))
  for(y in unique(tmp$year)){
    tmp2 = tmp %>% filter(year==y)
    map_data = geo_join(sf_map, tmp2, by_sp="GEOID", by_df="GEOID",how='left')
    
    indx = which(map_data@data$ALAND>0)
    map_data@polygons <- map_data@polygons[indx]
    map_data@data <- map_data@data[indx,]
    
    # dop next smallest areas of water...
    water_size = sort((as.numeric(map_data@data$AWATER)), decreasing=TRUE)[1:3]
    indx_match = which(as.numeric(map_data@data$AWATER) %in% water_size)
    map_data@polygons <- map_data@polygons[-indx_match]
    map_data@data <- map_data@data[-indx_match,]
    
    pal <- colorBin("Spectral", domain = map_data@data$N_calls_311,
                    bins=c(0, 2, 4, 6, 8, 10 , 12, 14, Inf)*100)
    
    leaf_map <- leaflet(map_data) %>% addProviderTiles("CartoDB.Positron") %>% 
      fitBounds(lng1=-122.47, lng2=-122.37, lat1=37.70, lat2=37.82) %>%
      addPolygons(fillColor = ~pal(N_calls_311),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7) %>%
      addLegend(pal = pal, values = ~N_calls_311, 
                title=paste(title, y),
                opacity = 0.7,
                position = "bottomright")
    
    map_result[[as.character(y)]] <- leaf_map
  }
  return(map_result)
}

n_311 <- create_map_list(crime_assualt_data, 
                sf_map)
library(leaflet)
library(animation)
library(png)
library(htmlwidgets)
library(webshot)
library(ggmap)

saveGIF({
  for (i in 1:8) {
    map  = n_311[[i]]
    saveWidget(map, 'temp.html', selfcontained = FALSE) ## save the html
    webshot('temp.html', file=sprintf('Rplot%02d.png', 1),cliprect = 'viewport') ## save as png
    img = readPNG("Rplot01.png") ### read the png
    plot(ggimage(img)) ###reading png file
  }
})
