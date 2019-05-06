
### train test splits by years
## 2010 test 2011
## 2010-2011 test 2012 etc.

library(glmnet)
library(randomForest)

create_one_lasso <- function(train_x, train_y, test_x, test_y,
                             test_p1_x, test_p1_y, cols, geoid){
  
  tmp_train_x = as.matrix(train_x[, which(names(train_x) %in% cols)])
  tmp_test_x = as.matrix(test_x[, which(names(test_x) %in% cols)])
  tmp_test_xp1 = as.matrix(test_p1_x[, which(names(test_p1_x) %in% cols)])
  
  tmp_model = glmnet(x=tmp_train_x,
                     y=train_y, 
                     family="gaussian", 
                     standardize=TRUE)
  
  train_preds = predict(tmp_model, newx=tmp_train_x)
  train_mse = apply(train_preds, 2, FUN=function(x) return(mean((x - train_y)^2)))
  test_preds = predict(tmp_model, newx=tmp_test_x)
  test_mse = apply(test_preds, 2, FUN=function(x) return(mean((x - test_y)^2)))
  test_p1_preds = predict(tmp_model, newx=tmp_test_xp1)
  test_p1_mse = apply(test_p1_preds, 2, FUN=function(x) return(mean((x - test_p1_y)^2)))
  
  lambdas = colnames(train_preds)
  mse_viz = data.frame(train=train_mse, 
                       test=test_mse, 
                       test_p1=test_p1_mse,
                       lambda=lambdas, 
                       GEOID=geoid)
  
  #print(head(tmp_test_x)) some do not exist..
  #print(head(mse_viz))
  
  return(mse_viz)
}

create_one_rf <- function(train_x, train_y, test_x, test_y,
                          test_p1_x, test_p1_y, cols){
  
  tmp_train0 = train_x[, which(names(train_x) %in% c(cols, "crime"))]
  tmp_test0 = test_x[, which(names(test_x) %in% cols)]
  tmp_testp10 = test_p1_x[, which(names(test_p1_x) %in% cols)]
  
  rf_model<- randomForest(log(crime)~.,
                          data=tmp_train0,
                          ntree=500, importance=TRUE)
  
  train_preds <- predict(rf_model)
  test_preds <- predict(rf_model, newdata=tmp_test0)
  test_p1_preds <- predict(rf_model, newdata=tmp_testp10)
  train_mse <- mean((train_preds-train_y)^2)
  test_mse <- mean((test_preds - test_y)^2)
  test_msep1 <- mean((test_p1_preds - test_p1_y)^2)
  
  return(list(train_mse=train_mse,
       test_mse=test_mse,
       importance=rf_model$importance,
       test_msep1=test_msep1)) # for 2018
}

fit_models_one_geo <- function(geo_data, geoid, model_type, cols,
                               visualize=FALSE){
  cv_results = list()
  min_yr = min(geo_data$year)
  max_yr = max(geo_data$year)
  for(y in seq(min_yr+1, max_yr-1, 1)){
    
    tmp_train = geo_data %>% filter(year<y)
    tmp_train_y = log(tmp_train$crime) # log of this
    
    tmp_test = geo_data %>% filter(year==y)
    tmp_test_y = log(tmp_test$crime)
    
    next_yr_test = geo_data %>% filter(year==(y+1))
    next_yr_test_y = log(next_yr_test$crime)
    
    
    if(model_type=="lasso"){
      try({
        mse_viz = create_one_lasso(tmp_train, tmp_train_y,
                                   tmp_test, tmp_test_y, 
                                   next_yr_test, next_yr_test_y, cols,
                                   geoid)
        
        if(visualize){
          print(ggplot(data=mse_viz) + 
                  geom_line(aes(x=lambda, y=train, color="train"), linetype=2) +
                  geom_line(aes(x=lambda, y=test, color="test"), linetype=3) +
                  theme_bw() + ggtitle(paste("LASSO MSE with Test Year", y)) +
                  xlab("Lambda") + ylab("MSE"))
          
          print(mse_viz[1:nrow(mse_viz) %in% c(which.min(test_mse), 
                                               which.min(train_mse)),])
          
        }
        cv_results[[as.character(y)]] = mse_viz
      })
      
    }
    else if(model_type=="random forest"){
      
      cv_results[[as.character(y)]] = create_one_rf(tmp_train,
                                                    tmp_train_y,
                                                    tmp_test,
                                                    tmp_test_y,
                                                    next_yr_test,
                                                    next_yr_test_y,
                                                    cols)
      
      if(visualize){
        print(cv_results[[as.character(y)]])
      }
    }
  }
  results = visualize_results(cv_results, model_type)
  return(results)
}

#  "solid", "dashed", "dotted", "dotdash", "longdash", and "twodash".
# 2-7

analyze_list_lasso <- function(cv_results, max_yr, visualize=FALSE){
  results0 <- data.frame()
  best_lambda_test <- NULL
  
  for(r in names(cv_results)){ # each year
    if(!is.null(cv_results[[r]]) | 
       nrow(cv_results[[r]])>0){
      results0 <- rbind(results0, cbind(year=as.numeric(r), 
                                        cv_results[[r]]))
      if(r==as.character(max_yr)) best_lambda_test <- cv_results[[r]]
    }
  }
  if("lambda" %in% names(results0) & nrow(results0)>0){
    
    results0 <- results0 %>% group_by(GEOID, lambda) %>% 
      summarize(train=mean(train),test=mean(test))
    
    if(visualize){
      print(ggplot() + 
              geom_line(data=results0, 
                        aes(x=lambda,  y=train, group=1, color="train"), linetype=2) + 
              theme_bw() + 
              geom_line(data=results0, 
                        aes(x=lambda, y=test, group=2,
                            color="test"), linetype=4) +
              xlab("Lambda") + ylab("MSE") + 
              scale_color_discrete("Type") +
              ggtitle("LASSO Lambdas") +
              theme(axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank()))
    }
    
    if(!is.nan(results0$test)[1] & !is.null(best_lambda_test)){
      
      best_lambda <- results0$lambda[which.min(results0$test)] # get best lambda
      outcome_row <- match(best_lambda, best_lambda_test$lambda)
      
      if(!is.na(outcome_row)){
        results <- best_lambda_test[outcome_row,]
      }
      else{
        results <- best_lambda_test[which.min(best_lambda_test$mse),]
      }
    }
  } else{
    results <- data.frame()
  }
  
  if(!exists("results")) results <- data.frame()
  return(results)
}

analyze_list_rf <- function(cv_results){
  results <- data.frame()
  for(r in names(cv_results)){
    if(!is.null(cv_results[[r]])){
      results <- rbind(results,
                       data.frame(year=as.numeric(r), 
                                  train_mse = cv_results[[r]]$train_mse,
                                  test_mse = cv_results[[r]]$test_mse,
                                  test_mse_p1 = cv_results[[r]]$test_msep1))
    }
  }
  
  print(ggplot(data=results) + 
          geom_line(aes(x=year, y=train_mse, group=1, 
                        color="train"),linetype=2) +
          geom_line(aes(x=year, y=test_mse, group=1, color="test"),
                    linetype=3)  + theme_bw() + ylab("MSE") + xlab("Year") +
          ggtitle("MSE on Random Forest"))
  
  return(results)
}

visualize_results <- function(cv_results, model_type, final_yr=2017){
  if(model_type=="lasso"){
    results <- analyze_list_lasso(cv_results, final_yr)
  }
  else if(model_type=="random forest"){
    results <- analyze_list_rf(cv_results)
  }
  return(results)
}

modeling <- function(data, model_type, cols){
  geo_split = split(data, data$GEOID)
  all_geos = lapply(geo_split, fit_models_one_geo, geoid=g, 
                    model_type=model_type, cols=cols)
  
  return(all_geos)
}

library(leaflet)
library(tigris)
library(rgdal)
library(tidycensus)

create_map <- function(vis_results, title, column){
  
  map_sf <- tracts("CA","San Francisco", year=2016) # hasn't changed since 2010
  
  map_data <- geo_join(map_sf, vis_results, 
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
  
  #bins = summary(map_data@data[column])[1:6]
  domain = map_data@data[[column]]
  bins = summary(map_data@data[[column]])[1:6]
  
  pal <- colorBin("Spectral", domain = domain,
                  bins=bins)
  
  
  p <- leaflet(map_data) %>% addProviderTiles("CartoDB.Positron") %>% 
    fitBounds(lng1=-122.47, lng2=-122.37, lat1=37.70, lat2=37.82) %>%
    addPolygons(fillColor = pal(map_data@data[[column]]),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7) %>%
    addLegend(pal = pal, 
              values = map_data@data[[column]], 
              opacity = 0.7, title = title,
              position = "bottomright")
  return(p)
}

visualize_map <- function(list_data, model_type, column){
  
  vis_results = data.frame()
  
  if(model_type=='random forest'){
    for(g in names(list_data)){
      if(nrow(list_data[[g]])==7){
        tmp_df = data.frame(GEOID=g, 
                            test_mse=list_data[[g]]$test_mse[7],
                            test_mse_p1=list_data[[g]]$test_mse_p1[7])
        vis_results <- rbind(vis_results, tmp_df)
      }
    }
  }
  else if(model_type=='lasso'){
    vis_results = data.frame()
    for(g in names(list_data)){
      if(nrow(list_data[[g]])>0){
        tmp_df = list_data[[g]]
        if(sapply(tmp_df[column], is.finite)==TRUE) vis_results <- rbind(vis_results, tmp_df)
      }
    }
  }
  
  sf <- create_map(vis_results, paste("MSE", model_type), column)
  return(sf)
}

all_geos_lasso_burg = modeling(crime_burglary_data, 
                               model_type="lasso", 
                               cols=c("land", "water", "N_housing",
                                       "quarter",
                                       "N_calls_311", "mta_stops", "school_total",
                                       "Estimate_Total","prop_rented",
                                       "prop_male", "prop_african_american",
                                       "prop_under_poverty_level",
                                       "prop_vacant_houses" ,
                                       "prop_stable","racial_index" ,
                                       "income_index" ,"age_index"  ,
                                       "working_class"))

all_geos_rf_burg = modeling(crime_burglary_data, 
                            model_type="random forest",
                            cols= 
                              c("land", "water", "N_housing", "quarter",
                                "N_calls_311", "mta_stops", "school_total",
                                "Estimate_Total","prop_rented",
                                "prop_male", "prop_african_american",
                                "prop_under_poverty_level",
                                "prop_vacant_houses" ,
                                "prop_stable","racial_index" ,
                                "income_index" ,"age_index"  ,
                                "working_class"))


visualize_map(all_geos_lasso_burg, 'lasso', 'test')
visualize_map(all_geos_rf_burg, 'random forest', '')



examine_these <- c("06075980900","06075026303", "06075020402",
                   "06075010500")

examine <- function(data, tracts){
  for(t in tracts){
    tmp = data %>% filter(GEOID==t)
    if(nrow(tmp)>0){
      print(ggplot(data=tmp, aes(x=interaction(year, quarter),
                                 y=crime, group=1)) + 
              geom_line() + theme_bw() +
              geom_smooth() + 
              ggtitle(t))
    }
    else{
      print(paste(t,  " has no obs"))
    }
    #tmp$new_x = interaction(year, quarter)
  }
}

examine(crime_assualt_data, examine_these)
examine(crime_burglary_data, examine_these)
examine(crime_robbery_data, examine_these)
examine(crime_theft_data, examine_these)
examine(crime_vehicle_theft_data, examine_these)
