
### train test splits by years
## 2010 - 2011
## 2010, 2011 - 2012
## 2010, 2011, 2012 - 2013
## etc until test year is 2016
## last one to test against is 2010-2016, test on 2017

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
       test_msep1=test_msep1, ## for 2018 maybe
       test_preds=test_preds)) # want to analzye predictions
}

fit_models_one_geo <- function(geo_data, model_type, cols,
                               visualize=FALSE){
  cv_results = list()
  geoid = unique(geo_data$GEOID)
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
      if(r==2017) preds <- cv_results[[r]]$test_preds # for 2017
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
  all_geos = lapply(geo_split, fit_models_one_geo, 
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



create_splits <- function(geo_data, cols){
  
  splits = list()
  geoid = unique(geo_data$GEOID)
  min_yr = min(geo_data$year)
  max_yr = max(geo_data$year)
  
  for(y in seq(min_yr+1, max_yr-1, 1)){
    
    tmp_train = geo_data %>% filter(year<y)
    tmp_train_y = log(tmp_train$crime) # log of this
    
    tmp_test = geo_data %>% filter(year==y)
    tmp_test_y = log(tmp_test$crime)
    
    tmp_train0 = tmp_train[, which(names(tmp_train) %in% c(cols, "crime"))]
    tmp_test0 = tmp_test[, which(names(tmp_test) %in% cols)]
    
    if(nrow(tmp_test)>0){
      # only if we can test the results
      splits[[as.character(y)]] <- list(train_x=tmp_train0,
                                        test_x=tmp_test0,
                                        test_y=tmp_test_y,
                                        geoid=geoid)
    }
    
  }
  return(splits)
}

create_one_rf <- function(list_data, yr, n_tree, max_yr, geoid){
  if(yr <= max_yr){
    train_x = list_data$train_x
    test_x = list_data$test_x
    test_y = list_data$test_y
    geoid = list_data$geoid
    
    rf_model = randomForest(log(crime)~.,
                            data=train_x,
                            ntree=n_tree)
    
    
    test_preds <- predict(rf_model, newdata=test_x)
    test_mse <- mean((test_preds - test_y)^2)
    tmp_row <- data.frame(year=as.numeric(yr), 
                          geoid = geoid ,
                          n_tree=n_tree, 
                          test_mse=test_mse,
                          stringsAsFactors = FALSE)
    return(tmp_row)
  }
}

create_one_geo_rf <- function(training_test_split, n_tree, max_yr, geoid){
  # geo_result <- mapply(FUN=function(list_data, yr) create_one_rf(list_data, yr, n_tree,
  #                                                                         max_yr, geoid),
  #                  list_data=training_test_split, yr=names(training_test_split))
  ## why arre certain things failing...
  geo_result <- data.frame()
  for(yr in names(training_test_split)){
    geo_result <- rbind(geo_result, 
                        create_one_rf(training_test_split[[yr]], yr, 
                                      n_tree, max_yr, geoid))
  }
  
  return(geo_result)
}

create_rf_test <- function(training_test_split, n_tree, test_yr, geoid){
  get_list_yr = match(as.character(test_yr), 
                      names(training_test_split))
  
  if(!is.na(get_list_yr)){
    tmp_list = training_test_split[[get_list_yr]]
    train_x = tmp_list$train_x
    test_x = tmp_list$test_x
    test_y = tmp_list$test_y
    geoid = tmp_list$geoid
    
    rf_model = randomForest(log(crime)~.,
                            data=train_x,
                            ntree=n_tree)
    
    test_preds <- predict(rf_model, newdata=test_x)
    predictions <- cbind(test_x, test_preds)
    predictions$n_tree <- n_tree
    predictions$true_y <- test_y
    predictions$mse <- (predictions$test_preds - predictions$true_y)^2 
    predictions$geoid <- geoid
  }
  else{
    predictions <- data.frame()
  }
  return(predictions)
}


create_rf_grid <- function(data, n_trees, cols, max_test_yr=2016){
  geo_split = split(data, data$GEOID) # split data
  training_test_splits = lapply(geo_split, create_splits, cols=cols)
  
  mse_results = do.call("rbind",
                        mapply(FUN=function(list_dat, n_tree, geoid) create_one_geo_rf(list_dat, 
                                                                                       n_tree,
                                                                                       max_test_yr,
                                                                                       geoid),
                               list_dat=training_test_splits, n_tree=n_trees, 
                               geoid=names(training_test_splits), SIMPLIFY = FALSE))
  row.names(mse_results) = NULL
  
  
  # max test year should be 2016 b/c we will test on 2017
  best_n_by_geoid = mse_results %>%
    group_by(geoid, n_tree) %>% 
    summarize(mean_mse = mean(test_mse)) %>% top_n(1, -mean_mse)
  
  best_result = do.call("rbind", 
                        mapply(FUN=function(list_dat, n_tree, geoid) create_rf_test(list_dat, n_tree,
                                                                                    max_test_yr+1, geoid),
                               list_dat=training_test_splits, 
                               n_tree=best_n_by_geoid$n_tree, 
                               geoid=names(training_test_splits), SIMPLIFY = FALSE))
  row.names(best_result) = NULL
  return(best_result)
}

n_trees=c(10, 20, 100, 500, 1000)
cols= c("land", "water", "N_housing", "quarter",
        "N_calls_311", "mta_stops", "school_total",
        "Estimate_Total","prop_rented",
        "prop_male", "prop_african_american",
        "prop_under_poverty_level",
        "prop_vacant_houses" ,
        "prop_stable","racial_index" ,
        "income_index" ,"age_index"  ,
        "working_class")

assault_results = create_rf_grid(crime_assualt_data, n_trees= n_trees,
                                 cols=cols)

assault_check = assault_results %>% top_n(5, mse)
assault_check2 = crime_assualt_data %>% 
  filter(GEOID %in% assault_check$geoid & year < 2018) 
library(lubridate)
assault_check2$date = yq(paste(assault_check2$year, assault_check2$quarter))
ggplot(data=assault_check2) + 
  geom_line(aes(x=date, y=crime)) + ylim(c(0,15)) + 
  facet_wrap(GEOID~.)
#, color=factor(GEOID)))

## now do LASSO search for param
training_test_split= training_test_splits[[1]]

create_splits_lasso <- function(geo_data, cols){
  
  splits = list()
  geoid = unique(geo_data$GEOID)
  min_yr = min(geo_data$year)
  max_yr = max(geo_data$year)
  
  for(y in seq(min_yr+1, max_yr-1, 1)){
    
    tmp_train = geo_data %>% filter(year<y)
    tmp_train_y = log(tmp_train$crime) # log of this
    
    tmp_test = geo_data %>% filter(year==y)
    tmp_test_y = log(tmp_test$crime)
    
    tmp_train0 = tmp_train[, which(names(tmp_train) %in% c(cols, "crime"))]
    tmp_test0 = tmp_test[, which(names(tmp_test) %in% cols)]
    
    if(nrow(tmp_test)>0){
      # only if we can test the results
      splits[[as.character(y)]] <- list(train_x=tmp_train0,
                                        test_x=tmp_test0,
                                        test_y=tmp_test_y,
                                        geoid=geoid)
    }
    
  }
  return(splits)
}
