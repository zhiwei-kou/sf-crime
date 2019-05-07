
### train test splits by years
## 2010 - 2011
## 2010, 2011 - 2012
## 2010, 2011, 2012 - 2013
## etc until test year is 2016
## last one to test against is 2010-2016, test on 2017
setwd("~/Cal/Spring2019/Stat222")

source("sf-crime/code/analysis/0_load_data.R", local=TRUE)
source("code/analysis/1_prepare_for_analysis.R", local=TRUE)

library(glmnet)
library(randomForest)
library(caret)
## can check time by package tictoc
library(tictoc)
create_cv_geo  <- function(geo_data, method, cols,
                       tuneGrid=NULL, test_yr){
  #tic("create data")
  tmp = geo_data %>% filter(year<test_yr)
  train_control = trainControl("timeslice",
                                initialWindow = 4,
                                horizon=4,
                                skip=4,
                                fixedWindow = F) # each year has 4
  tmp_x = tmp[, which(names(tmp) %in% cols)]
  tmp_test = geo_data %>% filter(year==test_yr)
  tmp_test_x = tmp_test[, which(names(tmp_test) %in% cols)]
  tmp_test_y = log(tmp_test[['crime']])
  #toc()
  
  ## if not custom  then
  if(is.null(tuneGrid)){
    if(method=="glmnet") tuneGrid <- expand.grid(alpha = 1,
                                                 lambda = seq(0.001,0.1,by = 0.001))
    if(method=="rf") tuneGrid <- expand.grid(mtry=seq(1, length(cols)))
    if(method=="parRF") tuneGrid <- expand.grid(mtry=seq(1, length(cols)))
  }
  
  #tic("cv model")
  cv_model = train(tmp_x, 
                   log(tmp[['crime']]), 
                   method = method, 
                   trControl = train_control,
                   preProcess = c("center","scale"),
                   metric = "RMSE",
                   tuneGrid = tuneGrid)
  #toc()
  
  geo_result <- list(best_tune= cv_model$bestTune,
       best_model=cv_model$finalModel,
       test_x=tmp_test_x, 
       true_y=tmp_test_y)
  
  #tic("final if else")
  if(method=='glmnet'){
    
    ## just return best params and fit with them
    geo_result <- append(geo_result, 
                         list(best_coef = coef(cv_model$finalModel, 
                                 cv_model$bestTune$lambda)))
    
  }
  if(method %in% c('rf', 'parRF')){
    geo_result <- append(geo_result,
                         list(importance = cv_model$finalModel$importance))
  }
  #toc()
  return(geo_result)
}


get_output <- function(list_data, geoid, method){
  if(method=='glmnet'){
    test_preds = as.vector(predict(list_data$best_model, 
                                   list_data$best_tune$lambda,
                                   newx=as.matrix(list_data$test_x)))
  }
  if(method %in% c('rf','parRF')){
    test_preds = as.vector(predict(list_data$best_model, 
                                   newx=as.matrix(list_data$test_x)))
  }
  
  test_results = cbind(list_data$test_x, test_preds)
  test_results = cbind(test_results, true_y=list_data$true_y)
  test_results$mse <- (test_results[['test_preds']]-list_data[['true_y']])^2
  test_results$GEOID <- geoid
  return(test_results)
}

check_window_data <- function(data){
  check = data %>% group_by(GEOID, year) %>% summarize(count=n()) # all need to be 4
  fail = check %>% filter(count!=4)
  return(unique(fail$GEOID))
}

create_outputs <- function(data, method, cols,
                           tuneGrid=NULL, test_yr=2017){
  # http://topepo.github.io/caret/available-models.html
  geo_split <- split(data, data$GEOID) # split data
  ignore_these = check_window_data(data)
  #keep = setdiff(names(geo_split), ignore_these)
  for(i in ignore_these){
    indx = match(i, names(geo_split))
    geo_split <- geo_split[-indx]
  }
  
  best_params_geo = lapply(geo_split, create_cv_geo, method=method,
                          cols=cols, tuneGrid=tuneGrid, test_yr=test_yr)
  if(method=='glmnet'){
    results <- do.call("rbind",
                       mapply(FUN=function(list_dat, geo) get_output(list_dat, geo, method),
                      list_dat=best_params_geo, geo=names(best_params_geo), 
                      SIMPLIFY=FALSE))
    row.names(results) <- NULL
  }
  if(method %in% c('rf', 'parRF')){
    results <- do.call("rbind",
                       mapply(FUN=function(list_dat, geo) get_output(list_dat, geo, method),
                              list_dat=best_params_geo, geo=names(best_params_geo), 
                              SIMPLIFY=FALSE))
    row.names(results) <- NULL
  }
  return(list(df_results=results, best_params=best_params_geo))
}

assault_results <- create_outputs(crime_assualt_data, method='rf', 
                                  cols=c("land", "water", "N_housing", "quarter",
                                         "N_calls_311", "mta_stops", "school_total",
                                         "Estimate_Total","prop_rented",
                                         "prop_male", "prop_african_american",
                                         "prop_under_poverty_level",
                                         "prop_vacant_houses" ,
                                         "prop_stable","racial_index" ,
                                         "income_index" ,"age_index"  ,
                                         "working_class"))

