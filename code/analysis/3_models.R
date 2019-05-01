
### train test splits by years
## 2010 test 2011
## 2010-2011 test 2012 etc.

library(glmnet)
library(randomForest)

fit_models_one_geo <- function(geo_data, model_type, cols,
                               visualize=FALSE){
  cv_results = list()
  min_yr = min(geo_data$year)
  max_yr = max(geo_data$year)
  for(y in seq(min_yr+1, max_yr-1, 1)){
    tmp_train = geo_data %>% filter(year<y)
    tmp_train_y = log(tmp_train$crime) # log of this
    
    tmp_test = geo_data %>% filter(year==y)
    tmp_test_y = log(tmp_test$crime)
    
    
    if(model_type=="lasso"){
      try({
        tmp_train_x = as.matrix(tmp_train[, which(names(tmp_train) %in% cols)])
        tmp_test_x = as.matrix(tmp_test[, which(names(tmp_test) %in% cols)])
        
        tmp_model = glmnet(x=tmp_train_x,
                           y=tmp_train_y, 
                           family="gaussian", 
                           standardize=TRUE)
        
        train_preds = predict(tmp_model, newx=tmp_train_x)
        train_mse = apply(train_preds, 2, FUN=function(x) return(mean((x - tmp_test_y)^2)))
        test_preds = predict(tmp_model, newx=tmp_test_x)
        test_mse = apply(test_preds, 2, FUN=function(x) return(mean((x - tmp_test_y)^2)))
        
        mse_viz = data.frame(train=train_mse, test=test_mse, 
                             lambda=1:length(train_mse))
        
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
      tmp_train0 = tmp_train[, which(names(tmp_train) %in% c(cols, "crime"))]
      tmp_test0 = tmp_test[, which(names(tmp_test) %in% cols)]
      rf_model<- randomForest(log(crime)~.,
                          data=tmp_train0,
                      ntree=500, importance=TRUE)
                      #xtest=tmp_test[, which(names(tmp_test) %in% cols)],
                      #ytest=tmp_test_y, importance=TRUE)
      train_preds <- predict(rf_model)
      test_preds <- predict(rf_model, newdata=tmp_test0)
      train_mse <- mean((train_preds-tmp_train_y)^2)
      test_mse <- mean((test_preds - tmp_test_y)^2)
      cv_results[[as.character(y)]] = list(train_mse=train_mse,
                                           test_mse=test_mse,
                                           importance=rf_model$importance)
      
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
visualize_results <- function(cv_results, model_type){
  if(model_type=="lasso"){
    results0 <- data.frame()
    for(r in names(cv_results)){
      if(!is.null(cv_results[[r]])){
      results0 <- rbind(results0, cbind(year=as.numeric(r), 
                                      cv_results[[r]]))
      }
    }
    results <- results0 %>% group_by(lambda) %>% summarize(train=mean(train),
                                                           test=mean(test))
    
    # ggplot() + geom_line(data=results, aes(x=lambda, 
    #                                        y=train,
    #                                        color=factor(year)), linetype=2) + 
    #   theme_bw() + geom_line(data=results, aes(x=lambda, 
    #                               y=test,
    #                               color=factor(year)), linetype=4) +
    #   xlab("Lambda") + ylab("MSE") + scale_color_discrete("Year") +
    #   ggtitle("LASSO Lambdas", subtitle ="Dash Line Train, Dash Dot Line Test") 
    print(ggplot() + geom_line(data=results, aes(x=lambda, 
                                           y=train, color="train"), linetype=2) + 
      theme_bw() + geom_line(data=results, aes(x=lambda, 
                                               y=test, color="test"), linetype=4) +
      xlab("Lambda") + ylab("MSE") + scale_color_discrete("Type") +
      ggtitle("LASSO Lambdas") )
  }
  else if(model_type=="random forest"){
    results <- data.frame()
    for(r in names(cv_results)){
      if(!is.null(cv_results[[r]])){
        results <- rbind(results,
                         data.frame(year=as.numeric(r), 
                                    train_mse = cv_results[[r]]$train_mse,
                                    test_mse = cv_results[[r]]$test_mse))
      }
    }
    print(ggplot(data=results) + geom_line(aes(x=year, y=train_mse,group=1, 
                                         color="train"),
                                     linetype=2) +
      geom_line(aes(x=year, y=test_mse, group=1, color="test"),
                linetype=3)  + theme_bw() + ylab("MSE") + xlab("Year") +
      ggtitle("MSE on Random Forest"))
  }
  return(results)
}

modeling <- function(data, model_type, cols){
  geo_split = split(data, data$GEOID)
  all_geos = list()
  for(g in names(geo_split)){
    tmp_data = geo_split[[g]]
    model_results = fit_models_one_geo(tmp_data, model_type, cols)
    all_geos[[g]] = model_results
  }
  return(all_geos)
}

all_geos_lasso_burg = modeling(crime_burglary_data, model_type="lasso", cols= c("land", "water", "eviction_n", "quarter",
                                           "N_calls_311", "mta_stops", "school_total",
                                           "Estimate_Total","prop_rented",
                                           "prop_male", "prop_african_american",
                                           "prop_under_poverty_level",
                                           "prop_vacant_houses" ,
                                           "prop_stable","racial_index" ,
                                           "income_index" ,"age_index"  ,
                                           "working_class"))

all_geos_rf_burg = modeling(crime_burglary_data, model_type="random forest", cols= c("land", "water", "eviction_n", "quarter",
                                           "N_calls_311", "mta_stops", "school_total",
                                           "Estimate_Total","prop_rented",
                                           "prop_male", "prop_african_american",
                                           "prop_under_poverty_level",
                                           "prop_vacant_houses" ,
                                           "prop_stable","racial_index" ,
                                           "income_index" ,"age_index"  ,
                                           "working_class"))
