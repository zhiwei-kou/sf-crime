#loading packages
library(ClustOfVar)
library(dplyr)
library(pscl)

#define some helper functions
select_features_from_clusters <- function(varCluster, y, X_scaled){
  n = max(varCluster$cluster)
  selected_vars <- setNames(data.frame(matrix(ncol = 2, nrow = n)), c("variable", "correlation"))
  
  for (i in 1:n){
    var_names <- varCluster$variable[varCluster$cluster==i]
    selected_vars[i,'variable'] <- var_names[which.max(cor(y, X_scaled[,var_names]))]
    selected_vars[i,'correlation'] <- max(cor(y, X_scaled[,var_names]))
  }
  
  return(selected_vars)
}

fit_regressions <- function(training_set, test_set, type){
  if (type == 'OLS'){
    fit <- lm(sqrt(crime_count)~., data=training_set)
    MSE <- mean((test_set$crime_count - (predict.lm(fit, test_set))^2) ^ 2)
    return(MSE)
  }
  else if(type == 'poisson'){
    fit <- glm(crime_count ~ ., data = training_set, family = poisson)
    MSE <- mean((test_set$crime_count - predict(fit, test_set, type = "response")) ^ 2)
    return(MSE)
  }
  else if(type == 'poisson'){
    fit <- MASS::glm.nb(crime_count ~ ., data = training_set)
    MSE <- mean((test_set$crime_count - predict(fit, test_set, type = "response")) ^ 2)
    return(MSE)
  }
  else if(type == 'zero_inflated_negbin'){
    fit1 <- zeroinfl(crime_count~., data=training_set, dist="poisson")
    fit2 <- zeroinfl(crime_count~., data=training_set, dist="negbin")
    print(summary(fit1))
  }
}

crime_types <- c("assualt", "burglary", "robbery", "theft", "vehicle_theft")
varCluster <- read.csv("../../data/variable20Clusters.csv", col.names=c("variable", "cluster"), 
                       colClasses = c("character", "integer"))

for (crime_type in crime_types){
  filename <- paste0("../../data/crime_", crime_type, "_data.csv")
  crime_data <- read.csv(filename)
  crime_data <- crime_data %>% 
    filter(year < 2018) %>% 
    select(-c(quarter, NAME))
  
  y_train <- crime_data[crime_data$year<2017,]$crime
  X_train <- crime_data[crime_data$year<2017,]
  y_test <- crime_data[crime_data$year==2017,]$crime
  X_test <- crime_data[crime_data$year==2017,]
  X_train_scaled <- scale(X_train)
  selected_vars <- select_features_from_clusters(varCluster, y_train, X_train_scaled)
  all_features <- append(selected_vars$variable, names(crime_data)[183:189])
  
  training_set <- data.frame(cbind(y_train, X_train_scaled[, all_features]))
  colnames(training_set)[1] <- 'crime_count'
  X_test_scaled <- scale(X_test, center=apply(X_train, 2, mean), 
                         scale=apply(X_train, 2, sd))
  test_set <- data.frame(cbind(y_test, X_test_scaled[, all_features]))
  colnames(test_set)[1] <- 'crime_count'
  
  cluster_ids <- 1:10
  for (id in cluster_ids){
    small_training_set <- training_set[which(X_train$cluster_id_10 == id), ]
    small_test_set <- test_set[which(X_test$cluster_id_10 == id), ]
    print(fit_regressions(small_training_set, small_test_set, 'OLS'))
  }
  
}
  

