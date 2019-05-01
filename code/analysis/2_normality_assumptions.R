## KS test for normal distribution
library(ggplot2)

ks_test <- function(data, type_of_crime){
  ## look at counts
  print(ggplot(data=data) + geom_histogram(aes(x=crime)) + theme_bw() + 
    xlab(paste(type_of_crime,"Crime Count")) + ylab("Frequency"))
  
  print(ggplot(data=data) + geom_histogram(aes(x=log(crime))) + theme_bw()  + 
    xlab(paste(type_of_crime, "Log Crime Count")) + ylab("Frequency"))
  
  new_y = rnorm(nrow(data), mean=mean(data$crime), sd=sd(data$crime))
  new_y_log = rnorm(nrow(data), mean=mean(log(data$crime)), sd=sd(log(data$crime)))
  # print(ks.test(x=data$crime, "pnorm", mean(data$crime), sd(data$crime)))
  # print(ks.test(x=log(data$crime),
  #               "pnorm", mean(log(data$crime)), sd(log(data$crime))))
  
  print(ks.test(x=data$crime, new_y))
  print(ks.test(x=log(data$crime), new_y_log))
  
  # print(ggplot(data=data) + stat_qq(aes(sample=crime)) + 
  #         stat_qq_line(aes(sample=crime)) +
  #         ggtitle(paste(type_of_crime, "Crime QQ Plot")))
  
  print(ggplot(data=data) + stat_qq(aes(sample=log(crime))) + 
          stat_qq_line(aes(sample=log(crime))) + 
          ggtitle(paste(type_of_crime, "Log Crime QQ Plot")) + 
          theme_bw())
  # both fail normality assumption...
  # print(shapiro.test(data$crime))
  # print(shapiro.test(log(data$crime)))
  
}
ks_test(crime_theft_data, "Theft")
ks_test(crime_robbery_data, "Robbery")
ks_test(crime_assualt_data, "Assault")
ks_test(crime_vehicle_theft_data, "Vehicle Theft")
ks_test(crime_burglary_data, "Burglary")


