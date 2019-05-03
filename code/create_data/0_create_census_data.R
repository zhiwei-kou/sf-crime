setwd("~/Cal/Spring2019/Stat222")

#####
#### Author: Andrea Bonilla
#### 

library(tidyverse)
library(tidycensus)
variables <- load_variables(2016, "acs5", cache = TRUE)

# Neighborhood size, median income, employment rate, age/gender/race composition

# income check
# gender and age check 
# race check
# neighborhood size
# education and gender

               
population_vars <- paste("B01001_0", c(paste("0", 1:9 ,sep=""), 10:49) ,sep="")
population_names <- gsub("( |!!)", "_", gsub("(Estimate!!Total!!)","",
                    variables$label[variables$name %in% population_vars]))
income_vars <- paste("B06010_0", c(paste("0", 3:9, sep=""), 10:11), sep="")
income_names <- gsub("( |!!)", "_", 
                     gsub("(Estimate!!Total!!|\\$|,)","",
                          variables$label[variables$name %in% income_vars]))

race_vars <- paste("B03002_0", c(paste("0",2:9 ,sep=""), 10:21), sep="")
race_names <- gsub("( |!!)", "_", 
                   gsub("(Estimate!!Total!!|,)","",
                        variables$label[variables$name %in% race_vars]))

housing_vars <- c("B25008_002","B25008_003")
housing_names <- gsub("( |!!)", "_", 
                      gsub("(Estimate!!Total!!|,)","",
                           variables$label[variables$name %in% housing_vars]))

mobility_vars <- paste("B07003_0",c(paste("0", 4:9, sep=""), 10:18), sep="")
mobility_names <- gsub("( |!!)", "_", 
                       gsub("(Estimate!!Total!!|,)","",
                            variables$label[variables$name %in% mobility_vars]))

education_vars <- paste("B15002_0", c(paste("0", 2:9, sep=""), 10:18, 
                                      20:35), sep="")
education_names <- gsub("( |!!)", "_", 
                        gsub("(Estimate!!Total!!|,)","",
                             variables$label[variables$name %in% education_vars]))

of_interest <- c(population_vars, income_vars, race_vars,
                 housing_vars, mobility_vars, education_vars)
of_interest_names <- c(population_names, income_names, race_names,
                       housing_names, mobility_names, education_names)

employment_vars <- paste("B17005_0", c(paste("0", 2:9, sep=""), 10:22), sep="")
# start at 2 to get rid of extra var name
employment_names <- gsub("( |!!)", "_", 
                         gsub("(Estimate!!Total!!|,)","",
                              variables$label[variables$name %in% employment_vars]))
of_interest <- c(population_vars, income_vars, race_vars,
                 housing_vars, mobility_vars, education_vars, employment_vars)
of_interest_names <- c(population_names, income_names, race_names,
                       housing_names, mobility_names, education_names, employment_names)
translation <- data.frame(variable=of_interest, 
                          census_var=of_interest_names, stringsAsFactors = FALSE)

summary(table(translation$census_var))

create_census_data <- function(years, geography){
  data <- data.frame()
  for(y in years){
    print(y)
    data0 <- get_acs(geography = geography, 
                    variables = of_interest , state='CA',
                    county="San Francisco", year = y)
    data0$year <- y
    data <- rbind(data, data0)
  }
  data <- left_join(data, translation, by="variable")
  return(data)
}

census_data_tract <- create_census_data(c(2009:2017), 'tract') # tract
#census_data_zip <- create_census_data(c(2009:2010), 'zcta') #zip NOPE Cant filter for dif parts in the state
#census_data_block <- create_census_data(2013:2017, 'block group') # only after 2013, smaller than tracts

library(reshape2)
census_data_tract2 <- melt(census_data_tract, id.vars=c('GEOID', 'NAME', 'year', 'census_var'), 
                           measure.vars=c('estimate'))

tract_wide <- dcast(census_data_tract2, GEOID+NAME+year~census_var, mean)

#census_data_block2 <- melt(census_data_block, id.vars=c('GEOID', 'NAME', 'year', 'census_var'), 
#                           measure.vars=c('estimate'))

#block_wide <- dcast(census_data_block2, GEOID+NAME+year~census_var, mean)

#sum(tract_wide$Estimate_Total[tract_wide$year>2012]) == sum(block_wide$Estimate_Total)

tract_wide %>% group_by(year) %>% summarize(pop=sum(Estimate_Total))

# NOTE: ACS is always off by a % for safety reasons... so fine that it os off
# by less than 20k
# Year	Population	Growth rate
# 2011	816,231	n/a
# 2012	829,517	1.63%
# 2013	840,715	1.35%
# 2014	852,537	1.41%
# 2015	864,816	1.44%
# 2016	870,887	0.70%
# 2017	878,887	0.92%
# 2018	889,092	1.16%


## create additional values
# [7] "prop_rented"              "prop_male"               
# [9] "prop_african_american"    "prop_under_poverty_level"
# [11] "prop_vacant_houses"       "prop_stable"             
# [13] "racial_index"             "income_index"            
# [15] "age_index"                "working_class"           
# [17] "land"                     "water"    
# fraction of male population, fraction of black population, 
# fraction of hispanic population, fraction of population
# under the poverty level, fraction of vacant households, 
# the fraction of rented households from the occupied ones, and the fraction of
# stable population (individuals who moved in prior to 2010).
# 
# The racial ethnic index is defined by the plurality of multiple ethnic
# and racial groups within a certain area and is
# computed based on five exhaustive and mutually exclusive aggregates
# (non-Hispanic whites, non-Hispanic blacks, Hispanics of any race, Asians,
#   and others – Native Americans, members of other races, and 
#   multi-racial persons). 
# 
# The age index measures the variance in ages of the residents
# across four main age groups (under 18, 18-34, 35-64, and over 65 years), 
# and the income index measures the variance in
# household income across three main income levels 
# (low = underr 35k, medium = 35-100k, and high-income = over 100k  households).

# race index sum p_r * ln(1/p_r)
# diveristy index = 1 - sum p^2

# get first varrs
tract_wide <- tract_wide %>% 
  mutate(under_18=Male_Under_5_years+Female_Under_5_years+Male_5_to_9_years+Female_5_to_9_years+
           Male_10_to_14_years+Female_10_to_14_years+Male_15_to_17_years+Female_15_to_17_years,
         between_18_34=Male_18_and_19_years+Female_18_and_19_years+Male_20_years+
           Female_20_years+Male_21_years+Female_21_years+
           Male_22_to_24_years+Female_22_to_24_years+Male_25_to_29_years+
           Female_25_to_29_years+Male_30_to_34_years+Female_30_to_34_years,
         between_35_64=Male_35_to_39_years+Female_35_to_39_years+Male_40_to_44_years+
           Female_40_to_44_years+Male_45_to_49_years+Female_45_to_49_years+
           Male_50_to_54_years+Female_50_to_54_years+Male_55_to_59_years+
           Female_55_to_59_years+Male_60_and_61_years+Female_60_and_61_years+
           Male_62_to_64_years+Female_62_to_64_years,
         over_64= Male_65_and_66_years+Female_65_and_66_years+
           Male_67_to_69_years+Female_67_to_69_years+Male_70_to_74_years+
           Female_70_to_74_years+Male_75_to_79_years+
           Female_75_to_79_years+Male_80_to_84_years+
           Female_80_to_84_years+Male_85_years_and_over+Female_85_years_and_over,
         low_income=With_income_1_to_9999_or_loss+With_income_10000_to_14999+
           With_income_15000_to_24999+With_income_25000_to_34999,
         medium_income=With_income_35000_to_49999+With_income_50000_to_64999+With_income_65000_to_74999,
         high_income=With_income_75000_or_more,
         other_race=Not_Hispanic_or_Latino_American_Indian_and_Alaska_Native_alone+
           Not_Hispanic_or_Latino_Native_Hawaiian_and_Other_Pacific_Islander_alone+
           Not_Hispanic_or_Latino_Some_other_race_alone+Not_Hispanic_or_Latino_Two_or_more_races,
         less_hs=Male_No_schooling_completed+Male_Nursery_to_4th_grade+
           Male_5th_and_6th_grade+Male_7th_and_8th_grade+Male_9th_grade+
           Male_10th_grade+Male_11th_grade+Male_12th_grade_no_diploma+
           Female_No_schooling_completed+Female_Nursery_to_4th_grade+
           Female_5th_and_6th_grade+Female_7th_and_8th_grade+Female_9th_grade+
           Female_10th_grade+Female_11th_grade+Female_12th_grade_no_diploma,
         hs_associate=`Male_High_school_graduate_(includes_equivalency)`+
           Male_Some_college_less_than_1_year+Male_Some_college_1_or_more_years_no_degree+
           `Male_Associate's_degree`+`Female_High_school_graduate_(includes_equivalency)`+
           Female_Some_college_less_than_1_year+Female_Some_college_1_or_more_years_no_degree+
           `Female_Associate's_degree`,
         bach_or_more=`Male_Bachelor's_degree`+`Male_Master's_degree`+Male_Professional_school_degree+
           Female_Doctorate_degree+`Female_Bachelor's_degree`+`Female_Master's_degree`+Female_Professional_school_degree+
           Female_Doctorate_degree
         ) %>%
  mutate(prop_male=Male/Estimate_Total,
         prop_black=Not_Hispanic_or_Latino_Black_or_African_American_alone/Estimate_Total,
         prop_hispanic=Hispanic_or_Latino/Estimate_Total,
         prop_under_poverty_level=Income_in_the_past_12_months_below_poverty_level/Estimate_Total,
         #prop_vacant=,
         prop_rented_to_owned=ifelse(Owner_occupied==0,
                                     Renter_occupied,Renter_occupied/Owner_occupied), # just keep renter occupied #
         prop_stable=Same_house_1_year_ago/Estimate_Total)

diversity_f <- function(df, vars) {
  values = which(names(df) %in% vars)
  est_total = which(names(df)=='Estimate_Total')
  return(1 - sum((as.numeric(c(df[values]))/as.numeric(df[est_total]))^2))
}

tract_wide$racial_index <- 
  apply(tract_wide, 1, FUN=diversity_f, vars=c("Hispanic_or_Latino",
                                                "Not_Hispanic_or_Latino_Black_or_African_American_alone",
                                                "Not_Hispanic_or_Latino_White_alone",
                                                "Not_Hispanic_or_Latino_Asian_alone",
                                                "other_race"))


tract_wide$age_index <- 
  apply(tract_wide, 1, FUN=diversity_f, vars=c("under_18",
                                                "between_18_34",
                                                "between_35_64",
                                                "over_64"))


tract_wide$income_index <- 
  apply(tract_wide, 1, FUN=diversity_f, vars=c("low_income",
                                                 "high_income",
                                                 "medium_income"))


tract_wide$education_index <- 
  apply(tract_wide, 1, FUN=diversity_f, vars=c("less_hs", "hs_associate",
                                                "bach_or_more"))

## replace all na b/c dividing by estimate total 0
which(colSums(is.na(tract_wide))>0)
tract_wide[is.na(tract_wide)] = 0

## quick checks
for(c in c("racial_index","age_index","income_index","education_index")){
  print(c)
  print(summary(tract_wide[c]))
}

library(tigris)
map_sf <- tracts("CA","San Francisco", year=2016)
map_sf <- map_sf@data # just grab associated land and mass data
names(map_sf)[9:10] <- c("land","water")
map_sf <- map_sf[,c(4,9,10)]

tract_wide <- left_join(tract_wide, map_sf, by="GEOID")
tract_wide <- tract_wide %>% filter(land>0)
# want area with at least some ground

write_csv(tract_wide, "census_tract_2009_2017.csv")
write_csv(block_wide, "census_block_2013_2017.csv")
write_csv(census_data_tract, "census_tract_2009_2017_long.csv")
write_csv(census_data_block, "census_block_2013_2017_long.csv")
# translation <- data.frame(name=c(population_vars, income_vars, race_vars,
#                                      housing_vars, mobility_vars, education_vars), 
#                           label=c(population_names, income_names, race_names,
#                                        housing_names, mobility_names, education_names))
# 
# ## quick check that 2009-2017 same 
# for(y in 2010:2010){
#   vars <- load_variables(y, "acs5", cache = TRUE)
#   vars <- subset(vars, name %in% translation$name)
#   if(nrow(vars) ==nrow(translation)){
#     vars$label = gsub("( |!!)", "_", 
#                       gsub("(Estimate!!Total!!|\\$|,)","",
#                            vars$label))
#     check <- full_join(vars, translation, by=c("label","name"))
#     if(nrow(check)!=nrow(translation)){
#       cat('error in year', y, nrow(check))
#     }
#   }else{
#     cat('error in year ', y)
#   }
# }
# 
