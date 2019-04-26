setwd("~/Cal/Spring2019/Stat222")

#####
#### Author: Andrea Bonilla
#### 

library(tidyverse)
fn <- list.files("data/", full.names = TRUE)
data0 <- read_csv(fn[2], col_types = cols(
  IncidntNum = col_integer(),
  Category = col_character(),
  Descript = col_character(),
  DayOfWeek = col_character(),
  Date = col_date(format="%m/%d/%Y"),
  Time = col_time(format = "%H:%M"),
  PdDistrict = col_character(),
  Resolution = col_character(),
  Address = col_character(),
  X = col_double(),
  Y = col_double(),
  Location = col_character(),
  PdId = col_double()
))

names(data0)[c(10:11)] <- c("lng","lat")
data0 <- data0[!is.na(data0$PdDistrict),]
library(lubridate)
data0$year <- year(data0$Date)

data0 <- filter(data0, year < 2018) # keep earlier data...data dif now

data1 <- read_csv(fn[1],
                  col_names = c("IncidntDatetime","Date",
                                "Time", "year",
                                "DayOfWeek",
                                "ReportDatetime", "RowID",
                                "IncidntID","IncidntNum",
                                "CADNum","ReportType",
                                "ReportTypeDescr",
                                "FiledOnline","IncidntCode",
                                "Category",
                                "Subcategory",
                                "Descript",
                                "Resolution", "Address",
                                "CNN","PdDistrict","AnalysisNeighborhood",
                                "SupervisorDistrict",
                                "lat","lng","Location"),
                  skip = 1,
                  col_types = cols(
                    .default = col_character(),
                    IncidntDatetime = col_datetime("%Y/%m/%d %H:%M:%S %p"),
                    Date = col_date(format = ""),
                    Time = col_time(format = ""),
                    year = col_integer(),
                    ReportDatetime = col_datetime("%Y/%m/%d %H:%M:%S %p"),
                    RowID = col_double(),
                    IncidntID = col_integer(),
                    IncidntNum = col_integer(),
                    CADNum = col_integer(),
                    CNN = col_double(),
                    lat = col_double(),
                    lng = col_double()
                  ))

data1 <- filter(data1, PdDistrict!="Out of SF")

data1$Category <- toupper(data1$Category)
data1$PdDistrict <- toupper(data1$PdDistrict)
data1$Descript <- toupper(data1$Descript)

# A tibble: 2 x 2
# year na_pd
# <int> <int>
#   1  2018  3951
# 2  2019   408
## how many are out of SF by year...
#high compared to before with just 1 PdDistrict is NA

## COMBINE 
data <- full_join(data0, data1)

nrow(data0) + nrow(data1) == nrow(data)

## data cleaning
#data$year <- year(data$Date)
data$month <- month(data$Date)
data$day <- day(data$Date)

#according to https://en.wikipedia.org/wiki/White-collar_crime#Blue-collar_crime

white_crime=c("FRAUD", "FORGERY/COUNTERFEITING", "BAD CHECKS" , "EXTORTION", 
              "EMBEZZLEMENT", "SUSPICIOUS OCC","BRIBERY",
              "FORGERY AND COUNTERFEITING")

blue_crime=c("VANDALISM", "LARCENY/THEFT", "STOLEN PROPERTY", "ROBBERY", 
             "DRIVING UNDER THE INFLUENCE",  "DISORDERLY CONDUCT", "LIQUOR LAWS", 
             "VEHICLE THEFT", "ASSAULT", "KIDNAPPING", "TRESPASS", 
             "ARSON", "RECOVERED VEHICLE", "BURGLARY", "LARCENY THEFT",
             "TRAFFIC VIOLATION ARREST", "MOTOR VEHICLE THEFT",
             "MOTOR VEHICLE THEFT?",
             "VEHICLE IMPOUNDED","VEHICLE MISPLACED")

sex_crime=c("SEX OFFENSES NON FORCIBLE", "SEX OFFENSES FORCIBLE", 
            "SEX OFFENSES, NON FORCIBLE", "SEX OFFENSES, FORCIBLE",
            "PROSTITUTION", "RAPE", "HUMAN TRAFFICKING, COMMERCIAL SEX ACTS",
            "HUMAN TRAFFICKING (A), COMMERCIAL SEX ACTS",
            "SEX OFFENSE")

drug_crime <- c("DRUG OFFENSE","DRUG/NARCOTIC",
                "DRUG VIOLATION")

other_crime=c("MISSING PERSON", "RUNAWAY", "FAMILY OFFENSES", 
              "PORNOGRAPHY/OBSCENE MAT", "WEAPON LAWS", "DRUNKENNESS", "SUICIDE", 
              "TREA",  "LOITERING",
              "OTHER OFFENSES", "NON-CRIMINAL", "GAMBLING", "WARRANTS",
              "SECONDARY CODES", "OTHER MISCELLANEOUS",
              "MALICIOUS MISCHIEF", "MISCELLANEOUS INVESTIGATION",
              "LOST PROPERTY", "VEHICLE IMPOUNDED", "OTHER", "JUVENILE OFFENSES",
              "OFFENCES AGAINST THE FAMILY AND CHILDREN", "WEAPONS CARRYING ETC",
              "WARRANT", "HOMICIDE",
              "WEAPONS OFFENCE", "COURTESY REPORT", "FIRE REPORT",
              "TRAFFIC COLLISION", "FAMILY OFFENSE", "WEAPONS OFFENSE",
              "CASE CLOSURE", "CIVIL SIDEWALKS", "SUSPICIOUS")

violent_crime=c("ROBBERY", "ASSAULT", "KIDNAPPING", 
                "RAPE", 
                "HOMICIDE")

data$CatDesc <- ifelse(data$Category %in% white_crime, "WhiteCollar",
                       ifelse(data$Category %in% blue_crime, "BlueCollar", 
                              ifelse(data$Category %in% drug_crime, "Drug", 
                                     ifelse(regexpr("DRUG", data$Descript) != -1 &
                                              regexpr("SEXUAL ASSAULT", data$Descript) == -1 , "Drug", # without word sexual assualt
                                            ifelse(data$Category %in% other_crime, "Other", 
                                                   ifelse(data$Category %in% sex_crime, "SexCrime",
                                                          NA))))))

data$marijuana <- ifelse(regexpr("MARIJUANA", data$Descript) != -1,
                         1, 0)
data$meth <-ifelse(regexpr("METHAMPHETAMINE|AMPHETAMINE|METH-AMPHETAMINE", 
                           data$Descript) != -1, 1, 0)
data$opium <- ifelse(regexpr("OPIUM", data$Descript) != -1, 1, 0)

#data$violent <- ifelse(data$Category %in% violent_crime, 1, 0)

data$theft <- ifelse(data$Category %in% c("LARCENY/THEFT","LARCENY THEFT"),
                     1, 0)
data$robbery <- ifelse(data$Category %in% c("ROBBERY"), 1, 0)
data$assault <- ifelse(data$Category %in% c( "ASSAULT"), 1, 0)
data$vehicle_theft <- ifelse(data$Category %in% c("VEHICLE THEFT", 
                                                  "MOTOR VEHICLE THEFT",
                                                  "MOTOR VEHICLE THEFT?"),
                             1, 0)
data$burglary <- ifelse(data$Category %in% c("BURGLARY"), 1, 0)


sum(is.na(data$CatDesc))

data$quarter <- quarter(data$Date)


rm(list=(setdiff(ls(),'data')))

