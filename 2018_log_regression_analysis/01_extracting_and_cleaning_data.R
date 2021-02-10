library(tidyverse)
library(lubridate)

# read in data
nhis18_modelling <- read.csv("../2018_analysis/final_data_2018_clean.csv")
  
  
  
#extracting variables needed for regression modelling
nhis18_modelling <- filter(nhis18_modelling, SMKSTAT2 <= 3)
nhis18_modelling <- nhis18_modelling %>%
  select(HHX, PPSU, PSTRAT, WTFA_SA, YEAR = SRVY_YR, MONTH = INTV_MON, DAY = DAY, REGION, binned_age, SEX, RACERPI2, EDUC1, binned_RATCAT,
         ORIENT_A,DEPEV_A, ANXEV_A ,mental_health_problem, HOUSEOWN, INCGRP5, FM_SIZE, FOOD_SECURITY, CIGQTYR, SMKSTAT2,
         SMKQTNO, SMKQTTP, SMKQTY)

#formatting the variable for income per capita
if(nhis18_modelling$INCGRP5 == 96| nhis18_modelling$INCGRP5 == 99){
  nhis18_modelling$inc_per_capita <- NA
}else if(nhis18_modelling$INCGRP5 == 1){
  nhis18_modelling$inc_per_capita <- (0 + 34999)/nhis18_modelling$FM_SIZE
}else if(nhis18_modelling$INCGRP5 == 2){
  nhis18_modelling$inc_per_capita <- (35000 + 74999)/nhis18_modelling$FM_SIZE
}else if(nhis18_modelling$INCGRP5 == 3){
  nhis18_modelling$inc_per_capita <- (75000 + 99999)/nhis18_modelling$FM_SIZE
}else{
  nhis18_modelling$inc_per_capita <- (100000 + 125000)/nhis18_modelling$FM_SIZE
}

#formatting the family income variable
nhis18_modelling$INC_GRP <- ifelse(nhis18_modelling$INCGRP5 == 01, "$0-34,999",
                         ifelse(nhis18_modelling$INCGRP5 == 02, "$35,000 - 74,999",
                                ifelse(nhis18_modelling$INCGRP5 == 03, "$75,000 - 99,999",
                                       ifelse(nhis18_modelling$INCGRP5 == 04, "$100,000 and over",
                                              NA))))


#assigning date of interview variable
nhis18_modelling$DATE_INTERVIEW <- as.character(make_date(nhis18_modelling$YEAR, 
                                                              nhis18_modelling$MONTH,
                                                              nhis18_modelling$DAY))
#assigning missing variables to the quit variables
nhis18_modelling$SMKQTNO <- ifelse(nhis18_modelling$SMKQTNO == 97|nhis18_modelling$SMKQTNO == 98|nhis18_modelling$SMKQTNO == 99, NA,
                                       nhis18_modelling$SMKQTNO)
nhis18_modelling$SMKQTTP <- ifelse(nhis18_modelling$SMKQTTP == 7| nhis18_modelling$SMKQTTP == 8|nhis18_modelling$SMKQTTP == 9, NA,
                                       nhis18_modelling$SMKQTTP)
nhis18_modelling$SMKQTY <- ifelse(nhis18_modelling$SMKQTY == 97|nhis18_modelling$SMKQTY == 98| nhis18_modelling$SMKQTY == 99, NA,
                                      nhis18_modelling$SMKQTY)

#creating new variable that converts all quit times to no of days since quit
nhis18_modelling$QUIT_DAY <- ifelse(nhis18_modelling$SMKQTTP == 1, nhis18_modelling$SMKQTNO,
                                        ifelse(nhis18_modelling$SMKQTTP == 2, nhis18_modelling$SMKQTNO * 7,
                                               ifelse(nhis18_modelling$SMKQTTP == 3, nhis18_modelling$SMKQTNO *30,
                                                      ifelse(nhis18_modelling$SMKQTTP == 4, nhis18_modelling$SMKQTNO * 365, NA))))

#creating new variable that converts all quit times to month(s) since quit
nhis18_modelling$QUIT_MONTHS <- floor(nhis18_modelling$QUIT_DAY/30)

# Creating a new variable that groups quit by months of quit using QUIT_MONTH and SMKSTAT2 variables
# 1 = current smokers
# 2 = people who have quit less than 6 months
# 3 = people who quit between 6 to 11 months
# 4 = people who quit at least 1 year

nhis18_modelling$QUIT_TYPE <- ifelse(nhis18_modelling$SMKSTAT2 == 1| nhis18_modelling$SMKSTAT2 == 2, 1,
                                         ifelse(nhis18_modelling$QUIT_MONTHS < 6, 2,
                                                ifelse(nhis18_modelling$QUIT_MONTHS >= 6 & nhis18_modelling$QUIT_MONTHS < 12, 3, 
                                                       ifelse(nhis18_modelling$QUIT_MONTHS >=12, 4, ""))))

#creating variable for date since quit smoking
nhis18_modelling$DATE_QUIT_SMOKING <- as.character(ymd(nhis18_modelling$DATE_INTERVIEW) - months(nhis18_modelling$QUIT_MONTHS))

#year of quit smoking
nhis18_modelling$QUIT_SMOKING_YEAR <- year(ymd(nhis18_modelling$DATE_QUIT_SMOKING))

#formatting attempt to quit smoking
# 1 means attempted to quit smoking
# 2 means did not attempt to quit smoking in the past year
nhis18_modelling$CIGQTYR <- ifelse(nhis18_modelling$CIGQTYR == 1, 1,
                                   ifelse(nhis18_modelling$CIGQTYR == 2, 0, NA))

write.csv(nhis18_modelling, "nhis18_modelling.csv")

