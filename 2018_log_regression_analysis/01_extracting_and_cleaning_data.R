library(tidyverse)
library(lubridate)

# read in data
nhis18_modelling <- read.csv("../2018_analysis/final_data_2018_clean.csv")
  
  
  
#extracting variables needed for regression modelling
nhis18_modelling <- filter(nhis18_modelling, SMKSTAT2 <= 3)
nhis18_modelling <- nhis18_modelling %>%
  select(HHX, PPSU, PSTRAT, WTFA_SA, YEAR = SRVY_YR, MONTH = INTV_MON, DAY = DAY,
         REGION, AGE=binned_age, SEX, RACE=RACERPI2,
         EDUCATION=EDUC1, `INCOME RATIO TO FPL`=binned_RATCAT, `SEXUAL ORIENTATION`=ORIENT_A,
         DEPEV_A, ANXEV_A, `MENTAL HEALTH PROBLEM`=mental_health_problem, HOUSEOWN,
         `INCOME GROUP`=INCGRP5,
         COVERAGE,
         FM_SIZE, 
         `FOOD SECURITY`=FOOD_SECURITY, CIGQTYR, SMKSTAT2,
         SMKQTNO, SMKQTTP, SMKQTY)

#formatting the Race variable
nhis18_modelling$RACE <- ifelse(nhis18_modelling$RACE == "white",  "White",
                          ifelse(nhis18_modelling$RACE == "Black/African American", "Black/African American",
                                 ifelse(nhis18_modelling$RACE == "AIAN"|
                                          nhis18_modelling$RACE == "Asian"|
                                          nhis18_modelling$RACE == "Multiple", "Other", NA)))

#formatting the family income variable
nhis18_modelling$`INCOME GROUP` <- ifelse(nhis18_modelling$`INCOME GROUP` == 01, "$0-34,999",
                         ifelse(nhis18_modelling$`INCOME GROUP` == 02, "$35,000 - 74,999",
                                ifelse(nhis18_modelling$`INCOME GROUP` == 03, "$75,000 - 99,999",
                                       ifelse(nhis18_modelling$`INCOME GROUP` == 04, "$100,000 and over",
                                              NA))))

#formatting the sexual orientation variable
nhis18_modelling$`SEXUAL ORIENTATION` <-  ifelse(nhis18_modelling$`SEXUAL ORIENTATION` == "Straight", "Straight",
                                                  ifelse(nhis18_modelling$`SEXUAL ORIENTATION` == "Bisexual"|
                                                          nhis18_modelling$`SEXUAL ORIENTATION` == "Something else"|
                                                          nhis18_modelling$`SEXUAL ORIENTATION` == "Don't know the answer"|
                                                          nhis18_modelling$`SEXUAL ORIENTATION` == "Gay/Lesbian",
                                                              "Other", NA)) 
#formatting the food security variable
nhis18_modelling$`FOOD SECURITY` <- ifelse(nhis18_modelling$`FOOD SECURITY` == "high", "High",
                                 ifelse(nhis18_modelling$`FOOD SECURITY` == "low"|
                                          nhis18_modelling$`FOOD SECURITY` == "very low",
                                 "Low", NA))



                                                            

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
                                                ifelse(nhis18_modelling$QUIT_MONTHS >= 6 & nhis18_modelling$QUIT_MONTHS <= 12, 3, 
                                                       ifelse(nhis18_modelling$QUIT_MONTHS >12, 4, ""))))

#creating variable for date since quit smoking
nhis18_modelling$DATE_QUIT_SMOKING <- as.character(ymd(nhis18_modelling$DATE_INTERVIEW) - months(nhis18_modelling$QUIT_MONTHS))

#year of quit smoking
nhis18_modelling$QUIT_SMOKING_YEAR <- year(ymd(nhis18_modelling$DATE_QUIT_SMOKING))

#formatting attempt to quit smoking
# 1 means attempted to quit smoking
# 2 means did not attempt to quit smoking in the past year
nhis18_modelling$CIGQTYR <- ifelse(nhis18_modelling$CIGQTYR == 1, 1,
                                   ifelse(nhis18_modelling$CIGQTYR == 2, 0, NA))

#We took out data for people who are between 18 to 24 years old.
#if people aged 18 to 24 are needed, commenting out the code below and running
# the whole program would return a data set that includes people who are 18 to 24

nhis18_modelling <- subset(nhis18_modelling, nhis18_modelling$AGE != "18-24")

#This code saves a CSV ready for the log regression analysis into the directory
write_csv(nhis18_modelling, "nhis18_modelling.csv")
