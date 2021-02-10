

library(tidyverse)

## renaming data
household2018 <- read.csv("../2018_datasets/househld.csv")
family2018 <- read.csv("../2018_datasets/familyxx.csv")
person2018 <- read.csv("../2018_datasets/personsx.csv")
adult2018 <- read.csv("../2018_datasets/samadult.csv")


##Extracting needed variables from household 2018
household2018_lite <- household2018 %>%
    select(HHX, SRVY_YR, ACPT_FAM, WTFA_HH, PSTRAT, PPSU, INTV_MON)

#extracting needed variables from family 2018
family2018_lite <- family2018 %>%
    select(SRVY_YR, HHX, FMX, WTFA_FAM,
           FM_SIZE, FM_KIDS, FM_ELDR, FM_TYPE, 
           HOUSEOWN,
           FSRUNOUT, FSLAST, FSBALANC, FSSKIP, FSSKDAYS, FSLESS, FSHUNGRY, FSWEIGHT, FSNOTEAT, FSNEDAYS,
           FHICOVYN, FHICOVCT, FHIPRVCT, FHIEXCT, FHISINCT, FHICARCT, FHICHPCT, FHIMILCT, FHIEBCCT, 
           INCGRP5, RAT_CAT5)

#extracting needed variables from adult 2018
adult2018_lite <- adult2018 %>%
    select(SRVY_YR, HHX, FMX, FPX, WTFA_SA,
             FMX, AGE_P, SEX, REGION, RACERPI2, R_MARITL, ASISIM, ASISIF,
             DOINGLWA, WRKCATA, ONEJOB, WRKLYR4, YRSWRKPA,  
             AWORPAY,
             COG_SS, COGCAUS2, ANX_1, DEP_1,
             SMKEV, SMKREG, SMKNOW, SMKSTAT2,SMKQTNO, SMKQTTP, SMKQTY, CIGSDA1, CIGDAMO, CIGSDA2, CIGSDAY, CIGQTYR, 
             ECIGEV2, ECIGCUR2, ECIG30D2, CIGAREV2, CIGCUR2, CIG30D2, PIPEV2, PIPECUR2, SMKLSTB1, SMKLSCR2) 

#extracting needed variables from family 2018
person2018_lite <- person2018 %>%
    select(SRVY_YR, HHX, FMX, FPX, EDUC1)


first_join <- merge(household2018_lite, family2018_lite, by =c("SRVY_YR", "HHX"))
second_join <- merge(first_join, adult2018_lite, by=c("SRVY_YR", "HHX", "FMX"))
final_join <- merge(second_join, person2018_lite, by=c("SRVY_YR", "HHX", "FMX", "FPX"))

#Rearranging the variables after merging
final_data_2018 <- final_join %>%
    select(SRVY_YR, INTV_MON, FMX, FPX,
         HHX, ACPT_FAM, WTFA_HH, WTFA_FAM, WTFA_SA, PSTRAT, PPSU,
        AGE_P, SEX, REGION, RACERPI2, R_MARITL, ASISIM, ASISIF,
        HOUSEOWN, FM_SIZE, FM_KIDS, FM_ELDR, FM_TYPE, EDUC1,
        DOINGLWA, WRKCATA, ONEJOB, WRKLYR4, YRSWRKPA, 
        INCGRP5, RAT_CAT5,
        FHICOVYN, FHICOVCT, FHIPRVCT, FHIEXCT, FHISINCT, FHICARCT, FHICHPCT, FHIMILCT, 
        FSRUNOUT, FSLAST, FSBALANC, FSSKIP, FSSKDAYS, FSLESS, FSHUNGRY, FSWEIGHT, FSNOTEAT, FSNEDAYS,
        AWORPAY,
        COG_SS, COGCAUS2, ANX_1, DEP_1,
        SMKEV, SMKREG, SMKNOW, SMKSTAT2, SMKQTNO, SMKQTTP, SMKQTY, CIGSDA1, CIGDAMO, CIGSDA2, CIGQTYR, 
        ECIGEV2, ECIGCUR2, ECIG30D2, CIGAREV2, CIGCUR2, CIG30D2, 
        PIPEV2, PIPECUR2, SMKLSTB1, SMKLSCR2)

final_data_2018 <- final_data_2018 %>%
    filter(AGE_P >= 18)

str(final_data_2018)

#Writing final data set as a CSV file
write_csv(final_data_2018, "final_data_2018.csv")





