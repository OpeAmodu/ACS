library(tidyverse)

#reading in data
nhis18 <- read.csv("final_data_2018.csv")
str(nhis18)

#Creating a day of month variable
nhis18$DAY <- 1

# formatting the region variable
nhis18$REGION <- ifelse(nhis18$REGION ==1, "Northeast", 
                        ifelse(nhis18$REGION ==2, "Midwest",
                               ifelse(nhis18$REGION == 3, "South",
                                      ifelse(nhis18$REGION == 4, "West", NA))))

# formatting and creating a new variable for insurance coverage
# - I used both COVER and COVER650 Variables 
# - COVER variable is for adults who are less than 65 years old
# - COVER65o is for adults who are 65 years or older 
# I changed missing data(NA) to 99 so that it does not affect the creation of the coverage variable 
is.na(nhis18$COVER) <- 99
is.na(nhis18$COVER65O) <- 99


nhis18$COVERAGE <- ifelse(nhis18$AGE_P<65 & nhis18$COVER==1| nhis18$AGE_P>64 & nhis18$COVER65O==1, "Private",
                          ifelse(nhis18$AGE_P<65 & nhis18$COVER == 2|nhis18$AGE_P>64 & nhis18$COVER65O == 2, "Medicaid (includes people with Medicare and Medicaid)",
                                        ifelse(nhis18$AGE_P>64 & nhis18$COVER65O == 3, "Medicare Only",
                                               ifelse(nhis18$AGE_P<65 & nhis18$COVER==3| nhis18$AGE_P>64 & nhis18$COVER65O==4, "Other Coverage",
                                                      ifelse(nhis18$AGE_P<65 & nhis18$COVER==4| nhis18$AGE_P>64 & nhis18$COVER65O==5, "Uninsured", 
                                                             NA)))))

#formatting the age variable
nhis18$binned_age <- cut(nhis18$AGE_P, c(18, 24, 44, 64, Inf),
                         labels = c("18-24", "25-44", "45-64", "65+"), include.lowest = TRUE)

#formatting the sex variable
nhis18$SEX <- ifelse(nhis18$SEX == 1, "Men",
                     ifelse(nhis18$SEX ==2, "Women", NA))


#formatting the Race variable
nhis18$RACERPI2 <- ifelse(nhis18$RACERPI2 == 01,  "white",
                          ifelse(nhis18$RACERPI2 == 02, "Black/African American",
                                 ifelse(nhis18$RACERPI2 == 03, "AIAN",
                                       ifelse(nhis18$RACERPI2 == 04,  "Asian",
                                              ifelse(nhis18$RACERPI2 == 06, "Multiple", NA)))))
                          

#formatting the education variable
nhis18$EDUC1 <- ifelse(nhis18$EDUC1 == 97|nhis18$EDUC1 == 98|nhis18$EDUC1 == 99|nhis18$EDUC1 == 96, NA, nhis18$EDUC1)
nhis18$EDUC1 <- cut(nhis18$EDUC1, c(00, 11, 14, 17, Inf), labels = c("<HS Grad", "HS Grad", "<College Grad",
                                                                     "College Grad+"), include.lowest = TRUE)

#formatting the ratio of income to federal poverty level variable
nhis18$RAT_CAT5 <- ifelse(nhis18$RAT_CAT5 == 96| nhis18$RAT_CAT5 == 99, NA,
                          ifelse(nhis18$RAT_CAT5 == 15, 03, 
                                 ifelse(nhis18$RAT_CAT5 == 16, 07,
                                        ifelse(nhis18$RAT_CAT5 == 17, 09,
                                               ifelse(nhis18$RAT_CAT5 == 18, 13, 
                                                      nhis18$RAT_CAT5)))))


nhis18$binned_RATCAT <- cut(nhis18$RAT_CAT5, breaks = c(01, 03, 07, 11, Inf),
                            labels = c("<100%", "100 - 200 %","200 - 400 %", ">400%" ),
                            include.lowest = TRUE)

# formatting and creating the sexual orientation for male variable
# The 2018 data has different variables for male and female sexual orientations
# I first made missing data for each to be 99 so that it does not affect the creation of new variable
# The final variable I created is called ORIENT_A
nhis18$ASISIM[is.na(nhis18$ASISIM)] <- 99

nhis18$ASISIF[is.na(nhis18$ASISIF)] <- 99

nhis18$ORIENT_A <- ifelse(nhis18$ASISIM ==1| nhis18$ASISIF == 1, 1,
                          ifelse(nhis18$ASISIM ==2| nhis18$ASISIF == 2, 2,
                                 ifelse(nhis18$ASISIM ==3| nhis18$ASISIF == 3, 3,
                                        ifelse(nhis18$ASISIM ==4| nhis18$ASISIF == 4, 4,
                                               ifelse(nhis18$ASISIM ==5| nhis18$ASISIF == 5, 5,
                                                      ifelse(nhis18$ASISIM ==7| nhis18$ASISIF == 7, 7,
                                                             ifelse(nhis18$ASISIM ==8| nhis18$ASISIF == 8, 8, "")))))))

nhis18$ORIENT_A <- ifelse(nhis18$ORIENT_A == 1, "Gay/Lesbian", 
                          ifelse(nhis18$ORIENT_A == 2, "Straight",
                                 ifelse(nhis18$ORIENT_A == 3, "Bisexual", 
                                        ifelse(nhis18$ORIENT_A == 4, "Something else",
                                               ifelse(nhis18$ORIENT_A == 5, "Don't know the answer", NA)))))


#formatting the anxiety variable
nhis18$ANXEV_A <- ifelse(nhis18$ANX_1 == 1|nhis18$ANX_1 == 2|nhis18$ANX_1 == 3,"Yes",
                         ifelse(nhis18$ANX_1 == 4|nhis18$ANX_1 == 5, "No", NA))


# formatting the depression variable
nhis18$DEPEV_A <- ifelse(nhis18$DEP_1 == 1|nhis18$DEP_1 == 2|nhis18$DEP_1 == 3,"Yes",
                         ifelse(nhis18$DEP_1 == 4|nhis18$DEP_1 == 5, "No", NA))


# formatting and creating variable for mental health status
nhis18$ANXEV_A[is.na(nhis18$ANXEV_A)] <- "99"
nhis18$DEPEV_A[is.na(nhis18$DEPEV_A)] <- "99"

nhis18$mental_health_problem <- ifelse(nhis18$ANXEV_A == "No"| nhis18$DEPEV_A == "No", "No",
                                       ifelse(nhis18$ANXEV_A == "Yes"| nhis18$DEPEV_A == "Yes", "Yes", NA))


#formatting the house own variable
nhis18$HOUSEOWN <- ifelse(nhis18$HOUSEOWN == 7| nhis18$HOUSEOWN == 8| nhis18$HOUSEOWN == 9, NA,
                          ifelse(nhis18$HOUSEOWN == 1, "Yes", "No"))




# formatting the food security variable
# food security relies on 9 variables, 
# The NHIS has documentation of how to group the variables and how to calculate FOOD SECURITY
# For each variable if the person answers affirmative for food security for the question I scored them 0 for the variable 
# if the person does  not have food security for the question asked, I scored them 1 for the variable 
# I created a final variable called FOOD_SECURITY, where i totaled the scores
# if the individual scored between 0 and 1, they have high food security, low is when they score between 2 and 4, 
# very low is when they score 5 and above

nhis18$FSRUNOUT <- ifelse(nhis18$FSRUNOUT == 7|nhis18$FSRUNOUT == 8|nhis18$FSRUNOUT == 9, NA,
                          ifelse(nhis18$FSRUNOUT == 1|nhis18$FSRUNOUT == 2, 1, 0))
nhis18$FSBALANC <- ifelse(nhis18$FSBALANC == 7|nhis18$FSBALANC == 8|nhis18$FSBALANC == 9, NA,
                          ifelse(nhis18$FSBALANC == 1|nhis18$FSBALANC == 2, 1, 0))
nhis18$FSLAST <- ifelse(nhis18$FSLAST == 7|nhis18$FSLAST == 8|nhis18$FSLAST == 9, NA,
                        ifelse(nhis18$FSLAST == 1|nhis18$FSSKIP == 2, 1, 0))
nhis18$FSSKIP <- ifelse(nhis18$FSSKIP == 7|nhis18$FSSKIP == 8|nhis18$FSSKIP == 9, NA,
                        ifelse(nhis18$FSSKIP == 1, 1, 0))
nhis18$FSSKDAYS <- ifelse(nhis18$FSSKDAYS == 97|nhis18$FSSKDAYS == 98|nhis18$FSSKDAYS == 99, NA,
                          ifelse(nhis18$FSSKDAYS >= 3, 1, 0))
nhis18$FSNEDAYS <- ifelse(nhis18$FSNEDAYS == 97|nhis18$FSNEDAYS == 98|nhis18$FSNEDAYS == 99, NA,
                          ifelse(nhis18$FSNEDAYS >= 3, 1, 0))
nhis18$FSLESS <- ifelse(nhis18$FSLESS  == 7|nhis18$FSLESS  == 8|nhis18$FSLESS  == 9, NA,
                        ifelse(nhis18$FSLESS  == 1, 1, 0))
nhis18$FSHUNGRY <- ifelse(nhis18$FSHUNGRY  == 7|nhis18$FSHUNGRY  == 8|nhis18$FSHUNGRY  == 9, NA,
                          ifelse(nhis18$FSHUNGRY == 1, 1, 0))
nhis18$FSWEIGHT <- ifelse(nhis18$FSWEIGHT  == 7|nhis18$FSWEIGHT  == 8|nhis18$FSWEIGHT  == 9, NA,
                          ifelse(nhis18$FSWEIGHT == 1, 1, 0))
nhis18$FSNOTEAT <- ifelse(nhis18$FSNOTEAT  == 7|nhis18$FSNOTEAT  == 8|nhis18$FSNOTEAT == 9, NA,
                          ifelse(nhis18$FSNOTEAT == 1, 1, 0))
nhis18$FOOD_SECURITY <- rowSums(nhis18[, c('FSRUNOUT', 'FSLAST', 'FSBALANC', 'FSSKIP', 'FSSKDAYS', 'FSNOTEAT','FSNEDAYS', 'FSLESS','FSHUNGRY','FSWEIGHT')], na.rm = TRUE)
nhis18$FOOD_SECURITY<- cut(nhis18$FOOD_SECURITY, breaks = c(0, 2, 5, Inf),
                           labels = c("high", "low","very low"),
                           include.lowest = TRUE)

# formatting the cigarette smoking variable
nhis18$SMKCIGST_A <- ifelse(nhis18$SMKSTAT2 == 1|nhis18$SMKSTAT2 == 2, 1, ifelse(nhis18$SMKSTAT2 ==5|
                                                                                   nhis18$SMKSTAT2 ==9,
                                                                                 NA, 2))
nhis18$SMKCIGST_A <- factor(nhis18$SMKCIGST_A, labels = c("Yes", "No"))


# formatting the e-cigarette smoking variable
nhis18$SMKECIGST_A <- ifelse(nhis18$ECIGCUR2 == 1|nhis18$ECIGCUR2 == 2, 1, ifelse(nhis18$ECIGCUR2 == 3, 2, NA))
nhis18$SMKECIGST_A[is.na(nhis18$SMKECIGST_A )] <- 99
nhis18$SMKECIGST_A <- ifelse(nhis18$SMKECIGST_A == 1, 1, ifelse(nhis18$SMKECIGST_A == 2, 2, 
                                                                ifelse(nhis18$ECIGEV2 == 2,2, NA)))
nhis18$SMKECIGST_A <- factor(nhis18$SMKECIGST_A, labels = c("Yes","No"))


# formatting the cigar use variable
nhis18$CIGARST_A <- ifelse(nhis18$CIGCUR2 == 1|nhis18$CIGCUR2 == 2, 1, ifelse(nhis18$CIGCUR2 == 3, 2, NA))
nhis18$CIGARST_A[is.na(nhis18$CIGARST_A)] <- 99
nhis18$CIGARST_A <- ifelse(nhis18$CIGARST_A == 1, 1, ifelse(nhis18$CIGARST_A == 2, 2, 
                                                            ifelse(nhis18$CIGAREV2 == 2,2, NA)))
nhis18$CIGARST_A  <- factor(nhis18$CIGARST_A, labels = c("Yes","No"))


# formatting the pipe smoking variable
nhis18$PIPEST_A <- ifelse(nhis18$PIPECUR2 == 1|nhis18$PIPECUR2 == 2, 1, ifelse(nhis18$PIPECUR2 == 3, 2, NA))
nhis18$PIPEST_A[is.na(nhis18$PIPEST_A)] <- 99
nhis18$PIPEST_A <- ifelse(nhis18$PIPEST_A == 1, 1, ifelse(nhis18$PIPEST_A== 2, 2, 
                                                          ifelse(nhis18$PIPEV2 == 2,2, NA)))
nhis18$PIPEST_A  <- factor(nhis18$PIPEST_A, labels = c("Yes","No"))


# formatting the smokeless tobacco use variable
nhis18$SMOKELSST_A <- ifelse(nhis18$SMKLSCR2 == 1|nhis18$SMKLSCR2 == 2, 1, ifelse(nhis18$SMKLSCR2 == 3, 2, NA))
nhis18$SMOKELSST_A[is.na(nhis18$SMOKELSST_A)] <- 99
nhis18$SMOKELSST_A <- ifelse(nhis18$SMOKELSST_A == 1, 1, ifelse(nhis18$SMOKELSST_A== 2, 2, 
                                                                ifelse(nhis18$SMKLSTB1 == 2,2, NA)))
nhis18$SMOKELSST_A  <- factor(nhis18$SMOKELSST_A, labels = c("Yes","No"))


# formatting the cigarette and e-cigarette dual use variable
nhis18$cig_ecig <- ifelse(nhis18$SMKCIGST_A == "Yes" & nhis18$SMKECIGST_A == "Yes", "Yes", "No")

# formatting the cigarette and cigar dual use variable
nhis18$cig_cigar <- ifelse(nhis18$SMKCIGST_A == "Yes" & nhis18$CIGARST_A == "Yes", "Yes", "No")

# formatting the cigarette and pipe dual use variable
nhis18$cig_pipe <- ifelse(nhis18$SMKCIGST_A == "Yes" & nhis18$PIPEST_A == "Yes", "Yes", "No")

# formatting the cigarette and smokeless tobacco dual use variable
nhis18$cig_smkles <- ifelse(nhis18$SMKCIGST_A == "Yes" & nhis18$SMOKELSST_A == "Yes", "Yes", "No")

table(nhis18$COVERAGE)
# creating a csv file for the cleaned data
write_csv(nhis18, "final_data_2018_clean.csv")

