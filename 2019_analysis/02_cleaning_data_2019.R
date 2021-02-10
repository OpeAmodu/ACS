# This script relies on cleaned data from the "extracting_needed_files_2019.R" module
# assigning the 2019 data set

nhis19 <- read.csv("final_data_2019.csv")

# making region a factor and setting labels
nhis19$REGION <- factor(nhis19$REGION, labels = c("Northeast", "Midwest", "South", "West"))

# making age variable a factor and grouping the age groups
nhis19$binned_age <- cut(nhis19$AGEP_A, c(18, 29, 49, Inf), labels = c("18-29", "30-49", "50+"), include.lowest = TRUE)

# formatting the sex variables and labeling the groups
nhis19$SEX_A <- ifelse(nhis19$SEX_A == 1 | nhis19$SEX_A ==2, nhis19$SEX_A, NA)
nhis19$SEX_A <- factor(nhis19$SEX_A, labels = c("male", "female"))

# formatting the race variable and labeling the groups
nhis19$RACEALLP_A <- ifelse(nhis19$RACEALLP_A == 7| nhis19$RACEALLP_A == 8| nhis19$RACEALLP_A == 9,
                            NA,nhis19$RACEALLP_A)
nhis19$RACEALLP_A <-factor(nhis19$RACEALLP_A, labels = c("White", "Black/African American", "Asian", "AIAN",
                                                         "AIAN and other", "Other single/multiple"))

# formatting the education variable and labeling the groups
nhis19$EDUC_A <- ifelse(nhis19$EDUC_A == 97|nhis19$EDUC_A == 98|nhis19$EDUC_A == 99, NA, nhis19$EDUC_A )
nhis19$binned_EDUC <- cut(nhis19$EDUC_A, c(00, 03, 04, 07, Inf), labels = c("<HS Grad", "HS Grad", "<College Grad",
                                                                            "College Grad+"), include.lowest = TRUE)

# formatting the ratio of family income to poverty level variable and labeling the groups
nhis19$RATCAT_A <- ifelse(nhis19$RATCAT_A == 98, NA, nhis19$RATCAT_A)
nhis19$binned_RATCAT <- cut(nhis19$RATCAT_A, breaks = c(01, 03, 07, 11, Inf),
                            labels = c("<100%", "100 - 200 %","200 - 400 %", ">400%" ),
                            include.lowest = TRUE)

# formatting the sexual orientation variables and labeling the group
nhis19$ORIENT_A <- ifelse(nhis19$ORIENT_A == 7| nhis19$ORIENT_A == 8|nhis19$ORIENT_A == 9, NA, nhis19$ORIENT_A)
nhis19$ORIENT_A <- factor(nhis19$ORIENT_A, labels =c("Gay/Lesbian", "Straight","Bisexual","Something else", 
                                                     "Don't know the answer"))

# formatting the dementia variable and labeling the group
# Yes- for has dementia
# No- for does not have dementia
nhis19$DEMENEV_A <- ifelse(nhis19$DEMENEV_A == 7|nhis19$DEMENEV_A == 8|nhis19$DEMENEV_A == 9, NA, nhis19$DEMENEV_A)
nhis19$DEMENEV_A <- factor(nhis19$DEMENEV_A, labels=c("Yes", "No"))


# formatting the anxiety variable and labeling the group
# Yes- for has anxiety
# No- for does not have dementia
nhis19$ANXEV_A <- ifelse(nhis19$ANXEV_A == 7|nhis19$ANXEV_A == 8|nhis19$ANXEV_A == 9, NA, nhis19$ANXEV_A)
nhis19$ANXEV_A <- factor(nhis19$ANXEV_A, labels=c("Yes", "No"))


# formatting the depression variable and labeling the group
# Yes- for has depression
# No- for does not have depression
nhis19$DEPEV_A <- ifelse(nhis19$DEPEV_A == 7|nhis19$DEPEV_A == 8|nhis19$DEPEV_A == 9, NA, nhis19$DEPEV_A)
nhis19$DEPEV_A <- factor(nhis19$DEPEV_A, labels=c("Yes", "No"))

# formatting the cigarette smoking variable
# Yes -smokes cigarette
# No- does not smoke cigarette
nhis19$SMKCIGST_A <- ifelse(nhis19$SMKCIGST_A == 1|nhis19$SMKCIGST_A == 2, 1, ifelse(nhis19$SMKCIGST_A ==5|
                                                                                       nhis19$SMKCIGST_A ==9,
                                                                                     NA, 2))
nhis19$SMKCIGST_A <- factor(nhis19$SMKCIGST_A, labels = c("Yes", "No"))


# formatting the e-cigarette smoking variable
# Yes -smokes e-cigarette
# No- does not smoke e-cigarette
nhis19$SMKECIGST_A <- ifelse(nhis19$SMKECIGST_A == 1, 1, ifelse(nhis19$SMKECIGST_A ==4|
                                                                  nhis19$SMKECIGST_A ==9,
                                                                NA, 2))
nhis19$SMKECIGST_A <- factor(nhis19$SMKECIGST_A, labels = c("Yes", "No"))

# formatting the cigar smoking variable
# Yes -smokes cigar
# No- does not smoke cigar
nhis19$CIGARST_A <- ifelse(nhis19$CIGARCUR_A == 1|nhis19$CIGARCUR_A == 2, 1, ifelse(nhis19$CIGARCUR_A == 3, 2, NA))
nhis19$CIGARST_A[is.na(nhis19$CIGARST_A)] <- 0
nhis19$CIGARST_A <- ifelse(nhis19$CIGARST_A == 1, 1, ifelse(nhis19$CIGARST_A == 2, 2, ifelse(nhis19$CIGAREV_A == 2,
                                                                                             2, NA)))
nhis19$CIGARST_A <- factor(nhis19$CIGARST_A, labels = c("Yes","No"))


# formatting the pipe smoking variable
# Yes - smokes pipe
# No - does not smoke pipe
nhis19$PIPEST_A <- ifelse(nhis19$PIPECUR_A == 1|nhis19$PIPECUR_A == 2, 1, ifelse(nhis19$PIPECUR_A == 3, 2, NA))
nhis19$PIPEST_A[is.na(nhis19$PIPEST_A)] <- 0
nhis19$PIPEST_A <- ifelse(nhis19$PIPEST_A == 1, 1, ifelse(nhis19$PIPEST_A == 2, 2, ifelse(nhis19$PIPEEV_A == 2,
                                                                                          2, NA)))
nhis19$PIPEST_A <- factor(nhis19$PIPEST_A, labels = c("Yes", "No"))


# formatting the smokeless tobacco use variable
# Yes -uses smokeless tobacco
# No- does not use smokeless tobacco
nhis19$SMOKELSST_A <- ifelse(nhis19$SMOKELSCUR_A == 1|nhis19$SMOKELSCUR_A== 2, 1, ifelse(nhis19$SMOKELSCUR_A==3,2,NA))
nhis19$SMOKELSST_A[is.na(nhis19$SMOKELSST_A)] <- 0
nhis19$SMOKELSST_A <- ifelse(nhis19$SMOKELSST_A== 1, 1,ifelse(nhis19$SMOKELSST_A == 2,2,ifelse(nhis19$SMOKELSEV_A ==2,
                                                                                               2, NA)))
nhis19$SMOKELSST_A <- factor(nhis19$SMOKELSST_A, labels = c("Yes", "No"))

# creating a variable for dual use of cigarette and e-cigarette
# Yes- uses cigarette and e-cigarette
# No- does not use both at once
nhis19$cig_ecig <- ifelse(nhis19$SMKCIGST_A == "Yes" & nhis19$SMKECIGST_A == "Yes", "Yes", "No")

# creating a variable for dual use of cigarette and cigar
# Yes- uses cigarette and cigar
# No- does not use both at cigar
nhis19$cig_cigar <- ifelse(nhis19$SMKCIGST_A == "Yes" & nhis19$CIGARST_A == "Yes", "Yes", "No")

# creating a variable for dual use of cigarette and pipe
# Yes- uses cigarette and pipe
# No- does not use both at once
nhis19$cig_pipe <- ifelse(nhis19$SMKCIGST_A == "Yes" & nhis19$PIPEST_A == "Yes", "Yes", "No")

# creating a variable for dual use of cigarette and smokeless tobacco
# Yes- uses cigarette and smokeless tobacco
# No- does not use both at once
nhis19$cig_smkles <- ifelse(nhis19$SMKCIGST_A == "Yes" & nhis19$SMOKELSST_A == "Yes", "Yes", "No")

#writing CSV for cleaned data
write_csv(nhis19, "final_data_2019_clean.csv")
