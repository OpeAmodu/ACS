library(tidyverse)


nhis2019 <- read.csv("../2019_datasets/adult19.csv")


#Extracting variables from adult 2019. These variables are easier to collect since they are not in different folders
final_data_2019 <- nhis2019 %>%
  select(HHX, WTFA_A, PSTRAT, PPSU, URBRRL, REGION, 
         AGEP_A, AGE65, SEX_A, EDUC_A, MARITAL_A, ORIENT_A, RACEALLP_A,
         PCNTFAM_A, PCNTADLT_A, PCNTKIDS_A, OVER65FLG_A, MLTFAMFLG_A, MAXEDUC_A,
         EMPWRKLSWK_A, EMPWRKFT_A, 
         PCNTADTWKP_A, PCNTADTWFP_A, INCGRP_A, RATCAT_A,
         NOTCOV_A, COVER_A, COVER65_A, 
         FDSCAT4_A,
         AFVET_A, AFVETTRN_A, COMBAT_A, VAHOSP_A, VACAREEV_A,
         NATUSBORN_A, YRSINUS_A, CITZNSTP_A, 
         PAYBLL12M_A, PAYNOBLLNW_A, PAYWORRY_A,
         ANXEV_A, DEPEV_A, DEMENEV_A,
         SMKEV_A, SMKNOW_A, SMKCIGST_A, CIGNOW_A, SMK30D_A, CIG30D_A, ECIGEV_A, ECIGNOW_A,
         SMKECIGST_A, CIGAREV_A, CIGARCUR_A, CIGAR30D_A, PIPEEV_A, PIPECUR_A, SMOKELSEV_A, SMOKELSCUR_A)

write_csv(final_data_2019, "final_data_2019.csv")

nrow(final_data_2019)
ncol(final_data_2019)
