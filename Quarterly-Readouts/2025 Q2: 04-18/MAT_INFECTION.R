#*****************************************************************************
#* PRISMA Maternal Infection
#* Drafted: 25 October 2023, Stacie Loisate
#* Last updated: 29 April 2025 

## This code will generate maternal infection outcomes at the following time points: 
# Enrollment
# 1. HIV
# 2. Syphilis
# 3. Gonorrhea
# 4. Chlamydia
# 5. Genital Ulcers
# 6. Malaria 
# 7. Hepatitis (Hep B, Hep C, Hep E)
# 8. TB
# 9. Zika/Dengue/Chikungunya
# 10. Leptospirosis

# Any visit 
# 1. Syphilis 
# 1. HIV
# 2. Syphilis
# 3. Gonorrhea
# 4. Chlamydia
# 5. Genital Ulcers
# 6. Malaria 
# 7. Hepatitis (Hep B, Hep C)
# 8. TB
#*****************************************************************************
## coding updates
## [INFECTION] any pregnancy: if either enrollment or ANC AND have at least one test (question: do we want it to be the same logic for testing)
## Add HEV variables from other dataframe 
#*****************************************************************************
#* Data Setup 
#*****************************************************************************
library(tidyverse)
library(readr)
library(dplyr)
library(readxl)
library(gmodels)
library(kableExtra)
library(lubridate)
library(haven)

# UPDATE EACH RUN # 
# set upload date 
UploadDate = "2025-04-18"

# set path to save 
path_to_save <- "D:/Users/stacie.loisate/Documents/PRISMA-Analysis-Stacie/Maternal-Outcomes/data/"
path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")

# set path to data
path_to_data <- paste0("D:/Users/stacie.loisate/Documents/import/", UploadDate)


mat_enroll <- read_xlsx(paste0(path_to_tnt, "MAT_ENROLL" ,".xlsx" )) %>% select(SITE, MOMID, PREGID, ENROLL, ENROLL_SCRN_DATE) %>% 
  filter(ENROLL == 1)

mat_end <- read_dta(paste0("Z:/Outcome Data/", UploadDate,  "/MAT_ENDPOINTS.dta")) %>%  
  ## only want all pregnancy endpoints excluding moms who have died before delivery and induced abortions
  filter(PREG_END ==1 & MAT_DEATH==0)

# import forms 
mnh02 <- read.csv(paste0(path_to_data,"/", "mnh02_merged.csv"))
mnh04 <- read.csv(paste0(path_to_data,"/", "mnh04_merged.csv"))
mnh06 <- read.csv(paste0(path_to_data,"/", "mnh06_merged.csv"))
mnh07 <- read.csv(paste0(path_to_data,"/", "mnh07_merged.csv"))
mnh08 <- read.csv(paste0(path_to_data,"/", "mnh08_merged.csv"))
mnh19 <- read.csv(paste0(path_to_data,"/", "mnh19_merged.csv"))

#MNH02
if (any(duplicated(mnh02[c("SITE","SCRNID", "MOMID", "PREGID", "M02_SCRN_OBSSTDAT")]))) {
  # extract duplicated ids
  duplicates_ids_02 <- which(duplicated(mnh02[c("SITE","SCRNID",  "MOMID", "PREGID", "M02_SCRN_OBSSTDAT")]) | 
                               duplicated(mnh02[c("SITE","SCRNID",  "MOMID", "PREGID", "M02_SCRN_OBSSTDAT")], fromLast = TRUE))
  duplicates_ids_02 <- mnh02[duplicates_ids_02, ]
  
  print(paste0("n= ",dim(duplicates_ids_02)[1],  " Duplicates in mnh02 exist"))
  
  # extract ids from main dataset
  mnh02 <- mnh02 %>% 
    group_by(SITE,SCRNID, MOMID, PREGID, M02_SCRN_OBSSTDAT) %>%
    arrange(desc(M02_SCRN_OBSSTDAT)) %>% 
    slice(1) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    select(-n) %>% 
    ungroup()
  
} else {
  print("No duplicates in mnh02")
}

#MNH04
if (any(duplicated(mnh04[c("SITE", "MOMID", "PREGID",  "M04_TYPE_VISIT")]))) {
  # extract duplicated ids
  duplicates_ids_04 <- which(duplicated(mnh04[c("SITE", "MOMID", "PREGID", "M04_TYPE_VISIT", "M04_ANC_OBSSTDAT")]) | 
                               duplicated(mnh04[c("SITE", "MOMID", "PREGID",  "M04_TYPE_VISIT", "M04_ANC_OBSSTDAT")], fromLast = TRUE))
  duplicates_ids_04 <- mnh04[duplicates_ids_04, ]
  
  print(paste0("n= ",dim(duplicates_ids_04)[1],  " Duplicates in mnh04 exist"))
  
  # extract ids from main dataset
  mnh04 <- mnh04 %>% group_by(SITE, MOMID, PREGID, M04_TYPE_VISIT) %>% 
    arrange(desc(M04_ANC_OBSSTDAT)) %>% 
    slice(1) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    select(-n) %>% 
    ungroup()
  
} else {
  print("No duplicates in mnh04")
}

#MNH06
if (any(duplicated(mnh06[c("SITE", "MOMID", "PREGID", "M06_TYPE_VISIT")]))) {
  # extract duplicated ids
  duplicates_ids_06 <- which(duplicated(mnh06[c("SITE", "MOMID", "PREGID", "M06_TYPE_VISIT", "M06_DIAG_VSDAT")]) | 
                               duplicated(mnh06[c("SITE", "MOMID", "PREGID",  "M06_TYPE_VISIT", "M06_DIAG_VSDAT")], fromLast = TRUE))
  duplicates_ids_06 <- mnh06[duplicates_ids_06, ]
  
  print(paste0("n= ",dim(duplicates_ids_06)[1],  " Duplicates in mnh06 exist"))
  
  # extract ids from main dataset
  mnh06 <- mnh06 %>% group_by(SITE, MOMID, PREGID, M06_TYPE_VISIT) %>% 
    arrange(desc(M06_DIAG_VSDAT)) %>% 
    slice(1) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    select(-n) %>% 
    ungroup()
  
  
} else {
  print("No duplicates in mnh06")
}

#MNH07
if (any(duplicated(mnh07[c("SITE", "MOMID", "PREGID", "M07_TYPE_VISIT")]))) {
  # extract duplicated ids
  duplicates_ids_07 <- which(duplicated(mnh07[c("SITE", "MOMID", "PREGID", "M07_TYPE_VISIT", "M07_MAT_SPEC_COLLECT_DAT")]) | 
                               duplicated(mnh07[c("SITE", "MOMID", "PREGID",  "M07_TYPE_VISIT", "M07_MAT_SPEC_COLLECT_DAT")], fromLast = TRUE))
  duplicates_ids_07 <- mnh07[duplicates_ids_07, ]
  
  print(paste0("n= ",dim(duplicates_ids_07)[1],  " Duplicates in mnh07 exist"))
  
  # extract ids from main dataset
  mnh07 <- mnh07 %>% group_by(SITE, MOMID, PREGID, M07_TYPE_VISIT) %>% 
    arrange(desc(M07_MAT_SPEC_COLLECT_DAT)) %>% 
    slice(1) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    select(-n) %>% 
    ungroup()
  
  
} else {
  print("No duplicates in mnh07")
}

#MNH08
if (any(duplicated(mnh08[c("SITE", "MOMID", "PREGID", "M08_TYPE_VISIT")]))) {
  # extract duplicated ids
  duplicates_ids_08 <- which(duplicated(mnh08[c("SITE", "MOMID", "PREGID", "M08_TYPE_VISIT", "M08_LBSTDAT")]) | 
                               duplicated(mnh08[c("SITE", "MOMID", "PREGID", "M08_TYPE_VISIT", "M08_LBSTDAT")], fromLast = TRUE))
  duplicates_ids_08 <- mnh08[duplicates_ids_08, ]
  
  print(paste0("n= ",dim(duplicates_ids_08)[1],  " Duplicates in mnh08 exist"))
  
  # extract ids from main dataset
  mnh08 <- mnh08 %>% group_by(SITE, MOMID, PREGID, M08_TYPE_VISIT) %>% 
    arrange(desc(M08_LBSTDAT)) %>% 
    slice(1) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    select(-n) %>% 
    ungroup()
  
} else {
  print("No duplicates in mnh08")
}

#*****************************************************************************
#### STIs ####
#*****************************************************************************

# extract enrolled ids for all datasets
mnh04_all_visits = mnh04 %>% right_join(mat_enroll, by = c("SITE", "MOMID", "PREGID")) %>% 
  # only want enrolled participants
  filter(ENROLL ==1) %>% 
  rename("TYPE_VISIT" = "M04_TYPE_VISIT") %>% 
  filter(TYPE_VISIT %in% c(1,2,3,4,5,7,8,9,10,11,12)) %>% 
  # Is there a form available for this participant? Defined by having any visit status
  mutate(M04_FORM_COMPLETE = ifelse(!is.na(M04_MAT_VISIT_MNH04), 1, 0)) %>% 
  mutate(MALARIA_TESTING = case_when(SITE == "Pakistan" & M04_ANC_OBSSTDAT < "2024-05-01" ~ 1, 
                                     SITE == "India-CMC" & M04_ANC_OBSSTDAT < "2024-06-13" ~ 1,
                                     SITE == "India-SAS" & M04_ANC_OBSSTDAT < "2024-05-31" ~ 1,
                                     SITE %in% c("Zambia", "Kenya", "Ghana") ~ 1, 
                                     TRUE ~ 0 
                                     
  ))


mnh06_all_visits = mnh06 %>% right_join(mat_enroll, by = c("SITE", "MOMID", "PREGID")) %>% 
  # only want enrolled participants
  filter(ENROLL ==1) %>% 
  rename("TYPE_VISIT" = "M06_TYPE_VISIT") %>%
  filter(TYPE_VISIT %in% c(1,2,3,4,5,7,8,9,10,11,12)) %>% 
  # Is there a form available for this participant? Defined by having any visit status
  mutate(M06_FORM_COMPLETE = ifelse(!is.na(M06_MAT_VISIT_MNH06), 1, 0)) %>% 
  mutate(MALARIA_TESTING = case_when(SITE == "Pakistan" & M06_DIAG_VSDAT < "2024-05-01" ~ 1, 
                                     SITE == "India-CMC" & M06_DIAG_VSDAT < "2024-06-13" ~ 1,
                                     SITE == "India-SAS" & M06_DIAG_VSDAT < "2024-05-31" ~ 1,
                                     SITE %in% c("Zambia", "Kenya", "Ghana") ~ 1, 
                                     TRUE ~ 0 
                                     
  ))


mnh07_all_visits = mnh07 %>% right_join(mat_enroll, by = c("SITE", "MOMID", "PREGID")) %>% 
  # only want enrolled participants
  filter(ENROLL ==1) %>% 
  rename("TYPE_VISIT" = "M07_TYPE_VISIT") %>%
  filter(TYPE_VISIT %in% c(1,2,3,4,5,7,8,9,10,11,12)) %>% 
  # Is there a form available for this participant? Defined by having any visit status
  mutate(M07_FORM_COMPLETE = ifelse(!is.na(M07_MAT_VISIT_MNH07), 1, 0)) %>% 
  select(SITE,MOMID, PREGID,TYPE_VISIT,M07_FORM_COMPLETE,  M07_MAT_VAG_SWAB_SPEC_COLLECT_YN)


## i think ghana is using 1, positive, 2, negative, 3, inconclusive (should be 1, positive; 2, negative; 3, inconclusive)
mnh08_all_visits = mnh08 %>% right_join(mat_enroll, by = c("SITE", "MOMID", "PREGID")) %>% 
  # only want enrolled participants
  filter(ENROLL ==1) %>% 
  rename("TYPE_VISIT" = "M08_TYPE_VISIT") %>%
  filter(TYPE_VISIT %in% c(1,2,3,4,5,7,8,9,10,11,12)) %>% 
  select(SITE, MOMID, PREGID,ENROLL_SCRN_DATE, M08_LBSTDAT, M08_MAT_VISIT_MNH08, TYPE_VISIT, M08_TB_CNFRM_LBORRES,
         contains("ZCD"),contains("CT"), contains("NG"), M08_LEPT_LBPERF_1, M08_LEPT_IGM_LBORRES, 
         M08_LEPT_LBPERF_2, M08_LEPT_IGG_LBORRES, M08_HEV_LBPERF_1, M08_HEV_IGM_LBORRES,
         M08_HEV_LBPERF_2, M08_HEV_IGG_LBORRES, M08_LB_EXPANSION, M08_HRP_LBORRES) %>% 
  # Is there a form available for this participant? Defined by having any visit status
  mutate(M08_FORM_COMPLETE = ifelse(!is.na(M08_MAT_VISIT_MNH08), 1, 0)) %>% 
  # generate proxy variable for zcd/lepto/hev test
  mutate(ENROLL_EXPANSION = case_when(M08_ZCD_LBPERF_1==1 | M08_ZCD_LBPERF_2==1 | M08_ZCD_LBPERF_3==1 | 
                                        M08_ZCD_LBPERF_4==1 | M08_ZCD_LBPERF_5==1 |M08_ZCD_LBPERF_6==1 |
                                        M08_LEPT_LBPERF_1==1 | M08_LEPT_LBPERF_2 == 1 | M08_HEV_LBPERF_1 == 1|
                                        M08_HEV_LBPERF_2 == 1  ~ 1, TRUE ~ 0)) %>% 
  mutate(MALARIA_TESTING = case_when(SITE == "Pakistan" & M08_LBSTDAT < "2024-05-01" ~ 1, 
                                     SITE == "India-CMC" & M08_LBSTDAT < "2024-06-13" ~ 1,
                                     SITE == "India-SAS" & M08_LBSTDAT < "2024-05-31" ~ 1,
                                     SITE %in% c("Zambia", "Kenya", "Ghana") ~ 1, 
                                     TRUE ~ 0 
  )) %>% 
  ## GENERATE VARIABLE FOR CT/NG EXPANSION 
  mutate(CTNG_EXPANSION_DATE = case_when(SITE == "Ghana" ~ "2024-04-08", ## CONFIRM 04-09 IS THE RIGHT DATE -- THERE ARE N=2 TESTS DATED 04-08
                                         SITE == "India-CMC" ~ "2024-07-03",
                                         SITE == "India-SAS" ~ "2024-03-11",
                                         SITE == "Kenya" ~ "2024-03-07",
                                         SITE == "Pakistan" ~ "2024-04-25",
                                         SITE == "Zambia" ~ "2023-11-09",
                                         TRUE ~ NA
  )) %>% 
  mutate(CTNG_EXPANSION_DATE = ymd(CTNG_EXPANSION_DATE)
  ) %>% 
  # generate binary variable if visit date is on or past the begin collection date
  mutate(CTNG_EXPANSION = case_when(ENROLL_SCRN_DATE >= CTNG_EXPANSION_DATE ~ 1, TRUE ~ 0)) 

# write.csv(enrolled_ids, paste0(path_to_save, "enrolled_ids" ,".csv"), row.names=FALSE)
## if a participant is missing an enrollment form then they will be EXCLULDED from the following analyses
#*****************************************************************************
#* MATERNAL INFECTION 
# Table 1. STIs 
# Table 2. Malaria at enrollment
# Table 3. Hepatitis at enrollment
# Table 4. TB at enrollment
# Table 5. All infections combined
#*****************************************************************************
#*****************************************************************************
#### STIs ####
## 55, missing
## 77, not applicable 
## 99, don't know 
#*****************************************************************************

mat_infection_sti <- mat_enroll %>% 
  full_join(mnh04_all_visits, by = c("SITE", "MOMID", "PREGID")) %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT,M04_FORM_COMPLETE, M04_SYPH_MHOCCUR,M04_HIV_EVER_MHOCCUR, M04_HIV_MHOCCUR,
         M04_OTHR_STI_MHOCCUR, M04_GONORRHEA_MHOCCUR, M04_CHLAMYDIA_MHOCCUR, M04_GENULCER_MHOCCUR, M04_STI_OTHR_MHOCCUR) %>% 
  # merge in mnh06 to extract rdt results 
  full_join(mnh06_all_visits[c("SITE", "MOMID", "PREGID", "TYPE_VISIT","M06_FORM_COMPLETE",
                               "M06_SYPH_POC_LBORRES", "M06_SYPH_POC_LBPERF", "M06_HIV_POC_LBORRES", "M06_HIV_POC_LBPERF")], 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT")) %>% 
  # was the test performed 
  rename(SYPH_MEAS_PERF = M06_SYPH_POC_LBPERF) %>% 
  rename(HIV_MEAS_PERF = M06_HIV_POC_LBPERF) %>% 
  
  # generate new var defining a positive result
  mutate(
    # is there a valid response (1,0)
    SYPH_DIAG_RESULT = case_when(M04_SYPH_MHOCCUR %in% c(1,0) ~ 1,
                                 TRUE ~ 0),
    HIV_DIAG_RESULT = case_when(M04_HIV_EVER_MHOCCUR %in% c(1,0) | M04_HIV_MHOCCUR %in% c(1,0)~ 1, TRUE ~ 0),
    GON_DIAG_RESULT = case_when(M04_OTHR_STI_MHOCCUR == 1 & M04_GONORRHEA_MHOCCUR %in% c(1,0) |
                                  (M04_OTHR_STI_MHOCCUR == 0) ~ 1, TRUE ~ 0),
    CHL_DIAG_RESULT = case_when(M04_OTHR_STI_MHOCCUR == 1 & M04_CHLAMYDIA_MHOCCUR %in% c(1,0) |
                                  (M04_OTHR_STI_MHOCCUR == 0) ~ 1, TRUE ~ 0),
    GENU_DIAG_RESULT = case_when(M04_OTHR_STI_MHOCCUR == 1 & M04_GENULCER_MHOCCUR %in% c(1,0)|
                                   (M04_OTHR_STI_MHOCCUR == 0) ~ 1, TRUE ~ 0),
    OTHR_DIAG_RESULT = case_when(M04_STI_OTHR_MHOCCUR %in% c(1,0) |
                                   (M04_OTHR_STI_MHOCCUR == 0) ~ 1, TRUE ~ 0),
    SYPH_MEAS_RESULT = case_when(M06_SYPH_POC_LBORRES %in% c(1,0) ~ 1, TRUE ~ 0),
    HIV_MEAS_RESULT = case_when(M06_HIV_POC_LBORRES %in% c(1,0) ~ 1, TRUE ~ 0),
    
    SYPH_POSITIVE = case_when(M04_SYPH_MHOCCUR == 1 | M06_SYPH_POC_LBORRES == 1 ~ 1, # positive 
                              SYPH_MEAS_PERF %in% c(55, 77,0) | SYPH_DIAG_RESULT == 0 ~ 55, ## if test was not performed OR no valid test result, code as 55 
                              SITE == "Kenya" & (M04_SYPH_MHOCCUR == 0 | M06_SYPH_POC_LBORRES == 0) ~ 0, ## Kenya not doing poc for all participants at enrollment; if either poc or previous dx is negative, this indicates no infection
                              SITE != "Kenya" & M06_SYPH_POC_LBORRES == 0 ~ 0, ## all other sites: only negative poc will get you "negative" 
                              M04_SYPH_MHOCCUR %in% c(77, 99, 55, NA) & M06_SYPH_POC_LBORRES %in% c(77, 99, 55, NA) ~ 55, 
                              TRUE ~ NA),
    ## Kenya: M04_SYPH_MHOCCUR == 0 | M06_SYPH_POC_LBORRES == 0 can be negative 0 (baseline and incident) 
    ## other sites: ONLY M06_SYPH_POC_LBORRES == 0 (baseline and incident) 
    HIV_POSITIVE = case_when(M04_HIV_MHOCCUR == 1 | M06_HIV_POC_LBORRES == 1 ~ 1, # positive 
                             SITE != "Kenya" & (HIV_MEAS_PERF %in% c(55, 77, 0) | HIV_DIAG_RESULT == 0) ~ 55, ## if test was not performed OR no valid test result, code as 55 
                             SITE == "Kenya" & (M04_HIV_MHOCCUR == 0 | M06_HIV_POC_LBORRES == 0) ~ 0, ## Kenya not doing poc for all participants at enrollment; if either poc or previous dx is negative, this indicates no infection
                             SITE != "Kenya" & M06_HIV_POC_LBORRES == 0 ~ 0, ## all other sites: only negative poc will get you "negative" 
                             M04_HIV_MHOCCUR %in% c(0, 77, 99, 55, NA) & M06_HIV_POC_LBORRES %in% c(77, 99, 55, NA) ~ 55, 
                             TRUE ~ NA), 
    HIV_POSITIVE_ENROLL = case_when(SITE != "Kenya" & (HIV_MEAS_PERF %in% c(55, 77,0) | HIV_DIAG_RESULT == 0) ~ 55, ## if test was not performed OR no valid test result, code as 55 
                                    TYPE_VISIT ==1 & (M04_HIV_EVER_MHOCCUR==1 | M04_HIV_MHOCCUR==1 | M06_HIV_POC_LBORRES == 1) ~ 1, 
                                    TYPE_VISIT ==1 &  (SITE == "Kenya" & (M04_HIV_EVER_MHOCCUR== 0 | M04_HIV_MHOCCUR == 0 | M06_HIV_POC_LBORRES == 0)) ~ 0, ## Kenya not doing poc for all participants at enrollment; if either poc or previous dx is negative, this indicates no infection
                                    TYPE_VISIT ==1 & (SITE != "Kenya" & M06_HIV_POC_LBORRES == 0) ~ 0, ## all other sites: only negative poc will get you "negative" 
                                    TYPE_VISIT ==1 & (M04_HIV_EVER_MHOCCUR %in% c(0, 77, 99, 55, NA) & M04_HIV_MHOCCUR %in% c(0, 77, 99, 55, NA) & M06_HIV_POC_LBORRES %in% c(77, 99, 55, NA)) ~ 55, 
                                    TYPE_VISIT !=1 | is.na(TYPE_VISIT) ~ 77, 
                                    TRUE ~ NA),
    ## M04_HIV_EVER_MHOCCUR == 1 gives you a positive but  M04_HIV_EVER_MHOCCUR == 0 does not produce a negative 
    ## Kenya: same as above 
    GON_POSITIVE = case_when(M04_GONORRHEA_MHOCCUR ==1 ~ 1,
                             M04_GONORRHEA_MHOCCUR ==0 ~ 0,
                             M04_GONORRHEA_MHOCCUR %in% c(NA, 55, 77, 99) ~ 55,
                             TRUE ~ NA),
    CHL_POSITIVE = case_when(M04_CHLAMYDIA_MHOCCUR ==1 ~ 1, 
                             M04_CHLAMYDIA_MHOCCUR ==0 ~ 0,
                             M04_CHLAMYDIA_MHOCCUR %in% c(NA, 55, 77, 99) ~ 55,
                             TRUE ~ NA),
    GENU_POSITIVE = case_when(M04_GENULCER_MHOCCUR ==1 ~ 1,
                              M04_GENULCER_MHOCCUR ==0 ~ 0,
                              M04_GENULCER_MHOCCUR %in% c(NA, 55, 77, 99) ~ 55,
                              TRUE ~ NA),
    OTHR_POSITIVE = case_when(M04_OTHR_STI_MHOCCUR ==1 ~ 1,
                              M04_OTHR_STI_MHOCCUR ==0 ~ 0,
                              M04_OTHR_STI_MHOCCUR %in% c(NA, 55, 77, 99) ~ 55,
                              TRUE ~ NA),
    
    ## Is the test result missing among those with a completed form? (form completed == yes AND test result = yes or no)
    SYPH_DIAG_MISSING = case_when(SYPH_DIAG_RESULT == 0 & M04_FORM_COMPLETE==1~ 1, TRUE ~ 0),
    HIV_DIAG_MISSING = case_when(HIV_DIAG_RESULT == 0 & M04_FORM_COMPLETE==1~ 1, TRUE ~ 0),
    GON_DIAG_MISSING = case_when(GON_DIAG_RESULT == 0 & M04_OTHR_STI_MHOCCUR==1~ 1, TRUE ~ 0), # STI skip pattern - if M04_OTHR_STI_MHOCCUR=1, then this question should be answered
    CHL_DIAG_MISSING = case_when(CHL_DIAG_RESULT == 0 & M04_OTHR_STI_MHOCCUR==1~ 1, TRUE ~ 0), # STI skip pattern - if M04_OTHR_STI_MHOCCUR=1, then this question should be answered
    GENU_DIAG_MISSING = case_when(GENU_DIAG_RESULT == 0 & M04_OTHR_STI_MHOCCUR==1~ 1, TRUE ~ 0), # STI skip pattern - if M04_OTHR_STI_MHOCCUR=1, then this question should be answered
    OTHR_DIAG_MISSING = case_when(OTHR_DIAG_RESULT == 0 & M04_OTHR_STI_MHOCCUR==1~ 1, TRUE ~ 0), # STI skip pattern - if M04_OTHR_STI_MHOCCUR=1, then this question should be answered
    SYPH_MEAS_MISSING = case_when(SYPH_MEAS_RESULT == 0 & M06_FORM_COMPLETE==1 ~ 1, TRUE ~ 0),
    HIV_MEAS_MISSING = case_when(HIV_MEAS_RESULT == 0 & M06_FORM_COMPLETE==1 ~ 1, TRUE ~ 0),
    
  ) %>%
  ## generate variable for any diagnosed or measured STI at enrollment
  mutate(ANY_DIAG_STI = case_when(M04_SYPH_MHOCCUR==1 | M04_HIV_EVER_MHOCCUR==1 | M04_HIV_MHOCCUR==1 |  M04_GONORRHEA_MHOCCUR==1 |
                                    M04_CHLAMYDIA_MHOCCUR==1 | M04_GENULCER_MHOCCUR==1| 
                                    M04_STI_OTHR_MHOCCUR==1~ 1,TRUE ~ 0),
         ANY_DIAG_STI_NO_HIV = case_when(M04_SYPH_MHOCCUR==1| M04_GONORRHEA_MHOCCUR==1 |
                                           M04_CHLAMYDIA_MHOCCUR==1 | M04_GENULCER_MHOCCUR==1| 
                                           M04_STI_OTHR_MHOCCUR==1~ 1,TRUE ~ 0),
         
         ANY_MEAS_STI = case_when(M06_HIV_POC_LBORRES == 1 | M06_SYPH_POC_LBORRES == 1 ~ 1, TRUE ~ 0),
         ANY_MEAS_STI_NO_HIV = case_when(M06_SYPH_POC_LBORRES == 1 ~ 1, TRUE ~ 0)
         
  ) %>% 
  
  # convert to wide format
  select(SITE, MOMID, PREGID, TYPE_VISIT,SYPH_MEAS_PERF,HIV_MEAS_PERF, M04_FORM_COMPLETE, M06_FORM_COMPLETE, SYPH_POSITIVE, HIV_POSITIVE,HIV_POSITIVE_ENROLL, GON_POSITIVE,CHL_POSITIVE, GENU_POSITIVE, OTHR_POSITIVE,
         contains("_RESULT"), contains("_MISSING"), ANY_DIAG_STI,ANY_MEAS_STI, ANY_DIAG_STI_NO_HIV, ANY_MEAS_STI_NO_HIV) %>%
  pivot_wider(
    names_from = TYPE_VISIT,
    values_from = c(SYPH_MEAS_PERF,HIV_MEAS_PERF,, M04_FORM_COMPLETE, M06_FORM_COMPLETE, SYPH_POSITIVE, HIV_POSITIVE,HIV_POSITIVE_ENROLL, GON_POSITIVE,CHL_POSITIVE, GENU_POSITIVE, OTHR_POSITIVE,
                    SYPH_DIAG_RESULT, HIV_DIAG_RESULT, GON_DIAG_RESULT, CHL_DIAG_RESULT, GENU_DIAG_RESULT, OTHR_DIAG_RESULT, 
                    SYPH_DIAG_MISSING, HIV_DIAG_MISSING, GON_DIAG_MISSING, CHL_DIAG_MISSING, GENU_DIAG_MISSING, OTHR_DIAG_MISSING, 
                    SYPH_MEAS_RESULT, HIV_MEAS_RESULT, SYPH_MEAS_MISSING, HIV_MEAS_MISSING,
                    ANY_DIAG_STI, ANY_MEAS_STI, ANY_DIAG_STI_NO_HIV, ANY_MEAS_STI_NO_HIV),
    names_glue = "{.value}_{TYPE_VISIT}"
  ) %>% 
  # generate new var for any measure infection positive result at any visit (EXCLUDING enrollment) 
  mutate(SYPH_POSITIVE_ANY_ANC = case_when(SYPH_POSITIVE_1 != 1 & (SYPH_POSITIVE_2 ==1 | SYPH_POSITIVE_3 ==1 | SYPH_POSITIVE_4 ==1 | SYPH_POSITIVE_5 ==1) ~ 1,
                                           SYPH_POSITIVE_1 ==1 & (SYPH_POSITIVE_2 ==1 | SYPH_POSITIVE_3 ==1 | SYPH_POSITIVE_4 ==1 | SYPH_POSITIVE_5 ==1) ~ 2, ## persistent syphilis infection
                                           SYPH_POSITIVE_2 ==0 | SYPH_POSITIVE_3 ==0 | SYPH_POSITIVE_4 ==0 | SYPH_POSITIVE_5 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                           SYPH_POSITIVE_2 %in% c(NA, 55) & SYPH_POSITIVE_3 %in% c(NA, 55) & SYPH_POSITIVE_4 %in% c(NA, 55) & SYPH_POSITIVE_5 %in% c(NA, 55) ~ 55,
                                           TRUE ~ NA),
         HIV_POSITIVE_ANY_ANC = case_when(HIV_POSITIVE_ENROLL_1 !=1 & (HIV_POSITIVE_2 ==1 | HIV_POSITIVE_3 ==1 | HIV_POSITIVE_4 ==1 | HIV_POSITIVE_5 ==1) ~ 1, 
                                          HIV_POSITIVE_ENROLL_1 ==1 & (HIV_POSITIVE_2 ==1 | HIV_POSITIVE_3 ==1 | HIV_POSITIVE_4 ==1 | HIV_POSITIVE_5 ==1) ~ 2, ## persistent HIV infection
                                          HIV_POSITIVE_2 ==0 | HIV_POSITIVE_3 ==0 | HIV_POSITIVE_4 ==0 | HIV_POSITIVE_5 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                          HIV_POSITIVE_2 %in% c(NA, 55) & HIV_POSITIVE_3 %in% c(NA, 55) & HIV_POSITIVE_4 %in% c(NA, 55) & HIV_POSITIVE_5 %in% c(NA, 55) ~ 55,
                                          TRUE ~ NA),
         
         GON_POSITIVE_ANY_ANC = case_when(GON_POSITIVE_1 != 1 & (GON_POSITIVE_2 ==1 | GON_POSITIVE_3 ==1 | GON_POSITIVE_4 ==1 | GON_POSITIVE_5 ==1) ~ 1,
                                          GON_POSITIVE_1 == 1 & (GON_POSITIVE_2 ==1 | GON_POSITIVE_3 ==1 | GON_POSITIVE_4 ==1 | GON_POSITIVE_5 ==1) ~ 2, ## persistent gonorrhea infection
                                          GON_POSITIVE_2 ==0 | GON_POSITIVE_3 ==0 | GON_POSITIVE_4 ==0 | GON_POSITIVE_5 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                          GON_POSITIVE_2 %in% c(NA, 55) & GON_POSITIVE_3 %in% c(NA, 55) & GON_POSITIVE_4 %in% c(NA, 55) & GON_POSITIVE_5 %in% c(NA, 55) ~ 55,
                                          TRUE ~ NA),
         
         CHL_POSITIVE_ANY_ANC = case_when(CHL_POSITIVE_1 != 1 & (CHL_POSITIVE_2 ==1 | CHL_POSITIVE_3 ==1 | CHL_POSITIVE_4 ==1 | CHL_POSITIVE_5 ==1) ~ 1,
                                          CHL_POSITIVE_1 == 1 & (CHL_POSITIVE_2 ==1 | CHL_POSITIVE_3 ==1 | CHL_POSITIVE_4 ==1 | CHL_POSITIVE_5 ==1) ~ 2, ## persistent chlamydia infection
                                          CHL_POSITIVE_2 ==0 | CHL_POSITIVE_3 ==0 | CHL_POSITIVE_4 ==0 | CHL_POSITIVE_5 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                          CHL_POSITIVE_2 %in% c(NA, 55) & CHL_POSITIVE_3 %in% c(NA, 55) & CHL_POSITIVE_4 %in% c(NA, 55) & CHL_POSITIVE_5 %in% c(NA, 55) ~ 55,
                                          TRUE ~ NA),
         
         GENU_POSITIVE_ANY_ANC = case_when(GENU_POSITIVE_1 != 1 & (GENU_POSITIVE_2 ==1 | GENU_POSITIVE_3 ==1 | GENU_POSITIVE_4 ==1 | GENU_POSITIVE_5 ==1) ~ 1,
                                           GENU_POSITIVE_1 == 1 & (GENU_POSITIVE_2 ==1 | GENU_POSITIVE_3 ==1 | GENU_POSITIVE_4 ==1 | GENU_POSITIVE_5 ==1) ~ 2, ## persistent genital ulcer infection
                                           GENU_POSITIVE_2 ==0 | GENU_POSITIVE_3 ==0 | GENU_POSITIVE_4 ==0 | GENU_POSITIVE_5 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                           GENU_POSITIVE_2 %in% c(NA, 55) & GENU_POSITIVE_3 %in% c(NA, 55) & GENU_POSITIVE_4 %in% c(NA, 55) & GENU_POSITIVE_5 %in% c(NA, 55) ~ 55,
                                           TRUE ~ NA),
         
         OTHR_POSITIVE_ANY_ANC = case_when(OTHR_POSITIVE_1 != 1 & (OTHR_POSITIVE_2 ==1 | OTHR_POSITIVE_3 ==1 | OTHR_POSITIVE_4 ==1 | OTHR_POSITIVE_5 ==1) ~ 1,
                                           OTHR_POSITIVE_1 == 1 & (OTHR_POSITIVE_2 ==1 | OTHR_POSITIVE_3 ==1 | OTHR_POSITIVE_4 ==1 | OTHR_POSITIVE_5 ==1) ~ 2, ## persistent other infection infection
                                           OTHR_POSITIVE_2 ==0 | OTHR_POSITIVE_3 ==0 | OTHR_POSITIVE_4 ==0 | OTHR_POSITIVE_5 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                           OTHR_POSITIVE_2 %in% c(NA, 55) & OTHR_POSITIVE_3 %in% c(NA, 55) & OTHR_POSITIVE_4 %in% c(NA, 55) & OTHR_POSITIVE_5 %in% c(NA, 55) ~ 55,
                                           
                                           TRUE ~ NA))  %>% 
  
  mutate(SYPH_POSITIVE_ANY_PNC = case_when((SYPH_POSITIVE_1 != 1 | SYPH_POSITIVE_ANY_ANC != 1) & (SYPH_POSITIVE_7 ==1 | SYPH_POSITIVE_8 ==1 | SYPH_POSITIVE_9 ==1 | SYPH_POSITIVE_10 ==1 |
                                                                                                    SYPH_POSITIVE_11 ==1 | SYPH_POSITIVE_12 ==1) ~ 1,
                                           
                                           (SYPH_POSITIVE_1 == 1 | SYPH_POSITIVE_ANY_ANC == 1) & (SYPH_POSITIVE_7 ==1 | SYPH_POSITIVE_8 ==1 | SYPH_POSITIVE_9 ==1 | SYPH_POSITIVE_10 ==1 |
                                                                                                    SYPH_POSITIVE_11 ==1 | SYPH_POSITIVE_12 ==1) ~ 2, ## persistent syphilis infection
                                           
                                           SYPH_POSITIVE_7 ==0 | SYPH_POSITIVE_8 ==0 | SYPH_POSITIVE_9 ==0 | SYPH_POSITIVE_10 ==0 | SYPH_POSITIVE_11 ==0 | SYPH_POSITIVE_12 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                           SYPH_POSITIVE_7 %in% c(NA, 55) & SYPH_POSITIVE_8 %in% c(NA, 55) & SYPH_POSITIVE_9 %in% c(NA, 55) & SYPH_POSITIVE_10 %in% c(NA, 55) & SYPH_POSITIVE_11 %in% c(NA, 55) & SYPH_POSITIVE_12 %in% c(NA, 55) ~ 55, 
                                           TRUE ~ NA),
         ## come back to the rest later
         HIV_POSITIVE_ANY_PNC = case_when((HIV_POSITIVE_1 != 1 | HIV_POSITIVE_ANY_ANC != 1) & HIV_POSITIVE_ENROLL_1 !=1 & (HIV_POSITIVE_7 ==1 | HIV_POSITIVE_8 ==1 | HIV_POSITIVE_9 ==1 | HIV_POSITIVE_10 ==1 |
                                                                                                                             HIV_POSITIVE_11 ==1 | HIV_POSITIVE_12 ==1) ~ 1, TRUE ~ 0),
         
         GON_POSITIVE_ANY_PNC = case_when((GON_POSITIVE_1 != 1 | GON_POSITIVE_ANY_ANC != 1) & (GON_POSITIVE_7 ==1 | GON_POSITIVE_8 ==1 | GON_POSITIVE_9 ==1 | GON_POSITIVE_10 ==1 |
                                                                                                 GON_POSITIVE_11 ==1 | GON_POSITIVE_12 ==1) ~ 1, TRUE ~ 0),
         
         CHL_POSITIVE_ANY_PNC = case_when((CHL_POSITIVE_1 != 1 | CHL_POSITIVE_ANY_ANC != 1) & (CHL_POSITIVE_7 ==1 | CHL_POSITIVE_8 ==1 | CHL_POSITIVE_9 ==1 | CHL_POSITIVE_10 ==1 |
                                                                                                 CHL_POSITIVE_11 ==1 | CHL_POSITIVE_12 ==1) ~ 1, TRUE ~ 0),
         
         GENU_POSITIVE_ANY_PNC = case_when((GENU_POSITIVE_1 != 1 | GENU_POSITIVE_ANY_ANC != 1) & (GENU_POSITIVE_7 ==1 | GENU_POSITIVE_8 ==1 | GENU_POSITIVE_9 ==1 | GENU_POSITIVE_10 ==1 |
                                                                                                    GENU_POSITIVE_11 ==1 | GENU_POSITIVE_12 ==1) ~ 1, TRUE ~ 0),
         
         OTHR_POSITIVE_ANY_PNC = case_when((OTHR_POSITIVE_1 != 1 | OTHR_POSITIVE_ANY_ANC != 1) & (OTHR_POSITIVE_7 ==1 | OTHR_POSITIVE_8 ==1 | OTHR_POSITIVE_9 ==1 | OTHR_POSITIVE_10 ==1 |
                                                                                                    OTHR_POSITIVE_11 ==1 | OTHR_POSITIVE_12 ==1) ~ 1, TRUE ~ 0),
  ) %>% 
  # generate new var for any infection missing diagnosed and rdt at enrollment
  mutate(SYPH_MISSING_ENROLL = case_when(SYPH_DIAG_RESULT_1 ==0 & SYPH_MEAS_RESULT_1 ==0  ~1, TRUE ~ 0),
         HIV_MISSING_ENROLL = case_when(HIV_DIAG_RESULT_1 ==0 & HIV_MEAS_RESULT_1 ==0  ~1, TRUE ~ 0)
  ) %>% 
  # generate enrollment prevalence variables (exclude missing)
  mutate(HIV_POSITIVE_ENROLL = case_when(HIV_POSITIVE_ENROLL_1 == 1 & HIV_MISSING_ENROLL==0 ~ 1,
                                         HIV_POSITIVE_ENROLL_1 == 0 ~ 0 ,
                                         HIV_POSITIVE_ENROLL_1 %in% c(NA, 55, 77, 99)  ~ 55, 
                                         TRUE ~ NA),                  
         
         SYPH_POSITIVE_ENROLL = case_when(SYPH_POSITIVE_1 == 1 & SYPH_MISSING_ENROLL==0 ~ 1, 
                                          SYPH_POSITIVE_1 == 0 ~ 0 ,
                                          SYPH_POSITIVE_1 %in% c(NA, 55, 77, 99)  ~ 55, 
                                          TRUE ~ NA), 
         GON_POSITIVE_ENROLL = case_when(GON_POSITIVE_1 == 1 & GON_DIAG_MISSING_1 == 0 ~ 1,
                                         GON_POSITIVE_1 == 0 ~ 0 ,
                                         GON_POSITIVE_1 %in% c(NA, 55, 77, 99)  ~ 55, 
                                         TRUE ~ NA), 
         CHL_POSITIVE_ENROLL = case_when(CHL_POSITIVE_1 == 1 & CHL_DIAG_MISSING_1 == 0 ~ 1,
                                         CHL_POSITIVE_1 == 0 ~ 0 ,
                                         CHL_POSITIVE_1 %in% c(NA, 55, 77, 99)  ~ 55, 
                                         TRUE ~ NA), 
         GENU_POSITIVE_ENROLL = case_when(GENU_POSITIVE_1 == 1 & GENU_DIAG_MISSING_1 == 0 ~ 1,
                                          GENU_POSITIVE_1 == 0 ~ 0 ,
                                          GENU_POSITIVE_1 %in% c(NA, 55, 77, 99)  ~ 55, 
                                          TRUE ~ NA), 
         OTHR_POSITIVE_ENROLL = case_when(OTHR_POSITIVE_1 == 1 & OTHR_DIAG_MISSING_1 == 0 ~ 1,
                                          OTHR_POSITIVE_1 == 0 ~ 0 ,
                                          OTHR_POSITIVE_1 %in% c(NA, 55, 77, 99)  | OTHR_DIAG_MISSING_1 == 1 ~ 55, 
                                          TRUE ~ NA)
  ) %>% 
  ## rename enrollment indicators (stis only reported for rdt; don't need to include conditions for dx or rdt, just need to rename)
  rename(GON_DIAG_MISSING_ENROLL = GON_DIAG_MISSING_1,
         CHL_DIAG_MISSING_ENROLL = CHL_DIAG_MISSING_1,
         GENU_DIAG_MISSING_ENROLL = GENU_DIAG_MISSING_1,
         OTHR_DIAG_MISSING_ENROLL = OTHR_DIAG_MISSING_1) %>% 
  ## generate variable for any diagnosed or measured STI at enrollment
  mutate(ANY_DIAG_STI_ENROLL = case_when(ANY_DIAG_STI_1 ==1 ~ 1, TRUE ~ 0),
         ANY_MEAS_STI_ENROLL = case_when(ANY_MEAS_STI_1 == 1 ~ 1, TRUE ~ 0)
  ) %>% 
  ## TO REMOVE
  mutate(STI_ANY_METHOD_ENROLL_NO_HIV = case_when(ANY_DIAG_STI_NO_HIV_1 == 1 |  # syphilis, gonorrhea, chlamydia, genital ulcers, other
                                                    ANY_MEAS_STI_NO_HIV_1 == 1 ~ 1, TRUE ~ 0) # syphilis
         
  ) %>% 
  # generate new var for any sti by any measurement
  mutate(STI_ANY_METHOD_ENROLL = case_when(ANY_DIAG_STI_ENROLL == 1 | ANY_MEAS_STI_ENROLL == 1 ~ 1, TRUE ~ 0)
         # STI_ANY_METHOD_DENOM = case_when(M04_FORM_COMPLETE_1 == 1 | M06_FORM_COMPLETE_1 == 1 ~ 1, TRUE ~ 0)
  )  %>% 
  ## generate vars for any test during pregnancy received. If you 
  mutate(SYPH_TEST_PERF_EVER_PREG = case_when(SYPH_MEAS_PERF_1 ==1 | SYPH_MEAS_PERF_2 ==1 | SYPH_MEAS_PERF_3 ==1 |SYPH_MEAS_PERF_4 ==1 | SYPH_MEAS_PERF_5 ==1 | 
                                                SYPH_DIAG_RESULT_1 ==1 | SYPH_DIAG_RESULT_2 ==1 | SYPH_DIAG_RESULT_3 ==1 |SYPH_DIAG_RESULT_4 ==1 | SYPH_DIAG_RESULT_5 ==1 ~ 1, 
                                              TRUE ~ 0
  ),
  HIV_TEST_PERF_EVER_PREG = case_when(HIV_MEAS_PERF_1 ==1 | HIV_MEAS_PERF_2 ==1 | HIV_MEAS_PERF_3 ==1 |HIV_MEAS_PERF_4 ==1 | HIV_MEAS_PERF_5 ==1 | 
                                        HIV_DIAG_RESULT_1 ==1 | HIV_DIAG_RESULT_2 ==1 | HIV_DIAG_RESULT_3 ==1 |HIV_DIAG_RESULT_4 ==1 | HIV_DIAG_RESULT_5 ==1 ~ 1, 
                                      TRUE ~ 0
  ), 
  GON_TEST_PERF_EVER_PREG = case_when(GON_DIAG_RESULT_1 ==1 | GON_DIAG_RESULT_2 ==1 | GON_DIAG_RESULT_3 ==1 |GON_DIAG_RESULT_4 ==1 | GON_DIAG_RESULT_5 ==1 ~ 1, 
                                      TRUE ~ 0
  ), 
  CHL_TEST_PERF_EVER_PREG = case_when(CHL_DIAG_RESULT_1 ==1 | CHL_DIAG_RESULT_2 ==1 | CHL_DIAG_RESULT_3 ==1 |CHL_DIAG_RESULT_4 ==1 | CHL_DIAG_RESULT_5 ==1 ~ 1, 
                                      TRUE ~ 0
  ), 
  GENU_TEST_PERF_EVER_PREG = case_when(GENU_DIAG_RESULT_1 ==1 | GENU_DIAG_RESULT_2 ==1 | GENU_DIAG_RESULT_3 ==1 |GENU_DIAG_RESULT_4 ==1 | GENU_DIAG_RESULT_5 ==1 ~ 1, 
                                       TRUE ~ 0
  ),
  OTHR_TEST_PERF_EVER_PREG = case_when(OTHR_DIAG_RESULT_1 ==1 | OTHR_DIAG_RESULT_2 ==1 | OTHR_DIAG_RESULT_3 ==1 |OTHR_DIAG_RESULT_4 ==1 | OTHR_DIAG_RESULT_5 ==1 ~ 1, 
                                       TRUE ~ 0
  )
  ) %>% 
  # select needed vars
  select(SITE, MOMID, PREGID, ends_with("_ENROLL"), contains("_ANY_ANC"), #  contains("_ANY_PNC"),
         contains("DENOM"), contains("_SYPH_POSITIVE"), SYPH_POSITIVE_ANY_PNC,
         contains("TEST_PERF")
  ) 


## CTNG EXPANSION 
mat_infection_ctng <-mat_enroll %>% 
  full_join(mnh08_all_visits , by = c("SITE", "MOMID", "PREGID")) %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT,M08_FORM_COMPLETE, M08_CTNG_CT_LBORRES,M08_CTNG_LBPERF_1, M08_CTNG_LBPERF_2,
         M08_CTNG_LBTSTDAT, M08_CTNG_NG_LBORRES, CTNG_EXPANSION_DATE, CTNG_EXPANSION) %>% 
  mutate(CTNG_EXPANSION = case_when(is.na(CTNG_EXPANSION) ~ 0, TRUE ~ CTNG_EXPANSION)) %>% 
  # generate new var defining test performance and test results 
  mutate(CT_TEST_PERF = case_when(M08_CTNG_LBPERF_1 ==1 & CTNG_EXPANSION==1 & M08_CTNG_CT_LBORRES %in% c(1,0) ~ 1,
                                  CTNG_EXPANSION==0 ~ 77,
                                  M08_CTNG_LBPERF_1 ==55 & CTNG_EXPANSION==1~ 55,
                                  TRUE ~ NA),
         NG_TEST_PERF = case_when(M08_CTNG_LBPERF_2 ==1 & CTNG_EXPANSION==1  & M08_CTNG_NG_LBORRES %in% c(1,0) ~ 1,
                                  CTNG_EXPANSION==0 ~ 77,
                                  M08_CTNG_LBPERF_1 ==55 & CTNG_EXPANSION==1~ 55,
                                  TRUE ~ NA),
         CTNG_TEST_PERF = case_when(CT_TEST_PERF == 1 | NG_TEST_PERF == 1 ~ 1,
                                    CT_TEST_PERF ==55 & NG_TEST_PERF==1~ 55,
                                    CTNG_EXPANSION==0 ~ 77,
                                    TRUE ~ NA),
         CT_POSITIVE = case_when(M08_CTNG_CT_LBORRES==1  & CTNG_EXPANSION==1~ 1,
                                 M08_CTNG_CT_LBORRES==0  & CTNG_EXPANSION==1~ 0,
                                 M08_CTNG_CT_LBORRES %in% c(55,77,99, NA) & CTNG_EXPANSION==1 ~ 55,
                                 CTNG_EXPANSION==0 ~ 77,
                                 TRUE ~ NA), 
         NG_POSITIVE = case_when(M08_CTNG_NG_LBORRES==1  & CTNG_EXPANSION==1~ 1,
                                 M08_CTNG_NG_LBORRES==0  & CTNG_EXPANSION==1~ 0,
                                 M08_CTNG_NG_LBORRES%in% c(55,77,99, NA)  & CTNG_EXPANSION==1 ~ 55,
                                 CTNG_EXPANSION==0 ~ 77,
                                 TRUE ~ 0)
  ) %>% 
  ## generate variable for any diagnosed or measured STI at enrollment
  mutate(ANY_EXPANSION = case_when(CT_POSITIVE==1 | NG_POSITIVE==1 ~ 1,
                                   CT_POSITIVE==0 & NG_POSITIVE==0 ~ 0,
                                   TRUE ~ 55)
         
  ) %>% 
  
  # convert to wide format
  select(SITE, MOMID, PREGID, TYPE_VISIT,CTNG_EXPANSION, CT_TEST_PERF, NG_TEST_PERF, CTNG_TEST_PERF, CT_POSITIVE,
         NG_POSITIVE, ANY_EXPANSION
  ) %>%
  ## TROUBLESHOOTING
  pivot_wider(
    names_from = TYPE_VISIT,
    values_from = c(CTNG_EXPANSION, CT_TEST_PERF, NG_TEST_PERF,CTNG_TEST_PERF, CT_POSITIVE,
                    NG_POSITIVE, ANY_EXPANSION),
    names_glue = "{.value}_{TYPE_VISIT}"
  ) %>% 
  
  ## Generate variables for a positive result at any other time-point not enrollment
  mutate(CT_POSITIVE_ANY_ANC = case_when(CTNG_EXPANSION_1==0 | CTNG_EXPANSION_2==0 | CTNG_EXPANSION_3==0 | CTNG_EXPANSION_4==0 | CTNG_EXPANSION_5==0 ~ 77, 
                                         (CT_POSITIVE_1 != 1 | is.na(CT_POSITIVE_1)) & (CT_POSITIVE_2 ==1 |CT_POSITIVE_3 ==1 | CT_POSITIVE_4 ==1 | CT_POSITIVE_5==1) ~ 1, ## positive during ANC
                                         CT_POSITIVE_1 == 1 & (CT_POSITIVE_2 ==1 |CT_POSITIVE_3 ==1 | CT_POSITIVE_4 ==1 | CT_POSITIVE_5==1) ~ 2, ## persistent positive
                                         CT_POSITIVE_2 ==0 | CT_POSITIVE_3 ==0 | CT_POSITIVE_4 ==0 | CT_POSITIVE_5 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                         CT_POSITIVE_2 %in% c(NA, 55) & CT_POSITIVE_3 %in% c(NA, 55) & CT_POSITIVE_4 %in% c(NA, 55) & CT_POSITIVE_5 %in% c(NA, 55) ~ 55,
                                         TRUE ~ NA),
         
         NG_POSITIVE_ANY_ANC = case_when(CTNG_EXPANSION_1==0 | CTNG_EXPANSION_2==0 | CTNG_EXPANSION_3==0 | CTNG_EXPANSION_4==0 | CTNG_EXPANSION_5==0 ~ 77, 
                                         (NG_POSITIVE_1 != 1 | is.na(NG_POSITIVE_1)) & (NG_POSITIVE_2 ==1 |NG_POSITIVE_3 ==1 | NG_POSITIVE_4 ==1 | NG_POSITIVE_5==1) ~ 1, ## positive during ANC
                                         NG_POSITIVE_1 == 1 & (NG_POSITIVE_2 ==1 |NG_POSITIVE_3 ==1 | NG_POSITIVE_4 ==1 | NG_POSITIVE_5==1) ~ 2, ## persistent positive
                                         NG_POSITIVE_2 ==0 | NG_POSITIVE_3 ==0 | NG_POSITIVE_4 ==0 | NG_POSITIVE_5 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                         NG_POSITIVE_2 %in% c(NA, 55) & NG_POSITIVE_3 %in% c(NA, 55) & NG_POSITIVE_4 %in% c(NA, 55) & NG_POSITIVE_5 %in% c(NA, 55) ~ 55,
                                         TRUE ~ NA),
         
         CTNG_TEST_PERF_ANY_ANC = case_when(CTNG_TEST_PERF_2 ==1 | CTNG_TEST_PERF_3 ==1 | CTNG_TEST_PERF_4==1 | CTNG_TEST_PERF_5 ==1 ~ 1,
                                            CTNG_TEST_PERF_2 %in% c(NA, 55) & CTNG_TEST_PERF_3 %in% c(NA, 55) & CTNG_TEST_PERF_4 %in% c(NA, 55) & CTNG_TEST_PERF_5 %in% c(NA, 55) ~ 55,
                                            CTNG_EXPANSION_1==0 ~ 77, ## if not expansion, then 77, not applicable
                                            TRUE ~ NA)
  ) %>% 
  ## generate variable names for positive test results 
  mutate(CTNG_TEST_PERF_ENROLL = CTNG_TEST_PERF_1,
         CTNG_TEST_PERF_ANC32 = case_when(CTNG_TEST_PERF_4==1 | CTNG_TEST_PERF_5==1 ~ 1,
                                          CTNG_EXPANSION_1==0 ~ 77, 
                                          CTNG_TEST_PERF_4==77 | CTNG_TEST_PERF_5==77 ~ 77,
                                          TRUE ~ 0),
         
         CT_POSITIVE_ENROLL = case_when(CT_POSITIVE_1 == 1 ~ 1, 
                                        CT_POSITIVE_1 == 0 ~ 0, 
                                        CT_POSITIVE_1 == 55 | is.na(CT_POSITIVE_1) ~ 55, 
                                        CTNG_EXPANSION_1==0 ~ 77, 
                                        TRUE ~ NA),
         NG_POSITIVE_ENROLL = case_when(NG_POSITIVE_1 == 1 ~ 1, 
                                        NG_POSITIVE_1 == 0 ~ 0, 
                                        NG_POSITIVE_1 == 55 | is.na(NG_POSITIVE_1) ~ 55, 
                                        CTNG_EXPANSION_1==0 ~ 77, 
                                        TRUE ~ NA)
  ) %>% 
  ## generate vars for any test during pregnancy received. If you 
  mutate(CTNG_TEST_PERF_EVER_PREG = case_when(CTNG_TEST_PERF_ENROLL ==1 | CTNG_TEST_PERF_ANY_ANC ==1 ~ 1, 
                                              TRUE ~ 0
  )
  ) %>% 
  rename(ANY_EXPANSION_ENROLL = ANY_EXPANSION_1) %>% 
  select(SITE, MOMID, PREGID,CTNG_EXPANSION_1, CTNG_TEST_PERF_ENROLL, CTNG_TEST_PERF_ANC32,CT_POSITIVE_ENROLL, NG_POSITIVE_ENROLL,
         CT_POSITIVE_ANY_ANC, NG_POSITIVE_ANY_ANC, CTNG_TEST_PERF_ANY_ANC,  CTNG_TEST_PERF_EVER_PREG, ANY_EXPANSION_ENROLL) #%>% 
# mutate(across(everything(), ~ replace(., is.na(.), as.numeric(77))))
table(mat_infection_ctng$CT_POSITIVE_ENROLL, mat_infection_ctng$ANY_EXPANSION_ENROLL)
table(mat_infection_ctng$NG_POSITIVE_ENROLL, mat_infection_ctng$ANY_EXPANSION_ENROLL)

table( mat_infection_ctng$ANY_EXPANSION_ENROLL)

subset <- mat_infection_ctng %>% filter(CTNG_EXPANSION_1!=0) %>%  select(SITE, MOMID, PREGID,CTNG_EXPANSION_1, contains("CT_POSITIVE")) 
table(mat_infection_ctng$CT_POSITIVE_ENROLL, mat_infection_ctng$SITE, useNA = "ifany")
table(mat_infection_ctng$CT_POSITIVE_ANY_ANC, mat_infection_ctng$SITE, useNA = "ifany")

table(mat_infection_ctng$NG_POSITIVE_ENROLL, mat_infection_ctng$SITE, useNA = "ifany")
table(mat_infection_ctng$NG_POSITIVE_ANY_ANC, mat_infection_ctng$SITE, useNA = "ifany")

## CHECK FOR PNC INFECTIONS
# table(mat_infection_sti$SYPH_POSITIVE_ANY_PNC, mat_infection_sti$SITE)
# table(mat_infection_sti$HIV_POSITIVE_ANY_PNC, mat_infection_sti$SITE)
# table(mat_infection_sti$GON_POSITIVE_ANY_PNC, mat_infection_sti$SITE)
# table(mat_infection_sti$CHL_POSITIVE_ANY_PNC, mat_infection_sti$SITE)
# table(mat_infection_sti$GENU_POSITIVE_ANY_PNC, mat_infection_sti$SITE)
# table(mat_infection_sti$OTHR_POSITIVE_ANY_PNC, mat_infection_sti$SITE)


#*****************************************************************************
#### Malaria at enrollment ####
#### Hepatitis at enrollment ####
#### TB at enrollment ####
#*****************************************************************************
mat_other_infection <- mat_enroll %>% 
  full_join(mnh04_all_visits, by = c("SITE", "MOMID", "PREGID")) %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, M04_FORM_COMPLETE, M04_FORM_COMPLETE, MALARIA_TESTING,
         M04_MALARIA_EVER_MHOCCUR,M04_COVID_LBORRES, contains("M04_TB_CETERM"), M04_TB_MHOCCUR) %>% 
  # merge in mnh06 to extract rdt results 
  full_join(mnh06_all_visits[c("SITE", "MOMID", "PREGID","M06_FORM_COMPLETE", "TYPE_VISIT", "M06_MALARIA_POC_LBORRES", "M06_HBV_POC_LBORRES",
                               "M06_HCV_POC_LBORRES", "M06_COVID_POC_LBORRES", "MALARIA_TESTING", "M06_HBV_POC_LBPERF", "M06_HCV_POC_LBPERF", "M06_MALARIA_POC_LBPERF")], 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "MALARIA_TESTING")) %>% 
  
  # merge in mnh08 to extract tb results 
  full_join(mnh08_all_visits[c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "M08_TB_CNFRM_LBORRES",
                               "M08_HRP_LBORRES", "MALARIA_TESTING")], 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "MALARIA_TESTING")) %>% 
  distinct(SITE, MOMID, PREGID, TYPE_VISIT, .keep_all = TRUE) %>%
  ## test performed
  rename(HBV_MEAS_PERF = M06_HBV_POC_LBPERF,
         HCV_MEAS_PERF = M06_HCV_POC_LBPERF,
         MAL_MEAS_PERF = M06_MALARIA_POC_LBPERF,
  ) %>% 
  ## Is the test result missing among those with a completed form?  
  # diagnosed
  mutate(MAL_DIAG_RESULT = case_when(MALARIA_TESTING == 1 & M04_MALARIA_EVER_MHOCCUR %in% c(1,0)~ 1, TRUE ~0),
         TB_DIAG_RESULT = case_when(M04_TB_MHOCCUR %in% c(1,0)~ 1, TRUE ~0),
         
         # measured
         MAL_MEAS_RESULT = case_when(MALARIA_TESTING == 1 & M06_MALARIA_POC_LBORRES %in% c(1,0) & MAL_MEAS_PERF == 1 ~ 1, TRUE ~0),
         HBV_MEAS_RESULT = case_when(M06_HBV_POC_LBORRES %in% c(1,0) & HBV_MEAS_PERF == 1 ~ 1, TRUE ~0),
         HCV_MEAS_RESULT = case_when(M06_HCV_POC_LBORRES %in% c(1,0) & HCV_MEAS_PERF == 1 ~ 1, TRUE ~0)
         
  ) %>% 
  # generate new var for any infection missing diagnosed and rdt
  mutate(MAL_DIAG_MISSING = case_when(MALARIA_TESTING==1 & MAL_DIAG_RESULT == 0 & M04_FORM_COMPLETE==1~ 1, TRUE ~0),
         TB_DIAG_MISSING = case_when(TB_DIAG_RESULT == 0 & M04_FORM_COMPLETE==1~ 1, TRUE ~0),
         
         # measured
         MAL_MEAS_MISSING = case_when(MALARIA_TESTING==1 & MAL_MEAS_RESULT == 0 & M06_FORM_COMPLETE==1~ 1, TRUE ~0),
         HBV_MEAS_MISSING = case_when(HBV_MEAS_RESULT == 0 & M06_FORM_COMPLETE==1~ 1, TRUE ~0),
         HCV_MEAS_MISSING = case_when(HCV_MEAS_RESULT == 0 & M06_FORM_COMPLETE==1~ 1, TRUE ~0),
         
  ) %>% 
  # generate new var for any infection missing diagnosed and rdt
  mutate(MAL_MISSING = case_when(MALARIA_TESTING==1 & (MAL_DIAG_MISSING==1 & MAL_MEAS_MISSING == 1) ~ 1, TRUE ~ 0)
  ) %>% 
  select(-MAL_DIAG_MISSING, -MAL_MEAS_MISSING) %>% 
  # generate new var defining a positive result
  mutate(
    MAL_POSITIVE = case_when(MALARIA_TESTING== 0 ~ 77,
                             MALARIA_TESTING==1 & (M04_MALARIA_EVER_MHOCCUR == 1 | M06_MALARIA_POC_LBORRES == 1) ~ 1, 
                             (M06_MALARIA_POC_LBORRES == 0) ~ 0, 
                             MAL_MEAS_RESULT %in% c(0, 55, 77) ~ 55, ## if test was not performed with a valid test result, code as missing
                             M04_MALARIA_EVER_MHOCCUR %in% c(0,55,77,99,NA) & M06_MALARIA_POC_LBORRES %in% c(0,55,77,99,NA) ~ 55,
                             TRUE ~ NA),
    HBV_POSITIVE = case_when(HBV_MEAS_RESULT %in% c(0, 55, 77) ~ 55, ## if test was not performed with a valid test result, code as missing
                             M06_HBV_POC_LBORRES == 1 ~ 1,
                             M06_HBV_POC_LBORRES == 0 ~ 0, 
                             M06_HBV_POC_LBORRES %in% c(55,77,99,NA) ~ 55,
                             TRUE ~ NA),
    HCV_POSITIVE = case_when(HCV_MEAS_RESULT %in% c(0, 55, 77) ~ 55, ## if test was not performed with a valid test result, code as missing
                             M06_HCV_POC_LBORRES == 1 ~ 1,
                             M06_HCV_POC_LBORRES == 0 ~ 0, 
                             M06_HCV_POC_LBORRES %in% c(55,77,99,NA) ~ 55,
                             TRUE ~ NA),
    ## Malaria positive by HRP OR POC 
    MAL_POSITIVE_HRP = case_when(M08_HRP_LBORRES > 0.0004 | (MALARIA_TESTING==1 & M06_MALARIA_POC_LBORRES == 1) ~ 1, TRUE ~0)
    
  ) %>%
  
  # TB at any visit: total with at least 1 symptom in W4SS in MNH04 (1=At least 1 symptom reported, 0=No symptoms)
  mutate(W4SS_SYMPTOMS_ANY = case_when(M04_TB_CETERM_1==1 | M04_TB_CETERM_2==1 | M04_TB_CETERM_3==1| M04_TB_CETERM_4==1 ~ 1,
                                       M04_TB_CETERM_1==0 & M04_TB_CETERM_2==0 & M04_TB_CETERM_3==0 & M04_TB_CETERM_4==0 ~ 0,
                                       M04_TB_CETERM_1%in% c(55,77,99,NA) & M04_TB_CETERM_2%in% c(55,77,99,NA) & M04_TB_CETERM_3%in% c(55,77,99,NA) & M04_TB_CETERM_4%in% c(55,77,99,NA) ~ 55,
                                       TRUE ~ NA),
         W4SS_RESPONSE = case_when(M04_TB_CETERM_1 %in% c(1,0) | M04_TB_CETERM_2 %in% c(1,0) |
                                     M04_TB_CETERM_3 %in% c(1,0) | M04_TB_CETERM_4 %in% c(1,0) |
                                     M04_TB_CETERM_77 %in% c(1,0) ~  1, TRUE ~ 0),
         # total number missing ALL symptoms -- right now use this 
         W4SS_MISSING_SYMP = case_when(M04_TB_CETERM_1 %in% c(55,77) & M04_TB_CETERM_2 %in% c(55,77) &
                                         M04_TB_CETERM_3 %in% c(55,77) & M04_TB_CETERM_4 %in% c(55,77) &
                                         M04_TB_CETERM_77 %in% c(55,77) ~ 1, TRUE ~ 0),
         
         TB_SYMP_POSITIVE = case_when(W4SS_SYMPTOMS_ANY == 1 ~ 1,
                                      W4SS_SYMPTOMS_ANY == 0 ~ 0,
                                      W4SS_SYMPTOMS_ANY == 55 ~ 55,
                                      TRUE ~ NA),
         TB_SPUTUM_POSITIVE = case_when(M08_TB_CNFRM_LBORRES == 1 ~ 1,
                                        M08_TB_CNFRM_LBORRES == 0 ~ 0,
                                        M08_TB_CNFRM_LBORRES %in% c(55,77) ~ 55,
                                        TRUE ~ NA),
         TB_LAB_RESULT = case_when(M08_TB_CNFRM_LBORRES %in% c(1,2,0) ~ 1, TRUE ~ 0)
         
  ) %>%
  
  ## generate summary any infection variables (diagnosed, measured, lab)
  mutate(OTHER_INFECTION_DIAG_ANY = case_when(M04_MALARIA_EVER_MHOCCUR==1 | M04_TB_MHOCCUR==1 ~ 1, TRUE ~ 0),
         OTHER_INFECTION_MEAS_ANY = case_when(M06_MALARIA_POC_LBORRES==1 | M06_HBV_POC_LBORRES==1 |
                                                M06_HCV_POC_LBORRES==1 ~ 1, TRUE ~ 0),
         OTHER_INFECTION_LAB_ANY = case_when(M08_TB_CNFRM_LBORRES==1 ~ 1, TRUE ~ 0)) %>% 
  
  # convert to wide format
  select(SITE, MOMID, PREGID, TYPE_VISIT, MALARIA_TESTING, MAL_POSITIVE_HRP,  MAL_POSITIVE, HBV_POSITIVE, HCV_POSITIVE,W4SS_RESPONSE, W4SS_MISSING_SYMP,
         TB_LAB_RESULT, TB_SYMP_POSITIVE, TB_SPUTUM_POSITIVE,W4SS_MISSING_SYMP, contains("MISSING"), contains("_RESULT"),contains("MEAS"), contains("DIAG"),
         OTHER_INFECTION_DIAG_ANY, OTHER_INFECTION_MEAS_ANY, OTHER_INFECTION_LAB_ANY) %>%
  
  pivot_wider(
    names_from = TYPE_VISIT,
    values_from = c(MAL_POSITIVE,MAL_POSITIVE_HRP,  HBV_POSITIVE, HCV_POSITIVE, MALARIA_TESTING,
                    MAL_DIAG_RESULT, TB_DIAG_RESULT, MAL_MEAS_RESULT, HBV_MEAS_RESULT, HCV_MEAS_RESULT,
                    TB_DIAG_MISSING,MAL_MISSING,HBV_MEAS_MISSING,HCV_MEAS_MISSING,
                    contains("_RESULT"),contains("MEAS"), contains("DIAG"),
                    TB_LAB_RESULT, TB_SYMP_POSITIVE, TB_SPUTUM_POSITIVE, W4SS_MISSING_SYMP, W4SS_RESPONSE,
                    OTHER_INFECTION_DIAG_ANY, OTHER_INFECTION_MEAS_ANY, OTHER_INFECTION_LAB_ANY),
    names_glue = "{.value}_{TYPE_VISIT}"
  ) %>%
  
  # generate new var for any syphilis positive result at any anc (EXCLUDING enrollment)
  mutate(MAL_POSITIVE_ANY_ANC = case_when(MALARIA_TESTING_1==0 ~ 77,
                                          MALARIA_TESTING_1==1 & MAL_POSITIVE_1 != 1 & (MAL_POSITIVE_2 ==1 | MAL_POSITIVE_3 ==1 | MAL_POSITIVE_4 ==1 | MAL_POSITIVE_5 ==1) ~ 1, 
                                          MALARIA_TESTING_1==1 & MAL_POSITIVE_1 == 1 & (MAL_POSITIVE_2 ==1 | MAL_POSITIVE_3 ==1 | MAL_POSITIVE_4 ==1 | MAL_POSITIVE_5 ==1) ~ 2, ## persistent infection 
                                          MAL_POSITIVE_2 ==0 | MAL_POSITIVE_3 ==0 | MAL_POSITIVE_4 ==0 | MAL_POSITIVE_5 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                          MAL_POSITIVE_2 %in% c(NA,55,77,99) & MAL_POSITIVE_3 %in% c(NA,55,77,99) & MAL_POSITIVE_4 %in% c(NA,55,77,99) & MAL_POSITIVE_5 %in% c(NA,55,77,99) ~ 55,
                                          TRUE ~ NA),
         
         HBV_POSITIVE_ANY_ANC = case_when(HBV_POSITIVE_1 != 1 & (HBV_POSITIVE_2 ==1 | HBV_POSITIVE_3 ==1 | HBV_POSITIVE_4 ==1 | HBV_POSITIVE_5 ==1) ~ 1,
                                          HBV_POSITIVE_1 == 1 & (HBV_POSITIVE_2 ==1 | HBV_POSITIVE_3 ==1 | HBV_POSITIVE_4 ==1 | HBV_POSITIVE_5 ==1) ~ 2, ## persistent infection 
                                          HBV_POSITIVE_2 ==0 | HBV_POSITIVE_3 ==0 | HBV_POSITIVE_4 ==0 | HBV_POSITIVE_5 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                          HBV_POSITIVE_2 %in% c(NA,55,77,99) & HBV_POSITIVE_3 %in% c(NA,55,77,99) & HBV_POSITIVE_4 %in% c(NA,55,77,99) & HBV_POSITIVE_5 %in% c(NA,55,77,99) ~ 55,
                                          TRUE ~ NA),
         
         HCV_POSITIVE_ANY_ANC = case_when(HCV_POSITIVE_1 != 1 & (HCV_POSITIVE_2 ==1 | HCV_POSITIVE_3 ==1 | HCV_POSITIVE_4 ==1 | HCV_POSITIVE_5 ==1) ~ 1,
                                          HCV_POSITIVE_1 == 1 & (HCV_POSITIVE_2 ==1 | HCV_POSITIVE_3 ==1 | HCV_POSITIVE_4 ==1 | HCV_POSITIVE_5 ==1) ~ 2, ## persistent infection 
                                          HCV_POSITIVE_2 ==0 | HCV_POSITIVE_3 ==0 | HCV_POSITIVE_4 ==0 | HCV_POSITIVE_5 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                          HCV_POSITIVE_2 %in% c(NA,55,77,99) & HCV_POSITIVE_3 %in% c(NA,55,77,99) & HCV_POSITIVE_4 %in% c(NA,55,77,99) & HCV_POSITIVE_5 %in% c(NA,55,77,99) ~ 55,
                                          TRUE ~ NA),
         
         TB_SYMP_POSITIVE_ANY_ANC = case_when(TB_SYMP_POSITIVE_1 != 1 & (TB_SYMP_POSITIVE_2 ==1 | TB_SYMP_POSITIVE_3 ==1 | TB_SYMP_POSITIVE_4 ==1 | TB_SYMP_POSITIVE_5 ==1) ~ 1,
                                              TB_SYMP_POSITIVE_1 == 1 & (TB_SYMP_POSITIVE_2 ==1 | TB_SYMP_POSITIVE_3 ==1 | TB_SYMP_POSITIVE_4 ==1 | TB_SYMP_POSITIVE_5 ==1) ~ 2, ## persistent infection 
                                              TB_SYMP_POSITIVE_2 ==0 | TB_SYMP_POSITIVE_3 ==0 | TB_SYMP_POSITIVE_4 ==0 | TB_SYMP_POSITIVE_5 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                              TB_SYMP_POSITIVE_2 %in% c(NA,55,77,99) & TB_SYMP_POSITIVE_3 %in% c(NA,55,77,99) & TB_SYMP_POSITIVE_4 %in% c(NA,55,77,99) & TB_SYMP_POSITIVE_5 %in% c(NA,55,77,99) ~ 55,
                                              TRUE ~ NA),
         
         TB_SPUTUM_POSITIVE_ANY_ANC = case_when(TB_SPUTUM_POSITIVE_1 != 1 & (TB_SPUTUM_POSITIVE_2 ==1 | TB_SPUTUM_POSITIVE_3 ==1 | TB_SPUTUM_POSITIVE_4 ==1 | TB_SPUTUM_POSITIVE_5 ==1) ~ 1, 
                                                TB_SPUTUM_POSITIVE_1 == 1 & (TB_SPUTUM_POSITIVE_2 ==1 | TB_SPUTUM_POSITIVE_3 ==1 | TB_SPUTUM_POSITIVE_4 ==1 | TB_SPUTUM_POSITIVE_5 ==1) ~ 2, ## persistent infection 
                                                TB_SPUTUM_POSITIVE_2 ==0 | TB_SPUTUM_POSITIVE_3 ==0 | TB_SPUTUM_POSITIVE_4 ==0 | TB_SPUTUM_POSITIVE_5 ==0 ~ 0, ## if at least one anc visit is negative, negative during anc is negative
                                                TB_SPUTUM_POSITIVE_2 %in% c(NA,55,77,99) & TB_SPUTUM_POSITIVE_3 %in% c(NA,55,77,99) & TB_SPUTUM_POSITIVE_4 %in% c(NA,55,77,99) & TB_SPUTUM_POSITIVE_5 %in% c(NA,55,77,99) ~ 55,
                                                TRUE ~ NA),
  ) %>% 
  # generate new var for any syphilis positive result at any pnc (EXCLUDING enrollment & anc)
  mutate(MAL_POSITIVE_ANY_PNC = case_when((MAL_POSITIVE_1 != 1 | MAL_POSITIVE_ANY_ANC != 1) & (MAL_POSITIVE_7 ==1 | MAL_POSITIVE_8 ==1 | MAL_POSITIVE_9 ==1 | MAL_POSITIVE_10 ==1 |
                                                                                                 MAL_POSITIVE_11 ==1 | MAL_POSITIVE_12 ==1) ~ 1, TRUE ~ 0),
         HBV_POSITIVE_ANY_PNC = case_when((HBV_POSITIVE_1 != 1 | HBV_POSITIVE_ANY_ANC != 1) & (HBV_POSITIVE_7 ==1 | HBV_POSITIVE_8 ==1 | HBV_POSITIVE_9 ==1 | HBV_POSITIVE_10 ==1 |
                                                                                                 HBV_POSITIVE_11 ==1 | HBV_POSITIVE_12 ==1) ~ 1, TRUE ~ 0),
         HCV_POSITIVE_ANY_PNC = case_when((HCV_POSITIVE_1 != 1 | HCV_POSITIVE_ANY_ANC != 1) & (HCV_POSITIVE_7 ==1 | HCV_POSITIVE_8 ==1 | HCV_POSITIVE_9 ==1 | HCV_POSITIVE_10 ==1 |
                                                                                                 HCV_POSITIVE_11 ==1 | HCV_POSITIVE_12 ==1) ~ 1, TRUE ~ 0),
         TB_SYMP_POSITIVE_ANY_PNC = case_when((TB_SYMP_POSITIVE_1 != 1 | TB_SYMP_POSITIVE_ANY_ANC != 1) & (TB_SYMP_POSITIVE_7 ==1 | TB_SYMP_POSITIVE_8 ==1 | TB_SYMP_POSITIVE_9 ==1 | TB_SYMP_POSITIVE_10 ==1 |
                                                                                                             TB_SYMP_POSITIVE_11 ==1 | TB_SYMP_POSITIVE_12 ==1) ~ 1, TRUE ~ 0),
         TB_SPUTUM_POSITIVE_ANY_PNC = case_when((TB_SPUTUM_POSITIVE_1 != 1 | TB_SPUTUM_POSITIVE_ANY_ANC != 1) & (TB_SPUTUM_POSITIVE_7 ==1 | TB_SPUTUM_POSITIVE_8 ==1 | TB_SPUTUM_POSITIVE_9 ==1 | TB_SPUTUM_POSITIVE_10 ==1 |
                                                                                                                   TB_SPUTUM_POSITIVE_11 ==1 | TB_SPUTUM_POSITIVE_12 ==1) ~ 1, TRUE ~ 0)
  ) %>% 
  
  # generate missing variables 
  mutate(MAL_MISSING_ENROLL = case_when(MALARIA_TESTING_1 == 1 & MAL_MISSING_1==1 ~ 1, TRUE ~ 0),
         HBV_MEAS_MISSING_ENROLL = case_when(HBV_MEAS_MISSING_1==1 ~ 1, TRUE ~ 0),
         HCV_MEAS_MISSING_ENROLL = case_when(HCV_MEAS_MISSING_1==1 ~ 1, TRUE ~ 0),
         W4SS_MISSING_SYMP_ENROLL = case_when(W4SS_MISSING_SYMP_1==1 ~ 1, TRUE ~ 0)
  ) %>% 
  
  # generate enrollment prevalence variables (exclude missing)
  mutate(MAL_POSITIVE_ENROLL = case_when(MALARIA_TESTING_1==1 & MAL_POSITIVE_1 == 1 & MAL_MISSING_ENROLL==0 ~ 1,
                                         MAL_POSITIVE_1 == 0 ~ 0,
                                         MAL_POSITIVE_1 == 77  ~ 77, # | SITE %in% c("Pakistan", "India-CMC", "India-SAS")
                                         MAL_POSITIVE_1 %in% c(55, NA) ~ 55, 
                                         TRUE ~ NA),
         
         MAL_POSITIVE_ENROLL_HRP_POC = case_when(MAL_POSITIVE_HRP_1 == 1~ 1, TRUE ~ 0),
         HBV_POSITIVE_ENROLL = case_when(HBV_POSITIVE_1 == 1 & HBV_MEAS_MISSING_ENROLL==0 ~ 1,
                                         HBV_POSITIVE_1 == 0 ~ 0,
                                         HBV_POSITIVE_1 %in% c(55,77, NA) ~ 55, 
                                         TRUE ~ NA),
         
         HCV_POSITIVE_ENROLL = case_when(HCV_POSITIVE_1 == 1 & HCV_MEAS_MISSING_ENROLL == 0 ~ 1,
                                         HCV_POSITIVE_1 == 0 ~ 0,
                                         HCV_POSITIVE_1 %in% c(55,77, NA) ~ 55, 
                                         TRUE ~ NA),
         
         TB_SYMP_POSITIVE_ENROLL = case_when(TB_SYMP_POSITIVE_1 == 1 ~ 1,
                                             TB_SYMP_POSITIVE_1 == 0 ~ 0,
                                             TB_SYMP_POSITIVE_1 %in% c(55,77, NA) ~ 55, 
                                             TRUE ~ NA),
         
         TB_SPUTUM_POSITIVE_ENROLL = case_when(TB_SPUTUM_POSITIVE_1 == 1 ~ 1,
                                               TB_SPUTUM_POSITIVE_1 == 0 ~ 0,
                                               TB_SPUTUM_POSITIVE_1 %in% c(55,77, NA) ~ 55, 
                                               TRUE ~ NA),
         
         W4SS_RESPONSE_ENROLL = case_when(W4SS_RESPONSE_1 == 1 ~ 1,
                                          W4SS_RESPONSE_1 == 0 ~ 0,
                                          W4SS_RESPONSE_1 %in% c(55,77, NA) ~ 55, 
                                          TRUE ~ NA),
         TB_LAB_RESULT_ENROLL = case_when(TB_LAB_RESULT_1 == 1 ~ 1,
                                          TB_LAB_RESULT_1 == 0 ~ 0,
                                          TB_LAB_RESULT_1 %in% c(55,77, NA) ~ 55, 
                                          TRUE ~ NA),
  ) %>% 
  # generate summary variables at enrollment 
  mutate(OTHER_INFECTION_DIAG_ANY_ENROLL = case_when(OTHER_INFECTION_DIAG_ANY_1 ==1 ~ 1, TRUE ~ 0),
         OTHER_INFECTION_MEAS_ANY_ENROLL = case_when(OTHER_INFECTION_MEAS_ANY_1 ==1 ~ 1, TRUE ~ 0),
         OTHER_INFECTION_LAB_ANY_ENROLL = case_when(OTHER_INFECTION_LAB_ANY_1 ==1 ~ 1, TRUE ~ 0),
  ) %>% 
  ## generate vars for any test during pregnancy received. If you 
  mutate(MAL_TEST_PERF_EVER_PREG = case_when(MAL_DIAG_RESULT_1 ==1 | MAL_DIAG_RESULT_2 ==1 | MAL_DIAG_RESULT_3 ==1 | MAL_DIAG_RESULT_4 ==1 | MAL_DIAG_RESULT_5 ==1 | 
                                               MAL_MEAS_RESULT_1 ==1 | MAL_MEAS_RESULT_2 ==1 | MAL_MEAS_RESULT_3 ==1 | MAL_MEAS_RESULT_4 ==1 | MAL_MEAS_RESULT_5 ==1  ~ 1, 
                                             TRUE ~ 0),
         HBV_TEST_PERF_EVER_PREG = case_when(HBV_MEAS_RESULT_1 ==1 | HBV_MEAS_RESULT_2 ==1 | HBV_MEAS_RESULT_3 ==1 | HBV_MEAS_RESULT_4 ==1 | HBV_MEAS_RESULT_5 ==1  ~ 1, 
                                             TRUE ~ 0
         ),
         HCV_TEST_PERF_EVER_PREG = case_when(HCV_MEAS_RESULT_1 ==1 | HCV_MEAS_RESULT_2 ==1 | HCV_MEAS_RESULT_3 ==1 | HCV_MEAS_RESULT_4 ==1 | HCV_MEAS_RESULT_5 ==1  ~ 1, 
                                             TRUE ~ 0
         ),
  ) %>% 
  
  select(SITE, MOMID, PREGID, ends_with("ENROLL"), MAL_POSITIVE_ENROLL_HRP_POC,contains("_ANY_ANC"),
         contains("DENOM"), MAL_POSITIVE_ANY_PNC, contains("EVER_PREG"))

## CHECK FOR PNC INFECTIONS
table(mat_other_infection$HCV_POSITIVE_ENROLL, mat_other_infection$SITE)
table(mat_other_infection$HBV_POSITIVE_ENROLL, mat_other_infection$SITE)
table(mat_other_infection$MAL_POSITIVE_ENROLL, mat_other_infection$SITE)
table(mat_other_infection$TB_SYMP_POSITIVE_ENROLL, mat_other_infection$SITE)
table(mat_other_infection$TB_SPUTUM_POSITIVE_ENROLL, mat_other_infection$SITE)

table(mat_other_infection$HCV_POSITIVE_ANY_ANC, mat_other_infection$SITE)
table(mat_other_infection$HBV_POSITIVE_ANY_ANC, mat_other_infection$SITE)
table(mat_other_infection$MAL_POSITIVE_ANY_ANC, mat_other_infection$SITE)
table(mat_other_infection$TB_SYMP_POSITIVE_ANY_ANC, mat_other_infection$SITE)
table(mat_other_infection$TB_SPUTUM_POSITIVE_ANY_ANC, mat_other_infection$SITE)

table(mnh08$M08_ZCD_CHKIGM_LBORRES, mnh08$SITE)

## ZCD + Lepto
mat_expansion_infection <- mat_enroll %>% 
  left_join(mnh08_all_visits, by = c("SITE", "MOMID", "PREGID")) %>% 
  # filter(ENROLL_EXPANSION==1) %>%
  mutate(ENROLL_EXPANSION = case_when(is.na(ENROLL_EXPANSION) ~ 0, TRUE ~ ENROLL_EXPANSION)) %>% 
  distinct(SITE, MOMID, PREGID, TYPE_VISIT, .keep_all = TRUE) %>%
  # Is the test result missing among those with a completed form?  
  # diagnosed
  mutate(
    HEV_IGM_RESULT = case_when(M08_HEV_IGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1 & M08_HEV_LBPERF_1 ==1 ~ 1,ENROLL_EXPANSION!=1 ~ 77, TRUE ~ 0), 
    HEV_IGG_RESULT = case_when(M08_HEV_IGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),##  & M08_HEV_LBPERF_2 ==1 -- remove for now since SAS has n=11 observations with positive result but M08_HEV_LBPERF_2 = 77
    
    ## zcd
    ZIK_IGM_RESULT = case_when(M08_ZCD_ZIKIGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1 & M08_ZCD_LBPERF_1 ==1 ~ 1, ENROLL_EXPANSION!=1 ~ 77, TRUE ~0),
    ZIK_IGG_RESULT = case_when(M08_ZCD_ZIKIGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1 & M08_ZCD_LBPERF_2 ==1 ~ 1, ENROLL_EXPANSION!=1 ~ 77, TRUE ~0),
    DEN_IGM_RESULT = case_when(M08_ZCD_DENIGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1 & M08_ZCD_LBPERF_3 ==1 ~ 1, ENROLL_EXPANSION!=1 ~ 77, TRUE ~0),
    DEN_IGG_RESULT = case_when(M08_ZCD_DENIGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1 & M08_ZCD_LBPERF_4 ==1 ~ 1, ENROLL_EXPANSION!=1 ~ 77, TRUE ~0),
    CHK_IGM_RESULT = case_when(M08_ZCD_CHKIGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1 & M08_ZCD_LBPERF_5 ==1 ~ 1, ENROLL_EXPANSION!=1 ~ 77, TRUE ~0),
    CHK_IGG_RESULT = case_when(M08_ZCD_CHKIGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1 & M08_ZCD_LBPERF_6 ==1 ~ 1, ENROLL_EXPANSION!=1 ~ 77,TRUE ~0),
    
    # lepto
    LEP_IGM_RESULT = case_when(M08_LEPT_IGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1 & M08_LEPT_LBPERF_1 ==1 ~ 1,ENROLL_EXPANSION!=1 ~ 77, TRUE ~0),
    LEP_IGG_RESULT = case_when(M08_LEPT_IGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1 & M08_LEPT_LBPERF_1 ==1 ~ 1,ENROLL_EXPANSION!=1 ~ 77, TRUE ~0)
    # 
  ) %>% 
  
  # generate new var for any infection missing diagnosed and rdt
  mutate(
    HEV_IGM_MISSING = case_when(HEV_IGM_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
    HEV_IGG_MISSING = case_when(HEV_IGG_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
    
    # ZCD
    ZIK_IGM_MISSING = case_when(ZIK_IGM_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
    ZIK_IGG_MISSING = case_when(ZIK_IGG_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
    DEN_IGM_MISSING = case_when(DEN_IGM_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
    DEN_IGG_MISSING = case_when(DEN_IGG_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
    CHK_IGM_MISSING = case_when(CHK_IGM_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
    CHK_IGG_MISSING = case_when(CHK_IGG_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
    
    # LEPTO
    LEP_IGM_MISSING = case_when(LEP_IGM_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
    LEP_IGG_MISSING = case_when(LEP_IGG_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0)
    
    
  ) %>% 
  # generate new var defining a positive result
  mutate(
    HEV_IGM_POSITIVE = case_when(ENROLL_EXPANSION==0 ~ 77,
                                 M08_HEV_IGM_LBORRES == 1 & HEV_IGM_RESULT ==1 ~ 1,
                                 M08_HEV_IGM_LBORRES == 0 ~ 0,
                                 M08_HEV_IGM_LBORRES == 2 ~ 2, ## inconclusive
                                 M08_HEV_IGM_LBORRES %in% c(NA,55, 77, 99) ~ 55,
                                 TRUE ~ NA),
    HEV_IGG_POSITIVE = case_when(ENROLL_EXPANSION==0 ~ 77,
                                 M08_HEV_IGG_LBORRES == 1 & HEV_IGG_RESULT ==1  ~ 1,
                                 M08_HEV_IGG_LBORRES == 0 ~ 0,
                                 M08_HEV_IGG_LBORRES == 2 ~ 2, ## inconclusive
                                 M08_HEV_IGG_LBORRES %in% c(NA,55, 77, 99) ~ 55,
                                 TRUE ~ NA),
    # ZCD
    ZIK_IGM_POSITIVE = case_when(ENROLL_EXPANSION==0 ~ 77,
                                 M08_ZCD_ZIKIGM_LBORRES == 1 & ZIK_IGM_RESULT ==1  ~ 1,
                                 M08_ZCD_ZIKIGM_LBORRES == 0 ~ 0,
                                 M08_ZCD_ZIKIGM_LBORRES == 2 ~ 2, ## inconclusive
                                 M08_ZCD_ZIKIGM_LBORRES %in% c(NA,55, 77, 99) ~ 55,
                                 TRUE ~ NA),
    
    ZIK_IGG_POSITIVE = case_when(ENROLL_EXPANSION==0 ~ 77,
                                 M08_ZCD_ZIKIGG_LBORRES == 1 & ZIK_IGG_RESULT ==1 ~ 1,
                                 M08_ZCD_ZIKIGG_LBORRES == 0 ~ 0,
                                 M08_ZCD_ZIKIGG_LBORRES == 2 ~ 2, ## inconclusive
                                 M08_ZCD_ZIKIGG_LBORRES %in% c(NA,55, 77, 99) ~ 55,
                                 TRUE ~ NA),
    
    DEN_IGM_POSITIVE = case_when(ENROLL_EXPANSION==0 ~ 77,
                                 M08_ZCD_DENIGM_LBORRES == 1 & DEN_IGM_RESULT ==1 ~ 1,
                                 M08_ZCD_DENIGM_LBORRES == 0 ~ 0,
                                 M08_ZCD_DENIGM_LBORRES == 2 ~ 2, ## inconclusive
                                 M08_ZCD_DENIGM_LBORRES %in% c(NA,55, 77, 99) ~ 55,
                                 TRUE ~ NA),
    
    DEN_IGG_POSITIVE = case_when(ENROLL_EXPANSION==0 ~ 77,
                                 M08_ZCD_DENIGG_LBORRES == 1 & DEN_IGG_RESULT ==1 ~ 1,
                                 M08_ZCD_DENIGG_LBORRES == 0 ~ 0,
                                 M08_ZCD_DENIGG_LBORRES == 2 ~ 2, ## inconclusive
                                 M08_ZCD_DENIGG_LBORRES %in% c(NA,55, 77, 99) ~ 55,
                                 TRUE ~ NA),                           
    
    CHK_IGM_POSITIVE = case_when(ENROLL_EXPANSION==0 ~ 77,
                                 M08_ZCD_CHKIGM_LBORRES == 1 & CHK_IGM_RESULT ==1 ~ 1,
                                 M08_ZCD_CHKIGM_LBORRES == 0 ~ 0,
                                 M08_ZCD_CHKIGM_LBORRES == 2 ~ 2, ## inconclusive
                                 M08_ZCD_CHKIGM_LBORRES %in% c(NA,55, 77, 99) ~ 55,
                                 TRUE ~ NA), 
    
    CHK_IGG_POSITIVE = case_when(ENROLL_EXPANSION==0 ~ 77,
                                 M08_ZCD_CHKIGG_LBORRES == 1 & CHK_IGG_RESULT ==1 ~ 1,
                                 M08_ZCD_CHKIGG_LBORRES == 0 ~ 0,
                                 M08_ZCD_CHKIGG_LBORRES == 2 ~ 2, ## inconclusive
                                 M08_ZCD_CHKIGG_LBORRES %in% c(NA,55, 77, 99) ~ 55,
                                 TRUE ~ NA), 
    
    # LEPTO
    LEP_IGM_POSITIVE = case_when(ENROLL_EXPANSION==0 ~ 77,
                                 M08_LEPT_IGM_LBORRES == 1 & LEP_IGM_RESULT ==1 ~ 1,
                                 M08_LEPT_IGM_LBORRES == 0 ~ 0,
                                 M08_LEPT_IGM_LBORRES == 2 ~ 2, ## inconclusive
                                 M08_LEPT_IGM_LBORRES %in% c(NA,55, 77, 99) ~ 55,
                                 TRUE ~ NA),
    
    LEP_IGG_POSITIVE = case_when(ENROLL_EXPANSION==0 ~ 77,
                                 M08_LEPT_IGG_LBORRES == 1 & LEP_IGG_RESULT ==1 ~ 1,
                                 M08_LEPT_IGG_LBORRES == 0 ~ 0,
                                 M08_LEPT_IGG_LBORRES == 2 ~ 2, ## inconclusive
                                 M08_LEPT_IGG_LBORRES %in% c(NA,55, 77, 99) ~ 55,
                                 TRUE ~ NA)   
    
  ) %>%
  # generate summary any infection variables (diagnosed, measured, lab)
  mutate(
    OTHER_INFECTION_MEAS_EXPANSION_ANY = case_when(M08_ZCD_LBPERF_1 ==1 | M08_ZCD_LBPERF_2 == 1|
                                                     M08_ZCD_LBPERF_3 ==1 | M08_ZCD_LBPERF_4 == 1|
                                                     M08_ZCD_LBPERF_5 ==1 | M08_ZCD_LBPERF_6 == 1|
                                                     M08_LEPT_LBPERF_1 ==1 | M08_LEPT_LBPERF_2 ==1 |
                                                     M08_HEV_LBPERF_1 ==1 | M08_HEV_LBPERF_2 ==1 ~ 1, TRUE ~ 0)) %>%
  mutate(ANY_ARBOVIRUS = case_when(HEV_IGM_POSITIVE == 1 | HEV_IGG_POSITIVE == 1 | 
                                     ZIK_IGM_POSITIVE == 1 | ZIK_IGG_POSITIVE == 1 | 
                                     DEN_IGM_POSITIVE == 1 | DEN_IGG_POSITIVE == 1 | 
                                     LEP_IGM_POSITIVE == 1 | LEP_IGG_POSITIVE == 1 | 
                                     CHK_IGM_POSITIVE == 1 | CHK_IGG_POSITIVE == 1  ~ 1, 
                                   HEV_IGM_POSITIVE == 0 & HEV_IGG_POSITIVE == 0 & 
                                     ZIK_IGM_POSITIVE == 0 & ZIK_IGG_POSITIVE == 0 & 
                                     DEN_IGM_POSITIVE == 0 & DEN_IGG_POSITIVE == 0 & 
                                     LEP_IGM_POSITIVE == 0 & LEP_IGG_POSITIVE == 0 & 
                                     CHK_IGM_POSITIVE == 0 & CHK_IGG_POSITIVE == 0 ~ 0,
                                   TRUE ~ 55)) %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, TYPE_VISIT, ENROLL_EXPANSION, OTHER_INFECTION_MEAS_EXPANSION_ANY,
         ZIK_IGM_POSITIVE, ZIK_IGG_POSITIVE, DEN_IGM_POSITIVE, DEN_IGG_POSITIVE, CHK_IGM_POSITIVE, CHK_IGG_POSITIVE,
         ZIK_IGM_MISSING, ZIK_IGG_MISSING,DEN_IGM_MISSING, DEN_IGG_MISSING, CHK_IGM_MISSING, CHK_IGG_MISSING,
         HEV_IGM_POSITIVE, HEV_IGG_POSITIVE, HEV_IGM_MISSING, HEV_IGG_MISSING, 
         LEP_IGM_POSITIVE, LEP_IGG_POSITIVE, LEP_IGM_MISSING, LEP_IGG_MISSING, 
         contains("MISSING"), contains("_RESULT"), ANY_ARBOVIRUS) %>% # , contains("_RESULT")
  
  ## TROUBLESHOOTING
  pivot_wider(
    names_from = TYPE_VISIT,
    values_from = c(ENROLL_EXPANSION, OTHER_INFECTION_MEAS_EXPANSION_ANY,
                    ZIK_IGM_POSITIVE, ZIK_IGG_POSITIVE, DEN_IGM_POSITIVE, DEN_IGG_POSITIVE, CHK_IGM_POSITIVE, CHK_IGG_POSITIVE, 
                    ZIK_IGM_MISSING, ZIK_IGG_MISSING,DEN_IGM_MISSING, DEN_IGG_MISSING, CHK_IGM_MISSING, CHK_IGG_MISSING,
                    HEV_IGM_POSITIVE, HEV_IGG_POSITIVE, HEV_IGM_MISSING, HEV_IGG_MISSING, 
                    LEP_IGM_POSITIVE, LEP_IGG_POSITIVE, LEP_IGM_MISSING, LEP_IGG_MISSING, contains("_RESULT"), ANY_ARBOVIRUS),
    names_glue = "{.value}_{TYPE_VISIT}"
  ) %>%
  # generate new var for any syphilis positive result at any visit (EXCLUDING enrollment) 
  mutate(
    HEV_IGM_POSITIVE_ANY_ANC = case_when(HEV_IGM_POSITIVE_1 != 1 & (HEV_IGM_POSITIVE_4 ==1) ~ 1, 
                                         HEV_IGM_POSITIVE_1 == 1 & (HEV_IGM_POSITIVE_4 ==1) ~ 2, ## persistent infection
                                         HEV_IGM_POSITIVE_4 == 0 ~ 0,
                                         HEV_IGM_POSITIVE_4 == 2 ~ 3, ## inconclusive test result 
                                         HEV_IGM_POSITIVE_4 %in% c(55, 77, 99, NA) ~ 55,
                                         TRUE ~ NA),
    
    HEV_IGG_POSITIVE_ANY_ANC = case_when(HEV_IGG_POSITIVE_1 != 1 & (HEV_IGG_POSITIVE_4 ==1)~ 1,
                                         HEV_IGG_POSITIVE_1 == 1 & (HEV_IGG_POSITIVE_4 ==1) ~ 2, ## persistent infection
                                         HEV_IGG_POSITIVE_4 == 0 ~ 0,
                                         HEV_IGG_POSITIVE_4 == 2 ~ 3, ## inconclusive test result 
                                         HEV_IGG_POSITIVE_4 %in% c(55, 77, 99, NA) ~ 55,
                                         TRUE ~ NA),    
    # ZCD
    ZIK_IGM_POSITIVE_ANY_ANC = case_when(ZIK_IGM_POSITIVE_1 != 1 & (ZIK_IGM_POSITIVE_4 ==1) ~ 1,
                                         ZIK_IGM_POSITIVE_1 == 1 & (ZIK_IGM_POSITIVE_4 ==1) ~ 2, ## persistent infection
                                         ZIK_IGM_POSITIVE_4 == 0 ~ 0,
                                         ZIK_IGM_POSITIVE_4 == 2 ~ 3, ## inconclusive test result 
                                         ZIK_IGM_POSITIVE_4 %in% c(55, 77, 99, NA) ~ 55,
                                         TRUE ~ NA), 
    
    ZIK_IGG_POSITIVE_ANY_ANC = case_when(ZIK_IGG_POSITIVE_1 != 1 & (ZIK_IGG_POSITIVE_4 ==1) ~ 1,
                                         ZIK_IGG_POSITIVE_1 == 1 & (ZIK_IGG_POSITIVE_4 ==1) ~ 2, ## persistent infection
                                         ZIK_IGG_POSITIVE_4 == 0 ~ 0,
                                         ZIK_IGG_POSITIVE_4 == 2 ~ 3, ## inconclusive test result 
                                         ZIK_IGG_POSITIVE_4 %in% c(55, 77, 99, NA) ~ 55,
                                         TRUE ~ NA),   
    
    DEN_IGM_POSITIVE_ANY_ANC = case_when(DEN_IGM_POSITIVE_1 != 1 & (DEN_IGM_POSITIVE_4 ==1) ~ 1, 
                                         DEN_IGM_POSITIVE_1 == 1 & (DEN_IGM_POSITIVE_4 ==1) ~ 2, ## persistent infection
                                         DEN_IGM_POSITIVE_4 == 0 ~ 0,
                                         DEN_IGM_POSITIVE_4 == 2 ~ 3, ## inconclusive test result 
                                         DEN_IGM_POSITIVE_4 %in% c(55, 77, 99, NA) ~ 55,
                                         TRUE ~ NA),   
    
    DEN_IGG_POSITIVE_ANY_ANC = case_when(DEN_IGG_POSITIVE_1 != 1 & (DEN_IGG_POSITIVE_4 ==1) ~ 1,
                                         DEN_IGG_POSITIVE_1 == 1 & (DEN_IGG_POSITIVE_4 ==1) ~ 2, ## persistent infection
                                         DEN_IGG_POSITIVE_4 == 0 ~ 0,
                                         DEN_IGG_POSITIVE_4 == 2 ~ 3, ## inconclusive test result 
                                         DEN_IGG_POSITIVE_4 %in% c(55, 77, 99, NA) ~ 55,
                                         TRUE ~ NA),  
    
    CHK_IGM_POSITIVE_ANY_ANC = case_when(CHK_IGM_POSITIVE_1 != 1 & (CHK_IGM_POSITIVE_4 ==1) ~ 1,
                                         CHK_IGM_POSITIVE_1 == 1 & (CHK_IGM_POSITIVE_4 ==1) ~ 2, ## persistent infection
                                         CHK_IGM_POSITIVE_4 == 0 ~ 0,
                                         CHK_IGM_POSITIVE_4 == 2 ~ 3, ## inconclusive test result 
                                         CHK_IGM_POSITIVE_4 %in% c(55, 77, 99, NA) ~ 55,
                                         TRUE ~ NA),  
    
    CHK_IGG_POSITIVE_ANY_ANC = case_when(CHK_IGG_POSITIVE_1 != 1 & (CHK_IGG_POSITIVE_4 ==1) ~ 1,
                                         CHK_IGG_POSITIVE_1 == 1 & (CHK_IGG_POSITIVE_4 ==1) ~ 2, ## persistent infection
                                         CHK_IGG_POSITIVE_4 == 0 ~ 0,
                                         CHK_IGG_POSITIVE_4 == 2 ~ 3, ## inconclusive test result 
                                         CHK_IGG_POSITIVE_4 %in% c(55, 77, 99, NA) ~ 55,
                                         TRUE ~ NA),  
    # LEPTO
    LEP_IGM_POSITIVE_ANY_ANC = case_when(LEP_IGM_POSITIVE_1 != 1 & (LEP_IGM_POSITIVE_4 ==1) ~ 1,
                                         LEP_IGM_POSITIVE_1 == 1 & (LEP_IGM_POSITIVE_4 ==1) ~ 2, ## persistent infection
                                         LEP_IGM_POSITIVE_4 == 0 ~ 0,
                                         LEP_IGM_POSITIVE_4 == 2 ~ 3, ## inconclusive test result 
                                         LEP_IGM_POSITIVE_4 %in% c(55, 77, 99, NA) ~ 55,
                                         TRUE ~ NA), 
    
    LEP_IGG_POSITIVE_ANY_ANC = case_when(LEP_IGG_POSITIVE_1 != 1 & (LEP_IGG_POSITIVE_4 ==1) ~ 1,
                                         LEP_IGG_POSITIVE_1 == 1 & (LEP_IGG_POSITIVE_4 ==1) ~ 2, ## persistent infection
                                         LEP_IGG_POSITIVE_4 == 0 ~ 0,
                                         LEP_IGG_POSITIVE_4 == 2 ~ 3, ## inconclusive test result 
                                         LEP_IGG_POSITIVE_4 %in% c(55, 77, 99, NA) ~ 55,
                                         TRUE ~ NA)    
  ) %>% 
  # generate new var for any syphilis positive result at any pnc (EXCLUDING enrollment & anc)
  mutate(
    HEV_IGM_POSITIVE_ANY_PNC = case_when((HEV_IGM_POSITIVE_1 != 1 | HEV_IGM_POSITIVE_ANY_ANC !=1) & (HEV_IGM_POSITIVE_10 ==1) ~ 1, TRUE ~ 0),
    HEV_IGG_POSITIVE_ANY_PNC = case_when((HEV_IGG_POSITIVE_1 != 1 | HEV_IGG_POSITIVE_ANY_ANC !=1) & (HEV_IGG_POSITIVE_10 ==1)~ 1, TRUE ~ 0),
    
    # ZCD
    ZIK_IGM_POSITIVE_ANY_PNC = case_when((ZIK_IGM_POSITIVE_1 != 1 | ZIK_IGM_POSITIVE_ANY_ANC !=1) & (ZIK_IGM_POSITIVE_10 ==1) ~ 1, TRUE ~ 0),
    ZIK_IGG_POSITIVE_ANY_PNC = case_when((ZIK_IGG_POSITIVE_1 != 1 | ZIK_IGG_POSITIVE_ANY_ANC != 1) & (ZIK_IGG_POSITIVE_10 ==1) ~ 1, TRUE ~ 0),
    DEN_IGM_POSITIVE_ANY_PNC = case_when((DEN_IGM_POSITIVE_1 != 1 | DEN_IGM_POSITIVE_ANY_ANC !=1) & (DEN_IGM_POSITIVE_10 ==1) ~ 1, TRUE ~ 0),
    DEN_IGG_POSITIVE_ANY_PNC = case_when((DEN_IGG_POSITIVE_1 != 1 | DEN_IGG_POSITIVE_ANY_ANC !=1) & (DEN_IGG_POSITIVE_10 ==1) ~ 1, TRUE ~ 0),
    CHK_IGM_POSITIVE_ANY_PNC = case_when((CHK_IGM_POSITIVE_1 != 1 | CHK_IGM_POSITIVE_ANY_ANC !=1) & (CHK_IGM_POSITIVE_10 ==1) ~ 1, TRUE ~ 0),
    CHK_IGG_POSITIVE_ANY_PNC = case_when((CHK_IGG_POSITIVE_1 != 1 | CHK_IGG_POSITIVE_ANY_ANC !=1) & (CHK_IGG_POSITIVE_10 ==1) ~ 1, TRUE ~ 0),
    
    # LEPTO
    LEP_IGM_POSITIVE_ANY_PNC = case_when((LEP_IGM_POSITIVE_1 != 1 | LEP_IGM_POSITIVE_ANY_ANC !=1) & (LEP_IGM_POSITIVE_10 ==1) ~ 1, TRUE ~ 0),
    LEP_IGG_POSITIVE_ANY_PNC = case_when((LEP_IGG_POSITIVE_1 != 1 | LEP_IGG_POSITIVE_ANY_ANC !=1) & (LEP_IGG_POSITIVE_10 ==1) ~ 1, TRUE ~ 0)
    
  ) %>% 
  # generate missing variables 
  mutate(
    HEV_IGM_MISSING_ENROLL = case_when(HEV_IGM_MISSING_1==1 ~ 1, TRUE ~ 0),
    HEV_IGG_MISSING_ENROLL = case_when(HEV_IGG_MISSING_1==1 ~ 1, TRUE ~ 0),
    
    # ZCCD
    ZIK_IGM_MISSING_ENROLL = case_when(ZIK_IGM_MISSING_1==1 ~ 1, TRUE ~ 0),
    ZIK_IGG_MISSING_ENROLL = case_when(ZIK_IGG_MISSING_1==1 ~ 1, TRUE ~ 0),
    DEN_IGM_MISSING_ENROLL = case_when(DEN_IGM_MISSING_1==1 ~ 1, TRUE ~ 0),
    DEN_IGG_MISSING_ENROLL = case_when(DEN_IGG_MISSING_1==1 ~ 1, TRUE ~ 0),
    CHK_IGM_MISSING_ENROLL = case_when(CHK_IGM_MISSING_1==1 ~ 1, TRUE ~ 0),
    CHK_IGG_MISSING_ENROLL = case_when(CHK_IGG_MISSING_1==1 ~ 1, TRUE ~ 0),
    
    # LEPTO
    LEP_IGM_MISSING_ENROLL = case_when(LEP_IGM_MISSING_1==1 ~ 1, TRUE ~ 0),
    LEP_IGG_MISSING_ENROLL = case_when(LEP_IGG_MISSING_1==1 ~ 1, TRUE ~ 0),
    
  ) %>% 
  
  # generate enrollment prevalence variables (exclude missing)
  mutate(
    HEV_IGM_POSITIVE_ENROLL = case_when(HEV_IGM_POSITIVE_1 == 1 & HEV_IGM_MISSING_ENROLL == 0 ~ 1,
                                        HEV_IGM_POSITIVE_1 == 0 ~ 0, 
                                        HEV_IGM_POSITIVE_1 == 2 ~ 3, ## inconclusive test result 
                                        HEV_IGM_POSITIVE_1 %in% c(NA, 55, 77) ~ 55,
                                        TRUE ~ NA),
    HEV_IGG_POSITIVE_ENROLL = case_when(HEV_IGG_POSITIVE_1 == 1 & HEV_IGG_MISSING_ENROLL == 0 ~ 1, 
                                        HEV_IGG_POSITIVE_1 == 0 ~ 0, 
                                        HEV_IGG_POSITIVE_1 == 2 ~ 3, ## inconclusive test result 
                                        HEV_IGG_POSITIVE_1 %in% c(NA, 55, 77) ~ 55,
                                        TRUE ~ NA),         
    #ZCD
    ZIK_IGM_POSITIVE_ENROLL = case_when(ZIK_IGM_POSITIVE_1 == 1 & ZIK_IGM_MISSING_ENROLL == 0 ~ 1,
                                        ZIK_IGM_POSITIVE_1 == 0 ~ 0, 
                                        ZIK_IGM_POSITIVE_1 == 2 ~ 3, ## inconclusive test result 
                                        ZIK_IGM_POSITIVE_1 %in% c(NA, 55, 77) ~ 55,
                                        TRUE ~ NA), 
    ZIK_IGG_POSITIVE_ENROLL = case_when(ZIK_IGG_POSITIVE_1 == 1 & ZIK_IGG_MISSING_ENROLL == 0 ~ 1,
                                        ZIK_IGG_POSITIVE_1 == 0 ~ 0, 
                                        ZIK_IGG_POSITIVE_1 == 2 ~ 3, ## inconclusive test result 
                                        ZIK_IGG_POSITIVE_1 %in% c(NA, 55, 77) ~ 55,
                                        TRUE ~ NA),        
    DEN_IGM_POSITIVE_ENROLL = case_when(DEN_IGM_POSITIVE_1 == 1 & DEN_IGM_MISSING_ENROLL == 0 ~ 1,
                                        DEN_IGM_POSITIVE_1 == 0 ~ 0,
                                        DEN_IGM_POSITIVE_1 == 2 ~ 3, ## inconclusive test result 
                                        DEN_IGM_POSITIVE_1 %in% c(NA, 55, 77) ~ 55,
                                        TRUE ~ NA), 
    DEN_IGG_POSITIVE_ENROLL = case_when(DEN_IGG_POSITIVE_1 == 1 & DEN_IGG_MISSING_ENROLL == 0 ~ 1,
                                        DEN_IGG_POSITIVE_1 == 0 ~ 0, 
                                        DEN_IGG_POSITIVE_1 == 2 ~ 3, ## inconclusive test result 
                                        DEN_IGG_POSITIVE_1 %in% c(NA, 55, 77) ~ 55,
                                        TRUE ~ NA), 
    CHK_IGM_POSITIVE_ENROLL = case_when(CHK_IGM_POSITIVE_1 == 1 & CHK_IGM_MISSING_ENROLL == 0 ~ 1,
                                        CHK_IGM_POSITIVE_1 == 0 ~ 0, 
                                        CHK_IGM_POSITIVE_1 == 2 ~ 3, ## inconclusive test result 
                                        CHK_IGM_POSITIVE_1 %in% c(NA, 55, 77) ~ 55,
                                        TRUE ~ NA),
    CHK_IGG_POSITIVE_ENROLL = case_when(CHK_IGG_POSITIVE_1 == 1 & CHK_IGG_MISSING_ENROLL == 0 ~ 1,
                                        CHK_IGG_POSITIVE_1 == 0 ~ 0, 
                                        CHK_IGG_POSITIVE_1 == 2 ~ 3, ## inconclusive test result 
                                        CHK_IGG_POSITIVE_1 %in% c(NA, 55, 77) ~ 55,
                                        TRUE ~ NA),         
    #LEPTO
    LEP_IGM_POSITIVE_ENROLL = case_when(LEP_IGM_POSITIVE_1 == 1 & LEP_IGM_MISSING_ENROLL == 0 ~ 1,
                                        LEP_IGM_POSITIVE_1 == 0 ~ 0, 
                                        LEP_IGM_POSITIVE_1 == 2 ~ 3, ## inconclusive test result 
                                        LEP_IGM_POSITIVE_1 %in% c(NA, 55, 77) ~ 55,
                                        TRUE ~ NA),
    LEP_IGG_POSITIVE_ENROLL = case_when(LEP_IGG_POSITIVE_1 == 1 & LEP_IGG_MISSING_ENROLL == 0 ~ 1,
                                        LEP_IGG_POSITIVE_1 == 0 ~ 0, 
                                        LEP_IGG_POSITIVE_1 == 2 ~ 3, ## inconclusive test result 
                                        LEP_IGG_POSITIVE_1 %in% c(NA, 55, 77) ~ 55,
                                        TRUE ~ NA)        
  ) %>% 
  ## generate vars for any test during pregnancy received. If you 
  mutate(HEV_IGM_TEST_PERF_EVER_PREG = case_when(HEV_IGM_RESULT_1 ==1 | HEV_IGM_RESULT_2 == 1 | HEV_IGM_RESULT_3 ==1 |HEV_IGM_RESULT_4 ==1 | HEV_IGM_RESULT_5 ==1 ~ 1, TRUE ~ 0),
         HEV_IGG_TEST_PERF_EVER_PREG = case_when(HEV_IGG_RESULT_1 ==1 | HEV_IGG_RESULT_2 == 1 | HEV_IGG_RESULT_3 ==1 |HEV_IGG_RESULT_4 ==1 | HEV_IGG_RESULT_5 ==1 ~ 1, TRUE ~ 0),
         ZIK_IGM_TEST_PERF_EVER_PREG = case_when(ZIK_IGM_RESULT_1 ==1 | ZIK_IGM_RESULT_2 == 1 | ZIK_IGM_RESULT_3 ==1 |ZIK_IGM_RESULT_4 ==1 | ZIK_IGM_RESULT_5 ==1 ~ 1, TRUE ~ 0),
         ZIK_IGG_TEST_PERF_EVER_PREG = case_when(ZIK_IGG_RESULT_1 ==1 | ZIK_IGG_RESULT_2 == 1 | ZIK_IGG_RESULT_3 ==1 |ZIK_IGG_RESULT_4 ==1 | ZIK_IGG_RESULT_5 ==1 ~ 1, TRUE ~ 0),
         DEN_IGM_TEST_PERF_EVER_PREG = case_when(DEN_IGM_RESULT_1 ==1 | DEN_IGM_RESULT_2 == 1 | DEN_IGM_RESULT_3 ==1 |DEN_IGM_RESULT_4 ==1 | DEN_IGM_RESULT_5 ==1 ~ 1, TRUE ~ 0),
         DEN_IGG_TEST_PERF_EVER_PREG = case_when(DEN_IGG_RESULT_1 ==1 | DEN_IGG_RESULT_2 == 1 | DEN_IGG_RESULT_3 ==1 |DEN_IGG_RESULT_4 ==1 | DEN_IGG_RESULT_5 ==1 ~ 1, TRUE ~ 0),
         CHK_IGM_TEST_PERF_EVER_PREG = case_when(CHK_IGM_RESULT_1 ==1 | CHK_IGM_RESULT_2 == 1 | CHK_IGM_RESULT_3 ==1 |CHK_IGM_RESULT_4 ==1 | CHK_IGM_RESULT_5 ==1 ~ 1, TRUE ~ 0),
         CHK_IGG_TEST_PERF_EVER_PREG = case_when(CHK_IGG_RESULT_1 ==1 | CHK_IGG_RESULT_2 == 1 | CHK_IGG_RESULT_3 ==1 |CHK_IGG_RESULT_4 ==1 | CHK_IGG_RESULT_5 ==1 ~ 1, TRUE ~ 0),
         LEP_IGM_TEST_PERF_EVER_PREG = case_when(LEP_IGM_RESULT_1 ==1 | LEP_IGM_RESULT_2 == 1 | LEP_IGM_RESULT_3 ==1 |LEP_IGM_RESULT_4 ==1 | LEP_IGM_RESULT_5 ==1 ~ 1, TRUE ~ 0),
         LEP_IGG_TEST_PERF_EVER_PREG = case_when(LEP_IGG_RESULT_1 ==1 | LEP_IGG_RESULT_2 == 1 | LEP_IGG_RESULT_3 ==1 |LEP_IGG_RESULT_4 ==1 | LEP_IGG_RESULT_5 ==1 ~ 1, TRUE ~ 0)
  ) %>%
  rename(ANY_ARBOVIRUS_ENROLL = ANY_ARBOVIRUS_1) %>% 
  select(SITE, MOMID, PREGID,ENROLL_EXPANSION_1,  ends_with("ENROLL"), contains("_ANY_ANC"),
         contains("DENOM"),contains("_EVER_PREG"),
         OTHER_INFECTION_MEAS_EXPANSION_ANY_1)

table(mat_expansion_infection$HEV_IGM_POSITIVE_ENROLL, mat_expansion_infection$SITE)
table(mat_expansion_infection$HEV_IGG_POSITIVE_ENROLL, mat_expansion_infection$SITE)
table(mat_expansion_infection$HEV_IGM_POSITIVE_ANY_ANC, mat_expansion_infection$SITE)
table(mat_expansion_infection$HEV_IGG_POSITIVE_ANY_ANC, mat_expansion_infection$SITE)

table(mat_expansion_infection$ANY_ARBOVIRUS_ENROLL)

table(mat_expansion_infection$HEV_IGM_POSITIVE_ENROLL, mat_expansion_infection$ANY_ARBOVIRUS_ENROLL)
table(mat_expansion_infection$HEV_IGG_POSITIVE_ENROLL, mat_expansion_infection$ANY_ARBOVIRUS_ENROLL)

#*****************************************************************************
#### All infections combined NEW ####
#*****************************************************************************

MAT_INFECTION <- full_join(mat_infection_sti, mat_other_infection, by = c("SITE", "MOMID", "PREGID")) %>% 
  # merge in enrollment indicator
  full_join(mat_enroll, by = c("SITE", "MOMID", "PREGID")) %>% 
  full_join(mat_expansion_infection, by = c("SITE", "MOMID", "PREGID")) %>% 
  full_join(mat_infection_ctng, by = c("SITE", "MOMID", "PREGID")) %>% 
  
  left_join(mat_end %>% select(SITE, MOMID, PREGID, PREG_END),  by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(PREG_END = case_when(PREG_END ==1 ~ 1, TRUE ~  0)) %>% 
  rename(ENROLL_EXPANSION = ENROLL_EXPANSION_1) %>% 
  mutate(ENROLL_EXPANSION = case_when(is.na(ENROLL_EXPANSION) ~ 0, TRUE ~ ENROLL_EXPANSION)) %>% 
  # generate variables for any infection diagnosed 
  mutate(ANY_INFECTION_DIAGNOSED_ENROLL = case_when(ANY_DIAG_STI_ENROLL == 1 | OTHER_INFECTION_DIAG_ANY_ENROLL==1~1, TRUE ~0),
         # generate variables for any infection diagnosed 
         ANY_INFECTION_MEASURED_ENROLL = case_when(ANY_MEAS_STI_ENROLL == 1 | OTHER_INFECTION_MEAS_ANY_ENROLL==1 | 
                                                     ANY_EXPANSION_ENROLL ==1 | ANY_ARBOVIRUS_ENROLL ==1 ~1, TRUE ~0), 
         # generate variables for any infection with either method 
         INFECTION_ANY_METHOD_ENROLL = case_when(ANY_INFECTION_DIAGNOSED_ENROLL == 1 | ANY_INFECTION_MEASURED_ENROLL==1~1, TRUE ~0)
  ) %>% 
  # generate denominators for any infection diagnosed by either method
  mutate(INFECTION_ENROLL_DENOM = case_when(ENROLL==1 ~ 1, TRUE ~ 0)) %>% # INFECTION_ANY_METHOD_DENOM = 1
  # rename variables 
  rename(EXPANSION_ENROLL = ENROLL_EXPANSION) %>% 
  select(-ENROLL,-ENROLL_SCRN_DATE, -OTHER_INFECTION_MEAS_EXPANSION_ANY_1, -OTHER_INFECTION_DIAG_ANY_ENROLL, 
         -OTHER_INFECTION_MEAS_ANY_ENROLL, -OTHER_INFECTION_LAB_ANY_ENROLL, -contains("_MEAS_PERF_")) %>% 
  ## EVER PREGNANCY VARIABLES 
  mutate(HIV_EVER_PREG = case_when(HIV_POSITIVE_ENROLL ==1 | HIV_POSITIVE_ANY_ANC==1 ~ 1, # positive during pregnancy
                                   HIV_POSITIVE_ENROLL ==0 | HIV_POSITIVE_ANY_ANC==0 ~ 0, # negative during pregnancy 
                                   HIV_POSITIVE_ENROLL %in% c(55, 77, 99) & HIV_POSITIVE_ANY_ANC %in% c(55, 77, 99) ~ 55, # all tests are missing during pregnancy
                                   TRUE ~ NA         
  ),
  SYPH_EVER_PREG = case_when(SYPH_POSITIVE_ENROLL ==1 | SYPH_POSITIVE_ANY_ANC==1 ~ 1, # positive during pregnancy
                             SYPH_POSITIVE_ENROLL ==0 | SYPH_POSITIVE_ANY_ANC==0 ~ 0, # negative during pregnancy 
                             SYPH_POSITIVE_ENROLL %in% c(55, 77, 99) & SYPH_POSITIVE_ANY_ANC %in% c(55, 77, 99) ~ 55, # all tests are missing during pregnancy
                             TRUE ~ NA         
  ),
  GON_EVER_PREG = case_when(GON_POSITIVE_ENROLL ==1 | GON_POSITIVE_ANY_ANC==1 ~ 1, # positive during pregnancy
                            GON_POSITIVE_ENROLL ==0 | GON_POSITIVE_ANY_ANC==0 ~ 0, # negative during pregnancy 
                            GON_POSITIVE_ENROLL %in% c(55, 77, 99) & GON_POSITIVE_ANY_ANC %in% c(55, 77, 99) ~ 55, # all tests are missing during pregnancy
                            TRUE ~ NA         
  ),
  CHL_EVER_PREG = case_when(CHL_POSITIVE_ENROLL ==1 | CHL_POSITIVE_ANY_ANC==1 ~ 1, # positive during pregnancy
                            CHL_POSITIVE_ENROLL ==0 | CHL_POSITIVE_ANY_ANC==0 ~ 0, # negative during pregnancy 
                            CHL_POSITIVE_ENROLL %in% c(55, 77, 99) & CHL_POSITIVE_ANY_ANC %in% c(55, 77, 99) ~ 55, # all tests are missing during pregnancy
                            TRUE ~ NA         
  ),
  GENU_EVER_PREG = case_when(GENU_POSITIVE_ENROLL ==1 | GENU_POSITIVE_ANY_ANC==1 ~ 1, # positive during pregnancy
                             GENU_POSITIVE_ENROLL ==0 | GENU_POSITIVE_ANY_ANC==0 ~ 0, # negative during pregnancy 
                             GENU_POSITIVE_ENROLL %in% c(55, 77, 99) & GENU_POSITIVE_ANY_ANC %in% c(55, 77, 99) ~ 55, # all tests are missing during pregnancy
                             TRUE ~ NA         
  ),
  OTHR_EVER_PREG = case_when(OTHR_POSITIVE_ENROLL ==1 | OTHR_POSITIVE_ANY_ANC==1 ~ 1, # positive during pregnancy
                             OTHR_POSITIVE_ENROLL ==0 | OTHR_POSITIVE_ANY_ANC==0 ~ 0, # negative during pregnancy 
                             OTHR_POSITIVE_ENROLL %in% c(55, 77, 99) & OTHR_POSITIVE_ANY_ANC %in% c(55, 77, 99) ~ 55, # all tests are missing during pregnancy
                             TRUE ~ NA         
  ),
  MAL_EVER_PREG = case_when(MAL_POSITIVE_ENROLL ==1 | MAL_POSITIVE_ANY_ANC==1 ~ 1, # positive during pregnancy
                            MAL_POSITIVE_ENROLL ==0 | MAL_POSITIVE_ANY_ANC==0 ~ 0, # negative during pregnancy 
                            MAL_POSITIVE_ENROLL %in% c(55, 77, 99) & MAL_POSITIVE_ANY_ANC %in% c(55, 77, 99) ~ 55, # all tests are missing during pregnancy
                            TRUE ~ NA         
  ),
  HBV_EVER_PREG = case_when(HBV_POSITIVE_ENROLL ==1 | HBV_POSITIVE_ANY_ANC==1 ~ 1, # positive during pregnancy
                            HBV_POSITIVE_ENROLL ==0 | HBV_POSITIVE_ANY_ANC==0 ~ 0, # negative during pregnancy 
                            HBV_POSITIVE_ENROLL %in% c(55, 77, 99) & HBV_POSITIVE_ANY_ANC %in% c(55, 77, 99) ~ 55, # all tests are missing during pregnancy
                            TRUE ~ NA         
  ),
  HCV_EVER_PREG = case_when(HCV_POSITIVE_ENROLL ==1 | HCV_POSITIVE_ANY_ANC==1 ~ 1, # positive during pregnancy
                            HCV_POSITIVE_ENROLL ==0 | HCV_POSITIVE_ANY_ANC==0 ~ 0, # negative during pregnancy 
                            HCV_POSITIVE_ENROLL %in% c(55, 77, 99) & HCV_POSITIVE_ANY_ANC %in% c(55, 77, 99) ~ 55, # all tests are missing during pregnancy
                            TRUE ~ NA         
  ),
  TB_SYMP_EVER_PREG = case_when(TB_SYMP_POSITIVE_ENROLL ==1 | TB_SYMP_POSITIVE_ANY_ANC==1 ~ 1, # positive during pregnancy
                                TB_SYMP_POSITIVE_ENROLL ==0 | TB_SYMP_POSITIVE_ANY_ANC==0 ~ 0, # negative during pregnancy 
                                TB_SYMP_POSITIVE_ENROLL %in% c(55, 77, 99) & TB_SYMP_POSITIVE_ANY_ANC %in% c(55, 77, 99) ~ 55, # all tests are missing during pregnancy
                                TRUE ~ NA         
  ),
  TB_SPUTUM_EVER_PREG = case_when(TB_SPUTUM_POSITIVE_ENROLL ==1 | TB_SPUTUM_POSITIVE_ANY_ANC==1 ~ 1, # positive during pregnancy
                                  TB_SPUTUM_POSITIVE_ENROLL ==0 | TB_SPUTUM_POSITIVE_ANY_ANC==0 ~ 0, # negative during pregnancy 
                                  TB_SPUTUM_POSITIVE_ENROLL %in% c(55, 77, 99) & TB_SPUTUM_POSITIVE_ANY_ANC %in% c(55, 77, 99) ~ 55, # all tests are missing during pregnancy
                                  TRUE ~ NA         
  )
  ) %>% 
  mutate(across(everything(), ~ replace(., is.na(.), as.numeric(77)))) 



table(MAT_INFECTION$HIV_POSITIVE_ENROLL,MAT_INFECTION$HIV_MISSING_ENROLL, useNA = "ifany")
table(MAT_INFECTION$SYPH_POSITIVE_ENROLL,MAT_INFECTION$SYPH_MISSING_ENROLL, useNA = "ifany")
table(MAT_INFECTION$HIV_POSITIVE_ENROLL,MAT_INFECTION$HIV_MISSING_ENROLL, useNA = "ifany")
table(MAT_INFECTION$SYPH_POSITIVE_ENROLL,MAT_INFECTION$SYPH_MISSING_ENROLL, useNA = "ifany")

table(MAT_INFECTION$SYPH_TEST_PERF_EVER_PREG, useNA = "ifany")
table(MAT_INFECTION$SYPH_MISSING_ENROLL, useNA = "ifany")
table(MAT_INFECTION$SYPH_TEST_PERF_EVER_PREG, useNA = "ifany")
table(test_enroll$GON_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$CHL_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$GENU_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$OTHR_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany") ##NAS
table(test_enroll$MAL_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$HBV_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$HCV_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$TB_SYMP_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$TB_SPUTUM_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$HEV_IGM_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany") ## NAS
table(MAT_INFECTION$HEV_IGG_POSITIVE_ENROLL, MAT_INFECTION$SITE, useNA = "ifany")
table(test_enroll$ZIK_IGM_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$ZIK_IGG_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$DEN_IGM_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$DEN_IGG_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$CHK_IGM_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$CHK_IGG_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$LEP_IGM_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$LEP_IGG_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$HEV_IGM_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$CT_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")
table(test_enroll$NG_POSITIVE_ENROLL, test_enroll$SITE, useNA = "ifany")


test_enroll <- MAT_INFECTION %>% select(SITE, MOMID, PREGID, contains("POSITIVE_ANY_ANC")) %>% 
  filter(if_any(everything(), is.na))

table(test_enroll$GON_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$CHL_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$GENU_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$OTHR_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany") ##NAS
table(test_enroll$MAL_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$HBV_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$HCV_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$TB_SYMP_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$TB_SPUTUM_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$HEV_IGM_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany") ## NAS
table(MAT_INFECTION$HEV_IGG_POSITIVE_ANY_ANC, MAT_INFECTION$SITE, useNA = "ifany")
table(test_enroll$ZIK_IGM_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$ZIK_IGG_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$DEN_IGM_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$DEN_IGG_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$CHK_IGM_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$CHK_IGG_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$LEP_IGM_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$LEP_IGG_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$HEV_IGM_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$CT_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")
table(test_enroll$NG_POSITIVE_ANY_ANC, test_enroll$SITE, useNA = "ifany")


print(sum(MAT_INFECTION$INFECTION_ENROLL_DENOM))
print(dim(mat_enroll)[1])

table(MAT_INFECTION$HEV_IGG_POSITIVE_ANY_ANC)
table(MAT_INFECTION$HEV_IGM_POSITIVE_ANY_ANC)
library(openxlsx)
## save data set; this will get called into the report
write.csv(MAT_INFECTION, paste0(path_to_save, "MAT_INFECTION" ,".csv"), na="", row.names=FALSE)
write.csv(MAT_INFECTION, paste0(path_to_tnt, "MAT_INFECTION" ,".csv"), na="", row.names=FALSE)
write.xlsx(MAT_INFECTION, paste0(path_to_tnt, "MAT_INFECTION" ,".xlsx"), na="", rownames=FALSE)
