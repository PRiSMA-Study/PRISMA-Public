#*****************************************************************************
#*Lab report 
#*Includes: 
#*1. Lab missingess table
#*2. Data visualization of lab results
#*Author: Xiaoyan 
#*****************************************************************************
library(tidyverse)
library(lubridate)
library(readxl)
library(naniar)

#Site data upload date
UploadDate = "2024-05-03"

#*****************************************************************************
#* Data preparation
#*****************************************************************************
#******load data
#load mnh06 data
mnh06 <- read.csv(paste0("Z:/Stacked Data/",UploadDate,"/mnh06_merged.csv")) 

#load mnh08 data
mnh08 <- read.csv(paste0("Z:/Stacked Data/",UploadDate,"/mnh08_merged.csv")) 

#load mnh04 data for TB denominator
mnh04 <- read.csv(paste0("Z:/Stacked Data/",UploadDate,"/mnh04_merged.csv")) %>% 
  select(MOMID, PREGID, SITE, M04_TYPE_VISIT, M04_TB_MHOCCUR)

#oad df-mat data --> df_mat is a data of PRISMA enrolled mom with basic identifiers only
load(paste0("D:/Users/xyh/Documents/github/0-data-base/", UploadDate, "/df_mat.rda"))

#******merge data
#extract unique mnh06 and make wide data
mnh06_uni <- mnh06 %>% 
  group_by(SITE, MOMID, PREGID, M06_TYPE_VISIT) %>% 
  mutate(n = n()) %>% 
  filter(n == 1, M06_TYPE_VISIT %in% c(1:12)) 

mnh06_wide <- mnh06_uni %>% 
  pivot_wider(
    names_from = M06_TYPE_VISIT,
    values_from = -c("MOMID", "PREGID", "SITE")
  ) %>% 
  select(-starts_with("n_"))

#extract unique mnh08 and wide data
mnh08_uni <- mnh08 %>% 
  group_by(SITE, MOMID, PREGID, M08_TYPE_VISIT) %>% 
  mutate(n = n()) %>% 
  filter(n == 1, M08_TYPE_VISIT %in% c(1:12))

mnh08_wide <- mnh08_uni %>% 
  pivot_wider(
    names_from = M08_TYPE_VISIT,
    values_from = -c("MOMID", "PREGID", "SITE")
  ) %>% 
  select(-starts_with("n_"))

#extract unique mnh04 and wide data
mnh04_uni <- mnh04 %>% 
  group_by(SITE, MOMID, PREGID, M04_TYPE_VISIT) %>% 
  mutate(n = n()) %>% 
  filter(n == 1, M04_TYPE_VISIT %in% c(1:12))

mnh04_wide <- mnh04_uni %>% 
  pivot_wider(
    names_from = M04_TYPE_VISIT,
    values_from = -c("MOMID", "PREGID", "SITE")
  ) %>% 
  select(-starts_with("n_"))

#merge to create maternal data
df_maternal <- df_mat %>% 
  select(SITE, MOMID, PREGID, DOB, BOE_GA_DAYS_ENROLL, EST_CONCEP_DATE, 
         M02_SCRN_OBSSTDAT,
         starts_with("M04_FETAL_LOSS_"),
         M23_CLOSE_DSSTDAT) %>% 
  left_join(mnh06_wide, by = c("MOMID", "PREGID", "SITE")) %>% 
  left_join(mnh08_wide, by = c("MOMID", "PREGID", "SITE")) %>% 
  left_join(mnh04_wide, by = c("MOMID", "PREGID", "SITE"))

#******clean data
#read in the lab variables we are gonna report
lab_var <- read_excel("Lab report variables.xlsx") %>% 
  slice(-1) %>%
  select(Form, `Variable Name`, Value) %>% 
  mutate(cat = ifelse(!is.na(Value), 1, 0))

#list of variables that are numeric
var_list_num <- lab_var %>% 
  filter(cat == 0) %>% 
  select(`Variable Name`) 

#list of variables that are categorical
var_list_cat <- lab_var %>% 
  filter(cat == 1) %>% 
  select(`Variable Name`)

#prepare data by replace default value with NA for numeric var and date
prep_lab_num <- df_maternal %>% 
  select(SITE, MOMID, PREGID,  
         all_of(as.vector(contains(var_list_num$`Variable Name`)))) %>%
  #replace 7s and 5s with NA
  replace_with_na_all(condition = ~.< 0) 

#prepare data by replace default value with NA for categorical var
  prep_lab_cat <- df_maternal %>% 
    select(SITE, MOMID, PREGID,
           starts_with("M04_FETAL_LOSS_DSDECOD_"),
           starts_with("M06_TYPE_VISIT_"),
           starts_with("M06_MAT_VISIT_MNH06_"),
           starts_with("M08_TYPE_VISIT_"),
           starts_with("M08_MAT_VISIT_MNH08_"),
           all_of(as.vector(contains(var_list_cat$`Variable Name`))),
           #77 is an option for M08_TB_BACKUP_LBORRES, not default value, but still replace 77 with NA since it's not real value
           # -starts_with("M08_TB_BACKUP_LBORRES") 
    ) %>%
    #replace 7s and 5s with NA
    replace_with_na_all(condition = ~. %in% c(55,77))

#prepare data by replace default value with NA for date var
prep_lab_date <- df_maternal %>% 
  select(SITE, MOMID, PREGID,
         M02_SCRN_OBSSTDAT,
         starts_with("M04_FETAL_LOSS_DSSTDAT_"),
         starts_with("M06_DIAG_VSDAT_"),
         starts_with("M08_LBSTDAT")) %>%
  #replace 7s and 5s with NA
  replace_with_na_all(condition = ~. %in% c("1907-07-07", "1905-05-05")) 

#prepare data by including basic variables
pre_lab_other <- df_maternal %>% 
  select(SITE, MOMID, PREGID, EST_CONCEP_DATE, BOE_GA_DAYS_ENROLL, DOB, M23_CLOSE_DSSTDAT)

#*!!!!!! temp solution if certain visits has no data --> keep observing to see if we are gonna have any
if(exists("prep_lab_cat$M08_TYPE_VISIT_12") == FALSE) {prep_lab_cat <- prep_lab_cat %>% mutate(M08_TYPE_VISIT_12 = NA)}
if(exists("prep_lab_cat$M08_MAT_VISIT_MNH08_12") == FALSE) {prep_lab_cat <- prep_lab_cat %>% mutate(M08_MAT_VISIT_MNH08_12 = NA)}

#******prepare df_lab --> data for missingness
df_lab <- prep_lab_num %>% 
  left_join(prep_lab_cat, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(prep_lab_date, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(pre_lab_other, by = c("SITE", "MOMID", "PREGID")) %>% 
  #define anc & pnc window variables (code from monitoring report by Stacie)
  mutate(EDD_BOE = EST_CONCEP_DATE + 280) %>%
  mutate(BOE_GA_WKS_ENROLL = BOE_GA_DAYS_ENROLL %/% 7) %>% 
  mutate(GESTAGE_AT_BIRTH_DAYS = as.numeric(DOB-EST_CONCEP_DATE)) %>% 
  mutate(ENROLL_LATE = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(139, unit="days"),
         ANC20_LATE = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(181, unit="days"),
         ANC28_LATE = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(216, unit="days"),
         ANC32_LATE = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(237, unit="days"), 
         ANC36_LATE = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(272, unit="days")) %>% 
  mutate(ENROLL_PASS_LATE = ifelse(ENROLL_LATE < UploadDate, 1, 0),
         ANC20_PASS_LATE = ifelse(ANC20_LATE < UploadDate & BOE_GA_WKS_ENROLL <= 17, 1, 0),
         ANC28_PASS_LATE = ifelse(ANC28_LATE < UploadDate, 1, 0),
         ANC32_PASS_LATE = ifelse(ANC32_LATE < UploadDate, 1, 0),
         ANC36_PASS_LATE = ifelse(ANC36_LATE < UploadDate, 1, 0)) %>% 
  mutate(PNC0_LATE = DOB + as.difftime(5, unit="days"),
         PNC1_LATE = DOB + as.difftime(14, unit="days"),
         PNC4_LATE = DOB + as.difftime(35, unit="days"),
         PNC6_LATE = DOB + as.difftime(104, unit="days"),
         PNC26_LATE = DOB + as.difftime(279, unit="days"),
         PNC52_LATE = DOB + as.difftime(454, unit="days")) %>%
  mutate(PNC0_PASS_LATE = ifelse(PNC0_LATE < UploadDate, 1, 0),
         PNC1_PASS_LATE = ifelse(PNC1_LATE < UploadDate, 1, 0),
         PNC4_PASS_LATE = ifelse(PNC4_LATE < UploadDate, 1, 0),
         PNC6_PASS_LATE = ifelse(PNC6_LATE < UploadDate, 1, 0),
         PNC26_PASS_LATE = ifelse(PNC26_LATE < UploadDate, 1, 0),
         PNC52_PASS_LATE = ifelse(PNC52_LATE < UploadDate, 1, 0)) %>%
  #define ENDPREG_DAYS
  mutate(MISCARRIAGE = ifelse(M04_FETAL_LOSS_DSDECOD_1== 1 | M04_FETAL_LOSS_DSDECOD_2== 1 |  M04_FETAL_LOSS_DSDECOD_3== 1 |
                                M04_FETAL_LOSS_DSDECOD_4== 1 | M04_FETAL_LOSS_DSDECOD_5== 1, 1, 0)) %>%
  mutate(MISCARRIAGE_DATE = pmin(ymd(M04_FETAL_LOSS_DSSTDAT_1), ymd(M04_FETAL_LOSS_DSSTDAT_2),
                                 ymd(M04_FETAL_LOSS_DSSTDAT_3) , ymd(M04_FETAL_LOSS_DSSTDAT_4), ymd(M04_FETAL_LOSS_DSSTDAT_5), na.rm = TRUE)) %>%
  mutate(ENDPREG_DAYS = ifelse(is.na(MISCARRIAGE), GESTAGE_AT_BIRTH_DAYS,
                               ifelse(MISCARRIAGE==1, MISCARRIAGE_DATE-EST_CONCEP_DATE, GESTAGE_AT_BIRTH_DAYS))) %>%
  mutate(VC_ENROLL_DENOM_LATE = ifelse(ENROLL_PASS_LATE == 1, 1, 0), 
         VC_ANC20_DENOM_LATE = ifelse(ANC20_PASS_LATE == 1 & BOE_GA_WKS_ENROLL <= 17 &
                                        (ENDPREG_DAYS>160 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC20_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),  ## closed out after window or has not yet closed out
         VC_ANC28_DENOM_LATE = ifelse(ANC28_PASS_LATE == 1  &
                                        (ENDPREG_DAYS>216 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC28_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0), 
         VC_ANC32_DENOM_LATE = ifelse(ANC32_PASS_LATE == 1  & 
                                        (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC32_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0), 
         VC_ANC36_DENOM_LATE = ifelse(ANC36_PASS_LATE == 1  & 
                                        (ENDPREG_DAYS>272 | is.na(ENDPREG_DAYS))  & 
                                        ((M23_CLOSE_DSSTDAT > ANC36_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)
  ) %>% 
  mutate(VC_PNC0_DENOM_LATE = ifelse(PNC0_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC0_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC1_DENOM_LATE = ifelse(PNC1_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC1_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC4_DENOM_LATE = ifelse(PNC4_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC4_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC6_DENOM_LATE = ifelse(PNC6_PASS_LATE==1 & 
                                       ((M23_CLOSE_DSSTDAT > PNC6_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         
         VC_PNC26_DENOM_LATE = ifelse(PNC26_PASS_LATE==1 & ENDPREG_DAYS>139 & 
                                        ((M23_CLOSE_DSSTDAT > PNC26_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         
         VC_PNC52_DENOM_LATE = ifelse(PNC52_PASS_LATE==1 & ENDPREG_DAYS>139 & 
                                        ((M23_CLOSE_DSSTDAT > PNC52_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)
  ) %>% 
  #define denominator for form missingness
  mutate(
    denom_form_1 = ifelse(M06_TYPE_VISIT_1 == 1 | M08_TYPE_VISIT_1 == 1 | VC_ENROLL_DENOM_LATE == 1, 1, 0),
    denom_form_2 = ifelse(M06_TYPE_VISIT_2 == 2 | M08_TYPE_VISIT_2 == 2 | VC_ANC20_DENOM_LATE == 1, 1, 0),
    denom_form_3 = ifelse(M06_TYPE_VISIT_3 == 3 | M08_TYPE_VISIT_3 == 3 | VC_ANC28_DENOM_LATE == 1, 1, 0),
    denom_form_4 = ifelse(M06_TYPE_VISIT_4 == 4 | M08_TYPE_VISIT_4 == 4 | VC_ANC32_DENOM_LATE == 1, 1, 0),
    denom_form_5 = ifelse(M06_TYPE_VISIT_5 == 5 | M08_TYPE_VISIT_5 == 5 | VC_ANC36_DENOM_LATE == 1, 1, 0),
    # denom_form_6 = ifelse(M06_TYPE_VISIT_6 == 6 | M08_TYPE_VISIT_6 == 6 | ipc_pass_late == 1, 1, 0),
    denom_form_7 = ifelse(M06_TYPE_VISIT_7 == 7 | M08_TYPE_VISIT_7 == 7 | VC_PNC0_DENOM_LATE == 1, 1, 0),
    denom_form_8 = ifelse(M06_TYPE_VISIT_8 == 8 | M08_TYPE_VISIT_8 == 8 | VC_PNC1_DENOM_LATE == 1, 1, 0),
    denom_form_9 = ifelse(M06_TYPE_VISIT_9 == 9 | M08_TYPE_VISIT_9 == 9 | VC_PNC4_DENOM_LATE == 1, 1, 0),
    denom_form_10 = ifelse(M06_TYPE_VISIT_10 == 10 | M08_TYPE_VISIT_10 == 10 | VC_PNC6_DENOM_LATE == 1, 1, 0),
    denom_form_11 = ifelse(M06_TYPE_VISIT_11 == 11 | M08_TYPE_VISIT_11 == 11 | VC_PNC26_DENOM_LATE == 1, 1, 0),
    denom_form_12 = ifelse(M06_TYPE_VISIT_12 == 12 | M08_TYPE_VISIT_12 == 12 | VC_PNC52_DENOM_LATE == 1, 1, 0),
  ) %>%
  #define denominator for lab missingness in MNH06
  mutate(
    denom_lab_06_1 = ifelse(M06_TYPE_VISIT_1 == 1 & M06_MAT_VISIT_MNH06_1 %in% c(1,2), 1, 0),
    denom_lab_06_2 = ifelse(M06_TYPE_VISIT_2 == 2 & M06_MAT_VISIT_MNH06_2 %in% c(1,2), 1, 0),
    denom_lab_06_3 = ifelse(M06_TYPE_VISIT_3 == 3 & M06_MAT_VISIT_MNH06_3 %in% c(1,2), 1, 0),
    denom_lab_06_4 = ifelse(M06_TYPE_VISIT_4 == 4 & M06_MAT_VISIT_MNH06_4 %in% c(1,2), 1, 0),
    denom_lab_06_5 = ifelse(M06_TYPE_VISIT_5 == 5 & M06_MAT_VISIT_MNH06_5 %in% c(1,2), 1, 0),
    denom_lab_06_6 = ifelse(M06_TYPE_VISIT_6 == 6 & M06_MAT_VISIT_MNH06_6 %in% c(1,2), 1, 0),
    denom_lab_06_7 = ifelse(M06_TYPE_VISIT_7 == 7 & M06_MAT_VISIT_MNH06_7 %in% c(1,2), 1, 0),
    denom_lab_06_8 = ifelse(M06_TYPE_VISIT_8 == 8 & M06_MAT_VISIT_MNH06_8 %in% c(1,2), 1, 0),
    denom_lab_06_9 = ifelse(M06_TYPE_VISIT_9 == 9 & M06_MAT_VISIT_MNH06_9 %in% c(1,2), 1, 0),
    denom_lab_06_10 = ifelse(M06_TYPE_VISIT_10 == 10 & M06_MAT_VISIT_MNH06_10 %in% c(1,2), 1, 0),
    denom_lab_06_11 = ifelse(M06_TYPE_VISIT_11 == 11 & M06_MAT_VISIT_MNH06_11 %in% c(1,2), 1, 0),
    denom_lab_06_12 = ifelse(M06_TYPE_VISIT_12 == 12 & M06_MAT_VISIT_MNH06_12 %in% c(1,2), 1, 0),
  ) %>%
  #define denominator for lab missingness in MNH08
  mutate(
    denom_lab_08_1 = ifelse(M08_TYPE_VISIT_1 == 1 & M08_MAT_VISIT_MNH08_1 %in% c(1,2), 1, 0),
    denom_lab_08_2 = ifelse(M08_TYPE_VISIT_2 == 2 & M08_MAT_VISIT_MNH08_2 %in% c(1,2), 1, 0),
    denom_lab_08_3 = ifelse(M08_TYPE_VISIT_3 == 3 & M08_MAT_VISIT_MNH08_3 %in% c(1,2), 1, 0),
    denom_lab_08_4 = ifelse(M08_TYPE_VISIT_4 == 4 & M08_MAT_VISIT_MNH08_4 %in% c(1,2), 1, 0),
    denom_lab_08_5 = ifelse(M08_TYPE_VISIT_5 == 5 & M08_MAT_VISIT_MNH08_5 %in% c(1,2), 1, 0),
    denom_lab_08_6 = ifelse(M08_TYPE_VISIT_6 == 6 & M08_MAT_VISIT_MNH08_6 %in% c(1,2), 1, 0),
    denom_lab_08_7 = ifelse(M08_TYPE_VISIT_7 == 7 & M08_MAT_VISIT_MNH08_7 %in% c(1,2), 1, 0),
    denom_lab_08_8 = ifelse(M08_TYPE_VISIT_8 == 8 & M08_MAT_VISIT_MNH08_8 %in% c(1,2), 1, 0),
    denom_lab_08_9 = ifelse(M08_TYPE_VISIT_9 == 9 & M08_MAT_VISIT_MNH08_9 %in% c(1,2), 1, 0),
    denom_lab_08_10 = ifelse(M08_TYPE_VISIT_10 == 10 & M08_MAT_VISIT_MNH08_10 %in% c(1,2), 1, 0),
    denom_lab_08_11 = ifelse(M08_TYPE_VISIT_11 == 11 & M08_MAT_VISIT_MNH08_11 %in% c(1,2), 1, 0),
    denom_lab_08_12 = ifelse(M08_TYPE_VISIT_12 == 12 & M08_MAT_VISIT_MNH08_12 %in% c(1,2), 1, 0),
  ) %>%
  #define denominator for ReMAPP exclusivie labs (RBC at enrollment)
  mutate(
    denom_remapp = case_when(
      denom_lab_08_1 == 1 & 
        ((SITE == "Ghana" & M02_SCRN_OBSSTDAT >= "2022-12-28") |
           (SITE == "Kenya" & M02_SCRN_OBSSTDAT >= "2023-04-14") |
           (SITE == "Zambia" & M02_SCRN_OBSSTDAT >= "2022-12-15") |
           (SITE == "Pakistan" & M02_SCRN_OBSSTDAT >= "2022-09-22") |
           (SITE == "India-CMC" & M02_SCRN_OBSSTDAT >= "2023-06-20") |
           (SITE == "India-SAS" & M02_SCRN_OBSSTDAT >= "2023-08-15")) ~ 1,
      TRUE ~ 0
    )
  ) 

#change data to long format for mnh06
df_lab_06 <- df_lab %>% 
  select(-matches("PASS_LATE"), -starts_with("M08_")) %>%
  pivot_longer(
    -c("MOMID","PREGID","SITE",  EST_CONCEP_DATE),
    names_to = c(".value", "visit_type"), 
    names_pattern = "^M\\d{2}_(.+)_(\\d+)"
  ) %>% 
  mutate(
    ga_wks = case_when(
      TYPE_VISIT >= 6 ~ NA_real_,
      TYPE_VISIT < 6 ~ as.numeric(ymd(DIAG_VSDAT) - EST_CONCEP_DATE)/7
    ),
    trimester = case_when(
      #at/after delivery data should be -5 for trimester
      ga_wks > 3 & ga_wks < 14 ~ 1,
      ga_wks >= 14 & ga_wks < 27 ~ 2,
      ga_wks >= 27 & ga_wks < 43 ~ 3, 
      TRUE ~ NA_real_
    ), 
    time = case_when(
      TYPE_VISIT < 6 ~ ga_wks,
      TYPE_VISIT == 6 ~ 60, 
      TYPE_VISIT == 7 ~ 61, 
      TYPE_VISIT == 8 ~ 62, 
      TYPE_VISIT == 9 ~ 63, 
      TYPE_VISIT == 10 ~ 64, 
      TYPE_VISIT == 11 ~ 65, 
      TYPE_VISIT == 12 ~ 66, 
    )
  ) %>% 
  filter(time > 0) #remove date errors which should be flagged in query report

#change data to long format for mnh08
df_lab_08 <- df_lab %>% 
  select(-matches("PASS_LATE"), -starts_with("M06_")) %>%
  pivot_longer(
    -c("MOMID","PREGID","SITE",  EST_CONCEP_DATE),
    names_to = c(".value", "visit_type"), 
    names_pattern = "^M\\d{2}_(.+)_(\\d+)"
  ) %>% 
  mutate(
    ga_wks = case_when(
      TYPE_VISIT >= 6 ~ NA_real_,
      TYPE_VISIT < 6 ~ as.numeric(ymd(LBSTDAT) - EST_CONCEP_DATE)/7
    ),
    trimester = case_when(
      #at/after delivery data should be -5 for trimester
      ga_wks > 3 & ga_wks < 14 ~ 1,
      ga_wks >= 14 & ga_wks < 27 ~ 2,
      ga_wks >= 27 & ga_wks < 43 ~ 3, 
      TRUE ~ NA_real_
    ), 
    time = case_when(
      TYPE_VISIT < 6 ~ ga_wks,
      TYPE_VISIT == 6 ~ 60, 
      TYPE_VISIT == 7 ~ 61, 
      TYPE_VISIT == 8 ~ 62, 
      TYPE_VISIT == 9 ~ 63, 
      TYPE_VISIT == 10 ~ 64, 
      TYPE_VISIT == 11 ~ 65, 
      TYPE_VISIT == 12 ~ 66, 
    ),
    #unit conversion
    albumin = case_when(
      SITE == "Kenya" ~ ALBUMIN_LBORRES/10,
      SITE != "Kenya" ~ ALBUMIN_LBORRES
    ),
    #Hematocrit (HCT): %
    hct_level = case_when(
      trimester == 1 & CBC_HCT_LBORRES < 31 ~ 1,  
      trimester == 1 & CBC_HCT_LBORRES >= 31 & CBC_HCT_LBORRES <= 41 ~ 2, 
      trimester == 1 & CBC_HCT_LBORRES > 41 ~ 3, 
      trimester == 2 & CBC_HCT_LBORRES < 30 ~ 1, 
      trimester == 2 & CBC_HCT_LBORRES >= 30 & CBC_HCT_LBORRES <= 39 ~ 2,
      trimester == 2 & CBC_HCT_LBORRES > 39 ~ 3, 
      trimester == 3 & CBC_HCT_LBORRES < 28 ~ 1, 
      trimester == 3 & CBC_HCT_LBORRES >= 28 & CBC_HCT_LBORRES <= 40 ~ 2,
      trimester == 3 & CBC_HCT_LBORRES > 40  ~ 3, 
      TRUE ~ NA_real_
    ),
    #White Blood Cell (WBC): x10³/mm³
    wbc_level = case_when(
      trimester == 1 & CBC_WBC_LBORRES < 5.7 ~ 1, 
      trimester == 1 & CBC_WBC_LBORRES >= 5.7 & CBC_WBC_LBORRES <= 13.6 ~ 2, 
      trimester == 1 & CBC_WBC_LBORRES > 13.6 ~ 3, 
      trimester == 2 & CBC_WBC_LBORRES < 5.6 ~ 1, 
      trimester == 2 & CBC_WBC_LBORRES >= 5.6 & CBC_WBC_LBORRES <= 14.8 ~ 2,
      trimester == 2 & CBC_WBC_LBORRES > 14.8 ~ 3, 
      trimester == 3 & CBC_WBC_LBORRES < 5.6 ~ 1, 
      trimester == 3 & CBC_WBC_LBORRES >= 5.6 & CBC_WBC_LBORRES <= 16.9 ~ 2,
      trimester == 3 & CBC_WBC_LBORRES > 16.9 ~ 3, 
      TRUE ~ NA_real_
    ),
    #Neutrophils (full cell count)	x10³/mm³
    neutrophils_level = case_when(
      trimester == 1 & CBC_NEU_FCC_LBORRES < 3.6 ~ 1, 
      trimester == 1 & CBC_NEU_FCC_LBORRES >= 3.6 & CBC_NEU_FCC_LBORRES <= 10.1 ~ 2, 
      trimester == 1 & CBC_NEU_FCC_LBORRES > 10.1 ~ 3, 
      trimester == 2 & CBC_NEU_FCC_LBORRES < 3.8 ~ 1, 
      trimester == 2 & CBC_NEU_FCC_LBORRES >= 3.8 & CBC_NEU_FCC_LBORRES <= 12.3 ~ 2,
      trimester == 2 & CBC_NEU_FCC_LBORRES > 12.3 ~ 3, 
      trimester == 3 & CBC_NEU_FCC_LBORRES < 3.9 ~ 1,
      trimester == 3 & CBC_NEU_FCC_LBORRES >= 3.9 & CBC_NEU_FCC_LBORRES <= 13.1 ~ 2,
      trimester == 3 & CBC_NEU_FCC_LBORRES > 13.1 ~ 3, 
      TRUE ~ NA_real_
    ),
    #Lymphocyte (full cell count)	x10³/mm³
    lymphocyte_level = case_when(
      trimester == 1 & CBC_LYMPH_FCC_LBORRES < 1.1 ~ 1, 
      trimester == 1 & CBC_LYMPH_FCC_LBORRES >= 1.1 & CBC_LYMPH_FCC_LBORRES <= 3.6 ~ 2, 
      trimester == 1 & CBC_LYMPH_FCC_LBORRES > 3.6 ~ 3, 
      trimester == 2 & CBC_LYMPH_FCC_LBORRES < 0.9 ~ 1,
      trimester == 2 & CBC_LYMPH_FCC_LBORRES >= 0.9 & CBC_LYMPH_FCC_LBORRES <= 3.9 ~ 2,
      trimester == 2 & CBC_LYMPH_FCC_LBORRES > 3.9 ~ 3, 
      trimester == 3 & CBC_LYMPH_FCC_LBORRES < 1 ~ 1,
      trimester == 3 & CBC_LYMPH_FCC_LBORRES >= 1 & CBC_LYMPH_FCC_LBORRES <= 3.6 ~ 2,
      trimester == 3 & CBC_LYMPH_FCC_LBORRES > 3.6 ~ 3, 
      TRUE ~ NA_real_
    ),
    #Mean cell volume (MCV): unit: µm³
    mcv_level = case_when(
      trimester == 1 & CBC_MCV_LBORRES < 85 ~ 1, 
      trimester == 1 & CBC_MCV_LBORRES >= 85 & CBC_MCV_LBORRES <= 97.9 ~ 2, 
      trimester == 1 & CBC_MCV_LBORRES > 97.9 ~ 3, 
      trimester == 2 & CBC_MCV_LBORRES < 85.8 ~ 1, 
      trimester == 2 & CBC_MCV_LBORRES >= 85.8 & CBC_MCV_LBORRES <= 99.4 ~ 2,
      trimester == 2 & CBC_MCV_LBORRES > 99.4 ~ 3, 
      trimester == 3 & CBC_MCV_LBORRES < 82.4 ~ 1, 
      trimester == 3 & CBC_MCV_LBORRES >= 82.4 & CBC_MCV_LBORRES <= 100.4 ~ 2,
      trimester == 3 & CBC_MCV_LBORRES > 100.4 ~ 3,
      TRUE ~ NA_real_
    ), 
    #Mean cell hemoglobin (MCH)	pg/cell
    mch_level = case_when(
      trimester == 1 & CBC_MCH_LBORRES < 30 ~ 1, 
      trimester == 1 & CBC_MCH_LBORRES >= 30 & CBC_MCH_LBORRES <= 32 ~ 2, 
      trimester == 1 & CBC_MCH_LBORRES > 32 ~ 3, 
      trimester == 2 & CBC_MCH_LBORRES < 30 ~ 1, 
      trimester == 2 & CBC_MCH_LBORRES >= 30 & CBC_MCH_LBORRES <= 33 ~ 2,
      trimester == 2 & CBC_MCH_LBORRES > 33 ~ 3,
      trimester == 3 & CBC_MCH_LBORRES < 29 ~ 1, 
      trimester == 3 & CBC_MCH_LBORRES >= 29 & CBC_MCH_LBORRES <= 32 ~ 2,
      trimester == 3 & CBC_MCH_LBORRES > 32 ~ 3,
      TRUE ~ NA_real_
    ), 
    #Mean corpuscular hemoglobin concentration (MCHC)	g/dL
    mchc_level = case_when(
      trimester == 1 & CBC_MCHC_GDL_LBORRES < 32.5 ~ 1, 
      trimester == 1 & CBC_MCHC_GDL_LBORRES >= 32.5 & CBC_MCHC_GDL_LBORRES <= 35.3 ~ 2, 
      trimester == 1 & CBC_MCHC_GDL_LBORRES > 35.3 ~ 3, 
      trimester == 2 & CBC_MCHC_GDL_LBORRES < 32.4 ~ 1, 
      trimester == 2 & CBC_MCHC_GDL_LBORRES >= 32.4 & CBC_MCHC_GDL_LBORRES <= 35.2 ~ 2,
      trimester == 2 & CBC_MCHC_GDL_LBORRES > 35.2 ~ 3,
      trimester == 3 & CBC_MCHC_GDL_LBORRES < 31.9 ~ 1, 
      trimester == 3 & CBC_MCHC_GDL_LBORRES >= 31.9 & CBC_MCHC_GDL_LBORRES <= 35.5 ~ 2,
      trimester == 3 & CBC_MCHC_GDL_LBORRES > 35.5 ~ 3,
      TRUE ~ NA_real_
    ), 
    #Platelets count unit: x10³/mm³
    platelets_level = case_when(
      trimester == 1 & CBC_PLATE_LBORRES < 174 ~ 1, 
      trimester == 1 & CBC_PLATE_LBORRES >= 174 & CBC_PLATE_LBORRES <= 391 ~ 2, 
      trimester == 1 & CBC_PLATE_LBORRES > 391 ~ 3, 
      trimester == 2 & CBC_PLATE_LBORRES < 155 ~ 1,
      trimester == 2 & CBC_PLATE_LBORRES >= 155 & CBC_PLATE_LBORRES <= 409 ~ 2,
      trimester == 2 & CBC_PLATE_LBORRES > 409 ~ 3,
      trimester == 3 & CBC_PLATE_LBORRES < 146 ~ 1,
      trimester == 3 & CBC_PLATE_LBORRES >= 146 & CBC_PLATE_LBORRES <= 429 ~ 2,
      trimester == 3 & CBC_PLATE_LBORRES > 429 ~ 3,
      TRUE ~ NA_real_
    ),
    #Monocyte (full cell count)	x10³/mm³
    monocyte_level = case_when(
      trimester == 1 & CBC_MONO_FCC_LBORRES < 0.1 ~ 1, 
      trimester == 1 & CBC_MONO_FCC_LBORRES >= 0.1 & CBC_MONO_FCC_LBORRES <= 1.1 ~ 2, 
      trimester == 1 & CBC_MONO_FCC_LBORRES > 1.1 ~ 3, 
      trimester == 2 & CBC_MONO_FCC_LBORRES < 0.1 ~ 1,
      trimester == 2 & CBC_MONO_FCC_LBORRES >= 0.1 & CBC_MONO_FCC_LBORRES <= 1.1 ~ 2,
      trimester == 2 & CBC_MONO_FCC_LBORRES > 1.1 ~ 3,
      trimester == 3 & CBC_MONO_FCC_LBORRES < 0.1 ~ 1,
      trimester == 3 & CBC_MONO_FCC_LBORRES >= 0.1 & CBC_MONO_FCC_LBORRES <= 1.4 ~ 2,
      trimester == 3 & CBC_MONO_FCC_LBORRES > 1.4 ~ 3,
      TRUE ~ NA_real_
    ),
    #Eosinophils (full cell count)	x10³/mm³
    eosinophils_level = case_when(
      trimester == 1 & CBC_EOS_FCC_LBORRES < 0 ~ 1, 
      trimester == 1 & CBC_EOS_FCC_LBORRES >= 0 & CBC_EOS_FCC_LBORRES <= 0.6 ~ 2, 
      trimester == 1 & CBC_EOS_FCC_LBORRES > 0.6 ~ 3, 
      trimester == 2 & CBC_EOS_FCC_LBORRES < 0 ~ 1,
      trimester == 2 & CBC_EOS_FCC_LBORRES >= 0 & CBC_EOS_FCC_LBORRES <= 0.6 ~ 2,
      trimester == 2 & CBC_EOS_FCC_LBORRES > 0.6 ~ 3,
      trimester == 3 & CBC_EOS_FCC_LBORRES < 0 ~ 1,
      trimester == 3 & CBC_EOS_FCC_LBORRES >= 0 & CBC_EOS_FCC_LBORRES <= 0.6 ~ 2,
      trimester == 3 & CBC_EOS_FCC_LBORRES > 0.6 ~ 3,
      TRUE ~ NA_real_
    ),
    #Red cell width (RDW)	%
    rdw_level = case_when(
      trimester == 1 & CBC_RDW_PCT_LBORRES < 11.7 ~ 1, 
      trimester == 1 & CBC_RDW_PCT_LBORRES >= 11.7 & CBC_RDW_PCT_LBORRES <= 14.9 ~ 2, 
      trimester == 1 & CBC_RDW_PCT_LBORRES > 14.9 ~ 3, 
      trimester == 2 & CBC_RDW_PCT_LBORRES < 12.3 ~ 1,
      trimester == 2 & CBC_RDW_PCT_LBORRES >= 12.3 & CBC_RDW_PCT_LBORRES <= 14.7 ~ 2,
      trimester == 2 & CBC_RDW_PCT_LBORRES > 14.7 ~ 3,
      trimester == 3 & CBC_RDW_PCT_LBORRES < 11.4 ~ 1,
      trimester == 3 & CBC_RDW_PCT_LBORRES >= 11.4 & CBC_RDW_PCT_LBORRES <= 16.6 ~ 2,
      trimester == 3 & CBC_RDW_PCT_LBORRES > 16.6 ~ 3,
      TRUE ~ NA_real_
    ),
    #Ferritin µg/dL
    ferritin_level = case_when(
      trimester == 1 & FERRITIN_LBORRES < 0.6 ~ 1, 
      trimester == 1 & FERRITIN_LBORRES >= 0.6 & FERRITIN_LBORRES <= 12.5 ~ 2, 
      trimester == 1 & FERRITIN_LBORRES > 12.5 ~ 3,  
      trimester == 2 & FERRITIN_LBORRES < 0.6 ~ 1,
      trimester == 2 & FERRITIN_LBORRES >= 0.6 & FERRITIN_LBORRES <= 7.4 ~ 2,
      trimester == 2 & FERRITIN_LBORRES > 7.4 ~ 3,
      trimester == 3 & FERRITIN_LBORRES < 0.3 ~ 1,
      trimester == 3 & FERRITIN_LBORRES >= 0.3 & FERRITIN_LBORRES <= 5.8 ~ 2,
      trimester == 3 & FERRITIN_LBORRES > 5.8 ~ 3,
      TRUE ~ NA_real_
    ),
    #Serum B12 total cobalamin pg/mL
    b12tc_level = case_when(
      trimester == 1 & VITB12_COB_LBORRES < 118 ~ 1, 
      trimester == 1 & VITB12_COB_LBORRES >= 118 & VITB12_COB_LBORRES <= 438 ~ 2, 
      trimester == 1 & VITB12_COB_LBORRES > 438 ~ 3, 
      trimester == 2 & VITB12_COB_LBORRES < 130 ~ 1,
      trimester == 2 & VITB12_COB_LBORRES >= 130 & VITB12_COB_LBORRES <= 656 ~ 2,
      trimester == 2 & VITB12_COB_LBORRES > 656 ~ 3,
      trimester == 3 & VITB12_COB_LBORRES < 99 ~ 1,
      trimester == 3 & VITB12_COB_LBORRES >= 99 & VITB12_COB_LBORRES <= 526 ~ 2,
      trimester == 3 & VITB12_COB_LBORRES > 526 ~ 3,
      TRUE ~ NA_real_
    ),
    #Free T4  ng/dL
    freet4_level = case_when(
      trimester == 1 & THYROID_FREET4_LBORRES < 0.55 ~ 1, 
      trimester == 1 & THYROID_FREET4_LBORRES >= 0.55 & THYROID_FREET4_LBORRES <= 1.37 ~ 2, 
      trimester == 1 & THYROID_FREET4_LBORRES > 1.37 ~ 3,  
      trimester == 2 & THYROID_FREET4_LBORRES < 0.55 ~ 1,
      trimester == 2 & THYROID_FREET4_LBORRES >= 0.55 & THYROID_FREET4_LBORRES <= 1.09 ~ 2,
      trimester == 2 & THYROID_FREET4_LBORRES > 1.09 ~ 3,
      trimester == 3 & THYROID_FREET4_LBORRES < 0.55 ~ 1,
      trimester == 3 & THYROID_FREET4_LBORRES >= 0.55 & THYROID_FREET4_LBORRES <= 1.09 ~ 2,
      trimester == 3 & THYROID_FREET4_LBORRES > 1.09 ~ 3,
      TRUE ~ NA_real_
    ),
  ) %>% 
  filter(time > 0) #remove date errors which should be flagged in query report

#******Prepare df_lab_06_l --> data for visualization for labs in MNH06
df_lab_06_l <- df_lab_06 %>% 
  mutate(across(ends_with("_level"),
                function(x) 
                  factor(x, 
                         levels = c(1,2,3),
                         labels = c("Low", "Normal", "High")
                  ))) %>%
  #convert categorical lab results to factor
  mutate(across(c("MALARIA_POC_LBORRES", "HIV_POC_LBORRES", "SYPH_POC_LBORRES", 
                  "HBV_POC_LBORRES", "HCV_POC_LBORRES", "COVID_POC_LBORRES"),
                function(x) 
                  factor(x, 
                         levels = c(0,1),
                         labels = c("Negative", "Positive")
                  ))) %>%
  filter(time < 70) #remove outlier 

#******Prepare df_lab_06_l --> data for visualization for labs in MNH08
df_lab_08_l <- df_lab_08 %>%
  #convert level variables to factor
  mutate(across(ends_with("_level"),
                function(x) 
                  factor(x, 
                         levels = c(1,2,3),
                         labels = c("Low", "Normal", "High")
                  ))) %>%
  #convert categorical lab results to factor
  mutate(across(c("RH_FACTOR_LBORRES", "SYPH_TITER_LBORRES"),
                function(x) 
                  factor(x, 
                         levels = c(0,1),
                         labels = c("Negative", "Positive")
                  ))) %>%
  #convert categorical lab results to factor
  mutate(across(c("RBC_SICKLE_LBORRES"),
                function(x) 
                  factor(x, 
                         levels = c(0,1),
                         labels = c("Normal, disease absent", "Sickle cell disease present")
                  ))) %>%
  #convert categorical lab results to factor
  mutate(across(c("RBC_THALA_LBORRES"),
                function(x) 
                  factor(x, 
                         levels = c(0,1),
                         labels = c("Normal, disease absent", "Hemoglobinopathy or Thalassemia")
                  ))) %>%
  #convert other categorical lab results to factor
  mutate(across(c("BLD_GRP_LBORRES"),
                function(x) 
                  factor(x, 
                         levels = c(4,3,2,1),
                         labels = c("O","AB","B","A")
                  ))) %>%
  #convert other categorical lab results to factor
  mutate(across(c("UA_PROT_LBORRES"),
                function(x) 
                  factor(x, 
                         levels = c(0,1,2,3,4,5),
                         labels = c("None(negative)", "Trace", "1+(0.3 g/L)", "2+(1 g/L)", "3+(3 g/L)", "4+(>10 g/L)")
                  ))) %>%
  #convert other categorical lab results to factor
  mutate(across(c("UA_LEUK_LBORRES"),
                function(x) 
                  factor(x, 
                         levels = c(0,1,2,3,4),
                         labels = c("None(negative)", "Trace", "1+", "2+", "3+")
                  ))) %>%
  #convert other categorical lab results to factor
  mutate(across(c("UA_NITRITE_LBORRES"),
                function(x) 
                  factor(x, 
                         levels = c(0,1),
                         labels = c("None(negative)", "Present(postive)")
                  ))) %>%
  #convert other categorical lab results to factor
  mutate(across(c("TB_CNFRM_LBORRES", "TB_BACKUP_LBORRES", "CTNG_CT_LBORRES", "CTNG_NG_LBORRES",
                  "ZCD_ZIKIGM_LBORRES", "ZCD_ZIKIGG_LBORRES", "ZCD_DENIGM_LBORRES", "ZCD_DENIGG_LBORRES",
                  "ZCD_CHKIGM_LBORRES", "ZCD_CHKIGG_LBORRES", 
                  "LEPT_IGM_LBORRES", "LEPT_IGG_LBORRES", "HEV_IGM_LBORRES", "HEV_IGG_LBORRES"),
                function(x) 
                  factor(x, 
                         levels = c(0,2,1),
                         labels = c("Negative", "Inconclusive", "Positive")
                  ))) %>%
  filter(time < 70) #remove outlier 

#save data
save(df_maternal, file = "derived_data/df_maternal.rda")
save(df_lab, file = "derived_data/df_lab.rda")
save(df_lab_06_l, file = "derived_data/df_lab_06_l.rda")
save(df_lab_08_l, file = "derived_data/df_lab_08_l.rda")

