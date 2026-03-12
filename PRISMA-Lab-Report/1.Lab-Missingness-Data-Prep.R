#*****************************************************************************
#*Lab report 
#*Includes: 
#*1. Lab missingess table
#*2. Data visualization of lab results
#*Author: Xiaoyan Hu & Stacie Loisate
#*Email:xyh@gwu.edu
#*****************************************************************************
library(tidyverse)
library(lubridate)
library(readxl)
library(haven)

#Site data upload date
UploadDate = "2026-03-06"
#*****************************************************************************
#* 1.Data preparation for lab missingness
#*****************************************************************************
# set path to data
path_to_data <- paste0("~/import/", UploadDate)

# set path to save 
path_to_save <- paste0("D:/Users/stacie.loisate/Documents/PRISMA-Analysis-Stacie/Lab-Missingness/data")

#******load data
#load mnh06 data
mnh06 <- read.csv(paste0(path_to_data,"/mnh06_merged.csv")) 

#load mnh08 data
mnh08 <- read.csv(paste0(path_to_data,"/mnh08_merged.csv"))

#load mnh04 data for some lab value denominator
mnh04 <- read.csv(paste0(path_to_data,"/mnh04_merged.csv"))

#load mnh07 data for some lab value denominator
mnh07 <- read.csv(paste0(path_to_data,"/mnh07_merged.csv")) 

#load mnh09 data for DOB
mnh09 <- read.csv(paste0(path_to_data,"/mnh09_merged.csv")) 

#load MAT_ENROLL 
MAT_ENROLL <- read_xlsx(paste0("Z:/Outcome Data/",UploadDate,"/MAT_ENROLL.xlsx"))

#load MAT_ENDPOINT 
MAT_ENDPOINTS <- read_dta(paste0("Z:/Outcome Data/",UploadDate,"/MAT_ENDPOINTS.dta"))

#load MAT_GDM 
MAT_GDM <- read_dta(paste0("Z:/Outcome Data/",UploadDate,"/MAT_GDM.dta"))

sas_expansion_ids <- read_xlsx(paste0("Z:/PRISMA_Data_Uploads/India-SAS_Expansion_IDs-2025-11-21.xlsx"))

## import raw mnh08 from kenya (adjust zcd/lepto/hev testing dates)
mnh08_ke <- read.csv(paste0("~/import/", UploadDate, "_ke/mnh08.csv"))

mnh08_ke <- mnh08_ke %>% 
  mutate(SITE = "Kenya") %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, LBSTDAT,
         ZCD_LBTSTDAT, LEPT_IGG_LBTSTDAT, HEV_LBTSTDAT, LEPT_IGM_LBTSTDAT) %>% 
  rename(M08_LBSTDAT = LBSTDAT,
         M08_TYPE_VISIT = TYPE_VISIT,
         M08_ZCD_LBTSTDAT = ZCD_LBTSTDAT, 
         M08_LEPT_IGG_LBTSTDAT = LEPT_IGG_LBTSTDAT, 
         M08_HEV_LBTSTDAT = HEV_LBTSTDAT, 
         M08_LEPT_IGM_LBTSTDAT = LEPT_IGM_LBTSTDAT
         ) %>% 
  mutate(M08_ZCD_LBTSTDAT = ymd(parse_date_time(M08_ZCD_LBTSTDAT, "%d/%b/%y")),
         M08_LEPT_IGG_LBTSTDAT = ymd(parse_date_time(M08_LEPT_IGG_LBTSTDAT, "%d/%b/%y")),
         M08_HEV_LBTSTDAT = ymd(parse_date_time(M08_HEV_LBTSTDAT, "%d/%b/%y")),
         M08_LEPT_IGM_LBTSTDAT = ymd(parse_date_time(M08_LEPT_IGM_LBTSTDAT, "%d/%b/%y"))
  ) %>% 
  group_by(SITE, MOMID, PREGID, M08_TYPE_VISIT) %>%
  # if a duplicate exists, take the first instance (sorting by date)
  arrange(-desc(M08_LBSTDAT)) %>% 
  slice(1) %>% 
  select(-M08_LBSTDAT, -M08_TYPE_VISIT)

mnh08_no_ke <- mnh08 %>% filter(SITE != "Kenya")

mnh08_ke_full <- mnh08 %>% filter(SITE == "Kenya") %>% 
  select(-M08_ZCD_LBTSTDAT, -M08_LEPT_IGG_LBTSTDAT, -M08_HEV_LBTSTDAT, -M08_LEPT_IGM_LBTSTDAT) %>% 
  left_join(mnh08_ke, by = c("SITE", "MOMID", "PREGID", "M08_TYPE_VISIT")) %>% 
  mutate(M08_ZCD_LBTSTDAT = as.character(M08_ZCD_LBTSTDAT), 
         M08_LEPT_IGG_LBTSTDAT = as.character(M08_LEPT_IGG_LBTSTDAT), 
         M08_HEV_LBTSTDAT = as.character(M08_HEV_LBTSTDAT), 
         M08_LEPT_IGM_LBTSTDAT = as.character(M08_LEPT_IGM_LBTSTDAT)
  ) %>% 
  group_by(SITE, MOMID, PREGID, M08_TYPE_VISIT) %>%
  # if a duplicate exists, take the first instance (sorting by date)
  arrange(-desc(M08_LBSTDAT)) %>% 
  slice(1)

mnh08 <- bind_rows(mnh08_ke_full, mnh08_no_ke)

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

#extract unique mnh07 and make wide data
mnh07_uni <- mnh07 %>% 
  group_by(SITE, MOMID, PREGID, M07_TYPE_VISIT) %>% 
  mutate(n = n()) %>% 
  filter(n == 1, M07_TYPE_VISIT %in% c(1:12)) 

mnh07_wide <- mnh07_uni %>%
  select(SITE, MOMID, PREGID,M07_TYPE_VISIT, M07_MAT_VISIT_MNH07, M07_MAT_SPEC_COLLECT_DAT, M07_MAT_TB_SPEC_COLLECT) %>% 
  pivot_wider(
    names_from = M07_TYPE_VISIT,
    values_from = -c("MOMID", "PREGID", "SITE")
  ) 

#extract unique mnh08 and wide data
mnh08_uni <- mnh08 %>% 
  group_by(SITE, MOMID, PREGID, M08_TYPE_VISIT) %>% 
  mutate(n = n()) %>% 
  filter(n == 1 & M08_TYPE_VISIT %in% c(1:12))

mnh08_wide <- mnh08_uni %>% 
  pivot_wider(
    names_from = M08_TYPE_VISIT,
    values_from = -c("MOMID", "PREGID", "SITE")
  ) %>% 
  select(-starts_with("n_"))

#extract unique mnh04 and wide data for variables needed 
mnh04_uni <- mnh04 %>% 
  group_by(SITE, MOMID, PREGID, M04_TYPE_VISIT) %>% 
  mutate(n = n()) %>% 
  filter(n == 1, M04_TYPE_VISIT %in% c(1:12))

mnh04_wide <- mnh04_uni %>% 
  select(MOMID, PREGID, SITE, M04_TYPE_VISIT, M04_TB_MHOCCUR, num_range("M04_TB_CETERM_", 1:5),
         M04_FETAL_LOSS_DSDECOD, M04_FETAL_LOSS_DSSTDAT, M04_HIV_EVER_MHOCCUR, M04_HIV_MHOCCUR) %>% 
  pivot_wider(
    names_from = M04_TYPE_VISIT,
    values_from = -c("MOMID", "PREGID", "SITE")
  ) %>% 
  select(-starts_with("n_"))

#merge to create maternal data 
df_maternal <- MAT_ENROLL %>% 
  select(SITE, MOMID, PREGID, BOE_GA_DAYS_ENROLL, PREG_START_DATE, 
         EDD_BOE, BOE_GA_WKS_ENROLL, ENROLL_SCRN_DATE,REMAPP_ENROLL,REMAPP_AIM3_ENROLL,REMAPP_AIM3_TRI,
         matches("^(ANC\\d+|ENROLL)_PASS_LATE$"), 
         matches("^(ANC\\d+|ENROLL)_LATE_WINDOW")) %>% 
  ## add remapp launch date for each site since these are remapp criteria 
  ## add remapp launch date for each site since these are remapp criteria 
  mutate(REMAPP_ENROLL_ZAM_FIX = case_when(SITE == "Zambia" &
                                             ENROLL_SCRN_DATE >= "2022-12-15" & ENROLL_SCRN_DATE <= "2025-03-20" ~ 1, 
                                           TRUE ~ REMAPP_ENROLL)) %>% 
  mutate(REMAPP_ENROLL = case_when(SITE != "Zambia" ~ REMAPP_ENROLL, 
                                   SITE == "Zambia" ~ REMAPP_ENROLL_ZAM_FIX, TRUE ~ REMAPP_ENROLL)) %>% 
  left_join(MAT_ENDPOINTS %>% select(SITE, MOMID, PREGID, CLOSEOUT_DT, PREG_LOSS, 
                                     MAT_DEATH, PREG_END_GA,MAT_DEATH_DATE,
                                     matches("PNC\\d+_PASS_LATE"), 
                                     matches("PNC\\d+_LATE_WINDOW"))) %>%
  left_join(MAT_GDM %>% select(SITE, MOMID, PREGID, DIAB_OVERT, DIAB_OVERT_DX)) %>% #HbA1c at enrollment >=6.5, preexisting overt diabetes
  left_join(mnh06_wide, by = c("MOMID", "PREGID", "SITE")) %>% 
  left_join(mnh08_wide, by = c("MOMID", "PREGID", "SITE")) %>% 
  left_join(mnh04_wide, by = c("MOMID", "PREGID", "SITE")) %>% 
  left_join(mnh07_wide, by = c("MOMID", "PREGID", "SITE")) %>% 
  left_join(sas_expansion_ids %>% mutate(SAS_EXPANSION = 1), by = c("PREGID"))

#******clean data
#read in the lab variables we are gonna report
lab_var <- read_excel("D:/Users/stacie.loisate/Documents/PRISMA-Public/PRISMA-Lab-Report/Lab report variables.xlsx") %>% 
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

#prepare data by replace default value with NA for numeric var 
prep_lab_num <- df_maternal %>% 
  select(SITE, MOMID, PREGID,  
         all_of(as.vector(contains(var_list_num$`Variable Name`))),contains("M08_FOLATE_RBC_NMOLL_LBORRES"), -contains("M08_RBC_THALA_"), -contains("M08_RBC_THALA_LBORRES_")) %>%
  #replace 7s and 5s with NA
  mutate_all(~ if_else(. < 0, NA, .)) %>% 
  mutate(across(
    matches("^M08_RBC_G6PD_LBORRES_|^M06_BGLUC_POC_MMOLL_LBORRES_|^M08_FOLATE_RBC_NMOLL_LBORRES"),
    ~ ifelse(. == 77, NA, .)
  ))


mnh08_g6pd_ke <- read.csv(paste0("~/import/", UploadDate, "_ke/mnh08.csv")) %>% 
  select(MOMID, PREGID,TYPE_VISIT, RBC_G6PD_LBORRES_Interpret) %>% 
  filter(RBC_G6PD_LBORRES_Interpret %in% c(1,2,3))

#prepare data by replace default value with NA for categorical var (not lab variables)
prep_lab_cat <- df_maternal %>%
  left_join(mnh08_g6pd_ke %>% select(-TYPE_VISIT), by = c("MOMID", "PREGID")) %>% 
  select(SITE, MOMID, PREGID,REMAPP_AIM3_ENROLL,REMAPP_AIM3_TRI, 
         REMAPP_ENROLL, MAT_DEATH_DATE,
         starts_with("M08_RBC_G6PD_LBORRES_Interpret"),
         contains("M08_LB_REMAPP3"),
         starts_with("M04_FETAL_LOSS_DSDECOD_"),
         starts_with("M04_HIV_EVER_MHOCCUR_"),
         starts_with("M04_TB_CETERM_"),
         starts_with("M04_HIV_MHOCCUR_"),
         starts_with("M06_TYPE_VISIT_"),
         starts_with("M06_MAT_VISIT_MNH06_"),
         starts_with("M06_HIV_POC_LBPERF_"),
         starts_with("M07_MAT_VISIT_MNH07"),
         contains("M07_MAT_TB_SPEC_COLLECT"),
         starts_with("M08_TYPE_VISIT_"),
         starts_with("M08_MAT_VISIT_MNH08_"),
         all_of(as.vector(contains(var_list_cat$`Variable Name`))),
         starts_with("M08_RBC_THALA_"),
         starts_with("M08_RBC_THALA_LBORRES_"),
         -contains("M08_RBC_THALA_LBTSTDAT")
         
  ) %>%
  #replace 7s and 5s with NA
  mutate_all(~ if_else(. %in% c(55,77), NA, .)) 

#prepare data by replace default value with NA for date var
prep_lab_date <- df_maternal %>% 
  select(SITE, MOMID, PREGID,
         ENROLL_SCRN_DATE,
         starts_with("M04_FETAL_LOSS_DSSTDAT_"),
         starts_with("M07_MAT_SPEC_COLLECT_DAT"),
         starts_with("M06_DIAG_VSDAT_"),
         starts_with("M08_LBSTDAT"), 
         starts_with("M08_SYPH_TITER_LBTSTDAT_"), 
         starts_with("M08_ZCD_LBTSTDAT_"), 
         starts_with("M08_LEPT_IGM_LBTSTDAT_"),
         starts_with("M08_LEPT_IGG_LBTSTDAT_"),
         starts_with("M08_HEV_LBTSTDAT_"), 
         starts_with("M08_CBC_LBTSTDAT_"), 
         starts_with("M08_LBSTDAT"), 
         starts_with("M08_RBC_THALA_LBTSTDAT"),
         M08_THYROID_LBTSTDAT_1, M08_THYROID_LBTSTDAT_4) %>%
  #replace 7s and 5s with NA
  mutate_all(~ if_else(. %in% c("1907-07-07", "1905-05-05"), NA, .)) 

#prepare data by including basic derived variables
pre_lab_other <- df_maternal %>% 
  select(SITE, MOMID, PREGID, PREG_START_DATE, BOE_GA_DAYS_ENROLL, BOE_GA_WKS_ENROLL, 
         CLOSEOUT_DT, PREG_END_GA, MAT_DEATH, PREG_LOSS, DIAB_OVERT, DIAB_OVERT_DX,
         ends_with("_PASS_LATE"), ends_with("_LATE_WINDOW")
         ) %>% 
  #replace with NA 
  mutate_all(~ if_else(. %in% c("1907-07-07"), NA, .)) 

#******prepare df_lab --> data for missingness
df_lab <- df_maternal %>% 
  select(SITE, MOMID, PREGID, SAS_EXPANSION) %>% 
  left_join(prep_lab_num, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(prep_lab_cat, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(prep_lab_date, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(pre_lab_other, by = c("SITE", "MOMID", "PREGID")) %>% 
  distinct() %>%
  #apply monitoring code 
  mutate(VC_ENROLL_DENOM_LATE = ifelse(ENROLL_PASS_LATE == 1, 1, 0), 
         VC_ANC20_DENOM_LATE = ifelse(ANC20_PASS_LATE == 1 & BOE_GA_WKS_ENROLL <= 17 &
                                        (PREG_END_GA>160 | is.na(PREG_END_GA)) & 
                                        ((CLOSEOUT_DT > ANC20_LATE_WINDOW) | is.na(CLOSEOUT_DT)), 1, 0),  ## closed out after window or has not yet closed out
         VC_ANC28_DENOM_LATE = ifelse(ANC28_PASS_LATE == 1  &
                                        (PREG_END_GA>216 | is.na(PREG_END_GA)) & 
                                        ((CLOSEOUT_DT > ANC28_LATE_WINDOW) | is.na(CLOSEOUT_DT)), 1, 0), 
         VC_ANC32_DENOM_LATE = ifelse(ANC32_PASS_LATE == 1  & 
                                        (PREG_END_GA>237 | is.na(PREG_END_GA)) & 
                                        ((CLOSEOUT_DT > ANC32_LATE_WINDOW) | is.na(CLOSEOUT_DT)), 1, 0), 
         VC_ANC36_DENOM_LATE = ifelse(ANC36_PASS_LATE == 1  & 
                                        (PREG_END_GA>272 | is.na(PREG_END_GA))  & 
                                        ((CLOSEOUT_DT > ANC36_LATE_WINDOW) | is.na(CLOSEOUT_DT)), 1, 0)
  ) %>% 
  mutate(VC_PNC0_DENOM_LATE = ifelse(PNC0_PASS_LATE==1 &
                                       ((CLOSEOUT_DT > PNC0_LATE_WINDOW) | is.na(CLOSEOUT_DT)) &
                                       ((MAT_DEATH_DATE > PNC0_LATE_WINDOW) | is.na(MAT_DEATH_DATE)), 1, 0),
         VC_PNC1_DENOM_LATE = ifelse(PNC1_PASS_LATE==1 &
                                       ((CLOSEOUT_DT > PNC1_LATE_WINDOW) | is.na(CLOSEOUT_DT)) &
                                       ((MAT_DEATH_DATE > PNC1_LATE_WINDOW) | is.na(MAT_DEATH_DATE)), 1, 0),
         VC_PNC4_DENOM_LATE = ifelse(PNC4_PASS_LATE==1 &
                                       ((CLOSEOUT_DT > PNC4_LATE_WINDOW) | is.na(CLOSEOUT_DT)) &
                                      ((MAT_DEATH_DATE > PNC4_LATE_WINDOW) | is.na(MAT_DEATH_DATE)), 1, 0),

         VC_PNC6_DENOM_LATE = ifelse(PNC6_PASS_LATE==1 & 
                                       ((CLOSEOUT_DT > PNC6_LATE_WINDOW) | is.na(CLOSEOUT_DT)) &
                                     ((MAT_DEATH_DATE > PNC6_LATE_WINDOW) | is.na(MAT_DEATH_DATE)), 1, 0),
         VC_PNC26_DENOM_LATE = ifelse(PNC26_PASS_LATE==1 & PREG_END_GA>139 & 
                                        ((CLOSEOUT_DT > PNC26_LATE_WINDOW) | is.na(CLOSEOUT_DT)) &
                                      ((MAT_DEATH_DATE > PNC26_LATE_WINDOW) | is.na(MAT_DEATH_DATE)), 1, 0),
         VC_PNC52_DENOM_LATE = ifelse(PNC52_PASS_LATE==1 & PREG_END_GA>139 & 
                                        ((CLOSEOUT_DT > PNC52_LATE_WINDOW) | is.na(CLOSEOUT_DT)) & 
                                      ((MAT_DEATH_DATE > PNC52_LATE_WINDOW) | is.na(MAT_DEATH_DATE)), 1, 0)
  ) %>% 
  #define denominator for form missingness
  mutate(
    #denominator for enrollment should be all enrolled in MAT_ENROLL
    denom_form_2 = ifelse(M06_TYPE_VISIT_2 == 2 | M08_TYPE_VISIT_2 == 2 | VC_ANC20_DENOM_LATE == 1, 1, 0),
    denom_form_3 = ifelse(M06_TYPE_VISIT_3 == 3 | M08_TYPE_VISIT_3 == 3 | VC_ANC28_DENOM_LATE == 1, 1, 0),
    denom_form_4 = ifelse(M06_TYPE_VISIT_4 == 4 | M08_TYPE_VISIT_4 == 4 | VC_ANC32_DENOM_LATE == 1, 1, 0),
    denom_form_5 = ifelse(M06_TYPE_VISIT_5 == 5 | M08_TYPE_VISIT_5 == 5 | VC_ANC36_DENOM_LATE == 1, 1, 0),
    denom_form_7 = ifelse(M06_TYPE_VISIT_7 == 7 | M08_TYPE_VISIT_7 == 7 | VC_PNC0_DENOM_LATE == 1, 1, 0),
    denom_form_8 = ifelse(M06_TYPE_VISIT_8 == 8 | M08_TYPE_VISIT_8 == 8 | VC_PNC1_DENOM_LATE == 1, 1, 0),
    denom_form_9 = ifelse(M06_TYPE_VISIT_9 == 9 | M08_TYPE_VISIT_9 == 9 | VC_PNC4_DENOM_LATE == 1, 1, 0),
    denom_form_10 = ifelse(M06_TYPE_VISIT_10 == 10 | M08_TYPE_VISIT_10 == 10 | VC_PNC6_DENOM_LATE == 1, 1, 0),
    denom_form_11 = ifelse(M06_TYPE_VISIT_11 == 11 | M08_TYPE_VISIT_11 == 11 | VC_PNC26_DENOM_LATE == 1, 1, 0),
    denom_form_12 = ifelse(M06_TYPE_VISIT_12 == 12 | M08_TYPE_VISIT_12 == 12 | VC_PNC52_DENOM_LATE == 1, 1, 0),
  ) %>%
  #define denominator for lab missingness in MNH06
  mutate(
    denom_lab_06_1 = ifelse(M06_TYPE_VISIT_1 == 1 & M06_MAT_VISIT_MNH06_1 == 1, 1, 0),
    denom_lab_06_2 = ifelse(M06_TYPE_VISIT_2 == 2 & M06_MAT_VISIT_MNH06_2 == 1, 1, 0),
    denom_lab_06_3 = ifelse(M06_TYPE_VISIT_3 == 3 & M06_MAT_VISIT_MNH06_3 == 1, 1, 0),
    denom_lab_06_4 = ifelse(M06_TYPE_VISIT_4 == 4 & M06_MAT_VISIT_MNH06_4 == 1, 1, 0),
    denom_lab_06_5 = ifelse(M06_TYPE_VISIT_5 == 5 & M06_MAT_VISIT_MNH06_5 == 1, 1, 0),
    denom_lab_06_6 = ifelse(M06_TYPE_VISIT_6 == 6 & M06_MAT_VISIT_MNH06_6 == 1, 1, 0),
    denom_lab_06_7 = ifelse(M06_TYPE_VISIT_7 == 7 & M06_MAT_VISIT_MNH06_7 == 1, 1, 0),
    denom_lab_06_8 = ifelse(M06_TYPE_VISIT_8 == 8 & M06_MAT_VISIT_MNH06_8 == 1, 1, 0),
    denom_lab_06_9 = ifelse(M06_TYPE_VISIT_9 == 9 & M06_MAT_VISIT_MNH06_9 == 1, 1, 0),
    denom_lab_06_10 = ifelse(M06_TYPE_VISIT_10 == 10 & M06_MAT_VISIT_MNH06_10 == 1, 1, 0),
    denom_lab_06_11 = ifelse(M06_TYPE_VISIT_11 == 11 & M06_MAT_VISIT_MNH06_11 == 1, 1, 0),
    denom_lab_06_12 = ifelse(M06_TYPE_VISIT_12 == 12 & M06_MAT_VISIT_MNH06_12 == 1, 1, 0),
  ) %>%
  #define denominator for lab missingness in MNH08
  mutate(
    denom_lab_08_1 = ifelse(M08_TYPE_VISIT_1 == 1 & M08_MAT_VISIT_MNH08_1 == 1, 1, 0),
    denom_lab_08_2 = ifelse(M08_TYPE_VISIT_2 == 2 & M08_MAT_VISIT_MNH08_2 == 1, 1, 0),
    denom_lab_08_3 = ifelse(M08_TYPE_VISIT_3 == 3 & M08_MAT_VISIT_MNH08_3 == 1, 1, 0),
    denom_lab_08_4 = ifelse(M08_TYPE_VISIT_4 == 4 & M08_MAT_VISIT_MNH08_4 == 1, 1, 0),
    denom_lab_08_5 = ifelse(M08_TYPE_VISIT_5 == 5 & M08_MAT_VISIT_MNH08_5 == 1, 1, 0),
    denom_lab_08_6 = ifelse(M08_TYPE_VISIT_6 == 6 & M08_MAT_VISIT_MNH08_6 == 1, 1, 0),
    denom_lab_08_7 = ifelse(M08_TYPE_VISIT_7 == 7 & M08_MAT_VISIT_MNH08_7 == 1, 1, 0),
    denom_lab_08_8 = ifelse(M08_TYPE_VISIT_8 == 8 & M08_MAT_VISIT_MNH08_8 == 1, 1, 0),
    denom_lab_08_9 = ifelse(M08_TYPE_VISIT_9 == 9 & M08_MAT_VISIT_MNH08_9 == 1, 1, 0),
    denom_lab_08_10 = ifelse(M08_TYPE_VISIT_10 == 10 & M08_MAT_VISIT_MNH08_10 == 1, 1, 0),
    denom_lab_08_11 = ifelse(M08_TYPE_VISIT_11 == 11 & M08_MAT_VISIT_MNH08_11 == 1, 1, 0),
    denom_lab_08_12 = ifelse(M08_TYPE_VISIT_12 == 12 & M08_MAT_VISIT_MNH08_12 == 1, 1, 0),
  ) %>%
  #define denominator for ReMAPP exclusive labs (RBC at enrollment)
  # ANC20, ANC28, ANC36, PNC6
  mutate(
    denom_remapp_cbc_2 = case_when( ## updated to filter by LBSTDAT instead of ENROLL_SCRN_DATE
      SITE %in% c("Ghana", "Kenya", "Zambia", "India-CMC", "India-SAS") & denom_lab_08_1 == 1 & REMAPP_ENROLL ==1 ~ 1,
      SITE == "Pakistan" & (M08_LBSTDAT_2 >= "2022-09-22" & M08_LBSTDAT_2 <= "2024-04-22") &
        denom_lab_08_1 == 1 & REMAPP_ENROLL ==1 ~ 1, TRUE ~ 0
    ),
    denom_remapp_cbc_3 = case_when( ## updated to filter by LBSTDAT instead of ENROLL_SCRN_DATE
      SITE %in% c("Ghana", "Kenya", "Zambia", "India-CMC", "India-SAS") & denom_lab_08_1 == 1 & REMAPP_ENROLL ==1 ~ 1,
      SITE == "Pakistan" & (M08_LBSTDAT_3 >= "2022-09-22" & M08_LBSTDAT_3 <= "2024-04-22") &
        denom_lab_08_3 == 1 & REMAPP_ENROLL ==1 ~ 1, TRUE ~ 0
    ),
    denom_remapp_cbc_5 = case_when( ## updated to filter by LBSTDAT instead of ENROLL_SCRN_DATE
      SITE %in% c("Ghana", "Kenya", "Zambia", "India-CMC", "India-SAS") & denom_lab_08_1 == 1 & REMAPP_ENROLL ==1 ~ 1,
      SITE == "Pakistan" & (M08_LBSTDAT_5 >= "2022-09-22" & M08_LBSTDAT_5 <= "2024-04-22") &
        denom_lab_08_5 == 1 & REMAPP_ENROLL ==1 ~ 1, TRUE ~ 0
    ),
    denom_remapp_cbc_10 = case_when( ## updated to filter by LBSTDAT instead of ENROLL_SCRN_DATE
      SITE %in% c("Ghana", "Kenya", "Zambia", "India-CMC", "India-SAS") & denom_lab_08_1 == 1 & REMAPP_ENROLL ==1 ~ 1,
      SITE == "Pakistan" & (M08_LBSTDAT_10 >= "2022-09-22" & M08_LBSTDAT_10 <= "2024-04-22") &
        denom_lab_08_10 == 1 & REMAPP_ENROLL ==1 ~ 1, TRUE ~ 0
    )
  ) %>% 
  mutate(
    denom_remapp = case_when( ## updated to filter by LBSTDAT instead of ENROLL_SCRN_DATE
      denom_lab_08_1 == 1 & REMAPP_ENROLL ==1 ~ 1, TRUE ~ 0
    )) %>% 
  #define denominator for syphilis (test date)
  mutate(
    denom_syphilis_enroll = case_when(
      (SITE == "Ghana" & M08_SYPH_TITER_LBTSTDAT_1 >= "2024-04-09") | 
        (SITE == "India-CMC" & M08_SYPH_TITER_LBTSTDAT_1 >= "2023-07-18") | 
        (SITE == "India-SAS" & M08_SYPH_TITER_LBTSTDAT_1 >= "2024-03-11") |
        (SITE == "Kenya" & M08_SYPH_TITER_LBTSTDAT_1 >= "2024-03-06") | 
        (SITE == "Pakistan" & M08_SYPH_TITER_LBTSTDAT_1 >= "2024-04-05") | 
        (SITE == "Zambia" & M08_SYPH_TITER_LBTSTDAT_1 >= "2023-11-09") ~ 1,
      TRUE ~ 0
    )) %>% 
  mutate(
    denom_syphilis_anc32 = case_when(
      (SITE == "Ghana" & M08_SYPH_TITER_LBTSTDAT_4 >= "2024-04-09") | 
        (SITE == "India-CMC" & M08_SYPH_TITER_LBTSTDAT_4 >= "2023-07-18") |  
        (SITE == "India-SAS" & M08_SYPH_TITER_LBTSTDAT_4 >= "2024-07-29" ) | 
        (SITE == "Kenya" & M08_SYPH_TITER_LBTSTDAT_4 >= "2024-03-06") |  
        (SITE == "Pakistan" & M08_SYPH_TITER_LBTSTDAT_4 >= "2024-07-19") | 
        (SITE == "Zambia" & M08_SYPH_TITER_LBTSTDAT_3 >= "2023-11-09") ~ 1,  #Zambia test on ANC28 
      TRUE ~ 0
    )) %>% 
  mutate(  # define denominator for syphilis point of care and anc32 
    denom_syph_poc_anc32 = case_when(
      denom_lab_06_3 ==1 & SITE == "Zambia" & M06_DIAG_VSDAT_3 >= "2023-11-09" ~ 1,
      denom_lab_06_4 == 1 & 
        ((SITE == "Ghana" & M06_DIAG_VSDAT_4 >= "2024-04-09") | 
           (SITE == "Kenya" &  M06_DIAG_VSDAT_4  >= "2024-03-06") |
           (SITE == "Zambia" &  M06_DIAG_VSDAT_4  >= "2023-11-09") |
           (SITE == "Pakistan" &  ENROLL_SCRN_DATE  >= "2024-04-24") | ## added from Zahra's feedback that they are performing syphilis POC at ANC32 for participants who were enrolled after April 24th
           (SITE == "India-CMC") |
           (SITE == "India-SAS" &  M06_DIAG_VSDAT_4  >= "2023-12-12" & SAS_EXPANSION==1)) ~ 1, ## only expansion ids are expected to have this test at this visit
      TRUE ~ 0)) %>% 
  #define denominator for glucose test
  mutate(
    denom_glucose= case_when(
      DIAB_OVERT == 1 | DIAB_OVERT_DX == 1 ~ 0, #overt diabetes
      TRUE ~ 1
    )) %>% 
  #define denominator for HIV test 
  mutate(
    denom_hiv= case_when(
      SITE != "Kenya" ~ 1, 
      SITE == "Kenya" & M04_HIV_EVER_MHOCCUR_1 == 0 & M06_HIV_POC_LBPERF_1 == 1 ~ 1, 
      TRUE ~ 0
    )) %>% 
## define denominator zcd/lepto/hev (denom_lept_igm, denom_lept_igg, denom_hev, denom_zcd) 
  mutate(
    denom_zcd_lept_hev_1 = case_when(
      SITE == "Ghana" &  if_any(c(M08_ZCD_LBTSTDAT_1, M08_LEPT_IGG_LBTSTDAT_1, M08_HEV_LBTSTDAT_1, M08_LEPT_IGM_LBTSTDAT_1), ~.x >= "2024-04-09") ~ 1,
      SITE == "India-CMC" &  if_any(c(M08_ZCD_LBTSTDAT_1, M08_LEPT_IGG_LBTSTDAT_1, M08_HEV_LBTSTDAT_1, M08_LEPT_IGM_LBTSTDAT_1), ~.x >= "2024-03-06") ~ 1,
      SITE == "India-SAS" & SAS_EXPANSION==1 & if_any(c(M08_ZCD_LBTSTDAT_1, M08_LEPT_IGG_LBTSTDAT_1, M08_HEV_LBTSTDAT_1, M08_LEPT_IGM_LBTSTDAT_1), ~.x >= "2024-03-11" & .x <= "2025-02-01") ~ 1,
      SITE == "Kenya"& if_any(c(M08_ZCD_LBTSTDAT_1, M08_LEPT_IGG_LBTSTDAT_1, M08_HEV_LBTSTDAT_1, M08_LEPT_IGM_LBTSTDAT_1), ~.x >= "2024-03-06") ~ 1,
      SITE == "Pakistan" &if_any(c(M08_ZCD_LBTSTDAT_1, M08_LEPT_IGG_LBTSTDAT_1, M08_HEV_LBTSTDAT_1, M08_LEPT_IGM_LBTSTDAT_1), ~.x >= "2024-04-08") ~ 1,
      SITE == "Zambia" & if_any(c(M08_ZCD_LBTSTDAT_1, M08_LEPT_IGG_LBTSTDAT_1, M08_HEV_LBTSTDAT_1, M08_LEPT_IGM_LBTSTDAT_1), ~.x >= "2023-11-09") ~ 1,
      TRUE ~ 0
    ),

    denom_zcd_lept_hev_4 = case_when(
      SITE == "Ghana" &  if_any(c(M08_ZCD_LBTSTDAT_4, M08_LEPT_IGG_LBTSTDAT_4, M08_HEV_LBTSTDAT_4, M08_LEPT_IGM_LBTSTDAT_4), ~.x >= "2024-04-09") ~ 1,
      SITE == "India-CMC" &  if_any(c(M08_ZCD_LBTSTDAT_4, M08_LEPT_IGG_LBTSTDAT_4, M08_HEV_LBTSTDAT_4, M08_LEPT_IGM_LBTSTDAT_4), ~.x >= "2024-03-06") ~ 1,
      SITE == "India-SAS" & SAS_EXPANSION==1 &  if_any(c(M08_ZCD_LBTSTDAT_4, M08_LEPT_IGG_LBTSTDAT_4, M08_HEV_LBTSTDAT_4, M08_LEPT_IGM_LBTSTDAT_4), ~.x >= "2024-03-11" & .x <= "2025-02-01") ~ 1,
      SITE == "Kenya"& if_any(c(M08_ZCD_LBTSTDAT_4, M08_LEPT_IGG_LBTSTDAT_4, M08_HEV_LBTSTDAT_4, M08_LEPT_IGM_LBTSTDAT_4), ~.x >= "2024-03-06") ~ 1,
      SITE == "Pakistan" &if_any(c(M08_ZCD_LBTSTDAT_4, M08_LEPT_IGG_LBTSTDAT_4, M08_HEV_LBTSTDAT_4, M08_LEPT_IGM_LBTSTDAT_4), ~.x >= "2024-04-08") ~ 1,
      SITE == "Zambia" & if_any(c(M08_ZCD_LBTSTDAT_4, M08_LEPT_IGG_LBTSTDAT_4, M08_HEV_LBTSTDAT_4, M08_LEPT_IGM_LBTSTDAT_4), ~.x >= "2023-11-09") ~ 1,
      TRUE ~ 0
    ),

    denom_zcd_lept_hev_10 = case_when(
      SITE == "Ghana" &  if_any(c(M08_ZCD_LBTSTDAT_10, M08_LEPT_IGG_LBTSTDAT_10, M08_HEV_LBTSTDAT_10, M08_LEPT_IGM_LBTSTDAT_10), ~.x >= "2024-04-09") ~ 1,
      SITE == "India-CMC" &  if_any(c(M08_ZCD_LBTSTDAT_10, M08_LEPT_IGG_LBTSTDAT_10, M08_HEV_LBTSTDAT_10, M08_LEPT_IGM_LBTSTDAT_10), ~.x >= "2024-03-06") ~ 1,
      SITE == "India-SAS" & SAS_EXPANSION==1 &  if_any(c(M08_ZCD_LBTSTDAT_10, M08_LEPT_IGG_LBTSTDAT_10, M08_HEV_LBTSTDAT_10, M08_LEPT_IGM_LBTSTDAT_10), ~.x >= "2024-03-11" & .x <= "2025-02-01") ~ 1,
      SITE == "Kenya"& if_any(c(M08_ZCD_LBTSTDAT_10, M08_LEPT_IGG_LBTSTDAT_10, M08_HEV_LBTSTDAT_10, M08_LEPT_IGM_LBTSTDAT_10), ~.x >= "2024-03-06") ~ 1,
      SITE == "Pakistan" &if_any(c(M08_ZCD_LBTSTDAT_10, M08_LEPT_IGG_LBTSTDAT_10, M08_HEV_LBTSTDAT_10, M08_LEPT_IGM_LBTSTDAT_10), ~.x >= "2024-04-08") ~ 1,
      SITE == "Zambia" & if_any(c(M08_ZCD_LBTSTDAT_10, M08_LEPT_IGG_LBTSTDAT_10, M08_HEV_LBTSTDAT_10, M08_LEPT_IGM_LBTSTDAT_10), ~.x >= "2023-11-09") ~ 1,
      TRUE ~ 0
    )
  ) %>%
  #define denominator for RBC disorder test 
  mutate(
    denom_rbc_disorder = case_when(
      denom_lab_08_1 == 1 & 
        ((SITE == "Ghana" & ENROLL_SCRN_DATE >= "2022-12-28" & ENROLL_SCRN_DATE <= "2024-10-29") | ## end date confirmed added 
           (SITE == "Kenya" & ENROLL_SCRN_DATE >= "2023-04-03") | ## should this be 3 April 
           (SITE == "Zambia" & ENROLL_SCRN_DATE >= "2022-12-15") |
           (SITE == "Pakistan" & M08_LBSTDAT_1 >= "2022-09-22" & M08_LBSTDAT_1 <= "2024-01-17") | 
           (SITE == "India-CMC" & ENROLL_SCRN_DATE >= "2023-06-20" & ENROLL_SCRN_DATE <= "2025-08-22") |
           (SITE == "India-SAS" & ENROLL_SCRN_DATE >= "2023-12-12")) ~ 1,
      TRUE ~ 0
    )) %>% 
  #define denominator for free t3 and free t4 at ANC32
  mutate(
    denom_t3t4_anc32 = case_when(
      denom_lab_08_4 == 1 & 
        ((SITE == "Ghana" & (M08_LBSTDAT_4 <= "2024-09-01" |  # M08_THYROID_LBTSTDAT_4
                            (M08_LBSTDAT_4 > "2024-09-01" & (M08_THYROID_TSH_LBORRES_4 < 0.3 | M08_THYROID_TSH_LBORRES_4 > 4)))) |
           (SITE == "Kenya" & (M08_LBSTDAT_4 <= "2025-01-14" |  (M08_LBSTDAT_4 > "2025-01-14" & 
                              (M08_THYROID_TSH_LBORRES_4 < 0.3 | M08_THYROID_TSH_LBORRES_4 > 4)))) |
           (SITE == "Zambia" & (M08_LBSTDAT_4 <= "2024-10-11" |  (M08_LBSTDAT_4 > "2024-10-11" & 
                               (M08_THYROID_TSH_LBORRES_4 < 0.3 | M08_THYROID_TSH_LBORRES_4 > 4)))) |
           (SITE == "Pakistan" & (M08_LBSTDAT_4 <= "2024-10-3" |  (M08_LBSTDAT_4 > "2024-10-03" & 
                                 (M08_THYROID_TSH_LBORRES_4 < 0.3 | M08_THYROID_TSH_LBORRES_4 > 4)))) |
           (SITE == "India-CMC" & (M08_LBSTDAT_4 <= "2024-10-14" |  (M08_LBSTDAT_4 > "2024-10-14" & ## should this be 10-16?
                                  (M08_THYROID_TSH_LBORRES_4 < 0.3 | M08_THYROID_TSH_LBORRES_4 > 4)))) |
           (SITE == "India-SAS")) ~ 1,
      TRUE ~ 0
    )) %>% 
  #define denominator for free t3 and free t4 at enrollment
  mutate(
    denom_t3t4_enroll = case_when(
      denom_lab_08_1 == 1 & 
        ((SITE == "Ghana" & M08_LBSTDAT_1 <= "2024-09-01" ) |  # M08_THYROID_LBTSTDAT_1
           (SITE == "Kenya" &  M08_LBSTDAT_1 <= "2025-01-14") |
           (SITE == "Zambia" &  M08_LBSTDAT_1 <= "2024-10-11") |
           (SITE == "Pakistan" &  M08_LBSTDAT_1 <= "2024-10-03") |
           (SITE == "India-CMC" &  M08_LBSTDAT_1 <= "2024-10-17") |
           (SITE == "India-SAS")) ~ 1,
      TRUE ~ 0
    )) %>% 
  #define denominator for kidney & liver function tests at enrollment 
  mutate(
    denom_kft_lft_fxn_enroll = case_when(
      denom_lab_08_1 == 1 & 
           (SITE %in% c("Ghana", "Kenya", "Pakistan", "Zambia") | 
           (SITE == "India-CMC" &  M08_LBSTDAT_1 <= "2025-01-31") |
           (SITE == "India-SAS" &  M08_LBSTDAT_1 <= "2024-08-31")) ~ 1, ## added dates to enrollment lft 
      TRUE ~ 0
    )) %>% 
  #define denominator for kidney & liver function tests at ANC32 
  mutate(
    denom_kft_lft_fxn_anc32 = case_when(
      denom_lab_08_4 == 1 & 
        ((SITE == "Ghana" & M08_LBSTDAT_4 <= "2024-10-01") | 
           (SITE == "Kenya" &  M08_LBSTDAT_4 <= "2025-01-14") |
           (SITE == "Zambia" &  M08_LBSTDAT_4 <= "2024-10-11") |
           (SITE == "Pakistan" &  M08_LBSTDAT_4 <= "2024-10-03") |
           (SITE == "India-CMC" &  M08_LBSTDAT_4 <= "2024-10-16") |
           (SITE == "India-SAS" &  M08_LBSTDAT_4 <= "2024-08-31")) ~ 1, ## updated from 2024-09-02 to 2024-08-31
      TRUE ~ 0
    )) %>% 
  
  #define denominator for transition from serum folate to RBC folate 
  mutate(
    denom_folate_rbc_1 = case_when(
      denom_lab_08_1 == 1 & 
        ((SITE == "Ghana" & M08_LBSTDAT_1 >= "2024-02-23") | 
           (SITE == "Kenya" &  M08_LBSTDAT_1 >= "2024-04-11") |
           (SITE == "Zambia" &  M08_LBSTDAT_1 >= "2024-04-12") |
           (SITE == "Pakistan" &  M08_LBSTDAT_1 >= "2024-04-08") |
           (SITE == "India-CMC" &  M08_LBSTDAT_1 >= "2024-03-05") |
           (SITE == "India-SAS" &  M08_LBSTDAT_1 >= "2024-03-01")) ~ 1,
      TRUE ~ 0
    ),
    denom_folate_rbc_4 = case_when(
      denom_lab_08_4 == 1 & 
        ((SITE == "Ghana" & M08_LBSTDAT_4 >= "2024-02-23") | 
           (SITE == "Kenya" &  M08_LBSTDAT_4 >= "2024-04-11") |
           (SITE == "Zambia" &  M08_LBSTDAT_4 >= "2024-04-12") |
           (SITE == "Pakistan" &  M08_LBSTDAT_4 >= "2024-04-08") |
           (SITE == "India-CMC" &  M08_LBSTDAT_4 >= "2024-03-05") |
           (SITE == "India-SAS" &  M08_LBSTDAT_4 >= "2024-03-01")) ~ 1,
      TRUE ~ 0
    ),
    denom_folate_rbc_10 = case_when(
      denom_lab_08_10 == 1 & 
        ((SITE == "Ghana" & M08_LBSTDAT_10 >= "2024-02-23") | 
           (SITE == "Kenya" &  M08_LBSTDAT_10 >= "2024-04-11") |
           (SITE == "Zambia" &  M08_LBSTDAT_10 >= "2024-04-12") |
           (SITE == "Pakistan" &  M08_LBSTDAT_10 >= "2024-04-08") |
           (SITE == "India-CMC" &  M08_LBSTDAT_10 >= "2024-03-05") |
           (SITE == "India-SAS" &  M08_LBSTDAT_10 >= "2024-03-01")) ~ 1,
      TRUE ~ 0
    )) %>% 
  #define denominator for transition for serum folate
  mutate(
    denom_folate_serum_1 = case_when(
      denom_lab_08_1 == 1 & 
        ((SITE == "Ghana" & M08_LBSTDAT_1 <"2024-02-23") | 
           (SITE == "Kenya" &  M08_LBSTDAT_1 <"2024-04-11") |
           (SITE == "Zambia" &  M08_LBSTDAT_1 <"2024-04-12") |
           (SITE == "Pakistan" &  M08_LBSTDAT_1 <"2024-04-08") |
           (SITE == "India-CMC" &  M08_LBSTDAT_1 <"2024-03-05") |
           (SITE == "India-SAS" &  M08_LBSTDAT_1 <"2024-03-01")) ~ 1,
      TRUE ~ 0
    ),
    denom_folate_serum_4 = case_when(
      denom_lab_08_4 == 1 & 
        ((SITE == "Ghana" & M08_LBSTDAT_4 <"2024-02-23") | 
           (SITE == "Kenya" &  M08_LBSTDAT_4 <"2024-04-11") |
           (SITE == "Zambia" &  M08_LBSTDAT_4 <"2024-04-12") |
           (SITE == "Pakistan" &  M08_LBSTDAT_4 <"2024-04-08") |
           (SITE == "India-CMC" &  M08_LBSTDAT_4 <"2024-03-05") |
           (SITE == "India-SAS" &  M08_LBSTDAT_4 <"2024-03-01")) ~ 1,
      TRUE ~ 0
    ),
    denom_folate_serum_10 = case_when(
      denom_lab_08_10 == 1 & 
        ((SITE == "Ghana" & M08_LBSTDAT_10 <"2024-02-23") | 
           (SITE == "Kenya" &  M08_LBSTDAT_10 <"2024-04-11") |
           (SITE == "Zambia" &  M08_LBSTDAT_10 <"2024-04-12") |
           (SITE == "Pakistan" &  M08_LBSTDAT_10 <"2024-04-08") |
           (SITE == "India-CMC" &  M08_LBSTDAT_10 <"2024-03-05") |
           (SITE == "India-SAS" &  M08_LBSTDAT_10 <"2024-03-01")) ~ 1,
      TRUE ~ 0
    )) %>% 
  
  #define denominator for holoTC @ Enroll/ANC32 
  mutate(
    denom_holo_1 = case_when(
      denom_lab_08_1 == 1 & 
        ((SITE == "Ghana" & M08_LBSTDAT_1 <= "2024-09-01") | 
           (SITE == "Kenya" &  M08_LBSTDAT_1 <= "2025-01-14") |
           (SITE == "Zambia" &  M08_LBSTDAT_1 <= "2024-10-11") |
           (SITE == "Pakistan" &  M08_LBSTDAT_1 <= "2024-10-03") |
           (SITE == "India-CMC" &  M08_LBSTDAT_1 <= "2024-10-15") |
           (SITE == "India-SAS" &  M08_LBSTDAT_1 <= "2024-08-31")) ~ 1, # updated from 2024-09-02 to 2024-08-31
      TRUE ~ 0
    ),
    denom_holo_4 = case_when(
      denom_lab_08_4 == 1 & 
        ((SITE == "Ghana" & M08_LBSTDAT_4 <= "2024-09-01") | 
           (SITE == "Kenya" &  M08_LBSTDAT_4 <= "2025-01-14") |
           (SITE == "Zambia" &  M08_LBSTDAT_4 <= "2024-10-11") |
           (SITE == "Pakistan" &  M08_LBSTDAT_4 <= "2024-10-03") |
           (SITE == "India-CMC" &  M08_LBSTDAT_4 <= "2024-10-15") |
           (SITE == "India-SAS" &  M08_LBSTDAT_4 <= "2024-09-02")) ~ 1,
      TRUE ~ 0
    ),
    ) %>% 
  #define denominator ctng at enrollment and anc32
  mutate(
    denom_ctng_1 = case_when( # M07_MAT_SPEC_COLLECT_DAT_1
      denom_lab_08_1 == 1 & 
        ((SITE == "Ghana" & denom_lab_08_1 ==1 & (M08_LBSTDAT_1 >= "2024-04-09" & M08_LBSTDAT_1 <= "2025-04-01")) | 
           (SITE == "Kenya"  & denom_lab_08_1 ==1 & (M08_LBSTDAT_1 >= "2024-03-07" & M08_LBSTDAT_1 <= "2025-02-27")) | 
           (SITE == "Zambia" &  M08_LBSTDAT_1 >= "2023-11-09" & denom_lab_08_1 ==1 & M08_LBSTDAT_1 <= "2025-03-13") |
           (SITE == "Pakistan" & denom_lab_08_1 ==1 & (M08_LBSTDAT_1 >= "2024-04-25" & M08_LBSTDAT_1 <= "2025-04-24")) |
           (SITE == "India-CMC" &  M08_LBSTDAT_1 >= "2024-07-03" & denom_lab_08_1 ==1 & M08_LBSTDAT_1 <= "2025-09-17") | 
           (SITE == "India-SAS" & denom_lab_08_1 ==1 & SAS_EXPANSION==1 & (M08_LBSTDAT_1 >= "2024-03-11" & M08_LBSTDAT_1 <= "2025-03-06"))) ~ 1,  ## only expansion ids are expected to have this test at this visit
      TRUE ~ 0
    ),
    denom_ctng_4 = case_when( # M07_MAT_SPEC_COLLECT_DAT_4; M08_LBSTDAT_1
      denom_lab_08_4 == 1 & 
        ((SITE == "Ghana" & denom_lab_08_4 ==1 & (M08_LBSTDAT_1 >= "2024-04-09" & M08_LBSTDAT_1 <= "2025-04-01")) | 
           (SITE == "Kenya"  & denom_lab_08_4 ==1 & (M08_LBSTDAT_1 >= "2024-03-07" & M08_LBSTDAT_1 <= "2025-02-27")) | 
           (SITE == "Zambia" & denom_lab_08_4 ==1 &  (M08_LBSTDAT_1 >= "2023-11-09" & M08_LBSTDAT_1 <= "2025-02-27")) | 
           (SITE == "Pakistan" & denom_lab_08_4 ==1 & (M08_LBSTDAT_1 >= "2024-04-25" & M08_LBSTDAT_1 <= "2025-04-24")) |
           (SITE == "India-CMC"& denom_lab_08_4 ==1 &  (M08_LBSTDAT_1 >= "2024-07-03"  & M08_LBSTDAT_1 <= "2025-09-17")) | 
           (SITE == "India-SAS" & denom_lab_08_4 ==1 & SAS_EXPANSION==1 & (M08_LBSTDAT_1 >= "2024-03-11" & M08_LBSTDAT_1 <= "2025-03-06"))) ~ 1,  ## only expansion ids are expected to have this test at this visit
      TRUE ~ 0
    )) %>% 
  # #define denominator for kenya carbon dioxide AND carbon dioxide for SAS at enrollment and ANC32
  mutate(co2_denom_lab_08_1 = case_when(!SITE %in% c("Kenya", "India-SAS") & denom_lab_08_1 == 1 ~ 1,
                                        (SITE ==  "Kenya" & denom_lab_08_1 == 1 & M08_LBSTDAT_1 >="2023-04-01") |
                                        (SITE ==  "India-SAS" & denom_lab_08_1 == 1 & M08_LBSTDAT_1 >="2024-07-13")
                                          ~ 1, TRUE ~ 0),
         
         co2_denom_lab_08_2 = case_when(SITE !=  "Kenya" & denom_lab_08_2 == 1 ~ 1,
                                        SITE ==  "Kenya" & denom_lab_08_2 == 1 & M08_LBSTDAT_2 >="2023-04-01" ~ 1, TRUE ~ 0
         ),
         co2_denom_lab_08_3 = case_when(SITE !=  "Kenya" & denom_lab_08_3 == 1 ~ 1,
                                        SITE ==  "Kenya" & denom_lab_08_3 == 1 & M08_LBSTDAT_3 >="2023-04-01" ~ 1, TRUE ~ 0
         ),
         co2_denom_lab_08_4 = case_when(!SITE %in% c("Kenya", "India-SAS") & denom_lab_08_4 == 1 ~ 1,
                                        (SITE ==  "Kenya" & denom_lab_08_4 == 1 & M08_LBSTDAT_4 >="2023-04-01") |
                                        (SITE ==  "India-SAS" & denom_lab_08_4 == 1 & M08_LBSTDAT_4 >="2024-07-13") ~ 1, TRUE ~ 0
         ),
         co2_denom_lab_08_5 = case_when(SITE !=  "Kenya" & denom_lab_08_5 == 1 ~ 1,
                                        SITE ==  "Kenya" & denom_lab_08_5 == 1 & M08_LBSTDAT_5 >="2023-04-01" ~ 1, TRUE ~ 0
         ),
         co2_denom_lab_08_6 = case_when(SITE !=  "Kenya" & denom_lab_08_6 == 1 ~ 1,
                                        SITE ==  "Kenya" & denom_lab_08_6 == 1 & M08_LBSTDAT_6 >="2023-04-01" ~ 1, TRUE ~ 0
         ),
         co2_denom_lab_08_7 = case_when(SITE !=  "Kenya" & denom_lab_08_7 == 1 ~ 1,
                                        SITE ==  "Kenya" & denom_lab_08_7 == 1 & M08_LBSTDAT_7 >="2023-04-01" ~ 1, TRUE ~ 0
         ),
         co2_denom_lab_08_8 = case_when(SITE !=  "Kenya" & denom_lab_08_8 == 1 ~ 1,
                                        SITE ==  "Kenya" & denom_lab_08_8 == 1 & M08_LBSTDAT_8 >="2023-04-01" ~ 1, TRUE ~ 0
         ),
         co2_denom_lab_08_9 = case_when(SITE !=  "Kenya" & denom_lab_08_9 == 1 ~ 1,
                                        SITE ==  "Kenya" & denom_lab_08_9 == 1 & M08_LBSTDAT_9 >="2023-04-01" ~ 1, TRUE ~ 0
         ),
         co2_denom_lab_08_10 = case_when(SITE !=  "Kenya" & denom_lab_08_10 == 1 ~ 1,
                                        SITE ==  "Kenya" & denom_lab_08_10 == 1 & M08_LBSTDAT_10 >="2023-04-01" ~ 1, TRUE ~ 0
         ),
         co2_denom_lab_08_11 = case_when(SITE !=  "Kenya" & denom_lab_08_11 == 1 ~ 1,
                                        SITE ==  "Kenya" & denom_lab_08_11 == 1 & M08_LBSTDAT_11 >="2023-04-01" ~ 1, TRUE ~ 0
         ),
         co2_denom_lab_08_12 = case_when(SITE !=  "Kenya" & denom_lab_08_12 == 1 ~ 1,
                                        SITE ==  "Kenya" & denom_lab_08_12 == 1 & M08_LBSTDAT_12 >="2023-04-01" ~ 1, TRUE ~ 0
         )
         ) %>% 
        # #define denominator for kenya carbon dioxide
        mutate(pdw_denom_lab_08_1 = case_when(SITE !=  "Kenya" & denom_lab_08_1 == 1 ~ 1,
                                              SITE ==  "Kenya" & denom_lab_08_1 == 1 & M08_LBSTDAT_1 >="2023-03-29" ~ 1, TRUE ~ 0
        ),
        
        pdw_denom_lab_08_2 = case_when(SITE !=  "Kenya" & denom_lab_08_2 == 1 ~ 1,
                                       SITE ==  "Kenya" & denom_lab_08_2 == 1 & M08_LBSTDAT_2 >="2023-03-29" ~ 1, TRUE ~ 0
        ),
        pdw_denom_lab_08_3 = case_when(SITE !=  "Kenya" & denom_lab_08_3 == 1 ~ 1,
                                       SITE ==  "Kenya" & denom_lab_08_3 == 1 & M08_LBSTDAT_3 >="2023-03-29" ~ 1, TRUE ~ 0
        ),
        pdw_denom_lab_08_4 = case_when(SITE !=  "Kenya" & denom_lab_08_4 == 1 ~ 1,
                                       SITE ==  "Kenya" & denom_lab_08_4 == 1 & M08_LBSTDAT_4 >="2023-03-29" ~ 1, TRUE ~ 0
        ),
        pdw_denom_lab_08_5 = case_when(SITE !=  "Kenya" & denom_lab_08_5 == 1 ~ 1,
                                       SITE ==  "Kenya" & denom_lab_08_5 == 1 & M08_LBSTDAT_5 >="2023-03-29" ~ 1, TRUE ~ 0
        ),
        pdw_denom_lab_08_6 = case_when(SITE !=  "Kenya" & denom_lab_08_6 == 1 ~ 1,
                                       SITE ==  "Kenya" & denom_lab_08_6 == 1 & M08_LBSTDAT_6 >="2023-03-29" ~ 1, TRUE ~ 0
        ),
        pdw_denom_lab_08_7 = case_when(SITE !=  "Kenya" & denom_lab_08_7 == 1 ~ 1,
                                       SITE ==  "Kenya" & denom_lab_08_7 == 1 & M08_LBSTDAT_7 >="2023-03-29" ~ 1, TRUE ~ 0
        ),
        pdw_denom_lab_08_8 = case_when(SITE !=  "Kenya" & denom_lab_08_8 == 1 ~ 1,
                                       SITE ==  "Kenya" & denom_lab_08_8 == 1 & M08_LBSTDAT_8 >="2023-03-29" ~ 1, TRUE ~ 0
        ),
        pdw_denom_lab_08_9 = case_when(SITE !=  "Kenya" & denom_lab_08_9 == 1 ~ 1,
                                       SITE ==  "Kenya" & denom_lab_08_9 == 1 & M08_LBSTDAT_9 >="2023-03-29" ~ 1, TRUE ~ 0
        ),
        pdw_denom_lab_08_10 = case_when(SITE !=  "Kenya" & denom_lab_08_10 == 1 ~ 1,
                                        SITE ==  "Kenya" & denom_lab_08_10 == 1 & M08_LBSTDAT_10 >="2023-03-29" ~ 1, TRUE ~ 0
        ),
        pdw_denom_lab_08_11 = case_when(SITE !=  "Kenya" & denom_lab_08_11 == 1 ~ 1,
                                        SITE ==  "Kenya" & denom_lab_08_11 == 1 & M08_LBSTDAT_11 >="2023-03-29" ~ 1, TRUE ~ 0
        ),
        pdw_denom_lab_08_12 = case_when(SITE !=  "Kenya" & denom_lab_08_12 == 1 ~ 1,
                                        SITE ==  "Kenya" & denom_lab_08_12 == 1 & M08_LBSTDAT_12 >="2023-03-29" ~ 1, TRUE ~ 0
        )
      ) %>% 
        mutate(W4SS_SCREEN_PERF_ENROLL = case_when(M04_TB_CETERM_1_1 %in% c(1, 0) & 
                                                     M04_TB_CETERM_2_1 %in% c(1, 0) & 
                                               M04_TB_CETERM_3_1 %in% c(1, 0) &
                                               M04_TB_CETERM_4_1 %in% c(1, 0) ~ 1,
                                            SITE %in% c("India-SAS", "India-CMC") & M04_TB_CETERM_1_1 %in% c(1, 0, NA) & 
                                               M04_TB_CETERM_2_1 %in% c(1, 0, NA) & 
                                               M04_TB_CETERM_3_1 %in% c(1, 0,NA) &
                                               M04_TB_CETERM_4_1 %in% c(1, 0,NA) ~ 1, 
                                             TRUE  ~ 0),
         W4SS_SCREEN_PERF_ANC32 = case_when(M04_TB_CETERM_1_4 %in% c(1, 0) & 
                                              M04_TB_CETERM_2_4 %in% c(1, 0) & 
                                              M04_TB_CETERM_3_4 %in% c(1, 0) &
                                              M04_TB_CETERM_4_4 %in% c(1, 0) ~ 1, 
                                            SITE %in% c("India-SAS", "India-CMC") & M04_TB_CETERM_1_4 %in% c(1, 0, NA) & 
                                              M04_TB_CETERM_2_4 %in% c(1, 0, NA) & 
                                              M04_TB_CETERM_3_4 %in% c(1, 0, NA) &
                                              M04_TB_CETERM_4_4 %in% c(1, 0, NA) ~ 1, 
                                            TRUE  ~ 0),
         
         W4SS_SCREEN_PERF_ANC36 = case_when(M04_TB_CETERM_1_5 %in% c(1, 0) & 
                                              M04_TB_CETERM_2_5 %in% c(1, 0) & 
                                              M04_TB_CETERM_3_5 %in% c(1, 0) &
                                              M04_TB_CETERM_4_5 %in% c(1, 0) ~ 1, 
                                            SITE %in% c("India-SAS", "India-CMC") & M04_TB_CETERM_1_5 %in% c(1, 0, NA) & 
                                              M04_TB_CETERM_2_5 %in% c(1, 0, NA) & 
                                              M04_TB_CETERM_3_5 %in% c(1, 0, NA) &
                                              M04_TB_CETERM_4_5 %in% c(1, 0, NA) ~ 1, 
                                            TRUE  ~ 0),
         
         TB_CULTURE_PERF_ENROLL = case_when(M08_TB_CNFRM_LBORRES_1 %in% c(1, 0, 2) ~ 1, TRUE ~ 0),
         TB_CULTURE_PERF_ANC32 = case_when(M08_TB_CNFRM_LBORRES_4 %in% c(1, 0, 2) ~ 1, TRUE ~ 0),
         TB_CULTURE_PERF_ANC36 = case_when(M08_TB_CNFRM_LBORRES_5 %in% c(1, 0, 2) ~ 1, TRUE ~ 0),
         
         W4SS_SCREEN_RESULT_ENROLL = case_when(M04_TB_CETERM_1_1 ==1 | M04_TB_CETERM_2_1 ==1 | M04_TB_CETERM_3_1 ==1 | M04_TB_CETERM_4_1 ==1 ~ 1,
                                               M04_TB_CETERM_1_1 ==0 & M04_TB_CETERM_2_1 ==0 & M04_TB_CETERM_3_1 ==0 & M04_TB_CETERM_4_1 ==0 ~ 0,
                                               TRUE ~ 77),
         W4SS_SCREEN_RESULT_ANC32 = case_when(M04_TB_CETERM_1_4 ==1 | M04_TB_CETERM_2_4 ==1 | M04_TB_CETERM_3_4 ==1 | M04_TB_CETERM_4_4 ==1 ~ 1,
                                              M04_TB_CETERM_1_4 ==0 & M04_TB_CETERM_2_4 ==0 & M04_TB_CETERM_3_4 ==0 & M04_TB_CETERM_4_4 ==0 ~ 0,
                                              TRUE ~ 77),
         W4SS_SCREEN_RESULT_ANC36 = case_when(M04_TB_CETERM_1_5 ==1 | M04_TB_CETERM_2_5 ==1 | M04_TB_CETERM_3_5 ==1 | M04_TB_CETERM_4_5 ==1 ~ 1,
                                              M04_TB_CETERM_1_5 ==0 & M04_TB_CETERM_2_5 ==0 & M04_TB_CETERM_3_5 ==0 & M04_TB_CETERM_4_5 ==0 ~ 0,
                                              TRUE ~ 77)
         
        ) %>% 
  
        rowwise() %>% 
        ## was the visit a remapp aim 3 visit>
        mutate(M08_LB_REMAPP3_4 = case_when(M08_LB_REMAPP3_4 > 1 ~ NA, TRUE ~ M08_LB_REMAPP3_4),
               M08_LB_REMAPP3_5 = case_when(M08_LB_REMAPP3_5 > 1 ~ NA, TRUE ~ M08_LB_REMAPP3_5)
        ) %>% 
        ## sum how many remapp aim 3 visit a participant had
        mutate(sum = sum(across(starts_with("M08_LB_REMAPP3")), na.rm=TRUE)) %>% 
        ## symptom screener
        mutate(W4SS_SCREEN_PERF_AIM3 = case_when(REMAPP_AIM3_ENROLL ==  1 & ((M04_TB_CETERM_1_1 %in% c(1, 0) & M04_TB_CETERM_2_1 %in% c(1, 0) & M04_TB_CETERM_3_1 %in% c(1, 0) & M04_TB_CETERM_4_1 %in% c(1, 0)) |
                                                                         (M04_TB_CETERM_1_2 %in% c(1, 0) & M04_TB_CETERM_2_2 %in% c(1, 0) & M04_TB_CETERM_3_2 %in% c(1, 0) & M04_TB_CETERM_4_2 %in% c(1, 0)) |
                                                                         (M04_TB_CETERM_1_3 %in% c(1, 0) & M04_TB_CETERM_2_3 %in% c(1, 0) & M04_TB_CETERM_3_3 %in% c(1, 0) & M04_TB_CETERM_4_3 %in% c(1, 0)) | 
                                                                         (M04_TB_CETERM_1_4 %in% c(1, 0) & M04_TB_CETERM_2_4 %in% c(1, 0) & M04_TB_CETERM_3_4 %in% c(1, 0) & M04_TB_CETERM_4_4 %in% c(1, 0)) |  
                                                                         (M04_TB_CETERM_1_5 %in% c(1, 0) & M04_TB_CETERM_2_5 %in% c(1, 0) & M04_TB_CETERM_3_5 %in% c(1, 0) & M04_TB_CETERM_4_5 %in% c(1, 0)))  ~ 1,
                                           REMAPP_AIM3_ENROLL ==  1 & ((SITE %in% c("India-SAS", "India-CMC") & M04_TB_CETERM_1_1 %in% c(1, 0, NA) &  M04_TB_CETERM_2_1 %in% c(1, 0, NA) & M04_TB_CETERM_3_1 %in% c(1, 0, NA) & M04_TB_CETERM_4_1 %in% c(1, 0, NA) |
                                                                          SITE %in% c("India-SAS", "India-CMC") & M04_TB_CETERM_1_2 %in% c(1, 0, NA) &  M04_TB_CETERM_2_2 %in% c(1, 0, NA) & M04_TB_CETERM_3_2 %in% c(1, 0, NA) & M04_TB_CETERM_4_2 %in% c(1, 0, NA) |
                                                                          SITE %in% c("India-SAS", "India-CMC") & M04_TB_CETERM_1_3 %in% c(1, 0, NA) &  M04_TB_CETERM_2_3 %in% c(1, 0, NA) & M04_TB_CETERM_3_3 %in% c(1, 0, NA) & M04_TB_CETERM_4_3 %in% c(1, 0, NA) |
                                                                          SITE %in% c("India-SAS", "India-CMC") & M04_TB_CETERM_1_4 %in% c(1, 0, NA) &  M04_TB_CETERM_2_4 %in% c(1, 0, NA) & M04_TB_CETERM_3_4 %in% c(1, 0, NA) & M04_TB_CETERM_4_4 %in% c(1, 0, NA) |
                                                                          SITE %in% c("India-SAS", "India-CMC") & M04_TB_CETERM_1_5 %in% c(1, 0, NA) &  M04_TB_CETERM_2_5 %in% c(1, 0, NA) & M04_TB_CETERM_3_5 %in% c(1, 0, NA) & M04_TB_CETERM_4_5 %in% c(1, 0, NA))) ~ 1,
                                           REMAPP_AIM3_ENROLL ==  0 ~ 77,
                                           TRUE  ~ 0),
         ## was the test perfromed
         TB_CULTURE_PERF_AIM3 = case_when(REMAPP_AIM3_ENROLL ==  1 & (( M08_TB_CNFRM_LBORRES_1 %in% c(1, 0, 2)) |
                                                                        (M08_TB_CNFRM_LBORRES_2 %in% c(1, 0, 2)) | 
                                                                        (M08_TB_CNFRM_LBORRES_3 %in% c(1, 0, 2)) |  
                                                                        (M08_TB_CNFRM_LBORRES_4 %in% c(1, 0, 2)) | 
                                                                        (M08_TB_CNFRM_LBORRES_5 %in% c(1, 0, 2))) ~ 1, 
                                          REMAPP_AIM3_ENROLL ==  0 ~ 77,
                                          TRUE ~ 0),
         ## was sputum collected 
         TB_SPUTUM_COLLECT_AIM3 = case_when(REMAPP_AIM3_ENROLL ==1 & (M07_MAT_TB_SPEC_COLLECT_1 ==1 | M07_MAT_TB_SPEC_COLLECT_2 ==1 | M07_MAT_TB_SPEC_COLLECT_3 ==1 | M07_MAT_TB_SPEC_COLLECT_4 == 1 | M07_MAT_TB_SPEC_COLLECT_5 ==1) ~ 1,
                                            REMAPP_AIM3_ENROLL ==0 ~ 77,
                                            TRUE ~ 0)
  )


df_lab <- df_lab %>% 
  mutate(W4SS_SCREEN_RESULT_ANC36_COMBINED = case_when(W4SS_SCREEN_RESULT_ANC32 ==1 | W4SS_SCREEN_RESULT_ANC36 ==1 ~ 1, TRUE ~ 0),
         W4SS_SCREEN_PERF_ANC36_COMBINED = case_when(W4SS_SCREEN_PERF_ANC32 == 1 | W4SS_SCREEN_PERF_ANC36 ==1 ~ 1, TRUE ~ 0),
         TB_CULTURE_PERF_ANC36_COMBINED = case_when(TB_CULTURE_PERF_ANC32 == 1 | TB_CULTURE_PERF_ANC36 ==1 ~ 1, TRUE ~ 0)
         )

df_lab <- df_lab %>% mutate(ENROLL=1)
#save data
save(df_maternal, file= paste(path_to_save,"/df_maternal", ".RData",sep = ""))
save(df_lab, file= paste(path_to_save,"/df_lab", ".RData",sep = ""))
save(MAT_ENROLL, file= paste(path_to_save,"/MAT_ENROLL", ".RData",sep = ""))
save(mnh06, file= paste(path_to_save,"/mnh06", ".RData",sep = ""))
save(mnh08, file= paste(path_to_save,"/mnh08", ".RData",sep = ""))
