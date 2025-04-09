#*****************************************************************************
#*Lab report 
#*Includes: 
#*1. Lab missingess table
#*2. Data visualization of lab results
#*Author: Xiaoyan Hu
#*Email:xyh@gwu.edu
#*****************************************************************************
library(tidyverse)
library(lubridate)
library(readxl)
library(haven)

#Site data upload date
UploadDate = "2025-04-04"

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

#load mnh09 data for DOB
mnh09 <- read.csv(paste0(path_to_data,"/mnh09_merged.csv")) 

#load MAT_ENROLL 
MAT_ENROLL <- read_xlsx(paste0("Z:/Outcome Data/",UploadDate,"/MAT_ENROLL.xlsx"))

#load MAT_ENDPOINT 
MAT_ENDPOINTS <- read_dta(paste0("Z:/Outcome Data/",UploadDate,"/MAT_ENDPOINTS.dta"))

#load MAT_GDM 
MAT_GDM <- read_dta(paste0("Z:/Outcome Data/",UploadDate,"/MAT_GDM.dta"))

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
         EDD_BOE, BOE_GA_WKS_ENROLL, ENROLL_SCRN_DATE,
         matches("^(ANC\\d+|ENROLL)_PASS_LATE$"), 
         matches("^(ANC\\d+|ENROLL)_LATE_WINDOW")) %>% 
  left_join(MAT_ENDPOINTS %>% select(SITE, MOMID, PREGID, CLOSEOUT_DT, PREG_LOSS, 
                                     MAT_DEATH, PREG_END_GA,
                                     matches("PNC\\d+_PASS_LATE"), 
                                     matches("PNC\\d+_LATE_WINDOW"))) %>%
  left_join(MAT_GDM %>% select(SITE, MOMID, PREGID, DIAB_OVERT, DIAB_OVERT_DX)) %>% #HbA1c at enrollment >=6.5, preexisting overt diabetes
  left_join(mnh06_wide, by = c("MOMID", "PREGID", "SITE")) %>% 
  left_join(mnh08_wide, by = c("MOMID", "PREGID", "SITE")) %>% 
  left_join(mnh04_wide, by = c("MOMID", "PREGID", "SITE")) 

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
         all_of(as.vector(contains(var_list_num$`Variable Name`)))) %>%
  #replace 7s and 5s with NA
  mutate_all(~ if_else(. < 0, NA, .)) %>% 
  #!!!!!! temp solution for wrong use of default value
  mutate_at(vars(starts_with("M08_RBC_G6PD_LBORRES_"), 
                 starts_with("M06_BGLUC_POC_MMOLL_LBORRES_")), 
            ~ifelse(. == 77, NA, .))

#prepare data by replace default value with NA for categorical var (not lab variables)
prep_lab_cat <- df_maternal %>% 
  select(SITE, MOMID, PREGID,
         starts_with("M04_FETAL_LOSS_DSDECOD_"),
         starts_with("M04_HIV_EVER_MHOCCUR_"),
         starts_with("M04_HIV_MHOCCUR_"),
         starts_with("M06_TYPE_VISIT_"),
         starts_with("M06_MAT_VISIT_MNH06_"),
         starts_with("M06_HIV_POC_LBPERF_"),
         starts_with("M08_TYPE_VISIT_"),
         starts_with("M08_MAT_VISIT_MNH08_"),
         all_of(as.vector(contains(var_list_cat$`Variable Name`)))
  ) %>%
  #replace 7s and 5s with NA
  mutate_all(~ if_else(. %in% c(55,77), NA, .)) 

#prepare data by replace default value with NA for date var
prep_lab_date <- df_maternal %>% 
  select(SITE, MOMID, PREGID,
         ENROLL_SCRN_DATE,
         starts_with("M04_FETAL_LOSS_DSSTDAT_"),
         starts_with("M06_DIAG_VSDAT_"),
         starts_with("M08_LBSTDAT"), 
         starts_with("M08_SYPH_TITER_LBTSTDAT_"), 
         starts_with("M08_ZCD_LBTSTDAT_"), 
         starts_with("M08_LEPT_IGM_LBTSTDAT_"),
         starts_with("M08_LEPT_IGG_LBTSTDAT_"),
         starts_with("M08_HEV_LBTSTDAT_"), 
         starts_with("M08_CBC_LBTSTDAT_"), 
         M08_THYROID_LBTSTDAT_4) %>%
  #replace 7s and 5s with NA
  mutate_all(~ if_else(. %in% c("1907-07-07", "1905-05-05"), NA, .)) 

#prepare data by including basic derived variables
pre_lab_other <- df_maternal %>% 
  select(SITE, MOMID, PREGID, PREG_START_DATE, BOE_GA_DAYS_ENROLL, BOE_GA_WKS_ENROLL, 
         CLOSEOUT_DT, PREG_END_GA, MAT_DEATH, PREG_LOSS, DIAB_OVERT, DIAB_OVERT_DX,
         ends_with("_PASS_LATE"), ends_with("_LATE_WINDOW"), contains("M08_LBSTDAT")
         ) %>% 
  #replace with NA 
  mutate_all(~ if_else(. %in% c("1907-07-07"), NA, .)) 

#******prepare df_lab --> data for missingness
df_lab <- prep_lab_num %>% 
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
                                       ((CLOSEOUT_DT > PNC0_LATE_WINDOW) | is.na(CLOSEOUT_DT)), 1, 0),
         VC_PNC1_DENOM_LATE = ifelse(PNC1_PASS_LATE==1 &
                                       ((CLOSEOUT_DT > PNC1_LATE_WINDOW) | is.na(CLOSEOUT_DT)), 1, 0),
         VC_PNC4_DENOM_LATE = ifelse(PNC4_PASS_LATE==1 &
                                       ((CLOSEOUT_DT > PNC4_LATE_WINDOW) | is.na(CLOSEOUT_DT)), 1, 0),
         VC_PNC6_DENOM_LATE = ifelse(PNC6_PASS_LATE==1 & 
                                       ((CLOSEOUT_DT > PNC6_LATE_WINDOW) | is.na(CLOSEOUT_DT)), 1, 0),
         VC_PNC26_DENOM_LATE = ifelse(PNC26_PASS_LATE==1 & PREG_END_GA>139 & 
                                        ((CLOSEOUT_DT > PNC26_LATE_WINDOW) | is.na(CLOSEOUT_DT)), 1, 0),
         
         VC_PNC52_DENOM_LATE = ifelse(PNC52_PASS_LATE==1 & PREG_END_GA>139 & 
                                        ((CLOSEOUT_DT > PNC52_LATE_WINDOW) | is.na(CLOSEOUT_DT)), 1, 0)
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
  mutate(
    denom_remapp = case_when(
      denom_lab_08_1 == 1 & 
        ((SITE == "Ghana" & ENROLL_SCRN_DATE >= "2022-12-28" & ENROLL_SCRN_DATE <= "2024-10-29") | ## end date confirmed added 
           (SITE == "Kenya" & ENROLL_SCRN_DATE >= "2023-04-03") | ## confirmed 
           (SITE == "Zambia" & ENROLL_SCRN_DATE >= "2022-12-15") |
           (SITE == "Pakistan" & ENROLL_SCRN_DATE >= "2022-09-22" & ENROLL_SCRN_DATE <= "2024-04-05") | ## end date confirmed added 
           (SITE == "India-CMC" & ENROLL_SCRN_DATE >= "2023-06-20") |
           (SITE == "India-SAS" & ENROLL_SCRN_DATE >= "2023-12-12")) ~ 1,
      TRUE ~ 0
    )) %>% 
  #define denominator India-SAS post-enrollment CBC start dates 
  mutate(
    denom_post_enrll_cbc = case_when(
      (denom_lab_08_1 == 1 &  SITE == "India-CMC" & ENROLL_SCRN_DATE >= "2024-07-22") ~ 1,
      TRUE ~ denom_remapp
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
        (SITE == "India-SAS" & M08_SYPH_TITER_LBTSTDAT_4 >= "2024-07-29") |
        (SITE == "Kenya" & M08_SYPH_TITER_LBTSTDAT_4 >= "2024-03-06") |  
        (SITE == "Pakistan" & M08_SYPH_TITER_LBTSTDAT_4 >= "2024-10-24") | 
        (SITE == "Zambia" & M08_SYPH_TITER_LBTSTDAT_3 >= "2023-11-09") ~ 1,  #Zambia test on ANC28 
      TRUE ~ 0
    )) %>% 
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
    denom_zcd_1 = case_when(
      (SITE == "Ghana" & (M08_LBSTDAT_1 >= "2024-04-09")) | 
        (SITE == "India-CMC" & (M08_LBSTDAT_1 >= "2024-03-06")) | 
        (SITE == "India-SAS" & (M08_LBSTDAT_1 >= "2024-03-11" & M08_LBSTDAT_1 <= "2025-02-01")) | ## confirmed end date added
        (SITE == "Kenya" & (M08_LBSTDAT_1 >= "2024-03-06")) | 
        (SITE == "Pakistan" & (M08_LBSTDAT_1 >= "2024-04-08")) |
        (SITE == "Zambia" & (M08_LBSTDAT_1 >= "2023-11-09")) ~ 1,
      TRUE ~ 0
    ),
    denom_zcd_4 = case_when(
      (SITE == "Ghana" & (M08_LBSTDAT_4 >= "2024-04-09")) | 
        (SITE == "India-CMC" & (M08_LBSTDAT_4 >= "2024-03-06")) | 
        (SITE == "India-SAS" & (M08_LBSTDAT_4 >= "2024-03-11" & M08_LBSTDAT_4 <= "2025-02-01")) | ## confirmed end date added
        (SITE == "Kenya" & (M08_LBSTDAT_4 >= "2024-03-06")) | 
        (SITE == "Pakistan" & (M08_LBSTDAT_4 >= "2024-04-08")) |
        (SITE == "Zambia" & (M08_LBSTDAT_4 >= "2023-11-09")) ~ 1,
      TRUE ~ 0
    ),
    denom_zcd_10 = case_when(
      (SITE == "Ghana" & (M08_LBSTDAT_10 >= "2024-04-09")) | 
        (SITE == "India-CMC" & (M08_LBSTDAT_10 >= "2024-03-06")) | 
        (SITE == "India-SAS" & (M08_LBSTDAT_10 >= "2024-03-11" & M08_LBSTDAT_10 <= "2025-02-01")) | ## confirmed end date added
        (SITE == "Kenya" & (M08_LBSTDAT_10 >= "2024-03-06")) | 
        (SITE == "Pakistan" & (M08_LBSTDAT_10 >= "2024-04-08")) |
        (SITE == "Zambia" & (M08_LBSTDAT_10 >= "2023-11-09")) ~ 1,
      TRUE ~ 0
    ),
    
    
    ) %>% 
#   mutate(
#     denom_zcd= case_when(
#       (SITE == "Ghana" & (M08_LBSTDAT >= "2024-04-09")) | 
#         (SITE == "India-CMC" & (M08_ZCD_LBTSTDAT_1 >= "2024-03-06")) | 
#         (SITE == "India-SAS" & (M08_ZCD_LBTSTDAT_1 >= "2024-03-12" & M08_ZCD_LBTSTDAT_1 <= "2025-02-01")) | ## confirmed end date added
#         (SITE == "Kenya" & (M08_ZCD_LBTSTDAT_1 >= "2024-03-06")) | 
#         (SITE == "Pakistan" & (M08_ZCD_LBTSTDAT_1 >= "2024-04-08")) |
#         (SITE == "Zambia" & (M08_ZCD_LBTSTDAT_1 >= "2023-11-09")) ~ 1,
#       TRUE ~ 0
#     )) %>% 
#   #define denominator zcd/lepto/hev (denom_lept_igm, denom_lept_igg, denom_hev)
# mutate(
#   denom_lept_igm= case_when(
#     (SITE == "Ghana" & (M08_LEPT_IGM_LBTSTDAT_1 >= "2024-04-09")) | 
#       (SITE == "India-CMC" & (M08_LEPT_IGM_LBTSTDAT_1 >= "2024-03-06")) | 
#       (SITE == "India-SAS" & (M08_LEPT_IGM_LBTSTDAT_1 >= "2024-03-12" & M08_ZCD_LBTSTDAT_1 <= "2025-02-01")) | ## confirmed end date added
#       (SITE == "Kenya" & (M08_LEPT_IGM_LBTSTDAT_1 >= "2024-03-06")) | 
#       (SITE == "Pakistan" & (M08_LEPT_IGM_LBTSTDAT_1 >= "2024-04-08")) | 
#       (SITE == "Zambia" & (M08_LEPT_IGM_LBTSTDAT_1 >= "2023-11-09")) ~ 1, 
#     TRUE ~ 0 
#   )) %>% 
#   #define denominator for leptospirosis igg test 
# mutate(
#   denom_lept_igg= case_when(
#     (SITE == "Ghana" & (M08_LEPT_IGG_LBTSTDAT_1 >= "2024-04-09")) | 
#       (SITE == "India-CMC" & (M08_LEPT_IGG_LBTSTDAT_1 >= "2024-03-06")) | 
#       (SITE == "India-SAS" & (M08_LEPT_IGG_LBTSTDAT_1 >= "2024-03-12" & M08_ZCD_LBTSTDAT_1 <= "2025-02-01")) | ## confirmed end date added
#       (SITE == "Kenya" & (M08_LEPT_IGG_LBTSTDAT_1 >= "2024-03-06")) | 
#       (SITE == "Pakistan" & (M08_LEPT_IGG_LBTSTDAT_1 >= "2024-04-08")) |
#       (SITE == "Zambia" & (M08_LEPT_IGG_LBTSTDAT_1 >= "2023-11-09")) ~ 1,
#     TRUE ~ 0 
#   )) %>% 
# #define denominator for hev test 
# mutate(
#   denom_hev= case_when(
#     (SITE == "Ghana" & (M08_HEV_LBTSTDAT_1 >= "2024-04-09")) | 
#       (SITE == "India-CMC" & (M08_HEV_LBTSTDAT_1 >= "2024-03-06")) | 
#       (SITE == "India-SAS" & (M08_HEV_LBTSTDAT_1 >= "2024-03-12" & M08_ZCD_LBTSTDAT_1 <= "2025-02-01")) | ## confirmed end date added
#       (SITE == "Kenya" & (M08_HEV_LBTSTDAT_1 >= "2024-03-06")) | 
#       (SITE == "Pakistan" & (M08_HEV_LBTSTDAT_1 >= "2024-04-08")) |
#       (SITE == "Zambia" & (M08_HEV_LBTSTDAT_1 >= "2023-11-09")) ~ 1,
#     TRUE ~ 0
#   )) %>% 
  #define denominator for RBC disorder test 
  mutate(
    denom_rbc_disorder = case_when(
      denom_lab_08_1 == 1 & 
        ((SITE == "Ghana" & ENROLL_SCRN_DATE >= "2022-12-28" & ENROLL_SCRN_DATE <= "2024-10-29") | ## end date confirmed added 
           (SITE == "Kenya" & ENROLL_SCRN_DATE >= "2023-04-3") | ## should this be 3 April 
           (SITE == "Zambia" & ENROLL_SCRN_DATE >= "2022-12-15") |
           (SITE == "Pakistan" & M08_LBSTDAT_1 >= "2022-09-22" & M08_LBSTDAT_1 < "2024-04-05") | ## confirmed 5 April
           (SITE == "India-CMC" & ENROLL_SCRN_DATE >= "2023-06-20") |
           (SITE == "India-SAS" & ENROLL_SCRN_DATE >= "2023-12-12")) ~ 1,
      TRUE ~ 0
    )) %>% 
  #define denominator for free t3 and free t4 at ANC32
  mutate(
    denom_t3t4_anc32 = case_when(
      denom_lab_08_4 == 1 & 
        ((SITE == "Ghana" & (M08_THYROID_LBTSTDAT_4 <= "2024-09-01" |  (M08_THYROID_LBTSTDAT_4 > "2024-09-01" & 
                             (M08_THYROID_TSH_LBORRES_4 < 0.3 | M08_THYROID_TSH_LBORRES_4 > 4)))) |
           (SITE == "Kenya" & (M08_THYROID_LBTSTDAT_4 <= "2025-01-14" |  (M08_THYROID_LBTSTDAT_4 > "2025-01-14" & 
                              (M08_THYROID_TSH_LBORRES_4 < 0.3 | M08_THYROID_TSH_LBORRES_4 > 4)))) |
           (SITE == "Zambia" & (M08_THYROID_LBTSTDAT_4 <= "2024-10-11" |  (M08_THYROID_LBTSTDAT_4 > "2024-10-11" & 
                                (M08_THYROID_TSH_LBORRES_4 < 0.3 | M08_THYROID_TSH_LBORRES_4 > 4)))) |
           (SITE == "Pakistan" & (M08_THYROID_LBTSTDAT_4 <= "2024-10-3" |  (M08_THYROID_LBTSTDAT_4 > "2024-10-03" & 
                                 (M08_THYROID_TSH_LBORRES_4 < 0.3 | M08_THYROID_TSH_LBORRES_4 > 4)))) |
           (SITE == "India-CMC" & (M08_THYROID_LBTSTDAT_4 <= "2024-10-14" |  (M08_THYROID_LBTSTDAT_4 > "2024-10-14" & ## should this be 10-16?
                                    (M08_THYROID_TSH_LBORRES_4 < 0.3 | M08_THYROID_TSH_LBORRES_4 > 4)))) |
           (SITE == "India-SAS")) ~ 1,
      TRUE ~ 0
    )) %>% 
  #define denominator for free t3 and free t4 at enrollment
  mutate(
    denom_t3t4_enroll = case_when(
      denom_lab_08_1 == 1 & 
        ((SITE == "Ghana" & M08_THYROID_LBTSTDAT_4 <= "2024-09-01") | 
           (SITE == "Kenya" &  M08_THYROID_LBTSTDAT_4 <= "2025-01-14") |
           (SITE == "Zambia" &  M08_THYROID_LBTSTDAT_4 <= "2024-10-11") |
           (SITE == "Pakistan" &  M08_THYROID_LBTSTDAT_4 <= "2024-10-03") |
           (SITE == "India-CMC" &  M08_THYROID_LBTSTDAT_4 <= "2024-10-17") |
           (SITE == "India-SAS")) ~ 1,
      TRUE ~ 0
    )) %>% 
  #define denominator for kidney & liver function tests at ANC32 
  mutate(
    denom_kft_lft_fxn_anc32 = case_when(
      denom_lab_08_4 == 1 & 
        ((SITE == "Ghana" & M08_THYROID_LBTSTDAT_4 <= "2024-10-01") | 
           (SITE == "Kenya" &  M08_THYROID_LBTSTDAT_4 <= "2025-01-14") |
           (SITE == "Zambia" &  M08_THYROID_LBTSTDAT_4 <= "2024-10-11") |
           (SITE == "Pakistan" &  M08_THYROID_LBTSTDAT_4 <= "2024-10-03") |
           (SITE == "India-CMC" &  M08_THYROID_LBTSTDAT_4 <= "2024-10-16") |
           (SITE == "India-SAS" &  M08_THYROID_LBTSTDAT_4 <= "2024-09-02")) ~ 1,
      TRUE ~ 0
    )) %>% 
  #define denominator for holo TC @ Enroll/ANC32 
  mutate(
    denom_holo_1 = case_when(
      denom_lab_08_1 == 1 & 
        ((SITE == "Ghana" & M08_THYROID_LBTSTDAT_4 <= "2024-09-01") | 
           (SITE == "Kenya" &  M08_THYROID_LBTSTDAT_4 <= "2025-01-14") |
           (SITE == "Zambia" &  M08_THYROID_LBTSTDAT_4 <= "2024-10-11") |
           (SITE == "Pakistan" &  M08_THYROID_LBTSTDAT_4 <= "2024-10-03") |
           (SITE == "India-CMC" &  M08_THYROID_LBTSTDAT_4 <= "2024-10-15") |
           (SITE == "India-SAS" &  M08_THYROID_LBTSTDAT_4 <= "2024-09-02")) ~ 1,
      TRUE ~ 0
    ),
    denom_holo_4 = case_when(
      denom_lab_08_4 == 1 & 
        ((SITE == "Ghana" & M08_THYROID_LBTSTDAT_4 <= "2024-09-01") | 
           (SITE == "Kenya" &  M08_THYROID_LBTSTDAT_4 <= "2025-01-14") |
           (SITE == "Zambia" &  M08_THYROID_LBTSTDAT_4 <= "2024-10-11") |
           (SITE == "Pakistan" &  M08_THYROID_LBTSTDAT_4 <= "2024-10-03") |
           (SITE == "India-CMC" &  M08_THYROID_LBTSTDAT_4 <= "2024-10-15") |
           (SITE == "India-SAS" &  M08_THYROID_LBTSTDAT_4 <= "2024-09-02")) ~ 1,
      TRUE ~ 0
    ),
    ) %>% 
  # #define denominator for kenya carbon dioxide AND carbon dioxide for SAS at enrollment and ANC32
  mutate(co2_denom_lab_08_1 = case_when(!SITE %in% c("Kenya", "India-SAS") & denom_lab_08_1 == 1 ~ 1,
                                        (SITE ==  "Kenya" & denom_lab_08_1 == 1 & M08_LBSTDAT_1 >="2023-04-01") |
                                        (SITE ==  "India-SAS" & denom_lab_08_1 == 1 & M08_LBSTDAT_1 >="2024-07-13")
                                          ~ 1, TRUE ~ 0
                                        ),
         
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
  )


backup = df_lab
# export data
# save(df_lab, file = paste0(path_to_save, "/df_lab.RData"))
save(df_lab, file= paste(path_to_save,"/df_lab.RData",sep = ""))


#********************************************************************************
#*Prepare data for plots 
#********************************************************************************
#change data to long format for mnh06
df_lab_06 <- df_lab %>% 
  select(-matches("PASS_LATE"), -starts_with("M08_")) %>%
  pivot_longer(
    -c("MOMID","PREGID","SITE",  PREG_START_DATE),
    names_to = c(".value", "visit_type"), 
    names_pattern = "^M\\d{2}_(.+)_(\\d+)"
  ) %>% 
  mutate(
    ga_wks = case_when(
      TYPE_VISIT >= 6 ~ NA_real_,
      TYPE_VISIT < 6 ~ as.numeric(ymd(DIAG_VSDAT) - ymd(PREG_START_DATE))/7
    ),
    trimester = case_when(
      #at/after delivery data should be NA for trimester
      ga_wks > 3 & ga_wks < 14 ~ 1,
      ga_wks >= 14 & ga_wks < 28 ~ 2,
      ga_wks >= 28 & ga_wks < 43 ~ 3, 
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
  ) %>% 
  filter(time > 0) #remove date errors which should be flagged in query report

#change data to long format for mnh08
df_lab_08 <- df_lab %>% 
  select(-matches("PASS_LATE"), -starts_with("M06_")) %>%
  pivot_longer(
    -c("MOMID","PREGID","SITE",  PREG_START_DATE),
    names_to = c(".value", "visit_type"), 
    names_pattern = "^M\\d{2}_(.+)_(\\d+)"
  ) %>% 
  mutate(
    ga_wks = case_when(
      TYPE_VISIT >= 6 ~ NA_real_,
      TYPE_VISIT < 6 ~ as.numeric(ymd(LBSTDAT) - ymd(PREG_START_DATE))/7
    ),
    trimester = case_when(
      #at/after delivery data should be NA for trimester
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
      trimester == 1 & CBC_HCT_LBORRES < 31 ~ 1, #low
      trimester == 1 & CBC_HCT_LBORRES >= 31 & CBC_HCT_LBORRES <= 41 ~ 2, #normal
      trimester == 1 & CBC_HCT_LBORRES > 41 ~ 3, #high
      trimester == 2 & CBC_HCT_LBORRES < 30 ~ 1, #low
      trimester == 2 & CBC_HCT_LBORRES >= 30 & CBC_HCT_LBORRES <= 39 ~ 2, #normal
      trimester == 2 & CBC_HCT_LBORRES > 39 ~ 3, #high
      trimester == 3 & CBC_HCT_LBORRES < 28 ~ 1, #low
      trimester == 3 & CBC_HCT_LBORRES >= 28 & CBC_HCT_LBORRES <= 40 ~ 2, #normal
      trimester == 3 & CBC_HCT_LBORRES > 40 ~ 3, #high
      TRUE ~ NA_real_
    ),
    #White Blood Cell (WBC): x10³/mm³
    wbc_level = case_when(
      trimester == 1 & CBC_WBC_LBORRES < 5.7 ~ 1, #low
      trimester == 1 & CBC_WBC_LBORRES >= 5.7 & CBC_WBC_LBORRES <= 13.6 ~ 2, #normal
      trimester == 1 & CBC_WBC_LBORRES > 13.6 ~ 3, #high
      trimester == 2 & CBC_WBC_LBORRES < 5.6 ~ 1, #low
      trimester == 2 & CBC_WBC_LBORRES >= 5.6 & CBC_WBC_LBORRES <= 14.8 ~ 2, #normal
      trimester == 2 & CBC_WBC_LBORRES > 14.8 ~ 3, #high
      trimester == 3 & CBC_WBC_LBORRES < 5.6 ~ 1, #low
      trimester == 3 & CBC_WBC_LBORRES >= 5.6 & CBC_WBC_LBORRES <= 16.9 ~ 2, #normal
      trimester == 3 & CBC_WBC_LBORRES > 16.9 ~ 3, #high
      TRUE ~ NA_real_
    ),
    #Neutrophils (full cell count) x10³/mm³
    neutrophils_level = case_when(
      trimester == 1 & CBC_NEU_FCC_LBORRES < 3.6 ~ 1, #low
      trimester == 1 & CBC_NEU_FCC_LBORRES >= 3.6 & CBC_NEU_FCC_LBORRES <= 10.1 ~ 2, #normal
      trimester == 1 & CBC_NEU_FCC_LBORRES > 10.1 ~ 3, #high
      trimester == 2 & CBC_NEU_FCC_LBORRES < 3.8 ~ 1, #low
      trimester == 2 & CBC_NEU_FCC_LBORRES >= 3.8 & CBC_NEU_FCC_LBORRES <= 12.3 ~ 2, #normal
      trimester == 2 & CBC_NEU_FCC_LBORRES > 12.3 ~ 3, #high
      trimester == 3 & CBC_NEU_FCC_LBORRES < 3.9 ~ 1, #low
      trimester == 3 & CBC_NEU_FCC_LBORRES >= 3.9 & CBC_NEU_FCC_LBORRES <= 13.1 ~ 2, #normal
      trimester == 3 & CBC_NEU_FCC_LBORRES > 13.1 ~ 3, #high
      TRUE ~ NA_real_
    ),
    #Lymphocyte (full cell count) x10³/mm³
    lymphocyte_level = case_when(
      trimester == 1 & CBC_LYMPH_FCC_LBORRES < 1.1 ~ 1, #low
      trimester == 1 & CBC_LYMPH_FCC_LBORRES >= 1.1 & CBC_LYMPH_FCC_LBORRES <= 3.6 ~ 2, #normal
      trimester == 1 & CBC_LYMPH_FCC_LBORRES > 3.6 ~ 3, #high
      trimester == 2 & CBC_LYMPH_FCC_LBORRES < 0.9 ~ 1, #low
      trimester == 2 & CBC_LYMPH_FCC_LBORRES >= 0.9 & CBC_LYMPH_FCC_LBORRES <= 3.9 ~ 2, #normal
      trimester == 2 & CBC_LYMPH_FCC_LBORRES > 3.9 ~ 3, #high
      trimester == 3 & CBC_LYMPH_FCC_LBORRES < 1 ~ 1, #low
      trimester == 3 & CBC_LYMPH_FCC_LBORRES >= 1 & CBC_LYMPH_FCC_LBORRES <= 3.6 ~ 2, #normal
      trimester == 3 & CBC_LYMPH_FCC_LBORRES > 3.6 ~ 3, #high
      TRUE ~ NA_real_
    ),
    #Mean cell volume (MCV): unit: µm³
    mcv_level = case_when(
      trimester == 1 & CBC_MCV_LBORRES < 85 ~ 1, #low
      trimester == 1 & CBC_MCV_LBORRES >= 85 & CBC_MCV_LBORRES <= 97.9 ~ 2, #normal
      trimester == 1 & CBC_MCV_LBORRES > 97.8 ~ 3, #high
      trimester == 2 & CBC_MCV_LBORRES < 85.8 ~ 1, #low
      trimester == 2 & CBC_MCV_LBORRES >= 85.8 & CBC_MCV_LBORRES <= 99.4 ~ 2, #normal
      trimester == 2 & CBC_MCV_LBORRES > 99.4 ~ 3, #high
      trimester == 3 & CBC_MCV_LBORRES < 82.4 ~ 1, #low
      trimester == 3 & CBC_MCV_LBORRES >= 82.4 & CBC_MCV_LBORRES <= 100.4 ~ 2, #normal
      trimester == 3 & CBC_MCV_LBORRES > 100.4 ~ 3, #high
      TRUE ~ NA_real_
    ),
    #Mean cell hemoglobin (MCH) pg/cell
    mch_level = case_when(
      trimester == 1 & CBC_MCH_LBORRES < 30 ~ 1, #low
      trimester == 1 & CBC_MCH_LBORRES >= 30 & CBC_MCH_LBORRES <= 32 ~ 2, #normal
      trimester == 1 & CBC_MCH_LBORRES > 32 ~ 3, #high
      trimester == 2 & CBC_MCH_LBORRES < 30 ~ 1, #low
      trimester == 2 & CBC_MCH_LBORRES >= 30 & CBC_MCH_LBORRES <= 33 ~ 2, #normal
      trimester == 2 & CBC_MCH_LBORRES > 33 ~ 3, #high
      trimester == 3 & CBC_MCH_LBORRES < 29 ~ 1, #low
      trimester == 3 & CBC_MCH_LBORRES >= 29 & CBC_MCH_LBORRES <= 32 ~ 2, #normal
      trimester == 3 & CBC_MCH_LBORRES > 32 ~ 3, #high
      TRUE ~ NA_real_
    ),
    #Mean corpuscular hemoglobin concentration (MCHC) g/dL
    mchc_level = case_when(
      trimester == 1 & CBC_MCHC_GDL_LBORRES < 32.5 ~ 1, #low
      trimester == 1 & CBC_MCHC_GDL_LBORRES >= 32.5 & CBC_MCHC_GDL_LBORRES <= 35.3 ~ 2, #normal
      trimester == 1 & CBC_MCHC_GDL_LBORRES > 35.3 ~ 3, #high
      trimester == 2 & CBC_MCHC_GDL_LBORRES < 32.4 ~ 1, #low
      trimester == 2 & CBC_MCHC_GDL_LBORRES >= 32.4 & CBC_MCHC_GDL_LBORRES <= 35.2 ~ 2, #normal
      trimester == 2 & CBC_MCHC_GDL_LBORRES > 35.2 ~ 3, #high
      trimester == 3 & CBC_MCHC_GDL_LBORRES < 31.9 ~ 1, #low
      trimester == 3 & CBC_MCHC_GDL_LBORRES >= 31.9 & CBC_MCHC_GDL_LBORRES <= 35.5 ~ 2, #normal
      trimester == 3 & CBC_MCHC_GDL_LBORRES > 35.5 ~ 3, #high
      TRUE ~ NA_real_
    ),
    #Platelets count unit: x10³/mm³
    platelets_level = case_when(
      trimester == 1 & CBC_PLATE_LBORRES < 174 ~ 1, #low
      trimester == 1 & CBC_PLATE_LBORRES >= 174 & CBC_PLATE_LBORRES <= 391 ~ 2, #normal
      trimester == 1 & CBC_PLATE_LBORRES > 391 ~ 3, #high
      trimester == 2 & CBC_PLATE_LBORRES < 155 ~ 1, #low
      trimester == 2 & CBC_PLATE_LBORRES >= 155 & CBC_PLATE_LBORRES <= 409 ~ 2, #normal
      trimester == 2 & CBC_PLATE_LBORRES > 409 ~ 3, #high
      trimester == 3 & CBC_PLATE_LBORRES < 146 ~ 1, #low
      trimester == 3 & CBC_PLATE_LBORRES >= 146 & CBC_PLATE_LBORRES <= 429 ~ 2, #normal
      trimester == 3 & CBC_PLATE_LBORRES > 429 ~ 3, #high
      TRUE ~ NA_real_
    ),
    #Monocyte (full cell count) x10³/mm³
    monocyte_level = case_when(
      trimester == 1 & CBC_MONO_FCC_LBORRES < 0.1 ~ 1, #low
      trimester == 1 & CBC_MONO_FCC_LBORRES >= 0.1 & CBC_MONO_FCC_LBORRES <= 1.1 ~ 2, #normal
      trimester == 1 & CBC_MONO_FCC_LBORRES > 1.1 ~ 3, #high
      trimester == 2 & CBC_MONO_FCC_LBORRES < 0.1 ~ 1, #low
      trimester == 2 & CBC_MONO_FCC_LBORRES >= 0.1 & CBC_MONO_FCC_LBORRES <= 1.1 ~ 2, #normal
      trimester == 2 & CBC_MONO_FCC_LBORRES > 1.1 ~ 3, #high
      trimester == 3 & CBC_MONO_FCC_LBORRES < 0.1 ~ 1, #low
      trimester == 3 & CBC_MONO_FCC_LBORRES >= 0.1 & CBC_MONO_FCC_LBORRES <= 1.4 ~ 2, #normal
      trimester == 3 & CBC_MONO_FCC_LBORRES > 1.4 ~ 3, #high
      TRUE ~ NA_real_
    ),
    #Eosinophils (full cell count) x10³/mm³
    eosinophils_level = case_when(
      trimester == 1 & CBC_EOS_FCC_LBORRES < 0 ~ 1, #low
      trimester == 1 & CBC_EOS_FCC_LBORRES >= 0 & CBC_EOS_FCC_LBORRES <= 0.6 ~ 2, #normal
      trimester == 1 & CBC_EOS_FCC_LBORRES > 0.6 ~ 3, #high
      trimester == 2 & CBC_EOS_FCC_LBORRES < 0 ~ 1, #low
      trimester == 2 & CBC_EOS_FCC_LBORRES >= 0 & CBC_EOS_FCC_LBORRES <= 0.6 ~ 2, #normal
      trimester == 2 & CBC_EOS_FCC_LBORRES > 0.6 ~ 3, #high
      trimester == 3 & CBC_EOS_FCC_LBORRES < 0 ~ 1, #low
      trimester == 3 & CBC_EOS_FCC_LBORRES >= 0 & CBC_EOS_FCC_LBORRES <= 0.6 ~ 2, #normal
      trimester == 3 & CBC_EOS_FCC_LBORRES > 0.6 ~ 3, #high
      TRUE ~ NA_real_
    ),
    #Red cell width (RDW) %
    rdw_level = case_when(
      trimester == 1 & CBC_RDW_PCT_LBORRES < 11.7 ~ 1, #low
      trimester == 1 & CBC_RDW_PCT_LBORRES >= 11.7 & CBC_RDW_PCT_LBORRES <= 14.9 ~ 2, #normal
      trimester == 1 & CBC_RDW_PCT_LBORRES > 14.9 ~ 3, #high
      trimester == 2 & CBC_RDW_PCT_LBORRES < 12.3 ~ 1, #low
      trimester == 2 & CBC_RDW_PCT_LBORRES >= 12.3 & CBC_RDW_PCT_LBORRES <= 14.7 ~ 2, #normal
      trimester == 2 & CBC_RDW_PCT_LBORRES > 14.7 ~ 3, #high
      trimester == 3 & CBC_RDW_PCT_LBORRES < 11.4 ~ 1, #low
      trimester == 3 & CBC_RDW_PCT_LBORRES >= 11.4 & CBC_RDW_PCT_LBORRES <= 16.6 ~ 2, #normal
      trimester == 3 & CBC_RDW_PCT_LBORRES > 16.6 ~ 3, #high
      TRUE ~ NA_real_
    ),
    #Ferritin µg/dL
    ferritin_level = case_when(
      trimester == 1 & FERRITIN_LBORRES < 0.6 ~ 1, #low
      trimester == 1 & FERRITIN_LBORRES >= 0.6 & FERRITIN_LBORRES <= 12.5 ~ 2, #normal
      trimester == 1 & FERRITIN_LBORRES > 12.5 ~ 3, #high
      trimester == 2 & FERRITIN_LBORRES < 0.6 ~ 1, #low
      trimester == 2 & FERRITIN_LBORRES >= 0.6 & FERRITIN_LBORRES <= 7.4 ~ 2, #normal
      trimester == 2 & FERRITIN_LBORRES > 7.4 ~ 3, #high
      trimester == 3 & FERRITIN_LBORRES < 0.3 ~ 1, #low
      trimester == 3 & FERRITIN_LBORRES >= 0.3 & FERRITIN_LBORRES <= 5.8 ~ 2, #normal
      trimester == 3 & FERRITIN_LBORRES > 5.8 ~ 3, #high
      TRUE ~ NA_real_
    ),
    #Serum B12 total cobalamin pg/mL
    b12tc_level = case_when(
      trimester == 1 & VITB12_COB_LBORRES < 118 ~ 1, #low
      trimester == 1 & VITB12_COB_LBORRES >= 118 & VITB12_COB_LBORRES <= 438 ~ 2, #normal
      trimester == 1 & VITB12_COB_LBORRES > 438 ~ 3, #high
      trimester == 2 & VITB12_COB_LBORRES < 130 ~ 1, #low
      trimester == 2 & VITB12_COB_LBORRES >= 130 & VITB12_COB_LBORRES <= 656 ~ 2, #normal
      trimester == 2 & VITB12_COB_LBORRES > 656 ~ 3, #high
      trimester == 3 & VITB12_COB_LBORRES < 99 ~ 1, #low
      trimester == 3 & VITB12_COB_LBORRES >= 99 & VITB12_COB_LBORRES <= 526 ~ 2, #normal
      trimester == 3 & VITB12_COB_LBORRES > 526 ~ 3, #high
      TRUE ~ NA_real_
    ),
    #Free T4 ng/dL
    freet4_level = case_when(
      trimester == 1 & THYROID_FREET4_LBORRES < 0.55 ~ 1, #low
      trimester == 1 & THYROID_FREET4_LBORRES >= 0.55 & THYROID_FREET4_LBORRES <= 1.37 ~ 2, #normal
      trimester == 1 & THYROID_FREET4_LBORRES > 1.37 ~ 3, #high
      trimester == 2 & THYROID_FREET4_LBORRES < 0.55 ~ 1, #low
      trimester == 2 & THYROID_FREET4_LBORRES >= 0.55 & THYROID_FREET4_LBORRES <= 1.09 ~ 2, #normal
      trimester == 2 & THYROID_FREET4_LBORRES > 1.09 ~ 3, #high
      trimester == 3 & THYROID_FREET4_LBORRES < 0.55 ~ 1, #low
      trimester == 3 & THYROID_FREET4_LBORRES >= 0.55 & THYROID_FREET4_LBORRES <= 1.09 ~ 2, #normal
      trimester == 3 & THYROID_FREET4_LBORRES > 1.09 ~ 3, #high
      TRUE ~ NA_real_
    )
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

#******Prepare df_lab_08_l --> data for visualization for labs in MNH06
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
save(df_maternal, file= paste(path_to_save,"/df_maternal", ".RData",sep = ""))
save(df_lab, file= paste(path_to_save,"/df_lab", ".RData",sep = ""))
save(df_lab_06_l, file= paste(path_to_save,"/df_lab_06_l", ".RData",sep = ""))
save(df_lab_08_l, file= paste(path_to_save,"/df_lab_08_l", ".RData",sep = ""))
save(MAT_ENROLL, file= paste(path_to_save,"/MAT_ENROLL", ".RData",sep = ""))
save(mnh06, file= paste(path_to_save,"/mnh06", ".RData",sep = ""))
save(mnh08, file= paste(path_to_save,"/mnh08", ".RData",sep = ""))

# save(df_maternal, file = "derived_data/df_maternal.rda")
# save(df_lab, file = "derived_data/df_lab.rda")
# save(df_lab_06_l, file = "derived_data/df_lab_06_l.rda")
# save(df_lab_08_l, file = "derived_data/df_lab_08_l.rda")
# save(MAT_ENROLL, file = "derived_data/MAT_ENROLL.rda")
# save(mnh06, file = "derived_data/mnh06.rda")
# save(mnh08, file = "derived_data/mnh08.rda")



#*****************table 1******************
matrix1 <- df_lab %>% 
  group_by(SITE) %>% 
  summarise(
    #en - expect forms as long as enrolled
    "Expected Form: N" = n(),
    "Missing MNH06: n (%)" = paste0(
      sum(is.na(M06_TYPE_VISIT_1), na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M06_TYPE_VISIT_1), na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),
    "Missing MNH08: n (%)" = paste0(
      sum(is.na(M08_TYPE_VISIT_1), na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M08_TYPE_VISIT_1), na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),
    #anc20
    "Expected Form: N " = sum(denom_form_2 == 1, na.rm = TRUE),
    "Missing MNH06: n (%) " = paste0(
      sum(is.na(M06_TYPE_VISIT_2) & denom_form_2 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M06_TYPE_VISIT_2) & denom_form_2 == 1, na.rm = TRUE)/sum(denom_form_2 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    "Missing MNH08: n (%) " = paste0(
      sum(is.na(M08_TYPE_VISIT_2) & denom_form_2 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M08_TYPE_VISIT_2) & denom_form_2 == 1, na.rm = TRUE)/sum(denom_form_2 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    #anc28
    "Expected Form: N  " = sum(denom_form_3 == 1, na.rm = TRUE),
    "Missing MNH06: n (%)  " = paste0(
      sum(is.na(M06_TYPE_VISIT_3) & denom_form_3 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M06_TYPE_VISIT_3) & denom_form_3 == 1, na.rm = TRUE)/sum(denom_form_3 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    "Missing MNH08: n (%)  " = paste0(
      sum(is.na(M08_TYPE_VISIT_3) & denom_form_3 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M08_TYPE_VISIT_3) & denom_form_3 == 1, na.rm = TRUE)/sum(denom_form_3 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    #anc32
    "Expected Form: N   " = sum(denom_form_4 == 1, na.rm = TRUE),
    "Missing MNH06: n (%)   " = paste0(
      sum(is.na(M06_TYPE_VISIT_4) & denom_form_4 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M06_TYPE_VISIT_4) & denom_form_4 == 1, na.rm = TRUE)/sum(denom_form_4 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    "Missing MNH08: n (%)   " = paste0(
      sum(is.na(M08_TYPE_VISIT_4) & denom_form_4 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M08_TYPE_VISIT_4) & denom_form_4 == 1, na.rm = TRUE)/sum(denom_form_4 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    #anc36
    "Expected Form: N    " = sum(denom_form_5 == 1, na.rm = TRUE),
    "Missing MNH06: n (%)    " = paste0(
      sum(is.na(M06_TYPE_VISIT_5) & denom_form_5 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M06_TYPE_VISIT_5) & denom_form_5 == 1, na.rm = TRUE)/sum(denom_form_5 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    "Missing MNH08: n (%)    " = paste0(
      sum(is.na(M08_TYPE_VISIT_5) & denom_form_5 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M08_TYPE_VISIT_5) & denom_form_5 == 1, na.rm = TRUE)/sum(denom_form_5 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    #pnc0
    "Expected Form: N     " = sum(denom_form_7 == 1, na.rm = TRUE),
    "Missing MNH06: n (%)     " = paste0(
      sum(is.na(M06_TYPE_VISIT_7) & denom_form_7 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M06_TYPE_VISIT_7) & denom_form_7 == 1, na.rm = TRUE)/sum(denom_form_7 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    "Missing MNH08: n (%)     " = paste0(
      sum(is.na(M08_TYPE_VISIT_7) & denom_form_7 == 1 & SITE %in% c("Ghana", "India-CMC", "Zambia"), na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M08_TYPE_VISIT_7) & denom_form_7 == 1 & SITE %in% c("Ghana", "India-CMC", "Zambia"), na.rm = TRUE)/
                     sum(denom_form_7 == 1 & SITE %in% c("Ghana", "India-CMC", "Zambia"), na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    #pnc1
    "Expected Form: N      " = sum(denom_form_8 == 1, na.rm = TRUE),
    "Missing MNH06: n (%)      " = paste0(
      sum(is.na(M06_TYPE_VISIT_8) & denom_form_8 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M06_TYPE_VISIT_8) & denom_form_8 == 1, na.rm = TRUE)/sum(denom_form_8 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    "Missing MNH08: n (%)      " = paste0(
      sum(is.na(M08_TYPE_VISIT_8) & denom_form_8 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M08_TYPE_VISIT_8) & denom_form_8 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE)/
                     sum(denom_form_8 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    #pnc4
    "Expected Form: N       " = sum(denom_form_9 == 1, na.rm = TRUE),
    "Missing MNH06: n (%)       " = paste0(
      sum(is.na(M06_TYPE_VISIT_9) & denom_form_9 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M06_TYPE_VISIT_9) & denom_form_9 == 1, na.rm = TRUE)/sum(denom_form_9 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    "Missing MNH08: n (%)       " = paste0(
      sum(is.na(M08_TYPE_VISIT_9) & denom_form_9 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M08_TYPE_VISIT_9) & denom_form_9 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE)/
                     sum(denom_form_9 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    #pnc6
    "Expected Form: N        " = sum(denom_form_10 == 1, na.rm = TRUE),
    "Missing MNH06: n (%)        " = paste0(
      sum(is.na(M06_TYPE_VISIT_10) & denom_form_10 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M06_TYPE_VISIT_10) & denom_form_10 == 1, na.rm = TRUE)/sum(denom_form_10 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    "Missing MNH08: n (%)        " = paste0(
      sum(is.na(M08_TYPE_VISIT_10) & denom_form_10 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M08_TYPE_VISIT_10) & denom_form_10 == 1, na.rm = TRUE)/sum(denom_form_10 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    #pnc26
    "Expected Form: N         " = sum(denom_form_11 == 1, na.rm = TRUE),
    "Missing MNH06: n (%)         " = paste0(
      sum(is.na(M06_TYPE_VISIT_11) & denom_form_11 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M06_TYPE_VISIT_11) & denom_form_11 == 1, na.rm = TRUE)/sum(denom_form_11 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    "Missing MNH08: n (%)         " = paste0(
      sum(is.na(M08_TYPE_VISIT_11) & denom_form_11 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE), # "India-CMC",
      " (", 
      format(round(sum(is.na(M08_TYPE_VISIT_11) & denom_form_11 == 1 & SITE %in% c("Ghana",  "Zambia"), na.rm = TRUE)/
                     sum(denom_form_11 == 1 & SITE %in% c("Ghana",  "Zambia"), na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    #pnc52
    "Expected Form: N          " = sum(denom_form_12 == 1, na.rm = TRUE),
    "Missing MNH06: n (%)          " = paste0(
      sum(is.na(M06_TYPE_VISIT_12) & denom_form_12 == 1, na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M06_TYPE_VISIT_12) & denom_form_12 == 1, na.rm = TRUE)/sum(denom_form_12 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    "Missing MNH08: n (%)          " = paste0(
      sum(is.na(M08_TYPE_VISIT_12) & denom_form_12 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE), 
      " (", 
      format(round(sum(is.na(M08_TYPE_VISIT_12) & denom_form_12 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE)/
                     sum(denom_form_12 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    #total 
  ) %>% 
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) %>% 
  add_column(
    .after = 6,
    "Total" = df_lab %>% 
      summarise(
        #en
        "Expected Form: N" = n(),
        "Missing MNH06: n (%)" = paste0(
          sum(is.na(M06_TYPE_VISIT_1), na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M06_TYPE_VISIT_1), na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
          ")"),
        "Missing MNH08: n (%)" = paste0(
          sum(is.na(M08_TYPE_VISIT_1), na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M08_TYPE_VISIT_1), na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
          ")"),
        #anc20
        "Expected Form: N " = sum(denom_form_2 == 1, na.rm = TRUE),
        "Missing MNH06: n (%) " = paste0(
          sum(is.na(M06_TYPE_VISIT_2) & denom_form_2 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M06_TYPE_VISIT_2) & denom_form_2 == 1, na.rm = TRUE)/sum(denom_form_2 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        "Missing MNH08: n (%) " = paste0(
          sum(is.na(M08_TYPE_VISIT_2) & denom_form_2 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M08_TYPE_VISIT_2) & denom_form_2 == 1, na.rm = TRUE)/sum(denom_form_2 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        #anc28
        "Expected Form: N  " = sum(denom_form_3 == 1, na.rm = TRUE),
        "Missing MNH06: n (%)  " = paste0(
          sum(is.na(M06_TYPE_VISIT_3) & denom_form_3 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M06_TYPE_VISIT_3) & denom_form_3 == 1, na.rm = TRUE)/sum(denom_form_3 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        "Missing MNH08: n (%)  " = paste0(
          sum(is.na(M08_TYPE_VISIT_3) & denom_form_3 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M08_TYPE_VISIT_3) & denom_form_3 == 1, na.rm = TRUE)/sum(denom_form_3 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        #anc32
        "Expected Form: N   " = sum(denom_form_4 == 1, na.rm = TRUE),
        "Missing MNH06: n (%)   " = paste0(
          sum(is.na(M06_TYPE_VISIT_4) & denom_form_4 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M06_TYPE_VISIT_4) & denom_form_4 == 1, na.rm = TRUE)/sum(denom_form_4 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        "Missing MNH08: n (%)   " = paste0(
          sum(is.na(M08_TYPE_VISIT_4) & denom_form_4 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M08_TYPE_VISIT_4) & denom_form_4 == 1, na.rm = TRUE)/sum(denom_form_4 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        #anc36
        "Expected Form: N    " = sum(denom_form_5 == 1, na.rm = TRUE),
        "Missing MNH06: n (%)    " = paste0(
          sum(is.na(M06_TYPE_VISIT_5) & denom_form_5 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M06_TYPE_VISIT_5) & denom_form_5 == 1, na.rm = TRUE)/sum(denom_form_5 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        "Missing MNH08: n (%)    " = paste0(
          sum(is.na(M08_TYPE_VISIT_5) & denom_form_5 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M08_TYPE_VISIT_5) & denom_form_5 == 1, na.rm = TRUE)/sum(denom_form_5 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        #pnc0
        "Expected Form: N     " = sum(denom_form_7 == 1, na.rm = TRUE),
        "Missing MNH06: n (%)     " = paste0(
          sum(is.na(M06_TYPE_VISIT_7) & denom_form_7 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M06_TYPE_VISIT_7) & denom_form_7 == 1, na.rm = TRUE)/sum(denom_form_7 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        "Missing MNH08: n (%)     " = paste0(
          sum(is.na(M08_TYPE_VISIT_7) & denom_form_7 == 1 & SITE %in% c("Ghana", "India-CMC", "Zambia"), na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M08_TYPE_VISIT_7) & denom_form_7 == 1 & SITE %in% c("Ghana", "India-CMC", "Zambia"), na.rm = TRUE)/
                         sum(denom_form_7 == 1 & SITE %in% c("Ghana", "India-CMC", "Zambia"), na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        #pnc1
        "Expected Form: N      " = sum(denom_form_8 == 1, na.rm = TRUE),
        "Missing MNH06: n (%)      " = paste0(
          sum(is.na(M06_TYPE_VISIT_8) & denom_form_8 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M06_TYPE_VISIT_8) & denom_form_8 == 1, na.rm = TRUE)/sum(denom_form_8 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        "Missing MNH08: n (%)      " = paste0(
          sum(is.na(M08_TYPE_VISIT_8) & denom_form_8 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M08_TYPE_VISIT_8) & denom_form_8 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE)/
                         sum(denom_form_8 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        #pnc4
        "Expected Form: N       " = sum(denom_form_9 == 1, na.rm = TRUE),
        "Missing MNH06: n (%)       " = paste0(
          sum(is.na(M06_TYPE_VISIT_9) & denom_form_9 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M06_TYPE_VISIT_9) & denom_form_9 == 1, na.rm = TRUE)/sum(denom_form_9 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        "Missing MNH08: n (%)       " = paste0(
          sum(is.na(M08_TYPE_VISIT_9) & denom_form_9 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M08_TYPE_VISIT_9) & denom_form_9 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE)/
                         sum(denom_form_9 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        #pnc6
        "Expected Form: N        " = sum(denom_form_10 == 1, na.rm = TRUE),
        "Missing MNH06: n (%)        " = paste0(
          sum(is.na(M06_TYPE_VISIT_10) & denom_form_10 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M06_TYPE_VISIT_10) & denom_form_10 == 1, na.rm = TRUE)/sum(denom_form_10 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        "Missing MNH08: n (%)        " = paste0(
          sum(is.na(M08_TYPE_VISIT_10) & denom_form_10 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M08_TYPE_VISIT_10) & denom_form_10 == 1, na.rm = TRUE)/sum(denom_form_10 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        #pnc26
        "Expected Form: N         " = sum(denom_form_11 == 1, na.rm = TRUE),
        "Missing MNH06: n (%)         " = paste0(
          sum(is.na(M06_TYPE_VISIT_11) & denom_form_11 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M06_TYPE_VISIT_11) & denom_form_11 == 1, na.rm = TRUE)/sum(denom_form_11 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        "Missing MNH08: n (%)         " = paste0(
          sum(is.na(M08_TYPE_VISIT_11) & denom_form_11 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M08_TYPE_VISIT_11) & denom_form_11 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE)/
                         sum(denom_form_11 == 1 & SITE %in% c("Ghana",  "Zambia"), na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        #pnc52
        "Expected Form: N          " = sum(denom_form_12 == 1, na.rm = TRUE),
        "Missing MNH06: n (%)          " = paste0(
          sum(is.na(M06_TYPE_VISIT_12) & denom_form_12 == 1, na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M06_TYPE_VISIT_12) & denom_form_12 == 1, na.rm = TRUE)/sum(denom_form_12 == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        "Missing MNH08: n (%)          " = paste0(
          sum(is.na(M08_TYPE_VISIT_12) & denom_form_12 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE), 
          " (", 
          format(round(sum(is.na(M08_TYPE_VISIT_12) & denom_form_12 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE)/
                         sum(denom_form_12 == 1 & SITE %in% c("Ghana", "Zambia"), na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
      ) %>% 
      t() %>% unlist()
  ) %>% 
  #replace denomintor is 0 cells with space
  mutate_all(funs(str_replace(., "(NaN)", "-"))) 
