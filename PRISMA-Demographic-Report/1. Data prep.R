#****************************************************************************
#*Demographic report
#*Author: Xiaoyan
#*Email: xyh@gwu.edu
#****************************************************************************
rm(list = ls())

library(tidyverse)
library(lubridate)
library(naniar)
library(haven)
library(openxlsx)
library(readxl)

UploadDate = "2026-05-01"

#****************************************************************************
# Helper functions ---- 
#****************************************************************************

## Date cleaning function (from Flowchart workflow) ----
clean_date <- function(x,
                       invalid_dates = as_date(c("1907-07-07", "2007-07-07",
                                                 "1905-05-05", "1909-07-09",
                                                 "1906-06-06", "1909-09-09")),
                       min_date = NULL,
                       max_date = NULL) {
  
  if (is.null(max_date)) max_date <- today()
  
  parsed <- parse_date_time(
    trimws(as.character(x)),
    orders = c("d/m/Y", "d-m-Y", "Y-m-d",
               "d-b-y", "d-m-y",
               "m/d/y", "m-d-y"),
    exact = FALSE
  ) %>% as_date()
  
  invalid_mask <- is.na(parsed) | parsed %in% invalid_dates
  if (!is.null(min_date)) invalid_mask <- invalid_mask | parsed < min_date
  if (!is.null(max_date)) invalid_mask <- invalid_mask | parsed > max_date
  
  parsed[invalid_mask] <- NA
  return(parsed)
}


#****************************************************************************
# Load and merge data ---- 
#****************************************************************************

#set path to save 
path_to_save <- paste0("~/Analysis/Demographics/", UploadDate, "/derived_data")
dir.create(path_to_save, recursive = TRUE, showWarnings = FALSE)

path_to_output <- paste0("~/Analysis/Demographics/", UploadDate, "/output")
dir.create(path_to_output, recursive = TRUE, showWarnings = FALSE)

path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")

#set path to data 
path_to_data <- paste0("~/Analysis/Merged_data/", UploadDate)

#load mnh00
#load MAT_ENROLL
MAT_ENROLL <- read.xlsx(paste0(path_to_tnt, "MAT_ENROLL" ,".xlsx"))

mnh00 <- read.csv(paste0(path_to_data, "/mnh00_merged.csv")) %>% # paste0("Z:/Stacked Data/",UploadDate,"/mnh00_merged.csv")
  select(SITE, SCRNID, 
         M00_BRTHDAT, M00_ESTIMATED_AGE, M00_SCHOOL_YRS_SCORRES, M00_SCHOOL_SCORRES)

#load mnh01
mnh01 <- read.csv(paste0(path_to_data, "/mnh01_merged.csv")) %>% 
  filter(M01_TYPE_VISIT == 1) %>% 
  group_by(SITE, SCRNID, MOMID, PREGID) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  ungroup() %>% 
  select(SITE, SCRNID, 
         num_range("M01_US_EDD_BRTHDAT_FTS",1:4),
         M01_FETUS_CT_PERES_US)

#load mnh03
mnh03 <- read.csv(paste0(path_to_data,"/mnh03_merged.csv")) %>% 
  select(SITE, MOMID, PREGID, 
         M03_MARITAL_SCORRES, M03_MARITAL_AGE,
         M03_HEAD_HH_FCORRES, M03_HEAD_HH_SPFY_FCORRES,
         M03_SMOKE_OECOCCUR,
         M03_CHEW_OECOCCUR, M03_CHEW_BNUT_OECOCCUR,
         M03_DRINK_OECOCCUR, M03_PD_BIRTH_OHOLOC,
         M03_HOUSE_OCC_TOT_FCORRES, M03_H2O_FCORRES,
         M03_TOILET_FCORRES, M03_TOILET_SHARE_FCORRES,
         M03_MOBILE_ACCESS_FCORRES, M03_SMOKE_HHOLD_OECOCCUR,
         M03_JOB_SCORRES, M03_JOB_OTHR_SPFY_SCORRES,
         M03_PD_DM_SCORRES)

#load mnh04
mnh04 <- read.csv(paste0(path_to_data,"/mnh04_merged.csv")) %>% 
  filter(M04_TYPE_VISIT == 1) %>% 
  select(SITE, MOMID, PREGID, 
         M04_PH_PREV_RPORRES, M04_PH_PREVN_RPORRES, M04_PH_LIVE_RPORRES, 
         M04_PH_OTH_RPORRES, M04_STILLBIRTH_CT_RPORRES, M04_STILLBIRTH_RPORRES,
         M04_FOLIC_ACID_CMOCCUR, M04_IFA_CMOCCUR, M04_INSECT_LSTNIGHT_OBSOCCUR,
         M04_MISCARRIAGE_RPORRES, M04_MISCARRIAGE_CT_RPORRES)

#load mnh05
mnh05 <- read.csv(paste0(path_to_data,"/mnh05_merged.csv")) %>% 
  filter(M05_TYPE_VISIT == 1) %>% 
  select(SITE, MOMID, PREGID, M05_WEIGHT_PERES, M05_HEIGHT_PERES, M05_MUAC_PERES)

#load mnh06
mnh06 <- read.csv(paste0(path_to_data,"/mnh06_merged.csv")) %>% 
  filter(M06_TYPE_VISIT == 1) %>% 
  select(SITE, MOMID, PREGID, M06_SINGLETON_PERES)


#****************************************************************************
# Define demographic characters ----
#****************************************************************************
## Merge all MNH files ----
df_maternal <- MAT_ENROLL %>%
  left_join(mnh00, by = c("SITE", "SCRNID")) %>%
  left_join(mnh01, by = c("SITE", "SCRNID")) %>%
  left_join(mnh03, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(mnh04, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(mnh05, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(mnh06, by = c("SITE", "MOMID", "PREGID"))

##Prepare demographic variables ----
prep_demo <- df_maternal %>%
  dplyr::select("SCRNID", "MOMID", "PREGID", "SITE",
                PREG_START_DATE, BOE_GA_DAYS_ENROLL,
                M00_BRTHDAT, M00_ESTIMATED_AGE,
                M00_SCHOOL_YRS_SCORRES, M00_SCHOOL_SCORRES,
                num_range("M01_US_EDD_BRTHDAT_FTS", 1:4),
                M01_FETUS_CT_PERES_US,
                ENROLL_SCRN_DATE,
                M03_MARITAL_SCORRES, M03_MARITAL_AGE,
                M03_HEAD_HH_FCORRES, M03_HEAD_HH_SPFY_FCORRES,
                M03_SMOKE_OECOCCUR,
                M03_CHEW_OECOCCUR, M03_CHEW_BNUT_OECOCCUR,
                M03_DRINK_OECOCCUR, M03_PD_BIRTH_OHOLOC,
                M03_HOUSE_OCC_TOT_FCORRES, M03_H2O_FCORRES,
                M03_TOILET_FCORRES, M03_TOILET_SHARE_FCORRES,
                M03_MOBILE_ACCESS_FCORRES, M03_SMOKE_HHOLD_OECOCCUR,
                M03_JOB_SCORRES, M03_JOB_OTHR_SPFY_SCORRES,
                M03_PD_DM_SCORRES,
                M04_PH_PREV_RPORRES, M04_PH_PREVN_RPORRES, M04_PH_LIVE_RPORRES,
                M04_PH_OTH_RPORRES, M04_STILLBIRTH_CT_RPORRES, M04_STILLBIRTH_RPORRES,
                M04_MISCARRIAGE_CT_RPORRES, M04_MISCARRIAGE_RPORRES,
                M04_FOLIC_ACID_CMOCCUR, M04_IFA_CMOCCUR, M04_INSECT_LSTNIGHT_OBSOCCUR,
                M05_WEIGHT_PERES, M05_HEIGHT_PERES, M05_MUAC_PERES,
                M06_SINGLETON_PERES
  ) %>%
  mutate(across(everything(), ~ ifelse(. < 0, NA, .))) %>%
  mutate(across(everything(), ~ ifelse(. %in% c("1907-07-07", "1905-05-05"), NA, .))) %>%
  mutate(M04_PH_PREV_RPORRES = case_when(M04_PH_PREV_RPORRES > 20 ~ 77, TRUE ~ M04_PH_PREV_RPORRES)) %>%
  mutate(M04_PH_LIVE_RPORRES = case_when(M04_PH_LIVE_RPORRES %in% c(-5, -7, 55, 77) ~ NA, TRUE ~ M04_PH_LIVE_RPORRES),
         M04_STILLBIRTH_CT_RPORRES = case_when(M04_STILLBIRTH_CT_RPORRES %in% c(-5, -7, 55, 77) ~ NA, TRUE ~ M04_STILLBIRTH_CT_RPORRES),
         M04_PH_PREV_RPORRES = case_when(M04_PH_PREV_RPORRES %in% c(-5, -7, 55, 77) ~ NA, TRUE ~ M04_PH_PREV_RPORRES)) %>%
  mutate(M04_PH_PREV_RPORRES = ifelse(M04_PH_PREV_RPORRES == 77, NA, M04_PH_PREV_RPORRES))

## Construct demographic variables ----
df_demo <- prep_demo %>%
  mutate(
    # Female-headed household
    hh_head_female = case_when(
      M03_HEAD_HH_FCORRES %in% c(1, 3) ~ 1,
      M03_HEAD_HH_FCORRES %in% c(2, 4) ~ 0,
      M03_HEAD_HH_FCORRES == 88 ~ 88,
      TRUE ~ NA_real_),
    
    # Number of people living in the household
    hh_size = case_when(
      M03_HOUSE_OCC_TOT_FCORRES %in% c(1:54) ~ M03_HOUSE_OCC_TOT_FCORRES,
      TRUE ~ NA_real_),
    
    # Household has an improved toilet facility
    toilet_improved = case_when(
      M03_TOILET_FCORRES %in% c(1:3, 5:7, 9) ~ 1,
      M03_TOILET_FCORRES %in% c(4, 8, 10:12) ~ 0,
      M03_TOILET_FCORRES == 88 ~ 88,
      TRUE ~ NA_real_),
    
    # Household toilet facility is shared
    toilet_shared = case_when(
      M03_TOILET_SHARE_FCORRES == 1 ~ 1,
      M03_TOILET_SHARE_FCORRES == 0 ~ 0,
      M03_TOILET_FCORRES == 12 ~ 0,
      TRUE ~ NA_real_),
    
    # Household has an improved water source
    water_improved = case_when(
      M03_H2O_FCORRES %in% c(1:5, 7, 9:11, 13, 14) ~ 1,
      M03_H2O_FCORRES %in% c(6, 8, 12) ~ 0,
      M03_H2O_FCORRES == 88 ~ 88,
      TRUE ~ NA_real_),
    
    # Participant's access to a mobile phone
    phone_access = case_when(
      M03_MOBILE_ACCESS_FCORRES == 1 ~ 1,
      M03_MOBILE_ACCESS_FCORRES == 2 ~ 2,
      M03_MOBILE_ACCESS_FCORRES == 0 ~ 3,
      TRUE ~ NA_real_),
    
    # Someone in the participant's household smokes
    hh_smoke = case_when(
      M03_SMOKE_HHOLD_OECOCCUR %in% c(1, 0) ~ M03_SMOKE_HHOLD_OECOCCUR,
      TRUE ~ NA_real_),
    
    # Maternal age calculated from date of birth and enrollment date
    age_temp = ifelse(
      !is.na(ENROLL_SCRN_DATE) & !is.na(M00_BRTHDAT),
      as.numeric(ymd(ENROLL_SCRN_DATE) - ymd(M00_BRTHDAT)) %/% 365,
      NA_real_),
    
    # Maternal age at enrollment using calculated age or estimated age
    mat_age = case_when(
      (SITE %in% c("Ghana", "Pakistan", "Zambia") & age_temp %in% c(15:77)) |
        (SITE %in% c("India-CMC", "India-SAS", "Kenya") & age_temp %in% c(18:77)) ~ age_temp,
      (SITE %in% c("Ghana", "Pakistan", "Zambia") & as.numeric(M00_ESTIMATED_AGE) %in% c(15:77)) |
        (SITE %in% c("India-CMC", "India-SAS", "Kenya") & as.numeric(M00_ESTIMATED_AGE) %in% c(18:77)) ~ as.numeric(M00_ESTIMATED_AGE),
      TRUE ~ NA_real_),
    
    # Participant is younger than 18 years at enrollment
    mat_age_under18 = case_when(
      mat_age > 14 & mat_age < 18 ~ 1,
      mat_age >= 18 ~ 0,
      TRUE ~ NA_real_),
    
    # Body mass index at enrollment
    bmi_enroll = case_when(
      M05_WEIGHT_PERES > 0 & M05_HEIGHT_PERES > 0 ~
        M05_WEIGHT_PERES / M05_HEIGHT_PERES / M05_HEIGHT_PERES * 10000,
      TRUE ~ NA_real_),
    
    # BMI category at enrollment
    bmi_level_enroll = case_when(
      bmi_enroll < 18.5 ~ 1,
      bmi_enroll >= 18.5 & bmi_enroll < 25 ~ 2,
      bmi_enroll >= 25 & bmi_enroll < 30 ~ 3,
      bmi_enroll >= 30 ~ 4,
      TRUE ~ NA_real_),
    
    # Gestational age in weeks at enrollment
    ga_wks_enroll = BOE_GA_DAYS_ENROLL / 7,
    
    # Years of schooling with age validity check
    school_yrs = case_when(
      M00_SCHOOL_YRS_SCORRES >= 0 & (M00_SCHOOL_YRS_SCORRES < mat_age) ~ as.numeric(M00_SCHOOL_YRS_SCORRES),
      M00_SCHOOL_YRS_SCORRES < mat_age ~ 55,
      M00_SCHOOL_SCORRES == 0 ~ 0,
      TRUE ~ NA_real_),
    
    # Participant is married or cohabiting
    married = case_when(
      M03_MARITAL_SCORRES %in% c(1, 2) ~ 1,
      M03_MARITAL_SCORRES %in% c(3:5) ~ 0,
      TRUE ~ NA_real_),
    
    # Detailed marital status
    marry_status = case_when(
      M03_MARITAL_SCORRES %in% c(1:5) ~ M03_MARITAL_SCORRES,
      TRUE ~ NA_real_),
    
    # Age at marriage
    marry_age = case_when(
      M03_MARITAL_AGE > 0 ~ M03_MARITAL_AGE,
      TRUE ~ NA_real_),
    
    # Maternal height category
    height_group = case_when(
      M05_HEIGHT_PERES > 0 & M05_HEIGHT_PERES < 145 ~ 1,
      M05_HEIGHT_PERES >= 145 & M05_HEIGHT_PERES < 150 ~ 2,
      M05_HEIGHT_PERES >= 150 & M05_HEIGHT_PERES < 155 ~ 3,
      M05_HEIGHT_PERES >= 155 ~ 4,
      TRUE ~ NA_real_),
    
    # Singleton pregnancy based on number of fetuses at ultrasound
    singleton = case_when(
      M01_FETUS_CT_PERES_US == 1 ~ 1,
      M01_FETUS_CT_PERES_US > 1 ~ 0,
      TRUE ~ NA_real_),
    
    # Participant has attended school
    educated = case_when(
      M00_SCHOOL_SCORRES %in% c(0, 1) ~ M00_SCHOOL_SCORRES,
      TRUE ~ NA_real_),
    
    # Number of completed years of schooling
    school_yrs = case_when(
      M00_SCHOOL_YRS_SCORRES >= 0 ~ as.numeric(M00_SCHOOL_YRS_SCORRES),
      M00_SCHOOL_SCORRES == 0 ~ 0,
      TRUE ~ NA_real_),
    
    # Total number of pregnancies including current pregnancy
    gravidity = case_when(
      M01_FETUS_CT_PERES_US >= 1 & M04_PH_PREVN_RPORRES > 0 ~ M04_PH_PREVN_RPORRES + 1,
      M01_FETUS_CT_PERES_US >= 1 & (M04_PH_PREV_RPORRES == 0 | M04_PH_PREVN_RPORRES == 0) ~ 1,
      TRUE ~ NA_real_),
    
    # Participant is in her first pregnancy
    primigravida = case_when(
      gravidity == 1 ~ 1,
      gravidity > 1 ~ 0,
      TRUE ~ NA_real_),
    
    # Number of previous births based on live births and stillbirths
    parity = case_when(
      M04_PH_PREV_RPORRES == 0 ~ 0,
      !is.na(M04_PH_LIVE_RPORRES) & !is.na(M04_STILLBIRTH_CT_RPORRES) ~
        M04_PH_LIVE_RPORRES + M04_STILLBIRTH_CT_RPORRES,
      !is.na(M04_PH_LIVE_RPORRES) & is.na(M04_STILLBIRTH_CT_RPORRES) ~ M04_PH_LIVE_RPORRES,
      is.na(M04_PH_LIVE_RPORRES) & !is.na(M04_STILLBIRTH_CT_RPORRES) ~ M04_STILLBIRTH_CT_RPORRES,
      M04_PH_OTH_RPORRES > 0 ~ 0,
      TRUE ~ NA_real_),
    
    # Participant has had no previous births
    nulliparous = case_when(
      parity == 0 ~ 1,
      parity > 0 ~ 0,
      TRUE ~ NA_real_),
    
    # Clean free-text occupation for paid work classification
    other_job = trimws(tolower(M03_JOB_OTHR_SPFY_SCORRES)),
    
    # Participant performs paid work
    paid_work = case_when(
      M03_JOB_SCORRES %in% c(1:8) ~ 1,
      other_job %in% c("apprentice", "apprentices", "apprenticeship", "aprintish",
                       "beautician", "beaution", "buetition", "bread seller",
                       "casual worker", "charcoal burning", "cleaner", "community health worker",
                       "company worker", "carpenter", "community  police",
                       "do hand embroidery", "daily wages", "embroidery work",
                       "farmer", "farming", "fish  monger", "food counter", "food seller",
                       "gold mining", "hair dresser", "hair dressing", "hair dressing and beauty therapy",
                       "hairdresser", "hairdressers", "eadreser", "home maid", "hotel owner",
                       "house keeper", "housekeeper", "headreser", "health worker",
                       "jua kali", "koko seler", "lab technician", "made", "mama mboga",
                       "petrol seller", "pety trader", "private surver", "pecking",
                       "receptionist", "sales person", "saleslady", "saloonist", "seamstress",
                       "seamtress", "selling  vegetables", "sells clothes", "selling cereals",
                       "Sells clothes", "service personnel", "stitching", "selling shoes",
                       "shopper pealing", "table banking", "tailor", "tailoring", "trader",
                       "trading", "tader", "teacher", "un skilled person", "welding") ~ 1,
      other_job %in% c("collage student", "health volunteer", "parents", "student",
                       "studying", "volunteer at jijenge", "volunteer as medical laboratory technician") ~ 0,
      M03_JOB_SCORRES %in% c(9, 77) ~ 0,
      M03_JOB_SCORRES == 88 ~ 88,
      TRUE ~ NA_real_),
    
    # Participant currently smokes
    smoke = case_when(
      M03_SMOKE_OECOCCUR %in% c(0, 1) ~ M03_SMOKE_OECOCCUR,
      TRUE ~ NA_real_),
    
    # Participant currently chews tobacco
    chew_tobacco = case_when(
      M03_CHEW_OECOCCUR %in% c(0, 1) ~ M03_CHEW_OECOCCUR,
      TRUE ~ NA_real_),
    
    # Participant currently chews betel nut
    chew_betelnut = case_when(
      M03_CHEW_BNUT_OECOCCUR %in% c(0, 1) ~ M03_CHEW_BNUT_OECOCCUR,
      TRUE ~ NA_real_),
    
    # Participant currently drinks alcohol
    drink = case_when(
      M03_DRINK_OECOCCUR %in% c(0, 1) ~ M03_DRINK_OECOCCUR,
      TRUE ~ NA_real_),
    
    # Participant received folic acid or iron-folic acid supplementation
    folic_suppl_enroll = case_when(
      M04_FOLIC_ACID_CMOCCUR == 1 | M04_IFA_CMOCCUR == 1 ~ 1,
      M04_FOLIC_ACID_CMOCCUR == 0 & M04_IFA_CMOCCUR == 0 ~ 0,
      TRUE ~ NA_real_),
    
    # Participant slept under an insecticide-treated net the previous night
    sleep_under_net = case_when(
      M04_INSECT_LSTNIGHT_OBSOCCUR %in% c(0, 1) ~ M04_INSECT_LSTNIGHT_OBSOCCUR,
      TRUE ~ NA_real_),
    
    # Planned delivery location is a health facility
    birth_facility = case_when(
      M03_PD_BIRTH_OHOLOC == 1 ~ 1,
      M03_PD_BIRTH_OHOLOC == 2 ~ 0,
      M03_PD_BIRTH_OHOLOC == 88 ~ 88),
    
    # Person responsible for deciding the planned birth location
    birth_loc_decision_maker = case_when(
      M03_PD_DM_SCORRES %in% c(1:5, 88) ~ M03_PD_DM_SCORRES,
      TRUE ~ NA_real_),
    
    # Number of fetuses identified at enrollment ultrasound
    num_fetus = case_when(
      M01_FETUS_CT_PERES_US > 0 ~ M01_FETUS_CT_PERES_US,
      TRUE ~ NA_real_),
    
    # Participant has a history of miscarriage
    miscarriage = case_when(
      M04_MISCARRIAGE_RPORRES %in% c(0, 1) ~ M04_MISCARRIAGE_RPORRES,
      M04_PH_PREV_RPORRES == 0 | M04_PH_OTH_RPORRES == 0 ~ 0,
      TRUE ~ NA_real_),
    
    # Number of previous miscarriages
    num_miscarriage = case_when(
      M04_MISCARRIAGE_CT_RPORRES >= 0 ~ M04_MISCARRIAGE_CT_RPORRES,
      M04_MISCARRIAGE_RPORRES == 0 ~ 0,
      M04_PH_OTH_RPORRES == 0 ~ 0,
      M04_PH_OTH_RPORRES == 1 & M04_MISCARRIAGE_RPORRES == 1 ~ 1,
      M04_PH_OTH_RPORRES == 1 &
        (M04_STILLBIRTH_CT_RPORRES == 1 | M04_STILLBIRTH_RPORRES == 1) ~ 0,
      M04_PH_OTH_RPORRES > 1 &
        (M04_PH_OTH_RPORRES == M04_STILLBIRTH_CT_RPORRES) ~ 0,
      M04_PH_PREV_RPORRES == 0 ~ NA_real_,
      TRUE ~ NA_real_),
    
    # Mid-upper arm circumference at enrollment
    muac_enroll = M05_MUAC_PERES
  ) %>%
  
  # Remove original MNH variables and temporary construction variables
  dplyr::select(
    -c(matches("M\\d{2}_"), BOE_GA_DAYS_ENROLL,
       PREG_START_DATE, other_job, age_temp)
  ) %>%
  
  # Convert final demographic variable names to uppercase
  rename_with(toupper)

## Add demographic variable labels ----
demo_labels <- c(
  SITE = "Study site",
  SCRNID = "Screening ID",
  MOMID = "Mom ID",
  PREGID = "Pregnancy ID",
  HH_HEAD_FEMALE = "Female-headed household",
  HH_SIZE = "Number of people living in household",
  TOILET_IMPROVED = "Household has improved toilet facility",
  TOILET_SHARED = "Household toilet facility is shared",
  WATER_IMPROVED = "Household has improved water source",
  PHONE_ACCESS = "Mobile phone access",
  HH_SMOKE = "Someone in household smokes",
  MAT_AGE = "Maternal age at enrollment",
  MAT_AGE_UNDER18 = "Maternal age under 18 years",
  BMI_ENROLL = "Body mass index at enrollment",
  BMI_LEVEL_ENROLL = "BMI category at enrollment",
  GA_WKS_ENROLL = "Gestational age in weeks at enrollment",
  SCHOOL_YRS = "Completed years of schooling",
  MARRIED = "Married or cohabiting",
  MARRY_STATUS = "Marital status",
  MARRY_AGE = "Age at marriage",
  HEIGHT_GROUP = "Maternal height category",
  SINGLETON = "Singleton pregnancy",
  EDUCATED = "Attended school",
  GRAVIDITY = "Total number of pregnancies including current pregnancy",
  PRIMIGRAVIDA = "First pregnancy",
  PARITY = "Number of previous births",
  NULLIPAROUS = "No previous births",
  PAID_WORK = "Performs paid work",
  SMOKE = "Currently smokes",
  CHEW_TOBACCO = "Currently chews tobacco",
  CHEW_BETELNUT = "Currently chews betel nut",
  DRINK = "Currently drinks alcohol",
  FOLIC_SUPPL_ENROLL = "Folic acid or iron-folic acid supplementation at enrollment",
  SLEEP_UNDER_NET = "Slept under insecticide-treated net previous night",
  BIRTH_FACILITY = "Planned delivery location is a health facility",
  BIRTH_LOC_DECISION_MAKER = "Person responsible for deciding planned birth location",
  NUM_FETUS = "Number of fetuses at enrollment ultrasound",
  MISCARRIAGE = "History of miscarriage",
  NUM_MISCARRIAGE = "Number of previous miscarriages",
  MUAC_ENROLL = "Mid-upper arm circumference at enrollment"
)

for (var in intersect(names(demo_labels), names(df_demo))) {
  attr(df_demo[[var]], "label") <- demo_labels[[var]]
}


save(df_maternal, file= paste(path_to_save,"/df_maternal", ".RData",sep = ""))
save(df_demo, file= paste(path_to_save,"/df_demo", ".RData", sep = ""))

write.csv(df_demo, file = paste0(path_to_tnt, "MAT_DEMOGRAPHIC.csv"), row.names = FALSE)
write.xlsx(df_demo, file = paste0(path_to_tnt, "MAT_DEMOGRAPHIC.xlsx"), rownames = FALSE) 
write_dta(df_demo, path = paste0(path_to_tnt, "MAT_DEMOGRAPHIC.dta"))
 
# save(df_maternal, file = "derived_data/df_maternal.rda")
# save(df_demo, file = "derived_data/df_demo.rda")
# write.csv(df_demo, file = "derived_data/MAT_DEMOGRAPHIC.csv", row.names = FALSE)
# write.xlsx(df_demo, file = "derived_data/MAT_DEMOGRAPHIC.xlsx")
# write_dta(df_demo, path = "derived_data/MAT_DEMOGRAPHIC.dta")
