#****************************************************************************
#*Demographic table
#*Includes: basic demographic characters 
#*Author: Xiaoyan
#*Email: xyh@gwu.edu
#****************************************************************************
library(tidyverse)
library(lubridate)
library(naniar)
library(haven)

UploadDate = "2024-10-18"

#load MAT_ENROLL
MAT_ENROLL <- read_dta(paste0("Z:/Outcome Data/",UploadDate,"/MAT_ENROLL.dta"))
#load mnh00
mnh00 <- read.csv(paste0("Z:/Stacked Data/",UploadDate,"/mnh00_merged.csv")) %>% 
  select(SITE, SCRNID, 
         M00_BRTHDAT, M00_ESTIMATED_AGE, M00_SCHOOL_YRS_SCORRES, M00_SCHOOL_SCORRES)

#load mnh01
mnh01 <- read.csv(paste0("Z:/Stacked Data/",UploadDate,"/mnh01_merged.csv")) %>% 
  filter(M01_TYPE_VISIT == 1) %>% 
  group_by(SITE, SCRNID, MOMID, PREGID) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  ungroup() %>% 
  select(SITE, SCRNID, 
         num_range("M01_US_EDD_BRTHDAT_FTS",1:4),
         M01_FETUS_CT_PERES_US)

#load mnh03
mnh03 <- read.csv(paste0("Z:/Stacked Data/",UploadDate,"/mnh03_merged.csv")) %>% 
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
mnh04 <- read.csv(paste0("Z:/Stacked Data/",UploadDate,"/mnh04_merged.csv")) %>% 
  filter(M04_TYPE_VISIT == 1) %>% 
  select(SITE, MOMID, PREGID, 
         M04_PH_PREV_RPORRES, M04_PH_PREVN_RPORRES, M04_PH_LIVE_RPORRES, 
         M04_PH_OTH_RPORRES, M04_STILLBIRTH_CT_RPORRES, M04_STILLBIRTH_RPORRES,
         M04_FOLIC_ACID_CMOCCUR, M04_IFA_CMOCCUR, M04_INSECT_LSTNIGHT_OBSOCCUR,
         M04_MISCARRIAGE_RPORRES, M04_MISCARRIAGE_CT_RPORRES)

#load mnh05
mnh05 <- read.csv(paste0("Z:/Stacked Data/",UploadDate,"/mnh05_merged.csv")) %>% 
  filter(M05_TYPE_VISIT == 1) %>% 
  select(SITE, MOMID, PREGID, M05_WEIGHT_PERES, M05_HEIGHT_PERES, M05_MUAC_PERES)

#load mnh06
mnh06 <- read.csv(paste0("Z:/Stacked Data/",UploadDate,"/mnh06_merged.csv")) %>% 
  filter(M06_TYPE_VISIT == 1) %>% 
  select(SITE, MOMID, PREGID, M06_SINGLETON_PERES)

#****************************************************************************
#Demographic chracters
#****************************************************************************
df_maternal <- MAT_ENROLL %>%
  left_join(mnh00, by = c("SITE", "SCRNID")) %>% 
  left_join(mnh01, by = c("SITE", "SCRNID")) %>%
  left_join(mnh03, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(mnh04, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(mnh05, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(mnh06, by = c("SITE", "MOMID", "PREGID")) 

prep_demo <- df_maternal %>% 
  dplyr::select("SCRNID", "MOMID", "PREGID", "SITE",
                EST_CONCEP_DATE, BOE_GA_DAYS_ENROLL,
                M00_BRTHDAT, M00_ESTIMATED_AGE,
                M00_SCHOOL_YRS_SCORRES, M00_SCHOOL_SCORRES,
                num_range("M01_US_EDD_BRTHDAT_FTS",1:4),
                M01_FETUS_CT_PERES_US,
                M02_SCRN_OBSSTDAT,
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
                M04_MISCARRIAGE_RPORRES, M04_MISCARRIAGE_CT_RPORRES,
                M05_WEIGHT_PERES, M05_HEIGHT_PERES, M05_MUAC_PERES,
                M06_SINGLETON_PERES
  ) %>% 
  #replace default value 7s with NA
  mutate(across(everything(), ~ ifelse(. < 0, NA, .))) %>% 
  mutate(across(everything(), ~ ifelse(. %in% c("1907-07-07", "1905-05-05"), NA, .))) %>% 
  mutate(M04_PH_PREV_RPORRES = ifelse(M04_PH_PREV_RPORRES == 77, NA, M04_PH_PREV_RPORRES))

#other baseline variable
df_demo <- prep_demo %>%
  mutate(
    #Female Head of household (codes 1,3)
    hh_head_female = case_when(
      M03_HEAD_HH_FCORRES %in% c(1,3) ~ 1, 
      M03_HEAD_HH_FCORRES %in% c(2,4) ~ 0,
      M03_HEAD_HH_FCORRES == 88 ~ 88,
      TRUE ~ NA_real_
    ),
    #Household Size
    hh_size = case_when(
      M03_HOUSE_OCC_TOT_FCORRES %in% c(1:54) ~ M03_HOUSE_OCC_TOT_FCORRES,
      TRUE ~ NA_real_
    ),
    #Improved toilet facility
    toilet_improved = case_when(
      M03_TOILET_FCORRES %in% c(1:3,5:7,9) ~ 1,
      M03_TOILET_FCORRES %in% c(4,8,10:12) ~ 0,
      M03_TOILET_FCORRES == 88 ~ 88,
      TRUE ~ NA_real_
    ), 
    #share toilet facility
    toilet_shared = case_when(
      M03_TOILET_SHARE_FCORRES == 1 ~ 1,
      M03_TOILET_SHARE_FCORRES == 0 ~ 0,
      M03_TOILET_FCORRES == 12 ~ 0,
      TRUE ~ NA_real_
    ), 
    #improved water source
    water_improved = case_when(
      M03_H2O_FCORRES %in% c(1:5, 7, 9:11, 13, 14) ~ 1,
      M03_H2O_FCORRES %in% c(6, 8, 12) ~ 0,
      M03_H2O_FCORRES == 88 ~ 88,
      TRUE ~ NA_real_
    ),
    #phone access
    phone_access = case_when(
      M03_MOBILE_ACCESS_FCORRES == 1 ~ 1, #Own phone
      M03_MOBILE_ACCESS_FCORRES == 2 ~ 2, #Share phone
      M03_MOBILE_ACCESS_FCORRES == 0 ~ 3, #No access to phone
      TRUE ~ NA_real_
    ),
    #Household member smoke
    hh_smoke = case_when(
      M03_SMOKE_HHOLD_OECOCCUR %in% c(1,0) ~ M03_SMOKE_HHOLD_OECOCCUR, 
      TRUE ~ NA_real_
    ),
    #Age
    age_temp = ifelse(!is.na(M02_SCRN_OBSSTDAT) & !is.na(M00_BRTHDAT), as.numeric(ymd(M02_SCRN_OBSSTDAT) - ymd(M00_BRTHDAT)) %/% 365, NA_real_), 
    age = case_when(
      (SITE %in% c("Ghana", "Pakistan", "Zambia") & age_temp >= 15) | #remove outlivers (not meet age requirement)
        (SITE %in% c("India-CMC", "India-SAS", "Kenya") & age_temp >= 18) ~ age_temp,
      (SITE %in% c("Ghana", "Pakistan", "Zambia") & as.numeric(M00_ESTIMATED_AGE) >= 15) |
        (SITE %in% c("India-CMC", "India-SAS", "Kenya") & as.numeric(M00_ESTIMATED_AGE) >= 18) ~ as.numeric(M00_ESTIMATED_AGE),
      TRUE ~ NA_real_
    ),
    #age below 18 
    age18 = case_when(
      age > 14 & age < 18 ~ 1, 
      age >= 18 ~ 0,
      TRUE ~ NA_real_
    ),
    #Body mass index 
    bmi_enroll = case_when(
      M05_WEIGHT_PERES > 0 & M05_HEIGHT_PERES > 0 ~ 
        M05_WEIGHT_PERES / M05_HEIGHT_PERES / M05_HEIGHT_PERES * 10000, 
      TRUE ~ NA_real_
    ),
    bmi_level = case_when(
      bmi_enroll < 18.5 ~ 1,
      bmi_enroll >= 18.5 & bmi_enroll < 25 ~ 2,
      bmi_enroll >= 25 & bmi_enroll < 30 ~ 3,
      bmi_enroll >= 30 ~ 4,
      TRUE ~ NA_real_
    ), 
    #GA
    ga_wks_enroll = BOE_GA_DAYS_ENROLL/7,
    #Years of formal education 
    school_yrs = case_when(
      M00_SCHOOL_YRS_SCORRES >= 0 ~ as.numeric(M00_SCHOOL_YRS_SCORRES), 
      M00_SCHOOL_SCORRES == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    #Married or cohabiting
    married = case_when(
      M03_MARITAL_SCORRES %in% c(1,2) ~ 1, #married/cohabiting
      M03_MARITAL_SCORRES %in% c(3:5) ~ 0, #divorced/permanently separated/widowed/single,never married
      TRUE ~ NA_real_
    ),
    #marriage_status
    marry_status = case_when(
      M03_MARITAL_SCORRES %in% c(1:5) ~ M03_MARITAL_SCORRES, 
      TRUE ~ NA_real_
    ),
    #marital age
    marry_age = case_when(
      M03_MARITAL_AGE > 0 ~ M03_MARITAL_AGE,
      TRUE ~ NA_real_
    ),
    #height_group
    height_group = case_when(
      M05_HEIGHT_PERES > 0 & M05_HEIGHT_PERES < 145 ~ 1,
      M05_HEIGHT_PERES >= 145 & M05_HEIGHT_PERES < 150 ~ 2,
      M05_HEIGHT_PERES >= 150 & M05_HEIGHT_PERES < 155 ~ 3,
      M05_HEIGHT_PERES >= 155 ~ 4,
      TRUE ~ NA_real_
    ), 
    #singleton
    singleton = case_when(
      M06_SINGLETON_PERES == 1 ~ 1,
      M06_SINGLETON_PERES == 0 ~ 0,
      M01_FETUS_CT_PERES_US == 1 ~ 1, 
      M01_FETUS_CT_PERES_US > 1 ~ 0,
      TRUE ~ NA_real_
    ), 
    #educated
    educated = case_when(
      M00_SCHOOL_SCORRES %in% c(0,1) ~ M00_SCHOOL_SCORRES, 
      TRUE ~ NA_real_
    ),
    #Years of formal education 
    school_yrs = case_when(
      M00_SCHOOL_YRS_SCORRES >= 0 ~ as.numeric(M00_SCHOOL_YRS_SCORRES), 
      M00_SCHOOL_SCORRES == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    #Gravidity-total number of times a woman has been pregnant, regardless of the outcome or duration of the pregnancy
    gravidity = case_when(
      M04_PH_PREVN_RPORRES > 0 ~ M04_PH_PREVN_RPORRES,
      M04_PH_PREV_RPORRES == 0 | M04_PH_PREVN_RPORRES == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    #Primigravida-an individual pregnant for the first time
    primigravida = case_when(
      gravidity == 0 ~ 1, #never pregnant or pregnancy =0 before
      gravidity > 0 ~ 0, 
      TRUE ~ NA_real_
    ),
    #parity-the number of times a woman has given birth to a fetus at a gestational age of 20 weeks or more, regardless of whether the baby was born alive or stillborn.
    parity = case_when(
      M04_PH_PREV_RPORRES == 0 ~ 0,
      !is.na(M04_PH_LIVE_RPORRES) & !is.na(M04_STILLBIRTH_CT_RPORRES) ~ M04_PH_LIVE_RPORRES + M04_STILLBIRTH_CT_RPORRES, 
      !is.na(M04_PH_LIVE_RPORRES) & is.na(M04_STILLBIRTH_CT_RPORRES) ~ M04_PH_LIVE_RPORRES, 
      is.na(M04_PH_LIVE_RPORRES) & !is.na(M04_STILLBIRTH_CT_RPORRES) ~ M04_STILLBIRTH_CT_RPORRES, 
      M04_PH_OTH_RPORRES > 0 ~ 0,
      TRUE ~ NA_real_
    ),
    #Nulliparous -  a female who has never given birth to a live baby or a stillbirth
    nulliparous = case_when(
      parity == 0 ~ 1, 
      parity > 0 ~ 0,
      TRUE ~ NA_real_
    ),
    #engaged in paid work 
    other_job = trimws(tolower(M03_JOB_OTHR_SPFY_SCORRES)),
    paid_work = case_when(
      M03_JOB_SCORRES %in% c(1:8) ~ 1,
      other_job %in% c("apprentice", "apprentices", "apprenticeship", "aprintish",
                       "beautician", "beaution", "buetition", "bread seller",
                       "casual worker", "charcoal burning", "cleaner", "community health worker", 
                       "company worker", "carpenter", "community  police",
                       "do hand embroidery", "daily wages",
                       "embroidery work",
                       "farmer", "farming", "fish  monger", "food counter", "food seller",
                       "gold mining",
                       "hair dresser", "hair dressing", "hair dressing and beauty therapy", 
                       "hairdresser", "hairdressers", "eadreser", "home maid", "hotel owner", 
                       "house keeper", "housekeeper", "headreser", "health worker",
                       "jua kali",
                       "koko seler",
                       "lab technician",
                       "made", "mama mboga",
                       "petrol seller", "pety trader", "private surver", "pecking",
                       "receptionist", 
                       "sales person", "saleslady", "saloonist", "seamstress", "seamtress", 
                       "selling  vegetables", "sells clothes", "selling cereals", 
                       "Sells clothes", "service personnel", "stitching", "selling shoes",
                       "shopper pealing",
                       "table banking", "tailor", "tailoring", "trader", "trading",
                       "tader", "teacher",
                       "un skilled person",
                       "welding") ~ 1,
      other_job %in% c("collage student", "health volunteer", "parents", "student", "studying", 
                       "volunteer at jijenge", "volunteer as medical laboratory technician") ~ 0,
      M03_JOB_SCORRES %in% c(9, 77) ~ 0, #not paid work or not working/na(77)
      M03_JOB_SCORRES == 88 ~ 88,
      TRUE ~ NA_real_
    ),
    #if smokes
    smoke = case_when(
      M03_SMOKE_OECOCCUR %in% c(0,1) ~ M03_SMOKE_OECOCCUR,
      TRUE ~ NA_real_
    ),
    #if chews tobacco
    chew_tobacco = case_when(
      M03_CHEW_OECOCCUR %in% c(0,1) ~ M03_CHEW_OECOCCUR,
      TRUE ~ NA_real_
    ),
    #if chew betel nut
    chew_betelnut = case_when(
      M03_CHEW_BNUT_OECOCCUR %in% c(0,1) ~ M03_CHEW_BNUT_OECOCCUR,
      TRUE ~ NA_real_
    ),
    #if drink alcohol
    drink = case_when(
      M03_DRINK_OECOCCUR %in% c(0,1) ~ M03_DRINK_OECOCCUR,
      TRUE ~ NA_real_
    ),
    #Folic supplementation
    folic = case_when(
      M04_FOLIC_ACID_CMOCCUR == 1 | M04_IFA_CMOCCUR == 1 ~ 1,
      M04_FOLIC_ACID_CMOCCUR == 0 & M04_IFA_CMOCCUR == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    #Sleep under net
    under_net = case_when(
      M04_INSECT_LSTNIGHT_OBSOCCUR %in% c(0,1) ~ M04_INSECT_LSTNIGHT_OBSOCCUR,
      TRUE ~ NA_real_
    ),
    #Plan to give birth at facility
    birth_facility = case_when(
      M03_PD_BIRTH_OHOLOC == 1 ~ 1,
      M03_PD_BIRTH_OHOLOC == 2 ~ 0,
      M03_PD_BIRTH_OHOLOC == 88 ~ 88
    ),
    #birth location decision maker
    birth_loc_decision_maker = case_when(
      M03_PD_DM_SCORRES %in% c(1:5,88) ~ M03_PD_DM_SCORRES, 
      TRUE ~ NA_real_
    ),
    #Number of Fetuses (not use M06_FETUS_CT_PERES)
    num_fetus = case_when(
      M01_FETUS_CT_PERES_US > 0  ~  M01_FETUS_CT_PERES_US,
      TRUE ~ NA_real_
    ),
    #Miscarriage
    miscarriage = case_when(
      M04_MISCARRIAGE_RPORRES %in% c(0,1) ~ M04_MISCARRIAGE_RPORRES,
      M04_PH_PREV_RPORRES == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    # number of miscarriages
    num_miscarriage = case_when(
      M04_MISCARRIAGE_CT_RPORRES >= 0 ~ M04_MISCARRIAGE_CT_RPORRES,
      M04_MISCARRIAGE_RPORRES == 0 ~ 0, 
      M04_PH_OTH_RPORRES == 0 ~ 0,
      M04_PH_OTH_RPORRES == 1 & M04_MISCARRIAGE_RPORRES == 1 ~ 1,
      M04_PH_OTH_RPORRES == 1 & (M04_STILLBIRTH_CT_RPORRES == 1 | M04_STILLBIRTH_RPORRES == 1) ~ 0,
      M04_PH_OTH_RPORRES > 1 & (M04_PH_OTH_RPORRES == M04_STILLBIRTH_CT_RPORRES) ~ 0,
      M04_PH_PREV_RPORRES == 0 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    #MUAC
    muac = M05_MUAC_PERES,
  ) %>% 
  dplyr::select(-c(matches("M\\d{2}_"), BOE_GA_DAYS_ENROLL, EST_CONCEP_DATE, other_job, age_temp)) %>% 
  rename_with(toupper)

save(df_maternal, file = "derived_data/df_maternal.rda")
save(df_demo, file = "derived_data/df_demo.rda")
write.csv(df_demo, file = "derived_data/MAT_DEMOGRAPHIC.csv", row.names = FALSE)
