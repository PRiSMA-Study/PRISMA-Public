#*****************************************************************************
#* PRISMA Maternal Hemorrhage
#* Last updated: 28 September 2026
#* Added a query export section!
#* Modified a few functions to work properly

#The first section, CONSTRUCTED VARIABLES GENERATION, below, the code generates datasets for 
#each form with additional variables that will be used for multiple outcomes. For example, mnh01_constructed 
#is a dataset that will be used for several outcomes. 

## Hemorrhage (antepartum)
# ## required variables & logic
# M04_APH_CEOCCUR_1-5==1 (Current clinical status: antepartum hemorrhage)
# APH_UNSCHED_ANY==1 (Current clinical status: antepartum hemorrhage @ at any unscheduled visit)
# M09_APH_CEOCCUR_6==1 (Did the mother experience antepartum hemorrhage?)
# HEM_HOSP_ANY==1 (specify type of labor/delivery or birth complication: APH or PPH or vaginal bleeding)
# M19_TIMING_OHOCAT==1 (timing of hospitalization = antenatal period)

## Hemorrhage (postpartum)
# ## required variables & logic
# PPH_CEOCCUR==1 [MNH09] (Did mother experience postpartum hemorrhage)
# PPH_ESTIMATE_FAORRES >=500 (Record estimated blood loss)
# PPH_FAORRES_1==1 [MNH09] (Procedures carried out for PPH, Balloon/condom tamponade)
# PPH_FAORRES_2==1 [MNH09] (Procedures carried out for PPH, Surgical interventions)
# PPH_FAORRES_3==1 [MNH09] (Procedures carried out for PPH, Brace sutures)
# PPH_FAORRES_4==1 [MNH09] (Procedures carried out for PPH, Vessel ligation)
# PPH_FAORRES_5==1 [MNH09] (Procedures carried out for PPH, Hysterectomy)
# PPH_FAORRES_88==1 [MNH09] (Procedures carried out for PPH, Other)
# PPH_TRNSFSN_PROCCUR==1 [MNH09] (Did the mother need a transfusion?) OR
# (HEM_HOSP_ANY==1 & M19_TIMING_OHOCAT==2) [MNH09]
# BIRTH_COMPL_MHTERM_1 [MNH12] (Was the mother diagnosed with any of the following birth complications, PPH)

## Hemorrhage (severe postpartum)
## Hemorrhage == 1 AND (PPH_ESTIMATE_FAORRES=1000 OR Blood transfusion OR any procedure) 
# PPH_ESTIMATE_FAORRES >=1000 [MNH09] (Record estimated blood loss) 
# PPH_FAORRES_1==1 [MNH09] (Procedures carried out for PPH, Balloon/condom tamponade)  
# PPH_FAORRES_2==1 [MNH09] (Procedures carried out for PPH, Surgical interventions) 
# PPH_FAORRES_3==1 [MNH09] (Procedures carried out for PPH, Brace sutures) 
# PPH_FAORRES_4==1 [MNH09] (Procedures carried out for PPH, Vessel ligation) 
# PPH_FAORRES_5==1 [MNH09] (Procedures carried out for PPH, Hysterectomy) 
# PPH_FAORRES_88==1 [MNH09] (Procedures carried out for PPH, Other) 
# PPH_TRNSFSN_PROCCUR==1 [MNH09] (Did the mother need a transfusion?) 


## Medications 
# M09_PPH_CMOCCUR_1_6 (Were any of the following medications given to prevent/treat PPH?, Oxytocin)
# M09_PPH_CMOCCUR_2_6 (Were any of the following medications given to prevent/treat PPH?, Misoprostol)
# M09_PPH_CMOCCUR_3_6 (Were any of the following medications given to prevent/treat PPH?, Tranexaminic acid)
# M09_PPH_CMOCCUR_4_6 (Were any of the following medications given to prevent/treat PPH?, Carbetocin)
# M09_PPH_CMOCCUR_5_6 (Were any of the following medications given to prevent/treat PPH?, Methylergonovine)
# M09_PPH_CMOCCUR_6_6 (Were any of the following medications given to prevent/treat PPH?, Carboprost (PGF2-alpha))
# M09_PPH_CMOCCUR_77_6 (Were any of the following medications given to prevent/treat PPH?, Other)
# M09_PPH_CMOCCUR_88_6 (Were any of the following medications given to prevent/treat PPH?, No medications given)
# M09_PPH_CMOCCUR_99_6 (Were any of the following medications given to prevent/treat PPH?, Don't know)

## Data quality checks and query export 
# Combines hemorrhage cases requiring site review and exports site-specific Excel workbooks.
# Queries flag records for review and do not automatically exclude them from the outcome.
# Previously resolved queries are removed in Query Export Step 2 to avoid repeat queries.
# Confirm these exclusions are working before sending files to sites.
# Document any new exclusions after September 28, 2026 and apply them to the specific query only.
#*****************************************************************************

### data queries 
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(data.table)
library(openxlsx)
library(haven)
library(readxl)

## set upload dates
UploadDate = "2026-05-01"

# Import data ---- 
## Set path to save ---- 
path_to_save <- paste0("~/Analysis/Hemorrhage/", UploadDate, "/derived_data")
dir.create(path_to_save, recursive = TRUE, showWarnings = FALSE)

path_to_output <- paste0("~/Analysis/Hemorrhage/", UploadDate, "/output")
dir.create(path_to_output, recursive = TRUE, showWarnings = FALSE)

path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")

## Set path to data ---- 
path_to_data <- paste0("~/Analysis/Merged_data/", UploadDate)

## Query Folder relevant paths ---
path_to_queries <- paste0("~/Analysis/Hemorrhage/", UploadDate, "/queries")
dir.create(path_to_queries, recursive = TRUE, showWarnings = FALSE)
#Define the path where the PPH files are located
pph_directory <- "D:/Users/williams_pj/Documents/Analysis/Quarterly-Readouts/2026 Q2 06-04/case review"

## Import enrolment, infant outcomes and mat_endpoint ---- 
mat_enroll <- read_xlsx(paste0(path_to_tnt, "MAT_ENROLL" ,".xlsx" )) %>% select(SITE, MOMID, PREGID, ENROLL, PREG_START_DATE,EST_CONCEP_DATE_LMP, EST_CONCEP_DATE_US) %>% 
  filter(ENROLL == 1)

mat_end <- read_dta(paste0(path_to_tnt, "MAT_ENDPOINTS" ,".dta" )) %>% 
  ## only want all pregnancy endpoints excluding moms who have died before delivery and induced abortions
  ##PJW: Update to death before delivery, deaths not caused by delivery or at delivery (no labour occured)
  ##PJW: PREG_LOSS_DEATH are "non-pregnancy related deaths" or deaths without any birth outcome
  filter(PREG_END == 1 & PREG_LOSS_DEATH != 1 & PREG_LOSS_INDUCED == 0)

mat_end_full <- read_dta(paste0(path_to_tnt, "MAT_ENDPOINTS" ,".dta" )) 
## only want all pregnancy endpoints excluding moms who have died before delivery and induced abortions

inf_outcomes <- read.csv(paste0(path_to_tnt, "INF_OUTCOMES" ,".csv" )) %>% 
  select(SITE, MOMID, PREGID, DOB, LIVEBIRTH, FETAL_LOSS) %>% 
  group_by(SITE, PREGID) %>% 
  arrange(desc(DOB)) %>% 
  slice(1) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) %>% 
  ungroup()

## Import stacked csv forms ---- 
mnh19 <- read.csv(paste0("Z:/Stacked Data/", UploadDate,"/", "mnh19_merged.csv"))


## mnh04 ----
mnh04 <- read.csv(paste0("Z:/Stacked Data/", UploadDate, "/", "mnh04_merged.csv"))
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

## mnh09 ----
mnh09 <- read.csv(paste0("Z:/Stacked Data/", UploadDate,"/", "mnh09_merged.csv"))
if (any(duplicated(mnh09[c("SITE", "MOMID", "PREGID", "M09_MAT_LD_OHOSTDAT")]))) {
  # extract duplicated ids
  duplicates_ids_09 <- which(duplicated(mnh09[c("SITE", "MOMID", "PREGID", "M09_MAT_LD_OHOSTDAT")]) | 
                               duplicated(mnh09[c("SITE", "MOMID", "PREGID",  "M09_MAT_LD_OHOSTDAT")], fromLast = TRUE))
  duplicates_ids_09 <- mnh09[duplicates_ids_09, ]
  
  print(paste0("n= ",dim(duplicates_ids_09)[1],  " Duplicates in mnh09 exist"))
  
  # extract ids from main dataset
  mnh09 <- mnh09 %>% group_by(SITE, MOMID, PREGID, M09_MAT_LD_OHOSTDAT) %>% 
    arrange(desc(M09_MAT_LD_OHOSTDAT)) %>% 
    slice(1) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    select(-n) %>% 
    ungroup()
  
  
} else {
  print("No duplicates in mnh09")
}

## mnh12 ----
mnh12 <- read.csv(paste0("Z:/Stacked Data/", UploadDate,"/", "mnh12_merged.csv"))
if (any(duplicated(mnh12[c("SITE", "MOMID", "PREGID",  "M12_TYPE_VISIT", "M12_VISIT_OBSSTDAT")]))) {
  # extract duplicated ids
  duplicates_ids_12 <- which(duplicated(mnh12[c("SITE", "MOMID", "PREGID",  "M12_TYPE_VISIT", "M12_VISIT_OBSSTDAT")]) | 
                               duplicated(mnh12[c("SITE", "MOMID", "PREGID",   "M12_TYPE_VISIT", "M12_VISIT_OBSSTDAT")], fromLast = TRUE))
  duplicates_ids_12 <- mnh12[duplicates_ids_12, ]
  
  print(paste0("n= ",dim(duplicates_ids_12)[1],  " Duplicates in mnh12 exist"))
  
  # extract ids from main dataset
  mnh12 <- mnh12 %>% group_by(SITE, MOMID, PREGID, M12_TYPE_VISIT) %>% 
    arrange(desc(M12_VISIT_OBSSTDAT)) %>% 
    slice(1) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    select(-n) %>% 
    ungroup() %>% 
    mutate(M12_VISIT_OBSSTDAT = case_when(
      str_trim(M12_VISIT_OBSSTDAT) == "" ~ NA_character_,
      TRUE ~ M12_VISIT_OBSSTDAT
    ))  
  
} else {
  print("No duplicates in mnh12")
}

## mnh19 ----
#PJW: Because its possible that sites have extracted the data for hospitalizations
## We are removing duplicates by date mother presented for care
## harmonizing the two dates instead of M19_OBSSTDAT
mnh19 <- read.csv(paste0("Z:/Stacked Data/", UploadDate,"/", "mnh19_merged.csv"))

# Create a harmonized hospitalization date
mnh19 <- mnh19 %>%
  mutate(
    M19_HOSPDATE = case_when(

      !ymd(M19_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05")) ~
        ymd(M19_OHOSTDAT),
      !ymd(M19_MAT_EST_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05")) ~
        ymd(M19_MAT_EST_OHOSTDAT),
      !ymd(M19_ADMIT_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05")) ~
        ymd(M19_ADMIT_OHOSTDAT),
      TRUE ~ as.Date(NA)
    )
  )
if (any(duplicated(mnh19[c("SITE", "MOMID", "PREGID", "M19_HOSPDATE")]))) {
  
  # Extract duplicated records
  duplicates_ids_19 <- which(
    duplicated(mnh19[c("SITE", "MOMID", "PREGID", "M19_HOSPDATE")]) |
      duplicated(mnh19[c("SITE", "MOMID", "PREGID", "M19_HOSPDATE")], fromLast = TRUE)
  )
  
  duplicates_ids_19 <- mnh19[duplicates_ids_19, ]
  
  print(paste0("n= ", nrow(duplicates_ids_19), " Duplicates in mnh19 exist"))
  
  # Keep one record per hospitalization date
  mnh19 <- mnh19 %>%
    group_by(SITE, MOMID, PREGID, M19_HOSPDATE) %>%
    arrange(desc(M19_OBSSTDAT), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    select(-M19_HOSPDATE)
  
} else {
  mnh19 <- mnh19 %>%
    select(-M19_HOSPDATE)
  
  print("No duplicates in mnh19")
}



#****************************************************************
# Data Generation ----
# 1. generate wide dataset with necessary variables from mnh09/mnh04/mnh12
# 2. generate separate dataset with unscheduled visits 
#****************************************************************

# ---- Generate L&D dataset ---- 
hem_ld <- mnh09 %>% 
  right_join(mat_end[c("SITE", "MOMID", "PREGID","PREG_END",
                       "PREG_END_DATE", "PREG_END_GA", "PREG_LOSS")],
             by = c("SITE", "MOMID", "PREGID")) %>%
  select(SITE, MOMID, PREGID,M09_MAT_LD_OHOSTDAT, PREG_END_DATE,PREG_END, contains("M09")) %>% 
  # rename_with(~paste0(., "_", 6), .cols = c(contains("M09")))  %>% 
  distinct(SITE, MOMID, PREGID, .keep_all = TRUE) %>% 
  left_join(inf_outcomes, by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(TYPE_VISIT = 6) %>% 
  mutate(M09_MAT_LD_OHOSTDAT = ymd(M09_MAT_LD_OHOSTDAT)) %>% 
  select(SITE, MOMID, PREGID,TYPE_VISIT,PREG_END_DATE,M09_MAT_LD_OHOSTDAT, PREG_END,DOB, contains("PPH"), M09_APH_CEOCCUR) %>% 
  mutate(VISIT_DATE = DOB) %>% 
  mutate(
    RSN_PPH_MED_OCY	= case_when(M09_PPH_CMOCCUR_1 == 1 ~ 1, M09_PPH_CMOCCUR_1 == 0 ~ 0, TRUE ~ 77),
    RSN_PPH_MED_MISO	= case_when(M09_PPH_CMOCCUR_2 == 1 ~ 1, M09_PPH_CMOCCUR_2 == 0 ~ 0, TRUE ~ 77), 
    RSN_PPH_MED_TRANEX	= case_when(M09_PPH_CMOCCUR_3 == 1 ~ 1, M09_PPH_CMOCCUR_3 == 0 ~ 0, TRUE ~ 77), 
    RSN_PPH_MED_CARBET	= case_when(M09_PPH_CMOCCUR_4 == 1 ~ 1, M09_PPH_CMOCCUR_4 == 0 ~ 0, TRUE ~ 77),
    RSN_PPH_MED_METHYL	= case_when(M09_PPH_CMOCCUR_5 == 1 ~ 1, M09_PPH_CMOCCUR_5 == 0 ~ 0, TRUE ~ 77), 
    RSN_PPH_MED_CARBOP	= case_when(M09_PPH_CMOCCUR_6 == 1 ~ 1, M09_PPH_CMOCCUR_6 == 0 ~ 0, TRUE ~ 77),
    RSN_PPH_MED_NONE	= case_when(M09_PPH_CMOCCUR_77 == 1 ~ 1, M09_PPH_CMOCCUR_77 == 0 ~ 0, TRUE ~ 77), 
    RSN_PPH_MED_OTHER	= case_when(M09_PPH_CMOCCUR_88 == 1 ~ 1, M09_PPH_CMOCCUR_88 == 0 ~ 0, TRUE ~ 77), 
    BLOOD_LOSS_METHOD_IPC = case_when(M09_PPH_PEMETHOD %in% c(NA, 55,77,99) ~99,  TRUE ~ M09_PPH_PEMETHOD),  
    
  )

# ---- Generate hospitalization dataset ---- 
hem_hosp <- mnh19 %>% 
  ## merge in mat_endpoints and mat_enroll datasets
  left_join(mat_end[c("SITE", "MOMID", "PREGID", "PREG_END", "PREG_END_DATE")], by = c("SITE", "MOMID", "PREGID")) %>% 
  select(SITE, MOMID,  PREGID, PREG_END, PREG_END_DATE, M19_TIMING_OHOCAT, M19_VAG_BLEED_CEOCCUR, M19_LD_COMPL_MHTERM_4,
         M19_LD_COMPL_ML, M19_LD_COMPL_MHTERM_5, M19_TX_PROCCUR_1,  M19_OBSSTDAT, M19_ADMIT_OHOSTDAT, M19_OHOSTDAT, 
         M19_MAT_EST_OHOSTDAT) %>% 
  left_join(mat_enroll[c("SITE", "MOMID", "PREGID", "PREG_START_DATE")], by = c("SITE", "MOMID", "PREGID")) %>% 
  ## only want participants who have had a pregnancy endpoint 
  filter(PREG_END == 1) %>% 
  mutate(HEM_HOSP_ANY = case_when(
    M19_LD_COMPL_MHTERM_4 ==1 |
      M19_LD_COMPL_MHTERM_5 == 1 | 
      M19_LD_COMPL_ML >= 500 ~ 1, 
    TRUE ~ 0)) %>% 
  ## calculate age at hospitalization 
  mutate(AGE_AT_HOSP = case_when( # anc (if hospitalization comes before pregnancy end date, then ANC)
    
    (!ymd(M19_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_OHOSTDAT) < ymd(PREG_END_DATE) ~  as.numeric(ymd(M19_OHOSTDAT) - ymd(PREG_START_DATE)),
    (!ymd(M19_MAT_EST_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_MAT_EST_OHOSTDAT) < ymd(PREG_END_DATE) ~  as.numeric(ymd(M19_MAT_EST_OHOSTDAT) - ymd(PREG_START_DATE)),
    (!ymd(M19_ADMIT_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_ADMIT_OHOSTDAT) < ymd(PREG_END_DATE) ~  as.numeric(ymd(M19_ADMIT_OHOSTDAT) - ymd(PREG_START_DATE)),
    
    # pnc (if hospitalization comes on or after pregnancy end date, then PNC)
    (!ymd(M19_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_OHOSTDAT) >= ymd(PREG_END_DATE) ~  as.numeric(ymd(M19_OHOSTDAT) - ymd(PREG_END_DATE)),
    (!ymd(M19_MAT_EST_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_MAT_EST_OHOSTDAT) >= ymd(PREG_END_DATE) ~  as.numeric(ymd(M19_MAT_EST_OHOSTDAT) - ymd(PREG_END_DATE)),
    (!ymd(M19_ADMIT_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_ADMIT_OHOSTDAT) < ymd(PREG_END_DATE) ~  as.numeric(ymd(M19_ADMIT_OHOSTDAT) - ymd(PREG_END_DATE)),
    TRUE ~ NA
  ),
  AGE_AT_HOSP_WKS = AGE_AT_HOSP %/% 7,
  DATE_HOSPITAL = case_when(!ymd(M19_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05")) ~ ymd(M19_OHOSTDAT),
                            !ymd(M19_MAT_EST_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05")) ~ ymd(M19_MAT_EST_OHOSTDAT),
                            !ymd(M19_ADMIT_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05")) ~ ymd(M19_ADMIT_OHOSTDAT),
                            TRUE ~ NA)) %>% 
  # generate ANC and PNC variables, where 2= pnc and 1= anc
  mutate(TIMING_HOSP = case_when(
    is.na(DATE_HOSPITAL) ~ NA_integer_,
    DATE_HOSPITAL < ymd(PREG_END_DATE) ~ 1L,
    DATE_HOSPITAL >= ymd(PREG_END_DATE) ~ 2L,
    TRUE ~ NA
  )) %>% 
  mutate(TIMING_HOSP_TEXT = case_when(TIMING_HOSP ==1 & HEM_HOSP_ANY ==1 ~ "HOSPITAL HEM ANC",
                                      TIMING_HOSP ==2 & HEM_HOSP_ANY ==1 ~ "HOSPITAL HEM PNC",
                                      TRUE ~ NA 
  )) %>% 
  # generate date of hemorrhage at hospitalization event
  mutate(DATE_HOSPITAL_HEM_ANC = case_when(HEM_HOSP_ANY ==1 & TIMING_HOSP ==1 ~ DATE_HOSPITAL,
                                           TRUE ~ NA_Date_
  )) %>% 
  mutate(DATE_HOSPITAL_HEM_PNC = case_when(HEM_HOSP_ANY ==1 & TIMING_HOSP ==2 ~ DATE_HOSPITAL,
                                           TRUE ~ NA_Date_
  )) %>% 
  ## if a participant has multiple hospitalizations during ANC or PNC, take the earliest incident of APH or PPH
  group_by(MOMID, PREGID) %>% 
  arrange(DATE_HOSPITAL) %>% 
  mutate(
    KEEP_ANC = if_else(TIMING_HOSP == 1 & HEM_HOSP_ANY == 1, 1, 0),
    KEEP_PNC = if_else(TIMING_HOSP == 2 & HEM_HOSP_ANY == 1, 1, 0)
  ) %>%
  #filter (HEM_HOSP_ANY == 1)
  filter(
    KEEP_ANC == 1 | KEEP_PNC == 1
  ) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(SITE, MOMID, PREGID, PREG_START_DATE, PREG_END_DATE, M19_OHOSTDAT, M19_MAT_EST_OHOSTDAT,DATE_HOSPITAL,
         AGE_AT_HOSP, TIMING_HOSP,DATE_HOSPITAL_HEM_ANC, DATE_HOSPITAL_HEM_PNC, M19_TIMING_OHOCAT, HEM_HOSP_ANY ,
         M19_TX_PROCCUR_1, M19_VAG_BLEED_CEOCCUR, M19_LD_COMPL_MHTERM_4, M19_LD_COMPL_MHTERM_5,
         M19_LD_COMPL_ML, TIMING_HOSP_TEXT) 


# ---- Merge all files and generate hemorrhage indicators ---- 
hem <- mat_enroll %>% 
  # select(SITE, MOMID, PREGID, PREG_START_DATE) %>% 
  full_join(hem_ld %>% select(-PREG_END, -PREG_END_DATE, -DOB), by = c("SITE", "MOMID", "PREGID")) %>%
  full_join(mnh04 %>% rename(TYPE_VISIT = "M04_TYPE_VISIT") %>%
              mutate(VISIT_DATE = M04_ANC_OBSSTDAT) %>%
              select(SITE, MOMID, PREGID,TYPE_VISIT,VISIT_DATE,M04_ANC_OBSSTDAT, M04_APH_CEOCCUR), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>%
  full_join(mnh12 %>% rename(TYPE_VISIT = "M12_TYPE_VISIT") %>% 
              mutate(VISIT_DATE = M12_VISIT_OBSSTDAT) %>%
              select(SITE, MOMID, PREGID,TYPE_VISIT,VISIT_DATE, M12_VISIT_OBSSTDAT, M12_BIRTH_COMPL_MHTERM_1, M12_VAG_BLEED_CESTDAT, M12_VAG_BLEED_LOSS_ML), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>%
  full_join(hem_hosp %>% mutate(VISIT_DATE = as.character(DATE_HOSPITAL)) %>% 
              select(-PREG_START_DATE, -PREG_END_DATE), by = c("SITE", "MOMID", "PREGID", "VISIT_DATE")) %>%
  full_join(mat_end[c("SITE", "MOMID", "PREGID","PREG_END",
                      "PREG_END_DATE", "PREG_END_GA", "PREG_LOSS")],
            by = c("SITE", "MOMID", "PREGID")) %>% 
  full_join(hem_ld %>% select(SITE, MOMID, PREGID, DOB), by = c("SITE", "MOMID", "PREGID")) %>%
  
  # hemorrhage in MNH04
  mutate(M04_MAT_HEM = case_when(M04_APH_CEOCCUR==1 ~ 1,
                                 M04_APH_CEOCCUR==0 ~ 0,
                                 TRUE ~ 55),
  ) %>% 
  # hemorrhage in MNH12
  # BIRTH_COMPL_MHTERM_1 [MNH12] (Was the mother diagnosed with any of the following birth complications, PPH)
  # M12_VAG_BLEED_LOSS_ML [MNH12] >=500 (Record estimated blood loss)
  mutate(M12_MAT_HEM = case_when(M12_BIRTH_COMPL_MHTERM_1==1 | 
                                   M12_VAG_BLEED_LOSS_ML >= 500 ~ 1, 
                                 TRUE ~ 0),
  )  %>% 
  
  # hemorrhage in MNH09
  # PPH_CEOCCUR==1 [MNH09] (Did mother experience postpartum hemorrhage)
  # PPH_ESTIMATE_FAORRES >=500 (Record estimated blood loss)
  # PPH_FAORRES_1==1 [MNH09] (Procedures carried out for PPH, Balloon/condom tamponade)
  # PPH_FAORRES_2==1 [MNH09] (Procedures carried out for PPH, Surgical interventions)
  # PPH_FAORRES_3==1 [MNH09] (Procedures carried out for PPH, Brace sutures)
  # PPH_FAORRES_4==1 [MNH09] (Procedures carried out for PPH, Vessel ligation)
  # PPH_FAORRES_5==1 [MNH09] (Procedures carried out for PPH, Hysterectomy)
  # PPH_FAORRES_88==1 [MNH09] (Procedures carried out for PPH, Other)
  # PPH_TRNSFSN_PROCCUR==1 [MNH09] (Did the mother need a transfusion?)
  mutate(M09_MAT_HEM = case_when(M09_PPH_CEOCCUR==1 | M09_PPH_FAORRES_1==1 | M09_PPH_FAORRES_2==1 |
                                   M09_PPH_FAORRES_3==1 | M09_PPH_FAORRES_4==1 |
                                   M09_PPH_FAORRES_5==1 | 
                                   M09_PPH_TRNSFSN_PROCCUR==1 | M09_PPH_ESTIMATE_FAORRES >=500 ~ 1, 
                                 TRUE ~ 0)) %>% 
  
  # severe hemorrhage in MNH09/MNH12/MNH19
  # PPH_ESTIMATE_FAORRES/M12_VAG_BLEED_LOSS_ML/M19_LD_COMPL_ML >=1000 [MNH09/12/19] (Record estimated blood loss) 
  # PPH_FAORRES_1==1 [MNH09] (Procedures carried out for PPH, Balloon/condom tamponade)  
  # PPH_FAORRES_2==1 [MNH09] (Procedures carried out for PPH, Surgical interventions) 
  # PPH_FAORRES_3==1 [MNH09] (Procedures carried out for PPH, Brace sutures) 
  # PPH_FAORRES_4==1 [MNH09] (Procedures carried out for PPH, Vessel ligation) 
  # PPH_FAORRES_5==1 [MNH09] (Procedures carried out for PPH, Hysterectomy) 
  # PPH_FAORRES_88==1 [MNH09] (Procedures carried out for PPH, Other) 
  # PPH_TRNSFSN_PROCCUR==1 [MNH09] (Did the mother need a transfusion?) 
  mutate(M09_MAT_HEM_SEV = case_when(M09_PPH_ESTIMATE_FAORRES >=1000 |
                                       M09_PPH_TRNSFSN_PROCCUR ==1 |
                                       M09_PPH_FAORRES_1==1 |
                                       M09_PPH_FAORRES_2==1 |
                                       M09_PPH_FAORRES_3==1 | M09_PPH_FAORRES_4==1 |
                                       M09_PPH_FAORRES_5==1~ 1, TRUE ~ 0),
         
         M12_MAT_HEM_SEV = case_when(M12_VAG_BLEED_LOSS_ML>=1000  ~ 1, TRUE ~ 0),
         M19_MAT_HEM_SEV = case_when(M19_LD_COMPL_ML >=1000  & TIMING_HOSP == 2 ~ 1, TRUE ~ 0)
  ) %>% 
  # generate indicator variable to remove any unscheduled visits that do not have an outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & (M04_MAT_HEM ==1 | M12_MAT_HEM ==1) ~ 1, # keep if hemorrhage was reported at an unscheduled visit
                                  !TYPE_VISIT %in% c(13,14) ~ 1, # keep if not unscheduled 
                                  TRUE ~ 0)) %>% 
  filter(KEEP_UNSCHED==1) %>% 
  mutate(M12_VAG_BLEED_CESTDAT = ymd(M12_VAG_BLEED_CESTDAT)) %>% 
  mutate(RSN_APH_REPORT_ANC = case_when(M04_APH_CEOCCUR == 1 ~ 1, M04_APH_CEOCCUR == 0 ~ 0, TRUE ~ NA),
         RSN_APH_REPORT_IPC = case_when(M09_APH_CEOCCUR == 1 ~ 1, M09_APH_CEOCCUR == 0 ~ 0, TRUE ~ NA),
         RSN_EST_BLOOD_LOSS_IPC = M09_PPH_ESTIMATE_FAORRES,
         RSN_PPH_PROC_IPC_BALLOON = case_when(M09_PPH_FAORRES_1 == 1 ~ 1, M09_PPH_FAORRES_1 == 0 ~ 0, TRUE ~ NA),  
         RSN_PPH_PROC_IPC_SURG	= case_when(M09_PPH_FAORRES_2 == 1 ~ 1, M09_PPH_FAORRES_2 == 0 ~ 0, TRUE ~ NA),  
         RSN_PPH_PROC_IPC_BRACE	= case_when(M09_PPH_FAORRES_3 == 1 ~ 1, M09_PPH_FAORRES_3 == 0 ~ 0, TRUE ~ NA), 
         RSN_PPH_PROC_IPC_VESSEL	= case_when(M09_PPH_FAORRES_4 == 1 ~ 1, M09_PPH_FAORRES_4 == 0 ~ 0, TRUE ~ NA), 
         RSN_PPH_PROC_IPC_HYSTER	= case_when(M09_PPH_FAORRES_5 == 1 ~ 1, M09_PPH_FAORRES_5 == 0 ~ 0, TRUE ~ NA), 
         RSN_PROC_PPH_IPC_OTHER	= case_when(M09_PPH_FAORRES_88 == 1 ~ 1, M09_PPH_FAORRES_88 == 0 ~ 0, TRUE ~ NA),
         RSN_BLOOD_TRANS_IPC	= case_when(M09_PPH_TRNSFSN_PROCCUR == 1 ~ 1, M09_PPH_TRNSFSN_PROCCUR == 0 ~ 0, TRUE ~ NA),
         RSN_PPH_REPORT_IPC	= case_when(M09_PPH_CEOCCUR == 1 ~ 1, M09_PPH_CEOCCUR == 0 ~ 0, TRUE ~ NA),  
         RSN_PPH_REPORT_BIRTH_COMPL_PNC	= case_when(M12_BIRTH_COMPL_MHTERM_1 == 1 ~ 1, M12_BIRTH_COMPL_MHTERM_1 == 0 ~ 0, TRUE ~ NA),   
         RSN_EST_BLOOD_LOSS_PNC	= M12_VAG_BLEED_LOSS_ML,
         RSN_APH_REPORT_HOSP = case_when(M19_LD_COMPL_MHTERM_4 == 1 & TIMING_HOSP ==1 ~ 1, TRUE ~ 0), 
         RSN_PPH_REPORT_HOSP = case_when(M19_LD_COMPL_MHTERM_5 == 1 & TIMING_HOSP ==2 ~ 1, TRUE ~ 0), 
         RSN_EST_BLOOD_LOSS_HOSP_ANC = case_when(TIMING_HOSP ==1 ~ M19_LD_COMPL_ML, TRUE ~ -7), 
         RSN_EST_BLOOD_LOSS_HOSP_PNC = case_when(TIMING_HOSP ==2 ~ M19_LD_COMPL_ML, TRUE ~ -7)
  ) 

# ---- Generate date of events and form source ---- 
hem_export <- hem %>% 
  # generate visit sequence variable. This will help us calculate date of hemorrhage for unscheduled visits (for example. scenarios where mom reported hemorrhage since the last study visit; we would want to take the midpoint)
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  
  ## generate denominator - all participants with a birth reported
  mutate(HEM_DENOM = case_when(PREG_END==1 ~ 1, TRUE ~ 0) ## denominator is all participants with a birth reported
  ) %>% 
  group_by(SITE, MOMID, PREGID) %>% 
  ## 1. Antepartum Hemorrhage
  mutate(HEM_APH = case_when(HEM_DENOM==1 & (M04_MAT_HEM ==1 | M09_APH_CEOCCUR ==1 | (HEM_HOSP_ANY==1 & TIMING_HOSP==1)) ~ 1, TRUE ~ 0)) %>% 
  mutate(HEM_APH_SOURCE = case_when(HEM_DENOM ==1 & M04_MAT_HEM ==1 & M09_APH_CEOCCUR %in% c(0,55,77,99,NA) & is.na(TIMING_HOSP_TEXT) ~ "MNH04",
                                    HEM_DENOM ==1 & M04_MAT_HEM !=1 & M09_APH_CEOCCUR %in% c(0,55,77,99,NA) & TIMING_HOSP_TEXT == "HOSPITAL HEM ANC" ~ "MNH19",
                                    HEM_DENOM ==1 & M04_MAT_HEM ==1 & M09_APH_CEOCCUR %in% c(0,55,77,99,NA) & TIMING_HOSP_TEXT == "HOSPITAL HEM ANC" ~ "MNH04 & MNH19",
                                    HEM_DENOM ==1 & M04_MAT_HEM ==1 & M09_APH_CEOCCUR ==1 ~ "MNH04 & MNH09",
                                    HEM_DENOM ==1 & M04_MAT_HEM !=1 & M09_APH_CEOCCUR ==1 ~ "MNH09",
                                    TRUE ~ NA
  )) %>% 
  # generate date of antepartum hemorrhage by taking the midpoint between the last study visit and the reported study visit
  arrange(PREGID, VISIT_SEQ) %>%
  mutate(VISIT_DATE = as.Date(VISIT_DATE)) %>%  
  group_by(PREGID) %>% 
  mutate(HEM_APH_DATE = case_when(HEM_APH == 1 & VISIT_SEQ == 1 ~ VISIT_DATE, 
                                           HEM_APH == 1 & M09_APH_CEOCCUR == 1 & VISIT_SEQ == 1 ~ M09_MAT_LD_OHOSTDAT, 
                                           HEM_APH == 1 ~ VISIT_DATE - ((VISIT_DATE - lag(VISIT_DATE)) / 2),
                                           TRUE ~ NA_Date_),
         HEM_APH_DATE_VISIT =  case_when(HEM_APH == 1 & M09_APH_CEOCCUR == 1 & VISIT_SEQ ==1 ~ M09_MAT_LD_OHOSTDAT, 
                                         HEM_APH == 1 ~ VISIT_DATE,
                                         TRUE ~ NA_Date_)
         
  ) %>% 
  ungroup() %>% 
  ## 2. Postpartum Hemorrhage; if reported in mnh09 or mnh12 or mnh19 (during pnc)
  mutate(HEM_PPH = case_when(HEM_DENOM == 1 & (M09_MAT_HEM ==1  | M12_MAT_HEM ==1 | (HEM_HOSP_ANY==1 & TIMING_HOSP==2)) ~ 1, TRUE ~ 0)) %>% 
  group_by(SITE, MOMID, PREGID) %>%
  # mutate(HEM_PPH_SOURCE = c(
  #   if (any(HEM_DENOM == 1 & M09_MAT_HEM == 1 & M12_MAT_HEM != 1 & is.na(TIMING_HOSP_TEXT), na.rm = TRUE)) "MNH09" else NULL,
  #   if (any(HEM_DENOM == 1 & M09_MAT_HEM != 1 & M12_MAT_HEM == 1 & is.na(TIMING_HOSP_TEXT), na.rm = TRUE)) "MNH12" else NULL,
  #   if (any(HEM_DENOM == 1 & M09_MAT_HEM != 1 & M12_MAT_HEM != 1 & TIMING_HOSP_TEXT == "HOSPITAL HEM PNC", na.rm = TRUE)) "MNH19" else NULL
  # ) %>% paste(collapse = " & ")) %>%
  # mutate(HEM_PPH_SOURCE = ifelse(HEM_PPH_SOURCE == "", NA, HEM_PPH_SOURCE)) %>%
  #PJW change: Fix this chunck of code due to errors with collapsing multi-form sources
  mutate(
    HEM_PPH_SOURCE = {
      sources <- c(
        if (any(HEM_DENOM == 1 & M09_MAT_HEM == 1, na.rm = TRUE)) "MNH09",
        if (any(HEM_DENOM == 1 & M12_MAT_HEM == 1, na.rm = TRUE)) "MNH12",
        if (any(
          HEM_DENOM == 1 &
          !is.na(TIMING_HOSP_TEXT) &
          TIMING_HOSP_TEXT == "HOSPITAL HEM PNC",
          na.rm = TRUE
        )) "MNH19"
      )
      
      if (length(sources) == 0) NA_character_
      else paste(unique(sources), collapse = " & ")
    }
  ) %>%
  ungroup() %>% 
  # generate date of postpartum hemorrhage by taking the midpoint between the last study visit and the reported study visit
  arrange(PREGID, VISIT_SEQ) %>%
  mutate(VISIT_DATE = as.Date(VISIT_DATE)) %>%  
  group_by(PREGID) %>% 
  ## need to generate vaginal bleeding date indicator
  ## PJW Edit: Modifying date to better capture multiform nuance 
  mutate(
    ## Calculate the estimated event date for MNH12.
    ## The event is assumed to have occurred halfway between the previous visit
    ## and the MNH12 visit.
    M12_MIDPOINT_DATE = case_when(
      HEM_PPH == 1 & grepl("\\bMNH12\\b", HEM_PPH_SOURCE) ~
        VISIT_DATE - ((VISIT_DATE - lag(VISIT_DATE)) / 2),
      TRUE ~ NA_Date_
    ),
    
    ## Assign the PPH event date based on the contributing source form(s)
    HEM_PPH_DATE = case_when(
      
      ## If MNH09 contributed (alone or with any other form),
      ## the event occurred during labor/delivery, so use the delivery date.
      HEM_PPH == 1 & grepl("\\bMNH09\\b", HEM_PPH_SOURCE) ~
        ymd(DOB),
      
      ## If both MNH12 and MNH19 contributed (and MNH09 did not),
      ## use the earliest estimated event date.
      HEM_PPH == 1 &
        grepl("\\bMNH12\\b", HEM_PPH_SOURCE) &
        grepl("\\bMNH19\\b", HEM_PPH_SOURCE) ~
        pmin(M12_MIDPOINT_DATE, ymd(DATE_HOSPITAL_HEM_PNC), na.rm = TRUE),
      
      ## If only MNH12 contributed,
      ## use the midpoint between the previous visit and the MNH12 visit.
      HEM_PPH == 1 &
        grepl("\\bMNH12\\b", HEM_PPH_SOURCE) ~
        M12_MIDPOINT_DATE,
      
      ## If only MNH19 contributed,
      ## use the recorded hospital hemorrhage date.
      HEM_PPH == 1 &
        grepl("\\bMNH19\\b", HEM_PPH_SOURCE) ~
        ymd(DATE_HOSPITAL_HEM_PNC),
      
      ## Otherwise, no event date is assigned.
      TRUE ~ NA_Date_
    )
  ) %>%
  # mutate(HEM_PPH_DATE = case_when(HEM_PPH == 1 & HEM_PPH_SOURCE %in% c("MNH09", "MNH09 & MNH12", "MNH09 & MNH19", "MNH09 & MNH12 & MNH19") ~ ymd(DOB), ## if event occured during L&D, pph date is the date of delivery 
  #                                 HEM_PPH == 1 & HEM_PPH_SOURCE == "MNH12" ~  VISIT_DATE - ((VISIT_DATE - lag(VISIT_DATE)) / 2), ## if event occured only during L&D, pph date is the date of delivery 
  #                                 HEM_PPH == 1 & HEM_PPH_SOURCE == "MNH19" ~  ymd(DATE_HOSPITAL_HEM_PNC), ## if event occured only during L&D, pph date is the date of delivery 
  #                                 TRUE ~ NA_Date_),
  #        M12_MIDPOINT_DATE = case_when(HEM_PPH ==1 & HEM_PPH_SOURCE == "MNH12" ~  VISIT_DATE - ((VISIT_DATE - lag(VISIT_DATE)) / 2), TRUE ~ NA_Date_),
  #        HEM_PPH_DATE = case_when(HEM_PPH == 1 & HEM_PPH_SOURCE == "MNH12 & MNH19" ~  pmin(M12_MIDPOINT_DATE, DATE_HOSPITAL_HEM_PNC), 
  #                                 TRUE ~ HEM_PPH_DATE)
  # ) %>% 
  ungroup() %>% 
  ## 3. Severe postpartum hemorrhage
  mutate(HEM_PPH_SEV = case_when(HEM_DENOM==1 & HEM_PPH==1 & (M09_PPH_ESTIMATE_FAORRES>=1000 |
                                                                M09_PPH_TRNSFSN_PROCCUR ==1 |
                                                                M09_PPH_FAORRES_1==1 | 
                                                                M09_PPH_FAORRES_2==1 |
                                                                M09_PPH_FAORRES_3==1 | M09_PPH_FAORRES_4==1 |
                                                                M09_PPH_FAORRES_5==1 |
                                                                M12_VAG_BLEED_LOSS_ML>=1000  |
                                                                (M19_LD_COMPL_ML >=1000  & TIMING_HOSP == 2)) ~ 1, TRUE ~0)
  ) %>%
  group_by(SITE, MOMID, PREGID) %>%
  #PJW change: Fix this chunck of code due to errors with collapsing multi-form sources
  mutate(
    HEM_PPH_SEV_SOURCE = {
      sources <- c(
        if (any(HEM_DENOM == 1 & M09_MAT_HEM_SEV== 1, na.rm = TRUE)) "MNH09",
        if (any(HEM_DENOM == 1 & M12_MAT_HEM_SEV== 1, na.rm = TRUE)) "MNH12",
        if (any(
          HEM_DENOM == 1 &  (M19_LD_COMPL_ML >=1000  & TIMING_HOSP == 2),
          na.rm = TRUE
        )) "MNH19"
      )
      if (length(sources) == 0) NA_character_
      else paste(unique(sources), collapse = " & ")
        }
      ) %>%
    ungroup() %>% 
  arrange(PREGID, VISIT_SEQ) %>%
  mutate(VISIT_DATE = as.Date(VISIT_DATE)) %>%  
  group_by(PREGID) %>% 
  mutate(
    # generate date of postpartum hemorrhage by taking the midpoint between the last study visit and the reported study visit
    ## Estimate the MNH12 event date as the midpoint between the previous
    ## visit and the MNH12 visit.
    M12_MIDPOINT_DATE = case_when(
      HEM_PPH_SEV == 1 &
        grepl("\\bMNH12\\b", HEM_PPH_SEV_SOURCE) ~
        VISIT_DATE - ((VISIT_DATE - lag(VISIT_DATE)) / 2),
      TRUE ~ NA_Date_
    ),
    
    ## Assign the severe PPH event date based on the contributing source form(s)
    HEM_PPH_SEV_DATE = case_when(
      ## If MNH09 contributed (alone or with any other form),
      ## assume the event occurred during labor/delivery.
      HEM_PPH_SEV == 1 &
        grepl("\\bMNH09\\b", HEM_PPH_SEV_SOURCE) ~
        ymd(DOB),
      
      ## If both MNH12 and MNH19 contributed (without MNH09),
      ## use the earliest available event date.
      HEM_PPH_SEV == 1 &
        grepl("\\bMNH12\\b", HEM_PPH_SEV_SOURCE) &
        grepl("\\bMNH19\\b", HEM_PPH_SEV_SOURCE) ~
        pmin(
          M12_MIDPOINT_DATE,
          ymd(DATE_HOSPITAL_HEM_PNC),
          na.rm = TRUE
        ),
      
      ## If only MNH12 contributed,
      ## use the estimated midpoint date.
      HEM_PPH_SEV == 1 &
        grepl("\\bMNH12\\b", HEM_PPH_SEV_SOURCE) ~
        M12_MIDPOINT_DATE,
      
      ## If only MNH19 contributed,
      ## use the hospital hemorrhage date.
      HEM_PPH_SEV == 1 &
        grepl("\\bMNH19\\b", HEM_PPH_SEV_SOURCE) ~
        ymd(DATE_HOSPITAL_HEM_PNC),
      
      TRUE ~ NA_Date_
    )
  ) %>% 
  ungroup() 

table(hem_export$HEM_APH, hem_export$SITE)   

# ---- Data quality checks ----
## Generate query list ----
hem_query_list <- list()
## 1. cases where multiple events in MNH12 across several visits ----
query_mult_events_m12 <- hem_export %>% 
  filter(HEM_PPH_SOURCE %in% c("MNH12")) %>% 
  group_by(SITE, MOMID, PREGID) %>% 
  mutate(n = sum(HEM_PPH)) %>% 
  select(SITE, PREGID, TYPE_VISIT, VISIT_DATE,VISIT_SEQ, HEM_PPH, HEM_PPH_SOURCE,M09_MAT_HEM, 
         M12_MAT_HEM, M12_VISIT_OBSSTDAT, HEM_PPH_DATE, DOB, n) %>% 
  filter(n>1) %>% 
  filter (HEM_PPH == 1) %>% 
  mutate(exclusion_rsn = "multiple mnh12 events")

dim(query_mult_events_m12)

if(dim(query_mult_events_m12)[1] >0) {
  hem_query_list[["multiple mnh12 events"]] <- query_mult_events_m12
}

## 2. cases where the event date (HEM_PPH_DATE) is before the pregnancy end date/dob ----
query_event_dt_before_dob <- hem_export %>% filter(HEM_PPH ==1) %>% 
  filter(HEM_PPH_SOURCE == "MNH12") %>% 
  select(SITE, PREGID,  HEM_PPH, HEM_PPH_SOURCE, VISIT_DATE, DOB,
         PREG_END_DATE, M12_VISIT_OBSSTDAT, HEM_PPH_DATE, HEM_HOSP_ANY,TIMING_HOSP_TEXT) %>% 
  mutate(DATE_BEFORE_DOB = case_when(HEM_PPH_DATE < DOB ~ 1, TRUE ~ 0)) %>% 
  filter(DATE_BEFORE_DOB==1) %>% 
  mutate(exclusion_rsn = "pph before dob")
dim(query_event_dt_before_dob)

if(dim(query_event_dt_before_dob)[1] >0) {
  hem_query_list[["pph before dob"]] <- query_event_dt_before_dob
}

## 3. cases where PPH reported at mnh09 and mnh12 ----
query_event_m12_m09 <- hem_export %>% 
  filter(HEM_PPH_SOURCE %in% c("MNH09 & MNH12")) %>% 
  group_by(SITE, MOMID, PREGID) %>% 
  mutate(n = sum(HEM_PPH)) %>% 
  filter (HEM_PPH == 1 & RSN_PPH_REPORT_BIRTH_COMPL_PNC != 1) %>% 
  select(SITE, PREGID, TYPE_VISIT, VISIT_DATE,VISIT_SEQ, HEM_PPH, HEM_PPH_SOURCE,M09_MAT_HEM, 
         M12_MAT_HEM,M12_VISIT_OBSSTDAT, HEM_PPH_DATE, DOB, n, RSN_PPH_REPORT_BIRTH_COMPL_PNC) %>% 
  filter(n>1) %>% 
  mutate(exclusion_rsn = "PPH reported at mnh09 and mnh12")
dim(query_event_m12_m09)

#For context sites can report pregnancy complications in 
if(dim(query_event_m12_m09)[1] >0) {
  hem_query_list[["pph reported at mnh09 and mnh12"]] <- query_event_m12_m09
}

# Create vectors for APH, PPH, SEVERE PPH inclusion criteria
aph_rsn <- c("RSN_APH_REPORT_ANC", "RSN_APH_REPORT_HOSP", "RSN_EST_BLOOD_LOSS_HOSP_ANC", "RSN_APH_REPORT_IPC")
pph_rsn <- c("RSN_EST_BLOOD_LOSS_IPC",grep("^RSN_PPH_PROC_", names(hem_export), value = TRUE),"RSN_BLOOD_TRANS_IPC", "RSN_PPH_REPORT_BIRTH_COMPL_PNC",
             "RSN_EST_BLOOD_LOSS_PNC", "RSN_PPH_REPORT_IPC", "RSN_PPH_REPORT_HOSP", "RSN_EST_BLOOD_LOSS_HOSP_PNC")
pph_sev_rsn <- c("RSN_EST_BLOOD_LOSS_IPC" ,grep("^RSN_PPH_PROC_", names(hem_export), value = TRUE),"RSN_BLOOD_TRANS_IPC",
                 "RSN_EST_BLOOD_LOSS_PNC", "RSN_PPH_REPORT_HOSP", "RSN_EST_BLOOD_LOSS_HOSP_PNC")

# Generate wide data with one row for each participant ----
# For each event (APH, PPH, SEVERE PPH): 
# 1. create var that pulls the max event 
# 2. create var that pulls the date of event; if participant has multiple events, pull the earliest 
# 3. create var that pulls the form source inclusion criteria pulled from 

hem_export_wide <- hem_export %>%
  
  filter(HEM_DENOM ==1) %>% ## only want participants with a pregnancy endpoint 
  select(SITE, MOMID, PREGID, VISIT_DATE, VISIT_SEQ, DOB, PREG_END_DATE, EST_CONCEP_DATE_LMP, 
         EST_CONCEP_DATE_US, M09_MAT_LD_OHOSTDAT, HEM_DENOM, HEM_APH, HEM_APH_SOURCE, 
         #HEM_APH_DATE_MIDPOINT, 
         HEM_APH_DATE_VISIT, HEM_APH_DATE, 
         HEM_PPH, HEM_PPH_SOURCE, HEM_PPH_DATE, HEM_PPH_SEV, HEM_PPH_SEV_SOURCE, HEM_PPH_SEV_DATE, contains("RSN")) %>% 
  group_by(SITE, MOMID, PREGID) %>%
  mutate(
    ## Antepartum hemorrhage (HEM_APH) ---- 
    
    # 1. create var that pulls the max event 
    HEM_APH_MAX = max(HEM_APH, na.rm = TRUE),
    
    # 2. create var that pulls the date of event; if participant has multiple events, pull the earliest 
    HEM_APH_DATE_MIDPOINT_MAX = if (any(HEM_APH == 1, na.rm = TRUE)) 
      min(HEM_APH_DATE[HEM_APH == 1], na.rm = TRUE) else NA_Date_,
    
    HEM_APH_DATE_VISIT_MAX = if (any(HEM_APH == 1, na.rm = TRUE)) 
      min(HEM_APH_DATE_VISIT[HEM_APH == 1], na.rm = TRUE) else NA_Date_,
    
    # 3. create var that pulls the form source inclusion criteria pulled from 
    HEM_APH_FORM_SOURCE_MAX = HEM_APH_SOURCE %>%
      na.omit() %>%                         # remove NAs
      str_split(",\\s*") %>%                # split comma-separated lists into vectors
      unlist() %>%                          # flatten list
      unique() %>%                          # keep only distinct sources
      sort() %>%                            # optional: alphabetical order
      paste(collapse = " & ") %>%            # re-collapse into single string
      ifelse(. == "", NA, .),               # return NA if empty
    .groups = "drop"
  ) %>%
  
  mutate(
    ## Postpartum hemorrhage (HEM_PPH)  ---- 
    
    # 1. create var that pulls the max event 
    HEM_PPH_MAX = max(HEM_PPH, na.rm = TRUE),
    
    # 2. create var that pulls the date of event; if participant has multiple events, pull the earliest 
    HEM_PPH_DATE_MAX = {
      pos_dates <- HEM_PPH_DATE[HEM_PPH == 1 & !is.na(HEM_PPH_DATE)]
      if (length(pos_dates) > 0) min(pos_dates, na.rm = TRUE) else as.Date(NA)
    },
    
    # 3. create var that pulls the form source inclusion criteria pulled from 
    HEM_PPH_FORM_SOURCE_MAX = HEM_PPH_SOURCE %>%
      na.omit() %>%                         # remove NAs
      str_split(",\\s*") %>%                # split comma-separated lists into vectors
      unlist() %>%                          # flatten list
      unique() %>%                          # keep only distinct sources
      sort() %>%                            # optional: alphabetical order
      paste(collapse = " & ") %>%            # re-collapse into single string
      ifelse(. == "", NA, .),               # return NA if empty
    .groups = "drop"
    
  ) %>% 
  
  ## Severe postpartum hemorrhage (HEM_PPH_SEV)  ---- 
  
  # 1. create var that pulls the max event
  mutate(HEM_PPH_SEV_MAX = max(HEM_PPH_SEV, na.rm = TRUE),
         
         # 2. create var that pulls the date of event; if participant has multiple events, pull the earliest
         HEM_PPH_SEV_DATE_MAX = {
           pos_dates <- HEM_PPH_SEV_DATE[HEM_PPH == 1 & !is.na(HEM_PPH_SEV_DATE)]
           if (length(pos_dates) > 0) min(pos_dates, na.rm = TRUE) else as.Date(NA)
         },
         
         # 3. create var that pulls the form source inclusion criteria pulled from
         HEM_PPH_SEV_FORM_SOURCE_MAX = HEM_PPH_SEV_SOURCE %>%
           na.omit() %>%                         # remove NAs
           str_split(",\\s*") %>%                # split comma-separated lists into vectors
           unlist() %>%                          # flatten list
           unique() %>%                          # keep only distinct sources
           sort() %>%                            # optional: alphabetical order
           paste(collapse = " & ") %>%            # re-collapse into single string
           ifelse(. == "", NA, .),               # return NA if empty
         .groups = "drop"
         
  )

# ---- Generate indicator variable that lists out all the inclusion criteria ----
# create var that pulls all the inclusion criteria 

hem_export_wide_rsn <- hem_export_wide %>% 
  rowwise() %>%
  ## APH ----
  mutate(
    APH_RSN_SOURCE = {
      vals <- c_across(all_of(aph_rsn))
      names(vals) <- aph_rsn  
      
      continuous_vars <- c("RSN_EST_BLOOD_LOSS_HOSP_ANC")
      
      # Evaluates each variable individually: 
      # Keeps categorical ONLY if they equal 1, keeps continuous ONLY if >= 500
      hits <- names(vals)[
        !is.na(vals) & (
          (!names(vals) %in% continuous_vars & vals == 1) |  
            (names(vals) %in% continuous_vars & vals >= 500)   
        )
      ]
      
      if (length(hits) == 0) NA_character_ else paste(hits, collapse = ", ")
    }
  ) %>% 
  ## PPH ----
  mutate(
    PPH_RSN_SOURCE = {
      vals <- c_across(all_of(pph_rsn))
      names(vals) <- pph_rsn 
      
      continuous_vars <- c("RSN_EST_BLOOD_LOSS_IPC", "RSN_EST_BLOOD_LOSS_PNC", "RSN_EST_BLOOD_LOSS_HOSP_PNC")
      
      hits <- names(vals)[
        !is.na(vals) & (
          (!names(vals) %in% continuous_vars & vals == 1) |  
            (names(vals) %in% continuous_vars & vals >= 500)   
        )
      ]
      
      if (length(hits) == 0) NA_character_ else paste(hits, collapse = ", ")
    }
  ) %>% 
  ## SEVERE PPH ----
  mutate(
    PPH_SEV_RSN_SOURCE = {
      vals <- c_across(all_of(pph_sev_rsn))
      names(vals) <- pph_sev_rsn  
      
      continuous_vars <- c("RSN_EST_BLOOD_LOSS_IPC", "RSN_EST_BLOOD_LOSS_PNC", "RSN_EST_BLOOD_LOSS_HOSP_PNC")
      
      hits <- names(vals)[
        !is.na(vals) & (
          (!names(vals) %in% continuous_vars & vals == 1) | 
            (names(vals) %in% continuous_vars & vals >= 1000) 
        )
      ]
      
      if (length(hits) == 0) NA_character_ else paste(hits, collapse = ", ")
    }
  ) %>%
  group_by(SITE, MOMID, PREGID) %>% 
  mutate(
    HEM_APH_RSN_SOURCE_MAX = APH_RSN_SOURCE %>%
      na.omit() %>%                         
      str_split(",\\s*") %>%                
      unlist() %>%                          
      unique() %>%                          
      sort() %>%                            
      paste(collapse = ", ") %>%            
      ifelse(. == "", NA, .)                
  ) %>%
  mutate(
    HEM_PPH_RSN_SOURCE_MAX = PPH_RSN_SOURCE %>%
      na.omit() %>%                         
      str_split(",\\s*") %>%                
      unlist() %>%                          
      unique() %>%                          
      sort() %>%                            
      paste(collapse = ", ") %>%            
      ifelse(. == "", NA, .)                
  ) %>% 
  mutate(
    HEM_PPH_SEV_RSN_SOURCE_MAX = PPH_SEV_RSN_SOURCE %>%
      na.omit() %>%                         
      str_split(",\\s*") %>%                
      unlist() %>%                          
      unique() %>%                          
      sort() %>%                            
      paste(collapse = ", ") %>%            
      ifelse(. == "", NA, .)                
  ) %>%
  ungroup() %>%
  select(SITE, MOMID, PREGID, DOB, PREG_END_DATE, HEM_DENOM, contains("_MAX")) %>% 
  distinct() %>% 
  rename_with(~ gsub("_MAX$", "", .x))

# generate a smaller dataset that pulls the max case for each inclusion criteria; this will later be merged onto the full dataset
hem_export_rsn_detail <- hem_export_wide %>%
  group_by(SITE, MOMID, PREGID) %>%
  summarise(across(all_of(c(aph_rsn, pph_rsn, pph_sev_rsn)),~ replace(max(.x, na.rm = TRUE), is.infinite(max(.x, na.rm = TRUE)), 77),
                   .names = "{.col}"),.groups = "drop") %>% 
  ungroup() 

## we would expect the wide dataset to match the dimensions of the pregnancy endpoints dataset 
dim(mat_end)
dim(hem_export_wide_rsn)
dim(hem_export_rsn_detail)

names (hem_export_rsn_detail)
names(hem_export_wide_rsn)


# ---- Merge dataset to export & Calculate age at event ----
hem_export_merged <- hem_export_wide_rsn %>% 
  # merge in inclusion criteria dataset generated above
  left_join(hem_export_rsn_detail, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(mat_enroll %>% select(SITE, PREGID, PREG_START_DATE), by = c("SITE", "PREGID")) %>% 
  mutate (HEM_APH_DATE = HEM_APH_DATE_MIDPOINT) %>%
  # calculate age at event 
  mutate(HEM_APH_GESTAGE_DAYS = case_when(HEM_APH ==1 ~ as.numeric(HEM_APH_DATE - ymd(PREG_START_DATE)), 
                                          TRUE ~ NA),
         HEM_APH_GESTAGE_WKS = HEM_APH_GESTAGE_DAYS %/% 7,
         HEM_PPH_AGE_PP_DAYS = case_when(HEM_PPH ==1 ~ as.numeric(HEM_PPH_DATE - PREG_END_DATE), 
                                         TRUE ~ NA),
         HEM_PPH_AGE_PP_WKS = HEM_PPH_AGE_PP_DAYS %/% 7,
         HEM_PPH_SEV_AGE_PP_DAYS = case_when(HEM_PPH_SEV ==1 ~ as.numeric(HEM_PPH_SEV_DATE - PREG_END_DATE), 
                                             TRUE ~ NA),
         HEM_PPH_SEV_AGE_PP_WKS = HEM_PPH_SEV_AGE_PP_DAYS %/% 7
  ) %>% 
  select(SITE, MOMID, PREGID, PREG_START_DATE, PREG_END_DATE, HEM_DENOM,
         HEM_APH, HEM_APH_DATE, HEM_APH_GESTAGE_DAYS, HEM_APH_GESTAGE_WKS, HEM_APH_FORM_SOURCE, HEM_APH_RSN_SOURCE,
         HEM_PPH, HEM_PPH_DATE, HEM_PPH_AGE_PP_DAYS, HEM_PPH_AGE_PP_WKS, HEM_PPH_FORM_SOURCE, HEM_PPH_RSN_SOURCE,
         HEM_PPH_SEV, HEM_PPH_SEV_DATE, HEM_PPH_SEV_AGE_PP_DAYS,HEM_PPH_SEV_AGE_PP_WKS, HEM_PPH_SEV_FORM_SOURCE, HEM_PPH_SEV_RSN_SOURCE,
         starts_with("RSN")
  ) %>% 
  left_join(hem_ld %>% select(SITE, PREGID, starts_with("RSN_PPH_MED"), BLOOD_LOSS_METHOD_IPC), by = c("SITE", "PREGID"))
dim(hem_export_merged)
names(hem_export_merged)

## run quick tab of each inclusion criteria 
rsn_vec <- c(aph_rsn, pph_rsn)
for (i in rsn_vec) {
  print(i)
  print(table(hem_export_merged[[i]], useNA = "ifany"))
}

# ---- Data quality checks: data to export ----

## 1. flag any missing PREG_END_DATES ---- 
query_missing_preg_end_date <- hem_export_merged %>% 
  filter(is.na(PREG_END_DATE)) %>% 
  select(SITE, MOMID, PREGID,  PREG_END_DATE, HEM_APH, HEM_PPH, HEM_PPH_FORM_SOURCE, HEM_PPH_RSN_SOURCE) %>% 
  left_join(mnh09 %>% select(SITE, MOMID, PREGID,M09_BIRTH_DSTERM_INF1, contains("M09_DELIV_DSSTDAT_INF")), by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(inf_outcomes %>% select(SITE, MOMID, PREGID,  LIVEBIRTH, FETAL_LOSS), by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(exclusion_rsn = "missing pregnancy end date")

dim(query_missing_preg_end_date)

if(dim(query_missing_preg_end_date)[1] >0) {
  hem_query_list[["missing pregnancy end date"]] <- query_missing_preg_end_date
}

## 2. flag any missing criteria ---- 
query_missing_criteria_aph <- hem_export_merged %>% 
  filter(HEM_APH ==1) %>% 
  select(SITE, MOMID, PREGID, contains("HEM_APH")) %>% 
  filter(is.na(HEM_APH_RSN_SOURCE)) %>% 
  left_join(hem_export %>% select(SITE, MOMID, PREGID, all_of(aph_rsn)), by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(exclusion_rsn = "missing aph criteria")

dim(query_missing_criteria_aph)

if(dim(query_missing_criteria_aph)[1] >0) {
  hem_query_list[["missing aph criteria"]] <- query_missing_criteria_aph
}

query_missing_criteria_pph <- hem_export_merged %>% 
  filter(HEM_PPH ==1) %>% 
  select(SITE, MOMID, PREGID, contains("HEM_PPH")) %>% 
  filter(is.na(HEM_PPH_RSN_SOURCE)) %>% 
  left_join(hem_export %>% select(SITE, MOMID, PREGID, all_of(pph_rsn)), by = c("SITE", "MOMID", "PREGID")) %>%
  mutate(exclusion_rsn = "missing pph criteria")

dim(query_missing_criteria_pph)

if(dim(query_missing_criteria_pph)[1] >0) {
  hem_query_list[["missing pph criteria"]] <- query_missing_criteria_pph
}

query_missing_criteria_sev <- hem_export_merged %>% 
  filter(HEM_PPH_SEV ==1) %>% 
  select(SITE, MOMID, PREGID, contains("HEM_PPH_SEV")) %>% 
  filter(is.na(HEM_PPH_SEV_RSN_SOURCE)) %>% 
  left_join(hem_export %>% select(SITE, MOMID, PREGID, all_of(pph_rsn)), by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(exclusion_rsn = "missing severe pph criteria") 

dim(query_missing_criteria_sev)

if(dim(query_missing_criteria_sev)[1] >0) {
  hem_query_list[["missing sev pph criteria"]] <- query_missing_criteria_sev
}

## 3. if reported at mnh09 and mnh12, make sure the right form source is selected ----
query_pph_source <- hem_export_merged %>% 
  filter(HEM_PPH ==1) %>% 
  select(SITE, MOMID, PREGID, contains("HEM_PPH")) %>% 
  filter(grepl("_PNC", HEM_PPH_RSN_SOURCE, ignore.case = TRUE) &
           grepl("_IPC", HEM_PPH_RSN_SOURCE, ignore.case = TRUE)) %>% 
  filter(HEM_PPH_FORM_SOURCE != "MNH09 & MNH12") %>%  ## filter out cases where reason included vars from both ipc and pnc but only one form selected
  left_join(hem_export_rsn_detail %>% select(SITE, MOMID, PREGID, all_of(pph_rsn)), by = c("SITE", "MOMID", "PREGID")) 

## 4. pph reported in late postpartum (>6 wks) ---- 

query_pph_late_pp <- hem_export_merged %>% 
  filter(HEM_PPH ==1) %>% 
  filter(HEM_PPH_AGE_PP_WKS >= 5) %>% 
  select(SITE, MOMID, PREGID, HEM_PPH,HEM_PPH_FORM_SOURCE, PREG_END_DATE, HEM_PPH_DATE, HEM_PPH_SEV_DATE,
         HEM_PPH_SEV, HEM_PPH_SEV_FORM_SOURCE, HEM_PPH_RSN_SOURCE, HEM_PPH_SEV_RSN_SOURCE, HEM_PPH_AGE_PP_WKS, HEM_PPH_AGE_PP_DAYS) %>% 
  mutate(exclusion_rsn = "pph reported late pp") 

dim(query_pph_late_pp)

if(dim(query_pph_late_pp)[1] >0) {
  hem_query_list[["pph reported late pp"]] <- query_pph_late_pp
}

## 5. if pph as a delivery complication was reported in mnh12, did they also have a report in mnh09 ---- 
query_pph_pnc_report <- hem_export_merged %>% 
  filter(HEM_PPH ==1 & str_detect(HEM_PPH_RSN_SOURCE, "RSN_PPH_REPORT_BIRTH_COMPL_PNC")) %>% 
  select(SITE, MOMID, PREGID, HEM_PPH,PREG_END_DATE, HEM_PPH_DATE, HEM_PPH_SEV_DATE,
         HEM_PPH_SEV, HEM_PPH_RSN_SOURCE, HEM_PPH_SEV_RSN_SOURCE, HEM_PPH_FORM_SOURCE) %>% 
  mutate(query = case_when(str_detect(HEM_PPH_RSN_SOURCE, "_IPC") != TRUE ~ 1, TRUE ~ 0)) %>% 
  filter(query ==1) %>% 
  left_join(hem_ld %>% select(-PREG_END_DATE, -DOB), by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(mnh12 %>% select(SITE, MOMID, PREGID,M12_TYPE_VISIT, M12_BIRTH_COMPL_MHTERM_1, M12_VAG_BLEED_LOSS_ML), by = c("SITE", "MOMID", "PREGID")) %>% 
  relocate(M12_TYPE_VISIT, .after = PREGID) %>% 
  mutate(exclusion_rsn = case_when(M12_VAG_BLEED_LOSS_ML >= 500 ~ "severe pph blood loss in mnh12 only",
                                   M12_BIRTH_COMPL_MHTERM_1 ==1 ~ "pph reported in mnh12 only",
                                   TRUE ~ NA)) %>% 
  filter(exclusion_rsn == "pph reported in mnh12 only")

dim(query_pph_pnc_report)

if(dim(query_pph_pnc_report)[1] >0) {
  hem_query_list[["pph reported in mnh12 only"]] <- query_pph_pnc_report
}

## 6. look at cases where pph blood loss was reported in mnh12 ---- 
query_pph_pnc_blood_loss <- hem_export_merged %>% 
  filter(HEM_PPH ==1 & str_detect(HEM_PPH_RSN_SOURCE, "RSN_EST_BLOOD_LOSS_PNC")) %>% 
  left_join(hem_ld %>% select(-PREG_END_DATE, -DOB), by = c("SITE", "MOMID", "PREGID")) %>% 
  filter(RSN_EST_BLOOD_LOSS_PNC >= 500) %>% 
  mutate(exclusion_rsn = case_when(# 1. -7 in M09, >= 500 in M12
    M09_PPH_ESTIMATE_FAORRES == -7 & RSN_EST_BLOOD_LOSS_PNC >= 500 ~ "pph blood loss in mnh12 only (-7 MNH09)",
    
    # 2. 500-999 in M09, >= 1000 in M12
    M09_PPH_ESTIMATE_FAORRES >= 500 & M09_PPH_ESTIMATE_FAORRES < 1000 &
      RSN_EST_BLOOD_LOSS_PNC >= 1000 ~ "pph in mnh09 then severe in mnh12",
    
    # 3. 0-499 in M09, >= 1000 in M12 (FIXED TYPO HERE)
    M09_PPH_ESTIMATE_FAORRES >= 0 & M09_PPH_ESTIMATE_FAORRES < 500 &
      RSN_EST_BLOOD_LOSS_PNC >= 1000 ~ "no pph in mnh09 then severe in mnh12",
    
    # 4. Blood loss can be in both! But if they differ, query
    M09_PPH_ESTIMATE_FAORRES >= 500 & RSN_EST_BLOOD_LOSS_PNC >= 500 & 
      M09_PPH_ESTIMATE_FAORRES != RSN_EST_BLOOD_LOSS_PNC ~ "pph blood loss differ mnh09 and mnh12",
    
    # 5. 0-499 in M09, 500-999 in M12 (FIXED TYPO HERE)
    M09_PPH_ESTIMATE_FAORRES >= 0 & M09_PPH_ESTIMATE_FAORRES < 500 &
      RSN_EST_BLOOD_LOSS_PNC >= 500 & RSN_EST_BLOOD_LOSS_PNC < 1000 ~ "no pph in mnh09 then pph in mnh12",
    
    # 6. Missing in either
    is.na(M09_PPH_ESTIMATE_FAORRES) | is.na(RSN_EST_BLOOD_LOSS_PNC) ~ "missing forms",
                                   TRUE ~ NA)) %>% 
  filter (!is.na(exclusion_rsn )) %>%
  select(SITE, MOMID, PREGID, HEM_PPH,HEM_PPH_AGE_PP_DAYS, HEM_PPH_SEV_AGE_PP_DAYS,HEM_PPH_AGE_PP_WKS, HEM_PPH_FORM_SOURCE, HEM_PPH_RSN_SOURCE, 
         M09_PPH_ESTIMATE_FAORRES, RSN_EST_BLOOD_LOSS_PNC, exclusion_rsn)

dim(query_pph_pnc_blood_loss)
if(dim(query_pph_pnc_blood_loss)[1] >0) {
  hem_query_list[["pph blood loss differ mnh09 and mnh12"]] <- query_pph_pnc_blood_loss
}

table(query_pph_pnc_blood_loss$exclusion_rsn)

total_n <- hem_export_merged %>%
  group_by(SITE) %>% 
  filter(HEM_PPH_RSN_SOURCE %in% c("RSN_EST_BLOOD_LOSS_PNC", "RSN_EST_BLOOD_LOSS_IPC",
                                   "RSN_EST_BLOOD_LOSS_PNC, RSN_EST_BLOOD_LOSS_IPC", "RSN_EST_BLOOD_LOSS_IPC, RSN_EST_BLOOD_LOSS_PNC")) %>% 
  summarise(total_pph=sum(HEM_PPH==1))


query6_output <- query_pph_pnc_blood_loss %>% 
  group_by(SITE) %>% 
  mutate(total=sum(HEM_PPH==1)) %>% 
  ungroup() %>% 
  group_by(SITE, exclusion_rsn) %>% 
  summarise(n = n()) %>% 
  left_join(total_n, by = c("SITE")) %>% 
  mutate(n_pct = round(n/total_pph*100, 1))


ggplot(data = query_pph_pnc_blood_loss %>% filter(exclusion_rsn == "pph blood loss in mnh12 only (-7 MNH09)"), aes(x = HEM_PPH_AGE_PP_DAYS, fill = SITE)) + 
  geom_histogram(color = "grey") + 
  xlab("Days postpartum") + 
  scale_x_continuous(breaks = seq(0,20,2), limits = c(0, 20)) +
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 11), # , angle = 90
    # axis.title.x = element_text(colour = "black", size = 10, margin = margin(t = 20, r = 20, b = 0, l = 0)),
    axis.text.y = element_text(size = 11, colour = "black", hjust=1))

## 7. flag any severe pph cases that happen after the pph date ---- 
## Yes. This is possible, if the bleeding progresses and gets worse
## Commenting out chunk which gets added to the list to send to site
query_sev_pph_after_pph <- hem_export_merged %>% 
  filter(HEM_PPH ==1 & HEM_PPH_SEV ==1) %>%
  filter(HEM_PPH_SEV_DATE > HEM_PPH_DATE) %>% 
  mutate(exclusion_rsn = "severe pph after pph") %>% 
  mutate(DAYS_BTWN_PPH_AND_SEVPPH = HEM_PPH_SEV_AGE_PP_DAYS - HEM_PPH_AGE_PP_DAYS) %>% 
  select(SITE, MOMID, PREGID,HEM_PPH_RSN_SOURCE,HEM_PPH_SEV_RSN_SOURCE, DAYS_BTWN_PPH_AND_SEVPPH,
         HEM_PPH, HEM_PPH_DATE,HEM_PPH_AGE_PP_DAYS, HEM_PPH_AGE_PP_WKS,
         HEM_PPH_SEV,HEM_PPH_SEV_DATE, HEM_PPH_SEV_AGE_PP_DAYS, HEM_PPH_SEV_AGE_PP_WKS, exclusion_rsn)


dim(query_sev_pph_after_pph)
# if(dim(query_sev_pph_after_pph)[1] >0) {
#   hem_query_list[["severe pph after pph"]] <- query_sev_pph_after_pph
# }

## 8. look at cases where aph was reported in mnh09 but there is no record in mnh04 ---- 
table(hem_export_merged$HEM_APH_FORM_SOURCE)
query_aph_in_mnh09 <- hem_export_merged %>% 
  # filter(HEM_APH==1 & HEM_APH_FORM_SOURCE == "MNH09") %>%
  filter(HEM_APH ==1 & !str_detect(HEM_APH_FORM_SOURCE, "MNH04")) %>% 
  left_join(mnh04 %>% select(SITE, MOMID, PREGID, M04_TYPE_VISIT, M04_APH_CEOCCUR)) %>% 
  select(SITE, MOMID, PREGID, M04_TYPE_VISIT, M04_APH_CEOCCUR,contains("APH"), any_of(aph_rsn)) %>% 
  mutate(exclusion_rsn = "aph reported in mnh09 or 19") 

table(query_aph_in_mnh09$HEM_APH_FORM_SOURCE)

dim(query_aph_in_mnh09)

#PJW - commenting this out, sites have a choice to choose between MNH04 and MNH09 to record adverse events
#Since MNH09 has documented evidence, no need to expect 04 at the same time
# if(dim(query_aph_in_mnh09)[1] >0) {
#   hem_query_list[["aph reported in mnh09 and not mnh04"]] <- query_aph_in_mnh09
# }

query_aph_in_mnh09 <- hem_export_merged %>% 
  # filter(HEM_APH==1 & HEM_APH_FORM_SOURCE == "MNH09") %>%
  filter(HEM_APH ==1 & !str_detect(HEM_APH_FORM_SOURCE, "MNH04")) %>% 
  select(SITE, MOMID, PREGID,contains("APH"), any_of(aph_rsn))  

# ---- Figures ----
hemorrhage_figs <- hem_export_merged %>% select(SITE, PREGID, RSN_EST_BLOOD_LOSS_IPC)  %>% 
  mutate(CUTOFF = case_when(RSN_EST_BLOOD_LOSS_IPC < 500 ~ "No hemorrhage (<500mL)", 
                            RSN_EST_BLOOD_LOSS_IPC >= 500 & RSN_EST_BLOOD_LOSS_IPC<1000 ~ "Hemorrhage (>=500mL)", 
                            RSN_EST_BLOOD_LOSS_IPC >= 1000 ~ "Severe Hemorrhage(>=1000mL)", 
                            TRUE ~ NA))
# Define the order of legend labels
hemorrhage_figs$CUTOFF <- factor(hemorrhage_figs$CUTOFF, levels = c("No hemorrhage (<500mL)","Hemorrhage (>=500mL)", 
                                                                    "Severe Hemorrhage(>=1000mL)"))
hemorrhage_fig <- ggplot() + 
  geom_histogram(data = hemorrhage_figs, aes(x = RSN_EST_BLOOD_LOSS_IPC, fill = CUTOFF),  color = "gray", binwidth = 50) + 
  scale_fill_manual(values = c("darkgreen", "darkorange","darkred"), name = "") + 
  facet_grid(rows = vars(SITE), scales = "free_y") + 
  xlab("Estimated blood loss (mL)") + 
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(0,3000,250), limits = c(0, 3000)) +
  geom_vline(xintercept = 500, linetype = "dashed") +
  geom_vline(xintercept = 1000, linetype = "dashed") + 
  theme_bw() + 
  theme(strip.background=element_rect(fill="white"),
        axis.text.x = element_text(vjust = 1, hjust=0.5),
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) 


# ggsave(paste0("hemorrhage_fig", ".pdf"), path = path_to_save,
#        width = 8, height = 6)

# ---- Data Export ----
dim(mat_end)[1]
dim(hem_export_merged)[1]

duplicates <- hem_export_merged %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
paste0(dim(duplicates)[1], " duplicates in final dataset")

## the only thing remaining is figuring out the form issue in mnh09 for aph
aph_subset <- hem_export_merged %>% filter(HEM_APH == 1) %>%
  select(SITE, MOMID, PREGID, PREG_START_DATE, PREG_END_DATE,
         contains("HEM_APH"), aph_rsn)

pph_subset <- hem_export_merged %>% filter(HEM_PPH == 1) %>%
  select(-contains("HEM_PPH_SEV")) %>%
  select(SITE, MOMID, PREGID, PREG_START_DATE, PREG_END_DATE,
         contains("HEM_PPH"), pph_rsn)

pph_sev_subset <- hem_export_merged %>% filter(HEM_PPH_SEV == 1) %>%
  select(SITE, MOMID, PREGID, PREG_START_DATE, PREG_END_DATE,
         contains("HEM_PPH_SEV"), pph_sev_rsn)

## run quick tab of each inclusion criteria 
for (i in names(hem_export_merged)[-c(1:5)]) {
  print(i)
  print(table(hem_export_merged[[i]], useNA = "ifany"))
}


write.csv(hem_export_merged, paste0(path_to_save, "/MAT_HEMORRHAGE" ,".csv"), na="",row.names=FALSE)
write.xlsx(hem_export_merged, paste0(path_to_save, "/MAT_HEMORRHAGE" ,".xlsx"),na="", rowNames=FALSE)

write.csv(hem_export_merged, paste0(path_to_tnt, "MAT_HEMORRHAGE" ,".csv"), na="",row.names=FALSE)
write.xlsx(hem_export_merged, paste0(path_to_tnt, "MAT_HEMORRHAGE" ,".xlsx"),na="", rowNames=FALSE)

table(hem_export_merged$HEM_APH, hem_export_merged$SITE)
table(hem_export_merged$HEM_PPH, hem_export_merged$SITE)
table(hem_export_merged$HEM_PPH_SEV, hem_export_merged$SITE)


#Query Export ----
##For the non-queries to exclude (These has been coded in STEP 4: Join query list!) ---- 
## All sites should have confirmed the following hemmorrhage event;
# The cases listed below were previously reviewed/adjudicated with the study
# Sites and should NOT be re-sent for the specified hemorrhage query.
# Previously reviewed queries to suppress:
# India-CMC
# 1. IBM-0020-01
#    Query: pph in mnh09 then severe in mnh12
# 2. IBM-0474-01
#    Query: pph blood loss in mnh12 only (-7 MNH09)
# 3. IBM-0741-01
#    Query: no pph in mnh09 then pph in mnh12
# Zambia
# 4. Z3-202-1692
#    Query: pph reported late pp
#    Site response: Confirmed true hemorrhage event; secondary PPH due to
#    retained products.
# 5. Z3-202-1692
#    Query: pph reported in mnh12 only
#    Same previously reviewed/confirmed hemorrhage event.
# Kenya
# 6. KEARC01540_P1
#    Query: pph reported in mnh12 only
#    Site confirmed true hemorrhage event.
# 7. KEARC00648_P1
#    Query: pph blood loss differ mnh09 and mnh12
#    Site confirmed the reported blood-loss history as a true hemorrhage event.
#Please note that exclusions should always use SITE + PREGID + exclusion_rsn. 
#Do not filter solely on PREGID, because doing so would incorrectly suppress other valid
#queries generated for the same participant.

##Secficically these India Ids do not have records to confirm!
# IBM-0020-01	MNH09 & MNH12	M12_VAG_BLEED_LOSS_ML == 1000 (Date: 2024-03-07) | 
# M09_PPH_ESTIMATE_FAORRES == 500 (Date: 2023-12-14)	M09 blood loss is 500-999mL, but PNC reports >= 1000mL.
# IBM-0474-01	MNH09, MNH12	M12_VAG_BLEED_LOSS_ML == 635 (Date: 2024-06-03) | 
# M09_PPH_ESTIMATE_FAORRES == -7 (Date: 2024-05-20)	M09 blood loss is -7, but PNC reports >= 500mL.



## 1. Define the mapping to the variables and relevant forms ----
query_metadata <- tibble::tribble(
  ~exclusion_rsn, ~Issue_Description, ~Form, ~Original_Variables_Flagged,
  "multiple mnh12 events", "Multiple PPH events reported across different PNC visits.", "MNH12", "M12_BIRTH_COMPL_MHTERM_1, M12_VAG_BLEED_LOSS_ML, M12_VISIT_OBSSTDAT",
  "pph before dob", "PPH date occurs before the infant's date of birth.", "MNH12, MNH09", "M12_VISIT_OBSSTDAT, DOB",
  "PPH reported at mnh09 and mnh12", "PPH reported on both L&D form and PNC form. Verify if distinct events or duplicates.", "MNH09, MNH12", "M09_PPH_CEOCCUR, M12_BIRTH_COMPL_MHTERM_1, M09_PPH_ESTIMATE_FAORRES, M12_VAG_BLEED_LOSS_ML",
  "missing pregnancy end date", "Missing pregnancy end date for a participant with a hemorrhage event.", "MAT_ENDPOINTS, MNH09", "PREG_END_DATE, M09_DELIV_DSSTDAT_EARLIEST",
  "missing aph criteria", "APH indicated, but underlying clinical criteria are missing.", "MNH04, MNH09, MNH19", "M04_APH_CEOCCUR, M09_APH_CEOCCUR, M19_LD_COMPL_MHTERM_4",
  "missing pph criteria", "PPH indicated, but underlying clinical criteria/blood loss values are missing.", "MNH09, MNH12, MNH19", "M09_PPH_CEOCCUR, M09_PPH_ESTIMATE_FAORRES, M12_VAG_BLEED_LOSS_ML",
  "missing sev pph criteria", "Severe PPH indicated, but underlying criteria missing.", "MNH09, MNH12, MNH19", "M09_PPH_ESTIMATE_FAORRES, M12_VAG_BLEED_LOSS_ML",
  "pph reported late pp", "PPH event date is 5+ weeks after delivery. Verify event date.", "MNH12, MNH19", "M12_VISIT_OBSSTDAT, M19_OHOSTDAT, PREG_END_DATE",
  "pph reported in mnh12 only", "PPH reported as a birth complication on PNC form, but not reported on L&D form.", "MNH09, MNH12", "M12_BIRTH_COMPL_MHTERM_1, M09_PPH_CEOCCUR",
  
  "pph blood loss in mnh12 only (-7 MNH09)", "M09 blood loss is -7, but PNC reports >= 500mL.", "MNH09, MNH12", "M12_VAG_BLEED_LOSS_ML, M09_PPH_ESTIMATE_FAORRES",
  "pph in mnh09 then severe in mnh12", "M09 blood loss is 500-999mL, but PNC reports >= 1000mL.", "MNH09, MNH12", "M12_VAG_BLEED_LOSS_ML, M09_PPH_ESTIMATE_FAORRES",
  "no pph in mnh09 then severe in mnh12", "M09 blood loss is < 500mL, but PNC reports >= 1000mL.", "MNH09, MNH12", "M12_VAG_BLEED_LOSS_ML, M09_PPH_ESTIMATE_FAORRES",
  "pph blood loss differ mnh09 and mnh12", "Discrepancy in estimated blood loss values between L&D and PNC forms.", "MNH09, MNH12", "M12_VAG_BLEED_LOSS_ML, M09_PPH_ESTIMATE_FAORRES",
  "no pph in mnh09 then pph in mnh12", "M09 blood loss is < 500mL, but PNC reports 500-999mL.", "MNH09, MNH12", "M12_VAG_BLEED_LOSS_ML, M09_PPH_ESTIMATE_FAORRES",
  "missing forms", "Missing forms preventing verification of blood loss.", "MNH09, MNH12", "M12_VAG_BLEED_LOSS_ML, M09_PPH_ESTIMATE_FAORRES",
  
  "severe pph after pph", "The date of Severe PPH is recorded as occurring after the standard PPH date.", "MNH09, MNH12, MNH19", "M12_VISIT_OBSSTDAT, M19_OHOSTDAT, M12_VAG_BLEED_LOSS_ML",
  "aph reported in mnh09 or 19", "APH reported on L&D form, but not reported on any ANC forms.", "MNH04, MNH09", "M09_APH_CEOCCUR, M04_APH_CEOCCUR"
)

## 2. Bind the query list ----
combined_queries <- hem_query_list %>%
  map(~ mutate(.x, across(everything(), as.character))) %>% 
  bind_rows(.id = "query_list_name") %>%
  mutate(MOMID = if("MOMID" %in% names(.)) MOMID else NA_character_) %>%
  ### Remove specific India-CMC cases from specific queries only ----
  ## the site confirmed they do not have data to confirm this
  filter(
    !(Site == "India-CMC" &
        ((PregID == "IBM-0020-01" &
             exclusion_rsn == "pph in mnh09 then severe in mnh12") |
            (PregID == "IBM-0474-01" &
               exclusion_rsn == "pph blood loss in mnh12 only (-7 MNH09)")))) %>%

  ### Remove previously reviewed and confirmed cases from specific queries only ----
  filter(
    !(
      # India-CMC
        (SITE == "India-CMC" &
           PREGID == "IBM-0741-01" &
           exclusion_rsn == "no pph in mnh09 then pph in mnh12") |
        # Zambia
        (SITE == "Zambia" &
           PREGID == "Z3-202-1692" &
           exclusion_rsn %in% c(
             "pph reported late pp",
             "pph reported in mnh12 only"
           )) |
        # Kenya
        (SITE == "Kenya" &
           PREGID == "KEARC01540_P1" &
           exclusion_rsn == "pph reported in mnh12 only") |
        (SITE == "Kenya" &
           PREGID == "KEARC00648_P1" &
           exclusion_rsn == "pph blood loss differ mnh09 and mnh12")))

## 3. Building this to backtrack the relevant variables raw data ----

long_04 <- mnh04 %>% 
  select(SITE, MOMID, PREGID, M04_ANC_OBSSTDAT, M04_APH_CEOCCUR) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = -c(SITE, MOMID, PREGID, M04_ANC_OBSSTDAT), names_to = "VAR_NAME", values_to = "VALUE") %>%
  rename(OBS_DATE = M04_ANC_OBSSTDAT)

long_09 <- mnh09 %>% 
  rowwise() %>%
  mutate(
    # Safely pull across all INF1-INF4 delivery dates and compute the earliest valid date
    M09_DELIV_DSSTDAT_EARLIEST = {
      # 1. Grab whatever INF columns exist in the dataset
      inf_dates <- c_across(any_of(c("M09_DELIV_DSSTDAT_INF1", 
                                     "M09_DELIV_DSSTDAT_INF2", 
                                     "M09_DELIV_DSSTDAT_INF3", 
                                     "M09_DELIV_DSSTDAT_INF4")))
      
      # 2. Filter out true NAs, blanks, and PRISMA default/missing date codes
      valid_dates <- inf_dates[!inf_dates %in% c(NA, "", "1907-07-07", "1905-05-05", "1909-09-09", "1906-06-06", "2007-07-07")]
      
      # 3. Convert to Date and take the min (earliest)
      if (length(valid_dates) > 0) {
        as.character(min(as.Date(valid_dates), na.rm = TRUE))
      } else {
        NA_character_
      }
    }
  ) %>%
  ungroup() %>%
  select(SITE, MOMID, PREGID, M09_MAT_LD_OHOSTDAT, M09_APH_CEOCCUR, M09_PPH_CEOCCUR, M09_PPH_ESTIMATE_FAORRES, 
         M09_DELIV_DSSTDAT_EARLIEST, M09_DELIV_DSSTDAT_INF1) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = -c(SITE, MOMID, PREGID, M09_MAT_LD_OHOSTDAT), names_to = "VAR_NAME", values_to = "VALUE") %>%
  rename(OBS_DATE = M09_MAT_LD_OHOSTDAT)

long_12 <- mnh12 %>% 
  select(SITE, MOMID, PREGID, M12_VISIT_OBSSTDAT, M12_BIRTH_COMPL_MHTERM_1, M12_VAG_BLEED_LOSS_ML) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = -c(SITE, MOMID, PREGID, M12_VISIT_OBSSTDAT), names_to = "VAR_NAME", values_to = "VALUE") %>%
  rename(OBS_DATE = M12_VISIT_OBSSTDAT)

long_19 <- mnh19 %>% 
  mutate(M19_DATE = coalesce(as.character(M19_OHOSTDAT), as.character(M19_ADMIT_OHOSTDAT), as.character(M19_MAT_EST_OHOSTDAT))) %>%
  # NEW: Added M19_LD_COMPL_MHTERM_5 to the select statement so it can be queried
  select(SITE, MOMID, PREGID, M19_DATE, M19_LD_COMPL_MHTERM_4, M19_LD_COMPL_MHTERM_5, M19_LD_COMPL_ML) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = -c(SITE, MOMID, PREGID, M19_DATE), names_to = "VAR_NAME", values_to = "VALUE") %>%
  rename(OBS_DATE = M19_DATE)

long_inf <- inf_outcomes %>% 
  select(SITE, MOMID, PREGID, DOB) %>% 
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = DOB, names_to = "VAR_NAME", values_to = "VALUE") %>% 
  mutate(OBS_DATE = VALUE)

long_mat <- mat_end %>% 
  select(SITE, MOMID, PREGID, PREG_END_DATE) %>% 
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = PREG_END_DATE, names_to = "VAR_NAME", values_to = "VALUE") %>% 
  mutate(OBS_DATE = VALUE)

master_lookup <- bind_rows(long_04, long_09, long_12, long_19, long_inf, long_mat) %>%
  filter(!is.na(VALUE) & trimws(VALUE) != "")

## 4. Join query list with the relevant raw data observations ----
combined_queries <- combined_queries %>%
  left_join(query_metadata, by = "exclusion_rsn") %>%
  
  # Overwrite the query 'Form' with the participant's ACTUAL form source
  mutate(
    Derived_Form = coalesce(
      if("HEM_PPH_FORM_SOURCE" %in% names(.)) HEM_PPH_FORM_SOURCE else NA_character_,
      if("HEM_APH_FORM_SOURCE" %in% names(.)) HEM_APH_FORM_SOURCE else NA_character_,
      Form # Fallback to the metadata table
    ),
    # If the flagged variables contain "M09" but the derived form missed it, prepend MNH09
    Form = case_when(
      grepl("M09", Original_Variables_Flagged) & !grepl("MNH09", Derived_Form) ~ paste0("MNH09, ", Derived_Form),
      # You can also force it for specific discrepancy reasons just to be safe
      grepl("discrepancy|mismatch", exclusion_rsn, ignore.case = TRUE) & !grepl("MNH09", Derived_Form) ~ paste0("MNH09, ", Derived_Form),
      TRUE ~ Derived_Form
    )
  ) %>%
  select(-Derived_Form) %>%
  
  # Dynamically override the metadata variables for Late PPH based on exactly what triggered it
  mutate(
    Original_Variables_Flagged = case_when(
      exclusion_rsn == "pph reported late pp" ~ paste0(
        "PREG_END_DATE",
        ifelse(grepl("MNH12", HEM_PPH_FORM_SOURCE), ", M12_VISIT_OBSSTDAT", ""),
        ifelse(grepl("MNH19", HEM_PPH_FORM_SOURCE), ", M19_DATE", ""),
        ifelse(grepl("RSN_PPH_REPORT_BIRTH_COMPL_PNC", HEM_PPH_RSN_SOURCE), ", M12_BIRTH_COMPL_MHTERM_1", ""),
        ifelse(grepl("RSN_EST_BLOOD_LOSS_PNC", HEM_PPH_RSN_SOURCE), ", M12_VAG_BLEED_LOSS_ML", ""),
        ifelse(grepl("RSN_PPH_REPORT_HOSP", HEM_PPH_RSN_SOURCE), ", M19_LD_COMPL_MHTERM_5", ""),
        ifelse(grepl("RSN_EST_BLOOD_LOSS_HOSP_PNC", HEM_PPH_RSN_SOURCE), ", M19_LD_COMPL_ML", "")
      ),
      TRUE ~ Original_Variables_Flagged
    )
  ) %>%
  
  rowwise() %>%
  mutate(
    `Variable where Dx is reported` = {
      if (is.na(Original_Variables_Flagged) || is.null(Original_Variables_Flagged) || trimws(Original_Variables_Flagged) == "") {
        NA_character_
      } else {
        # Clean up variables (removes trailing commas)
        vars <- trimws(unlist(strsplit(Original_Variables_Flagged, ",")))
        vars <- vars[vars != ""] 
        
        current_site <- SITE
        current_momid <- MOMID
        current_pregid <- PREGID 
        
        participant_data <- master_lookup[
          master_lookup$SITE == current_site & 
            master_lookup$MOMID == current_momid & 
            master_lookup$PREGID == current_pregid, 
        ]
        
        event_dates <- participant_data %>%
          filter(
            (grepl("CEOCCUR|MHTERM", VAR_NAME) & VALUE == "1") |
              (grepl("LOSS_ML|ESTIMATE_FAORRES|COMPL_ML", VAR_NAME) & 
                 !is.na(suppressWarnings(as.numeric(VALUE))) & as.numeric(VALUE) >= 500)
          ) %>%
          pull(OBS_DATE) %>%
          unique()
        
        vals <- sapply(vars, function(v) {
          history <- participant_data[participant_data$VAR_NAME == v, ]
          
          if (grepl("^M04_|^M12_|^M19_", v) && nrow(history) > 1 && length(event_dates) > 0) {
            matched_history <- history[history$OBS_DATE %in% event_dates, ]
            if (nrow(matched_history) > 0) {
              history <- matched_history
            }
          }
          
          # If no record is found, return NA to drop it silently
          if (nrow(history) == 0) {
            return(NA_character_)
          } else {
            
            # Check if this variable is a Date variable
            is_date_var <- grepl("DAT|DATE|DOB", v, ignore.case = TRUE)
            
            formatted_history <- sapply(1:nrow(history), function(i) {
              val <- history$VALUE[i]
              obs_date <- ifelse(is.na(history$OBS_DATE[i]), "Unknown", history$OBS_DATE[i])
              
              if (is_date_var) {
                return(paste0(history$VAR_NAME[i], " == ", val))
              } else {
                return(paste0(history$VAR_NAME[i], " == ", val, " (Date: ", obs_date, ")"))
              }
            })
            
            return(paste(formatted_history, collapse = " ; "))
          }
        })
        
        # Filter out the NA results from missing variables before joining
        vals <- vals[!is.na(vals) & vals != ""]
        if (length(vals) == 0) {
          NA_character_
        } else {
          paste(vals, collapse = " | ")
        }
      }
    }
  ) %>%
  ungroup() %>%
  mutate(
    `Date of (first) Relevant Dx` = str_extract(`Variable where Dx is reported`, "\\d{4}-\\d{2}-\\d{2}"),
    `Date of (first) Relevant Dx` = coalesce(`Date of (first) Relevant Dx`, "Review source variables"),
    `Site Comments` = "",
    `Final Determination: Please select for each case` = "",
    `Date implemented in the dataset (yyyy-mm-dd)` = "",
    `If other, please explain:` = ""
  ) %>%
  rename(
    Site = SITE,
    MomID = MOMID,
    PregID = PREGID, 
    `Forms implicated in Dx` = Form,
    `Reason for review request` = Issue_Description
  ) %>%
  select(
    Site, MomID, PregID, 
    #`Date of (first) Relevant Dx`, 
    `Forms implicated in Dx`, 
    `Variable where Dx is reported`, 
    `Reason for review request`,
    `Site Comments`, 
    `Final Determination: Please select for each case`,
    `Date implemented in the dataset (yyyy-mm-dd)`, 
    `If other, please explain:`
  ) %>%
  distinct()

## 5. Define dropdown options ----
dropdown_options <- c(
  "Confirmed: True Hemorrhage event",
  "Confirmed: Blood loss measurement verified",
  "Corrected: PPH (Hemorrhage) diagnosis removed",
  "Corrected: PPH (Hemorrhage) event dates fixed",
  "Corrected: Blood loss/criteria updated",
  "Corrected: Cross-form PPH duplicates resolved",
  "Other (Explain in comments)"
)

## 6. Export to excel ----
##PJW comment (please note to save this based on your folder files!)
site_list <- unique(combined_queries$Site)

for (current_site in site_list) {
  
  site_data <- combined_queries %>% filter(Site == current_site)
  if(nrow(site_data) == 0) next
  
  wb <- createWorkbook()
  
  # --- SHEET 1: Main worksheet ---
  sheet_name <- "Hemorrhage_Queries"
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x = site_data)
  
  # --- SHEET 2: Hidden worksheet for dropdowns (Bypasses 255 char limit) ---
  addWorksheet(wb, "Dropdown_Data", visible = FALSE)
  writeData(wb, sheet = "Dropdown_Data", x = dropdown_options, colNames = FALSE)
  
  # Apply Data Validation
  dropdown_col_index <- which(names(site_data) == "Final Determination: Please select for each case")
  dv_formula <- paste0("'Dropdown_Data'!$A$1:$A$", length(dropdown_options))
  
  dataValidation(
    wb, sheet = sheet_name, col = dropdown_col_index, rows = 2:(nrow(site_data) + 1),
    type = "list", value = dv_formula 
  )
  
  # --- FORMATTING (Sheet 1) ---
  
  # 1. Wrap text for the entire body of the query sheet
  body_style <- createStyle(wrapText = TRUE, valign = "top")
  addStyle(wb, sheet = sheet_name, style = body_style, rows = 2:(nrow(site_data) + 1), cols = 1:ncol(site_data), gridExpand = TRUE)
  
  # 2. Standard Header Style (Bold, wrapped, with a border)
  header_style <- createStyle(
    textDecoration = "bold", 
    wrapText = TRUE, 
    valign = "top",
    border = "TopBottomLeftRight", 
    borderStyle = "thin"           
  )
  addStyle(wb, sheet = sheet_name, style = header_style, rows = 1, cols = 1:ncol(site_data), gridExpand = TRUE)
  
  # 3. Highlighted Header Style for specific Site response columns (Yellow fill + border)
  highlight_header_style <- createStyle(
    textDecoration = "bold", 
    wrapText = TRUE, 
    valign = "top", 
    fgFill = "#FFFF00", 
    border = "TopBottomLeftRight", 
    borderStyle = "thin"
  )
  cols_to_highlight <- which(names(site_data) %in% c(
    "Site Comments", 
    "Final Determination: Please select for each case", 
    "Date implemented in the dataset (yyyy-mm-dd)", 
    "If other, please explain:"
  ))
  
  # Apply the highlight style just to row 1 of those specific columns
  if(length(cols_to_highlight) > 0) {
    addStyle(wb, sheet = sheet_name, style = highlight_header_style, rows = 1, cols = cols_to_highlight, gridExpand = TRUE)
  }
  
  # 4. Set Column Widths
  setColWidths(wb, sheet = sheet_name, cols = 1:ncol(site_data), widths = 15) 
  wide_cols <- which(names(site_data) %in% c("MOMID", "PREGID", "Variable where Dx is reported", "Reason for review request", "Site Comments", "Final Determination: Please select for each case", "If other, please explain:"))
  setColWidths(wb, sheet = sheet_name, cols = wide_cols, widths = 40)
  
  # --- SHEET 3: PPH Queries (Dynamic File Search by exact name) ---
  
  # Build the exact expected filename, e.g., "Ghana-Outcome-Queries-2026-05-01.xlsx"
  expected_pph_filename <- paste0(current_site, "-Outcome-Queries-", UploadDate, ".xlsx")
  
  # Look for that specific file (ignore.case = TRUE handles minor capitalization differences)
  pph_file_matches <- list.files(path = pph_directory, 
                                 pattern = expected_pph_filename, 
                                 full.names = TRUE, 
                                 ignore.case = TRUE)
  
  # If the exact file is found, process it
  if(length(pph_file_matches) > 0) {
    pph_target_file <- pph_file_matches[1]
    
    # Read the data from the PPH file
    pph_data <- read.xlsx(pph_target_file)
    
    # Add as the third worksheet
    addWorksheet(wb, "PPH (Hemorrhage)")
    writeData(wb, sheet = "PPH (Hemorrhage)", x = pph_data)
    
    # Apply basic body and header styling to this new sheet to match the first sheet
    addStyle(wb, sheet = "PPH (Hemorrhage)", style = body_style, rows = 2:(nrow(pph_data) + 1), cols = 1:ncol(pph_data), gridExpand = TRUE)
    addStyle(wb, sheet = "PPH (Hemorrhage)", style = header_style, rows = 1, cols = 1:ncol(pph_data), gridExpand = TRUE)
    setColWidths(wb, sheet = "PPH (Hemorrhage)", cols = 1:ncol(pph_data), widths = 15)
  }
  
  # --- SAVE WORKBOOK ---
  file_name <- paste0(path_to_queries, "/", current_site, "_Hemorrhage_Queries_", Sys.Date(), ".xlsx")
  saveWorkbook(wb, file_name, overwrite = TRUE)
}
