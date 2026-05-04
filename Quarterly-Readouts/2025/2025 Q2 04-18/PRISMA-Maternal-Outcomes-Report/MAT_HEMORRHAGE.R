#*****************************************************************************
#* PRISMA Maternal Hemorrhage
#* Last updated: 10 January 2025
 
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

#*****************************************************************************

### data queries 
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(data.table)
library(openxlsx)
library(haven)
## set upload date
UploadDate = "2024-12-13"

## import data
# set path to save 
path_to_save <- "D:/Users/stacie.loisate/Documents/PRISMA-Analysis-Stacie/Maternal-Outcomes/data/"
path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")

# set path to data
# path_to_data = paste0("Z:/Stacked Data/",UploadDate)
path_to_data <- paste0("D:/Users/stacie.loisate/Documents/import/", UploadDate)

mat_enroll <- read_csv(paste0(path_to_tnt, "MAT_ENROLL" ,".csv" )) %>% select(SITE, MOMID, PREGID, ENROLL, PREG_START_DATE ) %>% 
  filter(ENROLL == 1)

mat_end <- read_dta(paste0(path_to_tnt, "MAT_ENDPOINTS" ,".dta" )) %>% 
  ## only want all pregnancy endpoints excluding moms who have died before delivery and induced abortions
  filter(PREG_END ==1 & MAT_DEATH!=1 & PREG_LOSS_INDUCED==0)
  
table(mat_end$PREG_END)
table(mat_end$MAT_DEATH)
table(mat_end$PREG_LOSS_INDUCED)


# # import forms 
mnh04 <- read.csv(paste0(path_to_data,"/", "mnh04_merged.csv"))
mnh09 <- read.csv(paste0(path_to_data,"/", "mnh09_merged.csv"))
mnh12 <- read.csv(paste0(path_to_data,"/", "mnh12_merged.csv"))
mnh19 <- read.csv(paste0(path_to_data,"/", "mnh19_merged.csv"))

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

#MNH09
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

#MNH12
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
    ungroup()
  
  
} else {
  print("No duplicates in mnh12")
}


#MNH19
if (any(duplicated(mnh19[c("SITE", "MOMID", "PREGID", "M19_OBSSTDAT")]))) {
  # extract duplicated ids
  duplicates_ids_19 <- which(duplicated(mnh19[c("SITE", "MOMID", "PREGID", "M19_OBSSTDAT")]) | 
                               duplicated(mnh19[c("SITE", "MOMID", "PREGID", "M19_OBSSTDAT")], fromLast = TRUE))
  duplicates_ids_19 <- mnh19[duplicates_ids_19, ]
  
  print(paste0("n= ",dim(duplicates_ids_19)[1],  " Duplicates in mnh19 exist"))
  
  # extract ids from main dataset
  mnh19 <- mnh19 %>% group_by(SITE, MOMID, PREGID, M19_OBSSTDAT) %>% 
    arrange(desc(M19_OBSSTDAT)) %>% 
    slice(1) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    select(-n) %>% 
    ungroup()
  
} else {
  print("No duplicates in mnh19")
}


################################################################################
# data generation
# 1. generate wide dataset with necessary variables from mnh09/mnh04/mnh12
# 2. generate separate dataset with unscheduled visits 
################################################################################
# date variables: M04_ANC_OBSSTDAT, M12_VISIT_OBSSTDAT, M09_MAT_LD_OHOSTDAT
# data prep
mnh04_out <- mnh04 %>% 
  rename(TYPE_VISIT = "M04_TYPE_VISIT") %>%
  rename(M04_VISIT_DATE = "M04_ANC_OBSSTDAT")

# data prep
mnh12_out <- mnh12 %>% 
  rename(TYPE_VISIT = "M12_TYPE_VISIT") %>%
  rename(M12_VISIT_DATE = "M12_VISIT_OBSSTDAT") 

# merge mnh04, mnh09, and mnh12 together
hem <- mnh09 %>%
  select(SITE, MOMID, PREGID, M09_MAT_LD_OHOSTDAT, contains("PPH"), M09_APH_CEOCCUR) %>%
  mutate(TYPE_VISIT = 6) %>%
  full_join(mnh04_out[c("SITE", "MOMID", "PREGID","TYPE_VISIT","M04_VISIT_DATE", "M04_APH_CEOCCUR")], by = c("SITE", "MOMID", "PREGID","TYPE_VISIT")) %>%
  full_join(mnh12_out[c("SITE", "MOMID", "PREGID","TYPE_VISIT","M12_VISIT_DATE", "M12_VAG_BLEED_LOSS_ML", "M12_BIRTH_COMPL_MHTERM_1")],
            by = c("SITE", "MOMID", "PREGID","TYPE_VISIT")) %>%
  right_join(mat_end[c("SITE", "MOMID", "PREGID","PREG_END",
                      "PREG_END_DATE", "PREG_END_GA", "PREG_LOSS")],
            by = c("SITE", "MOMID", "PREGID")) %>%
  filter(PREG_END==1)


# Convert hemorrhage dataset to wide format
# extract smaller datasets by visit type and assign a suffix with the visit type. We can then merge back together 
# labor and delivery (visit type = 6)
hem_ld <- hem %>% filter(TYPE_VISIT==6) %>%
  select(SITE, MOMID, PREGID,PREG_END_DATE,PREG_END, contains("M09")) %>% 
  rename_with(~paste0(., "_", 6), .cols = c(contains("M09")))  %>% 
  distinct(SITE, MOMID, PREGID, .keep_all = TRUE)  

# vector of all visits in the dataset
visit_types_num <- c(1,2,3,4,5,7,8,9,10,11,12,13,14)
# vector of labels for all visits in the dataset
visit_types_name <- c("enroll", "anc20", "anc28", "anc32", "anc36", 
                      "pnc0", "pnc1", "pnc4",  "pnc6",  "pnc26","pnc52", "unsched_anc", "unsched_pnc")  # Add more visit types if needed

# generate a dataset for each visit type; we want to make the data wide so we separate by visit type here, add a suffix, and then will merge back together
hem_visit_list <- lapply(visit_types_num, function(visit_types_num) {
  hem %>%
    filter(TYPE_VISIT == visit_types_num) %>%
    select(SITE, MOMID, PREGID, contains("M04"), contains("M12")) %>%
    rename_with(~paste0(., "_", visit_types_num), .cols = c(contains("M04"), contains("M12")))
  
})
names(hem_visit_list) <- paste("hem_", visit_types_name, sep = "")

# remove unscheduled for now - deal with those later
remove_names <- c("hem_unsched_anc", "hem_unsched_pnc")
usched_visits_list <- hem_visit_list[remove_names]
list2env(usched_visits_list, envir = .GlobalEnv)


# for unscheduled visits and hospitalization: generate a single variable if a any outcome at an uscheduled visit
hem_unsched_anc <- hem_unsched_anc %>% 
  select(-M12_VAG_BLEED_LOSS_ML_13,-M12_BIRTH_COMPL_MHTERM_1_13, -M12_VISIT_DATE_13) %>%
  mutate(APH_UNSCHED_ANY = case_when(M04_APH_CEOCCUR_13==1~1, TRUE ~ 0)) %>% 
  filter(APH_UNSCHED_ANY==1)

hem_unsched_pnc <- hem_unsched_pnc %>% 
  select(-M04_APH_CEOCCUR_14, -M04_VISIT_DATE_14) %>%
  mutate(PPH_UNSCHED_ANY = case_when(M12_BIRTH_COMPL_MHTERM_1_14==1~1 | 
                                       M12_VAG_BLEED_LOSS_ML_14 >= 500, TRUE ~ 0))%>% 
  filter(PPH_UNSCHED_ANY==1)


## to calculate age at hospitalization, use either date mother presented for care/treatment (OHOSTDAT) or best estimate of date mother presented for care (MAT_EST_OHOSTDAT)
mnh19_out <- mnh19 %>% 
  ## merge in mat_endpoints dataset
  left_join(mat_end[c("SITE", "MOMID", "PREGID", "PREG_END", "PREG_END_DATE")], by = c("SITE", "MOMID", "PREGID")) %>% 
  select(SITE, MOMID,  PREGID, PREG_END, PREG_END_DATE, M19_TIMING_OHOCAT, M19_VAG_BLEED_CEOCCUR, M19_LD_COMPL_MHTERM_4,
         M19_LD_COMPL_ML, M19_LD_COMPL_MHTERM_5, M19_TX_PROCCUR_1,  M19_OBSSTDAT, M19_OHOSTDAT, M19_MAT_EST_OHOSTDAT) %>% 
  left_join(mat_enroll[c("SITE", "MOMID", "PREGID", "PREG_START_DATE")], by = c("SITE", "MOMID", "PREGID")) %>% 
  ## only want participants who have had a pregnancy endpoint 
  filter(PREG_END == 1) %>% 
  # select(-PREG_END) %>% 
  mutate(HEM_HOSP_ANY = case_when(
                                  # M19_VAG_BLEED_CEOCCUR ==1 |
                                  M19_LD_COMPL_MHTERM_4 ==1 |
                                  M19_LD_COMPL_MHTERM_5 == 1 | 
                                  M19_LD_COMPL_ML >= 500 ~ 1, 
                                  TRUE ~ 0)) %>% 
  ## calculate age at hospitalization 
  mutate(AGE_AT_HOSP = case_when( # anc (if hopsitalization comes before pregnancy end date, then ANC)
                                  (!ymd(M19_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_OHOSTDAT) < ymd(PREG_END_DATE) ~  as.numeric(ymd(M19_OHOSTDAT) - ymd(PREG_START_DATE)),
                                  # ymd(PREG_END_DATE) < ymd(M19_OBSSTDAT) & (!ymd(M19_OBSSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) ~  as.numeric(ymd(M19_OBSSTDAT) - ymd(PREG_START_DATE)),
                                  (!ymd(M19_MAT_EST_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_MAT_EST_OHOSTDAT) < ymd(PREG_END_DATE) ~  as.numeric(ymd(M19_MAT_EST_OHOSTDAT) - ymd(PREG_START_DATE)),
                                  
                                  # pnc (if hopsitalization comes on or after pregnancy end date, then PNC)
                                  (!ymd(M19_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_OHOSTDAT) >= ymd(PREG_END_DATE) ~  as.numeric(ymd(M19_OHOSTDAT) - ymd(PREG_END_DATE)),
                                 # ymd(PREG_END_DATE) >= ymd(M19_OBSSTDAT) & (!ymd(M19_OBSSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) ~  as.numeric(ymd(PREG_END_DATE) - ymd(M19_OBSSTDAT)),
                                  (!ymd(M19_MAT_EST_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_MAT_EST_OHOSTDAT) >= ymd(PREG_END_DATE) ~  as.numeric(ymd(M19_MAT_EST_OHOSTDAT) - ymd(PREG_END_DATE)),
                                  TRUE ~ NA
                                 ),
         AGE_AT_HOSP_WKS = AGE_AT_HOSP %/% 7,
         DATE_HOSPITAL = case_when(!ymd(M19_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05")) ~ ymd(M19_OHOSTDAT),
                                   !ymd(M19_MAT_EST_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05")) ~ ymd(M19_MAT_EST_OHOSTDAT),
                                   TRUE ~ NA
                                   )
         ) %>% 
  # generate ANC and PNC variables, where 2= pnc and 1=anc
  mutate(TIMING_HOSP = case_when(
                                (!ymd(M19_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_OHOSTDAT) < ymd(PREG_END_DATE) ~  1,
                                # ymd(PREG_END_DATE) < ymd(M19_OBSSTDAT) & (!ymd(M19_OBSSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) ~  1,
                                (!ymd(M19_MAT_EST_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_MAT_EST_OHOSTDAT) < ymd(PREG_END_DATE) ~  1,
                                
                                (!ymd(M19_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_OHOSTDAT) >= ymd(PREG_END_DATE) ~  2,
                                 # ymd(PREG_END_DATE) >= ymd(M19_OBSSTDAT) & (!ymd(M19_OBSSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) ~  2,
                                (!ymd(M19_MAT_EST_OHOSTDAT) %in% c(ymd("1907-07-07"), ymd("1905-05-05"))) & ymd(M19_MAT_EST_OHOSTDAT) >= ymd(PREG_END_DATE) ~  2,
                                TRUE ~ NA
  )) %>% 
  
  ## if a participant has multiple hospitalizations during ANC or PNC, take the earliest incident of APH or PPH
    group_by(MOMID, PREGID) %>% 
    arrange(DATE_HOSPITAL) %>% 
    mutate(
      KEEP_ANC = if_else(TIMING_HOSP == 1 & HEM_HOSP_ANY == 1, 1, 0),
      KEEP_PNC = if_else(TIMING_HOSP == 2 & HEM_HOSP_ANY == 1, 1, 0)
    ) %>% 
    filter(
      KEEP_ANC == 1 | KEEP_PNC == 1
    ) %>% 
    slice(1) %>% 
    ungroup() %>% 
  select(SITE, MOMID, PREGID, PREG_START_DATE, PREG_END_DATE, M19_OHOSTDAT, M19_MAT_EST_OHOSTDAT,DATE_HOSPITAL,
         AGE_AT_HOSP, TIMING_HOSP, M19_TIMING_OHOCAT, HEM_HOSP_ANY ,
         M19_TX_PROCCUR_1, M19_VAG_BLEED_CEOCCUR, M19_LD_COMPL_MHTERM_4, M19_LD_COMPL_MHTERM_5, M19_LD_COMPL_ML)
  

# Remove the specified data frames from the list
hem_visit_list <- hem_visit_list[setdiff(names(hem_visit_list), remove_names)]
hem_visit_list <- c(hem_visit_list, list(hem_ld = hem_ld))


# merge list of all visit type sub-datasets generated above (without unscheduled or hospitalization)
hem_wide <- hem_visit_list %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID")) %>% distinct() %>% 
  relocate(names(hem_ld), .after = PREGID) %>% 
  group_by(SITE, MOMID, PREGID) %>% 
  select(-PREG_END, -PREG_END_DATE)


## merge unscheduled and hospitalization information into the wide dataset 
hem_wide_full <-  mat_end %>% 
  select(SITE, MOMID, PREGID, PREG_END, PREG_END_DATE) %>%
  # filter for participants who have had a pregnancy outcome
  filter(PREG_END==1) %>% 
  full_join(hem_wide, by = c("SITE", "MOMID", "PREGID")) %>% 
  full_join(hem_unsched_anc[c("SITE", "MOMID", "PREGID", "APH_UNSCHED_ANY", "M04_VISIT_DATE_13")], by = c("SITE", "MOMID", "PREGID")) %>% 
  full_join(hem_unsched_pnc[c("SITE", "MOMID", "PREGID", "PPH_UNSCHED_ANY", "M12_VISIT_DATE_14")], by = c("SITE", "MOMID", "PREGID")) %>% 
  full_join(mnh19_out[c("SITE", "MOMID", "PREGID", "HEM_HOSP_ANY","M19_LD_COMPL_ML", "TIMING_HOSP")], by = c("SITE", "MOMID", "PREGID"))

## generate outcomes: 
hemorrhage <- hem_wide_full %>% 

  ## generate denominator - all participants with a birth reported
  mutate(HEM_DENOM = case_when(PREG_END==1 ~ 1, TRUE ~ 0) ## denominator is all participants with a birth reported
  ) %>% 
  
  ## 1. Antepartum Hemorrhage
  mutate(HEM_APH = case_when(HEM_DENOM==1 & (M04_APH_CEOCCUR_1==1 | M04_APH_CEOCCUR_2==1 |M04_APH_CEOCCUR_3==1 | M04_APH_CEOCCUR_4==1 | M04_APH_CEOCCUR_5==1 |
                                             APH_UNSCHED_ANY==1 | M09_APH_CEOCCUR_6 == 1 |
                                            (HEM_HOSP_ANY==1 & TIMING_HOSP==1)) ~ 1, TRUE ~ 0)) %>% 
 #    # indicator variables for criteria 
 #  mutate(APH_CRIT1_ENRL = case_when(M04_APH_CEOCCUR_1==1 ~ 1, TRUE ~ 0), # (Current clinical status: antepartum hemorrhage @ enrollment)
 #         APH_CRIT1_ANC20 = case_when(M04_APH_CEOCCUR_2==1 ~ 1, TRUE ~ 0), # (Current clinical status: antepartum hemorrhage @ ANC20)
 #         APH_CRIT1_ANC32 = case_when(M04_APH_CEOCCUR_3==1 ~ 1, TRUE ~ 0), # (Current clinical status: antepartum hemorrhage @ ANC32)
 #         APH_CRIT1_ANC36 = case_when(M04_APH_CEOCCUR_4==1 ~ 1, TRUE ~ 0), # (Current clinical status: antepartum hemorrhage @ ANC36)
 #         APH_CRIT2 = case_when(APH_UNSCHED_ANY==1 ~ 1, TRUE ~ 0), # (Current clinical status: antepartum hemorrhage @ at any unscheduled visit)
 #         APH_CRIT3 = case_when(M09_APH_CEOCCUR_6==1 ~ 1, TRUE ~ 0), # (Did the mother experience antepartum hemorrhage?)
 #         APH_CRIT4 = case_when((HEM_HOSP_ANY==1 & TIMING_HOSP==1) ~ 1, TRUE ~ 0) # (specify type of labor/delivery or birth complication: APH or PPH or vaginal bleeding) + (timing of hospitalization = antenatal period)
 #  ) %>% 
 #  # indicator variables for criteria 
 #  mutate(DATE_APH_SCHED = case_when(M04_APH_CEOCCUR_1==1 ~ ymd(M04_VISIT_DATE_1),
 #                                    M04_APH_CEOCCUR_2==1 ~ ymd(M04_VISIT_DATE_2),
 #                                    M04_APH_CEOCCUR_3==1 ~ ymd(M04_VISIT_DATE_3),
 #                                    M04_APH_CEOCCUR_4==1 ~ ymd(M04_VISIT_DATE_4),
 #                                    M04_APH_CEOCCUR_5==1 ~ ymd(M04_VISIT_DATE_5) ~ 1,
 #                                    M09_APH_CEOCCUR_6==1 ~ ymd(M09_MAT_LD_OHOSTDAT_6) ~ 1,
 #                                    TRUE~ 0
 #  ),                             
 #  DATE_APH_UNSCHED = case_when(APH_UNSCHED_ANY==1 ~ ymd(M04_VISIT_DATE_13),
 #                              (HEM_HOSP_ANY==1 & TIMING_HOSP==1) ~ ymd(DATE_HOSPITAL),
 #                              TRUE~ 0),
 #  
 #  DATE_APH = pmin(DATE_APH_SCHED,DATE_APH_UNSCHED),
 #  
 #  
 # ) %>% 
 #  
# M04_APH_CEOCCUR_1-5==1 (Current clinical status: antepartum hemorrhage)
# APH_UNSCHED_ANY==1 (Current clinical status: antepartum hemorrhage @ at any unscheduled visit)
# M09_APH_CEOCCUR_6==1 (Did the mother experience antepartum hemorrhage?)
# HEM_HOSP_ANY==1 (specify type of labor/delivery or birth complication: APH or PPH or vaginal bleeding)
# M19_TIMING_OHOCAT==1 (timing of hospitalization = antenatal period)

  ## 2. Postpartum Hemorrhage; 
  mutate(HEM_PPH = case_when(HEM_DENOM == 1 & (M09_PPH_CEOCCUR_6==1 | M09_PPH_FAORRES_1_6==1 | M09_PPH_FAORRES_2_6==1 |
                                               M09_PPH_FAORRES_3_6==1 | M09_PPH_FAORRES_4_6==1 |
                                               M09_PPH_FAORRES_5_6==1 | 
                                               M09_PPH_TRNSFSN_PROCCUR_6==1 | M09_PPH_ESTIMATE_FAORRES_6 >=500 |
                                               M12_VAG_BLEED_LOSS_ML_7>=500 | M12_VAG_BLEED_LOSS_ML_8>=500 | M12_VAG_BLEED_LOSS_ML_9>=500 | M12_VAG_BLEED_LOSS_ML_10>=500 |
                                               M12_VAG_BLEED_LOSS_ML_11>=500 | M12_VAG_BLEED_LOSS_ML_12>=500 |
                                               M12_BIRTH_COMPL_MHTERM_1_7==1 |  M12_BIRTH_COMPL_MHTERM_1_8==1 | M12_BIRTH_COMPL_MHTERM_1_9==1 | M12_BIRTH_COMPL_MHTERM_1_10==1 |
                                               M12_BIRTH_COMPL_MHTERM_1_11==1 | M12_BIRTH_COMPL_MHTERM_1_12==1 |
                                              (HEM_HOSP_ANY==1 & TIMING_HOSP==2)) ~ 1, TRUE ~ 0)) %>% 
  
  
  ## 3. Severe postpartum hemorrhage
  mutate(HEM_PPH_SEV = case_when(HEM_DENOM==1 & HEM_PPH==1 & (M09_PPH_ESTIMATE_FAORRES_6>=1000 | 
                                                                M12_VAG_BLEED_LOSS_ML_7>=1000 | M12_VAG_BLEED_LOSS_ML_8>=1000 | M12_VAG_BLEED_LOSS_ML_9>=1000 | M12_VAG_BLEED_LOSS_ML_10>=1000 |
                                                                M12_VAG_BLEED_LOSS_ML_11>=1000 | M12_VAG_BLEED_LOSS_ML_12>=1000 |
                                                                (M19_LD_COMPL_ML >=1000  & TIMING_HOSP == 2) | 
                                                                M09_PPH_TRNSFSN_PROCCUR_6==1 | 
                                                                M09_PPH_FAORRES_1_6==1 | M09_PPH_FAORRES_2_6==1 |
                                                                M09_PPH_FAORRES_3_6==1 | M09_PPH_FAORRES_4_6==1 |
                                                                M09_PPH_FAORRES_5_6==1) ~ 1, TRUE ~0)
  ) %>% 
  
  ## 4. Any hemorrhage at any time point
  mutate(HEM_ANY = case_when(HEM_APH==1 | HEM_PPH ==1| HEM_PPH_SEV==1~1, TRUE ~ 0)) %>% 
  select(SITE, MOMID, PREGID,PREG_END, HEM_DENOM, HEM_APH, HEM_PPH, HEM_PPH_SEV,HEM_ANY,
         contains("CRIT"),M09_PPH_ESTIMATE_FAORRES_6, contains("M09_PPH_FAORRES"), M12_VAG_BLEED_LOSS_ML_7, 
         M12_VAG_BLEED_LOSS_ML_8, M12_VAG_BLEED_LOSS_ML_9, M12_VAG_BLEED_LOSS_ML_10, M12_VAG_BLEED_LOSS_ML_11, 
         M12_VAG_BLEED_LOSS_ML_12,M09_PPH_SPFY_FAORRES_6, M09_PPH_TRNSFSN_PROCCUR_6, 
         contains("M09_PPH_CMOCCUR"), M09_PPH_CEOCCUR_6,
         HEM_HOSP_ANY, TIMING_HOSP, M09_PPH_PEMETHOD_6, 
         M12_BIRTH_COMPL_MHTERM_1_7, M12_BIRTH_COMPL_MHTERM_1_8, M12_BIRTH_COMPL_MHTERM_1_9, M12_BIRTH_COMPL_MHTERM_1_10, M12_BIRTH_COMPL_MHTERM_1_11, M12_BIRTH_COMPL_MHTERM_1_12)


# test <- hemorrhage %>% 
#   select(SITE, MOMID, PREGID, HEM_APH, HEM_PPH, HEM_PPH_SEV,HEM_ANY,
#                               contains("DATE_"), contains("CRIT")) %>% 
#   filter(HEM_APH==1)
# 

# Extract the column names that match the pattern "date" and "cond"
date_cols <- grep("DATE_", names(test), value = TRUE)
cond_cols <- grep("CRIT", names(test), value = TRUE)

# Function to find the earliest date of onset where condition == 1
earliest_dates <- apply(test, 1, function(row) {
  # Extract dates and conditions dynamically using the matched columns
  dates <- ymd(unlist(row[date_cols]))
  conditions <- unlist(row[cond_cols])
  
  # Filter dates where condition == 1
  valid_dates <- dates[conditions == 1]
  
  # Return the earliest date
  pmin(valid_dates, na.rm = TRUE)
})

# # Add the earliest date of onset to the original data-frame
# test$earliest_onset <- earliest_dates

table(hemorrhage$TIMING_HOSP, useNA = "ifany")
table(hemorrhage$HEM_APH, useNA = "ifany")
table(hemorrhage$HEM_PPH, hemorrhage$SITE, useNA = "ifany")
table(hemorrhage$HEM_PPH_SEV, useNA = "ifany")

# Define the order of legend labels
hemorrhage_figs$CUTOFF <- factor(hemorrhage_figs$CUTOFF, levels = c("No hemorrhage (<500mL)","Hemorrhage (>=500mL)", 
                                                                    "Severe Hemorrhage(>=1000mL)"))
hemorrhage_fig <- ggplot() + 
  geom_histogram(data = hemorrhage_figs, aes(x = M09_PPH_ESTIMATE_FAORRES_6, fill = CUTOFF),  color = "gray", binwidth = 50) + 
  scale_fill_manual(values = c("darkgreen", "darkorange","darkred"), name = "") + 
  facet_grid(rows = vars(SITE), scales = "free_y") + 
  xlab("Estimated blood loss (mL)") + 
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(0,2000,250), limits = c(0, 2000)) +
  geom_vline(xintercept = 500, linetype = "dashed") +
  geom_vline(xintercept = 1000, linetype = "dashed") + 
  theme_bw() + 
  theme(strip.background=element_rect(fill="white"),
        axis.text.x = element_text(vjust = 1, hjust=0.5),
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) 


ggsave(paste0("hemorrhage_fig", ".pdf"), path = path_to_save,
       width = 8, height = 6)

################################    
# RENAME ALL VARIABLES 
################################    


hemorrhage <- hemorrhage %>% 
  rename(EST_BLOOD_LOSS_IPC = M09_PPH_ESTIMATE_FAORRES_6,
          PPH_IPC_BALLOON = M09_PPH_FAORRES_1_6,
          PPH_IPC_SURG	= M09_PPH_FAORRES_2_6,
          PPH_IPC_BRACE	= M09_PPH_FAORRES_3_6,
          PPH_IPC_VESSEL	= M09_PPH_FAORRES_4_6,
          PPH_IPC_HYSTER	= M09_PPH_FAORRES_5_6,
          PPH_IPC_NA	= M09_PPH_FAORRES_77_6,
          PPH_IPC_OTHER	= M09_PPH_FAORRES_88_6,
          PPH_IPC_DONTKNOW	= M09_PPH_FAORRES_99_6,
          PPH_IPC_OTHER_SPFY	= M09_PPH_SPFY_FAORRES_6,
          BLOOD_TRANS_IPC	= M09_PPH_TRNSFSN_PROCCUR_6,
          PPH_MED_OCY	= M09_PPH_CMOCCUR_1_6,
          PPH_MED_MISO	= M09_PPH_CMOCCUR_2_6,
          PPH_MED_TRANEX	= M09_PPH_CMOCCUR_3_6,
          PPH_MED_CARBET	= M09_PPH_CMOCCUR_4_6,
          PPH_MED_METHYL	= M09_PPH_CMOCCUR_5_6,
          PPH_MED_CARBOP	= M09_PPH_CMOCCUR_6_6,
          PPH_MED_OTHER	= M09_PPH_CMOCCUR_77_6,
          PPH_MED_NONE	= M09_PPH_CMOCCUR_88_6,
          PPH_MED_DONTKNOW	= M09_PPH_CMOCCUR_99_6,
          PPH_REPORT_IPC	= M09_PPH_CEOCCUR_6,
          BLOOD_LOSS_METHOD_IPC = M09_PPH_PEMETHOD_6,
          BIRTH_COMPL_PPH_PNC0	= M12_BIRTH_COMPL_MHTERM_1_7,
          BIRTH_COMPL_PPH_PNC1	= M12_BIRTH_COMPL_MHTERM_1_8,
          BIRTH_COMPL_PPH_PNC4	= M12_BIRTH_COMPL_MHTERM_1_9,
          BIRTH_COMPL_PPH_PNC6	= M12_BIRTH_COMPL_MHTERM_1_10,
          BIRTH_COMPL_PPH_PNC26	= M12_BIRTH_COMPL_MHTERM_1_11,
          BIRTH_COMPL_PPH_PNC52	= M12_BIRTH_COMPL_MHTERM_1_12,
          EST_BLOOD_LOSS_PNC0	= M12_VAG_BLEED_LOSS_ML_7,
          EST_BLOOD_LOSS_PNC1	= M12_VAG_BLEED_LOSS_ML_8,
          EST_BLOOD_LOSS_PNC4	= M12_VAG_BLEED_LOSS_ML_9,
          EST_BLOOD_LOSS_PNC6	= M12_VAG_BLEED_LOSS_ML_10,
          EST_BLOOD_LOSS_PNC26	= M12_VAG_BLEED_LOSS_ML_11,
          EST_BLOOD_LOSS_PNC52	= M12_VAG_BLEED_LOSS_ML_12
          ) 

################################    
# EXPORT DATA
################################    
write.csv(hemorrhage, paste0(path_to_save, "MAT_HEMORRHAGE" ,".csv"), na="",row.names=FALSE)
write.xlsx(hemorrhage, paste0(path_to_save, "MAT_HEMORRHAGE" ,".xlsx"),na="", rowNames=FALSE)

write.csv(hemorrhage, paste0(path_to_tnt, "MAT_HEMORRHAGE" ,".csv"), na="",row.names=FALSE)
write.xlsx(hemorrhage, paste0(path_to_tnt, "MAT_HEMORRHAGE" ,".xlsx"),na="", rowNames=FALSE)

hemorrhage_figs <- hemorrhage %>% 
  select(SITE, MOMID, PREGID, M09_PPH_ESTIMATE_FAORRES_6,HEM_PPH, HEM_PPH_SEV) %>% 
  filter(M09_PPH_ESTIMATE_FAORRES_6 > 0) %>% 
  mutate(CUTOFF = case_when(M09_PPH_ESTIMATE_FAORRES_6 >= 1000~ "Severe Hemorrhage(>=1000mL)",
                            M09_PPH_ESTIMATE_FAORRES_6 >= 500 & M09_PPH_ESTIMATE_FAORRES_6 < 1000 ~ "Hemorrhage (>=500mL)",
                            TRUE ~ "No hemorrhage (<500mL)"
  ))

################################    
# Confirm all varnames are in the data dictionary
################################    
outcome_dd <-  read_excel("D:/Users/stacie.loisate/Desktop/PRISMA-Outcomes-DataDictionary-Active (2).xlsx",
                         sheet = "Maternal Outcomes") %>%
  filter(`Data set` == "MAT_HEMORRHAGE" ) %>%
  select(-`Data set`)

mat_hem <- read.csv(paste0(path_to_tnt, "MAT_HEMORRHAGE.csv")) 
  # select(-contains("DATE"), -M09_PPH_SPFY_6,
  #        -M12_VAG_BLEED_LOSS_ML_1,
  #        -M12_VAG_BLEED_LOSS_ML_2, -M12_VAG_BLEED_LOSS_ML_3, -M12_VAG_BLEED_LOSS_ML_4, -M12_VAG_BLEED_LOSS_ML_5,
  #        -PREG_END_DATE
  #        )
# hemorrhage = mat_hem

mat_hem_names <- as.data.frame(colnames(mat_hem)) %>%
  mutate(data = 1) %>%
  rename(`Variable Name` = "colnames(mat_hem)")

missing_names <-  outcome_dd  %>% mutate(dd=1) %>%
  full_join(mat_hem_names, by = c("Variable Name"))
# 
# 
