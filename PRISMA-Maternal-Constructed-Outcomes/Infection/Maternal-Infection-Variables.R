#*****************************************************************************
#* PRISMA Maternal Infection
#* Drafted: 25 October 2023, Stacie Loisate
#* Last updated: 7 November 2025

## This code will generate maternal infection outcomes at the following time points: 
# Enrollment
# 1. HIV
# 2. MALilis
# 3. Gonorrhea
# 4. Chlamydia
# 5. Genital Ulcers
# 6. Malaria 
# 7. Hepatitis (Hep B, Hep C, Hep E)
# 8. TB
# 9. Zika/Dengue/Chikungunya
# 10. Leptospirosis

# Any visit 
# 1. MALilis 
# 1. HIV
# 2. MALilis
# 3. Gonorrhea
# 4. Chlamydia
# 5. Genital Ulcers
# 6. Malaria 
# 7. Hepatitis (Hep B, Hep C)
# 8. TB
#*****************************************************************************
#*****************************************************************************
#* Data Setup  ----
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
UploadDate = "2025-10-31"

# set path to save 
path_to_save <- "D:/Users/stacie.loisate/Documents/Output/Infection-Troubleshooting/2025-08-25/"
path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")

# set path to data
# path_to_data = paste0("Z:/Stacked Data/",UploadDate)
path_to_data <- paste0("D:/Users/stacie.loisate/Documents/import/", UploadDate)

mat_enroll <- read_xlsx(paste0(path_to_tnt, "MAT_ENROLL" ,".xlsx" )) %>% select(SITE, MOMID, PREGID, ENROLL, ENROLL_SCRN_DATE, EDD_BOE, PREG_START_DATE) %>% 
  filter(ENROLL == 1)

mat_dem <- read_xlsx(paste0(path_to_tnt, "MAT_DEMOGRAPHIC" ,".xlsx" )) 

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


mnh04 <- mnh04 %>% 
  mutate(TYPE_VISIT=M04_TYPE_VISIT) %>% 
  mutate(VISIT_DATE=M04_ANC_OBSSTDAT) %>% 
  mutate(VISIT_DATE = replace(VISIT_DATE, VISIT_DATE==ymd("1907-07-07"), NA),
         VISIT_DATE = replace(VISIT_DATE, VISIT_DATE==ymd("1905-05-05"), NA),
         VISIT_DATE = replace(VISIT_DATE, str_trim(VISIT_DATE) == "", NA)
  ) %>% 
  mutate(
    M04_HIV_MHSTDAT = replace(M04_HIV_MHSTDAT, M04_HIV_MHSTDAT %in% c("1907-07-07", "1905-05-05", "1909-09-09"), NA),
    M04_HIV_MHSTDAT = replace(M04_HIV_MHSTDAT, str_trim(M04_HIV_MHSTDAT) == "", NA))


mnh06 <- mnh06 %>% 
  mutate(TYPE_VISIT=M06_TYPE_VISIT) %>% 
  mutate(VISIT_DATE=M06_DIAG_VSDAT)%>% 
  filter(M06_MAT_VISIT_MNH06 %in% c(1,2)) %>% 
  left_join(mat_enroll, by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(VISIT_DATE = replace(VISIT_DATE, VISIT_DATE==ymd("1907-07-07"), NA),
         VISIT_DATE = replace(VISIT_DATE, VISIT_DATE==ymd("1905-05-05"), NA),
         VISIT_DATE = replace(VISIT_DATE, str_trim(VISIT_DATE) == "", NA)
  ) 

mnh07 <- mnh07 %>% 
  mutate(TYPE_VISIT=M07_TYPE_VISIT) %>% 
  mutate(VISIT_DATE=M07_MAT_SPEC_COLLECT_DAT)%>% 
  filter(M07_MAT_VISIT_MNH07 %in% c(1,2)) %>% 
  mutate(VISIT_DATE = replace(VISIT_DATE, VISIT_DATE==ymd("1907-07-07"), NA),
         VISIT_DATE = replace(VISIT_DATE, VISIT_DATE==ymd("1905-05-05"), NA),
         VISIT_DATE = replace(VISIT_DATE, str_trim(VISIT_DATE) == "", NA)
  ) 

mnh08 <- mnh08 %>% 
  left_join(mat_enroll %>% select(SITE, MOMID, PREGID, ENROLL_SCRN_DATE), by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(TYPE_VISIT=M08_TYPE_VISIT) %>% 
  mutate(VISIT_DATE=M08_LBSTDAT)%>% 
  filter(M08_MAT_VISIT_MNH08 %in% c(1,2)) %>% 
  ## GENERATE VARIABLE FOR ZCD EXPANSION 
  mutate(ZCD_EXPANSION_DATE = case_when(SITE == "Ghana" ~ "2024-04-09", ## CONFIRM 04-09 IS THE RIGHT DATE -- THERE ARE N=2 TESTS DATED 04-08
                                        SITE == "India-CMC" ~ "2024-04-23",
                                        SITE == "India-SAS" ~ "2024-04-29",
                                        SITE == "Kenya" ~ "2024-05-06",
                                        SITE == "Pakistan" ~ "2024-04-05",
                                        SITE == "Zambia" ~ "2024-03-19",
                                        TRUE ~ NA
  )) %>% 
  ## GENERATE VARIABLE FOR HEV EXPANSION 
  mutate(HEV_EXPANSION_DATE = case_when(SITE == "Ghana" ~ "2024-04-09", ## CONFIRM 04-09 IS THE RIGHT DATE -- THERE ARE N=2 TESTS DATED 04-08
                                        SITE == "India-CMC" ~ "2024-03-06",
                                        SITE == "India-SAS" ~ "2024-03-11",
                                        SITE == "Kenya" ~ "2024-03-06",
                                        SITE == "Pakistan" ~ "2024-04-05",
                                        SITE == "Zambia" ~ "2024-03-19",
                                        TRUE ~ NA
  )) %>% 
  ## GENERATE VARIABLE FOR LEPTO EXPANSION 
  mutate(LEPT_EXPANSION_DATE = case_when(SITE == "Ghana" ~ "2024-04-09", ## CONFIRM 04-09 IS THE RIGHT DATE -- THERE ARE N=2 TESTS DATED 04-08
                                         SITE == "India-CMC" ~ "2024-03-06",
                                         SITE == "India-SAS" ~ "2024-03-11",
                                         SITE == "Kenya" ~ "2024-03-06",
                                         SITE == "Pakistan" ~ "2024-04-08",
                                         SITE == "Zambia" ~ "2023-11-09",
                                         TRUE ~ NA
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
  mutate(CTNG_EXPANSION = case_when(ENROLL_SCRN_DATE >= CTNG_EXPANSION_DATE ~ 1, TRUE ~ 0),
         ZCD_EXPANSION = case_when(VISIT_DATE >= ZCD_EXPANSION_DATE ~ 1, TRUE ~ 0),
         HEV_EXPANSION= case_when(VISIT_DATE >= HEV_EXPANSION_DATE ~ 1, TRUE ~ 0),
         LEPT_EXPANSION = case_when(VISIT_DATE >= LEPT_EXPANSION_DATE ~ 1, TRUE ~ 0)
  )  %>% 
  select(-ENROLL_SCRN_DATE) %>% 
  mutate(VISIT_DATE = replace(VISIT_DATE, VISIT_DATE==ymd("1907-07-07"), NA),
         VISIT_DATE = replace(VISIT_DATE, VISIT_DATE==ymd("1905-05-05"), NA),
         VISIT_DATE = replace(VISIT_DATE, str_trim(VISIT_DATE) == "", NA)
  ) 

#*****************************************************************************
# HIV ----
#*****************************************************************************

hiv <- mat_enroll %>% 
  left_join(mnh04, by = c("SITE", "MOMID", "PREGID")) %>% 
  select(SITE, MOMID, PREGID, ENROLL_SCRN_DATE, PREG_START_DATE, TYPE_VISIT, VISIT_DATE,
         M04_ANC_OBSSTDAT, M04_MAT_VISIT_MNH04, M04_HIV_EVER_MHOCCUR, M04_HIV_MHOCCUR, M04_HIV_MHSTDAT) %>% 
  # merge in mnh06 to pull rdt results 
  left_join(mnh06 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, M06_DIAG_VSDAT, M06_HIV_POC_LBORRES, M06_HIV_POC_LBPERF), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  # Was test performed?
  mutate(HIV_RDT_PERF = case_when(M06_HIV_POC_LBPERF ==1 ~ 1, TRUE ~ 0)) %>% 
  # Test result available? 
  mutate(HIV_RESULT_AVAI = case_when(M06_HIV_POC_LBORRES %in% c(1,0) ~ 1, TRUE ~ 0)) %>% 
  # Test result by RDT 
  mutate(HIV_RDT_POSITIVE = case_when(HIV_RDT_PERF == 1 & M06_HIV_POC_LBORRES == 1 ~ 1, # 1, positive
                                      HIV_RDT_PERF == 1 & M06_HIV_POC_LBORRES == 0 ~ 0,  # 0, negative
                                      HIV_RDT_PERF == 1 & M06_HIV_POC_LBORRES %in% c(55,77,99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      HIV_RDT_PERF == 0 ~ 77,  #77, na if test was not performed 
                                      TRUE ~ 77)) %>% 
  # Test result by Dx 
  mutate(HIV_DX_POSITIVE = case_when(TYPE_VISIT != 1 & M04_HIV_MHOCCUR == 1 ~ 1, 
                                     TYPE_VISIT != 1 & SITE == "Kenya" & M04_HIV_MHOCCUR == 0 ~ 0, # Kenya: M04_HIV_MHOCCUR == 0 can be negative 0 (baseline and incident) 
                                     TYPE_VISIT != 1 & M04_HIV_MHOCCUR %in% c(55, 77, 99, 0, NA) ~ 77,
                                     
                                     ## For enrollment, include history of hiv variable (M04_HIV_EVER_MHOCCUR)
                                     TYPE_VISIT == 1 & (M04_HIV_MHOCCUR == 1 | M04_HIV_EVER_MHOCCUR ==1) ~ 1, 
                                     TYPE_VISIT == 1 & SITE == "Kenya" & (M04_HIV_EVER_MHOCCUR== 0 | M04_HIV_MHOCCUR == 0) ~ 0, # Kenya: M04_HIV_EVER_MHOCCUR == 0 | M04_HIV_MHOCCUR == 0 can be negative 0 (baseline and incident) 
                                     TYPE_VISIT == 1 & (M04_HIV_MHOCCUR %in% c(55, 77, 99, 0, NA) | M04_HIV_EVER_MHOCCUR %in% c(55, 77, 99, 0, NA)) ~ 99, 
                                     TRUE ~ 77 
  )) %>% 
  
  # Test result by RDT+ or RDT+/Dx+
  mutate(HIV_POSITIVE = case_when(HIV_RDT_POSITIVE ==1 ~ 1, # 1, positive if RDT only is positive
                                  TYPE_VISIT == 1 & (HIV_RDT_POSITIVE ==1 | HIV_DX_POSITIVE==1) ~ 1, # 1, positive if RDT or Dx is positive AND type visit = 1 
                                  HIV_RDT_POSITIVE ==1 & HIV_DX_POSITIVE==1 ~ 1, #1, positive if RDT and Dx is positive
                                  HIV_RDT_POSITIVE==0 ~ 0, #0, negative if rdt is negative 
                                  TYPE_VISIT == 1 & HIV_DX_POSITIVE==0 ~ 0, # 0, if enrollment visit and no previous dx of hiv reported -- review this point 
                                  HIV_RDT_POSITIVE==55 ~ 55, # 55, missing (if test was performed but result is missing)
                                  HIV_RDT_PERF == 0 ~ 77,  #77, na if test was not performed 
                                  TRUE ~ 77)) %>% 
  # generate indicator variable to remove any unscheduled visits that do not have a test outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & HIV_RDT_PERF ==1 & HIV_RESULT_AVAI ==1 ~ 1,
                                  TYPE_VISIT %in% c(13,14) & HIV_RDT_POSITIVE != 1 ~ 0, TRUE ~ 1)) %>% 
  filter(KEEP_UNSCHED==1) %>% 
  # if positive then keep the date, if not, replace date with NA
  mutate(DATE_POSITIVE = case_when(M04_HIV_EVER_MHOCCUR ==1 & !is.na(ymd(M04_HIV_MHSTDAT)) ~ ymd(M04_HIV_MHSTDAT), 
                                   HIV_RDT_POSITIVE==1 ~ ymd(M06_DIAG_VSDAT), 
                                   HIV_DX_POSITIVE ==1 & SITE == "Kenya" & TYPE_VISIT==1 ~ ymd(M04_ANC_OBSSTDAT), 
                                   TRUE~ NA_Date_
  ))

## stop here to check if there are any reported unscheduled cases 
# if (dim(hiv %>% filter(TYPE_VISIT %in% c(13,14)))[1]>=1){ # if any missing signs of life are flagged, loop will run and extract query output
#   stop("unscheduled cases exist")
# } else {
#   print("no unscheduled cases")
# }

hiv_export <- hiv %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, VISIT_DATE, PREG_START_DATE, DATE_POSITIVE, starts_with("HIV_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("HIV_"), VISIT_DATE, DATE_POSITIVE),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(HIV_RDT_POSITIVE_ENROLL = HIV_RDT_POSITIVE_1,
         HIV_DX_POSITIVE_ENROLL = HIV_DX_POSITIVE_1,
         HIV_RDT_PERF_ENROLL = HIV_RDT_PERF_1,
         HIV_POSITIVE_ENROLL = HIV_POSITIVE_1,
         MISSING_ENROLL_STATUS = case_when(HIV_DX_POSITIVE_ENROLL %in% c(1,0) | HIV_RDT_POSITIVE_ENROLL %in% c(1,0) ~ 0, # for all sites, missing if Dx or RDT is not 1 or 0
                                           TRUE ~ 1),
         # this variable doesn't make a lot of sense -- don't need but see if there are data points 
         HIV_DX_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("HIV_DX_POSITIVE_")) == 1) ~ 1, # 1, Positive if DX+
                                               # any(na.omit(c_across(starts_with("HIV_DX_POSITIVE_"))) == 0) ~ 0, # Dx alone does not give you a negative result
                                               all(na.omit(c_across(starts_with("HIV_DX_POSITIVE_"))) ==55) ~ 55, # 55, Missing if both rdt and titers are missing
                                               all(na.omit(c_across(starts_with("HIV_DX_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                               TRUE ~ 77),
         HIV_RDT_PERF_EVER_PREG = case_when(any(c_across(starts_with("HIV_RDT_PERF_")) == 1) ~ 1, # 1, Positive if test not performed 
                                            any(na.omit(c_across(starts_with("HIV_RDT_PERF_"))) == 0) ~ 0, # 0, Negative if test not performed 
                                            all(na.omit(c_across(starts_with("HIV_RDT_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                            all(na.omit(c_across(starts_with("HIV_RDT_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                            TRUE ~ 77),
         HIV_RDT_POSITIVE_EVER_PREG = case_when(#!HIV_POSITIVE_ENROLL %in% c(1,0) ~ 55, ## in order to be included in ever during pregnancy, we need to know your baseline status 
           any(c_across(starts_with("HIV_RDT_POSITIVE_")) == 1) ~ 1, # 1, Positive if either rdt is + 
           any(na.omit(c_across(starts_with("HIV_RDT_POSITIVE_"))) == 0) ~ 0, # 0, Negative ifany rdt is -
           all(na.omit(c_across(starts_with("HIV_RDT_POSITIVE_"))) ==55) ~ 55, # 55, Missing if both rdt and titers are missing
           all(na.omit(c_across(starts_with("HIV_RDT_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
           TRUE ~ 77),
         
         HIV_POSITIVE_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("HIV_RDT_POSITIVE_")) == 1) & HIV_RDT_POSITIVE_ENROLL %in% c(55,77,99,NA)  & HIV_DX_POSITIVE_ENROLL %in% c(55,77,99,NA)  ~ 1,
                                                   TRUE ~ 0),
         
         HIV_POSITIVE_INCIDENT = case_when(HIV_RDT_POSITIVE_EVER_PREG ==1 & HIV_POSITIVE_ENROLL ==0 ~ 1,
                                           TRUE  ~ 0)) %>% 
  mutate(HIV_POSITIVE_EVER_PREG = case_when(HIV_RDT_POSITIVE_EVER_PREG ==1 | HIV_POSITIVE_INCIDENT==1 | HIV_POSITIVE_ENROLL ==1 | HIV_POSITIVE_UNKNOWN_BASELINE ==1 ~ 1, ## EVER PREG variable is RDT+ ever during pregnancy, incident RDT+, or DX+ or RDT+ at enrollment
                                            HIV_RDT_POSITIVE_EVER_PREG ==0 & HIV_POSITIVE_INCIDENT== 0 & HIV_POSITIVE_ENROLL !=1 ~ 0,
                                            TRUE ~ 77)) %>% 
  ## generate date syphilis infection 
  mutate(HIV_DATE_POSITIVE = do.call(pmin, c(across(starts_with("DATE_POSITIVE_")), na.rm = TRUE)),
         HIV_GESTAGE_POSITIVE_DAYS = as.numeric(HIV_DATE_POSITIVE-ymd(PREG_START_DATE)),
         HIV_GESTAGE_POSITIVE_WKS = HIV_GESTAGE_POSITIVE_DAYS %/% 7)


## add labels
hiv_labels <- hiv_export %>%
  mutate(
    HIV_DX_POSITIVE_ENROLL_LABEL = factor(HIV_DX_POSITIVE_ENROLL,levels = c(1, 0, 77), 
                                          labels = c("Dx+ at enrollment", "Dx- at enrollment (only Kenya is eligible to be negative by Dx)", "Missing Dx at enrollment")),
    HIV_RDT_POSITIVE_ENROLL_LABEL = factor(HIV_RDT_POSITIVE_ENROLL,levels = c(1, 0, 55, 77), 
                                           labels = c("RDT+ at enrollment", "RDT- at enrollment", "Missing RDT at enrollment", "NA/no test performed")),
    HIV_RDT_POSITIVE_EVER_PREG_LABEL = factor(HIV_RDT_POSITIVE_EVER_PREG,levels = c(1, 0,55,77), 
                                              labels = c("RDT+ ever during pregnancy", "RDT- during pregnancy", "Missing RDT during pregnancy", "NA/no test performed")),
    HIV_POSITIVE_UNKNOWN_BASELINE_LABEL = factor(HIV_POSITIVE_UNKNOWN_BASELINE,levels = c(1, 0), 
                                                 labels = c("RDT+ during pregnancy but missing baseline status", "No incident HIV with unknown baseline status")),
    HIV_POSITIVE_ENROLL_LABEL = factor(HIV_POSITIVE_ENROLL,levels = c(1, 0, 55, 77), 
                                       labels = c("HIV positive at enrollment by RDT or previous Dx", "HIV negative at enrollment", "Missing RDT at enrollment", "NA/no test performed")),
    HIV_POSITIVE_INCIDENT_LABEL = factor(HIV_POSITIVE_INCIDENT,levels = c(1, 0), 
                                         labels = c("Incident HIV RDT+ during pregnancy", "No incident HIV infection")),
    HIV_POSITIVE_EVER_PREG_LABEL = factor(HIV_POSITIVE_EVER_PREG,levels = c(1, 0, 77), 
                                          labels = c("HIV positive ever during pregnancy by RDT (or Dx at enrollment)", "HIV never during pregnancy", "NA/no test performed"))
  ) %>% 
  mutate(HIV_POSITIVE_EVER_PREG_CAT_LABEL = case_when(HIV_DX_POSITIVE_ENROLL ==1 & HIV_RDT_POSITIVE_ENROLL !=1 ~ "previous Dx at enroll",
                                                      HIV_DX_POSITIVE_ENROLL !=1 & HIV_RDT_POSITIVE_ENROLL ==1 ~ "RDT+ at enroll",
                                                      HIV_DX_POSITIVE_ENROLL ==1 & HIV_RDT_POSITIVE_ENROLL ==1 ~ "previous Dx & RDT+ at enroll",
                                                      HIV_POSITIVE_INCIDENT ==1 ~ "incident RDT+",
                                                      HIV_POSITIVE_UNKNOWN_BASELINE == 1 ~ "RDT+ with unknown baseline",
                                                      TRUE ~ NA),
         HIV_POSITIVE_EVER_PREG_CAT = case_when(HIV_DX_POSITIVE_ENROLL ==1 & HIV_RDT_POSITIVE_ENROLL !=1 ~ 1,
                                                HIV_DX_POSITIVE_ENROLL !=1 & HIV_RDT_POSITIVE_ENROLL ==1 ~ 2,
                                                HIV_DX_POSITIVE_ENROLL ==1 & HIV_RDT_POSITIVE_ENROLL ==1 ~ 3,
                                                HIV_POSITIVE_INCIDENT ==1 ~ 4,
                                                HIV_POSITIVE_UNKNOWN_BASELINE == 1 ~ 5,
                                                TRUE ~ NA))

hiv_export_labels <- hiv_labels %>% 
  select(SITE, MOMID, PREGID,
         HIV_DATE_POSITIVE, HIV_GESTAGE_POSITIVE_DAYS, HIV_GESTAGE_POSITIVE_WKS,
         HIV_DX_POSITIVE_ENROLL, HIV_DX_POSITIVE_ENROLL_LABEL,
         HIV_RDT_PERF_ENROLL, HIV_RDT_POSITIVE_ENROLL, HIV_RDT_POSITIVE_ENROLL_LABEL,
         HIV_RDT_PERF_EVER_PREG, HIV_POSITIVE_ENROLL, HIV_POSITIVE_ENROLL_LABEL, 
         HIV_POSITIVE_INCIDENT, HIV_POSITIVE_INCIDENT_LABEL,
         HIV_POSITIVE_UNKNOWN_BASELINE, HIV_POSITIVE_UNKNOWN_BASELINE_LABEL,
         HIV_POSITIVE_EVER_PREG, HIV_POSITIVE_EVER_PREG_LABEL,
         HIV_POSITIVE_EVER_PREG_CAT, HIV_POSITIVE_EVER_PREG_CAT_LABEL
         #HIV_RDT_POSITIVE_EVER_PREG, HIV_RDT_POSITIVE_EVER_PREG_LABEL,
  )

# view(hiv_table)
# write.xlsx(hiv_table, paste0(path_to_save, "hiv_table" ,".xlsx"), na="", rowNames=TRUE)

#*****************************************************************************
# Syphilis ----
#*****************************************************************************

syphilis <- mat_enroll %>% 
  left_join(mnh04, by = c("SITE", "MOMID", "PREGID")) %>% 
  select(SITE, MOMID, PREGID, ENROLL_SCRN_DATE,PREG_START_DATE, TYPE_VISIT, VISIT_DATE, M04_MAT_VISIT_MNH04,M04_ANC_OBSSTDAT, M04_SYPH_MHOCCUR) %>% 
  # merge in mnh06 to pull rdt results 
  left_join(mnh06 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, M06_DIAG_VSDAT, M06_SYPH_POC_LBORRES, M06_SYPH_POC_LBPERF), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  # merge in mnh07 for specimen collection date
  left_join(mnh07 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, M07_MAT_SPEC_COLLECT_DAT), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  # merge in mnh08 to pull titer results 
  left_join(mnh08 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, M08_SYPH_TITER_LBTSTDAT, M08_SYPH_TITER_LBORRES, M08_SYPH_TITER_LBPERF_1), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  # Was test performed?
  mutate(SYPH_RDT_PERF = case_when(M06_SYPH_POC_LBPERF ==1 ~ 1,
                                   is.na(M06_SYPH_POC_LBPERF) ~ 0,
                                   TRUE ~ 0),
         SYPH_TITER_PERF = case_when(M08_SYPH_TITER_LBPERF_1 ==1 ~ 1,
                                     is.na(M08_SYPH_TITER_LBPERF_1) ~ 55,
                                     TRUE ~ 0)
  ) %>%
  # Test result available? 
  mutate(SYPH_RDT_RESULT_AVAI = case_when(M06_SYPH_POC_LBORRES %in% c(1,0) ~ 1, TRUE ~ 0),
         SYPH_TITER_RESULT_AVAI = case_when(M08_SYPH_TITER_LBORRES %in% c(1,0) ~ 1, TRUE ~ 0)
  ) %>%
  # Test result by RDT or Titers
  mutate(SYPH_RDT_POSITIVE = case_when(SYPH_RDT_PERF == 1 & M06_SYPH_POC_LBORRES == 1 ~ 1, # 1, positive
                                       SYPH_RDT_PERF == 1 & M06_SYPH_POC_LBORRES == 0 ~ 0,  # 0, negative
                                       SYPH_RDT_PERF == 1 & M06_SYPH_POC_LBORRES %in% c(55,77,99) ~ 55, # 55, missing (if test was performed but result is missing)
                                       SYPH_RDT_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                       TRUE ~ 77),
         SYPH_TITER_POSITIVE = case_when(SYPH_TITER_PERF == 1 & M08_SYPH_TITER_LBORRES == 1 ~ 1, # 1, positive
                                         SYPH_TITER_PERF == 1 & M08_SYPH_TITER_LBORRES == 0 ~ 0,  # 0, negative
                                         SYPH_TITER_PERF == 1 & M08_SYPH_TITER_LBORRES %in% c(55,77,99) ~ 55, # 55, missing (if test was performed but result is missing)
                                         SYPH_TITER_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                         TRUE ~ NA)) %>% 
  # Test result by Dx 
  mutate(SYPH_DX_POSITIVE = case_when(M04_SYPH_MHOCCUR == 1 & SITE == "Kenya" ~ 1, 
                                      SITE == "Kenya" & M04_SYPH_MHOCCUR == 0 ~ 0, # Kenya: M04_SYPH_MHOCCUR == 0 can be negative 0 (baseline and incident) 
                                      M04_SYPH_MHOCCUR %in% c(55, 77, 99, 0, NA) ~ 77,
                                      TRUE ~ 77)) %>% 
  # Test result by RDT+ or RDT+/Dx+
  mutate(SYPH_POSITIVE = case_when(SYPH_RDT_POSITIVE ==1 ~ 1, # 1, positive if RDT only is positive
                                   SITE == "Kenya" & TYPE_VISIT == 1 & SYPH_DX_POSITIVE==1 ~ 1, # 1, kenya dx+ counts as syphilis positive at enrollment
                                   SYPH_RDT_POSITIVE==0 ~ 0, #0, negative if rdt is negative 
                                   SYPH_RDT_POSITIVE==55 ~ 55, # 55, missing (if test was performed but result is missing)
                                   SYPH_RDT_PERF == 0 ~ 77,  #77, na if test was not performed 
                                   TRUE ~ 77)) %>% 
  # generate indicator variable to remove any unscheduled visits that do not have a test outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & SYPH_RDT_PERF ==1 & SYPH_RDT_RESULT_AVAI ==1 ~ 1,
                                  TYPE_VISIT %in% c(13,14) & (SYPH_DX_POSITIVE ==1 | SYPH_RDT_POSITIVE %in% c(1,0)) ~ 1,
                                  TYPE_VISIT %in% c(13,14) & (SYPH_DX_POSITIVE %in% c(0,NA,55,77) |
                                                                SYPH_RDT_POSITIVE %in% c(NA,55,77)) ~ 1,
                                  TYPE_VISIT %in% c(1,2,3,4,5) ~ 1,
                                  TRUE ~ 0)) %>% 
  filter(KEEP_UNSCHED==1) %>% 
  # if positive then keep the date, if not, replace date with NA
  mutate(DATE_POSITIVE = case_when(SYPH_RDT_POSITIVE==1 ~ ymd(M06_DIAG_VSDAT), 
                                   SYPH_DX_POSITIVE ==1 & SITE == "Kenya" & TYPE_VISIT==1 ~ ymd(M04_ANC_OBSSTDAT), 
                                   TRUE~ NA_Date_
  ))

# table(syphilis$SYPH_DX_POSITIVE, syphilis$SITE)
## stop here to check if there are any reported unschedule cases 
# if (dim(syphilis %>% filter(TYPE_VISIT ==13))[1]>=1){ # if any missing signs of life are flagged, loop will run and extract query output
#   stop("PNC cases exist")
# } else {
#   print("no PNC cases")
# }

syph_export <- syphilis %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID,PREG_START_DATE, VISIT_SEQ, VISIT_DATE, starts_with("SYPH_"), DATE_POSITIVE) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("SYPH_"), VISIT_DATE, DATE_POSITIVE),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # Generate indicator variables for syphilis @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(SYPH_RDT_POSITIVE_ENROLL = SYPH_RDT_POSITIVE_1,
         SYPH_DX_POSITIVE_ENROLL = SYPH_DX_POSITIVE_1,
         SYPH_RDT_PERF_ENROLL = SYPH_RDT_PERF_1,
         SYPH_POSITIVE_ENROLL = SYPH_POSITIVE_1,
         SYPH_RDT_PERF_EVER_PREG = case_when(any(c_across(starts_with("SYPH_RDT_PERF_")) == 1) ~ 1, # 1, Positive if test not performed 
                                             any(na.omit(c_across(starts_with("SYPH_RDT_PERF_"))) == 0) ~ 0, # 0, Negative if test not performed 
                                             all(na.omit(c_across(starts_with("SYPH_RDT_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                             all(na.omit(c_across(starts_with("SYPH_RDT_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                             TRUE ~ 77),
         
         SYPH_RDT_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("SYPH_RDT_POSITIVE_")) == 1) ~ 1, # 1, Positive if either rdt is + 
                                                 any(na.omit(c_across(starts_with("SYPH_RDT_POSITIVE_"))) == 0) ~ 0, # 0, Negative ifany rdt is -
                                                 all(na.omit(c_across(starts_with("SYPH_RDT_POSITIVE_"))) ==55) ~ 55, # 55, Missing if both rdt and titers are missing
                                                 all(na.omit(c_across(starts_with("SYPH_RDT_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                 TRUE ~ 77),
         
         SYPH_POSITIVE_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("SYPH_RDT_POSITIVE_")) == 1) & SYPH_RDT_POSITIVE_ENROLL %in% c(55,77,99,NA) ~ 1,
                                                    TRUE ~ 0),
         
         SYPH_POSITIVE_INCIDENT = case_when(SYPH_RDT_POSITIVE_EVER_PREG ==1 & SYPH_POSITIVE_ENROLL ==0 ~ 1,
                                            TRUE  ~ 0)) %>% 
  
  mutate(SYPH_POSITIVE_EVER_PREG = case_when(SYPH_RDT_POSITIVE_EVER_PREG ==1 | SYPH_POSITIVE_INCIDENT==1 | SYPH_POSITIVE_ENROLL ==1 | SYPH_POSITIVE_UNKNOWN_BASELINE ==1 ~ 1, ## EVER PREG variable is RDT+ ever during pregnancy, incident RDT+, or DX+ or RDT+ at enrollment
                                             SYPH_RDT_POSITIVE_EVER_PREG ==0 & SYPH_POSITIVE_INCIDENT== 0 ~ 0,
                                             TRUE ~ 77)) %>% 
  ## generate date syphilis infection 
  mutate(SYPH_DATE_POSITIVE = do.call(pmin, c(across(starts_with("DATE_POSITIVE_")), na.rm = TRUE)),
         SYPH_GESTAGE_POSITIVE_DAYS = as.numeric(SYPH_DATE_POSITIVE-ymd(PREG_START_DATE)),
         SYPH_GESTAGE_POSITIVE_WKS = SYPH_GESTAGE_POSITIVE_DAYS %/% 7)


## add labels
syph_labels <- syph_export %>%
  mutate(
    SYPH_DX_POSITIVE_ENROLL_LABEL = factor(SYPH_DX_POSITIVE_ENROLL,levels = c(1, 0, 77), 
                                           labels = c("Dx+ at enrollment", "Dx- at enrollment (only Kenya is eligible to be negative by Dx)", "Missing Dx at enrollment")),
    SYPH_RDT_POSITIVE_ENROLL_LABEL = factor(SYPH_RDT_POSITIVE_ENROLL,levels = c(1, 0, 55, 77), 
                                            labels = c("RDT+ at enrollment", "RDT- at enrollment", "Missing RDT at enrollment", "NA/no test performed")),
    SYPH_RDT_POSITIVE_EVER_PREG_LABEL = factor(SYPH_RDT_POSITIVE_EVER_PREG,levels = c(1, 0,55,77), 
                                               labels = c("RDT+ ever during pregnancy", "RDT- during pregnancy", "Missing RDT during pregnancy", "NA/no test performed")),
    SYPH_POSITIVE_UNKNOWN_BASELINE_LABEL = factor(SYPH_POSITIVE_UNKNOWN_BASELINE,levels = c(1, 0), 
                                                  labels = c("RDT+ during pregnancy but missing baseline status", "No incident Syhilis with unknown baseline status")),
    SYPH_POSITIVE_ENROLL_LABEL = factor(SYPH_POSITIVE_ENROLL,levels = c(1, 0, 55, 77), 
                                        labels = c("Syhilis positive at enrollment by RDT (Dx only for Kenya)", "Syhilis negative at enrollment", "Missing RDT at enrollment", "NA/no test performed")),
    SYPH_POSITIVE_INCIDENT_LABEL = factor(SYPH_POSITIVE_INCIDENT,levels = c(1, 0), 
                                          labels = c("Incident Syhilis RDT+ during pregnancy", "No incident Syhilis infection")),
    SYPH_POSITIVE_EVER_PREG_LABEL = factor(SYPH_POSITIVE_EVER_PREG,levels = c(1, 0, 77), 
                                           labels = c("Syhilis positive ever during pregnancy by RDT (or Dx at enrollment[Kenya only])", "Syhilis never during pregnancy", "NA/no test performed"))
  ) %>% 
  mutate(SYPH_POSITIVE_EVER_PREG_CAT_LABEL = case_when(SYPH_DX_POSITIVE_ENROLL ==1 & SYPH_RDT_POSITIVE_ENROLL !=1 ~ "previous Dx at enroll",
                                                       SYPH_DX_POSITIVE_ENROLL !=1 & SYPH_RDT_POSITIVE_ENROLL ==1 ~ "RDT+ at enroll",
                                                       SYPH_DX_POSITIVE_ENROLL ==1 & SYPH_RDT_POSITIVE_ENROLL ==1 ~ "previous Dx & RDT+ at enroll",
                                                       SYPH_POSITIVE_INCIDENT ==1 ~ "incident RDT+",
                                                       SYPH_POSITIVE_UNKNOWN_BASELINE == 1 ~ "RDT+ with unknown baseline",
                                                       TRUE ~ NA),
         SYPH_POSITIVE_EVER_PREG_CAT = case_when(SYPH_DX_POSITIVE_ENROLL ==1 & SYPH_RDT_POSITIVE_ENROLL !=1 ~ 1,
                                                 SYPH_DX_POSITIVE_ENROLL !=1 & SYPH_RDT_POSITIVE_ENROLL ==1 ~ 2,
                                                 SYPH_DX_POSITIVE_ENROLL ==1 & SYPH_RDT_POSITIVE_ENROLL ==1 ~ 3,
                                                 SYPH_POSITIVE_INCIDENT ==1 ~ 4,
                                                 SYPH_POSITIVE_UNKNOWN_BASELINE == 1 ~ 5,
                                                 TRUE ~ NA))

syph_export_labels <- syph_labels %>% 
  select(SITE, MOMID, PREGID,
         SYPH_DATE_POSITIVE, SYPH_GESTAGE_POSITIVE_DAYS, SYPH_GESTAGE_POSITIVE_WKS,
         SYPH_DX_POSITIVE_ENROLL, SYPH_DX_POSITIVE_ENROLL_LABEL,
         SYPH_RDT_PERF_ENROLL, SYPH_RDT_POSITIVE_ENROLL, SYPH_RDT_POSITIVE_ENROLL_LABEL,
         SYPH_RDT_PERF_EVER_PREG, SYPH_POSITIVE_ENROLL, SYPH_POSITIVE_ENROLL_LABEL, 
         SYPH_POSITIVE_INCIDENT, SYPH_POSITIVE_INCIDENT_LABEL,
         SYPH_POSITIVE_UNKNOWN_BASELINE, SYPH_POSITIVE_UNKNOWN_BASELINE_LABEL,
         SYPH_POSITIVE_EVER_PREG, SYPH_POSITIVE_EVER_PREG_LABEL,
         SYPH_POSITIVE_EVER_PREG_CAT, SYPH_POSITIVE_EVER_PREG_CAT_LABEL
  )

# write.xlsx(syph_table, paste0(path_to_save, "syph_table" ,".xlsx"), na="", rowNames=TRUE)

#*****************************************************************************
# Malaria ----
#*****************************************************************************

malaria <- mat_enroll %>% 
  left_join(mnh04, by = c("SITE", "MOMID", "PREGID")) %>% 
  select(SITE, MOMID, PREGID,PREG_START_DATE, ENROLL_SCRN_DATE, TYPE_VISIT, VISIT_DATE, M04_MAT_VISIT_MNH04, M04_MALARIA_EVER_MHOCCUR) %>% 
  # merge in mnh06 to pull rdt results 
  left_join(mnh06 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE,M06_DIAG_VSDAT, M06_MALARIA_POC_LBPERF, M06_MALARIA_POC_LBORRES), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  # Asia sites stopped testing for malaria after the following dates: 
  mutate(MALARIA_TESTING = case_when(SITE == "Pakistan" & VISIT_DATE < "2024-05-01" ~ 1, 
                                     SITE == "India-CMC" & VISIT_DATE < "2024-06-13" ~ 1,
                                     SITE == "India-SAS" & VISIT_DATE < "2024-05-31" ~ 1,
                                     SITE %in% c("Zambia", "Kenya", "Ghana") ~ 1, 
                                     TRUE ~ 0 
                                     
  )) %>% 
  # Was test performed?
  mutate(MAL_RDT_PERF = case_when(M06_MALARIA_POC_LBPERF ==1 ~ 1, 
                                  M06_MALARIA_POC_LBPERF %in% c(NA, 0) ~ 0, 
                                  MALARIA_TESTING == 0 ~ 77, # 77, not applicable since no malaria testing 
                                  TRUE ~ 0)) %>% 
  # Test result available? 
  mutate(MAL_RDT_RESULT_AVAI = case_when(M06_MALARIA_POC_LBORRES %in% c(1,0) ~ 1, TRUE ~ 0)) %>% 
  # Test result by RDT 
  mutate(MAL_RDT_POSITIVE = case_when(MAL_RDT_PERF == 1 & MALARIA_TESTING == 1 &  M06_MALARIA_POC_LBORRES == 1 ~ 1, # 1, positive
                                      MAL_RDT_PERF == 1 & MALARIA_TESTING == 1 &  M06_MALARIA_POC_LBORRES == 0 ~ 0,  # 0, negative
                                      MAL_RDT_PERF == 1 & MALARIA_TESTING == 1 &  M06_MALARIA_POC_LBORRES %in% c(55,77,99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      MAL_RDT_PERF %in% c(0,55) | MALARIA_TESTING != 1 ~ 77,  #77, na if test was not performed 
                                      TRUE ~ NA)) %>% 
  # Test result by Dx 
  mutate(MAL_DX_POSITIVE = case_when(M04_MALARIA_EVER_MHOCCUR == 1 ~ 1, 
                                     M04_MALARIA_EVER_MHOCCUR == 0 ~ 0, 
                                     M04_MALARIA_EVER_MHOCCUR %in% c(55, 77, 99, 0, NA) ~ 77,
                                     TRUE ~ 77)) %>% 
  # Date positive test 
  mutate(DATE_POSITIVE = case_when(MAL_RDT_POSITIVE ==1 ~ ymd(M06_DIAG_VSDAT), 
                                   TRUE ~ NA_Date_)) %>% 
  # Test result by RDT+ or RDT+/Dx+
  # generate indicator variable to remove any unscheduled visits that do not have a test outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & MAL_RDT_PERF ==1 & MAL_RDT_RESULT_AVAI ==1 ~ 1,
                                  TYPE_VISIT %in% c(13,14) & (MAL_DX_POSITIVE ==1 | MAL_RDT_POSITIVE %in% c(1,0)) ~ 1,
                                  TYPE_VISIT %in% c(13,14) & (MAL_DX_POSITIVE %in% c(0,NA,55,77) |
                                                                MAL_RDT_POSITIVE %in% c(NA,55,77)) ~ 1,
                                  TYPE_VISIT %in% c(1,2,3,4,5) ~ 1,
                                  TRUE ~ 0)) %>% 
  filter(KEEP_UNSCHED==1)

mal_export <- malaria %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, PREG_START_DATE, VISIT_DATE,DATE_POSITIVE, starts_with("MAL_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("MAL_"), VISIT_DATE, DATE_POSITIVE),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(flag_gap = {
    vals <- c_across(starts_with("MAL_RDT_POSITIVE_"))
    ones <- which(vals == 1)
    any(vals == 0) &&
      length(ones) >= 2 &&
      any(diff(ones) > 1) &&
      any(vals[min(ones):max(ones)] == 0)
  }) %>%
  mutate(MAL_POSITIVE_REINFECT_DATE = {
    vals <- c_across(starts_with("MAL_RDT_POSITIVE_"))
    dates <- c_across(starts_with("DATE_POSITIVE_"))  # assumes same column order!
    
    ones <- which(vals == 1)
    reinf_date <- NA  # default return value
    
    if (length(ones) >= 2) {
      for (i in 2:length(ones)) {
        subseq <- vals[ones[i-1]:ones[i]]
        if (any(subseq == 0)) {
          reinf_date <- dates[ones[i]]  # store the "second positive" date
          break
        }
      }
    }
    reinf_date  # always return something
  }
  ) %>% 
  mutate(MAL_RDT_POSITIVE_ENROLL = MAL_RDT_POSITIVE_1,
         MAL_DX_POSITIVE_ENROLL = MAL_DX_POSITIVE_1,
         MAL_RDT_PERF_ENROLL = MAL_RDT_PERF_1,
         MAL_POSITIVE_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("MAL_RDT_POSITIVE_")) == 1) & MAL_RDT_POSITIVE_ENROLL %in% c(55,77,99,NA) ~ 1,
                                                   TRUE ~ 0),
         MAL_RDT_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("MAL_RDT_POSITIVE_")) == 1) ~ 1, # 1, Positive if either rdt is + 
                                                any(na.omit(c_across(starts_with("MAL_RDT_POSITIVE_"))) == 0) ~ 0, # 0, Negative ifany rdt is -
                                                all(na.omit(c_across(starts_with("MAL_RDT_POSITIVE_"))) ==55) ~ 55, # 55, Missing if both rdt and titers are missing
                                                all(na.omit(c_across(starts_with("MAL_RDT_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                TRUE ~ 99),
         
         MAL_DX_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("MAL_DX_POSITIVE_")) == 1) ~ 1, # 1, Positive if DX+
                                               # any(na.omit(c_across(starts_with("MAL_DX_POSITIVE_"))) == 0) ~ 0, # Dx alone does not give you a negative result
                                               all(na.omit(c_across(starts_with("MAL_DX_POSITIVE_"))) ==55) ~ 55, # 55, Missing if both rdt and titers are missing
                                               all(na.omit(c_across(starts_with("MAL_DX_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                               TRUE ~ 77),
         MAL_RDT_PERF_EVER_PREG = case_when(any(c_across(starts_with("MAL_RDT_PERF_")) == 1) ~ 1, # 1, Positive if test not performed 
                                            any(na.omit(c_across(starts_with("MAL_RDT_PERF_"))) == 0) ~ 0, # 0, Negative if test not performed 
                                            all(na.omit(c_across(starts_with("MAL_RDT_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                            all(na.omit(c_across(starts_with("MAL_RDT_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                            TRUE ~ 99),
         MAL_POSITIVE_REINFECT = case_when(flag_gap == TRUE ~ 1, # 1, Positive if test not performed 
                                           flag_gap == FALSE ~ 0, # 0, no re-infection
                                           TRUE ~ 99),
         MAL_POSITIVE_ENROLL = MAL_RDT_POSITIVE_ENROLL,
         MAL_POSITIVE_INCIDENT = case_when(MAL_RDT_POSITIVE_EVER_PREG ==1 & MAL_POSITIVE_ENROLL ==0 ~ 1,
                                           TRUE  ~ 0)
  ) %>% 
  mutate(MAL_POSITIVE_EVER_PREG = case_when(MAL_RDT_POSITIVE_EVER_PREG ==1 | MAL_POSITIVE_INCIDENT==1 | MAL_POSITIVE_ENROLL ==1 | MAL_POSITIVE_REINFECT ==1 ~ 1, ## EVER PREG variable is RDT+ ever during pregnancy, incident RDT+, or DX+ or RDT+ at enrollment
                                            MAL_RDT_POSITIVE_EVER_PREG ==0 & MAL_POSITIVE_ENROLL== 0 ~ 0,
                                            TRUE ~ 77)) %>% 
  
  # generate date and gestage at first infection
  mutate(MAL_POSITIVE_DATE = do.call(pmin, c(across(starts_with("DATE_POSITIVE_")), na.rm = TRUE)),
         MAL_GESTAGE_POSITIVE_DAYS = as.numeric(MAL_POSITIVE_DATE-ymd(PREG_START_DATE)),
         MAL_GESTAGE_POSITIVE_WKS = MAL_GESTAGE_POSITIVE_DAYS %/% 7
  ) %>% 
  # generate date and age second malaria  reinfection
  mutate(MAL_GESTAGE_POSITIVE_REINFECT_DAYS = as.numeric(MAL_POSITIVE_REINFECT_DATE-ymd(PREG_START_DATE)),
         MAL_GESTAGE_POSITIVE_REINFECT_WKS = MAL_GESTAGE_POSITIVE_REINFECT_DAYS %/% 7
  ) %>% 
  # generate variable of days between first infection and re-infection 
  mutate(MAL_TIME_BETWEEN_INFECTIONS_DAYS = as.numeric(MAL_POSITIVE_REINFECT_DATE-ymd(MAL_POSITIVE_DATE)),
         MAL_TIME_BETWEEN_INFECTIONS_WKS = MAL_TIME_BETWEEN_INFECTIONS_DAYS %/% 7)
table(mal_export$MAL_POSITIVE_INCIDENT)

## add labels
mal_labels <- mal_export %>%
  mutate(
    MAL_DX_POSITIVE_ENROLL_LABEL = factor(MAL_DX_POSITIVE_ENROLL, levels = c(1, 0, 77), 
                                          labels = c("Dx+ at enrollment", "Dx- at enrollment", "Missing Dx at enrollment")),
    MAL_DX_POSITIVE_EVER_PREG_LABEL = factor(MAL_DX_POSITIVE_EVER_PREG, levels = c(1, 77), 
                                             labels = c("Dx+ ever during pregnancy", "No Dx available or no Dx+ ever during pregnancy")),
    MAL_RDT_POSITIVE_ENROLL_LABEL = factor(MAL_RDT_POSITIVE_ENROLL,levels = c(1, 0, 55, 77), 
                                           labels = c("RDT+ at enrollment", "RDT- at enrollment", "Missing RDT at enrollment", "NA/no test performed")),
    MAL_RDT_POSITIVE_EVER_PREG_LABEL = factor(MAL_RDT_POSITIVE_EVER_PREG,levels = c(1, 0,55,77), 
                                              labels = c("RDT+ ever during pregnancy", "RDT- during pregnancy", "Missing RDT during pregnancy", "NA/no test performed")),
    MAL_POSITIVE_ENROLL_LABEL = factor(MAL_POSITIVE_ENROLL,levels = c(1, 0, 55, 77), 
                                       labels = c("Malaria positive at enrollment", "Malaria negative at enrollment", "Missing RDT at enrollment", "NA/no test performed")),
    MAL_POSITIVE_INCIDENT_LABEL = factor(MAL_POSITIVE_INCIDENT,levels = c(1, 0), 
                                         labels = c("Incident malaria RDT+ during pregnancy", "No incident malaria infection")),
    MAL_POSITIVE_EVER_PREG_LABEL = factor(MAL_POSITIVE_EVER_PREG,levels = c(1, 0, 77), 
                                          labels = c("Malaria positive ever during pregnancy", "Malaria never during pregnancy", "NA/no test performed")),
    MAL_POSITIVE_REINFECT_LABEL = factor(MAL_POSITIVE_REINFECT,levels = c(1, 0), 
                                         labels = c("Malaria re-infection during pregnancy", "No malaria re-infection during pregnancy")) 
  ) %>% 
  mutate(MAL_POSITIVE_EVER_PREG_CAT_LABEL = case_when(MAL_RDT_POSITIVE_ENROLL ==1 & MAL_DX_POSITIVE_ENROLL == 1 & MAL_POSITIVE_REINFECT == 0 ~ "Previous Dx+ and RDT+ at enroll",
                                                      MAL_RDT_POSITIVE_ENROLL ==1 & MAL_POSITIVE_REINFECT ==0 ~ "RDT+ at enroll",
                                                      MAL_POSITIVE_REINFECT ==1 ~ "Malaria re-infection (RDT+)",
                                                      MAL_POSITIVE_EVER_PREG ==1 ~ "RDT+ ever during pregnancy",
                                                      TRUE ~ NA),
         MAL_POSITIVE_EVER_PREG_CAT = case_when(MAL_RDT_POSITIVE_ENROLL ==1 & MAL_DX_POSITIVE_ENROLL == 1 & MAL_POSITIVE_REINFECT == 0 ~ 1,
                                                MAL_RDT_POSITIVE_ENROLL ==1 & MAL_POSITIVE_REINFECT ==0 ~ 2,
                                                MAL_POSITIVE_REINFECT ==1 ~ 3,
                                                MAL_POSITIVE_EVER_PREG ==1 ~ 4,
                                                TRUE ~ NA))

table(mal_labels$MAL_POSITIVE_EVER_PREG_CAT_LABEL, mal_labels$MAL_POSITIVE_EVER_PREG, useNA = "ifany")
## figure out how to do dates 
mal_export_labels <- mal_labels %>% 
  select(SITE, MOMID, PREGID,
         MAL_POSITIVE_DATE, MAL_POSITIVE_REINFECT_DATE, MAL_GESTAGE_POSITIVE_DAYS, MAL_GESTAGE_POSITIVE_WKS,
         MAL_GESTAGE_POSITIVE_REINFECT_DAYS, MAL_GESTAGE_POSITIVE_REINFECT_WKS,
         MAL_TIME_BETWEEN_INFECTIONS_DAYS, MAL_TIME_BETWEEN_INFECTIONS_WKS,
         MAL_DX_POSITIVE_ENROLL, MAL_DX_POSITIVE_ENROLL_LABEL,
         MAL_RDT_PERF_ENROLL, MAL_RDT_POSITIVE_ENROLL, MAL_RDT_POSITIVE_ENROLL_LABEL,
         MAL_RDT_PERF_EVER_PREG, MAL_RDT_POSITIVE_EVER_PREG, MAL_RDT_POSITIVE_EVER_PREG_LABEL,
         MAL_POSITIVE_ENROLL, MAL_POSITIVE_ENROLL_LABEL, MAL_POSITIVE_EVER_PREG, MAL_POSITIVE_EVER_PREG_LABEL,
         MAL_POSITIVE_REINFECT, MAL_POSITIVE_REINFECT_LABEL,
         MAL_POSITIVE_INCIDENT, MAL_POSITIVE_INCIDENT_LABEL,
         MAL_POSITIVE_EVER_PREG_CAT, MAL_POSITIVE_EVER_PREG_CAT_LABEL, MAL_POSITIVE_UNKNOWN_BASELINE)

ggplot() + 
  geom_histogram(data = mal_export_labels %>%
                   filter(MAL_POSITIVE_REINFECT==1), aes(x= MAL_TIME_BETWEEN_INFECTIONS_WKS), stat = "count") + 
  xlab("time between malaria re-infection in pregnancy (weeks)") + 
  scale_x_continuous(breaks = seq(1, 30, by = 2),
                     label = as.character(seq(1, 30, by = 2))) +
  theme_bw()

#*****************************************************************************
# Hep B ----
#*****************************************************************************

hbv <- mat_enroll %>% 
  # merge in mnh06 to pull rdt results 
  left_join(mnh06 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, M06_DIAG_VSDAT,
                             VISIT_DATE,M06_HBV_POC_LBPERF, M06_HBV_POC_LBORRES), 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  # Was test performed?
  mutate(HBV_RDT_PERF = case_when(M06_HBV_POC_LBPERF ==1 ~ 1, 
                                  is.na(M06_HBV_POC_LBPERF) ~ 0, 
                                  TRUE ~ 0)) %>% 
  # Test result available? 
  mutate(HBV_RDT_RESULT_AVAI = case_when(M06_HBV_POC_LBORRES %in% c(1,0) ~ 1, TRUE ~ 0)) %>% 
  # Test result by RDT
  mutate(HBV_RDT_POSITIVE = case_when(HBV_RDT_PERF == 1 & M06_HBV_POC_LBORRES == 1 ~ 1, # 1, positive
                                      HBV_RDT_PERF == 1 & M06_HBV_POC_LBORRES == 0 ~ 0,  # 0, negative
                                      HBV_RDT_PERF == 1 & M06_HBV_POC_LBORRES %in% c(55,77,99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      HBV_RDT_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                      TRUE ~ 77)) %>% 
  # Date positive test 
  mutate(DATE_POSITIVE = case_when(HBV_RDT_POSITIVE ==1 ~ ymd(M06_DIAG_VSDAT), 
                                   TRUE ~ NA_Date_)) %>% 
  # generate indicator variable to remove any unscheduled visits that do not have a test outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & HBV_RDT_PERF ==1 & HBV_RDT_RESULT_AVAI ==1 ~ 1, # keep if test was done and result is available
                                  TYPE_VISIT %in% c(13,14) & HBV_RDT_POSITIVE %in% c(1,0) ~ 1, ## keep if valid results (could probably remove)
                                  TYPE_VISIT %in% c(1,2,3,4,5) ~ 1, # keep if not an unscheduled visit 
                                  TYPE_VISIT %in% c(13,14) & HBV_RDT_POSITIVE %in% c(NA,55,77) ~ 0, # remove if missing 
                                  TRUE ~ 0)) %>% 
  filter(KEEP_UNSCHED==1)

hbv_export_anc <- hbv %>% 
  filter(!TYPE_VISIT %in% c(6,7,8,9,10,11,12,14)) %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ,PREG_START_DATE, VISIT_DATE,DATE_POSITIVE, starts_with("HBV_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("HBV_"), VISIT_DATE, DATE_POSITIVE),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(HBV_POSITIVE_ENROLL = HBV_RDT_POSITIVE_1,
         HBV_RDT_PERF_ENROLL = HBV_RDT_PERF_1,
         HBV_POSITIVE_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("HBV_RDT_POSITIVE_")) == 1) & HBV_POSITIVE_ENROLL %in% c(55,77,99,NA) ~ 1,
                                                   TRUE ~ 0),
         
         HBV_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("HBV_RDT_POSITIVE_")) == 1) ~ 1, # 1, Positive if  rdt is +
                                            any(na.omit(c_across(starts_with("HBV_RDT_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any rdt is -
                                            all(na.omit(c_across(starts_with("HBV_RDT_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                            TRUE ~ 77),
         HBV_POSITIVE_INCIDENT = case_when(HBV_POSITIVE_EVER_PREG ==1 & HBV_POSITIVE_ENROLL ==0 ~ 1,
                                           TRUE  ~ 0),
         HBV_RDT_PERF_EVER_PREG = case_when(any(c_across(starts_with("HBV_RDT_PERF_")) == 1) ~ 1, # 1, RDT performed
                                            all(na.omit(c_across(starts_with("HBV_RDT_PERF_"))) %in% c(0,55,77,99,NA))  ~ 0, # 77, NA no test performed
                                            TRUE ~ 0)
  ) %>% 
  # generate date and gestage at positive RDT infection
  mutate(HBV_POSITIVE_DATE = do.call(pmin, c(across(starts_with("DATE_POSITIVE_")), na.rm = TRUE)),
         HBV_GESTAGE_POSITIVE_DAYS = as.numeric(HBV_POSITIVE_DATE-ymd(PREG_START_DATE)),
         HBV_GESTAGE_POSITIVE_WKS = HBV_GESTAGE_POSITIVE_DAYS %/% 7
  )



## add labels
hbv_labels <- hbv_export_anc %>%
  mutate(
    HBV_POSITIVE_ENROLL_LABEL = factor(HBV_POSITIVE_ENROLL,levels = c(1, 0, 55, 77), 
                                       labels = c("HBV RDT+ at enrollment", "HBV RDT- at enrollment", "Missing HBV RDT at enrollment", "NA/no test performed")),
    HBV_POSITIVE_UNKNOWN_BASELINE_LABEL =factor(HBV_POSITIVE_UNKNOWN_BASELINE,levels = c(1, 0), 
                                                labels = c("RDT+ during pregnancy but missing baseline status", "No incident HBV with unknown baseline status")),
    HBV_POSITIVE_INCIDENT_LABEL = factor(HBV_POSITIVE_INCIDENT,levels = c(1, 0), 
                                         labels = c("Incident HBV RDT+ during pregnancy", "No incident HBV infection")),
    
    HBV_POSITIVE_EVER_PREG_LABEL = factor(HBV_POSITIVE_EVER_PREG,levels = c(1, 0, 77), 
                                          labels = c("HBV positive ever during pregnancy", "HBV never during pregnancy", "NA/no test performed"))
  ) %>% 
  mutate(HBV_POSITIVE_EVER_PREG_CAT_LABEL = case_when(HBV_POSITIVE_ENROLL ==1 ~ "RDT+ at enroll",
                                                      HBV_POSITIVE_INCIDENT ==1 ~ "incident RDT+",
                                                      HBV_POSITIVE_UNKNOWN_BASELINE==1 ~ "RDT+ with unknown baseline",
                                                      TRUE ~ NA),
         HBV_POSITIVE_EVER_PREG_CAT = case_when(HBV_POSITIVE_ENROLL ==1 ~ 1,
                                                HBV_POSITIVE_INCIDENT ==1 ~ 2,
                                                HBV_POSITIVE_UNKNOWN_BASELINE==1 ~ 3,
                                                TRUE ~ NA))

## figure out how to do dates 
hbv_export_labels <- hbv_labels %>% 
  select(SITE, MOMID, PREGID,
         HBV_POSITIVE_DATE, HBV_GESTAGE_POSITIVE_DAYS, HBV_GESTAGE_POSITIVE_WKS,
         HBV_POSITIVE_ENROLL, HBV_POSITIVE_ENROLL_LABEL, HBV_RDT_PERF_ENROLL,
         HBV_POSITIVE_EVER_PREG,HBV_POSITIVE_EVER_PREG_LABEL, HBV_RDT_PERF_EVER_PREG, 
         HBV_POSITIVE_UNKNOWN_BASELINE, HBV_POSITIVE_UNKNOWN_BASELINE_LABEL,
         HBV_POSITIVE_INCIDENT, HBV_POSITIVE_INCIDENT_LABEL, 
         HBV_POSITIVE_EVER_PREG_CAT, HBV_POSITIVE_EVER_PREG_CAT_LABEL)

# write.xlsx(hbv_table, paste0(path_to_save, "hbv_table" ,".xlsx"), na="", rowNames=TRUE)
#*****************************************************************************
# Hep C ----
#*****************************************************************************

hcv <- mat_enroll %>% 
  # merge in mnh06 to pull rdt results 
  left_join(mnh06 %>% select(SITE, MOMID, PREGID, TYPE_VISIT,  VISIT_DATE,
                             M06_DIAG_VSDAT, M06_HCV_POC_LBPERF, M06_HCV_POC_LBORRES), 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  # Was test performed?
  mutate(HCV_RDT_PERF = case_when(M06_HCV_POC_LBPERF ==1 ~ 1, 
                                  # is.na(M06_HCV_POC_LBPERF) ~ 0, 
                                  TRUE ~ 0)) %>% 
  # Test result available? 
  mutate(HCV_RDT_RESULT_AVAI = case_when(M06_HCV_POC_LBORRES %in% c(1,0) ~ 1, TRUE ~ 0)) %>% 
  # Test result by RDT
  mutate(HCV_RDT_POSITIVE = case_when(HCV_RDT_PERF == 1 & M06_HCV_POC_LBORRES == 1 ~ 1, # 1, positive
                                      HCV_RDT_PERF == 1 & M06_HCV_POC_LBORRES == 0 ~ 0,  # 0, negative
                                      HCV_RDT_PERF == 1 & M06_HCV_POC_LBORRES %in% c(55,77,99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      HCV_RDT_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                      TRUE ~ NA)) %>% 
  # Date positive test 
  mutate(DATE_POSITIVE = case_when(HCV_RDT_POSITIVE ==1 ~ ymd(M06_DIAG_VSDAT), 
                                   TRUE ~ NA_Date_)) %>% 
  # generate indicator variable to remove any unscheduled visits that do not have a test outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & HCV_RDT_PERF ==1 & HCV_RDT_RESULT_AVAI ==1 ~ 1, # keep if test was done and result is available
                                  TYPE_VISIT %in% c(13,14) & HCV_RDT_POSITIVE %in% c(1,0) ~ 1, ## keep if valid results (could probably remove)
                                  TYPE_VISIT %in% c(1,2,3,4,5) ~ 1, # keep if not an unscheduled visit 
                                  TYPE_VISIT %in% c(13,14) & HCV_RDT_POSITIVE %in% c(NA,55,77) ~ 0, # remove if missing 
                                  TRUE ~ 0)) %>% 
  filter(KEEP_UNSCHED==1)

hcv_export_anc <- hcv %>% 
  filter(!TYPE_VISIT %in% c(6,7,8,9,10,11,12,14)) %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, VISIT_DATE,PREG_START_DATE, DATE_POSITIVE, starts_with("HCV_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("HCV_"), DATE_POSITIVE, VISIT_DATE),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(HCV_POSITIVE_ENROLL = HCV_RDT_POSITIVE_1,
         HCV_RDT_PERF_ENROLL = HCV_RDT_PERF_1,
         HCV_POSITIVE_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("HCV_RDT_POSITIVE_")) == 1) & HCV_POSITIVE_ENROLL %in% c(55,77,99,NA) ~ 1,
                                                   TRUE ~ 0),
         
         HCV_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("HCV_RDT_POSITIVE_")) == 1) ~ 1, # 1, Positive if  rdt is +
                                            any(na.omit(c_across(starts_with("HCV_RDT_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any rdt is -
                                            all(na.omit(c_across(starts_with("HCV_RDT_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                            TRUE ~ 77),
         HCV_POSITIVE_INCIDENT = case_when(HCV_POSITIVE_EVER_PREG ==1 & HCV_POSITIVE_ENROLL ==0 ~ 1,
                                           TRUE  ~ 0),
         HCV_RDT_PERF_EVER_PREG = case_when(any(c_across(starts_with("HCV_RDT_PERF_")) == 1) ~ 1, # 1, RDT performed
                                            all(na.omit(c_across(starts_with("HCV_RDT_PERF_"))) %in% c(0,55,77,99,NA))  ~ 0, # 77, NA no test performed
                                            TRUE ~ 0)
  ) %>% 
  # generate date and gestage at positive RDT infection
  mutate(HCV_POSITIVE_DATE = do.call(pmin, c(across(starts_with("DATE_POSITIVE_")), na.rm = TRUE)),
         HCV_GESTAGE_POSITIVE_DAYS = as.numeric(HCV_POSITIVE_DATE-ymd(PREG_START_DATE)),
         HCV_GESTAGE_POSITIVE_WKS = HCV_GESTAGE_POSITIVE_DAYS %/% 7
  )



## add labels
hcv_labels <- hcv_export_anc %>%
  mutate(
    HCV_POSITIVE_ENROLL_LABEL = factor(HCV_POSITIVE_ENROLL,levels = c(1, 0, 55, 77), 
                                       labels = c("HCV RDT+ at enrollment", "HCV RDT- at enrollment", "Missing HCV RDT at enrollment", "NA/no test performed")),
    HCV_POSITIVE_UNKNOWN_BASELINE_LABEL =factor(HCV_POSITIVE_UNKNOWN_BASELINE,levels = c(1, 0), 
                                                labels = c("RDT+ during pregnancy but missing baseline status", "No incident HCV with unknown baseline status")),
    HCV_POSITIVE_INCIDENT_LABEL = factor(HCV_POSITIVE_INCIDENT,levels = c(1, 0), 
                                         labels = c("Incident HCV RDT+ during pregnancy", "No incident HCV infection")),
    
    HCV_POSITIVE_EVER_PREG_LABEL = factor(HCV_POSITIVE_EVER_PREG,levels = c(1, 0, 77), 
                                          labels = c("HCV positive ever during pregnancy", "HCV never during pregnancy", "NA/no test performed"))
  ) %>% 
  mutate(HCV_POSITIVE_EVER_PREG_CAT_LABEL = case_when(HCV_POSITIVE_ENROLL ==1 ~ "RDT+ at enroll",
                                                      HCV_POSITIVE_INCIDENT ==1 ~ "incident RDT+",
                                                      HCV_POSITIVE_UNKNOWN_BASELINE==1 ~ "RDT+ with unknown baseline",
                                                      TRUE ~ NA),
         HCV_POSITIVE_EVER_PREG_CAT = case_when(HCV_POSITIVE_ENROLL ==1 ~ 1,
                                                HCV_POSITIVE_INCIDENT ==1 ~ 2,
                                                HCV_POSITIVE_UNKNOWN_BASELINE==1 ~ 3,
                                                TRUE ~ NA))

## figure out how to do dates 
hcv_export_labels <- hcv_labels %>% 
  select(SITE, MOMID, PREGID,
         HCV_POSITIVE_DATE, HCV_GESTAGE_POSITIVE_DAYS, HCV_GESTAGE_POSITIVE_WKS,
         HCV_POSITIVE_ENROLL, HCV_POSITIVE_ENROLL_LABEL, HCV_RDT_PERF_ENROLL,
         HCV_POSITIVE_EVER_PREG,HCV_POSITIVE_EVER_PREG_LABEL, HCV_RDT_PERF_EVER_PREG, 
         HCV_POSITIVE_UNKNOWN_BASELINE, HCV_POSITIVE_UNKNOWN_BASELINE_LABEL,
         HCV_POSITIVE_INCIDENT, HCV_POSITIVE_INCIDENT_LABEL, 
         HCV_POSITIVE_EVER_PREG_CAT, HCV_POSITIVE_EVER_PREG_CAT_LABEL)



# write.xlsx(hcv_table, paste0(path_to_save, "hcv_table" ,".xlsx"), na="", rowNames=TRUE)
#*****************************************************************************
# Hep E ----
# Hep E is part of PRISMA expansion (create a proxy variable below for expected test) 
#*****************************************************************************

hev <- mat_enroll %>% 
  # merge in mnh08 to pull titer results 
  left_join(mnh08 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, HEV_EXPANSION, M08_LBSTDAT,
                             M08_HEV_LBPERF_1, M08_HEV_IGM_LBORRES, M08_HEV_LBPERF_2, M08_HEV_IGG_LBORRES, M08_HEV_LBTSTDAT), 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  # merge in mnh07 for specimen collection date
  left_join(mnh07 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, M07_MAT_SPEC_COLLECT_DAT), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  filter(HEV_EXPANSION ==1)%>%   # Was test performed?
  mutate(HEV_IGM_PERF = case_when(M08_HEV_LBPERF_1 ==1 ~ 1,
                                  is.na(M08_HEV_LBPERF_1) ~ 0,
                                  TRUE ~ 0),
         HEV_IGG_PERF = case_when(M08_HEV_LBPERF_2 ==1 ~ 1,
                                  is.na(M08_HEV_LBPERF_2) ~ 0,
                                  TRUE ~ 0)
  ) %>%
  # Test result available? 
  mutate(HEV_IGM_RESULT_AVAI = case_when(M08_HEV_IGG_LBORRES %in% c(0,1,2) ~ 1, TRUE ~ 0),
         HEV_IGG_RESULT_AVAI = case_when(M08_HEV_IGG_LBORRES %in% c(0,1,2) ~ 1, TRUE ~ 0)
  ) %>% 
  # Test result by titers
  mutate(HEV_IGM_POSITIVE = case_when(HEV_EXPANSION==0 ~ 77, # 77, na; visit was before expansion date
                                      HEV_IGM_PERF ==1 & HEV_EXPANSION == 1 & M08_HEV_IGM_LBORRES == 1 ~ 1, # 1, positive
                                      HEV_IGM_PERF ==1 & HEV_EXPANSION == 1 & M08_HEV_IGM_LBORRES == 0 ~ 0, # 0, negative
                                      HEV_IGM_PERF ==1 & HEV_EXPANSION == 1 & M08_HEV_IGM_LBORRES == 2 ~ 2, # 2, inconclusive
                                      HEV_IGM_PERF ==1 & HEV_EXPANSION == 1 & M08_HEV_IGM_LBORRES %in% c(NA,55, 77, 99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      HEV_EXPANSION ==1 & HEV_IGM_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                      TRUE ~ NA),
         HEV_IGG_POSITIVE = case_when(HEV_EXPANSION==0 ~ 77, # 77, na; visit was before expansion date
                                      HEV_IGG_PERF ==1 & HEV_EXPANSION == 1 & M08_HEV_IGG_LBORRES == 1 ~ 1, # 1, positive
                                      HEV_IGG_PERF ==1 & HEV_EXPANSION == 1 & M08_HEV_IGG_LBORRES == 0 ~ 0, # 0, negative
                                      HEV_IGG_PERF ==1 & HEV_EXPANSION == 1 & M08_HEV_IGG_LBORRES == 2 ~ 2, # 2, inconclusive
                                      HEV_IGG_PERF ==1 & HEV_EXPANSION == 1 & M08_HEV_IGG_LBORRES %in% c(NA,55, 77, 99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      HEV_EXPANSION ==1 & HEV_IGG_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                      TRUE ~ NA)) %>% 
  # Date positive test 
  mutate(DATE_IGM_POSITIVE = case_when(HEV_IGM_POSITIVE ==1 ~ ymd(M08_LBSTDAT), 
                                       TRUE ~ NA_Date_),
         DATE_IGG_POSITIVE = case_when(HEV_IGG_POSITIVE ==1 ~ ymd(M08_LBSTDAT), 
                                       TRUE ~ NA_Date_),
         
  ) %>% 
  
  # generate indicator variable to remove any unscheduled visits that do not have a test outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & (HEV_IGM_PERF ==1 | HEV_IGG_PERF ==1) & (HEV_IGM_RESULT_AVAI ==1 | HEV_IGG_RESULT_AVAI ==1) ~ 1, # keep if test was done and result is available
                                  TYPE_VISIT %in% c(13,14) & (HEV_IGM_POSITIVE %in% c(1,0,2) | HEV_IGG_POSITIVE %in% c(1,0,2)) ~ 1, ## keep if valid results (could probably remove)
                                  TYPE_VISIT %in% c(1,2,3,4,5) ~ 1, # keep if not an unscheduled visit 
                                  TYPE_VISIT %in% c(13,14) & HEV_IGM_POSITIVE %in% c(NA,55,77) & HEV_IGG_POSITIVE %in% c(NA,55,77) ~ 0, # remove if missing 
                                  TRUE ~ 0)) %>% 
  filter(KEEP_UNSCHED==1)

hev_export_anc <- hev %>% 
  filter(!TYPE_VISIT %in% c(6,7,8,9,10,11,12,14)) %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, PREG_START_DATE, VISIT_DATE, DATE_IGG_POSITIVE, DATE_IGM_POSITIVE,
         HEV_EXPANSION,M08_LBSTDAT, contains("_IGG_"), contains("_IGM_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(contains("_IGG_"), contains("_IGM_"), VISIT_DATE, DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, M08_LBSTDAT),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(HEV_IGM_POSITIVE_ENROLL = HEV_IGM_POSITIVE_1,
         HEV_IGG_POSITIVE_ENROLL = HEV_IGG_POSITIVE_1,
         HEV_IGM_PERF_ENROLL = HEV_IGM_PERF_1,
         HEV_IGM_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("HEV_IGM_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                                any(na.omit(c_across(starts_with("HEV_IGM_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                                all(na.omit(c_across(starts_with("HEV_IGM_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                                all(na.omit(c_across(starts_with("HEV_IGM_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                TRUE ~ 77),
         HEV_IGM_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("HEV_IGM_POSITIVE_")) == 1) & HEV_IGM_POSITIVE_ENROLL %in% c(2, 55,77,99,NA)~ 1, # 1, Positive if  titers is + 
                                              TRUE ~ 0),
         HEV_IGM_INCIDENT = case_when(HEV_IGM_POSITIVE_EVER_PREG ==1 & HEV_IGM_POSITIVE_ENROLL ==0 ~ 1, 
                                      TRUE  ~ 0),
         HEV_IGM_PERF_EVER_PREG = case_when(any(c_across(starts_with("HEV_IGM_PERF_")) == 1) ~ 1, # 1, titers performed
                                            any(na.omit(c_across(starts_with("HEV_IGM_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                            all(na.omit(c_across(starts_with("HEV_IGM_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                            all(na.omit(c_across(starts_with("HEV_IGM_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                            TRUE ~ 77),
         HEV_IGM_POSITIVE_EVER_PREG_TEXT = case_when(HEV_IGM_POSITIVE_ENROLL ==1 & HEV_IGM_INCIDENT!=1 & HEV_IGM_UNKNOWN_BASELINE!=1 ~ "Baseline IgM+", 
                                                     HEV_IGM_POSITIVE_ENROLL !=1 & HEV_IGM_INCIDENT==1 & HEV_IGM_UNKNOWN_BASELINE!=1 ~ "Incident IgM+", 
                                                     HEV_IGM_POSITIVE_ENROLL !=1 & HEV_IGM_INCIDENT!=1 & HEV_IGM_UNKNOWN_BASELINE==1 ~ "IgM+ with unknown baseline",
                                                     TRUE ~ NA),
         HEV_IGG_PERF_ENROLL = HEV_IGG_PERF_1,
         HEV_IGG_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("HEV_IGG_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                                any(na.omit(c_across(starts_with("HEV_IGG_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                                all(na.omit(c_across(starts_with("HEV_IGG_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                                all(na.omit(c_across(starts_with("HEV_IGG_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                TRUE ~ 77),
         HEV_IGG_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("HEV_IGG_POSITIVE_")) == 1) & HEV_IGG_POSITIVE_ENROLL %in% c(2, 55,77,99,NA) & HEV_IGM_POSITIVE_ENROLL %in% c(2, 55,77,99,NA) ~ 1, # 1, Positive if  titers is + 
                                              TRUE ~ 0),
         HEV_IGG_INCIDENT = case_when(HEV_IGG_POSITIVE_EVER_PREG ==1 & HEV_IGG_POSITIVE_ENROLL ==0 & HEV_IGM_POSITIVE_ENROLL ==0 ~ 1, 
                                      TRUE  ~ 0),
         HEV_IGG_POSITIVE_EVER_PREG_TEXT = case_when(HEV_IGG_POSITIVE_ENROLL ==1 & HEV_IGG_INCIDENT!=1 & HEV_IGG_UNKNOWN_BASELINE!=1 ~ "Baseline IgG+", 
                                                     HEV_IGG_POSITIVE_ENROLL !=1 & HEV_IGG_INCIDENT==1 & HEV_IGG_UNKNOWN_BASELINE!=1 ~ "Incident IgG+", 
                                                     HEV_IGG_POSITIVE_ENROLL !=1 & HEV_IGG_INCIDENT!=1 & HEV_IGG_UNKNOWN_BASELINE==1 ~ "IgG+ with unknown baseline",
                                                     TRUE ~ NA),
         HEV_IGG_PERF_EVER_PREG = case_when(any(c_across(starts_with("HEV_IGG_PERF_")) == 1) ~ 1, # 1, titers performed
                                            any(na.omit(c_across(starts_with("HEV_IGG_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                            all(na.omit(c_across(starts_with("HEV_IGG_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                            all(na.omit(c_across(starts_with("HEV_IGG_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                            TRUE ~ 77)
         
         
  ) %>% 
  mutate(HEV_POSITIVE_EVER_PREG = case_when(HEV_IGM_POSITIVE_EVER_PREG ==1 | HEV_IGG_INCIDENT==1 ~ 1, ## EVER PREG variable is HEVa IgM+ ever during pregnancy or incident IgG
                                            HEV_IGM_POSITIVE_EVER_PREG ==0 & HEV_IGG_INCIDENT== 0 ~ 0, 
                                            HEV_IGM_PERF_EVER_PREG==0 & HEV_IGG_PERF_EVER_PREG==0 ~ 77, # ## if test performed but no infection, these are NOs
                                            TRUE ~ 77) 
         
  ) %>% 
  mutate(HEV_DATE_IGM_POSITIVE = do.call(pmin, c(across(starts_with("DATE_IGM_POSITIVE_")), na.rm = TRUE)),
         HEV_GESTAGE_IGM_POSITIVE_DAYS = as.numeric(HEV_DATE_IGM_POSITIVE-ymd(PREG_START_DATE)),
         HEV_GESTAGE_IGM_POSITIVE_WKS = HEV_GESTAGE_IGM_POSITIVE_DAYS %/% 7
  ) %>%
  mutate(HEV_DATE_IGG_POSITIVE = do.call(pmin, c(across(starts_with("DATE_IGG_POSITIVE_")), na.rm = TRUE)),
         HEV_GESTAGE_IGG_POSITIVE_DAYS = as.numeric(HEV_DATE_IGG_POSITIVE-ymd(PREG_START_DATE)),
         HEV_GESTAGE_IGG_POSITIVE_WKS = HEV_GESTAGE_IGG_POSITIVE_DAYS %/% 7
  ) %>% 
  ## generate indicator if incident IGG
  mutate(HEV_INCIDENT_IGG_POSITIVE = case_when(HEV_IGG_POSITIVE_ENROLL==0 & HEV_IGG_POSITIVE_EVER_PREG==1 ~ 1, TRUE ~ 0),
         HEV_IGG_DAYS_BETWEEN_TESTS = case_when(HEV_INCIDENT_IGG_POSITIVE==1 ~ as.numeric(ymd(HEV_DATE_IGG_POSITIVE) - ymd(M08_LBSTDAT_1))),
         HEV_IGG_WKS_BETWEEN_TESTS = HEV_IGG_DAYS_BETWEEN_TESTS %/% 7
  )

## add labels
hev_labels <- hev_export_anc %>%
  mutate(
    HEV_IGM_POSITIVE_ENROLL_LABEL = factor(HEV_IGM_POSITIVE_ENROLL,levels = c(1, 0, 2, 55, 77),
                                           labels = c("HEV IgM+ at enrollment", "HEV IgM- at enrollment", "HEV IgM inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    HEV_IGG_POSITIVE_ENROLL_LABEL = factor(HEV_IGG_POSITIVE_ENROLL,levels = c(1, 0, 2, 55, 77),
                                           labels = c("HEV IgG+ at enrollment", "HEV IgG- at enrollment", "HEV IgG inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    HEV_IGM_UNKNOWN_BASELINE_LABEL = factor(HEV_IGM_UNKNOWN_BASELINE,levels = c(1, 0),
                                            labels = c("HEV IgM+ during pregnancy but missing baseline status", "No incident HEVa infection with unknown baseline status")),
    HEV_IGG_UNKNOWN_BASELINE_LABEL = factor(HEV_IGG_UNKNOWN_BASELINE,levels = c(1, 0),
                                            labels = c("HEV IgG+ during pregnancy but missing baseline status", "No incident HEVa infection with unknown baseline status")),
    HEV_IGM_INCIDENT_LABEL = factor(HEV_IGM_INCIDENT,levels = c(1, 0),
                                    labels = c("Incident HEV IgM+ during pregnancy", "No incident HEVa infection")),
    HEV_IGG_INCIDENT_LABEL = factor(HEV_IGG_INCIDENT,levels = c(1, 0),
                                    labels = c("Incident HEV IgG+ during pregnancy", "No incident HEVa IgG+ infection")),
    
    HEV_IGM_POSITIVE_EVER_PREG_LABEL = factor(HEV_IGM_POSITIVE_EVER_PREG,levels = c(1, 0, 2, 55, 77),
                                              labels = c("HEV IgM+ ever during pregnancy", "HEV IgM never during pregnancy", "HEV IgM inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    HEV_IGG_POSITIVE_EVER_PREG_LABEL = factor(HEV_IGG_POSITIVE_EVER_PREG,levels = c(1, 0, 2, 55, 77),
                                              labels = c("HEV IgG+ ever during pregnancy", "HEV IgG never during pregnancy", "HEV IgG inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    HEV_POSITIVE_EVER_PREG_LABEL = factor(HEV_POSITIVE_EVER_PREG,levels = c(1, 0, 77),
                                          labels = c("HEVa ever during pregnancy", "HEVa never during pregnancy", "NA/no test performed"))
  ) %>% 
  mutate(HEV_POSITIVE_EVER_PREG_CAT_LABEL = case_when(HEV_IGM_POSITIVE_ENROLL ==1 & HEV_IGM_INCIDENT!=1 & HEV_IGM_UNKNOWN_BASELINE!=1 & HEV_IGG_POSITIVE_ENROLL != 1 ~ "Baseline IgM+", 
                                                      HEV_IGG_POSITIVE_ENROLL ==1 & HEV_IGG_INCIDENT!=1 & HEV_IGG_UNKNOWN_BASELINE!=1 & HEV_IGM_POSITIVE_ENROLL != 1 ~ "Baseline IgG+",
                                                      HEV_IGM_POSITIVE_ENROLL ==1 & HEV_IGM_INCIDENT!=1 & HEV_IGM_UNKNOWN_BASELINE!=1 & HEV_IGG_POSITIVE_ENROLL == 1 ~ "Baseline IgM+ & IgG+", 
                                                      HEV_IGM_POSITIVE_ENROLL !=1 & HEV_IGM_INCIDENT==1 & HEV_IGM_UNKNOWN_BASELINE!=1 & HEV_IGG_INCIDENT != 1 ~ "Incident IgM+", 
                                                      HEV_IGG_POSITIVE_ENROLL !=1 & HEV_IGG_INCIDENT==1 & HEV_IGG_UNKNOWN_BASELINE!=1 & HEV_IGM_INCIDENT != 1 ~ "Incident IgG+", 
                                                      HEV_IGM_POSITIVE_ENROLL !=1 & HEV_IGM_INCIDENT==1 & HEV_IGM_UNKNOWN_BASELINE!=1 & HEV_IGG_INCIDENT == 1 ~ "Incident IgM+ & IgG+", 
                                                      HEV_IGM_POSITIVE_ENROLL !=1 & HEV_IGM_INCIDENT!=1 & HEV_IGM_UNKNOWN_BASELINE==1 & HEV_IGG_UNKNOWN_BASELINE != 1 ~ "IgM+ with unknown baseline",
                                                      HEV_IGG_POSITIVE_ENROLL !=1 & HEV_IGG_INCIDENT!=1 & HEV_IGG_UNKNOWN_BASELINE==1 & HEV_IGM_UNKNOWN_BASELINE != 1 ~ "IgG+ with unknown baseline",
                                                      HEV_IGM_POSITIVE_ENROLL !=1 & HEV_IGM_INCIDENT!=1 & HEV_IGM_UNKNOWN_BASELINE==1 & HEV_IGG_UNKNOWN_BASELINE == 1 ~ "IgM+ & IgG+ with unknown baseline",
                                                      TRUE ~ NA
  )) %>% 
  mutate(HEV_POSITIVE_EVER_PREG_CAT = case_when(HEV_IGM_POSITIVE_ENROLL ==1 & HEV_IGM_INCIDENT!=1 & HEV_IGM_UNKNOWN_BASELINE!=1 & HEV_IGG_POSITIVE_ENROLL != 1 ~ 1, 
                                                HEV_IGG_POSITIVE_ENROLL ==1 & HEV_IGG_INCIDENT!=1 & HEV_IGG_UNKNOWN_BASELINE!=1 & HEV_IGM_POSITIVE_ENROLL != 1 ~ 2,
                                                HEV_IGM_POSITIVE_ENROLL ==1 & HEV_IGM_INCIDENT!=1 & HEV_IGM_UNKNOWN_BASELINE!=1 & HEV_IGG_POSITIVE_ENROLL == 1 ~3, 
                                                HEV_IGM_POSITIVE_ENROLL !=1 & HEV_IGM_INCIDENT==1 & HEV_IGM_UNKNOWN_BASELINE!=1 & HEV_IGG_INCIDENT != 1 ~ 4, 
                                                HEV_IGG_POSITIVE_ENROLL !=1 & HEV_IGG_INCIDENT==1 & HEV_IGG_UNKNOWN_BASELINE!=1 & HEV_IGM_INCIDENT != 1 ~ 5, 
                                                HEV_IGM_POSITIVE_ENROLL !=1 & HEV_IGM_INCIDENT==1 & HEV_IGM_UNKNOWN_BASELINE!=1 & HEV_IGG_INCIDENT == 1 ~ 6, 
                                                HEV_IGM_POSITIVE_ENROLL !=1 & HEV_IGM_INCIDENT!=1 & HEV_IGM_UNKNOWN_BASELINE==1 & HEV_IGG_UNKNOWN_BASELINE != 1 ~ 7,
                                                HEV_IGG_POSITIVE_ENROLL !=1 & HEV_IGG_INCIDENT!=1 & HEV_IGG_UNKNOWN_BASELINE==1 & HEV_IGM_UNKNOWN_BASELINE != 1 ~ 8,
                                                HEV_IGM_POSITIVE_ENROLL !=1 & HEV_IGM_INCIDENT!=1 & HEV_IGM_UNKNOWN_BASELINE==1 & HEV_IGG_UNKNOWN_BASELINE == 1 ~ 9,
                                                TRUE ~ NA
  )) 

hev_export_labels <- hev_labels %>%
  select(SITE, MOMID, PREGID,
         HEV_IGM_PERF_ENROLL, HEV_IGM_POSITIVE_ENROLL, HEV_IGM_POSITIVE_ENROLL_LABEL, 
         HEV_IGG_PERF_ENROLL, HEV_IGG_POSITIVE_ENROLL, HEV_IGG_POSITIVE_ENROLL_LABEL,
         HEV_IGM_PERF_EVER_PREG, HEV_IGM_POSITIVE_EVER_PREG, HEV_IGM_POSITIVE_EVER_PREG_LABEL,
         HEV_DATE_IGM_POSITIVE, HEV_GESTAGE_IGM_POSITIVE_DAYS, HEV_GESTAGE_IGM_POSITIVE_WKS,
         HEV_IGG_PERF_EVER_PREG, HEV_IGG_POSITIVE_EVER_PREG, HEV_IGG_POSITIVE_EVER_PREG_LABEL,
         HEV_DATE_IGG_POSITIVE, HEV_GESTAGE_IGG_POSITIVE_DAYS, HEV_GESTAGE_IGG_POSITIVE_WKS,
         HEV_IGG_DAYS_BETWEEN_TESTS, HEV_IGG_WKS_BETWEEN_TESTS,
         HEV_IGM_UNKNOWN_BASELINE, HEV_IGM_UNKNOWN_BASELINE_LABEL, 
         HEV_IGG_UNKNOWN_BASELINE, HEV_IGG_UNKNOWN_BASELINE_LABEL,
         HEV_IGM_INCIDENT,HEV_IGM_INCIDENT_LABEL, HEV_IGG_INCIDENT, HEV_IGG_INCIDENT_LABEL,
         HEV_POSITIVE_EVER_PREG, HEV_POSITIVE_EVER_PREG_LABEL, 
         HEV_POSITIVE_EVER_PREG_CAT, HEV_POSITIVE_EVER_PREG_CAT_LABEL
  )


ggplot() + 
  geom_histogram(data = hev_export_labels %>%
                   filter(HEV_IGG_INCIDENT==1), aes(x= HEV_IGG_WKS_BETWEEN_TESTS), stat = "count") + 
  xlab("weeks between IgG- at enrol and IgG+ in pregnancy") + 
  scale_x_continuous(breaks = seq(12, 26, by = 1),
                     label = as.character(seq(12, 26, by = 1))) + 
  theme_bw()

# write.xlsx(hev_table, paste0(path_to_save, "hev_table" ,".xlsx"), na="", rowNames=TRUE)

#*****************************************************************************
# Chlamydia titers ----
# Vaginal swab testing
#*****************************************************************************
ct_titer <- mat_enroll %>% 
  # merge in mnh08 to pull titer results 
  left_join(mnh08 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, M08_LBSTDAT, CTNG_EXPANSION, 
                             M08_CTNG_CT_LBORRES,M08_CTNG_LBPERF_1,
                             M08_CTNG_LBTSTDAT, CTNG_EXPANSION_DATE), 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(CTNG_EXPANSION = case_when(is.na(CTNG_EXPANSION) ~ 0, TRUE ~ CTNG_EXPANSION)) %>% 
  filter(CTNG_EXPANSION==1) %>% 
  # merge in mnh07 for specimen collection date
  left_join(mnh07 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, M07_MAT_SPEC_COLLECT_DAT), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  # Was test performed?
  mutate(CT_TEST_PERF = case_when(CTNG_EXPANSION==0 ~ 0,
                                  M08_CTNG_LBPERF_1 ==1 ~ 1,
                                  is.na(M08_CTNG_LBPERF_1) ~ 0,
                                  TRUE ~ 0)) %>%
  # Test result available? 
  mutate(CT_TEST_RESULT_AVAI = case_when(CTNG_EXPANSION==0 ~ 0,
                                         M08_CTNG_CT_LBORRES %in% c(0,1,2) ~ 1, TRUE ~ 0)
  ) %>% 
  # Test result (positive, negative, inconclusive, missing)
  mutate(CT_TEST_POSITIVE = case_when(CTNG_EXPANSION==0 ~ 77, # 77, na; visit was before expansion date
                                      CT_TEST_PERF ==1 & CTNG_EXPANSION == 1 & M08_CTNG_CT_LBORRES == 1 ~ 1, # 1, positive
                                      CT_TEST_PERF ==1 & CTNG_EXPANSION == 1 & M08_CTNG_CT_LBORRES == 0 ~ 0, # 0, negative
                                      CT_TEST_PERF ==1 & CTNG_EXPANSION == 1 & M08_CTNG_CT_LBORRES == 2 ~ 2, # 2, inconclusive
                                      CT_TEST_PERF ==1 & CTNG_EXPANSION == 1 & M08_CTNG_CT_LBORRES %in% c(NA,55, 77, 99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      CTNG_EXPANSION ==1 & CT_TEST_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                      TRUE ~ 77)
  ) %>% 
  # Date positive test 
  mutate(DATE_POSITIVE = case_when(CT_TEST_POSITIVE ==1 ~ ymd(M08_LBSTDAT), 
                                   TRUE ~ NA_Date_)) %>% 
  # generate indicator variable to remove any unscheduled visits that do not have a test outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & CT_TEST_PERF ==1 & CT_TEST_RESULT_AVAI ==1 ~ 1, # keep if test was done and result is available
                                  TYPE_VISIT %in% c(13,14) & CT_TEST_POSITIVE %in% c(1,0) ~ 1, ## keep if valid results (could probably remove)
                                  TYPE_VISIT %in% c(1,2,3,4,5) ~ 1, # keep if not an unscheduled visit 
                                  TYPE_VISIT %in% c(13,14) & CT_TEST_POSITIVE %in% c(NA,55,77) ~ 0, # remove if missing 
                                  TRUE ~ 0)) %>% 
  filter(KEEP_UNSCHED==1)

ct_titer_export <- ct_titer %>% 
  filter(!TYPE_VISIT %in% c(6,7,8,9,10,11,12,14)) %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, VISIT_DATE,PREG_START_DATE, DATE_POSITIVE, starts_with("CT_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("CT_"), DATE_POSITIVE, VISIT_DATE),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(CT_TEST_POSITIVE_ENROLL = CT_TEST_POSITIVE_1,
         CT_TEST_PERF_ENROLL = CT_TEST_PERF_1,
         CT_TEST_POSITIVE_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("CT_TEST_POSITIVE_")) == 1) & CT_TEST_POSITIVE_ENROLL %in% c(55,77,99,NA) ~ 1,
                                                       TRUE ~ 0),
         
         CT_TEST_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("CT_TEST_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titer is +
                                                any(na.omit(c_across(starts_with("CT_TEST_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titer is -
                                                all(na.omit(c_across(starts_with("CT_TEST_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                TRUE ~ 77),
         CT_TEST_POSITIVE_INCIDENT = case_when(CT_TEST_POSITIVE_EVER_PREG ==1 & CT_TEST_POSITIVE_ENROLL ==0 ~ 1,
                                               TRUE  ~ 0),
         CT_TEST_PERF_EVER_PREG = case_when(any(c_across(starts_with("CT_TEST_PERF_")) == 1) ~ 1, # 1, TEST performed
                                            any(na.omit(c_across(starts_with("CT_TEST_PERF_"))) == 0) ~ 0, # 0, test not performed
                                            all(na.omit(c_across(starts_with("CT_TEST_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                            all(na.omit(c_across(starts_with("CT_TEST_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                            TRUE ~ 77)
  ) %>% 
  
  # generate date and gestage at positive RDT infection
  mutate(CT_TITER_POSITIVE_DATE = do.call(pmin, c(across(starts_with("DATE_POSITIVE_")), na.rm = TRUE)),
         CT_GESTAGE_POSITIVE_DAYS = as.numeric(CT_TITER_POSITIVE_DATE-ymd(PREG_START_DATE)),
         CT_GESTAGE_POSITIVE_WKS = CT_GESTAGE_POSITIVE_DAYS %/% 7
  ) 


## add labels
ct_labels <- ct_titer_export %>%
  mutate(
    CT_TEST_POSITIVE_ENROLL_LABEL = factor(CT_TEST_POSITIVE_ENROLL,levels = c(1,0,2,55,77),
                                           labels = c("Chlamydia titer+ at enrollment", "Chlamydia titer- at enrollment","Chlamydia titer inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    CT_TEST_POSITIVE_UNKNOWN_BASELINE_LABEL = factor(CT_TEST_POSITIVE_UNKNOWN_BASELINE,levels = c(1, 0),
                                                     labels = c("Titer+ during pregnancy but missing baseline status", "No incident chlamydia with unknown baseline status")),
    CT_TEST_POSITIVE_INCIDENT_LABEL = factor(CT_TEST_POSITIVE_INCIDENT,levels = c(1, 0), 
                                             labels = c("Incident chlamydia titer+ during pregnancy", "No incident chlamydia infection")),
    CT_TEST_POSITIVE_EVER_PREG_LABEL = factor(CT_TEST_POSITIVE_EVER_PREG,levels = c(1, 0, 77),
                                              labels = c("Chlamydia titer+ ever during pregnancy", "Chlamydia never during pregnancy", "NA/no test performed"))
  ) %>% 
  mutate(CT_TEST_POSITIVE_EVER_PREG_CAT_LABEL = case_when(CT_TEST_POSITIVE_ENROLL ==1 ~ "Titer+ at enroll",
                                                          CT_TEST_POSITIVE_INCIDENT ==1 ~ "incident Titer+",
                                                          CT_TEST_POSITIVE_UNKNOWN_BASELINE==1 ~ "Titer+ with unknown baseline",
                                                          TRUE ~ NA),
         CT_TEST_POSITIVE_EVER_PREG_CAT = case_when(CT_TEST_POSITIVE_ENROLL ==1 ~ 1,
                                                    CT_TEST_POSITIVE_INCIDENT ==1 ~ 2,
                                                    CT_TEST_POSITIVE_UNKNOWN_BASELINE==1 ~ 3,
                                                    TRUE ~ NA))

## figure out how to do dates
ct_export_labels <- ct_labels %>%
  select(SITE, MOMID, PREGID, 
         CT_TITER_POSITIVE_DATE, CT_GESTAGE_POSITIVE_DAYS, CT_GESTAGE_POSITIVE_WKS,
         CT_TEST_POSITIVE_ENROLL, CT_TEST_POSITIVE_ENROLL_LABEL, CT_TEST_PERF_ENROLL,
         CT_TEST_POSITIVE_EVER_PREG,CT_TEST_POSITIVE_EVER_PREG_LABEL, CT_TEST_PERF_EVER_PREG,
         CT_TEST_POSITIVE_UNKNOWN_BASELINE, CT_TEST_POSITIVE_UNKNOWN_BASELINE_LABEL,
         CT_TEST_POSITIVE_INCIDENT, CT_TEST_POSITIVE_INCIDENT_LABEL, 
         CT_TEST_POSITIVE_EVER_PREG_CAT, CT_TEST_POSITIVE_EVER_PREG_CAT_LABEL)


# write.xlsx(ct_titer_table, paste0(path_to_save, "ct_titer_table" ,".xlsx"), na="", rowNames=TRUE)

#*****************************************************************************
# Gonorrhea titers ----
# Vaginal swab testing
#*****************************************************************************
ng_titer <- mat_enroll %>% 
  # merge in mnh08 to pull titer results 
  left_join(mnh08 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE,M08_LBSTDAT, CTNG_EXPANSION, 
                             M08_CTNG_LBPERF_2, M08_CTNG_LBTSTDAT, M08_CTNG_NG_LBORRES, CTNG_EXPANSION_DATE, CTNG_EXPANSION), 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(CTNG_EXPANSION = case_when(is.na(CTNG_EXPANSION) ~ 0, TRUE ~ CTNG_EXPANSION)) %>% 
  filter(CTNG_EXPANSION==1) %>% 
  # merge in mnh07 for specimen collection date
  left_join(mnh07 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, M07_MAT_SPEC_COLLECT_DAT), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  # Was test performed?
  mutate(NG_TEST_PERF = case_when(M08_CTNG_LBPERF_2 ==1 ~ 1,
                                  is.na(M08_CTNG_LBPERF_2) ~ 0,
                                  CTNG_EXPANSION==0 ~ 0,
                                  TRUE ~ 0)) %>%
  # Test result available? 
  mutate(NG_TEST_RESULT_AVAI = case_when(M08_CTNG_NG_LBORRES %in% c(0,1,2) ~ 1, TRUE ~ 0)
  ) %>% 
  # Test result (positive, negative, inconclusive, missing)
  mutate(NG_TEST_POSITIVE = case_when(CTNG_EXPANSION==0 ~ 77, # 77, na; visit was before expansion date
                                      NG_TEST_PERF ==1 & CTNG_EXPANSION == 1 & M08_CTNG_NG_LBORRES == 1 ~ 1, # 1, positive
                                      NG_TEST_PERF ==1 & CTNG_EXPANSION == 1 & M08_CTNG_NG_LBORRES == 0 ~ 0, # 0, negative
                                      NG_TEST_PERF ==1 & CTNG_EXPANSION == 1 & M08_CTNG_NG_LBORRES == 2 ~ 2, # 2, inconclusive
                                      NG_TEST_PERF ==1 & CTNG_EXPANSION == 1 & M08_CTNG_NG_LBORRES %in% c(NA,55, 77, 99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      CTNG_EXPANSION ==1 & NG_TEST_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                      TRUE ~ NA)
  ) %>% 
  # Date positive test 
  mutate(DATE_POSITIVE = case_when(NG_TEST_POSITIVE ==1 ~ ymd(M08_LBSTDAT), 
                                   TRUE ~ NA_Date_)) %>% 
  # generate indicator variable to remove any unscheduled visits that do not have a test outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & NG_TEST_PERF ==1 & NG_TEST_RESULT_AVAI ==1 ~ 1, # keep if test was done and result is available
                                  TYPE_VISIT %in% c(13,14) & NG_TEST_POSITIVE %in% c(1,0) ~ 1, ## keep if valid results (could probably remove)
                                  TYPE_VISIT %in% c(1,2,3,4,5) ~ 1, # keep if not an unscheduled visit 
                                  TYPE_VISIT %in% c(13,14) & NG_TEST_POSITIVE %in% c(NA,55,77) ~ 0, # remove if missing 
                                  TRUE ~ 0)) %>% 
  filter(KEEP_UNSCHED==1)

ng_titer_export <- ng_titer %>% 
  filter(!TYPE_VISIT %in% c(6,7,8,9,10,11,12,14)) %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, VISIT_DATE,PREG_START_DATE, DATE_POSITIVE, starts_with("NG_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("NG_"), DATE_POSITIVE, VISIT_DATE),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(NG_TEST_POSITIVE_ENROLL = NG_TEST_POSITIVE_1,
         NG_TEST_PERF_ENROLL = NG_TEST_PERF_1,
         NG_TEST_POSITIVE_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("NG_TEST_POSITIVE_")) == 1) & NG_TEST_POSITIVE_ENROLL %in% c(55,77,99,NA) ~ 1,
                                                       TRUE ~ 0),
         
         NG_TEST_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("NG_TEST_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titer is +
                                                any(na.omit(c_across(starts_with("NG_TEST_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titer is -
                                                all(na.omit(c_across(starts_with("NG_TEST_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                TRUE ~ 77),
         NG_TEST_POSITIVE_INCIDENT = case_when(NG_TEST_POSITIVE_EVER_PREG ==1 & NG_TEST_POSITIVE_ENROLL ==0 ~ 1,
                                               TRUE  ~ 0),
         NG_TEST_PERF_EVER_PREG = case_when(any(c_across(starts_with("NG_TEST_PERF_")) == 1) ~ 1, # 1, TEST performed
                                            any(na.omit(c_across(starts_with("NG_TEST_PERF_"))) == 0) ~ 0, # 0, test not performed
                                            all(na.omit(c_across(starts_with("NG_TEST_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                            all(na.omit(c_across(starts_with("NG_TEST_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                            TRUE ~ 77)
  ) %>% 
  
  # generate date and gestage at positive RDT infection
  mutate(NG_TITER_POSITIVE_DATE = do.call(pmin, c(across(starts_with("DATE_POSITIVE_")), na.rm = TRUE)),
         NG_GESTAGE_POSITIVE_DAYS = as.numeric(NG_TITER_POSITIVE_DATE-ymd(PREG_START_DATE)),
         NG_GESTAGE_POSITIVE_WKS = NG_GESTAGE_POSITIVE_DAYS %/% 7
  ) 


## add labels
ng_labels <- ng_titer_export %>%
  mutate(
    NG_TEST_POSITIVE_ENROLL_LABEL = factor(NG_TEST_POSITIVE_ENROLL,levels = c(1,0,2,55,77),
                                           labels = c("Gonorrhea titer+ at enrollment", "Gonorrhea titer- at enrollment","Gonorrhea titer inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    NG_TEST_POSITIVE_UNKNOWN_BASELINE_LABEL = factor(NG_TEST_POSITIVE_UNKNOWN_BASELINE,levels = c(1, 0),
                                                     labels = c("Titer+ during pregnancy but missing baseline status", "No incident Gonorrhea with unknown baseline status")),
    NG_TEST_POSITIVE_INCIDENT_LABEL = factor(NG_TEST_POSITIVE_INCIDENT,levels = c(1, 0), 
                                             labels = c("Incident Gonorrhea titer+ during pregnancy", "No incident Gonorrhea infection")),
    NG_TEST_POSITIVE_EVER_PREG_LABEL = factor(NG_TEST_POSITIVE_EVER_PREG,levels = c(1, 0, 77),
                                              labels = c("Gonorrhea titer+ ever during pregnancy", "Gonorrhea never during pregnancy", "NA/no test performed"))
  ) %>% 
  mutate(NG_TEST_POSITIVE_EVER_PREG_CAT_LABEL = case_when(NG_TEST_POSITIVE_ENROLL ==1 ~ "Titer+ at enroll",
                                                          NG_TEST_POSITIVE_INCIDENT ==1 ~ "incident Titer+",
                                                          NG_TEST_POSITIVE_UNKNOWN_BASELINE==1 ~ "Titer+ with unknown baseline",
                                                          TRUE ~ NA),
         NG_TEST_POSITIVE_EVER_PREG_CAT = case_when(NG_TEST_POSITIVE_ENROLL ==1 ~ 1,
                                                    NG_TEST_POSITIVE_INCIDENT ==1 ~ 2,
                                                    NG_TEST_POSITIVE_UNKNOWN_BASELINE==1 ~ 3,
                                                    TRUE ~ NA))

## figure out how to do dates
ng_export_labels <- ng_labels %>%
  select(SITE, MOMID, PREGID, 
         NG_TITER_POSITIVE_DATE, NG_GESTAGE_POSITIVE_DAYS, NG_GESTAGE_POSITIVE_WKS,
         NG_TEST_POSITIVE_ENROLL, NG_TEST_POSITIVE_ENROLL_LABEL, NG_TEST_PERF_ENROLL,
         NG_TEST_POSITIVE_EVER_PREG,NG_TEST_POSITIVE_EVER_PREG_LABEL, NG_TEST_PERF_EVER_PREG,
         NG_TEST_POSITIVE_UNKNOWN_BASELINE, NG_TEST_POSITIVE_UNKNOWN_BASELINE_LABEL,
         NG_TEST_POSITIVE_INCIDENT, NG_TEST_POSITIVE_INCIDENT_LABEL, 
         NG_TEST_POSITIVE_EVER_PREG_CAT, NG_TEST_POSITIVE_EVER_PREG_CAT_LABEL)


# write.xlsx(ng_titer_table, paste0(path_to_save, "ng_titer_table" ,".xlsx"), na="", rowNames=TRUE)
#*****************************************************************************
# Zika ----
#*****************************************************************************

zik <- mat_enroll %>% 
  # merge in mnh08 to pull titer results 
  left_join(mnh08 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, ZCD_EXPANSION, M08_LBSTDAT,
                             M08_ZCD_LBPERF_1, M08_ZCD_ZIKIGM_LBORRES, M08_ZCD_LBPERF_2, M08_ZCD_ZIKIGG_LBORRES), 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  # merge in mnh07 for specimen collection date
  left_join(mnh07 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, M07_MAT_SPEC_COLLECT_DAT), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  filter(ZCD_EXPANSION==1) %>% 
  # Was test performed?
  mutate(ZIK_IGM_PERF = case_when(M08_ZCD_LBPERF_1 ==1 ~ 1,
                                  is.na(M08_ZCD_LBPERF_1) ~ 0,
                                  TRUE ~ 0),
         ZIK_IGG_PERF = case_when(M08_ZCD_LBPERF_2 ==1 ~ 1,
                                  is.na(M08_ZCD_LBPERF_2) ~ 0,
                                  TRUE ~ 0)
  ) %>%
  # Test result available? 
  mutate(ZIK_IGM_RESULT_AVAI = case_when(M08_ZCD_ZIKIGM_LBORRES %in% c(0,1,2) ~ 1, TRUE ~ 0),
         ZIK_IGG_RESULT_AVAI = case_when(M08_ZCD_ZIKIGG_LBORRES %in% c(0,1,2) ~ 1, TRUE ~ 0)
  ) %>% 
  
  # Test result by Titers
  mutate(ZIK_IGM_POSITIVE = case_when(ZCD_EXPANSION==0 ~ 77, # 77, na; visit was before expansion date
                                      ZIK_IGM_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_ZIKIGM_LBORRES == 1 ~ 1, # 1, positive
                                      ZIK_IGM_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_ZIKIGM_LBORRES == 0 ~ 0, # 0, negative
                                      ZIK_IGM_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_ZIKIGM_LBORRES == 2 ~ 2, # 2, inconclusive
                                      ZIK_IGM_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_ZIKIGM_LBORRES %in% c(NA,55, 77, 99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      ZCD_EXPANSION ==1 & ZIK_IGM_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                      TRUE ~ 77),
         ZIK_IGG_POSITIVE = case_when(ZCD_EXPANSION==0 ~ 77, # 77, na; visit was before expansion date
                                      ZIK_IGG_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_ZIKIGG_LBORRES == 1 ~ 1, # 1, positive
                                      ZIK_IGG_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_ZIKIGG_LBORRES == 0 ~ 0, # 0, negative
                                      ZIK_IGG_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_ZIKIGG_LBORRES == 2 ~ 2, # 2, inconclusive
                                      ZIK_IGG_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_ZIKIGG_LBORRES %in% c(NA,55, 77, 99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      ZCD_EXPANSION ==1 & ZIK_IGG_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                      TRUE ~ 77)) %>% 
  # Date positive test 
  mutate(DATE_IGM_POSITIVE = case_when(ZIK_IGM_POSITIVE ==1 ~ ymd(M08_LBSTDAT), 
                                       TRUE ~ NA_Date_),
         DATE_IGG_POSITIVE = case_when(ZIK_IGG_POSITIVE ==1 ~ ymd(M08_LBSTDAT), 
                                       TRUE ~ NA_Date_),
         
  ) %>% 
  
  # generate indicator variable to remove any unscheduled visits that do not have a test outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & (ZIK_IGM_PERF ==1 | ZIK_IGG_PERF ==1) & (ZIK_IGM_RESULT_AVAI ==1 | ZIK_IGG_RESULT_AVAI ==1) ~ 1, # keep if test was done and result is available
                                  TYPE_VISIT %in% c(13,14) & (ZIK_IGM_POSITIVE %in% c(1,0,2) | ZIK_IGG_POSITIVE %in% c(1,0,2)) ~ 1, ## keep if valid results (could probably remove)
                                  TYPE_VISIT %in% c(1,2,3,4,5,6,7,8,9,10,11,12) ~ 1, # keep if not an unscheduled visit 
                                  TYPE_VISIT %in% c(13,14) & ZIK_IGM_POSITIVE %in% c(NA,55,77) & ZIK_IGG_POSITIVE %in% c(NA,55,77) ~ 0, # remove if missing 
                                  TRUE ~ 0)) %>% 
  filter(KEEP_UNSCHED==1)

zik_export_anc <- zik %>% 
  filter(!TYPE_VISIT %in% c(6,7,8,9,10,11,12,14)) %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, VISIT_DATE, ZCD_EXPANSION,PREG_START_DATE,DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, M08_LBSTDAT, starts_with("ZIK_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("ZIK_"), VISIT_DATE, DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, M08_LBSTDAT),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(ZIK_IGM_POSITIVE_ENROLL = ZIK_IGM_POSITIVE_1,
         ZIK_IGG_POSITIVE_ENROLL = ZIK_IGG_POSITIVE_1,
         # ZIK_IGG_POSITIVE_ENROLL = case_when(ZIK_IGG_POSITIVE_1 ==1 & ZIK_IGM_POSITIVE_ENROLL ==0 ~ 1, 
         #                                     ZIK_IGG_POSITIVE_1 ==1 & ZIK_IGM_POSITIVE_ENROLL ==0 ~ 0,
         #                                     TRUE ~ ZIK_IGG_POSITIVE_1),
         ZIK_IGM_PERF_ENROLL = ZIK_IGM_PERF_1,
         ZIK_IGM_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("ZIK_IGM_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                                any(na.omit(c_across(starts_with("ZIK_IGM_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                                all(na.omit(c_across(starts_with("ZIK_IGM_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                                all(na.omit(c_across(starts_with("ZIK_IGM_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                TRUE ~ 77),
         ZIK_IGM_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("ZIK_IGM_POSITIVE_")) == 1) & ZIK_IGM_POSITIVE_ENROLL %in% c(2, 55,77,99,NA)~ 1, # 1, Positive if  titers is + 
                                              TRUE ~ 0),
         ZIK_IGM_INCIDENT = case_when(ZIK_IGM_POSITIVE_EVER_PREG ==1 & ZIK_IGM_POSITIVE_ENROLL ==0 ~ 1, 
                                      TRUE  ~ 0),
         ZIK_IGM_PERF_EVER_PREG = case_when(any(c_across(starts_with("ZIK_IGM_PERF_")) == 1) ~ 1, # 1, titers performed
                                            any(na.omit(c_across(starts_with("ZIK_IGM_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                            all(na.omit(c_across(starts_with("ZIK_IGM_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                            all(na.omit(c_across(starts_with("ZIK_IGM_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                            TRUE ~ 77),
         ZIK_IGM_POSITIVE_EVER_PREG_TEXT = case_when(ZIK_IGM_POSITIVE_ENROLL ==1 & ZIK_IGM_INCIDENT!=1 & ZIK_IGM_UNKNOWN_BASELINE!=1 ~ "Baseline IgM+", 
                                                     ZIK_IGM_POSITIVE_ENROLL !=1 & ZIK_IGM_INCIDENT==1 & ZIK_IGM_UNKNOWN_BASELINE!=1 ~ "Incident IgM+", 
                                                     ZIK_IGM_POSITIVE_ENROLL !=1 & ZIK_IGM_INCIDENT!=1 & ZIK_IGM_UNKNOWN_BASELINE==1 ~ "IgM+ with unknown baseline",
                                                     TRUE ~ NA),
         ZIK_IGG_PERF_ENROLL = ZIK_IGG_PERF_1,
         ZIK_IGG_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("ZIK_IGG_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                                any(na.omit(c_across(starts_with("ZIK_IGG_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                                all(na.omit(c_across(starts_with("ZIK_IGG_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                                all(na.omit(c_across(starts_with("ZIK_IGG_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                TRUE ~ 77),
         ZIK_IGG_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("ZIK_IGG_POSITIVE_")) == 1) & ZIK_IGG_POSITIVE_ENROLL %in% c(2, 55,77,99,NA) & ZIK_IGM_POSITIVE_ENROLL %in% c(2, 55,77,99,NA) ~ 1, # 1, Positive if  titers is + 
                                              TRUE ~ 0),
         ZIK_IGG_INCIDENT = case_when(ZIK_IGG_POSITIVE_EVER_PREG ==1 & ZIK_IGG_POSITIVE_ENROLL ==0 & ZIK_IGM_POSITIVE_ENROLL ==0 ~ 1, 
                                      TRUE  ~ 0),
         ZIK_IGG_POSITIVE_EVER_PREG_TEXT = case_when(ZIK_IGG_POSITIVE_ENROLL ==1 & ZIK_IGG_INCIDENT!=1 & ZIK_IGG_UNKNOWN_BASELINE!=1 ~ "Baseline IgG+", 
                                                     ZIK_IGG_POSITIVE_ENROLL !=1 & ZIK_IGG_INCIDENT==1 & ZIK_IGG_UNKNOWN_BASELINE!=1 ~ "Incident IgG+", 
                                                     ZIK_IGG_POSITIVE_ENROLL !=1 & ZIK_IGG_INCIDENT!=1 & ZIK_IGG_UNKNOWN_BASELINE==1 ~ "IgG+ with unknown baseline",
                                                     TRUE ~ NA),
         ZIK_IGG_PERF_EVER_PREG = case_when(any(c_across(starts_with("ZIK_IGG_PERF_")) == 1) ~ 1, # 1, titers performed
                                            any(na.omit(c_across(starts_with("ZIK_IGG_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                            all(na.omit(c_across(starts_with("ZIK_IGG_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                            all(na.omit(c_across(starts_with("ZIK_IGG_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                            TRUE ~ 77)
         
         
  ) %>% 
  mutate(ZIK_POSITIVE_EVER_PREG = case_when(ZIK_IGM_POSITIVE_EVER_PREG ==1 | ZIK_IGG_INCIDENT==1 ~ 1, ## EVER PREG variable is zika IgM+ ever during pregnancy or incident IgG
                                            ZIK_IGM_POSITIVE_EVER_PREG ==0 & ZIK_IGG_INCIDENT== 0 ~ 0, 
                                            ZIK_IGM_PERF_EVER_PREG==0 & ZIK_IGG_PERF_EVER_PREG==0 ~ 77, # ## if test performed but no infection, these are NOs
                                            TRUE ~ 77) 
         
  ) %>% 
  mutate(ZIK_DATE_IGM_POSITIVE = do.call(pmin, c(across(starts_with("DATE_IGM_POSITIVE_")), na.rm = TRUE)),
         ZIK_GESTAGE_IGM_POSITIVE_DAYS = as.numeric(ZIK_DATE_IGM_POSITIVE-ymd(PREG_START_DATE)),
         ZIK_GESTAGE_IGM_POSITIVE_WKS = ZIK_GESTAGE_IGM_POSITIVE_DAYS %/% 7
  ) %>%
  mutate(ZIK_DATE_IGG_POSITIVE = do.call(pmin, c(across(starts_with("DATE_IGG_POSITIVE_")), na.rm = TRUE)),
         ZIK_GESTAGE_IGG_POSITIVE_DAYS = as.numeric(ZIK_DATE_IGG_POSITIVE-ymd(PREG_START_DATE)),
         ZIK_GESTAGE_IGG_POSITIVE_WKS = ZIK_GESTAGE_IGG_POSITIVE_DAYS %/% 7
  ) %>% 
  ## generate indicator if incident IGG
  mutate(ZIK_INCIDENT_IGG_POSITIVE = case_when(ZIK_IGG_POSITIVE_ENROLL==0 & ZIK_IGG_POSITIVE_EVER_PREG==1 ~ 1, TRUE ~ 0),
         ZIK_IGG_DAYS_BETWEEN_TESTS = case_when(ZIK_INCIDENT_IGG_POSITIVE==1 ~ as.numeric(ymd(ZIK_DATE_IGG_POSITIVE) - ymd(M08_LBSTDAT_1))),
         ZIK_IGG_WKS_BETWEEN_TESTS = ZIK_IGG_DAYS_BETWEEN_TESTS %/% 7
  )

zik_export_pnc <- zik %>% 
  filter(TYPE_VISIT %in% c(6,7,8,9,10,11,12,14)) %>% 
  left_join(mat_end %>% select("SITE", "MOMID", "PREGID", "PREG_END_DATE") %>% mutate(PREG_END_DATE = ymd(PREG_END_DATE)), by = c("SITE", "MOMID", "PREGID")) %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, VISIT_DATE, ZCD_EXPANSION,PREG_END_DATE,DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, M08_LBSTDAT, starts_with("ZIK_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("ZIK_"), VISIT_DATE, DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, M08_LBSTDAT),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # merge in enrollment ever preg variables 
  left_join(zik_export_anc %>% select(SITE, MOMID, PREGID, ZIK_POSITIVE_EVER_PREG, ZIK_IGM_POSITIVE_EVER_PREG, ZIK_IGG_POSITIVE_EVER_PREG), by= c("SITE", "MOMID", "PREGID")) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(ZIK_IGM_POSITIVE_EVER_PP = case_when(any(c_across(starts_with("ZIK_IGM_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                              any(na.omit(c_across(starts_with("ZIK_IGM_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                              all(na.omit(c_across(starts_with("ZIK_IGM_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                              all(na.omit(c_across(starts_with("ZIK_IGM_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                              TRUE ~ 77),
         ZIK_IGG_POSITIVE_EVER_PP = case_when(any(c_across(starts_with("ZIK_IGG_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                              any(na.omit(c_across(starts_with("ZIK_IGG_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                              all(na.omit(c_across(starts_with("ZIK_IGG_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                              all(na.omit(c_across(starts_with("ZIK_IGG_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                              TRUE ~ 77),
         ZIK_IGM_INCIDENT_PP = case_when(ZIK_IGM_POSITIVE_EVER_PP ==1 & ZIK_IGM_POSITIVE_EVER_PREG ==0 ~ 1, 
                                         TRUE  ~ 0),
         ZIK_IGG_INCIDENT_PP = case_when(ZIK_IGG_POSITIVE_EVER_PP ==1 & ZIK_IGG_POSITIVE_EVER_PREG ==0 ~ 1, 
                                         TRUE  ~ 0),
         ZIK_IGM_PERF_EVER_PP = case_when(any(c_across(starts_with("ZIK_IGM_PERF_")) == 1) ~ 1, # 1, titers performed
                                          any(na.omit(c_across(starts_with("ZIK_IGM_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                          all(na.omit(c_across(starts_with("ZIK_IGM_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                          all(na.omit(c_across(starts_with("ZIK_IGM_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                          TRUE ~ 77),
         ZIK_IGG_PERF_EVER_PP = case_when(any(c_across(starts_with("ZIK_IGG_PERF_")) == 1) ~ 1, # 1, titers performed
                                          any(na.omit(c_across(starts_with("ZIK_IGG_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                          all(na.omit(c_across(starts_with("ZIK_IGG_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                          all(na.omit(c_across(starts_with("ZIK_IGG_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                          TRUE ~ 77),
         
         ZIK_IGM_POSITIVE_EVER_PP_TEXT = case_when(ZIK_IGM_POSITIVE_EVER_PP ==1 ~ "Postpartum IgM+", 
                                                   ZIK_IGM_INCIDENT_PP == 1 ~ "Incident IgM+ during Postpartum", 
                                                   TRUE ~ NA),
         
         ZIK_IGG_POSITIVE_EVER_PP_TEXT = case_when(ZIK_IGG_POSITIVE_EVER_PP ==1 ~ "Postpartum IgG+", 
                                                   ZIK_IGG_INCIDENT_PP == 1 ~ "Incident IgG+ during Postpartum", 
                                                   TRUE ~ NA)
  ) %>% 
  mutate(ZIK_POSITIVE_EVER_PP = case_when(ZIK_IGM_POSITIVE_EVER_PP ==1 | ZIK_IGM_INCIDENT_PP==1 ~ 1, ## EVER PREG variable is zika IgM+ ever during pregnancy or incident IgG
                                          ZIK_IGM_POSITIVE_EVER_PP ==0 & ZIK_IGG_INCIDENT_PP== 0 ~ 0, 
                                          ZIK_IGM_PERF_EVER_PP==0 & ZIK_IGG_PERF_EVER_PP==0 ~ 77, # ## if test performed but no infection, these are NOs
                                          TRUE ~ 77) 
         
  ) %>% 
  mutate(ZIK_DATE_IGM_POSITIVE_PP = do.call(pmin, c(across(starts_with("DATE_IGM_POSITIVE_")), na.rm = TRUE)),
         ZIK_GESTAGE_IGM_POSITIVE_DAYS_PP = as.numeric(ZIK_DATE_IGM_POSITIVE_PP-ymd(PREG_END_DATE)),
         ZIK_GESTAGE_IGM_POSITIVE_WKS_PP = ZIK_GESTAGE_IGM_POSITIVE_DAYS_PP %/% 7
  ) %>%
  mutate(ZIK_DATE_IGG_POSITIVE_PP = do.call(pmin, c(across(starts_with("DATE_IGG_POSITIVE_")), na.rm = TRUE)),
         ZIK_GESTAGE_IGG_POSITIVE_DAYS_PP = as.numeric(ZIK_DATE_IGG_POSITIVE_PP-ymd(PREG_END_DATE)),
         ZIK_GESTAGE_IGG_POSITIVE_WKS_PP = ZIK_GESTAGE_IGG_POSITIVE_DAYS_PP %/% 7
  ) %>% 
  select(SITE, MOMID, PREGID, contains("_PP"))


## add labels
zik_labels <- zik_export_anc %>%
  left_join(zik_export_pnc, by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(
    ZIK_IGM_POSITIVE_ENROLL_LABEL = factor(ZIK_IGM_POSITIVE_ENROLL,levels = c(1, 0, 2, 55, 77),
                                           labels = c("ZIK IgM+ at enrollment", "ZIK IgM- at enrollment", "ZIK IgM inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    ZIK_IGG_POSITIVE_ENROLL_LABEL = factor(ZIK_IGG_POSITIVE_ENROLL,levels = c(1, 0, 2, 55, 77),
                                           labels = c("ZIK IgG+ at enrollment", "ZIK IgG- at enrollment", "ZIK IgG inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    ZIK_IGM_UNKNOWN_BASELINE_LABEL = factor(ZIK_IGM_UNKNOWN_BASELINE,levels = c(1, 0),
                                            labels = c("ZIK IgM+ during pregnancy but missing baseline status", "No incident zika infection with unknown baseline status")),
    ZIK_IGG_UNKNOWN_BASELINE_LABEL = factor(ZIK_IGG_UNKNOWN_BASELINE,levels = c(1, 0),
                                            labels = c("ZIK IgG+ during pregnancy but missing baseline status", "No incident zika infection with unknown baseline status")),
    ZIK_IGM_INCIDENT_LABEL = factor(ZIK_IGM_INCIDENT,levels = c(1, 0),
                                    labels = c("Incident ZIK IgM+ during pregnancy", "No incident Zika infection")),
    ZIK_IGG_INCIDENT_LABEL = factor(ZIK_IGG_INCIDENT,levels = c(1, 0),
                                    labels = c("Incident ZIK IgG+ during pregnancy", "No incident Zika IgG+ infection")),
    ZIK_IGM_INCIDENT_PP_LABEL = factor(ZIK_IGM_INCIDENT_PP,levels = c(1, 0),
                                       labels = c("Incident ZIK IgM+ during postpartum", "No incident Zika infection during postpartum")),
    ZIK_IGG_INCIDENT_PP_LABEL = factor(ZIK_IGG_INCIDENT_PP,levels = c(1, 0),
                                       labels = c("Incident ZIK IgG+ during postpartum", "No incident Zika IgG+ infection during postpartum")),
    ZIK_IGM_POSITIVE_EVER_PREG_LABEL = factor(ZIK_IGM_POSITIVE_EVER_PREG,levels = c(1, 0, 2, 55, 77),
                                              labels = c("ZIK IgM+ ever during pregnancy", "ZIK IgM never during pregnancy", "ZIK IgM inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    ZIK_IGG_POSITIVE_EVER_PREG_LABEL = factor(ZIK_IGG_POSITIVE_EVER_PREG,levels = c(1, 0, 2, 55, 77),
                                              labels = c("ZIK IgG+ ever during pregnancy", "ZIK IgG never during pregnancy", "ZIK IgG inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    ZIK_POSITIVE_EVER_PREG_LABEL = factor(ZIK_POSITIVE_EVER_PREG,levels = c(1, 0, 77),
                                          labels = c("Zika ever during pregnancy", "Zika never during pregnancy", "NA/no test performed")),
    ZIK_IGM_POSITIVE_EVER_PP_LABEL = factor(ZIK_IGM_POSITIVE_EVER_PP,levels = c(1, 0, 2, 55, 77),
                                            labels = c("ZIK IgM+ ever during postpartum", "ZIK IgM never during postpartum", "ZIK IgM inconclusive during postpartum", "Test performed but missing test result", "NA/no test performed")),
    ZIK_IGG_POSITIVE_EVER_PP_LABEL = factor(ZIK_IGG_POSITIVE_EVER_PP,levels = c(1, 0, 2, 55, 77),
                                            labels = c("ZIK IgG+ ever during postpartum", "ZIK IgG never during postpartum", "ZIK IgG inconclusive during postpartum", "Test performed but missing test result", "NA/no test performed")),
    ZIK_POSITIVE_EVER_PP_LABEL = factor(ZIK_POSITIVE_EVER_PP,levels = c(1, 0, 77),
                                        labels = c("Zika ever during postpartum", "Zika never during postpartum", "NA/no test performed"))
  ) %>% 
  mutate(ZIK_POSITIVE_EVER_PREG_CAT_LABEL = case_when(ZIK_IGM_POSITIVE_ENROLL ==1 & ZIK_IGM_INCIDENT!=1 & ZIK_IGM_UNKNOWN_BASELINE!=1 & ZIK_IGG_POSITIVE_ENROLL != 1 ~ "Baseline IgM+", 
                                                      ZIK_IGG_POSITIVE_ENROLL ==1 & ZIK_IGG_INCIDENT!=1 & ZIK_IGG_UNKNOWN_BASELINE!=1 & ZIK_IGM_POSITIVE_ENROLL != 1 ~ "Baseline IgG+",
                                                      ZIK_IGM_POSITIVE_ENROLL ==1 & ZIK_IGM_INCIDENT!=1 & ZIK_IGM_UNKNOWN_BASELINE!=1 & ZIK_IGG_POSITIVE_ENROLL == 1 ~ "Baseline IgM+ & IgG+", 
                                                      ZIK_IGM_POSITIVE_ENROLL !=1 & ZIK_IGM_INCIDENT==1 & ZIK_IGM_UNKNOWN_BASELINE!=1 & ZIK_IGG_INCIDENT != 1 ~ "Incident IgM+", 
                                                      ZIK_IGG_POSITIVE_ENROLL !=1 & ZIK_IGG_INCIDENT==1 & ZIK_IGG_UNKNOWN_BASELINE!=1 & ZIK_IGM_INCIDENT != 1 ~ "Incident IgG+", 
                                                      ZIK_IGM_POSITIVE_ENROLL !=1 & ZIK_IGM_INCIDENT==1 & ZIK_IGM_UNKNOWN_BASELINE!=1 & ZIK_IGG_INCIDENT == 1 ~ "Incident IgM+ & IgG+", 
                                                      ZIK_IGM_POSITIVE_ENROLL !=1 & ZIK_IGM_INCIDENT!=1 & ZIK_IGM_UNKNOWN_BASELINE==1 & ZIK_IGG_UNKNOWN_BASELINE != 1 ~ "IgM+ with unknown baseline",
                                                      ZIK_IGG_POSITIVE_ENROLL !=1 & ZIK_IGG_INCIDENT!=1 & ZIK_IGG_UNKNOWN_BASELINE==1 & ZIK_IGM_UNKNOWN_BASELINE != 1 ~ "IgG+ with unknown baseline",
                                                      ZIK_IGM_POSITIVE_ENROLL !=1 & ZIK_IGM_INCIDENT!=1 & ZIK_IGM_UNKNOWN_BASELINE==1 & ZIK_IGG_UNKNOWN_BASELINE == 1 ~ "IgM+ & IgG+ with unknown baseline",
                                                      TRUE ~ NA
  )) %>% 
  mutate(ZIK_POSITIVE_EVER_PREG_CAT = case_when(ZIK_IGM_POSITIVE_ENROLL ==1 & ZIK_IGM_INCIDENT!=1 & ZIK_IGM_UNKNOWN_BASELINE!=1 & ZIK_IGG_POSITIVE_ENROLL != 1 ~ 1, 
                                                ZIK_IGG_POSITIVE_ENROLL ==1 & ZIK_IGG_INCIDENT!=1 & ZIK_IGG_UNKNOWN_BASELINE!=1 & ZIK_IGM_POSITIVE_ENROLL != 1 ~ 2,
                                                ZIK_IGM_POSITIVE_ENROLL ==1 & ZIK_IGM_INCIDENT!=1 & ZIK_IGM_UNKNOWN_BASELINE!=1 & ZIK_IGG_POSITIVE_ENROLL == 1 ~3, 
                                                ZIK_IGM_POSITIVE_ENROLL !=1 & ZIK_IGM_INCIDENT==1 & ZIK_IGM_UNKNOWN_BASELINE!=1 & ZIK_IGG_INCIDENT != 1 ~ 4, 
                                                ZIK_IGG_POSITIVE_ENROLL !=1 & ZIK_IGG_INCIDENT==1 & ZIK_IGG_UNKNOWN_BASELINE!=1 & ZIK_IGM_INCIDENT != 1 ~ 5, 
                                                ZIK_IGM_POSITIVE_ENROLL !=1 & ZIK_IGM_INCIDENT==1 & ZIK_IGM_UNKNOWN_BASELINE!=1 & ZIK_IGG_INCIDENT == 1 ~ 6, 
                                                ZIK_IGM_POSITIVE_ENROLL !=1 & ZIK_IGM_INCIDENT!=1 & ZIK_IGM_UNKNOWN_BASELINE==1 & ZIK_IGG_UNKNOWN_BASELINE != 1 ~ 7,
                                                ZIK_IGG_POSITIVE_ENROLL !=1 & ZIK_IGG_INCIDENT!=1 & ZIK_IGG_UNKNOWN_BASELINE==1 & ZIK_IGM_UNKNOWN_BASELINE != 1 ~ 8,
                                                ZIK_IGM_POSITIVE_ENROLL !=1 & ZIK_IGM_INCIDENT!=1 & ZIK_IGM_UNKNOWN_BASELINE==1 & ZIK_IGG_UNKNOWN_BASELINE == 1 ~ 9,
                                                TRUE ~ NA
  ))  %>% 
  mutate(ZIK_POSITIVE_EVER_PP_CAT_LABEL = case_when(ZIK_IGM_POSITIVE_EVER_PP ==1 & ZIK_IGM_POSITIVE_EVER_PREG==1 ~ "Zika IgM+ during pregnancy and postpartum", 
                                                    ZIK_IGG_POSITIVE_EVER_PP ==1 & ZIK_IGG_POSITIVE_EVER_PREG==1 ~ "Zika IgG+ during pregnancy and postpartum", 
                                                    ZIK_IGM_INCIDENT_PP ==1 & ZIK_IGG_INCIDENT_PP!=1 ~ "Incident Zika IgM+ during postpartum", 
                                                    ZIK_IGM_INCIDENT_PP !=1 & ZIK_IGG_INCIDENT_PP==1 ~ "Incident Zika IgG+ during postpartum", 
                                                    TRUE ~ NA
  )) %>% 
  mutate(ZIK_POSITIVE_EVER_PP_CAT = case_when(ZIK_IGM_POSITIVE_EVER_PP ==1 & ZIK_IGM_POSITIVE_EVER_PREG==1 ~ 1, 
                                              ZIK_IGG_POSITIVE_EVER_PP ==1 & ZIK_IGG_POSITIVE_EVER_PREG==1 ~ 2,
                                              ZIK_IGM_INCIDENT_PP ==1 & ZIK_IGG_INCIDENT_PP!=1 ~ 3,
                                              ZIK_IGM_INCIDENT_PP !=1 & ZIK_IGG_INCIDENT_PP==1 ~ 4, 
                                              TRUE ~ NA
  ))  

zik_export_labels <- zik_labels %>%
  select(SITE, MOMID, PREGID,
         ZIK_IGM_PERF_ENROLL, ZIK_IGM_POSITIVE_ENROLL, ZIK_IGM_POSITIVE_ENROLL_LABEL, 
         ZIK_IGG_PERF_ENROLL, ZIK_IGG_POSITIVE_ENROLL, ZIK_IGG_POSITIVE_ENROLL_LABEL,
         ZIK_IGM_PERF_EVER_PREG, ZIK_IGM_POSITIVE_EVER_PREG, ZIK_IGM_POSITIVE_EVER_PREG_LABEL,
         ZIK_DATE_IGM_POSITIVE, ZIK_GESTAGE_IGM_POSITIVE_DAYS, ZIK_GESTAGE_IGM_POSITIVE_WKS,
         ZIK_IGG_PERF_EVER_PREG, ZIK_IGG_POSITIVE_EVER_PREG, ZIK_IGG_POSITIVE_EVER_PREG_LABEL,
         ZIK_DATE_IGG_POSITIVE, ZIK_GESTAGE_IGG_POSITIVE_DAYS, ZIK_GESTAGE_IGG_POSITIVE_WKS,
         ZIK_IGG_DAYS_BETWEEN_TESTS, ZIK_IGG_WKS_BETWEEN_TESTS,
         ZIK_IGM_UNKNOWN_BASELINE, ZIK_IGM_UNKNOWN_BASELINE_LABEL, 
         ZIK_IGG_UNKNOWN_BASELINE, ZIK_IGG_UNKNOWN_BASELINE_LABEL,
         ZIK_IGM_INCIDENT,ZIK_IGM_INCIDENT_LABEL, ZIK_IGG_INCIDENT, ZIK_IGG_INCIDENT_LABEL,
         ZIK_POSITIVE_EVER_PREG, ZIK_POSITIVE_EVER_PREG_LABEL, 
         ZIK_POSITIVE_EVER_PREG_CAT, ZIK_POSITIVE_EVER_PREG_CAT_LABEL,
         ## 
         ZIK_IGM_INCIDENT_PP, ZIK_IGM_INCIDENT_PP_LABEL,
         ZIK_IGG_INCIDENT_PP, ZIK_IGG_INCIDENT_PP_LABEL, ZIK_IGM_PERF_EVER_PP, ZIK_IGG_PERF_EVER_PP,
         ZIK_POSITIVE_EVER_PP, ZIK_POSITIVE_EVER_PP_LABEL, ZIK_POSITIVE_EVER_PP_CAT, ZIK_POSITIVE_EVER_PP_CAT_LABEL,
         ZIK_DATE_IGM_POSITIVE_PP, ZIK_DATE_IGG_POSITIVE_PP, ZIK_GESTAGE_IGG_POSITIVE_DAYS_PP, ZIK_GESTAGE_IGG_POSITIVE_WKS_PP
  )

# write.xlsx(zik_table, paste0(path_to_save, "zik_table" ,".xlsx"), na="", rowNames=TRUE)

#*****************************************************************************
# Chikungunya ----
#*****************************************************************************

chk <- mat_enroll %>% 
  # merge in mnh08 to pull titer results 
  left_join(mnh08 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, ZCD_EXPANSION, M08_LBSTDAT,
                             M08_ZCD_LBPERF_5, M08_ZCD_CHKIGM_LBORRES, M08_ZCD_LBPERF_6, M08_ZCD_CHKIGG_LBORRES), 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  # merge in mnh07 for specimen collection date
  left_join(mnh07 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, M07_MAT_SPEC_COLLECT_DAT), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  filter(ZCD_EXPANSION==1) %>% 
  # Was test performed?
  mutate(CHK_IGM_PERF = case_when(M08_ZCD_LBPERF_5 ==1 ~ 1,
                                  is.na(M08_ZCD_LBPERF_5) ~ 55,
                                  TRUE ~ 0),
         CHK_IGG_PERF = case_when(M08_ZCD_LBPERF_6 ==1 ~ 1,
                                  is.na(M08_ZCD_LBPERF_6) ~ 55,
                                  TRUE ~ 0)
  ) %>%
  # Test result available? 
  mutate(CHK_IGM_RESULT_AVAI = case_when(M08_ZCD_CHKIGM_LBORRES %in% c(0,1,2) ~ 1, TRUE ~ 0),
         CHK_IGG_RESULT_AVAI = case_when(M08_ZCD_CHKIGG_LBORRES %in% c(0,1,2) ~ 1, TRUE ~ 0)
  ) %>% 
  # Test result by Titers
  mutate(CHK_IGM_POSITIVE = case_when(ZCD_EXPANSION==0 ~ 77, # 77, na; visit was before expansion date
                                      CHK_IGM_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_CHKIGM_LBORRES == 1 ~ 1, # 1, positive
                                      CHK_IGM_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_CHKIGM_LBORRES == 0 ~ 0, # 0, negative
                                      CHK_IGM_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_CHKIGM_LBORRES == 2 ~ 2, # 2, inconclusive
                                      CHK_IGM_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_CHKIGM_LBORRES %in% c(NA,55, 77, 99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      ZCD_EXPANSION ==1 & CHK_IGM_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                      TRUE ~ NA),
         CHK_IGG_POSITIVE = case_when(ZCD_EXPANSION==0 ~ 77, # 77, na; visit was before expansion date
                                      CHK_IGG_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_CHKIGG_LBORRES == 1 ~ 1, # 1, positive
                                      CHK_IGG_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_CHKIGG_LBORRES == 0 ~ 0, # 0, negative
                                      CHK_IGG_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_CHKIGG_LBORRES == 2 ~ 2, # 2, inconclusive
                                      CHK_IGG_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_CHKIGG_LBORRES %in% c(NA,55, 77, 99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      ZCD_EXPANSION ==1 & CHK_IGG_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                      TRUE ~ NA)) %>% 
  # Date positive test 
  mutate(DATE_IGM_POSITIVE = case_when(CHK_IGM_POSITIVE ==1 ~ ymd(M08_LBSTDAT), 
                                       TRUE ~ NA_Date_),
         DATE_IGG_POSITIVE = case_when(CHK_IGG_POSITIVE ==1 ~ ymd(M08_LBSTDAT), 
                                       TRUE ~ NA_Date_),
         
  ) %>% 
  # generate indicator variable to remove any unscheduled visits that do not have a test outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & (CHK_IGM_PERF ==1 | CHK_IGG_PERF ==1) & (CHK_IGM_RESULT_AVAI ==1 | CHK_IGG_RESULT_AVAI ==1) ~ 1, # keep if test was done and result is available
                                  TYPE_VISIT %in% c(13,14) & (CHK_IGM_POSITIVE %in% c(1,0,2) | CHK_IGG_POSITIVE %in% c(1,0,2)) ~ 1, ## keep if valid results (could probably remove)
                                  TYPE_VISIT %in% c(1,2,3,4,5) ~ 1, # keep if not an unscheduled visit 
                                  TYPE_VISIT %in% c(13,14) & CHK_IGM_POSITIVE %in% c(NA,55,77) & CHK_IGG_POSITIVE %in% c(NA,55,77) ~ 0, # remove if missing 
                                  TRUE ~ 0)) %>% 
  filter(KEEP_UNSCHED==1)

chk_export_anc <- chk %>% 
  filter(!TYPE_VISIT %in% c(6,7,8,9,10,11,12,14)) %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, VISIT_DATE, ZCD_EXPANSION, PREG_START_DATE, M08_LBSTDAT, DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, starts_with("CHK_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("CHK_"), VISIT_DATE, DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, M08_LBSTDAT),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(CHK_IGM_POSITIVE_ENROLL = CHK_IGM_POSITIVE_1,
         CHK_IGG_POSITIVE_ENROLL = CHK_IGG_POSITIVE_1,
         CHK_IGM_PERF_ENROLL = CHK_IGM_PERF_1,
         CHK_IGM_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("CHK_IGM_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                                any(na.omit(c_across(starts_with("CHK_IGM_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                                all(na.omit(c_across(starts_with("CHK_IGM_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                                all(na.omit(c_across(starts_with("CHK_IGM_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                TRUE ~ 77),
         CHK_IGM_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("CHK_IGM_POSITIVE_")) == 1) & CHK_IGM_POSITIVE_ENROLL %in% c(2, 55,77,99,NA)~ 1, # 1, Positive if  titers is + 
                                              TRUE ~ 0),
         CHK_IGM_INCIDENT = case_when(CHK_IGM_POSITIVE_EVER_PREG ==1 & CHK_IGM_POSITIVE_ENROLL ==0 ~ 1, 
                                      TRUE  ~ 0),
         CHK_IGM_PERF_EVER_PREG = case_when(any(c_across(starts_with("CHK_IGM_PERF_")) == 1) ~ 1, # 1, titers performed
                                            any(na.omit(c_across(starts_with("CHK_IGM_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                            all(na.omit(c_across(starts_with("CHK_IGM_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                            all(na.omit(c_across(starts_with("CHK_IGM_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                            TRUE ~ 77),
         CHK_IGM_POSITIVE_EVER_PREG_TEXT = case_when(CHK_IGM_POSITIVE_ENROLL ==1 & CHK_IGM_INCIDENT!=1 & CHK_IGM_UNKNOWN_BASELINE!=1 ~ "Baseline IgM+", 
                                                     CHK_IGM_POSITIVE_ENROLL !=1 & CHK_IGM_INCIDENT==1 & CHK_IGM_UNKNOWN_BASELINE!=1 ~ "Incident IgM+", 
                                                     CHK_IGM_POSITIVE_ENROLL !=1 & CHK_IGM_INCIDENT!=1 & CHK_IGM_UNKNOWN_BASELINE==1 ~ "IgM+ with unknown baseline",
                                                     TRUE ~ NA),
         CHK_IGG_PERF_ENROLL = CHK_IGG_PERF_1,
         CHK_IGG_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("CHK_IGG_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                                any(na.omit(c_across(starts_with("CHK_IGG_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                                all(na.omit(c_across(starts_with("CHK_IGG_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                                all(na.omit(c_across(starts_with("CHK_IGG_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                TRUE ~ 77),
         CHK_IGG_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("CHK_IGG_POSITIVE_")) == 1) & CHK_IGG_POSITIVE_ENROLL %in% c(2, 55,77,99,NA) & CHK_IGM_POSITIVE_ENROLL %in% c(2, 55,77,99,NA) ~ 1, # 1, Positive if  titers is + 
                                              TRUE ~ 0),
         CHK_IGG_INCIDENT = case_when(CHK_IGG_POSITIVE_EVER_PREG ==1 & CHK_IGG_POSITIVE_ENROLL ==0 & CHK_IGM_POSITIVE_ENROLL ==0 ~ 1, 
                                      TRUE  ~ 0),
         CHK_IGG_POSITIVE_EVER_PREG_TEXT = case_when(CHK_IGG_POSITIVE_ENROLL ==1 & CHK_IGG_INCIDENT!=1 & CHK_IGG_UNKNOWN_BASELINE!=1 ~ "Baseline IgG+", 
                                                     CHK_IGG_POSITIVE_ENROLL !=1 & CHK_IGG_INCIDENT==1 & CHK_IGG_UNKNOWN_BASELINE!=1 ~ "Incident IgG+", 
                                                     CHK_IGG_POSITIVE_ENROLL !=1 & CHK_IGG_INCIDENT!=1 & CHK_IGG_UNKNOWN_BASELINE==1 ~ "IgG+ with unknown baseline",
                                                     TRUE ~ NA),
         CHK_IGG_PERF_EVER_PREG = case_when(any(c_across(starts_with("CHK_IGG_PERF_")) == 1) ~ 1, # 1, titers performed
                                            any(na.omit(c_across(starts_with("CHK_IGG_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                            all(na.omit(c_across(starts_with("CHK_IGG_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                            all(na.omit(c_across(starts_with("CHK_IGG_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                            TRUE ~ 77)
         
         
  ) %>% 
  mutate(CHK_POSITIVE_EVER_PREG = case_when(CHK_IGM_POSITIVE_EVER_PREG ==1 | CHK_IGG_INCIDENT==1 ~ 1, ## EVER PREG variable is CHKa IgM+ ever during pregnancy or incident IgG
                                            CHK_IGM_POSITIVE_EVER_PREG ==0 & CHK_IGG_INCIDENT== 0 ~ 0, 
                                            CHK_IGM_PERF_EVER_PREG==0 & CHK_IGG_PERF_EVER_PREG==0 ~ 77, # ## if test performed but no infection, these are NOs
                                            TRUE ~ 77)) %>%   
  mutate(CHK_DATE_IGM_POSITIVE = do.call(pmin, c(across(starts_with("DATE_IGM_POSITIVE_")), na.rm = TRUE)),
         CHK_GESTAGE_IGM_POSITIVE_DAYS = as.numeric(CHK_DATE_IGM_POSITIVE-ymd(PREG_START_DATE)),
         CHK_GESTAGE_IGM_POSITIVE_WKS = CHK_GESTAGE_IGM_POSITIVE_DAYS %/% 7
  ) %>%
  mutate(CHK_DATE_IGG_POSITIVE = do.call(pmin, c(across(starts_with("DATE_IGG_POSITIVE_")), na.rm = TRUE)),
         CHK_GESTAGE_IGG_POSITIVE_DAYS = as.numeric(CHK_DATE_IGG_POSITIVE-ymd(PREG_START_DATE)),
         CHK_GESTAGE_IGG_POSITIVE_WKS = CHK_GESTAGE_IGG_POSITIVE_DAYS %/% 7
  ) %>%
  ## generate indicator if incident IGG
  mutate(CHK_INCIDENT_IGG_POSITIVE = case_when(CHK_IGG_POSITIVE_ENROLL==0 & CHK_IGG_POSITIVE_EVER_PREG==1 ~ 1, TRUE ~ 0),
         CHK_IGG_DAYS_BETWEEN_TESTS = case_when(CHK_INCIDENT_IGG_POSITIVE==1 ~ as.numeric(ymd(CHK_DATE_IGG_POSITIVE) - ymd(M08_LBSTDAT_1))),
         CHK_IGG_WKS_BETWEEN_TESTS = CHK_IGG_DAYS_BETWEEN_TESTS %/% 7
  )

chk_export_pnc <- chk %>% 
  filter(TYPE_VISIT %in% c(6,7,8,9,10,11,12,14)) %>% 
  left_join(mat_end %>% select("SITE", "MOMID", "PREGID", "PREG_END_DATE") %>% mutate(PREG_END_DATE = ymd(PREG_END_DATE)), by = c("SITE", "MOMID", "PREGID")) %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, VISIT_DATE, ZCD_EXPANSION,PREG_END_DATE,DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, M08_LBSTDAT, starts_with("CHK_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("CHK_"), VISIT_DATE, DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, M08_LBSTDAT),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # merge in enrollment ever preg variables 
  left_join(chk_export_anc %>% select(SITE, MOMID, PREGID, CHK_POSITIVE_EVER_PREG, CHK_IGM_POSITIVE_EVER_PREG, CHK_IGG_POSITIVE_EVER_PREG), by= c("SITE", "MOMID", "PREGID")) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(CHK_IGM_POSITIVE_EVER_PP = case_when(any(c_across(starts_with("CHK_IGM_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                              any(na.omit(c_across(starts_with("CHK_IGM_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                              all(na.omit(c_across(starts_with("CHK_IGM_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                              all(na.omit(c_across(starts_with("CHK_IGM_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                              TRUE ~ 77),
         CHK_IGG_POSITIVE_EVER_PP = case_when(any(c_across(starts_with("CHK_IGG_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                              any(na.omit(c_across(starts_with("CHK_IGG_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                              all(na.omit(c_across(starts_with("CHK_IGG_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                              all(na.omit(c_across(starts_with("CHK_IGG_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                              TRUE ~ 77),
         CHK_IGM_INCIDENT_PP = case_when(CHK_IGM_POSITIVE_EVER_PP ==1 & CHK_IGM_POSITIVE_EVER_PREG ==0 ~ 1, 
                                         TRUE  ~ 0),
         CHK_IGG_INCIDENT_PP = case_when(CHK_IGG_POSITIVE_EVER_PP ==1 & CHK_IGG_POSITIVE_EVER_PREG ==0 ~ 1, 
                                         TRUE  ~ 0),
         CHK_IGM_PERF_EVER_PP = case_when(any(c_across(starts_with("CHK_IGM_PERF_")) == 1) ~ 1, # 1, titers performed
                                          any(na.omit(c_across(starts_with("CHK_IGM_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                          all(na.omit(c_across(starts_with("CHK_IGM_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                          all(na.omit(c_across(starts_with("CHK_IGM_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                          TRUE ~ 77),
         CHK_IGG_PERF_EVER_PP = case_when(any(c_across(starts_with("CHK_IGG_PERF_")) == 1) ~ 1, # 1, titers performed
                                          any(na.omit(c_across(starts_with("CHK_IGG_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                          all(na.omit(c_across(starts_with("CHK_IGG_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                          all(na.omit(c_across(starts_with("CHK_IGG_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                          TRUE ~ 77),
         
         CHK_IGM_POSITIVE_EVER_PP_TEXT = case_when(CHK_IGM_POSITIVE_EVER_PP ==1 ~ "Postpartum IgM+", 
                                                   CHK_IGM_INCIDENT_PP == 1 ~ "Incident IgM+ during Postpartum", 
                                                   TRUE ~ NA),
         
         CHK_IGG_POSITIVE_EVER_PP_TEXT = case_when(CHK_IGG_POSITIVE_EVER_PP ==1 ~ "Postpartum IgG+", 
                                                   CHK_IGG_INCIDENT_PP == 1 ~ "Incident IgG+ during Postpartum", 
                                                   TRUE ~ NA)
  ) %>% 
  mutate(CHK_POSITIVE_EVER_PP = case_when(CHK_IGM_POSITIVE_EVER_PP ==1 | CHK_IGM_INCIDENT_PP==1 ~ 1, ## EVER PREG variable is zika IgM+ ever during pregnancy or incident IgG
                                          CHK_IGM_POSITIVE_EVER_PP ==0 & CHK_IGG_INCIDENT_PP== 0 ~ 0, 
                                          CHK_IGM_PERF_EVER_PP==0 & CHK_IGG_PERF_EVER_PP==0 ~ 77, # ## if test performed but no infection, these are NOs
                                          TRUE ~ 77) 
         
  ) %>% 
  mutate(CHK_DATE_IGM_POSITIVE_PP = do.call(pmin, c(across(starts_with("DATE_IGM_POSITIVE_")), na.rm = TRUE)),
         CHK_GESTAGE_IGM_POSITIVE_DAYS_PP = as.numeric(CHK_DATE_IGM_POSITIVE_PP-ymd(PREG_END_DATE)),
         CHK_GESTAGE_IGM_POSITIVE_WKS_PP = CHK_GESTAGE_IGM_POSITIVE_DAYS_PP %/% 7
  ) %>%
  mutate(CHK_DATE_IGG_POSITIVE_PP = do.call(pmin, c(across(starts_with("DATE_IGG_POSITIVE_")), na.rm = TRUE)),
         CHK_GESTAGE_IGG_POSITIVE_DAYS_PP = as.numeric(CHK_DATE_IGG_POSITIVE_PP-ymd(PREG_END_DATE)),
         CHK_GESTAGE_IGG_POSITIVE_WKS_PP = CHK_GESTAGE_IGG_POSITIVE_DAYS_PP %/% 7
  ) %>% 
  select(SITE, MOMID, PREGID, contains("_PP"))


## add labels
chk_labels <- chk_export_anc %>%
  left_join(chk_export_pnc, by = c("SITE", "MOMID", "PREGID")) %>% 
  
  mutate(
    CHK_IGM_POSITIVE_ENROLL_LABEL = factor(CHK_IGM_POSITIVE_ENROLL,levels = c(1, 0, 2, 55, 77),
                                           labels = c("CHK IgM+ at enrollment", "CHK IgM- at enrollment", "CHK IgM inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    CHK_IGG_POSITIVE_ENROLL_LABEL = factor(CHK_IGG_POSITIVE_ENROLL,levels = c(1, 0, 2, 55, 77),
                                           labels = c("CHK IgG+ at enrollment", "CHK IgG- at enrollment", "CHK IgG inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    CHK_IGM_UNKNOWN_BASELINE_LABEL = factor(CHK_IGM_UNKNOWN_BASELINE,levels = c(1, 0),
                                            labels = c("CHK IgM+ during pregnancy but missing baseline status", "No incident chikungunya infection with unknown baseline status")),
    CHK_IGG_UNKNOWN_BASELINE_LABEL = factor(CHK_IGG_UNKNOWN_BASELINE,levels = c(1, 0),
                                            labels = c("CHK IgG+ during pregnancy but missing baseline status", "No incident chikungunya infection with unknown baseline status")),
    CHK_IGM_INCIDENT_LABEL = factor(CHK_IGM_INCIDENT,levels = c(1, 0),
                                    labels = c("Incident CHK IgG+ during pregnancy", "No incident chikungunya IgG+ infection")),
    CHK_IGG_INCIDENT_LABEL = factor(CHK_IGG_INCIDENT,levels = c(1, 0),
                                    labels = c("Incident CHK IgG+ during pregnancy", "No incident chikungunya IgG+ infection")),
    CHK_IGM_INCIDENT_PP_LABEL = factor(CHK_IGM_INCIDENT_PP,levels = c(1, 0),
                                       labels = c("Incident CHK IgM+ during postpartum", "No incident Chikungunya infection during postpartum")),
    CHK_IGG_INCIDENT_PP_LABEL = factor(CHK_IGG_INCIDENT_PP,levels = c(1, 0),
                                       labels = c("Incident CHK IgG+ during postpartum", "No incident Chikungunya IgG+ infection during postpartum")),
    CHK_IGM_POSITIVE_EVER_PREG_LABEL = factor(CHK_IGM_POSITIVE_EVER_PREG,levels = c(1, 0, 2, 55, 77),
                                              labels = c("CHK IgM+ ever during pregnancy", "CHK IgM never during pregnancy", "CHK IgM inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    CHK_IGG_POSITIVE_EVER_PREG_LABEL = factor(CHK_IGG_POSITIVE_EVER_PREG,levels = c(1, 0, 2, 55, 77),
                                              labels = c("CHK IgG+ ever during pregnancy", "CHK IgG never during pregnancy", "CHK IgG inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    CHK_POSITIVE_EVER_PREG_LABEL = factor(CHK_POSITIVE_EVER_PREG,levels = c(1, 0, 77),
                                          labels = c("Chikungunya ever during pregnancy", "Chikungunya never during pregnancy", "NA/no test performed")),
    CHK_IGM_POSITIVE_EVER_PP_LABEL = factor(CHK_IGM_POSITIVE_EVER_PP,levels = c(1, 0, 2, 55, 77),
                                            labels = c("CHK IgM+ ever during postpartum", "CHK IgM never during postpartum", "CHK IgM inconclusive during postpartum", "Test performed but missing test result", "NA/no test performed")),
    CHK_IGG_POSITIVE_EVER_PP_LABEL = factor(CHK_IGG_POSITIVE_EVER_PP,levels = c(1, 0, 2, 55, 77),
                                            labels = c("CHK IgG+ ever during postpartum", "CHK IgG never during postpartum", "CHK IgG inconclusive during postpartum", "Test performed but missing test result", "NA/no test performed")),
    CHK_POSITIVE_EVER_PP_LABEL = factor(CHK_POSITIVE_EVER_PP,levels = c(1, 0, 77),
                                        labels = c("Chikungunya ever during postpartum", "Chikungunya never during postpartum", "NA/no test performed"))
  ) %>% 
  mutate(CHK_POSITIVE_EVER_PREG_CAT_LABEL = case_when(CHK_IGM_POSITIVE_ENROLL ==1 & CHK_IGM_INCIDENT!=1 & CHK_IGM_UNKNOWN_BASELINE!=1 & CHK_IGG_POSITIVE_ENROLL != 1 ~ "Baseline IgM+",
                                                      CHK_IGG_POSITIVE_ENROLL ==1 & CHK_IGG_INCIDENT!=1 & CHK_IGG_UNKNOWN_BASELINE!=1 & CHK_IGM_POSITIVE_ENROLL != 1 ~ "Baseline IgG+",
                                                      CHK_IGM_POSITIVE_ENROLL ==1 & CHK_IGM_INCIDENT!=1 & CHK_IGM_UNKNOWN_BASELINE!=1 & CHK_IGG_POSITIVE_ENROLL == 1 ~ "Baseline IgM+ & IgG+",
                                                      CHK_IGM_POSITIVE_ENROLL !=1 & CHK_IGM_INCIDENT==1 & CHK_IGM_UNKNOWN_BASELINE!=1 & CHK_IGG_INCIDENT != 1 ~ "Incident IgM+",
                                                      CHK_IGG_POSITIVE_ENROLL !=1 & CHK_IGG_INCIDENT==1 & CHK_IGG_UNKNOWN_BASELINE!=1 & CHK_IGM_INCIDENT != 1 ~ "Incident IgG+",
                                                      CHK_IGM_POSITIVE_ENROLL !=1 & CHK_IGM_INCIDENT==1 & CHK_IGM_UNKNOWN_BASELINE!=1 & CHK_IGG_INCIDENT == 1 ~ "Incident IgM+ & IgG+",
                                                      CHK_IGM_POSITIVE_ENROLL !=1 & CHK_IGM_INCIDENT!=1 & CHK_IGM_UNKNOWN_BASELINE==1 & CHK_IGG_UNKNOWN_BASELINE != 1 ~ "IgM+ with unknown baseline",
                                                      CHK_IGG_POSITIVE_ENROLL !=1 & CHK_IGG_INCIDENT!=1 & CHK_IGG_UNKNOWN_BASELINE==1 & CHK_IGM_UNKNOWN_BASELINE != 1 ~ "IgG+ with unknown baseline",
                                                      CHK_IGM_POSITIVE_ENROLL !=1 & CHK_IGM_INCIDENT!=1 & CHK_IGM_UNKNOWN_BASELINE==1 & CHK_IGG_UNKNOWN_BASELINE == 1 ~ "IgM+ & IgG+ with unknown baseline",
                                                      TRUE ~ NA
  )) %>% 
  mutate(CHK_POSITIVE_EVER_PREG_CAT = case_when(CHK_IGM_POSITIVE_ENROLL ==1 & CHK_IGM_INCIDENT!=1 & CHK_IGM_UNKNOWN_BASELINE!=1 & CHK_IGG_POSITIVE_ENROLL != 1 ~ 1,
                                                CHK_IGG_POSITIVE_ENROLL ==1 & CHK_IGG_INCIDENT!=1 & CHK_IGG_UNKNOWN_BASELINE!=1 & CHK_IGM_POSITIVE_ENROLL != 1 ~ 2,
                                                CHK_IGM_POSITIVE_ENROLL ==1 & CHK_IGM_INCIDENT!=1 & CHK_IGM_UNKNOWN_BASELINE!=1 & CHK_IGG_POSITIVE_ENROLL == 1 ~ 3,
                                                CHK_IGM_POSITIVE_ENROLL !=1 & CHK_IGM_INCIDENT==1 & CHK_IGM_UNKNOWN_BASELINE!=1 & CHK_IGG_INCIDENT != 1 ~ 4,
                                                CHK_IGG_POSITIVE_ENROLL !=1 & CHK_IGG_INCIDENT==1 & CHK_IGG_UNKNOWN_BASELINE!=1 & CHK_IGM_INCIDENT != 1 ~ 5,
                                                CHK_IGM_POSITIVE_ENROLL !=1 & CHK_IGM_INCIDENT==1 & CHK_IGM_UNKNOWN_BASELINE!=1 & CHK_IGG_INCIDENT == 1 ~ 6,
                                                CHK_IGM_POSITIVE_ENROLL !=1 & CHK_IGM_INCIDENT!=1 & CHK_IGM_UNKNOWN_BASELINE==1 & CHK_IGG_UNKNOWN_BASELINE != 1 ~ 7,
                                                CHK_IGG_POSITIVE_ENROLL !=1 & CHK_IGG_INCIDENT!=1 & CHK_IGG_UNKNOWN_BASELINE==1 & CHK_IGM_UNKNOWN_BASELINE != 1 ~ 8,
                                                CHK_IGM_POSITIVE_ENROLL !=1 & CHK_IGM_INCIDENT!=1 & CHK_IGM_UNKNOWN_BASELINE==1 & CHK_IGG_UNKNOWN_BASELINE == 1 ~ 9,
                                                TRUE ~ NA
  )) %>% 
  mutate(CHK_POSITIVE_EVER_PP_CAT_LABEL = case_when(CHK_IGM_POSITIVE_EVER_PP ==1 & CHK_IGM_POSITIVE_EVER_PREG==1 ~ "Chikungunya IgM+ during pregnancy and postpartum", 
                                                    CHK_IGG_POSITIVE_EVER_PP ==1 & CHK_IGG_POSITIVE_EVER_PREG==1 ~ "Chikungunya IgG+ during pregnancy and postpartum", 
                                                    CHK_IGM_INCIDENT_PP ==1 & CHK_IGG_INCIDENT_PP!=1 ~ "Incident Chikungunya IgM+ during postpartum", 
                                                    CHK_IGM_INCIDENT_PP !=1 & CHK_IGG_INCIDENT_PP==1 ~ "Incident Chikungunya IgG+ during postpartum", 
                                                    TRUE ~ NA
  )) %>% 
  mutate(CHK_POSITIVE_EVER_PP_CAT = case_when(CHK_IGM_POSITIVE_EVER_PP ==1 & CHK_IGM_POSITIVE_EVER_PREG==1 ~ 1, 
                                              CHK_IGG_POSITIVE_EVER_PP ==1 & CHK_IGG_POSITIVE_EVER_PREG==1 ~ 2,
                                              CHK_IGM_INCIDENT_PP ==1 & CHK_IGG_INCIDENT_PP!=1 ~ 3,
                                              CHK_IGM_INCIDENT_PP !=1 & CHK_IGG_INCIDENT_PP==1 ~ 4, 
                                              TRUE ~ NA
  ))  


chk_export_labels <- chk_labels %>%
  select(SITE, MOMID, PREGID,
         CHK_IGM_PERF_ENROLL, CHK_IGM_POSITIVE_ENROLL, CHK_IGM_POSITIVE_ENROLL_LABEL, 
         CHK_IGG_PERF_ENROLL, CHK_IGG_POSITIVE_ENROLL, CHK_IGG_POSITIVE_ENROLL_LABEL,
         CHK_IGM_PERF_EVER_PREG, CHK_IGM_POSITIVE_EVER_PREG, CHK_IGM_POSITIVE_EVER_PREG_LABEL,
         CHK_DATE_IGM_POSITIVE, CHK_GESTAGE_IGM_POSITIVE_DAYS, CHK_GESTAGE_IGM_POSITIVE_WKS,
         CHK_IGG_PERF_EVER_PREG, CHK_IGG_POSITIVE_EVER_PREG, CHK_IGG_POSITIVE_EVER_PREG_LABEL,
         CHK_DATE_IGG_POSITIVE, CHK_GESTAGE_IGG_POSITIVE_DAYS, CHK_GESTAGE_IGG_POSITIVE_WKS,
         CHK_IGG_DAYS_BETWEEN_TESTS, CHK_IGG_WKS_BETWEEN_TESTS,
         CHK_IGM_UNKNOWN_BASELINE, CHK_IGM_UNKNOWN_BASELINE_LABEL, 
         CHK_IGG_UNKNOWN_BASELINE, CHK_IGG_UNKNOWN_BASELINE_LABEL,
         CHK_IGM_INCIDENT, CHK_IGM_INCIDENT_LABEL, CHK_IGG_INCIDENT, CHK_IGG_INCIDENT_LABEL, 
         CHK_POSITIVE_EVER_PREG, CHK_POSITIVE_EVER_PREG_LABEL, 
         CHK_POSITIVE_EVER_PREG_CAT, CHK_POSITIVE_EVER_PREG_CAT_LABEL,
         ## 
         CHK_IGM_INCIDENT_PP, CHK_IGM_INCIDENT_PP_LABEL,
         CHK_IGG_INCIDENT_PP, CHK_IGG_INCIDENT_PP_LABEL, CHK_IGM_PERF_EVER_PP, CHK_IGG_PERF_EVER_PP,
         CHK_POSITIVE_EVER_PP, CHK_POSITIVE_EVER_PP_LABEL, CHK_POSITIVE_EVER_PP_CAT, CHK_POSITIVE_EVER_PP_CAT_LABEL,
         CHK_DATE_IGM_POSITIVE_PP, CHK_DATE_IGG_POSITIVE_PP, CHK_GESTAGE_IGG_POSITIVE_DAYS_PP, CHK_GESTAGE_IGG_POSITIVE_WKS_PP
  )


#*****************************************************************************
# Dengue ----
#*****************************************************************************

den <- mat_enroll %>% 
  # merge in mnh08 to pull titer results 
  left_join(mnh08 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, ZCD_EXPANSION, M08_LBSTDAT,
                             M08_ZCD_LBPERF_3, M08_ZCD_DENIGM_LBORRES, M08_ZCD_LBPERF_4, M08_ZCD_DENIGG_LBORRES), 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  # merge in mnh07 for specimen collection date
  left_join(mnh07 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, M07_MAT_SPEC_COLLECT_DAT), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  filter(ZCD_EXPANSION==1) %>% 
  # Was test performed?
  mutate(DEN_IGM_PERF = case_when(M08_ZCD_LBPERF_3 ==1 ~ 1,
                                  is.na(M08_ZCD_LBPERF_3) ~ 55,
                                  TRUE ~ 0),
         DEN_IGG_PERF = case_when(M08_ZCD_LBPERF_4 ==1 ~ 1,
                                  is.na(M08_ZCD_LBPERF_4) ~ 55,
                                  TRUE ~ 0)
  ) %>%
  # Test result available? 
  mutate(DEN_IGM_RESULT_AVAI = case_when(M08_ZCD_DENIGM_LBORRES %in% c(0,1,2) ~ 1, TRUE ~ 0),
         DEN_IGG_RESULT_AVAI = case_when(M08_ZCD_DENIGG_LBORRES %in% c(0,1,2) ~ 1, TRUE ~ 0)
  ) %>% 
  # Test result by Titers
  mutate(DEN_IGM_POSITIVE = case_when(ZCD_EXPANSION==0 ~ 77, # 77, na; visit was before expansion date
                                      DEN_IGM_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_DENIGM_LBORRES == 1 ~ 1, # 1, positive
                                      DEN_IGM_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_DENIGM_LBORRES == 0 ~ 0, # 0, negative
                                      DEN_IGM_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_DENIGM_LBORRES == 2 ~ 2, # 2, inconclusive
                                      DEN_IGM_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_DENIGM_LBORRES %in% c(NA,55, 77, 99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      ZCD_EXPANSION ==1 & DEN_IGM_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                      TRUE ~ NA),
         DEN_IGG_POSITIVE = case_when(ZCD_EXPANSION==0 ~ 77, # 77, na; visit was before expansion date
                                      DEN_IGG_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_DENIGG_LBORRES == 1 ~ 1, # 1, positive
                                      DEN_IGG_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_DENIGG_LBORRES == 0 ~ 0, # 0, negative
                                      DEN_IGG_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_DENIGG_LBORRES == 2 ~ 2, # 2, inconclusive
                                      DEN_IGG_PERF ==1 & ZCD_EXPANSION == 1 & M08_ZCD_DENIGG_LBORRES %in% c(NA,55, 77, 99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      ZCD_EXPANSION ==1 & DEN_IGG_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                      TRUE ~ NA)) %>% 
  # Date positive test 
  mutate(DATE_IGM_POSITIVE = case_when(DEN_IGM_POSITIVE ==1 ~ ymd(M08_LBSTDAT), 
                                       TRUE ~ NA_Date_),
         DATE_IGG_POSITIVE = case_when(DEN_IGG_POSITIVE ==1 ~ ymd(M08_LBSTDAT), 
                                       TRUE ~ NA_Date_),
         
  ) %>% 
  # generate indicator variable to remove any unscheduled visits that do not have a test outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & (DEN_IGM_PERF ==1 | DEN_IGG_PERF ==1) & (DEN_IGM_RESULT_AVAI ==1 | DEN_IGG_RESULT_AVAI ==1) ~ 1, # keep if test was done and result is available
                                  TYPE_VISIT %in% c(13,14) & (DEN_IGM_POSITIVE %in% c(1,0,2) | DEN_IGG_POSITIVE %in% c(1,0,2)) ~ 1, ## keep if valid results (could probably remove)
                                  TYPE_VISIT %in% c(1,2,3,4,5) ~ 1, # keep if not an unscheduled visit 
                                  TYPE_VISIT %in% c(13,14) & DEN_IGM_POSITIVE %in% c(NA,55,77) & DEN_IGG_POSITIVE %in% c(NA,55,77) ~ 0, # remove if missing 
                                  TRUE ~ 0)) %>% 
  filter(KEEP_UNSCHED==1)

den_export_anc <- den %>% 
  filter(!TYPE_VISIT %in% c(6,7,8,9,10,11,12,14)) %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, VISIT_DATE, ZCD_EXPANSION, PREG_START_DATE, M08_LBSTDAT, DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, starts_with("DEN_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("DEN_"), VISIT_DATE, DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, M08_LBSTDAT),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(DEN_IGM_POSITIVE_ENROLL = DEN_IGM_POSITIVE_1,
         DEN_IGG_POSITIVE_ENROLL = DEN_IGG_POSITIVE_1,
         DEN_IGM_PERF_ENROLL = DEN_IGM_PERF_1, 
         DEN_IGM_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("DEN_IGM_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                                any(na.omit(c_across(starts_with("DEN_IGM_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                                all(na.omit(c_across(starts_with("DEN_IGM_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                                all(na.omit(c_across(starts_with("DEN_IGM_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                TRUE ~ 77),
         DEN_IGM_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("DEN_IGM_POSITIVE_")) == 1) & DEN_IGM_POSITIVE_ENROLL %in% c(2, 55,77,99,NA)~ 1, # 1, Positive if  titers is + 
                                              TRUE ~ 0),
         DEN_IGM_INCIDENT = case_when(DEN_IGM_POSITIVE_EVER_PREG ==1 & DEN_IGM_POSITIVE_ENROLL ==0 ~ 1, 
                                      TRUE  ~ 0),
         DEN_IGM_PERF_EVER_PREG = case_when(any(c_across(starts_with("DEN_IGM_PERF_")) == 1) ~ 1, # 1, titers performed
                                            any(na.omit(c_across(starts_with("DEN_IGM_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                            all(na.omit(c_across(starts_with("DEN_IGM_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                            all(na.omit(c_across(starts_with("DEN_IGM_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                            TRUE ~ 77),
         DEN_IGM_POSITIVE_EVER_PREG_TEXT = case_when(DEN_IGM_POSITIVE_ENROLL ==1 & DEN_IGM_INCIDENT!=1 & DEN_IGM_UNKNOWN_BASELINE!=1 ~ "Baseline IgM+", 
                                                     DEN_IGM_POSITIVE_ENROLL !=1 & DEN_IGM_INCIDENT==1 & DEN_IGM_UNKNOWN_BASELINE!=1 ~ "Incident IgM+", 
                                                     DEN_IGM_POSITIVE_ENROLL !=1 & DEN_IGM_INCIDENT!=1 & DEN_IGM_UNKNOWN_BASELINE==1 ~ "IgM+ with unknown baseline",
                                                     TRUE ~ NA),
         DEN_IGG_PERF_ENROLL = DEN_IGG_PERF_1,
         DEN_IGG_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("DEN_IGG_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                                any(na.omit(c_across(starts_with("DEN_IGG_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                                all(na.omit(c_across(starts_with("DEN_IGG_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                                all(na.omit(c_across(starts_with("DEN_IGG_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                TRUE ~ 77),
         DEN_IGG_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("DEN_IGG_POSITIVE_")) == 1) & DEN_IGG_POSITIVE_ENROLL %in% c(2, 55,77,99,NA) & DEN_IGM_POSITIVE_ENROLL %in% c(2, 55,77,99,NA) ~ 1, # 1, Positive if  titers is + 
                                              TRUE ~ 0),
         DEN_IGG_INCIDENT = case_when(DEN_IGG_POSITIVE_EVER_PREG ==1 & DEN_IGG_POSITIVE_ENROLL ==0 & DEN_IGM_POSITIVE_ENROLL ==0 ~ 1, 
                                      TRUE  ~ 0),
         DEN_IGG_POSITIVE_EVER_PREG_TEXT = case_when(DEN_IGG_POSITIVE_ENROLL ==1 & DEN_IGG_INCIDENT!=1 & DEN_IGG_UNKNOWN_BASELINE!=1 ~ "Baseline IgG+", 
                                                     DEN_IGG_POSITIVE_ENROLL !=1 & DEN_IGG_INCIDENT==1 & DEN_IGG_UNKNOWN_BASELINE!=1 ~ "Incident IgG+", 
                                                     DEN_IGG_POSITIVE_ENROLL !=1 & DEN_IGG_INCIDENT!=1 & DEN_IGG_UNKNOWN_BASELINE==1 ~ "IgG+ with unknown baseline",
                                                     TRUE ~ NA),
         DEN_IGG_PERF_EVER_PREG = case_when(any(c_across(starts_with("DEN_IGG_PERF_")) == 1) ~ 1, # 1, titers performed
                                            any(na.omit(c_across(starts_with("DEN_IGG_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                            all(na.omit(c_across(starts_with("DEN_IGG_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                            all(na.omit(c_across(starts_with("DEN_IGG_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                            TRUE ~ 77)
         
         
  ) %>%
  mutate(DEN_POSITIVE_EVER_PREG = case_when(DEN_IGM_POSITIVE_EVER_PREG ==1 | DEN_IGG_INCIDENT==1 ~ 1, ## EVER PREG variable is DENGUE IgM+ ever during pregnancy or incident IgG
                                            DEN_IGM_POSITIVE_EVER_PREG ==0 & DEN_IGG_INCIDENT== 0 ~ 0, 
                                            DEN_IGM_PERF_EVER_PREG==0 & DEN_IGG_PERF_EVER_PREG==0 ~ 77, # ## if test performed but no infection, these are NOs
                                            TRUE ~ 77)) %>%   
  mutate(DEN_DATE_IGM_POSITIVE = do.call(pmin, c(across(starts_with("DATE_IGM_POSITIVE_")), na.rm = TRUE)),
         DEN_GESTAGE_IGM_POSITIVE_DAYS = as.numeric(DEN_DATE_IGM_POSITIVE-ymd(PREG_START_DATE)),
         DEN_GESTAGE_IGM_POSITIVE_WKS = DEN_GESTAGE_IGM_POSITIVE_DAYS %/% 7
  ) %>%
  mutate(DEN_DATE_IGG_POSITIVE = do.call(pmin, c(across(starts_with("DATE_IGG_POSITIVE_")), na.rm = TRUE)),
         DEN_GESTAGE_IGG_POSITIVE_DAYS = as.numeric(DEN_DATE_IGG_POSITIVE-ymd(PREG_START_DATE)),
         DEN_GESTAGE_IGG_POSITIVE_WKS = DEN_GESTAGE_IGG_POSITIVE_DAYS %/% 7
  ) %>%
  mutate(DEN_INCIDENT_IGG_POSITIVE = case_when(DEN_IGG_POSITIVE_ENROLL==0 & DEN_IGG_POSITIVE_EVER_PREG==1 ~ 1, TRUE ~ 0),
         DEN_IGG_DAYS_BETWEEN_TESTS = case_when(DEN_INCIDENT_IGG_POSITIVE==1 ~ as.numeric(ymd(DEN_DATE_IGG_POSITIVE) - ymd(M08_LBSTDAT_1))),
         DEN_IGG_WKS_BETWEEN_TESTS = DEN_IGG_DAYS_BETWEEN_TESTS %/% 7
  ) 


den_export_pnc <- den %>% 
  filter(TYPE_VISIT %in% c(6,7,8,9,10,11,12,14)) %>% 
  left_join(mat_end %>% select("SITE", "MOMID", "PREGID", "PREG_END_DATE") %>% mutate(PREG_END_DATE = ymd(PREG_END_DATE)), by = c("SITE", "MOMID", "PREGID")) %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, VISIT_DATE, ZCD_EXPANSION,PREG_END_DATE,DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, M08_LBSTDAT, starts_with("DEN_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("DEN_"), VISIT_DATE, DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, M08_LBSTDAT),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # merge in enrollment ever preg variables 
  left_join(den_export_anc %>% select(SITE, MOMID, PREGID, DEN_POSITIVE_EVER_PREG, DEN_IGM_POSITIVE_EVER_PREG, DEN_IGG_POSITIVE_EVER_PREG), by= c("SITE", "MOMID", "PREGID")) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(DEN_IGM_POSITIVE_EVER_PP = case_when(any(c_across(starts_with("DEN_IGM_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                              any(na.omit(c_across(starts_with("DEN_IGM_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                              all(na.omit(c_across(starts_with("DEN_IGM_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                              all(na.omit(c_across(starts_with("DEN_IGM_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                              TRUE ~ 77),
         DEN_IGG_POSITIVE_EVER_PP = case_when(any(c_across(starts_with("DEN_IGG_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                              any(na.omit(c_across(starts_with("DEN_IGG_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                              all(na.omit(c_across(starts_with("DEN_IGG_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                              all(na.omit(c_across(starts_with("DEN_IGG_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                              TRUE ~ 77),
         DEN_IGM_INCIDENT_PP = case_when(DEN_IGM_POSITIVE_EVER_PP ==1 & DEN_IGM_POSITIVE_EVER_PREG ==0 ~ 1, 
                                         TRUE  ~ 0),
         DEN_IGG_INCIDENT_PP = case_when(DEN_IGG_POSITIVE_EVER_PP ==1 & DEN_IGG_POSITIVE_EVER_PREG ==0 ~ 1, 
                                         TRUE  ~ 0),
         DEN_IGM_PERF_EVER_PP = case_when(any(c_across(starts_with("DEN_IGM_PERF_")) == 1) ~ 1, # 1, titers performed
                                          any(na.omit(c_across(starts_with("DEN_IGM_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                          all(na.omit(c_across(starts_with("DEN_IGM_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                          all(na.omit(c_across(starts_with("DEN_IGM_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                          TRUE ~ 77),
         DEN_IGG_PERF_EVER_PP = case_when(any(c_across(starts_with("DEN_IGG_PERF_")) == 1) ~ 1, # 1, titers performed
                                          any(na.omit(c_across(starts_with("DEN_IGG_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                          all(na.omit(c_across(starts_with("DEN_IGG_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                          all(na.omit(c_across(starts_with("DEN_IGG_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                          TRUE ~ 77),
         
         DEN_IGM_POSITIVE_EVER_PP_TEXT = case_when(DEN_IGM_POSITIVE_EVER_PP ==1 ~ "Postpartum IgM+", 
                                                   DEN_IGM_INCIDENT_PP == 1 ~ "Incident IgM+ during Postpartum", 
                                                   TRUE ~ NA),
         
         DEN_IGG_POSITIVE_EVER_PP_TEXT = case_when(DEN_IGG_POSITIVE_EVER_PP ==1 ~ "Postpartum IgG+", 
                                                   DEN_IGG_INCIDENT_PP == 1 ~ "Incident IgG+ during Postpartum", 
                                                   TRUE ~ NA)
  ) %>% 
  mutate(DEN_POSITIVE_EVER_PP = case_when(DEN_IGM_POSITIVE_EVER_PP ==1 | DEN_IGM_INCIDENT_PP==1 ~ 1, ## EVER PREG variable is zika IgM+ ever during pregnancy or incident IgG
                                          DEN_IGM_POSITIVE_EVER_PP ==0 & DEN_IGG_INCIDENT_PP== 0 ~ 0, 
                                          DEN_IGM_PERF_EVER_PP==0 & DEN_IGG_PERF_EVER_PP==0 ~ 77, # ## if test performed but no infection, these are NOs
                                          TRUE ~ 77) 
         
  ) %>% 
  mutate(DEN_DATE_IGM_POSITIVE_PP = do.call(pmin, c(across(starts_with("DATE_IGM_POSITIVE_")), na.rm = TRUE)),
         DEN_GESTAGE_IGM_POSITIVE_DAYS_PP = as.numeric(DEN_DATE_IGM_POSITIVE_PP-ymd(PREG_END_DATE)),
         DEN_GESTAGE_IGM_POSITIVE_WKS_PP = DEN_GESTAGE_IGM_POSITIVE_DAYS_PP %/% 7
  ) %>%
  mutate(DEN_DATE_IGG_POSITIVE_PP = do.call(pmin, c(across(starts_with("DATE_IGG_POSITIVE_")), na.rm = TRUE)),
         DEN_GESTAGE_IGG_POSITIVE_DAYS_PP = as.numeric(DEN_DATE_IGG_POSITIVE_PP-ymd(PREG_END_DATE)),
         DEN_GESTAGE_IGG_POSITIVE_WKS_PP = DEN_GESTAGE_IGG_POSITIVE_DAYS_PP %/% 7
  ) %>% 
  select(SITE, MOMID, PREGID, contains("_PP"))


## add labels
den_labels <- den_export_anc %>%
  left_join(den_export_pnc, by = c("SITE", "MOMID", "PREGID")) %>% 
  
  mutate(
    DEN_IGM_POSITIVE_ENROLL_LABEL = factor(DEN_IGM_POSITIVE_ENROLL,levels = c(1, 0, 2, 55, 77),
                                           labels = c("DEN IgM+ at enrollment", "DEN IgM- at enrollment", "DEN IgM inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    DEN_IGG_POSITIVE_ENROLL_LABEL = factor(DEN_IGG_POSITIVE_ENROLL,levels = c(1, 0, 2, 55, 77),
                                           labels = c("DEN IgG+ at enrollment", "DEN IgG- at enrollment", "DEN IgG inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    DEN_IGM_UNKNOWN_BASELINE_LABEL = factor(DEN_IGM_UNKNOWN_BASELINE,levels = c(1, 0),
                                            labels = c("DEN IgM+ during pregnancy but missing baseline status", "No incident dengue infection with unknown baseline status")),
    DEN_IGG_UNKNOWN_BASELINE_LABEL = factor(DEN_IGG_UNKNOWN_BASELINE,levels = c(1, 0),
                                            labels = c("DEN IgG+ during pregnancy but missing baseline status", "No incident dengue infection with unknown baseline status")),
    DEN_IGM_INCIDENT_LABEL = factor(DEN_IGM_INCIDENT,levels = c(1, 0),
                                    labels = c("Incident DEN IgG+ during pregnancy", "No incident dengue IgG+ infection")),
    DEN_IGG_INCIDENT_LABEL = factor(DEN_IGG_INCIDENT,levels = c(1, 0),
                                    labels = c("Incident DEN IgG+ during pregnancy", "No incident dengue IgG+ infection")),
    DEN_IGM_INCIDENT_PP_LABEL = factor(DEN_IGM_INCIDENT_PP,levels = c(1, 0),
                                       labels = c("Incident DEN IgM+ during postpartum", "No incident dengue infection during postpartum")),
    DEN_IGG_INCIDENT_PP_LABEL = factor(DEN_IGG_INCIDENT_PP,levels = c(1, 0),
                                       labels = c("Incident DEN IgG+ during postpartum", "No incident dengue IgG+ infection during postpartum")),
    DEN_IGM_POSITIVE_EVER_PREG_LABEL = factor(DEN_IGM_POSITIVE_EVER_PREG,levels = c(1, 0, 2, 55, 77),
                                              labels = c("DEN IgM+ ever during pregnancy", "DEN IgM never during pregnancy", "DEN IgM inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    DEN_IGG_POSITIVE_EVER_PREG_LABEL = factor(DEN_IGG_POSITIVE_EVER_PREG,levels = c(1, 0, 2, 55, 77),
                                              labels = c("DEN IgG+ ever during pregnancy", "DEN IgG never during pregnancy", "DEN IgG inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    DEN_POSITIVE_EVER_PREG_LABEL = factor(DEN_POSITIVE_EVER_PREG,levels = c(1, 0, 77),
                                          labels = c("dengue ever during pregnancy", "dengue never during pregnancy", "NA/no test performed")),
    DEN_IGM_POSITIVE_EVER_PP_LABEL = factor(DEN_IGM_POSITIVE_EVER_PP,levels = c(1, 0, 2, 55, 77),
                                            labels = c("DEN IgM+ ever during postpartum", "DEN IgM never during postpartum", "DEN IgM inconclusive during postpartum", "Test performed but missing test result", "NA/no test performed")),
    DEN_IGG_POSITIVE_EVER_PP_LABEL = factor(DEN_IGG_POSITIVE_EVER_PP,levels = c(1, 0, 2, 55, 77),
                                            labels = c("DEN IgG+ ever during postpartum", "DEN IgG never during postpartum", "DEN IgG inconclusive during postpartum", "Test performed but missing test result", "NA/no test performed")),
    DEN_POSITIVE_EVER_PP_LABEL = factor(DEN_POSITIVE_EVER_PP,levels = c(1, 0, 77),
                                        labels = c("dengue ever during postpartum", "dengue never during postpartum", "NA/no test performed"))
  ) %>% 
  mutate(DEN_POSITIVE_EVER_PREG_CAT_LABEL = case_when(DEN_IGM_POSITIVE_ENROLL ==1 & DEN_IGM_INCIDENT!=1 & DEN_IGM_UNKNOWN_BASELINE!=1 & DEN_IGG_POSITIVE_ENROLL != 1 ~ "Baseline IgM+",
                                                      DEN_IGG_POSITIVE_ENROLL ==1 & DEN_IGG_INCIDENT!=1 & DEN_IGG_UNKNOWN_BASELINE!=1 & DEN_IGM_POSITIVE_ENROLL != 1 ~ "Baseline IgG+",
                                                      DEN_IGM_POSITIVE_ENROLL ==1 & DEN_IGM_INCIDENT!=1 & DEN_IGM_UNKNOWN_BASELINE!=1 & DEN_IGG_POSITIVE_ENROLL == 1 ~ "Baseline IgM+ & IgG+",
                                                      DEN_IGM_POSITIVE_ENROLL !=1 & DEN_IGM_INCIDENT==1 & DEN_IGM_UNKNOWN_BASELINE!=1 & DEN_IGG_INCIDENT != 1 ~ "Incident IgM+",
                                                      DEN_IGG_POSITIVE_ENROLL !=1 & DEN_IGG_INCIDENT==1 & DEN_IGG_UNKNOWN_BASELINE!=1 & DEN_IGM_INCIDENT != 1 ~ "Incident IgG+",
                                                      DEN_IGM_POSITIVE_ENROLL !=1 & DEN_IGM_INCIDENT==1 & DEN_IGM_UNKNOWN_BASELINE!=1 & DEN_IGG_INCIDENT == 1 ~ "Incident IgM+ & IgG+",
                                                      DEN_IGM_POSITIVE_ENROLL !=1 & DEN_IGM_INCIDENT!=1 & DEN_IGM_UNKNOWN_BASELINE==1 & DEN_IGG_UNKNOWN_BASELINE != 1 ~ "IgM+ with unknown baseline",
                                                      DEN_IGG_POSITIVE_ENROLL !=1 & DEN_IGG_INCIDENT!=1 & DEN_IGG_UNKNOWN_BASELINE==1 & DEN_IGM_UNKNOWN_BASELINE != 1 ~ "IgG+ with unknown baseline",
                                                      DEN_IGM_POSITIVE_ENROLL !=1 & DEN_IGM_INCIDENT!=1 & DEN_IGM_UNKNOWN_BASELINE==1 & DEN_IGG_UNKNOWN_BASELINE == 1 ~ "IgM+ & IgG+ with unknown baseline",
                                                      TRUE ~ NA
  )) %>% 
  mutate(DEN_POSITIVE_EVER_PREG_CAT = case_when(DEN_IGM_POSITIVE_ENROLL ==1 & DEN_IGM_INCIDENT!=1 & DEN_IGM_UNKNOWN_BASELINE!=1 & DEN_IGG_POSITIVE_ENROLL != 1 ~ 1,
                                                DEN_IGG_POSITIVE_ENROLL ==1 & DEN_IGG_INCIDENT!=1 & DEN_IGG_UNKNOWN_BASELINE!=1 & DEN_IGM_POSITIVE_ENROLL != 1 ~ 2,
                                                DEN_IGM_POSITIVE_ENROLL ==1 & DEN_IGM_INCIDENT!=1 & DEN_IGM_UNKNOWN_BASELINE!=1 & DEN_IGG_POSITIVE_ENROLL == 1 ~ 3,
                                                DEN_IGM_POSITIVE_ENROLL !=1 & DEN_IGM_INCIDENT==1 & DEN_IGM_UNKNOWN_BASELINE!=1 & DEN_IGG_INCIDENT != 1 ~ 4,
                                                DEN_IGG_POSITIVE_ENROLL !=1 & DEN_IGG_INCIDENT==1 & DEN_IGG_UNKNOWN_BASELINE!=1 & DEN_IGM_INCIDENT != 1 ~ 5,
                                                DEN_IGM_POSITIVE_ENROLL !=1 & DEN_IGM_INCIDENT==1 & DEN_IGM_UNKNOWN_BASELINE!=1 & DEN_IGG_INCIDENT == 1 ~ 6,
                                                DEN_IGM_POSITIVE_ENROLL !=1 & DEN_IGM_INCIDENT!=1 & DEN_IGM_UNKNOWN_BASELINE==1 & DEN_IGG_UNKNOWN_BASELINE != 1 ~ 7,
                                                DEN_IGG_POSITIVE_ENROLL !=1 & DEN_IGG_INCIDENT!=1 & DEN_IGG_UNKNOWN_BASELINE==1 & DEN_IGM_UNKNOWN_BASELINE != 1 ~ 8,
                                                DEN_IGM_POSITIVE_ENROLL !=1 & DEN_IGM_INCIDENT!=1 & DEN_IGM_UNKNOWN_BASELINE==1 & DEN_IGG_UNKNOWN_BASELINE == 1 ~ 9,
                                                TRUE ~ NA
  )) %>% 
  mutate(DEN_POSITIVE_EVER_PP_CAT_LABEL = case_when(DEN_IGM_POSITIVE_EVER_PP ==1 & DEN_IGM_POSITIVE_EVER_PREG==1 ~ "Chikungunya IgM+ during pregnancy and postpartum", 
                                                    DEN_IGG_POSITIVE_EVER_PP ==1 & DEN_IGG_POSITIVE_EVER_PREG==1 ~ "Chikungunya IgG+ during pregnancy and postpartum", 
                                                    DEN_IGM_INCIDENT_PP ==1 & DEN_IGG_INCIDENT_PP!=1 ~ "Incident Chikungunya IgM+ during postpartum", 
                                                    DEN_IGM_INCIDENT_PP !=1 & DEN_IGG_INCIDENT_PP==1 ~ "Incident Chikungunya IgG+ during postpartum", 
                                                    TRUE ~ NA
  )) %>% 
  mutate(DEN_POSITIVE_EVER_PP_CAT = case_when(DEN_IGM_POSITIVE_EVER_PP ==1 & DEN_IGM_POSITIVE_EVER_PREG==1 ~ 1, 
                                              DEN_IGG_POSITIVE_EVER_PP ==1 & DEN_IGG_POSITIVE_EVER_PREG==1 ~ 2,
                                              DEN_IGM_INCIDENT_PP ==1 & DEN_IGG_INCIDENT_PP!=1 ~ 3,
                                              DEN_IGM_INCIDENT_PP !=1 & DEN_IGG_INCIDENT_PP==1 ~ 4, 
                                              TRUE ~ NA
  ))  


den_export_labels <- den_labels %>%
  select(SITE, MOMID, PREGID,
         DEN_IGM_PERF_ENROLL, DEN_IGM_POSITIVE_ENROLL, DEN_IGM_POSITIVE_ENROLL_LABEL,
         DEN_IGG_PERF_ENROLL, DEN_IGG_POSITIVE_ENROLL, DEN_IGG_POSITIVE_ENROLL_LABEL,
         DEN_IGM_PERF_EVER_PREG, DEN_IGM_POSITIVE_EVER_PREG, DEN_IGM_POSITIVE_EVER_PREG_LABEL,
         DEN_DATE_IGM_POSITIVE, DEN_GESTAGE_IGM_POSITIVE_DAYS, DEN_GESTAGE_IGM_POSITIVE_WKS,
         DEN_IGG_PERF_EVER_PREG, DEN_IGG_POSITIVE_EVER_PREG, DEN_IGG_POSITIVE_EVER_PREG_LABEL,
         DEN_DATE_IGG_POSITIVE, DEN_GESTAGE_IGG_POSITIVE_DAYS, DEN_GESTAGE_IGG_POSITIVE_WKS,
         DEN_IGG_DAYS_BETWEEN_TESTS, DEN_IGG_WKS_BETWEEN_TESTS,
         DEN_IGM_UNKNOWN_BASELINE, DEN_IGM_UNKNOWN_BASELINE_LABEL,
         DEN_IGG_UNKNOWN_BASELINE, DEN_IGG_UNKNOWN_BASELINE_LABEL,
         DEN_IGM_INCIDENT, DEN_IGM_INCIDENT_LABEL, DEN_IGG_INCIDENT, DEN_IGM_INCIDENT_LABEL,
         DEN_POSITIVE_EVER_PREG, DEN_POSITIVE_EVER_PREG_LABEL, 
         DEN_POSITIVE_EVER_PREG_CAT, DEN_POSITIVE_EVER_PREG_CAT_LABEL,
         ## 
         DEN_IGM_INCIDENT_PP, DEN_IGM_INCIDENT_PP_LABEL,
         DEN_IGG_INCIDENT_PP, DEN_IGG_INCIDENT_PP_LABEL, DEN_IGM_PERF_EVER_PP, DEN_IGG_PERF_EVER_PP,
         DEN_POSITIVE_EVER_PP, DEN_POSITIVE_EVER_PP_LABEL, DEN_POSITIVE_EVER_PP_CAT, DEN_POSITIVE_EVER_PP_CAT_LABEL,
         DEN_DATE_IGM_POSITIVE_PP, DEN_DATE_IGG_POSITIVE_PP, DEN_GESTAGE_IGG_POSITIVE_DAYS_PP, DEN_GESTAGE_IGG_POSITIVE_WKS_PP
         
  )


den_export_labels <-  den_export_labels %>% 
  left_join(zik_export_labels %>% select(SITE, PREGID, ZIK_DATE_IGG_POSITIVE, ZIK_DATE_IGM_POSITIVE), 
            by = c("SITE", "PREGID")) %>% 
  mutate(DEN_IGG_WITH_ZIK_IGG_PRIOR_INFECTION = case_when(ymd(ZIK_DATE_IGG_POSITIVE) < ymd(DEN_DATE_IGG_POSITIVE) ~ 1, TRUE ~0),
         DEN_IGG_WITH_ZIK_IGG_SAME_DAY = case_when(ymd(ZIK_DATE_IGG_POSITIVE) <= ymd(DEN_DATE_IGG_POSITIVE) ~ 1, TRUE ~0), 
  ) %>% 
  select(-ZIK_DATE_IGG_POSITIVE, -ZIK_DATE_IGM_POSITIVE)
zik_export_labels <- zik_export_labels %>% 
  left_join(den_export_labels %>% filter(DEN_IGG_INCIDENT == 1) %>%  select(SITE, PREGID, DEN_DATE_IGG_POSITIVE, DEN_DATE_IGM_POSITIVE), 
            by = c("SITE", "PREGID")) %>% 
  mutate(ZIK_IGG_WITH_DEN_IGG_PRIOR_INFECTION = case_when(ymd(DEN_DATE_IGG_POSITIVE) < ymd(ZIK_DATE_IGG_POSITIVE) ~ 1, TRUE ~0),
         ZIK_IGG_WITH_DEN_IGG_SAME_DAY = case_when(ymd(DEN_DATE_IGG_POSITIVE) <= ymd(ZIK_DATE_IGG_POSITIVE) ~ 1, TRUE ~0), 
  ) %>% 
  select(-DEN_DATE_IGG_POSITIVE, -DEN_DATE_IGM_POSITIVE)


#*****************************************************************************
# Leptospirosis ----
#*****************************************************************************

lept <- mat_enroll %>% 
  # merge in mnh08 to pull titer results 
  left_join(mnh08 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, LEPT_EXPANSION, M08_LBSTDAT,
                             M08_LEPT_LBPERF_1, M08_LEPT_IGM_LBORRES, M08_LEPT_LBPERF_2, M08_LEPT_IGG_LBORRES), 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  # merge in mnh07 for specimen collection date
  left_join(mnh07 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, M07_MAT_SPEC_COLLECT_DAT), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  filter(LEPT_EXPANSION==1) %>% 
  # Was test performed?
  mutate(LEPT_IGM_PERF = case_when(M08_LEPT_LBPERF_1 ==1 ~ 1,
                                   is.na(M08_LEPT_LBPERF_1) ~ 55,
                                   TRUE ~ 0),
         LEPT_IGG_PERF = case_when(M08_LEPT_LBPERF_2 ==1 ~ 1,
                                   is.na(M08_LEPT_LBPERF_2) ~ 55,
                                   TRUE ~ 0)
  ) %>%
  # Test result available? 
  mutate(LEPT_IGM_RESULT_AVAI = case_when(M08_LEPT_IGM_LBORRES %in% c(0,1,2) ~ 1, TRUE ~ 0),
         LEPT_IGG_RESULT_AVAI = case_when(M08_LEPT_IGG_LBORRES %in% c(0,1,2) ~ 1, TRUE ~ 0)
  ) %>% 
  # Test result by Titers
  mutate(LEPT_IGM_POSITIVE = case_when(LEPT_EXPANSION==0 ~ 77, # 77, na; visit was before expansion date
                                       LEPT_IGM_PERF ==1 & LEPT_EXPANSION == 1 & M08_LEPT_IGM_LBORRES == 1 ~ 1, # 1, positive
                                       LEPT_IGM_PERF ==1 & LEPT_EXPANSION == 1 & M08_LEPT_IGM_LBORRES == 0 ~ 0, # 0, negative
                                       LEPT_IGM_PERF ==1 & LEPT_EXPANSION == 1 & M08_LEPT_IGM_LBORRES == 2 ~ 2, # 2, inconclusive
                                       LEPT_IGM_PERF ==1 & LEPT_EXPANSION == 1 & M08_LEPT_IGM_LBORRES %in% c(NA,55, 77, 99) ~ 55, # 55, missing (if test was performed but result is missing)
                                       LEPT_EXPANSION ==1 & LEPT_IGM_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                       TRUE ~ NA),
         LEPT_IGG_POSITIVE = case_when(LEPT_EXPANSION==0 ~ 77, # 77, na; visit was before expansion date
                                       LEPT_IGG_PERF ==1 & LEPT_EXPANSION == 1 & M08_LEPT_IGG_LBORRES == 1 ~ 1, # 1, positive
                                       LEPT_IGG_PERF ==1 & LEPT_EXPANSION == 1 & M08_LEPT_IGG_LBORRES == 0 ~ 0, # 0, negative
                                       LEPT_IGG_PERF ==1 & LEPT_EXPANSION == 1 & M08_LEPT_IGG_LBORRES == 2 ~ 2, # 2, inconclusive
                                       LEPT_IGG_PERF ==1 & LEPT_EXPANSION == 1 & M08_LEPT_IGG_LBORRES %in% c(NA,55, 77, 99) ~ 55, # 55, missing (if test was performed but result is missing)
                                       LEPT_EXPANSION ==1 & LEPT_IGG_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                       TRUE ~ NA)) %>% 
  # Date positive test 
  mutate(DATE_IGM_POSITIVE = case_when(LEPT_IGM_POSITIVE ==1 ~ ymd(M08_LBSTDAT), 
                                       TRUE ~ NA_Date_),
         DATE_IGG_POSITIVE = case_when(LEPT_IGG_POSITIVE ==1 ~ ymd(M08_LBSTDAT), 
                                       TRUE ~ NA_Date_),
         
  ) %>% 
  # generate indicator variable to remove any unscheduled visits that do not have a test outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & (LEPT_IGM_PERF ==1 | LEPT_IGG_PERF ==1) & (LEPT_IGM_RESULT_AVAI ==1 | LEPT_IGG_RESULT_AVAI ==1) ~ 1, # keep if test was done and result is available
                                  TYPE_VISIT %in% c(13,14) & (LEPT_IGM_POSITIVE %in% c(1,0,2) | LEPT_IGG_POSITIVE %in% c(1,0,2)) ~ 1, ## keep if valid results (could probably remove)
                                  TYPE_VISIT %in% c(1,2,3,4,5) ~ 1, # keep if not an unscheduled visit 
                                  TYPE_VISIT %in% c(13,14) & LEPT_IGM_POSITIVE %in% c(NA,55,77) & LEPT_IGG_POSITIVE %in% c(NA,55,77) ~ 0, # remove if missing 
                                  TRUE ~ 0)) %>% 
  filter(KEEP_UNSCHED==1)

lept_export_anc <- lept %>% 
  filter(!TYPE_VISIT %in% c(6,7,8,9,10,11,12,14)) %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, VISIT_DATE, PREG_START_DATE, M08_LBSTDAT, DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, starts_with("LEPT_")) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("LEPT_"), VISIT_DATE, DATE_IGG_POSITIVE, DATE_IGM_POSITIVE, M08_LBSTDAT),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(LEPT_IGM_POSITIVE_ENROLL = LEPT_IGM_POSITIVE_1,
         LEPT_IGG_POSITIVE_ENROLL = LEPT_IGG_POSITIVE_1,
         LEPT_IGM_PERF_ENROLL = LEPT_IGM_PERF_1,
         LEPT_IGM_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("LEPT_IGM_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                                 any(na.omit(c_across(starts_with("LEPT_IGM_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                                 all(na.omit(c_across(starts_with("LEPT_IGM_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                                 all(na.omit(c_across(starts_with("LEPT_IGM_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                 TRUE ~ 77),
         LEPT_IGM_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("LEPT_IGM_POSITIVE_")) == 1) & LEPT_IGM_POSITIVE_ENROLL %in% c(2, 55,77,99,NA)~ 1, # 1, Positive if  titers is + 
                                               TRUE ~ 0),
         LEPT_IGM_INCIDENT = case_when(LEPT_IGM_POSITIVE_EVER_PREG ==1 & LEPT_IGM_POSITIVE_ENROLL ==0 ~ 1, 
                                       TRUE  ~ 0),
         LEPT_IGM_PERF_EVER_PREG = case_when(any(c_across(starts_with("LEPT_IGM_PERF_")) == 1) ~ 1, # 1, titers performed
                                             any(na.omit(c_across(starts_with("LEPT_IGM_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                             all(na.omit(c_across(starts_with("LEPT_IGM_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                             all(na.omit(c_across(starts_with("LEPT_IGM_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                             TRUE ~ 77),
         LEPT_IGM_POSITIVE_EVER_PREG_TEXT = case_when(LEPT_IGM_POSITIVE_ENROLL ==1 & LEPT_IGM_INCIDENT!=1 & LEPT_IGM_UNKNOWN_BASELINE!=1 ~ "Baseline IgM+", 
                                                      LEPT_IGM_POSITIVE_ENROLL !=1 & LEPT_IGM_INCIDENT==1 & LEPT_IGM_UNKNOWN_BASELINE!=1 ~ "Incident IgM+", 
                                                      LEPT_IGM_POSITIVE_ENROLL !=1 & LEPT_IGM_INCIDENT!=1 & LEPT_IGM_UNKNOWN_BASELINE==1 ~ "IgM+ with unknown baseline",
                                                      TRUE ~ NA),
         LEPT_IGG_PERF_ENROLL = LEPT_IGG_PERF_1,
         LEPT_IGG_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("LEPT_IGG_POSITIVE_")) == 1) ~ 1, # 1, Positive if  titers is + 
                                                 any(na.omit(c_across(starts_with("LEPT_IGG_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any titers is -
                                                 all(na.omit(c_across(starts_with("LEPT_IGG_POSITIVE_"))) ==55) ~ 55, # 55, Missing if titers are missing
                                                 all(na.omit(c_across(starts_with("LEPT_IGG_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed
                                                 TRUE ~ 77),
         LEPT_IGG_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("LEPT_IGG_POSITIVE_")) == 1) & LEPT_IGG_POSITIVE_ENROLL %in% c(2, 55,77,99,NA) & LEPT_IGM_POSITIVE_ENROLL %in% c(2, 55,77,99,NA) ~ 1, # 1, Positive if  titers is + 
                                               TRUE ~ 0),
         LEPT_IGG_INCIDENT = case_when(LEPT_IGG_POSITIVE_EVER_PREG ==1 & LEPT_IGG_POSITIVE_ENROLL ==0 & LEPT_IGM_POSITIVE_ENROLL ==0 ~ 1, 
                                       TRUE  ~ 0),
         LEPT_IGG_POSITIVE_EVER_PREG_TEXT = case_when(LEPT_IGG_POSITIVE_ENROLL ==1 & LEPT_IGG_INCIDENT!=1 & LEPT_IGG_UNKNOWN_BASELINE!=1 ~ "Baseline IgG+", 
                                                      LEPT_IGG_POSITIVE_ENROLL !=1 & LEPT_IGG_INCIDENT==1 & LEPT_IGG_UNKNOWN_BASELINE!=1 ~ "Incident IgG+", 
                                                      LEPT_IGG_POSITIVE_ENROLL !=1 & LEPT_IGG_INCIDENT!=1 & LEPT_IGG_UNKNOWN_BASELINE==1 ~ "IgG+ with unknown baseline",
                                                      TRUE ~ NA),
         LEPT_IGG_PERF_EVER_PREG = case_when(any(c_across(starts_with("LEPT_IGG_PERF_")) == 1) ~ 1, # 1, titers performed
                                             any(na.omit(c_across(starts_with("LEPT_IGG_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                             all(na.omit(c_across(starts_with("LEPT_IGG_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                             all(na.omit(c_across(starts_with("LEPT_IGG_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                             TRUE ~ 77)
         
         
  ) %>% 
  mutate(LEPT_POSITIVE_EVER_PREG = case_when(LEPT_IGM_POSITIVE_EVER_PREG ==1 | LEPT_IGG_INCIDENT==1 ~ 1, ## EVER PREG variable is LEPTa IgM+ ever during pregnancy or incident IgG
                                             LEPT_IGM_POSITIVE_EVER_PREG ==0 & LEPT_IGG_INCIDENT== 0 ~ 0, 
                                             LEPT_IGM_PERF_EVER_PREG==0 & LEPT_IGG_PERF_EVER_PREG==0 ~ 77, # ## if test performed but no infection, these are NOs
                                             TRUE ~ 77)) %>%   
  mutate(LEPT_DATE_IGM_POSITIVE = do.call(pmin, c(across(starts_with("DATE_IGM_POSITIVE_")), na.rm = TRUE)),
         LEPT_GESTAGE_IGM_POSITIVE_DAYS = as.numeric(LEPT_DATE_IGM_POSITIVE-ymd(PREG_START_DATE)),
         LEPT_GESTAGE_IGM_POSITIVE_WKS = LEPT_GESTAGE_IGM_POSITIVE_DAYS %/% 7
  ) %>%
  mutate(LEPT_DATE_IGG_POSITIVE = do.call(pmin, c(across(starts_with("DATE_IGG_POSITIVE_")), na.rm = TRUE)),
         LEPT_GESTAGE_IGG_POSITIVE_DAYS = as.numeric(LEPT_DATE_IGG_POSITIVE-ymd(PREG_START_DATE)),
         LEPT_GESTAGE_IGG_POSITIVE_WKS = LEPT_GESTAGE_IGG_POSITIVE_DAYS %/% 7
  ) %>%
  ## generate indicator if incident IGG
  mutate(LEPT_INCIDENT_IGG_POSITIVE = case_when(LEPT_IGG_POSITIVE_ENROLL==0 & LEPT_IGG_POSITIVE_EVER_PREG==1 ~ 1, TRUE ~ 0),
         LEPT_IGG_DAYS_BETWEEN_TESTS = case_when(LEPT_INCIDENT_IGG_POSITIVE==1 ~ as.numeric(ymd(LEPT_DATE_IGG_POSITIVE) - ymd(M08_LBSTDAT_1))),
         LEPT_IGG_WKS_BETWEEN_TESTS = LEPT_IGG_DAYS_BETWEEN_TESTS %/% 7
  )

## add labels
lept_labels <- lept_export_anc %>%
  mutate(
    LEPT_IGM_POSITIVE_ENROLL_LABEL = factor(LEPT_IGM_POSITIVE_ENROLL,levels = c(1, 0, 2, 55, 77),
                                            labels = c("LEPT IgM+ at enrollment", "LEPT IgM- at enrollment", "LEPT IgM inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    LEPT_IGG_POSITIVE_ENROLL_LABEL = factor(LEPT_IGG_POSITIVE_ENROLL,levels = c(1, 0, 2, 55, 77),
                                            labels = c("LEPT IgG+ at enrollment", "LEPT IgG- at enrollment", "LEPT IgG inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    LEPT_IGM_UNKNOWN_BASELINE_LABEL = factor(LEPT_IGM_UNKNOWN_BASELINE,levels = c(1, 0),
                                             labels = c("LEPT IgM+ during pregnancy but missing baseline status", "No incident lepto infection with unknown baseline status")),
    LEPT_IGG_UNKNOWN_BASELINE_LABEL = factor(LEPT_IGG_UNKNOWN_BASELINE,levels = c(1, 0),
                                             labels = c("LEPT IgG+ during pregnancy but missing baseline status", "No incident lepto infection with unknown baseline status")),
    LEPT_IGM_INCIDENT_LABEL = factor(LEPT_IGM_INCIDENT,levels = c(1, 0),
                                     labels = c("Incident LEPT IgG+ during pregnancy", "No incident lepto IgG+ infection")),
    LEPT_IGG_INCIDENT_LABEL = factor(LEPT_IGG_INCIDENT,levels = c(1, 0),
                                     labels = c("Incident LEPT IgG+ during pregnancy", "No incident lepto IgG+ infection")),
    LEPT_IGM_POSITIVE_EVER_PREG_LABEL = factor(LEPT_IGM_POSITIVE_EVER_PREG,levels = c(1, 0, 2, 55, 77),
                                               labels = c("LEPT IgM+ ever during pregnancy", "LEPT IgM never during pregnancy", "LEPT IgM inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    LEPT_IGG_POSITIVE_EVER_PREG_LABEL = factor(LEPT_IGG_POSITIVE_EVER_PREG,levels = c(1, 0, 2, 55, 77),
                                               labels = c("LEPT IgG+ ever during pregnancy", "LEPT IgG never during pregnancy", "LEPT IgG inconclusive at enrollment", "Test performed but missing test result", "NA/no test performed")),
    LEPT_POSITIVE_EVER_PREG_LABEL = factor(LEPT_POSITIVE_EVER_PREG,levels = c(1, 0, 77),
                                           labels = c("Lepto ever during pregnancy", "Lepto never during pregnancy", "NA/no test performed")),
  ) %>% 
  mutate(LEPT_POSITIVE_EVER_PREG_CAT_LABEL = case_when(LEPT_IGM_POSITIVE_ENROLL ==1 & LEPT_IGM_INCIDENT!=1 & LEPT_IGM_UNKNOWN_BASELINE!=1 & LEPT_IGG_POSITIVE_ENROLL != 1 ~ "Baseline IgM+",
                                                       LEPT_IGG_POSITIVE_ENROLL ==1 & LEPT_IGG_INCIDENT!=1 & LEPT_IGG_UNKNOWN_BASELINE!=1 & LEPT_IGM_POSITIVE_ENROLL != 1 ~ "Baseline IgG+",
                                                       LEPT_IGM_POSITIVE_ENROLL ==1 & LEPT_IGM_INCIDENT!=1 & LEPT_IGM_UNKNOWN_BASELINE!=1 & LEPT_IGG_POSITIVE_ENROLL == 1 ~ "Baseline IgM+ & IgG+",
                                                       LEPT_IGM_POSITIVE_ENROLL !=1 & LEPT_IGM_INCIDENT==1 & LEPT_IGM_UNKNOWN_BASELINE!=1 & LEPT_IGG_INCIDENT != 1 ~ "Incident IgM+",
                                                       LEPT_IGG_POSITIVE_ENROLL !=1 & LEPT_IGG_INCIDENT==1 & LEPT_IGG_UNKNOWN_BASELINE!=1 & LEPT_IGM_INCIDENT != 1 ~ "Incident IgG+",
                                                       LEPT_IGM_POSITIVE_ENROLL !=1 & LEPT_IGM_INCIDENT==1 & LEPT_IGM_UNKNOWN_BASELINE!=1 & LEPT_IGG_INCIDENT == 1 ~ "Incident IgM+ & IgG+",
                                                       LEPT_IGM_POSITIVE_ENROLL !=1 & LEPT_IGM_INCIDENT!=1 & LEPT_IGM_UNKNOWN_BASELINE==1 & LEPT_IGG_UNKNOWN_BASELINE != 1 ~ "IgM+ with unknown baseline",
                                                       LEPT_IGG_POSITIVE_ENROLL !=1 & LEPT_IGG_INCIDENT!=1 & LEPT_IGG_UNKNOWN_BASELINE==1 & LEPT_IGM_UNKNOWN_BASELINE != 1 ~ "IgG+ with unknown baseline",
                                                       LEPT_IGM_POSITIVE_ENROLL !=1 & LEPT_IGM_INCIDENT!=1 & LEPT_IGM_UNKNOWN_BASELINE==1 & LEPT_IGG_UNKNOWN_BASELINE == 1 ~ "IgM+ & IgG+ with unknown baseline",
                                                       TRUE ~ NA
  )) %>% 
  mutate(LEPT_POSITIVE_EVER_PREG_CAT = case_when(LEPT_IGM_POSITIVE_ENROLL ==1 & LEPT_IGM_INCIDENT!=1 & LEPT_IGM_UNKNOWN_BASELINE!=1 & LEPT_IGG_POSITIVE_ENROLL != 1 ~ 1,
                                                 LEPT_IGG_POSITIVE_ENROLL ==1 & LEPT_IGG_INCIDENT!=1 & LEPT_IGG_UNKNOWN_BASELINE!=1 & LEPT_IGM_POSITIVE_ENROLL != 1 ~ 2,
                                                 LEPT_IGM_POSITIVE_ENROLL ==1 & LEPT_IGM_INCIDENT!=1 & LEPT_IGM_UNKNOWN_BASELINE!=1 & LEPT_IGG_POSITIVE_ENROLL == 1 ~ 3,
                                                 LEPT_IGM_POSITIVE_ENROLL !=1 & LEPT_IGM_INCIDENT==1 & LEPT_IGM_UNKNOWN_BASELINE!=1 & LEPT_IGG_INCIDENT != 1 ~ 4,
                                                 LEPT_IGG_POSITIVE_ENROLL !=1 & LEPT_IGG_INCIDENT==1 & LEPT_IGG_UNKNOWN_BASELINE!=1 & LEPT_IGM_INCIDENT != 1 ~ 5,
                                                 LEPT_IGM_POSITIVE_ENROLL !=1 & LEPT_IGM_INCIDENT==1 & LEPT_IGM_UNKNOWN_BASELINE!=1 & LEPT_IGG_INCIDENT == 1 ~ 6,
                                                 LEPT_IGM_POSITIVE_ENROLL !=1 & LEPT_IGM_INCIDENT!=1 & LEPT_IGM_UNKNOWN_BASELINE==1 & LEPT_IGG_UNKNOWN_BASELINE != 1 ~ 7,
                                                 LEPT_IGG_POSITIVE_ENROLL !=1 & LEPT_IGG_INCIDENT!=1 & LEPT_IGG_UNKNOWN_BASELINE==1 & LEPT_IGM_UNKNOWN_BASELINE != 1 ~ 8,
                                                 LEPT_IGM_POSITIVE_ENROLL !=1 & LEPT_IGM_INCIDENT!=1 & LEPT_IGM_UNKNOWN_BASELINE==1 & LEPT_IGG_UNKNOWN_BASELINE == 1 ~ 9,
                                                 TRUE ~ NA
  ))



lept_export_labels <- lept_labels %>%
  select(SITE, MOMID, PREGID,
         LEPT_IGM_PERF_ENROLL, LEPT_IGM_POSITIVE_ENROLL, LEPT_IGM_POSITIVE_ENROLL_LABEL, 
         LEPT_IGG_PERF_ENROLL, LEPT_IGG_POSITIVE_ENROLL, LEPT_IGG_POSITIVE_ENROLL_LABEL,
         LEPT_IGM_PERF_EVER_PREG, LEPT_IGM_POSITIVE_EVER_PREG, LEPT_IGM_POSITIVE_EVER_PREG_LABEL,
         LEPT_DATE_IGM_POSITIVE, LEPT_GESTAGE_IGM_POSITIVE_DAYS, LEPT_GESTAGE_IGM_POSITIVE_WKS,
         LEPT_IGG_PERF_EVER_PREG, LEPT_IGG_POSITIVE_EVER_PREG, LEPT_IGG_POSITIVE_EVER_PREG_LABEL,
         LEPT_DATE_IGG_POSITIVE, LEPT_GESTAGE_IGG_POSITIVE_DAYS, LEPT_GESTAGE_IGG_POSITIVE_WKS,
         LEPT_IGG_DAYS_BETWEEN_TESTS, LEPT_IGG_WKS_BETWEEN_TESTS,
         LEPT_IGM_UNKNOWN_BASELINE, LEPT_IGM_UNKNOWN_BASELINE_LABEL, 
         LEPT_IGG_UNKNOWN_BASELINE, LEPT_IGG_UNKNOWN_BASELINE_LABEL,
         LEPT_IGM_INCIDENT, LEPT_IGM_INCIDENT_LABEL, LEPT_IGG_INCIDENT, LEPT_IGG_INCIDENT_LABEL, 
         LEPT_POSITIVE_EVER_PREG, LEPT_POSITIVE_EVER_PREG_LABEL, 
         LEPT_POSITIVE_EVER_PREG_CAT, LEPT_POSITIVE_EVER_PREG_CAT_LABEL
  )


#*****************************************************************************
# TB ----
#*****************************************************************************
tb <- mat_enroll %>% 
  left_join(mnh04, by = c("SITE", "MOMID", "PREGID")) %>% 
  select(SITE, MOMID, PREGID, ENROLL_SCRN_DATE, TYPE_VISIT, PREG_START_DATE, VISIT_DATE, M04_MAT_VISIT_MNH04, 
         M04_TB_MHOCCUR, M04_TB_CETERM_1, M04_TB_CETERM_2, M04_TB_CETERM_3, M04_TB_CETERM_4) %>% 
  # merge in mnh07 for specimen collection date
  left_join(mnh07 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE, M07_MAT_SPEC_COLLECT_DAT, M07_MAT_TB_SPEC_COLLECT), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  # merge in mnh08 to pull genexpert results 
  left_join(mnh08 %>% select(SITE, MOMID, PREGID, TYPE_VISIT, VISIT_DATE,M08_LBSTDAT, M08_TB_LBPERF_1,M08_TB_LBPERF_2, M08_TB_CNFRM_LBORRES), 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  # Was test performed?
  # for W4SS total with at least 1 symptom in W4SS in MNH04 (1=At least 1 symptom reported, 0=No symptoms)
  mutate(TB_CULT_PERF = case_when(M08_TB_LBPERF_1 ==1 | M08_TB_LBPERF_2 ==1 ~ 1,
                                  is.na(M08_TB_LBPERF_1) ~ 0,
                                  TRUE ~ 0),
         W4SS_PERF = case_when(M04_TB_CETERM_1==1 | M04_TB_CETERM_2==1 | M04_TB_CETERM_3==1| M04_TB_CETERM_4==1 ~ 1,
                               M04_TB_CETERM_1==0 & M04_TB_CETERM_2==0 & M04_TB_CETERM_3==0 & M04_TB_CETERM_4==0 ~ 0,
                               M04_TB_CETERM_1%in% c(55,77,99,NA) & M04_TB_CETERM_2%in% c(55,77,99,NA) & M04_TB_CETERM_3%in% c(55,77,99,NA) & M04_TB_CETERM_4%in% c(55,77,99,NA) ~ 55,
                               TRUE ~ NA)
  ) %>%
  # Test result available? 
  mutate(TB_CULT_RESULT_AVAI = case_when(M08_TB_CNFRM_LBORRES %in% c(1,0,2) ~ 1, TRUE ~ 0)
  ) %>% 
  # Test result (if w4ss +, participant should have tb test)
  mutate(TB_CULT_POSITIVE = case_when(TB_CULT_PERF == 1 & M08_TB_CNFRM_LBORRES == 1 ~ 1, # 1, positive
                                      TB_CULT_PERF == 1 & M08_TB_CNFRM_LBORRES == 0 ~ 0,  # 0, negative
                                      TB_CULT_PERF == 1 & M08_TB_CNFRM_LBORRES == 2 ~ 2,  # 2, inconclusive
                                      TB_CULT_PERF == 1 & M08_TB_CNFRM_LBORRES %in% c(55,77,99) ~ 55, # 55, missing (if test was performed but result is missing)
                                      TB_CULT_PERF %in% c(0,55) ~ 77,  #77, na if test was not performed 
                                      TRUE ~ 77)
  ) %>% 
  # Test result by Dx 
  mutate(TB_DX_POSITIVE = case_when(M04_TB_MHOCCUR == 1 ~ 1,
                                    M04_TB_MHOCCUR == 0 ~ 0,
                                    M04_TB_MHOCCUR %in% c(55, 77, 99,  NA) ~ 77,
                                    TRUE ~ 77)) %>%
  # TB positive by Dx or culture (negatives only count for culture)
  mutate(TB_POSTIVE = case_when(TB_DX_POSITIVE==1 | TB_CULT_POSITIVE==1 ~ 1, # 1, positive TB
                                TB_CULT_POSITIVE==0 | TB_DX_POSITIVE ==0 ~ 0, # 0, negative for TB
                                TB_CULT_POSITIVE == 2 ~ 2, # 2, inconclusive
                                TB_CULT_POSITIVE %in% c(55,77,99,NA) ~ 77, # na/missing test  
                                TRUE ~ 77)) %>% 
  # Data check below to see if any participant who was dx also has a culture performed 
  mutate(DX_AND_CULTURE = case_when(TB_DX_POSITIVE==1 & TB_CULT_PERF==1 ~ 1, 
                                    TB_DX_POSITIVE==1 & TB_CULT_PERF==0 ~ 55,
                                    TRUE ~ NA
                                    
  ))  %>%  
  # Date positive test 
  mutate(DATE_TB_CULT_POSITIVE = case_when(TB_CULT_POSITIVE ==1 ~ ymd(M08_LBSTDAT), 
                                           TRUE ~ NA_Date_)
  ) %>% 
  # generate indicator variable to remove any unscheduled visits that do not have a test outcome (removing these will make the conversion to wide format below cleaner)
  mutate(KEEP_UNSCHED = case_when(TYPE_VISIT %in% c(13,14) & (TB_POSTIVE ==1 | TB_CULT_RESULT_AVAI ==1) ~ 1, # keep if test was done and result is available
                                  TYPE_VISIT %in% c(13,14) & (TB_POSTIVE %in% c(1,0,2)) ~ 1, ## keep if valid results (could probably remove)
                                  TYPE_VISIT %in% c(1,2,3,4,5) ~ 1, # keep if not an unscheduled visit 
                                  TYPE_VISIT %in% c(13,14) & TB_POSTIVE %in% c(NA,55,77) ~ 0, # remove if missing 
                                  TRUE ~ 0)) %>% 
  filter(KEEP_UNSCHED==1)


tb_export <- tb %>% 
  # arrange all data points by date and assign an new visit sequence variable
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  ungroup() %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, VISIT_SEQ, VISIT_DATE, DX_AND_CULTURE, starts_with("TB_"), PREG_START_DATE, M08_LBSTDAT, DATE_TB_CULT_POSITIVE) %>%
  pivot_wider(
    names_from = VISIT_SEQ,
    values_from = c(starts_with("TB_"), M08_LBSTDAT, VISIT_DATE, DATE_TB_CULT_POSITIVE),
    names_glue = "{.value}_{VISIT_SEQ}"
  ) %>% 
  # Generate indicator variables for HIV @ enrollment or ever in pregnancy by any measured test 
  rowwise() %>% 
  mutate(TB_DX_POSITIVE_ENROLL = TB_DX_POSITIVE_1,
         TB_CULT_PERF_ENROLL = TB_CULT_PERF_1,
         TB_CULT_POSITIVE_ENROLL = TB_CULT_POSITIVE_1,
         TB_POSITIVE_ENROLL = TB_POSTIVE_1,
         # TB_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("TB_POSTIVE_")) == 1) ~ 1, # 1, Positive if Dx+ or culture is + 
         #                                   any(na.omit(c_across(starts_with("TB_POSTIVE_"))) == 0) ~ 0, # 0, Negative if any culture is -
         #                                   all(na.omit(c_across(starts_with("TB_POSTIVE_"))) ==55) ~ 55, # 55, Missing if culture missing
         #                                   all(na.omit(c_across(starts_with("TB_POSTIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no culture performed
         #                                   TRUE ~ 77),
         TB_CULT_PERF_EVER_PREG = case_when(any(c_across(starts_with("TB_CULT_PERF_")) == 1) ~ 1, # 1, culture performed
                                            any(na.omit(c_across(starts_with("TB_CULT_PERF_"))) == 0) ~ 0, # 0, test not performed 
                                            # all(na.omit(c_across(starts_with("TB_CULT_PERF_"))) ==55) ~ 55, # 55, Missing if test is missing
                                            all(na.omit(c_across(starts_with("TB_CULT_PERF_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no test performed 
                                            TRUE ~ 77),
         TB_CULT_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("TB_CULT_POSITIVE_")) == 1) ~ 1, # 1, Positive if culture is + 
                                                any(na.omit(c_across(starts_with("TB_CULT_POSITIVE_"))) == 0) ~ 0, # 0, Negative if any culture is -
                                                all(na.omit(c_across(starts_with("TB_CULT_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no culture performed
                                                TRUE ~ 77),
         TB_CULT_UNKNOWN_BASELINE = case_when(any(c_across(starts_with("TB_CULT_POSITIVE_")) == 1) & TB_CULT_POSITIVE_ENROLL %in% c(2, 55,77,99,NA) ~ 1, # 1, Positive if  titers is + 
                                              TRUE ~ 0),
         TB_CULT_POSITIVE_INCIDENT = case_when(TB_CULT_POSITIVE_EVER_PREG ==1 & TB_CULT_POSITIVE_ENROLL ==0 ~ 1, 
                                               TRUE  ~ 0),
         TB_DX_POSITIVE_EVER_PREG = case_when(any(c_across(starts_with("TB_DX_POSITIVE_")) == 1) ~ 1, # 1, Positive if  Dx is +
                                              all(na.omit(c_across(starts_with("TB_DX_POSITIVE_"))) ==55) ~ 55, # 55, Missing if both Dx missing
                                              all(na.omit(c_across(starts_with("TB_DX_POSITIVE_"))) %in% c(55,77,99,NA))  ~ 77, # 77, NA no Dx performed
                                              TRUE ~ 77),
         TB_POSITIVE_EVER_PREG = case_when(TB_POSITIVE_ENROLL ==1 | TB_CULT_POSITIVE_EVER_PREG ==1 | TB_CULT_POSITIVE_INCIDENT == 1 | TB_CULT_UNKNOWN_BASELINE == 1 ~ 1, 
                                           TB_DX_POSITIVE_ENROLL ==0 & TB_CULT_POSITIVE_ENROLL == 0 ~ 0, 
                                           TRUE~ 77)
  ) %>% 
  mutate(TB_CULT_POSITIVE_DATE = do.call(pmin, c(across(starts_with("DATE_TB_CULT_POSITIVE")), na.rm = TRUE)),
         TB_GESTAGE_CULT_POSITIVE_DAYS = as.numeric(TB_CULT_POSITIVE_DATE-ymd(PREG_START_DATE)),
         TB_GESTAGE_CULT_POSITIVE_WKS = TB_GESTAGE_CULT_POSITIVE_DAYS %/% 7
  )

## add labels
tb_labels <- tb_export %>%
  mutate(
    TB_POSITIVE_ENROLL_LABEL = factor(TB_POSITIVE_ENROLL,levels = c(1, 0, 2, 77),
                                      labels = c("TB positive at enrollment by previous Dx or culture", "TB negative culture or no previous TB Dx at enrollment", "TB culture inconclusive at enrollment", "NA/no test performed")),
    TB_POSITIVE_EVER_PREG_LABEL = factor(TB_POSITIVE_EVER_PREG,levels = c(1, 0, 77),
                                         labels = c("TB positive ever during pregnancy by previous Dx (enroll only) or culture", "TB negative culture or no previous TB Dx ever during pregnancy ", "NA/no test performed")),
    TB_CULT_POSITIVE_ENROLL_LABEL = factor(TB_CULT_POSITIVE_ENROLL,levels = c(1, 0, 2, 55, 77),
                                           labels = c("TB positive culture at enrollment", "TB negative culture at enrollment", "TB culture inconclusive at enrollment","Culture performed but missing test result", "NA/no test performed")),
    TB_CULT_POSITIVE_INCIDENT_LABEL = factor(TB_CULT_POSITIVE_INCIDENT,levels = c(1, 0),
                                             labels = c("TB culture+ during pregnancy", "No incident TB culture infection")),
    TB_CULT_UNKNOWN_BASELINE_LABEL = factor(TB_CULT_UNKNOWN_BASELINE,levels = c(1, 0),
                                            labels = c("TB culture+ during pregnancy but missing baseline status", "No incident tb infection with unknown baseline status")),
    TB_CULT_POSITIVE_EVER_PREG_LABEL = factor(TB_CULT_POSITIVE_EVER_PREG,levels = c(1, 0, 77),
                                              labels = c("TB positive culture ever during pregnancy", "TB culture+ never during pregnancy","NA/no test performed")),
  ) %>% 
  mutate(TB_POSITIVE_EVER_PREG_CAT_LABEL = case_when(TB_POSITIVE_ENROLL ==1 & TB_CULT_POSITIVE_ENROLL != 1 ~ "previous Dx+ at enroll",
                                                     TB_POSITIVE_ENROLL ==1 & TB_CULT_POSITIVE_ENROLL == 1 ~ "previous Dx+ and culture+ at enroll",
                                                     TB_POSITIVE_ENROLL !=1 & TB_CULT_POSITIVE_ENROLL == 1 ~ "culture+ at enroll",
                                                     TB_CULT_POSITIVE_INCIDENT ==1 ~ "incident culture+",
                                                     TB_CULT_UNKNOWN_BASELINE ==1 ~ "culture+ with unknown baseline",
                                                     TRUE ~ NA),
         TB_POSITIVE_EVER_PREG_CAT = case_when(TB_POSITIVE_ENROLL ==1 & TB_CULT_POSITIVE_ENROLL != 1 ~ 1,
                                               TB_POSITIVE_ENROLL ==1 & TB_CULT_POSITIVE_ENROLL == 1 ~ 2,
                                               TB_POSITIVE_ENROLL !=1 & TB_CULT_POSITIVE_ENROLL == 1 ~ 3,
                                               TB_CULT_POSITIVE_INCIDENT ==1 ~ 4,
                                               TB_CULT_UNKNOWN_BASELINE ==1 ~ 5,
                                               TRUE ~ NA))

tb_export_labels <- tb_labels %>%
  select(SITE, MOMID, PREGID, 
         TB_DX_POSITIVE_ENROLL,
         TB_CULT_PERF_ENROLL, TB_CULT_POSITIVE_ENROLL, TB_CULT_POSITIVE_ENROLL_LABEL,
         TB_CULT_PERF_EVER_PREG, TB_CULT_POSITIVE_EVER_PREG, TB_CULT_POSITIVE_EVER_PREG_LABEL,
         TB_CULT_POSITIVE_DATE, TB_GESTAGE_CULT_POSITIVE_DAYS, TB_GESTAGE_CULT_POSITIVE_WKS, 
         TB_POSITIVE_ENROLL, TB_POSITIVE_ENROLL_LABEL, 
         TB_POSITIVE_EVER_PREG, TB_POSITIVE_EVER_PREG_LABEL,
         TB_CULT_POSITIVE_INCIDENT, TB_CULT_POSITIVE_INCIDENT_LABEL,
         TB_CULT_UNKNOWN_BASELINE,TB_CULT_UNKNOWN_BASELINE_LABEL,
         TB_POSITIVE_EVER_PREG_CAT,TB_POSITIVE_EVER_PREG_CAT_LABEL
  )

# view(tb_table)
# write.xlsx(tb_table, paste0(path_to_save, "tb_table" ,".xlsx"), na="", rowNames=TRUE)



#*****************************************************************************
# EXPORTING ----
#*****************************************************************************
tb_export_labels_nodup <- tb_export_labels %>% filter(!is.na(TB_CULT_POSITIVE_ENROLL_LABEL))

#### Merge data together ----
datasets <- list(
  HEV = hev_export_labels,
  ZIK = zik_export_labels,
  CHK = chk_export_labels,
  DEN = den_export_labels,
  LEPT = lept_export_labels,
  HIV = hiv_export_labels,
  SYPH = syph_export_labels,
  MAL = mal_export_labels,
  HBV = hbv_export_labels,
  HCV = hcv_export_labels,
  CT = ct_export_labels,
  NG = ng_export_labels,
  TB = tb_export_labels_nodup
)

## THE FOLLOWING CODE WILL GENERATE A WIDE DATASET WITH ONE ROW FOR EACH MOM FOR EACH VISIT 
mat_infection <- datasets %>% reduce(full_join, by =  c("SITE","MOMID", "PREGID"))

duplicates<- tb_export_labels_nodup %>% group_by(MOMID, PREGID) %>%
  mutate(n=n()) %>% filter(n>1) %>% 
  select(SITE, MOMID, PREGID, n)

dim(mat_enroll)
dim(mat_infection)

## generate any infection variables 
enroll_infections <- c("HIV_POSITIVE_ENROLL", "SYPH_POSITIVE_ENROLL", "CT_TEST_POSITIVE_ENROLL", "NG_TEST_POSITIVE_ENROLL", "MAL_POSITIVE_ENROLL", 
                       "HBV_POSITIVE_ENROLL", "HCV_POSITIVE_ENROLL", "HEV_IGM_POSITIVE_ENROLL", "ZIK_IGM_POSITIVE_ENROLL", "CHK_IGM_POSITIVE_ENROLL", 
                       "DEN_IGM_POSITIVE_ENROLL", "LEPT_IGM_POSITIVE_ENROLL", "TB_POSITIVE_ENROLL")
ever_preg_infections <- c("HIV_POSITIVE_EVER_PREG", "SYPH_POSITIVE_EVER_PREG", "CT_TEST_POSITIVE_EVER_PREG", "NG_TEST_POSITIVE_EVER_PREG", "MAL_POSITIVE_EVER_PREG", 
                          "HBV_POSITIVE_EVER_PREG", "HCV_POSITIVE_EVER_PREG", "HEV_POSITIVE_EVER_PREG", "ZIK_POSITIVE_EVER_PREG", "CHK_POSITIVE_EVER_PREG", 
                          "DEN_POSITIVE_EVER_PREG", "LEPT_POSITIVE_EVER_PREG", "TB_POSITIVE_EVER_PREG")

incident_infections <- c("HIV_POSITIVE_INCIDENT", "SYPH_POSITIVE_INCIDENT", "CT_TEST_POSITIVE_INCIDENT", "NG_TEST_POSITIVE_INCIDENT", "MAL_POSITIVE_INCIDENT", 
                         "HBV_POSITIVE_INCIDENT", "HCV_POSITIVE_INCIDENT", "HEV_IGM_INCIDENT", "HEV_IGG_INCIDENT",  "ZIK_IGM_INCIDENT", "ZIK_IGG_INCIDENT", 
                         "CHK_IGM_INCIDENT", "CHK_IGG_INCIDENT", "DEN_IGM_INCIDENT", "DEN_IGG_INCIDENT", "LEPT_IGM_INCIDENT", "LEPT_IGG_INCIDENT", "TB_CULT_POSITIVE_INCIDENT")


mat_infection = MAT_INFECTION
mat_infection <- mat_infection %>% 
  # left_join(mat_end %>% select(SITE, PREGID, PREG_END), by = c("SITE", "PREGID")) %>% 
  mutate(ANY_INFECTION_ENROLL = case_when(HIV_POSITIVE_ENROLL ==1 | 
                                            SYPH_POSITIVE_ENROLL ==1 | 
                                            CT_TEST_POSITIVE_ENROLL ==1 | 
                                            NG_TEST_POSITIVE_ENROLL ==1 | 
                                            MAL_POSITIVE_ENROLL ==1 | 
                                            HBV_POSITIVE_ENROLL ==1 | 
                                            HCV_POSITIVE_ENROLL ==1 | 
                                            HEV_IGM_POSITIVE_ENROLL ==1 |
                                            ZIK_IGM_POSITIVE_ENROLL ==1 | 
                                            CHK_IGM_POSITIVE_ENROLL ==1 | 
                                            DEN_IGM_POSITIVE_ENROLL ==1 | 
                                            LEPT_IGM_POSITIVE_ENROLL ==1 | 
                                            TB_POSITIVE_ENROLL ==1 ~ 1, 
                                          TRUE ~ 0
  ), 
  ANY_INFECTION_EVER_PREG = case_when(HIV_POSITIVE_EVER_PREG ==1 | 
                                        SYPH_POSITIVE_EVER_PREG ==1 | 
                                        CT_TEST_POSITIVE_EVER_PREG ==1 | 
                                        NG_TEST_POSITIVE_EVER_PREG ==1 | 
                                        MAL_POSITIVE_EVER_PREG ==1 | 
                                        HBV_POSITIVE_EVER_PREG ==1 | 
                                        HCV_POSITIVE_EVER_PREG ==1 | 
                                        HEV_POSITIVE_EVER_PREG ==1 |
                                        ZIK_POSITIVE_EVER_PREG ==1 | 
                                        CHK_POSITIVE_EVER_PREG ==1 | 
                                        DEN_POSITIVE_EVER_PREG ==1 | 
                                        LEPT_POSITIVE_EVER_PREG ==1 | 
                                        TB_POSITIVE_EVER_PREG ==1 ~ 1, 
                                      TRUE ~ 0
  ),
  ANY_INFECTION_INCIDENT =  case_when(HIV_POSITIVE_INCIDENT ==1 | 
                                        SYPH_POSITIVE_INCIDENT ==1 | 
                                        CT_TEST_POSITIVE_INCIDENT ==1 | 
                                        NG_TEST_POSITIVE_INCIDENT ==1 | 
                                        MAL_POSITIVE_INCIDENT ==1 | 
                                        HBV_POSITIVE_INCIDENT ==1 | 
                                        HCV_POSITIVE_INCIDENT ==1 | 
                                        HEV_IGM_INCIDENT ==1 |
                                        HEV_IGG_INCIDENT ==1 |
                                        ZIK_IGM_INCIDENT ==1 | 
                                        ZIK_IGG_INCIDENT ==1 | 
                                        CHK_IGM_INCIDENT ==1 | 
                                        CHK_IGG_INCIDENT ==1 | 
                                        DEN_IGM_INCIDENT ==1 | 
                                        DEN_IGG_INCIDENT ==1 | 
                                        LEPT_IGM_INCIDENT ==1 | 
                                        LEPT_IGG_INCIDENT ==1 | 
                                        TB_CULT_POSITIVE_INCIDENT ==1 ~ 1, 
                                      TRUE ~ 0
  )
  )  %>%
  mutate(ANY_INFECTION_ENROLL_LIST = pmap_chr(select(., all_of(enroll_infections)), function(...) {
    vals <- c(...)
    # Remove NAs first, then check for == 1
    clean_vals <- vals[!is.na(vals)]
    keep_names <- names(clean_vals)[clean_vals == 1]
    paste(keep_names, collapse = ", ")
  })) %>% 
  mutate(ANY_INFECTION_INCIDENT_LIST = pmap_chr(select(., all_of(incident_infections)), function(...) {
    vals <- c(...)
    # Remove NAs first, then check for == 1
    clean_vals <- vals[!is.na(vals)]
    keep_names <- names(clean_vals)[clean_vals == 1]
    paste(keep_names, collapse = ", ")
  })) %>% 
  mutate(ANY_INFECTION_EVER_PREG_LIST = pmap_chr(select(., all_of(ever_preg_infections)), function(...) {
    vals <- c(...)
    # Remove NAs first, then check for == 1
    clean_vals <- vals[!is.na(vals)]
    keep_names <- names(clean_vals)[clean_vals == 1]
    paste(keep_names, collapse = ", ")
  })) %>% 
  mutate(ANY_INFECTION_ENROLL_LIST = case_when(ANY_INFECTION_ENROLL_LIST == "" ~ NA, TRUE ~ ANY_INFECTION_ENROLL_LIST),
         ANY_INFECTION_INCIDENT_LIST = case_when(ANY_INFECTION_INCIDENT_LIST == "" ~ NA, TRUE ~ ANY_INFECTION_INCIDENT_LIST),
         ANY_INFECTION_EVER_PREG_LIST = case_when(ANY_INFECTION_EVER_PREG_LIST == "" ~ NA, TRUE ~ ANY_INFECTION_EVER_PREG_LIST),
  ) 


mat_infection <- mat_infection %>%
  mutate(MAL_POSITIVE_UNKNOWN_BASELINE = case_when(MAL_RDT_POSITIVE_EVER_PREG==1 & MAL_POSITIVE_ENROLL %in% c(55,77,99,NA) ~ 1,
                                                   TRUE ~ 0))


table(mat_infection$ANY_INFECTION_ENROLL_LIST)
table(mat_infection$ANY_INFECTION_EVER_PREG_LIST)
table(mat_infection$ANY_INFECTION_INCIDENT_LIST)

test <- mat_infection %>% filter(ANY_INFECTION_ENROLL ==1) %>% 
  select(SITE, PREGID, ANY_INFECTION_ENROLL, ANY_INFECTION_ENROLL_LIST,all_of(enroll_infections)) %>% 
  filter(SITE == "Ghana")

## run quick tab of each inclusion criteria 
for (i in names(test)[-c(1:4)]) {
  print(i)
  print(table(test[[i]], useNA = "ifany"))
}
table(test$ANY_INFECTION_ENROLL_LIST)

# library(openxlsx)
write.xlsx(mat_infection, paste0("D:/Users/stacie.loisate/Documents/Output/Infection-Troubleshooting/data/", Sys.Date(),"-mat_infection" ,".xlsx"), na="", rowNames=FALSE)
write.csv(mat_infection, paste0("D:/Users/stacie.loisate/Documents/Output/Infection-Troubleshooting/data/", Sys.Date(),"-mat_infection" ,".csv"), row.names=FALSE)

path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")
write.xlsx(mat_infection, paste0(path_to_tnt, "MAT_INFECTION" ,".xlsx"), na="", row.names=FALSE)
write.csv(mat_infection, paste0(path_to_tnt, "MAT_INFECTION" ,".csv"),  row.names=FALSE)
