#*****************************************************************************
#* PRISMA Maternal Infection
#* Drafted: 25 October 2023, Stacie Loisate
#* Last updated: 1 July 2024 

## This code will generate maternal infection outcomes at the following time points: 
  # Enrollment
    # 1. HIV
    # 2. Syphilis
    # 3. Gonorrhea
    # 4. Chlamydia
    # 5. Genital Ulcers
    # 6. Malaria 
    # 7. Hepatitis
    # 8. TB

  # Any visit 
    # 1. Syphilis 
    # 2. HIV
#*****************************************************************************
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

# UPDATE EACH RUN # 
# set upload date 
UploadDate = "2024-06-28"

# set path to save 
path_to_save <- "D:/Users/stacie.loisate/Documents/PRISMA-Analysis-Stacie/Maternal-Outcomes/data/"
path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")

# set path to data
path_to_data = paste0("Z:/Stacked Data/",UploadDate)

mat_enroll <- read_csv(paste0(path_to_tnt, "MAT_ENROLL" ,".csv" )) %>% select(SITE, MOMID, PREGID, ENROLL) %>% 
  filter(ENROLL == 1)

# # import forms 
mnh02 <- read.csv(paste0(path_to_data,"/", "mnh02_merged.csv"))
mnh04 <- read.csv(paste0(path_to_data,"/", "mnh04_merged.csv"))
mnh06 <- read.csv(paste0(path_to_data,"/", "mnh06_merged.csv"))
mnh08 <- read.csv(paste0(path_to_data,"/", "mnh08_merged.csv"))

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
#* PULL IDS OF PARTICIPANTS WHO ARE ENROLLED 
# ENROLLED = meet eligibility criteria in MNH02; Section A; Questions 4-8
#*****************************************************************************

# enrolled_ids <- mnh02 %>% 
#   mutate(ENROLL = ifelse(M02_AGE_IEORRES == 1 & 
#                            M02_PC_IEORRES == 1 & 
#                            M02_CATCHMENT_IEORRES == 1 & 
#                            M02_CATCH_REMAIN_IEORRES == 1 & 
#                            M02_CONSENT_IEORRES == 1, 1, 0)) %>% 
#   select(SITE, SCRNID, MOMID, PREGID,ENROLL, M02_AGE_IEORRES, M02_PC_IEORRES, M02_CATCHMENT_IEORRES,M02_CATCH_REMAIN_IEORRES, M02_CONSENT_IEORRES) %>% 
#   filter(ENROLL == 1) %>% 
#   select(SITE, MOMID, PREGID, ENROLL) %>%
#   distinct()
# 
# enrolled_ids_vec <- as.vector(enrolled_ids$PREGID)

# extract enrolled ids for all datasets
mnh04_all_visits = mnh04 %>% right_join(mat_enroll, by = c("SITE", "MOMID", "PREGID")) %>% 
  # only want enrolled participants
  filter(ENROLL ==1) %>% 
  rename("TYPE_VISIT" = "M04_TYPE_VISIT") %>% 
  filter(TYPE_VISIT %in% c(1,2,3,4,5)) %>% 
  # Is there a form available for this participant? Defined by having any visit status
  mutate(M04_FORM_COMPLETE = ifelse(!is.na(M04_MAT_VISIT_MNH04), 1, 0)) 

mnh06_all_visits = mnh06 %>% right_join(mat_enroll, by = c("SITE", "MOMID", "PREGID")) %>% 
  # only want enrolled participants
  filter(ENROLL ==1) %>% 
  rename("TYPE_VISIT" = "M06_TYPE_VISIT") %>%
  filter(TYPE_VISIT %in% c(1,2,3,4,5)) %>% 
  # Is there a form available for this participant? Defined by having any visit status
  mutate(M06_FORM_COMPLETE = ifelse(!is.na(M06_MAT_VISIT_MNH06), 1, 0)) 


mnh08_all_visits = mnh08 %>% right_join(mat_enroll, by = c("SITE", "MOMID", "PREGID")) %>% 
  # only want enrolled participants
  filter(ENROLL ==1) %>% 
  rename("TYPE_VISIT" = "M08_TYPE_VISIT") %>%
  filter(TYPE_VISIT %in% c(1,2,3,4,5)) %>% 
  # Is there a form available for this participant? Defined by having any visit status
  mutate(M08_FORM_COMPLETE = ifelse(!is.na(M08_MAT_VISIT_MNH08), 1, 0)) 

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
#*#*****************************************************************************
#### STIs ####
#*****************************************************************************

mat_infection_sti <- mnh04_all_visits %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT,M04_FORM_COMPLETE, M04_SYPH_MHOCCUR,M04_HIV_EVER_MHOCCUR, M04_HIV_MHOCCUR,
         M04_OTHR_STI_MHOCCUR, M04_GONORRHEA_MHOCCUR, M04_CHLAMYDIA_MHOCCUR, M04_GENULCER_MHOCCUR, M04_STI_OTHR_MHOCCUR) %>% 
  # merge in mnh06 to extract rdt results 
  full_join(mnh06_all_visits[c("SITE", "MOMID", "PREGID", "TYPE_VISIT","M06_FORM_COMPLETE", "M06_SYPH_POC_LBORRES", "M06_HIV_POC_LBORRES")], 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT")) %>% 
  # generate new var defining a positive result
  mutate(
    SYPH_POSITIVE = case_when(M04_SYPH_MHOCCUR == 1 | M06_SYPH_POC_LBORRES == 1 ~ 1, TRUE ~ 0),
    HIV_POSITIVE = case_when(M04_HIV_MHOCCUR == 1 | M06_HIV_POC_LBORRES == 1 ~ 1, TRUE ~ 0),
    HIV_POSITIVE_ENROLL = case_when(M04_HIV_EVER_MHOCCUR==1 | M04_HIV_MHOCCUR==1 | M06_HIV_POC_LBORRES == 1 ~ 1, TRUE ~ 0),
    GON_POSITIVE = case_when(M04_GONORRHEA_MHOCCUR ==1  ~ 1, TRUE ~ 0),
    CHL_POSITIVE = case_when(M04_CHLAMYDIA_MHOCCUR ==1 ~ 1, TRUE ~ 0),
    GENU_POSITIVE = case_when(M04_GENULCER_MHOCCUR ==1 ~ 1, TRUE ~ 0),
    OTHR_POSITIVE = case_when(M04_OTHR_STI_MHOCCUR ==1 ~ 1, TRUE ~ 0),
    
    # is there a valid response (1,0)
    SYPH_DIAG_RESULT = case_when(M04_SYPH_MHOCCUR %in% c(1,0)~ 1, TRUE ~ 0),
    HIV_DIAG_RESULT = case_when(M04_HIV_EVER_MHOCCUR %in% c(1,0) | M04_HIV_MHOCCUR %in% c(1,0)~ 1, TRUE ~ 0),
    GON_DIAG_RESULT = case_when(M04_OTHR_STI_MHOCCUR == 1 & M04_GONORRHEA_MHOCCUR %in% c(1,0) |
                                  (M04_OTHR_STI_MHOCCUR == 0)~ 1, TRUE ~ 0),
    CHL_DIAG_RESULT = case_when(M04_OTHR_STI_MHOCCUR == 1 & M04_CHLAMYDIA_MHOCCUR %in% c(1,0) |
                                  (M04_OTHR_STI_MHOCCUR == 0)~ 1, TRUE ~ 0),
    GENU_DIAG_RESULT = case_when(M04_OTHR_STI_MHOCCUR == 1 & M04_GENULCER_MHOCCUR %in% c(1,0)|
                                   (M04_OTHR_STI_MHOCCUR == 0)~ 1, TRUE ~ 0),
    OTHR_DIAG_RESULT = case_when(M04_STI_OTHR_MHOCCUR %in% c(1,0) |
                                   (M04_OTHR_STI_MHOCCUR == 0)~ 1, TRUE ~ 0),
    SYPH_MEAS_RESULT = case_when(M06_SYPH_POC_LBORRES %in% c(1,0) ~ 1, TRUE ~ 0),
    HIV_MEAS_RESULT = case_when(M06_HIV_POC_LBORRES %in% c(1,0) ~ 1, TRUE ~ 0),
    
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
  mutate(ANY_DIAG_STI = case_when(M04_SYPH_MHOCCUR==1| M04_HIV_EVER_MHOCCUR==1 | M04_HIV_MHOCCUR==1 |  M04_GONORRHEA_MHOCCUR==1 |
                                    M04_CHLAMYDIA_MHOCCUR==1 | M04_GENULCER_MHOCCUR==1| M04_STI_OTHR_MHOCCUR==1~ 1,TRUE ~ 0),
         ANY_MEAS_STI = case_when(M06_HIV_POC_LBORRES == 1 | M06_SYPH_POC_LBORRES == 1 ~ 1, TRUE ~ 0)
         ) %>% 
  # convert to wide format
  select(SITE, MOMID, PREGID, TYPE_VISIT,M04_FORM_COMPLETE, M06_FORM_COMPLETE, SYPH_POSITIVE, HIV_POSITIVE,HIV_POSITIVE_ENROLL, GON_POSITIVE,CHL_POSITIVE, GENU_POSITIVE, OTHR_POSITIVE,
         contains("_RESULT"), contains("_MISSING"), ANY_DIAG_STI,ANY_MEAS_STI ) %>%
  pivot_wider(
    names_from = TYPE_VISIT,
    values_from = c(M04_FORM_COMPLETE, M06_FORM_COMPLETE, SYPH_POSITIVE, HIV_POSITIVE,HIV_POSITIVE_ENROLL, GON_POSITIVE,CHL_POSITIVE, GENU_POSITIVE, OTHR_POSITIVE,
                    SYPH_DIAG_RESULT, HIV_DIAG_RESULT, GON_DIAG_RESULT, CHL_DIAG_RESULT, GENU_DIAG_RESULT, OTHR_DIAG_RESULT, 
                    SYPH_DIAG_MISSING, HIV_DIAG_MISSING, GON_DIAG_MISSING, CHL_DIAG_MISSING, GENU_DIAG_MISSING, OTHR_DIAG_MISSING, 
                    SYPH_MEAS_RESULT, HIV_MEAS_RESULT, SYPH_MEAS_MISSING, HIV_MEAS_MISSING,
                    ANY_DIAG_STI, ANY_MEAS_STI),
    names_glue = "{.value}_{TYPE_VISIT}"
  ) %>% 
  # generate new var for any measure infection positive result at any visit (EXCLUDING enrollment)
  mutate(SYPH_POSITIVE_ANY_VISIT = case_when(SYPH_POSITIVE_2 ==1 | SYPH_POSITIVE_3 ==1 | SYPH_POSITIVE_4 ==1 | SYPH_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
         HIV_POSITIVE_ANY_VISIT = case_when(HIV_POSITIVE_2 ==1 | HIV_POSITIVE_3 ==1 | HIV_POSITIVE_4 ==1 | HIV_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
         GON_POSITIVE_ANY_VISIT = case_when(GON_POSITIVE_2 ==1 | GON_POSITIVE_3 ==1 | GON_POSITIVE_4 ==1 | GON_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
         CHL_POSITIVE_ANY_VISIT = case_when(CHL_POSITIVE_2 ==1 | CHL_POSITIVE_3 ==1 | CHL_POSITIVE_4 ==1 | CHL_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
         GENU_POSITIVE_ANY_VISIT = case_when(GENU_POSITIVE_2 ==1 | GENU_POSITIVE_3 ==1 | GENU_POSITIVE_4 ==1 | GENU_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
         OTHR_POSITIVE_ANY_VISIT = case_when(OTHR_POSITIVE_2 ==1 | OTHR_POSITIVE_3 ==1 | OTHR_POSITIVE_4 ==1 | OTHR_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
  ) %>% 
  # generate new var for any infection missing diagnosed and rdt at enrollment
  mutate(SYPH_MISSING_ENROLL = case_when(SYPH_DIAG_RESULT_1 ==0 & SYPH_MEAS_RESULT_1 ==0  ~1, TRUE ~ 0),
         HIV_MISSING_ENROLL = case_when(HIV_DIAG_RESULT_1 ==0 & HIV_MEAS_RESULT_1 ==0  ~1, TRUE ~ 0)
  ) %>% 
  # generate enrollment prevalnece variables (exclude missing)
  mutate(HIV_POSITIVE_ENROLL = case_when(HIV_POSITIVE_ENROLL_1 == 1 & HIV_MISSING_ENROLL==0 ~ 1, TRUE ~ 0 ),
         SYPH_POSITIVE_ENROLL = case_when(SYPH_POSITIVE_1 == 1 & SYPH_MISSING_ENROLL==0 ~ 1, TRUE ~ 0 ),
         GON_POSITIVE_ENROLL = case_when(GON_POSITIVE_1 == 1 & GON_DIAG_MISSING_1 == 0 ~ 1, TRUE ~ 0),
         CHL_POSITIVE_ENROLL = case_when(CHL_POSITIVE_1 == 1 & CHL_DIAG_MISSING_1 == 0 ~ 1, TRUE ~ 0),
         GENU_POSITIVE_ENROLL = case_when(GENU_POSITIVE_1 == 1 & GENU_DIAG_MISSING_1 == 0 ~ 1, TRUE ~ 0),
         OTHR_POSITIVE_ENROLL = case_when(OTHR_POSITIVE_1 == 1 & OTHR_DIAG_MISSING_1 == 0 ~ 1, TRUE ~ 0),
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
  # generate new var for any sti by any measurement
  mutate(STI_ANY_METHOD_ENROLL = case_when(ANY_DIAG_STI_ENROLL == 1 | ANY_MEAS_STI_ENROLL == 1 ~ 1, TRUE ~ 0)
         # STI_ANY_METHOD_DENOM = case_when(M04_FORM_COMPLETE_1 == 1 | M06_FORM_COMPLETE_1 == 1 ~ 1, TRUE ~ 0)
  )  %>% 
  # select needed vars
  select(SITE, MOMID, PREGID, ends_with("_ENROLL"), contains("_ANY_VISIT"), contains("DENOM"))
  
  


#*****************************************************************************
#### Malaria at enrollment ####
#### Hepatitis at enrollment ####
#### TB at enrollment ####
#*****************************************************************************

mat_other_infection <- mnh04_all_visits %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, M04_FORM_COMPLETE, M04_FORM_COMPLETE, 
         M04_MALARIA_EVER_MHOCCUR,M04_COVID_LBORRES, contains("M04_TB_CETERM"), M04_TB_MHOCCUR) %>% 
  # merge in mnh06 to extract rdt results 
  full_join(mnh06_all_visits[c("SITE", "MOMID", "PREGID","M06_FORM_COMPLETE", "TYPE_VISIT", "M06_MALARIA_POC_LBORRES", "M06_HBV_POC_LBORRES",
                               "M06_HCV_POC_LBORRES", "M06_COVID_POC_LBORRES")], 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT")) %>% 
  
  # merge in mnh08 to extract tb results 
  full_join(mnh08_all_visits[c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "M08_TB_CNFRM_LBORRES")], 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT")) %>% 
  distinct(SITE, MOMID, PREGID, TYPE_VISIT, .keep_all = TRUE) %>%
  ## Is the test result missing among those with a completed form?  
        # diagnosed
  mutate(MAL_DIAG_RESULT = case_when(M04_MALARIA_EVER_MHOCCUR %in% c(1,0)~ 1, TRUE ~0),
         TB_DIAG_RESULT = case_when(M04_TB_MHOCCUR %in% c(1,0)~ 1, TRUE ~0),
         # COVID_DIAG_RESULT = case_when(M04_COVID_LBORRES %in% c(1,0)~ 1, TRUE ~0),
         
         # measured
         MAL_MEAS_RESULT = case_when(M06_MALARIA_POC_LBORRES %in% c(1,0)~ 1, TRUE ~0),
         HBV_MEAS_RESULT = case_when(M06_HBV_POC_LBORRES %in% c(1,0)~ 1, TRUE ~0),
         HCV_MEAS_RESULT = case_when(M06_HCV_POC_LBORRES %in% c(1,0)~ 1, TRUE ~0),
         # COVID_MEAS_RESULT = case_when(M06_COVID_POC_LBORRES %in% c(1,0)~ 1, TRUE ~0)
         
  ) %>% 
  
  # generate new var for any infection missing diagnosed and rdt
  mutate(MAL_DIAG_MISSING = case_when(MAL_DIAG_RESULT == 0 & M04_FORM_COMPLETE==1~ 1, TRUE ~0),
         TB_DIAG_MISSING = case_when(TB_DIAG_RESULT == 0 & M04_FORM_COMPLETE==1~ 1, TRUE ~0),
         # COVID_DIAG_MISSING = case_when(COVID_DIAG_RESULT == 0 & M04_FORM_COMPLETE==1~ 1, TRUE ~0), 

         # measured
         MAL_MEAS_MISSING = case_when(MAL_MEAS_RESULT == 0 & M06_FORM_COMPLETE==1~ 1, TRUE ~0),
         HBV_MEAS_MISSING = case_when(HBV_MEAS_RESULT == 0 & M06_FORM_COMPLETE==1~ 1, TRUE ~0),
         HCV_MEAS_MISSING = case_when(HCV_MEAS_RESULT == 0 & M06_FORM_COMPLETE==1~ 1, TRUE ~0),
         # COVID_MEAS_MISSING = case_when(COVID_MEAS_RESULT == 0 & M06_FORM_COMPLETE==1~ 1, TRUE ~0),

  ) %>% 

  # generate new var for any infection missing diagnosed and rdt
  mutate(MAL_MISSING = case_when(MAL_DIAG_MISSING==1 | MAL_MEAS_MISSING == 1 ~ 1, TRUE ~ 0)
                ) %>% 
  select(-MAL_DIAG_MISSING, -MAL_MEAS_MISSING) %>% 
  # generate new var defining a positive result
  mutate(
    MAL_POSITIVE = case_when(M04_MALARIA_EVER_MHOCCUR == 1 | M06_MALARIA_POC_LBORRES == 1 ~ 1, TRUE ~ 0),
    HBV_POSITIVE = case_when(M06_HBV_POC_LBORRES == 1 ~ 1, TRUE ~ 0),
    HCV_POSITIVE = case_when(M06_HCV_POC_LBORRES == 1 ~ 1, TRUE ~ 0)
  ) %>%
  
  # TB at any visit: total with at least 1 symptom in W4SS in MNH04 (1=At least 1 symptom reported, 0=No symptoms)
  mutate(W4SS_SYMPTOMS_ANY = case_when(M04_TB_CETERM_1==1 | M04_TB_CETERM_2==1 | M04_TB_CETERM_3==1| M04_TB_CETERM_4==1~ 1, TRUE ~0),
         W4SS_RESPONSE = case_when(M04_TB_CETERM_1 %in% c(1,0) | M04_TB_CETERM_2 %in% c(1,0) |
                                     M04_TB_CETERM_3 %in% c(1,0) | M04_TB_CETERM_4 %in% c(1,0) |
                                     M04_TB_CETERM_77 %in% c(1,0) ~  1, TRUE ~ 0),
         # total number missing ALL symptoms -- right now use this 
         W4SS_MISSING_SYMP = case_when(M04_TB_CETERM_1 %in% c(55,77) & M04_TB_CETERM_2 %in% c(55,77) &
                                         M04_TB_CETERM_3 %in% c(55,77) & M04_TB_CETERM_4 %in% c(55,77) &
                                         M04_TB_CETERM_77 %in% c(55,77) ~ 1, TRUE ~ 0),
         
         TB_SYMP_POSITIVE = case_when(W4SS_SYMPTOMS_ANY == 1 ~ 1, TRUE ~ 0),
         TB_SPUTUM_POSITIVE = case_when(M08_TB_CNFRM_LBORRES == 1 ~ 1, TRUE ~ 0),
         TB_LAB_RESULT = case_when(M08_TB_CNFRM_LBORRES %in% c(1,2,0) ~ 1, TRUE ~ 0)
         
         ) %>%
  
  ## generate summary any infection variables (diagnosed, measured, lab)
  mutate(OTHER_INFECTION_DIAG_ANY = case_when(M04_MALARIA_EVER_MHOCCUR==1 | M04_TB_MHOCCUR==1 | M04_COVID_LBORRES==1 ~ 1, TRUE ~ 0),
         OTHER_INFECTION_MEAS_ANY = case_when(M06_MALARIA_POC_LBORRES==1 | M06_HBV_POC_LBORRES==1 |
                                             M06_HCV_POC_LBORRES==1 | M06_COVID_POC_LBORRES==1 ~ 1, TRUE ~ 0),
         OTHER_INFECTION_LAB_ANY = case_when(M08_TB_CNFRM_LBORRES==1 ~ 1, TRUE ~ 0)) %>% 
  
  # convert to wide format
  select(SITE, MOMID, PREGID, TYPE_VISIT, MAL_POSITIVE, HBV_POSITIVE, HCV_POSITIVE,W4SS_RESPONSE, W4SS_MISSING_SYMP,
         TB_LAB_RESULT, TB_SYMP_POSITIVE, TB_SPUTUM_POSITIVE,W4SS_MISSING_SYMP, contains("MISSING"), contains("_RESULT"),
         OTHER_INFECTION_DIAG_ANY, OTHER_INFECTION_MEAS_ANY, OTHER_INFECTION_LAB_ANY) %>%

  ## TROUBLESHOOTING
  # filter(PREGID == "AU5c8bf252-b491-4ffd-9467-43c7f00828851") %>% 
  pivot_wider(
    names_from = TYPE_VISIT,
    values_from = c(MAL_POSITIVE, HBV_POSITIVE, HCV_POSITIVE, 
                    MAL_DIAG_RESULT, TB_DIAG_RESULT, MAL_MEAS_RESULT, HBV_MEAS_RESULT, HCV_MEAS_RESULT,
                    TB_DIAG_MISSING,MAL_MISSING,HBV_MEAS_MISSING,HCV_MEAS_MISSING,
                    TB_LAB_RESULT, TB_SYMP_POSITIVE, TB_SPUTUM_POSITIVE, W4SS_MISSING_SYMP, W4SS_RESPONSE,
                    OTHER_INFECTION_DIAG_ANY, OTHER_INFECTION_MEAS_ANY, OTHER_INFECTION_LAB_ANY),
    names_glue = "{.value}_{TYPE_VISIT}"
  ) %>%
  
  # generate new var for any syphilis positive result at any visit (EXCLUDING enrollment)
  mutate(MAL_POSITIVE_ANY_VISIT = case_when(MAL_POSITIVE_2 ==1 | MAL_POSITIVE_3 ==1 | MAL_POSITIVE_4 ==1 | MAL_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
         HBV_POSITIVE_ANY_VISIT = case_when(HBV_POSITIVE_2 ==1 | HBV_POSITIVE_3 ==1 | HBV_POSITIVE_4 ==1 | HBV_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
         HCV_POSITIVE_ANY_VISIT = case_when(HCV_POSITIVE_2 ==1 | HCV_POSITIVE_3 ==1 | HCV_POSITIVE_4 ==1 | HCV_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
         TB_SYMP_POSITIVE_ANY_VISIT = case_when(TB_SYMP_POSITIVE_2 ==1 | TB_SYMP_POSITIVE_3 ==1 | TB_SYMP_POSITIVE_4 ==1 | TB_SYMP_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
         TB_SPUTUM_POSITIVE_ANY_VISIT = case_when(TB_SPUTUM_POSITIVE_2 ==1 | TB_SPUTUM_POSITIVE_3 ==1 | TB_SPUTUM_POSITIVE_4 ==1 | TB_SPUTUM_POSITIVE_5 ==1 ~ 1, TRUE ~ 0)
  )%>% 
  # generate missing variables 
  mutate(MAL_MISSING_ENROLL = case_when(MAL_MISSING_1==1 ~ 1, TRUE ~ 0),
         HBV_MEAS_MISSING_ENROLL = case_when(HBV_MEAS_MISSING_1==1 ~ 1, TRUE ~ 0),
         HCV_MEAS_MISSING_ENROLL = case_when(HCV_MEAS_MISSING_1==1 ~ 1, TRUE ~ 0),
         W4SS_MISSING_SYMP_ENROLL = case_when(W4SS_MISSING_SYMP_1==1 ~ 1, TRUE ~ 0)
         ) %>% 

  # generate enrollment prevalence variables (exclude missing)
  mutate(MAL_POSITIVE_ENROLL = case_when(MAL_POSITIVE_1 == 1 & MAL_MISSING_ENROLL==0 ~ 1, TRUE ~ 0 ),
         HBV_POSITIVE_ENROLL = case_when(HBV_POSITIVE_1 == 1 & HBV_MEAS_MISSING_ENROLL==0 ~ 1, TRUE ~ 0 ),
         HCV_POSITIVE_ENROLL = case_when(HCV_POSITIVE_1 == 1 & HCV_MEAS_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
         TB_SYMP_POSITIVE_ENROLL = case_when(TB_SYMP_POSITIVE_1 == 1 ~ 1, TRUE ~ 0),
         TB_SPUTUM_POSITIVE_ENROLL = case_when(TB_SPUTUM_POSITIVE_1 == 1 ~ 1, TRUE ~ 0), 
         W4SS_RESPONSE_ENROLL = case_when(W4SS_RESPONSE_1 == 1 ~ 1, TRUE ~ 0), 
         TB_LAB_RESULT_ENROLL = case_when(TB_LAB_RESULT_1 == 1 ~ 1, TRUE ~ 0), 
  ) %>% 
  # generate summary variables at enrollment 
  mutate(OTHER_INFECTION_DIAG_ANY_ENROLL = case_when(OTHER_INFECTION_DIAG_ANY_1 ==1 ~ 1, TRUE ~ 0),
         OTHER_INFECTION_MEAS_ANY_ENROLL = case_when(OTHER_INFECTION_MEAS_ANY_1 ==1 ~ 1, TRUE ~ 0),
         OTHER_INFECTION_LAB_ANY_ENROLL = case_when(OTHER_INFECTION_LAB_ANY_1 ==1 ~ 1, TRUE ~ 0),
 ) %>% 
  select(SITE, MOMID, PREGID, ends_with("ENROLL"), contains("_ANY_VISIT"), contains("DENOM")
  )


#*****************************************************************************
#### All infections combined NEW ####
#*****************************************************************************

MAT_INFECTION <- full_join(mat_infection_sti, mat_other_infection, by = c("SITE", "MOMID", "PREGID")) %>% 
    # merge in enrollment indicator
    full_join(mat_enroll, by = c("SITE", "MOMID", "PREGID")) %>% 
  # generate variables for any infection diagnosed 
  mutate(ANY_INFECTION_DIAGNOSED_ENROLL = case_when(ANY_DIAG_STI_ENROLL == 1 | OTHER_INFECTION_DIAG_ANY_ENROLL==1~1, TRUE ~0),
         # generate variables for any infection diagnosed 
         ANY_INFECTION_MEASURED_ENROLL = case_when(ANY_MEAS_STI_ENROLL == 1 | OTHER_INFECTION_MEAS_ANY_ENROLL==1~1, TRUE ~0), 
         # generate variables for any infection with either method 
         INFECTION_ANY_METHOD_ENROLL = case_when(ANY_INFECTION_DIAGNOSED_ENROLL == 1 | ANY_INFECTION_MEASURED_ENROLL==1~1, TRUE ~0)
  ) %>% 
  # generate denominators for any infection diagnosed by either method
  mutate(INFECTION_ENROLL_DENOM = case_when(ENROLL==1 ~ 1, TRUE ~ 0)) %>% # INFECTION_ANY_METHOD_DENOM = 1
  select(-ENROLL, -OTHER_INFECTION_DIAG_ANY_ENROLL, -OTHER_INFECTION_MEAS_ANY_ENROLL, -OTHER_INFECTION_LAB_ANY_ENROLL)

print(sum(MAT_INFECTION$INFECTION_ENROLL_DENOM))
print(dim(mat_enroll)[1])

# save data set; this will get called into the report
write.csv(MAT_INFECTION, paste0(path_to_save, "MAT_INFECTION" ,".csv"), na="", row.names=FALSE)
write.csv(MAT_INFECTION, paste0(path_to_tnt, "MAT_INFECTION" ,".csv"), na="", row.names=FALSE)


