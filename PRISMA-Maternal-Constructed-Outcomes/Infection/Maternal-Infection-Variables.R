#*****************************************************************************
#* PRISMA Maternal Infection
#* Drafted: 25 October 2023, Stacie Loisate
#* Last updated: 23 August 2024 

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

mat_enroll <- read_csv(paste0(path_to_tnt, "MAT_ENROLL" ,".csv" )) %>% select(SITE, MOMID, PREGID, ENROLL, M02_SCRN_OBSSTDAT) %>% 
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

#*#*****************************************************************************
#### STIs ####
#*****************************************************************************

# extract enrolled ids for all datasets
mnh04_all_visits = mnh04 %>% right_join(mat_enroll, by = c("SITE", "MOMID", "PREGID")) %>% 
  # only want enrolled participants
  filter(ENROLL ==1) %>% 
  rename("TYPE_VISIT" = "M04_TYPE_VISIT") %>% 
  filter(TYPE_VISIT %in% c(1,2,3,4,5)) %>% 
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
  filter(TYPE_VISIT %in% c(1,2,3,4,5)) %>% 
  # Is there a form available for this participant? Defined by having any visit status
  mutate(M06_FORM_COMPLETE = ifelse(!is.na(M06_MAT_VISIT_MNH06), 1, 0)) %>% 
  mutate(MALARIA_TESTING = case_when(SITE == "Pakistan" & M06_DIAG_VSDAT < "2024-05-01" ~ 1, 
                                     SITE == "India-CMC" & M06_DIAG_VSDAT < "2024-06-13" ~ 1,
                                     SITE == "India-SAS" & M06_DIAG_VSDAT < "2024-05-31" ~ 1,
                                     SITE %in% c("Zambia", "Kenya", "Ghana") ~ 1, 
                                     TRUE ~ 0 
                                     
  ))


## i think ghana is using 1, positive, 2, negative, 3, inconclusive (should be 1, positive; 2, negative; 3, inconclusive)
mnh08_all_visits = mnh08 %>% right_join(mat_enroll, by = c("SITE", "MOMID", "PREGID")) %>% 
  # only want enrolled participants
  filter(ENROLL ==1) %>% 
  rename("TYPE_VISIT" = "M08_TYPE_VISIT") %>%
  filter(TYPE_VISIT %in% c(1,2,3,4,5)) %>% 
  select(SITE, MOMID, PREGID,M02_SCRN_OBSSTDAT, M08_LBSTDAT, M08_MAT_VISIT_MNH08, TYPE_VISIT, M08_TB_CNFRM_LBORRES,
         contains("ZCD"), M08_LEPT_LBPERF_1, M08_LEPT_IGM_LBORRES, 
         M08_LEPT_LBPERF_2, M08_LEPT_IGG_LBORRES, M08_HEV_LBPERF_1, M08_HEV_IGM_LBORRES,
         M08_HEV_LBPERF_2, M08_HEV_IGG_LBORRES, M08_LB_EXPANSION) %>% 
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
                                     
                                     ))
 
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
#*#***************************************************************************
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
    HIV_POSITIVE_ENROLL = case_when(TYPE_VISIT ==1 & (M04_HIV_EVER_MHOCCUR==1 | M04_HIV_MHOCCUR==1 | M06_HIV_POC_LBORRES == 1) ~ 1, TRUE ~ 0),
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
                                    M04_CHLAMYDIA_MHOCCUR==1 | M04_GENULCER_MHOCCUR==1| 
                                    M04_STI_OTHR_MHOCCUR==1~ 1,TRUE ~ 0),
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
  mutate(SYPH_POSITIVE_ANY_VISIT = case_when(SYPH_POSITIVE_1 != 1 & (SYPH_POSITIVE_2 ==1 | SYPH_POSITIVE_3 ==1 | SYPH_POSITIVE_4 ==1 | SYPH_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
         HIV_POSITIVE_ANY_VISIT = case_when(HIV_POSITIVE_1 != 1 & HIV_POSITIVE_ENROLL_1 !=1 & (HIV_POSITIVE_2 ==1 | HIV_POSITIVE_3 ==1 | HIV_POSITIVE_4 ==1 | HIV_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
         GON_POSITIVE_ANY_VISIT = case_when(GON_POSITIVE_1 != 1 & (GON_POSITIVE_2 ==1 | GON_POSITIVE_3 ==1 | GON_POSITIVE_4 ==1 | GON_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
         CHL_POSITIVE_ANY_VISIT = case_when(CHL_POSITIVE_1 != 1 & (CHL_POSITIVE_2 ==1 | CHL_POSITIVE_3 ==1 | CHL_POSITIVE_4 ==1 | CHL_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
         GENU_POSITIVE_ANY_VISIT = case_when(GENU_POSITIVE_1 != 1 & (GENU_POSITIVE_2 ==1 | GENU_POSITIVE_3 ==1 | GENU_POSITIVE_4 ==1 | GENU_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
         OTHR_POSITIVE_ANY_VISIT = case_when(OTHR_POSITIVE_1 != 1 & (OTHR_POSITIVE_2 ==1 | OTHR_POSITIVE_3 ==1 | OTHR_POSITIVE_4 ==1 | OTHR_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
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
  
table(mat_infection_sti$HIV_POSITIVE_ANY_VISIT, mat_infection_sti$SITE)


#*****************************************************************************
#### Malaria at enrollment ####
#### Hepatitis at enrollment ####
#### TB at enrollment ####
#*****************************************************************************
mat_other_infection <- mnh04_all_visits %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, M04_FORM_COMPLETE, M04_FORM_COMPLETE, MALARIA_TESTING,
         M04_MALARIA_EVER_MHOCCUR,M04_COVID_LBORRES, contains("M04_TB_CETERM"), M04_TB_MHOCCUR) %>% 
  # merge in mnh06 to extract rdt results 
  full_join(mnh06_all_visits[c("SITE", "MOMID", "PREGID","M06_FORM_COMPLETE", "TYPE_VISIT", "M06_MALARIA_POC_LBORRES", "M06_HBV_POC_LBORRES",
                               "M06_HCV_POC_LBORRES", "M06_COVID_POC_LBORRES", "MALARIA_TESTING")], 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "MALARIA_TESTING")) %>% 
  
  # merge in mnh08 to extract tb results 
  full_join(mnh08_all_visits[c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "M08_TB_CNFRM_LBORRES", "MALARIA_TESTING")], 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "MALARIA_TESTING")) %>% 
  distinct(SITE, MOMID, PREGID, TYPE_VISIT, .keep_all = TRUE) %>%
  ## Is the test result missing among those with a completed form?  
  # diagnosed
  mutate(MAL_DIAG_RESULT = case_when(MALARIA_TESTING == 1 & M04_MALARIA_EVER_MHOCCUR %in% c(1,0)~ 1, TRUE ~0),
         TB_DIAG_RESULT = case_when(M04_TB_MHOCCUR %in% c(1,0)~ 1, TRUE ~0),

         # measured
         MAL_MEAS_RESULT = case_when(MALARIA_TESTING == 1 & M06_MALARIA_POC_LBORRES %in% c(1,0)~ 1, TRUE ~0),
         HBV_MEAS_RESULT = case_when(M06_HBV_POC_LBORRES %in% c(1,0)~ 1, TRUE ~0),
         HCV_MEAS_RESULT = case_when(M06_HCV_POC_LBORRES %in% c(1,0)~ 1, TRUE ~0),

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
    MAL_POSITIVE = case_when(MALARIA_TESTING==1 & (M04_MALARIA_EVER_MHOCCUR == 1 | M06_MALARIA_POC_LBORRES == 1) ~ 1, TRUE ~ 0),
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
  select(SITE, MOMID, PREGID, TYPE_VISIT,MALARIA_TESTING, MAL_POSITIVE, HBV_POSITIVE, HCV_POSITIVE,W4SS_RESPONSE, W4SS_MISSING_SYMP,
         TB_LAB_RESULT, TB_SYMP_POSITIVE, TB_SPUTUM_POSITIVE,W4SS_MISSING_SYMP, contains("MISSING"), contains("_RESULT"),
         OTHER_INFECTION_DIAG_ANY, OTHER_INFECTION_MEAS_ANY, OTHER_INFECTION_LAB_ANY) %>%
  
  pivot_wider(
    names_from = TYPE_VISIT,
    values_from = c(MAL_POSITIVE, HBV_POSITIVE, HCV_POSITIVE, MALARIA_TESTING,
                    MAL_DIAG_RESULT, TB_DIAG_RESULT, MAL_MEAS_RESULT, HBV_MEAS_RESULT, HCV_MEAS_RESULT,
                    TB_DIAG_MISSING,MAL_MISSING,HBV_MEAS_MISSING,HCV_MEAS_MISSING,
                    TB_LAB_RESULT, TB_SYMP_POSITIVE, TB_SPUTUM_POSITIVE, W4SS_MISSING_SYMP, W4SS_RESPONSE,
                    OTHER_INFECTION_DIAG_ANY, OTHER_INFECTION_MEAS_ANY, OTHER_INFECTION_LAB_ANY),
    names_glue = "{.value}_{TYPE_VISIT}"
  ) %>%
  
  # generate new var for any syphilis positive result at any visit (EXCLUDING enrollment)
  mutate(
              MAL_POSITIVE_ANY_VISIT = case_when(MALARIA_TESTING_1==1 & MAL_POSITIVE_1 != 1 & (MAL_POSITIVE_2 ==1 | MAL_POSITIVE_3 ==1 | MAL_POSITIVE_4 ==1 | MAL_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
              HBV_POSITIVE_ANY_VISIT = case_when(HBV_POSITIVE_1 != 1 & (HBV_POSITIVE_2 ==1 | HBV_POSITIVE_3 ==1 | HBV_POSITIVE_4 ==1 | HBV_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
              HCV_POSITIVE_ANY_VISIT = case_when(HCV_POSITIVE_1 != 1 & (HCV_POSITIVE_2 ==1 | HCV_POSITIVE_3 ==1 | HCV_POSITIVE_4 ==1 | HCV_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
              TB_SYMP_POSITIVE_ANY_VISIT = case_when(TB_SYMP_POSITIVE_1 != 1 & (TB_SYMP_POSITIVE_2 ==1 | TB_SYMP_POSITIVE_3 ==1 | TB_SYMP_POSITIVE_4 ==1 | TB_SYMP_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
              TB_SPUTUM_POSITIVE_ANY_VISIT = case_when(TB_SPUTUM_POSITIVE_1 != 1 & (TB_SPUTUM_POSITIVE_2 ==1 | TB_SPUTUM_POSITIVE_3 ==1 | TB_SPUTUM_POSITIVE_4 ==1 | TB_SPUTUM_POSITIVE_5 ==1) ~ 1, TRUE ~ 0)
                
         )%>% 
  # generate missing variables 
  mutate(MAL_MISSING_ENROLL = case_when(MALARIA_TESTING_1 == 1 & MAL_MISSING_1==1 ~ 1, TRUE ~ 0),
         HBV_MEAS_MISSING_ENROLL = case_when(HBV_MEAS_MISSING_1==1 ~ 1, TRUE ~ 0),
         HCV_MEAS_MISSING_ENROLL = case_when(HCV_MEAS_MISSING_1==1 ~ 1, TRUE ~ 0),
         W4SS_MISSING_SYMP_ENROLL = case_when(W4SS_MISSING_SYMP_1==1 ~ 1, TRUE ~ 0)
  ) %>% 
  
  # generate enrollment prevalence variables (exclude missing)
  mutate(MAL_POSITIVE_ENROLL = case_when(MALARIA_TESTING_1==1 & MAL_POSITIVE_1 == 1 & MAL_MISSING_ENROLL==0 ~ 1, TRUE ~ 0 ),
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
# 
# TEST <- mat_other_infection %>% select(SITE, MOMID, PREGID, contains("TB")) %>%
#   filter(TB_SYMP_POSITIVE_ANY_VISIT==1)
#  # out <- mat_other_infection
# test2 <- out %>% select(SITE, MOMID, PREGID,TYPE_VISIT, contains("TB"))

mat_expansion_infection <- mnh08_all_visits %>% 
  filter(ENROLL_EXPANSION==1) %>% 

  distinct(SITE, MOMID, PREGID, TYPE_VISIT, .keep_all = TRUE) %>%
  ## Is the test result missing among those with a completed form?  
  # diagnosed
   mutate(
         HEV_IGM_RESULT = case_when(M08_HEV_IGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
         HEV_IGG_RESULT = case_when(M08_HEV_IGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
         
         ## zcd
         ZIK_IGM_RESULT = case_when(M08_ZCD_CHKIGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
         ZIK_IGG_RESULT = case_when(M08_ZCD_CHKIGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
         DEN_IGM_RESULT = case_when(M08_ZCD_DENIGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
         DEN_IGG_RESULT = case_when(M08_ZCD_DENIGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
         CHK_IGM_RESULT = case_when(M08_ZCD_CHKIGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
         CHK_IGG_RESULT = case_when(M08_ZCD_CHKIGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),

         # lepto
         LEP_IGM_RESULT = case_when(M08_LEPT_IGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
         LEP_IGG_RESULT = case_when(M08_LEPT_IGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0)
         # 
  ) %>% 
  
  # generate new var for any infection missing diagnosed and rdt
  mutate(
         HEV_IGM_MISSING = case_when(HEV_IGM_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
         HEV_IGG_MISSING = case_when(HEV_IGG_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
         
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
    HEV_IGM_POSITIVE = case_when(M08_HEV_IGM_LBORRES == 1 ~ 1, TRUE ~ 0),
    HEV_IGG_POSITIVE = case_when(M08_HEV_IGG_LBORRES == 1 ~ 1, TRUE ~ 0),
    
    # ZCD
    ZIK_IGM_POSITIVE = case_when(M08_ZCD_ZIKIGM_LBORRES == 1 ~ 1, TRUE ~ 0),
    ZIK_IGG_POSITIVE = case_when(M08_ZCD_ZIKIGG_LBORRES == 1 ~ 1, TRUE ~ 0),
    DEN_IGM_POSITIVE = case_when(M08_ZCD_DENIGM_LBORRES == 1 ~ 1, TRUE ~ 0), 
    DEN_IGG_POSITIVE = case_when(M08_ZCD_DENIGG_LBORRES == 1 ~ 1, TRUE ~ 0), 
    CHK_IGM_POSITIVE = case_when(M08_ZCD_CHKIGM_LBORRES == 1 ~ 1, TRUE ~ 0), 
    CHK_IGG_POSITIVE = case_when(M08_ZCD_CHKIGG_LBORRES == 1 ~ 1, TRUE ~ 0),
    
    # LEPTO
    LEP_IGM_POSITIVE = case_when(M08_LEPT_IGM_LBORRES == 1 ~ 1, TRUE ~ 0), 
    LEP_IGG_POSITIVE = case_when(M08_LEPT_IGG_LBORRES == 1 ~ 1, TRUE ~ 0)
    
    
  ) %>%
  # generate summary any infection variables (diagnosed, measured, lab)
  mutate(
    OTHER_INFECTION_MEAS_EXPANSION_ANY = case_when(M08_ZCD_ZIKIGM_LBORRES ==1 | M08_ZCD_ZIKIGG_LBORRES == 1|
                                                M08_ZCD_DENIGM_LBORRES ==1 | M08_ZCD_DENIGG_LBORRES == 1|
                                                M08_ZCD_CHKIGM_LBORRES ==1 | M08_ZCD_CHKIGG_LBORRES == 1|
                                                M08_LEPT_IGM_LBORRES ==1 | M08_LEPT_IGG_LBORRES ==1 |
                                                M08_HEV_IGM_LBORRES ==1 | M08_HEV_IGG_LBORRES ==1 ~ 1, TRUE ~ 0)) %>%
  
  # convert to wide format
  select(SITE, MOMID, PREGID, TYPE_VISIT, ENROLL_EXPANSION, OTHER_INFECTION_MEAS_EXPANSION_ANY, 
         ZIK_IGM_POSITIVE, ZIK_IGG_POSITIVE, DEN_IGM_POSITIVE, DEN_IGG_POSITIVE, CHK_IGM_POSITIVE, CHK_IGG_POSITIVE,
         ZIK_IGM_MISSING, ZIK_IGG_MISSING,DEN_IGM_MISSING, DEN_IGG_MISSING, CHK_IGM_MISSING, CHK_IGG_MISSING,
         HEV_IGM_POSITIVE, HEV_IGG_POSITIVE, HEV_IGM_MISSING, HEV_IGG_MISSING, 
         LEP_IGM_POSITIVE, LEP_IGG_POSITIVE, LEP_IGM_MISSING, LEP_IGG_MISSING, contains("MISSING"), contains("_RESULT")) %>%
  
  ## TROUBLESHOOTING
  pivot_wider(
    names_from = TYPE_VISIT,
    values_from = c(ENROLL_EXPANSION,OTHER_INFECTION_MEAS_EXPANSION_ANY,
                    ZIK_IGM_POSITIVE, ZIK_IGG_POSITIVE, DEN_IGM_POSITIVE, DEN_IGG_POSITIVE, CHK_IGM_POSITIVE, CHK_IGG_POSITIVE, 
                    ZIK_IGM_MISSING, ZIK_IGG_MISSING,DEN_IGM_MISSING, DEN_IGG_MISSING, CHK_IGM_MISSING, CHK_IGG_MISSING,
                    HEV_IGM_POSITIVE, HEV_IGG_POSITIVE, HEV_IGM_MISSING, HEV_IGG_MISSING, 
                    LEP_IGM_POSITIVE, LEP_IGG_POSITIVE, LEP_IGM_MISSING, LEP_IGG_MISSING),
    names_glue = "{.value}_{TYPE_VISIT}"
  ) %>%
  
# generate new var for any syphilis positive result at any visit (EXCLUDING enrollment) ON HOLD
  # mutate(
  #        HEV_IGM_POSITIVE_ANY_VISIT = case_when(HEV_IGM_POSITIVE_2 ==1 | HEV_IGM_POSITIVE_3 ==1 | HEV_IGM_POSITIVE_4 ==1 | HEV_IGM_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
  #        HEV_IGG_POSITIVE_ANY_VISIT = case_when(HEV_IGG_POSITIVE_2 ==1 | HEV_IGG_POSITIVE_3 ==1 | HEV_IGG_POSITIVE_4 ==1 | HEV_IGG_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
  #        
  #        # ZCD
  #        ZIK_IGM_POSITIVE_ANY_VISIT = case_when(ZIK_IGM_POSITIVE_2 ==1 | ZIK_IGM_POSITIVE_3 ==1 | ZIK_IGM_POSITIVE_4 ==1 | ZIK_IGM_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
  #        ZIK_IGG_POSITIVE_ANY_VISIT = case_when(ZIK_IGG_POSITIVE_2 ==1 | ZIK_IGG_POSITIVE_3 ==1 | ZIK_IGG_POSITIVE_4 ==1 | ZIK_IGG_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
  #        DEN_IGM_POSITIVE_ANY_VISIT = case_when(DEN_IGM_POSITIVE_2 ==1 | DEN_IGM_POSITIVE_3 ==1 | DEN_IGM_POSITIVE_4 ==1 | DEN_IGM_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
  #        DEN_IGG_POSITIVE_ANY_VISIT = case_when(DEN_IGG_POSITIVE_2 ==1 | DEN_IGG_POSITIVE_3 ==1 | DEN_IGG_POSITIVE_4 ==1 | DEN_IGG_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
  #        CHK_IGM_POSITIVE_ANY_VISIT = case_when(CHK_IGM_POSITIVE_2 ==1 | CHK_IGM_POSITIVE_3 ==1 | CHK_IGM_POSITIVE_4 ==1 | CHK_IGM_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
  #        CHK_IGG_POSITIVE_ANY_VISIT = case_when(CHK_IGG_POSITIVE_2 ==1 | CHK_IGG_POSITIVE_3 ==1 | CHK_IGG_POSITIVE_4 ==1 | CHK_IGG_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
  #        
  #        # LEPTO
  #        LEP_IGM_POSITIVE_ANY_VISIT = case_when(LEP_IGM_POSITIVE_2 ==1 | LEP_IGM_POSITIVE_3 ==1 | LEP_IGM_POSITIVE_4 ==1 | LEP_IGM_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
  #        LEP_IGG_POSITIVE_ANY_VISIT = case_when(LEP_IGG_POSITIVE_2 ==1 | LEP_IGG_POSITIVE_3 ==1 | LEP_IGG_POSITIVE_4 ==1 | LEP_IGG_POSITIVE_5 ==1 ~ 1, TRUE ~ 0),
  #                
# HEV_IGM_POSITIVE_ANY_VISIT = case_when(HEV_IGM_POSITIVE_1 != 1 & (HEV_IGM_POSITIVE_2 ==1 | HEV_IGM_POSITIVE_3 ==1 | HEV_IGM_POSITIVE_4 ==1 | HEV_IGM_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
# HEV_IGG_POSITIVE_ANY_VISIT = case_when(HEV_IGG_POSITIVE_1 != 1 & (HEV_IGG_POSITIVE_2 ==1 | HEV_IGG_POSITIVE_3 ==1 | HEV_IGG_POSITIVE_4 ==1 | HEV_IGG_POSITIVE_5 ==1)~ 1, TRUE ~ 0),
# 
# # ZCD
# ZIK_IGM_POSITIVE_ANY_VISIT = case_when(ZIK_IGM_POSITIVE_1 != 1 & (ZIK_IGM_POSITIVE_2 ==1 | ZIK_IGM_POSITIVE_3 ==1 | ZIK_IGM_POSITIVE_4 ==1 | ZIK_IGM_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
# ZIK_IGG_POSITIVE_ANY_VISIT = case_when(ZIK_IGG_POSITIVE_1 != 1 & (ZIK_IGG_POSITIVE_2 ==1 | ZIK_IGG_POSITIVE_3 ==1 | ZIK_IGG_POSITIVE_4 ==1 | ZIK_IGG_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
# DEN_IGM_POSITIVE_ANY_VISIT = case_when(DEN_IGM_POSITIVE_1 != 1 & (DEN_IGM_POSITIVE_2 ==1 | DEN_IGM_POSITIVE_3 ==1 | DEN_IGM_POSITIVE_4 ==1 | DEN_IGM_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
# DEN_IGG_POSITIVE_ANY_VISIT = case_when(DEN_IGG_POSITIVE_1 != 1 & (DEN_IGG_POSITIVE_2 ==1 | DEN_IGG_POSITIVE_3 ==1 | DEN_IGG_POSITIVE_4 ==1 | DEN_IGG_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
# CHK_IGM_POSITIVE_ANY_VISIT = case_when(CHK_IGM_POSITIVE_1 != 1 & (CHK_IGM_POSITIVE_2 ==1 | CHK_IGM_POSITIVE_3 ==1 | CHK_IGM_POSITIVE_4 ==1 | CHK_IGM_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
# CHK_IGG_POSITIVE_ANY_VISIT = case_when(CHK_IGG_POSITIVE_1 != 1 & (CHK_IGG_POSITIVE_2 ==1 | CHK_IGG_POSITIVE_3 ==1 | CHK_IGG_POSITIVE_4 ==1 | CHK_IGG_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
# 
# # LEPTO
# LEP_IGM_POSITIVE_ANY_VISIT = case_when(LEP_IGM_POSITIVE_1 != 1 & (LEP_IGM_POSITIVE_2 ==1 | LEP_IGM_POSITIVE_3 ==1 | LEP_IGM_POSITIVE_4 ==1 | LEP_IGM_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
# LEP_IGG_POSITIVE_ANY_VISIT = case_when(LEP_IGG_POSITIVE_1 != 1 & (LEP_IGG_POSITIVE_2 ==1 | LEP_IGG_POSITIVE_3 ==1 | LEP_IGG_POSITIVE_4 ==1 | LEP_IGG_POSITIVE_5 ==1) ~ 1, TRUE ~ 0)



  # )%>% 
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
         HEV_IGM_POSITIVE_ENROLL = case_when(HEV_IGM_POSITIVE_1 == 1 & HEV_IGM_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
         HEV_IGG_POSITIVE_ENROLL = case_when(HEV_IGG_POSITIVE_1 == 1 & HEV_IGG_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
         
         #ZCD
         ZIK_IGM_POSITIVE_ENROLL = case_when(ZIK_IGM_POSITIVE_1 == 1 & ZIK_IGM_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
         ZIK_IGG_POSITIVE_ENROLL = case_when(ZIK_IGG_POSITIVE_1 == 1 & ZIK_IGG_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
         DEN_IGM_POSITIVE_ENROLL = case_when(DEN_IGM_POSITIVE_1 == 1 & DEN_IGM_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
         DEN_IGG_POSITIVE_ENROLL = case_when(DEN_IGG_POSITIVE_1 == 1 & DEN_IGG_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
         CHK_IGM_POSITIVE_ENROLL = case_when(CHK_IGM_POSITIVE_1 == 1 & CHK_IGM_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
         CHK_IGG_POSITIVE_ENROLL = case_when(CHK_IGG_POSITIVE_1 == 1 & CHK_IGG_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
         
         #LEPTO
         LEP_IGM_POSITIVE_ENROLL = case_when(LEP_IGM_POSITIVE_1 == 1 & LEP_IGM_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
         LEP_IGG_POSITIVE_ENROLL = case_when(LEP_IGG_POSITIVE_1 == 1 & LEP_IGG_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0)
         
         
  ) %>% 
  select(SITE, MOMID, PREGID, ENROLL_EXPANSION_1, ends_with("ENROLL"), contains("_ANY_VISIT"), contains("DENOM"),
         OTHER_INFECTION_MEAS_EXPANSION_ANY_1
  )


#*****************************************************************************
#### All infections combined NEW ####
#*****************************************************************************

# MAT_INFECTION <- full_join(mat_infection_sti, out, by = c("SITE", "MOMID", "PREGID")) %>% 
#     # merge in enrollment indicator
#     full_join(mat_enroll, by = c("SITE", "MOMID", "PREGID")) %>% 
#   # generate variables for any infection diagnosed 
#   mutate(ANY_INFECTION_DIAGNOSED_ENROLL = case_when(ANY_DIAG_STI_ENROLL == 1 | OTHER_INFECTION_DIAG_ANY_ENROLL==1~1, TRUE ~0),
#          # generate variables for any infection diagnosed 
#          ANY_INFECTION_MEASURED_ENROLL = case_when(ANY_MEAS_STI_ENROLL == 1 | OTHER_INFECTION_MEAS_ANY_ENROLL==1~1, TRUE ~0), 
#          # generate variables for any infection with either method 
#          INFECTION_ANY_METHOD_ENROLL = case_when(ANY_INFECTION_DIAGNOSED_ENROLL == 1 | ANY_INFECTION_MEASURED_ENROLL==1~1, TRUE ~0)
#   ) %>% 
#   # generate denominators for any infection diagnosed by either method
#   mutate(INFECTION_ENROLL_DENOM = case_when(ENROLL==1 ~ 1, TRUE ~ 0)) %>% # INFECTION_ANY_METHOD_DENOM = 1
#   select(-ENROLL, -OTHER_INFECTION_DIAG_ANY_ENROLL, -OTHER_INFECTION_MEAS_ANY_ENROLL, -OTHER_INFECTION_LAB_ANY_ENROLL)

test <- MAT_INFECTION %>% filter(is.na(OTHER_INFECTION_DIAG_ANY_ENROLL))
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

# # save data set; this will get called into the report
write.csv(MAT_INFECTION, paste0(path_to_save, "MAT_INFECTION" ,".csv"), na="", row.names=FALSE)
# write.csv(MAT_INFECTION, paste0(path_to_tnt, "MAT_INFECTION" ,".csv"), na="", row.names=FALSE)
# 
table(mat_enroll$ENROLL, mat_enroll$SITE)


mat_infection <-read.csv(paste0("Z:/Outcome Data/2024-06-28/MAT_INFECTION.csv"))
mat_end <-read_dta(paste0("Z:/Outcome Data/2024-06-28/MAT_ENDPOINTS.dta"))

table(mat_end$PREG_END)

mat_infection_full <- mat_infection %>% 
  full_join(mat_end[c("SITE", "MOMID", "PREGID", "PREG_END")], by = c("SITE", "MOMID", "PREGID"))

table(mat_infection_full$SYPH_POSITIVE_ENROLL)


## reorder to be output vars and then missing vars 
out <- mat_infection_full %>% 
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    "N with enrollment visit" = paste0(
      format(sum(INFECTION_ENROLL_DENOM == 1, na.rm = TRUE), nsmall = 0, digits = 2)),
    
    "N with pregnancy end" = paste0(
      format(sum(PREG_END == 1, na.rm = TRUE), nsmall = 0, digits = 2)),
    
    # Syphilis
    "Syphilis prevalence at enrollment (positive RDT or diagnosed)" = paste0(
      format(sum(SYPH_POSITIVE_ENROLL == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SYPH_POSITIVE_ENROLL == 1, na.rm = TRUE)/sum(INFECTION_ENROLL_DENOM ==1 & SYPH_MISSING_ENROLL == 0, na.rm=TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Syphilis incidence following enrollment (positive RDT or diagnosed) ^b^" = paste0(
      format(sum(SYPH_POSITIVE_ANY_VISIT == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SYPH_POSITIVE_ANY_VISIT == 1, na.rm = TRUE)/sum(INFECTION_ENROLL_DENOM == 1, na.rm=TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    # preg end

    "Syphilis prevalence at enrollment (positive RDT or diagnosed) (PREG_END)" = paste0(
      format(sum(SYPH_POSITIVE_ENROLL == 1 & PREG_END==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SYPH_POSITIVE_ENROLL == 1 & PREG_END==1, na.rm = TRUE)/sum(PREG_END==1 & SYPH_MISSING_ENROLL == 0, na.rm=TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Syphilis incidence following enrollment (positive RDT or diagnosed) (PREG_END)" = paste0(
      format(sum(SYPH_POSITIVE_ANY_VISIT == 1 & PREG_END==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SYPH_POSITIVE_ANY_VISIT == 1 & PREG_END==1, na.rm = TRUE)/sum(PREG_END==1, na.rm=TRUE)*100, 2), nsmall = 0, digits = 2),
      ")")
    
    
  )  %>%
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) 


#*****************************************************************************
#### ReMAPP AIM 3 participants (data check) ####
# how many participants with zika igm postive also are enrolled in remapp aim 3
#*****************************************************************************

aim3_ids <- mnh08 %>% filter(M08_LB_REMAPP3==1) %>% 
  distinct(SITE, PREGID) %>% 
  mutate(aim3= 1)

out_aim3 <- MAT_INFECTION %>% 
  select(SITE, MOMID, PREGID, contains("ZIK")) %>% 
  full_join(aim3_ids, by = c("SITE", "PREGID")) %>% 
  filter(ZIK_IGM_POSITIVE_ENROLL==1)

table(out_aim3$aim3, out_aim3$SITE)

### OLD CODE (KEEP FOR RECORDS):
# mat_other_infection <- mnh04_all_visits %>% 
#   select(SITE, MOMID, PREGID, TYPE_VISIT, M04_FORM_COMPLETE, M04_FORM_COMPLETE, 
#          M04_MALARIA_EVER_MHOCCUR,M04_COVID_LBORRES, contains("M04_TB_CETERM"), M04_TB_MHOCCUR) %>% 
#   # merge in mnh06 to extract rdt results 
#   full_join(mnh06_all_visits[c("SITE", "MOMID", "PREGID","M06_FORM_COMPLETE", "TYPE_VISIT", "M06_MALARIA_POC_LBORRES", "M06_HBV_POC_LBORRES",
#                                "M06_HCV_POC_LBORRES", "M06_COVID_POC_LBORRES")], 
#             by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT")) %>% 
#   
#   # merge in mnh08 to extract tb results 
#   full_join(mnh08_all_visits, 
#             by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT")) %>% 
#   distinct(SITE, MOMID, PREGID, TYPE_VISIT, .keep_all = TRUE) %>%
#   ## Is the test result missing among those with a completed form?  
#         # diagnosed
#   mutate(MAL_DIAG_RESULT = case_when(M04_MALARIA_EVER_MHOCCUR %in% c(1,0)~ 1, TRUE ~0),
#          TB_DIAG_RESULT = case_when(M04_TB_MHOCCUR %in% c(1,0)~ 1, TRUE ~0),
#          # COVID_DIAG_RESULT = case_when(M04_COVID_LBORRES %in% c(1,0)~ 1, TRUE ~0),
#          
#          # measured
#          MAL_MEAS_RESULT = case_when(M06_MALARIA_POC_LBORRES %in% c(1,0)~ 1, TRUE ~0),
#          HBV_MEAS_RESULT = case_when(M06_HBV_POC_LBORRES %in% c(1,0)~ 1, TRUE ~0),
#          HCV_MEAS_RESULT = case_when(M06_HCV_POC_LBORRES %in% c(1,0)~ 1, TRUE ~0),
#          HEV_IGM_RESULT = case_when(M08_HEV_IGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
#          HEV_IGG_RESULT = case_when(M08_HEV_IGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
# 
#          ## zcd
#          ZIK_IGM_RESULT = case_when(M08_ZCD_CHKIGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
#          ZIK_IGG_RESULT = case_when(M08_ZCD_CHKIGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
#          DEN_IGM_RESULT = case_when(M08_ZCD_DENIGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
#          DEN_IGG_RESULT = case_when(M08_ZCD_DENIGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
#          CHK_IGM_RESULT = case_when(M08_ZCD_CHKIGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
#          CHK_IGG_RESULT = case_when(M08_ZCD_CHKIGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
#          
#          # lepto
#          LEP_IGM_RESULT = case_when(M08_LEPT_IGM_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
#          LEP_IGG_RESULT = case_when(M08_LEPT_IGG_LBORRES %in% c(0,1,2) & ENROLL_EXPANSION ==1~ 1, TRUE ~0)
#          
#   ) %>% 
#   
#   # generate new var for any infection missing diagnosed and rdt
#   mutate(MAL_DIAG_MISSING = case_when(MAL_DIAG_RESULT == 0 & M04_FORM_COMPLETE==1~ 1, TRUE ~0),
#          TB_DIAG_MISSING = case_when(TB_DIAG_RESULT == 0 & M04_FORM_COMPLETE==1~ 1, TRUE ~0),
#          # COVID_DIAG_MISSING = case_when(COVID_DIAG_RESULT == 0 & M04_FORM_COMPLETE==1~ 1, TRUE ~0), 
# 
#          # measured
#          MAL_MEAS_MISSING = case_when(MAL_MEAS_RESULT == 0 & M06_FORM_COMPLETE==1~ 1, TRUE ~0),
#          HBV_MEAS_MISSING = case_when(HBV_MEAS_RESULT == 0 & M06_FORM_COMPLETE==1~ 1, TRUE ~0),
#          HCV_MEAS_MISSING = case_when(HCV_MEAS_RESULT == 0 & M06_FORM_COMPLETE==1~ 1, TRUE ~0),
#          HEV_IGM_MISSING = case_when(HEV_IGM_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
#          HEV_IGG_MISSING = case_when(HEV_IGG_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1~ 1, TRUE ~0),
#          
#          # ZCD
#          ZIK_IGM_MISSING = case_when(ZIK_IGM_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
#          ZIK_IGG_MISSING = case_when(ZIK_IGG_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
#          DEN_IGM_MISSING = case_when(DEN_IGM_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
#          DEN_IGG_MISSING = case_when(DEN_IGG_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
#          CHK_IGM_MISSING = case_when(CHK_IGM_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
#          CHK_IGG_MISSING = case_when(CHK_IGG_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
#          
#          # LEPTO
#          LEP_IGM_MISSING = case_when(LEP_IGM_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0),
#          LEP_IGG_MISSING = case_when(LEP_IGG_RESULT == 0 & M08_FORM_COMPLETE==1  & ENROLL_EXPANSION ==1 ~ 1, TRUE ~0)
#          
#         # COVID_MEAS_MISSING = case_when(COVID_MEAS_RESULT == 0 & M06_FORM_COMPLETE==1~ 1, TRUE ~0),
# 
#   ) %>% 
# 
#   # generate new var for any infection missing diagnosed and rdt
#   mutate(MAL_MISSING = case_when(MAL_DIAG_MISSING==1 | MAL_MEAS_MISSING == 1 ~ 1, TRUE ~ 0)
#                 ) %>% 
#   select(-MAL_DIAG_MISSING, -MAL_MEAS_MISSING) %>% 
#   # generate new var defining a positive result
#   mutate(
#     MAL_POSITIVE = case_when(M04_MALARIA_EVER_MHOCCUR == 1 | M06_MALARIA_POC_LBORRES == 1 ~ 1, TRUE ~ 0),
#     HBV_POSITIVE = case_when(M06_HBV_POC_LBORRES == 1 ~ 1, TRUE ~ 0),
#     HCV_POSITIVE = case_when(M06_HCV_POC_LBORRES == 1 ~ 1, TRUE ~ 0),
#     HEV_IGM_POSITIVE = case_when(M08_HEV_IGM_LBORRES == 1 ~ 1, TRUE ~ 0),
#     HEV_IGG_POSITIVE = case_when(M08_HEV_IGG_LBORRES == 1 ~ 1, TRUE ~ 0),
#     
#     # ZCD
#     ZIK_IGM_POSITIVE = case_when(M08_ZCD_ZIKIGM_LBORRES == 1 ~ 1, TRUE ~ 0),
#     ZIK_IGG_POSITIVE = case_when(M08_ZCD_ZIKIGG_LBORRES == 1 ~ 1, TRUE ~ 0),
#     DEN_IGM_POSITIVE = case_when(M08_ZCD_DENIGM_LBORRES == 1 ~ 1, TRUE ~ 0), 
#     DEN_IGG_POSITIVE = case_when(M08_ZCD_DENIGG_LBORRES == 1 ~ 1, TRUE ~ 0), 
#     CHK_IGM_POSITIVE = case_when(M08_ZCD_CHKIGM_LBORRES == 1 ~ 1, TRUE ~ 0), 
#     CHK_IGG_POSITIVE = case_when(M08_ZCD_CHKIGG_LBORRES == 1 ~ 1, TRUE ~ 0),
#     
#     # LEPTO
#     LEP_IGM_POSITIVE = case_when(M08_LEPT_IGM_LBORRES == 1 ~ 1, TRUE ~ 0), 
#     LEP_IGG_POSITIVE = case_when(M08_LEPT_IGG_LBORRES == 1 ~ 1, TRUE ~ 0)
#     
#     
#   ) %>%
#   
#   # TB at any visit: total with at least 1 symptom in W4SS in MNH04 (1=At least 1 symptom reported, 0=No symptoms)
#   mutate(W4SS_SYMPTOMS_ANY = case_when(M04_TB_CETERM_1==1 | M04_TB_CETERM_2==1 | M04_TB_CETERM_3==1| M04_TB_CETERM_4==1~ 1, TRUE ~0),
#          W4SS_RESPONSE = case_when(M04_TB_CETERM_1 %in% c(1,0) | M04_TB_CETERM_2 %in% c(1,0) |
#                                      M04_TB_CETERM_3 %in% c(1,0) | M04_TB_CETERM_4 %in% c(1,0) |
#                                      M04_TB_CETERM_77 %in% c(1,0) ~  1, TRUE ~ 0),
#          # total number missing ALL symptoms -- right now use this 
#          W4SS_MISSING_SYMP = case_when(M04_TB_CETERM_1 %in% c(55,77) & M04_TB_CETERM_2 %in% c(55,77) &
#                                          M04_TB_CETERM_3 %in% c(55,77) & M04_TB_CETERM_4 %in% c(55,77) &
#                                          M04_TB_CETERM_77 %in% c(55,77) ~ 1, TRUE ~ 0),
#          
#          TB_SYMP_POSITIVE = case_when(W4SS_SYMPTOMS_ANY == 1 ~ 1, TRUE ~ 0),
#          TB_SPUTUM_POSITIVE = case_when(M08_TB_CNFRM_LBORRES == 1 ~ 1, TRUE ~ 0),
#          TB_LAB_RESULT = case_when(M08_TB_CNFRM_LBORRES %in% c(1,2,0) ~ 1, TRUE ~ 0)
#          
#          ) %>%
#   
#   ## generate summary any infection variables (diagnosed, measured, lab)
#   mutate(OTHER_INFECTION_DIAG_ANY = case_when(M04_MALARIA_EVER_MHOCCUR==1 | M04_TB_MHOCCUR==1 | M04_COVID_LBORRES==1 ~ 1, TRUE ~ 0),
#          OTHER_INFECTION_MEAS_ANY = case_when(M06_MALARIA_POC_LBORRES==1 | M06_HBV_POC_LBORRES==1 |
#                                              M06_HCV_POC_LBORRES==1 | M06_COVID_POC_LBORRES==1 |
#                                              M08_ZCD_ZIKIGM_LBORRES ==1 | M08_ZCD_ZIKIGG_LBORRES == 1|
#                                              M08_ZCD_DENIGM_LBORRES ==1 | M08_ZCD_DENIGG_LBORRES == 1| 
#                                              M08_ZCD_CHKIGM_LBORRES ==1 | M08_ZCD_CHKIGG_LBORRES == 1|
#                                              M08_LEPT_IGM_LBORRES ==1 | M08_LEPT_IGG_LBORRES ==1 |
#                                              M08_HEV_IGM_LBORRES ==1 | M08_HEV_IGG_LBORRES ==1 ~ 1, TRUE ~ 0),
#          OTHER_INFECTION_LAB_ANY = case_when(M08_TB_CNFRM_LBORRES==1 ~ 1, TRUE ~ 0)) %>% 
#   
#   # convert to wide format
#   select(SITE, MOMID, PREGID, TYPE_VISIT, ENROLL_EXPANSION, MAL_POSITIVE, HBV_POSITIVE, HCV_POSITIVE,
#          ZIK_IGM_POSITIVE, ZIK_IGG_POSITIVE, DEN_IGM_POSITIVE, DEN_IGG_POSITIVE, CHK_IGM_POSITIVE, CHK_IGG_POSITIVE,
#          ZIK_IGM_MISSING, ZIK_IGG_MISSING,DEN_IGM_MISSING, DEN_IGG_MISSING, CHK_IGM_MISSING, CHK_IGG_MISSING,
#          HEV_IGM_POSITIVE, HEV_IGG_POSITIVE, HEV_IGM_MISSING, HEV_IGG_MISSING, 
#          LEP_IGM_POSITIVE, LEP_IGG_POSITIVE, LEP_IGM_MISSING, LEP_IGG_MISSING,
#          W4SS_RESPONSE, W4SS_MISSING_SYMP,
#          TB_LAB_RESULT, TB_SYMP_POSITIVE, TB_SPUTUM_POSITIVE,W4SS_MISSING_SYMP, contains("MISSING"), contains("_RESULT"),
#          OTHER_INFECTION_DIAG_ANY, OTHER_INFECTION_MEAS_ANY, OTHER_INFECTION_LAB_ANY) %>%
# 
#   ## TROUBLESHOOTING
#   # filter(PREGID == "AU5c8bf252-b491-4ffd-9467-43c7f00828851") %>% 
#   pivot_wider(
#     names_from = TYPE_VISIT,
#     values_from = c(MAL_POSITIVE, HBV_POSITIVE, HCV_POSITIVE, ENROLL_EXPANSION,
#                     ZIK_IGM_POSITIVE, ZIK_IGG_POSITIVE, DEN_IGM_POSITIVE, DEN_IGG_POSITIVE, CHK_IGM_POSITIVE, CHK_IGG_POSITIVE, 
#                     ZIK_IGM_MISSING, ZIK_IGG_MISSING,DEN_IGM_MISSING, DEN_IGG_MISSING, CHK_IGM_MISSING, CHK_IGG_MISSING,
#                     MAL_DIAG_RESULT, TB_DIAG_RESULT, MAL_MEAS_RESULT, HBV_MEAS_RESULT, HCV_MEAS_RESULT,
#                     TB_DIAG_MISSING,MAL_MISSING,HBV_MEAS_MISSING,HCV_MEAS_MISSING,
#                     HEV_IGM_POSITIVE, HEV_IGG_POSITIVE, HEV_IGM_MISSING, HEV_IGG_MISSING, 
#                     LEP_IGM_POSITIVE, LEP_IGG_POSITIVE, LEP_IGM_MISSING, LEP_IGG_MISSING,
#                     TB_LAB_RESULT, TB_SYMP_POSITIVE, TB_SPUTUM_POSITIVE, W4SS_MISSING_SYMP, W4SS_RESPONSE,
#                     OTHER_INFECTION_DIAG_ANY, OTHER_INFECTION_MEAS_ANY, OTHER_INFECTION_LAB_ANY),
#     names_glue = "{.value}_{TYPE_VISIT}"
#   ) %>%
#   
#   # generate new var for any syphilis positive result at any visit (EXCLUDING enrollment)
#   mutate(MAL_POSITIVE_ANY_VISIT = case_when(MAL_POSITIVE_1 != 1 & (MAL_POSITIVE_2 ==1 | MAL_POSITIVE_3 ==1 | MAL_POSITIVE_4 ==1 | MAL_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          HBV_POSITIVE_ANY_VISIT = case_when(HBV_POSITIVE_1 != 1 & (HBV_POSITIVE_2 ==1 | HBV_POSITIVE_3 ==1 | HBV_POSITIVE_4 ==1 | HBV_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          HCV_POSITIVE_ANY_VISIT = case_when(HCV_POSITIVE_1 != 1 & (HCV_POSITIVE_2 ==1 | HCV_POSITIVE_3 ==1 | HCV_POSITIVE_4 ==1 | HCV_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          TB_SYMP_POSITIVE_ANY_VISIT = case_when(TB_SYMP_POSITIVE_1 != 1 & (TB_SYMP_POSITIVE_2 ==1 | TB_SYMP_POSITIVE_3 ==1 | TB_SYMP_POSITIVE_4 ==1 | TB_SYMP_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          TB_SPUTUM_POSITIVE_ANY_VISIT = case_when(TB_SPUTUM_POSITIVE_1 != 1 & (TB_SPUTUM_POSITIVE_2 ==1 | TB_SPUTUM_POSITIVE_3 ==1 | TB_SPUTUM_POSITIVE_4 ==1 | TB_SPUTUM_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          HEV_IGM_POSITIVE_ANY_VISIT = case_when(HEV_IGM_POSITIVE_1 != 1 & (HEV_IGM_POSITIVE_2 ==1 | HEV_IGM_POSITIVE_3 ==1 | HEV_IGM_POSITIVE_4 ==1 | HEV_IGM_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          HEV_IGG_POSITIVE_ANY_VISIT = case_when(HEV_IGG_POSITIVE_1 != 1 & (HEV_IGG_POSITIVE_2 ==1 | HEV_IGG_POSITIVE_3 ==1 | HEV_IGG_POSITIVE_4 ==1 | HEV_IGG_POSITIVE_5 ==1)~ 1, TRUE ~ 0),
#          
#          # ZCD
#          ZIK_IGM_POSITIVE_ANY_VISIT = case_when(ZIK_IGM_POSITIVE_1 != 1 & (ZIK_IGM_POSITIVE_2 ==1 | ZIK_IGM_POSITIVE_3 ==1 | ZIK_IGM_POSITIVE_4 ==1 | ZIK_IGM_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          ZIK_IGG_POSITIVE_ANY_VISIT = case_when(ZIK_IGG_POSITIVE_1 != 1 & (ZIK_IGG_POSITIVE_2 ==1 | ZIK_IGG_POSITIVE_3 ==1 | ZIK_IGG_POSITIVE_4 ==1 | ZIK_IGG_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          DEN_IGM_POSITIVE_ANY_VISIT = case_when(DEN_IGM_POSITIVE_1 != 1 & (DEN_IGM_POSITIVE_2 ==1 | DEN_IGM_POSITIVE_3 ==1 | DEN_IGM_POSITIVE_4 ==1 | DEN_IGM_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          DEN_IGG_POSITIVE_ANY_VISIT = case_when(DEN_IGG_POSITIVE_1 != 1 & (DEN_IGG_POSITIVE_2 ==1 | DEN_IGG_POSITIVE_3 ==1 | DEN_IGG_POSITIVE_4 ==1 | DEN_IGG_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          CHK_IGM_POSITIVE_ANY_VISIT = case_when(CHK_IGM_POSITIVE_1 != 1 & (CHK_IGM_POSITIVE_2 ==1 | CHK_IGM_POSITIVE_3 ==1 | CHK_IGM_POSITIVE_4 ==1 | CHK_IGM_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          CHK_IGG_POSITIVE_ANY_VISIT = case_when(CHK_IGG_POSITIVE_1 != 1 & (CHK_IGG_POSITIVE_2 ==1 | CHK_IGG_POSITIVE_3 ==1 | CHK_IGG_POSITIVE_4 ==1 | CHK_IGG_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          
#          # LEPTO
#          LEP_IGM_POSITIVE_ANY_VISIT = case_when(LEP_IGM_POSITIVE_1 != 1 & (LEP_IGM_POSITIVE_2 ==1 | LEP_IGM_POSITIVE_3 ==1 | LEP_IGM_POSITIVE_4 ==1 | LEP_IGM_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          LEP_IGG_POSITIVE_ANY_VISIT = case_when(LEP_IGG_POSITIVE_1 != 1 & (LEP_IGG_POSITIVE_2 ==1 | LEP_IGG_POSITIVE_3 ==1 | LEP_IGG_POSITIVE_4 ==1 | LEP_IGG_POSITIVE_5 ==1) ~ 1, TRUE ~ 0),
#          
#            )%>% 
#   # generate missing variables 
#   mutate(MAL_MISSING_ENROLL = case_when(MAL_MISSING_1==1 ~ 1, TRUE ~ 0),
#          HBV_MEAS_MISSING_ENROLL = case_when(HBV_MEAS_MISSING_1==1 ~ 1, TRUE ~ 0),
#          HCV_MEAS_MISSING_ENROLL = case_when(HCV_MEAS_MISSING_1==1 ~ 1, TRUE ~ 0),
#          W4SS_MISSING_SYMP_ENROLL = case_when(W4SS_MISSING_SYMP_1==1 ~ 1, TRUE ~ 0),
#          HEV_IGM_MISSING_ENROLL = case_when(HEV_IGM_MISSING_1==1 ~ 1, TRUE ~ 0),
#          HEV_IGG_MISSING_ENROLL = case_when(HEV_IGG_MISSING_1==1 ~ 1, TRUE ~ 0),
#          
#          # ZCCD
#          ZIK_IGM_MISSING_ENROLL = case_when(ZIK_IGM_MISSING_1==1 ~ 1, TRUE ~ 0),
#          ZIK_IGG_MISSING_ENROLL = case_when(ZIK_IGG_MISSING_1==1 ~ 1, TRUE ~ 0),
#          DEN_IGM_MISSING_ENROLL = case_when(DEN_IGM_MISSING_1==1 ~ 1, TRUE ~ 0),
#          DEN_IGG_MISSING_ENROLL = case_when(DEN_IGG_MISSING_1==1 ~ 1, TRUE ~ 0),
#          CHK_IGM_MISSING_ENROLL = case_when(CHK_IGM_MISSING_1==1 ~ 1, TRUE ~ 0),
#          CHK_IGG_MISSING_ENROLL = case_when(CHK_IGG_MISSING_1==1 ~ 1, TRUE ~ 0),
#          
#          # LEPTO
#          LEP_IGM_MISSING_ENROLL = case_when(LEP_IGM_MISSING_1==1 ~ 1, TRUE ~ 0),
#          LEP_IGG_MISSING_ENROLL = case_when(LEP_IGG_MISSING_1==1 ~ 1, TRUE ~ 0),
#          
#          ) %>% 
# 
#   # generate enrollment prevalence variables (exclude missing)
#   mutate(MAL_POSITIVE_ENROLL = case_when(MAL_POSITIVE_1 == 1 & MAL_MISSING_ENROLL==0 ~ 1, TRUE ~ 0 ),
#          HBV_POSITIVE_ENROLL = case_when(HBV_POSITIVE_1 == 1 & HBV_MEAS_MISSING_ENROLL==0 ~ 1, TRUE ~ 0 ),
#          HCV_POSITIVE_ENROLL = case_when(HCV_POSITIVE_1 == 1 & HCV_MEAS_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
#          TB_SYMP_POSITIVE_ENROLL = case_when(TB_SYMP_POSITIVE_1 == 1 ~ 1, TRUE ~ 0),
#          TB_SPUTUM_POSITIVE_ENROLL = case_when(TB_SPUTUM_POSITIVE_1 == 1 ~ 1, TRUE ~ 0), 
#          W4SS_RESPONSE_ENROLL = case_when(W4SS_RESPONSE_1 == 1 ~ 1, TRUE ~ 0), 
#          TB_LAB_RESULT_ENROLL = case_when(TB_LAB_RESULT_1 == 1 ~ 1, TRUE ~ 0), 
#          HEV_IGM_POSITIVE_ENROLL = case_when(HEV_IGM_POSITIVE_1 == 1 & HEV_IGM_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
#          HEV_IGG_POSITIVE_ENROLL = case_when(HEV_IGG_POSITIVE_1 == 1 & HEV_IGG_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
#          
#          #ZCD
#          ZIK_IGM_POSITIVE_ENROLL = case_when(ZIK_IGM_POSITIVE_1 == 1 & ZIK_IGM_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
#          ZIK_IGG_POSITIVE_ENROLL = case_when(ZIK_IGG_POSITIVE_1 == 1 & ZIK_IGG_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
#          DEN_IGM_POSITIVE_ENROLL = case_when(DEN_IGM_POSITIVE_1 == 1 & DEN_IGM_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
#          DEN_IGG_POSITIVE_ENROLL = case_when(DEN_IGG_POSITIVE_1 == 1 & DEN_IGG_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
#          CHK_IGM_POSITIVE_ENROLL = case_when(CHK_IGM_POSITIVE_1 == 1 & CHK_IGM_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
#          CHK_IGG_POSITIVE_ENROLL = case_when(CHK_IGG_POSITIVE_1 == 1 & CHK_IGG_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
#          
#          #LEPTO
#          LEP_IGM_POSITIVE_ENROLL = case_when(LEP_IGM_POSITIVE_1 == 1 & LEP_IGM_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0),
#          LEP_IGG_POSITIVE_ENROLL = case_when(LEP_IGG_POSITIVE_1 == 1 & LEP_IGG_MISSING_ENROLL == 0 ~ 1, TRUE ~ 0)
#          
#          
#   ) %>% 
#   # generate summary variables at enrollment 
#   mutate(OTHER_INFECTION_DIAG_ANY_ENROLL = case_when(OTHER_INFECTION_DIAG_ANY_1 ==1 ~ 1, TRUE ~ 0),
#          OTHER_INFECTION_MEAS_ANY_ENROLL = case_when(OTHER_INFECTION_MEAS_ANY_1 ==1 ~ 1, TRUE ~ 0),
#          OTHER_INFECTION_LAB_ANY_ENROLL = case_when(OTHER_INFECTION_LAB_ANY_1 ==1 ~ 1, TRUE ~ 0),
#  ) %>% 
#   select(SITE, MOMID, PREGID, ENROLL_EXPANSION_1, ends_with("ENROLL"), contains("_ANY_VISIT"), contains("DENOM")
#   )

# OTHER_INFECTION_MEAS_EXPANSION_ANY

# test_wide <- mat_other_infection %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
# test_long <- mat_other_infection %>% group_by(SITE, MOMID, PREGID, TYPE_VISIT) %>% mutate(n=n()) 
# table(test_wide$n)
# 
# test2 <- test %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)


