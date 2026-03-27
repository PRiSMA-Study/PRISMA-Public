#*****************************************************************************
#### MONITORING REPORT SETUP ####
#* Function: Generate a dataset with 1 row for each enrolled particpant with key enrollment indicators (EDD, EST_CONCEP_DATE, ANC VISIT WINDOWS, etc.)
#* Input: stacked data .csv
#* Last updated: 31 March 2025
#* # added in remapp ids
#*****************************************************************************

### enrollment dataset generation ###
library(tidyverse)
library(readr)
library(dplyr)
library(data.table)
library(stringr)
library(lubridate)
library(naniar)
library(readxl)
library(haven)
#*****************************************************************************
#* IMPORT STACKED DATA 
#*****************************************************************************

## import stacked data 
UploadDate = "2025-06-13" 

# setwd(paste0("Z:/Stacked Data/", UploadDate))
setwd(paste0("~/import/", UploadDate))

# make subfolder in outcomes folder with upload date
maindir <- paste0("Z:/Outcome Data", sep = "")
subdir = UploadDate
dir.create(file.path(maindir, subdir), showWarnings = FALSE)

path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")

## import raw .CSVs in wide format 
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

#  ## make sure all column names are uppercase 
myfiles <- lapply(myfiles, function (x){
  upper <- toupper(names(x))
  setnames(x, upper)
})

## convert to individual dataframes 
names(myfiles) <- gsub("_merged.csv", paste0(""), temp)
list2env(myfiles, globalenv())
#*****************************************************************************
#* REMOVE DUPLICATE SCREENING INSTANCES FROM MNH02 & 01 
#*****************************************************************************

mnh01_filtered <- mnh01 %>% 
  # only want enrollment visit & remove any blank SCRNIDs 
  filter(M01_TYPE_VISIT ==1 & !is.na(SCRNID) & SCRNID != "") %>% 
  ## if the visit date is a default value, replace with NA
  mutate(M01_US_OHOSTDAT = replace(M01_US_OHOSTDAT, M01_US_OHOSTDAT == ymd("1907-07-07"), NA)) %>% 
  ## remove any duplicate screening visits -- if a duplicate, take the first visit 
  group_by(SCRNID) %>% 
  arrange(desc(M01_US_OHOSTDAT)) %>% 
  slice(1) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) 


mnh02_filtered <- mnh02 %>% 
  # only want enrollment visit & remove any blank SCRNIDs 
  filter(!is.na(SCRNID) & SCRNID != "") %>% 
  ## if the visit date is a default value, replace with NA
  mutate(M02_SCRN_OBSSTDAT = replace(M02_SCRN_OBSSTDAT, M02_SCRN_OBSSTDAT == ymd("1907-07-07"), NA)) %>% 
  ## remove any duplicate screening visits -- if a duplicate, take the first visit 
  group_by(SCRNID) %>% 
  arrange(desc(M02_SCRN_OBSSTDAT)) %>% 
  slice(1) %>%
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) 

## read in remapp lists 
remapp_ids <- read_dta(paste0("Z:/ReMAPP_Aim3_IDs/Finalized Lists/Allsites_Mar_2025.dta")) %>% 
  mutate(REMAPP_AIM3_ENROLL = 1) %>%
  rename(REMAPP_AIM3_TRI = LB_REMAPP3_TRI) %>% 
  select(SITE, MOMID, PREGID, REMAPP_AIM3_ENROLL, REMAPP_AIM3_TRI)  

## add SAS momids in remapp lists 
remapp_ids_sas <- remapp_ids %>% 
  filter(SITE == "India-SAS") %>% 
  select(-MOMID) %>% 
  left_join(mnh02_filtered %>% select(SITE, MOMID, PREGID), by = c("SITE", "PREGID"))

## add Zambia momids in remapp lists 
remapp_ids_zam <- remapp_ids %>% 
  filter(SITE == "Zambia") %>% 
  select(-MOMID) %>% 
  left_join(mnh02_filtered %>% select(SITE, MOMID, PREGID), by = c("SITE", "PREGID"))

remapp_ids_all_minus <- remapp_ids %>% 
  filter(!SITE %in% c("India-SAS", "Zambia"))

remapp_ids_final <- bind_rows(remapp_ids_sas,remapp_ids_zam, remapp_ids_all_minus)

table(remapp_ids$SITE)
table(remapp_ids_final$SITE)
#*****************************************************************************
#* ADD IDs in MNH01
#*****************************************************************************
## For sites that are not reporting MOMID/PREGID in MNH01 for enrollment visits, we will merge these IDs from MNH02 into MNH01
## Ghana ids
mnh02_gha_ids <- mnh02 %>% filter(SITE == "Ghana") %>% select(SCRNID, MOMID, PREGID) ## export mnh02 ids
mnh01_gha <- mnh01_filtered %>% filter(SITE == "Ghana", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>%  # pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
  left_join(mnh02_gha_ids, by = c("SCRNID"))
mnh01_all <- mnh01_filtered %>% filter(SITE != "Ghana") # extract site-specific from merged data 
mnh01_filtered <- bind_rows(mnh01_gha, mnh01_all) # rebind data 

## CMC ids
mnh02_cmc_ids <- mnh02 %>% filter(SITE == "India-CMC") %>% select(SCRNID, MOMID, PREGID) ## export mnh02 ids
mnh01_cmc <- mnh01_filtered %>% filter(SITE == "India-CMC", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>% # pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
  left_join(mnh02_cmc_ids, by = c("SCRNID"))
mnh01_all <- mnh01_filtered %>% filter(SITE != "India-CMC") # extract site-specific from merged data 
mnh01_filtered <- bind_rows(mnh01_cmc, mnh01_all) # rebind data 

mnh01_filtered$M01_US_OHOSTDAT <- as.Date(mnh01_filtered$M01_US_OHOSTDAT, format = "%Y-%m-%d")
mnh01_filtered <-mnh01_filtered %>% filter(M01_US_GA_DAYS_AGE_FTS1 != 'hence G.A more than 25weeks"') %>% 
  mutate(M01_US_GA_DAYS_AGE_FTS1 = as.numeric(M01_US_GA_DAYS_AGE_FTS1))

## SAS ids
mnh02_sas_ids <- mnh02 %>% filter(SITE == "India-SAS") %>% select(SCRNID, MOMID, PREGID) ## export mnh02 ids
mnh01_sas <- mnh01_filtered %>% filter(SITE == "India-SAS", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>% # pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
  left_join(mnh02_sas_ids, by = c("SCRNID"))
mnh01_all <- mnh01_filtered %>% filter(SITE != "India-SAS") # extract site-specific from merged data 
mnh01_filtered <- bind_rows(mnh01_sas, mnh01_all) # rebind data 

mnh01_filtered$M01_US_OHOSTDAT <- as.Date(mnh01_filtered$M01_US_OHOSTDAT, format = "%Y-%m-%d")
mnh01_filtered <-mnh01_filtered %>% filter(M01_US_GA_DAYS_AGE_FTS1 != 'hence G.A more than 25weeks"') %>% 
  mutate(M01_US_GA_DAYS_AGE_FTS1 = as.numeric(M01_US_GA_DAYS_AGE_FTS1))

## kenya ids
mnh02_ke_ids <- mnh02_filtered %>% filter(SITE == "Kenya") %>% select(SCRNID, MOMID, PREGID) ## export mnh02 ids
mnh01_ke <- mnh01_filtered %>% 
  filter(SITE == "Kenya", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>% 
  # pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
  left_join(mnh02_ke_ids, by = c("SCRNID"))

mnh01_all <- mnh01_filtered %>% filter(SITE != "Kenya") # extract site-specific from merged data 
mnh01_filtered <- bind_rows(mnh01_ke, mnh01_all) # rebind data 


mnh01_ke <- mnh01_filtered %>% filter(SITE == "Kenya", M01_TYPE_VISIT == 1)  %>% 
  group_by(SCRNID) %>% 
  mutate(n=n()) %>% select(SITE, SCRNID, MOMID, PREGID, n, M01_US_OHOSTDAT)

## Pakistan ids
mnh02_pak_ids <- mnh02 %>% filter(SITE == "Pakistan") %>% select(SCRNID, MOMID, PREGID) ## export mnh02 ids
mnh01_pak <- mnh01_filtered %>% filter(SITE == "Pakistan", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>%  # pull site-specific data & merge mnh01_filtered and mnh02 by scrnid to get momid/pregid in mnh01
  left_join(mnh02_pak_ids, by = c("SCRNID"))
mnh01_all <- mnh01_filtered %>% filter(SITE != "Pakistan") # extract site-specific from merged data 
mnh01_filtered <- bind_rows(mnh01_pak, mnh01_all) # rebind data 

## zambia ids
mnh02_zam_ids <- mnh02 %>% filter(SITE == "Zambia") %>% select(SCRNID, MOMID, PREGID) ## export mnh02 ids
mnh01_zam <- mnh01_filtered %>% filter(SITE == "Zambia", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>%  # pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
  left_join(mnh02_zam_ids, by = c("SCRNID"))
mnh01_all <- mnh01_filtered %>% filter(SITE != "Zambia") # extract site-specific from merged data 
mnh01_filtered <- bind_rows(mnh01_zam, mnh01_all) # rebind data 

## add in all other mnh01 visit info 
mnh01_all_visits <- mnh01 %>% filter(M01_TYPE_VISIT != 1) %>% 
  mutate(M01_US_OHOSTDAT = ymd(parse_date_time(M01_US_OHOSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))))

mnh01 <- bind_rows(mnh01_filtered, mnh01_all_visits)

#*****************************************************************************
#* REMOVE DUPLICATES FROM ALL FORMS 
#*****************************************************************************
# List of data frames
form_names <- ls(pattern = "^mnh\\d+$")
forms_list <- mget(form_names)

# List of columns to check for duplicates in each form
duplicate_columns <- list(
  mnh00 = c("SITE", "SCRNID", "M00_SCRN_OBSSTDAT"),
  mnh01 = c("SITE", "SCRNID", "MOMID", "PREGID", "M01_TYPE_VISIT","M01_US_OHOSTDAT"),
  mnh02 = c("SITE", "SCRNID", "MOMID", "PREGID", "M02_SCRN_OBSSTDAT"),
  mnh03 = c("SITE", "MOMID", "PREGID", "M03_SD_OBSSTDAT"),
  mnh04 = c("SITE", "MOMID", "PREGID", "M04_TYPE_VISIT", "M04_ANC_OBSSTDAT"),
  mnh05 = c("SITE", "MOMID", "PREGID", "M05_TYPE_VISIT", "M05_ANT_PEDAT"),
  mnh06 = c("SITE", "MOMID", "PREGID", "M06_TYPE_VISIT", "M06_DIAG_VSDAT"),
  mnh07 = c("SITE", "MOMID", "PREGID", "M07_TYPE_VISIT", "M07_MAT_SPEC_COLLECT_DAT"),
  mnh08 = c("SITE", "MOMID", "PREGID", "M08_TYPE_VISIT", "M08_LBSTDAT"),
  mnh09 = c("SITE", "MOMID", "PREGID", "M09_MAT_LD_OHOSTDAT"),
  mnh10 = c("SITE", "MOMID", "PREGID", "M10_VISIT_OBSSTDAT"),
  mnh11 = c("SITE", "MOMID", "PREGID","INFANTID",  "M11_VISIT_OBSSTDAT"),
  mnh12 = c("SITE", "MOMID", "PREGID", "M12_TYPE_VISIT", "M12_VISIT_OBSSTDAT"),
  mnh13 = c("SITE", "MOMID", "PREGID","INFANTID",  "M13_TYPE_VISIT", "M13_VISIT_OBSSTDAT"),
  mnh14 = c("SITE", "MOMID", "PREGID","INFANTID",  "M14_TYPE_VISIT", "M14_VISIT_OBSSTDAT"),
  mnh15 = c("SITE", "MOMID", "PREGID","INFANTID",  "M15_TYPE_VISIT", "M15_OBSSTDAT"),
  mnh16 = c("SITE", "MOMID", "PREGID", "M16_VISDAT"),
  mnh17 = c("SITE", "MOMID", "PREGID", "M17_VISDAT"),
  mnh18 = c("SITE", "MOMID", "PREGID", "M18_VISDAT"),
  mnh19 = c("SITE", "MOMID", "PREGID", "M19_OBSSTDAT"),
  mnh20 = c("SITE", "MOMID", "PREGID","INFANTID",  "M20_ADMIT_OHOSTDAT"),
  mnh21 = c("SITE", "MOMID", "PREGID", "M21_AESTDAT"),
  mnh22 = c("SITE", "MOMID", "PREGID", "M22_DVSTDAT"),
  mnh23 = c("SITE", "MOMID", "PREGID", "M23_CLOSE_DSSTDAT"),
  mnh24 = c("SITE", "MOMID", "PREGID","INFANTID",  "M24_CLOSE_DSSTDAT"),
  mnh25 = c("SITE", "MOMID", "PREGID", "M25_TYPE_VISIT", "M25_OBSSTDAT"),
  mnh26 = c("SITE", "MOMID", "PREGID", "M26_TYPE_VISIT", "M26_FTGE_OBSTDAT")
  # mnh27 = c("SITE", "MOMID", "PREGID", "M26_FTGE_OBSTDAT")
  # add other columns for other forms here
)

# Initialize an empty list to store duplicates
duplicates_list <- list()

# Loop through each form
for (form_name in names(forms_list)) {
  form_data <- forms_list[[form_name]]
  cols_to_check <- duplicate_columns[[form_name]]
  # Extract date column (last one in cols_to_check)
  date_col <- tail(cols_to_check, 1)
  
  # Check for duplicates
  if (any(duplicated(form_data[cols_to_check]))) {
    # Extract duplicated ids
    duplicates_ids <- which(duplicated(form_data[cols_to_check]) | 
                              duplicated(form_data[cols_to_check], fromLast = TRUE))
    duplicates_data <- form_data[duplicates_ids, ]
    
    # Store duplicates in the list
    duplicates_list[[paste0("duplicates_", form_name)]] <- duplicates_data
    
    # Remove duplicates from the original data frame
    forms_list[[form_name]] <- form_data %>%
      group_by(across(all_of(cols_to_check))) %>%
      # if a duplicate exists, take the first instance (sorting by date)
      arrange(-desc(date_col)) %>% 
      slice(1) %>% 
      mutate(n=n()) %>% 
      ungroup() %>% 
      select(-n) %>% 
      ungroup()
    
    print(paste0("n= ", dim(duplicates_data)[1], " Duplicates in ", form_name, " exist"))
  } else {
    print(paste0("No duplicates in ", form_name))
  }
}

# Result: cleaned forms_list and duplicates_list


# Output the duplicates list
list2env(forms_list, envir = .GlobalEnv)

#*****************************************************************************
#* EXTRACT VARIABLES FROM EACH FORM 
#*****************************************************************************
# MNH02: enrollment vars 
# MNH01: BOE dates/ga 
# ANC visit windows 
mnh02_sub <- mnh02 %>% 
  mutate(ENROLL = case_when(M02_AGE_IEORRES == 1 & 
                              M02_PC_IEORRES == 1 & 
                              M02_CATCHMENT_IEORRES == 1 &
                              M02_CATCH_REMAIN_IEORRES == 1  &
                              M02_CONSENT_IEORRES == 1 ~ 1, 
                            TRUE ~ 0
  )) %>% 
  ## in the event there is a duplicate screening id, but they came back and were enrolled, keep the enrolle
  # and drop the non-enrolled particpant 
  group_by(SITE, SCRNID) %>% 
  mutate(KEEP_ENROLLED = case_when(ENROLL ==1 ~ 1, TRUE ~ 0)) %>% 
  filter(KEEP_ENROLLED==1) %>% 
  ungroup() %>% 
  select(SITE, SCRNID, MOMID, PREGID, ENROLL, M02_SCRN_OBSSTDAT)

## Remove momid and pregid from MNH00 to merge -- we are going to only have the momid and pregid as defined in m02
mnh01_boe <- mnh01_filtered %>% filter(M01_TYPE_VISIT == 1) %>% ## only want enrollment visit 
  rename("TYPE_VISIT" = M01_TYPE_VISIT) %>% 
  # filter out any ultrasound visit dates that are 07-07-1907
  filter(M01_US_OHOSTDAT != ymd("1907-07-07")) %>%   ## FOR KENYA DATA, THIS WILL BE 2007-07-07
  # calculate us ga in days with reported ga in wks + days. if ga is -7 or -5, replace with NA
  ## combine ga weeks and days variables to get a single gestational age variable
  mutate(GA_US_DAYS_FTS1 =  ifelse(!SITE %in% c("India-CMC", "India-SAS") & M01_US_GA_WKS_AGE_FTS1!= -7 & M01_US_GA_DAYS_AGE_FTS1 != -7,  (M01_US_GA_WKS_AGE_FTS1 * 7 + M01_US_GA_DAYS_AGE_FTS1), NA), 
         GA_US_DAYS_FTS2 =  ifelse(!SITE %in% c("India-CMC", "India-SAS") & M01_US_GA_WKS_AGE_FTS2!= -7 & M01_US_GA_DAYS_AGE_FTS2 != -7,  (M01_US_GA_WKS_AGE_FTS2 * 7 + M01_US_GA_DAYS_AGE_FTS2), NA),
         GA_US_DAYS_FTS3 =  ifelse(!SITE %in% c("India-CMC", "India-SAS") & M01_US_GA_WKS_AGE_FTS3!= -7 & M01_US_GA_DAYS_AGE_FTS3 != -7,  (M01_US_GA_WKS_AGE_FTS3 * 7 + M01_US_GA_DAYS_AGE_FTS3), NA),
         GA_US_DAYS_FTS4 =  ifelse(!SITE %in% c("India-CMC", "India-SAS") & M01_US_GA_WKS_AGE_FTS4!= -7 & M01_US_GA_DAYS_AGE_FTS4 != -7,  (M01_US_GA_WKS_AGE_FTS4 * 7 + M01_US_GA_DAYS_AGE_FTS4), NA)) %>% 
  ## combine ga weeks and days variables to get a single gestational age variable - CMC is using acog - use this here 
  mutate(GA_US_DAYS_FTS1 =  ifelse(SITE %in% c("India-CMC", "India-SAS") & M01_CAL_GA_WKS_AGE_FTS1!= -7 & M01_CAL_GA_DAYS_AGE_FTS1 != -7,  (M01_CAL_GA_WKS_AGE_FTS1 * 7 + M01_CAL_GA_DAYS_AGE_FTS1), GA_US_DAYS_FTS1), 
         GA_US_DAYS_FTS2 =  ifelse(SITE %in% c("India-CMC", "India-SAS") & M01_CAL_GA_WKS_AGE_FTS2!= -7 & M01_CAL_GA_DAYS_AGE_FTS2 != -7,  (M01_CAL_GA_WKS_AGE_FTS2 * 7 + M01_CAL_GA_DAYS_AGE_FTS2), GA_US_DAYS_FTS2),
         GA_US_DAYS_FTS3 =  ifelse(SITE %in% c("India-CMC", "India-SAS") & M01_CAL_GA_WKS_AGE_FTS3!= -7 & M01_CAL_GA_DAYS_AGE_FTS3 != -7,  (M01_CAL_GA_WKS_AGE_FTS3 * 7 + M01_CAL_GA_DAYS_AGE_FTS3), GA_US_DAYS_FTS3),
         GA_US_DAYS_FTS4 =  ifelse(SITE %in% c("India-CMC", "India-SAS") & M01_CAL_GA_WKS_AGE_FTS4!= -7 & M01_CAL_GA_DAYS_AGE_FTS4 != -7,  (M01_CAL_GA_WKS_AGE_FTS4 * 7 + M01_CAL_GA_DAYS_AGE_FTS4), GA_US_DAYS_FTS4)) %>% 
  #  pull the largest GA for multiple fetuses + convert to weeks
  mutate(US_GA_DAYS_ENROLL = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% ## where GA_US_DAYS_FTSx is the reported GA by ultrasound (added together M01_US_GA_WKS_AGE_FTSx and M01_US_GA_DAYS_AGE_FTSx to get a single estimate in days)
  mutate(US_GA_WKS_ENROLL = US_GA_DAYS_ENROLL %/% 7) %>% 
  #  convert ga by LMP to days and wks
  mutate(LMP_GA_DAYS_ENROLL =  ifelse(M01_GA_LMP_WEEKS_SCORRES != -7 & M01_GA_LMP_DAYS_SCORRES != -7,  (M01_GA_LMP_WEEKS_SCORRES * 7 + M01_GA_LMP_DAYS_SCORRES), NA)) %>% 
  mutate(LMP_GA_WKS_ENROLL = LMP_GA_DAYS_ENROLL %/% 7) %>%
  ## generate indicator variable for missing US 
  mutate(MISSING_BOTH_US_LMP = ifelse((US_GA_WKS_ENROLL < 0 & LMP_GA_WKS_ENROLL < 0) | 
                                        (is.na(US_GA_WKS_ENROLL) & is.na(LMP_GA_WKS_ENROLL)), 1, 0)) %>% 
  #  calculate the difference in days between reported LMP and reported US
  mutate(GA_DIFF_DAYS = LMP_GA_DAYS_ENROLL-US_GA_DAYS_ENROLL) %>%
  #  obtain best obstetric estimate in weeks
  mutate(BOE_GA_DAYS_ENROLL = case_when(LMP_GA_DAYS_ENROLL %/% 7 < 9 ~
                                          if_else(abs(GA_DIFF_DAYS) <= 5,
                                                  LMP_GA_DAYS_ENROLL,
                                                  US_GA_DAYS_ENROLL),
                                        LMP_GA_DAYS_ENROLL %/% 7 < 16 ~
                                          if_else(abs(GA_DIFF_DAYS) <=7,
                                                  LMP_GA_DAYS_ENROLL, US_GA_DAYS_ENROLL),
                                        LMP_GA_DAYS_ENROLL %/% 7 >= 16 ~
                                          if_else(abs(GA_DIFF_DAYS) <=10,
                                                  LMP_GA_DAYS_ENROLL, US_GA_DAYS_ENROLL),
                                        TRUE ~ US_GA_DAYS_ENROLL)) %>%
  mutate(BOE_GA_WKS_ENROLL = BOE_GA_DAYS_ENROLL %/% 7) %>% 
  # generate EDD based on BOE 
  # "zero out" GA and obtain the estimated "date of conception" 
  mutate(EST_CONCEP_DATE = ymd(M01_US_OHOSTDAT) - BOE_GA_DAYS_ENROLL) %>% 
  # add 280 days to EST_CONCEP_DATE to generate EDD based on BOE 
  mutate(EDD_BOE = EST_CONCEP_DATE + 280) %>% 
  ## EDD based on ultrasound 
  mutate(EDD_US =  EST_CONCEP_DATE + 280) %>% 
  ## add EST_CONEP_DATE based on ultrasound
  mutate(EST_CONCEP_DATE_US = ymd(M01_US_OHOSTDAT) - US_GA_DAYS_ENROLL) %>% 
  ## add EST_CONEP_DATE based on ultrasound
  mutate(EST_CONCEP_DATE_LMP = ymd(M01_US_OHOSTDAT) - LMP_GA_DAYS_ENROLL) %>% 
  ## rename variables for LMP and US at enrollment 
  # generate indicator variable if LMP or US was used (where 1 = US and 2 = LMP)
  mutate(BOE_METHOD = ifelse(BOE_GA_DAYS_ENROLL == US_GA_DAYS_ENROLL, 1,
                             ifelse(BOE_GA_DAYS_ENROLL == LMP_GA_DAYS_ENROLL, 2, 55))) %>%
  ## QUESTION: do we want this to be weeks + days or just days
  select(SITE, SCRNID, MOMID, PREGID,M01_US_OHOSTDAT, EST_CONCEP_DATE,, EST_CONCEP_DATE_US, EST_CONCEP_DATE_LMP, GA_DIFF_DAYS,EDD_BOE,
         BOE_METHOD, BOE_GA_WKS_ENROLL, BOE_GA_DAYS_ENROLL,US_GA_WKS_ENROLL, US_GA_DAYS_ENROLL, LMP_GA_WKS_ENROLL, LMP_GA_DAYS_ENROLL) %>% 
  mutate(ENROLL_ONTIME_WINDOW = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(139, unit="days"),
         ENROLL_LATE_WINDOW = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(139, unit="days"),
         ANC20_ONTIME_WINDOW = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(160, unit="days"),
         ANC20_LATE_WINDOW = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(181, unit="days"),
         ANC28_ONTIME_WINDOW = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(216, unit="days"),
         ANC28_LATE_WINDOW = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(216, unit="days"),
         ANC32_ONTIME_WINDOW = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(237, unit="days"),
         ANC32_LATE_WINDOW = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(237, unit="days"), 
         ANC36_ONTIME_WINDOW = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(272, unit="days"),
         ANC36_LATE_WINDOW = (EDD_BOE - as.difftime(280, unit="days")) + as.difftime(272, unit="days")) %>%
  ## CALCULATE INDICATOR VARIABLES for passed ON-TIME ANC WINDOWS - same as overdue code, but exclude the visit completion piece
  ## using upload date
  mutate(ENROLL_PASS_ONTIME = ifelse(ENROLL_ONTIME_WINDOW<UploadDate, 1, 0),
         ANC20_PASS_ONTIME = ifelse(ANC20_ONTIME_WINDOW<UploadDate, 1, 0),
         ANC28_PASS_ONTIME = ifelse(ANC28_ONTIME_WINDOW<UploadDate, 1, 0),
         ANC32_PASS_ONTIME = ifelse(ANC32_ONTIME_WINDOW<UploadDate, 1, 0),
         ANC36_PASS_ONTIME = ifelse(ANC36_ONTIME_WINDOW<UploadDate, 1, 0)) %>%
  ## CALCULATE INDICATOR VARIABLES for missed PASSED LATE WINDOWS
  mutate(ENROLL_PASS_LATE = ifelse(ENROLL_LATE_WINDOW<UploadDate, 1, 0),
         ANC20_PASS_LATE = ifelse(ANC20_LATE_WINDOW<UploadDate & BOE_GA_WKS_ENROLL <= 17, 1, 0),
         ANC28_PASS_LATE = ifelse(ANC28_LATE_WINDOW<UploadDate, 1, 0),
         ANC32_PASS_LATE = ifelse(ANC32_LATE_WINDOW<UploadDate, 1, 0),
         ANC36_PASS_LATE = ifelse(ANC36_LATE_WINDOW<UploadDate, 1, 0))  

MAT_ENROLL_FULL <- mnh02_sub %>% 
  left_join(mnh01_boe, by = c("SITE", "SCRNID", "MOMID", "PREGID")) %>% 
  mutate(DROPPED = case_when(is.na(EDD_BOE) & ENROLL==1 ~ 1, ## drop participants who are enrolled but missing mnh01 
                             ENROLL==1 & M02_SCRN_OBSSTDAT > UploadDate ~ 1, ## drop participants who have an enrollment date greater than the upload date
                             TRUE ~ 0)) %>%  
  ## indicator variable for screening date after upload date
  mutate(INVALID_SCRN_DATE = case_when(ENROLL==1 & M02_SCRN_OBSSTDAT > UploadDate ~ 1, TRUE ~ 0)) %>% 
  ## RENAME VARIABLES 
  rename(ENROLL_SCRN_DATE = M02_SCRN_OBSSTDAT) %>% 
  rename(PREG_START_DATE = EST_CONCEP_DATE) %>% 
  ## recode missing lmp or us to -5
  mutate(US_GA_DAYS_ENROLL = case_when(is.na(US_GA_DAYS_ENROLL) ~ -5, TRUE ~ US_GA_DAYS_ENROLL),
         US_GA_WKS_ENROLL = case_when(is.na(US_GA_WKS_ENROLL) ~ -5, TRUE ~ US_GA_WKS_ENROLL),  
         LMP_GA_DAYS_ENROLL = case_when(is.na(LMP_GA_DAYS_ENROLL) ~ -5, TRUE ~ LMP_GA_DAYS_ENROLL),
         LMP_GA_WKS_ENROLL = case_when(is.na(LMP_GA_WKS_ENROLL) ~ -5, TRUE ~ LMP_GA_WKS_ENROLL))

## add in remapp ids 
MAT_ENROLL_FULL <- MAT_ENROLL_FULL %>%
  left_join(remapp_ids_final, by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(REMAPP_AIM3_ENROLL = case_when(is.na(REMAPP_AIM3_ENROLL) ~ 0, 
                                        TRUE ~ REMAPP_AIM3_ENROLL),
         REMAPP_AIM3_TRI = case_when(REMAPP_AIM3_ENROLL == 1 & is.na(REMAPP_AIM3_TRI)  ~ 55, 
                                     REMAPP_AIM3_ENROLL == 0 & is.na(REMAPP_AIM3_TRI) ~ 77,
                                     TRUE ~ REMAPP_AIM3_TRI)
  ) %>% 
  ## add remapp start dates 
  mutate(REMAPP_ENROLL = case_when(
    ((SITE == "Ghana" & ENROLL_SCRN_DATE >= "2022-12-28" & ENROLL_SCRN_DATE <= "2024-10-29") | ## end date confirmed added 
       (SITE == "Kenya" & ENROLL_SCRN_DATE >= "2023-04-03") | 
       (SITE == "Zambia" & ENROLL_SCRN_DATE >= "2022-12-15") |
       (SITE == "Pakistan" & ENROLL_SCRN_DATE >= "2022-09-22" & ENROLL_SCRN_DATE <= "2024-04-05") | ## end date confirmed added 
       (SITE == "India-CMC" & ENROLL_SCRN_DATE >= "2023-06-20") |
       (SITE == "India-SAS" & ENROLL_SCRN_DATE >= "2023-12-12")) ~ 1,
    TRUE ~ 0
  )) %>% 
  relocate(REMAPP_ENROLL, REMAPP_AIM3_ENROLL, REMAPP_AIM3_TRI, .after = ENROLL)

## SEND THESE TO SITES FOR REVIEW!!
invalid_scrn_dates <- MAT_ENROLL_FULL %>% filter(INVALID_SCRN_DATE==1 ) %>% select(SITE, SCRNID, MOMID, PREGID, ENROLL, ENROLL_SCRN_DATE)

MAT_ENROLL <- MAT_ENROLL_FULL %>% 
  filter(DROPPED != 1) %>%  
  select(-DROPPED) 

## data checks for any duplicates, blanks, etc. 
test <- MAT_ENROLL %>% group_by(SCRNID) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
test <- MAT_ENROLL %>% group_by(PREGID) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
test <- MAT_ENROLL %>% group_by(MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
test <- MAT_ENROLL %>% filter(SCRNID == " ")
dim(test)
test <- MAT_ENROLL %>% filter(MOMID == " ")
dim(test)
test <- MAT_ENROLL %>% filter(PREGID == " ")
dim(test)

# save data set; this will get called into the report
write.csv(MAT_ENROLL, paste0(path_to_tnt, "MAT_ENROLL" ,".csv"), na="", row.names=FALSE)

## change date class for preg_start_date so the export doesn't get messed up 
MAT_ENROLL$PREG_START_DATE <- as.character(MAT_ENROLL$PREG_START_DATE)
MAT_ENROLL$M01_US_OHOSTDAT <- as.character(MAT_ENROLL$M01_US_OHOSTDAT)
MAT_ENROLL$ENROLL_SCRN_DATE <- as.character(MAT_ENROLL$ENROLL_SCRN_DATE)
MAT_ENROLL$EDD_BOE <- as.character(MAT_ENROLL$EDD_BOE)
MAT_ENROLL$ENROLL_ONTIME_WINDOW <- as.character(MAT_ENROLL$ENROLL_ONTIME_WINDOW)
MAT_ENROLL$ANC20_ONTIME_WINDOW <- as.character(MAT_ENROLL$ANC20_ONTIME_WINDOW)
MAT_ENROLL$ANC28_ONTIME_WINDOW <- as.character(MAT_ENROLL$ANC28_ONTIME_WINDOW)
MAT_ENROLL$ANC32_ONTIME_WINDOW <- as.character(MAT_ENROLL$ANC32_ONTIME_WINDOW)
MAT_ENROLL$ANC36_ONTIME_WINDOW <- as.character(MAT_ENROLL$ANC36_ONTIME_WINDOW)

MAT_ENROLL$ENROLL_LATE_WINDOW <- as.character(MAT_ENROLL$ENROLL_LATE_WINDOW)
MAT_ENROLL$ANC20_LATE_WINDOW <- as.character(MAT_ENROLL$ANC20_LATE_WINDOW)
MAT_ENROLL$ANC28_LATE_WINDOW <- as.character(MAT_ENROLL$ANC28_LATE_WINDOW)
MAT_ENROLL$ANC32_LATE_WINDOW <- as.character(MAT_ENROLL$ANC32_LATE_WINDOW)
MAT_ENROLL$ANC36_LATE_WINDOW <- as.character(MAT_ENROLL$ANC36_LATE_WINDOW)

write.xlsx(MAT_ENROLL, paste0(path_to_tnt, "MAT_ENROLL" ,".xlsx"), na="", rowNames=FALSE)

