#*****************************************************************************
#* PRISMA Infant Outcomes
#* Drafted: 21 September 2023, Stacie Loisate
#* Last updated: 23 August 2024

# If you copy and paste the following, it will take you to that section: 
# 1. Low birth-weight 
# 2. Pre-term birth 
# 3. Size for Gestational Age (SGA) 
# 4. Neonatal Mortality 
# 5. Infant mortality 
# 6. Stillbirth 
# 7. Fetal death 
# 8. Birth asphyxia 
# 9. Hyperbili & Jaundice
# 10. psbi

#The first section, CONSTRUCTED VARIABLES GENERATION, below, the code generates datasets for 
#each form with additional variables that will be used for multiple outcomes. For example, mnh01_constructed 
#is a dataset that will be used for several outcomes. 

# The `growthstandards` R package allows the user to pull INTERGROWTH centiles for 
# newborn length, weight, and head circumference. 
# R Package linked: (https://ki-tools.github.io/growthstandards/articles/usage.html#intergrowth-newborn-standard-1).

# the package can be downloaded using the following code: 
# install.packages("remotes") # if "remotes" is not already installed 
# remotes::install_github("ki-tools/growthstandards") 
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
library(growthstandards) ## INTERGROWTH PACKAGE
library(TCB) ## TCB package 
library(openxlsx)

# UPDATE EACH RUN # 
# set upload date 
UploadDate = "2024-06-28"

# set path to data
path_to_data = paste0("~/Monitoring Report/data/merged/" ,UploadDate)

mnh15 <- load(paste0(path_to_data,"/", "m15_merged.RData"))
mnh15 <- read_csv("Z:/Stacked Data/2024-06-28/mnh15_merged.csv")

table(mnh15$M15_PENT_RCL_CMOCCUR, mnh15$SITE)
table(mnh15$M15_OPV_RCL_CMOCCUR, mnh15$SITE)

# set path to save 
path_to_save <- "D:/Users/stacie.loisate/Box/PRISMA-Analysis/Infant-Constructed-Variables/data/"
path_to_save_figures <- "D:/Users/stacie.loisate/Box/PRISMA-Analysis/output/"

path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")

# # import forms 
mnh01 <- load(paste0(path_to_data,"/", "m01_merged.RData"))
mnh01 <- m01_merged

mnh02 <- load(paste0(path_to_data,"/", "m02_merged.RData"))
mnh02 <- m02_merged

mnh04 <- load(paste0(path_to_data,"/", "m04_merged.RData"))
mnh04 <- m04_merged

mnh08 <- load(paste0(path_to_data,"/", "m08_merged.RData"))
mnh08 <- m08_merged

mnh09 <- load(paste0(path_to_data,"/", "m09_merged.RData"))
mnh09 <- m09_merged

mnh11 <- load(paste0(path_to_data,"/", "m11_merged.RData"))
mnh11 <- m11_merged

mnh13 <- load(paste0(path_to_data,"/", "m13_infantmerged.RData"))
mnh13 <- m13_infantmerged

mnh14 <- load(paste0(path_to_data,"/", "m14_infantmerged.RData"))
mnh14 <- m14_infantmerged

mnh15 <- load(paste0(path_to_data,"/", "m15_infantmerged.RData"))
mnh15 <- m15_infantmerged

mnh19 <- load(paste0(path_to_data,"/", "m19_merged.RData"))
mnh19 <- m19_merged


mnh20 <- load(paste0(path_to_data,"/", "m20_infantmerged.RData"))
mnh20 <- m20_infantmerged

mnh24 <- load(paste0(path_to_data,"/", "m24_infantmerged.RData"))
mnh24 <- m24_infantmerged

mat_enroll <- read.csv(paste0(path_to_tnt, "/MAT_ENROLL.csv"))

## For sites that are not reporting MOMID/PREGID in MNH01 for enrollment visits, we will merge these IDs from MNH02 into MNH01
## zambia ids
mnh02_zam_ids <- mnh02 %>% filter(SITE == "Zambia") %>% select(SCRNID, MOMID, PREGID) ## export mnh02 ids
mnh01_zam <- mnh01 %>% filter(SITE == "Zambia", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>%  # pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
  left_join(mnh02_zam_ids, by = c("SCRNID"))
mnh01_all <- mnh01 %>% filter(SITE != "Zambia") # extract site-specific from merged data 
mnh01 <- bind_rows(mnh01_zam, mnh01_all) # rebind data 

## kenya ids
mnh02_ke_ids <- mnh02 %>% filter(SITE == "Kenya") %>% select(SCRNID, MOMID, PREGID) ## export mnh02 ids
mnh01_ke <- mnh01 %>% filter(SITE == "Kenya", M01_TYPE_VISIT == 1)  %>% 
  select(-MOMID, -PREGID) %>% # pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
  left_join(mnh02_ke_ids, by = c("SCRNID"))
mnh01_all <- mnh01 %>% filter(SITE != "Kenya") # extract site-specific from merged data 
mnh01 <- bind_rows(mnh01_ke, mnh01_all) # rebind data 

## CMC ids
mnh02_cmc_ids <- mnh02 %>% filter(SITE == "India-CMC") %>% select(SCRNID, MOMID, PREGID) ## export mnh02 ids
mnh01_cmc <- mnh01 %>% filter(SITE == "India-CMC", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>% # pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
  left_join(mnh02_cmc_ids, by = c("SCRNID"))
mnh01_all <- mnh01 %>% filter(SITE != "India-CMC") # extract site-specific from merged data 
mnh01 <- bind_rows(mnh01_cmc, mnh01_all) # rebind data 

mnh01$M01_US_OHOSTDAT <- as.Date(mnh01$M01_US_OHOSTDAT, format = "%Y-%m-%d")
mnh01 <-mnh01 %>% filter(M01_US_GA_DAYS_AGE_FTS1 != 'hence G.A more than 25weeks"') %>% 
  mutate(M01_US_GA_DAYS_AGE_FTS1 = as.numeric(M01_US_GA_DAYS_AGE_FTS1))

#*****************************************************************************
#* PULL IDS OF INFANTS
#*****************************************************************************
# pull all infantids from mnh09
delivered_infantids <- mnh09 %>% 
  select(SITE, MOMID, PREGID, contains("M09_INFANTID_INF")) %>% 
  pivot_longer(cols = c(-SITE, -MOMID, -PREGID), 
               names_to = "var",
               values_to = "INFANTID") %>% 
  filter(!INFANTID %in% c("n/a", "0", "77", "1907-07-07", ""), 
         !is.na(INFANTID)) %>% 
  group_by(INFANTID) %>% 
  distinct() %>% 
  filter(PREGID %in% as.vector(mat_enroll$PREGID))

enrolled_ids_vec <- as.vector(mat_enroll$PREGID)

#*****************************************************************************
#* CONSTRUCTED VARIABLES GENERATION:
# Add constructed vars to forms that will be used across outcomes
#*****************************************************************************
### MNH01 ###
## add constructed vars for: 
# BOE_EDD, [varname: EDD_BOE]
# BOE_GA, [varnames: GESTAGE_ENROLL_BOE, BOE_GA_DAYS]
# Estimate conception date [varname: EST_CONCEP_DATE]

mnh01_constructed <- mnh01 %>% 
  ## only want the first ultrasound visit -- take the earliest date for each participant -- USE TYPE_VISIT = 1 FOR NOW
  filter(M01_TYPE_VISIT == 1 & PREGID %in% enrolled_ids_vec) %>% 
  arrange(M01_US_OHOSTDAT) %>%
  distinct(SITE, MOMID, PREGID, .keep_all = TRUE) # %>% 
select(SITE, MOMID, PREGID, M01_US_OHOSTDAT) 

### MNH04 ###
# this form will be used to pull any fetal losses reported in MNH04 
## add constructed vars for: 
# Gestational age at fetal loss [varname: GESTAGE_FETAL_LOSS_WKS]

mnh04_constructed <- mnh04 %>% 
  select(SITE, MOMID, PREGID,M04_ANC_OBSSTDAT,M04_TYPE_VISIT, M04_MAT_VISIT_MNH04, M04_PRG_DSDECOD, M04_FETAL_LOSS_DSSTDAT, M04_FETAL_LOSS_DSDECOD) %>% ## select only fetal loss variables
  # group_by(SITE, MOMID, PREGID) %>% 
  # arrange(M04_ANC_OBSSTDAT) %>%
  # distinct(MOMID, PREGID, M04_TYPE_VISIT, .keep_all = TRUE) %>%
  ## calculate the gestational age at fetal loss 
  # first join in EST_CONCEP_DATE from mnh01_constructed 
  left_join(mat_enroll[c("SITE", "MOMID", "PREGID", "EST_CONCEP_DATE")], by = c("SITE", "MOMID", "PREGID")) %>% 
  # replace default value date with NA 
  mutate(M04_FETAL_LOSS_DSSTDAT = replace(M04_FETAL_LOSS_DSSTDAT, M04_FETAL_LOSS_DSSTDAT==ymd("1907-07-07"), NA)) %>% 
  # calculate gestational age at fetal loss
  mutate(GESTAGE_FETAL_LOSS_DAYS = as.numeric(ymd(M04_FETAL_LOSS_DSSTDAT)-ymd(EST_CONCEP_DATE)), 
         GESTAGE_FETAL_LOSS_WKS = GESTAGE_FETAL_LOSS_DAYS %/% 7)

### MNH09 ###
## add constructed vars to mnh09 for:
# GA at Birth [varname: GESTAGEBIRTH_BOE_DAYS, GESTAGEBIRTH_BOE]
# DOB (earliest DOB in the event of multiple fetuses) [varname: DOB] 

mnh09_constructed <- mnh09 %>%
  ## 1. Calculating GA at birth ##
  # only want participants who are enrolled
  # merge in MNH01 info
  right_join(mat_enroll[c("SITE", "MOMID", "PREGID", "EST_CONCEP_DATE")], by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(mnh01_constructed[c("SITE", "MOMID", "PREGID", "M01_US_OHOSTDAT")], by = c("SITE", "MOMID", "PREGID")) %>% 
  # convert to date class
  mutate(M09_DELIV_DSSTDAT_INF1 = ymd(parse_date_time(M09_DELIV_DSSTDAT_INF1, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         M09_DELIV_DSSTDAT_INF2 = ymd(parse_date_time(M09_DELIV_DSSTDAT_INF2, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         M09_DELIV_DSSTDAT_INF3 = ymd(parse_date_time(M09_DELIV_DSSTDAT_INF3, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         M09_DELIV_DSSTDAT_INF4 = ymd(parse_date_time(M09_DELIV_DSSTDAT_INF4, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
  ) %>% 
  # pull earliest date of birth 
  # first replace default value date with NA 
  mutate(M09_DELIV_DSSTDAT_INF1 = replace(M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTDAT_INF1==ymd("1907-07-07"), NA),
         M09_DELIV_DSSTDAT_INF2 = replace(M09_DELIV_DSSTDAT_INF2, M09_DELIV_DSSTDAT_INF2%in% c(ymd("1907-07-07"), ymd("1905-05-05")), NA),
         M09_DELIV_DSSTDAT_INF3 = replace(M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTDAT_INF3==ymd("1907-07-07"), NA),
         M09_DELIV_DSSTDAT_INF4 = replace(M09_DELIV_DSSTDAT_INF4, M09_DELIV_DSSTDAT_INF4==ymd("1907-07-07"), NA)) %>% 
  mutate(DOB = 
           pmin(M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTDAT_INF2, 
                M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTDAT_INF4, na.rm = TRUE)) %>% 
  # generate indicator variable for having a birth outcome; if a birth outcome has been reported (M09_BIRTH_DSTERM =1 or 2), then BIRTH_OUTCOME_REPORTED ==1
  mutate(BIRTH_OUTCOME_REPORTED = ifelse(M09_BIRTH_DSTERM_INF1 == 1 | M09_BIRTH_DSTERM_INF1 == 2 | 
                                           M09_BIRTH_DSTERM_INF2 == 1 | M09_BIRTH_DSTERM_INF2 == 2 | 
                                           M09_BIRTH_DSTERM_INF3 == 1 | M09_BIRTH_DSTERM_INF3 == 2 |
                                           M09_BIRTH_DSTERM_INF4 == 1 | M09_BIRTH_DSTERM_INF4 == 2, 1, 0)) %>% 
  # calculate the number of days between DOB and estimated conception date
  mutate(GESTAGEBIRTH_BOE_DAYS = as.numeric(ymd(DOB) - ymd(EST_CONCEP_DATE)), 
         GESTAGEBIRTH_BOE = GESTAGEBIRTH_BOE_DAYS %/% 7) 


### MNH09 - long ###
# make data long for infant required outcomes -- pull out each infant's data and merge back together in long format
m09_INF1 <- mnh09_constructed %>%
  select(-contains("_INF2"), -contains("_INF3"), -contains("_INF4")) %>%
  rename_with(~str_remove(., '_INF1')) %>%
  rename("INFANTID" = "M09_INFANTID") %>%
  filter(!INFANTID %in% c("n/a", "0", "77", ""),
         !is.na(INFANTID)) %>%
  mutate(M09_DELIV_DSSTDAT = replace(M09_DELIV_DSSTDAT, M09_DELIV_DSSTDAT==ymd("1907-07-07"), NA), # replace default value date with NA
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="77:77", NA), # replace default value time with NA
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="07:07", NA), # replace default value time with NA
         DELIVERY_DATETIME = paste(M09_DELIV_DSSTDAT, M09_DELIV_DSSTTIM), # concatenate date and time of birth
         DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")  # assign time field type for time of birth
  )  %>%
  mutate_all(as.character) %>% 
  relocate(INFANTID, .after = PREGID)

m09_INF2 <- mnh09_constructed %>%
  select(-contains("_INF1"), -contains("_INF3"), -contains("_INF4")) %>%
  rename_with(~str_remove(., '_INF2')) %>%
  rename("INFANTID" = "M09_INFANTID") %>%
  filter(!INFANTID %in% c("n/a", "0", "77", ""),
         !is.na(INFANTID)) %>%
  mutate(M09_DELIV_DSSTDAT = replace(M09_DELIV_DSSTDAT, M09_DELIV_DSSTDAT==ymd("1907-07-07"), NA), # replace default value date with NA
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="77:77", NA), # replace default value time with NA
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="07:07", NA), # replace default value time with NA
         DELIVERY_DATETIME = paste(M09_DELIV_DSSTDAT, M09_DELIV_DSSTTIM), # concatenate date and time of birth
         DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")  # assign time field type for time of birth
  )  %>%
  mutate_all(as.character) %>% 
  relocate(INFANTID, .after = PREGID)


m09_INF3 <- mnh09_constructed %>%
  select(-contains("_INF1"), -contains("_INF2"), -contains("_INF4")) %>%
  rename_with(~str_remove(., '_INF3')) %>%
  rename("INFANTID" = "M09_INFANTID") %>%
  filter(!INFANTID %in% c("n/a", "0", "77", ""),
         !is.na(INFANTID)) %>%
  mutate(M09_DELIV_DSSTDAT = replace(M09_DELIV_DSSTDAT, M09_DELIV_DSSTDAT==ymd("1907-07-07"), NA), # replace default value date with NA
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="77:77", NA), # replace default value time with NA
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="07:07", NA), # replace default value time with NA
         DELIVERY_DATETIME = paste(M09_DELIV_DSSTDAT, M09_DELIV_DSSTTIM), # concatenate date and time of birth
         DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")  # assign time field type for time of birth
  )   %>%
  mutate_all(as.character) %>% 
  relocate(INFANTID, .after = PREGID)

# # to add when we need to 
# m09_INF4 <- mnh09_constructed %>%
#   select(-contains("_INF1"), -contains("_INF2"), -contains("_INF3")) %>%
#   rename_with(~str_remove(., '_INF4')) %>%
#   rename("INFANTID" = "M09_INFANTID") %>%
#   filter(!INFANTID %in% c("n/a", "0", "77", "),
#          !is.na(INFANTID)) %>%
#   mutate(M09_DELIV_DSSTDAT = replace(M09_DELIV_DSSTDAT, M09_DELIV_DSSTDAT==ymd("1907-07-07"), NA), # replace default value date with NA
#          M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="77:77", NA), # replace default value time with NA
#          M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="07:07", NA), # replace default value time with NA
#          DELIVERY_DATETIME = paste(M09_DELIV_DSSTDAT, M09_DELIV_DSSTTIM), # concatenate date and time of birth
#          DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")  # assign time field type for time of birth
#   )   %>%
#   mutate_all(as.character) %>% 
# relocate(INFANTID, .after = PREGID)

## bind all infants together 
m09_INF1$M09_DELIV_DSSTTIM = as.character(m09_INF1$M09_DELIV_DSSTTIM) ## data housekeeping here for time issues in data
m09_INF2$M09_DELIV_DSSTTIM = as.character(m09_INF2$M09_DELIV_DSSTTIM) ## data housekeeping here for time issues in data
m09_INF3$M09_DELIV_DSSTTIM = as.character(m09_INF3$M09_DELIV_DSSTTIM) ## data housekeeping here for time issues in data
# m09_INF4$M09_DELIV_DSSTTIM = as.character(m09_INF4$M09_DELIV_DSSTTIM) ## data housekeeping here for time issues in data

mnh09_long <- bind_rows(m09_INF1, m09_INF2, m09_INF3) %>%
  mutate(DOB = ymd(DOB)) %>%
  ## EXTRACT UNIQUE INFANTIDS FROM DELIVERY
  filter(INFANTID %in% as.vector(delivered_infantids$INFANTID)) %>% 
  mutate(GESTAGEBIRTH_BOE=as.numeric(GESTAGEBIRTH_BOE))

### MNH11 ###
## add constructed vars to mnh11 for:
# birthweight: PRISMA [varname: BWEIGHT_PRISMA] and PRISMA + Facility [varname: BWEIGHT_ANY]

## PRISMA
#1. prisma bw & hours <72 --> prisma 
#2. prisma bw w/o hours  --> use prisma 
#3. prisma bw & hours >=72 --> facility
mnh11_constructed <- mnh11 %>% 
  ## EXTRACT UNIQUE INFANTIDS FROM DELIVERY 
  filter(INFANTID %in% as.vector(delivered_infantids$INFANTID)) %>% 
  mutate(BWEIGHT_PRISMA = case_when(M11_BW_EST_FAORRES >=0 & M11_BW_EST_FAORRES < 72 & M11_BW_FAORRES > 0 ~ M11_BW_FAORRES,  # if time since birth infant was weight is between 0 & 72 hours
                                    M11_BW_FAORRES > 0 & (is.na(M11_BW_EST_FAORRES) | M11_BW_EST_FAORRES %in% c(-5,-7)) ~ M11_BW_FAORRES, # if prisma birthweight available and no time reported, use prisma
                                    M11_BW_FAORRES > 0 & M11_BW_EST_FAORRES >=72 ~ -5, # if prisma birthweight is available but time is >= 72 hours, not usable
                                    M11_BW_FAORRES < 0 ~ -5, # if prisma birthweight is missing, missing
                                    TRUE ~ -5), # if prisma birthweight is missing, replace with default value -5
         
         BWEIGHT_ANY = case_when((BWEIGHT_PRISMA <= 0  & M11_BW_FAORRES_REPORT > 0) | ## if PRISMA is missing and facility is not 
                                   (BWEIGHT_PRISMA < 0 & M11_BW_EST_FAORRES >= 72 & M11_BW_FAORRES_REPORT >0) ~ M11_BW_FAORRES_REPORT, ## OR if prisma is not missing but time is >7days, select facility
                                 BWEIGHT_PRISMA < 0 &  M11_BW_FAORRES_REPORT < 0 ~ -5, # if prisma is available but the time is invalid, use facility
                                 TRUE ~ M11_BW_FAORRES))  %>% 
  ## left join mnh09 birth outcome 
  full_join(mnh09_long[c("SITE", "MOMID", "PREGID", "INFANTID", "BIRTH_OUTCOME_REPORTED", "M09_BIRTH_DSTERM")],
            by = c("SITE", "MOMID", "PREGID", "INFANTID"))

### PULL LATEST VISIT ### 
## MNH11 + MNH13/14/15 -- pull the latest visit date for each infant - we will use this to calculate the "age infant was last seen"
mnh11_latest <- mnh11 %>% filter(M11_INF_VITAL_MNH11 ==1) %>%  select(SITE, INFANTID, M11_VISIT_OBSSTDAT)  %>% rename("VISITDATE" = M11_VISIT_OBSSTDAT) 
mnh13_latest <- mnh13%>% filter(M13_INF_VITAL_MNH13 ==1) %>%select(SITE, INFANTID, M13_VISIT_OBSSTDAT)  %>% rename("VISITDATE" = M13_VISIT_OBSSTDAT) %>% 
  filter(VISITDATE != 0) %>% mutate(VISITDATE = ymd(VISITDATE))
mnh14_latest <- mnh14%>% filter(M14_INF_VITAL_MNH14 ==1) %>%select(SITE, INFANTID, M14_VISIT_OBSSTDAT)  %>% rename("VISITDATE" = M14_VISIT_OBSSTDAT)
mnh15_latest <- mnh15%>% filter(M15_INF_VITAL_MNH15 ==1) %>%select(SITE, INFANTID, M15_OBSSTDAT) %>% rename("VISITDATE" = M15_OBSSTDAT)

# merge together 
latest_visit <- bind_rows(mnh11_latest, mnh13_latest, mnh14_latest, mnh15_latest) %>% group_by(SITE, INFANTID) %>% 
  summarise(LATESTDATE = max(VISITDATE))

### MNH24 ###
## add constructed vars to mnh24 for:
# Indicator if an infant dies [varname: DTH_INDICATOR]
# Age at death in days [varname: AGEDEATH_DAYS]
# Age at death in hours [varname: AGEDEATH_HRS]

mnh24_constructed <- mnh24 %>% 
  # merge in MNH13 to get visit dates 
  left_join(latest_visit, by = c("SITE", "INFANTID")) %>% 
  # merge in DOB information 
  left_join(mnh09_long, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  # concatenate death date time 
  mutate(M24_DTHDAT = replace(M24_DTHDAT, M24_DTHDAT==ymd("1907-07-07"), NA), # replace default value date with NA 
         M24_DTHTIM = replace(M24_DTHTIM, is.na(M24_DTHTIM), "12:30:00"), # replace default value time with NA 
         # M24_DTHTIM = replace(M24_DTHTIM, M24_DTHTIM=="07:07", NA), # replace default value time with NA 
         DEATH_DATETIME = paste(M24_DTHDAT, M24_DTHTIM), # concatenate date and time of birth 
         DEATH_DATETIME = as.POSIXct(DEATH_DATETIME, format= "%Y-%m-%d %H:%M")) %>% # assign time field type for time of birth
  mutate(DTHDAT = M24_DTHDAT) %>%
  # generate indicator if an infant died
  mutate(DTH_INDICATOR = case_when(M24_CLOSE_DSDECOD == 3 | !is.na(DTHDAT) ~ 1,TRUE~ 0)) %>% 
  # calculate age at death 
  mutate(AGEDEATH_DATETIME = floor(difftime(DEATH_DATETIME,DELIVERY_DATETIME,units = "hours")),
         AGEDEATH_DAYS = as.numeric(AGEDEATH_DATETIME) %/% 24,
         AGEDEATH_HRS = as.numeric(AGEDEATH_DATETIME) %% 24) %>% 
  select(names(mnh24),DOB,M24_DTHTIM,DEATH_DATETIME,DELIVERY_DATETIME, DTH_INDICATOR, DTHDAT,AGEDEATH_DAYS, AGEDEATH_HRS) %>%
  ## generate indicator variables for where things could go wrong
  mutate(MISSING_MNH09 = ifelse(is.na(DOB), 1, 0), # MISSING MNH09
         DOB_BEFORE_BIRTH = ifelse(is.na(DOB), 55,
                                   ifelse(DOB > DTHDAT, 1, 0)), # DEATH DATE BEOFRE DOB
         MISSING_TIME_DEATH = ifelse(DTH_INDICATOR==1 & is.na(M24_DTHTIM) | M24_DTHTIM == "77:77", 1, 0),
         CLOSEOUTID_MISSING_MNH02 = ifelse(PREGID %in% enrolled_ids_vec, 0, 1)) # NOT ENROLLED

## remove duplicate for zambia: 
mnh24_constructed <- mnh24_constructed %>% group_by(SITE, INFANTID) %>% 
  arrange(desc(M24_DTHDAT)) %>% 
  slice(1) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) %>% 
  ungroup()

#*****************************************************************************
#* Generate dataset with core infant variables (IDs, DOB, US date, BOE, Birth outcome (MNH09))

# Forms and variables needed: 
# IDs
# US date [MNH01_constructed]
# BOE [MNH01_constructed]
# Birth outcome [MNH09_long]
# DOB [MNH09_long]
# Loss reported [MNH04_constructed]
# closeout [MNH24_constructed]
#*****************************************************************************
mnh04_constructed_fetal_loss <- mnh04_constructed %>% filter(M04_PRG_DSDECOD == 2) %>% 
  select(SITE, MOMID, PREGID, M04_PRG_DSDECOD, M04_FETAL_LOSS_DSDECOD, M04_FETAL_LOSS_DSSTDAT,
         GESTAGE_FETAL_LOSS_DAYS, GESTAGE_FETAL_LOSS_WKS)


mnh09_long_sub <- mnh09_long %>%   select(SITE, MOMID, PREGID,INFANTID,M01_US_OHOSTDAT,
                                          M09_DELIV_DSSTDAT, M09_DELIV_DSSTTIM,DOB,  DELIVERY_DATETIME,M09_BIRTH_DSTERM, 
                                          GESTAGEBIRTH_BOE_DAYS, GESTAGEBIRTH_BOE)

inf_baseline <- mat_enroll %>% 
  full_join(mnh09_long_sub, by = c("SITE", "MOMID", "PREGID")) %>% 
  select(SITE, MOMID, PREGID,INFANTID, M01_US_OHOSTDAT,BOE_METHOD, GA_DIFF_DAYS, EDD_BOE, BOE_GA_DAYS_ENROLL, EST_CONCEP_DATE,
         DOB, M09_DELIV_DSSTTIM, DELIVERY_DATETIME,M09_BIRTH_DSTERM, 
         GESTAGEBIRTH_BOE_DAYS, GESTAGEBIRTH_BOE
  ) %>% 
  full_join(mnh04_constructed_fetal_loss, by = c("SITE", "MOMID", "PREGID")) %>% 
  ## add new var with a indicator variable for birth outcome reported
  mutate(BIRTH_OUTCOME_REPORTED = case_when(!is.na(DOB) | !is.na(M04_FETAL_LOSS_DSSTDAT) ~ 1, 
                                            TRUE ~ 0)) %>% 
  filter(BIRTH_OUTCOME_REPORTED==1) %>% 
  ## add new var with a single ga at birth --use mnh09, if missing, use mnh04; if both available, take the earliest reported date
  mutate(GESTAGEBIRTH_ANY = case_when(is.na(GESTAGEBIRTH_BOE) ~ as.numeric(GESTAGE_FETAL_LOSS_WKS), 
                                      !is.na(GESTAGE_FETAL_LOSS_WKS) & !is.na(GESTAGEBIRTH_BOE) & GESTAGEBIRTH_BOE <= GESTAGE_FETAL_LOSS_WKS ~ as.numeric(GESTAGEBIRTH_BOE), 
                                      !is.na(GESTAGE_FETAL_LOSS_WKS) & !is.na(GESTAGEBIRTH_BOE) & GESTAGE_FETAL_LOSS_WKS < GESTAGEBIRTH_BOE ~ as.numeric(GESTAGE_FETAL_LOSS_WKS), 
                                      
                                      TRUE ~ as.numeric(GESTAGEBIRTH_BOE))) %>% 
  mutate(GESTAGEBIRTH_ANY_DAYS = case_when(is.na(GESTAGEBIRTH_BOE_DAYS) ~ as.numeric(GESTAGE_FETAL_LOSS_DAYS), 
                                           !is.na(GESTAGE_FETAL_LOSS_DAYS) & !is.na(GESTAGEBIRTH_BOE_DAYS) & GESTAGEBIRTH_BOE_DAYS <= GESTAGE_FETAL_LOSS_WKS ~ as.numeric(GESTAGEBIRTH_BOE), 
                                           !is.na(GESTAGE_FETAL_LOSS_DAYS) & !is.na(GESTAGEBIRTH_BOE_DAYS) & GESTAGE_FETAL_LOSS_DAYS < GESTAGEBIRTH_BOE ~ as.numeric(GESTAGE_FETAL_LOSS_WKS), 
                                           
                                           TRUE ~ as.numeric(GESTAGEBIRTH_BOE_DAYS))) %>% 
  # if gestage at birth (reported in mnh09) was before the fetal loss date, replace the fetal loss date with the earliest report of loss
  mutate(GESTAGE_FETAL_LOSS_WKS = case_when(GESTAGEBIRTH_BOE < GESTAGE_FETAL_LOSS_WKS ~ as.numeric(GESTAGEBIRTH_BOE), 
                                            GESTAGE_FETAL_LOSS_WKS < GESTAGEBIRTH_BOE ~ as.numeric(GESTAGE_FETAL_LOSS_WKS), 
                                            TRUE ~ as.numeric(GESTAGE_FETAL_LOSS_WKS))) %>% 
  mutate(GESTAGE_FETAL_LOSS_DAYS = case_when(GESTAGEBIRTH_BOE_DAYS < GESTAGE_FETAL_LOSS_DAYS ~ as.numeric(GESTAGEBIRTH_BOE_DAYS), 
                                             GESTAGE_FETAL_LOSS_DAYS < GESTAGEBIRTH_BOE_DAYS ~ as.numeric(GESTAGE_FETAL_LOSS_DAYS), 
                                             TRUE ~ as.numeric(GESTAGE_FETAL_LOSS_DAYS))) %>% 
  ## remove any negative GESTAGEBIRTH_ANY; this likely due to a data entry error (year was off)
  filter(GESTAGEBIRTH_ANY >= 0) %>%
  ## generate indicator variable for loss reported in MNH04 or MNH09 
  mutate(FETAL_LOSS = case_when(!is.na(M04_FETAL_LOSS_DSSTDAT) | M09_BIRTH_DSTERM ==2 ~ 1, TRUE ~0 )) %>% 
  ## merge in closeout date 
  left_join(mnh24_constructed[c("SITE", "INFANTID", "M24_CLOSE_DSDECOD","DTH_INDICATOR",
                                "DEATH_DATETIME", "M24_DTHDAT","M24_DTHTIM", "AGEDEATH_DAYS",  "AGEDEATH_HRS")], by = c("SITE", "INFANTID")) %>% 
  # generate indicator if infant has closed out 
  mutate(CLOSEOUT = case_when(is.na(M24_CLOSE_DSDECOD) ~ 0, TRUE ~ 1)) %>% 
  
  ## rename variables 
  rename(ENROLL_US_DATE = M01_US_OHOSTDAT,
         TIME_BIRTH = M09_DELIV_DSSTTIM,
         INF_CLOSEOUT_REASON = M24_CLOSE_DSDECOD, 
         DEATHDATE_MNH24 = M24_DTHDAT, 
         DEATHTIME_MNH24 = M24_DTHTIM,
         BIRTH_OUTCOME = M09_BIRTH_DSTERM
  ) %>% 
  # generate indicator variable for delivery date (fetal loss/stillbirths/livebiths)
  mutate(PREG_END_DATE = case_when(!is.na(DOB) ~ ymd(DOB),
                                   !is.na(M04_FETAL_LOSS_DSSTDAT) ~ ymd(M04_FETAL_LOSS_DSSTDAT), 
                                   TRUE ~ NA)) %>% 
  # generate adjudication variable; instances where a loss was reported in MNH04 but a live birth reported in MNH09
  mutate(ADJUD_NEEDED = case_when(BIRTH_OUTCOME ==1 & FETAL_LOSS==1 ~ 1, 
                                  TRUE ~ 0)) %>% 
  # generate variable for livebirths 
  mutate(LIVEBIRTH = case_when(BIRTH_OUTCOME==1 ~ 1, TRUE ~ 0)
  ) %>% 
  select(SITE, MOMID, PREGID, INFANTID, ENROLL_US_DATE,BOE_METHOD, GA_DIFF_DAYS, EDD_BOE, BOE_GA_DAYS_ENROLL, EST_CONCEP_DATE,PREG_END_DATE, GESTAGEBIRTH_ANY,GESTAGEBIRTH_ANY_DAYS,
         BIRTH_OUTCOME, FETAL_LOSS,LIVEBIRTH,ADJUD_NEEDED, DOB, TIME_BIRTH, DELIVERY_DATETIME,M04_FETAL_LOSS_DSSTDAT,
         CLOSEOUT, INF_CLOSEOUT_REASON, DTH_INDICATOR, DEATHDATE_MNH24,DEATHTIME_MNH24,
         DEATH_DATETIME, AGEDEATH_DAYS, AGEDEATH_HRS,  GESTAGEBIRTH_BOE, GESTAGE_FETAL_LOSS_WKS
  ) 



## TIME VARYING DATASET 
# generate constructed variables that will be used for time-varyign outcomes 
# Indicator if an infant dies [varname: DTH_INDICATOR]
# Date the infant was last seen [varname: DATE_LAST_SEEN]
# Age the infant was last seen [varname: AGE_LAST_SEEN]

# Include variables for death indicator, agedeath, age last seen 
timevarying_constructed <- inf_baseline %>% 
  left_join(mnh09_long[c("SITE", "MOMID", "PREGID", "INFANTID", "M09_MAT_VISIT_MNH09")], by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  # merge in mnh11 
  full_join(mnh11_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  ## EXTRACT UNIQUE INFANTIDS FROM DELIVERY 
  filter(INFANTID %in% as.vector(inf_baseline$INFANTID)) %>% 
  # merge in latest visit data
  left_join(latest_visit, by = c("SITE", "INFANTID")) %>% 
  # if infant has died, the date of last visit is the death date
  mutate(DATE_LAST_SEEN = case_when(DTH_INDICATOR == 1 ~ ymd(DEATHDATE_MNH24), 
                                    TRUE ~ ymd(LATESTDATE))) %>% 
  select(-LATESTDATE) %>% 
  # calculate age at last seen for live births (age at death for dead)
  mutate(AGE_LAST_SEEN = case_when(DTH_INDICATOR == 0 ~ as.numeric(DATE_LAST_SEEN - DOB), 
                                   DTH_INDICATOR ==1 & is.na(AGEDEATH_DAYS) ~ as.numeric(DATE_LAST_SEEN - DOB), 
                                   DTH_INDICATOR == 1 ~ AGEDEATH_DAYS,
                                   TRUE ~ NA)) %>% 
  select(SITE, MOMID, PREGID, INFANTID,DTH_INDICATOR, DEATHDATE_MNH24, M09_MAT_VISIT_MNH09,BIRTH_OUTCOME, DOB, DELIVERY_DATETIME,M11_INF_VISIT_MNH11,
         BIRTH_OUTCOME_REPORTED, DATE_LAST_SEEN, AGE_LAST_SEEN, M09_MAT_VISIT_MNH09, M11_INF_VISIT_MNH11, M11_INF_DSTERM,
         DTH_INDICATOR, DEATHTIME_MNH24, DEATHDATE_MNH24, DEATH_DATETIME, AGEDEATH_DAYS,
         AGEDEATH_HRS, DATE_LAST_SEEN, AGE_LAST_SEEN)

#*****************************************************************************
#* 1. Low birth-weight 
# a. PRISMA staff weight (missing if no weight taken): [varname: LBW2500_PRISMA, LBW1500_PRISMA]
# b. PRISMA (+facility weight if PRISMA is missing): [varname:LBW2500_ANY, LBW1500_ANY]
# c. HOLD: PRISMA staff weight adjusted for time at weighing (+facility weight if PRISMA is missing) 

# Forms and variables needed: 
# M11_INF_DSTERM [MNH11]
# M11_BW_FAORRES [MNH11]
# M11_BW_FAORRES_REPORT [MNH11]
# M11_BW_EST_FAORRES [MNH11]
# BWEIGHT_PRISMA [mnh11_constructed]
# BWEIGHT_ANY [mnh11_constructed]
#*****************************************************************************
## QUESTION: for missing prisma -- currently we have it as missing prisma but haVe facility; should we change to be just missing prisma (same goes for facility)
## this will make the numbers add up a bit better 

lowbirthweight <- inf_baseline %>% 
  select(SITE, MOMID, PREGID, INFANTID, BIRTH_OUTCOME, LIVEBIRTH) %>% 
  left_join(mnh11_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  ## pull key variables 
  select(SITE, MOMID,PREGID, INFANTID, BIRTH_OUTCOME,LIVEBIRTH, BWEIGHT_PRISMA, BWEIGHT_ANY, M11_BW_FAORRES, M11_BW_FAORRES_REPORT, M11_BW_EST_FAORRES) %>% 
  ## LBW PRISMA measured (bw <2500g)
  mutate(LBW2500_PRISMA = case_when(BWEIGHT_PRISMA >= 0 & BWEIGHT_PRISMA < 2500 ~ 1,
                                    BWEIGHT_PRISMA <= 0 ~  55, 
                                    TRUE ~ 0)) %>% 
  ## LBW PRISMA measured (bw <1500g)
  mutate(LBW1500_PRISMA = case_when(BWEIGHT_PRISMA >= 0 & BWEIGHT_PRISMA < 1500 ~ 1,
                                    BWEIGHT_PRISMA <= 0 ~ 55,
                                    TRUE ~ 0)) %>% 
  ## LBW PRISMA measured (bw <2500g); varname: LBW2500_ANY
  mutate(LBW2500_ANY = case_when(BWEIGHT_ANY >= 0 & BWEIGHT_ANY < 2500 ~ 1,
                                 BWEIGHT_ANY <= 0 | is.na(BWEIGHT_ANY) ~ 55,
                                 TRUE ~ 0)) %>% 
  ## LBW PRISMA measured (bw <1500g); varname: LBW1500_ANY 
  mutate(LBW1500_ANY = case_when(BWEIGHT_ANY >= 0 & BWEIGHT_ANY < 1500 ~ 1,
                                 BWEIGHT_ANY <= 0 | is.na(BWEIGHT_ANY) ~ 55,
                                 TRUE ~ 0)) %>% 
  mutate(LBW_CAT_PRISMA = case_when(BWEIGHT_PRISMA >= 0 & BWEIGHT_PRISMA < 1500 ~ 11, 
                                    BWEIGHT_PRISMA >= 1500 & BWEIGHT_PRISMA < 2500 ~ 12, 
                                    BWEIGHT_PRISMA >= 2500 ~ 13, 
                                    BWEIGHT_PRISMA < 0 | M11_BW_EST_FAORRES > 150 ~ 55,
                                    TRUE ~ NA)) %>% 
  ## ANY LBW categorical variable: (any bw <1500g)=11, (any bw <2500g)=12, (any bw >= 2500g)
  mutate(LBW_CAT_ANY = case_when(BWEIGHT_ANY >= 0 & BWEIGHT_ANY < 1500 ~ 11,
                                 BWEIGHT_ANY >= 1500 & BWEIGHT_ANY < 2500 ~ 12,
                                 BWEIGHT_ANY >= 2500 ~ 13, 
                                 BWEIGHT_ANY < 0 ~ 55,
                                 TRUE ~ NA)) %>% 
  ## generate denominator (remove missing)
  mutate(DATA_COMPLETE_DENOM = case_when(BIRTH_OUTCOME==1 ~ 1, TRUE ~ 0),
         LBW_PRISMA_DENOM = case_when(LIVEBIRTH==1 & LBW_CAT_PRISMA %in% c(11,12,13) ~ 1, TRUE ~ 0), 
         LBW_ANY_DENOM = case_when(LIVEBIRTH==1 & LBW_CAT_ANY %in% c(11,12,13) ~ 1, TRUE ~ 0)) %>% 
  ## generate indicator for data completeness
  mutate(MISSING_PRISMA = case_when(is.na(M11_BW_FAORRES) | (M11_BW_FAORRES < 0) ~ 1,
                                    TRUE ~ 0), 
         MISSING_FACILITY = case_when(M11_BW_FAORRES_REPORT < 0 ~ 1,
                                      TRUE ~ 0),
         MISSING_BOTH = case_when((M11_BW_FAORRES < 0 | is.na(M11_BW_FAORRES)) &
                                    (M11_BW_FAORRES_REPORT < 0 | is.na(M11_BW_FAORRES_REPORT)) ~ 1,
                                  TRUE ~ 0), 
         MISSING_TIME = case_when(M11_BW_EST_FAORRES < 0 | is.na(M11_BW_EST_FAORRES) ~ 1,
                                  TRUE ~ 0)) %>% 
  mutate(BW_TIME = case_when(M11_BW_EST_FAORRES < 0 | M11_BW_EST_FAORRES >= 97 ~ NA,
                             TRUE ~ M11_BW_EST_FAORRES)) 



# write.csv(lowbirthweight, paste0(path_to_save, "lowbirthweight", ".csv"), row.names=FALSE)


## LOWBIRTHWEIGIHT DATA VIZ BELOW
colors <- c("#ea4336", "#fbbd05", "#33a853")

lowbirthweight_plot_prisma <- lowbirthweight %>% filter(BWEIGHT_PRISMA>0) %>% 
  mutate(FILL = ifelse(BWEIGHT_PRISMA>=0 & BWEIGHT_PRISMA <1500, 1, 
                       ifelse(BWEIGHT_PRISMA>=1500 & BWEIGHT_PRISMA < 2500, 2, 
                              ifelse(BWEIGHT_PRISMA>= 2500, 3, NA))))

# histogram of birthweights by prisma trained staff 
PRISMA_lowbirthweight <- ggplot(data = lowbirthweight_plot_prisma) + 
  geom_histogram(aes(x = BWEIGHT_PRISMA, fill = as.factor(FILL)))  + 
  scale_fill_manual(values = colors, 
                    labels = c("<1500g", "1500 to <2500g", ">2500g")) +  
  facet_grid(vars(SITE), scales = "free") + 
  scale_x_continuous(breaks = seq(0,5000,500)) + 
  ggtitle("Birthweight by PRISMA") + 
  ylab("Count") + 
  xlab("Birthweight (grams)") + 
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + #color
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5), 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  geom_vline(mapping=aes(xintercept=1500), linetype ="dashed", color = "black") +
  geom_vline(mapping=aes(xintercept=2500), linetype ="dashed", color = "black")

# ggsave(paste0("PRISMA_lowbirthweight_", UploadDate, ".pdf"), path = path_to_save_figures, 
#        width = 6, height = 4)


# histogram of birthweights by facility reports
lowbirthweight_plot_facility <- lowbirthweight %>% filter(M11_BW_FAORRES_REPORT>0) %>% 
  mutate(FILL = ifelse(M11_BW_FAORRES_REPORT>=0 & M11_BW_FAORRES_REPORT <1500, 1, 
                       ifelse(M11_BW_FAORRES_REPORT>=1500 & M11_BW_FAORRES_REPORT < 2500, 2, 
                              ifelse(M11_BW_FAORRES_REPORT>= 2500, 3, NA))))

FACILITY_lowbirthweight <- ggplot(data = lowbirthweight_plot_facility) + 
  geom_histogram(aes(x = M11_BW_FAORRES_REPORT, fill = as.factor(FILL)))  + 
  scale_fill_manual(values = colors, 
                    labels = c("<1500g", "1500 to <2500g", ">2500g")) +  
  facet_grid(vars(SITE), scales = "free") + 
  scale_x_continuous(breaks = seq(0,5000,500)) + 
  ggtitle("Birthweight by Facility") + 
  ylab("Count") + 
  xlab("Birthweight (grams)") + 
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + #color
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5), 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  geom_vline(mapping=aes(xintercept=1500), linetype ="dashed", color = "black") +
  geom_vline(mapping=aes(xintercept=2500), linetype ="dashed", color = "black")

# ggsave(paste0("FACILITY_lowbirthweight_", UploadDate, ".pdf"), path = path_to_save_figures,
#        width = 6, height = 4)


## birthweight timing 
Hours_birthweight <- ggplot(data=lowbirthweight,
                            aes(x=BW_TIME, fill = BW_TIME)) + 
  geom_histogram(aes(fill = ..x..), binwidth = 1) + 
  facet_grid(vars(SITE), scales = "free") +
  scale_x_continuous(breaks = seq(0,96,8)) + 
  scale_fill_gradient(low='#6fd404', high='red', guide = "none") + 
  ggtitle("Hours from birth infant was weighed, all births, by Site") + 
  ylab("Count") + 
  xlab("Hours") + 
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + #color
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5), # angle = 60, 
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_vline(mapping=aes(xintercept=24), linetype ="dashed", color = "black") +
  geom_vline(mapping=aes(xintercept=48), linetype ="dashed", color = "black") + 
  geom_vline(mapping=aes(xintercept=72), linetype ="dashed", color = "black")

# ggsave(paste0("Hours_birthweight_", UploadDate, ".pdf"), path = path_to_save_figures, 
#        width = 6, height = 4)

#*****************************************************************************
#* 6. Stillbirth
# a. STILLBIRTH_SIGNS_LIFE 
# b. STILLBIRTH_20WK
# c. STILLBIRTH_22WK
# d. STILLBIRTH_28WK
# e. STILLBIRTH_GESTAGE_CAT
# f. STILLBIRTH_TIMING

# Forms and variables needed: 
# M04_PRG_DSDECOD [mnh04]
# GESTAGEBIRTH_BOE [mnh09_constructed]
# GESTAGE_FETAL_LOSS_WKS [mnh04_constructed]
# CRY_CEOCCUR_INF1-4 [MNH09]
# FHR_VSTAT_INF1-4 [MNH09]
# MACER_CEOCCUR_INF1-4 [MNH09]
# CORD_PULS_CEOCCUR_INF1-4 [MNH09]

# Notes: 
# all induced abortions are excluded 
#*****************************************************************************
stillbirth <- inf_baseline %>% 
  # select(SITE,INFANTID, MOMID, PREGID, DOB, TIME_BIRTH, DELIVERY_DATETIME,  FETAL_LOSS, GESTAGEBIRTH_ANY,GESTAGEBIRTH_ANY_DAYS, BIRTH_OUTCOME) %>% 
  left_join(mnh09_long[c("SITE", "MOMID", "PREGID", "INFANTID", "M09_MAT_VISIT_MNH09", "M09_CRY_CEOCCUR", "M09_FHR_VSTAT",
                         "M09_MACER_CEOCCUR", "M09_CORD_PULS_CEOCCUR")],
            by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  
  # merge in mnh11 information to get birth outcome and signs of live
  left_join(mnh11_constructed[c("SITE","INFANTID", "MOMID", "PREGID", "M11_BREATH_FAIL_CEOCCUR")], 
            by = c("SITE","INFANTID", "MOMID", "PREGID")) %>% 
  
  # generate date of fetal loss variable 
  mutate(FETAL_LOSS_DATE = case_when(BIRTH_OUTCOME == 2 | FETAL_LOSS==1 ~ PREG_END_DATE,TRUE ~ NA)) %>% 
  
  ## add new var if the ga at birth is <20wks 
  mutate(GESTAGE_UNDER20 = case_when(GESTAGEBIRTH_ANY<20 ~ 1,
                                     TRUE ~ 0)) %>% 
  
  ## START CONSTRUCTING OUTCOMES ## Death prior to delivery of a fetus at 20 weeks of gestation (or >350 g weight, if gestational age is unavailable).
  # a. STILLBIRTH_SIGNS_LIFE: Delivery of a fetus showing no signs of life, as indicated by absence of breathing, heartbeat, pulsation of the umbilical cord, or definite movements of voluntary muscles.
  # 1, Yes = Definitely live birth: cried, pulsate, initiated and sustained breathing
  # 0, No = Definitely dead: no heart rate, macerated
  mutate(STILLBIRTH_SIGNS_LIFE = case_when(M09_CRY_CEOCCUR ==1 | M09_CORD_PULS_CEOCCUR ==1 | M11_BREATH_FAIL_CEOCCUR==0 ~ 1, 
                                           M09_FHR_VSTAT ==0 | M09_MACER_CEOCCUR ==1 ~ 0,
                                           TRUE ~ 55)) %>%
  # b. STILLBIRTH_20WK
  mutate(STILLBIRTH_20WK = case_when(is.na(GESTAGEBIRTH_ANY) ~ 55, ## if ga at birth is missing OR ga at birth is <20
                                     GESTAGEBIRTH_ANY >= 20 & (LIVEBIRTH ==0 | FETAL_LOSS ==1) ~ 1, # if birth outcome is fetal loss and gestage birth is >= 20wks
                                     GESTAGEBIRTH_ANY < 20 ~ 0,
                                     # GA_AT_BIRTH_ANY < 20 ~ 66,
                                     (LIVEBIRTH ==0 | FETAL_LOSS ==1) & STILLBIRTH_SIGNS_LIFE == 1 ~ 66,  # if fetal loss not reported but there are signs of life reported -- 66 
                                     BIRTH_OUTCOME == 1 & STILLBIRTH_SIGNS_LIFE == 0 ~ 99, # if birth outcome is reported but is missing signs of life -- 99 
                                     TRUE ~ 0)) %>%
  ## if missing ga at birth & ga at fetal loss--55
  # c. STILLBIRTH_22WK
  mutate(STILLBIRTH_22WK = case_when(STILLBIRTH_20WK ==1 & GESTAGEBIRTH_ANY >= 22 ~ 1, # if birth outcome is fetal loss and gestage birth is >= 22wks
                                     TRUE ~ 0)) %>% 
  
  # d. STILLBIRTH_24WK
  mutate(STILLBIRTH_24WK = case_when(STILLBIRTH_20WK ==1 & GESTAGEBIRTH_ANY >= 24 ~ 1, # if birth outcome is fetal loss and gestage birth is >= 24wks
                                     TRUE ~ 0)) %>% 
  
  # e. STILLBIRTH_28WK
  mutate(STILLBIRTH_28WK = case_when(STILLBIRTH_20WK ==1 & GESTAGEBIRTH_ANY >= 28 ~ 1, # if birth outcome is fetal loss and gestage birth is >= 28wks
                                     TRUE ~ 0)) %>%
  
  # f. STILLBIRTH_GESTAGE_CAT - 
  mutate(STILLBIRTH_GESTAGE_CAT = case_when(BIRTH_OUTCOME==1 ~ 14, ##live birth
                                            STILLBIRTH_20WK == 1 & GESTAGEBIRTH_ANY>=20 & GESTAGEBIRTH_ANY <28 ~ 11, # "Early: Death prior to delivery of a fetus at 20 to 27 weeks of gestation.  
                                            STILLBIRTH_20WK == 1 & GESTAGEBIRTH_ANY>=28 & GESTAGEBIRTH_ANY <37 ~ 12, # Late: Death prior to delivery of a fetus at 28 to 36 weeks of gestation.
                                            STILLBIRTH_20WK == 1 &  GESTAGEBIRTH_ANY >= 37 ~ 13, # Term: Death prior to delivery of a fetus at >37 weeks of gestation.    
                                            GESTAGEBIRTH_ANY<20 ~ 66, # if GA at birth is <20, exclude from these categories
                                            TRUE ~ 55)) %>% 
  # g. STILLBIRTH_TIMING
  mutate(STILLBIRTH_TIMING = case_when(STILLBIRTH_20WK == 1 & (M09_FHR_VSTAT==0 | M09_MACER_CEOCCUR ==1) ~ 11,
                                       STILLBIRTH_20WK == 1 & (M09_FHR_VSTAT ==1 & M09_MACER_CEOCCUR == 0) ~ 12,
                                       STILLBIRTH_20WK == 0 | GESTAGEBIRTH_ANY < 20 ~ 77, ## if no stillbirth or GA<20 (miscarriage), the stillbirth timing is 77, not applicable 
                                       (STILLBIRTH_20WK == 1 & (is.na(M09_FHR_VSTAT) | is.na(M09_MACER_CEOCCUR))) | 
                                         (STILLBIRTH_20WK == 1 & (M09_FHR_VSTAT %in% c(55,77,99,66) | M09_MACER_CEOCCUR%in% c(55,77,99,66)))~ 99, 
                                       TRUE ~ 99)) %>% 
  
  # STILLBIRTH_DENOMINATOR - stillbirth or live birth 
  mutate(STILLBIRTH_DENOM = case_when((STILLBIRTH_20WK==1 | LIVEBIRTH==1) & !STILLBIRTH_GESTAGE_CAT %in% c(55,66) ~ 1,
                                      TRUE ~ 0)) %>% 
  mutate(DATA_COMPLETE_DENOM = case_when(BIRTH_OUTCOME ==1 | BIRTH_OUTCOME==2 ~ 1, TRUE ~ 0)) %>% 
  
  ## EXTRA INFO FOR REPORT ## 
  # missing signs of life information -- denominator is anyone who had a fetal loss reported in mnh04 or mnh09 
  mutate(MISSING_SIGNS_OF_LIFE = case_when(BIRTH_OUTCOME==2 & (M09_CRY_CEOCCUR == 77 | M09_CORD_PULS_CEOCCUR == 77 | 
                                                                 M09_FHR_VSTAT==77 | M09_MACER_CEOCCUR == 77 | M11_BREATH_FAIL_CEOCCUR == 77) ~ 1,
                                           TRUE ~ 0)) %>% 
  mutate(INFANTID = case_when(INFANTID == "" ~ NA, TRUE ~ INFANTID)) %>% 
  select(SITE, MOMID, PREGID, INFANTID, BIRTH_OUTCOME, FETAL_LOSS, GESTAGEBIRTH_ANY, FETAL_LOSS_DATE,  PREG_END_DATE, contains("STILLBIRTH"), MISSING_SIGNS_OF_LIFE)


table(stillbirth$STILLBIRTH_TIMING, stillbirth$SITE)


table(stillbirth$M09_BIRTH_DSTERM,"signs of life"= stillbirth$STILLBIRTH_SIGNS_LIFE)

# export data 
# write.csv(stillbirth, paste0(path_to_save, "stillbirth" ,".csv"), row.names=FALSE)

#*****************************************************************************
#* 2. Pre-term delivery 
# a. Postterm delivery (>=41 weeks): Delivery after 41 weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_GT41]
# b. Term delivery (37 to <41 weeks): Delivery between 37 and <41 weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT41]
# c. Preterm delivery (<37 weeks): Delivery prior to 37 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT37]
# d. Preterm delivery (<34 weeks): Delivery prior to 34 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT34]
# e. Preterm delivery (<32 weeks): Delivery prior to 32 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT32]
# f. Preterm delivery (<28 weeks): Delivery prior to 28 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT28]
# g. Preterm delivery severity (categorical): Post-term (>=41 wks), Late preterm (34 to <37 wks), early preterm (32 to <34 wks), very preterm (28 to <32 wks), extermely preterm (<28 weeks) [varname: PRETERMBIRTH_CAT]

# Forms and variables needed: 
# M09_BIRTH_DSTERM_INF1-4 [MNH09]
# GESTAGEBIRTH_BOE [mnh01_constructed]
#*****************************************************************************
test <- infant_outcomes %>% select(SITE, MOMID, PREGID, INFANTID,GESTAGEBIRTH_ANY,PRETERMDELIV_CAT, LIVEBIRTH, STILLBIRTH_20WK, FETAL_LOSS, BIRTH_OUTCOME_REPORTED) %>%
  mutate(discrep = case_when((STILLBIRTH_20WK==1 | LIVEBIRTH==1) & BIRTH_OUTCOME_REPORTED != 1 ~ 1, TRUE ~ 0))

## Forms required: MNH01 constructed, MNH09_constructed
preterm_birth <- inf_baseline %>% 
  select(SITE, MOMID, PREGID, INFANTID, BIRTH_OUTCOME, LIVEBIRTH, GESTAGEBIRTH_ANY, GA_DIFF_DAYS) %>%
  left_join(stillbirth[c("SITE", "MOMID", "PREGID","INFANTID", "STILLBIRTH_20WK")], by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  ## 1. Generate indicator variable for those who have had a birth outcome ## 
  ## 2. Generate Outcomes for PRETERM BIRTH (livebirths)  
  # a. Post-term delivery (>=41 weeks): Delivery after 41 weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_GT41]
  mutate(PRETERMBIRTH_GT41 = case_when(LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 41 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # b. Term delivery (37 to <41 weeks): Delivery between 37 and <41 weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT41]
  mutate(PRETERMBIRTH_LT41 = case_when(LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 41 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # c. Preterm birth (<37 weeks): Delivery prior to 37 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT37]
  mutate(PRETERMBIRTH_LT37 = case_when(LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 37 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # d. Preterm birth (<34 weeks): Delivery prior to 34 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT34]
  mutate(PRETERMBIRTH_LT34 = case_when(LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 34 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # e. Preterm birth (<32 weeks): Delivery prior to 32 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT32]
  mutate(PRETERMBIRTH_LT32 = case_when(LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 32 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # f. Preterm birth (<28 weeks): Delivery prior to 28 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT28]
  mutate(PRETERMBIRTH_LT28 = case_when(LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 28 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # g. Preterm birth severity (categorical): POSTTERM (>=41 wks), term (37 to 41 wks), Late preterm (34 to <37 wks), early preterm (32 to <34 wks), very preterm (28 to <32 wks), extermely preterm (<28 weeks) [varname: PRETERMBIRTH_CAT]
  mutate(PRETERMBIRTH_CAT = case_when(LIVEBIRTH == 0 ~ 77, 
                                      LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >=41 ~ 10, 
                                      LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 37 & GESTAGEBIRTH_ANY <41 ~  11,  
                                      LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 34 & GESTAGEBIRTH_ANY < 37 ~ 12, 
                                      LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 32 & GESTAGEBIRTH_ANY < 34 ~ 13,
                                      LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 28 & GESTAGEBIRTH_ANY < 32 ~ 14,
                                      LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY <28 ~ 15,
                                      TRUE ~ 55)) %>% 
  
  ## 2. Generate Outcomes for PRETERM DELIVERY (livebirths + stillbirths) ## 
  mutate(PRETERMDELIV_GT41 = case_when((LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 41 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # b. Term delivery (37 to <41 weeks): Delivery between 37 and <41 weeks of gestation (live or stillbirth). [varname: PRETERMDELIV_LT41]
  mutate(PRETERMDELIV_LT41 = case_when((LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 41 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # c. Preterm birth (<37 weeks): Delivery prior to 37 completed weeks of gestation (live or stillbirth). [varname: PRETERMDELIV_LT37]
  mutate(PRETERMDELIV_LT37 = case_when((LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 37 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # d. Preterm birth (<34 weeks): Delivery prior to 34 completed weeks of gestation (live or stillbirth). [varname: PRETERMDELIV_LT34]
  mutate(PRETERMDELIV_LT34 = case_when((LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 34 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # e. Preterm birth (<32 weeks): Delivery prior to 32 completed weeks of gestation (live or stillbirth). [varname: PRETERMDELIV_LT32]
  mutate(PRETERMDELIV_LT32 = case_when((LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 32 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # f. Preterm birth (<28 weeks): Delivery prior to 28 completed weeks of gestation (live or stillbirth). [varname: PRETERMDELIV_LT28]
  mutate(PRETERMDELIV_LT28 = case_when((LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 28 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # g. Preterm birth severity (categorical): postterm (>41 wks), term (37 to 41 wks wks), Late preterm (34 to <37 wks), early preterm (32 to <34 wks), very preterm (28 to <32 wks), extermely preterm (<28 weeks) [varname: PRETERMDELIV_CAT]
  mutate(PRETERMDELIV_CAT = case_when((LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 41 ~ 10, 
                                      (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 37 & GESTAGEBIRTH_ANY <41 ~ 11,  
                                      (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 34 & GESTAGEBIRTH_ANY < 37 ~ 12, 
                                      (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 32 & GESTAGEBIRTH_ANY < 34 ~ 13,
                                      (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 28 & GESTAGEBIRTH_ANY < 32 ~ 14,
                                      (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY <28 ~ 15,
                                      TRUE ~ 55)) %>% 
  ## generate denominators 
  mutate(DATA_COMPLETE_DENOM = case_when((LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) ~ 1, TRUE ~ 0))  %>% 
  mutate(BIRTH_OUTCOME_REPORTED = case_when((BIRTH_OUTCOME ==1 | BIRTH_OUTCOME ==2) ~ 1, TRUE ~ 0))

#*****************************************************************************
#* 3. Size for Gestational Age (SGA)
# a. Size for gestational age - categorical. [varname: SGA_CAT]
# b. Preterm small for gestational age: Preterm < 37 weeks AND SGA (<10th). [varname: INF_SGA_PRETERM]
# c. Preterm appropriate for gestational age: Preterm < 37 weeks AND not SGA (<10th). [varname: INF_AGA_PRETERM]
# d. Term small for gestational age: Term >=37 weeks AND SGA (<10th). [varname: INF_SGA_TERM]
# e. Term appropriate for gestational age: Term >=37 weeks AND not SGA (<10th). [varname: INF_AGA_TERM]

# Forms and variables needed: 
# EDD_BOE [mnh09_long]
# GESTAGEBIRTH_BOE [mnh09_long]
# BIRTH_DSTERM_INF1-4 [mnh09]
# SEX_INF1-4 [mnh09]
# BWEIGHT_PRISMA [mnh11_constructed]
# BWEIGHT_ANY [mnh11_constructed]
# PRETERMBIRTH_LT37 [preterm_birth; generated in section above]
# PRETERMBIRTH_CAT [preterm_birth; generated in section above]
#*****************************************************************************
sga <- inf_baseline %>% 
  select(SITE,INFANTID, MOMID, PREGID, GESTAGEBIRTH_ANY,GESTAGEBIRTH_ANY_DAYS, LIVEBIRTH, BIRTH_OUTCOME)  %>% 
  left_join(mnh09_long[c("SITE", "MOMID", "PREGID", "INFANTID", "M09_BIRTH_DSTERM", "M09_SEX", "M09_INFANTS_FAORRES")],
            by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  ## merge with mnh11 
  left_join(mnh11_constructed %>% select(-c(M09_BIRTH_DSTERM)), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  ## convert weight from grams to kg 
  mutate(BWEIGHT_PRISMA_KG = case_when(LIVEBIRTH==0 ~ -7, 
                                       BWEIGHT_PRISMA > 0 ~ BWEIGHT_PRISMA/1000,
                                       TRUE ~ -5), 
         BWEIGHT_ANY_KG = case_when(LIVEBIRTH==0 ~ -7, 
                                    BWEIGHT_ANY < 0 | is.na(BWEIGHT_ANY) ~ -5, 
                                    TRUE ~ BWEIGHT_ANY/1000)) %>% 
  # convert to numeric 
  mutate(across(c(GESTAGEBIRTH_ANY_DAYS, BWEIGHT_ANY_KG, M09_SEX), as.numeric)) %>%
  
  ## calculate percentile
  mutate(SGA_CENTILE = case_when(LIVEBIRTH==0 ~ -7, 
                                 is.na(GESTAGEBIRTH_ANY_DAYS) ~ -5,  
                                 M09_SEX == 1  & BWEIGHT_ANY_KG > 0 ~ floor(igb_wtkg2centile(GESTAGEBIRTH_ANY_DAYS, BWEIGHT_ANY_KG, sex = "Male")),
                                 M09_SEX == 2 & BWEIGHT_ANY_KG > 0 ~ floor(igb_wtkg2centile(GESTAGEBIRTH_ANY_DAYS, BWEIGHT_ANY_KG, sex = "Female")),
                                 BWEIGHT_ANY_KG <= 0 ~ -5, 
                                 TRUE ~ -5)) %>% 
  
  ## START CONSTRUCTING OUTCOMES ##
  # a. Size for gestational age - categorical. [varname: SGA_CAT]
  mutate(SGA_CAT = case_when(LIVEBIRTH==0 ~ 77, 
                             SGA_CENTILE >= 0 & SGA_CENTILE < 3 ~ 11,   # SGA_CENTILE < 3rd
                             SGA_CENTILE >= 3 & SGA_CENTILE < 10 ~ 12,  # SGA_CENTILE < 10th
                             SGA_CENTILE >= 10 & SGA_CENTILE < 90 ~ 13, # AGA 10th to <90th 
                             SGA_CENTILE >= 90 ~ 14, # LGA >= 90
                             TRUE ~ 55)) %>%  # 55 for missing
  ## merge with preterm births dataset to get preterm vars 
  left_join(preterm_birth %>% select(-GESTAGEBIRTH_ANY, -LIVEBIRTH), by = c("SITE","INFANTID", "MOMID", "PREGID")) %>%
  # b. Preterm small for gestational age: Preterm < 37 weeks AND SGA (<10th). [varname: INF_SGA_PRETERM]
  mutate(INF_SGA_PRETERM = case_when(LIVEBIRTH==0 ~ 77, 
                                     PRETERMBIRTH_LT37 == 1 & SGA_CAT == 12 ~ 1, 
                                     SGA_CAT == 55 ~ 55,
                                     TRUE ~ 0)) %>% 
  # c. Preterm appropriate for gestational age: Preterm < 37 weeks AND not SGA (<10th). [varname: INF_AGA_PRETERM]
  mutate(INF_AGA_PRETERM = case_when(LIVEBIRTH==0 ~ 77,
                                     PRETERMBIRTH_LT37 == 1 & (SGA_CAT == 13 | SGA_CAT == 14) ~ 1,
                                     TRUE ~ 0)) %>% 
  # d. Term small for gestational age: Term >=37 weeks AND SGA (<10th). [varname: INF_SGA_TERM]
  mutate(INF_SGA_TERM = case_when(LIVEBIRTH==0 ~ 77,
                                  PRETERMBIRTH_CAT == 11 & SGA_CAT == 12 ~ 1,
                                  TRUE ~ 0)) %>% 
  # e. Term appropriate for gestational age: Term >=37 weeks AND not SGA (<10th). [varname: INF_AGA_TERM]
  mutate(INF_AGA_TERM = case_when(LIVEBIRTH==0 ~ 77,
                                  PRETERMBIRTH_CAT == 11 & (SGA_CAT == 13 | SGA_CAT == 14) ~ 1,
                                  TRUE ~ 0))  %>% 
  # generate denominator 
  # mutate(SGA_DENOM = case_when(LIVEBIRTH ==1 & GESTAGEBIRTH_ANY_DAYS >= 168 & GESTAGEBIRTH_ANY_DAYS <= 300 ~ 1, TRUE ~0)) %>% ## package will only run for births between 24+0 & 42+6wks 
  # mutate(SGA_DENOM = case_when(LIVEBIRTH ==1 ~ 1, TRUE ~0)) %>% ## package will only run for births between 24+0 & 42+6wks 
  
  select(SITE, MOMID, PREGID, INFANTID,LIVEBIRTH, M09_SEX,GESTAGEBIRTH_ANY,GESTAGEBIRTH_ANY_DAYS,BWEIGHT_ANY_KG, M11_BW_EST_FAORRES, M11_BW_FAORRES,
         M11_BW_FAORRES_REPORT, BWEIGHT_ANY, SGA_CENTILE,SGA_CAT, INF_SGA_PRETERM, INF_AGA_PRETERM, INF_SGA_TERM, INF_AGA_TERM, M09_INFANTS_FAORRES, SGA_DENOM) 

# export
# write.csv(sga, paste0(path_to_save, "sga" ,".csv"), row.names=FALSE)
#*****************************************************************************
#* Mortality
#  4. Neonatal mortality 
# a. <24 hours 
# b. Early neontal mortality: first  7 days 
# c. Late neonatal mortality: between 7 & 28 days

# 5. Infant mortality: death during the first year of life 

# Forms and variables needed: 
# M11_INF_DSTERM [mnh11]
# AGEDEATH [timevarying_constructed]
# AGEDEATH_HRS [timevarying_constructed]
# DTH_INDICATOR [timevarying_constructed]
# DOB [MNH09]
# AGEDEATH_HRS [AGE_LAST_SEEN]
#*****************************************************************************
## DENOMINATOR is anyone with: 
# Date of birth (mnh09)
# live birth (mnh11)
# have to be able to link mnh09 with mnh11

mortality <- inf_baseline %>% 
  select(SITE,INFANTID, MOMID, PREGID,LIVEBIRTH, CLOSEOUT, PREG_END_DATE,DOB,TIME_BIRTH,  DELIVERY_DATETIME, FETAL_LOSS, 
         GESTAGEBIRTH_ANY,GESTAGEBIRTH_ANY_DAYS, BIRTH_OUTCOME) %>% 
  left_join(timevarying_constructed[c("SITE", "MOMID", "PREGID", "INFANTID", "M09_MAT_VISIT_MNH09", "M11_INF_VISIT_MNH11", "M11_INF_DSTERM",
                                      "DTH_INDICATOR", "DEATHTIME_MNH24", "DEATHDATE_MNH24", "DEATH_DATETIME", "AGEDEATH_DAYS",
                                      "AGEDEATH_HRS", "DATE_LAST_SEEN", "AGE_LAST_SEEN")],
            by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  
  ## generate variable if death is reported among live birth but is missing time of death 
  mutate(DTH_TIME_MISSING = case_when(BIRTH_OUTCOME==1 & DTH_INDICATOR==1 & (DEATHTIME_MNH24%in% c("55:55", "77:77") | is.na(DEATHTIME_MNH24)) ~ 1,
                                      TRUE ~ 0),
         DTH_DATE_MISSING = case_when(BIRTH_OUTCOME==1 & DTH_INDICATOR==1 & (DEATHDATE_MNH24%in% c(ymd("1905-05-05"), ymd("1907-07-07")) | is.na(DEATHDATE_MNH24)) ~ 1,
                                      TRUE ~ 0)) %>% 
  
  ## generate outcome for neonatal death if infant dies <28 days of life
  mutate(NEO_DTH = case_when((BIRTH_OUTCOME==1 & DTH_INDICATOR==1 & is.na(AGEDEATH_DAYS)) | 
                               (BIRTH_OUTCOME==1 & DTH_INDICATOR==1 & AGEDEATH_DAYS < 0) |
                               DTH_TIME_MISSING ==1 | DTH_DATE_MISSING ==1  ~ 55,  ## if missing the three criteria, they are missing
                             DTH_INDICATOR == 0 ~ 0, 
                             BIRTH_OUTCOME == 1 & AGEDEATH_DAYS >= 0 & AGEDEATH_DAYS < 28 ~ 1, ## if live birth AND age of death is <= 28, they get a 1
                             is.na(BIRTH_OUTCOME) & is.na(DOB) ~ 55, ## THESE ARE PEOPLE WHO ARE REPORTING A DEATH BUT ARE MISSING OR HAVE INVALID AGE AT DEATH
                             TRUE ~ 0)) %>%  
  
  ## generate outcome for infant death if infant dies <365 days of life
  mutate(INF_DTH = case_when((BIRTH_OUTCOME==1 & DTH_INDICATOR==1 & is.na(AGEDEATH_DAYS)) | 
                               (BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & AGEDEATH_DAYS < 0) |
                               DTH_TIME_MISSING ==1 | DTH_DATE_MISSING ==1~ 55,  ## if missing the three criteria, they are missing
                             DTH_INDICATOR == 0 ~ 0, 
                             BIRTH_OUTCOME == 1 & AGEDEATH_DAYS < 365 ~ 1, ## if live birth AND age of death is < 365, they get a 1
                             is.na(BIRTH_OUTCOME) & is.na(DOB) ~ 55, ## THESE ARE PEOPLE WHO ARE REPORTING A DEATH BUT ARE MISSING OR HAVE INVALID AGE AT DEATH
                             TRUE ~ 0)) %>%  
  
  ## timing of neonatal mortality
  mutate(DTH_0D = case_when(BIRTH_OUTCOME == 1 & DTH_INDICATOR ==1 & is.na(AGEDEATH_HRS) ~ 0,
                            BIRTH_OUTCOME == 1 & DTH_INDICATOR ==1 & (AGEDEATH_DAYS ==0 & AGEDEATH_HRS >= 0 & AGEDEATH_HRS < 24) ~ 1,
                            TRUE ~ 0),
         DTH_7D = case_when(BIRTH_OUTCOME == 1 & DTH_INDICATOR ==1 & ((AGEDEATH_DAYS ==0 & AGEDEATH_HRS >= 0) & AGEDEATH_DAYS < 7) ~ 1,
                            TRUE ~ 0), 
         # DTH_7D = case_when(BIRTH_OUTCOME == 1 & DTH_INDICATOR ==1 & AGEDEATH_HRS >=0 & AGEDEATH < 7 ~ 1,
         #                    TRUE ~ 0), 
         DTH_28D = case_when(BIRTH_OUTCOME == 1 & DTH_INDICATOR ==1 & AGEDEATH_DAYS >=0 & AGEDEATH_DAYS < 28 ~ 1,
                             TRUE ~ 0),
         DTH_365D = case_when(BIRTH_OUTCOME == 1 & DTH_INDICATOR ==1 & AGEDEATH_DAYS >=28 & AGEDEATH_DAYS < 365 ~ 1,
                              TRUE ~ 0)) %>% 
  ## calculate denominators; if you have passed the risk window (age last seen >= risk window) 
  mutate(
    # DENOM_28d = case_when(AGE_LAST_SEEN >= 28 | (DTH_INDICATOR ==1 & AGEDEATH_DAYS < 28) |
    #                          (DTH_INDICATOR==1 & DTH_TIME_MISSING==1) ~ 1,
    #                        TRUE ~ 0),
    # to generate risk period for neonatal and infant deaths 
    ESTIMATED_AGE_AT_UPLOAD = as.numeric(ymd(UploadDate)-DOB),
    DENOM_365d = case_when(LIVEBIRTH ==1 & (AGE_LAST_SEEN >= 365 | ESTIMATED_AGE_AT_UPLOAD >= 365) ~ 1, 
                           TRUE ~ 0),
    
    DENOM_28d = case_when((LIVEBIRTH ==1 & (ESTIMATED_AGE_AT_UPLOAD >= 28 | (DTH_INDICATOR ==1 & AGEDEATH_DAYS < 28) |
                                              (DTH_INDICATOR==1 & DTH_TIME_MISSING==1))) ~ 1,
                          TRUE ~ 0),
    
  ) %>%
  ## generate denominators for data completeness (all live births) 
  mutate(DATA_COMPLETE_DENOM = case_when(BIRTH_OUTCOME==1 ~ 1, TRUE ~ 0)) %>% 
  # generate indicator variables where things could go wrong with this outcome
  mutate(ID_MISSING_ENROLLMENT = case_when(PREGID %in% enrolled_ids_vec ~ 0,
                                           TRUE ~ 1),
         DOB_AFTER_DEATH = case_when(BIRTH_OUTCOME==1 & DEATH_DATETIME < DELIVERY_DATETIME ~ 1, ## if death comes before dob
                                     TRUE ~ 0), 
         MISSING_MNH09 = case_when(is.na(M09_MAT_VISIT_MNH09) ~ 1, ## if mnh09 is missing (we need this for DOB)
                                   TRUE ~ 0), 
         MISSING_MNH11 = case_when(is.na(M11_INF_VISIT_MNH11) ~ 1, ## if mnh11 is missing (we need this form for birth outcome)
                                   TRUE ~ 0), 
         INVALID_DTH_REPORT = case_when(BIRTH_OUTCOME ==2  & DTH_INDICATOR == 1 ~ 1, # in order to be included as a neonatal or infant death, the infant had to have been born alive
                                        TRUE ~ 0) 
  ) %>% 
  select(SITE, MOMID, PREGID, INFANTID,ESTIMATED_AGE_AT_UPLOAD, CLOSEOUT, DOB, DELIVERY_DATETIME, BIRTH_OUTCOME, DATE_LAST_SEEN,DEATHTIME_MNH24, DEATHDATE_MNH24,
         AGE_LAST_SEEN, DTH_INDICATOR,DTH_DATE_MISSING, DEATH_DATETIME,DTH_TIME_MISSING, AGEDEATH_DAYS, AGEDEATH_HRS,NEO_DTH,INF_DTH, DATA_COMPLETE_DENOM,
         ID_MISSING_ENROLLMENT,DOB_AFTER_DEATH, contains("MISSING"),contains("DTH"), contains("DENOM"), INVALID_DTH_REPORT, contains("has")) 

table(mortality$DENOM_28d2, mortality$SITE)
table(mortality$DENOM_28d, mortality$SITE)

test <- mortality %>% filter(DTH_365D==1) %>% select(SITE, MOMID, PREGID, INFANTID, DTH_INDICATOR, 
                                                     BIRTH_OUTCOME, DEATHDATE_MNH24,DATE_LAST_SEEN, 
                                                     AGE_LAST_SEEN,ESTIMATED_AGE_AT_UPLOAD,AGEDEATH_DAYS, DTH_365D, DENOM_365d)
# export
# write.csv(mortality, paste0(path_to_save, "mortality" ,".csv"), row.names=FALSE)

#  4. Neonatal mortality: Denominator is all live births reported in MNH11 with mnh09 filled out 
# a. <24 hours 
# b. Early neontal mortality: first  7 days 
# c. Late neonatal mortality: between 7 & 28 days
neonatal_mortality <- mortality %>%
  # generate total neonatal deaths 
  mutate(TOTAL_NEO_DEATHS = case_when(BIRTH_OUTCOME ==1 & DTH_INDICATOR ==1 & AGEDEATH_DAYS < 28 & DTH_TIME_MISSING==0 & DOB_AFTER_DEATH==0~ 1,
                                      TRUE ~ 0)) %>% 
  # generate single timing variables 
  # Death of a liveborn baby within the first 24 hours of life [NEO_DTH_24HR]
  mutate(NEO_DTH_24HR = case_when((BIRTH_OUTCOME ==1 & DTH_INDICATOR == 1 & (is.na(AGEDEATH_HRS)| DEATHTIME_MNH24=="77:77")) | 
                                    (BIRTH_OUTCOME == 1 & DTH_INDICATOR == 1 & AGEDEATH_HRS < 0) ~ 55, 
                                  BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & (AGEDEATH_DAYS==0 & AGEDEATH_HRS <24) ~  1,
                                  TRUE ~ 0)) %>% 
  
  # Death of a liveborn baby from 1 to 7 days following delivery. [NEO_DTH_EAR]
  mutate(NEO_DTH_EAR = case_when(BIRTH_OUTCOME == 1 & DTH_INDICATOR == 1 & is.na(AGEDEATH_DAYS) ~ 55, 
                                 BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & AGEDEATH_DAYS >= 1 & AGEDEATH_DAYS < 7 ~ 1,
                                 TRUE ~ 0)) %>% 
  
  # Death of a liveborn baby from 7 to 28 days following delivery. [NEO_DTH_LATE]
  mutate(NEO_DTH_LATE = case_when(BIRTH_OUTCOME == 1 & DTH_INDICATOR == 1 & is.na(AGEDEATH_DAYS) ~ 55, 
                                  BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & AGEDEATH_DAYS >= 7 & AGEDEATH_DAYS < 28 ~ 1,
                                  TRUE ~ 0)) %>% 
  # generate categorical outcome
  mutate(NEO_DTH_CAT = case_when(NEO_DTH_24HR == 1 ~ 11, 
                                 NEO_DTH_EAR == 1 ~ 12, 
                                 NEO_DTH_LATE == 1 ~ 13, 
                                 DTH_INDICATOR != 1 ~ 10, ## no death
                                 # AGEDEATH >= 28 ~ 55, ## this is infant mortality 
                                 (DTH_INDICATOR==1 & is.na(AGEDEATH_DAYS)) | DTH_TIME_MISSING==1 | DOB_AFTER_DEATH==1|
                                   (DTH_INDICATOR==1 & AGEDEATH_DAYS ==0 & (is.na(AGEDEATH_HRS) | AGEDEATH_HRS <0)) ~ 55 ## death reporting but missing valid time of death
  )
  )


table(neonatal_mortality$TOTAL_NEO_DEATHS, neonatal_mortality$SITE)
table(neonatal_mortality$NEO_DTH_CAT, neonatal_mortality$SITE)

# export
# write.csv(neonatal_mortality, paste0(path_to_save, "neonatal_mortality" ,".csv"), row.names=FALSE)

#  5. Infant mortality 
# a. <365 days
infant_mortality <- mortality %>% 
  # generate categorical outcome
  mutate(INF_DTH_CAT = case_when(BIRTH_OUTCOME == 1 & (AGEDEATH_DAYS>= 28 & AGEDEATH_DAYS < 365) ~ 14, 
                                 BIRTH_OUTCOME == 1 & DTH_INDICATOR != 1 ~ 10, ## no death
                                 BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & is.na(AGEDEATH_DAYS)  ~ 55 ## death reporting but missing valid time of death
  )
  ) %>% 
  # generate variable for infant death from 28 weeks of life (do to avoid overlap of neonatal mortality)
  mutate(INF_DTH_FROM28 = case_when(BIRTH_OUTCOME == 1 & (AGEDEATH_DAYS>= 28 & AGEDEATH_DAYS < 365) ~ 1, 
                                    BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & is.na(AGEDEATH_DAYS)  ~ 55 ## death reporting but missing valid time of death
                                    TRUE ~ 0))

# generate total infant deaths 
mutate(TOTAL_INF_DEATHS = case_when(BIRTH_OUTCOME == 1 & DTH_INDICATOR ==1 & (AGEDEATH_DAYS < 365) ~ 1,
                                    TRUE ~ 0))

table(infant_mortality$TOTAL_INF_DEATHS, infant_mortality$SITE)
table(infant_mortality$INF_DTH, infant_mortality$SITE)



test <- mortality %>% filter(INF_DTH == 1)
table(mortality$INF_DTH, mortality$SITE)

# export
# write.csv(infant_mortality, paste0(path_to_save, "infant_mortality" ,".csv"), row.names=FALSE)


#*****************************************************************************
#* 7. Fetal Death
# Definition: A product of human conception, irrespective of the duration of the pregnancy, 
# which, after expulsion or extraction, does not breath or show any other evidence of life 
# such as beating of the heart, pulsation of the umbilical cord, or definite movement of voluntary muscles, 
# whether or not the umbilical cord has been cut or the placenta is attached. 
# a. INF_ABOR_SPN
# b. INF_ABOR_IND
# c. INF_FETAL_DTH

# Forms and variables needed: 
# STILLBIRTH_20WK [stillbirth]
# GESTAGE_FETAL_LOSS_DAYS [mnh04_constructed_fetal_loss]
# GESTAGE_FETAL_LOSS_WKS [mnh04_constructed_fetal_loss]
# M04_FETAL_LOSS_DSDECOD [mnh04_constructed_fetal_loss]
# M01_FETUS_CT_PERES_US [mnh01_constructed]

#*****************************************************************************
fetal_death <- inf_baseline %>% 
  select(SITE, INFANTID, MOMID, PREGID, BIRTH_OUTCOME, PREG_END_DATE, GESTAGEBIRTH_ANY) %>%
  # merge in stillbirth data (generated above in the "stillbirth" outcome/dataset)
  left_join(stillbirth %>% select(-c(PREG_END_DATE, BIRTH_OUTCOME, GESTAGEBIRTH_ANY)), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  
  # merge in mnh04 fetal loss data 
  left_join(mnh04_constructed_fetal_loss[c("SITE", "MOMID", "PREGID", "M04_PRG_DSDECOD", "M04_FETAL_LOSS_DSDECOD", "M04_FETAL_LOSS_DSSTDAT")],
            by = c("SITE", "MOMID", "PREGID")) %>%
  # generate invalid fetal loss response variable (where fetal loss is reported, but the specify type variable is a default value)
  mutate(INVALID_FETAL_LOSS = case_when(M04_PRG_DSDECOD == 2 & # if fetal loss is reported 
                                          (is.na(M04_FETAL_LOSS_DSDECOD) | (M04_FETAL_LOSS_DSDECOD == 55 | # but type of fetal loss is missing or default value, this is invalid
                                                                              M04_FETAL_LOSS_DSDECOD == 77)) ~ 1, 
                                        TRUE ~ 0)) %>% 
  # a. generate variable for spontaneous abortion (Fetal loss <20 weeks (miscarriage))
  ## [varname: INF_ABOR_SPN]
  mutate(INF_ABOR_SPN = case_when(GESTAGEBIRTH_ANY < 20 & (is.na(M04_FETAL_LOSS_DSDECOD) | M04_FETAL_LOSS_DSDECOD != 2) ~ 1,  # if GA at time of fetal loss is <20wks and not induced
                                  TRUE~ 0)) %>% 
  # b. generate variable for induced abortion (Elective surgical procedure or medical intervention to terminate the pregnancy at any gestational age) 
  ## [varname: INF_ABOR_IND]
  mutate(INF_ABOR_IND = case_when(M04_FETAL_LOSS_DSDECOD == 2 ~ 1, # if specified fetal loss is "induced abortion" at any GA 
                                  # M04_FETAL_LOSS_DSDECOD == 2 & 
                                  #   is.na(GESTAGEBIRTH_ANY) ~ 55, # if  fetal loss is induced abortion but no fetal loss date reported --> missing
                                  
                                  TRUE~ 0)) %>% 
  # c. generate variable for fetal death @ unknown GA (reported fetal loss but missing fetal loss date)
  ## [varname: INF_FETAL_DTH_UNGA]
  mutate(INF_FETAL_DTH_UNGA = case_when(M04_PRG_DSDECOD == 2 & is.na(PREG_END_DATE) ~ 1, # if fetal loss is reported but is missing fetal loss date
                                        TRUE~ 0)) %>% 
  
  # d. generate variable for all fetal death (stillbirth or miscarriage) 
  ## [varname: INF_FETAL_DTH]
  mutate(INF_FETAL_DTH = case_when(STILLBIRTH_20WK == 1 | INF_ABOR_SPN == 1 | INF_FETAL_DTH_UNGA == 1 ~ 1,
                                   # is.na(GESTAGEBIRTH_BOE) & INF_ABOR_IND == 55 ~ 55,
                                   TRUE~ 0)) %>% 
  # d. generate fetal death denominator (all deliveries EXCLUDING induced abortions) 
  ## [varname: INF_FETAL_DTH_DENOM]
  mutate(INF_FETAL_DTH_DENOM = case_when(INF_ABOR_IND==1 ~ 0,
                                         TRUE~ 1))  %>% 
  # d. generate fetal death denominator (all deliveries) 
  ## [varname: INF_FETAL_DTH_OTHR_DENOM]
  mutate(INF_FETAL_DTH_OTHR_DENOM = 1) 


# export data 
# write.csv(fetal_death, paste0(path_to_save, "fetal_death" ,".csv"), row.names=FALSE)
#*****************************************************************************
#* 8. Birth Asphyxia
# defintion: Clinician reports failure to breathe spontaneously in the first minute after delivery.
# a. INF_ASPH

# Forms and variables needed: 
# BREATH_FAIL_CEOCCUR [mnh11_constructed]; FEB 2 UPDATE: ADD  
# INF_PROCCUR_1 [mnh11_constructed] (did the infant require breathing assistance: Oxygen); FEB 2 UPDATE: REMOVE 
# INF_PROCCUR_2 [mnh11_constructed] (did the infant require breathing assistance: bag and mask ventilation)
# INF_PROCCUR_3 [mnh11_constructed] (did the infant require breathing assistance: continuous positive airway pressure)
# INF_PROCCUR_4 [mnh11_constructed] (did the infant require breathing assistance: repeated stimulation/suction as part of resuscitation at birth)
# INF_PROCCUR_5 [mnh11_constructed] (did the infant require breathing assistance: intubation and mechanical ventilation)
# INF_PROCCUR_6 [mnh11_constructed] (did the infant require breathing assistance: chest compressions)
# BIRTH_COMPL_MHTERM_3 [mnh20] (specify birth complication: birth asphyxia/respiratory distress of the newborn)
# Id10110 [mnh28] ## TO ADD IN LATER
#*****************************************************************************

birth_asphyxia <- inf_baseline %>% 
  select(SITE, INFANTID, MOMID, PREGID, BIRTH_OUTCOME) %>%
  # merge in mnh11 data 
  left_join(mnh11_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  select(SITE, INFANTID, MOMID, PREGID, BIRTH_OUTCOME, M11_BREATH_FAIL_CEOCCUR, contains("M11_INF_PROCCUR_")) %>%
  # merge in mnh20 variables 
  left_join(mnh20[c("SITE", "MOMID", "PREGID", "M20_BIRTH_COMPL_MHTERM_3")], 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  # a. generate variable for birth asphyxia (Clinician reports failure to breathe spontaneously in the first minute after delivery.)
  ## [varname: INF_ASPH]
  mutate(INF_ASPH = case_when(M11_BREATH_FAIL_CEOCCUR == 1 | M20_BIRTH_COMPL_MHTERM_3 == 1 | 
                                M11_INF_PROCCUR_2 == 1 |  
                                M11_INF_PROCCUR_3 == 1 | M11_INF_PROCCUR_4 == 1 |
                                M11_INF_PROCCUR_5 == 1 |  M11_INF_PROCCUR_6 == 1 ~ 1, 
                              
                              M11_BREATH_FAIL_CEOCCUR == 77  & 
                                M11_INF_PROCCUR_2 == 77 & 
                                M11_INF_PROCCUR_3 == 77 & M11_INF_PROCCUR_4 == 77 &
                                M11_INF_PROCCUR_5 == 77 & M11_INF_PROCCUR_6 == 77 ~ 66, ## all input variables are 77
                              
                              is.na(M11_BREATH_FAIL_CEOCCUR) & 
                                is.na(M11_INF_PROCCUR_2) & 
                                is.na(M11_INF_PROCCUR_3) & is.na(M11_INF_PROCCUR_4) &
                                is.na(M11_INF_PROCCUR_5) & is.na(M11_INF_PROCCUR_6) ~ 55, ## all input variables are NA
                              
                              TRUE ~ 0)) %>% 
  # generate denominators 
  # data complete denominator: all live births 
  mutate(DATA_COMPLETE_DENOM = case_when(BIRTH_OUTCOME ==1 ~ 1, TRUE ~ 0),
         INF_ASPH_DENOM = case_when(BIRTH_OUTCOME ==1 ~ 1, TRUE ~ 0)
  ) 

# export data 
# write.csv(birth_asphyxia, paste0(path_to_save, "birth_asphyxia" ,".csv"), row.names=FALSE)

#*****************************************************************************
#* 9. Hyberbilirubinemia
# definition: Defined as the presence of excess bilirubin during the first week of life (delivery to 7 days of age). Clinician reports failure to breathe spontaneously in the first minute after delivery.
# a. INF_ASPH

# Forms and variables needed: 
# For IPC: 
# VISIT_OBSSTDAT [mnh11_constructed] (Interview date)
# BILIRUBIN_LBPERF [mnh11_constructed] (Was transcutaneous bilirubin measured within 24 hours of birth?)
# TBILIRUBIN_UMOLL_LBORRES [mnh11_constructed] (Record total bilirubin results, umol/L)
# TBILIRUBIN_ OBSSTTIM [mnh11_constructed] (Record time that transcutaneous bilirubin was assessed)
# YELLOW_CEOCCUR [mnh11_constructed] (Does infant have yellow palms and soles of foot?)
# JAUND_CEOCCUR [mnh11_constructed] (Observe infant for jaundice (yellow color of eyes or skin). Is jaundice present?)
# JAUND_CESTDAT [mnh11_constructed] (When did jaundice first appear?)
# DOB [mnh09_long]

# For PNC: 
# VISIT_OBSSTDAT [mnh14] (Date of point-of-care diagnostics)
# TCB_VSSTAT [mnh14] (Was transcutaneous bilirubin assessed at this visit?)
# TCB_OBSSTTIM [mnh14] (Specify time that measurement was taken)
# TCB_UMOLL_LBORRES [mnh14] (Record total bilirubin results at time of visit)
# JAUND_CEOCCUR [mnh13] (Observe infant for jaundice (yellow color of eyes or skin). Is jaundice present?)
# JAUND_CESTTIM [mnh13] (If PNC-0 visit, when did jaundice first appear?)
# YELL_CEOCCUR [mnh13] (Does infant have yellow palms and soles of foot?)
# DOB [mnh09_long]

#*****************************************************************************
# Three criteria
# 1. TCB >15 at any time (TBILIRUBIN_UMOLL_LBORRES @ IPC OR TCB_UMOLL_LBORRES @ PNC)
# 2. TCB >AAP time-specific cutoff (serum bili threshold minus 3 for each GA+age group)
# 3. By IMCI jaundice criteria (YELLOW_CEOCCUR, JAUND_CEOCCUR, JAUND_CESTDAT)

## rename visit type variables for mnh13 and mnh14
mnh13_hyperbili <- mnh13 %>% 
  rename(TYPE_VISIT = "M13_TYPE_VISIT") %>% 
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(M13_VISIT_OBSSTDAT) %>%
  filter(INFANTID %in% as.vector(inf_baseline$INFANTID)) %>% 
  distinct(MOMID, PREGID, TYPE_VISIT, .keep_all = TRUE) 

mnh14_hyperbili <- mnh14 %>% 
  rename(TYPE_VISIT = "M14_TYPE_VISIT") %>% 
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(M14_VISIT_OBSSTDAT) %>%
  filter(INFANTID %in% as.vector(inf_baseline$INFANTID)) %>% 
  distinct(MOMID, PREGID, TYPE_VISIT, .keep_all = TRUE) 

# generate dataset for hyperbili outcomes  
hyperbili <- inf_baseline %>% 
  select(SITE, INFANTID, MOMID, PREGID, LIVEBIRTH, BIRTH_OUTCOME, DELIVERY_DATETIME, DOB, TIME_BIRTH, GESTAGEBIRTH_ANY, GESTAGEBIRTH_ANY_DAYS) %>%
  # merge in mnh11 data 
  left_join(mnh11_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  select(SITE, MOMID, PREGID, INFANTID, LIVEBIRTH, BIRTH_OUTCOME, DELIVERY_DATETIME, DOB, TIME_BIRTH, GESTAGEBIRTH_ANY,GESTAGEBIRTH_ANY_DAYS,
         M11_VISIT_OBSSTDAT,M11_VISIT_OBSSTTIM, M11_BILIRUBIN_LBPERF, M11_TBILIRUBIN_UMOLL_LBORRES, 
         M11_TBILIRUBIN_OBSSTTIM, M11_YELLOW_CEOCCUR, M11_JAUND_CEOCCUR, M11_JAUND_CESTDAT) %>% 
  mutate(TYPE_VISIT = 6) %>% 
  # merge in infant PNC clinical status information
  full_join(mnh13_hyperbili[c("SITE", "MOMID", "PREGID", "INFANTID","TYPE_VISIT","M13_VISIT_OBSSTDAT", "M13_JAUND_CEOCCUR", "M13_JAUND_CESTTIM", "M13_YELL_CEOCCUR")],
            by = c("SITE", "MOMID", "PREGID", "INFANTID", "TYPE_VISIT")) %>% 
  # merge in infant PoC information
  full_join(mnh14_hyperbili[c("SITE", "MOMID", "PREGID", "INFANTID","TYPE_VISIT", "M14_VISIT_OBSSTDAT", "M14_TCB_VSSTAT", "M14_TCB_OBSSTTIM", "M14_TCB_UMOLL_LBORRES")],
            by = c("SITE", "MOMID", "PREGID", "INFANTID", "TYPE_VISIT")) %>% 
  # move type visit and delivery variables to the front of the dataset 
  relocate(TYPE_VISIT, .after="INFANTID") %>% 
  relocate(contains("M09"), .after = "TYPE_VISIT") %>% 
  # severe jaundice at any time point in IPC or PNC
  group_by(SITE, MOMID, PREGID, INFANTID) %>% 
  fill(DOB, .direction = "down") %>% 
  fill(DELIVERY_DATETIME, .direction = "down") %>% 
  fill(GESTAGEBIRTH_ANY, .direction = "down") %>% 
  ungroup() %>% 
  # convert time variables to time class
  mutate(M11_VISIT_OBSSTTIM = replace(M11_VISIT_OBSSTTIM, M11_VISIT_OBSSTTIM %in% c("77:77", "99:99", "55:55:00"), NA), # replace default value time with NA 
         M11_VISIT_DATETIME = as.POSIXct(paste(M11_VISIT_OBSSTDAT, M11_VISIT_OBSSTTIM), format= "%Y-%m-%d %H:%M"),  # assign time field type 
         # generate variable for TCB assessment date time (M14_TCB_DATETIME)
         M14_TCB_OBSSTTIM = replace(M14_TCB_OBSSTTIM, M14_TCB_OBSSTTIM %in% c("77:77", "99:99", "55:55:00"), NA), # replace default value time with NA 
         M14_TCB_DATETIME = as.POSIXct(paste(M14_VISIT_OBSSTDAT, M14_TCB_OBSSTTIM), format= "%Y-%m-%d %H:%M")  # assign time field type
  ) %>% 
  # calculate age (hours and days) at MNH11 visit (if no default value visit date, then calculate)
  mutate(M11_AGE_AT_VISIT_DATETIME = floor(difftime(M11_VISIT_DATETIME,DELIVERY_DATETIME,units = "hours")),
         M11_AGE_AT_VISIT_DAYS = as.numeric(M11_AGE_AT_VISIT_DATETIME) %/% 24,
         M11_AGE_AT_VISIT_HRS = as.numeric(M11_AGE_AT_VISIT_DATETIME) %% 24) %>% 
  
  # calculate age (days) at visit mnh13
  mutate(M13_AGE_AT_VISIT_DAYS = ifelse(!M13_VISIT_OBSSTDAT %in% c(ymd("1907-07-07"), ymd("1905-05-05")), 
                                        floor(difftime(M13_VISIT_OBSSTDAT,DOB,units = "days")), NA)) %>% 
  # calculate age (hours and days) at TCB assessment (mnh14) visit (if no default value visit date, then calculate)
  mutate(M14_AGE_AT_VISIT_DATETIME = floor(difftime(M14_TCB_DATETIME,DELIVERY_DATETIME,units = "hours")),
         M14_AGE_AT_VISIT_DAYS = as.numeric(M14_AGE_AT_VISIT_DATETIME) %/% 24,
         M14_AGE_AT_VISIT_HRS = as.numeric(M14_AGE_AT_VISIT_DATETIME) %% 24) %>% 
  # units conversion 
  mutate(M14_TCB_UMOLL_LBORRES = ifelse(SITE %in% c("Kenya", "Zambia") & !M14_TCB_UMOLL_LBORRES %in% c(-5,-7), M14_TCB_UMOLL_LBORRES/17.1, M14_TCB_UMOLL_LBORRES),
         M11_TBILIRUBIN_UMOLL_LBORRES = ifelse(SITE %in% c("Kenya", "Zambia") & !M11_TBILIRUBIN_UMOLL_LBORRES %in% c(-5,-7), M11_TBILIRUBIN_UMOLL_LBORRES/17.1, M11_TBILIRUBIN_UMOLL_LBORRES),
  )

# Convert hyperbili dataset to wide format
# extract smaller datasets by visit type and assign a suffix with the visit type. We can then merge back together 
# labor and delivery (visit type = 6)
hyperbili_ld <- hyperbili %>% filter(TYPE_VISIT==6) %>%
  select(SITE, MOMID, PREGID, INFANTID, DOB, DELIVERY_DATETIME,GESTAGEBIRTH_ANY, GESTAGEBIRTH_ANY_DAYS, contains("M09"), contains("M11")) %>% 
  rename_with(~paste0(., "_", 6), .cols = c(contains("M11"), contains("M09"))) 

visit_types_num <- c(7, 8, 9)  # Add more visit types if needed
visit_types_name <- c("pnc0", "pnc1", "pnc4")  # Add more visit types if needed

hyperbili_visit_list <- lapply(visit_types_num, function(visit_types_num) {
  hyperbili %>%
    filter(TYPE_VISIT == visit_types_num) %>%
    select(SITE, MOMID, PREGID, INFANTID, contains("M13"), contains("M14")) %>%
    rename_with(~paste0(., "_", visit_types_num), .cols = c(contains("M13"), contains("M14")))
  
})
names(hyperbili_visit_list) <- paste("hyperbili_", visit_types_name, sep = "")

hyperbili_visit_list <- c(hyperbili_visit_list, list(hyperbili_ld = hyperbili_ld))
hyperbili_wide <- hyperbili_visit_list %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID", "INFANTID")) %>% distinct() %>% 
  relocate(names(hyperbili_ld), .after = INFANTID) %>% 
  mutate(DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format = "%Y-%m-%d %H:%M"))


## Generating outcomes
# Timepoints: within 24 hours, 24 hours to 5 days, >5 days, EVER within the first week (or ten days).

# Criteria 1. TCB >=15 at any time (TBILIRUBIN_UMOLL_LBORRES @ IPC OR TCB_UMOLL_LBORRES @ PNC)
hyperbili_crit1 <- hyperbili_wide %>%
  select(SITE, MOMID, PREGID, INFANTID, DELIVERY_DATETIME, M11_AGE_AT_VISIT_HRS_6, M11_AGE_AT_VISIT_DAYS_6, M11_TBILIRUBIN_UMOLL_LBORRES_6, 
         contains("M14_AGE_AT_VISIT_HRS"), contains("M14_AGE_AT_VISIT_DAYS"), contains("M14_TCB_UMOLL_LBORRES")) %>% 
  ## generate variable for hyperbilirubin at each visit 
  mutate(INF_HYPERBILI_TCB15_IPC = case_when(M11_TBILIRUBIN_UMOLL_LBORRES_6 >15 ~ 1, TRUE ~ 0), 
         INF_HYPERBILI_TCB15_PNC0 = case_when(M14_TCB_UMOLL_LBORRES_7 >15 ~ 1, TRUE ~ 0), 
         INF_HYPERBILI_TCB15_PNC1 = case_when(M14_TCB_UMOLL_LBORRES_8 >15 ~ 1, TRUE ~ 0), 
         INF_HYPERBILI_TCB15_PNC4 = case_when(M14_TCB_UMOLL_LBORRES_9 >15 ~ 1, TRUE ~ 0) 
  ) %>% 
  ## calculate age at FIRST TCB diagnosis - days
  mutate(INF_HYPERBILI_TCB15_AGE_DAYS = case_when(INF_HYPERBILI_TCB15_IPC == 1 ~ as.numeric(M11_AGE_AT_VISIT_DAYS_6),
                                                  INF_HYPERBILI_TCB15_PNC0 == 1 ~ as.numeric(M14_AGE_AT_VISIT_DAYS_7),
                                                  INF_HYPERBILI_TCB15_PNC1 == 1 ~ as.numeric(M14_AGE_AT_VISIT_DAYS_8),
                                                  INF_HYPERBILI_TCB15_PNC4 == 1 ~ as.numeric(M14_AGE_AT_VISIT_DAYS_9),
                                                  TRUE ~ NA
  )) %>%
  ## calculate age at FIRST TCB diagnosis - hours
  mutate(INF_HYPERBILI_TCB15_AGE_HRS = case_when(INF_HYPERBILI_TCB15_IPC == 1 ~ as.numeric(M11_AGE_AT_VISIT_HRS_6),
                                                 INF_HYPERBILI_TCB15_PNC0 == 1 ~ as.numeric(M14_AGE_AT_VISIT_HRS_7),
                                                 INF_HYPERBILI_TCB15_PNC1 == 1 ~ as.numeric(M14_AGE_AT_VISIT_HRS_8),
                                                 INF_HYPERBILI_TCB15_PNC4 == 1 ~ as.numeric(M14_AGE_AT_VISIT_HRS_9),
                                                 TRUE ~ NA
  )) %>%
  ## generate variable for hyperbili at any time point
  mutate(INF_HYPERBILI_TCB15_ANY = case_when(INF_HYPERBILI_TCB15_IPC==1 | INF_HYPERBILI_TCB15_PNC0==1 | 
                                               INF_HYPERBILI_TCB15_PNC1 == 1 | INF_HYPERBILI_TCB15_PNC4 == 1 ~ 1, 
                                             TRUE ~ 0),
         INF_HYPERBILI_TCB15_24HR = case_when(INF_HYPERBILI_TCB15_ANY==1 &
                                                (INF_HYPERBILI_TCB15_AGE_DAYS == 0 & INF_HYPERBILI_TCB15_AGE_HRS >=0 & INF_HYPERBILI_TCB15_AGE_HRS <24) ~ 1, ## 0 days & 0-23 hours
                                              TRUE ~ 0),
         INF_HYPERBILI_TCB15_5DAY = case_when(INF_HYPERBILI_TCB15_ANY==1 &
                                                (INF_HYPERBILI_TCB15_AGE_DAYS >=1 & INF_HYPERBILI_TCB15_AGE_DAYS <5) ~ 1,
                                              TRUE ~ 0),
         INF_HYPERBILI_TCB15_14DAY = case_when(INF_HYPERBILI_TCB15_ANY==1 &
                                                 (INF_HYPERBILI_TCB15_AGE_DAYS >=5 & INF_HYPERBILI_TCB15_AGE_DAYS <14) ~ 1,
                                               TRUE ~ 0)) 

## 15 cutoff is mg/dl which is 256.5 umol/L 
# test_kenya <- hyperbili %>% filter(SITE == "Kenya")   ## umol/L --> mg/dl 
# test_pak <- hyperbili %>% filter(SITE == "Pakistan")  ## mg/dl 
# test_gha <- hyperbili %>% filter(SITE == "Ghana")     ## mg/dl 
# test_zam <- hyperbili %>% filter(SITE == "Zambia")    ## umol/L --> mg/dl  
# test_cmc <- hyperbili %>% filter(SITE == "India-CMC") ## mg/dl 
test_zam <- hyperbili %>% filter(SITE == "Zambia")      ## mg/dl 
summary(test_zam$M14_TCB_UMOLL_LBORRES)

# Criteria 2. TCB >AAP time-specific cutoff (serum bilirubin threshold minus 3 for each GA+age group)
hyperbili_crit2 <- hyperbili_wide %>%
  select(SITE, MOMID, PREGID, INFANTID, DELIVERY_DATETIME,GESTAGEBIRTH_ANY, GESTAGEBIRTH_ANY_DAYS, contains("M14")) %>% 
  ## generate day at visit and hours at visit variables (the tcb package requires this input)
  mutate(TCB_DAYS_PNC0 = as.numeric(M14_AGE_AT_VISIT_DATETIME_7) %/% 24,
         TCB_HRS_PNC0 = as.numeric(M14_AGE_AT_VISIT_DATETIME_7) %% 24,
         TCB_DAYS_PNC1 = as.numeric(M14_AGE_AT_VISIT_DATETIME_8) %/% 24,
         TCB_HRS_PNC1 = as.numeric(M14_AGE_AT_VISIT_DATETIME_8) %% 24,
         TCB_DAYS_PNC4 = as.numeric(M14_AGE_AT_VISIT_DATETIME_9) %/% 24,
         TCB_HRS_PNC4 = as.numeric(M14_AGE_AT_VISIT_DATETIME_9) %% 24
  ) %>% 
  # the TCB package will not run if there is missing among the input variables. here we make an indicator variable to condition on in the tcb code below
  mutate(MISSING_PNC0 = case_when((GESTAGEBIRTH_ANY <0 | is.na(GESTAGEBIRTH_ANY)) |
                                    (TCB_DAYS_PNC0 <0 | is.na(TCB_DAYS_PNC0)) | (TCB_HRS_PNC0 <0 | is.na(TCB_HRS_PNC0)) ~ 1, TRUE ~0),
         MISSING_PNC1 = case_when((GESTAGEBIRTH_ANY <0 | is.na(GESTAGEBIRTH_ANY)) |
                                    (TCB_DAYS_PNC1 <0 | is.na(TCB_DAYS_PNC1)) | (TCB_HRS_PNC1 <0 | is.na(TCB_HRS_PNC1)) ~ 1, TRUE ~0),
         MISSING_PNC4 = case_when((GESTAGEBIRTH_ANY <0 | is.na(GESTAGEBIRTH_ANY)) |
                                    (TCB_DAYS_PNC4 <0 | is.na(TCB_DAYS_PNC4)) | (TCB_HRS_PNC4 <0 | is.na(TCB_HRS_PNC4)) ~ 1, TRUE ~0),
  ) %>% 
  rowwise() %>% 
  # run through tcb package to generate thresholds (if no missing among input vars (MISSING_PNC ==0), then run through the package)
  mutate(TCB_AAP_THRESH_PNC0 = ifelse(MISSING_PNC0 ==0, TSB("P0",paste0(GESTAGEBIRTH_ANY, " weeks"),days = TCB_DAYS_PNC0,hours=TCB_HRS_PNC0)-3, NA),
         TCB_AAP_THRESH_PNC1 = ifelse(MISSING_PNC1 ==0, TSB("P0",paste0(GESTAGEBIRTH_ANY, " weeks"),days = TCB_DAYS_PNC1,hours=TCB_HRS_PNC1)-3, NA),
         TCB_AAP_THRESH_PNC4 = ifelse(MISSING_PNC4 ==0, TSB("P0",paste0(GESTAGEBIRTH_ANY, " weeks"),days = TCB_DAYS_PNC4,hours=TCB_HRS_PNC4)-3, NA)
  ) %>% 
  # create hyperbili AAP variable
  mutate(INF_HYPERBILI_AAP_PNC0 = case_when(M14_TCB_UMOLL_LBORRES_7>= TCB_AAP_THRESH_PNC0 ~ 1, TRUE ~ 0),
         INF_HYPERBILI_AAP_PNC1 = case_when(M14_TCB_UMOLL_LBORRES_8>= TCB_AAP_THRESH_PNC1 ~ 1, TRUE ~ 0),
         INF_HYPERBILI_AAP_PNC4 = case_when(M14_TCB_UMOLL_LBORRES_9>= TCB_AAP_THRESH_PNC4 ~ 1, TRUE ~ 0)
  ) %>% 
  # generate timepoint variables
  mutate(INF_HYPERBILI_AAP_ANY = case_when(INF_HYPERBILI_AAP_PNC0 ==1 | INF_HYPERBILI_AAP_PNC1 ==1 | 
                                             INF_HYPERBILI_AAP_PNC4 ==1 ~ 1, TRUE ~ 0)) %>% 
  ## calculate age at FIRST TCB diagnosis
  mutate(INF_HYPERBILI_AAP_AGE_DAYS = case_when(INF_HYPERBILI_AAP_PNC0 ==1 ~ as.numeric(TCB_DAYS_PNC0),
                                                INF_HYPERBILI_AAP_PNC1 ==1 ~ as.numeric(TCB_DAYS_PNC1),
                                                INF_HYPERBILI_AAP_PNC4 ==1 ~ as.numeric(TCB_DAYS_PNC4),
                                                TRUE ~ NA)) %>%
  ## calculate age at FIRST TCB diagnosis
  mutate(INF_HYPERBILI_AAP_AGE_HRS = case_when(INF_HYPERBILI_AAP_PNC0 ==1 ~ as.numeric(TCB_HRS_PNC0),
                                               INF_HYPERBILI_AAP_PNC1 ==1 ~ as.numeric(TCB_HRS_PNC1),
                                               INF_HYPERBILI_AAP_PNC4 ==1 ~ as.numeric(TCB_HRS_PNC4),
                                               TRUE ~ NA)) %>% 
  ## generate variable for hyperbili at any time point
  mutate(INF_HYPERBILI_AAP_ANY = case_when(INF_HYPERBILI_AAP_PNC0==1 | INF_HYPERBILI_AAP_PNC1 ==1 | 
                                             INF_HYPERBILI_AAP_PNC4 ==1 ~ 1,
                                           TRUE ~ 0),
         INF_HYPERBILI_AAP_24HR = case_when(INF_HYPERBILI_AAP_ANY ==1 &
                                              (INF_HYPERBILI_AAP_AGE_DAYS ==0 & INF_HYPERBILI_AAP_AGE_HRS >=0 & INF_HYPERBILI_AAP_AGE_HRS <24) ~ 1, ## 0 days & 0-23 hours
                                            TRUE ~ 0),
         INF_HYPERBILI_AAP_5DAY = case_when(INF_HYPERBILI_AAP_ANY==1 &
                                              (INF_HYPERBILI_AAP_AGE_DAYS >=1 & INF_HYPERBILI_AAP_AGE_DAYS <5) ~ 1,
                                            TRUE ~ 0),
         INF_HYPERBILI_AAP_14DAY = case_when(INF_HYPERBILI_AAP_ANY==1 &
                                               (INF_HYPERBILI_AAP_AGE_DAYS >=5 & INF_HYPERBILI_AAP_AGE_DAYS <14) ~ 1,
                                             TRUE ~ 0)
  )


# Criteria 3. By IMCI jaundice criteria (YELLOW_CEOCCUR, JAUND_CEOCCUR, JAUND_CESTDAT) -- WIDE 
hyperbili_crit3 <- hyperbili_wide %>%
  select(SITE, MOMID, PREGID, INFANTID,M11_AGE_AT_VISIT_HRS_6, M11_AGE_AT_VISIT_DAYS_6, contains("M13_AGE_AT_VISIT_DAYS"),DOB,contains("M13_VISIT_OBSSTDAT"),
         contains("JAUND"),contains("YELL")) %>% 
  
  # generate severity variable 
  # jaundice 
  mutate(INF_JAUN_IPC = case_when(M11_JAUND_CEOCCUR_6==1 | M11_YELLOW_CEOCCUR_6==1 ~ 1,TRUE ~ 0), ## if jaundice was observed OR yellow palms or feet were observed, then jaundice = yes
         INF_JAUN_PNC0 = case_when(M13_JAUND_CEOCCUR_7==1 | M13_YELL_CEOCCUR_7==1 ~ 1,TRUE ~ 0),  ## if jaundice was observed OR yellow palms or feet were observed, then jaundice = yes
         INF_JAUN_PNC1 = case_when(M13_JAUND_CEOCCUR_8==1 | M13_YELL_CEOCCUR_8==1 ~ 1,TRUE ~ 0),  ## if jaundice was observed OR yellow palms or feet were observed, then jaundice = yes
         INF_JAUN_PNC4 = case_when(M13_JAUND_CEOCCUR_9==1 | M13_YELL_CEOCCUR_9==1 ~ 1,TRUE ~ 0),  ## if jaundice was observed OR yellow palms or feet were observed, then jaundice = yes
         INF_JAUN_ANY = case_when(INF_JAUN_IPC==1 | INF_JAUN_PNC0==1 | INF_JAUN_PNC1==1 | INF_JAUN_PNC4==1 ~1, TRUE ~0)
  ) %>%
  
  # severe jaundice 
  mutate(INF_JAUN_SEV_IPC = case_when(INF_JAUN_IPC==1 & M11_YELLOW_CEOCCUR_6==1 ~ 1, TRUE ~ 0), ## if jaundice was observed AND (jaundice <24hrs or yellow palms) = severe jaundice
         INF_JAUN_SEV_PNC0 = case_when(INF_JAUN_PNC0==1 & M13_YELL_CEOCCUR_7==1 ~ 1, TRUE ~ 0), ## if jaundice was observed AND (jaundice <24hrs or yellow palms) = severe jaundice
         INF_JAUN_SEV_PNC1 = case_when(INF_JAUN_PNC1==1 & M13_YELL_CEOCCUR_8==1 ~ 1, TRUE ~ 0), ## if jaundice was observed AND (jaundice <24hrs or yellow palms) = severe jaundice
         INF_JAUN_SEV_PNC4 = case_when(INF_JAUN_PNC4==1 & M13_YELL_CEOCCUR_9==1 ~ 1, TRUE ~ 0), ## if jaundice was observed AND (jaundice <24hrs or yellow palms) = severe jaundice
         INF_JAUN_SEV_ANY = case_when(INF_JAUN_SEV_IPC == 1 | INF_JAUN_SEV_PNC0 == 1 | INF_JAUN_SEV_PNC1 == 1 | INF_JAUN_SEV_PNC4==1 ~ 1, TRUE ~ 0)
  ) %>% 
  
  # non-severe jaundice 
  mutate(INF_JAUN_NON_SEV_IPC = case_when(INF_JAUN_IPC==1 & INF_JAUN_SEV_IPC==0 ~ 1, TRUE ~ 0),    ## if jaundice was observed AND severe = 0, then non-severe jaundice
         INF_JAUN_NON_SEV_PNC0 = case_when(INF_JAUN_PNC0==1 & INF_JAUN_SEV_PNC0==0 ~ 1, TRUE ~ 0), ## if jaundice was observed AND severe = 0, then non-severe jaundice
         INF_JAUN_NON_SEV_PNC1 = case_when(INF_JAUN_PNC1==1 & INF_JAUN_SEV_PNC1==0 ~ 1, TRUE ~ 0), ## if jaundice was observed AND severe = 0, then non-severe jaundice
         INF_JAUN_NON_SEV_PNC4 = case_when(INF_JAUN_PNC4==1 & INF_JAUN_SEV_PNC4==0 ~ 1, TRUE ~ 0), ## if jaundice was observed AND severe = 0, then non-severe jaundice
         INF_JAUN_NON_SEV_ANY = case_when(INF_JAUN_NON_SEV_IPC == 1 | INF_JAUN_NON_SEV_PNC0 == 1 | INF_JAUN_NON_SEV_PNC1 == 1 | INF_JAUN_NON_SEV_PNC4==1 ~ 1, TRUE ~ 0)
  ) %>%
  
  ## generate timing variable for severe jaundice at each visit (check logic in Pakistan -- no jaundice, yes yellow soles)
  # logic: (timing of diagnosis is during the first 24hours AND jaundice is present) OR (Jaundice is present <24hrs AND has yellow palms or soles of foot)
  ## ADD 24 HOURS - 24 vs not 24 hours
  mutate(INF_JAUN_SEV_24HR = case_when((INF_JAUN_SEV_IPC==1 & (M11_JAUND_CESTDAT_6==1 | (M11_AGE_AT_VISIT_DAYS_6==0 & M11_AGE_AT_VISIT_HRS_6 <24))) |  ## if severe jaundice = 1 & timing <24hrs marked 
                                         (INF_JAUN_SEV_PNC0==1 & M13_JAUND_CESTTIM_7==1) | ## if severe jaundice = 1 & timing <24hrs marked 
                                         (INF_JAUN_SEV_PNC1==1 & M13_JAUND_CESTTIM_8==1) | ## if severe jaundice = 1 & timing <24hrs marked 
                                         (INF_JAUN_SEV_PNC4==1 & M13_JAUND_CESTTIM_9==1)  ~ 1, ## if severe jaundice = 1 & timing <24hrs marked 
                                       TRUE ~ 0)) %>% 
  
  ## ADD 24 HOURS - 24 vs not 24 hours
  mutate(INF_JAUN_SEV_GREATER_24HR = case_when((INF_JAUN_SEV_IPC==1 & (M11_JAUND_CESTDAT_6==2 | M11_AGE_AT_VISIT_DAYS_6 != 0))|        ## if severe jaundice = 1 & timing <24hrs NOT marked or age at visit is not 0
                                                 (INF_JAUN_SEV_PNC0==1 & (M13_JAUND_CESTTIM_7==2 | M13_AGE_AT_VISIT_DAYS_7 != 0)) |      ## if severe jaundice = 1 & timing <24hrs NOT marked or age at visit is not 0
                                                 (INF_JAUN_SEV_PNC1==1 & (M13_JAUND_CESTTIM_8==2 | M13_AGE_AT_VISIT_DAYS_8 != 0)) |      ## if severe jaundice = 1 & timing <24hrs NOT marked or age at visit is not 0
                                                 (INF_JAUN_SEV_PNC4==1 & (M13_JAUND_CESTTIM_9==2 | M13_AGE_AT_VISIT_DAYS_9 != 0))  ~ 1,  ## if severe jaundice = 1 & timing <24hrs NOT marked or age at visit is not 0
                                               TRUE ~ 0)) %>% 
  
  ## denominator 
  mutate(DENOM_JAUN  = case_when(((M11_JAUND_CEOCCUR_6 %in% c(1,0) | M11_YELLOW_CEOCCUR_6 %in% c(1,0)) & M11_AGE_AT_VISIT_DAYS_6 <14) |
                                   ((M13_JAUND_CEOCCUR_7 %in% c(1,0) | M13_YELL_CEOCCUR_7 %in% c(1,0)) &  M13_AGE_AT_VISIT_DAYS_7 <14) | 
                                   ((M13_JAUND_CEOCCUR_8 %in% c(1,0) | M13_YELL_CEOCCUR_8 %in% c(1,0)) &  M13_AGE_AT_VISIT_DAYS_8 <14) | 
                                   ((M13_JAUND_CEOCCUR_9 %in% c(1,0) | M13_YELL_CEOCCUR_9 %in% c(1,0)) &  M13_AGE_AT_VISIT_DAYS_9 <14) ~1, TRUE ~ 0)) 


##
table(hyperbili_crit3$INF_JAUN_NON_SEV_ANY, hyperbili_crit3$SITE)                                   
table(hyperbili_crit3$INF_JAUN_SEV_GREATER_24HR, hyperbili_crit3$SITE)   
table(hyperbili_crit3$INF_JAUN_SEV_24HR, hyperbili_crit3$SITE)   
table(hyperbili_crit3$INF_JAUN_SEV_ANY, hyperbili_crit3$SITE)   
table(hyperbili_crit3$INF_JAUN_ANY , hyperbili_crit3$SITE)                                   


## MERGE ALL CRITERIA TOGETHER INTO ONE DATASET
hyperbili_crit2_sub <- hyperbili_crit2 %>% select(SITE, MOMID, PREGID, INFANTID,DELIVERY_DATETIME, GESTAGEBIRTH_ANY, 
                                                  contains("AAP_THRESH"), contains("INF_HYPERBILI_AAP"))
hyperbili_crit3_sub <- hyperbili_crit3 %>% select(SITE, MOMID, PREGID, INFANTID,contains("M13_AGE_AT_VISIT_DAYS"), contains("JAUND_CEOCCUR"), contains("JAUND_CESTTIM"),
                                                  contains("YELLOW_CEOCCUR"), contains("YELL_CEOCCUR"),DENOM_JAUN, contains("JAUN"))


hyperbili_all_crit <- hyperbili_crit1 %>% 
  select(SITE, MOMID, PREGID, INFANTID,M11_TBILIRUBIN_UMOLL_LBORRES_6, contains("M14_TCB_UMOLL_LBORRES_"),
         M11_AGE_AT_VISIT_DAYS_6, M11_AGE_AT_VISIT_HRS_6, 
         M14_AGE_AT_VISIT_DAYS_7, M14_AGE_AT_VISIT_HRS_7, M14_AGE_AT_VISIT_DAYS_8, 
         M14_AGE_AT_VISIT_HRS_8, M14_AGE_AT_VISIT_DAYS_9, M14_AGE_AT_VISIT_HRS_9, contains("INF_HYPERBILI")) %>% 
  full_join(hyperbili_crit2_sub, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  full_join(hyperbili_crit3_sub, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  select(SITE, MOMID, PREGID, INFANTID, DELIVERY_DATETIME, GESTAGEBIRTH_ANY, contains("M14_TCB_UMOLL_LBORRES_"), 
         M11_TBILIRUBIN_UMOLL_LBORRES_6,M11_AGE_AT_VISIT_DAYS_6,M11_AGE_AT_VISIT_HRS_6, contains("M13_AGE_AT_VISIT_DAYS"), M14_AGE_AT_VISIT_DAYS_7, M14_AGE_AT_VISIT_HRS_7, 
         M14_AGE_AT_VISIT_DAYS_8, M14_AGE_AT_VISIT_HRS_8, M14_AGE_AT_VISIT_DAYS_9, M14_AGE_AT_VISIT_HRS_9,
         contains("INF_HYPERBILI"), contains("AAP_THRESH"), contains("HYPERBILI_AAP"), 
         contains("JAUND_CEOCCUR"), contains("JAUND_CESTTIM"),
         contains("YELLOW_CEOCCUR"), contains("YELL_CEOCCUR"), contains("JAUN"), DENOM_JAUN) %>%
  rowwise() %>% 
  # generate variable if a measurement is recorded 
  mutate(M11_TCB_REPORTED = ifelse(!M11_TBILIRUBIN_UMOLL_LBORRES_6 %in% c(-5,-7) | !is.na(M11_TBILIRUBIN_UMOLL_LBORRES_6),1, 0),
         M14_TCB_REPORTED_7 = ifelse(!M14_TCB_UMOLL_LBORRES_7 %in% c(-5,-7) | !is.na(M14_TCB_UMOLL_LBORRES_7),1, 0), 
         M14_TCB_REPORTED_8 = ifelse(!M14_TCB_UMOLL_LBORRES_8 %in% c(-5,-7) | !is.na(M14_TCB_UMOLL_LBORRES_8),1, 0),
         M14_TCB_REPORTED_9 = ifelse(!M14_TCB_UMOLL_LBORRES_9 %in% c(-5,-7) | !is.na(M14_TCB_UMOLL_LBORRES_9),1, 0)) %>% 
  ## generate denominators for 
  # step 1: pull the age last seen for bilirubin testing
  # mutate(AGE_LAST_SEEN_HYPERBILI = pmax(M11_AGE_AT_VISIT_DAYS_6, M14_AGE_AT_VISIT_DAYS_7, M14_AGE_AT_VISIT_DAYS_8, M14_AGE_AT_VISIT_DAYS_9, na.rm = TRUE)) %>% 
  # step 2: generate passed period variables 
  mutate(DENOM_HYPERBILI_24HR = case_when((M11_TCB_REPORTED == 1 | M14_TCB_REPORTED_7 == 1 | M14_TCB_REPORTED_8==1 | M14_TCB_REPORTED_9==1) & 
                                            ((M11_AGE_AT_VISIT_DAYS_6 ==0 & M11_AGE_AT_VISIT_HRS_6 < 24) |
                                               (M14_AGE_AT_VISIT_DAYS_7 ==0 &  M14_AGE_AT_VISIT_HRS_7 <24) | 
                                               (M14_AGE_AT_VISIT_DAYS_8 ==0 &  M14_AGE_AT_VISIT_HRS_8 <24) | 
                                               (M14_AGE_AT_VISIT_DAYS_9 ==0 &  M14_AGE_AT_VISIT_HRS_9 <24))  ~ 1,
                                          TRUE ~ 0),
         DENOM_HYPERBILI_5DAY = case_when((M11_TCB_REPORTED == 1 | M14_TCB_REPORTED_7 == 1 | M14_TCB_REPORTED_8==1 | M14_TCB_REPORTED_9==1) & 
                                            (M11_AGE_AT_VISIT_DAYS_6 >=1 & M11_AGE_AT_VISIT_DAYS_6 <5) |
                                            (M14_AGE_AT_VISIT_DAYS_7 >=1 & M14_AGE_AT_VISIT_DAYS_7 <5) | 
                                            (M14_AGE_AT_VISIT_DAYS_8 >=1 & M14_AGE_AT_VISIT_DAYS_8 <5) | 
                                            (M14_AGE_AT_VISIT_DAYS_9 >=1 & M14_AGE_AT_VISIT_DAYS_9 <5)  ~ 1,
                                          TRUE ~ 0),
         DENOM_HYPERBILI_14DAY = case_when((M11_TCB_REPORTED == 1 | M14_TCB_REPORTED_7 == 1 | M14_TCB_REPORTED_8==1 | M14_TCB_REPORTED_9==1) & 
                                             (M11_AGE_AT_VISIT_DAYS_6 >=5 & M11_AGE_AT_VISIT_DAYS_6 <14) |
                                             (M14_AGE_AT_VISIT_DAYS_7 >=5 & M14_AGE_AT_VISIT_DAYS_7 <14) | 
                                             (M14_AGE_AT_VISIT_DAYS_8 >=5 & M14_AGE_AT_VISIT_DAYS_8 <14) | 
                                             (M14_AGE_AT_VISIT_DAYS_9 >=5 & M14_AGE_AT_VISIT_DAYS_9 <14)  ~ 1,
                                           TRUE ~ 0),
         DENOM_HYPERBILI_ANY = case_when((M11_TCB_REPORTED == 1 | M14_TCB_REPORTED_7 == 1 | M14_TCB_REPORTED_8==1 | M14_TCB_REPORTED_9==1) & 
                                           (M11_AGE_AT_VISIT_DAYS_6 <14 |
                                              M14_AGE_AT_VISIT_DAYS_7 <14 | 
                                              M14_AGE_AT_VISIT_DAYS_8 <14 | 
                                              M14_AGE_AT_VISIT_DAYS_9 <14)  ~ 1,
                                         TRUE ~ 0)) 


# export data 
# write.csv(hyperbili_all_crit, paste0(path_to_save, "hyperbili_all_crit" ,".csv"), row.names=FALSE)


## INF_HYPERBILI_ANY: any hyperbilirubin defined by TCB >15 at any time (TBILIRUBIN_UMOLL_LBORRES @ IPC OR TCB_UMOLL_LBORRES @ PNC)
## INF_HYPERBILI_AAP_ANY: any hyperbilirubin defined by TCB >AAP time-specific cutoff (serum bili threshold minus 3 for each GA+age group)
## INF_JAUN_ANY: jaundice at any timepoint defined by IMCI jaundice criteria (YELLOW_CEOCCUR, JAUND_CEOCCUR, JAUND_CESTDAT)


#*****************************************************************************
#* 9. PSBI 
#* Presence of any clinical signs or symptoms as defined by the WHO IMCI criteria, 
#* which are consistent with possible severe bacterial infection from delivery to 59 days. 

# Forms needed: 
# MNH11 
# MNH13 

# Variables needed
#* Has the baby had difficulty in feeding (i.e., feeding poorly or not feeding at all)? [POOR_FEED_CEOCCUR, POOR_FEED_CEOCCUR_MR]
#* Has the baby had convulsions? [CONV_CEOCCUR, CONV_CEOCCUR_MR]
#* Has the baby had fast or difficult breathing? [BREATH_VSORRES_1, BREATH_VSORRES_2, BREATH_CEOCCUR_MR]
#* Does infant have severe chest in-drawing? [CHESTINDRAW_CEOCCUR, CHEST_CEOCCUR]
#* Has the baby had a fever? [TEMP_VSORRES_1, TEMP_VSORRES_2, FEVER_CEOCCUR_MR]
#* Record infant temperature at time of visit. Is the baby cold [TEMP_VSORRES, HYPO_CEOCCUR_MR]
#*****************************************************************************
## Forms required: mnh11_constructed, mnh13_constructed
## generate vector of livebirths 

mnh13_constructed <- inf_baseline %>% 
  select(SITE, INFANTID, MOMID, PREGID, BIRTH_OUTCOME, LIVEBIRTH, DOB, TIME_BIRTH, GESTAGEBIRTH_ANY, GESTAGEBIRTH_ANY_DAYS) %>%
  # merge in mnh11 data 
  full_join(mnh13, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  # # only want live births 
  filter(LIVEBIRTH==1) %>%
  # calculate age at visit in days 
  mutate(AGE_AT_VISIT = as.numeric(ymd(M13_VISIT_OBSSTDAT)-DOB)) %>% 
  # psbi is only for neonates ages 0-59 days; filter 
  filter(AGE_AT_VISIT >=0 & AGE_AT_VISIT <=59) %>%
  # rename type_visit variable 
  rename(TYPE_VISIT=M13_TYPE_VISIT) %>% 
  # rename visit date and birth outcome variable 
  rename(VISIT_DATE=M13_VISIT_OBSSTDAT) %>%
  mutate(VISIT_DATE = ymd(VISIT_DATE)) %>% 
  # replace default value with NA
  mutate(M13_BREATH_VSORRES_1 = case_when(SITE == "Ghana" & M13_BREATH_VSORRES_1==77 ~ NA, TRUE ~  M13_BREATH_VSORRES_1),
         M13_BREATH_VSORRES_2 = case_when(SITE == "Ghana" & M13_BREATH_VSORRES_2==77~ NA, TRUE ~  M13_BREATH_VSORRES_2),
         M13_TEMP_VSORRES_1 = case_when(SITE == "Ghana" & M13_TEMP_VSORRES_1==77~ NA, TRUE ~  M13_TEMP_VSORRES_1),
         M13_TEMP_VSORRES_2 = case_when(SITE == "Ghana" & M13_TEMP_VSORRES_2==77~ NA, TRUE ~   M13_TEMP_VSORRES_2),
  ) %>% 
  # pull psbi classification variables 
  select(SITE, MOMID, PREGID, INFANTID,TYPE_VISIT,VISIT_DATE, AGE_AT_VISIT, M13_POOR_FEED_CEOCCUR, M13_POOR_FEED_CEOCCUR_MR, 
         M13_CONV_CEOCCUR, M13_CONV_CEOCCUR_MR, M13_BREATH_VSORRES_1, M13_BREATH_VSORRES_2, 
         M13_BREATH_CEOCCUR_MR, M13_CHEST_CEOCCUR, M13_TEMP_VSORRES_1, M13_TEMP_VSORRES_2, M13_FEVER_CEOCCUR_MR, 
         M13_HYPO_CEOCCUR_MR, M13_BLD_CULT_LBPERF, M13_BLD_CULT_LBORRES)

table(mnh13$M13_BLD_CULT_LBPERF, mnh13$SITE) ## only have n=30 blood cultures in mnh13 (we only ask for this if a patient has been diagnosed with sepsis)

mnh20_constructed <- inf_baseline %>%
  # only want live births 
  filter(LIVEBIRTH ==1) %>% 
  select(SITE, INFANTID, MOMID, PREGID, DOB) %>%
  # merge in mnh20 data 
  right_join(mnh20, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  # calculate age at visit in days 
  mutate(AGE_AT_VISIT = as.numeric(ymd(M20_OBSSTDAT)-DOB)) %>% 
  # psbi is only for neonates ages 0-59 days; filter 
  filter(AGE_AT_VISIT >=0 & AGE_AT_VISIT <=59) %>% 
  # generate type_visit variable and set to 15 as a proxy for hospitalization
  mutate(TYPE_VISIT=15) %>% 
  # rename visit date variable
  rename(VISIT_DATE = M20_OBSSTDAT) %>% 
  mutate(VISIT_DATE = ymd(VISIT_DATE)) %>% 
  # pull psbi classification variables 
  select(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT, AGE_AT_VISIT,VISIT_DATE, M20_ADMIT_YN, M20_TEMP_VSORRES, M20_MAX_TEMP_VSORRES, M20_LOW_TEMP_VSORRES,
         M20_RR_VSORRES, M20_MAX_RR_VSORRES, M20_POOR_FEED_CEOCCUR, M20_CONV_CEOCCUR, M20_CHEST_CEOCCUR, 
         M20_FEVER_CEOCCUR, M20_HYPO_CEOCCUR,M20_BLD_CULT_LBPERF, M20_BLD_CULT_LBORRES)


table(mnh20$M20_BLD_CULT_LBPERF, mnh20$SITE) ## only have n=20 blood cultures in hospitalization

## only want live births for mnh11 
# mnh11_constructed_livebirths <- mnh11_constructed %>% filter(INFANTID %in% livebirths) 
# table(mnh11$M11_CULTURE_LBPERF, mnh11$SITE) 

psbi_long <-  inf_baseline %>%
  # only want live births 
  # filter(LIVEBIRTH ==1) %>% 
  select(SITE, INFANTID, MOMID, PREGID, DOB, BIRTH_OUTCOME, LIVEBIRTH) %>%
  # merge in mnh20 data 
  full_join(mnh11_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  # rename visit date variable
  rename(VISIT_DATE = M11_VISIT_OBSSTDAT) %>% 
  mutate(VISIT_DATE = ymd(VISIT_DATE)) %>% 
  # add column for ipc type_visit (will use this to merge/make data wide with PNC forms)
  mutate(TYPE_VISIT = 6) %>% 
  full_join(mnh13_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID", "TYPE_VISIT", "VISIT_DATE")) %>% 
  # merge in infant hospitalization form 
  full_join(mnh20_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID", "TYPE_VISIT","VISIT_DATE", "AGE_AT_VISIT")) %>%
  # generate visit sequence variable to make merging in unscheduled visits more streamlines 
  group_by(SITE, MOMID, PREGID, INFANTID) %>% 
  arrange(VISIT_DATE) %>% 
  mutate(VISIT_SEQ = row_number()) %>% 
  select(SITE, MOMID, PREGID, INFANTID,DOB, BIRTH_OUTCOME, LIVEBIRTH, TYPE_VISIT, VISIT_SEQ,VISIT_DATE, AGE_AT_VISIT, 
         M11_POOR_FEED_CEOCCUR, M11_POOR_FEED_CEOCCUR_MR, 
         M11_CONV_CEOCCUR, M11_CONV_CEOCCUR_MR, M11_BREATH_VSORRES_1, M11_BREATH_VSORRES_2, 
         M11_BREATH_CEOCCUR_MR, M11_CHESTINDRAW_CEOCCUR, M11_TEMP_VSORRES, M11_TEMP_VSORRES_2, M11_FEVER_CEOCCUR_MR, 
         M11_HYPO_CEOCCUR_MR,
         M13_POOR_FEED_CEOCCUR, M13_POOR_FEED_CEOCCUR_MR, 
         M13_CONV_CEOCCUR, M13_CONV_CEOCCUR_MR, M13_BREATH_VSORRES_1, M13_BREATH_VSORRES_2, 
         M13_BREATH_CEOCCUR_MR, M13_CHEST_CEOCCUR, M13_TEMP_VSORRES_1, M13_TEMP_VSORRES_2, M13_FEVER_CEOCCUR_MR, 
         M13_HYPO_CEOCCUR_MR, contains("M20")) 

## PSBI at Hospitalization
psbi_hos <- psbi_long %>% 
  filter(TYPE_VISIT == 15) %>% 
  group_by(SITE, MOMID, PREGID, INFANTID) %>% 
  mutate(INF_PSBI = case_when(M20_POOR_FEED_CEOCCUR == 1 |                                 # Has the baby had difficulty in feeding
                                M20_CONV_CEOCCUR == 1 |                                      # Has the baby had convulsions? 
                                (M20_RR_VSORRES >= 60 | M20_MAX_RR_VSORRES >=60) |            # Has the baby had fast or difficult breathing? (60 breaths per minute or more)
                                M20_CHEST_CEOCCUR==1 |                                       # Does infant have severe chest in-drawing?
                                (M20_TEMP_VSORRES >= 38 | M20_MAX_TEMP_VSORRES >= 38) |       # Has the baby had a fever? (38C or higher)
                                ((M20_TEMP_VSORRES > 0 & M20_TEMP_VSORRES < 35.5) |          # Has the baby had low body temperature (<35.5C)
                                   (M20_LOW_TEMP_VSORRES > 0 & M20_LOW_TEMP_VSORRES < 35.5)) ~ 1, 
                              TRUE ~ 0)) 
# select(SITE, MOMID, PREGID, INFANTID, INF_PSBI, contains("M20"))


## PSBI at IPC
psbi_ipc <- psbi_long %>% 
  filter(TYPE_VISIT == 6) %>% 
  group_by(SITE, MOMID, PREGID, INFANTID, VISIT_SEQ) %>% 
  mutate(INF_PSBI = case_when(M11_POOR_FEED_CEOCCUR == 1 |                                 # Has the baby had difficulty in feeding
                                M11_CONV_CEOCCUR == 1 |                                      # Has the baby had convulsions? 
                                (M11_BREATH_VSORRES_1 >= 60 | M11_BREATH_VSORRES_2 >=60) |    # Has the baby had fast or difficult breathing? (60 breaths per minute or more)
                                M11_CHESTINDRAW_CEOCCUR==1 |                                 # Does infant have severe chest in-drawing?
                                (M11_TEMP_VSORRES >= 38 | M11_TEMP_VSORRES_2 >= 38) |         # Has the baby had a fever? (38C or higher)
                                ((M11_TEMP_VSORRES > 0 & M11_TEMP_VSORRES < 35.5) |           # Has the baby had low body temperature (<35.5C)
                                   (M11_TEMP_VSORRES_2 > 0 & M11_TEMP_VSORRES_2 < 35.5)) ~ 1, 
                              TRUE ~ 0)) 
# select(SITE, MOMID, PREGID,INFANTID, TYPE_VISIT, VISIT_SEQ, INF_PSBI, M11_POOR_FEED_CEOCCUR, M11_CONV_CEOCCUR, M11_BREATH_VSORRES_1, M11_BREATH_VSORRES_2,
#        M11_CHESTINDRAW_CEOCCUR, M11_TEMP_VSORRES, M11_TEMP_VSORRES_2)


## PSBI at PNC (includes unscheduled visits)
psbi_pnc <- psbi_long %>% 
  filter(!TYPE_VISIT %in% c(6,15)) %>% 
  group_by(SITE, MOMID, PREGID, INFANTID, VISIT_SEQ) %>% 
  mutate(INF_PSBI = case_when(M13_POOR_FEED_CEOCCUR == 1 |                                 # Has the baby had difficulty in feeding
                                M13_CONV_CEOCCUR == 1 |                                      # Has the baby had convulsions? 
                                (M13_BREATH_VSORRES_1 >= 60 | M13_BREATH_VSORRES_2 >= 60) |   # Has the baby had fast or difficult breathing? (60 breaths per minute or more)
                                M13_CHEST_CEOCCUR == 1 |                                     # Does infant have severe chest in-drawing?
                                (M13_TEMP_VSORRES_1 >= 38 | M13_TEMP_VSORRES_2 >= 38) |       # Has the baby had a fever? (38C or higher)
                                ((M13_TEMP_VSORRES_1 > 0 & M13_TEMP_VSORRES_1 < 35.5) |        # Has the baby had low body temperature (<35.5C)
                                   (M13_TEMP_VSORRES_2 > 0 & M13_TEMP_VSORRES_2 < 35.5)) ~ 1, 
                              TRUE ~ 0
                              
  ))  
# select(SITE, MOMID, PREGID,INFANTID, TYPE_VISIT, VISIT_SEQ, INF_PSBI, M13_POOR_FEED_CEOCCUR, M13_CONV_CEOCCUR, M13_BREATH_VSORRES_1, M13_BREATH_VSORRES_2, 
#        M13_CHEST_CEOCCUR, M13_TEMP_VSORRES_1, M13_TEMP_VSORRES_2)


psbi <- bind_rows(psbi_ipc, psbi_pnc, psbi_hos)

psbi_outcome <- psbi %>% 
  # generate variables for psbi by visit
  mutate(INF_PSBI_IPC = case_when(TYPE_VISIT==6 & INF_PSBI==1 ~ 1, TRUE ~ 0), 
         INF_PSBI_PNC0 = case_when(TYPE_VISIT==7 & INF_PSBI==1 ~ 1, TRUE ~ 0),
         INF_PSBI_PNC1 = case_when(TYPE_VISIT==8 & INF_PSBI==1 ~ 1, TRUE ~ 0),
         INF_PSBI_PNC4 = case_when(TYPE_VISIT==9 & INF_PSBI==1 ~ 1, TRUE ~ 0),
         INF_PSBI_PNC6 = case_when(TYPE_VISIT==10 & INF_PSBI==1 ~ 1, TRUE ~ 0),
         INF_PSBI_UNSCHED = case_when(TYPE_VISIT==14 & INF_PSBI==1 ~ 1, TRUE ~ 0),
         INF_PSBI_HOSPITAL = case_when(TYPE_VISIT==15 & INF_PSBI==1 ~ 1, TRUE ~ 0)
  ) %>% 
  # generate denominators (denominator is all livebirths aged 0-59 days) 
  group_by(SITE, MOMID, PREGID, INFANTID) %>% 
  mutate(INF_PSBI_DENOM = case_when(BIRTH_OUTCOME==1 ~ 1, TRUE ~ 0)) %>% 
  # in the event an infant has multiple psbi diagnoses, we want to generate a single variable for diagnosis at ANY ONE time point to avoid overcounting
  mutate(INF_PSBI_ANY = if_else(INF_PSBI == 1 & cumsum(INF_PSBI == 1) == 1, 1, 0)) %>% 
  ungroup() %>% 
  group_by(SITE, MOMID, PREGID, INFANTID) %>% 
  mutate(CHECK = n()) %>% 
  # generate missingness variables for confirmatory RR and temperature measures 
  mutate(MISSING_TEMP_PNC_HIGH = case_when(M13_TEMP_VSORRES_1 >=38 & M13_TEMP_VSORRES_2 < 0 ~ 1, TRUE ~ 0), 
         MISSING_TEMP_PNC_LOW = case_when((M13_TEMP_VSORRES_1 > 0 & M13_TEMP_VSORRES_1 < 35.5) & M13_TEMP_VSORRES_2 < 0 ~ 1, TRUE ~ 0),
         MISSING_TEMP_PNC = case_when(MISSING_TEMP_PNC_HIGH==1 | MISSING_TEMP_PNC_LOW==1 ~ 1, TRUE ~ 0)) %>% 
  ## remove instances of Ghana == 77 for rr
  mutate(keep_rr = case_when(SITE == "Ghana" & (M13_BREATH_VSORRES_2 == 77 | M13_BREATH_VSORRES_1 == 77) ~ 0, TRUE ~ 1),
         keep_temp = case_when(SITE == "Ghana" & (M13_TEMP_VSORRES_1 == 77 | M13_TEMP_VSORRES_2 == 77) ~ 0, TRUE ~ 1),
  )  %>% 
  filter(keep_rr == 1 & keep_temp == 1)

psbi_outcome_wide <- psbi_outcome %>% 
  group_by(SITE, MOMID, PREGID, INFANTID) %>% 
  # Summarize to ensure any 1 is captured within the group
  summarise(
    INF_PSBI_IPC = max(INF_PSBI_IPC, na.rm = TRUE),
    INF_PSBI_PNC0 = max(INF_PSBI_PNC0, na.rm = TRUE),
    INF_PSBI_PNC1 = max(INF_PSBI_PNC1, na.rm = TRUE),
    INF_PSBI_PNC4 = max(INF_PSBI_PNC4, na.rm = TRUE),
    INF_PSBI_PNC6 = max(INF_PSBI_PNC6, na.rm = TRUE),
    INF_PSBI_UNSCHED = max(INF_PSBI_UNSCHED, na.rm = TRUE),
    INF_PSBI_HOSPITAL = max(INF_PSBI_HOSPITAL, na.rm = TRUE),
    INF_PSBI_DENOM = max(INF_PSBI_DENOM, na.rm = TRUE)
  ) %>%
  # Ungroup after summarizing
  ungroup() %>% 
  ## merge onto inf_baseline dataset
  right_join(inf_baseline %>% select(SITE,MOMID, PREGID, INFANTID), by = c("SITE","MOMID", "PREGID", "INFANTID"))


# export data (long data will need to be called in separately)
# write.csv(psbi_outcome, paste0(path_to_save, "INF_PSBI_LONG" ,".csv"), row.names=FALSE)


### DATA CHECKS BELOW: 

## check scenarios where just temperature is the inclusion factor 
# test <- psbi_outcome %>% filter(INF_PSBI_PNC0==1 | INF_PSBI_PNC1==1 | INF_PSBI_PNC4==1 | INF_PSBI_PNC6 ==1) %>% 
#   mutate(PNC_FEED = case_when(M13_POOR_FEED_CEOCCUR==1 ~ 1,TRUE ~ 0),
#          PNC_CONV = case_when(M13_CONV_CEOCCUR==1 ~ 1,TRUE ~ 0),
#          PNC_BREATH = case_when((M13_BREATH_VSORRES_1 >= 60 | M13_BREATH_VSORRES_2 >= 60)~ 1,TRUE ~ 0),
#          PNC_CHEST = case_when(M13_CHEST_CEOCCUR==1 ~ 1,TRUE ~ 0),
#          PNC_TEMP = case_when((M13_TEMP_VSORRES_1 >= 38 | M13_TEMP_VSORRES_2 >= 38)~ 1,TRUE ~ 0),
#          PNC_HYPO = case_when(((M13_TEMP_VSORRES_1 > 0 & M13_TEMP_VSORRES_1 < 35.5) |       # Has the baby had low body temperature (<35.5C)
#                                  (M13_TEMP_VSORRES_2 > 0 & M13_TEMP_VSORRES_2 < 35.5)) ~ 1, TRUE ~ 0)
#          ) %>% 
#   rowwise() %>% 
#   mutate(TOTAL = sum(PNC_FEED, PNC_CONV,PNC_BREATH,  PNC_CHEST, PNC_TEMP,  PNC_HYPO, na.rm = TRUE)) %>% 
#   select(SITE, MOMID, PREGID, INFANTID,INF_PSBI, PNC_FEED, PNC_CONV,PNC_BREATH,  PNC_CHEST, PNC_TEMP,  PNC_HYPO,
#          TOTAL, contains("M13"))
#   
# table(test$TOTAL)

# ## among those with PSBI, how many are included based off of high temperature ALONE (n = 149)
#   ## among these, how many would be confirmed with a secondary confirmatory measurement (n=79 confirmed, n=70 unconfirmed)
# out <- test %>% filter(TOTAL == 1 & PNC_TEMP == 1) %>% 
#   mutate(CONFIRM = case_when(M13_TEMP_VSORRES_1 >= 38 & M13_TEMP_VSORRES_2 >= 38 ~ 1, TRUE ~ 0))
# 
# ## among those with PSBI, how many are included based off of hypthermina ALONE (n = 377)
# ## among these, how many would be confirmed with a secondary confirmatory measurement (n=18 confirmed, n=359 unconfirmed)
# out <- test %>% filter(TOTAL == 1 & PNC_HYPO == 1) %>% 
#   mutate(CONFIRM = case_when(((M13_TEMP_VSORRES_1 > 0 & M13_TEMP_VSORRES_1 < 35.5) &      # Has the baby had low body temperature (<35.5C)
#                                 (M13_TEMP_VSORRES_2 > 0 & M13_TEMP_VSORRES_2 < 35.5)) ~ 1, TRUE ~ 0))
# 
# 
# ## among those with PSBI, how many are included based off of RR ALONE (n = 195)
# ## among these, how many would be confirmed with a secondary confirmatory measurement (n=150 confirmed, n=45 unconfirmed)
# out <- test %>% filter(TOTAL == 1 & PNC_BREATH == 1) %>% 
#   mutate(CONFIRM = case_when(M13_BREATH_VSORRES_1 >= 60 & M13_BREATH_VSORRES_2 >= 60 ~ 1, TRUE ~ 0))


# 
# 
# ## hospitalization review (if a baby has psbi, they should also go to the hospital)
# test <- psbi_outcome %>% filter(SITE != "Ghana" & INF_PSBI==1) %>% 
#   group_by(SITE, MOMID, PREGID, INFANTID) %>% 
#   summarise(
#     HOSP = as.integer(any(TYPE_VISIT == 15)),  # Use any() to check if any TYPE_VISIT is 15
#     .groups = "drop"  # Ungroup after summarizing
#   ) ## only 41 infants had a hospitalization visit 
# table(test$HOSP)
#   

# ## instances where temp1 was reported <35.5 and no second measurement is reported  (n = 14)
# cold_discrepancies_m11 <- psbi_outcome %>% select(SITE, MOMID, PREGID, INFANTID, INF_PSBI_IPC, M11_POOR_FEED_CEOCCUR,
#                                 M11_CONV_CEOCCUR,M11_BREATH_VSORRES_1, M11_BREATH_VSORRES_2, 
#                                 M11_CHESTINDRAW_CEOCCUR, M11_TEMP_VSORRES, M11_TEMP_VSORRES_2) %>% 
#   filter(M11_TEMP_VSORRES < 35.5) %>% 
#   mutate(discrep = case_when((M11_TEMP_VSORRES > 0 & M11_TEMP_VSORRES< 35.5 ) &
#                                M11_TEMP_VSORRES_2 <0 ~ 1, TRUE ~ 0)) %>% 
#   filter(discrep == 1)
# table(cold_discrepancies_m11$SITE)
# 
# 
# cold_discrepancies_m13 <- psbi_outcome %>% select(SITE, MOMID, PREGID, INFANTID, INF_PSBI, M13_POOR_FEED_CEOCCUR,
#                                                   M13_CONV_CEOCCUR,M13_BREATH_VSORRES_1, M13_BREATH_VSORRES_2, 
#                                                   M13_TEMP_VSORRES_1, M13_TEMP_VSORRES_2) %>% 
#   filter(M13_TEMP_VSORRES_1 < 35.5) %>% 
#   mutate(discrep = case_when((M13_TEMP_VSORRES_1 > 0 & M13_TEMP_VSORRES_1< 35.5 ) &
#                                M13_TEMP_VSORRES_2 <0 ~ 1, TRUE ~ 0)) %>% 
#   filter(discrep == 1)
# table(cold_discrepancies_m13$SITE)
# 
# 
# cold_discrepancies_hos  <- psbi_outcome %>% select(SITE, MOMID, PREGID, INFANTID, INF_PSBI, M20_TEMP_VSORRES,
#                                                   M20_MAX_TEMP_VSORRES, M20_LOW_TEMP_VSORRES) %>% 
#   filter(M20_TEMP_VSORRES < 35.5) %>% 
#   mutate(discrep = case_when((M20_TEMP_VSORRES > 0 & M20_TEMP_VSORRES < 35.5 ) &
#                                M20_LOW_TEMP_VSORRES <0 ~ 1, TRUE ~ 0)) %>% 
#   filter(discrep == 1)
# table(cold_discrepancies_hos$SITE)
# 
# 
# 
# ## instances where temp1 was reported >= 38 and no second measurement is reported 
# fever_discrepancies_m11 <- psbi_outcome %>% select(SITE, MOMID, PREGID, INFANTID, INF_PSBI_IPC, M11_POOR_FEED_CEOCCUR,
#                                                   M11_CONV_CEOCCUR,M11_BREATH_VSORRES_1, M11_BREATH_VSORRES_2, 
#                                                   M11_CHESTINDRAW_CEOCCUR, M11_TEMP_VSORRES, M11_TEMP_VSORRES_2) %>% 
#   filter(M11_TEMP_VSORRES >= 38) %>% 
#   mutate(discrep = case_when(M11_TEMP_VSORRES>=38 & M11_TEMP_VSORRES_2 <0 ~ 1, TRUE ~ 0)) %>% 
#   filter(discrep == 1)
# table(fever_discrepancies_m11$SITE)
# 
# fever_discrepancies_m13 <- psbi_outcome %>% select(SITE, MOMID, PREGID, INFANTID, INF_PSBI, M13_POOR_FEED_CEOCCUR,
#                                                   M13_CONV_CEOCCUR,M13_BREATH_VSORRES_1, M13_BREATH_VSORRES_2, 
#                                                   M13_TEMP_VSORRES_1, M13_TEMP_VSORRES_2) %>% 
#   filter(M13_TEMP_VSORRES_1 >=  38) %>% 
#   mutate(discrep = case_when(M13_TEMP_VSORRES_1>=38 & M13_TEMP_VSORRES_2 <0 ~ 1, TRUE ~ 0)) %>% 
#   filter(discrep == 1)
# table(fever_discrepancies_m13$SITE)
# 
# fever_discrepancies_hos  <- psbi_outcome %>% select(SITE, MOMID, PREGID, INFANTID, INF_PSBI, M20_TEMP_VSORRES,
#                                                    M20_MAX_TEMP_VSORRES, M20_LOW_TEMP_VSORRES) %>% 
#   filter(M20_TEMP_VSORRES >=  38) %>% 
#   mutate(discrep = case_when(M20_TEMP_VSORRES>=38 & M20_MAX_TEMP_VSORRES <0 ~ 1, TRUE ~ 0)) %>% 
#   filter(discrep == 1)
# table(fever_discrepancies_hos$SITE)
# 
# 
# 
# ## instances where rr1 was reported >=60 and no second measurement is reported  (n = 14)
# rr_discrepancies_m11 <- psbi_outcome %>% select(SITE, MOMID, PREGID, INFANTID, INF_PSBI_IPC, M11_POOR_FEED_CEOCCUR,
#                                                   M11_CONV_CEOCCUR,M11_BREATH_VSORRES_1, M11_BREATH_VSORRES_2, 
#                                                   M11_CHESTINDRAW_CEOCCUR, M11_TEMP_VSORRES, M11_TEMP_VSORRES_2) %>% 
#   filter(M11_BREATH_VSORRES_1 >=60) %>% 
#   mutate(discrep = case_when(M11_BREATH_VSORRES_1>=60 & M11_BREATH_VSORRES_2 <0 ~ 1, TRUE ~ 0)) %>% 
#   filter(discrep == 1)
# table(rr_discrepancies_m11$SITE)
# 
# rr_discrepancies_m13 <- psbi_outcome %>% select(SITE, MOMID, PREGID, INFANTID, INF_PSBI, M13_POOR_FEED_CEOCCUR,
#                                                   M13_CONV_CEOCCUR,M13_BREATH_VSORRES_1, M13_BREATH_VSORRES_2, 
#                                                   M13_TEMP_VSORRES_1, M13_TEMP_VSORRES_2) %>% 
#   filter(M13_BREATH_VSORRES_1 >=60) %>% 
#   mutate(discrep = case_when(M13_BREATH_VSORRES_1>=60 & M13_BREATH_VSORRES_2 <0 ~ 1, TRUE ~ 0)) %>% 
#   filter(discrep == 1)
# table(rr_discrepancies_m13$SITE)
# 
# rr_discrepancies_hos <- psbi_outcome %>% select(SITE, MOMID, PREGID, INFANTID, INF_PSBI, M20_TEMP_VSORRES,
#                                                    M20_MAX_TEMP_VSORRES, M20_LOW_TEMP_VSORRES, M20_RR_VSORRES, M20_MAX_RR_VSORRES) %>% 
#   filter(M20_RR_VSORRES >=60) %>% 
#   mutate(discrep = case_when(M20_RR_VSORRES>=60 & M20_MAX_RR_VSORRES <0 ~ 1, TRUE ~ 0)) %>% 
#   filter(discrep == 1)
# table(rr_discrepancies_hos$SITE)


#*****************************************************************************
#* MERGE ALL OUTCOMES TOGETHER TO FORM AN OUTCOME DATASET 
#* outcomes included: 
# 1. Low birth-weight 
# 2. Pre-term birth
# 3. Size for Gestational Age (SGA)
# 4. Neonatal Mortality
# 5. Infant mortality 
# 6. Stillbirth
# 7. Fetal death
# 8. Birth Asphyxia
# 9. Hyperbili
# 10. PSBI 
#*****************************************************************************
infant_outcomes <- inf_baseline %>% 
  full_join(lowbirthweight[c("SITE", "INFANTID", "MOMID","PREGID",
                             "BWEIGHT_PRISMA", "BWEIGHT_ANY", "LBW2500_PRISMA", "LBW1500_PRISMA",
                             "LBW2500_ANY", "LBW1500_ANY","LBW_CAT_PRISMA", "LBW_CAT_ANY","M11_BW_FAORRES_REPORT","BW_TIME",
                             "LBW_PRISMA_DENOM", "LBW_ANY_DENOM", "MISSING_TIME", "MISSING_PRISMA", "MISSING_FACILITY", "MISSING_BOTH")],
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>%
  
  full_join(preterm_birth[c("SITE", "INFANTID", "MOMID", "PREGID",
                            "PRETERMBIRTH_LT37", "PRETERMBIRTH_LT34", "PRETERMBIRTH_LT32", "PRETERMBIRTH_LT28", "PRETERMBIRTH_CAT",
                            "PRETERMDELIV_LT37", "PRETERMDELIV_LT34", "PRETERMDELIV_LT32", "PRETERMDELIV_LT28", "PRETERMDELIV_CAT")],
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>%
  
  full_join(sga[c("SITE", "INFANTID", "MOMID", "PREGID",
                  "INF_SGA_PRETERM", "INF_AGA_PRETERM", "INF_SGA_TERM", "INF_AGA_TERM",
                  "SGA_CENTILE", "SGA_CAT")], by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>%
  
  full_join(mortality[c("SITE", "INFANTID", "MOMID", "PREGID",
                        "MISSING_MNH09", "MISSING_MNH11", "DTH_TIME_MISSING", "DOB_AFTER_DEATH",
                        "DTH_0D", "DTH_7D", "DTH_28D", "DTH_365D", "DENOM_28d", "DENOM_365d", "AGE_LAST_SEEN")],
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>%
  
  full_join(neonatal_mortality[c("SITE", "INFANTID", "MOMID", "PREGID",
                                 "NEO_DTH_24HR", "NEO_DTH_EAR", "NEO_DTH_LATE", "NEO_DTH_CAT", "NEO_DTH")],
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>%
  
  full_join(infant_mortality[c("SITE", "INFANTID", "MOMID", "PREGID", "INF_DTH", "INF_DTH_FROM28")],
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>%
  
  full_join(stillbirth[c("SITE", "INFANTID", "MOMID", "PREGID", 
                         "MISSING_SIGNS_OF_LIFE", "STILLBIRTH_SIGNS_LIFE","FETAL_LOSS_DATE", 
                         "STILLBIRTH_20WK", "STILLBIRTH_22WK","STILLBIRTH_24WK", "STILLBIRTH_28WK", 
                         "STILLBIRTH_TIMING", "STILLBIRTH_GESTAGE_CAT", "STILLBIRTH_DENOM")], 
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>% 
  
  full_join(fetal_death[c("SITE", "INFANTID", "MOMID", "PREGID", 
                          "INF_ABOR_SPN", "INF_ABOR_IND", "INF_FETAL_DTH",
                          "INF_FETAL_DTH_UNGA", "INF_FETAL_DTH_DENOM", "INF_FETAL_DTH_OTHR_DENOM")], 
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>% 
  
  full_join(birth_asphyxia[c("SITE", "INFANTID", "MOMID", "PREGID", "INF_ASPH",
                             "M11_BREATH_FAIL_CEOCCUR", "M11_INF_PROCCUR_2", "M11_INF_PROCCUR_3", "M11_INF_PROCCUR_4", "M11_INF_PROCCUR_5", "M11_INF_PROCCUR_6")],
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>% 
  
  full_join(hyperbili_all_crit[c("SITE", "INFANTID", "MOMID", "PREGID",
                                 "INF_HYPERBILI_TCB15_24HR", "INF_HYPERBILI_TCB15_5DAY", "INF_HYPERBILI_TCB15_14DAY",
                                 "INF_HYPERBILI_AAP_24HR", "INF_HYPERBILI_AAP_5DAY", "INF_HYPERBILI_AAP_14DAY",
                                 "INF_JAUN_NON_SEV_ANY", "INF_JAUN_SEV_24HR", "INF_JAUN_SEV_GREATER_24HR", "DENOM_HYPERBILI_ANY",
                                 "DENOM_HYPERBILI_24HR", "DENOM_HYPERBILI_5DAY", "DENOM_HYPERBILI_14DAY", "DENOM_JAUN")],
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>% 
  full_join(psbi_outcome_wide[c("SITE", "INFANTID", "MOMID", "PREGID",
                                "INF_PSBI_IPC", "INF_PSBI_PNC0",
                                "INF_PSBI_PNC1", "INF_PSBI_PNC4", "INF_PSBI_PNC6",
                                "INF_PSBI_UNSCHED", "INF_PSBI_HOSPITAL", "INF_PSBI_DENOM")],
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) 


## data cleaning - recode missing 
infant_outcomes<- infant_outcomes %>% 
  mutate(STILLBIRTH_20WK = ifelse(STILLBIRTH_20WK==99 | (STILLBIRTH_20WK==55 & GESTAGEBIRTH_ANY<20), 66, STILLBIRTH_20WK),
         # STILLBIRTH_TIMING = ifelse(STILLBIRTH_TIMING==99, 66, STILLBIRTH_TIMING),
         INF_ASPH = ifelse(INF_ASPH==66,55, INF_ASPH)) %>% 
  # generate any birth outcome variable 
  mutate(BIRTH_OUTCOME_REPORTED = case_when((LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & INF_ABOR_IND==0 & INF_ABOR_SPN==0~ 1, TRUE ~ 0)) %>% 
  # update fetal loss variable to include all identified stillbirths, miscarriages, and induced abortions 
  mutate(FETAL_LOSS = case_when(STILLBIRTH_20WK==1 | INF_ABOR_SPN==1 | INF_ABOR_IND==1 ~ 1, TRUE ~ 0)) %>% 
  # update dob variable to only include dates for livebirths and stillbirths 
  mutate(DOB = case_when(INF_ABOR_IND==1 | INF_ABOR_SPN==1 ~ NA, TRUE ~ ymd(DOB))) %>% 
  # update dth_indicator variable (replace NA with 0)
  mutate(DTH_INDICATOR = case_when(DTH_INDICATOR==1 ~ 1, TRUE ~ 0)) %>% 
  
  select(SITE, MOMID, PREGID, INFANTID, ENROLL_US_DATE,BOE_METHOD, GA_DIFF_DAYS, EDD_BOE, EST_CONCEP_DATE,
         LIVEBIRTH, FETAL_LOSS, BIRTH_OUTCOME_REPORTED,ADJUD_NEEDED, FETAL_LOSS_DATE, FETAL_LOSS_DATE, DOB, 
         DELIVERY_DATETIME,GESTAGEBIRTH_ANY,GESTAGEBIRTH_ANY_DAYS, GESTAGE_FETAL_LOSS_WKS, 
         CLOSEOUT, DTH_INDICATOR,AGE_LAST_SEEN, DEATHDATE_MNH24, DEATHTIME_MNH24, DEATH_DATETIME, AGEDEATH_DAYS, AGEDEATH_HRS,
         DENOM_28d, DENOM_365d,MISSING_MNH09, MISSING_MNH11, DTH_TIME_MISSING, DOB_AFTER_DEATH,
         BWEIGHT_PRISMA, BWEIGHT_ANY, contains("LBW"),BW_TIME, MISSING_TIME, MISSING_PRISMA, MISSING_FACILITY, MISSING_BOTH, contains("PRETERM"), contains("SGA"), contains("NEO_DTH"),INF_DTH,INF_FETAL_DTH_DENOM,
         contains("STILLBIRTH"), MISSING_SIGNS_OF_LIFE,contains("INF_"), contains("DENOM_HYPERBILI"), DENOM_JAUN,
         M11_BREATH_FAIL_CEOCCUR, M11_INF_PROCCUR_2, M11_INF_PROCCUR_3, M11_INF_PROCCUR_4, M11_INF_PROCCUR_5, M11_INF_PROCCUR_6)


# export data to local
# write.csv(infant_outcomes, paste0(path_to_save, "infant_outcomes" ,".csv"), row.names=FALSE)

# # save data set; this will get called into the report
# write.csv(infant_outcomes, paste0(path_to_tnt, "INF_OUTCOMES" ,".csv"), na="", row.names=FALSE)
# write.xlsx(infant_outcomes, paste0(path_to_tnt, "INF_OUTCOMES" ,".xlsx"), na="", row.names=FALSE)


# 3. set working directory
#first need to make subfolder with upload date
maindir <- paste0("Z:/Outcome Data", sep = "")
subdir = UploadDate
dir.create(file.path(maindir, subdir), showWarnings = FALSE)

# export data to tnt drive
write.csv(infant_outcomes, paste0("Z:/Outcome Data/",UploadDate, "/INF_OUTCOMES" ,".csv"), row.names=FALSE)

## data check to see how many infants across sites 
for (i in unique(mnh09_long$SITE)) {
  df <- mnh09_long %>% filter(SITE == i)
  print(paste0(i, " infants, n", "=", length(unique(df$INFANTID))))
}
 
################################    
# Verbal autopsy data checks 
################################    

test <- infant_outcomes %>% select(SITE, MOMID, PREGID, INFANTID,DTH_INDICATOR,FETAL_LOSS,INF_DTH, NEO_DTH, PREG_END_DATE, FETAL_LOSS_DATE, DEATHDATE_MNH24, AGEDEATH_DAYS, AGEDEATH_HRS) %>% 
  full_join(inf_cod %>% select(SITE, MOMID, PREGID, INFANTID, DEATH_DATE, AGE_DTH, END_PREG_CALC),  by = c("SITE", "MOMID", "PREGID", "INFANTID"))

# test <- infant_outcomes %>% select(SITE, MOMID, PREGID, INFANTID,DTH_INDICATOR,FETAL_LOSS,STILLBIRTH_20WK)

table(infant_outcomes$DTH_INDICATOR, infant_outcomes$SITE)
table(inf_cod$SITE)
test2 <- infant_outcomes %>% 
  filter((DTH_INDICATOR==1 | FETAL_LOSS==1)) 
table(test2$SITE)

gha <- infant_outcomes %>% 
  filter(SITE == "Ghana" & (DTH_INDICATOR==1 | FETAL_LOSS==1)) %>% 
  select(SITE, MOMID, PREGID, INFANTID,DTH_INDICATOR, FETAL_LOSS,INF_DTH, NEO_DTH, DEATHDATE_MNH24, AGEDEATH_DAYS, AGEDEATH_HRS) %>% 
  mutate(inf_outcomes = 1) %>% 
  full_join(inf_cod %>% filter(SITE == "Ghana") %>% 
              select(SITE, MOMID, PREGID, INFANTID, DEATH_DATE, AGE_DTH), 
            by = c("SITE", "MOMID", "PREGID", "INFANTID"))

cmc <- infant_outcomes %>% 
  filter(SITE == "India-CMC" & (DTH_INDICATOR==1 | FETAL_LOSS==1)) %>% 
  select(SITE, MOMID, PREGID, INFANTID,GESTAGEBIRTH_ANY, DTH_INDICATOR, FETAL_LOSS,INF_DTH, NEO_DTH, DEATHDATE_MNH24, AGEDEATH_DAYS, AGEDEATH_HRS) %>% 
  mutate(inf_outcomes = 1) %>% 
  full_join(inf_cod %>% filter(SITE == "India-CMC") %>% 
              select(SITE, MOMID, PREGID, INFANTID, DEATH_DATE, AGE_DTH), 
            by = c("SITE", "MOMID", "PREGID", "INFANTID"))

sas <- infant_outcomes %>% 
  filter(SITE == "India-SAS" & (DTH_INDICATOR==1 | FETAL_LOSS==1)) %>% 
  select(SITE, MOMID, PREGID, INFANTID,GESTAGEBIRTH_ANY, DTH_INDICATOR, FETAL_LOSS,INF_DTH, NEO_DTH, DEATHDATE_MNH24, AGEDEATH_DAYS, AGEDEATH_HRS) %>% 
  mutate(inf_outcomes = 1) %>% 
  full_join(inf_cod %>% filter(SITE == "India-SAS") %>% 
              select(SITE, MOMID, PREGID, INFANTID, DEATH_DATE, AGE_DTH), 
            by = c("SITE", "MOMID", "PREGID", "INFANTID"))

# REM-37-00048-01-01

ken <- infant_outcomes %>% 
  filter(SITE == "Kenya" & (DTH_INDICATOR==1 | FETAL_LOSS==1)) %>% 
  select(SITE, MOMID, PREGID, INFANTID,GESTAGEBIRTH_ANY, DTH_INDICATOR, FETAL_LOSS,INF_DTH, NEO_DTH, DEATHDATE_MNH24, AGEDEATH_DAYS, AGEDEATH_HRS) %>% 
  mutate(inf_outcomes = 1) %>% 
  full_join(inf_cod %>% filter(SITE == "Kenya") %>% 
              select(SITE, MOMID, PREGID, INFANTID, DEATH_DATE, AGE_DTH), 
            by = c("SITE", "MOMID", "PREGID", "INFANTID"))

pak <- infant_outcomes %>% 
  filter(SITE == "Pakistan" & (DTH_INDICATOR==1 | FETAL_LOSS==1)) %>% 
  select(SITE, MOMID, PREGID, INFANTID,GESTAGEBIRTH_ANY, DTH_INDICATOR, FETAL_LOSS,INF_DTH, NEO_DTH, DEATHDATE_MNH24, AGEDEATH_DAYS, AGEDEATH_HRS) %>% 
  mutate(inf_outcomes = 1) %>% 
  full_join(inf_cod %>% filter(SITE == "Pakistan") %>% 
              select(SITE, MOMID, PREGID, INFANTID, DEATH_DATE, AGE_DTH), 
            by = c("SITE", "MOMID", "PREGID", "INFANTID"))

zam <- infant_outcomes %>% 
  filter(SITE == "Zambia" & (DTH_INDICATOR==1 | FETAL_LOSS==1)) %>% 
  select(SITE, MOMID, PREGID, INFANTID,GESTAGEBIRTH_ANY, DTH_INDICATOR, FETAL_LOSS,INF_DTH, NEO_DTH, DEATHDATE_MNH24, AGEDEATH_DAYS, AGEDEATH_HRS) %>% 
  mutate(inf_outcomes = 1) %>% 
  full_join(inf_cod %>% filter(SITE == "Zambia") %>% 
              select(SITE, MOMID, PREGID, INFANTID, DEATH_DATE, AGE_DTH), 
            by = c("SITE", "MOMID", "PREGID", "INFANTID"))


################################    
# Sepsis diagnosis checkbox - DATA CHECKS
# INFANT_MHTERM_10 mnh13
# INFECTION_MHTERM_1 mnh20
################################    
# sepsis_check <- psbi_outcome %>% filter(INF_PSBI_ANY==1) %>%  select(SITE, MOMID, PREGID, contains("INF_PSBI")) 
# 
# mnh13_check <- mnh13 %>% filter(M13_INFANT_MHTERM_10==1 & INFANTID %in% livebirths) %>% select(SITE, MOMID, PREGID, M13_TYPE_VISIT, M13_INFANT_MHTERM_10) %>% 
#   mutate(M13_SEPSIS_CHECK =1)
# table(mnh13_check$SITE)
# 
# # n = 21 with sepsis checked in mnh13
# 
# mnh20_check <- mnh20 %>% filter(M20_INFECTION_MHTERM_1==1) %>% select(SITE, MOMID, PREGID, M20_INFECTION_MHTERM_1) %>% 
#   mutate(M20_SEPSIS_CHECK=1)
# table(mnh20_check$SITE)
# 
# # n = 43 
# 
# ## IDs in mnh13_check that are also in psbi 
# sepsis_check_m13 <- sepsis_check %>% full_join(mnh13_check, by = c("SITE", "MOMID", "PREGID")) %>% 
#   mutate(NO_PSBI_REPORTED = case_when(M13_SEPSIS_CHECK ==1 & is.na(INF_PSBI_ANY) ~ 1, TRUE ~ 0),
#          CONFIRMED = case_when(M13_SEPSIS_CHECK ==1 & INF_PSBI_ANY==1 ~ 1, TRUE ~ 0)
#   )
# table(sepsis_check_m13$NO_PSBI_REPORTED, sepsis_check_m13$SITE) # where NO_PSBI_REPORTED = 1 indicates sepsis checked and not psbi reported
# table(sepsis_check_m13$CONFIRMED, sepsis_check_m13$SITE) # where CONFIRMED = 1 indicates sepsis checked and psbi reported
# 
# test <- sepsis_check_m13 %>% filter(NO_PSBI_REPORTED==1)
# test2 <- psbi_outcome_wide %>% filter(PREGID %in% as.vector(test$PREGID) & SITE == "Zambia")
# ## n = 3 for zambia
# 
# ## IDs in mnh20_check that are also in psbi 
# sepsis_check_m20 <- sepsis_check %>% full_join(mnh20_check, by = c("SITE", "MOMID", "PREGID")) %>% 
#   mutate(NO_PSBI_REPORTED = case_when(M20_SEPSIS_CHECK ==1 & is.na(INF_PSBI_ANY) ~ 1, TRUE ~ 0),
#          CONFIRMED = case_when(M20_SEPSIS_CHECK ==1 & INF_PSBI_ANY==1 ~ 1, TRUE ~ 0)
#   )
# table(sepsis_check_m20$NO_PSBI_REPORTED, sepsis_check_m20$SITE) # where NO_PSBI_REPORTED = 1 indicates sepsis checked and not psbi reported
# table(sepsis_check_m20$CONFIRMED, sepsis_check_m20$SITE) # where CONFIRMED = 1 indicates sepsis checked and psbi reported

# ## hospitalization review (if a baby has psbi, they should also go to the hospital)
# test <- psbi_outcome %>% filter(SITE != "Ghana" & INF_PSBI==1) %>% 
#   group_by(SITE, MOMID, PREGID, INFANTID) %>% 
#   summarise(
#     HOSP = as.integer(any(TYPE_VISIT == 15)),  # Use any() to check if any TYPE_VISIT is 15
#     .groups = "drop"  # Ungroup after summarizing
#   ) ## only 41 infants had a hospitalization visit 
# table(test$HOSP)
#   
