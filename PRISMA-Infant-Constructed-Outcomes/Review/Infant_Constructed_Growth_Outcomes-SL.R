#*****************************************************************************
#* PRISMA Infant Outcomes: 
#* Drafted: 01 June 2024, Precious Williams
#* Last updated: 08 August 2024

# 9. Infant Growth Outcomes

# The R package "anthro"
# It includes functions to calculate z-scores and prevalence estimates (and CIs), 
# It provides results for the indicators: 
# length/height-for-age, weight-for-age, weight-for-length, weight-for-height, 
# body mass index-for-age, head circumference-for-age, arm circumference-for-age, 
# The package is available in the CRAN repository at https://CRAN.R-project.org/package=anthro.


## SL CODE REVIEW (2024-08-13)
#*****************************************************************************
# clear environment 
rm(list = ls())

# load packages 
library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(openxlsx)
library(anthro)
library(ggplot2)
library(kableExtra)
library(knitr)
library(table1)
library(labelled)
library(growthstandards)
library(haven)

#*****************************************************************************
#* load data
#*****************************************************************************

## UPDATE EACH RUN ## 
UploadDate = "2024-06-28"

#Set your main directory 
path_to_data <- paste0("~/Analysis/Merged_data/", UploadDate)

## SL: set data directory 
path_to_data <- paste0("Z:/Stacked Data/", UploadDate)

# set path to save 
# path_to_save <- paste0("~/Analysis/Infant-Constructed-Variables/data/")
# path_to_save_figures <- paste0("~/Analysis/Infant-Constructed-Variables/output/")
# path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")


# # import forms 
mnh01 <- read_csv (paste0(path_to_data, "/mnh01_merged.csv"))

mnh02 <- read_csv (paste0(path_to_data, "/mnh02_merged.csv"))

mnh09 <- read_csv (paste0(path_to_data, "/mnh09_merged.csv"))

mnh11 <- read_csv (paste0(path_to_data, "/mnh11_merged.csv"))

mnh13 <- read_csv (paste0(path_to_data, "/mnh13_merged.csv"))

df_names <- ls(pattern = "^mnh", envir = .GlobalEnv)
#df_list <- mget(df_names)

# Define a function to rename variables in a data frame
rename_variables <- function(df) {
  # Exclude specific "ID" variables from renaming
  exclude_variables <- c("MOMID", "PREGID", "SCRNID", "INFANTID", "SITE")
  id_variables <- intersect(exclude_variables, names(df))
  var_names <- names(df)[!names(df) %in% id_variables]
  
  # Rename the variables
  new_var_names <- gsub("^M.._", "", var_names)  # Remove the first three characters and underscore
  names(df)[!names(df) %in% id_variables] <- new_var_names
  
  return(df)
}

# Loop through each data frame, excluding specific "ID" variables
for (df_name in df_names) {
  df <- get(df_name)  # Retrieve the data frame by name
  if (is.data.frame(df)) {  # Check if the object is a data frame
    assign(df_name, rename_variables(df), envir = .GlobalEnv)
  }
}

## SL: update paths(just for me to run - no action needed)
mat_end_point <- read_dta("Z:/Outcome Data/2024-06-28/MAT_ENDPOINTS.dta")

mat_enroll <- read_csv ("Z:/Outcome Data/2024-06-28/MAT_ENROLL.csv")

inf_outcomes <- read_csv ("Z:/Outcome Data/2024-06-28/INF_OUTCOMES.csv")


# pull all enrolled participants
## SL: instead of using the following chunk enrolled IDs, we should pull from mat_enroll instead. There are 4 IDs that are not in mat_enroll but are in the following codes. 
        # I need to update my codes as well - thoughts on this? 
enrolled_ids <- mnh02 %>% 
  mutate(ENROLL = ifelse(AGE_IEORRES == 1 & 
                           PC_IEORRES == 1 & 
                           CATCHMENT_IEORRES == 1 & 
                           CATCH_REMAIN_IEORRES == 1 & 
                           CONSENT_IEORRES == 1, 1, 0)) %>% 
  select(SITE, SCRNID, MOMID, PREGID,ENROLL, AGE_IEORRES, PC_IEORRES, CATCHMENT_IEORRES,CATCH_REMAIN_IEORRES, CONSENT_IEORRES) %>% 
  filter(ENROLL == 1) %>% 
  select(SITE, MOMID, PREGID, ENROLL)

enrolled_ids_vec <- as.vector(enrolled_ids$PREGID)

#pull pregnancy end ga
end_preg <- mat_end_point %>% 
  select(SITE, MOMID, PREGID, PREG_END_GA)

#pull pregnancy estimated due date
mat_edd <- mat_enroll %>% 
  select(SITE, MOMID, PREGID, EDD_BOE)

#read in infant outcomes and classify PRETERM - as any preterm
inf_outcomes_f <- inf_outcomes %>% 
  select (SITE, MOMID, PREGID, INFANTID, LBW_CAT_ANY, PRETERMBIRTH_CAT, SGA_CENTILE, SGA_CAT, INF_JAUN_NON_SEV_ANY, 
          INF_JAUN_SEV_24HR, INF_JAUN_SEV_GREATER_24HR, INF_SGA_PRETERM,INF_AGA_PRETERM,INF_SGA_TERM,INF_AGA_TERM) %>% 
  ## SL: add comments below indicating what 1, 2, 3 represent 
  mutate (PRETERM = case_when( PRETERMBIRTH_CAT %in% c (12,13,14,15) ~ 1, ## any preterm
                               PRETERMBIRTH_CAT == 11 ~ 2, ## term (37 to <41 wks)
                               PRETERMBIRTH_CAT == 10 ~ 3, ## postterm (>= 41 wks)
                               TRUE ~ 55))

#*****************************************************************************
#* PULL IDS OF INFANTS & CREATE DELIVERY DATE, TIME FROM MNH09
#*****************************************************************************
mnh09_sub <- mnh09 %>%
  filter(PREGID %in% enrolled_ids_vec) %>%
  select(MOMID, PREGID, SITE, INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4, DELIV_DSSTDAT_INF1,
         DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4,DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF2, 
         DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF4, BIRTH_DSTERM_INF1,BIRTH_DSTERM_INF2, BIRTH_DSTERM_INF3,
         BIRTH_DSTERM_INF4, SEX_INF1, SEX_INF2, SEX_INF3, SEX_INF4, DELIV_PRROUTE_INF1, DELIV_PRROUTE_INF2, 
         DELIV_PRROUTE_INF3, DELIV_PRROUTE_INF4) %>% 
  
  mutate(DELIV_DSSTDAT_INF1 = ymd(parse_date_time(DELIV_DSSTDAT_INF1, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
         DELIV_DSSTDAT_INF2 = ymd(parse_date_time(DELIV_DSSTDAT_INF2, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
         DELIV_DSSTDAT_INF3 = ymd(parse_date_time(DELIV_DSSTDAT_INF3, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
         DELIV_DSSTDAT_INF4 = ymd(parse_date_time(DELIV_DSSTDAT_INF4, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
         SEX_INF1 = as.numeric(SEX_INF1), SEX_INF2 = as.numeric(SEX_INF2), SEX_INF3 = as.numeric(SEX_INF3), SEX_INF4 = as.numeric(SEX_INF4)
  )%>%
  # replace default value date with NA 
  mutate(DELIV_DSSTDAT_INF1 = replace(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF1==ymd("1907-07-07"), NA),
         DELIV_DSSTDAT_INF2 = replace(DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF2==ymd("1907-07-07"), NA),
         DELIV_DSSTDAT_INF3 = replace(DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF3==ymd("1907-07-07"), NA),
         DELIV_DSSTDAT_INF4 = replace(DELIV_DSSTDAT_INF4, DELIV_DSSTDAT_INF4==ymd("1907-07-07"), NA)) %>%
  
  # replace default value time with NA 
  mutate(DELIV_DSSTTIM_INF1 = replace(DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF1=="77:77", NA),  ## should be 77:77, but pak is using 07:07
         DELIV_DSSTTIM_INF2 = replace(DELIV_DSSTTIM_INF2, DELIV_DSSTTIM_INF2=="77:77", NA),
         DELIV_DSSTTIM_INF3 = replace(DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF3=="77:77", NA),
         DELIV_DSSTTIM_INF4 = replace(DELIV_DSSTTIM_INF4, DELIV_DSSTTIM_INF4=="77:77", NA)) %>%
  
  # Convert time to time format
  mutate( DELIV_DSSTTIM_INF1 = if_else(!is.na(DELIV_DSSTTIM_INF1), as.ITime(DELIV_DSSTTIM_INF1), NA),
          DELIV_DSSTTIM_INF2 = if_else(!is.na(DELIV_DSSTTIM_INF2), as.ITime(DELIV_DSSTTIM_INF2), NA),
          DELIV_DSSTTIM_INF3 = if_else(!is.na(DELIV_DSSTTIM_INF3), as.ITime(DELIV_DSSTTIM_INF3), NA),
          DELIV_DSSTTIM_INF4 = if_else(!is.na(DELIV_DSSTTIM_INF4), as.ITime(DELIV_DSSTTIM_INF4), NA))%>%
  
  # Concatenate dates and times and convert to datetime format
  mutate( DELIVERY_DATETIME_INF1 = if_else(!is.na(DELIV_DSSTDAT_INF1) & !is.na(DELIV_DSSTTIM_INF1),
                                           as.POSIXct(paste(DELIV_DSSTDAT_INF1, DELIV_DSSTTIM_INF1), format = "%Y-%m-%d %H:%M:%S"),
                                           DELIV_DSSTDAT_INF1),
          DELIVERY_DATETIME_INF2 = if_else(!is.na(DELIV_DSSTDAT_INF2) & !is.na(DELIV_DSSTTIM_INF2),
                                           as.POSIXct(paste(DELIV_DSSTDAT_INF2, DELIV_DSSTTIM_INF2), format = "%Y-%m-%d %H:%M:%S"),
                                           DELIV_DSSTDAT_INF2),
          DELIVERY_DATETIME_INF3 = if_else(!is.na(DELIV_DSSTDAT_INF3) & !is.na(DELIV_DSSTTIM_INF3),
                                           as.POSIXct(paste(DELIV_DSSTDAT_INF3, DELIV_DSSTTIM_INF3), format = "%Y-%m-%d %H:%M:%S"),
                                           DELIV_DSSTDAT_INF3),
          DELIVERY_DATETIME_INF4 = if_else(!is.na(DELIV_DSSTDAT_INF4) & !is.na(DELIV_DSSTTIM_INF4),
                                           as.POSIXct(paste(DELIV_DSSTDAT_INF4, DELIV_DSSTTIM_INF4), format = "%Y-%m-%d %H:%M:%S"),
                                           DELIV_DSSTDAT_INF4)) 

# SL:  Something goofy is happening with some of the entries where the infantid is 
    # overrode with 0 or 1 and is then being either excluded or duplicated in the final data. there are n = 148 duplicates in the infant_dob dataset

# Getting the Date of Birth, Sex and Birth Outcome for Each ID
infant_dob <- mnh09_sub %>%
  # Pivot the data from wide to long format
  pivot_longer(
    # Select columns to pivot (INFANTID_INF1-4 and DELIVERY_DATETIME_INF1-4)
    cols = c( INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4,
              DELIVERY_DATETIME_INF1, DELIVERY_DATETIME_INF2, DELIVERY_DATETIME_INF3, DELIVERY_DATETIME_INF4, 
              BIRTH_DSTERM_INF1, BIRTH_DSTERM_INF2, BIRTH_DSTERM_INF3, BIRTH_DSTERM_INF4,
              SEX_INF1, SEX_INF2, SEX_INF3, SEX_INF4,
              DELIV_PRROUTE_INF1, DELIV_PRROUTE_INF2, DELIV_PRROUTE_INF3, DELIV_PRROUTE_INF4),
    
    # Specify how to separate column names: extract suffixes and values
    names_to = c(".value", "infant_suffix"),
    # Define the pattern: splitting by "_INF" and matching the suffix
    names_pattern = "(.*)_INF(\\d)$" ) %>%
  
  # Rename the columns
  rename(INFANTID = INFANTID,
         DOB = DELIVERY_DATETIME ) %>%
  
  # Drop the suffix column since it was used for reshaping
  select(MOMID, PREGID, INFANTID, SITE, DOB, BIRTH_DSTERM, SEX, DELIV_PRROUTE )  %>%
  
  # Filter out rows where INFANTID is NA
  filter(INFANTID != "" & INFANTID != "n/a" ) %>% 
  
  # generate indicator variable for having a birth outcome; if a birth outcome has been reported (BIRTH_DSTERM =1 or 2), then BIRTH_OUTCOME_REPORTED ==1
  mutate(BIRTH_OUTCOME_REPORTED = ifelse(BIRTH_DSTERM == 1 | BIRTH_DSTERM == 2, 1, 0),
         SEX = case_when( 
           SEX %in% c("M", "1") ~ 1,
           SEX %in% c("F", "2") ~ 2),
         MNH09_HAVE = 1)  %>%
  
  # only want those who have had a birth outcome 
  filter(BIRTH_OUTCOME_REPORTED == 1 )

test <- infant_dob %>% group_by(SITE, MOMID, PREGID, INFANTID) %>% mutate(n=n()) %>% filter(n>1)
## SL: n = 108 duplicates in the living_infants dataset

# only want those who have had a live birth outcome 
living_infants <- infant_dob %>% filter (BIRTH_DSTERM == 1) 
## SL: i have n = 5213 BIRTH_DSTERM=1 in my infant outcome dataset; you have n = 5217 

test <- living_infants %>% group_by(SITE, MOMID, PREGID, INFANTID) %>% mutate(n=n()) %>% filter(n>1)
## SL: n = 104 duplicates in the living_infants dataset

# discrep_livebirths <- living_infants %>% select(SITE, MOMID, PREGID, INFANTID, BIRTH_DSTERM) %>% 
#   mutate(pw_df = 1) %>% 
#   full_join(mnh09_long[c("SITE", "MOMID", "PREGID", "INFANTID", "M09_BIRTH_DSTERM")], by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
#   filter(M09_BIRTH_DSTERM==1)

# only want those who have had a live birth outcome 
deceased_infants <- infant_dob %>% filter (BIRTH_DSTERM == 2)
test <- deceased_infants %>% group_by(SITE, MOMID, PREGID, INFANTID) %>% mutate(n=n()) %>% filter(n>1)
## SL: n = 4 duplicates in the deceased_infants dataset; I have n = 172 BIRTH_DSTERM=2 in my mnh09_long data; you have n = 166


# Create birth weight dataframe to merge with MNH09 
mnh11_constructed <- mnh11 %>% 
  mutate(
    WEIGHT_PRISMA = case_when(
      BW_EST_FAORRES >= 0 & BW_EST_FAORRES < 72 & BW_FAORRES > 0 ~ BW_FAORRES,  # if time since birth infant was weight is between 0 & 72 hours
      BW_FAORRES > 0 & (is.na(BW_EST_FAORRES) | BW_EST_FAORRES %in% c(-5, -7, ., NA)) ~ BW_FAORRES, # if prisma birthweight available and no time reported, use prisma
      BW_FAORRES > 0 & BW_EST_FAORRES >= 72 ~ -5, # if prisma birthweight is available but time is >= 72 hours, not usable
      BW_FAORRES < 0 ~ -5, # if prisma birthweight is missing, missing
      TRUE ~ -5 # if prisma birthweight is missing, replace with default value -5
    ),
    
    WEIGHT_ANY = case_when(
      (WEIGHT_PRISMA <= 0 & BW_FAORRES_REPORT > 0) | 
        (WEIGHT_PRISMA < 0 & BW_EST_FAORRES >= 72 & BW_FAORRES_REPORT > 0) ~ BW_FAORRES_REPORT, ## if PRISMA is missing and facility is not OR if prisma is not missing but time is >7days, select facility
      WEIGHT_PRISMA < 0 & BW_FAORRES_REPORT < 0 ~ -5, # if prisma is available but the time is invalid, use facility
      TRUE ~ BW_FAORRES
    ),
    
    M11_VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
    M11_VISIT_OBSSTDAT = replace(M11_VISIT_OBSSTDAT, M11_VISIT_OBSSTDAT %in% as.Date(c("1907-07-07", "2007-07-07", "1909-07-07", "1909-09-09", "1905-05-05")), NA),
    
    # Create an indicator variable for if birth MNH11 date is available
    ## SL: add underscore in varname to keep consistency 
    M11DATE_MRSRE = ifelse(is.na(M11_VISIT_OBSSTDAT), 0, 1),
    
    # Create an indicator variable for if birth weight is available
    WEIGHT_MRSRE = ifelse(WEIGHT_ANY > 0 & !is.na (WEIGHT_ANY), 1, 0),
    
    # Calculate the length of infant at birth
    LENGTH = ifelse( LENGTH_FAORRES_1 > 0 & !is.na(LENGTH_FAORRES_1) & LENGTH_FAORRES_2 > 0 & !is.na(LENGTH_FAORRES_2) & LENGTH_FAORRES_3 > 0 & !is.na(LENGTH_FAORRES_3),
                     round(rowMeans(select(., c(LENGTH_FAORRES_1, LENGTH_FAORRES_2, LENGTH_FAORRES_3)), na.rm = TRUE), 2),
                     ifelse(LENGTH_FAORRES_1 > 0 & LENGTH_FAORRES_2 > 0 & !is.na(LENGTH_FAORRES_1) & is.na(LENGTH_FAORRES_2), ## SL: should is.na(LENGTH_FAORRES_2) be !is.na(LENGTH_FAORRES_2) instead?
                            round(rowMeans(select(., c(LENGTH_FAORRES_1, LENGTH_FAORRES_2)), na.rm = TRUE), 2),
                            ifelse (LENGTH_FAORRES_1 > 0 & !is.na(LENGTH_FAORRES_1),
                                    LENGTH_FAORRES_1, -5 ))),
    
    # Create an indicator variable for if birth length is available
    LENGTH_MRSRE = ifelse(LENGTH > 0 , 1, 0),
    
    # Calculate the head circumference measurement of infant at birth
    HC_FAORRES = ifelse( HC_FAORRES_1 > 0 & !is.na(HC_FAORRES_1) & HC_FAORRES_2 > 0 & !is.na(HC_FAORRES_2) & HC_FAORRES_3 > 0 & !is.na(HC_FAORRES_3),
                         round(rowMeans(select(., c(HC_FAORRES_1, HC_FAORRES_2, HC_FAORRES_3)), na.rm = TRUE), 2),
                         ifelse(HC_FAORRES_1 > 0 & HC_FAORRES_2 > 0 & !is.na(HC_FAORRES_1) & is.na(HC_FAORRES_2), ## SL: should is.na(HC_FAORRES_2) be !is.na(HC_FAORRES_2) instead?
                                round(rowMeans(select(., c(HC_FAORRES_1, HC_FAORRES_2)), na.rm = TRUE), 2),
                                ifelse (HC_FAORRES_1 > 0 & !is.na(HC_FAORRES_1),
                                        HC_FAORRES_1, -5 ))), 
    # Create an indicator variable for if birth length is available
    HC_MRSRE = ifelse(HC_FAORRES > 0 , 1, 0), 
    # Create an indicator variable for if they have MNH11 form
    MNH11_HAVE = 1, 
    TYPE_VISIT = 6
  ) %>% 
  
  #only keep living infants
  filter (INF_VITAL_MNH11 == 1 ) %>%
  
  arrange(SITE, MOMID, PREGID, INFANTID, M11_VISIT_OBSSTDAT) %>% 
  
  select(SITE, MOMID, PREGID, INFANTID, M11_VISIT_OBSSTDAT,  MNH11_HAVE, WEIGHT_PRISMA, WEIGHT_ANY, WEIGHT_MRSRE, #INF_VISIT_72HR_MNH11,
         LENGTH, LENGTH_MRSRE, HC_FAORRES, HC_FAORRES_1, HC_FAORRES_2, HC_FAORRES_3, HC_MRSRE, SEX_INF, INF_VITAL_MNH11, M11DATE_MRSRE, TYPE_VISIT)

#Create a dataset which joins MNH09 & MNH11 together 
allbirth_anthro <- mnh11_constructed %>% 
  right_join(living_infants, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  left_join(mat_edd, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join (end_preg, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join (inf_outcomes_f, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  mutate( SEX_MNH09 = SEX,
          SEX_MNH11 = replace(SEX_INF, SEX_INF %in% c( 55, 77, 99), NA),
  ) %>% 
  mutate( 
    DOB_Date = as.Date(trunc(DOB, 'days')), 
    AGE_IN_DAYS = round(as.numeric (difftime (M11_VISIT_OBSSTDAT, DOB_Date, units = "days")), 0),
    AGE_IN_WKS = round(as.numeric (difftime  (M11_VISIT_OBSSTDAT, DOB_Date, units = "weeks")), 0),
    WEIGHT = round(WEIGHT_ANY / 1000, 2),
    AGE_IN_DAYS = ifelse (AGE_IN_DAYS > 5 & TYPE_VISIT == 6, -5, AGE_IN_DAYS),
    PREG_GA_CALC = (round(as.numeric (difftime (DOB_Date, EDD_BOE, units = "days")), 0)) + 280
  ) 

test <- allbirth_anthro %>% group_by(SITE, MOMID, PREGID, INFANTID) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
## SL: n = 438 duplicates in the allbirth_anthro dataset

## SL: for measurements at birth using MNH11 does the alogrithm only required days since birth or hours? 
#mutate to create all the necessary z scores
tbirth_measure <- allbirth_anthro %>% 
  # Replace -7 and -5 with NA across all columns
  mutate(across(everything(), ~replace(., . %in% c(-7, -5), NA))) %>% 
  # Filter records where AGE_IN_DAYS is between 0 and 356 and SEX is not NA
  filter(AGE_IN_DAYS >= 0 & AGE_IN_DAYS <= 356 & !is.na(SEX)) %>%
  mutate(
    # Handle negative AGE_IN_DAYS by setting them to NA
    AGE_IN_DAYS = ifelse(AGE_IN_DAYS < 0, NA, AGE_IN_DAYS),
    # Calculate anthropometric z-scores
    with( ., anthro_zscores(sex = SEX, age = AGE_IN_DAYS, weight = WEIGHT, lenhei = LENGTH, headc = HC_FAORRES)),
    # Calculate weight-to-length ratio
    wlr = WEIGHT/(LENGTH/100),
    # Calculate length-for-age z-score using INTERGROWTH-21st standards
    ZLEN_IG = case_when(SEX == 1 & LENGTH > 0 ~ igb_lencm2zscore(PREG_GA_CALC, LENGTH, sex = "Male"),
                        SEX == 2 & LENGTH > 0 ~ igb_lencm2zscore(PREG_GA_CALC, LENGTH, sex = "Female"),
                        TRUE ~ NA),
    
    # Calculate weight-for-age z-score using INTERGROWTH-21st standards
    ZWEI_IG = case_when(SEX == 1  & WEIGHT > 0 ~ igb_wtkg2zscore(PREG_GA_CALC, WEIGHT, sex = "Male"),
                        SEX == 2 & WEIGHT > 0 ~ igb_wtkg2zscore(PREG_GA_CALC, WEIGHT, sex = "Female"),
                        TRUE ~ NA),
    
    # Calculate weight-for-length z-score using INTERGROWTH-21st standards
    ZWFL_IG = case_when(SEX == 1  & WEIGHT > 0 ~ igb_wlr2zscore(PREG_GA_CALC, wlr, sex = "Male"),
                        SEX == 2 & WEIGHT > 0 ~ igb_wlr2zscore(PREG_GA_CALC, wlr, sex = "Female"),
                        TRUE ~ NA),
    
    # Calculate head circumference-for-age z-score using INTERGROWTH-21st standards
    ZHC_IG = case_when(SEX == 1  & HC_FAORRES > 0 ~ igb_hcircm2zscore(PREG_GA_CALC, HC_FAORRES, sex = "Male"),
                       SEX == 2 & HC_FAORRES > 0 ~ igb_hcircm2zscore(PREG_GA_CALC, HC_FAORRES, sex = "Female"),
                       TRUE ~ NA)) %>% 
    mutate(
    # Round the z-scores to 2 decimal places
    ZLEN_IG = round(ZLEN_IG, 2),
    ZWEI_IG = round(ZWEI_IG, 2),
    ZWFL_IG = round(ZWFL_IG, 2),
    ZHC_IG = round(ZHC_IG, 2),
    # Create flags for extreme z-scores
    FLEN_IG = ifelse(abs(ZLEN_IG) > 6, 1, 0),
    FWEI_IG = ifelse(ZWEI_IG > 5 | ZWEI_IG < -6, 1, 0),
    FWFL_IG = ifelse(abs(ZWFL_IG) > 5, 1, 0),
    FHC_IG = ifelse(abs(ZHC_IG) > 5, 1, 0)
  ) %>% 
  # Rename M11_VISIT_OBSSTDAT to VISIT_DATE
  rename(VISIT_DATE = M11_VISIT_OBSSTDAT)

test <- tbirth_measure %>% group_by(SITE, MOMID, PREGID, INFANTID) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
## SL: n = 378 duplicates in the tbirth_measure dataset

# query <- allbirth_anthro %>% filter (AGE_IN_DAYS > 5 | AGE_IN_DAYS < 0) %>% 
#   mutate(PSTAFF_BIRTH = case_when(WEIGHT_MRSRE == 1 & WEIGHT_PRISMA > 0 ~ 1,
#                                   WEIGHT_MRSRE == 1 & WEIGHT_PRISMA < 0 & WEIGHT_ANY > 0 ~ 0,
#                                   TRUE ~ 55))

# Create mnh13_sub dataframe with necessary calculations and selections
mnh13_constructed <- mnh13 %>% 
  filter(INF_VITAL_MNH13 == 1 & INF_VISIT_MNH13 %in% c (1,2,3)) %>%
  mutate(
    LENGTH = case_when(
      LENGTH_PERES_1 > 0 & LENGTH_PERES_2 > 0 & LENGTH_PERES_3 > 0 ~ round(rowMeans(select(., c(LENGTH_PERES_1, LENGTH_PERES_2, LENGTH_PERES_3)), na.rm = TRUE), 2),
      LENGTH_PERES_1 > 0 & LENGTH_PERES_2 > 0 & LENGTH_PERES_3 <= 0 ~ round(rowMeans(select(., c(LENGTH_PERES_1, LENGTH_PERES_2)), na.rm = TRUE), 2),
      LENGTH_PERES_1 > 0 & LENGTH_PERES_2 <= 0 & LENGTH_PERES_3 <= 0 ~ LENGTH_PERES_1,
      LENGTH_PERES_1 <= 0 ~ -5,
      TRUE ~ -5
    ),
    HC_FAORRES = case_when(
      HC_PERES_1 > 0 & HC_PERES_2 > 0 & HC_PERES_3 > 0 ~ round(rowMeans(select(., c(HC_PERES_1, HC_PERES_2, HC_PERES_3)), na.rm = TRUE), 2),
      HC_PERES_1 > 0 & HC_PERES_2 > 0 & HC_PERES_3 <= 0 ~ round(rowMeans(select(., c(HC_PERES_1, HC_PERES_2)), na.rm = TRUE), 2),
      HC_PERES_1 > 0 & HC_PERES_2 <= 0 & HC_PERES_3 <= 0 ~ HC_PERES_1,
      HC_PERES_1 <= 0 ~ -5,
      TRUE ~ -5
    ),
   
     M13_VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
    
    #change weight to kg from grams
    WEIGHT = round(WEIGHT_PERES / 1000, 2),
    
    # Create an indicator variable for if length, weight, headcircumference is available
    HC_MRSRE = ifelse(HC_FAORRES > 0 , 1, 0), 
    LENGTH_MRSRE = ifelse(LENGTH > 0 , 1, 0), 
    # Create an indicator variable for if birth weight is available
    WEIGHT_MRSRE = ifelse(WEIGHT > 0 | !is.na (WEIGHT), 1, 0),
    
  ) %>%
  select (SITE, MOMID, PREGID, INFANTID, TYPE_VISIT, M13_VISIT_OBSSTDAT, WEIGHT, LENGTH, HC_FAORRES, HC_PERES_1, HC_PERES_2, HC_PERES_3,
          INF_VITAL_MNH13, HC_MRSRE, WEIGHT_MRSRE, LENGTH_MRSRE) 

# Step 2: Create allvisit_anthro dataframe with necessary calculations and selections
allvisit_anthro <- mnh13_constructed %>%
  right_join(living_infants, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  left_join(mat_edd, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join (end_preg, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join (inf_outcomes_f, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  mutate(
    DOB_Date = as.Date(trunc(DOB, 'days')),
    M13_VISIT_OBSSTDAT = replace(M13_VISIT_OBSSTDAT, M13_VISIT_OBSSTDAT %in% as.Date(c("1907-07-07", "2007-07-07", "1909-07-07", "1909-09-09", "1905-05-05")), NA),
    AGE_IN_DAYS = round(as.numeric(difftime(M13_VISIT_OBSSTDAT, DOB_Date, units = "days")), 0),
    AGE_IN_WKS = round(as.numeric(difftime(M13_VISIT_OBSSTDAT, DOB_Date, units = "weeks")), 0), 
    PREG_GA_CALC = (round(as.numeric (difftime (DOB_Date, EDD_BOE, units = "days")), 0)) + 280,
    # Create an indicator variable for if birth MNH11 date is available
    M13DATE_MRSRE = ifelse(is.na(M13_VISIT_OBSSTDAT), 0, 1)
  ) %>%
  mutate(across(everything(), ~replace(., . %in% c(-7, -5), NA))) 


test <- allvisit_anthro %>% group_by(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
## SL: n = 519 duplicates in the allvisit_anthro dataset (including type_visit)

tvisit_measure <- allvisit_anthro %>%
  # Filter records where AGE_IN_DAYS is between 0 and 500 and SEX is not NA
  filter(AGE_IN_DAYS >= 0 & AGE_IN_DAYS < 500 & !is.na(SEX)) %>%
  mutate(
    # Handle negative AGE_IN_DAYS by setting them to NA
    AGE_IN_DAYS = ifelse(AGE_IN_DAYS < 0, NA, AGE_IN_DAYS),
    # Calculate postmenstrual age in days
    PMAGE_DAYS = ifelse(AGE_IN_DAYS < 0, NA, AGE_IN_DAYS + PREG_GA_CALC),
    # Calculate anthropometric z-scores
    with( ., anthro_zscores(sex = SEX, age = AGE_IN_DAYS, weight = WEIGHT, lenhei = LENGTH, headc = HC_FAORRES ))
  ) %>% 
  mutate(
    # Calculate weight-to-length ratio
    wlr = WEIGHT/(LENGTH/100),
    # Calculate length-for-age z-score using INTERGROWTH-21st standards for preterm infants
    ZLEN_IG = case_when(PRETERM == 1 & SEX == 1 & LENGTH > 0 ~ igprepost_lencm2zscore(PMAGE_DAYS, LENGTH, sex = "Male"),
                        PRETERM == 1 & SEX == 2 & LENGTH > 0 ~ igprepost_lencm2zscore(PMAGE_DAYS, LENGTH, sex = "Female"),
                        PRETERM %in% c(2, 3) ~ zlen,
                        TRUE ~ NA),
    
    # Calculate weight-for-age z-score using INTERGROWTH-21st standards for preterm infants
    ZWEI_IG = case_when(PRETERM == 1 & SEX == 1  & WEIGHT > 0 ~ igprepost_wtkg2zscore(PMAGE_DAYS, WEIGHT, sex = "Male"),
                        PRETERM == 1 & SEX == 2 & WEIGHT > 0 ~ igprepost_wtkg2zscore(PMAGE_DAYS, WEIGHT, sex = "Female"),
                        PRETERM %in% c(2, 3) ~ zwei,
                        TRUE ~ NA),
    
    # Use pre-calculated weight-for-length z-score
    ZWFL_IG = zwfl,
    
    # Calculate head circumference-for-age z-score using INTERGROWTH-21st standards for preterm infants
    ZHC_IG = case_when(PRETERM == 1 & SEX == 1  & HC_FAORRES > 0 ~ igprepost_hcircm2zscore(PMAGE_DAYS, HC_FAORRES, sex = "Male"),
                       PRETERM == 1 & SEX == 2 & HC_FAORRES > 0 ~ igprepost_hcircm2zscore(PMAGE_DAYS, HC_FAORRES, sex = "Female"),
                       PRETERM %in% c(2, 3) ~ zhc,
                       TRUE ~ NA)) %>% 
    mutate(
    # Round the z-scores to 2 decimal places
    ZLEN_IG = round(ZLEN_IG, 2),
    ZWEI_IG = round(ZWEI_IG, 2),
    ZWFL_IG = round(ZWFL_IG, 2),
    ZHC_IG = round(ZHC_IG, 2),
    
    # Create flags for extreme z-scores
    FLEN_IG = ifelse(abs(ZLEN_IG) > 6, 1, 0),
    FWEI_IG = ifelse(ZWEI_IG > 5 | ZWEI_IG < -6, 1, 0),
    FWFL_IG = fwfl,
    FHC_IG = ifelse(abs(ZHC_IG) > 5, 1, 0)
  ) %>% 
  
  # Rename M13_VISIT_OBSSTDAT to VISIT_DATE
  rename(VISIT_DATE = M13_VISIT_OBSSTDAT)

test <- tvisit_measure %>% group_by(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
## SL: n = 515 duplicates in the tvisit_measure dataset (including type_visit)

#prepare birth and visit dataframes with Z scores for merging 
for_merge_mnh11 <- tbirth_measure %>% 
  select(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT, VISIT_DATE, AGE_IN_DAYS, PREG_GA_CALC, AGE_IN_WKS, WEIGHT, LENGTH, HC_FAORRES, 
         LBW_CAT_ANY, PRETERMBIRTH_CAT, SGA_CAT, SGA_CENTILE, INF_JAUN_NON_SEV_ANY,INF_JAUN_SEV_24HR, INF_JAUN_SEV_GREATER_24HR,
         INF_SGA_PRETERM,INF_AGA_PRETERM,INF_SGA_TERM,INF_AGA_TERM,
         VITAL = INF_VITAL_MNH11, FLEN = flen, FWFL = fwfl, FWEI = fwei, ZLEN = zlen, FHC = fhc, ZWFL = zwfl, ZWEI = zwei, ZHC = zhc,
         ZLEN_IG, ZWEI_IG, ZWFL_IG, ZHC_IG, FLEN_IG, FWEI_IG, FWFL_IG, FHC_IG )

for_merge_mnh13 <- tvisit_measure %>% 
  select(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT, VISIT_DATE, AGE_IN_DAYS, PREG_GA_CALC, AGE_IN_WKS, WEIGHT, LENGTH, HC_FAORRES, 
         LBW_CAT_ANY, PRETERMBIRTH_CAT, SGA_CAT, SGA_CENTILE, INF_JAUN_NON_SEV_ANY,INF_JAUN_SEV_24HR, INF_JAUN_SEV_GREATER_24HR,
         INF_SGA_PRETERM,INF_AGA_PRETERM,INF_SGA_TERM,INF_AGA_TERM,
         VITAL = INF_VITAL_MNH13, FLEN = flen, FWFL = fwfl, FWEI = fwei, ZLEN = zlen, FHC = fhc, ZWFL = zwfl, ZWEI = zwei, ZHC = zhc,
         ZLEN_IG, ZWEI_IG, ZWFL_IG, ZHC_IG, FLEN_IG, FWEI_IG, FWFL_IG, FHC_IG )

#merge birth and visit dataframes with Z scores 
merged_anthro <- bind_rows(for_merge_mnh11, for_merge_mnh13) 

#these are all births, including those with incomplete measures
all_data_birth <-  allbirth_anthro %>%
  select(SITE, MOMID, PREGID, INFANTID, INF_VISIT_COMP = INF_VITAL_MNH11, VISIT_DATE = M11_VISIT_OBSSTDAT, 
         WEIGHT_MRSRE, LENGTH_MRSRE, HC_MRSRE, DATE_MRSRE = M11DATE_MRSRE, TYPE_VISIT, MNH09_HAVE, DOB, SEX)

#these are all visits, including those with incomplete measures
all_data_visit <-  allvisit_anthro %>%
  mutate(MNH09_HAVE = NA) %>%
  select(SITE, MOMID, PREGID, INFANTID, INF_VISIT_COMP = INF_VITAL_MNH13, WEIGHT_MRSRE, 
         VISIT_DATE = M13_VISIT_OBSSTDAT, DATE_MRSRE = M13DATE_MRSRE, LENGTH_MRSRE, HC_MRSRE, TYPE_VISIT, DOB, MNH09_HAVE, SEX)

#these are all visits and births present that have all the indicators of what is missing 
merged_visits <- bind_rows(all_data_birth, all_data_visit) 

#merge all the visits with all the data from calculated z scores
outcome_data_prep <- merged_visits %>% 
  left_join(merged_anthro, by = c("SITE", "MOMID", "PREGID", "INFANTID", "TYPE_VISIT", "VISIT_DATE")) 

test <- outcome_data_prep %>% group_by(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
## SL: n = 1935 duplicates in the outcome_data_prep dataset (including type_visit)

#Create the outcomes
outcome_all <- outcome_data_prep %>%
  mutate(
# WHO Standards
    UNDER_WEIGHT = case_when(
      is.na(ZWEI) | FWEI == 1 ~ 55, 
      ZWEI < -2.0 ~ 1,
      ZWEI >= -2.0 ~ 0  
    ),
    SEVERE_UNDER = case_when(
      is.na(ZWEI) | FWEI == 1 ~ 55,
      ZWEI < -3.0 ~ 1,
      ZWEI >= -3.0 ~ 0 
    ),
    STUNTING = case_when(
      is.na(ZLEN) | FLEN == 1 ~ 55,
      ZLEN < -2.0 ~ 1,
      ZLEN >= -2.0 ~ 0
    ),
    SEVERE_STUNTING = case_when(
      is.na(ZLEN) | FLEN == 1 ~ 55,
      ZLEN < -3.0 ~ 1,
      ZLEN >= -3.0 ~ 0  
    ),
    WASTING = case_when(
      is.na(ZWFL) | FWFL == 1 ~ 55,
      ZWFL < -2.0 ~ 1,
      ZWFL >= -2.0 ~ 0
    ),
    SEVERE_WASTING = case_when(
      is.na(ZWFL) | FWFL == 1 ~ 55,
      ZWFL < -3.0 ~ 1,
      ZWFL >= -3.0 ~ 0  
    ),
    OVERWEIGHT = case_when(
      is.na(ZWFL) | FWFL == 1 ~ 55,
      ZWFL > 2.0 ~ 1,
      ZWFL <= 2.0 ~ 0 
    ), 
    MICROCEPHALY = case_when(
      is.na(ZHC) | FHC == 1 ~ 55,
      ZHC < -3.0 ~ 1,
      ZHC >= -3.0 ~ 0 
    ),
    
#Intergrowth Standards
    UNDER_WEIGHT_IG = case_when(
      is.na(ZWEI_IG) | FWEI_IG == 1 ~ 55, 
      ZWEI_IG < -2.0 ~ 1,
      ZWEI_IG >= -2.0 ~ 0  
    ),
    SEVERE_UNDER_IG = case_when(
      is.na(ZWEI_IG) | FWEI_IG == 1 ~ 55,
      ZWEI_IG < -3.0 ~ 1,
      ZWEI_IG >= -3.0 ~ 0 
    ),
    STUNTING_IG = case_when(
      is.na(ZLEN_IG) | FLEN_IG == 1 ~ 55,
      ZLEN_IG < -2.0 ~ 1,
      ZLEN_IG >= -2.0 ~ 0
    ),
    SEVERE_STUNTING_IG = case_when(
      is.na(ZLEN_IG) | FLEN_IG == 1 ~ 55,
      ZLEN_IG < -3.0 ~ 1,
      ZLEN_IG >= -3.0 ~ 0  
    ),
    WASTING_IG = case_when(
      is.na(ZWFL_IG) | FWFL_IG == 1 ~ 55,
      ZWFL_IG < -2.0 ~ 1,
      ZWFL_IG >= -2.0 ~ 0
    ),
    SEVERE_WASTING_IG = case_when(
      is.na(ZWFL_IG) | FWFL_IG == 1 ~ 55,
      ZWFL_IG < -3.0 ~ 1,
      ZWFL_IG >= -3.0 ~ 0  
    ),
    OVERWEIGHT_IG = case_when(
      is.na(ZWFL_IG) | FWFL_IG == 1 ~ 55,
      ZWFL_IG > 2.0 ~ 1,
      ZWFL_IG <= 2.0 ~ 0 
    ), 
    MICROCEPHALY_IG = case_when(
      is.na(ZHC_IG) | FHC_IG == 1 ~ 55,
      ZHC_IG < -3.0 ~ 1,
      ZHC_IG >= -3.0 ~ 0 
    ),
    
    # Combined (ANY) Standards
    UNDER_WEIGHT_BOTH = case_when(
      (is.na(ZWEI) | is.na(ZWEI_IG)) | (FWEI == 1 | FWEI_IG == 1) ~ 55, 
      ZWEI < -2.0 & ZWEI_IG < -2.0 ~ 1,
      TRUE ~ 0  
    ),
    SEVERE_UNDER_BOTH = case_when(
      (is.na(ZWEI) | is.na(ZWEI_IG)) | (FWEI == 1 | FWEI_IG == 1) ~ 55,
      ZWEI < -3.0 & ZWEI_IG < -3.0 ~ 1,
      TRUE ~ 0 
    ),
    STUNTING_BOTH = case_when(
      (is.na(ZLEN) | is.na(ZLEN_IG)) | (FLEN == 1 | FLEN_IG == 1) ~ 55,
      ZLEN < -2.0 & ZLEN_IG < -2.0 ~ 1,
      TRUE ~ 0
    ),
    SEVERE_STUNTING_BOTH = case_when(
      (is.na(ZLEN) | is.na(ZLEN_IG)) | (FLEN == 1 | FLEN_IG == 1) ~ 55,
      ZLEN < -3.0 & ZLEN_IG < -3.0 ~ 1,
      TRUE ~ 0  
    ),
    WASTING_BOTH = case_when(
      (is.na(ZWFL) | is.na(ZWFL_IG)) | (FWFL == 1 | FWFL_IG == 1) ~ 55,
      ZWFL < -2.0 & ZWFL_IG < -2.0 ~ 1,
      TRUE ~ 0
    ),
    SEVERE_WASTING_BOTH = case_when(
      (is.na(ZWFL)| is.na(ZWFL_IG)) | (FWFL == 1 | FWFL_IG == 1) ~ 55,
      ZWFL < -3.0 & ZWFL_IG < -3.0 ~ 1,
      TRUE ~ 0  
    ),
    OVERWEIGHT_BOTH = case_when(
      (is.na(ZWFL) | is.na(ZWFL_IG)) | (FWFL == 1 | FWFL_IG == 1) ~ 55,
      ZWFL > 2.0 & ZWFL_IG > 2.0 ~ 1,
      TRUE ~ 0 
    ), 
    MICROCEPHALY_BOTH = case_when(
      (is.na(ZHC) | is.na(ZHC_IG)) | (FHC == 1 | FHC_IG == 1) ~ 55,
      ZHC < -3.0 & ZHC_IG < -3.0 ~ 1,
      TRUE ~ 0 
    ),
    #are all the measures complete? 1 - Yes, 0 - No
    MSRE_COMPLT = case_when( LENGTH_MRSRE == 1 & HC_MRSRE == 1 & WEIGHT_MRSRE == 1 & DATE_MRSRE == 1 & 
                               !is.na(AGE_IN_DAYS) & !is.na(SEX) ~ 1, TRUE ~ 0 ),
   
   #are any of the z-scores not calculated? Yes - 1, No - 0 
    FALL = case_when ((is.na(ZHC) | is.na(ZWFL) | is.na(ZWEI) | is.na(ZLEN)) |
                     (is.na(ZHC_IG) | is.na(ZWFL_IG) | is.na(ZWEI_IG) | is.na(ZLEN_IG)) &
                      MSRE_COMPLT == 1 ~ 1, 
                     
                     (!is.na(ZHC) & !is.na(ZWFL) & !is.na(ZWEI) & !is.na(ZLEN) &
                      !is.na(ZHC_IG) & !is.na(ZWFL_IG) & !is.na(ZWEI_IG) & !is.na(ZLEN_IG)) &
                      MSRE_COMPLT == 1 ~ 0,
                      TRUE ~ 55), 
   
    #so we want to create an indicator for where there are biologically accurate Z-scores and where all measures are complete
    NON_TRAJ_FLAG = case_when ((FHC == 0 & FLEN == 0 & FWFL == 0 & FWEI == 0 & FALL == 0) & 
                               (FHC_IG == 0 & FLEN_IG == 0 & FWFL_IG == 0 & FWEI_IG == 0 & FALL == 0) & 
                                MSRE_COMPLT == 1 ~ 1,
                               
                               (FHC == 1 | FLEN == 1 | FWEI == 1 | FWFL == 1 | FALL == 1) & 
                               (FHC_IG == 1 | FLEN_IG == 1 | FWEI_IG == 1 | FWFL_IG == 1 | FALL == 1) & 
                                MSRE_COMPLT == 1 ~ 0,
                                TRUE ~ 55),
    
    #create an indicator for where at least one measure is flagged
    ONE_MSRE_FLAG = case_when ((FHC == 1 | FLEN == 1 | FWEI == 1 | FWFL == 1 | FALL == 1) & 
                               (FHC_IG == 1 | FLEN_IG == 1 | FWEI_IG == 1 | FWFL_IG == 1 | FALL == 1) &
                                MSRE_COMPLT == 1 ~ 1, 
                               
                               (FHC == 0 & FLEN == 0 & FWFL == 0 & FWEI == 0 & FALL == 0) & 
                               (FHC_IG == 0 & FLEN_IG == 0 & FWFL_IG == 0 & FWEI_IG == 0 & FALL == 0) & 
                                MSRE_COMPLT == 1 ~ 0,
                               TRUE ~ 55),

AGE_IN_MTH = floor(AGE_IN_DAYS / 30.4375))

test <- outcome_all %>% group_by(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
## SL: n = 1935 duplicates in the outcome_all dataset (including type_visit)

# write.csv(outcome_all, paste0(path_to_save, "INF_GROWTH_LONG" ,".csv"), row.names=FALSE)
# write.csv(outcome_all, paste0(path_to_tnt, "INF_GROWTH_LONG" ,".csv"), na="", row.names=FALSE)


# test <- outcome_all %>%  filter(FHC == 0 & FLEN == 0 & FWFL == 0 & FWEI == 0) 
# test1 <- outcome_all %>%  filter(FHC == 0 & FLEN == 0 & FWEI == 0) 
# test2 <- outcome_all %>%  filter(FHC == 1 | FLEN == 1 | FWEI == 1 | FWFL == 1) 
# test3 <- outcome_all %>% filter(FHC == 1 & FLEN == 1 & FWEI == 1)

#So to create a wide format of the data for three time points we separate the dataframe
#choose measurement at the latest age for month one and six if there are multiple/duplicates

#1- at birth  
birth_filtered <- outcome_all %>%  filter(TYPE_VISIT == 6) 

test <- birth_filtered %>% group_by(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
## SL: n = 816 duplicates in the birth_filtered dataset (including type_visit)

#2- at one month
# Filter, group, and select specific columns
one_month_filtered <- outcome_all %>%
  filter(AGE_IN_DAYS >= 21 & AGE_IN_DAYS <= 42) %>%
  group_by(MOMID, PREGID, INFANTID, SITE) %>%
  filter(AGE_IN_DAYS == max(AGE_IN_DAYS)) %>%
  ungroup() %>%
  select(
    SITE, MOMID, PREGID, INFANTID, INF_VISIT_COMP, TYPE_VISIT, VISIT_DATE, AGE_IN_DAYS, AGE_IN_WKS, AGE_IN_MTH, 
    WEIGHT_MRSRE, LENGTH_MRSRE, HC_MRSRE, WEIGHT, LENGTH, HC_FAORRES, 
    VITAL, FLEN, FWFL, FWEI, MSRE_COMPLT, NON_TRAJ_FLAG, ONE_MSRE_FLAG, ZLEN, FHC, ZWFL, ZWEI, ZHC, 
    ZLEN_IG, ZWEI_IG, ZWFL_IG, ZHC_IG, FLEN_IG, FWEI_IG, FWFL_IG, FHC_IG, 
    UNDER_WEIGHT, SEVERE_UNDER, STUNTING, SEVERE_STUNTING, WASTING, SEVERE_WASTING, OVERWEIGHT, MICROCEPHALY,
    UNDER_WEIGHT_IG, SEVERE_UNDER_IG, STUNTING_IG, SEVERE_STUNTING_IG, 
    WASTING_IG, SEVERE_WASTING_IG, OVERWEIGHT_IG, MICROCEPHALY_IG, 
    UNDER_WEIGHT_BOTH, SEVERE_UNDER_BOTH, STUNTING_BOTH, SEVERE_STUNTING_BOTH, 
    WASTING_BOTH, SEVERE_WASTING_BOTH, OVERWEIGHT_BOTH, MICROCEPHALY_BOTH
  )

test <- one_month_filtered %>% group_by(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
## SL: n = 184 duplicates in the one_month_filtered dataset (including type_visit)


# Rename columns
names(one_month_filtered) <- c(
  "SITE", "MOMID", "PREGID", "INFANTID", "INF_VISIT_COMP_M1", "TYPE_VISIT_M1", "VISIT_DATE_M1", 
  "AGE_IN_DAYS_M1", "AGE_IN_WKS_M1", "AGE_IN_MTH_M1",
  "WEIGHT_MRSRE_M1", "LENGTH_MRSRE_M1", "HC_MRSRE_M1", "WEIGHT_M1", 
  "LENGTH_M1", "HC_FAORRES_M1", "VITAL_M1", "FLEN_M1", "FWFL_M1", "FWEI_M1", "MSRE_COMPLT_M1", "NON_TRAJ_FLAG_M1", 
  "ONE_MSRE_FLAG_M1", "ZLEN_M1", "FHC_M1", "ZWFL_M1", "ZWEI_M1", "ZHC_M1", 
  "ZLEN_IG_M1", "ZWEI_IG_M1", "ZWFL_IG_M1", "ZHC_IG_M1", "FLEN_IG_M1", "FWEI_IG_M1", "FWFL_IG_M1", "FHC_IG_M1", 
  "UNDER_WEIGHT_M1", "SEVERE_UNDER_M1", "STUNTING_M1", "SEVERE_STUNTING_M1", 
  "WASTING_M1", "SEVERE_WASTING_M1", "OVERWEIGHT_M1", "MICROCEPHALY_M1", 
  "UNDER_WEIGHT_IG_M1", "SEVERE_UNDER_IG_M1", "STUNTING_IG_M1", "SEVERE_STUNTING_IG_M1", 
  "WASTING_IG_M1", "SEVERE_WASTING_IG_M1", "OVERWEIGHT_IG_M1", "MICROCEPHALY_IG_M1", 
  "UNDER_WEIGHT_BOTH_M1", "SEVERE_UNDER_BOTH_M1", "STUNTING_BOTH_M1", "SEVERE_STUNTING_BOTH_M1", 
  "WASTING_BOTH_M1", "SEVERE_WASTING_BOTH_M1", "OVERWEIGHT_BOTH_M1", "MICROCEPHALY_BOTH_M1"
)

#3- at six months
six_months_filtered <- outcome_all %>%
  filter(AGE_IN_DAYS >= 171 & AGE_IN_DAYS <= 222) %>%
  group_by(MOMID, PREGID, INFANTID, SITE) %>%
  filter(AGE_IN_DAYS == max(AGE_IN_DAYS)) %>%
  ungroup() %>%
  select(
      SITE, MOMID, PREGID, INFANTID, INF_VISIT_COMP, TYPE_VISIT, VISIT_DATE, AGE_IN_DAYS, AGE_IN_WKS, AGE_IN_MTH, 
      WEIGHT_MRSRE, LENGTH_MRSRE, HC_MRSRE, WEIGHT, LENGTH, HC_FAORRES, 
      VITAL, FLEN, FWFL, FWEI, MSRE_COMPLT, NON_TRAJ_FLAG, ONE_MSRE_FLAG, ZLEN, FHC, ZWFL, ZWEI, ZHC, 
      ZLEN_IG, ZWEI_IG, ZWFL_IG, ZHC_IG, FLEN_IG, FWEI_IG, FWFL_IG, FHC_IG, 
      UNDER_WEIGHT, SEVERE_UNDER, STUNTING, SEVERE_STUNTING, WASTING, SEVERE_WASTING, OVERWEIGHT, MICROCEPHALY,
      UNDER_WEIGHT_IG, SEVERE_UNDER_IG, STUNTING_IG, SEVERE_STUNTING_IG, 
      WASTING_IG, SEVERE_WASTING_IG, OVERWEIGHT_IG, MICROCEPHALY_IG, 
      UNDER_WEIGHT_BOTH, SEVERE_UNDER_BOTH, STUNTING_BOTH, SEVERE_STUNTING_BOTH, 
      WASTING_BOTH, SEVERE_WASTING_BOTH, OVERWEIGHT_BOTH, MICROCEPHALY_BOTH
    )

test <- six_months_filtered %>% group_by(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
## SL: n = 28 duplicates in the six_months_filtered dataset (including type_visit)

    # Rename columns
names(six_months_filtered) <- c(
      "SITE", "MOMID", "PREGID", "INFANTID", "INF_VISIT_COMP_M6", "TYPE_VISIT_M6", "VISIT_DATE_M6", 
      "AGE_IN_DAYS_M6", "AGE_IN_WKS_M6", "AGE_IN_MTH_M6", 
      "WEIGHT_MRSRE_M6", "LENGTH_MRSRE_M6", "HC_MRSRE_M6", "WEIGHT_M6", 
      "LENGTH_M6", "HC_FAORRES_M6", "VITAL_M6", "FLEN_M6", "FWFL_M6", "FWEI_M6", "MSRE_COMPLT_M6", "NON_TRAJ_FLAG_M6", 
      "ONE_MSRE_FLAG_M6", "ZLEN_M6", "FHC_M6", "ZWFL_M6", "ZWEI_M6", "ZHC_M6", 
      "ZLEN_IG_M6", "ZWEI_IG_M6", "ZWFL_IG_M6", "ZHC_IG_M6", "FLEN_IG_M6", "FWEI_IG_M6", "FWFL_IG_M6", "FHC_IG_M6", 
      "UNDER_WEIGHT_M6", "SEVERE_UNDER_M6", "STUNTING_M6", "SEVERE_STUNTING_M6", 
      "WASTING_M6", "SEVERE_WASTING_M6", "OVERWEIGHT_M6", "MICROCEPHALY_M6", 
      "UNDER_WEIGHT_IG_M6", "SEVERE_UNDER_IG_M6", "STUNTING_IG_M6", "SEVERE_STUNTING_IG_M6", 
      "WASTING_IG_M6", "SEVERE_WASTING_IG_M6", "OVERWEIGHT_IG_M6", "MICROCEPHALY_IG_M6", 
      "UNDER_WEIGHT_BOTH_M6", "SEVERE_UNDER_BOTH_M6", "STUNTING_BOTH_M6", "SEVERE_STUNTING_BOTH_M6", 
      "WASTING_BOTH_M6", "SEVERE_WASTING_BOTH_M6", "OVERWEIGHT_BOTH_M6", "MICROCEPHALY_BOTH_M6"
    )

# Join birth_filtered and one_month_filtered
combined_df <- birth_filtered %>%
  left_join(one_month_filtered, by = c("MOMID", "PREGID", "INFANTID", "SITE"))

test <- combined_df %>% group_by(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
## SL: n = 1288 duplicates in the combined_df dataset (including type_visit)

# Join the resulting dataframe with six_months_filtered
wide_infant_trajectories <- combined_df %>%
  left_join(six_months_filtered, by = c("MOMID", "PREGID", "INFANTID", "SITE"))


test <- wide_infant_trajectories %>% group_by(SITE, MOMID, PREGID, INFANTID ,TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
## SL: n = 1576 duplicates in the wide_infant_trajectories dataset (including type_visit)


# export data 
# write.csv(wide_infant_trajectories, paste0(path_to_save, "INF_GROWTH_WIDE" ,".csv"), row.names=FALSE)
# write.csv(wide_infant_trajectories, paste0(path_to_tnt, "INF_GROWTH_WIDE" ,".csv"), na="", row.names=FALSE)


### TESTING WITH REPORT TABLES TO MAKE SURE I GOT THE SAME NUMBERS 
infant_trajectory = wide_infant_trajectories

inf_traj_birth_tab <- infant_trajectory %>% 
  ## If India-SAS doesn't have data, add empty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    # data completeness
    
    "Denominator ^a^" =  paste0(
      format(sum(TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2)),
    
    "Missing birth date (MNH09)" = paste0(
      format(sum(is.na(DOB) & TYPE_VISIT == 6), nsmall = 0, digits = 2),
      " (",
      format(round(sum(is.na(DOB) & TYPE_VISIT == 6)/sum(TYPE_VISIT==6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Missing visit date at birth (MNH11)" = paste0(
      format(sum(is.na(VISIT_DATE) & TYPE_VISIT == 6), nsmall = 0, digits = 2),
      " (",
      format(round(sum(is.na(VISIT_DATE) & TYPE_VISIT == 6)/sum(TYPE_VISIT==6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Missing information on infant sex" = paste0(
      format(sum(is.na(SEX) & TYPE_VISIT == 6), nsmall = 0, digits = 2),
      " (",
      format(round(sum(is.na(SEX) & TYPE_VISIT == 6)/sum(TYPE_VISIT==6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Missing information on infant length" = paste0(
      format(sum(LENGTH_MRSRE == 0 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(LENGTH_MRSRE == 0 & TYPE_VISIT == 6, na.rm = TRUE)/sum(TYPE_VISIT==6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Missing information on infant weight" = paste0(
      format(sum(WEIGHT_MRSRE == 0 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(WEIGHT_MRSRE == 0 & TYPE_VISIT == 6, na.rm = TRUE)/sum(TYPE_VISIT==6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Missing information on head circumference" = paste0(
      format(sum(HC_MRSRE == 0 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(HC_MRSRE == 0 & TYPE_VISIT == 6, na.rm = TRUE)/sum(TYPE_VISIT==6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Missing at least one anthropometry measurement" = paste0(
      format(sum(MSRE_COMPLT == 0 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(MSRE_COMPLT == 0 & TYPE_VISIT == 6, na.rm = TRUE)/sum(TYPE_VISIT==6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),    
    
    "Invalid Z score ^b^ ^c^" = paste0(
      format(sum(ONE_MSRE_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(ONE_MSRE_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(TYPE_VISIT==6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),    
    
    ## infant trajectories at birth outcome (WHO Standards)
    "Denominator ^d^" =  paste0(
      format(sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2)),
    
    "Underweight (WAZ < -2)" = paste0(
      format(sum(UNDER_WEIGHT == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(UNDER_WEIGHT == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),    
    
    "Severe underweight (WAZ <-3)" = paste0(
      format(sum(SEVERE_UNDER == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SEVERE_UNDER == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Wasting (WFL < -2)" = paste0(
      format(sum(WASTING == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(WASTING == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Severe wasting (WFL < -3)" = paste0(
      format(sum(SEVERE_WASTING == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SEVERE_WASTING == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Stunting (LAZ <- 2)" = paste0(
      format(sum(STUNTING == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(STUNTING == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Severe Stunting (LAZ <- 3)" = paste0(
      format(sum(SEVERE_STUNTING == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SEVERE_STUNTING == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Overweight (WFL > 3)" = paste0(
      format(sum(OVERWEIGHT == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(OVERWEIGHT == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Microcephaly (HCZ < -3)" = paste0(
      format(sum(MICROCEPHALY == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(MICROCEPHALY == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    ## infant trajectories at birth outcome (InterGrowth Adjusted Standards for Preterm)
    "Underweight (WAZ < -2) " = paste0(
      format(sum(UNDER_WEIGHT_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(UNDER_WEIGHT_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),    
    
    "Severe underweight (WAZ <-3) " = paste0(
      format(sum(SEVERE_UNDER_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SEVERE_UNDER_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Wasting (WFL < -2) " = paste0(
      format(sum(WASTING_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(WASTING_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Severe wasting (WFL < -3) " = paste0(
      format(sum(SEVERE_WASTING_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SEVERE_WASTING_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Stunting (LAZ <- 2) " = paste0(
      format(sum(STUNTING_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(STUNTING_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Severe Stunting (LAZ <- 3) " = paste0(
      format(sum(SEVERE_STUNTING_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SEVERE_STUNTING_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Overweight (WFL > 3) " = paste0(
      format(sum(OVERWEIGHT_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(OVERWEIGHT_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Microcephaly (HCZ < -3) " = paste0(
      format(sum(MICROCEPHALY_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(MICROCEPHALY_IG == 1 & MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)/sum(MSRE_COMPLT == 1 & NON_TRAJ_FLAG == 1 & TYPE_VISIT == 6, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")")
    
    
  ) %>% 
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) 


