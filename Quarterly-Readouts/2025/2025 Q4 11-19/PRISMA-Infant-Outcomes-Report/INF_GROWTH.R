#*****************************************************************************
#* PRISMA Infant Outcomes: 
#* Drafted: 01 June 2024, Precious Williams `williams_pj@gwu.edu`
#* Last updated: 12 November 2025

# 9. Infant Growth Outcomes

# The R package "anthro"
# It includes functions to calculate z-scores and prevalence estimates (and CIs), 
# It provides results for the indicators: 
# length/height-for-age, weight-for-age, weight-for-length, weight-for-height, 
# body mass index-for-age, head circumference-for-age, arm circumference-for-age, 
# The package is available in the CRAN repository at https://CRAN.R-project.org/package=anthro.

#*****************************************************************************
# clear environment 
#rm(list = ls())

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
library(gigs)
#*****************************************************************************
#* load data
#*****************************************************************************

## UPDATE EACH RUN ## 
UploadDate = "2025-10-31"

#Set your main directory 
path_to_data <- paste0("~/Analysis/Merged_data/", UploadDate)
path_to_raw <- paste0("Z:/Stacked Data/", UploadDate, "/")

# set path to save 
path_to_save <- paste0("~/Analysis/Infant-Constructed-Variables/data/")
path_to_save_figures <- paste0("~/Analysis/Infant-Constructed-Variables/output/")

path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")


# # import forms

mnh09 <- read.csv (paste0(path_to_raw, "/mnh09_merged.csv"))

mnh11 <- read.csv (paste0(path_to_raw, "/mnh11_merged.csv"))

mnh13 <- read.csv (paste0(path_to_raw, "/mnh13_merged.csv"))

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

# mat_end_point <- read_dta (paste0(path_to_tnt, "/MAT_ENDPOINTS.dta"))
gc()

mat_enroll <- read.csv (paste0(path_to_tnt, "/MAT_ENROLL.csv"))
inf_outcomes <- read.csv(paste0(path_to_tnt, "/INF_OUTCOMES.csv"))

# pull all enrolled participants
enrolled_ids <- mat_enroll %>% 
  filter(ENROLL == 1) %>% 
  select(SITE, MOMID, PREGID, ENROLL) %>% 
  distinct(SITE, MOMID, PREGID, .keep_all = TRUE)

enrolled_ids_vec <- as.vector(enrolled_ids$PREGID)

#pull pregnancy end ga
# end_preg <- mat_end_point %>% 
#   select(SITE, MOMID, PREGID, PREG_END_GA)  %>% 
#   distinct(SITE, MOMID, PREGID, .keep_all = TRUE)

#pull pregnancy estimated due date
mat_edd <- mat_enroll %>% 
  select(SITE, MOMID, PREGID, EDD_BOE)  %>% 
  distinct(SITE, MOMID, PREGID, .keep_all = TRUE)

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
  mutate(DELIV_DSSTDAT_INF1 = replace(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF1 %in% c(ymd("1907-07-07"), ymd("1909-07-07"),ymd("1905-05-05"), ymd("2007-07-07")), NA),
         DELIV_DSSTDAT_INF2 = replace(DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF2 %in% c(ymd("1907-07-07"), ymd("1909-07-07"),ymd("1905-05-05"), ymd("2007-07-07")), NA),
         DELIV_DSSTDAT_INF3 = replace(DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF3 %in% c(ymd("1907-07-07"), ymd("1909-07-07"),ymd("1905-05-05"), ymd("2007-07-07")), NA),
         DELIV_DSSTDAT_INF4 = replace(DELIV_DSSTDAT_INF4, DELIV_DSSTDAT_INF4 %in% c(ymd("1907-07-07"), ymd("1909-07-07"),ymd("1905-05-05"), ymd("2007-07-07")), NA)) %>%
  
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
                                           DELIV_DSSTDAT_INF4))  %>% 
  filter (!is.na(INFANTID_INF1))  %>% 
  distinct(SITE, MOMID, PREGID, .keep_all = TRUE)

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
  filter(INFANTID != "" & INFANTID != "n/a" & !is.na(INFANTID)) %>% 
  
  # generate indicator variable for having a birth outcome; if a birth outcome has been reported (BIRTH_DSTERM =1 or 2), then BIRTH_OUTCOME_REPORTED ==1
  mutate(BIRTH_OUTCOME_REPORTED = ifelse(BIRTH_DSTERM == 1 | BIRTH_DSTERM == 2, 1, 0),
         SEX = case_when( 
           SEX %in% c("M", "1") ~ 1,
           SEX %in% c("F", "2") ~ 2),
         MNH09_HAVE = 1)  %>%
  
  # only want those who have had a birth outcome 
  filter(BIRTH_OUTCOME_REPORTED == 1 & PREGID %in% enrolled_ids_vec )  %>% 
  
  distinct(SITE, PREGID, INFANTID, .keep_all = TRUE)

#read in infant outcomes and classify PRETERM - as any preterm
inf_living <- inf_outcomes %>% filter(LIVEBIRTH == 1) %>%
  select (SITE, PREGID, INFANTID, LBW_CAT_ANY, PRETERMBIRTH_CAT, SGA_CENTILE, SGA_CAT, INF_JAUN_NON_SEV_ANY, 
          INF_JAUN_SEV_24HR, INF_JAUN_SEV_GREATER_24HR, INF_SGA_PRETERM,INF_AGA_PRETERM,INF_SGA_TERM,INF_AGA_TERM) %>% 
  mutate (PRETERM = case_when( PRETERMBIRTH_CAT %in% c (12,13,14,15) ~ 1, ## any preterm
                               PRETERMBIRTH_CAT == 11 ~ 2, ## term (37 to <41 wks)
                               PRETERMBIRTH_CAT == 10 ~ 3, ## postterm (>= 41 wks)
                               TRUE ~ 55),
            TERM_GA = case_when(
              PRETERMBIRTH_CAT %in% c(12, 13, 14, 15) & SGA_CAT %in% c(11, 12) ~ "Preterm-SGA",
              PRETERMBIRTH_CAT %in% c(12, 13, 14, 15) & SGA_CAT %in% c(13, 14) ~ "Preterm-AGA",
              PRETERMBIRTH_CAT %in% c(10, 11) & SGA_CAT %in% c(11, 12) ~ "Term-SGA",
              PRETERMBIRTH_CAT %in% c(10, 11) & SGA_CAT %in% c(13, 14) ~ "Term-AGA",
              TRUE ~ NA_character_ # Use NA for unknowns
            )
          ) %>%
  distinct(SITE, PREGID, INFANTID, .keep_all = TRUE)

# only want those who have had a live birth outcome 
living_infants <- infant_dob %>%
  right_join(inf_living, by = c("SITE", "INFANTID", "PREGID")) %>%
  distinct(SITE, PREGID, INFANTID, .keep_all = TRUE)

# only want those who have had a live birth outcome 
deceased_infants <- infant_dob %>% filter (BIRTH_DSTERM == 2) 

# Create birth weight dataframe to merge with MNH09 
mnh11_constructed <- mnh11 %>% 
  mutate(
    # Determine PRISMA birthweight
    WEIGHT_PRISMA = case_when(
      BW_FAORRES > 0 & BW_EST_FAORRES >= 0 & BW_EST_FAORRES < 72 ~ BW_FAORRES,  # Measured within 72h
      BW_FAORRES > 0 & (is.na(BW_EST_FAORRES) | BW_EST_FAORRES %in% c(-5, -7)) ~ BW_FAORRES,  # No valid time, but BW is usable
      BW_FAORRES > 0 & BW_EST_FAORRES >= 72 ~ -5,  # Measured too late
      BW_FAORRES <= 0 | is.na(BW_FAORRES) ~ -5,  # Invalid or missing BW
      TRUE ~ -5
    ),
    
    # Choose final birthweight (PRISMA preferred, fallback to facility)
    WEIGHT_ANY = case_when(
      WEIGHT_PRISMA <= 0 & BW_FAORRES_REPORT > 0 ~ BW_FAORRES_REPORT,  # No usable PRISMA, use facility
      WEIGHT_PRISMA < 0 & BW_EST_FAORRES >= 72 & BW_FAORRES_REPORT > 0 ~ BW_FAORRES_REPORT,  # PRISMA too late, fallback to facility
      WEIGHT_PRISMA < 0 & (BW_FAORRES_REPORT <= 0 | is.na(BW_FAORRES_REPORT)) ~ -5,  # No usable weight
      TRUE ~ WEIGHT_PRISMA  # Use PRISMA if valid
    ),
    
    M11_VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
    M11_VISIT_OBSSTDAT = replace(M11_VISIT_OBSSTDAT, M11_VISIT_OBSSTDAT %in% as.Date(c("1907-07-07", "2007-07-07", "1909-07-07", "1909-09-09", "1905-05-05")), NA),
    
    # Create an indicator variable for if birth MNH11 date is available
    M11_DATE_MRSRE = ifelse(is.na(M11_VISIT_OBSSTDAT), 0, 1),
    
    # Create an indicator variable for if birth weight is available
    WEIGHT_MRSRE = ifelse(WEIGHT_ANY > 0 & !is.na (WEIGHT_ANY), 1, 0)) %>%
    
   rowwise() %>%
    mutate(
      LENGTH = {
        values <- c_across(c(LENGTH_FAORRES_1, LENGTH_FAORRES_2, LENGTH_FAORRES_3))
        valid_values <- values[!is.na(values) & values > 0]
        if (length(valid_values) > 0) {
          round(mean(valid_values), 2)
        } else {
          -5
        }
      }
    ) %>%
    mutate(
      HC_FAORRES = {
        values <- c_across(c(HC_FAORRES_1, HC_FAORRES_2, HC_FAORRES_3))
        valid_values <- values[!is.na(values) & values > 0]
        if (length(valid_values) > 0) {
          round(mean(valid_values), 2)
        } else {
          -5
        }
      }
    ) %>%
  ungroup() %>%
  
  mutate (
  # Create an indicator variable for if birth length is available
  LENGTH_MRSRE = ifelse(LENGTH > 0 , 1, 0),
  # Create an indicator variable for if birth length is available
  HC_MRSRE = ifelse(HC_FAORRES > 0 , 1, 0), 
  # Create an indicator variable for if they have MNH11 form
  MNH11_HAVE = 1, 
  TYPE_VISIT = 6 ) %>% 
  
  #only keep living infants
  filter (INF_VITAL_MNH11 == 1 ) %>%
  mutate(
    completeness_score = WEIGHT_MRSRE + LENGTH_MRSRE + HC_MRSRE + M11_DATE_MRSRE
  ) %>%
  arrange(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT, desc(completeness_score), M11_VISIT_OBSSTDAT) %>%
  # Keep only the best record per infant
  group_by(SITE, PREGID, INFANTID, TYPE_VISIT) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  # Final variable selection
  select(
    SITE, MOMID, PREGID, INFANTID, M11_VISIT_OBSSTDAT, MNH11_HAVE,
    WEIGHT_PRISMA, WEIGHT_ANY, WEIGHT_MRSRE, BW_EST_FAORRES,
    LENGTH, LENGTH_FAORRES_1, LENGTH_FAORRES_2, LENGTH_FAORRES_3, LENGTH_MRSRE,
    HC_FAORRES, HC_FAORRES_1, HC_FAORRES_2, HC_FAORRES_3, HC_MRSRE,
    SEX_INF, INF_VITAL_MNH11, M11_DATE_MRSRE, TYPE_VISIT
  )

#Create a dataset which joins MNH09 & MNH11 together 
allbirth_anthro <- mnh11_constructed %>% 
  filter (PREGID %in% enrolled_ids_vec)  %>%
  right_join(living_infants, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  left_join (mat_edd, by = c("SITE", "MOMID", "PREGID")) %>% 
  #left_join (end_preg, by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate( SEX_MNH09 = SEX,
          SEX_MNH11 = replace(SEX_INF, SEX_INF %in% c( 55, 77, 99), NA),
  ) %>% 
  mutate( 
    DOB_Date = as.Date(trunc(DOB, 'days')), 
    AGE_IN_DAYS = round(as.numeric (difftime (M11_VISIT_OBSSTDAT, DOB_Date, units = "days")), 0),
    AGE_IN_WKS = round(as.numeric (difftime  (M11_VISIT_OBSSTDAT, DOB_Date, units = "weeks")), 0),
    WEIGHT = case_when(!is.na(WEIGHT_ANY) & WEIGHT_ANY > 0 ~ round(WEIGHT_ANY / 1000, 2),
                    TRUE ~ NA),
    AGE_IN_DAYS = ifelse ((AGE_IN_DAYS > 5 | AGE_IN_DAYS < 0 ), NA, AGE_IN_DAYS),
    PREG_GA_CALC = (round(as.numeric (difftime (DOB_Date, EDD_BOE, units = "days")), 0)) + 280,
    TYPE_VISIT = 6 #basically since we assume this is all birth dataset
    ) %>% 
  select(SITE, MOMID, PREGID, INFANTID, AGE_IN_DAYS,  M11_VISIT_OBSSTDAT, 
         DOB, DOB_Date, EDD_BOE, PREG_GA_CALC, AGE_IN_DAYS, everything()) %>% 
  distinct(SITE, PREGID, INFANTID, TYPE_VISIT, .keep_all = TRUE) 


# test <- allbirth_anthro %>% group_by(SITE, MOMID, PREGID, INFANTID) %>% mutate(n=n()) %>% filter(n>1)
# dim(test)

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
    # Get a GIGS package sex classification
    sex_gigs = if_else(SEX == 1, "M", "F")) %>%
    # Calculate z-score using INTERGROWTH-21st standards
    # --- INTERGROWTH EXTENDED (gigs) ---
  # --- INTERGROWTH EXTENDED (gigs) ---
  rowwise() %>%
  mutate(
    ZLEN_IG = if (!is.na(PREG_GA_CALC) && !is.na(LENGTH) && 
      PREG_GA_CALC >= 154 && PREG_GA_CALC <= 314 && LENGTH > 0
    ) {
      value2zscore(y = LENGTH, x = PREG_GA_CALC, sex = sex_gigs,
                   family = "ig_nbs_ext", acronym = "lfga")
    } else {
      NA_real_
    },
    
    ZWEI_IG = if ( !is.na(PREG_GA_CALC) && !is.na(WEIGHT) && 
      PREG_GA_CALC >= 154 && PREG_GA_CALC <= 314 && WEIGHT > 0
    ) {
      value2zscore(y = WEIGHT, x = PREG_GA_CALC, sex = sex_gigs,
                   family = "ig_nbs_ext", acronym = "wfga")
    } else {
      NA_real_
    },
    
    ZHC_IG = if (!is.na(PREG_GA_CALC) &&  !is.na(HC_FAORRES) && 
      PREG_GA_CALC >= 154 && PREG_GA_CALC <= 314 && HC_FAORRES > 0
    ) {
      value2zscore(y = HC_FAORRES, x = PREG_GA_CALC, sex = sex_gigs,
                   family = "ig_nbs_ext", acronym = "hcfga")
    } else {
      NA_real_
    }
  ) %>%
  ungroup() %>% 
      mutate(
    
    # Calculate weight-for-length z-score using INTERGROWTH-21st standards
    ZWFL_IG = case_when(SEX == 1  & WEIGHT > 0 ~ igb_wlr2zscore(PREG_GA_CALC, wlr, sex = "Male"),
                        SEX == 2 & WEIGHT > 0 ~ igb_wlr2zscore(PREG_GA_CALC, wlr, sex = "Female"),
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


# query <- allbirth_anthro %>% filter (AGE_IN_DAYS > 5 | AGE_IN_DAYS < 0) %>% 
#   mutate(PSTAFF_BIRTH = case_when(WEIGHT_MRSRE == 1 & WEIGHT_PRISMA > 0 ~ 1,
#                                   WEIGHT_MRSRE == 1 & WEIGHT_PRISMA < 0 & WEIGHT_ANY > 0 ~ 0,
#                                   TRUE ~ 55))

# Create mnh13_sub dataframe with necessary calculations and selections
mnh13_constructed <- mnh13 %>% 
  # Keep only living infants and valid visit types
  filter(INF_VITAL_MNH13 == 1) %>%
  
  # Join DOB from living_infants
  left_join(
    living_infants %>% select(SITE, MOMID, PREGID, INFANTID, DOB),
    by = c("SITE", "MOMID", "PREGID", "INFANTID")
  ) %>%
  
  # Clean and compute variables
  mutate(
    DOB_Date = as.Date(trunc(DOB, 'days')), 
    M13_VISIT_OBSSTDAT = ymd(VISIT_OBSSTDAT),  # ensures consistent Date format
    M13_VISIT_OBSSTDAT = replace(
      M13_VISIT_OBSSTDAT,
      M13_VISIT_OBSSTDAT %in% as.Date(c("1907-07-07", "2007-07-07", "1909-07-07", "1909-09-09", "1905-05-05")),
      NA
    ),
    AGE_IN_DAYS = round(as.numeric(difftime(M13_VISIT_OBSSTDAT, DOB_Date, units = "days")), 0)
  ) %>%
  rowwise() %>%
  mutate(
    LENGTH = {
      values <- c_across(c(LENGTH_PERES_1, LENGTH_PERES_2, LENGTH_PERES_3))
      valid_values <- values[!is.na(values) & values > 0]
      if (length(valid_values) > 0) {
        round(mean(valid_values), 2)
      } else {
        -5
      }
    },
    HC_FAORRES = {
      values <- c_across(c(HC_PERES_1, HC_PERES_2, HC_PERES_3))
      valid_values <- values[!is.na(values) & values > 0]
      if (length(valid_values) > 0) {
        round(mean(valid_values), 2)
      } else {
        -5
      }
    }
  ) %>%
  ungroup() %>%
  mutate(
    M13_VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
    
    #change weight to kg from grams
    WEIGHT = case_when(!is.na(WEIGHT_PERES) & WEIGHT_PERES > 0 ~ round(WEIGHT_PERES / 1000, 2),
                       TRUE ~ NA),
    
    # Create an indicator variable for if length, weight, headcircumference is available
    HC_MRSRE = ifelse(HC_FAORRES > 0 , 1, 0), 
    LENGTH_MRSRE = ifelse(LENGTH > 0 , 1, 0), 
    # Create an indicator variable for if birth weight is available
    WEIGHT_MRSRE = ifelse(WEIGHT >= 0.01 & !is.na (WEIGHT), 1, 0),
    completeness_score = HC_MRSRE + LENGTH_MRSRE + WEIGHT_MRSRE) %>%
  group_by(SITE, PREGID, INFANTID, AGE_IN_DAYS) %>%
  slice_max(order_by = completeness_score, with_ties = FALSE) %>%
  ungroup() %>%
  select (SITE, MOMID, PREGID, INFANTID, TYPE_VISIT, AGE_IN_DAYS, M13_VISIT_OBSSTDAT, WEIGHT, WEIGHT_PERES, 
          LENGTH, LENGTH_PERES_1, LENGTH_PERES_2, LENGTH_PERES_3, HC_FAORRES, HC_PERES_1, HC_PERES_2, HC_PERES_3,
          INF_VITAL_MNH13, HC_MRSRE, WEIGHT_MRSRE, LENGTH_MRSRE) %>% 
  distinct(SITE, PREGID, INFANTID, AGE_IN_DAYS, .keep_all = TRUE) 

# Step 2: Create allvisit_anthro dataframe with necessary calculations and selections
allvisit_anthro <- mnh13_constructed %>%
  filter (PREGID %in% enrolled_ids_vec)  %>%
  left_join(living_infants, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  left_join(mat_edd, by = c("SITE", "MOMID", "PREGID")) %>% 
  #left_join (end_preg, by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(
    DOB_Date = as.Date(trunc(DOB, 'days')), 
    AGE_IN_WKS = round(as.numeric(difftime(M13_VISIT_OBSSTDAT, DOB_Date, units = "weeks")), 0), 
    PREG_GA_CALC = (round(as.numeric (difftime (DOB_Date, EDD_BOE, units = "days")), 0)) + 280,
    # Create an indicator variable for if birth MNH11 date is available
    M13_DATE_MRSRE = ifelse(is.na(M13_VISIT_OBSSTDAT), 0, 1)
  ) %>%
  mutate(across(everything(), ~replace(., . %in% c(-7, -5), NA))) %>% 
  distinct(SITE, PREGID, INFANTID, AGE_IN_DAYS, .keep_all = TRUE)


tvisit_measure <- allvisit_anthro %>%
  # Filter records where AGE_IN_DAYS is between 0 and 500 and SEX is not NA
  filter(AGE_IN_DAYS >= 0 & AGE_IN_DAYS < 500 & !is.na(SEX)) %>%
  mutate(
    # Handle negative AGE_IN_DAYS by setting them to NA
    AGE_IN_DAYS = ifelse(AGE_IN_DAYS < 0, NA, AGE_IN_DAYS),
    # Calculate postmenstrual age in days
    PMAGE_DAYS = ifelse(AGE_IN_DAYS < 0, NA, AGE_IN_DAYS + PREG_GA_CALC),
    PMAGE_WKS = floor(PMAGE_DAYS / 7),
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

#prepare birth and visit dataframes with Z scores for merging 
for_merge_mnh11 <- tbirth_measure %>% 
  select(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT, VISIT_DATE, AGE_IN_DAYS, PREG_GA_CALC, AGE_IN_WKS, WEIGHT, LENGTH, HC_FAORRES,
        HC_MRSRE, WEIGHT_MRSRE, LENGTH_MRSRE,
         VITAL = INF_VITAL_MNH11, FLEN = flen, FWFL = fwfl, FWEI = fwei, ZLEN = zlen, FHC = fhc, ZWFL = zwfl, ZWEI = zwei, ZHC = zhc,
         ZLEN_IG, ZWEI_IG, ZWFL_IG, ZHC_IG, FLEN_IG, FWEI_IG, FWFL_IG, FHC_IG )


for_merge_mnh13 <- tvisit_measure %>% 
  select(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT, VISIT_DATE, AGE_IN_DAYS, PREG_GA_CALC, AGE_IN_WKS, WEIGHT, LENGTH, HC_FAORRES, 
         HC_MRSRE, WEIGHT_MRSRE, LENGTH_MRSRE,
         VITAL = INF_VITAL_MNH13, FLEN = flen, FWFL = fwfl, FWEI = fwei, ZLEN = zlen, FHC = fhc, ZWFL = zwfl, ZWEI = zwei, ZHC = zhc,
         ZLEN_IG, ZWEI_IG, ZWFL_IG, ZHC_IG, FLEN_IG, FWEI_IG, FWFL_IG, FHC_IG )

#merge birth and visit dataframes with Z scores 
merged_anthro <- bind_rows(for_merge_mnh11, for_merge_mnh13) 

names (merged_anthro)
gc()
#check if there are duplicates
test <- merged_anthro %>% group_by(SITE, MOMID, PREGID, INFANTID, AGE_IN_DAYS) %>% mutate(n=n()) %>% filter(n>1)
dim(test)

duplicates <- merged_anthro %>%
  group_by(SITE, MOMID, PREGID, INFANTID, AGE_IN_DAYS) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1)

# Step 1: Add completeness score using existing indicators
merged_anthro_scored <- merged_anthro %>%
  mutate(
    row_id = row_number(),
    completeness_score = HC_MRSRE + WEIGHT_MRSRE + LENGTH_MRSRE
  )

# Step 2: Identify best (most complete) row per infant-age group
# If completeness_score ties, prefer TYPE_VISIT == 6 (Visit 6)
chosen_dups <- merged_anthro_scored %>%
  group_by(SITE, MOMID, PREGID, INFANTID, AGE_IN_DAYS) %>%
  arrange(desc(completeness_score), TYPE_VISIT) %>%  # lower TYPE_VISIT comes first (6 < 7)
  slice(1) %>%
  ungroup() %>%
  mutate(dup_choice = "KEPT")

# Step 3: Tag duplicates for review
dups_review <- merged_anthro_scored %>%
  semi_join(
    merged_anthro_scored %>%
      group_by(SITE, MOMID, PREGID, INFANTID, AGE_IN_DAYS) %>%
      filter(n() > 1),
    by = c("SITE", "MOMID", "PREGID", "INFANTID", "AGE_IN_DAYS")
  ) %>%
  mutate(dup_choice = ifelse(row_id %in% chosen_dups$row_id, "KEPT", "REMOVED"))

# Step 4: Final deduplicated dataset
merged_anthro_dedup <- chosen_dups %>% select(-HC_MRSRE, -WEIGHT_MRSRE, -LENGTH_MRSRE)

# Step 5: Get the real issues
dups_flags <- dups_review %>%
  group_by(SITE, MOMID, PREGID, INFANTID, AGE_IN_DAYS) %>%
  summarise(
    same_weight = n_distinct(WEIGHT, na.rm = TRUE) == 1,
    same_length = n_distinct(LENGTH, na.rm = TRUE) == 1,
    same_hc     = n_distinct(HC_FAORRES, na.rm = TRUE) == 1,
    .groups = "drop"
  )


mnh11_data_query <- mnh11 %>%
  filter(INFANTID %in% dups_review$INFANTID) %>%
  
  # Join dups_review to pull the duplicate-record AGE_IN_DAYS for cross-check
  left_join(
    dups_review %>% select(MOMID, PREGID, INFANTID, AGE_IN_DAYS),
    by = c("MOMID", "PREGID", "INFANTID")
  ) %>%
  
  # Calculate BW_TIME and timing mismatch
  mutate(
    BW_TIME = case_when(
      BW_EST_FAORRES < 0 ~ NA_real_,
      TRUE ~ BW_EST_FAORRES
    ),
    AGE_MISMATCH = case_when(
      !is.na(BW_TIME) & BW_TIME < 72 & AGE_IN_DAYS > 3 ~ TRUE,
      TRUE ~ FALSE
    )
  )

ggplot(mnh11_data_query, aes(x = BW_TIME, fill = AGE_MISMATCH)) +
  geom_histogram(binwidth = 25, color = "black", position = "identity") +
  geom_vline(xintercept = 72, color = "red", linetype = "dashed", size = 0.5) +
  facet_wrap(~ SITE, scales = "free_y") +
  scale_fill_manual(
    name = "Timing Mismatch",
    values = c("TRUE" = "red", "FALSE" = "darkgreen"),
    labels = c("TRUE" = "Mismatch (BW_TIME < 72, AGE > 3)", "FALSE" = "No mismatch")
  ) +
  labs(
    title = "How many hours after birth was infant weighed?",
    x = "Time in hours",
    y = "Count"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

summary_by_site <- dups_flags %>%
  group_by(SITE) %>%
  summarise(
    total_dups = n(),
    same_weight = sum(same_weight),
    diff_weight = total_dups - same_weight,
    same_length = sum(same_length),
    diff_length = total_dups - same_length,
    same_hc = sum(same_hc),
    diff_hc = total_dups - same_hc
  )

#these are all births, including those with incomplete measures
all_data_birth <-  allbirth_anthro %>%
  select(SITE, MOMID, PREGID, INFANTID, INF_VISIT_COMP = INF_VITAL_MNH11, VISIT_DATE = M11_VISIT_OBSSTDAT, 
         WEIGHT_MRSRE, LENGTH_MRSRE, HC_MRSRE, DATE_MRSRE = M11_DATE_MRSRE, TYPE_VISIT, MNH09_HAVE, DOB, SEX, 
         LBW_CAT_ANY, PRETERMBIRTH_CAT, SGA_CAT, SGA_CENTILE, INF_JAUN_NON_SEV_ANY,INF_JAUN_SEV_24HR, INF_JAUN_SEV_GREATER_24HR,
         INF_SGA_PRETERM,INF_AGA_PRETERM, INF_SGA_TERM,INF_AGA_TERM, TERM_GA) %>%
  distinct(SITE, PREGID, INFANTID, VISIT_DATE, .keep_all = TRUE)

#these are all visits, including those with incomplete measures
all_data_visit <-  allvisit_anthro %>%
  mutate(MNH09_HAVE = NA) %>%
  select(SITE, MOMID, PREGID, INFANTID, INF_VISIT_COMP = INF_VITAL_MNH13, WEIGHT_MRSRE, 
         VISIT_DATE = M13_VISIT_OBSSTDAT, DATE_MRSRE = M13_DATE_MRSRE, LENGTH_MRSRE, HC_MRSRE, TYPE_VISIT, DOB, MNH09_HAVE, SEX,
         LBW_CAT_ANY, PRETERMBIRTH_CAT, SGA_CAT, SGA_CENTILE, INF_JAUN_NON_SEV_ANY,INF_JAUN_SEV_24HR, INF_JAUN_SEV_GREATER_24HR,
         INF_SGA_PRETERM,INF_AGA_PRETERM, INF_SGA_TERM,INF_AGA_TERM, TERM_GA) %>% 
  group_by(SITE, PREGID, INFANTID, VISIT_DATE) %>%
  slice_min(order_by = TYPE_VISIT, with_ties = FALSE) %>%
  ungroup() 

# test <- all_data_visit %>% group_by(SITE, MOMID, PREGID, INFANTID, VISIT_DATE) %>% mutate(n=n()) %>% filter(n>1)
# dim(test)

#these are all visits and births present that have all the indicators of what is missing 
merged_visits <- bind_rows(all_data_birth, all_data_visit) 

# test <- merged_visits %>% group_by(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT, VISIT_DATE) %>% mutate(n=n()) %>% filter(n>1)
# dim(test)

#merge all the visits with all the data from calculated z scores
outcome_data_prep <- merged_visits %>% 
  left_join(merged_anthro_dedup, by = c("SITE", "MOMID", "PREGID", "INFANTID", "TYPE_VISIT", "VISIT_DATE"))


# test <- outcome_data_prep %>% group_by(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT, VISIT_DATE) %>% mutate(n=n()) %>% filter(n>1)
# dim(test)


#Create the outcomes
outcome_all <- outcome_data_prep %>%
  mutate(
# WHO Standards
    UNDER_WEIGHT = case_when(
      is.na(ZWEI) | FWEI == 1 ~ 55, 
      ZWEI < -2.0 ~ 1,
      ZWEI >= -2.0 ~ 0  
    ),
    UNDERWEIGHT_SEVERE = case_when(
      is.na(ZWEI) | FWEI == 1 ~ 55,
      ZWEI < -3.0 ~ 1,
      ZWEI >= -3.0 ~ 0 
    ),
    STUNTING = case_when(
      is.na(ZLEN) | FLEN == 1 ~ 55,
      ZLEN < -2.0 ~ 1,
      ZLEN >= -2.0 ~ 0
    ),
    STUNTING_SEVERE = case_when(
      is.na(ZLEN) | FLEN == 1 ~ 55,
      ZLEN < -3.0 ~ 1,
      ZLEN >= -3.0 ~ 0  
    ),
    WASTING = case_when(
      is.na(ZWFL) | FWFL == 1 ~ 55,
      ZWFL < -2.0 ~ 1,
      ZWFL >= -2.0 ~ 0
    ),
    WASTING_SEVERE = case_when(
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
    UNDERWEIGHT_SEVERE_IG = case_when(
      is.na(ZWEI_IG) | FWEI_IG == 1 ~ 55,
      ZWEI_IG < -3.0 ~ 1,
      ZWEI_IG >= -3.0 ~ 0 
    ),
    STUNTING_IG = case_when(
      is.na(ZLEN_IG) | FLEN_IG == 1 ~ 55,
      ZLEN_IG < -2.0 ~ 1,
      ZLEN_IG >= -2.0 ~ 0
    ),
    STUNTING_SEVERE_IG = case_when(
      is.na(ZLEN_IG) | FLEN_IG == 1 ~ 55,
      ZLEN_IG < -3.0 ~ 1,
      ZLEN_IG >= -3.0 ~ 0  
    ),
    WASTING_IG = case_when(
      is.na(ZWFL_IG) | FWFL_IG == 1 ~ 55,
      ZWFL_IG < -2.0 ~ 1,
      ZWFL_IG >= -2.0 ~ 0
    ),
    WASTING_SEVERE_IG = case_when(
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
    UNDERWEIGHT_SEVERE_BOTH = case_when(
      (is.na(ZWEI) | is.na(ZWEI_IG)) | (FWEI == 1 | FWEI_IG == 1) ~ 55,
      ZWEI < -3.0 & ZWEI_IG < -3.0 ~ 1,
      TRUE ~ 0 
    ),
    STUNTING_BOTH = case_when(
      (is.na(ZLEN) | is.na(ZLEN_IG)) | (FLEN == 1 | FLEN_IG == 1) ~ 55,
      ZLEN < -2.0 & ZLEN_IG < -2.0 ~ 1,
      TRUE ~ 0
    ),
    STUNTING_SEVERE_BOTH = case_when(
      (is.na(ZLEN) | is.na(ZLEN_IG)) | (FLEN == 1 | FLEN_IG == 1) ~ 55,
      ZLEN < -3.0 & ZLEN_IG < -3.0 ~ 1,
      TRUE ~ 0  
    ),
    WASTING_BOTH = case_when(
      (is.na(ZWFL) | is.na(ZWFL_IG)) | (FWFL == 1 | FWFL_IG == 1) ~ 55,
      ZWFL < -2.0 & ZWFL_IG < -2.0 ~ 1,
      TRUE ~ 0
    ),
    WASTING_SEVERE_BOTH = case_when(
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
                               !is.na(AGE_IN_DAYS) & SEX %in% c(1,2) ~ 1, TRUE ~ 0 ),
   
   #are any of the z-scores not calculated? Yes - 1, No - 0 
    FALL = case_when ((is.na(ZHC) | is.na(ZWFL) | is.na(ZWEI) | is.na(ZLEN)) |
                     (is.na(ZHC_IG) | is.na(ZWFL_IG) | is.na(ZWEI_IG) | is.na(ZLEN_IG)) &
                      MSRE_COMPLT == 1 ~ 1, 
                     
                     (!is.na(ZHC) & !is.na(ZWFL) & !is.na(ZWEI) & !is.na(ZLEN) &
                      !is.na(ZHC_IG) & !is.na(ZWFL_IG) & !is.na(ZWEI_IG) & !is.na(ZLEN_IG)) &
                      MSRE_COMPLT == 1 ~ 0,
                      TRUE ~ 55), 
   
    #so we want to create an indicator for where there are biologically accurate Z-scores and where all measures are complete
    NON_TRAJ_FLAG = case_when ((FHC == 0 & FLEN == 0 & FWFL == 0 & FWEI == 0) & 
                               (FHC_IG == 0 & FLEN_IG == 0 & FWFL_IG == 0 & FWEI_IG == 0 ) & FALL == 0 & 
                                MSRE_COMPLT == 1 ~ 1,
                               
                               (FHC == 1 | FLEN == 1 | FWEI == 1 | FWFL == 1 | FALL == 1 | 
                                FHC_IG == 1 | FLEN_IG == 1 | FWEI_IG == 1 | FWFL_IG == 1) & 
                                MSRE_COMPLT == 1 ~ 0,
                                TRUE ~ 55),
    
    #create an indicator for where at least one measure is flagged and/or **missing (unable to be computed)
    ONE_MSRE_FLAG = case_when ((FHC == 1 | FLEN == 1 | FWEI == 1 | FWFL == 1 | 
                               FHC_IG == 1 | FLEN_IG == 1 | FWEI_IG == 1 | FWFL_IG == 1 | FALL == 1) &
                                MSRE_COMPLT == 1 ~ 1, 
                               
                               (FHC == 0 & FLEN == 0 & FWFL == 0 & FWEI == 0) & 
                               (FHC_IG == 0 & FLEN_IG == 0 & FWFL_IG == 0 & FWEI_IG == 0) & FALL == 0  &
                                MSRE_COMPLT == 1 ~ 0,
                               TRUE ~ 55),

AGE_IN_MTH = floor(AGE_IN_DAYS / 30.4375)) %>%
  select(
    SITE, MOMID, PREGID, INFANTID, INF_VISIT_COMP, TYPE_VISIT, VISIT_DATE, 
    AGE_IN_DAYS, AGE_IN_WKS, AGE_IN_MTH, PRETERMBIRTH_CAT, SGA_CAT, SGA_CENTILE, 
    INF_SGA_PRETERM,INF_AGA_PRETERM, INF_SGA_TERM,INF_AGA_TERM, TERM_GA,
    WEIGHT_MRSRE, LENGTH_MRSRE, HC_MRSRE, WEIGHT, LENGTH, HC_FAORRES,  SEX,
    VITAL, FLEN, FWFL, FWEI, MSRE_COMPLT, NON_TRAJ_FLAG, ONE_MSRE_FLAG, ZLEN, FHC, ZWFL, ZWEI, ZHC, 
    ZLEN_IG, ZWEI_IG, ZWFL_IG, ZHC_IG, FLEN_IG, FWEI_IG, FWFL_IG, FHC_IG, 
    UNDER_WEIGHT, UNDERWEIGHT_SEVERE, STUNTING, STUNTING_SEVERE, WASTING, WASTING_SEVERE, OVERWEIGHT, MICROCEPHALY,
    UNDER_WEIGHT_IG, UNDERWEIGHT_SEVERE_IG, STUNTING_IG, STUNTING_SEVERE_IG, 
    WASTING_IG, WASTING_SEVERE_IG, OVERWEIGHT_IG, MICROCEPHALY_IG, 
    UNDER_WEIGHT_BOTH, UNDERWEIGHT_SEVERE_BOTH, STUNTING_BOTH, STUNTING_SEVERE_BOTH, 
    WASTING_BOTH, WASTING_SEVERE_BOTH, OVERWEIGHT_BOTH, MICROCEPHALY_BOTH
  )

test2 <- outcome_all %>% filter (INFANTID == "IBM-0038-01-01")

write.csv(outcome_all, paste0(path_to_save, "INF_GROWTH_LONG_",UploadDate ,".csv"), row.names=FALSE)

write.csv(outcome_all, paste0(path_to_tnt, "INF_GROWTH_LONG" ,".csv"), na="", row.names=FALSE)


# test <- outcome_all %>%  filter(FHC == 0 & FLEN == 0 & FWFL == 0 & FWEI == 0) 
# test1 <- outcome_all %>%  filter(FHC == 0 & FLEN == 0 & FWEI == 0) 
# test2 <- outcome_all %>%  filter(FHC == 1 | FLEN == 1 | FWEI == 1 | FWFL == 1) 
# test3 <- outcome_all %>% filter(FHC == 1 & FLEN == 1 & FWEI == 1)

#So to create a wide format of the data for three time points we separate the dataframe
#choose measurement at the latest age for month one and six if there are multiple/duplicates

#1- at birth  
birth_filtered <- outcome_all %>%  filter(TYPE_VISIT == 6) 

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
    UNDER_WEIGHT, UNDERWEIGHT_SEVERE, STUNTING, STUNTING_SEVERE, WASTING, WASTING_SEVERE, OVERWEIGHT, MICROCEPHALY,
    UNDER_WEIGHT_IG, UNDERWEIGHT_SEVERE_IG, STUNTING_IG, STUNTING_SEVERE_IG, 
    WASTING_IG, WASTING_SEVERE_IG, OVERWEIGHT_IG, MICROCEPHALY_IG, 
    UNDER_WEIGHT_BOTH, UNDERWEIGHT_SEVERE_BOTH, STUNTING_BOTH, STUNTING_SEVERE_BOTH, 
    WASTING_BOTH, WASTING_SEVERE_BOTH, OVERWEIGHT_BOTH, MICROCEPHALY_BOTH
  )

# Rename columns
names(one_month_filtered) <- c(
  "SITE", "MOMID", "PREGID", "INFANTID", "INF_VISIT_COMP_M1", "TYPE_VISIT_M1", "VISIT_DATE_M1", 
  "AGE_IN_DAYS_M1", "AGE_IN_WKS_M1", "AGE_IN_MTH_M1",
  "WEIGHT_MRSRE_M1", "LENGTH_MRSRE_M1", "HC_MRSRE_M1", "WEIGHT_M1", 
  "LENGTH_M1", "HC_FAORRES_M1", "VITAL_M1", "FLEN_M1", "FWFL_M1", "FWEI_M1", "MSRE_COMPLT_M1", "NON_TRAJ_FLAG_M1", 
  "ONE_MSRE_FLAG_M1", "ZLEN_M1", "FHC_M1", "ZWFL_M1", "ZWEI_M1", "ZHC_M1", 
  "ZLEN_IG_M1", "ZWEI_IG_M1", "ZWFL_IG_M1", "ZHC_IG_M1", "FLEN_IG_M1", "FWEI_IG_M1", "FWFL_IG_M1", "FHC_IG_M1", 
  "UNDER_WEIGHT_M1", "UNDERWEIGHT_SEVERE_M1", "STUNTING_M1", "STUNTING_SEVERE_M1", 
  "WASTING_M1", "WASTING_SEVERE_M1", "OVERWEIGHT_M1", "MICROCEPHALY_M1", 
  "UNDER_WEIGHT_IG_M1", "UNDERWEIGHT_SEVERE_IG_M1", "STUNTING_IG_M1", "STUNTING_SEVERE_IG_M1", 
  "WASTING_IG_M1", "WASTING_SEVERE_IG_M1", "OVERWEIGHT_IG_M1", "MICROCEPHALY_IG_M1", 
  "UNDER_WEIGHT_BOTH_M1", "UNDERWEIGHT_SEVERE_BOTH_M1", "STUNTING_BOTH_M1", "STUNTING_SEVERE_BOTH_M1", 
  "WASTING_BOTH_M1", "WASTING_SEVERE_BOTH_M1", "OVERWEIGHT_BOTH_M1", "MICROCEPHALY_BOTH_M1"
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
      UNDER_WEIGHT, UNDERWEIGHT_SEVERE, STUNTING, STUNTING_SEVERE, WASTING, WASTING_SEVERE, OVERWEIGHT, MICROCEPHALY,
      UNDER_WEIGHT_IG, UNDERWEIGHT_SEVERE_IG, STUNTING_IG, STUNTING_SEVERE_IG, 
      WASTING_IG, WASTING_SEVERE_IG, OVERWEIGHT_IG, MICROCEPHALY_IG, 
      UNDER_WEIGHT_BOTH, UNDERWEIGHT_SEVERE_BOTH, STUNTING_BOTH, STUNTING_SEVERE_BOTH, 
      WASTING_BOTH, WASTING_SEVERE_BOTH, OVERWEIGHT_BOTH, MICROCEPHALY_BOTH
    )
    
    # Rename columns
names(six_months_filtered) <- c(
      "SITE", "MOMID", "PREGID", "INFANTID", "INF_VISIT_COMP_M6", "TYPE_VISIT_M6", "VISIT_DATE_M6", 
      "AGE_IN_DAYS_M6", "AGE_IN_WKS_M6", "AGE_IN_MTH_M6", 
      "WEIGHT_MRSRE_M6", "LENGTH_MRSRE_M6", "HC_MRSRE_M6", "WEIGHT_M6", 
      "LENGTH_M6", "HC_FAORRES_M6", "VITAL_M6", "FLEN_M6", "FWFL_M6", "FWEI_M6", "MSRE_COMPLT_M6", "NON_TRAJ_FLAG_M6", 
      "ONE_MSRE_FLAG_M6", "ZLEN_M6", "FHC_M6", "ZWFL_M6", "ZWEI_M6", "ZHC_M6", 
      "ZLEN_IG_M6", "ZWEI_IG_M6", "ZWFL_IG_M6", "ZHC_IG_M6", "FLEN_IG_M6", "FWEI_IG_M6", "FWFL_IG_M6", "FHC_IG_M6", 
      "UNDER_WEIGHT_M6", "UNDERWEIGHT_SEVERE_M6", "STUNTING_M6", "STUNTING_SEVERE_M6", 
      "WASTING_M6", "WASTING_SEVERE_M6", "OVERWEIGHT_M6", "MICROCEPHALY_M6", 
      "UNDER_WEIGHT_IG_M6", "UNDERWEIGHT_SEVERE_IG_M6", "STUNTING_IG_M6", "STUNTING_SEVERE_IG_M6", 
      "WASTING_IG_M6", "WASTING_SEVERE_IG_M6", "OVERWEIGHT_IG_M6", "MICROCEPHALY_IG_M6", 
      "UNDER_WEIGHT_BOTH_M6", "UNDERWEIGHT_SEVERE_BOTH_M6", "STUNTING_BOTH_M6", "STUNTING_SEVERE_BOTH_M6", 
      "WASTING_BOTH_M6", "WASTING_SEVERE_BOTH_M6", "OVERWEIGHT_BOTH_M6", "MICROCEPHALY_BOTH_M6"
    )

# Join birth_filtered and one_month_filtered
combined_df <- birth_filtered %>%
  left_join(one_month_filtered, by = c("MOMID", "PREGID", "INFANTID", "SITE"))

# Join the resulting dataframe with six_months_filtered
wide_infant_trajectories <- combined_df %>%
  left_join(six_months_filtered, by = c("MOMID", "PREGID", "INFANTID", "SITE"))

# export data 
write.csv(wide_infant_trajectories, paste0(path_to_save, "INF_GROWTH_WIDE_", UploadDate ,".csv"), row.names=FALSE)
write.csv(wide_infant_trajectories, paste0(path_to_tnt, "INF_GROWTH_WIDE" ,".csv"), na="", row.names=FALSE)




