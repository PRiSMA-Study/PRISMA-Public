
#*****************************************************************************
#* PRISMA Verbal Autopsy Report: 
#* Drafted: 25 June 2024, Precious Williams
#* Last updated: 22 October 2024

# 
#*****************************************************************************

# clear environment 
rm(list = ls())


library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(openxlsx)
library(haven)
library(dplyr)
library(readr)
library(stringr)
library(hms)  # For time parsing

#*****************************************************************************
#* load data
#*****************************************************************************

## UPDATE EACH RUN ## 
UploadDate = "2025-10-17"

#Set your main directory 
#path_to_data <- paste0("~/Analysis/Merged_data/", UploadDate)
path_to_save <- paste0("~/Analysis/COD/data/")
path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")


#Import forms---- 
mnh24 <- read.csv ( paste0("Z:/Stacked Data/", UploadDate, "/mnh24_merged.csv"))

mnh04 <- read.csv ( paste0("Z:/Stacked Data/", UploadDate, "/mnh04_merged.csv"))

mnh09 <- read.csv ( paste0("Z:/Stacked Data/", UploadDate, "/mnh09_merged.csv"))

mnh10 <- read.csv ( paste0("Z:/Stacked Data/", UploadDate, "/mnh10_merged.csv"))

mnh12 <- read.csv ( paste0("Z:/Stacked Data/", UploadDate, "/mnh12_merged.csv"))

mnh19 <- read.csv ( paste0("Z:/Stacked Data/", UploadDate, "/mnh19_merged.csv"))

mnh23 <- read.csv ( paste0("Z:/Stacked Data/", UploadDate, "/mnh23_merged.csv"))

#inf_mortality <- read.csv(paste0("Z:/Outcome Data/",UploadDate,"/INF_OUTCOMES.csv"))

inf_mortality <- read_excel(paste0("Z:/Outcome Data/", UploadDate, "/INF_OUTCOMES.xlsx"))

mat_enroll <- read.csv (paste0(path_to_tnt,"MAT_ENROLL.csv"))

mat_endpoints <- read_dta (paste0(path_to_tnt,"MAT_ENDPOINTS.dta"))

mat_endpoints <- read_dta("Z:/Outcome Data/2025-10-03/MAT_ENDPOINTS.dta")

## Call in permanently missing forms ----
prisma_file <- file.path("Z:/PRISMA Mortality IDs", "PRISMA_Mortality_IDs.xlsx")

perm_maternal <- read.xlsx(prisma_file, sheet = "Maternal") %>%
  transmute(
    SITE, MOMID, PREGID,
    perm_va = grepl("VA", FORM_MISSING, ignore.case = TRUE)
  ) %>%
  distinct(SITE, MOMID, PREGID, .keep_all = TRUE)

perm_infant <- read.xlsx(prisma_file, sheet = "Infant") %>%
  transmute(
    SITE, INFANTID,
    perm_va = grepl("VA", FORM_MISSING, ignore.case = TRUE)
  ) %>%
  distinct(SITE, INFANTID, .keep_all = TRUE)


## Maternal Source of death if not reported in mnh23----

# Helper function to clean and convert date columns
clean_date <- function(date_col) {
  date_col <- trimws(date_col)
  date_col <- na_if(date_col, "")
  as.Date(date_col)
}

# MNH04
library(dplyr)
library(purrr)


# MNH04
mnh04_dth <- mnh04 %>%
  mutate(M04_DTHDAT = clean_date(M04_DTHDAT)) %>%
  mutate(
    FLAG_VITAL = M04_MAT_VITAL_MNH04 == 2,
    FLAG_VISIT = M04_MAT_VISIT_MNH04 == 8,
    FLAG_DATE  = !is.na(M04_DTHDAT) & !M04_DTHDAT %in% 
      as.Date(c("1905-05-05", "1907-07-07","1909-09-09","1909-09-07","1909-07-09"))
  ) %>%
  filter(FLAG_VITAL | FLAG_VISIT | FLAG_DATE) %>%
  mutate(
    SOURCE_DTH = "MNH04",
    DEATH_DATE_SOURCE = M04_DTHDAT,
    DEATH_TRIGGER = pmap_chr(
      list(FLAG_VITAL, FLAG_VISIT, FLAG_DATE),
      ~ paste(c("M04_MAT_VITAL_MNH04==2", "M04_MAT_VISIT_MNH04==8", "M04_DTHDAT valid")[c(..1, ..2, ..3)], collapse = " | ")
    )
  ) %>%
  select(SITE, MOMID, PREGID, SOURCE_DTH, DEATH_DATE_SOURCE,
         FLAG_VITAL, FLAG_VISIT, FLAG_DATE, DEATH_TRIGGER)


# MNH09

mnh09_dth <- mnh09 %>%
  mutate(M09_MAT_DEATH_DTHDAT = clean_date(M09_MAT_DEATH_DTHDAT)) %>%
  mutate(
    FLAG_VITAL = M09_MAT_VITAL_MNH09 == 2,
    FLAG_VISIT = M09_MAT_VISIT_MNH09 == 8,
    FLAG_DATE  = !is.na(M09_MAT_DEATH_DTHDAT) & !M09_MAT_DEATH_DTHDAT %in% as.Date(c( as.Date(c("1905-05-05", "1907-07-07","1909-09-09","1909-09-07","1909-07-09"))))
  ) %>%
  filter(FLAG_VITAL | FLAG_VISIT | FLAG_DATE) %>%
  mutate(
    SOURCE_DTH = "MNH09",
    DEATH_DATE_SOURCE = M09_MAT_DEATH_DTHDAT,
    DEATH_TRIGGER = pmap_chr(
      list(FLAG_VITAL, FLAG_VISIT, FLAG_DATE),
      ~ paste(c("M09_MAT_VITAL_MNH09==2", "M09_MAT_VISIT_MNH09==8", "M09_MAT_DEATH_DTHDAT valid")[c(..1, ..2, ..3)], collapse = " | ")
    )
  ) %>%
  select(SITE, MOMID, PREGID, SOURCE_DTH, DEATH_DATE_SOURCE,
         FLAG_VITAL, FLAG_VISIT, FLAG_DATE, DEATH_TRIGGER)


# MNH10

mnh10_dth <- mnh10 %>%
  mutate(M10_MAT_DEATH_DTHDAT = clean_date(M10_MAT_DEATH_DTHDAT)) %>%
  mutate(
    FLAG_VITAL  = M10_MAT_VITAL_MNH10 == 2,
    FLAG_VISIT  = M10_MAT_VISIT_MNH10 == 8,
    FLAG_DSTERM = M10_MAT_DSTERM == 3,
    FLAG_DATE   = !is.na(M10_MAT_DEATH_DTHDAT) & !M10_MAT_DEATH_DTHDAT %in% as.Date(c("1905-05-05", "1907-07-07"))
  ) %>%
  filter(FLAG_VITAL | FLAG_VISIT | FLAG_DSTERM | FLAG_DATE) %>%
  mutate(
    SOURCE_DTH = "MNH10",
    DEATH_DATE_SOURCE = M10_MAT_DEATH_DTHDAT,
    DEATH_TRIGGER = pmap_chr(
      list(FLAG_VITAL, FLAG_VISIT, FLAG_DSTERM, FLAG_DATE),
      ~ paste(c("M10_MAT_VITAL_MNH10==2", "M10_MAT_VISIT_MNH10==8", "M10_MAT_DSTERM==3", "M10_MAT_DEATH_DTHDAT valid")[c(..1, ..2, ..3, ..4)], collapse = " | ")
    )
  ) %>%
  select(SITE, MOMID, PREGID, SOURCE_DTH, DEATH_DATE_SOURCE,
         FLAG_VITAL, FLAG_VISIT, FLAG_DSTERM, FLAG_DATE, DEATH_TRIGGER)


# MNH12

mnh12_dth <- mnh12 %>%
  mutate(M12_MAT_DEATH_DTHDAT = clean_date(M12_MAT_DEATH_DTHDAT)) %>%
  mutate(
    FLAG_VITAL   = M12_MAT_VITAL_MNH12 == 2,
    FLAG_VISIT   = M12_MAT_VISIT_MNH12 == 8,
    FLAG_DSDECOD = M12_MATERNAL_DSDECOD == 3,
    FLAG_DATE    = !is.na(M12_MAT_DEATH_DTHDAT) & !M12_MAT_DEATH_DTHDAT %in% as.Date(c( as.Date(c("1905-05-05", "1907-07-07","1909-09-09","1909-09-07","1909-07-09"))))
  ) %>%
  filter(FLAG_VITAL | FLAG_VISIT | FLAG_DSDECOD | FLAG_DATE) %>%
  mutate(
    SOURCE_DTH = "MNH12",
    DEATH_DATE_SOURCE = M12_MAT_DEATH_DTHDAT,
    DEATH_TRIGGER = pmap_chr(
      list(FLAG_VITAL, FLAG_VISIT, FLAG_DSDECOD, FLAG_DATE),
      ~ paste(c("M12_MAT_VITAL_MNH12==2", "M12_MAT_VISIT_MNH12==8", "M12_MATERNAL_DSDECOD==3", "M12_MAT_DEATH_DTHDAT valid")[c(..1, ..2, ..3, ..4)], collapse = " | ")
    )
  ) %>%
  select(SITE, MOMID, PREGID, SOURCE_DTH, DEATH_DATE_SOURCE,
         FLAG_VITAL, FLAG_VISIT, FLAG_DSDECOD, FLAG_DATE, DEATH_TRIGGER)


# MNH19
mnh19_dth <- mnh19 %>%
  mutate(M19_DTHDAT = clean_date(M19_DTHDAT)) %>%
  mutate(
    FLAG_VISIT_FAORRES    = M19_VISIT_FAORRES == 5,
    FLAG_ADMIT_DSTERM     = M19_ADMIT_DSTERM == 3,
    FLAG_ARRIVAL_DSDECOD  = M19_MAT_ARRIVAL_DSDECOD == 2,
    FLAG_DATE             = !is.na(M19_DTHDAT) & !M19_DTHDAT %in% as.Date(c( as.Date(c("1905-05-05", "1907-07-07","1909-09-09","1909-09-07","1909-07-09"))))
  ) %>%
  filter(FLAG_VISIT_FAORRES | FLAG_ADMIT_DSTERM | FLAG_ARRIVAL_DSDECOD | FLAG_DATE) %>%
  mutate(
    SOURCE_DTH = "MNH19",
    DEATH_DATE_SOURCE = M19_DTHDAT,
    DEATH_TRIGGER = pmap_chr(
      list(FLAG_VISIT_FAORRES, FLAG_ADMIT_DSTERM, FLAG_ARRIVAL_DSDECOD, FLAG_DATE),
      ~ paste(c("M19_VISIT_FAORRES==5", "M19_ADMIT_DSTERM==3", "M19_MAT_ARRIVAL_DSDECOD==2", "M19_DTHDAT valid")[c(..1, ..2, ..3, ..4)], collapse = " | ")
    )
  ) %>%
  select(SITE, MOMID, PREGID, SOURCE_DTH, DEATH_DATE_SOURCE, DEATH_TRIGGER)

mnh23_dth <- mnh23 %>%
  mutate(M23_DTHDAT = clean_date(M23_DTHDAT)) %>%
  mutate(
    FLAG_CLOSE_DSDECOD    = M23_CLOSE_DSDECOD == 3,
    FLAG_DATE             = !is.na(M23_DTHDAT) & !M23_DTHDAT %in% as.Date(c( as.Date(c("1905-05-05", "1907-07-07","1909-09-09","1909-09-07","1909-07-09"))))
  ) %>%
  filter(FLAG_CLOSE_DSDECOD | FLAG_DATE) %>%
  mutate(
    SOURCE_DTH = "MNH23",
    DEATH_DATE_SOURCE = M23_DTHDAT,
    DEATH_TRIGGER = pmap_chr(
      list(FLAG_CLOSE_DSDECOD, FLAG_DATE),
      ~ paste(c("M23_CLOSE_DSDECOD==3", "M23_DTHDAT valid")[c(..1, ..2)], collapse = " | ")
    )
  ) %>%
  select(SITE, MOMID, PREGID, SOURCE_DTH, DEATH_DATE_SOURCE, DEATH_TRIGGER)


# Combine and deduplicate
mat_dth_source <- bind_rows(mnh04_dth, mnh09_dth, mnh10_dth, mnh12_dth, mnh19_dth, mnh23_dth) %>%
  mutate(DEATH_DATE_SOURCE = na_if(DEATH_DATE_SOURCE, as.Date("1905-05-05")),
         DEATH_DATE_SOURCE = na_if(DEATH_DATE_SOURCE, as.Date("1907-07-07"))) %>%
  arrange(MOMID, PREGID, is.na(DEATH_DATE_SOURCE), DEATH_DATE_SOURCE) %>%
  group_by(SITE, MOMID, PREGID) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(SITE, MOMID, PREGID, SOURCE_DTH, DEATH_DATE_SOURCE, DEATH_TRIGGER)

##Infant Source of Death MNH09----
# Clean and reshape MNH09 data
mnh09_sub <- mnh09 %>%
  rename_with(~ str_remove(., "^M09_")) %>%  # Remove 'M09_' prefix from all column names
  
  # Select needed variables
  select(
    SITE, MOMID, PREGID,
    INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4,
    DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4,
    DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF2, DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF4,
    BIRTH_DSTERM_INF1, BIRTH_DSTERM_INF2, BIRTH_DSTERM_INF3, BIRTH_DSTERM_INF4,
    SEX_INF1, SEX_INF2, SEX_INF3, SEX_INF4,
    DELIV_PRROUTE_INF1, DELIV_PRROUTE_INF2, DELIV_PRROUTE_INF3, DELIV_PRROUTE_INF4
  ) %>%
  
  # Convert sex to numeric
  mutate(across(starts_with("SEX_INF"), as.numeric)) %>%
  
  # Parse and standardize date fields
  mutate(across(
    starts_with("DELIV_DSSTDAT_INF"),
    ~ ymd(parse_date_time(.x, orders = c("dmy", "ymd", "d-b-y", "d-m-y")))
  )) %>%
  
  # Replace default placeholder values with NA
  mutate(across(starts_with("DELIV_DSSTDAT_INF"),
                ~ replace(.x, .x %in% c(ymd("1907-07-07"), ymd("2007-07-07"), 
                                        ymd("1905-05-05"),  ymd("1909-09-09"),
                                        ymd("1909-09-07"),ymd("1909-07-09")), NA)),
         across(starts_with("DELIV_DSSTTIM_INF"),
                ~ replace(.x, .x %in% c("77:77", "", "55:55", NA), NA))) %>%
  
  # Convert time strings to hms format
  mutate(across(
    starts_with("DELIV_DSSTTIM_INF"),
    ~ if_else(!is.na(.x), as_hms(parse_time(.x, format = "%H:%M")), NA)
  )) %>%
  
  # Combine date and time into datetime
  mutate(
    DELIVERY_DATETIME_INF1 = if_else(!is.na(DELIV_DSSTDAT_INF1) & !is.na(DELIV_DSSTTIM_INF1),
                                     as.POSIXct(DELIV_DSSTDAT_INF1 + DELIV_DSSTTIM_INF1), NA_POSIXct_),
    DELIVERY_DATETIME_INF2 = if_else(!is.na(DELIV_DSSTDAT_INF2) & !is.na(DELIV_DSSTTIM_INF2),
                                     as.POSIXct(DELIV_DSSTDAT_INF2 + DELIV_DSSTTIM_INF2), NA_POSIXct_),
    DELIVERY_DATETIME_INF3 = if_else(!is.na(DELIV_DSSTDAT_INF3) & !is.na(DELIV_DSSTTIM_INF3),
                                     as.POSIXct(DELIV_DSSTDAT_INF3 + DELIV_DSSTTIM_INF3), NA_POSIXct_),
    DELIVERY_DATETIME_INF4 = if_else(!is.na(DELIV_DSSTDAT_INF4) & !is.na(DELIV_DSSTTIM_INF4),
                                     as.POSIXct(DELIV_DSSTDAT_INF4 + DELIV_DSSTTIM_INF4), NA_POSIXct_)
  )

# Getting the Date of Birth, Sex and Birth Outcome for Each ID
# Pivot data to long format by infant
inf_dob <- mnh09_sub %>%
  pivot_longer(
    cols = c(
      INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4,
      DELIVERY_DATETIME_INF1, DELIVERY_DATETIME_INF2, DELIVERY_DATETIME_INF3, DELIVERY_DATETIME_INF4,
      BIRTH_DSTERM_INF1, BIRTH_DSTERM_INF2, BIRTH_DSTERM_INF3, BIRTH_DSTERM_INF4,
      SEX_INF1, SEX_INF2, SEX_INF3, SEX_INF4,
      DELIV_PRROUTE_INF1, DELIV_PRROUTE_INF2, DELIV_PRROUTE_INF3, DELIV_PRROUTE_INF4
    ),
    names_to = c(".value", "infant_suffix"),
    names_pattern = "(.*)_INF(\\d)"
  ) %>%
  
  # Clean up
  filter(!is.na(INFANTID) & INFANTID != "" & INFANTID != "n/a") %>%
  rename(
    INFANTID = INFANTID,
    DOB = DELIVERY_DATETIME
  ) %>%
  select(SITE, MOMID, PREGID, INFANTID, DOB, BIRTH_DSTERM, SEX, DELIV_PRROUTE) 
  
  
  inf_dth_source <- inf_dob  %>%
  filter (BIRTH_DSTERM == 2 | DELIV_PRROUTE == 3) %>% 
  mutate(SOURCE_DTH = "MNH09", DEATH_DATE_SOURCE = DOB) %>%
  select(SITE, MOMID, PREGID, INFANTID, SOURCE_DTH, DEATH_DATE_SOURCE)

##MNH24 
mnh24_combined <- mnh24 %>%
  rename(
    CLOSE_DSDECOD = M24_CLOSE_DSDECOD,
    CLOSE_DSSTDAT = M24_CLOSE_DSSTDAT
        )  %>%
  mutate(
    CLOSE_DSSTDAT = ymd(parse_date_time(CLOSE_DSSTDAT, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))
  ) %>%  select(SITE, MOMID, PREGID, INFANTID, DTHDAT = M24_DTHDAT , DTHTIM = M24_DTHTIM , CLOSE_DSDECOD, CLOSE_DSSTDAT ) 



#Read in the mat end points dataset and subset for where MAT_DEATH == 1 (For Maternal Death)----
#select variables which are needed
mat_death <- mat_endpoints %>% 
  filter (MAT_DEATH == 1) %>% 
  select (SITE, MOMID, PREGID, MAT_DEATH, CLOSEOUT_DT, STOP_DATE,
          CLOSE_DSDECOD = CLOSEOUT_TYPE, DEATH_DATE = MAT_DEATH_DATE)

# Standardizing date formats
date_columns <- c( "DEATH_DATE", "CLOSEOUT_DT", "STOP_DATE")

mat_death[date_columns] <- lapply(mat_death[date_columns], function(x) {
  ymd(parse_date_time(x, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))
})

# Creating new columns based on conditions
mat_dth_df <- mat_death %>%
  mutate(
    # Determine the death date based on various conditions
    DEATH_DATE = ifelse(!is.na(CLOSE_DSDECOD), DEATH_DATE, STOP_DATE),
    # Flag if death date is missing or if it matches specific placeholder dates
    DEATH_DATE_MISS = if_else(is.na(DEATH_DATE) | DEATH_DATE %in% as.Date(c("1905-05-05", "1907-07-07","1909-09-09","1909-09-07","1909-07-09")), 1, 0), 
    # Flag if CLOSE_DSDECOD is not missing
    MNH23_CMPTE = if_else(!is.na(CLOSE_DSDECOD), 1, 0),
    # Calculate the late date as 42 days after death date if death date is not missing
    LATE_DATE = if_else(DEATH_DATE_MISS == 0, DEATH_DATE + 42, NA),
    # Determine if verbal autopsy (VA) is due based on the late date and upload date
    VA_DUE = case_when(
      LATE_DATE <= UploadDate ~ 1,  # If late date is on or before the upload date, VA is due
      LATE_DATE > UploadDate ~ 0,   # If late date is after the upload date, VA is not due
      is.na(LATE_DATE) ~ 55         # If late date is missing, assign 55 as a placeholder value
    )
  ) %>% select (SITE, MOMID, PREGID, DEATH_DATE, DEATH_DATE_MISS, MNH23_CMPTE, LATE_DATE, VA_DUE)

date_columns <- c( "DEATH_DATE", "LATE_DATE")

mat_dth_df[date_columns] <- lapply(mat_dth_df[date_columns], function(x) {
  as.Date(x, origin = "1970-01-01")
})

# Filter and mutate infant death data
inf_death <- mnh24_combined %>%
  filter(CLOSE_DSDECOD %in% c(3, 6) | 
        (!is.na(DTHDAT) & !DTHDAT %in% as.Date(c("1905-05-05", "1907-07-07","1909-09-09","1909-09-07","1909-07-09")))) %>%
  mutate(
    MNH24_CMPTE = 1,
    DTHDAT = ymd(parse_date_time(DTHDAT, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
    # Replace invalid DTHTIM values (77:77, 55:55) with NA
    DTHTIM = replace(DTHTIM, DTHTIM %in% c("77:77", "55:55"), NA),
    # Convert DTHTIM to ITime format if not NA
    DTHTIM = if_else(!is.na(DTHTIM), as.ITime(DTHTIM), NA),
    # Replace specific placeholder dates (1907-07-07, 1905-05-05, 2007-07-07) in DTHDAT with NA
    DTHDAT = replace(DTHDAT, DTHDAT %in% c(ymd("1907-07-07"), ymd("2007-07-07"), 
                                           ymd("1905-05-05"),  ymd("1909-09-09"),
                                           ymd("1909-09-07"),ymd("1909-07-09")), NA),
    # Combine DTHDAT and DTHTIM into a POSIXct datetime if both are not NA, otherwise use DTHDAT
    DTH_TME = if_else(!is.na(DTHTIM) & !is.na(DTHDAT), as.POSIXct(paste(DTHDAT, DTHTIM), format = "%Y-%m-%d %H:%M:%S"), DTHDAT)) %>% 
  filter (!is.na(INFANTID) & INFANTID != "Not Available")

#Read in the outcome (For Neonatal Death)
infant_dth <- inf_mortality %>% 
              filter (INF_DTH == 1 | STILLBIRTH_20WK == 1) %>% 
              select (SITE, MOMID, PREGID, INFANTID, EDD_BOE, DOB, DEATH_DATETIME, INF_DTH, NEO_DTH_CAT, STILLBIRTH_GESTAGE_CAT, 
                      FETAL_LOSS_DATE, STILLBIRTH_20WK, STILLBIRTH_SIGNS_LIFE, STILLBIRTH_TIMING) %>% 
              distinct(SITE, MOMID, PREGID, INFANTID, .keep_all = TRUE)  %>% 
              filter (!is.na (INFANTID) & INFANTID != "Not Available" &  INFANTID != "" )


#Importing all the COD Files----

##Zambia----
#zambia_cod <- read.csv (paste0("Z:/SynapseCSVs/Zambia/", UploadDate, "/mnh37.csv"))
zambia_cod <- read.csv (paste0("Z:/PRISMA_Data_Uploads/", UploadDate, "/", UploadDate, "_zam/mnh37.csv"))
zambia_mat <- zambia_cod %>% 
  filter (VA_TYPE == 1 | FINAL_MAT_CS %in% c (1:18)) %>% 
  select (MOMID, PREGID, FINAL_MAT_DAT, FINAL_MAT_CS, IVA_MAT_CS1, VA_COMPL, IVA_MAT_OTHR_SPFY_CS1, FINAL_MAT_OTHR_SPFY_CS) %>%
  mutate(
    COD = FINAL_MAT_CS,
    COD_TEXT = case_when(
    FINAL_MAT_CS == 1  ~ 'Road traffic accident',
    FINAL_MAT_CS == 2  ~ 'Obstetric haemorrhage',
    FINAL_MAT_CS == 3  ~ 'Pregnancy-induced hypertension',
    FINAL_MAT_CS == 4  ~ 'Pregnancy-related sepsis',
    FINAL_MAT_CS == 5  ~ 'ARI, including pneumonia',
    FINAL_MAT_CS == 6  ~ 'HIV/AIDS related death',
    FINAL_MAT_CS == 7  ~ 'Reproductive neoplasms MMF',
    FINAL_MAT_CS == 8  ~ 'Pulmonary TB',
    FINAL_MAT_CS == 9  ~ 'Malaria',
    FINAL_MAT_CS == 10 ~ 'Meningitis',
    FINAL_MAT_CS == 11 ~ 'Diarrheal diseases',
    FINAL_MAT_CS == 12 ~ 'Abortion-related death',
    FINAL_MAT_CS == 13 ~ 'Ectopic pregnancy',
    FINAL_MAT_CS == 14 ~ 'Other and unspecified cardiac diseases',
    FINAL_MAT_CS == 15 ~ 'Other infectious diseases',
    FINAL_MAT_CS == 16 ~ 'Other NCD',
    FINAL_MAT_CS == 18 ~ 'Indeterminate',
    FINAL_MAT_CS == 17 ~ FINAL_MAT_OTHR_SPFY_CS,
    TRUE ~ "Unknown" ), # Default for any other values
    SITE = "Zambia") %>% 
  select(SITE, MOMID, PREGID, COD, COD_DATE = FINAL_MAT_DAT, COD_TEXT, IVA_MAT_CS1, VA_COMPL, IVA_MAT_OTHR_SPFY_CS1)

zambia_inf <- zambia_cod %>% 
  filter (VA_TYPE == 2 | FINAL_INF_CS %in% c (1:18)) %>% 
  select (MOMID, PREGID, INFANTID, FINAL_INF_CS, FINAL_INF_OTHR_SPFY_CS, FINAL_INF_DAT, IVA_INF_CS1, VA_COMPL, IVA_INF_OTHR_SPFY_CS1) %>%
  mutate(
    COD = FINAL_INF_CS,
    COD_TEXT = case_when(
      FINAL_INF_CS == 1  ~ 'Fresh stillbirth',
      FINAL_INF_CS == 2  ~ 'Macerated stillbirth',
      FINAL_INF_CS == 4  ~ 'Prematurity',
      FINAL_INF_CS == 5  ~ 'Birth asphyxia',
      FINAL_INF_CS == 6  ~ 'Tetanus',
      FINAL_INF_CS == 7  ~ 'Congenital malformation',
      FINAL_INF_CS == 8  ~ 'Diarrheal diseases',
      FINAL_INF_CS == 9  ~ 'Acute respiratory infection including pneumonia',
      FINAL_INF_CS == 10 ~ 'Meningitis and encephalitis',
      FINAL_INF_CS == 11 ~ 'Neonatal pneumonia',
      FINAL_INF_CS == 12 ~ 'Neonatal sepsis',
      FINAL_INF_CS == 13 ~ 'Road traffic accident',
      FINAL_INF_CS == 14 ~ 'Other and unspecified infectious diseases',
      FINAL_INF_CS == 15 ~ 'Other and unspecified cardiac diseases',
      FINAL_INF_CS == 16 ~ 'Severe malnutrition',
      FINAL_INF_CS == 17 ~ 'Renal failure',
      FINAL_INF_CS == 18 ~ FINAL_INF_OTHR_SPFY_CS,  # Other, specify
      FINAL_INF_CS == 19 ~ 'Indeterminate',
      TRUE ~ "Unknown" ), 
    SITE = "Zambia")  %>%
  select (SITE, MOMID, PREGID, INFANTID, COD, COD_TEXT, COD_DATE = FINAL_INF_DAT, IVA_INF_CS1, VA_COMPL, IVA_INF_OTHR_SPFY_CS1) 

##Pakistan----

# pakistan_cod <- read.csv (paste0("Z:/2025-01-04_pak/mnh37.csv"))
pakistan_cod <-  read.csv (paste0("Z:/PRISMA_Data_Uploads/", UploadDate, "/", UploadDate, "_pak/mnh37.csv"))
#pakistan_cod <- read.csv (paste0("Z:/SynapseCSVs/Pakistan/", UploadDate, "/mnh37.csv"))
pakistan_mat <- pakistan_cod %>% 
  filter (VA_TYPE == 1 | FINAL_MAT_CS %in% c (1:18)) %>% 
    select (MOMID, PREGID, FINAL_MAT_DAT, FINAL_MAT_CS, IVA_MAT_CS1, VA_COMPL, IVA_MAT_OTHR_SPFY_CS1, FINAL_MAT_OTHR_SPFY_CS) %>%
  mutate(
    COD = FINAL_MAT_CS,
    COD_TEXT = case_when(
    FINAL_MAT_CS == 1  ~ 'Road traffic accident',
    FINAL_MAT_CS == 2  ~ 'Obstetric haemorrhage',
    FINAL_MAT_CS == 3  ~ 'Pregnancy-induced hypertension',
    FINAL_MAT_CS == 4  ~ 'Pregnancy-related sepsis',
    FINAL_MAT_CS == 5  ~ 'ARI, including pneumonia',
    FINAL_MAT_CS == 6  ~ 'HIV/AIDS related death',
    FINAL_MAT_CS == 7  ~ 'Reproductive neoplasms MMF',
    FINAL_MAT_CS == 8  ~ 'Pulmonary TB',
    FINAL_MAT_CS == 9  ~ 'Malaria',
    FINAL_MAT_CS == 10 ~ 'Meningitis',
    FINAL_MAT_CS == 11 ~ 'Diarrheal diseases',
    FINAL_MAT_CS == 12 ~ 'Abortion-related death',
    FINAL_MAT_CS == 13 ~ 'Ectopic pregnancy',
    FINAL_MAT_CS == 14 ~ 'Other and unspecified cardiac diseases',
    FINAL_MAT_CS == 15 ~ 'Other infectious diseases',
    FINAL_MAT_CS == 16 ~ 'Other NCD',
    FINAL_MAT_CS == 18 ~ 'Indeterminate',
    FINAL_MAT_CS == 17 ~ FINAL_MAT_OTHR_SPFY_CS,
    TRUE ~ "Unknown" ), # Default for any other values
    SITE = "Pakistan") %>% 
  select(SITE, MOMID, PREGID, COD, COD_DATE = FINAL_MAT_DAT, COD_TEXT, IVA_MAT_CS1, VA_COMPL, IVA_MAT_OTHR_SPFY_CS1)

pakistan_inf <- pakistan_cod %>% 
  filter (VA_TYPE == 2 | FINAL_INF_CS %in% c (1:18)) %>% 
  select (MOMID, PREGID, INFANTID, FINAL_INF_CS, FINAL_INF_OTHR_SPFY_CS, FINAL_INF_DAT, IVA_INF_CS1, VA_COMPL, IVA_INF_OTHR_SPFY_CS1) %>%
  mutate(
    COD = FINAL_INF_CS,
    COD_TEXT = case_when(
      FINAL_INF_CS == 1  ~ 'Fresh stillbirth',
      FINAL_INF_CS == 2  ~ 'Macerated stillbirth',
      FINAL_INF_CS == 4  ~ 'Prematurity',
      FINAL_INF_CS == 5  ~ 'Birth asphyxia',
      FINAL_INF_CS == 6  ~ 'Tetanus',
      FINAL_INF_CS == 7  ~ 'Congenital malformation',
      FINAL_INF_CS == 8  ~ 'Diarrheal diseases',
      FINAL_INF_CS == 9  ~ 'Acute respiratory infection including pneumonia',
      FINAL_INF_CS == 10 ~ 'Meningitis and encephalitis',
      FINAL_INF_CS == 11 ~ 'Neonatal pneumonia',
      FINAL_INF_CS == 12 ~ 'Neonatal sepsis',
      FINAL_INF_CS == 13 ~ 'Road traffic accident',
      FINAL_INF_CS == 14 ~ 'Other and unspecified infectious diseases',
      FINAL_INF_CS == 15 ~ 'Other and unspecified cardiac diseases',
      FINAL_INF_CS == 16 ~ 'Severe malnutrition',
      FINAL_INF_CS == 17 ~ 'Renal failure',
      FINAL_INF_CS == 18 ~ FINAL_INF_OTHR_SPFY_CS,  # Other, specify
      FINAL_INF_CS == 19 ~ 'Indeterminate',
      TRUE ~ "Unknown" ), 
    SITE = "Pakistan")  %>%
  select (SITE, MOMID, PREGID, INFANTID, COD, COD_TEXT, COD_DATE = FINAL_INF_DAT, IVA_INF_CS1, VA_COMPL, IVA_INF_OTHR_SPFY_CS1)

##Kenya----
# kenya_cod <- read.csv(paste0("Z:/Stacked Data/", UploadDate, "/mnh37_merged.csv"))

kenya_cod <- read.csv (paste0("Z:/PRISMA_Data_Uploads/", UploadDate, "/", UploadDate, "_ke/mnh37.csv"))

kenya_mat <- kenya_cod %>%
  filter (VA_COMPL_FORM == 1 | FINAL_MAT_CS %in% c (1:18)) %>%
  #filter (VA_TYPE == 1 | FINAL_MAT_CS %in% c (1:18)) %>%
  select (MOMID, PREGID, FINAL_MAT_DAT, FINAL_MAT_CS, IVA_MAT_CS1, VA_COMPL, IVA_MAT_OTHR_SPFY_CS1, FINAL_MAT_OTHR_SPFY_CS)  %>%
  mutate(
    COD = FINAL_MAT_CS,
    COD_TEXT = case_when(
      FINAL_MAT_CS == 1  ~ 'Road traffic accident',
      FINAL_MAT_CS == 2  ~ 'Obstetric haemorrhage',
      FINAL_MAT_CS == 3  ~ 'Pregnancy-induced hypertension',
      FINAL_MAT_CS == 4  ~ 'Pregnancy-related sepsis',
      FINAL_MAT_CS == 5  ~ 'ARI, including pneumonia',
      FINAL_MAT_CS == 6  ~ 'HIV/AIDS related death',
      FINAL_MAT_CS == 7  ~ 'Reproductive neoplasms MMF',
      FINAL_MAT_CS == 8  ~ 'Pulmonary TB',
      FINAL_MAT_CS == 9  ~ 'Malaria',
      FINAL_MAT_CS == 10 ~ 'Meningitis',
      FINAL_MAT_CS == 11 ~ 'Diarrheal diseases',
      FINAL_MAT_CS == 12 ~ 'Abortion-related death',
      FINAL_MAT_CS == 13 ~ 'Ectopic pregnancy',
      FINAL_MAT_CS == 14 ~ 'Other and unspecified cardiac diseases',
      FINAL_MAT_CS == 15 ~ 'Other infectious diseases',
      FINAL_MAT_CS == 16 ~ 'Other NCD',
      FINAL_MAT_CS == 18 ~ 'Indeterminate',
      FINAL_MAT_CS == 17 ~ FINAL_MAT_OTHR_SPFY_CS,
      TRUE ~ "Unknown" ), # Default for any other values
    SITE = "Kenya") %>% 
  select(SITE, MOMID, PREGID, COD, COD_DATE = FINAL_MAT_DAT, COD_TEXT, IVA_MAT_CS1, VA_COMPL, IVA_MAT_OTHR_SPFY_CS1)

kenya_inf <- kenya_cod %>% 
  filter (VA_COMPL_FORM %in% c(2,3) | FINAL_INF_CS %in% c (1:18)) %>%
  #filter (VA_TYPE == 2 | FINAL_INF_CS %in% c (1:18)) %>% 
  select (MOMID, PREGID, INFANTID, FINAL_INF_CS, FINAL_INF_OTHR_SPFY_CS, FINAL_INF_DAT, IVA_INF_CS1, VA_COMPL, IVA_INF_OTHR_SPFY_CS1) %>%
  mutate(
    COD = FINAL_INF_CS,
    COD_TEXT = case_when(
      FINAL_INF_CS == 1  ~ 'Fresh stillbirth',
      FINAL_INF_CS == 2  ~ 'Macerated stillbirth',
      FINAL_INF_CS == 4  ~ 'Prematurity',
      FINAL_INF_CS == 5  ~ 'Birth asphyxia',
      FINAL_INF_CS == 6  ~ 'Tetanus',
      FINAL_INF_CS == 7  ~ 'Congenital malformation',
      FINAL_INF_CS == 8  ~ 'Diarrheal diseases',
      FINAL_INF_CS == 9  ~ 'Acute respiratory infection including pneumonia',
      FINAL_INF_CS == 10 ~ 'Meningitis and encephalitis',
      FINAL_INF_CS == 11 ~ 'Neonatal pneumonia',
      FINAL_INF_CS == 12 ~ 'Neonatal sepsis',
      FINAL_INF_CS == 13 ~ 'Road traffic accident',
      FINAL_INF_CS == 14 ~ 'Other and unspecified infectious diseases',
      FINAL_INF_CS == 15 ~ 'Other and unspecified cardiac diseases',
      FINAL_INF_CS == 16 ~ 'Severe malnutrition',
      FINAL_INF_CS == 17 ~ 'Renal failure',
      FINAL_INF_CS == 18 ~ FINAL_INF_OTHR_SPFY_CS,  # Other, specify
      FINAL_INF_CS == 19 ~ 'Indeterminate',
      TRUE ~ "Unknown" ), 
    SITE = "Kenya")  %>%
  select (SITE, MOMID, PREGID, INFANTID, COD, COD_TEXT, COD_DATE = FINAL_INF_DAT, IVA_INF_CS1, VA_COMPL, IVA_INF_OTHR_SPFY_CS1)

true_vars <- c("MOMID", "PREGID", "INFANTID", "VA_COMPL", "VA_FORM_DAT", "VA_COMPL_FORM",
               "IVA_MAT_CS1", "IVA_MAT_OTHR_SPFY_CS1", "IVA_MAT_CS1_PROB", "IVA_MAT_CS2",
               "IVA_MAT_OTHR_SPFY_CS2", "IVA_MAT_CS2_PROB", "IVA_MAT_CS3", "IVA_MAT_OTHR_SPFY_CS3",
               "IVA_MAT_CS3_PROB", "IVA_INF_CS1", "IVA_INF_OTHR_SPFY_CS1", "IVA_INF_CS1_PROB",
               "IVA_INF_CS2", "IVA_INF_OTHR_SPFY_CS2", "IVA_INF_CS2_PROB", "IVA_INF_CS3",
               "IVA_INF_OTHR_SPFY_CS3", "IVA_INF_CS3_PROB", "PHYS_COMPL", "PHYS_MAT_CS",
               "PHYS_MAT_OTHR_SPFY_CS", "PHYS_INF_CS", "PHYS_INF_OTHR_SPFY_CS", "OTH_COMPL",
               "OTH_MAT_CS", "OTH_MAT_OTHR_SPFY_CS", "OTH_INF_CS", "OTH_INF_OTHR_SPFY_CS",
               "FINAL_MAT_CS", "FINAL_MAT_OTHR_SPFY_CS", "FINAL_MAT_DAT", "FINAL_MAT_INFO",
               "FINAL_INF_CS", "FINAL_INF_OTHR_SPFY_CS", "FINAL_INF_DAT", "FINAL_INF_TIM",
               "FINAL_INF_INFO", "COYN_MNH37", "FORMCOMPLDAT_MNH37", "FORMCOMPLID_MNH37")

##Ghana ----
#ghana_cod <- read.csv (paste0("Z:/SynapseCSVs/Ghana/", UploadDate, "/mnh37.csv"))
ghana_cod <- read.csv (paste0("Z:/PRISMA_Data_Uploads/", UploadDate, "/", UploadDate, "_gha/mnh37.csv"))

# Rename columns: strip prefix if column ends with a true variable
names(ghana_cod) <- sapply(names(ghana_cod), function(col) {
  match <- true_vars[endsWith(col, true_vars)]
  if (length(match) > 0) {
    return(match[1])  # Keep only the matching true variable name
  } else {
    return(col)  # Leave it unchanged if no match
  }
})


ghana_mat <- ghana_cod %>%
  filter (VA_COMPL_FORM == 1 | FINAL_MAT_CS %in% c (1:18)) %>%
  #filter (VA_TYPE == 1 | FINAL_MAT_CS %in% c (1:18)) %>%
  select (MOMID, PREGID, FINAL_MAT_DAT, FINAL_MAT_CS, IVA_MAT_CS1, VA_COMPL, VA_COMPL_FORM, IVA_MAT_OTHR_SPFY_CS1, FINAL_MAT_OTHR_SPFY_CS)  %>%
  mutate(
    FINAL_MAT_OTHR_SPFY_CS = as.character(FINAL_MAT_OTHR_SPFY_CS),
    COD = as.numeric(FINAL_MAT_CS),
    COD_TEXT = case_when(
      FINAL_MAT_CS == 1  ~ 'Road traffic accident',
      FINAL_MAT_CS == 2  ~ 'Obstetric haemorrhage',
      FINAL_MAT_CS == 3  ~ 'Pregnancy-induced hypertension',
      FINAL_MAT_CS == 4  ~ 'Pregnancy-related sepsis',
      FINAL_MAT_CS == 5  ~ 'ARI, including pneumonia',
      FINAL_MAT_CS == 6  ~ 'HIV/AIDS related death',
      FINAL_MAT_CS == 7  ~ 'Reproductive neoplasms MMF',
      FINAL_MAT_CS == 8  ~ 'Pulmonary TB',
      FINAL_MAT_CS == 9  ~ 'Malaria',
      FINAL_MAT_CS == 10 ~ 'Meningitis',
      FINAL_MAT_CS == 11 ~ 'Diarrheal diseases',
      FINAL_MAT_CS == 12 ~ 'Abortion-related death',
      FINAL_MAT_CS == 13 ~ 'Ectopic pregnancy',
      FINAL_MAT_CS == 14 ~ 'Other and unspecified cardiac diseases',
      FINAL_MAT_CS == 15 ~ 'Other infectious diseases',
      FINAL_MAT_CS == 16 ~ 'Other NCD',
      FINAL_MAT_CS == 18 ~ 'Indeterminate',
      FINAL_MAT_CS == 17 ~ FINAL_MAT_OTHR_SPFY_CS,
      TRUE ~ "Unknown" ), # Default for any other values
    SITE = "Ghana") %>% select(SITE, MOMID, PREGID, COD, COD_DATE = FINAL_MAT_DAT, COD_TEXT, IVA_MAT_CS1, VA_COMPL, IVA_MAT_OTHR_SPFY_CS1)

ghana_inf <- ghana_cod %>%
  filter (VA_COMPL_FORM %in% c(2,3) | FINAL_INF_CS %in% c (1:18)) %>%
  select (MOMID, PREGID, INFANTID, FINAL_INF_CS, FINAL_INF_OTHR_SPFY_CS, FINAL_INF_DAT, IVA_INF_CS1, VA_COMPL, IVA_INF_OTHR_SPFY_CS1) %>%
  mutate(
    COD = FINAL_INF_CS,
    COD_TEXT = case_when(
      FINAL_INF_CS == 1  ~ 'Fresh stillbirth',
      FINAL_INF_CS == 2  ~ 'Macerated stillbirth',
      FINAL_INF_CS == 4  ~ 'Prematurity',
      FINAL_INF_CS == 5  ~ 'Birth asphyxia',
      FINAL_INF_CS == 6  ~ 'Tetanus',
      FINAL_INF_CS == 7  ~ 'Congenital malformation',
      FINAL_INF_CS == 8  ~ 'Diarrheal diseases',
      FINAL_INF_CS == 9  ~ 'Acute respiratory infection including pneumonia',
      FINAL_INF_CS == 10 ~ 'Meningitis and encephalitis',
      FINAL_INF_CS == 11 ~ 'Neonatal pneumonia',
      FINAL_INF_CS == 12 ~ 'Neonatal sepsis',
      FINAL_INF_CS == 13 ~ 'Road traffic accident',
      FINAL_INF_CS == 14 ~ 'Other and unspecified infectious diseases',
      FINAL_INF_CS == 15 ~ 'Other and unspecified cardiac diseases',
      FINAL_INF_CS == 16 ~ 'Severe malnutrition',
      FINAL_INF_CS == 17 ~ 'Renal failure',
      FINAL_INF_CS == 18 ~ FINAL_INF_OTHR_SPFY_CS,  # Other, specify
      FINAL_INF_CS == 19 ~ 'Indeterminate',
      TRUE ~ "Unknown" ),
    SITE = "Ghana")  %>%
  select (SITE, MOMID, PREGID, INFANTID, COD, COD_TEXT, COD_DATE = FINAL_INF_DAT, IVA_INF_CS1, VA_COMPL, IVA_INF_OTHR_SPFY_CS1)


##India-CMC----
#india_cmc_cod <- read.csv (paste0("Z:/SynapseCSVs/India_CMC/", UploadDate, "/mnh37.csv")) 
india_cmc_cod <- read.csv (paste0("Z:/PRISMA_Data_Uploads/", UploadDate, "/", UploadDate, "_cmc/mnh37.csv"))

#india_cmc_cod <- read.csv (paste0("Z:/SynapseCSVs/India_CMC/", "2025-04-17", "/mnh37.csv")) 
# india_cmc_cod <- read.csv(paste0("Z:/Stacked Data/", UploadDate, "/mnh37_merged.csv")) %>%
#   filter(SITE == "India-CMC") %>%
#   rename_with(~ str_remove(., "^M37_"))

india_cmc_mat <- india_cmc_cod %>%
  filter (VA_TYPE == 1 | FINAL_MAT_CS %in% c (1:18)) %>%
  select (MOMID, PREGID, FINAL_MAT_DAT, FINAL_MAT_CS, IVA_MAT_CS1, VA_COMPL, IVA_MAT_OTHR_SPFY_CS1, FINAL_MAT_OTHR_SPFY_CS)  %>%
  mutate(
    COD = FINAL_MAT_CS,
    COD_TEXT = case_when(
      FINAL_MAT_CS == 1  ~ 'Road traffic accident',
      FINAL_MAT_CS == 2  ~ 'Obstetric haemorrhage',
      FINAL_MAT_CS == 3  ~ 'Pregnancy-induced hypertension',
      FINAL_MAT_CS == 4  ~ 'Pregnancy-related sepsis',
      FINAL_MAT_CS == 5  ~ 'ARI, including pneumonia',
      FINAL_MAT_CS == 6  ~ 'HIV/AIDS related death',
      FINAL_MAT_CS == 7  ~ 'Reproductive neoplasms MMF',
      FINAL_MAT_CS == 8  ~ 'Pulmonary TB',
      FINAL_MAT_CS == 9  ~ 'Malaria',
      FINAL_MAT_CS == 10 ~ 'Meningitis',
      FINAL_MAT_CS == 11 ~ 'Diarrheal diseases',
      FINAL_MAT_CS == 12 ~ 'Abortion-related death',
      FINAL_MAT_CS == 13 ~ 'Ectopic pregnancy',
      FINAL_MAT_CS == 14 ~ 'Other and unspecified cardiac diseases',
      FINAL_MAT_CS == 15 ~ 'Other infectious diseases',
      FINAL_MAT_CS == 16 ~ 'Other NCD',
      FINAL_MAT_CS == 18 ~ 'Indeterminate',
      FINAL_MAT_CS == 17 ~ FINAL_MAT_OTHR_SPFY_CS,
      TRUE ~ "Unknown" ), # Default for any other values
    SITE = "India-CMC") %>% select(SITE, MOMID, PREGID, COD, COD_DATE = FINAL_MAT_DAT, COD_TEXT, IVA_MAT_CS1, VA_COMPL, IVA_MAT_OTHR_SPFY_CS1)

india_cmc_inf <- india_cmc_cod %>% 
  filter (VA_TYPE == 2 | FINAL_INF_CS %in% c (1:18)) %>% 
  select (MOMID, PREGID, INFANTID, FINAL_INF_CS, FINAL_INF_OTHR_SPFY_CS, FINAL_INF_DAT, IVA_INF_CS1, VA_COMPL, IVA_INF_OTHR_SPFY_CS1) %>%
  mutate(
    COD = FINAL_INF_CS,
    COD_TEXT = case_when(
      FINAL_INF_CS == 1  ~ 'Fresh stillbirth',
      FINAL_INF_CS == 2  ~ 'Macerated stillbirth',
      FINAL_INF_CS == 4  ~ 'Prematurity',
      FINAL_INF_CS == 5  ~ 'Birth asphyxia',
      FINAL_INF_CS == 6  ~ 'Tetanus',
      FINAL_INF_CS == 7  ~ 'Congenital malformation',
      FINAL_INF_CS == 8  ~ 'Diarrheal diseases',
      FINAL_INF_CS == 9  ~ 'Acute respiratory infection including pneumonia',
      FINAL_INF_CS == 10 ~ 'Meningitis and encephalitis',
      FINAL_INF_CS == 11 ~ 'Neonatal pneumonia',
      FINAL_INF_CS == 12 ~ 'Neonatal sepsis',
      FINAL_INF_CS == 13 ~ 'Road traffic accident',
      FINAL_INF_CS == 14 ~ 'Other and unspecified infectious diseases',
      FINAL_INF_CS == 15 ~ 'Other and unspecified cardiac diseases',
      FINAL_INF_CS == 16 ~ 'Severe malnutrition',
      FINAL_INF_CS == 17 ~ 'Renal failure',
      FINAL_INF_CS == 18 ~ FINAL_INF_OTHR_SPFY_CS,  # Other, specify
      FINAL_INF_CS == 19 ~ 'Indeterminate',
      TRUE ~ "Unknown" ), 
    SITE = "India-CMC")  %>%
  select (SITE, MOMID, PREGID, INFANTID, COD, COD_TEXT, COD_DATE = FINAL_INF_DAT, IVA_INF_CS1, VA_COMPL, IVA_INF_OTHR_SPFY_CS1)

##India-SAS----
india_sas_cod <- read.csv (paste0("Z:/PRISMA_Data_Uploads/", UploadDate, "/", UploadDate, "_sas/mnh37.csv"))

# india_sas_cod <- read.csv(paste0("Z:/Stacked Data/", UploadDate, "/mnh37_merged.csv")) %>%
#   filter(SITE == "India-SAS") %>%
#   rename_with(~ str_remove(., "^M37_"))

india_sas_mat <- india_sas_cod %>%
  filter (VA_TYPE == 1 | FINAL_MAT_CS %in% c (1:18)) %>%
  select (MOMID, PREGID, FINAL_MAT_DAT, FINAL_MAT_CS, IVA_MAT_CS1, VA_COMPL, IVA_MAT_OTHR_SPFY_CS1, FINAL_MAT_OTHR_SPFY_CS)  %>%
  mutate(
    COD = FINAL_MAT_CS,
    COD_TEXT = case_when(
      FINAL_MAT_CS == 1  ~ 'Road traffic accident',
      FINAL_MAT_CS == 2  ~ 'Obstetric haemorrhage',
      FINAL_MAT_CS == 3  ~ 'Pregnancy-induced hypertension',
      FINAL_MAT_CS == 4  ~ 'Pregnancy-related sepsis',
      FINAL_MAT_CS == 5  ~ 'ARI, including pneumonia',
      FINAL_MAT_CS == 6  ~ 'HIV/AIDS related death',
      FINAL_MAT_CS == 7  ~ 'Reproductive neoplasms MMF',
      FINAL_MAT_CS == 8  ~ 'Pulmonary TB',
      FINAL_MAT_CS == 9  ~ 'Malaria',
      FINAL_MAT_CS == 10 ~ 'Meningitis',
      FINAL_MAT_CS == 11 ~ 'Diarrheal diseases',
      FINAL_MAT_CS == 12 ~ 'Abortion-related death',
      FINAL_MAT_CS == 13 ~ 'Ectopic pregnancy',
      FINAL_MAT_CS == 14 ~ 'Other and unspecified cardiac diseases',
      FINAL_MAT_CS == 15 ~ 'Other infectious diseases',
      FINAL_MAT_CS == 16 ~ 'Other NCD',
      FINAL_MAT_CS == 18 ~ 'Indeterminate',
      FINAL_MAT_CS == 17 ~ FINAL_MAT_OTHR_SPFY_CS,
      TRUE ~ "Unknown" ), # Default for any other values
    SITE = "India-SAS") %>% select(SITE, MOMID, PREGID, COD, COD_DATE = FINAL_MAT_DAT, COD_TEXT, IVA_MAT_CS1, VA_COMPL, IVA_MAT_OTHR_SPFY_CS1)

india_sas_inf <- india_sas_cod %>% 
  filter (VA_TYPE == 2 | FINAL_INF_CS %in% c (1:18)) %>% 
  select (MOMID, PREGID, INFANTID, FINAL_INF_CS, FINAL_INF_OTHR_SPFY_CS, FINAL_INF_DAT, IVA_INF_CS1, VA_COMPL, IVA_INF_OTHR_SPFY_CS1) %>%
  mutate(
    COD = FINAL_INF_CS,
    COD_TEXT = case_when(
      FINAL_INF_CS == 1  ~ 'Fresh stillbirth',
      FINAL_INF_CS == 2  ~ 'Macerated stillbirth',
      FINAL_INF_CS == 4  ~ 'Prematurity',
      FINAL_INF_CS == 5  ~ 'Birth asphyxia',
      FINAL_INF_CS == 6  ~ 'Tetanus',
      FINAL_INF_CS == 7  ~ 'Congenital malformation',
      FINAL_INF_CS == 8  ~ 'Diarrheal diseases',
      FINAL_INF_CS == 9  ~ 'Acute respiratory infection including pneumonia',
      FINAL_INF_CS == 10 ~ 'Meningitis and encephalitis',
      FINAL_INF_CS == 11 ~ 'Neonatal pneumonia',
      FINAL_INF_CS == 12 ~ 'Neonatal sepsis',
      FINAL_INF_CS == 13 ~ 'Road traffic accident',
      FINAL_INF_CS == 14 ~ 'Other and unspecified infectious diseases',
      FINAL_INF_CS == 15 ~ 'Other and unspecified cardiac diseases',
      FINAL_INF_CS == 16 ~ 'Severe malnutrition',
      FINAL_INF_CS == 17 ~ 'Renal failure',
      FINAL_INF_CS == 18 ~ FINAL_INF_OTHR_SPFY_CS,  # Other, specify
      FINAL_INF_CS == 19 ~ 'Indeterminate',
      TRUE ~ "Unknown" ), 
    SITE = "India-SAS")  %>%
  select (SITE, MOMID, PREGID, INFANTID, COD, COD_TEXT, COD_DATE = FINAL_INF_DAT, IVA_INF_CS1, VA_COMPL, IVA_INF_OTHR_SPFY_CS1)

library(lubridate)

# Define a function to apply to each dataset
clean_dataset <- function(data) {
  data %>%
    mutate(across(c(COD, IVA_INF_CS1, VA_COMPL), as.numeric),
           IVA_INF_OTHR_SPFY_CS1 = as.character (IVA_INF_OTHR_SPFY_CS1),
           COD_DATE = ymd(parse_date_time(COD_DATE, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))
}


clean_dataset_mat <- function(data) {
  data %>%
    mutate(across(c(COD, IVA_MAT_CS1, VA_COMPL), as.numeric),
           IVA_MAT_OTHR_SPFY_CS1 = as.character (IVA_MAT_OTHR_SPFY_CS1),
           COD_DATE = ymd(parse_date_time(COD_DATE, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))
}

# Apply cleaning function to each dataset----
zambia_inf <- clean_dataset(zambia_inf)
pakistan_inf <- clean_dataset(pakistan_inf)
ghana_inf <- clean_dataset(ghana_inf)
kenya_inf <- clean_dataset(kenya_inf)
india_cmc_inf <- clean_dataset(india_cmc_inf)
india_sas_inf <- clean_dataset(india_sas_inf)


zambia_mat <- clean_dataset_mat(zambia_mat)
pakistan_mat <- clean_dataset_mat(pakistan_mat)
ghana_mat <- clean_dataset_mat(ghana_mat)
kenya_mat <- clean_dataset_mat(kenya_mat)
india_cmc_mat <- clean_dataset_mat(india_cmc_mat)
india_sas_mat <- clean_dataset_mat(india_sas_mat)

# Bind all datasets and remove duplicates ----
all_inf_cod_in <- bind_rows(zambia_inf, pakistan_inf, ghana_inf, kenya_inf, india_cmc_inf, india_sas_inf) %>%
  distinct(SITE, MOMID, PREGID, INFANTID, .keep_all = TRUE) %>% 
  mutate (COD_FORM_COMPL = 1)

all_inf_cod <- all_inf_cod_in %>% distinct(SITE, MOMID, PREGID, INFANTID, .keep_all = TRUE) 


inf_dth_df <- infant_dth %>% 
  full_join(inf_death, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  full_join(all_inf_cod, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  select(-DOB) %>%
  left_join(
    inf_dob %>% select(SITE, MOMID, PREGID, INFANTID, DOB),
    by = c("SITE", "MOMID", "PREGID", "INFANTID")
  ) %>%
  mutate(
    # Convert character fields to datetime and initialize variables
    # DEATH_DATETIME = as.POSIXct(DEATH_DATETIME, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    # FETAL_LOSS_DATE = as.POSIXct(FETAL_LOSS_DATE, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    DEATH_DATETIME = as.Date(DEATH_DATETIME),
    FETAL_LOSS_DATE = as.Date(FETAL_LOSS_DATE),
    COD_DATE = as.character(COD_DATE),
    DTH_TME = as.character(DTH_TME),
    DEATH_DATE = as.Date(NA) # Initialize DEATH_DATE with NA
  ) %>%
  mutate(
    # First check FETAL_LOSS_DATE for death date
    DEATH_DATE = if_else(!is.na(FETAL_LOSS_DATE), as.Date(FETAL_LOSS_DATE), DEATH_DATE),
    SOURCE_DDATE = if_else(!is.na(FETAL_LOSS_DATE), "INF_OUT", NA_character_),
    
    # Then check MNH37 is available and DEATH_DATE is still missing
    DEATH_DATE = if_else(!is.na(COD_DATE) & is.na(DEATH_DATE), as.Date(COD_DATE), DEATH_DATE),
    SOURCE_DDATE = if_else(!is.na(COD_DATE) & is.na(SOURCE_DDATE), "MNH37", SOURCE_DDATE),
    
    # Then check DTH_TME if available and DEATH_DATE is still missing
    DEATH_DATE = if_else(!is.na(DTHDAT) & is.na(DEATH_DATE), as.Date(DTHDAT), DEATH_DATE),
    SOURCE_DDATE = if_else(!is.na(DTHDAT) & is.na(SOURCE_DDATE), "MNH24", SOURCE_DDATE),
    
    # Then check DEATH_DATETIME if DEATH_DATE is still missing
    DEATH_DATE = if_else(!is.na(DEATH_DATETIME) & is.na(DEATH_DATE), as.Date(DEATH_DATETIME), DEATH_DATE),
    SOURCE_DDATE = if_else(!is.na(DEATH_DATETIME) & is.na(SOURCE_DDATE), "INF_OUT", SOURCE_DDATE),
    
    # Stillbirth or fetal death case: set death date to DOB if applicable
    DEATH_DATE = if_else((STILLBIRTH_20WK == 1 | CLOSE_DSDECOD == 6) & !is.na(DOB) & is.na(DEATH_DATE), as.Date(DOB), DEATH_DATE),
    SOURCE_DDATE = if_else((STILLBIRTH_20WK == 1 | CLOSE_DSDECOD == 6) & !is.na(DOB) & is.na(SOURCE_DDATE), "DOB", SOURCE_DDATE),
    
    # Check if death date is missing or has impossible values
    DEATH_DATE_MISS = if_else(is.na(DEATH_DATE) | DEATH_DATE %in% as.Date(c("1905-05-05", "1907-07-07","1909-09-09","1909-09-07","1909-07-09")), 1, 0),
    
    # Calculate age at death and pregnancy end date
    DOB_Date = as.Date(DOB),
    END_PREG_CALC = round(as.numeric(difftime(DOB_Date, EDD_BOE, units = "days")), 0) + 280,
    
    # Calculate age at death based on death and birth dates
    AGE_DEATH = case_when(
      STILLBIRTH_20WK == 1 | CLOSE_DSDECOD == 6 ~ 0,  # If stillbirth or fetal death, then age is 0
     !is.na(DOB) & !is.na(DEATH_DATE) ~ round(abs(as.numeric(difftime(DEATH_DATE, DOB_Date, units = "days"))), 0),
      TRUE ~ -5
    ),
    
    # Determine the reason for death
    DEATH_RSN = case_when(
      STILLBIRTH_20WK == 1 | CLOSE_DSDECOD == 6 ~ 1, # Stillbirth
      NEO_DTH_CAT %in% c(11, 12, 13) | (CLOSE_DSDECOD == 3 & AGE_DEATH < 28) ~ 2, # Neonatal death (before 28 days)
      ((INF_DTH == 1 & !(NEO_DTH_CAT %in% c(11, 12, 13))) | CLOSE_DSDECOD == 3) & AGE_DEATH >= 28 ~ 3, # Livebirth after 28 days
      TRUE ~ 55
    ),
    
    # Calculate the latest expected date for VA forms based on death date or EDD
    LATE_DATE = case_when(
      DEATH_DATE_MISS == 0 ~ as.Date(DEATH_DATE) + 42, 
      DEATH_DATE_MISS == 1 & DEATH_RSN %in% c(1, 2) ~ as.Date(EDD_BOE) + 42, 
      TRUE ~ NA_Date_
    ),
    
    # Determine if verbal autopsy (VA) is due based on LATE_DATE and UploadDate
    VA_DUE = case_when(
      LATE_DATE <= UploadDate ~ 1,  # VA due if late date is on or before upload date
      LATE_DATE > UploadDate ~ 0,   # VA not due if late date is after upload date
      is.na(LATE_DATE) ~ 55         # Missing late date, assign 55 as placeholder
    )
  ) %>%
  # Select relevant columns for the final output
  select(SITE, INFANTID, MOMID, PREGID, DOB, EDD_BOE, AGE_DEATH, END_PREG_CALC, MNH24_CMPTE, 
         DEATH_DATE, CLOSE_DSDECOD, DEATH_DATE_M24 = DTH_TME, SOURCE_DDATE, DEATH_DATE_MISS, 
         DEATH_RSN, LATE_DATE, VA_DUE, FETAL_LOSS_DATE)



all_mat_cod <- bind_rows(zambia_mat, pakistan_mat, kenya_mat, 
                         india_cmc_mat, ghana_mat, india_sas_mat) %>% 
  distinct(SITE, MOMID, PREGID, .keep_all = TRUE) %>% 
  mutate (COD_FORM_COMPL = 1) 

#Sites VA renaming ----
data_dictionary <- read_excel("~/Analysis/Dictionary/va_dict.xlsx")

##The function is to rename variables to manually correct SITES variable name errors ----
## Function to remove duplicate columns
remove_duplicate_columns <- function(df) {
  df <- df[, !duplicated(names(df))]
  return(df)
}

## Function to clean column names to ensure valid UTF-8----
clean_column_names <- function(df) {
  names(df) <- names(df) %>%
    str_replace_all("[^[:ascii:]]", "") %>%
    make.names(unique = TRUE)
  return(df)
}

## Function to rename ID variables based on the data dictionary----
rename_vars <- function(df, dictionary) {
  for (i in 1:nrow(dictionary)) {
    pattern <- dictionary$`Variable name`[i]
    new_name <- dictionary$`Variable name`[i] # Adjusted to use the pattern as the new name
    if (!is.na(pattern) && !is.na(new_name) && new_name != "") {
      # Skip renaming if the pattern is already a variable in the dataframe
      if (new_name %in% names(df)) {
        next
      }
      
      case_insensitive_pattern <- paste0("(?i)\\b", str_replace_all(pattern, "_", "\\_"), "\\b")
      matched_columns <- names(df)[str_detect(names(df), case_insensitive_pattern)]
      
      # Remove duplicate columns if any
      if (length(matched_columns) > 1) {
        df <- df %>% select(-one_of(matched_columns[-1]))
      }
      
      # Perform renaming on the first matched column if it exists
      if (length(matched_columns) > 0) {
        df <- df %>% rename(!!sym(new_name) := !!sym(matched_columns[1]))
      }
    }
  }
  return(df)
}

## Function to clean and process the form ----
process_form <- function(df, form_name, dictionary, site) {
  # Clean column names to ensure valid UTF-8
  df <- clean_column_names(df)
  
  # Filter dictionary for the specific form
  dictionary <- dictionary %>% filter(Form == toupper(form_name))
  
  df <- remove_duplicate_columns(df)
  df <- rename_vars(df, dictionary)
  
  required_columns <- c("momid", "pregid", "infantid", "visit_obsstdat", "id10022", "id10019", "id10023_a")
  for (col in required_columns) {
    if (!tolower(col) %in% tolower(colnames(df))) {
      df[[toupper(col)]] <- NA
    }
  }
  
  colnames(df) <- toupper(make.names(colnames(df), unique = TRUE)) # Ensure unique column names
  df <- df %>%
    mutate(FORM = toupper(form_name), SITE = site) %>%  # Add the SITE variable
    select(all_of(toupper(required_columns)), FORM, SITE) %>%
    mutate(across(everything(), as.character))
  
  return(df)
}

## Function to read CSV with cleaning ----
# Use read.csv (base R)
safe_read_csv <- function(path) {
  if (file.info(path)$size == 0) {
    message("Empty file: ", path)
    return(NULL)
  }
  
  tryCatch({
    dat <- read.csv(path,
                    stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8-BOM")
    
    if (nrow(dat) == 0) {
      message("File has header but no data: ", path)
      return(NULL)
    }
    
    dat
  },
  error = function(e) {
    message("Error reading: ", path, " — ", e$message)
    NULL
  })
}


## Define the sites, forms, and upload date
sites <- c("Pakistan", "Kenya", "India_CMC", "India_SAS", "Ghana", "Zambia")
forms <- c("mnh27", "mnh28", "mnh29")
prefixes <- c("pak", "ke", "cmc", "sas", "gha", "zam")

## Lists to store processed data
mnh27_list <- list()
mnh28_mnh29_list <- list()

## Iterate over sites and forms ----
for (site in sites) {
  
  for (form in forms) {
    

    # Make a named vector for lookup
    site_prefix_map <- setNames(prefixes, sites)

    #file_path <- paste0("Z:/SynapseCSVs/", site, "/", UploadDate, "/", form, ".csv")
    file_path <- paste0("Z:/PRISMA_Data_Uploads/", UploadDate, "/", UploadDate, "_", site_prefix_map[site], "/", form, ".csv")
    
    
    if (!file.exists(file_path)) {
      message("File not found: ", file_path)
      next
    }
    
    form_data <- safe_read_csv(file_path)
    
    if (is.null(form_data)) next
    
    processed_data <- process_form(form_data, form, data_dictionary, site)
    
    if (is.null(processed_data)) next
    
    if (form == "mnh27") {
      mnh27_list[[paste0(site, "_", form)]] <- processed_data
    } else {
      mnh28_mnh29_list[[paste0(site, "_", form)]] <- processed_data
    }
  }
}

## Bind all SITES mnh27 and mnh28/29 forms ----
all_mnh27 <- bind_rows(mnh27_list, .id = "source")
all_mnh28_mnh29 <- bind_rows(mnh28_mnh29_list, .id = "source")


## for mnh forms, rename variables and transform VA dod to date time format
mat_form_comp <- all_mnh27 %>%
  mutate(ID10022 = tolower (ID10022),
         ID10019 = tolower (ID10022),
         DATA_COMP = 1,
         SITE = gsub("_", "-", SITE),
         #MOMID = ifelse(SITE == "Ghana", substr(PREGID, 1, nchar(PREGID) - 1), MOMID),         
         DEATH_DATE_VA = parse_date_time(ID10023_A, orders = c("%d-%m-%Y","%m/%d/%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y", "%d/%m/%Y", "%b %d %Y %I:%M%p"))) %>% 
  select (-source, -INFANTID)

#for mnh forms, rename variables and transform VA dod to date time format
inf_form_comp <- all_mnh28_mnh29 %>%
  mutate(ID10022 = tolower (ID10022),
         ID10019 = tolower (ID10022),
         DATA_COMP = 1,
         SITE = gsub("_", "-", SITE),
         INFANTID = ifelse(SITE == "Ghana", paste0(PREGID, "1"), INFANTID),
         #MOMID = ifelse(SITE == "Ghana", substr(PREGID, 1, nchar(PREGID) - 1), MOMID),         
         DEATH_DATE_VA = parse_date_time(ID10023_A, orders = c("%d-%m-%Y","%m/%d/%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y", "%d/%m/%Y", "%b %d %Y %I:%M%p"))) %>% 
  select (-source)

#remove spaces in between variables to avoid MOMID/PREGID/INFANTID mismatch
inf_dth_df$SITE <- trimws(inf_dth_df$SITE)
inf_dth_df$MOMID <- trimws(inf_dth_df$MOMID)
inf_dth_df$PREGID <- trimws(inf_dth_df$PREGID)
inf_dth_df$INFANTID <- trimws(inf_dth_df$INFANTID)

all_inf_cod$SITE <- trimws(all_inf_cod$SITE)
all_inf_cod$INFANTID <- trimws(all_inf_cod$INFANTID)

inf_dth_source$SITE <- trimws(inf_dth_source$SITE)
inf_dth_source$MOMID <- trimws(inf_dth_source$MOMID)
inf_dth_source$PREGID <- trimws(inf_dth_source$PREGID)
inf_dth_source$INFANTID <- trimws(inf_dth_source$INFANTID)

#Combine all maternal and infant datasets for final COD outcomes ----

## Final Infant Datasets ----

### Join inf_dth_df and all_inf_cod by INFANTID, MOMID, and PREGID ----
merged_inf_df1 <- left_join (inf_dth_df, all_inf_cod, by = c( "SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  distinct(SITE, INFANTID, .keep_all = TRUE)  
  
### Then join the result with inf_form_comp ----
all_inf_merged <- left_join(merged_inf_df1, inf_form_comp, by = c("INFANTID", "MOMID", "PREGID", "SITE")) %>% 
  left_join (inf_dth_source, by = c( "SITE", "MOMID", "INFANTID", "PREGID")) 

### All merged infant mortality dataset ----     
all_inf_merged <- all_inf_merged %>%
  # NEW: bring in permanent VA info now
  left_join(perm_infant, by = c("SITE", "INFANTID")) %>%
  mutate(
    DEATH_DATE_TRC = as.Date(trunc(DEATH_DATE, 'days')),
    
    # NEW: unified status (PERM overrides everything)
    VA_STATUS = case_when(
      perm_va == TRUE                ~ "PERM",
      VA_DUE == 1 & is.na(FORM)      ~ "YES",
      TRUE                           ~ "NO"
    ),
    
    # keep your numeric VA_MISS for existing logic (PERM is not a “query”)
    VA_MISS = ifelse(VA_STATUS == "YES", 1, 0),                              
                              # If DEATH_SOURCE is missing and MNH23 data is complete (MNH23_CMPTE == 1), set DEATH_SOURCE to "MNH23"
                              DEATH_SOURCE = case_when(is.na(SOURCE_DTH) & MNH24_CMPTE == 1 ~ "MNH24", 
                                                       is.na(SOURCE_DTH) & COD_FORM_COMPL == 1 ~ "MNH37",
                                                       is.na(SOURCE_DTH) & !is.na(FORM) == 1 ~ FORM,
                                                       is.na(SOURCE_DTH) & !is.na(FETAL_LOSS_DATE) == 1 ~ "MNH04",
                                                       TRUE ~ SOURCE_DTH),
                              
                              DDTH_VA_MISS = case_when (VA_DUE == 0 | VA_MISS == 1 ~ 55,
                                                     is.na(DEATH_DATE_VA) | DEATH_DATE_VA %in% as.Date(c("1905-05-05", "1907-07-07","1909-09-09","1909-09-07","1909-07-09")) ~ 1, 
                                                     !is.na(DEATH_DATE_VA) & !(DEATH_DATE_VA %in% as.Date(c("1905-05-05", "1907-07-07","1909-09-09","1909-09-07","1909-07-09"))) ~ 0, 
                                                     TRUE ~ 77), #is death date miss? Yes-1, No-0
                              
                              COD_FORM_MISS = case_when (COD_FORM_COMPL == 1 ~ 0,
                                                         VA_DUE == 1 & is.na(COD_FORM_COMPL) ~ 1,
                                                         TRUE ~ 55), #is cause of death form missing? Yes-1, No-0, 55- Not yet due
                              
                              COD_MISS = ifelse (COD_FORM_COMPL == 1 & (is.na(COD) | COD %in% c(77, 55)), 1, 0), #is cause of death missing in the form? Yes-1, No-0 
                              
                              #is dod in VA the same as dod in crfs, Yes, the are equal - 1, No, not equal - 0
                              DOD_EQ =  case_when (is.na (DEATH_DATE_TRC) | is.na (DEATH_DATE_VA) ~ 55, 
                                                   DEATH_DATE_VA == DEATH_DATE_TRC ~ 1,
                                                   DEATH_DATE_VA != DEATH_DATE_TRC ~ 0,
                                                    TRUE ~ NA),
                              #is the adequate form for status at death the right form filled for infants:
                              #Yes, the form is correct - 1, No, not correct - 0
                              FORM_EQ = case_when ( is.na(FORM) & VA_DUE == 1 ~ 55,
                                                    is.na(FORM) & VA_DUE == 0 ~ 77,
                                                    DEATH_RSN %in% c(1,2) & FORM == "MNH28" ~ 1,
                                                    DEATH_RSN == 3 & FORM == "MNH29" ~ 1,
                                                    DEATH_RSN %in% c(1,2) & FORM == "MNH29" ~ 0,
                                                    DEATH_RSN == 3 & FORM == "MNH28" ~ 0,
                                                    TRUE ~ NA),
                              
                              #is the stillbirth classified death correct or is stillbirth misclassified?
                              STILLBIRTH_EQ = case_when(is.na(DEATH_RSN) | is.na(COD) | COD %in% c(77, 55) ~ 55,
                                                         DEATH_RSN %in% c(2, 3) & COD %in% c(1,2) ~ 0,
                                                         TRUE ~ 1),
                              
                              IV_FINAL_DIFF_COD = case_when (COD_FORM_COMPL == 1 & IVA_INF_CS1 != COD ~ 1,
                                                             COD_FORM_COMPL == 1 & IVA_INF_CS1 == COD ~ 0,
                                                             TRUE ~ NA)) %>%
  
                      select(SITE, INFANTID, MOMID, PREGID, DOB, EDD_BOE, AGE_DEATH, 
                             END_PREG_CALC, DEATH_SOURCE, DEATH_DATE, DEATH_DATE_M24, SOURCE_DDATE, 
                             DEATH_DATE_MISS, DDTH_VA_MISS, MNH24_CMPTE, DEATH_RSN, LATE_DATE, 
                             VA_DUE, COD, COD_TEXT, IVA_INF_CS1, FORM, IV_FINAL_DIFF_COD, VA_COMPL,
                             DATA_COMP, DEATH_DATE_VA, DEATH_DATE_TRC, VA_MISS, VA_STATUS, 
                             COD_DATE, COD_MISS, COD_FORM_MISS, DOD_EQ, FORM_EQ, STILLBIRTH_EQ)

##Final Maternal Datasets ----
### Join mat_dth_df and all_mat_cod by matANTID, MOMID, and PREGID----
merged_mat_df1 <- left_join (mat_dth_df, all_mat_cod, by = c( "SITE", "MOMID", "PREGID")) %>% 
                  left_join (mat_dth_source, by = c( "SITE", "MOMID", "PREGID")) %>% 
                  distinct(SITE, MOMID, PREGID, .keep_all = TRUE)  

### Then join the result with mat_form_comp ----
all_mat_merged <- left_join(merged_mat_df1, mat_form_comp, by = c("MOMID", "PREGID", "SITE"))

### All merged maternal mortality dataset ----     
all_mat_merged <- all_mat_merged %>% 
  # NEW: bring in permanent VA info now
  left_join(perm_maternal, by = c("SITE", "MOMID", "PREGID")) %>%
  mutate(
    # unified status (PERM overrides everything)
    VA_STATUS = case_when(
      perm_va == TRUE                ~ "PERM",
      VA_DUE == 1 & is.na(FORM)      ~ "YES",
      TRUE                           ~ "NO"
    ),
    
    # keep numeric VA_MISS for your existing logic
    VA_MISS = ifelse(VA_STATUS == "YES", 1, 0),
          
    COD_FORM_MISS = case_when (COD_FORM_COMPL == 1 ~ 0,
                               VA_DUE == 1 & is.na(COD_FORM_COMPL) ~ 1,
                               TRUE ~ 55), #is cause of death form missing? Yes-1, No-0, 55- Not yet due
    
    # If DEATH_SOURCE is missing and MNH23 data is complete (MNH23_CMPTE == 1), set DEATH_SOURCE to "MNH23"
    DEATH_SOURCE = case_when( is.na(SOURCE_DTH) & MNH23_CMPTE == 1 ~ "MNH23",
                              TRUE ~ SOURCE_DTH),
                              
    COD_MISS = ifelse (COD_FORM_COMPL == 1 & (is.na(COD) | COD %in% c(77, 55)), 1, 0), #is cause of death missing in the form? Yes-1, No-0 
    
    DDTH_VA_MISS = case_when (VA_DUE == 0 | VA_MISS == 1 ~ 55,
                              is.na(DEATH_DATE_VA) | DEATH_DATE_VA %in% as.Date(c("1905-05-05", "1907-07-07","1909-09-09","1909-09-07","1909-07-09")) ~ 1, 
                              !is.na(DEATH_DATE_VA) & !(DEATH_DATE_VA %in% as.Date(c("1905-05-05", "1907-07-07","1909-09-09","1909-09-07","1909-07-09"))) ~ 0, 
                           
                                 TRUE ~ 77), #is death date miss? Yes-1, No-0
    DEATH_DATE_VA_TRC =  as.Date(trunc(DEATH_DATE_VA, 'days')),
    
    DOD_EQ =  case_when (is.na (DEATH_DATE) | is.na (DEATH_DATE_VA_TRC) ~ 55, 
                         DEATH_DATE_VA_TRC == DEATH_DATE ~ 1,
                         DEATH_DATE_VA_TRC != DEATH_DATE ~ 0,
                         TRUE ~ NA),
    
    IV_FINAL_DIFF_COD = case_when (COD_FORM_COMPL == 1 & IVA_MAT_CS1 != COD ~ 1,
                                   COD_FORM_COMPL == 1 & IVA_MAT_CS1 == COD ~ 0,
                                   
                                   TRUE ~ NA)) %>%
  select(SITE, MOMID, PREGID, DEATH_SOURCE, DEATH_DATE, DEATH_DATE_MISS, 
     DEATH_VAR = DEATH_TRIGGER, MNH23_CMPTE, VA_COMPL, VA_STATUS,
     IV_FINAL_DIFF_COD, COD_FORM_MISS, COD_MISS, LATE_DATE, VA_DUE, COD, COD_TEXT, 
     FORM, DATA_COMP, DEATH_DATE_VA, DDTH_VA_MISS, VA_MISS, COD_MISS, DOD_EQ, IVA_MAT_CS1, COD_DATE)

## Save final datasets to path ----                                           
write.csv(all_inf_merged, paste0(path_to_tnt, "INF_COD" ,".csv"), na="", row.names=FALSE)
write.csv(all_mat_merged, paste0(path_to_tnt, "MAT_COD" ,".csv"), na="", row.names=FALSE)
write.csv(all_inf_merged, paste0(path_to_save, "INF_COD" ,".csv"), na="", row.names=FALSE)
write.csv(all_mat_merged, paste0(path_to_save, "MAT_COD" ,".csv"), na="", row.names=FALSE)


#Developing site query list----

## 1) Build site_issues with PERM for maternal VA when applicable ----
site_issues <- all_mat_merged %>%
  # bring in permanent VA flag
  left_join(perm_maternal, by = c("MOMID", "PREGID","SITE")) %>%
  mutate(
    # other issue flags unchanged
    `MISSING MNH23 FORM`      = case_when(MNH23_CMPTE == 1 ~ "NO", TRUE ~ "YES"),
    `MISSING MNH37 FORM`      = case_when(COD_FORM_MISS == 0 ~ "NO", TRUE ~ "YES"),
    `CAUSE OF DEATH MISSING`  = case_when(is.na(COD) | COD %in% c(77, 55) ~ "YES", TRUE ~ "NO"),
    # VA: use PERM when flagged; else NO/YES as before
    `MISSING VA (MNH27) FORM` = VA_STATUS) %>%
  select(
    SITE, MOMID, PREGID, DEATH_DATE, DEATH_SOURCE, DEATH_VAR, COD_TEXT,
    `MISSING MNH23 FORM`, `MISSING MNH37 FORM`, `CAUSE OF DEATH MISSING`,
    `MISSING VA (MNH27) FORM`
  ) %>%
  filter(!is.na(DEATH_SOURCE))

## 2) Build site_issues_inf with PERM for infant VA when applicable ----
site_issues_inf <- all_inf_merged %>%
  # bring in permanent VA flag
  left_join(perm_infant, by = c("INFANTID", "SITE")) %>%
  mutate(
    `MISSING MNH24 FORM`          = case_when(MNH24_CMPTE == 1 ~ "NO", TRUE ~ "YES"),
    `MISSING MNH37 FORM`          = case_when(COD_FORM_MISS == 0 ~ "NO", TRUE ~ "YES"),
    `CAUSE OF DEATH MISSING`      = case_when(is.na(COD) | COD %in% c(77, 55) ~ "YES", TRUE ~ "NO"),
    `MISSING VA (MNH28/29) FORM`  = VA_STATUS,
    `INCORRECT VA FORM`           = case_when(FORM_EQ == 0 & DATA_COMP == 1 ~ "YES", TRUE ~ "NO"),
    `VA FORM COMPLETED`           = FORM
  ) %>%
  select(
    SITE, MOMID, PREGID, INFANTID, DEATH_DATE, AGE_DEATH, DEATH_SOURCE,
    COD_TEXT, `VA FORM COMPLETED`,
    `MISSING MNH24 FORM`, `MISSING MNH37 FORM`, `CAUSE OF DEATH MISSING`,
    `MISSING VA (MNH28/29) FORM`, `INCORRECT VA FORM`
  ) %>%
  filter(!is.na(DEATH_SOURCE))

## 3) Output styling & writing (adds yellow for PERM and keeps filtering by "YES") ----
library(dplyr)
library(openxlsx)

output_dir <- file.path("~/Analysis/Verbal Autopsy", UploadDate)
dir.create(output_dir, showWarnings = FALSE)

mom_issue_cols <- c("MISSING MNH23 FORM", "MISSING MNH37 FORM", "CAUSE OF DEATH MISSING", "MISSING VA (MNH27) FORM")
inf_issue_cols <- c("MISSING MNH24 FORM", "MISSING MNH37 FORM", "CAUSE OF DEATH MISSING", "MISSING VA (MNH28/29) FORM", "INCORRECT VA FORM")

all_sites <- union(unique(site_issues$SITE), unique(site_issues_inf$SITE))

# styles
red_style <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE", textDecoration = "bold")
# yellow style for PERM
yellow_style <- createStyle(fontColour = "#7A5900", bgFill = "#FFF59D", textDecoration = "bold")
header_style <- createStyle(
  fontColour = "#000000", fgFill = "#EAFFE1",
  halign = "center", valign = "center",
  textDecoration = "bold", wrapText = TRUE,
  border = "TopBottomLeftRight", borderColour = "#000000", borderStyle = "thin"
)

for (s in all_sites) {
  mom_data <- site_issues %>%
    filter(SITE == s) %>%
    # keep only rows with an actual query (YES). "PERM" is *not* a query.
    filter(if_any(all_of(mom_issue_cols), ~ .x == "YES"))
  
  inf_data <- site_issues_inf %>%
    filter(SITE == s) %>%
    filter(if_any(all_of(inf_issue_cols), ~ .x == "YES"))
  
  if (nrow(mom_data) > 0 || nrow(inf_data) > 0) {
    wb <- createWorkbook()
    
    if (nrow(mom_data) > 0) {
      addWorksheet(wb, "MOM_ISSUES")
      writeData(wb, "MOM_ISSUES", mom_data, headerStyle = header_style)
      setColWidths(wb, "MOM_ISSUES", cols = 1:ncol(mom_data), widths = "auto")
      setRowHeights(wb, "MOM_ISSUES", rows = 1, height = 35)
      
      # "YES" -> red
      conditionalFormatting(
        wb, "MOM_ISSUES",
        cols = which(names(mom_data) %in% mom_issue_cols),
        rows = 2:(nrow(mom_data) + 1),
        rule = '=="YES"', style = red_style
      )
      # "PERM" -> yellow (for completeness, in case any PERM sneaks in)
      conditionalFormatting(
        wb, "MOM_ISSUES",
        cols = which(names(mom_data) %in% mom_issue_cols),
        rows = 2:(nrow(mom_data) + 1),
        rule = '=="PERM"', style = yellow_style
      )
      
      freezePane(wb, "MOM_ISSUES", firstRow = TRUE)
    }
    
    if (nrow(inf_data) > 0) {
      addWorksheet(wb, "INFANT_ISSUES")
      writeData(wb, "INFANT_ISSUES", inf_data, headerStyle = header_style)
      setColWidths(wb, "INFANT_ISSUES", cols = 1:ncol(inf_data), widths = "auto")
      setRowHeights(wb, "INFANT_ISSUES", rows = 1, height = 35)
      
      conditionalFormatting(
        wb, "INFANT_ISSUES",
        cols = which(names(inf_data) %in% inf_issue_cols),
        rows = 2:(nrow(inf_data) + 1),
        rule = '=="YES"', style = red_style
      )
      conditionalFormatting(
        wb, "INFANT_ISSUES",
        cols = which(names(inf_data) %in% inf_issue_cols),
        rows = 2:(nrow(inf_data) + 1),
        rule = '=="PERM"', style = yellow_style
      )
      
      freezePane(wb, "INFANT_ISSUES", firstRow = TRUE)
    }
    
    saveWorkbook(wb, file.path(output_dir, paste0(s, "_", UploadDate, "_Mortality_Issues.xlsx")), overwrite = TRUE)
  }
}
