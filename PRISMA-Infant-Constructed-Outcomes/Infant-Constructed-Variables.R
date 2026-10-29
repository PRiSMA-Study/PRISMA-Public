#*****************************************************************************
#* PRISMA Infant Outcomes
#* Drafted: 21 September 2023, Stacie Loisate
#* Last updated: 26 March 2026 (added in loop for converting mnh09 to long format; added in loop for converting tcb units based on means)

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
# 10. PSBI

#The first section, CONSTRUCTED VARIABLES GENERATION, below, the code generates datasets for 
#each form with additional variables that will be used for multiple outcomes. For example, mnh01_constructed 
#is a dataset that will be used for several outcomes. 

# The `growthstandards` R package allows the user to pull INTERGROWTH centiles for 
# newborn length, weight, and head circumference. 
# R Package linked: (https://ki-tools.github.io/growthstandards/articles/usage.html#intergrowth-newborn-standard-1).

# the package can be downloaded using the following code: 
# install.packages("remotes") # if "remotes" is not already installed 
# remotes::install_github("ki-tools/growthstandards") 

## ➤ TCB package:
## Go to:
## https://github.com/PRiSMA-Study/TCB-R-package
## → Download the file: TCB_0.1.0.tar.gz
## → Save it to your Downloads folder

## ➤ TSB.NICE package:
## Go to:
## https://github.com/PRiSMA-Study/PRISMA-Analysis-Yipeng/blob/main/TcB-Analysis/TcB-R-Package/NICE-Guideline/TSB.NICE_1.1.0.tar.gz
## → Click "Download" (or "Raw" → Save As)
## → Save it to your Downloads folder

## Install TCB
# install.packages(
#   "D:/Users/williams_pj/Downloads/TCB_0.1.0.tar.gz",
#   repos = NULL,
#   type = "source"
# )

## Install TSB.NICE
# install.packages(
#   "D:/Users/williams_pj/Downloads/TSB.NICE_1.1.0.tar.gz",
#   repos = NULL,
#   type = "source"
# )

#*****************************************************************************
# clear environment 
rm(list = ls())

#*****************************************************************************
# Data Setup ----
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
library(TSB.NICE) ##TSB package  
library(writexl)
library(haven)

# UPDATE EACH RUN # 
# set upload date 
UploadDate = "2026-09-18"

## Directory setup ----
# set path to data
path_to_data <- paste0("~/Analysis/Merged_data/", UploadDate)
path_to_tnt <- paste0("Z:/Constructed Data/", UploadDate, "/")

# set path to save 
path_to_save <- paste0("~/Analysis/Infant-Constructed-Variables/data/",  UploadDate, "/")
dir.create(path_to_save, recursive = TRUE, showWarnings = FALSE)

path_to_output <- paste0("~/Analysis/Infant-Constructed-Variables/output/", UploadDate, "/")
dir.create(path_to_output, recursive = TRUE, showWarnings = FALSE)
path_to_save_figures <- path_to_output

path_to_save_queries <- paste0("~/Analysis/Infant-Constructed-Variables/queries/", UploadDate, "/")
dir.create(path_to_save_queries, recursive = TRUE, showWarnings = FALSE)


## Import forms ----
mnh01 <- read.csv(paste0(path_to_data,"/", "mnh01_merged.csv"))

mnh02 <- read.csv(paste0(path_to_data,"/", "mnh02_merged.csv"))

mnh04 <- read.csv(paste0(path_to_data,"/", "mnh04_merged.csv"))

mnh08 <- read.csv(paste0(path_to_data,"/", "mnh08_merged.csv"))

mnh09 <- read.csv(paste0(path_to_data,"/", "mnh09_merged.csv"))

mnh11 <- read.csv(paste0(path_to_data,"/", "mnh11_merged.csv"))

mnh13 <- read.csv(paste0(path_to_data,"/", "mnh13_merged.csv"))

mnh14 <- read.csv(paste0(path_to_data,"/", "mnh14_merged.csv"))

mnh15 <- read.csv(paste0(path_to_data,"/", "mnh15_merged.csv"))

mnh19 <- read.csv(paste0(path_to_data,"/", "mnh19_merged.csv"))

mnh20 <- read.csv(paste0(path_to_data,"/", "mnh20_merged.csv"))

mnh24 <- read.csv(paste0(path_to_data,"/", "mnh24_merged.csv"))

mat_enroll <-  haven::read_dta(paste0(path_to_tnt, "/MAT_STUDYSTATUS.dta"))

mat_endpoints <- mat_enroll

# mat_endpoints <- haven::read_dta(paste0(path_to_tnt, "/MAT_ENDPOINTS.dta"))

## For sites that are not reporting MOMID/PREGID in MNH01 for enrollment visits, we will merge these IDs from MNH02 into MNH01
# ## zambia ids
# mnh02_zam_ids <- mnh02 %>% filter(SITE == "Zambia") %>% select(SCRNID, MOMID, PREGID)                  # export mnh02 ids
# mnh01_zam <- mnh01 %>% filter(SITE == "Zambia", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>%  # pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
#   left_join(mnh02_zam_ids, by = c("SCRNID"))
# mnh01_all <- mnh01 %>% filter(SITE != "Zambia") # extract site-specific from merged data 
# mnh01 <- bind_rows(mnh01_zam, mnh01_all)        # rebind data 
# 
# ## kenya ids
# mnh02_ke_ids <- mnh02 %>% filter(SITE == "Kenya") %>% select(SCRNID, MOMID, PREGID) ## export mnh02 ids
# mnh01_ke <- mnh01 %>% filter(SITE == "Kenya", M01_TYPE_VISIT == 1)  %>% 
#   select(-MOMID, -PREGID) %>%                                                       ## pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
#   left_join(mnh02_ke_ids, by = c("SCRNID"))
# mnh01_all <- mnh01 %>% filter(SITE != "Kenya")                                      ## extract site-specific from merged data 
# mnh01 <- bind_rows(mnh01_ke, mnh01_all)                                             ## rebind data 
# 
# ## CMC ids
# mnh02_cmc_ids <- mnh02 %>% filter(SITE == "India-CMC") %>% select(SCRNID, MOMID, PREGID)                 # export mnh02 ids
# mnh01_cmc <- mnh01 %>% filter(SITE == "India-CMC", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>% # pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
#   left_join(mnh02_cmc_ids, by = c("SCRNID"))
# mnh01_all <- mnh01 %>% filter(SITE != "India-CMC")                                                       # extract site-specific from merged data 
# mnh01 <- bind_rows(mnh01_cmc, mnh01_all)                                                                 # rebind data 

##MNH01 processing IDs ----
## For sites that are not reporting MOMID/PREGID in MNH01 for enrollment visits, we will merge these IDs from MNH02 into MNH01
na_variations <- c("n/a", "N/A", "na", "NA", "", " ")

is_missing <- function(x) {
  x2 <- trimws(tolower(as.character(x)))
  is.na(x) | x2 %in% trimws(tolower(na_variations))
}

# Normalize missing-like values to NA
for (col in c("SITE", "SCRNID", "MOMID", "PREGID")) {
  if (col %in% names(mnh01)) mnh01[[col]][is_missing(mnh01[[col]])] <- NA
  if (col %in% names(mnh02)) mnh02[[col]][is_missing(mnh02[[col]])] <- NA
}

# Reference table (dedup)
ref <- unique(mnh02[, c("SITE", "SCRNID", "MOMID", "PREGID")])

# Keep original order
mnh01$.rowid <- seq_len(nrow(mnh01))

# Split rows by what keys are available
has_scr <- !is_missing(mnh01$SCRNID)
has_m   <- !is_missing(mnh01$MOMID)
has_p   <- !is_missing(mnh01$PREGID)

idx_scr <- which(has_scr)
idx_mp  <- which(!has_scr & has_m & has_p)
idx_m   <- which(!has_scr & has_m & !has_p)
idx_p   <- which(!has_scr & !has_m & has_p)
idx_none<- which(!has_scr & !has_m & !has_p)

out_parts <- list()

# 1) Merge by SITE + SCRNID
if (length(idx_scr) > 0) {
  
  ref_scr <- unique(
    ref[!is_missing(ref$SCRNID),
        c("SITE","SCRNID","MOMID","PREGID")]
  )
  
  x <- merge(
    mnh01[idx_scr, ],
    ref_scr,
    by = c("SITE", "SCRNID"),
    all.x = TRUE,
    suffixes = c("", ".ref")
  )
  
  x$MOMID  <- ifelse(is_missing(x$MOMID),  x$MOMID.ref,  x$MOMID)
  x$PREGID <- ifelse(is_missing(x$PREGID), x$PREGID.ref, x$PREGID)
  
  x$MOMID.ref  <- NULL
  x$PREGID.ref <- NULL
  
  out_parts[["scr"]] <- x
}

# 2) Merge by SITE + MOMID + PREGID
if (length(idx_mp) > 0) {
  
  ref_mp <- unique(
    ref[
      !is_missing(ref$MOMID) &
        !is_missing(ref$PREGID) &
        !is_missing(ref$SCRNID),
      c("SITE","MOMID","PREGID","SCRNID")
    ]
  )
  
  # enforce unique mapping
  key <- paste(ref_mp$SITE, ref_mp$MOMID, ref_mp$PREGID, sep="||")
  ref_mp <- ref_mp[!duplicated(key), ]
  
  x <- merge(
    mnh01[idx_mp, ],
    ref_mp,
    by = c("SITE","MOMID","PREGID"),
    all.x = TRUE,
    suffixes = c("", ".ref")
  )
  
  x$SCRNID <- ifelse(is_missing(x$SCRNID), x$SCRNID.ref, x$SCRNID)
  x$SCRNID.ref <- NULL
  
  out_parts[["mp"]] <- x
}

# helper: keep only IDs mapping uniquely within SITE
unique_map <- function(df, key_col) {
  
  agg <- aggregate(
    df$SCRNID,
    by = list(df$SITE, df[[key_col]]),
    FUN = function(z) length(unique(z))
  )
  
  names(agg) <- c("SITE", key_col, "n_scr")
  
  df2 <- merge(df, agg, by = c("SITE", key_col), all.x = TRUE)
  
  df2 <- df2[df2$n_scr == 1,
             c("SITE", key_col, "SCRNID")]
  
  unique(df2)
}

# 3) Merge by SITE + MOMID only
if (length(idx_m) > 0) {
  
  ref_m <- ref[
    !is_missing(ref$MOMID) &
      !is_missing(ref$SCRNID),
    c("SITE","MOMID","SCRNID")
  ]
  
  ref_m <- unique_map(ref_m, "MOMID")
  
  x <- merge(
    mnh01[idx_m, ],
    ref_m,
    by = c("SITE", "MOMID"),
    all.x = TRUE,
    suffixes = c("", ".ref")
  )
  
  x$SCRNID <- ifelse(is_missing(x$SCRNID), x$SCRNID.ref, x$SCRNID)
  x$SCRNID.ref <- NULL
  
  out_parts[["m"]] <- x
}

# 4) Merge by SITE + PREGID only
if (length(idx_p) > 0) {
  
  ref_p <- ref[
    !is_missing(ref$PREGID) &
      !is_missing(ref$SCRNID),
    c("SITE","PREGID","SCRNID")
  ]
  
  ref_p <- unique_map(ref_p, "PREGID")
  
  x <- merge(
    mnh01[idx_p, ],
    ref_p,
    by = c("SITE", "PREGID"),
    all.x = TRUE,
    suffixes = c("", ".ref")
  )
  
  x$SCRNID <- ifelse(is_missing(x$SCRNID), x$SCRNID.ref, x$SCRNID)
  x$SCRNID.ref <- NULL
  
  out_parts[["p"]] <- x
}

# 5) Rows with no usable keys
if (length(idx_none) > 0) {
  out_parts[["none"]] <- mnh01[idx_none, ]
}

# Recombine + restore order
mnh01 <- do.call(rbind, out_parts)

mnh01 <- mnh01[order(mnh01$.rowid), ]

mnh01$.rowid <- NULL


###formatting date ----
mnh01$M01_US_OHOSTDAT <- as.Date(mnh01$M01_US_OHOSTDAT, format = "%Y-%m-%d")
mnh01 <-mnh01 %>% 
  ungroup() %>% 
  filter(M01_US_GA_DAYS_AGE_FTS1 != 'hence G.A more than 25weeks"') %>% 
  mutate(M01_US_GA_DAYS_AGE_FTS1 = as.numeric(M01_US_GA_DAYS_AGE_FTS1))


##Checking for duplicates ----
# List of data frames
form_names <- ls(pattern = "^mnh\\d+$")
forms_list <- mget(form_names)

# List of columns to check for duplicates in each form
duplicate_columns <- list(
  mnh00 = c("SITE", "SCRNID", "M00_SCRN_OBSSTDAT"),
  mnh01 = c("SITE", "SCRNID", "MOMID", "PREGID", "M01_TYPE_VISIT","M01_US_OHOSTDAT"),
  mnh02 = c("SITE", "SCRNID", "MOMID", "PREGID", "M02_SCRN_OBSSTDAT"),
  mnh04 = c("SITE", "MOMID", "PREGID", "M04_TYPE_VISIT", "M04_ANC_OBSSTDAT"),
  mnh08 = c("SITE", "MOMID", "PREGID", "M08_TYPE_VISIT", "M08_LBSTDAT"),
  mnh09 = c("SITE", "MOMID", "PREGID", "M09_MAT_LD_OHOSTDAT"),
  mnh11 = c("SITE", "MOMID", "PREGID","INFANTID",  "M11_VISIT_OBSSTDAT"),
  mnh13 = c("SITE", "MOMID", "PREGID","INFANTID",  "M13_TYPE_VISIT", "M13_VISIT_OBSSTDAT"),
  mnh14 = c("SITE", "MOMID", "PREGID","INFANTID",  "M14_TYPE_VISIT", "M14_VISIT_OBSSTDAT"),
  mnh15 = c("SITE", "MOMID", "PREGID","INFANTID",  "M15_TYPE_VISIT", "M15_OBSSTDAT"),
  mnh19 = c("SITE", "MOMID", "PREGID", "M19_OBSSTDAT"),
  mnh20 = c("SITE", "MOMID", "PREGID","INFANTID",  "M20_ADMIT_OHOSTDAT"),
  mnh24 = c("SITE", "MOMID", "PREGID","INFANTID",  "M24_CLOSE_DSSTDAT")
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

list2env(forms_list, envir = .GlobalEnv)

#*****************************************************************************
## Pull IDs of delivered infants ----
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
## Add constructed vars to forms that will be used across outcomes ----
#*****************************************************************************
### MNH01 ----
## add constructed vars for: 
# BOE_EDD, [varname: EDD_BOE]
# BOE_GA, [varnames: GESTAGE_ENROLL_BOE, BOE_GA_DAYS]
# Estimate conception date [varname: PREG_START_DATE]
### MNH01 ###
## add constructed vars for: 
# BOE_EDD, [varname: EDD_BOE]
# BOE_GA, [varnames: GESTAGE_ENROLL_BOE, BOE_GA_DAYS]
# Estimate conception date [varname: PREG_START_DATE]

mnh01_constructed <- mnh01 %>% 
  ## only want the first ultrasound visit -- take the earliest date for each participant -- USE TYPE_VISIT = 1 FOR NOW
  filter(M01_TYPE_VISIT == 1 & PREGID %in% enrolled_ids_vec) %>% 
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(-desc(M01_US_OHOSTDAT)) %>% 
  slice(1) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) %>% 
  ungroup() %>%
  select(SITE, MOMID, PREGID,  M01_US_OHOSTDAT)

### MNH04----
# this form will be used to pull any fetal losses reported in MNH04 
## add constructed vars for: 
# Gestational age at fetal loss [varname: GESTAGE_FETAL_LOSS_WKS]

mnh04_constructed <- mnh04 %>% 
  select(SITE, MOMID, PREGID,M04_ANC_OBSSTDAT,M04_TYPE_VISIT, M04_MAT_VISIT_MNH04, 
         M04_PRG_DSDECOD, M04_FETAL_LOSS_DSSTDAT, M04_FETAL_LOSS_DSDECOD) %>% ## select only fetal loss variables
  ## calculate the gestational age at fetal loss 
  # first join in PREG_START_DATE from mnh01_constructed 
  left_join(mat_enroll[c("SITE", "MOMID", "PREGID", "PREG_START_DATE")], by = c("SITE", "MOMID", "PREGID")) %>% 
  # replace default value date with NA 
  mutate(M04_FETAL_LOSS_DSSTDAT = ymd(M04_FETAL_LOSS_DSSTDAT)) %>% 
  mutate(M04_FETAL_LOSS_DSSTDAT = replace(M04_FETAL_LOSS_DSSTDAT,
                                          M04_FETAL_LOSS_DSSTDAT %in% ymd("1907-07-07"), NA),
         
         M04_FETAL_LOSS_DSSTDAT = replace(M04_FETAL_LOSS_DSSTDAT,
                                          M04_FETAL_LOSS_DSSTDAT %in% ymd("1905-05-05"), NA),
  ) %>% 
  # calculate gestational age at fetal loss
  mutate(GESTAGE_FETAL_LOSS_DAYS = as.numeric(ymd(M04_FETAL_LOSS_DSSTDAT)-ymd(PREG_START_DATE)), 
         GESTAGE_FETAL_LOSS_WKS = GESTAGE_FETAL_LOSS_DAYS %/% 7)

### MNH09----
mnh09_constructed <- mnh09 %>%
  ## 1. Calculating GA at birth ##
  # only want participants who are enrolled
  # merge in MNH01 info
  right_join(mat_enroll[c("SITE", "MOMID", "PREGID", "PREG_START_DATE")], by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(mnh01_constructed[c("SITE", "MOMID", "PREGID", "M01_US_OHOSTDAT")], by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(
    M09_DELIV_DSSTDAT_INF1 = ymd(parse_date_time(M09_DELIV_DSSTDAT_INF1, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
    M09_DELIV_DSSTDAT_INF2 = ymd(parse_date_time(M09_DELIV_DSSTDAT_INF2, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
    M09_DELIV_DSSTDAT_INF3 = ymd(parse_date_time(M09_DELIV_DSSTDAT_INF3, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
    M09_DELIV_DSSTDAT_INF4 = ymd(parse_date_time(M09_DELIV_DSSTDAT_INF4, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
  ) %>%
  # pull earliest date of birth 
  # first replace default value date with NA 
  mutate(M09_DELIV_DSSTDAT_INF1 = replace(M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTDAT_INF1==ymd("1907-07-07"), NA),
         M09_DELIV_DSSTDAT_INF2 = replace(M09_DELIV_DSSTDAT_INF2, M09_DELIV_DSSTDAT_INF2 %in% c(ymd("1907-07-07"), ymd("1905-05-05")), NA),
         M09_DELIV_DSSTDAT_INF3 = replace(M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTDAT_INF3==ymd("1907-07-07"), NA),
         M09_DELIV_DSSTDAT_INF4 = replace(M09_DELIV_DSSTDAT_INF4, M09_DELIV_DSSTDAT_INF4==ymd("1907-07-07"), NA)) %>% 
  mutate(DOB = 
           pmin(M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTDAT_INF2, 
                M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTDAT_INF4, na.rm = TRUE)) %>% 
  mutate(DOB = case_when(DOB < ymd("1907-07-07") ~ NA_Date_, 
                         TRUE ~ DOB)) %>% 
  # generate indicator variable for having a birth outcome; if a birth outcome has been reported (M09_BIRTH_DSTERM =1 or 2), then BIRTH_OUTCOME_REPORTED ==1
  mutate(BIRTH_OUTCOME_REPORTED = ifelse(M09_BIRTH_DSTERM_INF1 == 1 | M09_BIRTH_DSTERM_INF1 == 2 | 
                                           M09_BIRTH_DSTERM_INF2 == 1 | M09_BIRTH_DSTERM_INF2 == 2 | 
                                           M09_BIRTH_DSTERM_INF3 == 1 | M09_BIRTH_DSTERM_INF3 == 2 |
                                           M09_BIRTH_DSTERM_INF4 == 1 | M09_BIRTH_DSTERM_INF4 == 2, 1, 0)) %>% 
  mutate(BIRTH_OUTCOME_REPORTED = case_when(is.na(DOB) & !is.na(BIRTH_OUTCOME_REPORTED) ~ NA, 
                                            TRUE ~ BIRTH_OUTCOME_REPORTED)) %>% 
  # calculate the number of days between DOB and estimated conception date
  mutate(GESTAGEBIRTH_BOE_DAYS = as.numeric(ymd(DOB) - ymd(PREG_START_DATE)), 
         GESTAGEBIRTH_BOE = GESTAGEBIRTH_BOE_DAYS %/% 7) 

## generate new variable to for number of infants 
mnh09_constructed <- mnh09_constructed %>% 
  mutate(across(c(M09_INFANTID_INF1, M09_INFANTID_INF2, M09_INFANTID_INF3, M09_INFANTID_INF4), ~ na_if(.x, ""))) %>% 
  mutate(across(c(M09_INFANTID_INF1, M09_INFANTID_INF2, M09_INFANTID_INF3, M09_INFANTID_INF4), ~ na_if(.x, "n/a"))) %>% 
  group_by(MOMID, PREGID) %>% 
  mutate(INF_NUM = 4-rowSums(is.na(across(c(M09_INFANTID_INF1, M09_INFANTID_INF2, M09_INFANTID_INF3, M09_INFANTID_INF4))))) %>% 
  ungroup()


#### long format----
# make data long for infant required outcomes -- pull out each infant's data and merge back together in long format
m09_INF1 <- mnh09_constructed %>%
  select(-contains("_INF2"), -contains("_INF3"), -contains("_INF4")) %>%
  rename_with(~str_remove(., '_INF1')) %>%
  rename("INFANTID" = "M09_INFANTID") %>%
  mutate(INFANTID = case_when(INFANTID %in% c("n/a", "0", "77", "") ~ NA, 
                              TRUE ~ INFANTID)) %>% 
  mutate(M09_DELIV_DSSTDAT = replace(M09_DELIV_DSSTDAT, M09_DELIV_DSSTDAT==ymd("1907-07-07"), NA), # replace default value date with NA
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="77:77", NA), # replace default value time with NA
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="07:07", NA), # replace default value time with NA
         DELIVERY_DATETIME = paste(M09_DELIV_DSSTDAT, M09_DELIV_DSSTTIM), # concatenate date and time of birth
         DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")  # assign time field type for time of birth
  )  %>%
  mutate_all(as.character) %>% 
  relocate(INFANTID, .after = PREGID)

m09_INF1$M09_DELIV_DSSTTIM = as.character(m09_INF1$M09_DELIV_DSSTTIM) ## data housekeeping here for time issues in data

m09_INF2 <- mnh09_constructed %>%
  select(-contains("_INF1"), -contains("_INF3"), -contains("_INF4")) %>%
  rename_with(~str_remove(., '_INF2')) %>%
  rename("INFANTID" = "M09_INFANTID") %>%
  mutate(INFANTID = case_when(INFANTID %in% c("n/a", "0", "77", "") ~ NA, 
                              TRUE ~ INFANTID)) %>% 
  mutate(M09_DELIV_DSSTDAT = replace(M09_DELIV_DSSTDAT, M09_DELIV_DSSTDAT==ymd("1907-07-07"), NA), # replace default value date with NA
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="77:77", NA), # replace default value time with NA
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="07:07", NA), # replace default value time with NA
         DELIVERY_DATETIME = paste(M09_DELIV_DSSTDAT, M09_DELIV_DSSTTIM), # concatenate date and time of birth
         DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")  # assign time field type for time of birth
  )  %>%
  mutate_all(as.character) %>% 
  relocate(INFANTID, .after = PREGID)

m09_INF2$M09_DELIV_DSSTTIM = as.character(m09_INF2$M09_DELIV_DSSTTIM) ## data housekeeping here for time issues in data


m09_INF3 <- mnh09_constructed %>%
  select(-contains("_INF1"), -contains("_INF2"), -contains("_INF4")) %>%
  rename_with(~str_remove(., '_INF3')) %>%
  rename("INFANTID" = "M09_INFANTID") %>%
  mutate(INFANTID = case_when(INFANTID %in% c("n/a", "0", "77", "") ~ NA, 
                              TRUE ~ INFANTID)) %>% 
  mutate(M09_DELIV_DSSTDAT = replace(M09_DELIV_DSSTDAT, M09_DELIV_DSSTDAT==ymd("1907-07-07"), NA), # replace default value date with NA
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="77:77", NA), # replace default value time with NA
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="07:07", NA), # replace default value time with NA
         DELIVERY_DATETIME = paste(M09_DELIV_DSSTDAT, M09_DELIV_DSSTTIM), # concatenate date and time of birth
         DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")  # assign time field type for time of birth
  )   %>%
  mutate_all(as.character) %>% 
  relocate(INFANTID, .after = PREGID)

m09_INF3$M09_DELIV_DSSTTIM = as.character(m09_INF3$M09_DELIV_DSSTTIM) ## data housekeeping here for time issues in data

if (dim(mnh09_constructed %>% filter(INF_NUM==4))[1]>=1) { 
  m09_INF4 <- mnh09_constructed %>%
    select(-contains("_INF1"), -contains("_INF2"), -contains("_INF3")) %>%
    rename_with(~str_remove(., '_INF4')) %>%
    rename("INFANTID" = "M09_INFANTID") %>%
    mutate(INFANTID = case_when(INFANTID %in% c("n/a", "0", "77", "") ~ NA,
                                TRUE ~ INFANTID)) %>%
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
  
  m09_INF4$M09_DELIV_DSSTTIM = as.character(m09_INF4$M09_DELIV_DSSTTIM) ## data housekeeping here for time issues in data
  mnh09_long <- bind_rows(m09_INF1, m09_INF2, m09_INF3, m09_INF4) %>%
    mutate(DOB = ymd(DOB)) %>%
    ## EXTRACT UNIQUE INFANTIDS FROM DELIVERY
    filter(INFANTID %in% as.vector(delivered_infantids$INFANTID)) %>% 
    mutate(GESTAGEBIRTH_BOE=as.numeric(GESTAGEBIRTH_BOE))
  
  
} else {
  
  print("no quads exist")
  
  mnh09_long <- bind_rows(m09_INF1, m09_INF2, m09_INF3) %>%
    mutate(DOB = ymd(DOB)) %>%
    ## EXTRACT UNIQUE INFANTIDS FROM DELIVERY
    filter(INFANTID %in% as.vector(delivered_infantids$INFANTID)) %>% 
    mutate(GESTAGEBIRTH_BOE=as.numeric(GESTAGEBIRTH_BOE))
  
  print("mnh09_long generated successfully")
  
  
}

# save data set
write.csv(mnh09_long, paste0(path_to_save, "mnh09_long-",UploadDate,".csv"), row.names=FALSE)

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
mnh11_latest <- mnh11 %>% filter(M11_INF_VITAL_MNH11 ==1) %>%  
  select(SITE, INFANTID, M11_VISIT_OBSSTDAT)  %>% 
  
  rename("VISITDATE" = M11_VISIT_OBSSTDAT) %>% 
  mutate(VISITDATE = ymd(VISITDATE))

mnh13_latest <- mnh13 %>% filter(M13_INF_VITAL_MNH13 ==1) %>% select(SITE, INFANTID, M13_VISIT_OBSSTDAT) %>% rename("VISITDATE" = M13_VISIT_OBSSTDAT) %>% 
  filter(VISITDATE != 0) %>% mutate(VISITDATE = ymd(VISITDATE))

mnh14_latest <- mnh14 %>% filter(M14_INF_VITAL_MNH14 ==1) %>% select(SITE, INFANTID, M14_VISIT_OBSSTDAT) %>% rename("VISITDATE" = M14_VISIT_OBSSTDAT) %>% mutate(VISITDATE = ymd(VISITDATE))
mnh15_latest <- mnh15 %>% filter(M15_INF_VITAL_MNH15 ==1) %>% select(SITE, INFANTID, M15_OBSSTDAT) %>% rename("VISITDATE" = M15_OBSSTDAT) %>% mutate(VISITDATE = ymd(VISITDATE))

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
  mutate(M24_DTHDAT = case_when(M24_DTHDAT %in% c("1907-07-07", "1905-05-05", "1909-09-09") ~ NA, # replace default value date with NA 
                                TRUE ~ M24_DTHDAT),
         M24_DTHTIM = case_when(M24_DTHTIM %in% c("77:77", "55:55", "99:99") ~ NA,                # replace default value time with NA 
                                TRUE ~ M24_DTHTIM),
         # M24_DTHTIM = replace(M24_DTHTIM, is.na(M24_DTHTIM), "12:30:00"),                       # TO DISCUSS: if time is NA, should we replace with midpoint (12:30:00)
         DEATH_DATETIME = paste(M24_DTHDAT, M24_DTHTIM),                                          # concatenate date and time of birth 
         DEATH_DATETIME = as.POSIXct(DEATH_DATETIME, format= "%Y-%m-%d %H:%M"),
         DELIVERY_DATETIME =  as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")) %>%               # assign time field type for time of birth; TO DISCUSS: if time is missing, then this variable will be empty 
  mutate(DTHDAT = M24_DTHDAT) %>%
  # generate indicator if an infant died
  mutate(DTH_INDICATOR = case_when(M24_CLOSE_DSDECOD == 3 | !is.na(DTHDAT) ~ 1,TRUE~ 0)) %>% 
  # calculate age at death 
  mutate(AGEDEATH_DATETIME = floor(as.numeric( difftime(DEATH_DATETIME, DELIVERY_DATETIME, units = "hours"))),
         AGEDEATH_DAYS = AGEDEATH_DATETIME %/% 24,
         AGEDEATH_HRS = AGEDEATH_DATETIME %% 24) %>% 
  # TO DISCUSS: if time of death is unknown but death occurs > 1 week after delivery, calculate agedeath_days using just the dates; this is not currently incorporated but could be if appropriate
  # mutate(AGEDEATH_DAYS = case_when(ymd(DTHDAT)-DOB >= 7 & is.na(M24_DTHTIM) ~ as.numeric(ymd(DTHDAT)-DOB), 
  #                                       ymd(DTHDAT)-DOB <7 & is.na(M24_DTHTIM) ~ AGEDEATH_DAYS, 
  #                                  TRUE ~ AGEDEATH_DAYS)) %>% 
  select(names(mnh24),DOB,M24_DTHTIM,DELIVERY_DATETIME, DEATH_DATETIME, DTH_INDICATOR, DTHDAT,AGEDEATH_DAYS, AGEDEATH_HRS) %>%
  ## generate indicator variables for where things could go wrong
  mutate(MISSING_MNH09 = ifelse(is.na(DOB), 1, 0), # MISSING MNH09
         DOB_BEFORE_BIRTH = ifelse(is.na(DOB), 55,
                                   ifelse(DOB > DTHDAT, 1, 0)), # DEATH DATE BEOFRE DOB
         MISSING_TIME_DEATH = ifelse(DTH_INDICATOR==1 & is.na(M24_DTHTIM) | M24_DTHTIM == "77:77", 1, 0),
         CLOSEOUTID_MISSING_MNH02 = ifelse(PREGID %in% enrolled_ids_vec, 0, 1)) # NOT ENROLLED


## QUERY: export site-specfic cases where delivery datetime is after death datetime
sites <- c("India-SAS", "Ghana","India-CMC", "Kenya", "Pakistan",  "Zambia")

mortality_queries <- list()
for (i in seq_along(sites)) {
  site_name <- sites[i]  # Get site name
  
  mortality_queries[[as.character(site_name)]] <- mnh24_constructed %>%
    filter(DEATH_DATETIME<DELIVERY_DATETIME) %>%
    filter(SITE == site_name)
  
  cat("n =", dim(mortality_queries[[as.character(site_name)]])[1], "delivery date after death date in",site_name, "\n" )
  
  if (dim(mortality_queries[[as.character(site_name)]])[1] > 0){
    
    write_xlsx(mortality_queries[[as.character(site_name)]], path = paste0(path_to_save_queries, UploadDate, "-infant_mortality_queries_", site_name, ".xlsx"))
    cat("Successfully exported infant mortality query for", site_name, "\n")
    
  }
}

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

mnh04_constructed_fetal_loss <- mnh04_constructed %>% filter(M04_PRG_DSDECOD == 2  | M04_FETAL_LOSS_DSDECOD %in% c(1, 2, 3)) %>% #
  ## if loss is reported at multiple visits for a participant, take the earliest report
  group_by(SITE, MOMID, PREGID) %>% 
  arrange(-desc(M04_ANC_OBSSTDAT)) %>% # M04_ANC_OBSSTDAT
  slice(1) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) %>% 
  ungroup() %>% 
  select(SITE, MOMID, PREGID, M04_PRG_DSDECOD, M04_FETAL_LOSS_DSDECOD, M04_FETAL_LOSS_DSSTDAT,
         GESTAGE_FETAL_LOSS_DAYS, GESTAGE_FETAL_LOSS_WKS) %>% 
  mutate(M04_FETAL_LOSS_DSSTDAT = ymd(M04_FETAL_LOSS_DSSTDAT))

mnh09_long_sub <- mnh09_long %>%   select(SITE, MOMID, PREGID,INFANTID,M09_SEX,
                                          M09_DELIV_DSSTDAT, M09_DELIV_DSSTTIM,DOB,  DELIVERY_DATETIME,M09_BIRTH_DSTERM, 
                                          GESTAGEBIRTH_BOE_DAYS, GESTAGEBIRTH_BOE)

# Infant baseline dataset ----
inf_baseline <- mat_enroll %>% 
  full_join(mnh09_long_sub, by = c("SITE", "MOMID", "PREGID")) %>% 
  select(SITE, MOMID, PREGID,INFANTID,M09_SEX, ENROLL_SCRN_DATE, BOE_METHOD,M01_US_OHOSTDAT, GA_DIFF_DAYS, EDD_BOE, BOE_GA_DAYS_ENROLL, PREG_START_DATE,
         DOB, M09_DELIV_DSSTTIM, DELIVERY_DATETIME,M09_BIRTH_DSTERM, 
         GESTAGEBIRTH_BOE_DAYS, GESTAGEBIRTH_BOE
  ) %>% 
  full_join(mnh04_constructed_fetal_loss , by = c("SITE", "MOMID", "PREGID")) %>% 
  ## add new var with a indicator variable for birth outcome reported
  mutate(BIRTH_OUTCOME_REPORTED = case_when(!is.na(DOB) | !is.na(M04_FETAL_LOSS_DSSTDAT) ~ 1, 
                                            TRUE ~ 0)) %>% 
  filter(BIRTH_OUTCOME_REPORTED==1) %>% 
  ## if only fetal loss date reported in mnh04 (and no date in mnh09, replace DOB with fetal loss date)\
  mutate(DOB = case_when(is.na(DOB) & !is.na(M04_FETAL_LOSS_DSSTDAT) ~ ymd(M04_FETAL_LOSS_DSSTDAT), 
                         TRUE ~ ymd(DOB))) %>% 
  ## add new var with a single ga at birth --use mnh09, if missing, use mnh04; if both available, take the earliest reported date
  mutate(GESTAGEBIRTH_ANY = case_when(is.na(GESTAGEBIRTH_BOE) ~ as.numeric(GESTAGE_FETAL_LOSS_WKS), 
                                      !is.na(GESTAGE_FETAL_LOSS_WKS) & !is.na(GESTAGEBIRTH_BOE) & GESTAGEBIRTH_BOE <= GESTAGE_FETAL_LOSS_WKS ~ as.numeric(GESTAGEBIRTH_BOE), 
                                      !is.na(GESTAGE_FETAL_LOSS_WKS) & !is.na(GESTAGEBIRTH_BOE) & GESTAGE_FETAL_LOSS_WKS < GESTAGEBIRTH_BOE ~ as.numeric(GESTAGE_FETAL_LOSS_WKS), 
                                      
                                      TRUE ~ as.numeric(GESTAGEBIRTH_BOE))) %>% 
  mutate(GESTAGEBIRTH_ANY_DAYS = case_when(is.na(GESTAGEBIRTH_BOE_DAYS) ~ as.numeric(GESTAGE_FETAL_LOSS_DAYS), 
                                           !is.na(GESTAGE_FETAL_LOSS_DAYS) & !is.na(GESTAGEBIRTH_BOE_DAYS) & GESTAGEBIRTH_BOE_DAYS <= GESTAGE_FETAL_LOSS_DAYS ~ as.numeric(GESTAGEBIRTH_BOE_DAYS), 
                                           !is.na(GESTAGE_FETAL_LOSS_DAYS) & !is.na(GESTAGEBIRTH_BOE_DAYS) & GESTAGE_FETAL_LOSS_DAYS < GESTAGEBIRTH_BOE_DAYS ~ as.numeric(GESTAGE_FETAL_LOSS_DAYS), 
                                           
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
  mutate(FETAL_LOSS = case_when(!is.na(M04_FETAL_LOSS_DSSTDAT) | M09_BIRTH_DSTERM ==2 ~ 1, TRUE ~0)) %>% 
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
                                   TRUE ~ NA_Date_)) %>% 
  mutate(BIRTH_OUTCOME = as.numeric(BIRTH_OUTCOME)) %>% 
  # generate adjudication variable; instances where a loss was reported in MNH04 but a live birth reported in MNH09
  mutate(ADJUD_NEEDED = case_when(BIRTH_OUTCOME ==1 & FETAL_LOSS==1 ~ 1, 
                                  TRUE ~ 0)) %>% 
  # if adjudication needed, replace birthout with 55
  mutate(BIRTH_OUTCOME = case_when(ADJUD_NEEDED==1 ~ 55, 
                                   TRUE ~ BIRTH_OUTCOME),
         FETAL_LOSS = case_when(ADJUD_NEEDED==1 ~ 55, 
                                TRUE ~ FETAL_LOSS)
  ) %>% 
  # generate variable for livebirths 
  mutate(LIVEBIRTH = case_when(BIRTH_OUTCOME==1 ~ 1, 
                               ADJUD_NEEDED ==1 ~ 55,
                               TRUE ~ 0)
  ) %>% 
  select(SITE, MOMID, PREGID, INFANTID, M09_SEX, ENROLL_US_DATE,BOE_METHOD, GA_DIFF_DAYS, EDD_BOE, BOE_GA_DAYS_ENROLL, PREG_START_DATE,PREG_END_DATE, GESTAGEBIRTH_ANY,GESTAGEBIRTH_ANY_DAYS,
         BIRTH_OUTCOME, FETAL_LOSS,LIVEBIRTH,ADJUD_NEEDED, DOB, TIME_BIRTH, DELIVERY_DATETIME,M04_FETAL_LOSS_DSSTDAT,M04_FETAL_LOSS_DSDECOD,
         CLOSEOUT, INF_CLOSEOUT_REASON, DTH_INDICATOR, DEATHDATE_MNH24,DEATHTIME_MNH24,
         DEATH_DATETIME, AGEDEATH_DAYS, AGEDEATH_HRS,  GESTAGEBIRTH_BOE, GESTAGE_FETAL_LOSS_WKS
  ) 

adjucation_cases <- inf_baseline %>% filter (ADJUD_NEEDED == 1)
  
  
# save data set
write.csv(inf_baseline, paste0(path_to_save, "inf_baseline-",UploadDate,".csv"), row.names=FALSE)

table(inf_baseline$LIVEBIRTH, inf_baseline$SITE)
table(inf_baseline$FETAL_LOSS, inf_baseline$SITE)
table(inf_baseline$ADJUD_NEEDED, inf_baseline$SITE)
table(inf_baseline$FETAL_LOSS, inf_baseline$LIVEBIRTH)
table(inf_baseline$GESTAGEBIRTH_ANY, inf_baseline$SITE)

## TIME VARYING DATASET 
# generate constructed variables that will be used for time-varyign outcomes 
# Indicator if an infant dies [varname: DTH_INDICATOR]
# Date the infant was last seen [varname: DATE_LAST_SEEN]
# Age the infant was last seen [varname: AGE_LAST_SEEN]
# Include variables for death indicator, agedeath, age last seen 
timevarying_constructed <- inf_baseline %>% 
  left_join(mnh09_long[c("SITE", "MOMID", "PREGID", "INFANTID", "M09_MAT_VISIT_MNH09")],
            by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
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
# Outcome Construction ----
## Low birth-weight ----
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
  select(SITE, MOMID, PREGID, INFANTID, BIRTH_OUTCOME, LIVEBIRTH, ADJUD_NEEDED) %>% 
  left_join(mnh11_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  ## pull key variables 
  select(SITE, MOMID,PREGID, INFANTID, BIRTH_OUTCOME,LIVEBIRTH,ADJUD_NEEDED, BWEIGHT_PRISMA, BWEIGHT_ANY, M11_BW_FAORRES, M11_BW_FAORRES_REPORT, M11_BW_EST_FAORRES) %>% 
  ## LBW PRISMA measured (bw <2500g)
  mutate(LBW2500_PRISMA = case_when(ADJUD_NEEDED ==1 ~ 55, 
                                    BWEIGHT_PRISMA >= 0 & BWEIGHT_PRISMA < 2500 ~ 1,
                                    BWEIGHT_PRISMA <= 0 ~  55, 
                                    TRUE ~ 0)) %>% 
  ## LBW PRISMA measured (bw <1500g)
  mutate(LBW1500_PRISMA = case_when(ADJUD_NEEDED ==1 ~ 55,
                                    BWEIGHT_PRISMA >= 0 & BWEIGHT_PRISMA < 1500 ~ 1,
                                    BWEIGHT_PRISMA <= 0 ~ 55,
                                    TRUE ~ 0)) %>% 
  ## LBW PRISMA measured (bw <2500g); varname: LBW2500_ANY
  mutate(LBW2500_ANY = case_when(ADJUD_NEEDED ==1 ~ 55,
                                 BWEIGHT_ANY >= 0 & BWEIGHT_ANY < 2500 ~ 1,
                                 BWEIGHT_ANY <= 0 | is.na(BWEIGHT_ANY) ~ 55,
                                 TRUE ~ 0)) %>% 
  ## LBW PRISMA measured (bw <1500g); varname: LBW1500_ANY 
  mutate(LBW1500_ANY = case_when(ADJUD_NEEDED ==1 ~ 55,
                                 BWEIGHT_ANY >= 0 & BWEIGHT_ANY < 1500 ~ 1,
                                 BWEIGHT_ANY <= 0 | is.na(BWEIGHT_ANY) ~ 55,
                                 TRUE ~ 0)) %>% 
  mutate(LBW_CAT_PRISMA = case_when(ADJUD_NEEDED ==1 ~ 55,
                                    BWEIGHT_PRISMA >= 0 & BWEIGHT_PRISMA < 1500 ~ 11, 
                                    BWEIGHT_PRISMA >= 1500 & BWEIGHT_PRISMA < 2500 ~ 12, 
                                    BWEIGHT_PRISMA >= 2500 ~ 13, 
                                    BWEIGHT_PRISMA < 0 | M11_BW_EST_FAORRES > 150 ~ 55,
                                    TRUE ~ NA)) %>% 
  ## ANY LBW categorical variable: (any bw <1500g)=11, (any bw <2500g)=12, (any bw >= 2500g)
  mutate(LBW_CAT_ANY = case_when(ADJUD_NEEDED ==1 ~ 55,
                                 BWEIGHT_ANY >= 0 & BWEIGHT_ANY < 1500 ~ 11,
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
  mutate(BW_TIME = case_when(ADJUD_NEEDED==1 ~ -5,
                             M11_BW_EST_FAORRES < 0 | M11_BW_EST_FAORRES >= 97 ~ NA,
                             TRUE ~ M11_BW_EST_FAORRES))  %>% 
  select(-ADJUD_NEEDED)

lowbirthweight_test <- lowbirthweight %>% filter(SITE == "Ghana")

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
## Stillbirth ----
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
  mutate(STILLBIRTH_20WK = case_when(is.na(GESTAGEBIRTH_ANY)| (LIVEBIRTH ==0 & FETAL_LOSS ==0) ~ 55, ## if ga at birth is missing OR ga at birth is <20 
                                     GESTAGEBIRTH_ANY < 20 | M04_FETAL_LOSS_DSDECOD == 2 ~ 0,
                                     GESTAGEBIRTH_ANY >= 20 & (LIVEBIRTH ==0 | FETAL_LOSS ==1) ~ 1, # if birth outcome is fetal loss and gestage birth is >= 20wks
                                     # GA_AT_BIRTH_ANY < 20 ~ 66,
                                     (LIVEBIRTH ==0 | FETAL_LOSS ==1) & STILLBIRTH_SIGNS_LIFE == 1 ~ 66,  # if fetal loss reported but there are signs of life reported -- 66 
                                     BIRTH_OUTCOME == 1 & STILLBIRTH_SIGNS_LIFE == 0 ~ 99, # if birth outcome is reported but is missing signs of life -- 99 
                                     ADJUD_NEEDED == 1 ~ 55,
                                     TRUE ~ 0)) %>%
  ## if missing ga at birth & ga at fetal loss--55
  # c. STILLBIRTH_22WK
  mutate(STILLBIRTH_22WK = case_when(ADJUD_NEEDED == 1 | STILLBIRTH_20WK %in% c(55,66,99)~ 55, #  | STILLBIRTH_20WK %in% c(55,66)
                                     STILLBIRTH_20WK ==1 & GESTAGEBIRTH_ANY >= 22 ~ 1, # if birth outcome is fetal loss and gestage birth is >= 22wks
                                     TRUE ~ 0)) %>% 
  
  # d. STILLBIRTH_24WK
  mutate(STILLBIRTH_24WK = case_when(ADJUD_NEEDED == 1 | STILLBIRTH_20WK %in% c(55,66,99) ~ 55,
                                     STILLBIRTH_20WK ==1 & GESTAGEBIRTH_ANY >= 24 ~ 1, # if birth outcome is fetal loss and gestage birth is >= 24wks
                                     TRUE ~ 0)) %>% 
  
  # e. STILLBIRTH_28WK
  mutate(STILLBIRTH_28WK = case_when(ADJUD_NEEDED == 1 | STILLBIRTH_20WK %in% c(55,66,99) ~ 55,
                                     STILLBIRTH_20WK ==1 & GESTAGEBIRTH_ANY >= 28 ~ 1, # if birth outcome is fetal loss and gestage birth is >= 28wks
                                     TRUE ~ 0)) %>%
  
  # f. STILLBIRTH_GESTAGE_CAT - 
  mutate(STILLBIRTH_GESTAGE_CAT = case_when(ADJUD_NEEDED == 1 ~ 55,
                                            BIRTH_OUTCOME==1 ~ 14, ##live birth
                                            STILLBIRTH_20WK == 1 & GESTAGEBIRTH_ANY>=20 & GESTAGEBIRTH_ANY <28 ~ 11, # "Early: Death prior to delivery of a fetus at 20 to 27 weeks of gestation.  
                                            STILLBIRTH_20WK == 1 & GESTAGEBIRTH_ANY>=28 & GESTAGEBIRTH_ANY <37 ~ 12, # Late: Death prior to delivery of a fetus at 28 to 36 weeks of gestation.
                                            STILLBIRTH_20WK == 1 &  GESTAGEBIRTH_ANY >= 37 ~ 13, # Term: Death prior to delivery of a fetus at >37 weeks of gestation.    
                                            GESTAGEBIRTH_ANY<20 ~ 77, # if GA at birth is <20, exclude from these categories
                                            TRUE ~ 55)) %>% 
  # g. STILLBIRTH_TIMING
  mutate(STILLBIRTH_TIMING = case_when(ADJUD_NEEDED==1 ~ 55, 
                                       STILLBIRTH_20WK == 1 & (M09_FHR_VSTAT==0 | M09_MACER_CEOCCUR ==1) ~ 11,
                                       STILLBIRTH_20WK == 1 & (M09_FHR_VSTAT ==1 & M09_MACER_CEOCCUR == 0) ~ 12,
                                       STILLBIRTH_20WK == 0 | GESTAGEBIRTH_ANY < 20 ~ 77, ## if no stillbirth or GA<20 (miscarriage), the stillbirth timing is 77, not applicable 
                                       (STILLBIRTH_20WK == 1 & (is.na(M09_FHR_VSTAT) | is.na(M09_MACER_CEOCCUR))) | 
                                         (STILLBIRTH_20WK == 1 & (M09_FHR_VSTAT %in% c(55,77,99,66) | M09_MACER_CEOCCUR %in% c(55,77,99,66)))~ 99, 
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

#*****************************************************************************
## Pre-term delivery ----
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

## Forms required: MNH01 constructed, MNH09_constructed
preterm_birth <- inf_baseline %>% 
  select(SITE, MOMID, PREGID, INFANTID, BIRTH_OUTCOME,ADJUD_NEEDED, LIVEBIRTH, GESTAGEBIRTH_ANY, GA_DIFF_DAYS) %>%
  left_join(stillbirth[c("SITE", "MOMID", "PREGID","INFANTID", "STILLBIRTH_20WK")], by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  ## 1. Generate indicator variable for those who have had a birth outcome ## 
  ## 2. Generate Outcomes for PRETERM BIRTH (livebirths)  
  # a. Post-term delivery (>=41 weeks): Delivery after 41 weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_GT41]
  mutate(PRETERMBIRTH_GT41 = case_when(ADJUD_NEEDED==1 ~ 55, 
                                       LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 41 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # b. Term delivery (37 to <41 weeks): Delivery between 37 and <41 weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT41]
  mutate(PRETERMBIRTH_LT41 = case_when(ADJUD_NEEDED==1 ~ 55, 
                                       LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 41 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # c. Preterm birth (<37 weeks): Delivery prior to 37 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT37]
  mutate(PRETERMBIRTH_LT37 = case_when(ADJUD_NEEDED==1 ~ 55, 
                                       LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 37 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # d. Preterm birth (<34 weeks): Delivery prior to 34 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT34]
  mutate(PRETERMBIRTH_LT34 = case_when(ADJUD_NEEDED==1 ~ 55, 
                                       LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 34 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # e. Preterm birth (<32 weeks): Delivery prior to 32 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT32]
  mutate(PRETERMBIRTH_LT32 = case_when(ADJUD_NEEDED==1 ~ 55, 
                                       LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 32 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # f. Preterm birth (<28 weeks): Delivery prior to 28 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT28]
  mutate(PRETERMBIRTH_LT28 = case_when(LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 28 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # g. Preterm birth severity (categorical): POSTTERM (>=41 wks), term (37 to 41 wks), Late preterm (34 to <37 wks), early preterm (32 to <34 wks), very preterm (28 to <32 wks), extermely preterm (<28 weeks) [varname: PRETERMBIRTH_CAT]
  mutate(PRETERMBIRTH_CAT = case_when(ADJUD_NEEDED==1 ~ 55, 
                                      LIVEBIRTH == 0 ~ 77, 
                                      LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >=41 ~ 10, 
                                      LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 37 & GESTAGEBIRTH_ANY <41 ~  11,  
                                      LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 34 & GESTAGEBIRTH_ANY < 37 ~ 12, 
                                      LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 32 & GESTAGEBIRTH_ANY < 34 ~ 13,
                                      LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 28 & GESTAGEBIRTH_ANY < 32 ~ 14,
                                      LIVEBIRTH == 1 & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY <28 ~ 15,
                                      TRUE ~ 55)) %>% 
  
  ## 2. Generate Outcomes for PRETERM DELIVERY (livebirths + stillbirths) ## 
  mutate(PRETERMDELIV_GT41 = case_when(ADJUD_NEEDED==1 ~ 55, 
                                       (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 41 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # b. Term delivery (37 to <41 weeks): Delivery between 37 and <41 weeks of gestation (live or stillbirth). [varname: PRETERMDELIV_LT41]
  mutate(PRETERMDELIV_LT41 = case_when(ADJUD_NEEDED==1 ~ 55, 
                                       (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 41 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # c. Preterm birth (<37 weeks): Delivery prior to 37 completed weeks of gestation (live or stillbirth). [varname: PRETERMDELIV_LT37]
  mutate(PRETERMDELIV_LT37 = case_when(ADJUD_NEEDED==1 ~ 55, 
                                       (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 37 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # d. Preterm birth (<34 weeks): Delivery prior to 34 completed weeks of gestation (live or stillbirth). [varname: PRETERMDELIV_LT34]
  mutate(PRETERMDELIV_LT34 = case_when(ADJUD_NEEDED==1 ~ 55, 
                                       (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 34 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # e. Preterm birth (<32 weeks): Delivery prior to 32 completed weeks of gestation (live or stillbirth). [varname: PRETERMDELIV_LT32]
  mutate(PRETERMDELIV_LT32 = case_when(ADJUD_NEEDED==1 ~ 55, 
                                       (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 32 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # f. Preterm birth (<28 weeks): Delivery prior to 28 completed weeks of gestation (live or stillbirth). [varname: PRETERMDELIV_LT28]
  mutate(PRETERMDELIV_LT28 = case_when(ADJUD_NEEDED==1 ~ 55, 
                                       (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY < 28 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  # g. Preterm birth severity (categorical): postterm (>41 wks), term (37 to 41 wks wks), Late preterm (34 to <37 wks), early preterm (32 to <34 wks), very preterm (28 to <32 wks), extermely preterm (<28 weeks) [varname: PRETERMDELIV_CAT]
  mutate(PRETERMDELIV_CAT = case_when(ADJUD_NEEDED==1 ~ 55, 
                                      (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 41 ~ 10, 
                                      (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 37 & GESTAGEBIRTH_ANY <41 ~ 11,  
                                      (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 34 & GESTAGEBIRTH_ANY < 37 ~ 12, 
                                      (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 32 & GESTAGEBIRTH_ANY < 34 ~ 13,
                                      (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 28 & GESTAGEBIRTH_ANY < 32 ~ 14,
                                      (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & GESTAGEBIRTH_ANY >= 20 & GESTAGEBIRTH_ANY <28 ~ 15,
                                      TRUE ~ 55)) %>% 
  ## generate denominators 
  mutate(DATA_COMPLETE_DENOM = case_when(ADJUD_NEEDED==1 ~ 55, 
                                         (LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) ~ 1, TRUE ~ 0))  %>% 
  mutate(BIRTH_OUTCOME_REPORTED = case_when(ADJUD_NEEDED==1 ~ 55, 
                                            (BIRTH_OUTCOME ==1 | BIRTH_OUTCOME ==2) ~ 1, TRUE ~ 0)) %>% 
  select(-ADJUD_NEEDED)

table(preterm_birth$PRETERMBIRTH_CAT, preterm_birth$SITE)

#*****************************************************************************
## Size for Gestational Age (SGA) ----
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
  select(SITE,INFANTID, MOMID, PREGID, GESTAGEBIRTH_ANY,GESTAGEBIRTH_ANY_DAYS, LIVEBIRTH, BIRTH_OUTCOME, ADJUD_NEEDED)  %>% 
  left_join(mnh09_long[c("SITE", "MOMID", "PREGID", "INFANTID", "M09_BIRTH_DSTERM", "M09_SEX", "M09_INFANTS_FAORRES")],
            by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  ## merge with mnh11 
  left_join(mnh11_constructed %>% select(-c(M09_BIRTH_DSTERM)), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  ## convert weight from grams to kg 
  mutate(BWEIGHT_PRISMA_KG = case_when(LIVEBIRTH == 0 ~ -7, 
                                       BWEIGHT_PRISMA > 0 ~ BWEIGHT_PRISMA/1000,
                                       TRUE ~ -5), 
         BWEIGHT_ANY_KG = case_when(LIVEBIRTH==0 ~ -7, 
                                    BWEIGHT_ANY < 0 | is.na(BWEIGHT_ANY) ~ -5, 
                                    TRUE ~ BWEIGHT_ANY/1000)) %>% 
  # convert to numeric 
  mutate(across(c(GESTAGEBIRTH_ANY_DAYS, BWEIGHT_ANY_KG, M09_SEX), as.numeric)) %>%
  
  ## calculate percentile
  mutate(SGA_CENTILE = case_when(ADJUD_NEEDED==1 ~ -5, 
                                 LIVEBIRTH==0 ~ -7, 
                                 is.na(GESTAGEBIRTH_ANY_DAYS) ~ -5,  
                                 M09_SEX == 1  & BWEIGHT_ANY_KG > 0 ~ suppressWarnings(floor(igb_wtkg2centile(GESTAGEBIRTH_ANY_DAYS, BWEIGHT_ANY_KG, sex = "Male"))),
                                 M09_SEX == 2 & BWEIGHT_ANY_KG > 0 ~ suppressWarnings(floor(igb_wtkg2centile(GESTAGEBIRTH_ANY_DAYS, BWEIGHT_ANY_KG, sex = "Female"))),
                                 BWEIGHT_ANY_KG <= 0 ~ -5, 
                                 TRUE ~ -5)) %>% 
  
  ## START CONSTRUCTING OUTCOMES ##
  # a. Size for gestational age - categorical. [varname: SGA_CAT]
  mutate(SGA_CAT = case_when(ADJUD_NEEDED==1 ~ 55, 
                             LIVEBIRTH==0 ~ 77, 
                             SGA_CENTILE >= 0 & SGA_CENTILE < 3 ~ 11,   # SGA_CENTILE < 3rd
                             SGA_CENTILE >= 3 & SGA_CENTILE < 10 ~ 12,  # SGA_CENTILE 3 to < 10th
                             SGA_CENTILE >= 10 & SGA_CENTILE < 90 ~ 13, # AGA 10th to <90th 
                             SGA_CENTILE >= 90 ~ 14, # LGA >= 90
                             TRUE ~ 55)) %>% # 55 for missing
  ## merge with preterm births dataset to get preterm vars 
  left_join(preterm_birth %>% select(-GESTAGEBIRTH_ANY, -LIVEBIRTH), by = c("SITE","INFANTID", "MOMID", "PREGID")) %>%
  # b. Preterm small for gestational age: Preterm < 37 weeks AND SGA (<10th). [varname: INF_SGA_PRETERM]
  mutate(INF_SGA_PRETERM = case_when(ADJUD_NEEDED==1 ~ 55, 
                                     LIVEBIRTH==0 ~ 77, 
                                     PRETERMBIRTH_LT37 == 1 & SGA_CAT == 12 ~ 1, 
                                     SGA_CAT == 55 ~ 55,
                                     TRUE ~ 0)) %>% 
  # c. Preterm appropriate for gestational age: Preterm < 37 weeks AND not SGA (<10th). [varname: INF_AGA_PRETERM]
  mutate(INF_AGA_PRETERM = case_when(ADJUD_NEEDED==1 ~ 55, 
                                     LIVEBIRTH==0 ~ 77,
                                     PRETERMBIRTH_LT37 == 1 & (SGA_CAT == 13 | SGA_CAT == 14) ~ 1,
                                     TRUE ~ 0)) %>% 
  # d. Term small for gestational age: Term >=37 weeks AND SGA (<10th). [varname: INF_SGA_TERM]
  mutate(INF_SGA_TERM = case_when(ADJUD_NEEDED==1 ~ 55, 
                                  LIVEBIRTH==0 ~ 77,
                                  PRETERMBIRTH_CAT == 11 & (SGA_CAT == 11 | SGA_CAT == 12) ~ 1,
                                  TRUE ~ 0)) %>% 
  # e. Term appropriate for gestational age: Term >=37 weeks AND not SGA (<10th). [varname: INF_AGA_TERM]
  mutate(INF_AGA_TERM = case_when(ADJUD_NEEDED==1 ~ 55, 
                                  LIVEBIRTH==0 ~ 77,
                                  PRETERMBIRTH_CAT == 11 & (SGA_CAT == 13 | SGA_CAT == 14) ~ 1, 
                                  TRUE ~ 0)) %>% 
  # d. Term small for gestational age: Post Term >=41 weeks AND SGA (<10th). [varname: INF_SGA_POSTTERM]
  mutate(INF_SGA_POSTTERM = case_when(ADJUD_NEEDED==1 ~ 55, 
                                      LIVEBIRTH==0 ~ 77,
                                      PRETERMBIRTH_CAT == 10 & (SGA_CAT == 11 | SGA_CAT == 12) ~ 1,
                                      TRUE ~ 0)) %>% 
  # e. Term appropriate for gestational age: Post Term >=41 weeks AND not SGA (<10th). [varname: INF_AGA_POSTTERM]
  mutate(INF_AGA_POSTTERM = case_when(ADJUD_NEEDED==1 ~ 55,
                                      LIVEBIRTH==0 ~ 77,
                                      PRETERMBIRTH_CAT == 10 & (SGA_CAT == 13 | SGA_CAT == 14) ~ 1, 
                                      TRUE ~ 0))  %>% 
  # generate denominator 
  select(SITE, MOMID, PREGID, INFANTID,LIVEBIRTH,ADJUD_NEEDED, M09_SEX,GESTAGEBIRTH_ANY,GESTAGEBIRTH_ANY_DAYS,BWEIGHT_ANY_KG, M11_BW_EST_FAORRES, M11_BW_FAORRES,
         M11_BW_FAORRES_REPORT, BWEIGHT_ANY, SGA_CENTILE,SGA_CAT, INF_SGA_PRETERM, INF_AGA_PRETERM, INF_SGA_TERM, INF_AGA_TERM,
         INF_SGA_POSTTERM, INF_AGA_POSTTERM, M09_INFANTS_FAORRES) 

table(sga$SGA_CAT)
table(sga$INF_AGA_POSTTERM, sga$SITE)
table(sga$INF_SGA_POSTTERM, sga$SITE)

# export
# write.csv(sga, paste0(path_to_save, "sga" ,".csv"), row.names=FALSE)
#*****************************************************************************
## Mortality ----
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
# pak_table5_ids <- mortality %>% filter(SITE =="Pakistan") %>% 
#   filter(DOB_AFTER_DEATH==1) %>% 
#   select(SITE, MOMID, PREGID, INFANTID, DELIVERY_DATETIME, DEATH_DATETIME)
# write.xlsx(pak_table5_ids, paste0("D:/Users/stacie.loisate/Documents/Output/Outcomes-Queries/PAK_table5_ids-2024-09-20" ,".xlsx"),na="", rowNames=FALSE)

mortality <- inf_baseline %>% 
  select(SITE,INFANTID, MOMID, PREGID,LIVEBIRTH, CLOSEOUT, PREG_END_DATE,DOB,TIME_BIRTH,  DELIVERY_DATETIME, FETAL_LOSS, 
         GESTAGEBIRTH_ANY,GESTAGEBIRTH_ANY_DAYS, BIRTH_OUTCOME, ADJUD_NEEDED) %>% 
  left_join(timevarying_constructed[c("SITE", "MOMID", "PREGID", "INFANTID", "M09_MAT_VISIT_MNH09", "M11_INF_VISIT_MNH11", "M11_INF_DSTERM",
                                      "DTH_INDICATOR", "DEATHTIME_MNH24", "DEATHDATE_MNH24", "DEATH_DATETIME", "AGEDEATH_DAYS",
                                      "AGEDEATH_HRS", "DATE_LAST_SEEN", "AGE_LAST_SEEN")],
            by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  
  ## generate variable if death is reported among live birth but is missing time of death 
  mutate(DTH_TIME_MISSING = case_when(BIRTH_OUTCOME==1 & DTH_INDICATOR==1 & (DEATHTIME_MNH24%in% c("55:55", "77:77") | is.na(DEATHTIME_MNH24) | 
                                                                               (str_length(as.character(DEATHTIME_MNH24)) ==5 & str_detect(as.character(DEATHTIME_MNH24), "^[0-9]+$"))
  ) ~ 1,
  TRUE ~ 0),
  DTH_DATE_MISSING = case_when(BIRTH_OUTCOME==1 & DTH_INDICATOR==1 & (DEATHDATE_MNH24%in% c(ymd("1905-05-05"), ymd("1907-07-07")) | is.na(DEATHDATE_MNH24)) ~ 1,
                               TRUE ~ 0)) %>% 
  
  ## generate outcome for neonatal death if infant dies <28 days of life
  mutate(NEO_DTH = case_when(ADJUD_NEEDED ==1 ~ 55, 
                             (BIRTH_OUTCOME==1 & DTH_INDICATOR==1 & is.na(AGEDEATH_DAYS)) | 
                              (BIRTH_OUTCOME==1 & DTH_INDICATOR==1 & AGEDEATH_DAYS < 0) |
                               DTH_TIME_MISSING ==1 | DTH_DATE_MISSING ==1  ~ 55,  ## if missing the three criteria, they are missing
                             DTH_INDICATOR == 0 ~ 0, 
                             BIRTH_OUTCOME == 1 & AGEDEATH_DAYS >= 0 & AGEDEATH_DAYS < 28 ~ 1, ## if live birth AND age of death is < 28, they get a 1
                             is.na(BIRTH_OUTCOME) & is.na(DOB) ~ 55, ## THESE ARE PEOPLE WHO ARE REPORTING A DEATH BUT ARE MISSING OR HAVE INVALID AGE AT DEATH
                             TRUE ~ 0)) %>%  
  
  ## generate outcome for infant death if infant dies <365 days of life
  mutate(INF_DTH = case_when(ADJUD_NEEDED ==1 ~ 55, 
                             (BIRTH_OUTCOME==1 & DTH_INDICATOR==1 & is.na(AGEDEATH_DAYS)) | 
                               (BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & AGEDEATH_DAYS < 0) |
                               DTH_TIME_MISSING ==1 | DTH_DATE_MISSING ==1~ 55,  ## if missing the three criteria, they are missing
                             DTH_INDICATOR == 0 ~ 0, 
                             BIRTH_OUTCOME == 1 & AGEDEATH_DAYS < 365 ~ 1, ## if live birth AND age of death is < 365, they get a 1
                             is.na(BIRTH_OUTCOME) & is.na(DOB) ~ 55, ## THESE ARE PEOPLE WHO ARE REPORTING A DEATH BUT ARE MISSING OR HAVE INVALID AGE AT DEATH
                             TRUE ~ 0)) %>%  
  
  ## timing of neonatal mortality
  mutate(DTH_0D = case_when(ADJUD_NEEDED ==1 ~ 55, 
                            BIRTH_OUTCOME == 1 & DTH_INDICATOR ==1 & is.na(AGEDEATH_HRS) ~ 0,
                            BIRTH_OUTCOME == 1 & DTH_INDICATOR ==1 & (AGEDEATH_DAYS ==0 & AGEDEATH_HRS >= 0 & AGEDEATH_HRS < 24) ~ 1,
                            TRUE ~ 0),
         DTH_7D = case_when(ADJUD_NEEDED ==1 ~ 55, 
                            BIRTH_OUTCOME == 1 & DTH_INDICATOR ==1 & ((AGEDEATH_DAYS ==0 & AGEDEATH_HRS >= 0) & AGEDEATH_DAYS < 7) ~ 1,
                            TRUE ~ 0), 
         DTH_28D = case_when(ADJUD_NEEDED ==1 ~ 55, 
                             BIRTH_OUTCOME == 1 & DTH_INDICATOR ==1 & AGEDEATH_DAYS >=0 & AGEDEATH_DAYS < 28 ~ 1,
                             TRUE ~ 0),
         DTH_365D = case_when(ADJUD_NEEDED ==1 ~ 55, 
                              BIRTH_OUTCOME == 1 & DTH_INDICATOR ==1 & AGEDEATH_DAYS >=28 & AGEDEATH_DAYS < 365 ~ 1,
                              TRUE ~ 0)) %>% 
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
  ## calculate denominators; if you have passed the risk window (age last seen >= risk window) 
  mutate(
    # to generate risk period for neonatal and infant deaths 
    ESTIMATED_AGE_AT_UPLOAD = as.numeric(ymd(UploadDate)-DOB),
    D365_DENOM = case_when(ADJUD_NEEDED ==1 ~ 55,
                           LIVEBIRTH ==1 & (AGE_LAST_SEEN >= 365 | ESTIMATED_AGE_AT_UPLOAD >= 365) ~ 1,
                           TRUE ~ 0),
    
    D28_DENOM = case_when(ADJUD_NEEDED ==1 ~ 55, 
                          (LIVEBIRTH ==1 & (ESTIMATED_AGE_AT_UPLOAD >= 28 | (DTH_INDICATOR ==1 & AGEDEATH_DAYS < 28) |
                                              (DTH_INDICATOR==1 & DTH_TIME_MISSING==1) | (DTH_INDICATOR==1 & DOB_AFTER_DEATH==1))) ~ 1,
                          TRUE ~ 0),
  ) %>%
  ## generate variable for data completeness table 
  select(SITE, MOMID, PREGID, INFANTID,LIVEBIRTH,ESTIMATED_AGE_AT_UPLOAD,ADJUD_NEEDED, CLOSEOUT, DOB, DELIVERY_DATETIME, BIRTH_OUTCOME, DATE_LAST_SEEN,DEATHTIME_MNH24, DEATHDATE_MNH24,
         AGE_LAST_SEEN, DTH_INDICATOR,DTH_DATE_MISSING, DEATH_DATETIME,DTH_TIME_MISSING, AGEDEATH_DAYS, AGEDEATH_HRS, NEO_DTH,INF_DTH, DATA_COMPLETE_DENOM,
         ID_MISSING_ENROLLMENT,DOB_AFTER_DEATH, contains("MISSING"),contains("DTH"), contains("DENOM"), INVALID_DTH_REPORT, contains("has"))

# export
# write.csv(mortality, paste0(path_to_save, "mortality" ,".csv"), row.names=FALSE)

#  4. Neonatal mortality: Denominator is all live births reported in MNH11 with mnh09 filled out 
# a. <24 hours 
# b. Early neonatal mortality: first  7 days 
# c. Late neonatal mortality: between 7 & 28 days
neonatal_mortality <- mortality %>%
  # generate total neonatal deaths 
  mutate(TOTAL_NEO_DEATHS = case_when(BIRTH_OUTCOME ==1  &  DTH_INDICATOR ==1 & AGEDEATH_DAYS < 28 & DTH_TIME_MISSING==0 & DOB_AFTER_DEATH==0~ 1,
                                      TRUE ~ 0)) %>% 
  # generate single timing variables 
  # Death of a liveborn baby within the first 24 hours of life [NEO_DTH_24HR]
  mutate(NEO_DTH_24HR = case_when(ADJUD_NEEDED==1 ~ 55,
                                  (BIRTH_OUTCOME ==1 & DTH_INDICATOR == 1 & (is.na(AGEDEATH_HRS)| DEATHTIME_MNH24=="77:77")) | 
                                    (BIRTH_OUTCOME == 1 & DTH_INDICATOR == 1 & AGEDEATH_HRS < 0) ~ 55, 
                                  BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & (AGEDEATH_DAYS==0 & AGEDEATH_HRS <24) ~  1,
                                  TRUE ~ 0)) %>% 
  
  # Death of a liveborn baby from 1 to 7 days following delivery. [NEO_DTH_EAR]
  mutate(NEO_DTH_EAR = case_when(ADJUD_NEEDED==1 ~ 55,
                                 BIRTH_OUTCOME == 1 & DTH_INDICATOR == 1 & is.na(AGEDEATH_DAYS) ~ 55, 
                                 BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & AGEDEATH_DAYS >= 1 & AGEDEATH_DAYS < 7 ~ 1,
                                 TRUE ~ 0)) %>% 
  
  # Death of a liveborn baby from 7 to 28 days following delivery. [NEO_DTH_LATE]
  mutate(NEO_DTH_LATE = case_when(ADJUD_NEEDED==1~ 55,
                                  BIRTH_OUTCOME == 1 & DTH_INDICATOR == 1 & is.na(AGEDEATH_DAYS) ~ 55, 
                                  BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & AGEDEATH_DAYS >= 7 & AGEDEATH_DAYS < 28 ~ 1,
                                  TRUE ~ 0)) %>% 
  # generate categorical outcome
  mutate(NEO_DTH_CAT = case_when(ADJUD_NEEDED==1 ~ 55,
                                 NEO_DTH_24HR == 1 ~ 11,
                                 NEO_DTH_EAR == 1 ~ 12,
                                 NEO_DTH_LATE == 1 ~ 13,
                                 DTH_INDICATOR ==0 | is.na(DTH_INDICATOR) | LIVEBIRTH==0~ 10, ## no neonatal death
                                 AGEDEATH_DAYS >= 28 ~ 10, ## this is infant mortality
                                 (DTH_INDICATOR==1 & is.na(AGEDEATH_DAYS)) | DTH_TIME_MISSING==1 | AGEDEATH_DAYS <0 | 
                                   (DTH_INDICATOR==1 & AGEDEATH_DAYS ==0 & (is.na(AGEDEATH_HRS) | AGEDEATH_HRS <0)) ~ 55 ## death reporting but missing valid time of death
  ))


table(neonatal_mortality$NEO_DTH_CAT, neonatal_mortality$ADJUD_NEEDED)
table(neonatal_mortality$TOTAL_NEO_DEATHS, neonatal_mortality$SITE)
table(neonatal_mortality$NEO_DTH_CAT, neonatal_mortality$SITE)
table(neonatal_mortality$TOTAL_NEO_DEATHS)

# export
# write.csv(neonatal_mortality, paste0(path_to_save, "neonatal_mortality" ,".csv"), row.names=FALSE)

#  5. Infant mortality 
# a. <365 days

infant_mortality <- mortality %>% 
  # generate categorical outcome
  mutate(INF_DTH_CAT = case_when(ADJUD_NEEDED==1 ~ 55,
                                 BIRTH_OUTCOME == 1 & (AGEDEATH_DAYS>= 28 & AGEDEATH_DAYS < 365) ~ 14, 
                                 BIRTH_OUTCOME == 1 & DTH_INDICATOR != 1 ~ 10, ## no death
                                 BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & is.na(AGEDEATH_DAYS)  ~ 55 ## death reporting but missing valid time of death
  )
  ) %>% 
  # generate variable for infant death from 28 weeks of life (do to avoid overlap of neonatal mortality)
  mutate(INF_DTH_FROM28 = case_when(ADJUD_NEEDED==1 ~ 55 ,
                                    BIRTH_OUTCOME == 1 & (AGEDEATH_DAYS>= 28 & AGEDEATH_DAYS < 365) ~ 1, 
                                    BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & is.na(AGEDEATH_DAYS)  ~ 55, ## death reporting but missing valid time of death
                                    TRUE ~ 0)) %>% 
  
  # generate total infant deaths 
  mutate(TOTAL_INF_DEATHS = case_when(BIRTH_OUTCOME == 1 & DTH_INDICATOR ==1 & (AGEDEATH_DAYS < 365) ~ 1,
                                      TRUE ~ 0))

table(infant_mortality$TOTAL_INF_DEATHS, infant_mortality$SITE)
table(infant_mortality$INF_DTH, infant_mortality$SITE)
table(infant_mortality$INF_DTH_FROM28, infant_mortality$SITE)

# export
# write.csv(infant_mortality, paste0(path_to_save, "infant_mortality" ,".csv"), row.names=FALSE)
#*****************************************************************************
## Fetal Death ----
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
  select(SITE, INFANTID, MOMID, PREGID, BIRTH_OUTCOME, PREG_END_DATE, GESTAGEBIRTH_ANY, ADJUD_NEEDED) %>%
  # merge in stillbirth data (generated above in the "stillbirth" outcome/dataset)
  left_join(stillbirth %>% select(-c(PREG_END_DATE, BIRTH_OUTCOME, GESTAGEBIRTH_ANY)), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  
  # merge in mnh04 fetal loss data 
  left_join(mnh04_constructed_fetal_loss[c("SITE", "MOMID", "PREGID", "M04_PRG_DSDECOD", "M04_FETAL_LOSS_DSDECOD", "M04_FETAL_LOSS_DSSTDAT")],
            by = c("SITE", "MOMID", "PREGID")) %>%
  # generate invalid fetal loss response variable (where fetal loss is reported, but the specify type variable is a default value)
  mutate(INVALID_FETAL_LOSS = case_when(ADJUD_NEEDED==1 ~ 55, 
                                        M04_PRG_DSDECOD == 2 & # if fetal loss is reported 
                                          (is.na(M04_FETAL_LOSS_DSDECOD) | (M04_FETAL_LOSS_DSDECOD == 55 | # but type of fetal loss is missing or default value, this is invalid
                                                                              M04_FETAL_LOSS_DSDECOD == 77)) ~ 1, 
                                        TRUE ~ 0)) %>% 
  # a. generate variable for spontaneous abortion (Fetal loss <20 weeks (miscarriage))
  ## [varname: INF_ABOR_SPN]
  mutate(INF_ABOR_SPN = case_when(ADJUD_NEEDED==1 ~ 55, 
                                  GESTAGEBIRTH_ANY < 20 & (is.na(M04_FETAL_LOSS_DSDECOD) | M04_FETAL_LOSS_DSDECOD != 2) ~ 1,  # if GA at time of fetal loss is <20wks and not induced
                                  TRUE~ 0)) %>% 
  # b. generate variable for induced abortion (Elective surgical procedure or medical intervention to terminate the pregnancy at any gestational age) 
  ## [varname: INF_ABOR_IND]
  mutate(INF_ABOR_IND = case_when(ADJUD_NEEDED==1 ~ 55, 
                                  M04_FETAL_LOSS_DSDECOD == 2 ~ 1, # if specified fetal loss is "induced abortion" at any GA 
                                  TRUE~ 0)) %>% 
  # c. generate variable for fetal death @ unknown GA (reported fetal loss but missing fetal loss date)
  ## [varname: INF_FETAL_DTH_UNGA]
  mutate(INF_FETAL_DTH_UNGA = case_when(ADJUD_NEEDED==1 ~ 55, 
                                        M04_PRG_DSDECOD == 2 & is.na(PREG_END_DATE) ~ 1, # if fetal loss is reported but is missing fetal loss date
                                        TRUE~ 0)) %>% 
  
  # d. generate variable for all fetal death (stillbirth or miscarriage) 
  ## [varname: INF_FETAL_DTH]
  mutate(INF_FETAL_DTH = case_when(ADJUD_NEEDED==1 ~ 55, 
                                   STILLBIRTH_20WK == 1 | INF_ABOR_SPN == 1 | INF_FETAL_DTH_UNGA == 1 ~ 1,
                                   TRUE~ 0)) %>% 
  # d. generate fetal death denominator (all deliveries EXCLUDING induced abortions) 
  ## [varname: INF_FETAL_DTH_DENOM]
  mutate(INF_FETAL_DTH_DENOM = case_when(ADJUD_NEEDED==1 ~ 0, 
                                         INF_ABOR_IND==1 ~ 0,
                                         TRUE~ 1))  %>% 
  # d. generate fetal death denominator (all deliveries) 
  ## [varname: INF_FETAL_DTH_OTHR_DENOM]
  mutate(INF_FETAL_DTH_OTHR_DENOM = case_when(ADJUD_NEEDED==1 ~ 55, TRUE ~ 1)) 

# export data 
# write.csv(fetal_death, paste0(path_to_save, "fetal_death" ,".csv"), row.names=FALSE)
#*****************************************************************************
## Birth Asphyxia ----
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
  select(SITE, INFANTID, MOMID, PREGID, BIRTH_OUTCOME, ADJUD_NEEDED) %>%
  # merge in mnh11 data 
  left_join(mnh11_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  select(SITE, INFANTID, MOMID, PREGID, BIRTH_OUTCOME,ADJUD_NEEDED, M11_BREATH_FAIL_CEOCCUR, contains("M11_INF_PROCCUR_")) %>%
  # merge in mnh20 variables 
  left_join(mnh20[c("SITE", "MOMID", "PREGID","INFANTID", "M20_BIRTH_COMPL_MHTERM_3")], 
            by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  # a. generate variable for birth asphyxia (Clinician reports failure to breathe spontaneously in the first minute after delivery.)
  ## [varname: INF_ASPH]
  mutate(INF_ASPH = case_when(ADJUD_NEEDED==1 ~ 55, 
                              
                              M11_BREATH_FAIL_CEOCCUR == 1 | M20_BIRTH_COMPL_MHTERM_3 == 1 | 
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
  mutate(DATA_COMPLETE_DENOM = case_when(ADJUD_NEEDED==1 ~ 55, 
                                         BIRTH_OUTCOME ==1 ~ 1, TRUE ~ 0),
         INF_ASPH_DENOM = case_when(ADJUD_NEEDED==1 ~ 55, 
                                    BIRTH_OUTCOME ==1 ~ 1, TRUE ~ 0)
  ) %>% 
  ## rename variables 
  rename(INF_BREATH_MASK_VENT = M11_INF_PROCCUR_2,
         INF_BREATH_PRESSURE	= M11_INF_PROCCUR_3,
         INF_BREATH_SUCTION	= M11_INF_PROCCUR_4,
         INF_BREATH_INTUBATION	= M11_INF_PROCCUR_5,
         INF_BREATH_COMPRESS	= M11_INF_PROCCUR_6,
         INF_BREATH_FAIL	= M11_BREATH_FAIL_CEOCCUR
  )



# export data 
# write.csv(birth_asphyxia, paste0(path_to_save, "birth_asphyxia" ,".csv"), row.names=FALSE)
#*******************************************************************************
## Hyperbilirubinemia ----
#
# Refactor of the existing outcome code. Final outcome variable names are retained,
# but TYPE_VISIT is not used to assign PNC visits. Instead:
#   1. Every available IPC, PNC, and hospitalization observation is retained.
#   2. Infant age is calculated from DOB/delivery datetime and the actual visit date/time.
#   3. PNC windows are assigned from age using the late visit windows.
#   4. Repeated and in-between visits remain in the long datasets.
#   5. Wide audit files store repeated/extra visits as pipe-delimited lists.
#   6. Final hyperbilirubinemia and PSBI outcome names are kept consistent with
#      the existing code.
#
# JAUND_CESTDAT is categorical, NOT a date:
#   1 = jaundice appeared <24 hours after birth
#   2 = jaundice appeared >=24 hours after birth
#  77 = no jaundice present
#*******************************************************************************


# AGE-BASED INFANT VISIT CONSTRUCTION
# Replaces TYPE_VISIT-based assignment for TCB/hyperbilirubinemia and PSBI.
# Keeps every observed visit/result in long data and derives scheduled PNC windows
# from infant age at the visit.

library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(stringr)


# TCB / HYPERBILIRUBINEMIA 
# Helper functions 
## Date cleaning function (from Flowchart workflow) 
clean_date <- function(x,
                       invalid_dates = as_date(c("1907-07-07", "2007-07-07",
                                                 "1905-05-05", "1909-07-09",
                                                 "1906-06-06", "1909-09-09")),
                       min_date = NULL,
                       max_date = NULL) {
  if (is.null(max_date)) max_date <- today()
  parsed <- parse_date_time(
    trimws(as.character(x)),
    orders = c("d/m/Y", "d-m-Y", "Y-m-d",
               "d-b-y", "d-m-y",
               "m/d/y", "m-d-y"),
    exact = FALSE
  ) %>% as_date()
  invalid_mask <- is.na(parsed) | parsed %in% invalid_dates
  if (!is.null(min_date)) invalid_mask <- invalid_mask | parsed < min_date
  if (!is.null(max_date)) invalid_mask <- invalid_mask | parsed > max_date
  parsed[invalid_mask] <- NA
  return(parsed)
}

assign_pnc_window <- function(age_days) {
  case_when(
    age_days >= 3   & age_days <= 5   ~ "PNC0",
    age_days >= 7   & age_days <= 14  ~ "PNC1",
    age_days >= 28  & age_days <= 35  ~ "PNC4",
    age_days >= 42  & age_days <= 104 ~ "PNC6",
    age_days >= 182 & age_days <= 279 ~ "PNC26",
    age_days >= 364 & age_days <= 454 ~ "PNC52",
    !is.na(age_days)                  ~ "INBETWEEN",
    TRUE                              ~ NA_character_
  )
}

collapse_list <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = " | ")
}

## Construct ALL TCB observations in long format 

## IPC TCB observations (MNH11)
tcb_ipc_long <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, LIVEBIRTH, BIRTH_OUTCOME,
         DOB, DELIVERY_DATETIME, GESTAGEBIRTH_ANY, GESTAGEBIRTH_ANY_DAYS) %>%
  left_join(mnh11_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  mutate(
    DOB = clean_date(DOB),
    M11_VISIT_OBSSTDAT = clean_date(M11_VISIT_OBSSTDAT),
    M11_VISIT_OBSSTTIM = replace(M11_VISIT_OBSSTTIM,
                                 M11_VISIT_OBSSTTIM %in% c("77:77", "99:99", "55:55:00"), NA),
    
    # --- Normalize time: pad "H:MM" / "HH:M" to "HH:MM"; keep NA for now ---
    M11_TIME_CLEAN = {
      t <- trimws(as.character(M11_VISIT_OBSSTTIM))
      t <- ifelse(grepl("^\\d{1,2}:\\d{1,2}$", t),
                  sprintf("%02d:%02d",
                          as.integer(sub(":.*", "", t)),
                          as.integer(sub(".*:", "", t))),
                  NA_character_)
      t
    },
    
    DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format = "%Y-%m-%d %H:%M"),
    
    # --- Impute 12:00 (midpoint of day) ONLY when the time is missing AND
    #     the visit date differs from DOB. Same-day visits keep NA time so
    #     hour-of-life is not fabricated on day 0. ---
    M11_TIME_USE = case_when(
      !is.na(M11_TIME_CLEAN)                    ~ M11_TIME_CLEAN,
      is.na(M11_VISIT_OBSSTDAT) | is.na(DOB)    ~ NA_character_,
      M11_VISIT_OBSSTDAT != DOB                 ~ "12:00",
      TRUE                                      ~ NA_character_
    ),
    
    VISIT_DATETIME = as.POSIXct(paste(M11_VISIT_OBSSTDAT, M11_TIME_USE),
                                format = "%Y-%m-%d %H:%M"),
    VISIT_DATE = M11_VISIT_OBSSTDAT,
    
    VISIT_TIME_IMPUTED = as.integer(
      is.na(M11_TIME_CLEAN) &
        !is.na(M11_VISIT_OBSSTDAT) & !is.na(DOB) &
        M11_VISIT_OBSSTDAT != DOB
    ),
    
    # --- Age from datetime when possible ---
    AGE_AT_VISIT_HOURS_FROM_DT = floor(
      as.numeric(difftime(VISIT_DATETIME, DELIVERY_DATETIME, units = "hours"))
    ),
    
    # --- Age from dates only as fallback (covers same-day visits with no time) ---
    AGE_AT_VISIT_DAYS_FROM_DATE = if_else(
      !is.na(VISIT_DATE) & !is.na(DOB) & VISIT_DATE >= DOB,
      as.numeric(VISIT_DATE - DOB),
      NA_real_
    ),
    
    # --- Resolve: prefer datetime, fall back to date-only ---
    AGE_AT_VISIT_HOURS_TOTAL = case_when(
      !is.na(AGE_AT_VISIT_HOURS_FROM_DT) & AGE_AT_VISIT_HOURS_FROM_DT >= 0 ~ AGE_AT_VISIT_HOURS_FROM_DT,
      !is.na(AGE_AT_VISIT_DAYS_FROM_DATE)                                   ~ AGE_AT_VISIT_DAYS_FROM_DATE * 24,
      TRUE                                                                  ~ NA_real_
    ),
    AGE_AT_VISIT_DAYS = as.numeric(AGE_AT_VISIT_HOURS_TOTAL) %/% 24,
    AGE_AT_VISIT_HRS  = as.numeric(AGE_AT_VISIT_HOURS_TOTAL) %% 24,
    
    VISIT_SOURCE = "IPC",
    VISIT_WINDOW = "IPC",
    TCB_ASSESSED = M11_BILIRUBIN_LBPERF,
    TCB_VALUE_RAW = suppressWarnings(as.numeric(M11_TBILIRUBIN_UMOLL_LBORRES)),
    TCB_VALUE = if_else(!is.na(TCB_VALUE_RAW) & TCB_VALUE_RAW >= 0, TCB_VALUE_RAW, NA_real_),
    JAUND_CEOCCUR = M11_JAUND_CEOCCUR,
    YELLOW_CEOCCUR = M11_YELLOW_CEOCCUR,
    JAUND_ONSET = suppressWarnings(as.numeric(M11_JAUND_CESTDAT))
  ) %>%
  select(SITE, MOMID, PREGID, INFANTID, LIVEBIRTH, BIRTH_OUTCOME, DOB,
         DELIVERY_DATETIME, GESTAGEBIRTH_ANY, GESTAGEBIRTH_ANY_DAYS,
         VISIT_SOURCE, VISIT_WINDOW, VISIT_DATE, VISIT_DATETIME,
         VISIT_TIME_IMPUTED,
         AGE_AT_VISIT_DAYS, AGE_AT_VISIT_HRS, AGE_AT_VISIT_HOURS_TOTAL,
         TCB_ASSESSED, TCB_VALUE_RAW, TCB_VALUE,
         JAUND_CEOCCUR, YELLOW_CEOCCUR, JAUND_ONSET)

## PNC clinical observations (MNH13)
pnc_clinical_long <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, DOB) %>%
  right_join(mnh13, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  mutate(
    DOB = clean_date(DOB),
    VISIT_DATE = clean_date(
      M13_VISIT_OBSSTDAT,
      invalid_dates = as_date(c("1907-07-07", "2007-07-07", "1905-05-05",
                                "1909-07-09", "1906-06-06", "1909-09-09",
                                "2028-08-12", "2084-10-25", "2924-03-22",
                                "8024-04-08"))
    ),
    AGE_AT_VISIT_DAYS = case_when(
      !is.na(VISIT_DATE) & !is.na(DOB) & VISIT_DATE >= DOB ~ as.numeric(VISIT_DATE - DOB),
      TRUE ~ NA_real_
    ),
    VISIT_WINDOW = assign_pnc_window(AGE_AT_VISIT_DAYS)
  ) %>%
  select(SITE, MOMID, PREGID, INFANTID, VISIT_DATE, AGE_AT_VISIT_DAYS,
         VISIT_WINDOW, M13_JAUND_CEOCCUR, M13_JAUND_CESTTIM, M13_YELL_CEOCCUR)

## PNC TCB observations (MNH14)
tcb_pnc_long <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, LIVEBIRTH, BIRTH_OUTCOME,
         DOB, DELIVERY_DATETIME, GESTAGEBIRTH_ANY, GESTAGEBIRTH_ANY_DAYS) %>%
  right_join(mnh14, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  mutate(
    DOB = clean_date(DOB),
    VISIT_DATE = clean_date(M14_VISIT_OBSSTDAT),
    M14_TCB_OBSSTTIM = replace(M14_TCB_OBSSTTIM,
                               M14_TCB_OBSSTTIM %in% c("77:77", "99:99", "55:55:00"), NA),
    
    # --- Normalize time: pad "H:MM" / "HH:M" to "HH:MM"; keep NA for now ---
    M14_TIME_CLEAN = {
      t <- trimws(as.character(M14_TCB_OBSSTTIM))
      t <- ifelse(grepl("^\\d{1,2}:\\d{1,2}$", t),
                  sprintf("%02d:%02d",
                          as.integer(sub(":.*", "", t)),
                          as.integer(sub(".*:", "", t))),
                  NA_character_)
      t
    },
    
    DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format = "%Y-%m-%d %H:%M"),
    
    # --- Impute 12:00 (midpoint of day) ONLY when the time is missing AND
    #     the visit date differs from DOB. Same-day visits keep NA time. ---
    M14_TIME_USE = case_when(
      !is.na(M14_TIME_CLEAN)                 ~ M14_TIME_CLEAN,
      is.na(VISIT_DATE) | is.na(DOB)         ~ NA_character_,
      VISIT_DATE != DOB                      ~ "12:00",
      TRUE                                   ~ NA_character_
    ),
    
    VISIT_DATETIME = as.POSIXct(paste(VISIT_DATE, M14_TIME_USE),
                                format = "%Y-%m-%d %H:%M"),
    
    VISIT_TIME_IMPUTED = as.integer(
      is.na(M14_TIME_CLEAN) &
        !is.na(VISIT_DATE) & !is.na(DOB) &
        VISIT_DATE != DOB
    ),
    
    # --- Age from datetime when possible ---
    AGE_AT_VISIT_HOURS_FROM_DT = floor(
      as.numeric(difftime(VISIT_DATETIME, DELIVERY_DATETIME, units = "hours"))
    ),
    
    # --- Age from dates only as fallback ---
    AGE_AT_VISIT_DAYS_FROM_DATE = if_else(
      !is.na(VISIT_DATE) & !is.na(DOB) & VISIT_DATE >= DOB,
      as.numeric(VISIT_DATE - DOB),
      NA_real_
    ),
    
    # --- Resolve: prefer datetime, fall back to date-only ---
    AGE_AT_VISIT_HOURS_TOTAL = case_when(
      !is.na(AGE_AT_VISIT_HOURS_FROM_DT) & AGE_AT_VISIT_HOURS_FROM_DT >= 0 ~ AGE_AT_VISIT_HOURS_FROM_DT,
      !is.na(AGE_AT_VISIT_DAYS_FROM_DATE)                                   ~ AGE_AT_VISIT_DAYS_FROM_DATE * 24,
      TRUE                                                                  ~ NA_real_
    ),
    AGE_AT_VISIT_DAYS = as.numeric(AGE_AT_VISIT_HOURS_TOTAL) %/% 24,
    AGE_AT_VISIT_HRS  = as.numeric(AGE_AT_VISIT_HOURS_TOTAL) %% 24,
    
    VISIT_SOURCE = "PNC",
    VISIT_WINDOW = assign_pnc_window(AGE_AT_VISIT_DAYS),
    TCB_ASSESSED = M14_TCB_VSSTAT,
    TCB_VALUE_RAW = suppressWarnings(as.numeric(M14_TCB_UMOLL_LBORRES)),
    TCB_VALUE = if_else(!is.na(TCB_VALUE_RAW) & TCB_VALUE_RAW >= 0, TCB_VALUE_RAW, NA_real_)
  ) %>%
  left_join(
    pnc_clinical_long %>%
      select(SITE, MOMID, PREGID, INFANTID, VISIT_DATE,
             M13_JAUND_CEOCCUR, M13_JAUND_CESTTIM, M13_YELL_CEOCCUR),
    by = c("SITE", "MOMID", "PREGID", "INFANTID", "VISIT_DATE")
  ) %>%
  mutate(
    JAUND_CEOCCUR = M13_JAUND_CEOCCUR,
    YELLOW_CEOCCUR = M13_YELL_CEOCCUR,
    JAUND_ONSET = NA_real_
  )

## Determine site-level TCB units and convert all actual measurements 

## MNH14
tcb_pnc_long <- tcb_pnc_long %>%
  group_by(SITE) %>%
  mutate(
    m14_mean = mean(TCB_VALUE, na.rm = TRUE),
    unit = case_when(
      is.nan(m14_mean) ~ NA_character_,
      m14_mean >= 25 ~ "umol/L",
      m14_mean < 25 ~ "mg/dL",
      TRUE ~ NA_character_
    ),
    TCB_VALUE = case_when(
      unit == "umol/L" ~ TCB_VALUE / 17.1,
      TRUE ~ TCB_VALUE
    )
  ) %>%
  ungroup()

m14_subset <- tcb_pnc_long %>% distinct(SITE, unit)
for (i in m14_subset$SITE) {
  out <- m14_subset %>% filter(SITE == i)
  cat(i, "reporting in", out$unit, "in MNH14", "\n")
}

## MNH11
tcb_ipc_long <- tcb_ipc_long %>%
  group_by(SITE) %>%
  mutate(
    m11_mean = mean(TCB_VALUE, na.rm = TRUE),
    unit = case_when(
      is.nan(m11_mean) ~ NA_character_,
      m11_mean >= 25 ~ "umol/L",
      m11_mean < 25 ~ "mg/dL",
      TRUE ~ NA_character_
    ),
    TCB_VALUE = case_when(
      unit == "umol/L" ~ TCB_VALUE / 17.1,
      TRUE ~ TCB_VALUE
    )
  ) %>%
  ungroup()

m11_subset <- tcb_ipc_long %>% distinct(SITE, unit)
for (i in m11_subset$SITE) {
  out <- m11_subset %>% filter(SITE == i)
  cat(i, "reporting in", out$unit, "in MNH11", "\n")
}

## Add clinical PNC visits without a corresponding MNH14 TCB record 
pnc_clinical_only <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, LIVEBIRTH, BIRTH_OUTCOME,
         DOB, DELIVERY_DATETIME, GESTAGEBIRTH_ANY, GESTAGEBIRTH_ANY_DAYS) %>%
  right_join(pnc_clinical_long, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  anti_join(
    tcb_pnc_long %>% distinct(SITE, MOMID, PREGID, INFANTID, VISIT_DATE),
    by = c("SITE", "MOMID", "PREGID", "INFANTID", "VISIT_DATE")
  ) %>%
  filter(
    (!is.na(M13_JAUND_CEOCCUR) & !M13_JAUND_CEOCCUR %in% c(-5, -7, 77, 99)) |
      (!is.na(M13_YELL_CEOCCUR) & !M13_YELL_CEOCCUR %in% c(-5, -7, 77, 99))
  ) %>%
  mutate(
    VISIT_SOURCE = "PNC",
    VISIT_DATETIME = as.POSIXct(NA),
    DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format = "%Y-%m-%d %H:%M"),
    AGE_AT_VISIT_HRS = NA_real_,
    AGE_AT_VISIT_HOURS_TOTAL = NA_real_,
    TCB_ASSESSED = NA,
    TCB_VALUE_RAW = NA_real_,
    TCB_VALUE = NA_real_,
    JAUND_CEOCCUR = M13_JAUND_CEOCCUR,
    YELLOW_CEOCCUR = M13_YELL_CEOCCUR,
    JAUND_ONSET = NA_real_
  )

## Combine ALL TCB + jaundice observations 
tcb_long <- bind_rows(
  tcb_ipc_long %>% select(any_of(c(
    "SITE","MOMID","PREGID","INFANTID","LIVEBIRTH","BIRTH_OUTCOME","DOB",
    "DELIVERY_DATETIME","GESTAGEBIRTH_ANY","GESTAGEBIRTH_ANY_DAYS",
    "VISIT_SOURCE","VISIT_WINDOW","VISIT_DATE","VISIT_DATETIME",
    "AGE_AT_VISIT_DAYS","AGE_AT_VISIT_HRS","AGE_AT_VISIT_HOURS_TOTAL",
    "TCB_ASSESSED","TCB_VALUE_RAW","TCB_VALUE",
    "JAUND_CEOCCUR","YELLOW_CEOCCUR","JAUND_ONSET","unit"))),
  
  tcb_pnc_long %>% select(any_of(c(
    "SITE","MOMID","PREGID","INFANTID","LIVEBIRTH","BIRTH_OUTCOME","DOB",
    "DELIVERY_DATETIME","GESTAGEBIRTH_ANY","GESTAGEBIRTH_ANY_DAYS",
    "VISIT_SOURCE","VISIT_WINDOW","VISIT_DATE","VISIT_DATETIME",
    "AGE_AT_VISIT_DAYS","AGE_AT_VISIT_HRS","AGE_AT_VISIT_HOURS_TOTAL",
    "TCB_ASSESSED","TCB_VALUE_RAW","TCB_VALUE",
    "JAUND_CEOCCUR","YELLOW_CEOCCUR","JAUND_ONSET","unit"))),
  
  pnc_clinical_only %>% select(any_of(c(
    "SITE","MOMID","PREGID","INFANTID","LIVEBIRTH","BIRTH_OUTCOME","DOB",
    "DELIVERY_DATETIME","GESTAGEBIRTH_ANY","GESTAGEBIRTH_ANY_DAYS",
    "VISIT_SOURCE","VISIT_WINDOW","VISIT_DATE","VISIT_DATETIME",
    "AGE_AT_VISIT_DAYS","AGE_AT_VISIT_HRS","AGE_AT_VISIT_HOURS_TOTAL",
    "TCB_ASSESSED","TCB_VALUE_RAW","TCB_VALUE",
    "JAUND_CEOCCUR","YELLOW_CEOCCUR","JAUND_ONSET")))
) %>%
  mutate(
    AGE_AT_VISIT_HOURS_USE = case_when(
      !is.na(AGE_AT_VISIT_HOURS_TOTAL) & AGE_AT_VISIT_HOURS_TOTAL >= 0 ~ AGE_AT_VISIT_HOURS_TOTAL,
      !is.na(AGE_AT_VISIT_DAYS) & AGE_AT_VISIT_DAYS >= 0 ~ AGE_AT_VISIT_DAYS * 24,
      TRUE ~ NA_real_
    ),
    AGE_SOURCE = case_when(
      !is.na(AGE_AT_VISIT_HOURS_TOTAL) & AGE_AT_VISIT_HOURS_TOTAL >= 0 ~ "DATETIME",
      !is.na(AGE_AT_VISIT_DAYS) & AGE_AT_VISIT_DAYS >= 0 ~ "DATE_ONLY",
      TRUE ~ "MISSING"
    ),
    TCB_REPORTED = as.integer(!is.na(TCB_VALUE)),
    TCB_AGE_WINDOW = case_when(
      TCB_REPORTED == 1 & AGE_AT_VISIT_HOURS_USE >= 0 & AGE_AT_VISIT_HOURS_USE < 24 ~ "24HR",
      TCB_REPORTED == 1 & AGE_AT_VISIT_HOURS_USE >= 24 & AGE_AT_VISIT_HOURS_USE < 5*24 ~ "5DAY",
      TCB_REPORTED == 1 & AGE_AT_VISIT_HOURS_USE >= 5*24 & AGE_AT_VISIT_HOURS_USE < 14*24 ~ "14DAY",
      TCB_REPORTED == 1 & AGE_AT_VISIT_HOURS_USE >= 14*24 ~ ">=14DAY",
      TCB_REPORTED == 1 & is.na(AGE_AT_VISIT_HOURS_USE) ~ "AGE_MISSING",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(SITE, MOMID, PREGID, INFANTID, VISIT_DATE, VISIT_DATETIME) %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  mutate(VISIT_SEQ = row_number()) %>%
  ungroup()

## Audit 
cat("\nTCB AVAILABILITY CHECK\n")
cat("Actual usable TCB measurements:", sum(!is.na(tcb_long$TCB_VALUE)), "\n")
cat("Negative raw TCB values converted to missing:",
    sum(tcb_long$TCB_VALUE_RAW < 0, na.rm = TRUE), "\n")
cat("Negative TCB values remaining after cleaning:",
    sum(tcb_long$TCB_VALUE < 0, na.rm = TRUE), "\n")
cat("Actual TCBs with exact datetime age:",
    sum(!is.na(tcb_long$TCB_VALUE) & tcb_long$AGE_SOURCE == "DATETIME"), "\n")
cat("Actual TCBs with date-only age:",
    sum(!is.na(tcb_long$TCB_VALUE) & tcb_long$AGE_SOURCE == "DATE_ONLY"), "\n")
cat("Actual TCBs with missing age:",
    sum(!is.na(tcb_long$TCB_VALUE) & tcb_long$AGE_SOURCE == "MISSING"), "\n")

tcb_reported_check <- tcb_long %>%
  filter(TCB_REPORTED == 1) %>%
  count(VISIT_SOURCE, AGE_SOURCE, TCB_AGE_WINDOW, name = "N_TCB") %>%
  arrange(VISIT_SOURCE, AGE_SOURCE, TCB_AGE_WINDOW)
print(tcb_reported_check)

## Per-observation TCB criteria 
tcb_long <- tcb_long %>%
  mutate(
    INF_HYPERBILI_TCB15 = case_when(
      TCB_REPORTED == 1 & TCB_VALUE > 15 ~ 1L,
      TCB_REPORTED == 1 ~ 0L,
      TRUE ~ NA_integer_
    ),
    TCB_DAYS = floor(AGE_AT_VISIT_HOURS_USE / 24),
    TCB_HRS = floor(AGE_AT_VISIT_HOURS_USE %% 24)
  )

## AAP and NICE threshold lookup 
threshold_file <- "~/PRISMA-Public/PRISMA-Infant-Constructed-Outcomes/TCB_threshold_lookup.csv"

if (file.exists(threshold_file)) {
  tcb_thresholds <- read.csv(threshold_file) %>%
    mutate(
      GESTAGEBIRTH_ANY = as.numeric(GESTAGEBIRTH_ANY),
      TCB_DAYS = as.numeric(TCB_DAYS),
      TCB_HRS = as.numeric(TCB_HRS)
    )
} else {
  tcb_thresholds <- tibble(
    GESTAGEBIRTH_ANY = numeric(),
    TCB_DAYS = numeric(),
    TCB_HRS = numeric(),
    TCB_AAP_THRESH = numeric(),
    TCB_NICE_THRESH = numeric()
  )
}

thresholds_needed <- tcb_long %>%
  filter(TCB_REPORTED == 1, !is.na(GESTAGEBIRTH_ANY),
         !is.na(TCB_DAYS), !is.na(TCB_HRS), TCB_DAYS >= 0) %>%
  distinct(GESTAGEBIRTH_ANY, TCB_DAYS, TCB_HRS)

new_thresholds <- thresholds_needed %>%
  anti_join(tcb_thresholds,
            by = c("GESTAGEBIRTH_ANY", "TCB_DAYS", "TCB_HRS"))

if (nrow(new_thresholds) > 0) {
  cat("Calculating", nrow(new_thresholds), "new TCB threshold combinations\n")
  new_thresholds <- new_thresholds %>%
    rowwise() %>%
    mutate(
      # AAP: only valid for GA >= 35 weeks; NA otherwise.
      TCB_AAP_THRESH = ifelse(
        !is.na(GESTAGEBIRTH_ANY) & GESTAGEBIRTH_ANY >= 35,
        tryCatch(
          TSB("P0", paste0(GESTAGEBIRTH_ANY, " weeks"),
              days = TCB_DAYS, hours = TCB_HRS) - 3,
          error = function(e) NA_real_
        ),
        NA_real_
      ),
      # NICE: valid for all GA; floor GA at 23 weeks.
      TCB_NICE_THRESH = tryCatch(
        TSB_NICE("P0", paste0(max(GESTAGEBIRTH_ANY, 23), " weeks"),
                 days = TCB_DAYS, hours = TCB_HRS) - 3,
        error = function(e) NA_real_
      )
    ) %>%
    ungroup()
  
  tcb_thresholds <- bind_rows(tcb_thresholds, new_thresholds) %>%
    distinct(GESTAGEBIRTH_ANY, TCB_DAYS, TCB_HRS, .keep_all = TRUE)
  write.csv(tcb_thresholds, threshold_file, row.names = FALSE)
} else {
  cat("All TCB thresholds already available in lookup\n")
}

# Join thresholds and classify. NOTE: 77L = ineligible (GA < 35 for AAP),
# so it's distinguishable from NA (no data / no threshold).
tcb_long <- tcb_long %>%
  left_join(tcb_thresholds,
            by = c("GESTAGEBIRTH_ANY", "TCB_DAYS", "TCB_HRS")) %>%
  mutate(
    # AAP:
    #   77 = GA < 35 weeks or GA missing (ineligible by design)
    #    1 = TCB >= AAP threshold
    #    0 = TCB < AAP threshold
    #    0 = threshold is NA after TSB() ran (package couldn't produce a value)
    #   NA = no TCB result at all
    INF_HYPERBILI_AAP = case_when(
      TCB_REPORTED == 1 & (is.na(GESTAGEBIRTH_ANY) | GESTAGEBIRTH_ANY < 35) ~ 77L,
      TCB_REPORTED == 1 & is.na(TCB_AAP_THRESH) ~ 0L,   # <- threshold NA after TSB() -> 0
      TCB_REPORTED == 1 & TCB_VALUE >= TCB_AAP_THRESH ~ 1L,
      TCB_REPORTED == 1 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # NICE: same pattern, no GA restriction.
    INF_HYPERBILI_NICE = case_when(
      TCB_REPORTED == 1 & is.na(TCB_NICE_THRESH) ~ 0L,  # <- same rule for NICE
      TCB_REPORTED == 1 & TCB_VALUE >= TCB_NICE_THRESH ~ 1L,
      TCB_REPORTED == 1 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # Age window tags per visit — this is what makes the multi-visit case work
    TCB_WINDOW = case_when(
      !is.na(TCB_VALUE) & !is.na(AGE_AT_VISIT_HOURS_USE) &
        AGE_AT_VISIT_HOURS_USE >= 0     & AGE_AT_VISIT_HOURS_USE < 24    ~ "24HR",
      !is.na(TCB_VALUE) & !is.na(AGE_AT_VISIT_HOURS_USE) &
        AGE_AT_VISIT_HOURS_USE >= 24    & AGE_AT_VISIT_HOURS_USE < 5*24  ~ "5DAY",
      !is.na(TCB_VALUE) & !is.na(AGE_AT_VISIT_HOURS_USE) &
        AGE_AT_VISIT_HOURS_USE >= 5*24  & AGE_AT_VISIT_HOURS_USE <= 14*24 ~ "14DAY",
      !is.na(TCB_VALUE) & !is.na(AGE_AT_VISIT_HOURS_USE) &
        AGE_AT_VISIT_HOURS_USE > 14*24 ~ ">14DAY",
      TRUE ~ NA_character_
    )
  )

## Infant-level TCB outcomes 
first_positive_age <- function(flag, age) {
  z <- age[flag == 1 & !is.na(flag) & !is.na(age)]
  if (length(z) == 0) NA_real_ else min(z)
}

# Helper: collapse a vector of flags to 1 / 0 / 77 / NA
#   1 if any 1
#   0 if no 1 but at least one non-missing 0
#   77 if no 1 and no 0 but at least one 77  (only used for AAP)
#   NA otherwise (all missing)
collapse_flag <- function(x, allow_77 = FALSE) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_integer_)
  if (any(x == 1L))  return(1L)
  if (any(x == 0L))  return(0L)
  if (allow_77 && any(x == 77L)) return(77L)
  NA_integer_
}

### TCB >15 mg/dL ----
hyperbili_crit1 <- tcb_long %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  summarise(
    INF_HYPERBILI_TCB15_AGE_DAYS = first_positive_age(INF_HYPERBILI_TCB15, AGE_AT_VISIT_DAYS),
    INF_HYPERBILI_TCB15_AGE_HRS = {
      a <- first_positive_age(INF_HYPERBILI_TCB15, AGE_AT_VISIT_HOURS_USE)
      if (is.na(a)) NA_real_ else a %% 24
    },
    INF_HYPERBILI_TCB15_ANY   = collapse_flag(INF_HYPERBILI_TCB15[TCB_WINDOW %in% c("24HR","5DAY","14DAY")]),
    INF_HYPERBILI_TCB15_24HR  = collapse_flag(INF_HYPERBILI_TCB15[TCB_WINDOW == "24HR"]),
    INF_HYPERBILI_TCB15_5DAY  = collapse_flag(INF_HYPERBILI_TCB15[TCB_WINDOW == "5DAY"]),
    INF_HYPERBILI_TCB15_14DAY = collapse_flag(INF_HYPERBILI_TCB15[TCB_WINDOW == "14DAY"]),
    .groups = "drop"
  )


### TCB >= AAP threshold ---- 
hyperbili_crit2 <- tcb_long %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  summarise(
    GESTAGEBIRTH_ANY = first(na.omit(GESTAGEBIRTH_ANY), default = NA_real_),
    INF_HYPERBILI_AAP_AGE_DAYS = first_positive_age(INF_HYPERBILI_AAP, AGE_AT_VISIT_DAYS),
    INF_HYPERBILI_AAP_AGE_HRS = {
      a <- first_positive_age(INF_HYPERBILI_AAP, AGE_AT_VISIT_HOURS_USE)
      if (is.na(a)) NA_real_ else a %% 24
    },
    INF_HYPERBILI_AAP_ANY   = collapse_flag(INF_HYPERBILI_AAP[TCB_WINDOW %in% c("24HR","5DAY","14DAY")], allow_77 = TRUE),
    INF_HYPERBILI_AAP_24HR  = collapse_flag(INF_HYPERBILI_AAP[TCB_WINDOW == "24HR"], allow_77 = TRUE),
    INF_HYPERBILI_AAP_5DAY  = collapse_flag(INF_HYPERBILI_AAP[TCB_WINDOW == "5DAY"], allow_77 = TRUE),
    INF_HYPERBILI_AAP_14DAY = collapse_flag(INF_HYPERBILI_AAP[TCB_WINDOW == "14DAY"], allow_77 = TRUE),
    .groups = "drop"
  )


### TCB >= NICE threshold ----
hyperbili_crit4 <- tcb_long %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  summarise(
    INF_HYPERBILI_NICE_AGE_DAYS = first_positive_age(INF_HYPERBILI_NICE, AGE_AT_VISIT_DAYS),
    INF_HYPERBILI_NICE_AGE_HRS = {
      a <- first_positive_age(INF_HYPERBILI_NICE, AGE_AT_VISIT_HOURS_USE)
      if (is.na(a)) NA_real_ else a %% 24
    },
    INF_HYPERBILI_NICE_ANY   = collapse_flag(INF_HYPERBILI_NICE[TCB_WINDOW %in% c("24HR","5DAY","14DAY")]),
    INF_HYPERBILI_NICE_24HR  = collapse_flag(INF_HYPERBILI_NICE[TCB_WINDOW == "24HR"]),
    INF_HYPERBILI_NICE_5DAY  = collapse_flag(INF_HYPERBILI_NICE[TCB_WINDOW == "5DAY"]),
    INF_HYPERBILI_NICE_14DAY = collapse_flag(INF_HYPERBILI_NICE[TCB_WINDOW == "14DAY"]),
    .groups = "drop"
  )

### Clinical jaundice ---- 
jaundice_long <- tcb_long %>%
  filter(!is.na(AGE_AT_VISIT_DAYS), AGE_AT_VISIT_DAYS <= 35) %>%
  mutate(
    INF_JAUN = case_when(
      JAUND_CEOCCUR == 1 | YELLOW_CEOCCUR == 1 ~ 1L,
      JAUND_CEOCCUR %in% c(0,1) | YELLOW_CEOCCUR %in% c(0,1) ~ 0L,
      TRUE ~ NA_integer_
    ),
    INF_JAUN_DATE = if_else(INF_JAUN == 1, VISIT_DATE, as.Date(NA)),
    INF_JAUN_AGE_DAYS = if_else(INF_JAUN == 1, AGE_AT_VISIT_DAYS, NA_real_),
    INF_JAUN_AGE_HOURS_IPC = if_else(INF_JAUN == 1 & VISIT_SOURCE == "IPC",
                                     AGE_AT_VISIT_HRS, NA_real_),
    INF_JAUN_SEV = case_when(
      INF_JAUN == 1 & (YELLOW_CEOCCUR == 1 | JAUND_ONSET == 1) ~ 1L,
      INF_JAUN == 1 ~ 0L,
      TRUE ~ 0L
    ),
    INF_JAUN_NON_SEV = case_when(
      INF_JAUN == 1 & INF_JAUN_SEV == 0 ~ 1L,
      TRUE ~ 0L
    ),
    DENOM_JAUN = as.integer(
      (JAUND_CEOCCUR %in% c(0,1) | YELLOW_CEOCCUR %in% c(0,1)) &
        AGE_AT_VISIT_DAYS <= 14 #14days!
    )
  )


first_positive_date_j <- function(flag, date) {
  # coerce to Date no matter what comes in
  date <- suppressWarnings(as.Date(date))
  z <- date[!is.na(flag) & flag == 1 & !is.na(date)]
  if (length(z) == 0) return(as.Date(NA))
  suppressWarnings(min(z, na.rm = TRUE))
}

first_positive_age_j <- function(flag, age) {
  age <- suppressWarnings(as.numeric(age))
  z <- age[!is.na(flag) & flag == 1 & !is.na(age)]
  if (length(z) == 0) return(NA_real_)
  min(z)
}

first_positive_hours_j <- function(flag, hours) {
  hours <- suppressWarnings(as.numeric(hours))
  z <- hours[!is.na(flag) & flag == 1 & !is.na(hours)]
  if (length(z) == 0) return(NA_real_)
  min(z)
}
## per-infant (wide) — group_by + summarise collapses visits
hyperbili_crit3 <- jaundice_long %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  summarise(
    
    ##  Overall jaundice 
    INF_JAUN          = as.integer(any(INF_JAUN == 1, na.rm = TRUE)),
    INF_JAUN_DATE     = first_positive_date_j(INF_JAUN, INF_JAUN_DATE),
    INF_JAUN_AGE_DAYS = first_positive_age_j(INF_JAUN, INF_JAUN_AGE_DAYS),
    INF_JAUN_AGE_HRS  = first_positive_hours_j(INF_JAUN, INF_JAUN_AGE_HOURS_IPC),
    
    ##  Non-severe jaundice 
    INF_JAUN_NON_SEV_ANY      = as.integer(any(INF_JAUN_NON_SEV == 1, na.rm = TRUE)),
    INF_JAUN_NON_SEV_DATE     = first_positive_date_j(INF_JAUN_NON_SEV, INF_JAUN_DATE),
    INF_JAUN_NON_SEV_AGE_DAYS = first_positive_age_j(INF_JAUN_NON_SEV, INF_JAUN_AGE_DAYS),
    INF_JAUN_NON_SEV_AGE_HRS  = first_positive_hours_j(INF_JAUN_NON_SEV, INF_JAUN_AGE_HOURS_IPC),
    
    ##  Severe jaundice 
    INF_JAUN_SEV           = as.integer(any(INF_JAUN_SEV == 1, na.rm = TRUE)),
    INF_JAUN_SEV_DATE      = first_positive_date_j(INF_JAUN_SEV, INF_JAUN_DATE),
    INF_JAUN_SEV_AGE_DAYS  = first_positive_age_j(INF_JAUN_SEV, INF_JAUN_AGE_DAYS),
    INF_JAUN_SEV_AGE_HRS   = first_positive_hours_j(INF_JAUN_SEV, INF_JAUN_AGE_HOURS_IPC),
    
    ##  Severe within 24hr of onset 
    INF_JAUN_SEV_24HR = as.integer(any(
      INF_JAUN_SEV == 1 &
        (JAUND_ONSET == 1 |
           (!is.na(AGE_AT_VISIT_HOURS_USE) &
              AGE_AT_VISIT_HOURS_USE >= 0 &
              AGE_AT_VISIT_HOURS_USE < 24)),
      na.rm = TRUE
    )),
    
    ##  Severe >24hr after onset 
    INF_JAUN_SEV_GREATER_24HR = as.integer(any(
      INF_JAUN_SEV == 1 &
        (JAUND_ONSET == 2 |
           (is.na(JAUND_ONSET) &
              !is.na(AGE_AT_VISIT_HOURS_USE) &
              AGE_AT_VISIT_HOURS_USE >= 24)),
      na.rm = TRUE
    )),
    
    ##  Denominator 
    DENOM_JAUN = as.integer(any(DENOM_JAUN == 1, na.rm = TRUE)),
    
    .groups = "drop"
  )

## 10. TCB denominators 
# Built off the same TCB_WINDOW tag used in hyperbili_crit1/2/4,
# so the denominator and the outcome cannot disagree on window boundaries.
hyperbili_denom <- tcb_long %>%
  mutate(
    TCB_WINDOW = toupper(trimws(as.character(TCB_WINDOW))),
    TCB_VALUE  = suppressWarnings(as.numeric(as.character(TCB_VALUE)))
  ) %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  summarise(
    DENOM_HYPERBILI_24HR = as.integer(
      any(TCB_WINDOW == "24HR" & !is.na(TCB_VALUE))
    ),
    
    DENOM_HYPERBILI_5DAY = as.integer(
      any(TCB_WINDOW == "5DAY" & !is.na(TCB_VALUE))
    ),
    
    DENOM_HYPERBILI_14DAY = as.integer(
      any(TCB_WINDOW == "14DAY" & !is.na(TCB_VALUE))
    ),
    
    DENOM_HYPERBILI_ANY = as.integer(
      any(TCB_WINDOW %in% c("24HR", "5DAY", "14DAY") & !is.na(TCB_VALUE))
    ),
    
    .groups = "drop"
  )

## 11. Denominator audit 
cat("\nRAW TCB DENOMINATOR CHECK\n")
cat("ANY:", sum(hyperbili_denom$DENOM_HYPERBILI_ANY == 1), "\n")
cat("\n[3] Denominator totals (n = 1)\n")
cat(sprintf("    ANY   : %d\n", sum(hyperbili_denom$DENOM_HYPERBILI_ANY   == 1, na.rm = TRUE)))
cat(sprintf("    24HR  : %d\n", sum(hyperbili_denom$DENOM_HYPERBILI_24HR  == 1, na.rm = TRUE)))
cat(sprintf("    5DAY  : %d\n", sum(hyperbili_denom$DENOM_HYPERBILI_5DAY  == 1, na.rm = TRUE)))
cat(sprintf("    14DAY : %d\n", sum(hyperbili_denom$DENOM_HYPERBILI_14DAY == 1, na.rm = TRUE)))


tcb_age_summary <- tcb_long %>%
  filter(TCB_REPORTED == 1) %>%
  count(TCB_AGE_WINDOW, name = "N_TCB") %>%
  mutate(PERCENT = round(100 * N_TCB / sum(N_TCB), 1))

cat("\nWHERE ALL ACTUAL TCB MEASUREMENTS FALL BY AGE\n")
for (i in seq_len(nrow(tcb_age_summary))) {
  cat(tcb_age_summary$TCB_AGE_WINDOW[i], ":",
      tcb_age_summary$N_TCB[i], "TCBs (",
      tcb_age_summary$PERCENT[i], "%)\n", sep = " ")
}

## 12. Final infant-level dataset 
hyperbili_all_crit <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, DELIVERY_DATETIME,
         GESTAGEBIRTH_ANY, ADJUD_NEEDED, LIVEBIRTH) %>%
  left_join(hyperbili_crit1, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(hyperbili_crit2 %>% select(-GESTAGEBIRTH_ANY),
            by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(hyperbili_crit3, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(hyperbili_crit4, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(hyperbili_denom, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  mutate(across(
    c(DENOM_HYPERBILI_ANY, DENOM_HYPERBILI_24HR,
      DENOM_HYPERBILI_5DAY, DENOM_HYPERBILI_14DAY),
    ~ replace_na(.x, 0L)
  )) %>%
  mutate(across(c(
    INF_HYPERBILI_TCB15_24HR, INF_HYPERBILI_TCB15_5DAY, INF_HYPERBILI_TCB15_14DAY,
    INF_HYPERBILI_AAP_24HR, INF_HYPERBILI_AAP_5DAY, INF_HYPERBILI_AAP_14DAY,
    INF_HYPERBILI_NICE_24HR, INF_HYPERBILI_NICE_5DAY, INF_HYPERBILI_NICE_14DAY,
    INF_JAUN_NON_SEV_ANY, INF_JAUN_SEV_24HR, INF_JAUN_SEV_GREATER_24HR,
    DENOM_HYPERBILI_ANY, DENOM_HYPERBILI_24HR, DENOM_HYPERBILI_5DAY,
    DENOM_HYPERBILI_14DAY, DENOM_JAUN),
    ~ case_when(
      ADJUD_NEEDED == 1 ~ 55L,
      TRUE ~ as.integer(.x)
    )
  )) %>%
  select(-ADJUD_NEEDED) %>%
  mutate(
    across(
      c(INF_HYPERBILI_TCB15_ANY, INF_HYPERBILI_TCB15_24HR,
        INF_HYPERBILI_TCB15_5DAY, INF_HYPERBILI_TCB15_14DAY),
      ~ case_when(
        is.na(.x) & LIVEBIRTH != 1 ~ 77L,
        is.na(.x) & LIVEBIRTH == 1 ~ 55L,
        TRUE ~ as.integer(.x))
    ),
    across(
      c(INF_HYPERBILI_AAP_ANY, INF_HYPERBILI_AAP_24HR,
        INF_HYPERBILI_AAP_5DAY, INF_HYPERBILI_AAP_14DAY),
      ~ case_when(
        is.na(.x) & LIVEBIRTH != 1 ~ 77L,
        is.na(.x) & LIVEBIRTH == 1 & !is.na(GESTAGEBIRTH_ANY) & GESTAGEBIRTH_ANY < 35 ~ 77L,
        is.na(.x) & LIVEBIRTH == 1 ~ 55L,
        TRUE ~ as.integer(.x))
    ),
    across(
      c(INF_HYPERBILI_NICE_ANY, INF_HYPERBILI_NICE_24HR,
        INF_HYPERBILI_NICE_5DAY, INF_HYPERBILI_NICE_14DAY),
      ~ case_when(
        is.na(.x) & LIVEBIRTH != 1 ~ 77L,
        is.na(.x) & LIVEBIRTH == 1 ~ 55L,
        TRUE ~ as.integer(.x))
    )
  )

## 13. Final checks 
cat("\nFINAL TCB WORKFLOW CHECK\n")
cat("Actual TCB measurements:", sum(tcb_long$TCB_REPORTED == 1, na.rm = TRUE), "\n")
cat("Negative cleaned TCB values:", sum(tcb_long$TCB_VALUE < 0, na.rm = TRUE), "\n")
cat("TCBs with exact datetime age:",
    sum(tcb_long$TCB_REPORTED == 1 & tcb_long$AGE_SOURCE == "DATETIME", na.rm = TRUE), "\n")
cat("TCBs with date-only age:",
    sum(tcb_long$TCB_REPORTED == 1 & tcb_long$AGE_SOURCE == "DATE_ONLY", na.rm = TRUE), "\n")
cat("TCBs with missing age:",
    sum(tcb_long$TCB_REPORTED == 1 & tcb_long$AGE_SOURCE == "MISSING", na.rm = TRUE), "\n")

# Distribution of the per-observation flags — this is the key diagnostic
cat("\nPER-OBSERVATION FLAG DISTRIBUTIONS\n")
cat("INF_HYPERBILI_TCB15:\n"); print(table(tcb_long$INF_HYPERBILI_TCB15, useNA = "always"))
cat("INF_HYPERBILI_AAP:\n");   print(table(tcb_long$INF_HYPERBILI_AAP,   useNA = "always"))
cat("INF_HYPERBILI_NICE:\n");  print(table(tcb_long$INF_HYPERBILI_NICE,  useNA = "always"))

tcb_long_export <- tcb_long %>% filter (!is.na(TCB_VALUE) & TCB_REPORTED == 1)

## Export 
# Long datasets: every available visit/result.
write.csv(tcb_long_export, paste0(path_to_save, "INF_TCB_LONG.csv"), row.names = FALSE)
write.csv(hyperbili_all_crit, paste0(path_to_save, "hyperbili_all_crit.csv"), row.names = FALSE)
# write.csv(psbi_outcome, paste0(path_to_tnt, "INF_PSBI_LONG.csv"), row.names = FALSE)

#*****************************************************************************
## PSBI ----
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

## PSBI outcomes 

## Presence of any clinical signs or symptoms as defined by the WHO IMCI
## criteria, consistent with possible severe bacterial infection (PSBI),
## from delivery through 59 days of age.
##
## Forms needed: MNH11 (IPC), MNH13 (PNC), MNH20 (hospitalization)
##
## Variables used:
##   Has the baby had difficulty in feeding (feeding poorly or not at all)?
##     [POOR_FEED_CEOCCUR, POOR_FEED_CEOCCUR_MR]
##   Has the baby had convulsions?
##     [CONV_CEOCCUR, CONV_CEOCCUR_MR]
##   Has the baby had fast or difficult breathing?
##     [BREATH_VSORRES_1, BREATH_VSORRES_2, BREATH_CEOCCUR_MR]
##   Does the infant have severe chest in-drawing?
##     [CHESTINDRAW_CEOCCUR, CHEST_CEOCCUR]
##   Has the baby had a fever?
##     [TEMP_VSORRES_1, TEMP_VSORRES_2, FEVER_CEOCCUR_MR]
##   Record infant temperature at time of visit. Is the baby cold?
##     [TEMP_VSORRES, HYPO_CEOCCUR_MR]

##  Helper Functions 
## Return the earliest age / date / hour among rows where the flag is 1.
## If there are no positive rows, return NA.

first_positive_age <- function(flag, age) {
  age <- suppressWarnings(as.numeric(age))
  z <- age[!is.na(flag) & flag == 1 & !is.na(age)]
  if (length(z) == 0) NA_real_ else min(z)
}

first_positive_date <- function(flag, date) {
  date <- suppressWarnings(as.Date(date))
  z <- date[!is.na(flag) & flag == 1 & !is.na(date)]
  if (length(z) == 0) as.Date(NA) else suppressWarnings(min(z, na.rm = TRUE))
}

first_positive_hours <- function(flag, hours) {
  hours <- suppressWarnings(as.numeric(hours))
  z <- hours[!is.na(flag) & flag == 1 & !is.na(hours)]
  if (length(z) == 0) NA_real_ else min(z)
}


## Long dataset creation 

## One row per infant per visit. PSBI is evaluated visit by visit, using
## the raw clinical signs from whichever form the visit came from.

##  IPC visits 
## IPC (immediate postnatal care) visits come from MNH11.

psbi_ipc_long <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, DOB, BIRTH_OUTCOME, LIVEBIRTH) %>%
  left_join(mnh11_constructed, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  mutate(
    DOB          = clean_date(DOB),
    VISIT_DATE   = clean_date(M11_VISIT_OBSSTDAT),
    AGE_AT_VISIT = as.numeric(VISIT_DATE - DOB),
    ## Flag visits where the date is missing or before birth.
    INVALID_DATE_INFO = as.integer(AGE_AT_VISIT < 0 | is.na(AGE_AT_VISIT)),
    VISIT_SOURCE = "IPC",
    VISIT_WINDOW = "IPC",
    
    ## In India-SAS, 77 is the default "not done" code; treat it as missing.
    M11_BREATH_VSORRES_1 = if_else(SITE == "India-SAS" & M11_BREATH_VSORRES_1 == 77,
                                   NA_real_, as.numeric(M11_BREATH_VSORRES_1)),
    M11_BREATH_VSORRES_2 = if_else(SITE == "India-SAS" & M11_BREATH_VSORRES_2 == 77,
                                   NA_real_, as.numeric(M11_BREATH_VSORRES_2)),
    
    ## PSBI = 1 if ANY of the WHO IMCI danger signs is present:
    ##   poor feeding, convulsions, fast breathing (>=60 bpm),
    ##   severe chest in-drawing, fever (>=38 C), or low temp (<35.5 C).
    INF_PSBI = case_when(
      M11_POOR_FEED_CEOCCUR == 1 |
        M11_CONV_CEOCCUR == 1 |
        M11_BREATH_VSORRES_1 >= 60 |
        M11_BREATH_VSORRES_2 >= 60 |
        M11_CHESTINDRAW_CEOCCUR == 1 |
        M11_TEMP_VSORRES   >= 38 |
        M11_TEMP_VSORRES_2 >= 38 |
        (M11_TEMP_VSORRES   > 0 & M11_TEMP_VSORRES   < 35.5) |
        (M11_TEMP_VSORRES_2 > 0 & M11_TEMP_VSORRES_2 < 35.5) ~ 1L,
      TRUE ~ 0L
    )
  )

##  PNC visits 
## PNC (postnatal care) visits come from MNH13. Windows are assigned by age.

psbi_pnc_long <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, DOB, BIRTH_OUTCOME, LIVEBIRTH) %>%
  right_join(mnh13, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  mutate(
    DOB          = clean_date(DOB),
    VISIT_DATE   = clean_date(M13_VISIT_OBSSTDAT),
    AGE_AT_VISIT = as.numeric(VISIT_DATE - DOB),
    INVALID_DATE_INFO = as.integer(AGE_AT_VISIT < 0 | is.na(AGE_AT_VISIT)),
    VISIT_SOURCE = "PNC",
    ## Classify the visit into a scheduled PNC window or "INBETWEEN" based on age.
    VISIT_WINDOW = assign_pnc_window(AGE_AT_VISIT),
    
    ## In Ghana, 77 is the default "not done" code; treat it as missing.
    M13_BREATH_VSORRES_1 = if_else(SITE == "Ghana" & M13_BREATH_VSORRES_1 == 77,
                                   NA_real_, as.numeric(M13_BREATH_VSORRES_1)),
    M13_BREATH_VSORRES_2 = if_else(SITE == "Ghana" & M13_BREATH_VSORRES_2 == 77,
                                   NA_real_, as.numeric(M13_BREATH_VSORRES_2)),
    M13_TEMP_VSORRES_1   = if_else(SITE == "Ghana" & M13_TEMP_VSORRES_1 == 77,
                                   NA_real_, as.numeric(M13_TEMP_VSORRES_1)),
    M13_TEMP_VSORRES_2   = if_else(SITE == "Ghana" & M13_TEMP_VSORRES_2 == 77,
                                   NA_real_, as.numeric(M13_TEMP_VSORRES_2)),
    
    ## PSBI = 1 if ANY of the WHO IMCI danger signs is present.
    INF_PSBI = case_when(
      M13_POOR_FEED_CEOCCUR == 1 |
        M13_CONV_CEOCCUR == 1 |
        M13_BREATH_VSORRES_1 >= 60 |
        M13_BREATH_VSORRES_2 >= 60 |
        M13_CHEST_CEOCCUR == 1 |
        M13_TEMP_VSORRES_1 >= 38 |
        M13_TEMP_VSORRES_2 >= 38 |
        (M13_TEMP_VSORRES_1 > 0 & M13_TEMP_VSORRES_1 < 35.5) |
        (M13_TEMP_VSORRES_2 > 0 & M13_TEMP_VSORRES_2 < 35.5) ~ 1L,
      TRUE ~ 0L
    ),
    
    ## Flag cases where a high or low temperature was recorded on the first
    ## measurement but the confirmatory second measurement is missing.
    MISSING_TEMP_PNC_HIGH = as.integer(M13_TEMP_VSORRES_1 >= 38 & M13_TEMP_VSORRES_2 < 0),
    MISSING_TEMP_PNC_LOW  = as.integer(M13_TEMP_VSORRES_1 > 0 &
                                         M13_TEMP_VSORRES_1 < 35.5 &
                                         M13_TEMP_VSORRES_2 < 0),
    MISSING_TEMP_PNC      = as.integer(MISSING_TEMP_PNC_HIGH == 1 | MISSING_TEMP_PNC_LOW == 1)
  )

## which signs prevail more?
psbi_ipc_long %>%
  filter(INF_PSBI == 1) %>%
  summarise(
    Total_PSBI    = n(),
    Poor_feeding  = sum(M11_POOR_FEED_CEOCCUR == 1, na.rm = TRUE),
    Convulsions   = sum(M11_CONV_CEOCCUR == 1, na.rm = TRUE),
    Fast_breath   = sum(M11_BREATH_VSORRES_1 >= 60 | M11_BREATH_VSORRES_2 >= 60, na.rm = TRUE),
    Chest_indraw  = sum(M11_CHESTINDRAW_CEOCCUR == 1, na.rm = TRUE),
    Fever         = sum(M11_TEMP_VSORRES >= 38 | M11_TEMP_VSORRES_2 >= 38, na.rm = TRUE),
    Low_temp      = sum((M11_TEMP_VSORRES > 0 & M11_TEMP_VSORRES < 35.5) |
                          (M11_TEMP_VSORRES_2 > 0 & M11_TEMP_VSORRES_2 < 35.5), na.rm = TRUE)
  )

psbi_pnc_long %>%
  filter(INF_PSBI == 1) %>%
  summarise(
    Total_PSBI    = n(),
    Poor_feeding  = sum(M13_POOR_FEED_CEOCCUR == 1, na.rm = TRUE),
    Convulsions   = sum(M13_CONV_CEOCCUR == 1, na.rm = TRUE),
    Fast_breath   = sum(M13_BREATH_VSORRES_1 >= 60 | M13_BREATH_VSORRES_2 >= 60, na.rm = TRUE),
    Chest_indraw  = sum(M13_CHEST_CEOCCUR == 1, na.rm = TRUE),
    Fever         = sum(M13_TEMP_VSORRES_1 >= 38 | M13_TEMP_VSORRES_2 >= 38, na.rm = TRUE),
    Low_temp      = sum((M13_TEMP_VSORRES_1 > 0 & M13_TEMP_VSORRES_1 < 35.5) |
                          (M13_TEMP_VSORRES_2 > 0 & M13_TEMP_VSORRES_2 < 35.5), na.rm = TRUE)
  )

##  Hospital visits 
## Hospitalization visits come from MNH20. Treated as their own source.

psbi_hospital_long <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, DOB, BIRTH_OUTCOME, LIVEBIRTH) %>%
  right_join(mnh20, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  mutate(
    DOB          = clean_date(DOB),
    VISIT_DATE   = clean_date(M20_OBSSTDAT),
    AGE_AT_VISIT = as.numeric(VISIT_DATE - DOB),
    INVALID_DATE_INFO = as.integer(AGE_AT_VISIT < 0 | is.na(AGE_AT_VISIT)),
    VISIT_SOURCE = "HOSPITAL",
    VISIT_WINDOW = "HOSPITAL",
    
    ## PSBI = 1 if ANY of the WHO IMCI danger signs is present.
    INF_PSBI = case_when(
      M20_POOR_FEED_CEOCCUR == 1 |
        M20_CONV_CEOCCUR == 1 |
        M20_RR_VSORRES >= 60 |
        M20_MAX_RR_VSORRES >= 60 |
        M20_CHEST_CEOCCUR == 1 |
        M20_TEMP_VSORRES >= 38 |
        M20_MAX_TEMP_VSORRES >= 38 |
        (M20_TEMP_VSORRES     > 0 & M20_TEMP_VSORRES     < 35.5) |
        (M20_LOW_TEMP_VSORRES > 0 & M20_LOW_TEMP_VSORRES < 35.5) ~ 1L,
      TRUE ~ 0L
    )
  )

##  Bind and filter to birth through day 59 
## PSBI is only defined for live births between 0 and 59 days of age.

psbi_long <- bind_rows(psbi_ipc_long, psbi_pnc_long, psbi_hospital_long) %>%
  filter(LIVEBIRTH == 1, !is.na(AGE_AT_VISIT), AGE_AT_VISIT >= 0, AGE_AT_VISIT <= 59) %>%
  arrange(SITE, MOMID, PREGID, INFANTID, AGE_AT_VISIT, VISIT_DATE) %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  ## Number visits per infant in chronological order.
  mutate(VISIT_SEQ = row_number()) %>%
  ungroup()


## Adds two kinds of per-visit indicator:
##   HAS_VISIT_*    - does this infant have at least one visit in this window?
##   INF_PSBI_*_FIRST - is this row the FIRST positive visit in this window?
## The *_FIRST markers use cumsum() so only the earliest positive row is
## flagged, which prevents an infant from being counted twice.

psbi_outcome <- psbi_long %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  arrange(AGE_AT_VISIT, VISIT_DATE, .by_group = TRUE) %>%
  mutate(
    ##  Visit-presence indicators 
    ## These let us later distinguish "no visit" (unknown) from
    ## "visited but no PSBI" (confirmed zero).
    HAS_VISIT_ANY      = as.integer(any(!is.na(VISIT_DATE))),
    HAS_VISIT_IPC      = as.integer(any(VISIT_SOURCE == "IPC")),
    HAS_VISIT_PNC0     = as.integer(any(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC0")),
    HAS_VISIT_PNC1     = as.integer(any(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC1")),
    HAS_VISIT_PNC4     = as.integer(any(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC4")),
    HAS_VISIT_PNC6     = as.integer(any(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC6")),
    HAS_VISIT_UNSCHED  = as.integer(any(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "INBETWEEN")),
    HAS_VISIT_HOSPITAL = as.integer(any(VISIT_SOURCE == "HOSPITAL")),
    
    ##  First-positive flags 
    ## Mark only the first visit where PSBI = 1 for each category, so an
    ## infant with multiple PSBI visits is counted once.
    INF_PSBI_FIRST = as.integer(INF_PSBI == 1 & cumsum(INF_PSBI == 1) == 1),
    INF_PSBI_IPC_FIRST = as.integer(
      VISIT_SOURCE == "IPC" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "IPC" & INF_PSBI == 1) == 1
    ),
    INF_PSBI_PNC0_FIRST = as.integer(
      VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC0" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC0" & INF_PSBI == 1) == 1
    ),
    INF_PSBI_PNC1_FIRST = as.integer(
      VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC1" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC1" & INF_PSBI == 1) == 1
    ),
    INF_PSBI_PNC4_FIRST = as.integer(
      VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC4" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC4" & INF_PSBI == 1) == 1
    ),
    INF_PSBI_PNC6_FIRST = as.integer(
      VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC6" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC6" & INF_PSBI == 1) == 1
    ),
    INF_PSBI_UNSCHED_FIRST = as.integer(
      VISIT_SOURCE == "PNC" & VISIT_WINDOW == "INBETWEEN" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "INBETWEEN" & INF_PSBI == 1) == 1
    ),
    INF_PSBI_HOSPITAL_FIRST = as.integer(
      VISIT_SOURCE == "HOSPITAL" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "HOSPITAL" & INF_PSBI == 1) == 1
    ),
    
    ##  Age-based adjudication for IPC 
    ## Flag IPC-positive visits where the infant was older than 3 days.
    ## These get recoded to 55 later.
    INF_PSBI_IPC_AGE_GT3 = as.integer(
      INF_PSBI_IPC_FIRST == 1 & !is.na(AGE_AT_VISIT) & AGE_AT_VISIT > 3
    ),
    
    ##  Denominator 
    ## Every live birth is in the denominator.
    INF_PSBI_DENOM = as.integer(BIRTH_OUTCOME == 1)
  ) %>%
  ungroup()


## Collapse to one row per infant. For each PSBI category we keep:
##   - the date and age of the FIRST positive visit
##   - whether any positive visit occurred
##   - whether any visit occurred (for the 0 vs 55 decision)

psbi_outcome_wide <- psbi_outcome %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  summarise(
    DOB = first(DOB),
    
    ##  First-positive dates and ages 
    INF_PSBI_DATE     = first_positive_date(INF_PSBI_FIRST, VISIT_DATE),
    INF_PSBI_AGE_DAYS = first_positive_age(INF_PSBI_FIRST, AGE_AT_VISIT),
    ## Hours are stored as hours-within-day (mod 24).
    INF_PSBI_AGE_HRS  = first_positive_hours(
      INF_PSBI_FIRST,
      as.numeric(difftime(VISIT_DATE, DOB, units = "hours")) %% 24
    ),
    INF_PSBI_IPC_DATE          = first_positive_date(INF_PSBI_IPC_FIRST, VISIT_DATE),
    INF_PSBI_IPC_AGE_DAYS      = first_positive_age(INF_PSBI_IPC_FIRST, AGE_AT_VISIT),
    INF_PSBI_PNC0_DATE         = first_positive_date(INF_PSBI_PNC0_FIRST, VISIT_DATE),
    INF_PSBI_PNC0_AGE_DAYS     = first_positive_age(INF_PSBI_PNC0_FIRST, AGE_AT_VISIT),
    INF_PSBI_PNC1_DATE         = first_positive_date(INF_PSBI_PNC1_FIRST, VISIT_DATE),
    INF_PSBI_PNC1_AGE_DAYS     = first_positive_age(INF_PSBI_PNC1_FIRST, AGE_AT_VISIT),
    INF_PSBI_PNC4_DATE         = first_positive_date(INF_PSBI_PNC4_FIRST, VISIT_DATE),
    INF_PSBI_PNC4_AGE_DAYS     = first_positive_age(INF_PSBI_PNC4_FIRST, AGE_AT_VISIT),
    INF_PSBI_PNC6_DATE         = first_positive_date(INF_PSBI_PNC6_FIRST, VISIT_DATE),
    INF_PSBI_PNC6_AGE_DAYS     = first_positive_age(INF_PSBI_PNC6_FIRST, AGE_AT_VISIT),
    INF_PSBI_UNSCHED_DATE      = first_positive_date(INF_PSBI_UNSCHED_FIRST, VISIT_DATE),
    INF_PSBI_UNSCHED_AGE_DAYS  = first_positive_age(INF_PSBI_UNSCHED_FIRST, AGE_AT_VISIT),
    INF_PSBI_HOSPITAL_DATE     = first_positive_date(INF_PSBI_HOSPITAL_FIRST, VISIT_DATE),
    INF_PSBI_HOSPITAL_AGE_DAYS = first_positive_age(INF_PSBI_HOSPITAL_FIRST, AGE_AT_VISIT),
    
    ##  Visit-presence rollups 
    HAS_VISIT_ANY      = as.integer(any(HAS_VISIT_ANY      == 1, na.rm = TRUE)),
    HAS_VISIT_IPC      = as.integer(any(HAS_VISIT_IPC      == 1, na.rm = TRUE)),
    HAS_VISIT_PNC0     = as.integer(any(HAS_VISIT_PNC0     == 1, na.rm = TRUE)),
    HAS_VISIT_PNC1     = as.integer(any(HAS_VISIT_PNC1     == 1, na.rm = TRUE)),
    HAS_VISIT_PNC4     = as.integer(any(HAS_VISIT_PNC4     == 1, na.rm = TRUE)),
    HAS_VISIT_PNC6     = as.integer(any(HAS_VISIT_PNC6     == 1, na.rm = TRUE)),
    HAS_VISIT_UNSCHED  = as.integer(any(HAS_VISIT_UNSCHED  == 1, na.rm = TRUE)),
    HAS_VISIT_HOSPITAL = as.integer(any(HAS_VISIT_HOSPITAL == 1, na.rm = TRUE)),
    
    ##  Positive rollups 
    INF_PSBI_ANY      = as.integer(any(INF_PSBI_FIRST      == 1, na.rm = TRUE)),
    INF_PSBI_IPC      = as.integer(any(INF_PSBI_IPC_FIRST  == 1, na.rm = TRUE)),
    INF_PSBI_PNC0     = as.integer(any(INF_PSBI_PNC0_FIRST == 1, na.rm = TRUE)),
    INF_PSBI_PNC1     = as.integer(any(INF_PSBI_PNC1_FIRST == 1, na.rm = TRUE)),
    INF_PSBI_PNC4     = as.integer(any(INF_PSBI_PNC4_FIRST == 1, na.rm = TRUE)),
    INF_PSBI_PNC6     = as.integer(any(INF_PSBI_PNC6_FIRST == 1, na.rm = TRUE)),
    INF_PSBI_UNSCHED  = as.integer(any(INF_PSBI_UNSCHED_FIRST == 1, na.rm = TRUE)),
    INF_PSBI_HOSPITAL = as.integer(any(INF_PSBI_HOSPITAL_FIRST == 1, na.rm = TRUE)),
    
    ##  Age>3 adjudication rollup 
    INF_PSBI_IPC_AGE_GT3 = as.integer(any(INF_PSBI_IPC_AGE_GT3 == 1, na.rm = TRUE)),
    
    ##  Denominator 
    INF_PSBI_DENOM = as.integer(any(INF_PSBI_DENOM == 1, na.rm = TRUE)),
    
    .groups = "drop"
  ) %>%
  ## Replace Inf (from max/min on empty groups) with NA.
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)))

##  Recode to 0/1/55/77 
## For each source-specific flag:
##   1  = had a PSBI-positive visit
##   0  = had a visit in that window, but no PSBI
##   55 = no visit in that window (unknown)
##   77 = no unscheduled or hospital visit (not applicable)
recode_psbi <- function(x, has_visit, no_visit_value = 55L) {
  case_when(x == 1 ~ 1L, has_visit == 1 ~ 0L, TRUE ~ no_visit_value)
}

psbi_outcome_wide <- psbi_outcome_wide %>%
  mutate(
    INF_PSBI_IPC      = recode_psbi(INF_PSBI_IPC,      HAS_VISIT_IPC),
    INF_PSBI_PNC0     = recode_psbi(INF_PSBI_PNC0,     HAS_VISIT_PNC0),
    INF_PSBI_PNC1     = recode_psbi(INF_PSBI_PNC1,     HAS_VISIT_PNC1),
    INF_PSBI_PNC4     = recode_psbi(INF_PSBI_PNC4,     HAS_VISIT_PNC4),
    INF_PSBI_PNC6     = recode_psbi(INF_PSBI_PNC6,     HAS_VISIT_PNC6),
    INF_PSBI_UNSCHED  = recode_psbi(INF_PSBI_UNSCHED,  HAS_VISIT_UNSCHED,  no_visit_value = 77L),
    INF_PSBI_HOSPITAL = recode_psbi(INF_PSBI_HOSPITAL, HAS_VISIT_HOSPITAL, no_visit_value = 77L),
    INF_PSBI_ANY      = recode_psbi(INF_PSBI_ANY,      HAS_VISIT_ANY),
    ## IPC-positive visits at age > 3 days get recoded to 55.
    INF_PSBI_IPC      = if_else(INF_PSBI_IPC == 1 & INF_PSBI_IPC_AGE_GT3 == 1, 55L, INF_PSBI_IPC),
    INF_PSBI_DENOM    = as.integer(INF_PSBI_DENOM)
  )

##  Ihme subset 
## Collapse to one row per infant, keeping the FIRST PSBI-positive visit.
## Recode into the IHME-style categories (1/0/55/77/99).

psbi_outcome_sub <- psbi_outcome %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  arrange(AGE_AT_VISIT, VISIT_DATE, .by_group = TRUE) %>%
  summarise(
    LIVEBIRTH         = first(LIVEBIRTH),
    ANY_PSBI          = as.integer(any(INF_PSBI == 1, na.rm = TRUE)),
    FIRST_PSBI_AGE    = first_positive_age(INF_PSBI, AGE_AT_VISIT),
    FIRST_PSBI_DATE   = first_positive_date(INF_PSBI, VISIT_DATE),
    INVALID_DATE_INFO = as.integer(any(INF_PSBI == 1 & INVALID_DATE_INFO == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  right_join(
    inf_baseline %>% select(SITE, MOMID, PREGID, INFANTID, LIVEBIRTH),
    by = c("SITE","MOMID","PREGID","INFANTID"),
    suffix = c("", ".base")
  ) %>%
  mutate(LIVEBIRTH = coalesce(LIVEBIRTH, LIVEBIRTH.base)) %>%
  select(-LIVEBIRTH.base) %>%
  mutate(
    ## PSBI before 28 days.
    PSBI_LESS28 = case_when(
      LIVEBIRTH %in% c(0,55) ~ 99L,
      ANY_PSBI == 1 & INVALID_DATE_INFO == 1 ~ 55L,
      ANY_PSBI == 1 & FIRST_PSBI_AGE >= 0 & FIRST_PSBI_AGE < 28 ~ 1L,
      ANY_PSBI == 1 & FIRST_PSBI_AGE >= 28 ~ 77L,
      ANY_PSBI == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    ## PSBI between 28 and 59 days.
    PSBI_GREATER28 = case_when(
      LIVEBIRTH %in% c(0,55) ~ 99L,
      ANY_PSBI == 1 & INVALID_DATE_INFO == 1 ~ 55L,
      ANY_PSBI == 1 & FIRST_PSBI_AGE >= 28 & FIRST_PSBI_AGE <= 59 ~ 1L,
      ANY_PSBI == 1 & FIRST_PSBI_AGE < 28 ~ 77L,
      ANY_PSBI == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    ## PSBI before 59 days (union of the two windows above).
    PSBI_LESS59 = case_when(
      LIVEBIRTH %in% c(0,55) ~ 99L,
      PSBI_LESS28 == 1 | PSBI_GREATER28 == 1 ~ 1L,
      PSBI_LESS28 == 55 | PSBI_GREATER28 == 55 ~ 55L,
      ANY_PSBI == 0 ~ 0L,
      TRUE ~ 77L
    )
  )

psbi_long_export <- psbi_long %>%
  filter(INF_PSBI == 1) %>%
  arrange(SITE, MOMID, PREGID, INFANTID, VISIT_DATE) %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  mutate(VISIT_SEQ = row_number()) %>%
  ungroup() %>%
  select(
    SITE, MOMID, PREGID, INFANTID,
    VISIT_DATE, AGE_AT_VISIT, VISIT_SEQ,
    # --- PSBI variables ---
    INF_PSBI, VISIT_WINDOW,
    # --- symptom variables ---
    # Clinical signs and symptoms
    contains("SYMPTOM"), contains("SIGN"),
    contains("POOR_FEED"), contains("CONV"),
    contains("BREATH"), contains("RR"),
    contains("CHEST"), contains("COUGH"),
    contains("FEVER"), contains("TEMP"),
    contains("HYPO"))


##  Exports 
write.csv(psbi_long_export, file.path(path_to_save, "INF_PSBI_LONG.csv"),       row.names = FALSE)
# write.csv(psbi_long_export, file.path(path_to_tnt,  "INF_PSBI_LONG.csv"),       row.names = FALSE)
write.csv(psbi_outcome_wide,file.path(path_to_save, "INF_PSBI_WIDE.csv"),       row.names = FALSE)
write.csv(psbi_outcome_sub, file.path(path_to_save, "INF_PSBI_IHME.csv"),       row.names = FALSE)

#*****************************************************************************
## Neonatal Sepsis ----
#* Inflammatory response and organ dysfunction following presence of:
#*  a severe infection from delivery to 28 days as suspected (by a clinician) or proven (with culture).

# Forms needed: 
# MNH11, MNH13, MNH20
## Checkboxes: 
# MNH11, MNH13, MNH20
## Blood culture:
# MNH11, MNH13, MNH20
#*****************************************************************************

mnh11_subset <- mnh11 %>% select(SITE, MOMID, PREGID, INFANTID,
                                 M11_VISIT_OBSSTDAT, M11_VISIT_OBSSTTIM, M11_INF_VISIT_72HR_MNH11,
                                 M11_INF_VISIT_MNH11, M11_INFANT_MHTERM_10,
                                 M11_INFANT_SPFY_MHTERM, M11_CULTURE_LBPERF, M11_CULTURE_LBTSTDAT,
                                 M11_CULTURE_LBORRES)

mnh13_subset <- mnh13 %>% select(SITE, MOMID, PREGID, INFANTID,
                                 M13_VISIT_OBSSTDAT, M13_VISIT_OBSLOC, M13_TYPE_VISIT,
                                 M13_INF_VISIT_MNH13, M13_INFANT_MHTERM_10, 
                                 M13_INFANT_SPFY_MHTERM, M13_BLD_CULT_LBPERF,
                                 M13_BLD_CULT_LBTSTDAT, M13_BLD_CULT_LBORRES
)


mnh20_subset <- mnh20 %>% select(SITE, MOMID, PREGID, INFANTID,
                                 M20_OBSSTDAT, M20_VISDAT_YN, M20_UNPLANNED_VISDAT, M20_EST_UNPLANNED_VISDAT,
                                 M20_INFECTION_MHTERM_1, M20_INFECTION_SPFY_MHTERM, 
                                 M20_BLD_CULT_LBPERF, M20_BLD_CULT_LBDAT, M20_BLD_CULT_LBORRES
)

## test to see if there are any instances where an infant has more than one hospitalization
test <- mnh20_subset %>% group_by(INFANTID) %>% mutate(n=n()) %>% filter(n>1)
dim(test)

## Merge together 
## first need to pull any instance in mnh13
mnh13_subset_pos <- mnh13_subset %>% 
  filter(M13_INFANT_MHTERM_10==1 | M13_BLD_CULT_LBORRES==1) %>% ## where the checkbox indicated sepsis OR had a positive culture
  group_by(INFANTID) %>% 
  ## if multiple, take the first report
  arrange(-desc(M13_VISIT_OBSSTDAT)) %>%
  slice(1) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  select(-n) %>%
  ungroup() 

inf_outcomes_merge <- inf_baseline %>% 
  select(SITE, MOMID, PREGID, INFANTID) %>% 
  left_join(mnh11_subset, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  left_join(mnh13_subset_pos, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  left_join(mnh20_subset, by = c("SITE", "MOMID", "PREGID", "INFANTID"))

## Start constructing 
inf_outcomes_sepsis <- inf_outcomes_merge %>% 
  mutate(SEPSIS_CHECK = case_when(M11_INFANT_MHTERM_10==1 | M13_INFANT_MHTERM_10==1 | M20_INFECTION_MHTERM_1==1 ~ 1,
                                  TRUE ~ 0)) %>% 
  # date of sepsis checkbox (pull the earliest instance)
  mutate(SEPSIS_CHECK_DATE_M11 = case_when(M11_INFANT_MHTERM_10==1 ~ ymd(M11_VISIT_OBSSTDAT),
                                           TRUE ~ NA),
         
         SEPSIS_CHECK_DATE_M13 = case_when(M13_INFANT_MHTERM_10==1 ~ ymd(M13_VISIT_OBSSTDAT),
                                           TRUE ~ NA),
         SEPSIS_CHECK_DATE_M20 = case_when(M20_INFECTION_MHTERM_1==1 ~ ymd(M20_OBSSTDAT),
                                           TRUE ~ NA)
  ) %>%
  ## for each row/positive instance, pull the earlies sepsis check date 
  rowwise() %>% 
  mutate(SEPSIS_CHECK_DATE = pmin(SEPSIS_CHECK_DATE_M11, SEPSIS_CHECK_DATE_M13, SEPSIS_CHECK_DATE_M20, na.rm = TRUE)) %>% 
  ## Generate indicator variable if any culture is positive
  mutate(POSITIVE_CULTURE = case_when(M11_CULTURE_LBORRES==1 | M13_BLD_CULT_LBORRES==1 | M20_BLD_CULT_LBORRES==1 ~ 1, 
                                      TRUE ~ 0)) %>%
  # date of culture confirmation (pull the earliest instance)
  mutate(POSITIVE_CULTURE_DATE_M11 = case_when(M11_CULTURE_LBORRES==1 ~ ymd(M11_VISIT_OBSSTDAT),
                                               TRUE ~ NA),
         
         POSITIVE_CULTURE_DATE_M13 = case_when(M13_BLD_CULT_LBORRES==1 ~ ymd(M13_VISIT_OBSSTDAT),
                                               TRUE ~ NA),
         POSITIVE_CULTURE_DATE_M20 = case_when(M20_BLD_CULT_LBORRES==1 ~ ymd(M20_OBSSTDAT),
                                               TRUE ~ NA)
  ) %>% 
  rowwise() %>% 
  ## for each row/positive instance, pull the earliest culture confirmation date 
  mutate(POSITIVE_CULTURE_DATE = pmin(POSITIVE_CULTURE_DATE_M11, POSITIVE_CULTURE_DATE_M13, POSITIVE_CULTURE_DATE_M20, na.rm = TRUE)) %>% 
  mutate(SEPSIS_CULTURE_CONFIRMED = case_when(M11_INFANT_MHTERM_10 ==1 & M11_CULTURE_LBORRES ==1 ~ 1,   ## BOTH checkbox and culture confirmed @ the same visit
                                              M13_INFANT_MHTERM_10==1 & M13_BLD_CULT_LBORRES==1 ~ 2,    ## BOTH checkbox and culture confirmed @ the same visit
                                              M20_INFECTION_MHTERM_1==1 & M20_BLD_CULT_LBORRES==1 ~ 3,  ## BOTH checkbox and culture confirmed @ the same visit
                                              M11_INFANT_MHTERM_10 ==1 & M11_CULTURE_LBORRES !=1 ~ 15,  ## ONLY checkbox and no culture confirmed @ the same visit
                                              M13_INFANT_MHTERM_10 ==1 & M13_BLD_CULT_LBORRES!=1 ~ 25,  ## ONLY checkbox and no culture confirmed @ the same visit
                                              M20_INFECTION_MHTERM_1==1 & M20_BLD_CULT_LBORRES!=1 ~ 35, ## ONLY checkbox and no culture confirmed @ the same visit
                                              SEPSIS_CHECK !=1 & POSITIVE_CULTURE !=1 ~ 0, ## no sepsis or positive culture (we are not interested in these)
                                              TRUE ~ 99)) %>% 
  relocate(c("SEPSIS_CHECK_DATE", "POSITIVE_CULTURE_DATE", "M20_EST_UNPLANNED_VISDAT"), .after = "INFANTID") 


## merge in psbi data 
neo_sepsis_subset <- inf_outcomes_sepsis %>% 
  ## only want subset of positives 
  filter(SEPSIS_CHECK==1 | POSITIVE_CULTURE==1)  %>% 
  left_join(psbi_outcome_wide, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  relocate(c("DOB", "INF_PSBI_DATE"), .after = "POSITIVE_CULTURE_DATE") %>% 
  mutate(PSBI_DX_DATE = ymd(INF_PSBI_DATE)) %>% 
  rowwise() %>% 
  mutate(PSBI_DX_DATE = case_when(INF_PSBI_IPC==1 | INF_PSBI_PNC0==1 | INF_PSBI_PNC1==1 | 
                                    INF_PSBI_PNC4== 1|INF_PSBI_PNC6==1 |INF_PSBI_UNSCHED==1 |
                                    INF_PSBI_HOSPITAL==1 ~ PSBI_DX_DATE, TRUE ~ NA
  )) %>% 
  # generate new variable if PSBI_DX_DATE is close to the checkbox date (using 2 weeks as indicator)
  mutate(PSBI_DX_DATE_CHECKBOX = case_when(abs(ymd(SEPSIS_CHECK_DATE)-PSBI_DX_DATE) <= 14 ~ 1, TRUE ~ 0)) %>% 
  # generate new variable if PSBI_DX_DATE is close to the culture date 
  mutate(PSBI_DX_DATE_CULTURE= case_when(abs(ymd(POSITIVE_CULTURE_DATE)-PSBI_DX_DATE) <= 14 ~ 1, TRUE ~ 0))  %>% 
  select(SITE, MOMID, PREGID, INFANTID, DOB, SEPSIS_CHECK,POSITIVE_CULTURE,
         SEPSIS_CHECK_DATE, POSITIVE_CULTURE_DATE, PSBI_DX_DATE, 
         PSBI_DX_DATE_CHECKBOX, PSBI_DX_DATE_CULTURE,
         M11_INFANT_MHTERM_10, M13_INFANT_MHTERM_10, M20_INFECTION_MHTERM_1, M11_CULTURE_LBORRES, M13_BLD_CULT_LBORRES, M20_BLD_CULT_LBORRES,
         contains("INF_PSBI")
  )

## Variables needed for tables 
# N sepsis cases (checkbox or culture confirmed) NEO_SEPSIS ==1
# N sepsis cases with confirmed culture NEO_SEPSIS_CULTURE ==1 
# N sepsis cases with reported PSBI NEO_SEPSIS_PSBI ==1

neo_sepsis <- neo_sepsis_subset %>% 
  right_join(inf_baseline %>% select(SITE, MOMID, PREGID, INFANTID, LIVEBIRTH, ADJUD_NEEDED), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  # generate age at sepsis dx
  mutate(SEPSIS_DATE = pmin(SEPSIS_CHECK_DATE, POSITIVE_CULTURE_DATE, na.rm = TRUE )) %>% 
  mutate(NEO_SEPSIS_AGE =  as.numeric(ymd(SEPSIS_DATE)- ymd(DOB))) %>% 
  # generate indicator variable if age at sepsis dx is >28 days 
  mutate(NEO_SEPSIS_AGE_GREATER28 = case_when(NEO_SEPSIS_AGE >=28 ~ 1, 
                                              NEO_SEPSIS_AGE >= 0 & NEO_SEPSIS_AGE<28 ~ 0, 
                                              TRUE ~ 55)) %>% 
  # generate neonatal sepsis variable 
  mutate(NEO_SEPSIS = case_when(LIVEBIRTH ==1 & NEO_SEPSIS_AGE >= 0 & NEO_SEPSIS_AGE<28 & (SEPSIS_CHECK==1 | POSITIVE_CULTURE==1) ~ 1, # if livebirth and either checkbox or culture reported, ==1
                                LIVEBIRTH ==1 & NEO_SEPSIS_AGE >=28 & (SEPSIS_CHECK==1 | POSITIVE_CULTURE==1) ~ 0, # if live birth with sepsis, but age at sepsis is >= 28, then no neonatal sepsis
                                LIVEBIRTH ==1 & NEO_SEPSIS_AGE >= 0 & NEO_SEPSIS_AGE<28 & ((SEPSIS_CHECK==0 & POSITIVE_CULTURE==0) | is.na(SEPSIS_CHECK) & is.na(POSITIVE_CULTURE)) ~ 0, # if livebirth and checkbox or culture negative, ==0
                                LIVEBIRTH == 0  | ADJUD_NEEDED ==1 | LIVEBIRTH == 55 | is.na(LIVEBIRTH)  ~ 77, # if no livebirth, ==7 NA
                                TRUE ~ 55)) %>% 
  # generate variable with concurrent PSBI 
  mutate(NEO_SEPSIS_PSBI = case_when(NEO_SEPSIS==1 & NEO_SEPSIS_AGE >= 0 & NEO_SEPSIS_AGE<28 & (PSBI_DX_DATE_CHECKBOX==1 | PSBI_DX_DATE_CULTURE==1) ~ 1, # if sepsis and psbi dx ocurred within 14 days of eachother, ==1
                                     LIVEBIRTH==0 | ADJUD_NEEDED ==1 | NEO_SEPSIS==0 | LIVEBIRTH == 55 | is.na(LIVEBIRTH) | NEO_SEPSIS_AGE >=28 ~ 77, # if no sepsis, ==77 NA
                                     NEO_SEPSIS==1 &  NEO_SEPSIS_AGE >= 0 & NEO_SEPSIS_AGE<28 & ((PSBI_DX_DATE_CHECKBOX==0 & PSBI_DX_DATE_CULTURE==0) | is.na(PSBI_DX_DATE_CHECKBOX) & is.na(PSBI_DX_DATE_CULTURE)) ~ 0, # if sepsis but no concurrent psbi with 14 days, ==0,
                                     TRUE ~ 55
  )) %>% 
  ## generate cleaner variable for culture confirmed 
  mutate(NEO_SEPSIS_CULTURE = case_when(NEO_SEPSIS==1 & POSITIVE_CULTURE==1 ~ 1, 
                                        NEO_SEPSIS==1 & POSITIVE_CULTURE==0 ~ 0,
                                        NEO_SEPSIS==0 | NEO_SEPSIS == 77 ~ 77,
                                        TRUE ~ 55
  ))  

table(neo_sepsis$NEO_SEPSIS, neo_sepsis$SITE)
table(neo_sepsis$NEO_SEPSIS_CULTURE, neo_sepsis$SITE)
table(neo_sepsis$NEO_SEPSIS_PSBI, neo_sepsis$SITE)
#*****************************************************************************
## Fetal Anomaly ----
## Purpose: Construct the Fetal Anomaly outcome
## Translated from Stata: construct_fetal-anom
#*****************************************************************************
## 2. INF_ANOMALY : any fetal anomaly noted at delivery
##    1 = yes, 0 = no, 55 = unknown (77/99 in raw recoded to 55)
fetal_anomaly <- mnh11 %>%
  mutate(
    INF_ANOMALY = M11_CON_MALFORM_MHOCCUR,
    INF_ANOMALY = ifelse(
      M11_CON_MALFORM_MHOCCUR %in% c(77, 99),
      55,
      INF_ANOMALY
    )
  )

## 3. INF_ANOMALY_COUNT : count of anomaly types checked (1–13, 88)
term_cols <- paste0("M11_CON_MALFORM_MHTERM_", c(1:13, 88))

fetal_anomaly <- fetal_anomaly %>%
  mutate(
    INF_ANOMALY_COUNT = ifelse(INF_ANOMALY == 1, 0L, NA_integer_),
    INF_ANOMALY_COUNT = rowSums(
      across(all_of(term_cols), ~ coalesce(.x, 0) == 1),
      na.rm = TRUE
    ) + ifelse(INF_ANOMALY == 1, 0L, NA_integer_)
  ) %>%
  mutate(
    INF_ANOMALY_COUNT = ifelse(
      INF_ANOMALY == 1,
      INF_ANOMALY_COUNT,
      NA_integer_
    )
  )

print(table(fetal_anomaly$INF_ANOMALY_COUNT, useNA = "ifany"))


## 4. Reassign "Other" free-text that matches listed categories
##    e.g. "Imperforate anus" → Anorectal malformation (category 6)

fetal_anomaly <- fetal_anomaly %>%
  mutate(
    M11_CON_MALFORM_MHTERM_6 = ifelse(
      M11_CON_MALFORM_SPFY_MHTERM == "Imperforate  anus",
      1,
      M11_CON_MALFORM_MHTERM_6
    ),
    M11_CON_MALFORM_MHTERM_88 = ifelse(
      M11_CON_MALFORM_MHTERM_6 == 1 &
        M11_CON_MALFORM_SPFY_MHTERM == "Imperforate  anus",
      0,
      M11_CON_MALFORM_MHTERM_88
    ),
    M11_CON_MALFORM_MHTERM_88 = ifelse(
      M11_CON_MALFORM_SPFY_MHTERM == "n/a" &
        INF_ANOMALY_COUNT == 2 &
        M11_CON_MALFORM_MHTERM_88 == 1,
      0,
      M11_CON_MALFORM_MHTERM_88
    )
  )


## 5. INF_ANOMALY_FINGERS : extra-digit anomalies captured under "Other"
fingers_strings <- c(
  "Extra digits on both upper arms",
  "Extra  digit",
  "Xtra digits ",
  "Extra finger digits on both sides ",
  "Extra fingers in both hands",
  "Extra finger digits- tied with nylon after consent of the mother. ",
  "Extra figure digits",
  "Extra finger digit on left hand.",
  "Extra digits on both hands",
  "ACC--Dysmorphism-polydactyly-cisterna ",
  "Extra digit left hand ",
  "Extra digits ",
  "Polydactyl",
  "Six finger on both hand",
  "Extra finger ",
  "Extra digit on the right arm",
  "Extra digits in both the lower and upper limbs.",
  "Left polydactyly",
  "Right foot Post axial polydactyly"
)

fetal_anomaly <- fetal_anomaly %>%
  mutate(
    INF_ANOMALY_FINGERS = case_when(
      INF_ANOMALY != 1 ~ INF_ANOMALY,
      INF_ANOMALY == 1 &
        M11_CON_MALFORM_SPFY_MHTERM %in% fingers_strings ~ 1L,
      INF_ANOMALY == 1 ~ 0L,
      TRUE ~ NA_integer_
    ),
    M11_CON_MALFORM_MHTERM_88 = ifelse(
      M11_CON_MALFORM_MHTERM_88 == 1 &
        INF_ANOMALY_FINGERS == 1 &
        M11_CON_MALFORM_SPFY_MHTERM !=
        "ACC--Dysmorphism-polydactyly-cisterna ",
      0,
      M11_CON_MALFORM_MHTERM_88
    )
  )


## 6. Recompute INF_ANOMALY_COUNT (now with finger anomalies)
fetal_anomaly <- fetal_anomaly %>%
  mutate(
    INF_ANOMALY_COUNT = ifelse(INF_ANOMALY == 1, 0L, NA_integer_),
    INF_ANOMALY_COUNT = rowSums(
      across(all_of(term_cols), ~ coalesce(.x, 0) == 1),
      na.rm = TRUE
    ),
    INF_ANOMALY_COUNT =
      INF_ANOMALY_COUNT + coalesce(INF_ANOMALY_FINGERS, 0L),
    INF_ANOMALY_COUNT = ifelse(
      INF_ANOMALY == 1,
      INF_ANOMALY_COUNT,
      NA_integer_
    )
  )

print(table(fetal_anomaly$INF_ANOMALY_COUNT, useNA = "ifany"))


## 7. INF_ANOMALY_TYPE : categorical anomaly type (KEPT NUMERIC)
##    1–13 = single listed anomaly
##    14   = extra digits only
##    66   = multiple anomalies (count >= 2)
##    88   = other (single)
##    99   = unknown (count == 0 but INF_ANOMALY == 1)

fetal_anomaly <- fetal_anomaly %>%
  mutate(INF_ANOMALY_TYPE = NA_integer_)

for (num in c(1:13, 88)) {
  col <- paste0("M11_CON_MALFORM_MHTERM_", num)
  fetal_anomaly <- fetal_anomaly %>%
    mutate(
      INF_ANOMALY_TYPE = ifelse(
        .data[[col]] == 1 & INF_ANOMALY_COUNT == 1,
        num,
        INF_ANOMALY_TYPE
      )
    )
}

fetal_anomaly <- fetal_anomaly %>%
  mutate(
    INF_ANOMALY_TYPE = ifelse(
      INF_ANOMALY_FINGERS == 1 & INF_ANOMALY_COUNT == 1,
      14L,
      INF_ANOMALY_TYPE
    ),
    INF_ANOMALY_TYPE = ifelse(
      INF_ANOMALY_COUNT == 0,
      99L,
      INF_ANOMALY_TYPE
    ),
    INF_ANOMALY_TYPE = ifelse(
      !is.na(INF_ANOMALY_COUNT) & INF_ANOMALY_COUNT >= 2,
      66L,
      INF_ANOMALY_TYPE
    )
  )


## Value labels — stored as an attribute on the numeric column,
## exactly like Stata's `label values`
anomaly_labels <- c(
  "1-Cleft lip/pallet"                 = 1L,
  "2-Neural tube defect"               = 2L,
  "3-Abdominal wall defect"            = 3L,
  "4-Bladder exstrophy"                = 4L,
  "5-Kidney malformation"              = 5L,
  "6-Anorectal malformation"           = 6L,
  "7-Congenital heart defect"          = 7L,
  "8-Bowel obstruction"                = 8L,
  "9-Tracheoesophageal fistula"        = 9L,
  "10-Hydrocephalus"                   = 10L,
  "11-Developmental dysplasia of hip"  = 11L,
  "12-Skeletal dysplasia"              = 12L,
  "13-Club foot"                       = 13L,
  "14-Extra digits (fingers/toes)"     = 14L,
  "66-Multiple anomalies"              = 66L,
  "88-Other"                           = 88L,
  "99-Unknown anomaly"                 = 99L
)

fetal_anomaly <- fetal_anomaly %>%
  mutate(
    INF_ANOMALY_TYPE = haven::labelled(
      as.integer(INF_ANOMALY_TYPE),
      labels = anomaly_labels,
      label  = "Type of fetal anomaly (categorical)"
    )
  )

print(table(as.integer(fetal_anomaly$INF_ANOMALY_TYPE), useNA = "ifany"))
print(attr(fetal_anomaly$INF_ANOMALY_TYPE, "labels"))


## 8. Rename individual anomaly indicator columns
fetal_anomaly <- fetal_anomaly %>%
  rename(
    INF_ANOMALY_CLEFTLIP  = M11_CON_MALFORM_MHTERM_1,
    INF_ANOMALY_NEURAL    = M11_CON_MALFORM_MHTERM_2,
    INF_ANOMALY_ABDOMINAL = M11_CON_MALFORM_MHTERM_3,
    INF_ANOMALY_BLADDER   = M11_CON_MALFORM_MHTERM_4,
    INF_ANOMALY_KIDNEY    = M11_CON_MALFORM_MHTERM_5,
    INF_ANOMALY_ANORECTAL = M11_CON_MALFORM_MHTERM_6,
    INF_ANOMALY_HEART     = M11_CON_MALFORM_MHTERM_7,
    INF_ANOMALY_BOWEL     = M11_CON_MALFORM_MHTERM_8,
    INF_ANOMALY_TRACH     = M11_CON_MALFORM_MHTERM_9,
    INF_ANOMALY_HYDROCEPH = M11_CON_MALFORM_MHTERM_10,
    INF_ANOMALY_HIPDYSP   = M11_CON_MALFORM_MHTERM_11,
    INF_ANOMALY_SKELDYSP  = M11_CON_MALFORM_MHTERM_12,
    INF_ANOMALY_CLUBFOOT  = M11_CON_MALFORM_MHTERM_13,
    INF_ANOMALY_OTHER     = M11_CON_MALFORM_MHTERM_88,
    INF_ANOMALY_UNKNOWN   = M11_CON_MALFORM_MHTERM_99
  )


## 9. Recode individual indicators so 0/1 aligns with INF_ANOMALY
##    77 = not applicable, 55 = unknown, 0 = anomaly present but not this type
indicator_vars <- c(
  "INF_ANOMALY_CLEFTLIP", "INF_ANOMALY_ABDOMINAL", "INF_ANOMALY_BLADDER",
  "INF_ANOMALY_KIDNEY",   "INF_ANOMALY_ANORECTAL","INF_ANOMALY_HEART",
  "INF_ANOMALY_BOWEL",    "INF_ANOMALY_TRACH",    "INF_ANOMALY_HYDROCEPH",
  "INF_ANOMALY_HIPDYSP",  "INF_ANOMALY_SKELDYSP", "INF_ANOMALY_CLUBFOOT",
  "INF_ANOMALY_OTHER",    "INF_ANOMALY_FINGERS",  "INF_ANOMALY_UNKNOWN"
)

for (v in indicator_vars) {
  fetal_anomaly <- fetal_anomaly %>%
    mutate(
      !!v := ifelse(INF_ANOMALY == 0,  77L, .data[[v]]),
      !!v := ifelse(INF_ANOMALY == 55, 55L, .data[[v]]),
      !!v := ifelse(INF_ANOMALY == 1 & .data[[v]] == 77, 0L, .data[[v]])
    )
}

fetal_anomaly <- fetal_anomaly %>%
  mutate(
    INF_ANOMALY_UNKNOWN = ifelse(
      INF_ANOMALY_COUNT == 0 & INF_ANOMALY == 1,
      1L,
      INF_ANOMALY_UNKNOWN
    )
  )


## 10. Keep only final constructed variables
fetal_anomaly_final <- fetal_anomaly %>%
  select(
    MOMID, PREGID, INFANTID, SITE,
    INF_ANOMALY, INF_ANOMALY_TYPE, INF_ANOMALY_COUNT,
    INF_ANOMALY_CLEFTLIP, INF_ANOMALY_NEURAL, INF_ANOMALY_ABDOMINAL,
    INF_ANOMALY_BLADDER, INF_ANOMALY_KIDNEY, INF_ANOMALY_ANORECTAL,
    INF_ANOMALY_HEART, INF_ANOMALY_BOWEL, INF_ANOMALY_TRACH,
    INF_ANOMALY_HYDROCEPH, INF_ANOMALY_HIPDYSP, INF_ANOMALY_SKELDYSP,
    INF_ANOMALY_CLUBFOOT, INF_ANOMALY_OTHER, INF_ANOMALY_FINGERS,
    INF_ANOMALY_UNKNOWN
  )


## 11. Final checks
print(table(as.integer(fetal_anomaly_final$INF_ANOMALY_TYPE), useNA = "ifany"))
print(table(fetal_anomaly_final$INF_ANOMALY,             useNA = "ifany"))
print(table(fetal_anomaly_final$INF_ANOMALY_COUNT,       useNA = "ifany"))


## 12. Save to TNT Drive
haven::write_dta(fetal_anomaly_final, file.path(path_to_tnt, "INF_ANOMALY.dta"))
readr::write_csv(fetal_anomaly_final, file.path(path_to_tnt, "INF_ANOMALY.csv"))

#*****************************************************************************
## Birth Position & Mode of Delivery ----
## Purpose: Construct risk factor variables related to birth position
##          & mode of delivery, by infant.
## Translated from Stata: construct_birthposition
#*****************************************************************************

## 2. Reshape MNH09 from wide (INF1-INF4) to long (1 row per infant)
## Stata loops 1/4, keeps the *_INF`num' columns, renames them to generic
## names, drops empty/unknown INFANTID, and appends. We do the same in R
## with map_dfr + bind_rows.

position_long <- purrr::map_dfr(1:4, function(num) {
  mnh09 %>%
    select(
      MOMID, PREGID, SITE,
      INFANTID         = paste0("M09_INFANTID_INF", num),
      PRESENT_FAORRES  = paste0("M09_PRESENT_FAORRES_INF", num),
      DELIV_PRROUTE    = paste0("M09_DELIV_PRROUTE_INF", num),
      VAG_PRROUTE      = paste0("M09_VAG_PRROUTE_INF", num)
    ) %>%
    mutate(INFANT_NUM = num) %>%
    filter(!is.na(INFANTID),
           !as.character(INFANTID) %in% c("", "55", "77", "n/a"))
})



## 3. Construct INF_PRES (fetal presentation)
##    1=Cephalic, 2=Breech, 3=Transverse, 4=Brow/face, 5=Other,
##    55=Missing/unknown

position_long <- position_long %>%
  mutate(
    INF_PRES = PRESENT_FAORRES,
    INF_PRES = ifelse(INF_PRES == 88, 5, INF_PRES),
    INF_PRES = ifelse(INF_PRES %in% c(77, 99) | is.na(INF_PRES), 55, INF_PRES)
  )

pres_labels <- c(
  "1-Cephalic"        = 1L,
  "2-Breech"          = 2L,
  "3-Transverse"      = 3L,
  "4-Brow or face"    = 4L,
  "5-Other"           = 5L,
  "55-Missing/unknown"= 55L
)

position_long <- position_long %>%
  mutate(
    INF_PRES = haven::labelled(as.integer(INF_PRES),
                               labels = pres_labels,
                               label  = "Fetal presentation (1=cephalic; 2=breech)")
  )

## 4. Create presentation indicator variables (1/0/55)
for (num in 1:5) {
  col <- paste0("INF_PRES_", num)
  position_long <- position_long %>%
    mutate(
      !!col := case_when(
        as.integer(INF_PRES) == num ~ 1L,
        as.integer(INF_PRES) == 55 ~ 55L,
        TRUE ~ 0L
      )
    )
}

position_long <- position_long %>%
  rename(
    INF_PRES_CEPH   = INF_PRES_1,
    INF_PRES_BREECH = INF_PRES_2,
    INF_PRES_TRANS  = INF_PRES_3,
    INF_PRES_BROW   = INF_PRES_4,
    INF_PRES_OTHER  = INF_PRES_5
  )



## 5. Construct INF_MODE_CES / INF_MODE_VAG
##    DELIV_PRROUTE: 1=vaginal, 2=c-section, 3=N/A (mat death pre-delivery)

position_long <- position_long %>%
  mutate(
    INF_MODE_CES = case_when(
      DELIV_PRROUTE == 1 ~ 0L,
      DELIV_PRROUTE == 2 ~ 1L,
      DELIV_PRROUTE == 3 ~ 77L,
      DELIV_PRROUTE %in% c(55, 77) | is.na(DELIV_PRROUTE) ~ 55L,
      TRUE ~ NA_integer_
    ),
    INF_MODE_VAG = case_when(
      DELIV_PRROUTE == 2 ~ 0L,
      DELIV_PRROUTE == 1 ~ 1L,
      DELIV_PRROUTE == 3 ~ 77L,
      DELIV_PRROUTE %in% c(55, 77) | is.na(DELIV_PRROUTE) ~ 55L,
      TRUE ~ NA_integer_
    )
  )

## 6. Construct INF_MODE_VAG_ASSISTED and INF_MODE_VAG_FORCEPS
##    VAG_PRROUTE: 1=spontaneous, 2=assisted breech, 3=vacuum,
##                 4=forceps, 88=other, 99/77/55=unknown
position_long <- position_long %>%
  mutate(
    ## Any assisted vaginal
    INF_MODE_VAG_ASSISTED = case_when(
      INF_MODE_CES == 1 ~ 0L,
      INF_MODE_VAG == 55 ~ 55L,
      INF_MODE_VAG == 77 ~ 77L,
      VAG_PRROUTE == 1 ~ 0L,
      VAG_PRROUTE %in% c(2, 3, 4, 88) ~ 1L,
      INF_MODE_VAG == 1 & VAG_PRROUTE %in% c(99, 77, 55) ~ 55L,
      TRUE ~ NA_integer_
    ),
    ## Forceps-assisted only
    INF_MODE_VAG_FORCEPS = case_when(
      INF_MODE_CES == 1 ~ 0L,
      INF_MODE_VAG == 55 ~ 55L,
      INF_MODE_VAG == 77 ~ 77L,
      VAG_PRROUTE %in% c(1, 2, 3, 88) ~ 0L,
      VAG_PRROUTE == 4 ~ 1L,
      INF_MODE_VAG == 1 & VAG_PRROUTE %in% c(99, 77, 55) ~ 55L,
      TRUE ~ NA_integer_
    )
  )

## 7. Keep constructed variables
position_clean <- position_long %>%
  select(
    MOMID, PREGID, SITE, INFANTID,
    INF_PRES, INF_PRES_CEPH, INF_PRES_BREECH, INF_PRES_TRANS,
    INF_PRES_BROW, INF_PRES_OTHER,
    INF_MODE_CES, INF_MODE_VAG, INF_MODE_VAG_ASSISTED, INF_MODE_VAG_FORCEPS
  )



## 8. Merge MAT_ENDPOINTS, restrict to deliveries >= 20 weeks GA
position_clean <- position_clean %>%
  left_join(
    mat_endpoints %>% select(MOMID, PREGID, PREG_END, PREG_END_GA),
    by = c("MOMID", "PREGID")
  ) %>%
  filter(PREG_END == 1,
         !is.na(PREG_END_GA),
         PREG_END_GA >= 140)



## 9. Denominators
inf_presentation <- position_clean %>%
  mutate(
    INF_PRES_DENOM = as.integer(PREG_END_GA >= 140 & !is.na(PREG_END_GA) & PREG_END == 1),
    INF_MODE_DENOM = as.integer(PREG_END_GA >= 140 & !is.na(PREG_END_GA) & PREG_END == 1)
  ) %>%
  select(-PREG_END, -PREG_END_GA)

## 10. Save
haven::write_dta(inf_presentation, file.path(path_to_tnt, "INF_PRESENTATION.dta"))
readr::write_csv(inf_presentation, file.path(path_to_tnt, "INF_PRESENTATION.csv"))
#*****************************************************************************
# Exporting ----
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
# 11. Neonatal sepsis
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
                  "INF_SGA_POSTTERM", "INF_AGA_POSTTERM", "SGA_CENTILE", "SGA_CAT")],
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>%
  
  full_join(mortality[c("SITE", "INFANTID", "MOMID", "PREGID",
                        "MISSING_MNH09", "MISSING_MNH11", "DTH_TIME_MISSING", "DOB_AFTER_DEATH",
                        "DTH_0D", "DTH_7D", "DTH_28D", "DTH_365D", "D28_DENOM", "D365_DENOM", "AGE_LAST_SEEN")],
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
                             "INF_BREATH_MASK_VENT", "INF_BREATH_PRESSURE", "INF_BREATH_SUCTION", "INF_BREATH_INTUBATION", "INF_BREATH_COMPRESS", "INF_BREATH_FAIL")],
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>%
  
  full_join(hyperbili_all_crit[c("SITE", "INFANTID", "MOMID", "PREGID",
                                 "INF_HYPERBILI_TCB15_24HR", "INF_HYPERBILI_TCB15_5DAY", "INF_HYPERBILI_TCB15_14DAY",
                                 "INF_HYPERBILI_AAP_24HR", "INF_HYPERBILI_AAP_5DAY", "INF_HYPERBILI_AAP_14DAY",
                                 "INF_HYPERBILI_NICE_24HR", "INF_HYPERBILI_NICE_5DAY", "INF_HYPERBILI_NICE_14DAY",
                                 "INF_JAUN_NON_SEV_ANY", "INF_JAUN_SEV_24HR", "INF_JAUN_SEV_GREATER_24HR", "DENOM_HYPERBILI_ANY",
                                 "DENOM_HYPERBILI_24HR", "DENOM_HYPERBILI_5DAY", "DENOM_HYPERBILI_14DAY", "DENOM_JAUN")],
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>%
  full_join(psbi_outcome_wide[c("SITE", "INFANTID", "MOMID", "PREGID",
                                "INF_PSBI_IPC", "INF_PSBI_PNC0",
                                "INF_PSBI_PNC1", "INF_PSBI_PNC4", "INF_PSBI_PNC6",
                                "INF_PSBI_UNSCHED", "INF_PSBI_HOSPITAL", "INF_PSBI_DENOM", "INF_PSBI_AGE_DAYS")],
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>%
  full_join(neo_sepsis[c("SITE", "INFANTID", "MOMID", "PREGID",
                         "NEO_SEPSIS", "NEO_SEPSIS_PSBI",
                         "NEO_SEPSIS_CULTURE", "NEO_SEPSIS_AGE")],
            by = c("SITE", "INFANTID", "MOMID", "PREGID")) 
  # full_join(fetal_anomaly_final[c("SITE", "INFANTID", "MOMID", "PREGID",
  #                                 "INF_ANOMALY", "INF_ANOMALY_TYPE", "INF_ANOMALY_COUNT",
  #                                 "INF_ANOMALY_CLEFTLIP", "INF_ANOMALY_NEURAL", "INF_ANOMALY_ABDOMINAL",
  #                                 "INF_ANOMALY_BLADDER", "INF_ANOMALY_KIDNEY", "INF_ANOMALY_ANORECTAL",
  #                                 "INF_ANOMALY_HEART", "INF_ANOMALY_BOWEL", "INF_ANOMALY_TRACH",
  #                                 "INF_ANOMALY_HYDROCEPH", "INF_ANOMALY_HIPDYSP", "INF_ANOMALY_SKELDYSP",
  #                                 "INF_ANOMALY_CLUBFOOT", "INF_ANOMALY_OTHER", "INF_ANOMALY_FINGERS",
  #                                 "INF_ANOMALY_UNKNOWN")],
  #           by = c("SITE", "INFANTID", "MOMID", "PREGID")) %>%
  # full_join(inf_presentation[c("SITE", "INFANTID", "MOMID", "PREGID",
  #                              "INF_PRES", "INF_PRES_CEPH", "INF_PRES_BREECH",
  #                              "INF_PRES_TRANS", "INF_PRES_BROW", "INF_PRES_OTHER",
  #                              "INF_MODE_CES", "INF_MODE_VAG",
  #                              "INF_MODE_VAG_ASSISTED", "INF_MODE_VAG_FORCEPS",
  #                              "INF_PRES_DENOM", "INF_MODE_DENOM")],
  #           by = c("SITE", "INFANTID", "MOMID", "PREGID"))


## data cleaning - recode missing 
infant_outcomes<- infant_outcomes %>% 
  #rename(INF_PSBI_AGE_DAYS = PSBI_AGE_DAYS) %>%
  mutate(STILLBIRTH_20WK = ifelse(STILLBIRTH_20WK==99 | (STILLBIRTH_20WK==55 & GESTAGEBIRTH_ANY<20), 66, STILLBIRTH_20WK),
         INF_ASPH = ifelse(INF_ASPH==66,55, INF_ASPH)
         
  ) %>% 
  # generate any birth outcome variable 
  mutate(BIRTH_OUTCOME_REPORTED = case_when((LIVEBIRTH ==1 | STILLBIRTH_20WK ==1) & INF_ABOR_IND==0 & INF_ABOR_SPN==0~ 1,
                                            ADJUD_NEEDED==1 ~ 55, 
                                            TRUE ~ 0)) %>% 
  # remove adjudication cased from variables 
  mutate(LIVEBIRTH = case_when(ADJUD_NEEDED==1 | STILLBIRTH_20WK == 66 | STILLBIRTH_20WK == 99~ 55, TRUE ~ LIVEBIRTH),
         STILLBIRTH_20WK = case_when(ADJUD_NEEDED==1 ~ 55, TRUE ~ STILLBIRTH_20WK)
  ) %>% 
  # update fetal loss variable to include all identified stillbirths, miscarriages, and induced abortions 
  mutate(FETAL_LOSS = case_when(STILLBIRTH_20WK==1 | INF_ABOR_SPN==1 | INF_ABOR_IND==1 ~ 1,
                                ADJUD_NEEDED==1 ~ 55,
                                TRUE ~ 0)) %>% 
  # update dob variable to only include dates for livebirths and stillbirths 
  mutate(DOB = case_when(INF_ABOR_IND==1 | INF_ABOR_SPN==1 ~ NA, TRUE ~ ymd(DOB))) %>% 
  # update dth_indicator variable (replace NA with 0)
  mutate(DTH_INDICATOR = case_when(DTH_INDICATOR==1 ~ 1, TRUE ~ 0)) %>% 
  
  # rename variables 
  rename(SEX = M09_SEX) %>% 
  select(SITE, MOMID, PREGID, INFANTID, ENROLL_US_DATE,BOE_METHOD, GA_DIFF_DAYS, EDD_BOE, PREG_START_DATE,
         LIVEBIRTH, FETAL_LOSS, BIRTH_OUTCOME_REPORTED,ADJUD_NEEDED, FETAL_LOSS_DATE, FETAL_LOSS_DATE, DOB, 
         DELIVERY_DATETIME,GESTAGEBIRTH_ANY,GESTAGEBIRTH_ANY_DAYS, GESTAGE_FETAL_LOSS_WKS, 
         CLOSEOUT, DTH_INDICATOR,AGE_LAST_SEEN, DEATHDATE_MNH24, DEATHTIME_MNH24, DEATH_DATETIME, AGEDEATH_DAYS, AGEDEATH_HRS,
         D28_DENOM, D365_DENOM,MISSING_MNH09, MISSING_MNH11, DTH_TIME_MISSING, DOB_AFTER_DEATH,
         BWEIGHT_PRISMA, BWEIGHT_ANY,M11_BW_FAORRES_REPORT, contains("LBW"),BW_TIME, MISSING_TIME, MISSING_PRISMA, MISSING_FACILITY, 
         MISSING_BOTH, contains("PRETERM"), contains("SGA"), contains("NEO_DTH"),INF_DTH,INF_FETAL_DTH_DENOM,
         contains("STILLBIRTH"), MISSING_SIGNS_OF_LIFE, contains("INF_"),
         contains("DENOM_HYPERBILI"), DENOM_JAUN,NEO_SEPSIS, NEO_SEPSIS_PSBI, NEO_SEPSIS_CULTURE, NEO_SEPSIS_AGE,
         INF_SGA_POSTTERM, INF_AGA_POSTTERM, INF_BREATH_MASK_VENT, INF_BREATH_PRESSURE, INF_BREATH_SUCTION,
         INF_BREATH_INTUBATION, INF_BREATH_COMPRESS, INF_BREATH_FAIL, SEX, contains("INF_ANOMALY"))

# test <- infant_outcomes %>% filter(LIVEBIRTH ==1 & FETAL_LOSS == 1)

table(infant_outcomes$LIVEBIRTH, infant_outcomes$SITE)
table(infant_outcomes$NEO_SEPSIS, infant_outcomes$SITE)
table(infant_outcomes$NEO_SEPSIS_CULTURE, infant_outcomes$SITE)
table(infant_outcomes$NEO_SEPSIS_PSBI, infant_outcomes$SITE)
table(infant_outcomes$INF_ABOR_SPN, infant_outcomes$SITE)

infant_outcomes$DOB <- as.character(infant_outcomes$DOB)
infant_outcomes$DEATHDATE_MNH24 <- as.character(infant_outcomes$DEATHDATE_MNH24)
infant_outcomes$FETAL_LOSS_DATE <- as.character(infant_outcomes$FETAL_LOSS_DATE)
infant_outcomes$PREG_START_DATE <- as.character(infant_outcomes$PREG_START_DATE)
infant_outcomes$ENROLL_US_DATE <- as.character(infant_outcomes$ENROLL_US_DATE)

table(infant_outcomes$GESTAGEBIRTH_ANY_DAYS, infant_outcomes$FETAL_LOSS)

test <- infant_outcomes %>% select(SITE, PREGID, INFANTID, GESTAGEBIRTH_ANY_DAYS, GESTAGEBIRTH_ANY, FETAL_LOSS) %>% 
  mutate(discrep = case_when(GESTAGEBIRTH_ANY_DAYS==GESTAGEBIRTH_ANY ~ 1, TRUE ~ 0))

#path_to_save = "D:/Users/stacie.loisate/Documents/PRISMA-Analysis-Stacie/"
write.csv(infant_outcomes, paste0(path_to_save, "INF_OUTCOMES" ,".csv"), row.names=FALSE)

# save data set; this will get called into the report
write.csv(infant_outcomes, paste0(path_to_tnt, "INF_OUTCOMES" ,".csv"), na="", row.names=FALSE)
write.xlsx(infant_outcomes, paste0(path_to_tnt, "INF_OUTCOMES" ,".xlsx"), na="", rownames=FALSE)

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

for (i in unique(infant_outcomes$SITE)) {
  df <- infant_outcomes %>% filter(SITE == i)
  print(paste0(i, " infants, n", "=", length(unique(df$INFANTID))))
}


#*****************************************************************************
# Adjudication cases for export ----
#*****************************************************************************

library(dplyr)
library(openxlsx)
library(stringr)
library(lubridate)

base_queries_dir <- "~/Analysis/Infant-Constructed-Variables/queries"
path_to_queries <- file.path(base_queries_dir, UploadDate)
dir.create(path_to_save_queries, recursive = TRUE, showWarnings = FALSE)

## Create new adjudication review joining relevant dataframes ----
adjudication_cases <- inf_baseline %>%
  filter(ADJUD_NEEDED == 1) %>%
  select(SITE,MOMID,PREGID,INFANTID) %>%
  ## Pull original MNH04 variables
  left_join(
    mnh04_constructed_fetal_loss %>%
      select(
        SITE,
        MOMID,
        PREGID,
        M04_PRG_DSDECOD,
        M04_FETAL_LOSS_DSDECOD,
        M04_FETAL_LOSS_DSSTDAT
      ) %>%
      distinct(SITE, MOMID, PREGID, .keep_all = TRUE),
    by = c("SITE", "MOMID", "PREGID")
  ) %>%
  
  ## Pull original MNH09 variables
  left_join(
    mnh09_long %>%
      select(
        SITE,
        MOMID,
        PREGID,
        INFANTID,
        M09_BIRTH_DSTERM,
        M09_DELIV_DSSTDAT,
        M09_DELIV_DSSTTIM
      ) %>%
      distinct(SITE, MOMID, PREGID, INFANTID, .keep_all = TRUE),
    by = c("SITE", "MOMID", "PREGID", "INFANTID")
  ) %>%
  
  mutate(
    Form = "MNH04 and MNH09",
    
    `Reason for Review` = paste0(
      "MNH04 reports a fetal loss, while MNH09 reports a live birth. ",
      "Please review both forms and determine the correct pregnancy and infant outcome."
    ),
    
    `Variables for Review` = paste0(
      "M04_PRG_DSDECOD: ",
      coalesce(as.character(M04_PRG_DSDECOD), "NA"),
      
      " | M04_FETAL_LOSS_DSDECOD: ",
      coalesce(as.character(M04_FETAL_LOSS_DSDECOD), "NA"),
      
      " | M04_FETAL_LOSS_DSSTDAT: ",
      coalesce(
        format(as.Date(M04_FETAL_LOSS_DSSTDAT), "%Y-%m-%d"),
        "NA"
      ),
      
      " | M09_BIRTH_DSTERM: ",
      coalesce(as.character(M09_BIRTH_DSTERM), "NA"),
      
      " | M09_DELIV_DSSTDAT: ",
      coalesce(
        format(as.Date(M09_DELIV_DSSTDAT), "%Y-%m-%d"),
        "NA"
      ),
      
      " | M09_DELIV_DSSTTIM: ",
      coalesce(as.character(M09_DELIV_DSSTTIM), "NA")
    ),
    
    `Site Comments` = "",
    `Final Determination` = "",
    `Date implemented in the dataset (yyyy-mm-dd)` = "",
    `If other, please explain` = ""
  ) %>%
  
  transmute(
    Site = SITE,
    MomID = MOMID,
    PregID = PREGID,
    InfantID = INFANTID,
    Form,
    `Reason for Review`,
    `Variables for Review`,
    `Site Comments`,
    `Final Determination`,
    `Date implemented in the dataset (yyyy-mm-dd)`,
    `If other, please explain`
  ) %>%
  arrange(Site, MomID, PregID, InfantID)

##Workbook spreadsheet workbook ----
library(openxlsx)

adjudication_wb <- createWorkbook()

header_style <- createStyle(
  fontColour = "black",
  fgFill = "lightblue",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "TopBottomLeftRight"
)

body_style <- createStyle(
  valign = "top",
  wrapText = TRUE,
  border = "TopBottomLeftRight"
)

editable_style <- createStyle(
  fgFill = "#f8ff00",
  valign = "top",
  wrapText = TRUE,
  border = "TopBottomLeftRight"
)

date_style <- createStyle(
  numFmt = "yyyy-mm-dd",
  valign = "top",
  border = "TopBottomLeftRight"
)

dropdown_values <- c(
  "Confirmed only live birth, no data change",
  "Confirmed only live birth, data corrected",
  "Confirmed only fetal loss, no data change",
  "Confirmed only fetal loss, data corrected",
  "Unable to adjudicate, no additional information",
  "Other"
)

## Spreadsheet formatting ----
write_adjudication_sheet <- function(wb, sheet_name, data) {
  
  sheet_name <- substr(sheet_name, 1, 31)
  
  addWorksheet(
    wb,
    sheetName = sheet_name,
    gridLines = FALSE
  )
  
  writeData(
    wb,
    sheet = sheet_name,
    x = data,
    headerStyle = header_style,
    withFilter = TRUE
  )
  
  if (nrow(data) > 0) {
    
    addStyle(
      wb,
      sheet = sheet_name,
      style = body_style,
      rows = 2:(nrow(data) + 1),
      cols = 1:ncol(data),
      gridExpand = TRUE,
      stack = TRUE
    )
    
    editable_cols <- match(
      c(
        "Site Comments",
        "Final Determination",
        "Date implemented in the dataset (yyyy-mm-dd)",
        "If other, please explain"
      ),
      names(data)
    )
    
    addStyle(
      wb,
      sheet = sheet_name,
      style = editable_style,
      rows = 2:(nrow(data) + 1),
      cols = editable_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
    
    # Hidden dropdown source in the next unused column
    dropdown_col <- ncol(data) + 2
    
    writeData(
      wb,
      sheet = sheet_name,
      x = dropdown_values,
      startCol = dropdown_col,
      startRow = 1,
      colNames = FALSE
    )
    
    determination_col <- match(
      "Final Determination",
      names(data)
    )
    
    dropdown_letter <- int2col(dropdown_col)
    
    dataValidation(
      wb,
      sheet = sheet_name,
      cols = determination_col,
      rows = 2:(nrow(data) + 1),
      type = "list",
      value = paste0(
        "$",
        dropdown_letter,
        "$1:$",
        dropdown_letter,
        "$",
        length(dropdown_values)
      )
    )
    
    # Hide dropdown source column
    setColWidths(
      wb,
      sheet = sheet_name,
      cols = dropdown_col,
      widths = 0
    )
    
    date_col <- match(
      "Date implemented in the dataset (yyyy-mm-dd)",
      names(data)
    )
    
    addStyle(
      wb,
      sheet = sheet_name,
      style = date_style,
      rows = 2:(nrow(data) + 1),
      cols = date_col,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  
  freezePane(
    wb,
    sheet = sheet_name,
    firstRow = TRUE
  )
  
  setColWidths(wb, sheet_name, cols = 1, widths = 14)
  setColWidths(wb, sheet_name, cols = 2:4, widths = 20)
  setColWidths(wb, sheet_name, cols = 5, widths = 20)
  setColWidths(wb, sheet_name, cols = 6, widths = 55)
  setColWidths(wb, sheet_name, cols = 7, widths = 85)
  setColWidths(wb, sheet_name, cols = 8, widths = 40)
  setColWidths(wb, sheet_name, cols = 9, widths = 32)
  setColWidths(wb, sheet_name, cols = 10, widths = 24)
  setColWidths(wb, sheet_name, cols = 11, widths = 40)
  
  setRowHeights(
    wb,
    sheet = sheet_name,
    rows = 1,
    heights = 40
  )
}

### Combined sheet ----
write_adjudication_sheet(
  wb = adjudication_wb,
  sheet_name = "All Adjudication Cases",
  data = adjudication_cases
)

# Site-specific sheets
for (site_name in unique(adjudication_cases$Site)) {
  
  site_data <- adjudication_cases %>%
    filter(Site == site_name)
  
  write_adjudication_sheet(
    wb = adjudication_wb,
    sheet_name = paste0(site_name, " Cases"),
    data = site_data
  )
}

adjudication_output_path <- file.path(
  path_to_save_queries,
  paste0(
    "Infant_Birth_Outcome_Adjudication_Cases_",
    UploadDate,
    ".xlsx"
  )
)

saveWorkbook(
  adjudication_wb,
  file = adjudication_output_path,
  overwrite = TRUE
)

cat(
  "Adjudication workbook saved to:\n",
  adjudication_output_path,
  "\n"
)