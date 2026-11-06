# Date: Feb 12, 2026
# Author: Xinyi Li

# THIS FILE IS CREATED TO GENERATE DATASET TO IMPUTE EARLY PREGNANCY WEIGHT AND PREPARE DATASETS FOR READOUTS

rm(list = ls())

library(tidyverse)
library(lubridate)
library(naniar)
library(writexl)
library(haven)
library(lubridate)
library(gridExtra)
library(grid)
library(dplyr)
library(base)
library(tidyr)
library(ggplot2)
library(readr)
library(wesanderson)
library(readxl)

setwd("D:/Users/xinyili/Documents/Analysis/Gestational Weight Gain")
folder_path <- paste0("D:/Users/xinyili/Documents/Analysis/Gestational Weight Gain/data_out")

UploadDate = "2026-05-01"

#****************************************************************************
# 0. PULL IN DATASETS
#****************************************************************************

#*************************
# Read files (first time)
#*************************
# # Define the path to the folder containing the CSV files
# folder_path <- paste0("D:/Users/xinyili/Documents/Analysis/Gestational Weight Gain/Stacked Data/", UploadDate)
# 
# # Get a list of all CSV files in the folder
# csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
# 
# # NOTE: Don't need to read the files in over and over again now that I've created an RDA file.
# # Loop through each CSV file
# for (file in csv_files) {
#   # Extract the base name of the file (without the path)
#   base_name <- basename(file)
# 
#   # Extract the part of the name before the underscore
#   name_prefix <- str_split(base_name, "_")[[1]][1]
# 
#   # Read the CSV file into a data frame
#   data_frame <- read.csv(file)
# 
#   # Assign the data frame to a variable with the extracted name
#   assign(name_prefix, data_frame)
# }
# 
# # Optional: List all variables in the environment to verify
# ls()
# 
# save(mnh01, mnh02, mnh05,
#      file = (paste0("data_out/mnh_dataframes_imputedbmi_",UploadDate,".rda")))

#*******************************************
# Load in the Rda file (after first time)
#*******************************************
load(paste0("data_out/mnh_dataframes_imputedbmi_",UploadDate,".rda"))


#*******************************************
# Pull in all other datasets for later use
#*******************************************
mat_enroll <- read_dta(paste0("Z:/Outcome Data/",UploadDate,"/MAT_ENROLL.dta"))
mat_endpoints <- read_dta(paste0("Z:/Outcome Data/",UploadDate,"/MAT_ENDPOINTS.dta"))
mat_demographic <- read_dta(paste0("Z:/Outcome Data/",UploadDate,"/MAT_DEMOGRAPHIC.dta"))


#****************************************************************************
# 1. DATA CLEANING FOR IMPUTED BMI
#****************************************************************************

#*************************************************
# Merge data in for enrolled women
#*************************************************
# Check unique count for SCRNID, MOMID, and PREGID
# length(unique(mat_enroll$SCRNID))
# length(unique(mat_enroll$MOMID))
# length(unique(mat_enroll$PREGID))
# Note to Lili: everyone in mat_enroll is eligible to enroll, SCRNID n = PREGID n

# Testing
# mat_enroll2 <- mat_enroll

# Step 1: Extract SCRNID and merge mnh01 & mnh05 based on SCRNID & PREGID
# SCRNID_vector <- mat_enroll %>% 
#   pull(SCRNID) %>% 
#   unique()

PREGID_vector <- mat_enroll %>% 
  pull(PREGID) %>% 
  unique()

# Step 2: Filter mnh01  & mnh05 based on SCRNIDs vector
# mnh01_filtered <- mnh01 %>%
#   filter(SCRNID %in% SCRNID_vector)

mnh05_filtered <- mnh05 %>%
  filter(PREGID %in% PREGID_vector)

# Step 3: Subset mnh01
# mnh01_filtered2 <- mnh01_filtered %>%
#   select(SITE, SCRNID, MOMID, PREGID, M01_TYPE_VISIT,
#          M01_US_OHOSTDAT, 
#          M01_US_GA_DAYS_AGE_FTS1, M01_US_GA_DAYS_AGE_FTS2, 
#          M01_US_GA_DAYS_AGE_FTS3, M01_US_GA_DAYS_AGE_FTS4,
#          M01_US_GA_WKS_AGE_FTS1, M01_US_GA_WKS_AGE_FTS2, 
#          M01_US_GA_WKS_AGE_FTS3, M01_US_GA_WKS_AGE_FTS4, 
#          M01_GA_LMP_WEEKS_SCORRES,M01_GA_LMP_DAYS_SCORRES)

# Step 4: Merge mnh01 & mnh05 to mat_enroll
# mnh01_filtered2 <- mnh01_filtered2 %>%
#   rename(TYPE_VISIT = M01_TYPE_VISIT)
# 
# mnh_enroll_01 <- left_join(mat_enroll2,mnh01_filtered2,
#                       by = c("SCRNID","MOMID","PREGID","SITE"))

mnh05_filtered <- mnh05 %>%
  filter(PREGID %in% PREGID_vector) %>%
  relocate(SITE, MOMID, PREGID, M05_TYPE_VISIT) %>%
  rename(TYPE_VISIT = M05_TYPE_VISIT) %>%
  select(MOMID, PREGID, SITE, TYPE_VISIT, M05_ANT_PEDAT, M05_WEIGHT_PERES, M05_WEIGHT_PEPERF, M05_HEIGHT_PEPERF,M05_HEIGHT_PERES)

mnh_enroll_05 <- full_join(mat_enroll, mnh05_filtered,
                         by = c("MOMID", "PREGID", "SITE"))

# mnh_enroll_05 %>% distinct (MOMID, PREGID, SITE, .keep_all = TRUE) %>% count(SITE)


#**********************************
# Remove NA rows on missing PREGID
#**********************************
mnh_enroll_05_nomissing <- mnh_enroll_05 %>%
  filter(!is.na(PREGID))



#*****************************************
# 2. Create GWG variables for imputed BMI
#*****************************************

#* *******************************************************
#* Backfill weight from unplanned visit to missing ANC visit
#* *******************************************************
# Replace -7 values to NA for maternal height and weight
merged_df<- mnh_enroll_05_nomissing %>%
  mutate(M05_WEIGHT_PERES = ifelse(M05_WEIGHT_PERES==-7, NA, M05_WEIGHT_PERES),
         M05_HEIGHT_PERES = ifelse(M05_HEIGHT_PERES==-7, NA, M05_HEIGHT_PERES),
         M05_WEIGHT_PERES = ifelse(M05_WEIGHT_PERES==-5, NA, M05_WEIGHT_PERES),
         M05_HEIGHT_PERES = ifelse(M05_HEIGHT_PERES==-5, NA, M05_HEIGHT_PERES))

# Create an updated type visit where type_visit==13 gets replaced with NA first
# Then we fill it with the updated type visit for that window

merged_df2 <- merged_df %>%
  filter(TYPE_VISIT %in% c(1,2,3,4,5,13))

# merged_df2 <- merged_df2 %>%
#   #DATE OF VISIT - CONCEPTION DATE = GA AT VISIT. 
#   mutate(M05_GA_AT_VISIT = as.integer(floor(as.numeric(as.Date(M05_ANT_PEDAT) - as.Date(PREG_START_DATE)))/7)) 

weight_df <- merged_df2 %>%
  mutate(TYPE_VISIT_UPDATED = ifelse(TYPE_VISIT==13, NA, TYPE_VISIT)) %>% 
  group_by(MOMID, PREGID, SITE) %>%
  arrange(MOMID, PREGID, SITE, as.Date(M05_ANT_PEDAT), TYPE_VISIT) %>%
  fill(TYPE_VISIT_UPDATED, .direction = "down") %>% 
  # Drop NA rows based on WEIGHT
  drop_na(M05_WEIGHT_PERES) %>% 
  # NOW doing the numbering:
  group_by(MOMID, PREGID, SITE, TYPE_VISIT_UPDATED) %>%
  filter(row_number() == 1)

temp <- weight_df %>%
  # filter(TYPE_VISIT==13) %>%
  select(TYPE_VISIT_UPDATED,TYPE_VISIT,M05_ANT_PEDAT,M05_WEIGHT_PERES)


#***************************************
#* Create initial weight & height vars 
#***************************************
# Create WEIGHT_ENROLL variable from TYPE_VISIT 1
weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(WEIGHT_ENROLL = M05_WEIGHT_PERES[TYPE_VISIT_UPDATED == 1][1],
         HEIGHT_ENROLL = M05_HEIGHT_PERES[TYPE_VISIT_UPDATED == 1][1]) %>%
  ungroup()

#* *****************************************
#* Calculating GA days, weeks, and GA flags
#* *****************************************
weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(
    GA_DAYS = as.numeric(difftime(as.Date(M05_ANT_PEDAT), as.Date(PREG_START_DATE), units = "days")),
    GA_WKS = as.numeric(difftime(as.Date(M05_ANT_PEDAT), as.Date(PREG_START_DATE), units = "weeks")),
    GA_WKS_FLAG = case_when(
      GA_WKS >= 0 & GA_WKS <= 42 ~ 0,
      GA_WKS < 0 | GA_WKS > 42 ~ 1,
      TRUE ~ NA_real_
    ),
    GA_WKS = floor(GA_WKS)  # Apply floor directly in mutate
  ) %>%
  ungroup()

#* *****************************************
#* Export dataset for imputed BMI in SAS
#* *****************************************
imputed_bmi <- weight_df %>%
  select(SITE, SCRNID, MOMID, PREGID, TYPE_VISIT_UPDATED,	PREG_START_DATE, M05_HEIGHT_PERES, M05_WEIGHT_PERES, WEIGHT_ENROLL, GA_WKS)

write.csv(imputed_bmi, paste0("data_out/",'GWG_IMPUTE_LONG_', UploadDate, ".csv"))



#****************************************************
# 3. Merge imputed BMI csv back and prepare datasets
#****************************************************

#**************************
# Import imputed BMI file
#**************************
imputed_df <- read.csv(paste0(folder_path, '/GWG_ImputeObs_wk9_wk14.csv')) 

imputed_df <- imputed_df %>%
  rename(
    SITE = ifelse("site" %in% colnames(imputed_df),"site","SITE"),
    PREGID = ifelse("pregid" %in% colnames(imputed_df),"pregid","PREGID"),
    WEIGHT_IMPUTED_9 = ifelse("weight_imputed_9" %in% colnames(imputed_df), "weight_imputed_9", "WEIGHT_IMPUTED_9"),
    WEIGHT_IMPUTED_14 = ifelse("weight_imputed_14" %in% colnames(imputed_df), "weight_imputed_14", "WEIGHT_IMPUTED_14")) %>%
  select(SITE,PREGID,WEIGHT_IMPUTED_9, WEIGHT_IMPUTED_14)

#********************************
# Merge imputed BMI to weight_df
#********************************
weight_df2 <- left_join(weight_df, imputed_df, by = c("SITE","PREGID"))

weight_df2 <- weight_df2 %>%
  arrange(PREGID, TYPE_VISIT) %>%  # Ensure the data is sorted by participant and visit
  group_by(PREGID) %>%  # Group by participant
  fill(WEIGHT_IMPUTED_9, .direction = "down") %>%  # Fill missing ENROLLMENT_WEIGHT down within each group
  ungroup()

#********************************
# Merge singleton to weight_df
#********************************
mat_demographic_sub <- mat_demographic %>%
  select(SCRNID, MOMID, PREGID, SITE, SINGLETON)

weight_df2 <- left_join(weight_df2,mat_demographic_sub, by = c("SCRNID","MOMID","PREGID","SITE"))

#***************************************
# Merge pregnancy endpoint to weight_df
#***************************************
mat_endpoints_sub <- mat_endpoints %>%
  select(MOMID,PREGID,SITE,PREG_END)

weight_df2 <- left_join(weight_df2,mat_endpoints_sub, by = c("MOMID","PREGID","SITE"))


#*********************************
# Create variables for datasets
#*********************************
# Fill in enrollment weight with imputed/recorded data
weight_df2 <- weight_df2 %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(GA_Enroll_Condition = BOE_GA_WKS_ENROLL[TYPE_VISIT_UPDATED == 1][1]) %>%
  ungroup()

weight_df2 <- weight_df2 %>%
  mutate(
    WEIGHT_ENROLL_IMPUTED = if_else(
      GA_Enroll_Condition > 9,
      WEIGHT_IMPUTED_9,
      WEIGHT_ENROLL),
    WEIGHT_IMPUTED_FLAG=case_when(GA_Enroll_Condition<=9~0,
                                  GA_Enroll_Condition>9~1,
                                  TRUE~NA)
  )

weight_df2 <- weight_df2 %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(WEIGHT_ENROLL_TEMP = WEIGHT_ENROLL,
         WEIGHT_ENROLL = WEIGHT_ENROLL_IMPUTED) %>%
  ungroup()

# Create BMI4CAT
weight_df2 <- weight_df2 %>%
  mutate(BMI=(WEIGHT_ENROLL/(HEIGHT_ENROLL^2))*10000,
         
         BMI4CAT=ifelse(BMI>12 & BMI<18.5,1, # underweight
                        ifelse(BMI>=18.5 & BMI<25,2, # normal weight
                               ifelse(BMI>=25 & BMI<30,3, # overweight
                                      ifelse(BMI >= 30 & BMI < 50, 4, NA)))),# obese
         BMI_EARLYPREG=factor(BMI4CAT,levels=c(1,2,3,4),
                              labels= c("Underweight", "Normal Weight", "Overweight", "Obesity")),
         BMI4CAT_MISS = case_when(BMI > 50 | BMI < 12 ~ 1,
                                  is.na(BMI) ~ 2,
                                  TRUE ~ NA_real_))
# Create GWG_TOTAL
# weight_df2 <- weight_df2 %>% 
#   group_by(SITE, MOMID, PREGID) %>%
#   arrange(as.Date(M05_ANT_PEDAT)) %>% 
#   mutate(GWG_FROM_ENROLL = M05_WEIGHT_PERES - WEIGHT_ENROLL,
#     GWG_TOTAL = ifelse(PREG_END==1,
#                       GWG_FROM_ENROLL[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)], NA))

weight_df2 <- weight_df2%>%
  group_by(PREGID) %>%
  mutate(
    GWG_TOTAL = if_else(
      PREG_END == 1 & TYPE_VISIT_UPDATED == max(TYPE_VISIT_UPDATED, na.rm = TRUE), 
      M05_WEIGHT_PERES - WEIGHT_ENROLL,
      NA_real_
    ),
    GWG_TOTAL = coalesce(GWG_TOTAL, last(GWG_TOTAL)),
    GWG_TOTAL = if_else(GWG_TOTAL < 0, NA_real_, GWG_TOTAL)
  ) %>%
  ungroup()

weight_df2 <- weight_df2 %>%
  mutate(
    GWG_TOTAL_FLAG = case_when(
      GWG_TOTAL == 0 ~ 1,
      GWG_TOTAL >= -10 & GWG_TOTAL <= 30 ~ 0,
      GWG_TOTAL < -10 | GWG_TOTAL > 30 ~ 1,
      TRUE ~ NA_real_
    )
  )

# temp2 <-weight_df2 %>%
#   select(SITE,MOMID,PREGID,GWG_TOTAL,GWG_TOTAL_FLAG)


# Create IOM_ADEQUACY
weight_df2 <- weight_df2 %>%
  group_by(SITE, MOMID, PREGID) %>% 
  mutate(GWG_REC_LO = case_when(
    BMI4CAT == 1 ~ ((2/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.44),
    BMI4CAT == 2 ~ ((2/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.35),
    BMI4CAT == 3 ~ ((1/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.23),
    BMI4CAT == 4 ~ ((0.5/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.17))) %>% 
  mutate(GWG_REC_LO = if_else(PREG_END==1, GWG_REC_LO, NA)) %>% 
  ungroup() 


weight_df2 <- weight_df2 %>%
  group_by(SITE, MOMID, PREGID) %>% 
  mutate(GWG_REC_HI = case_when(
    # First piece is weight gain in the first trimester.  2nd piece is weight gain in the 2nd and 3rd tri (after wk 13)
    BMI4CAT == 1 ~ ((2/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.58),
    BMI4CAT == 2 ~ ((2/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.50),
    BMI4CAT == 3 ~ ((1/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.33),
    BMI4CAT == 4 ~ ((0.5/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.27))) %>% 
  mutate(GWG_REC_HI = if_else(PREG_END==1, GWG_REC_HI, NA)) %>% 
  ungroup()

weight_df2$GWG_REC_LO <- round(weight_df2$GWG_REC_LO, digits = 2)
weight_df2$GWG_REC_HI <- round(weight_df2$GWG_REC_HI, digits = 2)

weight_df2 <- weight_df2 %>%
  mutate(GWG_REC_LO = if_else(GWG_REC_LO < 0, NA_real_, GWG_REC_LO),
         GWG_REC_HI = if_else(GWG_REC_HI < 0, NA_real_, GWG_REC_HI))


weight_df2 <- weight_df2 %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(IOM_ADEQUACY = case_when(
    (BMI4CAT %in% c(1,2,3,4) & GWG_TOTAL < GWG_REC_LO)  ~ 1,
    (BMI4CAT %in% c(1,2,3,4) & (GWG_TOTAL >= GWG_REC_LO & GWG_TOTAL<= GWG_REC_HI))  ~ 2,
    (BMI4CAT %in% c(1,2,3,4) & GWG_TOTAL > GWG_REC_HI)  ~ 3,
    TRUE ~ NA_real_))%>%
  ungroup()

# temp <- weight_df2%>%
#   select(SITE,MOMID,PREGID,GWG_REC_LO,GWG_TOTAL,GWG_REC_HI,IOM_ADEQUACY)
  
weight_df2 <- weight_df2 %>%
  mutate(GWG_ADEQUACY = case_when(
    IOM_ADEQUACY == 1 ~ "Inadequate",
    IOM_ADEQUACY == 2 ~ "Adequate",
    IOM_ADEQUACY == 3 ~ "Excessive",
    TRUE ~ NA_character_
  ))

# Create variables for MAT NUTRIENT report
gwg_data <- weight_df2 %>%
  arrange(MOMID,PREGID,TYPE_VISIT_UPDATED) %>%
  group_by(MOMID,PREGID) %>%
  mutate(VISNUM = row_number(),
         VISTOTAL = n(),
         lastentry = if_else(VISNUM == VISTOTAL,1,0)
  ) %>%
  ungroup() %>%
  filter(lastentry == 1)

# Create GWG_VALID & BMI4CAT_D - denominator
gwg_data <- gwg_data %>%
#  select(SITE,MOMID,PREGID,GWG_TOTAL,GWG_ADEQUACY,BMI4CAT,BMI,PREG_END) %>%
  mutate(GWG_VALID = if_else(!is.na(GWG_TOTAL),1,NA_real_),
         BMI4CAT_D = if_else(BMI_EARLYPREG %in% c("Underweight", "Normal Weight", "Overweight", "Obesity"),1,0),#BMI4CAT is numeric, BMI_EARLYPREG is factor, no difference
         GWG_MISS = case_when(is.na(GWG_TOTAL_FLAG)~1,
                                   GWG_TOTAL_FLAG==1 ~ 2,# GWG lost more than 10kg or gain more than 30kg, or gain 0 kg (most likely one visit)
                                   TRUE ~ NA_real_),
         BMI4CAT_MISS = case_when(BMI > 50 | BMI < 12 ~ 1,
                                  is.na(BMI) ~ 2,
                                  TRUE ~ NA_real_),# BMI4CAT_MISS = 1(out of range),BMI4CAT_MISS = 2(missing weight gain), missing means within normal range
         BMI4CAT_D = if_else(BMI4CAT_MISS == 1,NA_real_,BMI4CAT_D,missing=BMI4CAT_D),#denominator
         BMI = if_else(BMI4CAT_MISS == 1,NA,BMI,missing = BMI),# replace out of range with missing, create a new variable for this
         BMI4CAT = if_else(BMI4CAT_MISS == 1,NA_real_,BMI4CAT,missing = BMI4CAT),
         GWG_VALID = if_else(BMI4CAT_MISS == 1,NA_real_,GWG_VALID,missing = GWG_VALID),# keep a cohort not out of range & not missing, have a clean cohort
         GWG_ADEQUACY = if_else(BMI4CAT_MISS %in% c(1,2) | GWG_MISS %in% c(1,2),NA_character_,GWG_ADEQUACY,missing = GWG_ADEQUACY)
          )

temp.df <-gwg_data %>%
  select(SITE,MOMID,PREGID,BMI,BMI4CAT,BMI_EARLYPREG,BMI4CAT_D,BMI4CAT_MISS,GWG_TOTAL,GWG_TOTAL_FLAG,GWG_VALID,GWG_ADEQUACY,IOM_ADEQUACY,PREG_END)

# Change GWG_TOTAL out of range to missing
gwg_data2 <- gwg_data %>%
  mutate(GWG_TOTAL = if_else(GWG_TOTAL_FLAG == 1, NA_real_, GWG_TOTAL))

# temp <- gwg_data2%>%
#   filter(GWG_TOTAL_FLAG==1) %>%
#   select(SITE,MOMID,PREGID,GWG_TOTAL,GWG_TOTAL_FLAG)

# Export datasets
## For Precious
precious_subset <- weight_df2 %>%
  select(SITE, SCRNID, MOMID, PREGID, TYPE_VISIT_UPDATED,	M05_ANT_PEDAT,HEIGHT_ENROLL,M05_WEIGHT_PERES,WEIGHT_ENROLL,BMI,BMI_EARLYPREG,BMI4CAT,WEIGHT_IMPUTED_FLAG)

write.csv(precious_subset, paste0("data_out/",'GWG_OUTCOME_LONG_ALL_', UploadDate, ".csv"))

## For MAT_GWG
mat_gwg_subset <- gwg_data2 %>%
  filter(PREG_END==1 & SINGLETON==1) %>%
  select(SITE,MOMID,PREGID,PREG_END,GWG_TOTAL,GWG_ADEQUACY,GWG_VALID,GWG_MISS,BMI,BMI4CAT,BMI4CAT_MISS,BMI4CAT_D)

write_dta(mat_gwg_subset, paste0("data_out/","MAT_GWG_",UploadDate,".dta"))
