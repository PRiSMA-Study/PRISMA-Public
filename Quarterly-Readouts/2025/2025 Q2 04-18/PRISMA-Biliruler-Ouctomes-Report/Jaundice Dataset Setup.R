#*****************************************************************************
#* PRISMA Jaundice Dataset Set-up Bili-ruler
#* Drafted: 11 April 2025, Alyssa Shapiro 
#* Last updated: 13 June 2025

# This code generates a wide and long version of the dataset used to analyze 
#jaundice-related outcomes (Visual Inspection, Bili-ruler, TCB, and TSB).
#This code is being used to setup the dataset for MULTIPLE analyses: 
# -- Se/Sp of Visual Inspection
# -- Biliruler PRISMA substudy
# -- Biliruler Q2 2025 Output

#Inputs: 
#Infant Outcomes
#MN09: Labor & Delivery
#MNH11: Newborn Birth Outcome
#MNH13: Infant Clinical Status
#MNH14: Infant POC Diagnostics
#MNH20: Newborn Hospitalization
#infant_outcomes: Infant Outcomes constructed variables
#MNH36: Bili-ruler

#Outputs: 
#1. infants_combined_wide: Combination of Infant Outcomes, MNH11, MNH13 (up to PNC-6), MNH14 (Up to PNC-6), 
   #AND BILI-RULER data, One row per unique infant

   #IMPORTANT! You will see a difference in row numbers if certain infants have MNH36 but not MNH11. 
   #This means that the number of infants represented in this dataset is slightly more than the number of 
   #infants in Stacie's infant_outcomes. 

#2. infants_combined_long: ^ same but long, one row per instance of jaundice evaluation

#*****************************************************************************
#*****************************************************************************
#* Data Setup 
#*****************************************************************************
library(knitr)
library(tidyverse)
library(reshape2)
library(lubridate)
library(kableExtra)
library(naniar)
library(RColorBrewer)
library(gt) ## for table gen
library(webshot2)  ## for table gen
library(readxl)
library(networkD3)
library(formattable)
library(flowchart)
library(TSB.NICE) #Yipeng's TCB package, NICE version
library(TSB.AAP) #Yipeng's TCB package, AAP version
library(gridExtra) #layouts of figures
library(DescTools) #AUC calculations
library(readxl)
library(colorRamps)
library(scales)

# set upload date 
UploadDate = "2025-05-30"

# set path to data
path_to_data <- paste0("Z:/Stacked Data/" ,UploadDate)
path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")
path_to_outcomes <- paste0("Z:/Outcome Data/", UploadDate) 
path_to_save <- "D:/Users/alyssa.shapiro/Box/Misc R code"

#Forms of interest: 
#MN09: Labor & Delivery
#MNH11: Newborn Birth Outcome
#MNH13: Infant Clinical Status
#MNH14: Infant POC Diagnostics
#MNH24: Infant Closeout
#infant_outcomes: Infant Outcomes constructed variables
#MNH36: Bili-ruler

#Upload forms
mnh09 <- read_xlsx(paste0(path_to_data,"/", "mnh09_merged.xlsx"))
mnh11 <- read_xlsx(paste0(path_to_data,"/", "mnh11_merged.xlsx"))
mnh13 <- read_xlsx(paste0(path_to_data,"/", "mnh13_merged.xlsx"))
mnh14 <- read_xlsx(paste0(path_to_data,"/", "mnh14_merged.xlsx"))
mnh24 <- read_xlsx(paste0(path_to_data,"/", "mnh24_merged.xlsx"))
infant_outcomes <- read_xlsx(paste0(path_to_outcomes, "/", "INF_OUTCOMES.xlsx"))
mnh36 <- read_xlsx(paste0(path_to_data,"/", "mnh36_merged.xlsx"))

#*****************************************************************************
#*****************************************************************************
#* Table Formatting 
#*****************************************************************************

tb_theme1 <- function(matrix){
  tb <- matrix %>%
    gt(
      rownames_to_stub = TRUE
    ) %>%
    opt_align_table_header(align = "left") %>%
    tab_spanner(
      label = "Sites"
      # columns = c(-Total)
    ) %>%
    opt_stylize(style = 6, color = 'gray') %>%
    tab_style(
      style = list(
        cell_fill(color = "#317773"),
        cell_text(weight = "bold"),
        cell_text(v_align = "middle"),
        cell_text(color = "white", size = px(18))
      ),
      locations = list(
        cells_title()
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#E2D1F9"),
        cell_text(color = "black", size = px(18)),
        cell_text(v_align = "middle")
      ),
      locations = list(
        cells_stubhead(),
        cells_column_spanners(),
        cells_column_labels()
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#F0F0F0"),
        cell_text(v_align = "middle", size = px(18))
      ),
      locations = list(
        cells_stub()
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(align = "center", size = px(18))
      ),
      locations = list(
        cells_column_labels(columns = everything())
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(align = "center", size = px(18))
      ),
      locations = list(
        cells_body(columns = everything())
      )
    ) %>%
    fmt_markdown(columns = everything()) %>%
    tab_options(table.width = pct(100)) %>%
    cols_width(1 ~ px(300))
}


# Function to extract and format legend horizontally
g_legend <- function(p) {
  tmp <- ggplot_gtable(ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend_gtable <- tmp$grobs[[leg]]
  
  # Arrange legend components horizontally
  legend_gtable$widths <- unit(rep(1, ncol(legend_gtable)), "null")
  legend_gtable$grobs <- legend_gtable$grobs[order(legend_gtable$layout$l)]
  
  return(legend_gtable)
}


setwd(paste0(path_to_save))

#*****************************************************************************
#*****************************************************************************
#*Create wide format of infant outcomes + MNH11 + MNH13 + MNH14
#*****************************************************************************
#*
#*
infants_livebirths <- infant_outcomes %>%
  filter(LIVEBIRTH==1) %>%
  #Add MNH11
  left_join(mnh11,  by = c("SITE", "MOMID", "PREGID","INFANTID")) %>%
  #Add postnatal age 
  mutate(M11_VISIT_OBSSTTIM = replace(M11_VISIT_OBSSTTIM, M11_VISIT_OBSSTTIM %in% c("77:77", "99:99", "55:55:00","55:55"), NA), # replace default value time with NA 
         M11_VISIT_OBSSTDAT = as.character(M11_VISIT_OBSSTDAT)) %>%
  mutate(M11_VISIT_OBSSTDAT = replace(M11_VISIT_OBSSTDAT, M11_VISIT_OBSSTDAT=="1905-05-05",NA)) %>%
  mutate(M11_VISIT_DATETIME = case_when(!is.na(M11_VISIT_OBSSTTIM) ~ as.POSIXct(paste(M11_VISIT_OBSSTDAT, M11_VISIT_OBSSTTIM), format= "%Y-%m-%d %H:%M"),
                                        is.na(M11_VISIT_OBSSTTIM) ~ as.POSIXct(paste(M11_VISIT_OBSSTDAT,"00:00"),format = "%Y-%m-%d %H:%M"))) %>%  # assign time field type 
  # calculate age (hours and days) at MNH11 visit (if no default value visit date, then calculate)
  mutate(DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME,format="%Y-%m-%d %H:%M")) %>%
  #fix some typos
  #mutate(M11_VISIT_DATETIME = replace(M11_VISIT_DATETIME,M11_VISIT_DATETIME=="2013-07-18 08:45:00","2023-07-18 08:45:00"),
  #       M11_VISIT_DATETIME = replace(M11_VISIT_DATETIME,M11_VISIT_DATETIME=="2023-02-11 00:00:00" & INFANTID=="Z3-202-2138-A","2025-02-11 00:00:00"),
  #       M11_VISIT_DATETIME = replace(M11_VISIT_DATETIME,M11_VISIT_DATETIME=="2024-01-12 10:30:00" & INFANTID=="Z3-025-1904-A","2025-01-12 10:30:00"),
  #       M11_VISIT_DATETIME = replace(M11_VISIT_DATETIME,M11_VISIT_DATETIME=="2024-01-19 11:00:00" & INFANTID=="Z3-025-1967-A","2025-01-19 11:00:00"),
  #       M11_VISIT_DATETIME = replace(M11_VISIT_DATETIME,M11_VISIT_DATETIME=="2023-08-13 11:10:00" & INFANTID=="ZA-202-5145-A","2024-08-13 11:10:00"),
  #       M11_VISIT_DATETIME = replace(M11_VISIT_DATETIME,M11_VISIT_DATETIME=="2023-02-22 03:15:00" & INFANTID=="Z3-025-1290-A","2024-02-22 03:15:00"),
  #       M11_VISIT_DATETIME = replace(M11_VISIT_DATETIME,M11_VISIT_DATETIME=="2024-01-22 10:15:00" & INFANTID=="Z3-202-1889-A","2024-07-22 10:15:00")) %>%
  
  mutate(M11_AGE_AT_VISIT_DATETIME = case_when(!is.na(DELIVERY_DATETIME) ~ floor(difftime(M11_VISIT_DATETIME,DELIVERY_DATETIME,units = "hours")),
                                               is.na(DELIVERY_DATETIME) ~ floor(difftime(M11_VISIT_DATETIME,DOB,units="hours")))) %>%
  ###Address typos in dates:
  #Age at visit is - by <24 hours, due to a mismatch in visit time and delivery time
  mutate(M11_AGE_AT_VISIT_DATETIME = replace(M11_AGE_AT_VISIT_DATETIME,M11_AGE_AT_VISIT_DATETIME<0 & M11_AGE_AT_VISIT_DATETIME>=-24,0)) 

#Add PNC-0 
mnh13_pnc0 <- mnh13 %>% filter(M13_TYPE_VISIT==7)
mnh14_pnc0 <- mnh14 %>% filter(M14_TYPE_VISIT==7)
infants_livebirths_pnc0 <- infants_livebirths %>%
  left_join(mnh13_pnc0,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(mnh14_pnc0,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  #rename 
  rename_with(~paste0(., "_", 7), .cols = c(contains("M13"), contains("M14")))

#Add PNC-1
mnh13_pnc1 <- mnh13 %>% filter(M13_TYPE_VISIT==8)
mnh14_pnc1 <- mnh14 %>% filter(M14_TYPE_VISIT==8)
infants_livebirths_pnc1 <- infants_livebirths %>%
  left_join(mnh13_pnc1,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(mnh14_pnc1,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  rename_with(~paste0(., "_", 8), .cols = c(contains("M13"), contains("M14"))) %>%
  select("SITE","MOMID","PREGID","INFANTID",contains("M13"),contains("M14"))

#Combine 2 pnc visits
infants_livebirths_pnccombined <- infants_livebirths_pnc0 %>%
  left_join(infants_livebirths_pnc1,by=c("SITE","MOMID","PREGID","INFANTID"))

#Add PNC-4
mnh13_pnc4 <- mnh13 %>% filter(M13_TYPE_VISIT==9)
mnh14_pnc4 <- mnh14 %>% filter(M14_TYPE_VISIT==9)
infants_livebirths_pnc4 <- infants_livebirths %>%
  left_join(mnh13_pnc4,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(mnh14_pnc4,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  rename_with(~paste0(., "_", 9), .cols = c(contains("M13"), contains("M14"))) %>%
  select("SITE","MOMID","PREGID","INFANTID",contains("M13"),contains("M14"))


#Add PNC-6
mnh13_pnc6 <- mnh13 %>% filter(M13_TYPE_VISIT==10)
mnh14_pnc6 <- mnh14 %>% filter(M14_TYPE_VISIT==10)
infants_livebirths_pnc6 <- infants_livebirths %>%
  left_join(mnh13_pnc6,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(mnh14_pnc6,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  rename_with(~paste0(., "_", 10), .cols = c(contains("M13"), contains("M14"))) %>%
  select("SITE","MOMID","PREGID","INFANTID",contains("M13"),contains("M14"))

#COMBINE ALL PNC VISITS
infants_livebirths_pnccombined <- infants_livebirths_pnccombined %>%
  left_join(infants_livebirths_pnc4,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(infants_livebirths_pnc6,by=c("SITE","MOMID","PREGID","INFANTID"))

#Add MNH24 (Infant closeout)
infants_livebirths_combined <- infants_livebirths_pnccombined %>%
  left_join(mnh24,by=c("SITE","MOMID","PREGID","INFANTID"))%>%
  distinct(INFANTID,.keep_all=TRUE)

#subset <- infants_livebirths_combined %>% select(SITE,MOMID,INFANTID,M14_TCB_OBSSTTIM_7,M14_VISIT_OBSSTDAT_7,M14_VISIT_DATETIME_7,M14_AGE_AT_VISIT_DATETIME_7,DELIVERY_DATETIME,DOB)

#More postnatal age calculations
infants_livebirths_combined <- infants_livebirths_combined %>%
  #PNC-0
  #fix typos
  mutate(M14_TCB_OBSSTTIM_7 = replace(M14_TCB_OBSSTTIM_7,M14_TCB_OBSSTTIM_7=="1015","10:15")) %>%
  #replace default values with NAs
  mutate(M14_TCB_OBSSTTIM_7 = replace(M14_TCB_OBSSTTIM_7, M14_TCB_OBSSTTIM_7 %in% c("77:77", "77:77:77","99:99", "55:55:00","55:55"), NA),
         M14_VISIT_OBSSTDAT_7 = as.character(M14_VISIT_OBSSTDAT_7)) %>% 
  mutate(M14_VISIT_OBSSTDAT_7 = replace(M14_VISIT_OBSSTDAT_7,M14_VISIT_OBSSTDAT_7 %in% c("1905-05-05","1907-07-07 UTC"),NA)) %>%
  mutate(M14_VISIT_DATETIME_7 = case_when(!is.na(M14_TCB_OBSSTTIM_7) ~ as.POSIXct(paste(M14_VISIT_OBSSTDAT_7, M14_TCB_OBSSTTIM_7), format= "%Y-%m-%d %H:%M"),
                                          is.na(M14_TCB_OBSSTTIM_7) ~ as.POSIXct(paste(M14_VISIT_OBSSTDAT_7,"00:00"), format= "%Y-%m-%d %H:%M"))) %>%  # assign time field type 
  # calculate age (hours and days) at PNC-0 visit (if no default value visit date, then calculate)
  mutate(M14_AGE_AT_VISIT_DATETIME_7 = case_when(!is.na(DELIVERY_DATETIME) ~ floor(difftime(M14_VISIT_DATETIME_7,DELIVERY_DATETIME,units = "hours")),
                                                 is.na(DELIVERY_DATETIME) ~ floor(difftime(M14_VISIT_DATETIME_7,DOB,units="hours")))) %>%
  mutate(M14_AGE_AT_VISIT_DATETIME_7 = replace(M14_AGE_AT_VISIT_DATETIME_7,M14_AGE_AT_VISIT_DATETIME_7<0 & M14_AGE_AT_VISIT_DATETIME_7>=-24,0)) %>%
  
  #PNC-1
  mutate(M14_TCB_OBSSTTIM_8 = replace(M14_TCB_OBSSTTIM_8, M14_TCB_OBSSTTIM_8 %in% c("77:77", "77:77:77", "99:99", "55:55:00", "55:55"), NA),
         M14_VISIT_OBSSTDAT_8 = as.character(M14_VISIT_OBSSTDAT_8)) %>% # replace default value time with NA 
  mutate(M14_VISIT_OBSSTDAT_8 = replace(M14_VISIT_OBSSTDAT_8,M14_VISIT_OBSSTDAT_8 %in% c("1905-05-05","1907-07-07 UTC"),NA)) %>%
  mutate(M14_VISIT_DATETIME_8 = case_when(!is.na(M14_TCB_OBSSTTIM_8) ~ as.POSIXct(paste(M14_VISIT_OBSSTDAT_8, M14_TCB_OBSSTTIM_8), format= "%Y-%m-%d %H:%M"),
                                          is.na(M14_TCB_OBSSTTIM_8) ~ as.POSIXct(paste(M14_VISIT_OBSSTDAT_8,"00:00"), format= "%Y-%m-%d %H:%M"))) %>%  # assign time field type 
  # calculate age (hours and days) at PNC-1 visit (if no default value visit date, then calculate)
  mutate(M14_AGE_AT_VISIT_DATETIME_8 = case_when(!is.na(DELIVERY_DATETIME) ~ floor(difftime(M14_VISIT_DATETIME_8,DELIVERY_DATETIME,units = "hours")),
                                                   is.na(DELIVERY_DATETIME) ~ floor(difftime(M14_VISIT_DATETIME_8,DOB,units="hours")))) %>%
  mutate(M14_AGE_AT_VISIT_DATETIME_8 = replace(M14_AGE_AT_VISIT_DATETIME_8,M14_AGE_AT_VISIT_DATETIME_8<0 & M14_AGE_AT_VISIT_DATETIME_8>=-24,0)) %>%
  
  #PNC-4
  mutate(M14_TCB_OBSSTTIM_9 = replace(M14_TCB_OBSSTTIM_9, M14_TCB_OBSSTTIM_9 %in% c("77:77", "77:77:77","99:99", "55:55:00","55:55"), NA),
         M14_VISIT_OBSSTDAT_9 = as.character(M14_VISIT_OBSSTDAT_9)) %>% # replace default value time with NA 
  mutate(M14_VISIT_OBSSTDAT_9 = replace(M14_VISIT_OBSSTDAT_9,M14_VISIT_OBSSTDAT_9 %in% c("1905-05-05","1907-07-07 UTC"),NA)) %>%
  mutate(M14_VISIT_DATETIME_9 = case_when(!is.na(M14_TCB_OBSSTTIM_9) ~ as.POSIXct(paste(M14_VISIT_OBSSTDAT_9, M14_TCB_OBSSTTIM_9), format= "%Y-%m-%d %H:%M"),
                                          is.na(M14_TCB_OBSSTTIM_9) ~ as.POSIXct(paste(M14_VISIT_OBSSTDAT_9,"00:00"), format= "%Y-%m-%d %H:%M"))) %>%  # assign time field type 
  # calculate age (hours and days) at PNC-1 visit (if no default value visit date, then calculate)
  mutate(M14_AGE_AT_VISIT_DATETIME_9 = case_when(!is.na(DELIVERY_DATETIME) ~ floor(difftime(M14_VISIT_DATETIME_9,DELIVERY_DATETIME,units = "hours")),
                                                 is.na(DELIVERY_DATETIME) ~ floor(difftime(M14_VISIT_DATETIME_9,DOB,units="hours")))) %>%
  mutate(M14_AGE_AT_VISIT_DATETIME_9 = replace(M14_AGE_AT_VISIT_DATETIME_9,M14_AGE_AT_VISIT_DATETIME_9<0 & M14_AGE_AT_VISIT_DATETIME_9>=-24,0)) %>%
  
    #pnc-6
  mutate(M14_TCB_OBSSTTIM_10 = replace(M14_TCB_OBSSTTIM_10, M14_TCB_OBSSTTIM_10 %in% c("77:77", "77:77:77","99:99", "55:55:00","55:55"), NA),
         M14_VISIT_OBSSTDAT_10 = as.character(M14_VISIT_OBSSTDAT_10)) %>% # replace default value time with NA 
  mutate(M14_VISIT_OBSSTDAT_10 = replace(M14_VISIT_OBSSTDAT_10,M14_VISIT_OBSSTDAT_10 %in% c("1905-05-05","1907-07-07 UTC"),NA)) %>%
  mutate(M14_VISIT_DATETIME_10 = case_when(!is.na(M14_TCB_OBSSTTIM_10) ~ as.POSIXct(paste(M14_VISIT_OBSSTDAT_10, M14_TCB_OBSSTTIM_10), format= "%Y-%m-%d %H:%M"),
                                          is.na(M14_TCB_OBSSTTIM_10) ~ as.POSIXct(paste(M14_VISIT_OBSSTDAT_10,"00:00"), format= "%Y-%m-%d %H:%M"))) %>%  # assign time field type 
  # calculate age (hours and days) at PNC-1 visit (if no default value visit date, then calculate)
  mutate(M14_AGE_AT_VISIT_DATETIME_10 = case_when(!is.na(DELIVERY_DATETIME) ~ floor(difftime(M14_VISIT_DATETIME_10,DELIVERY_DATETIME,units = "hours")),
                                                 is.na(DELIVERY_DATETIME) ~ floor(difftime(M14_VISIT_DATETIME_10,DOB,units="hours")))) %>%
  mutate(M14_AGE_AT_VISIT_DATETIME_10 = replace(M14_AGE_AT_VISIT_DATETIME_10,M14_AGE_AT_VISIT_DATETIME_10<0 & M14_AGE_AT_VISIT_DATETIME_10>=-24,0)) %>%
  
  #Replace values with NAs
  mutate(M11_TBILIRUBIN_UMOLL_LBORRES = replace(M11_TBILIRUBIN_UMOLL_LBORRES,M11_TBILIRUBIN_UMOLL_LBORRES=="-7",NA),
         M14_TCB_UMOLL_LBORRES_7 = replace(M14_TCB_UMOLL_LBORRES_7,M14_TCB_UMOLL_LBORRES_7=="-7",NA),
         M14_TCB_UMOLL_LBORRES_8 = replace(M14_TCB_UMOLL_LBORRES_8,M14_TCB_UMOLL_LBORRES_8=="-7",NA),
         M14_TCB_UMOLL_LBORRES_9 = replace(M14_TCB_UMOLL_LBORRES_9,M14_TCB_UMOLL_LBORRES_9=="-7",NA),
         M14_TCB_UMOLL_LBORRES_10 = replace(M14_TCB_UMOLL_LBORRES_10,M14_TCB_UMOLL_LBORRES_10=="-7",NA)
  ) %>%
  
  #Convert units from umol/L to mg/dL at 3 sites
  mutate(M11_TBILIRUBIN_UMOLL_LBORRES = 
           case_when(SITE=="Zambia" | 
                       SITE=="Kenya" | 
                       SITE=="India-SAS" 
                     ~ M11_TBILIRUBIN_UMOLL_LBORRES / 17.1,
                     TRUE ~ M11_TBILIRUBIN_UMOLL_LBORRES )) %>%
  mutate(M14_TCB_UMOLL_LBORRES_7 = 
           case_when(SITE=="Zambia" | 
                       SITE=="Kenya" | 
                       SITE=="India-SAS" 
                     ~ M14_TCB_UMOLL_LBORRES_7 / 17.1,
                     TRUE ~ M14_TCB_UMOLL_LBORRES_7)) %>%
  mutate(M14_TCB_UMOLL_LBORRES_8 = 
           case_when(SITE=="Zambia" | 
                       SITE=="Kenya" | 
                       SITE=="India-SAS" 
                     ~ M14_TCB_UMOLL_LBORRES_8 / 17.1,
                     TRUE ~ M14_TCB_UMOLL_LBORRES_8)) %>%
  mutate(M14_TCB_UMOLL_LBORRES_9 = 
           case_when(SITE=="Zambia" | 
                       SITE=="Kenya" | 
                       SITE=="India-SAS" 
                     ~ M14_TCB_UMOLL_LBORRES_9 / 17.1,
                     TRUE ~ M14_TCB_UMOLL_LBORRES_9)) %>%
  mutate(M14_TCB_UMOLL_LBORRES_10 = 
           case_when(SITE=="Zambia" | 
                       SITE=="Kenya" | 
                       SITE=="India-SAS" 
                     ~ M14_TCB_UMOLL_LBORRES_10 / 17.1,
                     TRUE ~ M14_TCB_UMOLL_LBORRES_9)) %>%
  
  #change datetime to make comparisons
  mutate(DEATHDATE_MNH24 = as.POSIXct(DEATHDATE_MNH24,format="%Y-%m-%d")) %>%
  mutate(M11_VISIT_OBSSTDAT=as.POSIXct(M11_VISIT_OBSSTDAT,format="%Y-%m-%d")) %>%
  mutate(M13_VISIT_OBSSTDAT_7=as.POSIXct(M13_VISIT_OBSSTDAT_7,format="%Y-%m-%d")) %>%
  mutate(M13_VISIT_OBSSTDAT_8=as.POSIXct(M13_VISIT_OBSSTDAT_8,format="%Y-%m-%d")) %>%
  mutate(M14_VISIT_OBSSTDAT_7=as.POSIXct(M14_VISIT_OBSSTDAT_7,format="%Y-%m-%d")) %>%
  mutate(M14_VISIT_OBSSTDAT_8=as.POSIXct(M14_VISIT_OBSSTDAT_8,format="%Y-%m-%d")) %>%
  mutate(M14_VISIT_OBSSTDAT_9=as.POSIXct(M14_VISIT_OBSSTDAT_9,format="%Y-%m-%d")) %>%
  mutate(M14_VISIT_OBSSTDAT_10=as.POSIXct(M14_VISIT_OBSSTDAT_10,format="%Y-%m-%d")) %>%
  mutate(DOB=as.POSIXct(DOB,format="%Y-%m-%d")) %>%
  mutate(M24_CLOSE_DSSTDAT=as.POSIXct(M24_CLOSE_DSSTDAT,format="%Y-%m-%d"))


#pull out duplicates
mnh36IPC <- mnh36 %>%
  filter(M36_TYPE_VISIT==6) 
IPCduplicates <- mnh36IPC %>%
  filter(duplicated(INFANTID))
mnh36IPC <- mnh36IPC %>%
  filter(!duplicated(INFANTID))

mnh36PNC0 <- mnh36 %>%
  filter(M36_TYPE_VISIT==7) 
PNC0duplicates <- mnh36PNC0 %>%
  filter(duplicated(INFANTID))
mnh36PNC0 <- mnh36PNC0 %>%
  filter(!duplicated(INFANTID))

mnh36PNC1 <- mnh36 %>%
  filter(M36_TYPE_VISIT==8) 
PNC1duplicates <- mnh36PNC1 %>%
  filter(duplicated(INFANTID))
mnh36PNC1 <- mnh36PNC1 %>%
  filter(!duplicated(INFANTID))

mnh36PNC4 <- mnh36 %>%
  filter(M36_TYPE_VISIT==9) 
PNC4duplicates <- mnh36PNC4 %>%
  filter(duplicated(INFANTID))
mnh36PNC4 <- mnh36PNC4 %>%
  filter(!duplicated(INFANTID))

mnh36PNC6 <- mnh36 %>%
  filter(M36_TYPE_VISIT==10) 
PNC6duplicates <- mnh36PNC6 %>%
  filter(duplicated(INFANTID))
mnh36PNC6 <- mnh36PNC6 %>%
  filter(!duplicated(INFANTID))

mnh36_77 <- mnh36 %>%
  filter(M36_TYPE_VISIT==77) 
duplicates77 <- mnh36_77 %>%
  filter(duplicated(INFANTID))
mnh36_77 <- mnh36_77 %>%
  filter(!duplicated(INFANTID))

mnh36 <- bind_rows(mnh36IPC,mnh36PNC0,mnh36PNC1,mnh36PNC4,mnh36PNC6,mnh36_77)

#If 3 bili-ruler or MST measurements are taken, then the outlier is dropped, and the remaining two are averaged
joint_function <- function(BILI_1,BILI_2,BILI_JOINT){
  vec <- sort(c(BILI_1,BILI_2,BILI_JOINT))
  if(abs(vec[3]-vec[2]) > abs(vec[2]-vec[1])){
    return((vec[1]+vec[2])/2)
  }  
  else if(abs(vec[3]-vec[2]) < abs(vec[2]-vec[1])){
    return((vec[3]+vec[2])/2)
  }
  else if(abs(vec[3]-vec[2]) == abs(vec[2]-vec[1])){
    return((vec[1]+vec[2]+vec[3])/3)
  }
}

#mnh36 will be the long format version
#Calculate the 'final' bili-ruler and MST value
mnh36 <- mnh36 %>%
  filter(!is.na(M36_MST_1)& !is.na(M36_MST_2) & !is.na(M36_MST_JOINT) & 
           !is.na(M36_BILI_1) & !is.na(M36_BILI_2) & !is.na(M36_BILI_JOINT)) %>%
  rowwise() %>%
  mutate(M36_BILI_FINAL = case_when(
    #2 valid #s, diff of 1 or 0
    (M36_BILI_1 %in% c(1,2,3,4,5,6) & 
       M36_BILI_2 %in% c(1,2,3,4,5,6) &
       abs(M36_BILI_1 - M36_BILI_2) < 2)
    ~ (M36_BILI_1+M36_BILI_2)/2,
    #2 PRISMA staff, 2 valid IDs, diff of 2 or more, 3rd measurement successful
    (M36_BILI_1 %in% c(1,2,3,4,5,6) & 
       M36_BILI_2 %in% c(1,2,3,4,5,6) & 
       abs(M36_BILI_1 - M36_BILI_2) >= 2 & 
       M36_BILI_JOINT %in% c(1,2,3,4,5,6)) 
    ~ joint_function(M36_BILI_1,M36_BILI_2,M36_BILI_JOINT),
    #2 PRISMA staff, 2 valid IDs, diff of 2 or more, 3rd measurement not valid
    (M36_BILI_1 %in% c(1,2,3,4,5,6) & 
       M36_BILI_2 %in% c(1,2,3,4,5,6) & 
       abs(M36_BILI_1 - M36_BILI_2) >= 2 & 
       !(M36_BILI_JOINT %in% c(1,2,3,4,5,6)))
    ~ (M36_BILI_1+M36_BILI_2)/2,
    #1 valid #
    (M36_BILI_1 %in% c(1,2,3,4,5,6) & 
       !(M36_BILI_2 %in% c(1,2,3,4,4,5,6)))
    ~ M36_BILI_1,
    (M36_BILI_2 %in% c(1,2,3,4,5,6) & 
       !(M36_BILI_1 %in% c(1,2,3,4,4,5,6)))
    ~ M36_BILI_2,
    #no valid #
    !(M36_BILI_1 %in% c(1,2,3,4,5,6)) & 
      !(M36_BILI_2 %in% c(1,2,3,4,4,5,6))
    ~ NA,
    TRUE ~ NA),
    #Same thing for MST values:
    M36_MST_FINAL = case_when(
      #2 valid #s, diff of 1 or 0
      (M36_MST_1 %in% c(1,2,3,4,5,6) & 
         M36_MST_2 %in% c(1,2,3,4,5,6) &
         abs(M36_MST_1 - M36_MST_2) < 2)
      ~ (M36_MST_1+M36_MST_2)/2,
      #2 PRISMA staff, 2 valid IDs, diff of 2 or more, 3rd measurement successful
      (M36_MST_1 %in% c(1,2,3,4,5,6) & 
         M36_MST_2 %in% c(1,2,3,4,5,6) & 
         abs(M36_MST_1 - M36_MST_2) >= 2 & 
         M36_MST_JOINT %in% c(1,2,3,4,5,6))
      ~ joint_function(M36_MST_1,M36_MST_2,M36_MST_JOINT),
      #2 PRISMA staff, 2 valid IDs, diff of 2 or more, 3rd measurement not valid
      (M36_MST_1 %in% c(1,2,3,4,5,6) & 
         M36_MST_2 %in% c(1,2,3,4,5,6) & 
         abs(M36_MST_1 - M36_MST_2) >= 2 & 
         !(M36_MST_JOINT %in% c(1,2,3,4,5,6)))
      ~ (M36_MST_1+M36_MST_2)/2,
      #1 valid #
      (M36_MST_1 %in% c(1,2,3,4,5,6) & 
         !(M36_MST_2 %in% c(1,2,3,4,4,5,6)))
      ~ M36_MST_1,
      (M36_MST_2 %in% c(1,2,3,4,5,6) & 
         !(M36_MST_1 %in% c(1,2,3,4,4,5,6)))
      ~ M36_MST_2,
      #no valid #
      (!(M36_MST_1 %in% c(1,2,3,4,5,6)) & 
         !(M36_MST_2 %in% c(1,2,3,4,4,5,6)))
      ~ NA,
      TRUE ~ NA))


#After duplicates are pulled out, rename variables so they are associated the visit (*_7, etc)

mnh36IPC <- mnh36 %>%
  filter(M36_TYPE_VISIT==6) 
names(mnh36IPC) <- paste0(names(mnh36IPC),"_6")
mnh36IPC <- mnh36IPC %>%
  rename("MOMID" = "MOMID_6",
         "PREGID" = "PREGID_6",
         "INFANTID" = "INFANTID_6",
         "SITE" = "SITE_6")

mnh36PNC0 <- mnh36 %>%
  filter(M36_TYPE_VISIT==7) 
names(mnh36PNC0) <- paste0(names(mnh36PNC0),"_7")
mnh36PNC0 <- mnh36PNC0 %>%
  rename("MOMID" = "MOMID_7",
         "PREGID" = "PREGID_7",
         "INFANTID" = "INFANTID_7",
         "SITE" = "SITE_7")

mnh36PNC1 <- mnh36 %>%
  filter(M36_TYPE_VISIT==8) 
names(mnh36PNC1) <- paste0(names(mnh36PNC1),"_8")
mnh36PNC1 <- mnh36PNC1 %>%
  rename("MOMID" = "MOMID_8",
         "PREGID" = "PREGID_8",
         "INFANTID" = "INFANTID_8",
         "SITE" = "SITE_8") 

mnh36PNC4 <- mnh36 %>%
  filter(M36_TYPE_VISIT==9) 
names(mnh36PNC4) <- paste0(names(mnh36PNC4),"_9")
mnh36PNC4 <- mnh36PNC4 %>%
  rename("MOMID" = "MOMID_9",
         "PREGID" = "PREGID_9",
         "INFANTID" = "INFANTID_9",
         "SITE" = "SITE_9")  

mnh36PNC6 <- mnh36 %>%
  filter(M36_TYPE_VISIT==10)
names(mnh36PNC6) <- paste0(names(mnh36PNC6),"_10")
mnh36PNC6 <- mnh36PNC6 %>% 
  rename("MOMID" = "MOMID_10",
         "PREGID" = "PREGID_10",
         "INFANTID" = "INFANTID_10",
         "SITE" = "SITE_10") 

mnh36_77 <- mnh36 %>%
  filter(M36_TYPE_VISIT==77)
names(mnh36_77) <- paste0(names(mnh36_77),"_77")
mnh36_77 <- mnh36_77 %>% 
  rename("MOMID" = "MOMID_77",
         "PREGID" = "PREGID_77",
         "INFANTID" = "INFANTID_77",
         "SITE" = "SITE_77") 

mnh36merged <- mnh36IPC %>%
  full_join(mnh36PNC0,by=c("MOMID","PREGID","INFANTID","SITE")) %>%
  full_join(mnh36PNC1,by=c("MOMID","PREGID","INFANTID","SITE")) %>%
  full_join(mnh36PNC4,by=c("MOMID","PREGID","INFANTID","SITE")) %>%
  full_join(mnh36PNC6,by=c("MOMID","PREGID","INFANTID","SITE")) %>%
  full_join(mnh36_77,by=c("MOMID","PREGID","INFANTID","SITE")) 

#Add mnh36 data to infant outcomes
infants_combined_wide <- infants_livebirths_combined %>%
  full_join(mnh36merged,by=c("MOMID","PREGID","INFANTID","SITE")) %>%
  mutate(DOB = as.Date(DOB)) %>%
  mutate(M13_VISIT_OBSSTDAT_9 = as.Date(M13_VISIT_OBSSTDAT_9),
         M14_VISIT_OBSSTDAT_9 = as.Date(M14_VISIT_OBSSTDAT_9)) %>%
  #Add bili-ruler
  mutate(
    STUDYSTARTDATE = case_when(
      # SITE=="Ghana" ~ NA,
      SITE=="India-CMC" ~ as.Date(strptime("2024-11-13",format="%Y-%m-%d")),
      SITE=="India-SAS" ~ as.Date(strptime("2024-12-03",format="%Y-%m-%d")),
      SITE=="Kenya" ~ as.Date(strptime("2025-01-13",format="%Y-%m-%d")),
      SITE=="Pakistan" ~ as.Date(strptime("2024-10-24",format="%Y-%m-%d")),
      SITE=="Zambia" ~ as.Date(strptime("2024-11-04",format="%Y-%m-%d")), 
      TRUE ~ NA)
  ) 

infants_combined_wide <- infants_combined_wide %>%
  #add all M09 birthoutcome variables
  left_join(mnh09 %>% select("MOMID", "PREGID", "SITE", matches("_INF1")), 
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF1", "SITE")) %>% 
  left_join(mnh09 %>% select("MOMID", "PREGID", "SITE", matches("_INF2")), 
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF2", "SITE")) %>% 
  left_join(mnh09 %>% dplyr::select("MOMID", "PREGID", "SITE", matches("_INF3")), 
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF3", "SITE")) %>% 
  left_join(mnh09 %>% select("MOMID", "PREGID", "SITE", matches("_INF4")), 
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF4", "SITE"))  


#*****************************************************************************
#*****************************************************************************
#* Create long format of the jaundice dataset
#*****************************************************************************

combined_ipc <- infants_combined_wide %>%
  filter(!is.na(M36_TYPE_VISIT_6) | 
           !is.na(M11_INF_VISIT_MNH11)) %>%  
  select(
    #general
    SITE, MOMID, PREGID, INFANTID, DOB, GESTAGEBIRTH_ANY, M11_AGE_AT_VISIT_DATETIME, M11_JAUND_CEOCCUR, M11_JAUND_CESTDAT, M11_YELLOW_CEOCCUR, M11_TBILIRUBIN_UMOLL_LBORRES, contains("M36_")) %>%
  select(-matches("_7([^7]|$)"),
         -contains("_8"),
         -contains("_9"),
         -contains("_10")) %>%
  select(-M36_STAFFID_1_6,-M36_STAFFID_2_6) %>%
  rename(
    TCB = M11_TBILIRUBIN_UMOLL_LBORRES,
    POSTNATALAGE = M11_AGE_AT_VISIT_DATETIME,
    IMCI_JAUND_CEOCCUR = M11_JAUND_CEOCCUR,
    YELLOW_CEOCCUR = M11_YELLOW_CEOCCUR
  ) %>%
  rename_with(~ gsub("_6$", "", .x), ends_with("_6")) %>%
  rename_with(~ gsub("^M36_", "", .x), starts_with("M36_")) %>%
  rename_with(~ gsub("^M11_", "", .x), starts_with("M11_")) %>%
  mutate(TYPE_VISIT=6)

combined_pnc0 <- infants_combined_wide %>%
  filter(!is.na(M36_TYPE_VISIT_7) |
           !is.na(M13_INF_VISIT_MNH13_7)) %>%
  select(
    #general
    SITE, MOMID, PREGID, INFANTID, DOB, GESTAGEBIRTH_ANY, M14_AGE_AT_VISIT_DATETIME_7, M13_JAUND_CEOCCUR_7, M13_YELL_CEOCCUR_7, M13_JAUND_CESTTIM_7, M14_TCB_UMOLL_LBORRES_7, contains("M36_")) %>%
  select(-contains("_6"),
         -contains("_8"),
         -contains("_9"),-contains("_10")) %>%
  select(-M36_STAFFID_1_7,-M36_STAFFID_2_7) %>%
  rename(
    TCB = M14_TCB_UMOLL_LBORRES_7,
    POSTNATALAGE = M14_AGE_AT_VISIT_DATETIME_7,
    IMCI_JAUND_CEOCCUR = M13_JAUND_CEOCCUR_7,
    YELLOW_CEOCCUR = M13_YELL_CEOCCUR_7
  ) %>%
  rename_with(~ gsub("_7$", "", .x), ends_with("_7")) %>%
  rename_with(~ gsub("^M36_", "", .x), starts_with("M36_")) %>%
  rename_with(~ gsub("^M13_", "", .x), starts_with("M13_")) %>%
  mutate(TYPE_VISIT=7)

combined_pnc1 <- infants_combined_wide %>%
  filter(!is.na(M36_TYPE_VISIT_8) |
                  !is.na(M13_INF_VISIT_MNH13_8)) %>%
  select(
    #general
    SITE, MOMID, PREGID, INFANTID, DOB, GESTAGEBIRTH_ANY, M14_AGE_AT_VISIT_DATETIME_8, M13_JAUND_CEOCCUR_8, M13_YELL_CEOCCUR_8, M14_TCB_UMOLL_LBORRES_8, contains("M36_")) %>%
  select(-contains("_6"),
         -matches("_7([^7]|$)"),
         -contains("_9"),
         -contains("_10")) %>%
  select(-M36_STAFFID_1_8,-M36_STAFFID_2_8) %>%  
  rename(
    TCB = M14_TCB_UMOLL_LBORRES_8,
    POSTNATALAGE = M14_AGE_AT_VISIT_DATETIME_8,
    IMCI_JAUND_CEOCCUR = M13_JAUND_CEOCCUR_8
  ) %>%
  rename_with(~ gsub("_8$", "", .x), ends_with("_8")) %>%
  rename_with(~ gsub("^M36_", "", .x), starts_with("M36_")) %>%
  mutate(TYPE_VISIT=8)


combined_pnc4 <- infants_combined_wide %>%
  filter(!is.na(M36_TYPE_VISIT_9)) %>%
  select(
    #general
    SITE, MOMID, PREGID, INFANTID, DOB, GESTAGEBIRTH_ANY, M14_AGE_AT_VISIT_DATETIME_9, M13_JAUND_CEOCCUR_9, M13_YELL_CEOCCUR_9, M14_TCB_UMOLL_LBORRES_9, contains("M36_")) %>%
  select(-contains("_6"), 
         -matches("_7([^7]|$)"),
         -contains("_8"),
         -contains("_10")) %>%
  select(-M36_STAFFID_1_9,-M36_STAFFID_2_9) %>%
  rename(
    TCB = M14_TCB_UMOLL_LBORRES_9,
    POSTNATALAGE = M14_AGE_AT_VISIT_DATETIME_9,
    IMCI_JAUND_CEOCCUR = M13_JAUND_CEOCCUR_9
  ) %>%
  rename_with(~ gsub("_9$", "", .x), ends_with("_9")) %>%
  rename_with(~ gsub("^M36_", "", .x), starts_with("M36_")) %>%
  mutate(TYPE_VISIT=9)

combined_pnc6 <- infants_combined_wide %>%
  filter(!is.na(M36_TYPE_VISIT_10)) %>%
  select(
    #general
    SITE, MOMID, PREGID, INFANTID, DOB, GESTAGEBIRTH_ANY, M14_AGE_AT_VISIT_DATETIME_10, M13_JAUND_CEOCCUR_10, M13_YELL_CEOCCUR_10, M14_TCB_UMOLL_LBORRES_10, contains("M36_")) %>%
  select(-contains("_6"),
         -matches("_7([^7]|$)"),
         -contains("_8"),
         -contains("_9")) %>%
  select(-M36_STAFFID_1_10,-M36_STAFFID_2_10) %>%
  rename(
    TCB = M14_TCB_UMOLL_LBORRES_10,
    POSTNATALAGE = M14_AGE_AT_VISIT_DATETIME_10,
    IMCI_JAUND_CEOCCUR = M13_JAUND_CEOCCUR_10
  ) %>%
  rename_with(~ gsub("_10$", "", .x), ends_with("_10")) %>%
  rename_with(~ gsub("^M36_", "", .x), starts_with("M36_")) %>%
  mutate(TYPE_VISIT=10)

#Bind visits together to create the infants_combined_long dataset (1 row per jaundice evaluation)
infants_combined_long <- bind_rows(combined_ipc, combined_pnc0, combined_pnc1, combined_pnc4, combined_pnc6) %>%
  #JAUNDATVISIT = IMCI result (Severe Jaundice = 2; Jaundice = 1; Not Jaundiced = 0)
  mutate(JAUNDATVISIT = case_when((IMCI_JAUND_CEOCCUR==1 & POSTNATALAGE < 24) | 
                                    (IMCI_JAUND_CEOCCUR==1 & POSTNATALAGE >= (21*24)) | 
                                    YELLOW_CEOCCUR==1 
                                  ~ 2,
                                  IMCI_JAUND_CEOCCUR ==1 & 
                                    POSTNATALAGE >= 24 & 
                                    POSTNATALAGE < (21*24) 
                                  ~ 1,
                                  TRUE ~ 0))

infants_combined_long$JAUNDATVISIT <- as.factor(infants_combined_long$JAUNDATVISIT)

#Add the NICE and AAP cutoffs
infants_combined_long <- infants_combined_long %>%
  mutate(CUTOFF_NICE = 0,
         CUTOFF_AAP = 0)

infants_combined_long$POSTNATALAGE <- as.numeric(infants_combined_long$POSTNATALAGE)

for (x in 1:nrow(infants_combined_long)){
  if ( 
    #Check for valid postnatal age, IMCI result, TCB result,
    #valid postnatal age
    (!is.na(infants_combined_long$POSTNATALAGE[x]) & 
     infants_combined_long$POSTNATALAGE[x]>=0) & 
       #valid TCB
       (infants_combined_long$TCB[x] <= 20 & 
        infants_combined_long$TCB[x] >= 0 &
        !is.na(infants_combined_long$TCB[x])) & 
       #valid GA
       !is.na(infants_combined_long$GESTAGEBIRTH_ANY[x])) {
    
    #Then, run through Yipeng's TCB code for both NICE and AAP
    if (infants_combined_long$GESTAGEBIRTH_ANY[x] <= 23){
      
      #if GA < 23, only run this for NICE BUT ROUND UP TO 23
      
      #NICE
      Y <- TSB_NICE(threshold = "P0", 
                    GA = "23 weeks", 
                    days = infants_combined_long$POSTNATALAGE[x] %/% 24, 
                    hours = infants_combined_long$POSTNATALAGE[x] %% 24)
      
      infants_combined_long$CUTOFF_NICE[x] = Y
      
      # AAP is invalid for this gestational age
      infants_combined_long$CUTOFF_AAP[x] = NA }
    
    #From 23-35 weeks, NICE works fine (input the exact gestational age), 
    #AAP is still invalid
    else if (infants_combined_long$GESTAGEBIRTH_ANY[x] < 35){
      
      #NICE
        Y <- TSB_NICE(threshold = "P0", 
                      GA = paste0(infants_combined_long$GESTAGEBIRTH_ANY[x]," weeks"), 
                      days = infants_combined_long$POSTNATALAGE[x] %/% 24, 
                      hours = infants_combined_long$POSTNATALAGE[x] %% 24)
        
        infants_combined_long$CUTOFF_NICE[x] = Y
        
        # AAP is invalid for this gestational age
        infants_combined_long$CUTOFF_AAP[x] = NA }
        
    else if (infants_combined_long$GESTAGEBIRTH_ANY[x] <38) {
      
      #NICE
      Y <- TSB_NICE(threshold = "P0", 
                    GA = paste0(infants_combined_long$GESTAGEBIRTH_ANY[x]," weeks"), 
                    days = infants_combined_long$POSTNATALAGE[x] %/% 24, 
                    hours = infants_combined_long$POSTNATALAGE[x] %% 24)
      
      infants_combined_long$CUTOFF_NICE[x] = Y
      
      #AAP
      Y <- TSB_AAP(threshold = "P0", 
                    GA = paste0(infants_combined_long$GESTAGEBIRTH_ANY[x]," weeks"), 
                    days = infants_combined_long$POSTNATALAGE[x] %/% 24, 
                    hours = infants_combined_long$POSTNATALAGE[x] %% 24)
     
      infants_combined_long$CUTOFF_AAP[x] = Y}
    
    else if (infants_combined_long$GESTAGEBIRTH_ANY[x] >= 38) {
      
      #NICE
      Y <- TSB_NICE(threshold = "P0",
                    GA = ">= 38 weeks",
                    days = as.numeric(infants_combined_long$POSTNATALAGE[x] %/% 24), 
                    hours = as.numeric(infants_combined_long$POSTNATALAGE[x] %% 24))
      
      infants_combined_long$CUTOFF_NICE[x] = Y
      
      #AAP
      Y <- TSB_AAP(threshold = "P0",
                    GA = ">= 38 weeks",
                    days = as.numeric(infants_combined_long$POSTNATALAGE[x] %/% 24), 
                    hours = as.numeric(infants_combined_long$POSTNATALAGE[x] %% 24))
      
      infants_combined_long$CUTOFF_AAP[x] = Y }
  }
  else{
    infants_combined_long$CUTOFF_NICE[x] = NA
    infants_combined_long$CUTOFF_AAP[x] = NA
    }
    
    }
  

infants_combined_long$POSTNATALAGE <- as.numeric(infants_combined_long$POSTNATALAGE)
infants_combined_long$JAUNDATVISIT <- as.factor(infants_combined_long$JAUNDATVISIT)

#*****************************************************************************
#*****************************************************************************
#* Remove datasets that aren't needed 
#*****************************************************************************
#*
rm(infants_livebirths)
rm(infants_livebirths_pnc0)
rm(infants_livebirths_pnc1)
rm(infants_livebirths_pnc4)
rm(infants_livebirths_pnc6)
rm(infants_livebirths_pnccombined)
rm(mnh09)
rm(mnh13_pnc0)
rm(mnh13_pnc1)
rm(mnh13_pnc4)
rm(mnh13_pnc6)
rm(mnh14)
rm(mnh14_pnc0)
rm(mnh14_pnc1)
rm(mnh14_pnc4)
rm(mnh14_pnc6)
rm(mnh09)
rm(mnh13)
rm(mnh11)
rm(mnh20)
rm(mnh24)
rm(combined_ipc)
rm(combined_pnc0)
rm(combined_pnc1)
rm(combined_pnc4)
rm(combined_pnc6)
