#*****************************************************************************
#*PRIMSA Flowchart dataset creation
#* Written by: Precious Williams
#* Date Created:  29 November 2025
#* Last updated: 11 March 2026
#* Updates (Current): 
#* 

#*****************************************************************************
rm(list = ls())

library(lubridate)
library(stringr)
library(dplyr)
library(haven)
library(gt)
library(flowchart)
library(magick)

UploadDate = "2026-01-30"
#Prevdate <- "2025-12-12"
path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate)

#LOAD MNH00-02 ---
mnh00 <- read.csv(paste0("Z:/Stacked Data/",UploadDate,"/mnh00_merged.csv"))
mnh01 <- read.csv(paste0("Z:/Stacked Data/",UploadDate,"/mnh01_merged.csv"))
mnh02 <- read.csv(paste0("Z:/Stacked Data/",UploadDate,"/mnh02_merged.csv"))
mnh23 <- read.csv(paste0("Z:/Stacked Data/",UploadDate,"/mnh23_merged.csv"))
mat_end <- read_dta(paste0("Z:/Outcome Data/", UploadDate, "/MAT_ENDPOINTS.dta"))
mat_enroll <- read.csv(paste0("Z:/Outcome Data/", UploadDate, "/MAT_ENROLL.csv"))

#Function to clean dates
clean_date <- function(x,
                       invalid_dates = as_date(c("1907-07-07", "2007-07-07",
                                                 "1905-05-05", "1909-07-07")),
                       min_date = NULL,
                       max_date = NULL) {
  
  # If max_date = NULL, default to today()
  if (is.null(max_date)) {
    max_date <- today()
  }
  
  # Parse many possible date formats
  parsed <- parse_date_time(
    trimws(as.character(x)),
    orders = c(
      "d/m/Y", "d-m-Y", "Y-m-d",   # day-first or ISO
      "d-b-y", "d-m-y",           # abbreviated formats
      "m/d/y", "m-d-y"            # US formats
    ),
    exact = FALSE
  ) %>% 
    as_date()
  
  # Build a logical mask of invalid cases
  invalid_mask <- is.na(parsed) | parsed %in% invalid_dates
  
  # Apply optional min_date
  if (!is.null(min_date)) {
    invalid_mask <- invalid_mask | parsed < min_date
  }
  
  # Apply optional max_date
  if (!is.null(max_date)) {
    invalid_mask <- invalid_mask | parsed > max_date
  }
  
  # Set invalid values to NA
  parsed[invalid_mask] <- NA
  
  return(parsed)
}

# #MNH23 ----
# mnh00$M00_SCRN_OBSSTDAT <- clean_date(
#   mnh00$M00_SCRN_OBSSTDAT,
#   min_date = ymd("2018-01-01"),
#   max_date = ymd(UploadDate)
# )
#MAT_PREG_END ----
mat_flow_end <- mat_end %>% 
  left_join(mnh23 %>% 
            select (SITE, MOMID, PREGID, M23_CLOSE_DSDECOD, M23_CLOSE_DSSTDAT), 
            by = c("MOMID", "PREGID", "SITE")) %>%
  # 0 = preg ended with live outcome
  # 1 = maternal death
  # 2 = lost to follow-up
  # 3 = withdrew prior to delivery
  # 4 = terminated from study
  # 55 = unknown
  mutate(
    M23_END_DATE = clean_date(
      M23_CLOSE_DSSTDAT,
      min_date = ymd("2018-01-01"),
      max_date = ymd(UploadDate)
    ),
    PREG_END_ALIVE = case_when(
      PREG_END == 1 & PREG_END_SOURCE %in% 1:3 ~ 1L,
      PREG_END == 0 | (PREG_END == 1 & PREG_END_SOURCE == 4) ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    PREG_NOT_END_REASN = case_when( 
      # pregnancy ended with a live outcome
      PREG_END_ALIVE == 1 ~ 0L,
      
      # maternal death
      PREG_LOSS_DEATH == 1 | 
        (PREG_END == 1 & PREG_END_SOURCE == 4) ~ 1L, 
      
      # lost to follow-up
      (CLOSEOUT_TYPE %in% c(1, 2, 6) |
      (CLOSEOUT == 1 & is.na(CLOSEOUT_TYPE))) & PREG_END == 0 ~ 2L,
      
      # withdrew prior to delivery
      CLOSEOUT_TYPE == 4 & PREG_END == 0 ~ 3L,
      
      # terminated from study
      CLOSEOUT_TYPE == 5 & PREG_END == 0 ~ 4L,
      
      # unknown
      TRUE ~ 55L
    )
  ) %>%
  select(SITE, MOMID, PREGID, PREG_END_ALIVE, PREG_NOT_END_REASN, STOP_DATE, 
         CLOSEOUT_DT, CLOSEOUT_GA, CLOSEOUT_DAYS_PP, M23_CLOSE_DSDECOD, M23_END_DATE)


#MNH00 ----
mnh00$M00_SCRN_OBSSTDAT <- clean_date(
  mnh00$M00_SCRN_OBSSTDAT,
  min_date = ymd("2018-01-01"),
  max_date = ymd(UploadDate)
)

mnh00$M00_BRTHDAT <- clean_date(
  mnh00$M00_BRTHDAT,
  max_date = ymd(UploadDate)
)

m00_df <- mnh00 %>% 
  group_by(SCRNID) %>% 
  arrange(desc(M00_SCRN_OBSSTDAT)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter(!is.na(SCRNID) & SCRNID != "") %>% 
  mutate(
    REMAPP_PRESCRN = case_when(
      SITE == "Ghana"      & M00_SCRN_OBSSTDAT >= ymd("2022-12-28") & M00_SCRN_OBSSTDAT <= ymd("2024-10-29") ~ 1,
      SITE == "Kenya"      & M00_SCRN_OBSSTDAT >= ymd("2023-04-03") & M00_SCRN_OBSSTDAT <= ymd("2025-03-11") ~ 1,
      SITE == "Zambia"     & M00_SCRN_OBSSTDAT >= ymd("2022-12-15") & M00_SCRN_OBSSTDAT <= ymd("2025-03-20") ~ 1,
      SITE == "Pakistan"   & M00_SCRN_OBSSTDAT >= ymd("2022-09-22") & M00_SCRN_OBSSTDAT <= ymd("2024-01-17") ~ 1,
      SITE == "India-CMC"  & M00_SCRN_OBSSTDAT >= ymd("2023-06-20") & M00_SCRN_OBSSTDAT <= ymd("2025-07-01") ~ 1,
      SITE == "India-SAS"  & M00_SCRN_OBSSTDAT >= ymd("2023-12-12") & M00_SCRN_OBSSTDAT <= ymd("2025-03-06") ~ 1,
      TRUE ~ 0
    ),
    PRESCREEN_DF = 1
  ) %>%
  distinct(SITE, SCRNID, .keep_all = TRUE)

#MNH01 ----
#MNH01 Date Cleaning 
#Ultrasound EDD
mnh01$M01_US_EDD_BRTHDAT_FTS1 <- clean_date(
  mnh01$M01_US_EDD_BRTHDAT_FTS1,
  min_date = ymd("2018-01-01"),
  max_date = ymd(UploadDate)
)
mnh01$M01_US_EDD_BRTHDAT_FTS2 <- clean_date(
  mnh01$M01_US_EDD_BRTHDAT_FTS2,
  min_date = ymd("2018-01-01"),
  max_date = ymd(UploadDate)
)
mnh01$M01_US_EDD_BRTHDAT_FTS3 <- clean_date(
  mnh01$M01_US_EDD_BRTHDAT_FTS3,
  min_date = ymd("2018-01-01"),
  max_date = ymd(UploadDate)
)
mnh01$M01_US_EDD_BRTHDAT_FTS4 <- clean_date(
  mnh01$M01_US_EDD_BRTHDAT_FTS4,
  min_date = ymd("2018-01-01"),
  max_date = ymd(UploadDate)
)

#CAL EDD
mnh01$M01_CAL_EDD_BRTHDAT_FTS1 <- clean_date(
  mnh01$M01_CAL_EDD_BRTHDAT_FTS1,
  min_date = ymd("2018-01-01"),
  max_date = ymd(UploadDate)
)
mnh01$M01_CAL_EDD_BRTHDAT_FTS2 <- clean_date(
  mnh01$M01_CAL_EDD_BRTHDAT_FTS2,
  min_date = ymd("2018-01-01"),
  max_date = ymd(UploadDate)
)
mnh01$M01_CAL_EDD_BRTHDAT_FTS3 <- clean_date(
  mnh01$M01_CAL_EDD_BRTHDAT_FTS3,
  min_date = ymd("2018-01-01"),
  max_date = ymd(UploadDate)
)
mnh01$M01_CAL_EDD_BRTHDAT_FTS4 <- clean_date(
  mnh01$M01_CAL_EDD_BRTHDAT_FTS4,
  min_date = ymd("2018-01-01"),
  max_date = ymd(UploadDate)
)
#MNH01 Enrolment Date
mnh01$M01_US_OHOSTDAT <- clean_date(
  mnh01$M01_US_OHOSTDAT,
  min_date = ymd("2018-01-01"),
  max_date = ymd(UploadDate)
)


m01_df <- mnh01 %>% filter(M01_TYPE_VISIT == 1) %>% ## only want enrollment visit 
  rename("TYPE_VISIT" = M01_TYPE_VISIT) %>% 
  # filter out any ultrasound visit dates that are 07-07-1907
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
  ## if LMP < 9 weeks & difference between lmp & us <= 5 days --> use LMP, if difference >5 days --> use US
  ## if LMP <16 weeks and difference between lmp & us <=7 days --> use LMP, if difference >7 days --> use US
  ## if LMP >= 16 weeks and difference between lmp & us <=10 days --> use LMP, if difference >10 days, use US
  ## if no LMP reported --> use US
  ## if no US reported --> use LMP
  
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
                             ifelse(BOE_GA_DAYS_ENROLL == LMP_GA_DAYS_ENROLL, 2, 55)),
         MISSING_USFORMDATE = case_when(is.na(M01_US_OHOSTDAT) ~ 1, TRUE ~ 0)) %>%
  ## QUESTION: do we want this to be weeks + days or just days
  select(SITE, SCRNID, MOMID, PREGID, M01_US_OHOSTDAT, EST_CONCEP_DATE, US_COMPLETE = TYPE_VISIT,
         EST_CONCEP_DATE_US, EST_CONCEP_DATE_LMP, GA_DIFF_DAYS, EDD_BOE, 
         BOE_METHOD, BOE_GA_WKS_ENROLL, BOE_GA_DAYS_ENROLL, US_GA_WKS_ENROLL, 
         US_GA_DAYS_ENROLL, LMP_GA_WKS_ENROLL, LMP_GA_DAYS_ENROLL,
         MISSING_BOTH_US_LMP, MISSING_USFORMDATE) 


#MNH02 ----
mnh02$M02_SCRN_OBSSTDAT <- clean_date(
  mnh02$M02_SCRN_OBSSTDAT,
  min_date = ymd("2018-01-01"),
  max_date = ymd(UploadDate)
)

m02_df <- mnh02 %>% 
  group_by(SCRNID) %>%
  arrange(-desc(M02_SCRN_OBSSTDAT)) %>%
  slice(1) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  select(-n) %>% 
  mutate(MOMID = ifelse(str_detect(MOMID, "n/a"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "n/a"), NA, PREGID)) %>% 
  mutate(MOMID = ifelse(str_detect(MOMID, "N/A"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "N/A"), NA, PREGID)) %>% 
  mutate(MOMID = case_when(MOMID == "" ~ NA, TRUE ~ MOMID),
         PREGID = case_when(PREGID == "" ~ NA, TRUE ~ PREGID),
  ) %>% filter (!SCRNID %in% c("") & !is.na(SCRNID)) %>% 
  mutate(REMAPP_SCRN = case_when(
            SITE == "Ghana"      & M02_SCRN_OBSSTDAT >= ymd("2022-12-28") & M02_SCRN_OBSSTDAT <= ymd("2024-10-29") ~ 1,
            SITE == "Kenya"      & M02_SCRN_OBSSTDAT >= ymd("2023-04-03") & M02_SCRN_OBSSTDAT <= ymd("2025-03-11") ~ 1,
            SITE == "Zambia"     & M02_SCRN_OBSSTDAT >= ymd("2022-12-15") & M02_SCRN_OBSSTDAT <= ymd("2025-03-20") ~ 1,
            SITE == "Pakistan"   & M02_SCRN_OBSSTDAT >= ymd("2022-09-22") & M02_SCRN_OBSSTDAT <= ymd("2024-01-17") ~ 1,
            SITE == "India-CMC"  & M02_SCRN_OBSSTDAT >= ymd("2023-06-20") & M02_SCRN_OBSSTDAT <= ymd("2025-07-01") ~ 1,
            SITE == "India-SAS"  & M02_SCRN_OBSSTDAT >= ymd("2023-12-12") & M02_SCRN_OBSSTDAT <= ymd("2025-03-06") ~ 1,
            TRUE ~ 0
          ),
         SCREEN_DF = 1) %>%
 distinct(SCRNID, SITE, .keep_all = TRUE)

#BIND PRESCRN AND SCRN ----
screen_df <- m00_df %>% 
    full_join(m02_df, by = c("SCRNID", "SITE")) %>% 
    
    ## PRESCREENING LOGIC ----
    mutate (
    PRESCREEN_PREGSIGN = ifelse(M00_PREGNANT_IEORRES == 1, 1,
                             ifelse(M00_PREGNANT_IEORRES == 0, 0, 99)),
    
    ## if you answer 1 to PRESCREEN_PREGSIGN, you answer PRESCREEN_GA25
    PRESCREEN_GA25 = ifelse(M00_EGA_LT25_IEORRES == 1 | (M00_EGA_LT25_IEORRES==77 & SITE == "Pakistan"), 1,
                         ifelse(M00_EGA_LT25_IEORRES == 0 & M00_PREGNANT_IEORRES == 1, 0, 99)),
    ## if you answer 1 to PRESCREEN_PREGSIGN & PRESCREEN_GA25, you answer PRESCREEN_AGE
    PRESCREEN_AGE = ifelse(M00_AGE_IEORRES == 1, 1,
                        ifelse(M00_AGE_IEORRES == 0  & M00_PREGNANT_IEORRES == 1 & 
                                 (M00_EGA_LT25_IEORRES==1 | (M00_EGA_LT25_IEORRES==77 & SITE == "Pakistan")), 0, 99)),
    ## if you answer 1 to PRESCREEN_PREGSIGN & PRESCREEN_GA25 & PRESCREEN_AGE, you answer PRESCREEN_CATCHAREA
    PRESCREEN_CATCHAREA = ifelse(M00_CATCHMENT_IEORRES == 1, 1,
                              ifelse(M00_CATCHMENT_IEORRES == 0 & 
                                       M00_AGE_IEORRES == 1  & 
                                       M00_PREGNANT_IEORRES == 1 & 
                                       (M00_EGA_LT25_IEORRES==1 | (M00_EGA_LT25_IEORRES==77 & SITE == "Pakistan")), 0, 99)),
    ## if you answer 1 to PRESCREEN_PREGSIGN & PRESCREEN_GA25 & PRESCREEN_AGE & PRESCREEN_CATCHAREA, you answer PRESCREEN_OTHER
    PRESCREEN_OTHER = ifelse(M00_OTHR_IEORRES == 0 | (M00_OTHR_IEORRES==77 & SITE == "Pakistan"), 1,
                          ifelse(M00_OTHR_IEORRES == 1 & 
                                   M00_CATCHMENT_IEORRES == 1 & 
                                   M00_AGE_IEORRES == 1  & 
                                   M00_PREGNANT_IEORRES == 1 & 
                                   (M00_EGA_LT25_IEORRES==1 | (M00_EGA_LT25_IEORRES==77 & SITE == "Pakistan")), 0, 99)), 
    ## if you answer 1 to PRESCREEN_PREGSIGN & PRESCREEN_GA25 & PRESCREEN_AGE & PRESCREEN_CATCHAREA & PRESCREEN_OTHER, you answer PRESCREEN_CONSENT
    PRESCREEN_CONSENT = ifelse(M00_CON_YN_DSDECOD == 1 | M00_ASSNT_YN_DSDECOD == 1| 
                               M00_CON_LAR_YN_DSDECOD == 1, 1, 
                               ifelse((M00_CON_YN_DSDECOD == 0 | 
                                         M00_ASSNT_YN_DSDECOD == 0| M00_ASSNT_YN_DSDECOD == 0) & ## consent has "or" statements. if one method did not consent, then the rest are "77"
                                        (M00_CON_LAR_YN_DSDECOD == 0 | M00_CON_LAR_YN_DSDECOD == 77) & 
                                        (M00_OTHR_IEORRES == 0 | (M00_OTHR_IEORRES==77 & SITE == "Pakistan")) & 
                                        M00_CATCHMENT_IEORRES == 1 & 
                                        M00_AGE_IEORRES == 1  & 
                                        M00_PREGNANT_IEORRES == 1 & 
                                        (M00_EGA_LT25_IEORRES==1 | (M00_EGA_LT25_IEORRES==77 & SITE == "Pakistan")), 0, 99))) %>% 
      # eligible based on pre-screening
      mutate(PRESCREEN_ELIGIBLE = ifelse(M00_PREGNANT_IEORRES == 1 & 
                                        (M00_EGA_LT25_IEORRES == 1 | (M00_EGA_LT25_IEORRES==77 & SITE == "Pakistan")) & 
                                        M00_AGE_IEORRES == 1 &
                                        M00_CATCHMENT_IEORRES == 1 & 
                                        (M00_OTHR_IEORRES == 0 | (M00_OTHR_IEORRES==77 & SITE == "Pakistan")) & 
                                        PRESCREEN_CONSENT == 1, 1,
                                      ifelse(PRESCREEN_PREGSIGN == 0 | PRESCREEN_GA25 == 0 | PRESCREEN_AGE == 0 |
                                               PRESCREEN_CATCHAREA == 0 | PRESCREEN_OTHER == 0 | PRESCREEN_CONSENT == 0, 0, 99)
      )) %>% #reason for exclusion in screen/enroll
  ## SCREENING CRITERIA -----
    mutate(
      SCRN_RETURN = case_when(
        M02_SCRN_RETURN == 0 ~ 0,   # didn't return / missing prescreening form
        SCREEN_DF == 1 &  !is.na(M02_SCRN_RETURN) ~ 1,
        TRUE ~ 99                   
      ),
      
      SCREEN_AGE = ifelse(
        M02_AGE_IEORRES == 1, 1,
        ifelse(M02_AGE_IEORRES == 0 & SCRN_RETURN == 1, 0, 99)
      ),
      
      SCREEN_GA20 = ifelse(
        M02_PC_IEORRES == 1, 1,
        ifelse(M02_PC_IEORRES == 0 &
                 M02_AGE_IEORRES == 1 &
                 SCRN_RETURN == 1, 0, 99)
      ),
      
      SCREEN_CATCHAREA = ifelse(
        M02_CATCHMENT_IEORRES == 1, 1,
        ifelse(M02_CATCHMENT_IEORRES == 0 &
                 M02_PC_IEORRES == 1 &
                 M02_AGE_IEORRES == 1 &
                 SCRN_RETURN == 1, 0, 99)
      ),
      
      SCREEN_CATCHREMAIN = ifelse(
        M02_CATCH_REMAIN_IEORRES == 1, 1,
        ifelse(M02_CATCH_REMAIN_IEORRES == 0 &
                 M02_CATCHMENT_IEORRES == 1 &
                 M02_PC_IEORRES == 1 &
                 M02_AGE_IEORRES == 1 &
                 SCRN_RETURN == 1, 0, 99)
      ),
      
      SCREEN_CONSENT = ifelse(
        M02_CONSENT_IEORRES == 1, 1,
        ifelse(M02_CONSENT_IEORRES == 0 &
                 M02_CATCH_REMAIN_IEORRES == 1 &
                 M02_CATCHMENT_IEORRES == 1 &
                 M02_PC_IEORRES == 1 &
                 M02_AGE_IEORRES == 1 &
                 SCRN_RETURN == 1, 0, 99)
      )
    ) %>%
    ## DERIVED FLAGS -----
  mutate(
    PRESCREEN = case_when(PRESCREEN_DF == 1 ~ 1, TRUE ~ 0),
    GAP_CONSENT = ifelse(M00_OTHR_IEORRES == 0 & M00_CON_YN_DSDECOD == 77, 1, 0),
    SCREEN = case_when(SCREEN_DF == 1 ~ 1, TRUE ~ 0),
    ## SCREENING CRITERIA
    ## eligible & enrolled
    ## M02_AGE_IEORRES = meet age requirement?
    ## M02_PC_IEORRES = <20wks gestation?
    ## M02_CATCHMENT_IEORRES = live in catchment area?
    ## M02_CATCH_REMAIN_IEORRES = stay in catchment area?
    ELIGIBLE = case_when(
      SCRN_RETURN == 1 &  SCREEN_AGE == 1 &  SCREEN_GA20 == 1 &  SCREEN_CATCHAREA == 1 &
      SCREEN_CATCHREMAIN == 1 &  SCREEN_CONSENT == 1 ~ 1,
      SCRN_RETURN == 0 |  SCREEN_AGE == 0 |  SCREEN_GA20 == 0 |  SCREEN_CATCHAREA == 0 |
      SCREEN_CATCHREMAIN == 0 |  SCREEN_CONSENT == 0 ~ 0,
      TRUE ~ 99
    )
  ) %>% 
    
    ## DENOMINATORS ----
  mutate(
    PRESCREEN_DENOM = case_when(PRESCREEN == 1 ~ 1, TRUE ~ 0),
    SCREEN_DENOM    = case_when(SCREEN == 1 ~ 1, TRUE ~ 0),
    ENROLL_DENOM    = case_when(ELIGIBLE == 1 ~ 1, TRUE ~ 0)) %>% 
    
    ## RE-ENROLMENT ----
  group_by(SITE, MOMID) %>% 
    mutate(n = n()) %>% 
    mutate(ENROLL_FREQ = case_when(n > 1 ~ 1, TRUE ~ 0)) %>% 
    ungroup() %>% 
    # keep those contributing to prescreen/screen denominators
    filter(PRESCREEN_DENOM == 1 | SCREEN_DENOM == 1)


#BIND SCRN/PRESCRN with ULTRASOUND AND MAT_END ----
screen_df_clean <- screen_df %>% 
  left_join(m01_df %>%  
             select(SITE, SCRNID, US_COMPLETE, BOE_GA_WKS_ENROLL, 
                    BOE_METHOD, M01_US_OHOSTDAT,
                    MISSING_USFORMDATE, MISSING_BOTH_US_LMP, EST_CONCEP_DATE),
             by = c("SITE", "SCRNID")) %>% 
  
  left_join(mat_flow_end , by = c("SITE", "MOMID", "PREGID")) %>% 
  
  mutate(MISSING_USFORM = case_when(is.na(US_COMPLETE) ~ 1, TRUE ~ 0)) %>%
 
   select(SITE, SCRNID, MOMID, PREGID, REMAPP_PRESCRN, REMAPP_SCRN, 
           
          PRESCREEN_DATE = M00_SCRN_OBSSTDAT, SCREEN_DATE = M02_SCRN_OBSSTDAT,
           
          PRESCREEN_PREGSIGN, PRESCREEN_GA25, PRESCREEN_AGE, 
          PRESCREEN_CATCHAREA, PRESCREEN_OTHER, PRESCREEN_CONSENT, PRESCREEN_ELIGIBLE,
          
          US_COMPLETE, BOE_GA_WKS_ENROLL, MISSING_USFORMDATE, MISSING_BOTH_US_LMP,
          EST_CONCEP_DATE, PREG_END_ALIVE, PREG_NOT_END_REASN, M01_US_OHOSTDAT,
          
          SCRN_RETURN,  SCREEN_AGE,  SCREEN_GA20,  SCREEN_CATCHAREA,  SCREEN_CATCHREMAIN, 
          SCREEN_CONSENT, BOE_METHOD,
          
          PRESCREEN, GAP_CONSENT, SCREEN, ELIGIBLE, MISSING_USFORM,
          
          PRESCREEN_DENOM, SCREEN_DENOM, ENROLL_DENOM, ENROLL_FREQ,
          
          STOP_DATE,  CLOSEOUT_DT, CLOSEOUT_GA, CLOSEOUT_DAYS_PP)  %>%
  
  ##Prescreen and Screen Eligibility ----
  mutate(
    PRESCREEN_PASS = case_when(
      (PRESCREEN == 1 & PRESCREEN_ELIGIBLE == 1 | (SCREEN == 1 & ELIGIBLE == 1)) ~ "Yes",
       TRUE ~ "No"
    ),
    SCREEN_PASS = case_when(
      SCREEN == 1 & ELIGIBLE == 1 ~ "Yes",
      TRUE ~ "No"
    ),
    
    SCREEN_DATE = case_when(is.na(SCREEN_DATE) & !is.na(M01_US_OHOSTDAT) ~ M01_US_OHOSTDAT, 
                            TRUE ~ SCREEN_DATE),
    
    MISSING_PRESCRNDATE = case_when(is.na(PRESCREEN_DATE) ~ 1, TRUE ~ 0),
    MISSING_SCRNDATE = case_when(is.na(SCREEN_DATE) ~ 1, TRUE ~ 0)
  ) %>%
  ##Identifying participants with enrolment missing data and issues ----
  mutate(PRISMA_ENROLL = case_when(MISSING_PRESCRNDATE == 0 &
                                      MISSING_SCRNDATE == 0 & 
                                      MISSING_USFORMDATE == 0 & 
                                      MISSING_USFORM == 0 &   
                                      MISSING_BOTH_US_LMP == 0 &
                                      PRESCREEN_PASS == "Yes" &
                                      SCREEN_PASS == "Yes"~ 1, 
                                      TRUE ~ 0)) %>%
  mutate(
    # GA today
    GA_TODAY_DAYS  = as.numeric(as.Date(UploadDate) - EST_CONCEP_DATE),
    GA_TODAY_WEEKS = floor(GA_TODAY_DAYS / 7),
    
    ##Preg end for all enrolled participants ----
    # 77 = not enrolled / not applicable
    # 0  = does not have a pregnancy end
    # 1  = has a pregnancy end (uses PREG_END_ALIVE coding)
    PREG_END_ENROLLED = case_when(
      PRISMA_ENROLL == 0                           ~ 77,              # not enrolled
      !is.na(PREG_END_ALIVE) & PRISMA_ENROLL == 1  ~ PREG_END_ALIVE,   # use existing code
      TRUE                                            ~ 0                # enrolled but no preg end
    ),
    
    # PREG_INCOMPLETE_REASON codes (example):
    # 77 = not enrolled / not applicable
    # 2  = loss to follow-up (>= 42 weeks with no preg end)
    # 5  = no delivery at data end (< 42 weeks, still pregnant)
    # 55 = unknown / other
    PREG_INCOMPLETE_REASON = case_when(
      PRISMA_ENROLL == 0                                  ~ 77,               # not enrolled, N/A
      !is.na(PREG_NOT_END_REASN)                             ~ PREG_NOT_END_REASN,
      is.na(PREG_END_ALIVE) & GA_TODAY_WEEKS >= 45           ~ 2,                # LTFU beyond 42 wks
      is.na(PREG_END_ALIVE) & GA_TODAY_WEEKS < 45            ~ 5,                # no delivery at data end
      TRUE                                                   ~ 55                # unknown
    )) %>%
  ## ReMAPP Variables ----
  mutate(
    REMAPP_ENROLLED = case_when(REMAPP_SCRN == 1 & PRISMA_ENROLL == 1 ~ 1, 
                                REMAPP_SCRN == 1 & PRISMA_ENROLL != 1 ~ 0,
                                TRUE ~ 77),
    REMAPP_PRESCRN  = case_when(REMAPP_SCRN == 1 ~ 1, 
                                REMAPP_PRESCRN == 1 & REMAPP_SCRN != 1 & ELIGIBLE == 1 ~ 0,
                                TRUE ~ REMAPP_PRESCRN)) %>%
  
  ## Prescreenining exclusion reasons ----
  mutate(
    PRESCREEN_EXCL_REASON = case_when(
      PRESCREEN_PREGSIGN == 0 & PRESCREEN_PASS == "No" ~ 1,
      PRESCREEN_CONSENT == 0 & PRESCREEN_PASS == "No" ~ 2,
      PRESCREEN_OTHER == 0 & PRESCREEN_PASS == "No" ~ 3,
      PRESCREEN_GA25 == 0 & PRESCREEN_PASS == "No" ~ 4,
      PRESCREEN_CATCHAREA == 0 & PRESCREEN_PASS == "No" ~ 5,
      PRESCREEN_AGE == 0 & PRESCREEN_PASS == "No" ~ 6,
      PRESCREEN_ELIGIBLE == 99 & PRESCREEN_PASS == "No" ~ 7,
      PRESCREEN != 1 & PRESCREEN_PASS != "Yes" ~ 8,
      PRESCREEN_PASS == "Yes" ~ 77,
      TRUE ~ NA_real_),
    PRESCREEN_EXCL_REASON_LABEL = case_when(
      PRESCREEN_EXCL_REASON == 1  ~ "not pregnant",
      PRESCREEN_EXCL_REASON == 2  ~ "did not consent",
      PRESCREEN_EXCL_REASON == 3  ~ "other site-specified reasons",
      PRESCREEN_EXCL_REASON == 4  ~ "GA >= 25 weeks",
      PRESCREEN_EXCL_REASON == 5  ~ "doesn't live in area",
      PRESCREEN_EXCL_REASON == 6  ~ "doesn't meet age criteria",
      PRESCREEN_EXCL_REASON == 7  ~ "unspecified exclusion criteria",
      PRESCREEN_EXCL_REASON == 8  ~ "incomplete prescreening data",
      PRESCREEN_EXCL_REASON == 77 ~ "passed prescreen",
      TRUE ~ NA_character_)) %>% 
  mutate(
    ## Screenining exclusion reasons ----
      SCREEN_EXCL_REASON = case_when(
        # 2: Did not return for screening
        SCRN_RETURN == 0 &
          SCREEN_PASS == "No" &
          PRESCREEN_PASS == "Yes" ~ 1,
        
        # 1: GA ≥ 20 weeks at screening
        SCREEN_GA20 == 0 &
          SCREEN_PASS == "No" &
          PRESCREEN_PASS == "Yes" ~ 2,
        
        # 3: Withdrew consent at screening
        SCREEN_CONSENT == 0 &
          SCREEN_PASS == "No" &
          PRESCREEN_PASS == "Yes" ~ 3,
        
        # 4: Won’t remain in area
        SCREEN_CATCHREMAIN == 0 &
          SCREEN_PASS == "No" &
          PRESCREEN_PASS == "Yes" ~ 4,
        
        # 5: Catchment area failure
        SCREEN_CATCHAREA == 0 &
          SCREEN_PASS == "No" &
          PRESCREEN_PASS == "Yes" ~ 5,
        
        # 6: Age ineligible at screening
        SCREEN_AGE == 0 &
          SCREEN_PASS == "No" &
          PRESCREEN_PASS == "Yes" ~ 6,
        
        # 7: Missing screening form
        SCREEN_DENOM != 1 &
          SCREEN_PASS == "No" &
          PRESCREEN_PASS == "Yes" ~ 7,
        
        # 8: Unspecified screening exclusion (ELIGIBLE == 99)
        ELIGIBLE == 99 &
          SCREEN_DENOM == 1 &
          SCREEN_PASS == "No" &
          PRESCREEN_PASS == "Yes" ~ 8,
        
        # 9: Missing any screening data
        (MISSING_PRESCRNDATE == 1 |
           MISSING_SCRNDATE == 1 |
           PRESCREEN_DENOM != 1) &
          SCREEN_PASS == "Yes" ~ 9,
        
        # 10: Missing any ultrasound data
        (MISSING_USFORMDATE == 1 |
           MISSING_USFORM == 1 |
           MISSING_BOTH_US_LMP == 1) &
          SCREEN_PASS == "Yes" ~ 10,
        
        # 77: Passed screening (no exclusion reason above applied)
        SCREEN_PASS == "Yes" ~ 77,
        
        TRUE ~ NA_real_),

    SCREEN_EXCL_REASON_LABEL = case_when(
      SCREEN_EXCL_REASON == 2  ~ "GA >= 20wks",
      SCREEN_EXCL_REASON == 1  ~ "did not return for screening",
      SCREEN_EXCL_REASON == 3  ~ "withdrew consent",
      SCREEN_EXCL_REASON == 4  ~ "won't remain in area",
      SCREEN_EXCL_REASON == 5  ~ "doesn't live in area",
      SCREEN_EXCL_REASON == 6  ~ "doesn't meet age criteria",
      SCREEN_EXCL_REASON == 7  ~ "missing screening form",
      SCREEN_EXCL_REASON == 8  ~ "other unspecified reasons",
      SCREEN_EXCL_REASON == 9  ~ "missing any screening data",
      SCREEN_EXCL_REASON == 10 ~ "missing any ultrasound data",
      SCREEN_EXCL_REASON == 77 ~ "passed screening",
      TRUE ~ NA_character_
    )) %>%
  mutate(
    ELIGIBLE_EXCL_REASON = case_when(
      PREG_INCOMPLETE_REASON == 1 &
        PRISMA_ENROLL == 1 ~ 1,   # maternal death
      
      PREG_INCOMPLETE_REASON == 2 &
        PRISMA_ENROLL == 1 ~ 2,   # lost to follow-up
      
      PREG_INCOMPLETE_REASON == 3 &
        PRISMA_ENROLL == 1 ~ 3,   # withdrew prior to delivery
      
      PREG_INCOMPLETE_REASON == 4 &
        PRISMA_ENROLL == 1 ~ 4,   # terminated from study
      
      PREG_INCOMPLETE_REASON == 5 &
        PRISMA_ENROLL == 1 ~ 5,   # no delivery at data end
      
      PREG_INCOMPLETE_REASON == 55 &
        PRISMA_ENROLL == 1 ~ 55,  # other unspecified reasons
      
      TRUE ~ NA_real_
    ),
    ELIGIBLE_EXCL_REASON_LABEL = case_when(
      ELIGIBLE_EXCL_REASON == 1  ~ "maternal death",
      ELIGIBLE_EXCL_REASON == 2  ~ "lost to follow-up",
      ELIGIBLE_EXCL_REASON == 3  ~ "withdrew prior to delivery",
      ELIGIBLE_EXCL_REASON == 4  ~ "terminated from study",
      ELIGIBLE_EXCL_REASON == 5  ~ "no delivery at data end",
      ELIGIBLE_EXCL_REASON == 55 ~ "other unspecified reasons",
      TRUE ~ NA_character_
    )
  ) %>%
  distinct(SITE, SCRNID, MOMID, PREGID, .keep_all = TRUE)


## Screening and prescreening query----
Datequery <- screen_df_clean %>% 
  filter(!is.na(SCREEN_DATE) & !is.na(PRESCREEN_DATE) & !is.na(EST_CONCEP_DATE)) %>%
  mutate(
    # time between visits
    DATE_DIFF = as.numeric(SCREEN_DATE - PRESCREEN_DATE),
    
    # GA (in days) at each visit (visit date minus estimated conception date)
    GA_PRESCREEN_DAYS = as.numeric(PRESCREEN_DATE - EST_CONCEP_DATE),
    GA_SCREEN_DAYS    = as.numeric(SCREEN_DATE    - EST_CONCEP_DATE),
    
    # helper: GA at prescreening is "not reasonable"
    GA_PRESCREEN_NOT_REASONABLE =
      GA_PRESCREEN_DAYS < 0 | GA_PRESCREEN_DAYS < 28,  # negative or < 6 weeks
    
    # your query rules
    QUERY_FLAG = case_when(
      DATE_DIFF < 0                            ~ "Prescreen and screen date difference is negative",  # negative interval
      GA_PRESCREEN_DAYS < 0                    ~ "Gestational age at prescreening is negative",  
      GA_SCREEN_DAYS < 0                       ~ "Gestational age at screening is negative", 
      DATE_DIFF > 126 & SCREEN_PASS == "Yes"   ~ "Prescreen and screen date difference greater than 126days",  # outside 7–130 window (upper)
      DATE_DIFF >= 0 & DATE_DIFF <= 130 & GA_PRESCREEN_DAYS < 0  ~ "Gestational age at prescreening < 0",
      DATE_DIFF >= 0 & DATE_DIFF <= 130 & GA_PRESCREEN_DAYS < 28 ~ "Gestational age at prescreening < 4 weeks",
      TRUE ~ "No query"
    ),
    `SITE COMMENT` = ""
  ) %>%
  filter(QUERY_FLAG != "No query") %>%
  select(SITE, SCRNID, MOMID, PREGID, EST_CONCEP_DATE, 
         PRESCREEN_DATE, SCREEN_DATE,
         GA_PRESCREEN_DAYS, GA_SCREEN_DAYS, 
         PRESCREEN_PASS, SCREEN_PASS,  
         DATE_DIFF, QUERY_FLAG, `SITE COMMENT`) 

path_to_save <- path_to_save <- paste0("~/Analysis/Flowchart/", UploadDate)
# Expand the path (in case of ~)
path_to_save <- path.expand(path_to_save)
# Create the directory if it doesn't exist
if (!file.exists(path_to_save)) {
  dir.create(path_to_save, recursive = TRUE)
}

library(writexl)
# Write one spreadsheet per SITE
split(Datequery, Datequery$SITE) %>%
  lapply(function(df_site) {
    write_xlsx(
      df_site,
      paste0(path_to_save, "/Screen_Date_Queries_", UploadDate, "_", unique(df_site$SITE), ".xlsx")
    )
  })


table (Datequery$QUERY_FLAG, Datequery$SITE)
flowchart_df <- screen_df_clean %>% 
  select(-GA_TODAY_DAYS, -GA_TODAY_WEEKS, -GAP_CONSENT, -PREG_END_ALIVE, 
         -PREG_INCOMPLETE_REASON, -EST_CONCEP_DATE)

# SAVE FILE -----
write.csv(flowchart_df, paste0(path_to_tnt, "/MAT_FLOWCHART" ,".csv"), na="", row.names=FALSE)
