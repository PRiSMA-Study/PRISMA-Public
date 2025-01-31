### Neonatal sepsis

## Goals 
  # pull from sepsis check boxes 
  # pull from positive cultures 
  # pull from VA 
  # pull timing variables (within 72 hours)
  # plot side by side with PSBI 

path_to_data = "~/import/2025-01-10"
path_to_tnt = "Z:/Outcome Data/2025-01-10"

## Data import ---- 
mnh01 <- read.csv(paste0(path_to_data,"/", "mnh01_merged.csv"))
mnh11 <- read.csv(paste0(path_to_data,"/", "mnh11_merged.csv"))
mnh13 <- read.csv(paste0(path_to_data,"/", "mnh13_merged.csv"))
mnh14 <- read.csv(paste0(path_to_data,"/", "mnh14_merged.csv"))
mnh15 <- read.csv(paste0(path_to_data,"/", "mnh15_merged.csv"))
mnh20 <- read.csv(paste0(path_to_data,"/", "mnh20_merged.csv"))
mnh24 <- read.csv(paste0(path_to_data,"/", "mnh24_merged.csv"))

# inf_baseline ## pulled from infant constructed outcomes code
inf_outcomes <- read.xlsx("Z:/Outcome Data/2025-01-10/INF_OUTCOMES.xlsx")

# inf_baseline_sub <- inf_baseline %>% select(SITE, MOMID, PREGID, INFANTID )
inf_outcomes <- read.xlsx("Z:/Outcome Data/2025-01-10/INF_OUTCOMES.xlsx")
mat_enroll <- read.xlsx(paste0(path_to_tnt, "/MAT_ENROLL.xlsx")) 

inf_outcomes_sub <- inf_outcomes %>% select(SITE, MOMID, PREGID, INFANTID,
                                            contains("PSBI"))


# mnh28 & mnh29 
## include VA forms (or precious COD)

# Forms needed: 
  # MNH11, MNH13, MNH20
## Checkboxes: 
  # MNH11, MNH13, MNH20
## Blood culture:
  # MNH11, MNH13, MNH20

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

## Merge together ----
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
  
## check for duplicates (if there are duplicates or positives at multiple visits present, this will cause issues in merging later)
test <- mnh13_subset_pos %>% group_by(INFANTID) %>% mutate(n=n()) %>% filter(n>1)

inf_outcomes_merge <- inf_outcomes_sub %>% 
  left_join(mnh11_subset, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  left_join(mnh13_subset_pos, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  left_join(mnh20_subset, by = c("SITE", "MOMID", "PREGID", "INFANTID"))

## Start constructing ----
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

# subset_dates <- subset_new  %>% 
#   mutate(N_CHECK = rowSums(!is.na(across(c(SEPSIS_CHECK_DATE_M11, SEPSIS_CHECK_DATE_M13, SEPSIS_CHECK_DATE_M20))))) %>% 
#   mutate(N_CULTURE = rowSums(!is.na(across(c(POSITIVE_CULTURE_DATE_M11, POSITIVE_CULTURE_DATE_M13, POSITIVE_CULTURE_DATE_M20)))))  %>% 
#  select(
#       SITE, MOMID, PREGID, INFANTID,SEPSIS_CHECK,N_CULTURE, SEPSIS_CHECK_DATE,SEPSIS_CHECK_DATE_M11, SEPSIS_CHECK_DATE_M13, SEPSIS_CHECK_DATE_M20,
#       PSBI_DX_DATE,POSITIVE_CULTURE,  POSITIVE_CULTURE_DATE, POSITIVE_CULTURE_DATE_M11, POSITIVE_CULTURE_DATE_M13, POSITIVE_CULTURE_DATE_M20,
#       SEPSIS_CULTURE_CONFIRMED
#     )
# 
# subset_dates_test <- subset_dates %>% filter(N_CULTURE>1)

subset <- inf_outcomes_sepsis %>% 
  ## only want subset of positives 
  filter(SEPSIS_CHECK==1 | POSITIVE_CULTURE==1)  %>% 
  # filter(SEPSIS_CULTURE_CONFIRMED %in% c(1,2,3,15,25,35)) %>%
  select(SITE, MOMID, PREGID, INFANTID,SEPSIS_CHECK_DATE, POSITIVE_CULTURE_DATE,
         SEPSIS_CHECK, POSITIVE_CULTURE, SEPSIS_CULTURE_CONFIRMED, M11_INFANT_MHTERM_10,
         M13_INFANT_MHTERM_10, M20_INFECTION_MHTERM_1, M11_CULTURE_LBORRES, M13_BLD_CULT_LBORRES, M20_BLD_CULT_LBORRES
         ) 
# test <- inf_outcomes_sepsis %>% select(SITE, MOMID, PREGID,INFANTID,SEPSIS_CHECK, M11_INFANT_MHTERM_10,
#                                        M13_INFANT_MHTERM_10, M20_INFECTION_MHTERM_1, SEPSIS_CHECK_DATE_M11, 
#                                        SEPSIS_CHECK_DATE_M13, SEPSIS_CHECK_DATE_M20, SEPSIS_CHECK_DATE)



## merge in psbi data 
subset_new <- subset %>% 
  left_join(psbi_outcome_wide , by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  relocate(c("DOB", "VISIT_DATE"), .after = "POSITIVE_CULTURE_DATE") %>% 
  rename(PSBI_DX_DATE = VISIT_DATE) %>% 
  # generate new variable if PSBI_DX_DATE is close to the checkbox date (using 2 weeks as indicator)
  mutate(PSBI_DX_DATE_CHECKBOX = case_when(abs(ymd(SEPSIS_CHECK_DATE)-PSBI_DX_DATE) <= 14~ 1, TRUE ~ 0)) %>% 
  # generate new variable if PSBI_DX_DATE is close to the culture date 
  mutate(PSBI_DX_DATE_CULTURE= case_when(abs(ymd(POSITIVE_CULTURE_DATE)-PSBI_DX_DATE) <= 14 ~ 1, TRUE ~ 0))  %>% 
  # generate variable for any PSBI 
  mutate(INF_PSBI_ANY = case_when(INF_PSBI_IPC==1 | INF_PSBI_PNC0 ==1 | INF_PSBI_PNC1 ==1 | INF_PSBI_PNC4 ==1 | 
                                    INF_PSBI_PNC6 ==1~ 1, TRUE ~ 0)) %>% 
  select(SITE, MOMID, PREGID, INFANTID, DOB, SEPSIS_CHECK,POSITIVE_CULTURE,
         SEPSIS_CHECK_DATE, POSITIVE_CULTURE_DATE, PSBI_DX_DATE, 
         PSBI_DX_DATE_CHECKBOX, PSBI_DX_DATE_CULTURE, AGE_AT_VISIT,
         M11_INFANT_MHTERM_10, M13_INFANT_MHTERM_10, M20_INFECTION_MHTERM_1, M11_CULTURE_LBORRES, M13_BLD_CULT_LBORRES, M20_BLD_CULT_LBORRES,
         contains("INF_PSBI")
         )


## Left off: i have the date of culture, date of checkbox, date of psbi -- next step is to identify any cases where these things happened concurrently 
## Variables needed for tables 
# N sepsis cases (checkbox or culture confirmed) NEO_SEPSIS ==1
# N sepsis cases with confirmed culture POSITIVE_CULTURE ==1 
# N sepsis cases with reported PSBI NEO_SEPSIS_PSBI ==1

neo_sepsis <- subset_new %>% 
  right_join(inf_outcomes %>% select(SITE, MOMID, PREGID, INFANTID, LIVEBIRTH, ADJUD_NEEDED), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  # generate age at sepsis dx
  mutate(SEPSIS_DATE = pmin(SEPSIS_CHECK_DATE, POSITIVE_CULTURE_DATE, na.rm = TRUE )) %>% 
  mutate(NEO_SEPSIS_AGE =  as.numeric(ymd(SEPSIS_DATE)- ymd(DOB))) %>% 
  # generate indicator variable if age at sepsis dx is >28 days 
  mutate(NEO_SEPSIS_AGE_GREATER28 = case_when(NEO_SEPSIS_AGE >=28 ~ 1, 
                                              NEO_SEPSIS_AGE >= 0 & NEO_SEPSIS_AGE<28 ~ 0, 
                                              TRUE ~ 55)) %>% 
  # generate neonatal sepsis variable 
  mutate(NEO_SEPSIS = case_when(LIVEBIRTH ==1 & NEO_SEPSIS_AGE >= 0 & NEO_SEPSIS_AGE<28 & (SEPSIS_CHECK==1 | POSITIVE_CULTURE==1) ~ 1, # if livebirth and <28 days of age at dx and either checkbox or culture reported, ==1
                                
                                LIVEBIRTH ==1 & NEO_SEPSIS_AGE >=28 & 
                                  (SEPSIS_CHECK==1 | POSITIVE_CULTURE==1) ~ 0, # if live birth with sepsis, but age at sepsis is >= 28, then no neonatal sepsis
                                
                                LIVEBIRTH ==1 & ((SEPSIS_CHECK==0 & POSITIVE_CULTURE==0) | 
                                                                                            is.na(SEPSIS_CHECK) & is.na(POSITIVE_CULTURE)) ~ 0, # if livebirth and checkbox or culture negative, ==0
                                LIVEBIRTH == 0  | ADJUD_NEEDED ==1 | LIVEBIRTH == 55 | is.na(LIVEBIRTH)  ~ 77, # if no livebirth, ==7 NA
                                TRUE ~ 55)) %>% 
  # generate variable with concurrent PSBI 
  mutate(NEO_SEPSIS_PSBI = case_when(NEO_SEPSIS==1 & NEO_SEPSIS_AGE >= 0 & NEO_SEPSIS_AGE<28 & INF_PSBI_ANY == 1 &
                                       (PSBI_DX_DATE_CHECKBOX==1 | PSBI_DX_DATE_CULTURE==1) ~ 1, # if sepsis and psbi dx ocurred within 14 days of eachother, ==1
                                     LIVEBIRTH==0 | ADJUD_NEEDED ==1 | NEO_SEPSIS==0 | LIVEBIRTH == 55 | 
                                       is.na(LIVEBIRTH) | NEO_SEPSIS_AGE >=28 ~ 77, # if no sepsis, ==77 NA
                                     NEO_SEPSIS==1 &  NEO_SEPSIS_AGE >= 0 & NEO_SEPSIS_AGE<28 & ((PSBI_DX_DATE_CHECKBOX==0 & PSBI_DX_DATE_CULTURE==0) | is.na(PSBI_DX_DATE_CHECKBOX) & is.na(PSBI_DX_DATE_CULTURE) | INF_PSBI_ANY ==0) ~ 0, # if sepsis but no concurrent psbi with 14 days, ==0,
                                     TRUE ~ 55
                                     )) %>% 
  ## generate cleaner variable for culture confirmed 
  mutate(NEO_SEPSIS_CULTURE = case_when(NEO_SEPSIS==1 & POSITIVE_CULTURE==1 ~ 1, 
                                        NEO_SEPSIS==1 & POSITIVE_CULTURE==0 ~ 0,
                                        NEO_SEPSIS==0 | NEO_SEPSIS == 77 ~ 77,
                                        TRUE ~ 55
                                        ))  

table(neo_sepsis$NEO_SEPSIS)
table(neo_sepsis$NEO_SEPSIS_PSBI)

test <- neo_sepsis %>% 
filter(NEO_SEPSIS==1) %>% 
  select(SITE, INFANTID, ADJUD_NEEDED, LIVEBIRTH, NEO_SEPSIS, NEO_SEPSIS_PSBI, contains("INF_PSBI"), AGE_AT_VISIT)

# select(INFANTID, ADJUD_NEEDED, LIVEBIRTH, NEO_SEPSIS,NEO_SEPSIS_PSBI, NEO_SEPSIS_CULTURE,
#        NEO_SEPSIS_AGE, NEO_SEPSIS_AGE_GREATER28, SEPSIS_DATE,DOB) %>% 

## Tables ----
# N sepsis cases (checkbox or culture confirmed) NEO_SEPSIS ==1
# N sepsis cases with confirmed culture NEO_SEPSIS_CULTURE ==1 
# N sepsis cases with reported PSBI NEO_SEPSIS_PSBI ==1

neonatal_sepsis_tab <- neo_sepsis %>% ## denominator is anyone with a birth outcome reported 
  ## If India-SAS doesn't have data, add empty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    # data completeness 
    "Denominator" =  paste0(
      format(sum(LIVEBIRTH == 1, na.rm = TRUE), nsmall = 0, digits = 2)),
    
    "Neonatal sepsis^a^" = paste0(
      format(sum(NEO_SEPSIS == 1 & LIVEBIRTH == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(NEO_SEPSIS == 1 & LIVEBIRTH == 1, na.rm = TRUE)/sum(LIVEBIRTH == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    # Factors 
    "Confirmed positive blood culture^b^" = paste0(
      format(sum(NEO_SEPSIS_CULTURE == 1 & NEO_SEPSIS == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(NEO_SEPSIS_CULTURE == 1 & NEO_SEPSIS == 1, na.rm = TRUE)/sum(NEO_SEPSIS == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Concurrent PSBI^b, c^" = paste0(
      format(sum(NEO_SEPSIS_PSBI == 1 & NEO_SEPSIS == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(NEO_SEPSIS_PSBI == 1 & NEO_SEPSIS == 1, na.rm = TRUE)/sum(NEO_SEPSIS == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")")
    
  ) %>% 
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1)  %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) %>% 
  mutate_all(funs(str_replace(., "NA", "0")))  

neo_sepsis_to_merge <-neo_sepsis %>% select(SITE, MOMID, PREGID, INFANTID, NEO_SEPSIS, NEO_SEPSIS_PSBI, NEO_SEPSIS_CULTURE, NEO_SEPSIS_AGE) %>% 
  full_join(inf_outcomes, by = c("SITE", "MOMID", "PREGID", "INFANTID"))


path_to_save = "D:/Users/stacie.loisate/Documents/PRISMA-Analysis-Stacie/"
write.csv(neo_sepsis_to_merge, paste0(path_to_save, "INF_OUTCOMES-SEPSIS" ,".csv"), row.names=FALSE)
path_to_tnt <- paste0("Z:/Outcome Data/", "2025-01-10", "/")

# # save data set; this will get called into the report
# write.csv(neo_sepsis_to_merge, paste0(path_to_tnt, "INF_OUTCOMES-SEPSIS" ,".csv"), na="", row.names=FALSE)
# write.xlsx(neo_sepsis_to_merge, paste0(path_to_tnt, "INF_OUTCOMES-SEPSIS" ,".xlsx"), na="", rownames=FALSE)

