#*****************************************************************************
#* PRISMA Maternal Hemorrhage
#* Last updated: 21 May 2024
 
#The first section, CONSTRUCTED VARIABLES GENERATION, below, the code generates datasets for 
#each form with additional variables that will be used for multiple outcomes. For example, mnh01_constructed 
#is a dataset that will be used for several outcomes. 

## Hemorrhage (antepartum)
# ## required variables & logic
# M04_APH_CEOCCUR_1-5==1 (Current clinical status: antepartum hemorrhage)
# APH_UNSCHED_ANY==1 (Current clinical status: antepartum hemorrhage @ at any unscheduled visit)
# M09_APH_CEOCCUR_6==1 (Did the mother experience antepartum hemorrhage?)
# HEM_HOSP_ANY==1 (specify type of labor/delivery or birth complication: APH or PPH or vaginal bleeding)
# M19_TIMING_OHOCAT==1 (timing of hospitalization = antenatal period)

## Hemorrhage (postpartum)
# ## required variables & logic
# PPH_CEOCCUR==1 [MNH09] (Did mother experience postpartum hemorrhage)
# PPH_ESTIMATE_FAORRES >=500 (Record estimated blood loss)
# PPH_FAORRES_1==1 [MNH09] (Procedures carried out for PPH, Balloon/condom tamponade)
# PPH_FAORRES_2==1 [MNH09] (Procedures carried out for PPH, Surgical interventions)
# PPH_FAORRES_3==1 [MNH09] (Procedures carried out for PPH, Brace sutures)
# PPH_FAORRES_4==1 [MNH09] (Procedures carried out for PPH, Vessel ligation)
# PPH_FAORRES_5==1 [MNH09] (Procedures carried out for PPH, Hysterectomy)
# PPH_FAORRES_88==1 [MNH09] (Procedures carried out for PPH, Other)
# PPH_TRNSFSN_PROCCUR==1 [MNH09] (Did the mother need a transfusion?) OR
# (HEM_HOSP_ANY==1 & M19_TIMING_OHOCAT==2) [MNH09]
# BIRTH_COMPL_MHTERM_1 [MNH12] (Was the mother diagnosed with any of the following birth complications, PPH)

## Hemorrhage (severe postpartum)
# ## required variables & logic
# PPH_CEOCCUR==1 [MNH09] (Did mother experience postpartum hemorrhage)
# PPH_ESTIMATE_FAORRES >=1000 [MNH09] (Record estimated blood loss)
# PPH_FAORRES_1==1 [MNH09] (Procedures carried out for PPH, Balloon/condom tamponade)  
# PPH_FAORRES_2==1 [MNH09] (Procedures carried out for PPH, Surgical interventions) 
# PPH_FAORRES_3==1 [MNH09] (Procedures carried out for PPH, Brace sutures) 
# PPH_FAORRES_4==1 [MNH09] (Procedures carried out for PPH, Vessel ligation) 
# PPH_FAORRES_5==1 [MNH09] (Procedures carried out for PPH, Hysterectomy) 
# PPH_FAORRES_88==1 [MNH09] (Procedures carried out for PPH, Other) 
# PPH_TRNSFSN_PROCCUR==1 [MNH09] (Did the mother need a transfusion?) OR
# (HEM_HOSP_ANY==1 & M19_TIMING_OHOCAT==2) [MNH09]
# BIRTH_COMPL_MHTERM_1 [MNH12] (Was the mother diagnosed with any of the following birth complications, PPH)


## Medications 
# M09_PPH_CMOCCUR_1_6 (Were any of the following medications given to prevent/treat PPH?, Oxytocin)
# M09_PPH_CMOCCUR_2_6 (Were any of the following medications given to prevent/treat PPH?, Misoprostol)
# M09_PPH_CMOCCUR_3_6 (Were any of the following medications given to prevent/treat PPH?, Tranexaminic acid)
# M09_PPH_CMOCCUR_4_6 (Were any of the following medications given to prevent/treat PPH?, Carbetocin)
# M09_PPH_CMOCCUR_5_6 (Were any of the following medications given to prevent/treat PPH?, Methylergonovine)
# M09_PPH_CMOCCUR_6_6 (Were any of the following medications given to prevent/treat PPH?, Carboprost (PGF2-alpha))
# M09_PPH_CMOCCUR_77_6 (Were any of the following medications given to prevent/treat PPH?, Other)
# M09_PPH_CMOCCUR_88_6 (Were any of the following medications given to prevent/treat PPH?, No medications given)
# M09_PPH_CMOCCUR_99_6 (Were any of the following medications given to prevent/treat PPH?, Don't know)

#*****************************************************************************

### data queries 
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(data.table)
library(openxlsx)

## set upload date
UploadDate = "2024-05-17"

## import data
path_to_save <- "D:/Users/stacie.loisate/Documents/PRISMA-Analysis-Stacie/Maternal-Outcomes/data/"
path_to_data = paste0("~/Monitoring Report/data/merged/" ,UploadDate)

mnh04 <- load(paste0(path_to_data,"/", "m04_merged.RData"))
mnh04 <- m04_merged

mnh09 <- load(paste0(path_to_data,"/", "m09_merged.RData"))
mnh09 <- m09_merged

mnh12 <- load(paste0(path_to_data,"/", "m12_merged.RData"))
mnh12 <- m12_merged

mnh19 <- load(paste0(path_to_data,"/", "m19_merged.RData"))
mnh19 <- m19_merged

################################################################################
# data generation
# 1. generate wide dataset with necessary variables from mnh09/mnh04/mnh12
# 2. generate separate dataset with unscheduled visits 
################################################################################

# data prep
mnh04_out <- mnh04 %>% 
  rename(TYPE_VISIT = "M04_TYPE_VISIT") %>%
  rename(M04_VISIT_DATE = "M04_ANC_OBSSTDAT")

# data prep
mnh12_out <- mnh12 %>% 
  rename(TYPE_VISIT = "M12_TYPE_VISIT") %>%
  rename(VISIT_DATE = "M12_VISIT_OBSSTDAT") %>% 
  select(-VISIT_DATE)

# data prep 
mnh09_out <- mnh09 %>% 
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
                M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTDAT_INF4, na.rm = TRUE)) 
  
# merge mnh04, mnh09, and mnh12 together 
hem <- mnh09_out %>% 
  select(SITE, MOMID, PREGID,DOB, M09_MAT_LD_OHOSTDAT, contains("PPH"), M09_APH_CEOCCUR) %>% 
  mutate(TYPE_VISIT = 6) %>% 
  full_join(mnh04_out[c("SITE", "MOMID", "PREGID","TYPE_VISIT","M04_VISIT_DATE", "M04_APH_CEOCCUR")], by = c("SITE", "MOMID", "PREGID","TYPE_VISIT")) %>% 
  full_join(mnh12_out[c("SITE", "MOMID", "PREGID","TYPE_VISIT", "M12_VAG_BLEED_LOSS_ML", "M12_BIRTH_COMPL_MHTERM_1")], 
            by = c("SITE", "MOMID", "PREGID","TYPE_VISIT")) 



# Convert hemorrhage dataset to wide format
# extract smaller datasets by visit type and assign a suffix with the visit type. We can then merge back together 
# labor and delivery (visit type = 6)
hem_ld <- hem %>% filter(TYPE_VISIT==6) %>%
  select(SITE, MOMID, PREGID,DOB, contains("M09")) %>% 
  rename_with(~paste0(., "_", 6), .cols = c(contains("M09")))  %>% 
  distinct(SITE, MOMID, PREGID, .keep_all = TRUE)  

# vector of all visits in the dataset
visit_types_num <- c(1,2,3,4,5,7, 8, 9,10,11,12, 13, 14)
# vector of labels for all visits in the dataset
visit_types_name <- c("enroll", "anc20", "anc28", "anc32", "anc36", 
                      "pnc0", "pnc1", "pnc4",  "pnc6",  "pnc26","pnc52", "unsched_anc", "unsched_pnc")  # Add more visit types if needed

# generate a dataset for each visit type; we want to make the data wide so we separate by visit type here, add a suffix, and then will merge back together
hem_visit_list <- lapply(visit_types_num, function(visit_types_num) {
  hem %>%
    filter(TYPE_VISIT == visit_types_num) %>%
    select(SITE, MOMID, PREGID, contains("M04"), contains("M12")) %>%
    rename_with(~paste0(., "_", visit_types_num), .cols = c(contains("M04"), contains("M12")))
  
})
names(hem_visit_list) <- paste("hem_", visit_types_name, sep = "")

# remove unscheduled for now - deal with those later
remove_names <- c("hem_unsched_anc", "hem_unsched_pnc")
usched_visits_list <- hem_visit_list[remove_names]
list2env(usched_visits_list, envir = .GlobalEnv)


# for unscheduled visits and hospitalization: generate a single variable if a any outcome at an uscheduled visit
hem_unsched_anc <- hem_unsched_anc %>% 
  select(-M12_VAG_BLEED_LOSS_ML_13,-M12_BIRTH_COMPL_MHTERM_1_13) %>%
  mutate(APH_UNSCHED_ANY = case_when(M04_APH_CEOCCUR_13==1~1, TRUE ~ 0)) %>% 
  filter(APH_UNSCHED_ANY==1)

hem_unsched_pnc <- hem_unsched_pnc %>% 
  select(-M04_APH_CEOCCUR_14) %>%
  mutate(PPH_UNSCHED_ANY = case_when(M12_BIRTH_COMPL_MHTERM_1_14==1~1 | 
                                       M12_VAG_BLEED_LOSS_ML_14 >= 500, TRUE ~ 0))%>% 
  filter(PPH_UNSCHED_ANY==1)

mnh19_out <- mnh19 %>% 
  select(SITE, MOMID,  PREGID, M19_TIMING_OHOCAT,M19_VAG_BLEED_CEOCCUR, M19_LD_COMPL_MHTERM_4,
         M19_LD_COMPL_ML, M19_LD_COMPL_MHTERM_5, M19_TX_PROCCUR_1,  M19_OBSSTDAT) %>% 
  mutate(HEM_HOSP_ANY = case_when(M19_VAG_BLEED_CEOCCUR ==1 |
                                    M19_LD_COMPL_MHTERM_4 ==1 |
                                    M19_LD_COMPL_MHTERM_5 == 1 | 
                                    M19_LD_COMPL_ML >= 500 | 
                                    M19_TX_PROCCUR_1 == 1 ~ 1, 
                                  TRUE ~ 0)) %>% 
  ## if a participant has multiple hospitalizations during ANC or PNC, take the earliest incident of APH or PPH
    group_by(MOMID, PREGID) %>% 
    arrange(M19_OBSSTDAT) %>% 
    mutate(
      KEEP_ANC = if_else(M19_TIMING_OHOCAT == 1 & HEM_HOSP_ANY == 1, 1, 0),
      KEEP_PNC = if_else(M19_TIMING_OHOCAT == 2 & HEM_HOSP_ANY == 1, 1, 0)
    ) %>% 
    filter(
      KEEP_ANC == 1 | KEEP_PNC == 1
    ) %>% 
    slice(1) %>% 
    ungroup()

# Remove the specified data frames from the list
hem_visit_list <- hem_visit_list[setdiff(names(hem_visit_list), remove_names)]
hem_visit_list <- c(hem_visit_list, list(hem_ld = hem_ld))


# merge list of all visit type sub-datasets generated above (without unscheduled or hospitalization)
hem_wide <- hem_visit_list %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID")) %>% distinct() %>% 
  relocate(names(hem_ld), .after = PREGID) %>% 
  group_by(SITE, MOMID, PREGID) 


## merge unscheduled and hospitalization information into the wide dataset 
hem_wide_full <- hem_wide %>% 
  full_join(hem_unsched_anc[c("SITE", "MOMID", "PREGID", "APH_UNSCHED_ANY")], by = c("SITE", "MOMID", "PREGID")) %>% 
  full_join(hem_unsched_pnc[c("SITE", "MOMID", "PREGID", "PPH_UNSCHED_ANY")], by = c("SITE", "MOMID", "PREGID")) %>% 
  full_join(mnh19_out[c("SITE", "MOMID", "PREGID", "HEM_HOSP_ANY", "M19_TIMING_OHOCAT")], by = c("SITE", "MOMID", "PREGID"))


## generate outcomes: 
hemorrhage <- hem_wide_full %>% 
  
  ## generate denominator - all participants with a birth reported
  mutate(HEM_DENOM = case_when(!is.na(DOB) ~ 1, TRUE ~ 0) ## denominator is all participants with a birth reported
  ) %>% 
  
  ## 1. Antepartum Hemorrhage
  mutate(HEM_APH = case_when(HEM_DENOM==1 & (M04_APH_CEOCCUR_1==1 | M04_APH_CEOCCUR_2==1 |M04_APH_CEOCCUR_3==1 | M04_APH_CEOCCUR_4==1 | M04_APH_CEOCCUR_5==1 |
                               APH_UNSCHED_ANY==1 | M09_APH_CEOCCUR_6 == 1 |
                               (HEM_HOSP_ANY==1 & M19_TIMING_OHOCAT==1)) ~ 1, TRUE ~ 0)) %>% 
  ## 2. Postpartum Hemorrhage
  mutate(HEM_PPH = case_when(HEM_DENOM == 1 & (M09_PPH_CEOCCUR_6==1 | M09_PPH_FAORRES_1_6==1 | M09_PPH_FAORRES_2_6==1 |
                               M09_PPH_FAORRES_3_6==1 | M09_PPH_FAORRES_4_6==1 |
                               M09_PPH_FAORRES_5_6==1 | M09_PPH_FAORRES_88_6==1 |
                               M09_PPH_TRNSFSN_PROCCUR_6==1 | M09_PPH_ESTIMATE_FAORRES_6 >=500 |
                               M12_VAG_BLEED_LOSS_ML_7>=500 | M12_VAG_BLEED_LOSS_ML_8>=500 | M12_VAG_BLEED_LOSS_ML_9>=500 | M12_VAG_BLEED_LOSS_ML_10>=500 |
                               M12_VAG_BLEED_LOSS_ML_11>=500 | M12_VAG_BLEED_LOSS_ML_12>=500 |
                               M12_BIRTH_COMPL_MHTERM_1_7==1 |  M12_BIRTH_COMPL_MHTERM_1_8==1 | M12_BIRTH_COMPL_MHTERM_1_9==1 | M12_BIRTH_COMPL_MHTERM_1_10==1 |
                               M12_BIRTH_COMPL_MHTERM_1_11==1 | M12_BIRTH_COMPL_MHTERM_1_12==1 |
                               (HEM_HOSP_ANY==1 & M19_TIMING_OHOCAT==2)
                               ) ~ 1, TRUE ~ 0)) %>% 
  
  ## 3. Severe postpartum hemorrhage
  mutate(HEM_PPH_SEV = case_when(HEM_DENOM==1 & (M09_PPH_CEOCCUR_6==1 | M09_PPH_FAORRES_1_6==1 | M09_PPH_FAORRES_2_6==1 |
                                   M09_PPH_FAORRES_3_6==1 | M09_PPH_FAORRES_4_6==1 |
                                   M09_PPH_FAORRES_5_6==1 | M09_PPH_FAORRES_88_6==1 |
                                   M09_PPH_TRNSFSN_PROCCUR_6==1 | M09_PPH_ESTIMATE_FAORRES_6 >=1000 |
                                 # M12_VAG_BLEED_LOSS_ML_7>=1000 | M12_VAG_BLEED_LOSS_ML_8>=1000 | M12_VAG_BLEED_LOSS_ML_9>=1000 | M12_VAG_BLEED_LOSS_ML_10>=1000 |
                                 # M12_VAG_BLEED_LOSS_ML_11>=1000 | M12_VAG_BLEED_LOSS_ML_12>=1000 |
                                   M12_BIRTH_COMPL_MHTERM_1_7==1 |  M12_BIRTH_COMPL_MHTERM_1_8==1 | M12_BIRTH_COMPL_MHTERM_1_9==1 | M12_BIRTH_COMPL_MHTERM_1_10==1 |
                                   M12_BIRTH_COMPL_MHTERM_1_11==1 | M12_BIRTH_COMPL_MHTERM_1_12==1 |
                                   (HEM_HOSP_ANY==1 & M19_TIMING_OHOCAT==2)
                                   ) ~ 1, TRUE ~ 0)) %>% 
  ## 4. Any hemorrhage at any time point
  mutate(HEM_ANY = case_when(HEM_APH==1 | HEM_PPH ==1| HEM_PPH_SEV==1~1, TRUE ~ 0)) %>% 
  select(SITE, MOMID, PREGID,DOB, HEM_DENOM, HEM_APH, HEM_PPH, HEM_PPH_SEV,HEM_ANY,  
         M09_PPH_ESTIMATE_FAORRES_6, contains("M09_PPH_FAORRES"), M09_PPH_TRNSFSN_PROCCUR_6, 
         contains("M09_PPH_CMOCCUR"), M09_PPH_CEOCCUR_6,
         HEM_HOSP_ANY, M19_TIMING_OHOCAT, M09_PPH_PEMETHOD_6, 
         M12_BIRTH_COMPL_MHTERM_1_7, M12_BIRTH_COMPL_MHTERM_1_8, M12_BIRTH_COMPL_MHTERM_1_9, M12_BIRTH_COMPL_MHTERM_1_10, M12_BIRTH_COMPL_MHTERM_1_11, M12_BIRTH_COMPL_MHTERM_1_12)


table(hemorrhage$HEM_DENOM)
table(hemorrhage$HEM_APH)
table(hemorrhage$HEM_PPH)
table(hemorrhage$HEM_PPH_SEV)

# set path to save 
path_to_save <- "D:/Users/stacie.loisate/Documents/PRISMA-Analysis-Stacie/Maternal-Outcomes/data/"

# export data 
write.csv(hemorrhage, paste0(path_to_save, "hemorrhage" ,".csv"), row.names=FALSE)
write.xlsx(hemorrhage, paste0(path_to_save, "hemorrhage" ,".xlsx"),na="", rowNames=FALSE)



hemorrhage_figs <- hemorrhage %>% 
  select(SITE, MOMID, PREGID, M09_PPH_ESTIMATE_FAORRES_6,HEM_PPH, HEM_PPH_SEV) %>% 
  filter(M09_PPH_ESTIMATE_FAORRES_6 > 0) %>% 
  mutate(CUTOFF = case_when(M09_PPH_ESTIMATE_FAORRES_6 >= 1000~ "Severe Hemorrhage(>=1000mL)",
                            M09_PPH_ESTIMATE_FAORRES_6 >= 500 & M09_PPH_ESTIMATE_FAORRES_6 < 1000 ~ "Hemorrhage (>=500mL)",
                            TRUE ~ "No hemorrhage (<500mL)"
  ))

# Define the order of legend labels
hemorrhage_figs$CUTOFF <- factor(hemorrhage_figs$CUTOFF, levels = c("No hemorrhage (<500mL)","Hemorrhage (>=500mL)", 
                                                                    "Severe Hemorrhage(>=1000mL)"))
hemorrhage_fig <- ggplot() + 
  geom_histogram(data = hemorrhage_figs, aes(x = M09_PPH_ESTIMATE_FAORRES_6, fill = CUTOFF),  color = "gray", binwidth = 50) + 
  scale_fill_manual(values = c("darkgreen", "darkorange","darkred"), name = "") + 
  facet_grid(rows = vars(SITE), scales = "free_y") + 
  xlab("Estimated blood loss (mL)") + 
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(0,2000,250), limits = c(0, 2000)) +
  geom_vline(xintercept = 500, linetype = "dashed") +
  geom_vline(xintercept = 1000, linetype = "dashed") + 
  theme_bw() + 
  theme(strip.background=element_rect(fill="white"),
        axis.text.x = element_text(vjust = 1, hjust=0.5),
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) 


ggsave(paste0("hemorrhage_fig", ".pdf"), path = path_to_save,
       width = 8, height = 6)
