#*****************************************************************************
#*Depression (MNH25) Analysis
#*****************************************************************************
#*
#*#*****************************************************************************
rm(list = ls())

library(tidyverse)
library(naniar)

#read data
uploadDate = "2023-10-13"
mnh25 <- read.csv(paste0("Z:/Stacked Data/",uploadDate,"/mnh25_merged.csv"))

#*****************************************************************************
#prepare data 
df_depress <- mnh25 %>% 
  #remove not required visit
  filter(M25_TYPE_VISIT %in% c(1,2,4,10)) %>% 
  mutate(
    dep_score = M25_EPDS01_SCORRES, 
    dep_lh = case_when(
      SITE == "Kenya" & dep_score >= 13 ~ 1, #Possible Depression 
      SITE == "Ghana" & dep_score >= 11 ~ 1,
      SITE == "Pakistan" & dep_score >= 14 ~ 1,
      SITE == "India-CMC" & dep_score >= 8 ~ 1,
      SITE == "India-SAS" & dep_score >= 10 ~ 1,
      SITE == "Zambia" & dep_score >= 10 ~ 1, 
      dep_score >= 0 ~ 0, #Not Likely Depression 
      TRUE ~ -5
    ),
    #dep_lh2 is for cross checking the existing variable values
    dep_lh2 = case_when(
      SITE == "Kenya" ~ M25_EPDS01_CAT_SCORRES, #Possible Depression 
      SITE == "Ghana" ~ M25_EPDS01_CAT_SCORRES,
      SITE == "Pakistan" ~ M25_EPDS01_CAT_SCORRES,
      SITE == "India-CMC" & dep_score >= 8 ~ 1,
      SITE == "India-SAS" & dep_score >= 10 ~ 1,
      SITE == "Zambia" & dep_score >= 10 ~ 1, 
      dep_score >= 0 ~ 0, #Not Likely Depression 
      TRUE ~ -5
    ),
    suicidal = case_when(
      SITE != "Zambia" ~ M25_EPDS0110_SCORRES, 
      SITE == "Zambia" & M25_EPDS0110 %in% c(1,2) ~ 1, #yes
      SITE == "Zambia" & M25_EPDS0110 %in% c(3,4) ~ 0, #no
      TRUE ~ -5
    ),
    visit_window = case_when(
      M25_TYPE_VISIT %in% c(1,2) ~ 1, #combine enrollment and ANC-20
      M25_TYPE_VISIT == 4 ~ 2, 
      M25_TYPE_VISIT == 10 ~ 3, 
      TRUE ~ -5
    )
    )

df_depress$suicidal <- factor(
  df_depress$suicidal, 
  levels = c(1,0,-5),
  labels = c("Yes","No","NA"))

df_depress$visit_window <- factor(
  df_depress$visit_window, 
  levels = c(1,2,3),
  labels = c("ANC-20", "ANC-32", "PNC-6"))

#save data
save(df_depress, file = "derived_data/df_depress.rda")


