#*****************************************************************************
#*FACIT Fatigue Assessment (MNH26) Variable Response Report
#*****************************************************************************
rm(list = ls())

library(tidyverse)
library(naniar)

#read data
uploadDate = "2023-10-13"
mnh26 <- read.csv(paste0("Z:/Stacked Data/",uploadDate,"/mnh26_merged.csv"))

#*****************************************************************************
#prepare data 
prep_ftg <- mnh26 %>% 
  #remove not required visit
  filter(M26_TYPE_VISIT %in% c(1,2,4,10)) %>% 
  mutate(
    Q1 = as.numeric(M26_FTGE_AN2), 
    Q2 = as.numeric(M26_FTGE_HI7), 
    Q3 = as.numeric(M26_FTGE_HI12), 
    Q4 = as.numeric(M26_FTGE_AN1V), 
    Q5 = as.numeric(M26_FTGE_AN3), 
    Q6 = as.numeric(M26_FTGE_AN4),
    Q7 = case_when(
      M26_FTGE_AN5 == 4 ~ 0,
      M26_FTGE_AN5 == 3 ~ 1,
      M26_FTGE_AN5 == 2 ~ 2,
      M26_FTGE_AN5 == 1 ~ 3,
      M26_FTGE_AN5 == 0 ~ 4,
      TRUE ~ as.numeric(M26_FTGE_AN5)
    ),
    Q8 = case_when(
      M26_FTGE_AN7 == 4 ~ 0,
      M26_FTGE_AN7 == 3 ~ 1,
      M26_FTGE_AN7 == 2 ~ 2,
      M26_FTGE_AN7 == 1 ~ 3,
      M26_FTGE_AN7 == 0 ~ 4,
      TRUE ~ as.numeric(M26_FTGE_AN7)
    ),
    Q9 = as.numeric(M26_FTGE_AN8),
    Q10 = as.numeric(M26_FTGE_AN12),
    Q11 = as.numeric(M26_FTGE_AN14),
    Q12 = as.numeric(M26_FTGE_AN15),
    Q13 = as.numeric(M26_FTGE_AN16
  )) %>% 
  select(MOMID, PREGID, SITE, M26_TYPE_VISIT, starts_with("Q")) %>%
  #replace 66 and 77 with NA
  replace_with_na_all(condition = ~. %in% c(66,77,-5)) 

#ftg data
df_ftg <- prep_ftg %>% 
    rowwise() %>%
    mutate(
      n_answered = sum(across(starts_with("Q"), ~.x %in% c(0:4)), na.rm = TRUE),
      ftg_score = round(sum(4-c_across(starts_with("Q")), na.rm = TRUE)*13/n_answered)
     ) %>%
    ungroup() %>%
  select(MOMID, PREGID, SITE, M26_TYPE_VISIT, starts_with("Q"), n_answered, ftg_score)

#1. tired
df_ftg$Q1 <- factor(
  df_ftg$Q1, 
  levels = c(66, 4, 3, 2, 1, 0),
  labels = c("Refused to answer", 
             "Very much", "Quite a bit", "Somewhat", "A little bit", 
             "Not at all"))

#2. fatigued
df_ftg$Q2 <- factor(
  df_ftg$Q2, 
  levels = c(66, 4, 3, 2, 1, 0),
  labels = c("Refused to answer", 
             "Very much", "Quite a bit", "Somewhat", "A little bit", 
             "Not at all"))

#3. weak
df_ftg$Q3 <- factor(
  df_ftg$Q3, 
  levels = c(66, 4, 3, 2, 1, 0),
  labels = c("Refused to answer", 
             "Very much", "Quite a bit", "Somewhat", "A little bit", 
             "Not at all"))

#4. listless
df_ftg$Q4 <- factor(
  df_ftg$Q4, 
  levels = c(66, 4, 3, 2, 1, 0),
  labels = c("Refused to answer", 
             "Very much", "Quite a bit", "Somewhat", "A little bit", 
             "Not at all"))

#5. troulbe staring things because tired
df_ftg$Q5 <- factor(
  df_ftg$Q5, 
  levels = c(66, 4, 3, 2, 1, 0),
  labels = c("Refused to answer", 
             "Very much", "Quite a bit", "Somewhat", "A little bit", 
             "Not at all"))

#6. troulbe finishing things because tired
df_ftg$Q6 <- factor(
  df_ftg$Q6, 
  levels = c(66, 4, 3, 2, 1, 0),
  labels = c("Refused to answer", 
             "Very much", "Quite a bit", "Somewhat", "A little bit", 
             "Not at all"))

#7. have energy --> no energy
df_ftg$Q7 <- factor(
  df_ftg$Q7, 
  levels = c(66, 4, 3, 2, 1, 0),
  labels = c("Refused to answer", 
             "Very much", "Quite a bit", "Somewhat", "A little bit", 
             "Not at all"))

#8. able to do usual activities --> unable to do usual activities
df_ftg$Q8 <- factor(
  df_ftg$Q8, 
  levels = c(66, 4, 3, 2, 1, 0),
  labels = c("Refused to answer", 
             "Very much", "Quite a bit", "Somewhat", "A little bit", 
             "Not at all"))

#9. need to sleep during the day
df_ftg$Q9 <- factor(
  df_ftg$Q9, 
  levels = c(66, 4, 3, 2, 1, 0),
  labels = c("Refused to answer", 
             "Very much", "Quite a bit", "Somewhat", "A little bit", 
             "Not at all"))

#10. too tired to eat
df_ftg$Q10 <- factor(
  df_ftg$Q10, 
  levels = c(66, 4, 3, 2, 1, 0),
  labels = c("Refused to answer", 
             "Very much", "Quite a bit", "Somewhat", "A little bit", 
             "Not at all"))

#11. need help doing usual activities
df_ftg$Q11 <- factor(
  df_ftg$Q11, 
  levels = c(66, 4, 3, 2, 1, 0),
  labels = c("Refused to answer", 
             "Very much", "Quite a bit", "Somewhat", "A little bit", 
             "Not at all"))

#12. frustrated by being too tired to do things
df_ftg$Q12 <- factor(
  df_ftg$Q12, 
  levels = c(66, 4, 3, 2, 1, 0),
  labels = c("Refused to answer", 
             "Very much", "Quite a bit", "Somewhat", "A little bit", 
             "Not at all"))

#13. have to limit social activity because tired
df_ftg$Q13 <- factor(
  df_ftg$Q13, 
  levels = c(66, 4, 3, 2, 1, 0),
  labels = c("Refused to answer", 
             "Very much", "Quite a bit", "Somewhat", "A little bit", 
             "Not at all"))

df_ftg$M26_TYPE_VISIT <- factor(
  df_ftg$M26_TYPE_VISIT, 
  levels = c(1, 2, 4, 10),
  labels = c("Enroll", "ANC-20", "ANC-32", "PNC-6"))

save(df_ftg, file = "derived_data/df_ftg.rda")
#******************************************************************************
#prepare long data
df_ftg_long <- df_ftg %>% 
  pivot_longer(cols = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", 
                        "Q9", "Q10", "Q11", "Q12", "Q13"), 
               names_to = "Variable", 
               values_to = "Value")

df_ftg_long$Variable <- factor(
  df_ftg_long$Variable,
  levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", 
             "Q9", "Q10", "Q11", "Q12", "Q13"))

#save data
save(df_ftg_long, file = "derived_data/df_ftg_long.rda")




