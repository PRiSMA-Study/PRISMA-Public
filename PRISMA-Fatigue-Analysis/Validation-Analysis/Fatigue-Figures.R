rm(list = ls())

library(knitr)
library(tidyverse)
library(reshape2)
library(lubridate)
library(kableExtra)
library(naniar)
library(RColorBrewer)
library(haven)
library(dplyr)
library(cowplot)

# set path to save 

# Savannah's path: 
path_to_data <- "Z:/Savannah_working_files/Fatigue/" 
path_to_save <- "Z:/Savannah_working_files/Fatigue/output/"

datadate <- "2024-11-18"
today <- "2024 November 22"

fatigue<- read_dta(paste0(path_to_data, "FATIGUE_250.dta"))  %>% 
  rename_all(toupper) %>% filter(SITE != "") 
 
 fatigue <-  fatigue %>% mutate(
    VISTYPE=case_when(
      TYPE_VISIT <= 2 ~ "ANC20",
      TYPE_VISIT == 4 | TYPE_VISIT==5 ~ "ANC32",
      TYPE_VISIT == 10 ~ "PNC6"
    )
  )
 table(fatigue$TYPE_VISIT, fatigue$VISTYPE)

 fatigue_vistype <- ggplot(subset(fatigue, VISTYPE %in% c("ANC20", "ANC32", "PNC6")), 
        aes(x=VISTYPE, y=FATIGUE, fill=VISTYPE) ) +
   geom_jitter( aes(color=VISTYPE), width=0.1,alpha=0.7) + 
   geom_boxplot(alpha=0.8, outlier.shape = NA) + 
   facet_wrap(~SITE,nrow=1) +
   #scale_fill_manual(values=c("#E09F3E" , "#9e2a2b", "#540B0E")) + 
   scale_fill_brewer(palette = "Paired") + 
   scale_color_brewer(palette = "Paired", guide="none") + 
   theme_classic() +
   labs(title = "Fatigue by visit type", tag="") + 
   guides(fill=guide_legend(title="Time")) + 
   theme(legend.position = "right", axis.title.x = element_blank(),
        axis.text.x = element_blank(),axis.ticks.x = element_blank()) 
 
 ggsave(paste0("fatigue_vistype", ".png"), path = path_to_save,
        width = 8, height = 5)
 
 fatigue <-  fatigue %>% 
   mutate(
  HB_STR=case_when(
       HB10 == 0 ~ "Hb>10",
       HB10 == 1 ~ "Hb<10"
                ),
  Parity=case_when(
    PARITY_CAT == 0 ~ "0",
    PARITY_CAT == 1 ~ "1",
    PARITY_CAT == 2 ~ "2+",
    PARITY_CAT == 55 ~ "Miss"),
  DEPR = case_when(
    DEP_SUM<11 ~ "No",
    DEP_SUM>=11 & DEP_SUM<=30 ~ "Yes"
  ),
  CONTINENT = case_when(
    SITE %in% c("Ghana", "Kenya","Zambia") ~ "Africa",
    SITE %in% c("India-CMC", "India-SAS", "Pakistan") ~ "Asia"
  )
  )
 
 ggplot(fatigue, aes(FATIGUE, colour = SITE)) + facet_wrap(~CONTINENT) +
   geom_freqpoly(binwidth = 1,size=0.8) +  
   scale_color_manual(values=c("#b30000", "#7c1158", "#1a53ff","#00b7c7", "#E09F3E" ,"black")) 
 
 
 ggplot(fatigue, aes(FATIGUE,fill=SITE)) + facet_wrap(~SITE) + theme_classic() +
   geom_histogram(binwidth = 1,colour="black") +  guides(fill="none") + 
   scale_fill_manual(values=c("#E09F3E", "#7c1158", "#7c1158","#E09F3E", "#7c1158" ,"#E09F3E")) 
 
 
 table(fatigue$HB_STR, fatigue$HB10)
 table(fatigue$Parity, fatigue$PARITY_CAT, useNA="always")
 table(fatigue$DEP_SUM, fatigue$DEPR, useNA="always")
 table(fatigue$SITE, fatigue$CONTINENT, useNA="always")

 
 ##Fatigue by concurrent hemoglobin
 ggplot(subset(fatigue, HB_STR %in% c("Hb>10", "Hb<11")), 
        aes(x=SITE, y=FATIGUE, fill=HB_STR) ) +
   geom_boxplot(outlier.shape = NA) + 
   #scale_fill_manual(values=c("#E09F3E" , "#9e2a2b", "#540B0E")) + 
   scale_fill_brewer(palette = "Purples") + 
   theme_classic() +
   labs(title = "Fatigue by hemoglobin and site", tag="") + 
   guides(fill=guide_legend(title="Hemoglobin")) + theme(legend.position = "bottom") 
 

 
 hb_site <- ggplot(fatigue, 
        aes(x=HB_LBORRES, y=FATIGUE) ) + 
   geom_point( alpha=0.2) + 
   facet_wrap(~SITE) +
   #Ghana      CMC        SAS          Kenya     Pakistan  Zambia
   scale_color_manual(values=c("#b30000", "#7c1158", "#1a53ff","#00b7c7", "#E09F3E" ,"black")) +
   #scale_color_brewer(palette = "Set2") + 
   geom_smooth(color="red") +   theme_classic() +
   labs(title = "Fatigue over hemoglobin", tag="", caption="") +
   ylab("Fatigue") + xlab("Hemoglobin") +
   guides(fill=guide_legend(title="Site")) + 
   theme(legend.position = "right") + 
   geom_vline(xintercept=11, linetype="dashed",  size=.8) 
 
 ggsave(paste0("hb_site", ".png"), path = path_to_save,
        width = 8, height = 5)
 
 hb_continent <- ggplot(fatigue, 
        aes(x=HB_LBORRES, y=FATIGUE) ) + 
   geom_point( mapping=aes(color=SITE, shape=SITE),alpha=0.5) + 
   facet_wrap(~CONTINENT) +
                                #Ghana      CMC        SAS          Kenya     Pakistan  Zambia
   scale_color_manual(values=c("#b30000", "#7c1158", "#1a53ff","#00b7c7", "#E09F3E" ,"black")) +
   #scale_color_brewer(palette = "Set2") + 
   geom_smooth(color="black") +   theme_classic() +
   labs(title = "Fatigue over hemoglobin", tag="", caption="") +
   ylab("Fatigue") + xlab("Hemoglobin") +
   guides(fill=guide_legend(title="Site")) + 
   theme(legend.position = "right") + 
   geom_vline(xintercept=11, linetype="dashed",  size=.8) 
 
 ggsave(paste0("hb_continent", ".png"), path = path_to_save,
        width = 8, height = 5)
 
 #Fatigue by parity
parity <- ggplot(subset(fatigue, Parity %in% c("0", "1","2+")), 
        aes(x=SITE, y=FATIGUE, fill=Parity) ) +
   geom_boxplot(outlier.shape = NA) + 
   #scale_fill_manual(values=c("#E09F3E" , "#9e2a2b", "#540B0E")) + 
   scale_fill_brewer(palette = "Purple") + 
   theme_classic() +
   labs(title = "Fatigue by parity and site", tag="") + 
   guides(fill=guide_legend(title="Parity")) + theme(legend.position = "bottom") 
 
 ggsave(paste0("parity", ".png"), path = path_to_save,
        width = 8, height = 5)
 
 #Fatigue by depression
 depr_cat <- ggplot(subset(fatigue, DEPR %in% c("No", "Yes")), 
        aes(x=SITE, y=FATIGUE, fill=DEPR) ) +
   geom_boxplot(outlier.shape = NA) + 
   #scale_fill_manual(values=c("#E09F3E" , "#9e2a2b", "#540B0E")) + 
   scale_fill_brewer(palette = "Purple") + 
   theme_classic() +
   labs(title = "Fatigue by depressive symptoms and site", tag="") + 
   guides(fill=guide_legend(title="Depressive symptoms")) + 
   theme(legend.position = "bottom") 
 
 ggsave(paste0("depr_cat", ".png"), path = path_to_save,
        width = 8, height = 5)
 
 depr_cont <- ggplot(fatigue, 
        aes(x=DEP_SUM, y=FATIGUE) ) + 
   geom_point( alpha=0.5) + 
   facet_wrap(~SITE) +
   #Ghana      CMC        SAS          Kenya     Pakistan  Zambia
   scale_color_manual(values=c("#b30000", "#7c1158", "#1a53ff","#00b7c7", "#E09F3E" ,"black")) +
   #scale_color_brewer(palette = "Set2") + 
    geom_smooth(color="red") +   theme_classic() +
   labs(title = "Fatigue over depression", tag="", caption="") +
   ylab("Fatigue") + xlab("Depressive symptoms") +
   guides(fill=guide_legend(title="Site")) + 
   theme(legend.position = "right") + 
   geom_vline(xintercept=11, linetype="dashed",  size=.8) 
 
 ggsave(paste0("depr_cont", ".png"), path = path_to_save,
        width = 8, height = 5)
 
 
