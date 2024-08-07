---
title: "<span style='font-size: 18px'> <span style='text-align: center'> PRISMA-Infant-Outcomes (Issued: 2024 May 21)"
output:
  pdf_document:
    toc: no
    toc_depth: 4
    latex_engine: xelatex
    keep_tex: true
include-in-header:
- \usepackage{ booktabs,longtable}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, out.width = "100%") 
knitr::opts_chunk$set(out.width = "100%", fig.align = "center")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

#*****************************************************************************
#* PRISMA Infant Outcomes -- TABLES 
#* Drafted: 01 January 2024, Stacie Loisate
#* Last updated: 13 May 2024
#*****************************************************************************
library(knitr)
library(tidyverse)
library(reshape2)
library(lubridate)
library(kableExtra)
library(emo)
library(naniar)
library(RColorBrewer)
library(gt) ## for table gen
library(webshot2)  ## for table gen


path_to_data <- "D:/Users/stacie.loisate/Box/PRISMA-Analysis/Infant-Constructed-Variables/data/"
path_to_save <- "D:/Users/stacie.loisate/Box/PRISMA-Analysis/Infant-Constructed-Variables/output/"

lowbirthweight <- read.csv(paste0(path_to_data, "lowbirthweight.csv")) 
preterm_birth <- read.csv(paste0(path_to_data, "preterm_birth.csv")) 
sga <- read.csv(paste0(path_to_data, "sga.csv")) 
mortality <- read.csv(paste0(path_to_data, "mortality.csv")) 
infant_mortality  <- read.csv(paste0(path_to_data, "infant_mortality.csv")) 
neonatal_mortality <- read.csv(paste0(path_to_data, "neonatal_mortality.csv")) 
stillbirth <- read.csv(paste0(path_to_data, "stillbirth", ".csv")) 
birth_asphyxia <- read.csv(paste0(path_to_data, "birth_asphyxia", ".csv")) 
fetal_death <- read.csv(paste0(path_to_data, "fetal_death", ".csv")) 
hyperbili_all_crit <- read.csv(paste0(path_to_data, "hyperbili_all_crit", ".csv")) 
infant_outcomes <- read.csv(paste0(path_to_data, "infant_outcomes", ".csv")) 

mnh01_constructed <- read.csv(paste0(path_to_data, "mnh01_constructed.csv")) 
mnh09_long <- read.csv(paste0(path_to_data, "mnh09_long.csv")) 

# 
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
```

**Includes data from synapse last updated:** 2024 May 17 (Zambia: 2024 April 19)

\tableofcontents

\newpage

## 1. Summary

### Table 1. Summary of all infant outcomes included in this report. 
**Denominator:** All livebirths unless otherwise specified. All missing is excluded.  

*For more detailed output, please refer to the specified sections below. *
```{r}
## updated 1/12
## LIVE BIRTHS (MNH11): lbw, asphyxia
## LIVE BIRTHS (MNH09): sga, preterm

## special: neo/inf mortality, stillbirth, fetal death


## lbw: Any infant with a MNH11 form and a reported "live birth" `(BIRTH_DSTERM=1 [MNH09])`.
## preterm: Any participant with delivery after 20 weeks with a MNH09 form filled out with a reported "birth outcome" `(BIRTH_DSTERM_INF1-4=1 OR BIRTH_DSTERM_INF1-4=2 [MNH09])` AND has valid gestational age reported in MNH01 `(US_GA_WKS_AGE_FTS1-4 [MNH01], US_GA_DAYS_AGE_FTS1-4 [MNH01], GA_LMP_WEEKS_SCORRES [MNH01])`. Preterm delivery includes live or stillbirths; preterm birth includes only livebirths. 
## sga: Any participant with a MNH09 form filled out with a reported "live birth" `(BIRTH_DSTERM_INF1-4=1 [MNH09])`.
## neo/inf mortality: All live births with MNH09 and MNH11 filled out AND have passed the risk period with a visit OR died within the risk period.
## stillbirth: Any infant with a birth outcome reported in MNH04 `(PRG_DSDECOD)` or MNH09 `(BIRTH_DSTERM)`.
## fetal death: Any infant with a fetal loss reported in MNH04 `(PRG_DSDECOD)` or birth outcome in MNH09 `(BIRTH_DSTERM_INF1)`.
## asphyxia

all_births <- mnh09_long %>%
  ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
    rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    "Total livebirths" = paste0(
      format(sum(M09_BIRTH_DSTERM == 1, na.rm = TRUE), nsmall = 0, digits = 2))
  )  %>%
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) %>% 
        add_column(
      .before = 1,
      "Total" = mnh09_long %>%
        plyr::summarise(
          
    "Total livebirths" = paste0(
      format(sum(M09_BIRTH_DSTERM == 1, na.rm = TRUE), nsmall = 0, digits = 2))
  ) %>%
        t() %>%
        as.data.frame() %>%
      `colnames<-`(c(.[1,])) %>% unlist()
)
  
  
lbw_sum <- lowbirthweight %>% 
  ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  filter(LBW_CAT_ANY != 55) %>%  ## remove missing
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    "Low birthweight <2500g (PRISMA or facility measured), n (%)" = paste0(
      format(sum(LBW2500_ANY == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(LBW2500_ANY == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Low birthweight <1500g (PRISMA or facility measured), n (%)" = paste0(
      format(sum(LBW1500_ANY == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(LBW1500_ANY == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")")
    
  )  %>%
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) %>% 
        add_column(
      .before = 1,
    "Total" = lowbirthweight %>%
        plyr::summarise(
          
    "Low birthweight <2500g (PRISMA or facility measured), n (%)" = paste0(
      format(sum(LBW2500_ANY == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(LBW2500_ANY == 1, na.rm = TRUE)/dim(lowbirthweight)[1]*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Low birthweight <1500g (PRISMA or facility measured), n (%)" = paste0(
      format(sum(LBW1500_ANY == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(LBW1500_ANY == 1, na.rm = TRUE)/dim(lowbirthweight)[1]*100, 2), nsmall = 0, digits = 2),
      ")")
    
  ) %>%
        t() %>%
        as.data.frame() %>%
      `colnames<-`(c(.[1,])) %>% unlist()
)


## preterm:
preterm_sum <- preterm_birth %>% 
  ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  filter(PRETERMBIRTH_CAT != 55) %>%  ## remove missing
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    "Preterm birth <37wks, n (%)" = paste0(
      format(sum(PRETERMBIRTH_LT37 == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(PRETERMBIRTH_LT37 == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")")
    
  )  %>%
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) %>% 
        add_column(
      .before = 1,
    "Total" = preterm_birth %>%
        plyr::summarise(
          
    "Preterm birth <37wks, n (%)" = paste0(
      format(sum(PRETERMBIRTH_LT37 == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(PRETERMBIRTH_LT37 == 1, na.rm = TRUE)/dim(preterm_birth)[1]*100, 2), nsmall = 0, digits = 2),
      ")")
    
  ) %>%
        t() %>%
        as.data.frame() %>%
      `colnames<-`(c(.[1,])) %>% unlist()
)

#sga
sga_sum <- sga %>%
  ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  filter(SGA_CAT != 55) %>%  ## remove missing
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    "SGA <3rd" = paste0(
      format(sum(SGA_CENTILE >= 0 & SGA_CENTILE < 3, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SGA_CENTILE >= 0 & SGA_CENTILE < 3, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "SGA <10th, n (%)" = paste0(
      format(sum(SGA_CENTILE >= 0 & SGA_CENTILE < 10, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SGA_CENTILE >= 0 & SGA_CENTILE < 10, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "LGA >=90th, n (%)" = paste0(
      format(sum(SGA_CENTILE >=90, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SGA_CENTILE >=90, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")")
    
  )  %>%
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) %>% 
        add_column(
      .before = 1,
    "Total" = sga %>%
        plyr::summarise(
        
    "SGA <3rd" = paste0(
      format(sum(SGA_CENTILE >= 0 & SGA_CENTILE < 3, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SGA_CENTILE >= 0 & SGA_CENTILE < 3, na.rm = TRUE)/dim(sga)[1]*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "SGA <10th, n (%)" = paste0(
      format(sum(SGA_CENTILE >= 0 & SGA_CENTILE < 10, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SGA_CENTILE >= 0 & SGA_CENTILE < 10, na.rm = TRUE)/dim(sga)[1]*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "LGA >=90th, n (%)" = paste0(
      format(sum(SGA_CENTILE >=90, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SGA_CENTILE >=90, na.rm = TRUE)/dim(sga)[1]*100, 2), nsmall = 0, digits = 2),
      ")")

    
  ) %>%
        t() %>%
        as.data.frame() %>%
      `colnames<-`(c(.[1,])) %>% unlist()
)

# neonatal mortality (no missing here)
neo_mort_sum <- neonatal_mortality %>% 
  ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    "Neonatal mortality, n per 1000^a^" = paste0(
      format(sum(TOTAL_NEO_DEATHS==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(TOTAL_NEO_DEATHS==1, na.rm = TRUE)/sum(DENOM_28d==1, na.rm = TRUE)*1000, 2), nsmall = 0, digits = 2),
      ")")
    
  )  %>%
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) %>% 
        add_column(
      .before = 1,
    "Total" = neonatal_mortality %>%
        plyr::summarise(
        
    "Neonatal mortality, n per 1000^a^" = paste0(
      format(sum(TOTAL_NEO_DEATHS==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(TOTAL_NEO_DEATHS==1, na.rm = TRUE)/sum(DENOM_28d==1, na.rm = TRUE)*1000, 2), nsmall = 0, digits = 2),
      ")")

    
  ) %>%
        t() %>%
        as.data.frame() %>%
      `colnames<-`(c(.[1,])) %>% unlist()
) %>% 
   mutate_all(funs(str_replace(., "NaN", "0")))


## remove infant mortality for now -- add back in when we have data 
# # infant mortality (no missing here)
# inf_mort_sum <- infant_mortality %>% 
#   rowwise() %>% 
#   group_by(SITE) %>% 
#   summarise(
#     
#     "Infant mortality, n per 1000^b^" = paste0(
#       format(sum(TOTAL_INF_DEATHS==1 & AGE_LAST_SEEN >= 365, na.rm = TRUE), nsmall = 0, digits = 2),
#       " (",
#       format(round(sum(TOTAL_INF_DEATHS==1 & AGE_LAST_SEEN >= 365, na.rm = TRUE)/sum(DENOM_365d==1, na.rm = TRUE)*1000, 2), nsmall = 0, digits = 2),
#       ")")
#     
#   )  %>%
#   t() %>% as.data.frame() %>% 
#   `colnames<-`(c(.[1,])) %>% 
#   slice(-1) %>% 
#         add_column(
#       .before = 1,
#     "Total" = infant_mortality %>%
#         plyr::summarise(
#         
#     "Infant mortality, n per 1000^b^" = paste0(
#       format(sum(TOTAL_INF_DEATHS==1 & AGE_LAST_SEEN >= 365, na.rm = TRUE), nsmall = 0, digits = 2),
#       " (",
#       format(round(sum(TOTAL_INF_DEATHS==1 & AGE_LAST_SEEN >= 365, na.rm = TRUE)/sum(DENOM_365d==1, na.rm = TRUE)*1000, 2), nsmall = 0, digits = 2),
#       ")")
# 
#     
#   ) %>%
#         t() %>%
#         as.data.frame() %>%
#       `colnames<-`(c(.[1,])) %>% unlist()
# ) %>% 
#   mutate_all(funs(str_replace(., "NaN", "0"))) 

# Stillbirth
stillbirth_sum <- stillbirth %>%
  ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  filter(STILLBIRTH_GESTAGE_CAT != 55) %>%  ## remove missing
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    "Stillbirth >=20wks, n per 1000^b^" = paste0(
      format(sum(STILLBIRTH_20WK==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(STILLBIRTH_20WK==1, na.rm = TRUE)/sum(STILLBIRTH_DENOM==1, na.rm = TRUE)*1000, 2), nsmall = 0, digits = 2),
      ")")
    
  )  %>%
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) %>% 
        add_column(
      .before = 1,
    "Total" = stillbirth %>%
        plyr::summarise(
        
    "Stillbirth >=20wks, n per 1000^b^" = paste0(
      format(sum(STILLBIRTH_20WK==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(STILLBIRTH_20WK==1, na.rm = TRUE)/sum(STILLBIRTH_DENOM==1, na.rm = TRUE)*1000, 2), nsmall = 0, digits = 2),
      ")")

    
  ) %>%
        t() %>%
        as.data.frame() %>%
      `colnames<-`(c(.[1,])) %>% unlist()
) %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) 


# Fetal death (no missing)
fetal_dth_sum <- fetal_death %>%
  ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    "Total fetal deaths, n (%)^c^" = paste0(
      format(sum(INF_FETAL_DTH == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_FETAL_DTH == 1, na.rm = TRUE)/sum(INF_FETAL_DTH_DENOM==1, na.rm= TRUE)*100, 2), nsmall = 0, digits = 2),
      ")")
    
  )  %>%
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) %>% 
        add_column(
      .before = 1,
    "Total" = fetal_death %>%
        plyr::summarise(
          
    "Total fetal deaths, n (%)^c^" = paste0(
      format(sum(INF_FETAL_DTH == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_FETAL_DTH == 1, na.rm = TRUE)/sum(INF_FETAL_DTH_DENOM==1, na.rm= TRUE)*100, 2), nsmall = 0, digits = 2),
      ")")
    
  ) %>%
        t() %>%
        as.data.frame() %>%
      `colnames<-`(c(.[1,])) %>% unlist()
) %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) 


# asphyxia (no missing)
birth_asphyxia_sum <- birth_asphyxia %>% 
  ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    "Perinatal birth asphyxia, n (%)" = paste0(
      format(sum(INF_ASPH == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_ASPH == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")")
    
  )  %>%
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) %>% 
        add_column(
      .before = 1,
    "Total" = birth_asphyxia %>%
        plyr::summarise(
          
    "Perinatal birth asphyxia, n (%)" = paste0(
      format(sum(INF_ASPH == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_ASPH == 1, na.rm = TRUE)/dim(birth_asphyxia)[1]*100, 2), nsmall = 0, digits = 2),
      ")")
    
  ) %>%
        t() %>%
        as.data.frame() %>%
      `colnames<-`(c(.[1,])) %>% unlist()
) %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) 



# hyperbili by AAP (no missing)
hyperbili_sum <- hyperbili_all_crit %>% 
  ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    "Hyperbilirubinemia by AAP threshold, n (%)^d^" = paste0(
      format(sum(INF_HYPERBILI_AAP_24HR == 1 | 
                (INF_HYPERBILI_AAP_5DAY == 1) |
                (INF_HYPERBILI_AAP_14DAY == 1), na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_HYPERBILI_AAP_24HR == 1 | 
                (INF_HYPERBILI_AAP_5DAY == 1) |
                (INF_HYPERBILI_AAP_14DAY == 1), na.rm = TRUE)/sum(DENOM_ANY==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Any jaundice identified at any time^e^" = paste0(
      format(sum(INF_JAUN_NON_SEV_ANY==1 | 
                   INF_JAUN_SEV_24HR == 1 |
                   INF_JAUN_SEV_GREATER_24HR == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_JAUN_NON_SEV_ANY==1 | 
                         INF_JAUN_SEV_24HR == 1 |
                         INF_JAUN_SEV_GREATER_24HR == 1, na.rm = TRUE)/sum(DENOM_JAUN == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")")

    
  )  %>%
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) %>% 
        add_column(
      .before = 1,
    "Total" = hyperbili_all_crit %>%
        plyr::summarise(
          
    "Hyperbilirubinemia by AAP threshold, n (%)^d^" = paste0(
      format(sum(INF_HYPERBILI_AAP_24HR == 1 | 
                (INF_HYPERBILI_AAP_5DAY == 1) |
                (INF_HYPERBILI_AAP_14DAY == 1), na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_HYPERBILI_AAP_24HR == 1 | 
                (INF_HYPERBILI_AAP_5DAY == 1) |
                (INF_HYPERBILI_AAP_14DAY == 1), na.rm = TRUE)/sum(DENOM_ANY==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Any jaundice identified at any time^e^" = paste0(
      format(sum(INF_JAUN_NON_SEV_ANY==1 | 
                   INF_JAUN_SEV_24HR == 1 |
                   INF_JAUN_SEV_GREATER_24HR == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_JAUN_NON_SEV_ANY==1 | 
                         INF_JAUN_SEV_24HR == 1 |
                         INF_JAUN_SEV_GREATER_24HR == 1, na.rm = TRUE)/sum(DENOM_JAUN == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")")
    
  ) %>%
        t() %>%
        as.data.frame() %>%
      `colnames<-`(c(.[1,])) %>% unlist()
) %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) 


```

```{r}

infantoutcomes_combined <- bind_rows(all_births, lbw_sum, preterm_sum, sga_sum, neo_mort_sum, stillbirth_sum, fetal_dth_sum, birth_asphyxia_sum, hyperbili_sum) %>% select("Total", "Ghana", "India-CMC","India-SAS", "Kenya", "Pakistan", "Zambia")

infantoutcomes_output <-  infantoutcomes_combined %>% 
  select(-Total) 

# replace NA with 0s (this likely means there is not yet data)
infantoutcomes_combined[is.na(infantoutcomes_combined)] <- paste0("0 (0)")

infantoutcomes_output <- tb_theme1(infantoutcomes_combined) %>% 
  tab_header(
    title = md("**Table 1**")
  )  %>% 
tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>a</sup> Denominator is all live births with MNH09 and MNH11 filled out AND have passed the risk period with a visit OR died within the risk period (risk period: age = 28days).</span>")
 ) %>% 
  ## remove infant deaths for now - add back in when we have data 
 #  tab_footnote(
 #    footnote = html("<span style='font-size: 18px'><sup>b</sup>Denominator is all live births with MNH09 and MNH11 filled out AND have passed the risk period with a visit (risk period: age = 365days).</span>")
 # ) %>% 
  tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>b</sup> Denominator is all livebirths and stillbirths.</span>")
 ) %>% 
  tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>c</sup> Denominator is all infants with a fetal loss reported in MNH04 `(PRG_DSDECOD)` or birth outcome in MNH09 `(BIRTH_DSTERM_INF1)`.</span>")
 ) %>% 
  tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>d</sup> 
All infants with a TcB measurement taken at <14 weeks of life.</span>")
 ) %>% 
  tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>e</sup> 
All infants with a jaundice assessment at <14 weeks of life.</span>")
 ) 

infantoutcomes_summary <-infantoutcomes_combined
# save data set of summary table to add to monitoring report
save(infantoutcomes_summary, file= paste("D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data/infantoutcomes_summary", ".RData", sep = ""))


```

```{r, out.width = '100%'}
infantoutcomes_output <- infantoutcomes_output %>% gtsave("infantoutcomes_output.png", expand = 10)
knitr::include_graphics("infantoutcomes_output.png")
```


\newpage

## 2. Low birthweight 

**Definition:** Defined as liveborn infant weighing less than 2500g at birth. 

**Denominator:** All infants with a MNH11 form and a reported "live birth" `(BIRTH_DSTERM=1 [MNH09])`.

&nbsp;

**To be included as "non-missing" for this outcome, a participant must have:**

**1.** Live birth (varname: `BIRTH_DSTERM [MNH09]`).

**2.** Birthweight measured by PRISMA staff <72 hours following birth (varnames [form]: `BW_EST_FAORRES [MNH11]`, `BW_FAORRES [MNH11]`, `BW_FAORRES_REPORT [MNH11]`).

**3.** Facility reported birthweight where PRISMA <72 hours not available (varnames [form]: `BW_FAORRES_REPORT [MNH11]`).

### Table 2. Low birthweight 
```{r}

lowbirthweight_tab <- lowbirthweight %>%
  ## If India-SAS doesn't have data, add empty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    "Denominator" =  paste0(
      format(sum(DATA_COMPLETE_DENOM == 1, na.rm = TRUE), nsmall = 0, digits = 2)),
      
    "Missing Hours Since Birth weight was reported, n (%)" = paste0(
      format(sum(MISSING_TIME == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(MISSING_TIME == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),

    "Missing PRISMA, n (%)" = paste0(
      format(sum(MISSING_PRISMA == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(MISSING_PRISMA == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Missing Facility, n (%)" = paste0(
      format(sum(MISSING_FACILITY == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(MISSING_FACILITY == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Missing PRISMA & Facility, n (%)" = paste0(
      format(sum(MISSING_BOTH == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(MISSING_BOTH == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    ## PRISMA MEASURED BIRTH WEIGHT CATEGORICAL VARIABLE
    "Denominator " =  paste0(
      format(sum(LBW_PRISMA_DENOM == 1, na.rm = TRUE), nsmall = 0, digits = 2)),

    "PRISMA LBW - >=2500, n (%)" = paste0(
      format(sum(LBW_CAT_PRISMA == 13, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(LBW_CAT_PRISMA == 13, na.rm = TRUE)/sum(LBW_PRISMA_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "PRISMA LBW - <2500, n (%)" = paste0(
      format(sum(LBW_CAT_PRISMA == 12, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(LBW_CAT_PRISMA == 12, na.rm = TRUE)/sum(LBW_PRISMA_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "PRISMA LBW - <1500, n (%)" = paste0(
      format(sum(LBW_CAT_PRISMA == 11, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(LBW_CAT_PRISMA == 11, na.rm = TRUE)/sum(LBW_PRISMA_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    ## ANY BIRTH WEIGHT CATEGORICAL VARIABLE
    "Denominator  " =  paste0(
      format(sum(LBW_ANY_DENOM == 1, na.rm = TRUE), nsmall = 0, digits = 2)),

    "ANY LBW - >=2500, n (%)" = paste0(
      format(sum(LBW_CAT_ANY == 13, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(LBW_CAT_ANY == 13, na.rm = TRUE)/sum(LBW_ANY_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "ANY LBW - <2500, n (%)" = paste0(
      format(sum(LBW_CAT_ANY == 12, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(LBW_CAT_ANY == 12, na.rm = TRUE)/sum(LBW_ANY_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "ANY LBW - <1500, n (%)" = paste0(
      format(sum(LBW_CAT_ANY == 11, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(LBW_CAT_ANY == 11, na.rm = TRUE)/sum(LBW_ANY_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),

  )  %>%
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) 

```

```{r}
lowbirthweight_tab <- lowbirthweight_tab %>% 
  `rownames<-` (c(
    "Denominator",
    "Missing hours since birthweight measured, n (%)",
    "Missing PRISMA birthweight, n (%)",
    "Missing facility birthweight, n (%)",
    "Missing PRISMA & facility birthweight, n (%)",
    
    ## PRISMA MEASURED 
    "Denominator ",
    "Normal birthweight >=2500g",
    "Low birthweight 1500 to <2500g",
    "Very low birthweight <1500g",

    ## ANY MEASURED 
    "Denominator  ",
    "Normal birthweight >=2500g ",
    "Low birthweight 1500 to <2500g ",
    "Very low birthweight <1500g "
  )
  ) %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) 

# replace NA with 0s (this likely means there is not yet data)
lowbirthweight_tab[is.na(lowbirthweight_tab)] <- paste0("0 (0)")

lbw_output <- tb_theme1(lowbirthweight_tab) %>% 
  tab_header(
    title = md("**Table 2**")
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Data completeness</span>"),
    rows = 1:5
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>PRISMA measured birthweight, n (%)</span>"),
    rows = 6:9
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Any method measured birthweight, n (%)</span>"),
    rows = 10:13
  ) %>%
  row_group_order(groups = c("<span style='font-size: 18px'>Data completeness</span>",
                  "<span style='font-size: 18px'>PRISMA measured birthweight, n (%)</span>",
                  "<span style='font-size: 18px'>Any method measured birthweight, n (%)</span>")
                  )  %>%
  tab_style(
      style = list(
        cell_text(weight = "bold")
        ),
      locations = cells_body(rows = c(
                 "Denominator",
                 "Denominator ", 
                 "Denominator  "))) %>% 
  tab_style(
      style = list(
        cell_text(weight = "bold")
        ),
      locations = cells_stub(rows = c(
                 "Denominator",
                 "Denominator ", 
                 "Denominator  "))) 


# ) %>% 
# tab_footnote(
#     footnote = html("<span style='font-size: 18px'><sup>a</sup> PRISMA-measured birthweight is missing OR time of measurement is >= 72 hours.</span>")
#  ) %>%   
# tab_footnote(
#     footnote = html("<span style='font-size: 18px'><sup>b</sup> Both facility and PRISMA-measured birthweights are missing.</span>")
#  )     
```

```{r, out.width = '100%'}
lbw_output <- lbw_output %>% gtsave("lbw_output.png", expand = 10)
knitr::include_graphics("lbw_output.png")
```

\newpage

### Figure 2a. PRISMA-measured birthweights across sites. 
```{r, out.width = '100%'}

lowbirthweight_nomissing_prisma <- lowbirthweight %>% filter(BWEIGHT_PRISMA > 0, BWEIGHT_PRISMA<=5000)

## prisma birthweight (varname: BWEIGHT_PRISMA)
ggplot(data=lowbirthweight_nomissing_prisma,
       aes(x=BWEIGHT_PRISMA)) + 
  geom_histogram(fill = "#317773", color = "black") + #binwidth = 100
  facet_grid(vars(SITE), scales = "free") +
  scale_x_continuous(breaks = seq(0,5000,500)) + 
  # ggtitle("Birthweight by any reporting method, by Site") + 
  ylab("Count") + 
  xlab("Birthweight, g") + 
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + #color
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5), # angle = 60, 
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) + 
  geom_vline(mapping=aes(xintercept=1500), linetype ="dashed", color = "red") +
  geom_vline(mapping=aes(xintercept=2500), linetype ="dashed", color = "red") 

  

```

### Figure 2b. Facility-reported birthweights across sites. 
```{r, out.width = '100%'}

lowbirthweight_nomissing_facility <- lowbirthweight %>% filter(M11_BW_FAORRES_REPORT > 0, M11_BW_FAORRES_REPORT<=5000)

## facility birthweight (varname: M11_BW_EST_FAORRES)
ggplot(data=lowbirthweight_nomissing_facility,
       aes(x=M11_BW_FAORRES_REPORT)) + 
  geom_histogram(fill = "#317773", color = "black") +
  facet_grid(vars(SITE), scales = "free") +
  scale_x_continuous(breaks = seq(0,5000,500)) + 
  ylab("Count") + 
  xlab("Birthweight, g") + 
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + #color
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5), # angle = 60, 
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) + 
  geom_vline(mapping=aes(xintercept=1500), linetype ="dashed", color = "red") +
  geom_vline(mapping=aes(xintercept=2500), linetype ="dashed", color = "red") 

```

\newpage

### Figure 2c. Hours following birth infant was weighed across sites.
```{r}

ggplot(data=lowbirthweight,
       aes(x=BW_TIME)) + 
  geom_histogram(binwidth = 1, fill = "#317773", color = "black") + 
  facet_grid(vars(SITE), scales = "free") +
  scale_x_continuous(breaks = seq(0,96,8)) + 
  ylab("Count") + 
  xlab("Hours") + 
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + #color
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5), # angle = 60, 
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_vline(mapping=aes(xintercept=24), linetype ="dashed", color = "red") +
  geom_vline(mapping=aes(xintercept=48), linetype ="dashed", color = "red") + 
  geom_vline(mapping=aes(xintercept=72), linetype ="dashed", color = "red")
  
```
<span style="font-size: 88%;">
Figure 2c. Hours following birth weight measurement recorded by PRISMA-trained staff (or Facility reported where PRISMA measurement not available). Report of "0" indicates the measurement was taken <1hr following birth. Dashed lines represent 24, 48, and 72 hour time points. 
</span>


\newpage

## 3. Gestational age at delivery

**Definition:** Preterm delivery prior to 37 completed weeks of gestation. Further classified as: extremely preterm (<28 weeks), very preterm (28 to <32 weeks), moderate preterm (32 to <34 weeks), late preterm (34 to <37 weeks), term (37 to <41 weeks), and postterm (>41 weeks). 

**Denominator:** All participants with delivery after 20 weeks with a MNH09 form filled out with a reported "birth outcome" `(BIRTH_DSTERM_INF1-4=1 OR BIRTH_DSTERM_INF1-4=2 [MNH09])` AND has valid gestational age reported in MNH01 `(US_GA_WKS_AGE_FTS1-4 [MNH01], US_GA_DAYS_AGE_FTS1-4 [MNH01], GA_LMP_WEEKS_SCORRES [MNH01])`. Preterm delivery includes live or stillbirths; preterm birth includes only livebirths. 

***Note:** Gestational age information collected in MNH01 is used to generate best obstetric estimates for EDD. These constructed variables are then used to calculate GA at time of birth.* 

&nbsp;

**To be included as "non-Missing" for this outcome, a participant must have:**

**1.** Reported gestational age by either LMP or Ultrasound (varnames [form]: `US_GA_WKS_AGE_FTS1-4 [MNH01]`, `US_GA_DAYS_AGE_FTS1-4 [MNH01]`, `GA_LMP_WEEKS_SCORRES [MNH01]`).

**2.** Valid enrollment ultrasound visit date (varname: `US_OHOSTDAT [MNH01]`)

**3.** Valid date of birth (varname: `DELIV_DSSTDAT_INF1-4 [MNH09]`).

**4.** Birth outcome reported as a "Live birth" or "Fetal death" (varname: `BIRTH_DSTERM_INF1-4 [MNH09]`).

&nbsp;

**Common causes for a participant to be marked as "Missing":**

**-** Participant is missing a reported GA by Ultrasound AND GA by LMP in MNH01.

**-** Participant has multiple or is missing an enrollment ultrasound visit (`TYPE_VISIT=1`).

**-** Default value is used for enrollment ultrasound visit date (`US_OHOSTDAT=07-07-1907`). 

**-** Birth outcome is reported as "77, Not applicable". 

\newpage

### Table 3. Preterm delivery & preterm birth

```{r}

# N(%) without both US and LMP; (MISSING_BOTH_US_LMP)
# Distribution of GA_DIFF_DAYS; 
# N% where US is used vs. where LMP is used. (BOE_METHOD (where 1 = US and 2 = LMP))
# Histograms of GA at birth (GA_AT_BIRTH_WKS)

# a. Post-term delivery (>=41 weeks): Delivery after 41 weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_GT41]
# b. Term delivery (37 to <41 weeks): Delivery between 37 and <41 weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT41]
# c. Preterm delivery (<37 weeks): Delivery prior to 37 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT37]
# d. Preterm delivery (<34 weeks): Delivery prior to 34 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT34]
# e. Preterm delivery (<32 weeks): Delivery prior to 32 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT32]
# f. Preterm delivery (<28 weeks): Delivery prior to 28 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT28]
# g. Preterm delivery severity (categorical): Late preterm (34 to <37 wks), early preterm (32 to <34 wks), very preterm (28 to <32 wks), extremely preterm (<28 weeks) [varname: PRETERMBIRTH_CAT]

mnh01_tab <- preterm_birth %>% 
  ## If India-SAS doesn't have data, add empty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(

    "Denominator" =  paste0(
      format(sum(DATA_COMPLETE_DENOM == 1, na.rm = TRUE), nsmall = 0, digits = 2)),

    "Missing both US and LMP GA" = paste0(
      format(sum(is.na(BOE_METHOD)), nsmall = 0, digits = 2),
      " (",
      format(round(sum(is.na(BOE_METHOD))/sum(DATA_COMPLETE_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),

    "BOE = Ultrasound" = paste0(
      format(sum(BOE_METHOD == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(BOE_METHOD == 1, na.rm = TRUE)/sum(DATA_COMPLETE_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "BOE = LMP" = paste0(
      format(sum(BOE_METHOD == 2, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(BOE_METHOD == 2, na.rm = TRUE)/sum(DATA_COMPLETE_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")")
  ) %>% 
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) 

  
preterm_birth_live_tab <- preterm_birth %>% 
  ## If India-SAS doesn't have data, add empty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    "Denominator " =  paste0(
      format(sum(LIVEBIRTH == 1, na.rm = TRUE), nsmall = 0, digits = 2)),

    "Preterm birth severity (categorical), n-15" = paste0(
          format(sum(PRETERMBIRTH_CAT == 15, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(PRETERMBIRTH_CAT == 15, na.rm = TRUE)/sum(LIVEBIRTH==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"), 
    
    "Preterm birth severity (categorical), n-14" = paste0(
          format(sum(PRETERMBIRTH_CAT == 14, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(PRETERMBIRTH_CAT == 14, na.rm = TRUE)/sum(LIVEBIRTH==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
          
    "Preterm birth severity (categorical), n-13" = paste0(
          format(sum(PRETERMBIRTH_CAT == 13, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(PRETERMBIRTH_CAT == 13, na.rm = TRUE)/sum(LIVEBIRTH==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
    
    "Preterm birth severity (categorical), n-12" = paste0(
          format(sum(PRETERMBIRTH_CAT == 12, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(PRETERMBIRTH_CAT == 12, na.rm = TRUE)/sum(LIVEBIRTH==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
    
    "Preterm birth severity (categorical), n-11" = paste0(
          format(sum(PRETERMBIRTH_CAT == 11, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(PRETERMBIRTH_CAT == 11, na.rm = TRUE)/sum(LIVEBIRTH==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
    
   "Preterm birth severity (categorical), n-10" = paste0(
          format(sum(PRETERMBIRTH_CAT == 10, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(PRETERMBIRTH_CAT == 10, na.rm = TRUE)/sum(LIVEBIRTH==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")")

  )  %>%
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) 

  
preterm_birth_tab <- preterm_birth %>%
  ## If India-SAS doesn't have data, add empty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
      
    "Denominator  " =  paste0(
      format(sum(BIRTH_OUTCOME_REPORTED == 1, na.rm = TRUE), nsmall = 0, digits = 2)),

    "Preterm delivery severity (categorical), n-15" = paste0(
          format(sum(PRETERMDELIV_CAT == 15, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(PRETERMDELIV_CAT == 15, na.rm = TRUE)/sum(BIRTH_OUTCOME_REPORTED==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"), 
    
    "Preterm delivery severity (categorical), n-14" = paste0(
          format(sum(PRETERMDELIV_CAT == 14, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(PRETERMDELIV_CAT == 14, na.rm = TRUE)/sum(BIRTH_OUTCOME_REPORTED==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
          
    "Preterm delivery severity (categorical), n-13" = paste0(
          format(sum(PRETERMDELIV_CAT == 13, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(PRETERMDELIV_CAT == 13, na.rm = TRUE)/sum(BIRTH_OUTCOME_REPORTED==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
    
    "Preterm delivery severity (categorical), n-12" = paste0(
          format(sum(PRETERMDELIV_CAT == 12, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(PRETERMDELIV_CAT == 12, na.rm = TRUE)/sum(BIRTH_OUTCOME_REPORTED==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
    
    "Preterm delivery severity (categorical), n-11" = paste0(
          format(sum(PRETERMDELIV_CAT == 11, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(PRETERMDELIV_CAT == 11, na.rm = TRUE)/sum(BIRTH_OUTCOME_REPORTED==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
    
   "Preterm delivery severity (categorical), n-10" = paste0(
          format(sum(PRETERMDELIV_CAT == 10, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(PRETERMDELIV_CAT == 10, na.rm = TRUE)/sum(BIRTH_OUTCOME_REPORTED==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")")
  )  %>%
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) 

```

```{r}

preterm_tab <- bind_rows(mnh01_tab, preterm_birth_live_tab, preterm_birth_tab) %>% 
  `rownames<-` (c(
    "Denominator",
    "Missing both US and LMP GA, n (%)",
    "BOE = Ultrasound, n (%)",
    "BOE = LMP, n (%)",
    
    "Denominator ",
    "Extremely preterm (<28wks)",
    "Very preterm (28 to <32wks)",
    "Moderate preterm (32 to <34wks)",
    "Late preterm (34 to <37wks)",
    "Term (37 to <41wks)",
    "Postterm (>=41wks)",

    "Denominator  ",
    "Extremely preterm (<28wks) ",
    "Very preterm (28 to <32wks) ",
    "Moderate preterm (32 to <34wks) ",
    "Late preterm (34 to <37wks) ",
    "Term (37 to <41wks) ",
    "Postterm (>=41wks) "

  )
  ) %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) 

# replace NA with 0s (this likely means there is not yet data)
preterm_tab[is.na(preterm_tab)] <- paste0("0 (0)")

preterm_output <- tb_theme1(preterm_tab) %>% 
  tab_header(
    title = md("**Table 3**")
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Data Completeness [MNH01] (among participants with a birth outcome)</span>"),
    rows = 1:4
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Preterm birth severity (livebirths) (categorical), n (%)</span>"),
    rows = 5:11
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Preterm delivery severity (live & stillbirths) (categorical), n (%)</span>"),
    rows = 12:18
  ) %>%
  row_group_order(groups = c("<span style='font-size: 18px'>Data Completeness [MNH01] (among participants with a birth outcome)</span>",
                  "<span style='font-size: 18px'>Preterm birth severity (livebirths) (categorical), n (%)</span>",
                  "<span style='font-size: 18px'>Preterm delivery severity (live & stillbirths) (categorical), n (%)</span>")
                  ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")
        ),
      locations = cells_body(rows = c(
                 "Denominator",
                 "Denominator ", 
                 "Denominator  "))) %>% 
  tab_style(
      style = list(
        cell_text(weight = "bold")
        ),
      locations = cells_stub(rows = c(
                 "Denominator",
                 "Denominator ", 
                 "Denominator  "))) 


```

```{r, out.width = '100%'}
preterm_output <- preterm_output %>% gtsave("preterm_output.png", expand = 10)
knitr::include_graphics("preterm_output.png")
```

\newpage

### Figure 3a. Distribution of days difference between GA by US and GA by LMP reporting across sites.
```{r}

mnh01_constructed$GA_DIFF_DAYS = abs(mnh01_constructed$GA_DIFF_DAYS)

summary_stats <- summary(mnh01_constructed$GA_DIFF_DAYS)

ggplot(data=mnh01_constructed,
       aes(x=abs(GA_DIFF_DAYS))) + 
  geom_histogram(fill = "#317773", color = "black") + #binwidth = 100
  facet_grid(vars(SITE), scales = "free") +
  ylab("Count") + 
  xlab("Days difference") + 
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + #color
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5), # angle = 60, 
        strip.text = element_text(size = 8),  # Adjust the size here
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) 

```

\newpage

### Figure 3b. Distribution of gestational age at birth in weeks. 
```{r}

ggplot(data=preterm_birth,
       aes(x=GESTAGEBIRTH_BOE)) + 
  geom_histogram(binwidth = 1, fill = "#317773", color = "black") + 
  facet_grid(vars(SITE), scales = "free") +
  scale_x_continuous(breaks = seq(20,45,1), 
                     limits = c(20,45)) + 
  ylab("Count") + 
  xlab("Gestational Age at Birth (Weeks)") + 
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + #color
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5), # angle = 60,
        strip.text = element_text(size = 8),  # Adjust the size here
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())
  
```
<span style="font-size: 88%;">
Figure 3b. Gestational age at birth was calculated by taking the difference between the DOB and "estimated conception date" determined by BOE.  
</span>


\newpage

## 4. Size for gestational age (SGA/LGA)
**Definition:** GA & sex-specific birthweight percentiles per INTERGROWTH standard.Further classified as <3rd, 3rd to 10th, 10th to <90th, >=90th. 

**Denominator:** All participants with a MNH09 form filled out with a reported "live birth" `(BIRTH_DSTERM_INF1-4=1 [MNH09])`.

***Note:** Gestational age information collected in MNH01 is used to generate best obstetric estimates for EDD. These constructed variables are then used to calculate GA at time of birth.* 

&nbsp;

**To be included as "non-Missing" for this outcome, a participant must have:**

**1.** Reported gestational age by either LMP or Ultrasound (varnames [form]: `US_GA_WKS_AGE_FTS1-4 [MNH01]`, `US_GA_DAYS_AGE_FTS1-4 [MNH01]`, `GA_LMP_WEEKS_SCORRES [MNH01]`).

**2.** Valid date of birth (varname: `DELIV_DSSTDAT_INF1-4 [MNH09]`).

**3.** Birthweight measured by PRISMA staff <72 hours following birth OR facility reported birthweight where PRISMA not available (varnames [form]: `BW_EST_FAORRES [MNH11]`, `BW_FAORRES [MNH11]`, `BW_FAORRES_REPORT [MNH11]`).

**4.** Live birth (varname: `BIRTH_DSTERM_INF1-4 [MNH09]`).

**5.** Sex of infant (varname: `SEX_INF1-4 [MNH09]`).

&nbsp;

**Common causes for a participant to be marked as "Missing":**

**-** Participant is missing a reported GA by Ultrasound AND GA by LMP in MNH01.

**-** Participant is missing an enrollment ultrasound visit (`TYPE_VISIT=1`).

**-** PRISMA-measured AND Facility-reported birthweights are missing from MNH11.

**-** MNH11 forms missing for infants (i.e. infantid present in MNH09, but is missing an MNH11 form).

**-** Gestational age at birth less than 33 weeks or over 42 weeks. 

**-** INFANTID in MNH09 `(INFANTID_INF1-4)` that can be linked to MNH11 `(INFANTID)`.

### Table 4. Size for gestational age at birth 
```{r}
#* 3. SGA
# a. Size for gestational age - categorical. [varname: SGA_CAT]
# b. Preterm small for gestational age: Preterm < 37 weeks AND SGA (<10th). [varname: INF_SGA_PRETERM]
# c. Preterm appropriate for gestational age: Preterm < 37 weeks AND not SGA (<10th). [varname: INF_AGA_PRETERM]
# d. Term small for gestational age: Term >=37 weeks AND SGA (<10th). [varname: INF_SGA_TERM]
# e. Term appropriate for gestational age: Term >=37 weeks AND not SGA (<10th). [varname: INF_AGA_TERM]

sga_tab <- sga %>% 
  ## If India-SAS doesn't have data, add empty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    
    "Denominator" =  paste0(
      format(sum(SGA_DENOM == 1, na.rm = TRUE), nsmall = 0, digits = 2)),

    "SGA <3rd percentile" = paste0(
      format(sum(SGA_CAT == 11, na.rm=TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SGA_CAT == 11, na.rm = TRUE)/sum(SGA_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),

    "SGA 3rd to <10th percentile" = paste0(
      format(sum(SGA_CAT == 12, na.rm=TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SGA_CAT == 12, na.rm = TRUE)/sum(SGA_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "AGA 10th to <90th percentile" = paste0(
      format(sum(SGA_CAT == 13, na.rm=TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SGA_CAT == 13, na.rm = TRUE)/sum(SGA_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "LGA >= 90th percentile" = paste0(
      format(sum(SGA_CAT == 14, na.rm=TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(SGA_CAT == 14, na.rm = TRUE)/sum(SGA_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
  ) %>% 
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) 



# out <- sga %>% filter(SITE == "Kenya" & SGA_CAT==55) %>% select(SITE, MOMID, PREGID, INFANTID, GESTAGEBIRTH_BOE_DAYS, GESTAGEBIRTH_BOE, BWEIGHT_ANY, M09_SEX)
# write.csv(out, paste0("~/ken_out_ids.csv"), row.names=FALSE)

```

```{r}

sga_tab <- sga_tab %>% 
  `rownames<-` (c(
    "SGA <3rd percentile",
    "SGA 3rd to <10th percentile",
    "AGA 10th to <90th percentile",
    "LGA >= 90th percentile",
    "Missing"
  )
  ) %>%
  # mutate_all(funs(str_replace(., "NA (0)", "0 (0)"))) %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) 

# replace NA with 0s (this likely means there is not yet data)
sga_tab[is.na(sga_tab)] <- paste0("0 (0)")

sga_output <- tb_theme1(sga_tab) %>% 
  tab_header(
    title = md("**Table 4**")
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Size for Gestational Age (Cateogorical)<sup>a</sup>, n (%)</span>"),
    rows = 1:5
  )  %>%
  row_group_order(groups = c("<span style='font-size: 18px'>Size for Gestational Age (Cateogorical)<sup>a</sup>, n (%)</span>"))

```

```{r, out.width = '100%'}
sga_output <- sga_output %>% gtsave("sga_output.png", expand = 10)
knitr::include_graphics("sga_output.png")
```

\newpage

### Figure 4. Distribution of INTERGROWTH percentiles across sites.
```{r}

ggplot(data=sga,
       aes(x=SGA_CENTILE)) + 
  geom_histogram(binwidth = 5, fill = "#317773", color = "black") + 
  facet_grid(vars(SITE), scales = "free") +
  scale_x_continuous(breaks = seq(0,100,10),
                     limits = c(0,100)) +
  ggtitle("Distribution of INTERGROWTH percentiles, by Site") + 
  ylab("Count") + 
  xlab("INTERGROWTH Percentile") + 
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + #color
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5), # angle = 60, 
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) 

```


\newpage


## 5. Mortality

**Definition:** Death of a liveborn baby. Further stratified as neonatal mortality (death of a liveborn baby in the first 28 days of life) and infant mortality (death of a liveborn baby in the first 365 days of life).


**Denominator:** All live births with MNH09 and MNH11 filled out AND have passed the risk period with a visit OR died within the risk period.

&nbsp;

**To be included as "non-Missing" for this outcome, a participant must have:**

**1.** Valid date of birth (varname: `DELIV_DSSTDAT_INF1-4 [MNH09]`). 

**2.** Birth outcome reported as "Live birth" (varname: `BIRTH_DSTERM [MNH09]`)

### Table 5. Mortality
```{r, mortality, message = FALSE, warning = FALSE}
#  4. Neonatal mortality: Denominator is all live births reported in MNH11 with mh09 filled out 
  # a. <24 hours 
  # b. Early neontal mortality: first  7 days 
  # c. Late neonatal mortality: between 7 & 28 days

mortality_tab <- mortality %>% ## denominator is anyone with an MNH09 or MNH11 filled out 
     ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(

    "Missing MNH09 (but has MNH11 or MNH24)" = paste0(
      format(sum(MISSING_MNH09 == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(MISSING_MNH09 == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Missing MNH11 (but has MNH09 or MNH24)" = paste0(
      format(sum(MISSING_MNH11 == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(MISSING_MNH11 == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")")

  ) %>% 
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) 

neo_mortality_tab <- neonatal_mortality %>% ## denominator is all live births with an mnh11 and mnh09 filled out  
  ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    "Denominator " = paste0(
      format(sum(DENOM_28d == 1, na.rm = TRUE), nsmall = 0, digits = 2)),

    "Death <24 hrs of life" = paste0(
      format(sum(NEO_DTH_CAT == 11, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(NEO_DTH_CAT == 11, na.rm = TRUE)/sum(DENOM_28d == 1 , na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Death 24 hrs to <7 days of life" = paste0(
      format(sum(NEO_DTH_CAT == 12, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(NEO_DTH_CAT == 12, na.rm = TRUE)/sum(DENOM_28d == 1 , na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Death 7 days to <28 days of life" = paste0(
      format(sum(NEO_DTH_CAT == 13, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(NEO_DTH_CAT == 13, na.rm = TRUE)/sum(DENOM_28d == 1 , na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Death reported but missing time of death " = paste0(
      format(sum(NEO_DTH_CAT == 66, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(NEO_DTH_CAT == 66, na.rm = TRUE)/sum(DENOM_28d == 1 , na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Total neonatal deaths " = paste0(
      format(sum(TOTAL_NEO_DEATHS == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(TOTAL_NEO_DEATHS == 1, na.rm = TRUE)/sum(DENOM_28d == 1 , na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")")


  ) %>% 
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) 


inf_mortality_tab <- infant_mortality %>% ## denominator is all live births with an mnh11 and mnh09 filled out  
  ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(

    "Denominator  " = paste0(
      format(sum(DENOM_365d == 1, na.rm = TRUE), nsmall = 0, digits = 2)),

    "Death <365 days of life" = paste0(
      format(sum(INF_DTH_CAT == 14 & AGE_LAST_SEEN >= 365, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_DTH_CAT == 14 & AGE_LAST_SEEN >= 365, na.rm = TRUE)/sum(DENOM_365d == 1 , na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Death reported but missing time of death  " = paste0(
      format(sum(INF_DTH_CAT == 66 & AGE_LAST_SEEN >= 365, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_DTH_CAT == 66 & AGE_LAST_SEEN >= 365, na.rm = TRUE)/sum(DENOM_28d == 1 , na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),

    
    "Total infant deaths  " = paste0(
      format(sum(TOTAL_INF_DEATHS == 1 & AGE_LAST_SEEN >= 365, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(TOTAL_INF_DEATHS == 1 & AGE_LAST_SEEN >= 365, na.rm = TRUE)/sum(DENOM_365d == 1 , na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")")

  ) %>% 
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) 

```

```{r}
mortality_all_tab <- bind_rows(mortality_tab, neo_mortality_tab, inf_mortality_tab) %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) 

# replace NA with 0s (this likely means there is not yet data)
mortality_all_tab[is.na(mortality_all_tab)] <- paste0("0 (0)")

mortality_output <- tb_theme1(mortality_all_tab) %>% 
  tab_header(
    title = md("**Table 5**")
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Data Completeness</span>"),
    rows = 1:2
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Timing of Neonatal Mortality<sup>a</sup></span>"),
    rows = 3:8
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Timing of Infant Mortality<sup>b</sup></span>"),
    rows = 9:12
  ) %>%
  row_group_order(groups = c("<span style='font-size: 18px'>Data Completeness</span>",
                  "<span style='font-size: 18px'>Timing of Neonatal Mortality<sup>a</sup></span>",
                  "<span style='font-size: 18px'>Timing of Infant Mortality<sup>b</sup></span>")
                  ) %>% 
tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>a</sup> Denominator is all live births with MNH09 and MNH11 filled out AND have passed the risk period with a visit OR died within the risk period (risk period: age = 28days).</span>")
 )  %>% 
  tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>b</sup> Denominator is all live births with MNH09 and MNH11 filled out AND have passed the risk period (risk period: age = 365days).</span>")
 ) 
```

```{r, out.width = '100%'}
mortality_output <- mortality_output %>% gtsave("mortality_output.png", expand = 8)
knitr::include_graphics("mortality_output.png")
```

\newpage

## 6. Stillbirth

**Definition:** Death prior to delivery of a fetus at >=20 weeks of gestation, excluding induced abortions.

**Denominator:** All infants with a birth outcome reported in MNH04 `(PRG_DSDECOD)` or MNH09 `(BIRTH_DSTERM)`.


&nbsp;

**To be included as "non-Missing" for this outcome, a participant must have:**

**1.** Valid date of birth OR valid fetal loss date (varname: `FETAL_LOSS_DSSTDAT [MNH04]` OR `DELIV_DSSTDAT_INF1-4 [MNH09]`).

**2.** Birth outcome reported in MNH04 or MNH11 (varname: `PRG_DSDECOD [MNH04]` OR `BIRTH_DSTERM [MNH09]`)

**3.** Reported gestational age by either LMP or Ultrasound (varnames [form]: `US_GA_WKS_AGE_FTS1-4 [MNH01]`, `US_GA_DAYS_AGE_FTS1-4 [MNH01]`, `GA_LMP_WEEKS_SCORRES [MNH01]`).


### Table 6. Stillbirth
```{r}

stillbirth_tab <- stillbirth %>% ## denominator is anyone with a birth outcome reported 
   ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(

    "Fetal loss or fetal death reported but missing signs of life (NA or 77 reported)^a^ ^b^" = paste0(
      format(sum(MISSING_SIGNS_OF_LIFE == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(MISSING_SIGNS_OF_LIFE == 1, na.rm = TRUE)/sum(BIRTH_OUTCOME == 0 & GA_AT_BIRTH_ANY >= 20, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Early stillbirth (20-27wks)" = paste0(
      format(sum(STILLBIRTH_GESTAGE_CAT == 11, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(STILLBIRTH_GESTAGE_CAT == 11, na.rm = TRUE)/sum(STILLBIRTH_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Late stillbirth (28-36wks)" = paste0(
      format(sum(STILLBIRTH_GESTAGE_CAT == 12, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(STILLBIRTH_GESTAGE_CAT == 12, na.rm = TRUE)/sum(STILLBIRTH_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Term stillbirth (>=37wks)" = paste0(
      format(sum(STILLBIRTH_GESTAGE_CAT == 13, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(STILLBIRTH_GESTAGE_CAT == 13, na.rm = TRUE)/sum(STILLBIRTH_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
  "Total stillbirths reported" = paste0(
      format(sum(STILLBIRTH_20WK == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(STILLBIRTH_20WK == 1, na.rm = TRUE)/sum(STILLBIRTH_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 

    
    "Missing" = paste0(
      format(sum(STILLBIRTH_GESTAGE_CAT == 55, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(STILLBIRTH_GESTAGE_CAT == 55, na.rm = TRUE)/sum(STILLBIRTH_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
        
   "Antepartum" = paste0(
      format(sum(STILLBIRTH_TIMING == 11, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(STILLBIRTH_TIMING == 11, na.rm = TRUE)/sum(BIRTH_OUTCOME == 0 &  GA_AT_BIRTH_ANY>=20, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Intrapartum" = paste0(
      format(sum(STILLBIRTH_TIMING == 12, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(STILLBIRTH_TIMING == 12, na.rm = TRUE)/sum(BIRTH_OUTCOME == 0 &  GA_AT_BIRTH_ANY>=20, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
   
    "Don't Know" = paste0(
      format(sum(STILLBIRTH_TIMING == 99, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(STILLBIRTH_TIMING == 99, na.rm = TRUE)/sum(BIRTH_OUTCOME == 0 &  GA_AT_BIRTH_ANY>=20, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),

  ) %>% 
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1)  %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) 

```

```{r}

# replace NA with 0s (this likely means there is not yet data)
stillbirth_tab[is.na(stillbirth_tab)] <- paste0("0 (0)")

stillbirth_output <- tb_theme1(stillbirth_tab) %>% 
  tab_header(
    title = md("**Table 6**")
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Data completeness</span>"),
    rows = 1:1
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Stillbirth (categorical)<sup>c</sup></span>"),
    rows = 2:6
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Timing of stillbirth<sup>a</sup></span>"),
    rows = 7:9
  ) %>%
  row_group_order(groups = c("<span style='font-size: 18px'>Data completeness</span>",
                  "<span style='font-size: 18px'>Stillbirth (categorical)<sup>c</sup></span>",
                  "<span style='font-size: 18px'>Timing of stillbirth<sup>a</sup></span>")
                  ) %>% 
  tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>a</sup> Denominator is all reported stillbirths.</span>")
 ) %>% 
  tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>b</sup> Valid signs of life reported in MNH09 and MNH11 (varname: `CRY_CEOCCUR_INF1-4 [MNH09]`, `FHR_VSTAT_INF1-4 [MNH09]`, `MACER_CEOCCUR_INF1-4 [MNH09]`, `CORD_PULS_CEOCCUR_INF1-4 [MNH09]`, `BREATH_FAIL_CEOCCUR [MNH11]`).</span>")
 ) %>% 
tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>c</sup> Denominator is all participants with a birth outcome (`BIRTH_DSTERM=1 or 0 [MNH09]` OR `PRG_DSDECOD=2 or 3 [MNH04]`).</span>")
 )  
```

```{r, out.width = '100%'}
stillbirth_output <- stillbirth_output %>% gtsave("stillbirth_output.png", expand = 10)
knitr::include_graphics("stillbirth_output.png")
```

\newpage

## 7. Fetal death

**Definition:** Death prior to delivery of a fetus at any gestational age. 

**Denominator:** All infants with a fetal loss reported in MNH04 `(PRG_DSDECOD)` or birth outcome in MNH09 `(BIRTH_DSTERM_INF1)`.

&nbsp;

**To be included as "non-Missing" for this outcome, a participant must have:**

**1.** Valid date of birth OR valid fetal loss date (varname: `FETAL_LOSS_DSSTDAT [MNH04]` OR `DELIV_DSSTDAT_INF1-4 [MNH09]`).

### Table 7. Fetal death
```{r}

fetal_death_tab <- fetal_death %>% ## denominator is anyone with a birth outcome reported 
   ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(

    "Fetal loss reported but specified type of loss is missing" = paste0(
      format(sum(INVALID_FETAL_LOSS == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INVALID_FETAL_LOSS == 1, na.rm = TRUE)/sum(M04_PRG_DSDECOD == 2, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Stillbirth (fetal loss >=20wks)" = paste0(
      format(sum(STILLBIRTH_20WK == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(STILLBIRTH_20WK == 1, na.rm = TRUE)/sum(INF_FETAL_DTH_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 

    "Spontaneous abortion (fetal loss <20wks)" = paste0(
      format(sum(INF_ABOR_SPN == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_ABOR_SPN == 1, na.rm = TRUE)/sum(INF_FETAL_DTH_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Fetal death at unknown GA" = paste0(
      format(sum(INF_FETAL_DTH_UNGA == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_FETAL_DTH_UNGA == 1, na.rm = TRUE)/sum(INF_FETAL_DTH_DENOM == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 
    
    "Total fetal deaths" = paste0(
      format(sum(INF_FETAL_DTH == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_FETAL_DTH == 1, na.rm = TRUE)/sum(INF_FETAL_DTH_DENOM, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"), 

    
    "Induced abortion^d^" = paste0(
      format(sum(INF_ABOR_IND == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_ABOR_IND == 1, na.rm = TRUE)/sum(INF_FETAL_DTH == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")")

  ) %>% 
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1)  %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) 

```

```{r}

# replace NA with 0s (this likely means there is not yet data)
fetal_death_tab[is.na(fetal_death_tab)] <- paste0("0 (0)")

fetaldeath_output <- tb_theme1(fetal_death_tab) %>% 
  tab_header(
    title = md("**Table 7**")
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Data completeness<sup>a</sup></span>"),
    rows = 1:1
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Fetal death<sup>b</sup></span>"),
    rows = 2:5
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Other fetal death<sup>c</sup></span>"),
    rows = 6:6
  ) %>%
  row_group_order(groups = c("<span style='font-size: 18px'>Data completeness<sup>a</sup></span>",
                  "<span style='font-size: 18px'>Fetal death<sup>b</sup></span>",
                  "<span style='font-size: 18px'>Other fetal death<sup>c</sup></span>")) %>% 
  tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>a</sup> Denominator is n reported fetal loss as pregnancy status in mnh04 (varname: `PRG_DSDECOD [MNH01]`).</span>")
 ) %>% 
tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>b</sup> Denominator is n total births excluding induced abortions</span>")
 )  %>% 
tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>c</sup> Denominator is n total births.</span>")
 ) %>% 
  tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>d</sup> Induced abortion defined as an elective surgical procedure or medical intervention to terminate the pregnancy at any gestational age).</span>")
 )  
```

```{r, out.width = '100%'}
fetaldeath_output <- fetaldeath_output %>% gtsave("fetaldeath_output.png", expand = 10)
knitr::include_graphics("fetaldeath_output.png")
```

\newpage

## 8. Perinatal birth asphyxia

**Definition:** Clinician reports failure to breathe spontaneously in the first minute after delivery or breathing assistance was required. 

**Denominator:** All liveborn infants with a MNH11 form filled out.

&nbsp;

**To be included as "non-Missing" for this outcome, a participant must have:**

**1.** Valid birth complications reported in MNH11 or MNH20 (varname: `INF_PROCCUR_1-6 [MNH11]`)

**2.** Valid birth complications reported in MNH20, if applicable (varname: `BIRTH_COMPL_MHTERM_3 [MNH20]`)

### Table 8. Perinatal birth asphyxia
```{r}

birth_asphyxia_tab <- birth_asphyxia %>% ## denominator is anyone with a birth outcome reported 
 ## If india-SAS doesn't have data, add emprty row here 
  mutate(existing_site = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_site == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(

    "Invalid birth complications in MNH11 or MNH20 (NA or 77 reported)^a^" = paste0(
      format(sum(INF_ASPH == 55, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_ASPH == 55, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Failed to initiate and sustain breathing at birth" = paste0(
      format(sum(M11_BREATH_FAIL_CEOCCUR == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(M11_BREATH_FAIL_CEOCCUR == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),

    "Bag and mask ventilation" = paste0(
      format(sum(M11_INF_PROCCUR_2 == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(M11_INF_PROCCUR_2 == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Continuous positive airway pressure" = paste0(
      format(sum(M11_INF_PROCCUR_3 == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(M11_INF_PROCCUR_3 == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),

    "Repeated stimulation/suction" = paste0(
      format(sum(M11_INF_PROCCUR_4 == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(M11_INF_PROCCUR_4 == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Intubation and mechanical ventilation" = paste0(
      format(sum(M11_INF_PROCCUR_5 == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(M11_INF_PROCCUR_5 == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),

    "Chest compressions" = paste0(
      format(sum(M11_INF_PROCCUR_6 == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(M11_INF_PROCCUR_6 == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")"),

    "Birth asphyxia^b^" = paste0(
      format(sum(INF_ASPH == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_ASPH == 1, na.rm = TRUE)/n()*100, 2), nsmall = 0, digits = 2),
      ")")

  ) %>% 
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1)  %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) %>% 
  mutate_all(funs(str_replace(., "NA", "0"))) 


```

```{r}
# replace NA with 0s (this likely means there is not yet data)
birth_asphyxia_tab[is.na(birth_asphyxia_tab)] <- paste0("0 (0)")

asphyxia_output <- tb_theme1(birth_asphyxia_tab) %>% 
  tab_header(
    title = md("**Table 8**")) %>% 
  tab_row_group(
    label = html("<span style='font-size: 18px'>Data completeness<sup>a</sup></span>"),
    rows = 1:1
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Indicators of birth asphyxia<sup>a</sup></span>"),
    rows = 2:7
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Perinatal birth asphyxia<sup>a</sup></span>"),
    rows = 8:8
  ) %>%
  row_group_order(groups = c("<span style='font-size: 18px'>Data completeness<sup>a</sup></span>",
                  "<span style='font-size: 18px'>Indicators of birth asphyxia<sup>a</sup></span>",
                  "<span style='font-size: 18px'>Perinatal birth asphyxia<sup>a</sup></span>")) %>% 

  tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>a</sup> Denominator is total liveborn infants with a MNH11 form filled out.</span>")
 ) %>% 
  tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>b</sup> Defined as required any breathing assistance at birth.</span>")
 )

```

```{r, out.width = '100%'}
asphyxia_output <- asphyxia_output %>% gtsave("asphyxia_output.png", expand = 10)
knitr::include_graphics("asphyxia_output.png")
```

\newpage

## 9. Hyperbilirubinemia

**Definition:** Defined as the presence of excess bilirubin during the first week of life (delivery to 7 days of age). 

**Denominator:** All infants with a measurement taken within the testing window. 

&nbsp;

### Table 9. Hyperbilirubinemia
```{r, hyperbilirubinemia, message = FALSE, warning = FALSE}
#  9. Hyperbilirubinemia: Denominator is all live births reported in MNH11 with mh09 filled out and have passed the risk period with a visit OR died within the risk period 
  # a. hyperbilirubinemia by TCB
  # b. hyperbilirubinemia by AAP
  # c. jaundice

hyperbili_all_crit_tab <- hyperbili_all_crit %>% ## Denominator is all live births reported in MNH11 with mh09 filled out and have passed the risk period with a visit OR died within the risk period 
  rowwise() %>% 
  group_by(SITE) %>% 
  summarise(
    # "Denominator" = paste0(
    #   format(sum(DENOM_24HR == 1, na.rm = TRUE), nsmall = 0, digits = 2)),
    # 
    "Hyperbilirubinemia <24 hrs of life" = paste0(
      format(sum(INF_HYPERBILI_TCB15_24HR == 1 , na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_HYPERBILI_TCB15_24HR == 1, na.rm = TRUE)/sum(DENOM_24HR == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Hyperbilirubinemia 24 hrs to <5 days of life" = paste0(
      format(sum(INF_HYPERBILI_TCB15_5DAY == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_HYPERBILI_TCB15_5DAY == 1, na.rm = TRUE)/sum(DENOM_5DAY == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Hyperbilirubinemia 5 days to <14 days of life" = paste0(
      format(sum(INF_HYPERBILI_TCB15_14DAY == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_HYPERBILI_TCB15_14DAY == 1, na.rm = TRUE)/sum(DENOM_14DAY == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Total hyperbilirubinemia by TCB >15mg/dL" = paste0(
      format(sum(INF_HYPERBILI_TCB15_24HR == 1 | 
                (INF_HYPERBILI_TCB15_5DAY == 1) |
                (INF_HYPERBILI_TCB15_14DAY == 1), na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_HYPERBILI_TCB15_24HR == 1 | 
                (INF_HYPERBILI_TCB15_5DAY == 1) |
                (INF_HYPERBILI_TCB15_14DAY == 1), na.rm = TRUE)/sum(DENOM_ANY==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),

## Hyperbilirubinemia by AAP thresholds
    # "Denominator " = paste0(
    #   format(sum(DENOM_24HR == 1, na.rm = TRUE), nsmall = 0, digits = 2)),
    # 
    "Hyperbilirubinemia <24 hrs of life " = paste0(
      format(sum(INF_HYPERBILI_AAP_24HR == 1 , na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_HYPERBILI_AAP_24HR == 1, na.rm = TRUE)/sum(DENOM_24HR == 1 , na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Hyperbilirubinemia 24 hrs to <5 days of life " = paste0(
      format(sum(INF_HYPERBILI_AAP_5DAY == 1 , na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_HYPERBILI_AAP_5DAY == 1, na.rm = TRUE)/sum(DENOM_5DAY == 1 , na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Hyperbilirubinemia 5 days to <14 days of life " = paste0(
      format(sum(INF_HYPERBILI_AAP_14DAY == 1 , na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_HYPERBILI_AAP_14DAY == 1, na.rm = TRUE)/sum(DENOM_14DAY == 1 , na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Total hyperbilirubinemia by AAP threshold" = paste0(
      format(sum(INF_HYPERBILI_AAP_24HR == 1 | 
                (INF_HYPERBILI_AAP_5DAY == 1) |
                (INF_HYPERBILI_AAP_14DAY == 1), na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_HYPERBILI_AAP_24HR == 1 | 
                (INF_HYPERBILI_AAP_5DAY == 1) |
                (INF_HYPERBILI_AAP_14DAY == 1), na.rm = TRUE)/sum(DENOM_ANY==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
  
  ## Jaundice Criteria
  
    "Non-severe jaundice identified at any time" = paste0(
      format(sum(INF_JAUN_NON_SEV_ANY == 1 , na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_JAUN_NON_SEV_ANY == 1, na.rm = TRUE)/sum(DENOM_JAUN == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "Severe jaundice identified <24 hrs of life" = paste0(
      format(sum(INF_JAUN_SEV_24HR == 1 , na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_JAUN_SEV_24HR == 1, na.rm = TRUE)/sum(DENOM_JAUN==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),

    "Severe jaundice identified >=24 hrs of life" = paste0(
      format(sum(INF_JAUN_SEV_GREATER_24HR == 1 , na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_JAUN_SEV_GREATER_24HR == 1, na.rm = TRUE)/sum(DENOM_JAUN==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),

    "Any jaundice identified at any time" = paste0(
      format(sum(INF_JAUN_NON_SEV_ANY==1 | 
                   INF_JAUN_SEV_24HR == 1 |
                   INF_JAUN_SEV_GREATER_24HR == 1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(INF_JAUN_NON_SEV_ANY==1 | 
                         INF_JAUN_SEV_24HR == 1 |
                         INF_JAUN_SEV_GREATER_24HR == 1, na.rm = TRUE)/sum(DENOM_JAUN == 1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")")

  ) %>% 
  t() %>% as.data.frame() %>% 
  `colnames<-`(c(.[1,])) %>% 
  slice(-1) 

```

```{r}
hyperbili_all_crit_tab <- hyperbili_all_crit_tab %>% 
  mutate_all(funs(str_replace(., "NaN", "0"))) 

# replace NA with 0s (this likely means there is not yet data)
hyperbili_all_crit_tab[is.na(hyperbili_all_crit_tab)] <- paste0("0 (0)")

hyperbili_all_crit_output <- tb_theme1(hyperbili_all_crit_tab) %>% 
  tab_header(
    title = md("**Table 9a**")
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Hyperbilirubinemia by TCB (>15mg/dL)</span>"),
    rows = 1:4
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Hyperbilirubinemia by TCB (AAP threshold)<sup>a</sup></span>"),
    rows = 5:8
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>Jaundice (IMCI criteria)</span>"),
    rows = 9:12
  ) %>%
  row_group_order(groups = c("<span style='font-size: 18px'>Hyperbilirubinemia by TCB (>15mg/dL)</span>",
                  "<span style='font-size: 18px'>Hyperbilirubinemia by TCB (AAP threshold)<sup>a</sup></span>",
                  "<span style='font-size: 18px'>Jaundice (IMCI criteria)</span>")
                  ) %>% 
  tab_footnote(
      footnote = html("<span style='font-size: 18px'><sup>a</sup> AAP threshold determined by the following inputs: phototherapy thresholds with no hyperbilirubinemia neurotoxicity risk factor, gestational age at delivery, and infant age at time of assessment (days and hours).</span>")
   )  

```

```{r, out.width = '100%'}
hyperbili_all_crit_output <- hyperbili_all_crit_output %>% gtsave("hyperbili_all_crit_output.png", expand = 8)
knitr::include_graphics("hyperbili_all_crit_output.png")
```

\newpage
