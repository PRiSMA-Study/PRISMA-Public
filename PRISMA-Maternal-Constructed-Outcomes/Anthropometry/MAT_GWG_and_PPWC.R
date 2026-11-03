# October 7, 2026
# Casey Kalman

# This file was created to combine the Gestational Weigh Gain (GWG) and Postpartum Weight Change (PPWC) codes. 

#environment setup----
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, #  dplyr, tidyr, ggplot2, readr, stringr, lubridate
  naniar,
  writexl,
  haven,
  gridExtra,
  wesanderson,
  readxl,
  splines,
  nlme, 
  janitor,
  flextable
)


#functions----

check_lme_fit <- function(fit, df, kn, ga_impute = 9, imputed = NULL, plot = NULL) {
  
  flags <- list()
  add <- function(check, ok, detail) {
    flags[[length(flags) + 1]] <<- data.frame(
      check = check, status = ifelse(ok, "pass", "FLAG"), detail = detail)
  }
  
  # 1. Random-effects covariance: variances, correlations, singularity
  D  <- getVarCov(fit)                 # random-effects covariance matrix
  sds <- sqrt(diag(D))
  R  <- cov2cor(D)
  cors <- R[lower.tri(R)]
  eig <- eigen(D, symmetric = TRUE, only.values = TRUE)$values
  
  add("Random-effect SDs > 0",
      all(sds > 1e-3),
      paste(names(sds), round(sds, 3), collapse = "; "))
  
  add("No random-effect correlation at +/-1",
      all(abs(cors) < 0.999),
      paste("max |cor| =", round(max(abs(cors)), 4)))
  
  add("Covariance matrix not near-singular",
      min(eig) / max(eig) > 1e-8,
      paste("eigenvalue ratio =", signif(min(eig) / max(eig), 3)))
  
  # 2. Approximate variance-covariance of the estimates
  #    (lme stores a character message here when it could not be computed)
  add("apVar computed (non-singular)",
      !is.character(fit$apVar),
      ifelse(is.character(fit$apVar), fit$apVar, "OK"))
  
  # 3. Confidence intervals
  iv <- tryCatch(intervals(fit), error = function(e) e)
  if (inherits(iv, "error")) {
    add("intervals() computes", FALSE, conditionMessage(iv))
  } else {
    re <- iv$reStruct[[1]]
    ok_iv <- !any(is.na(re)) && all(is.finite(as.matrix(re)))
    add("intervals() computes and finite", ok_iv, "random-effects block")
    if (ok_iv) {
      cor_rows <- grepl("^cor", rownames(re))
      add("Correlation intervals exclude +/-1",
          all(re$upper[cor_rows] < 0.999 & re$lower[cor_rows] > -0.999),
          paste(rownames(re)[cor_rows],
                round(re$upper[cor_rows], 3), collapse = "; "))
      sd_rows <- grepl("^sd", rownames(re))
      add("SD lower bounds > 0",
          all(re$lower[sd_rows] > 1e-3),
          paste(round(re$lower[sd_rows], 3), collapse = "; "))
    }
  }
  
  # 4. Fixed effects and residual SD are finite
  add("Fixed effects finite",
      all(is.finite(fixef(fit))), paste(round(fixef(fit), 3), collapse = "; "))
  add("Residual SD finite and > 0",
      is.finite(fit$sigma) && fit$sigma > 0, paste(round(fit$sigma, 3)))
  
  # 5. Residuals: heavy tails
  r <- resid(fit, type = "pearson")
  add("Few extreme residuals (|r| > 5)",
      mean(abs(r) > 5, na.rm = TRUE) < 0.001,
      paste(sum(abs(r) > 5, na.rm = TRUE), "of", length(r), "observations"))
  
  # 6. Data sparsity: observations per woman
  n_per <- table(df$PREGID)
  add("Most women have >= 2 observations",
      mean(n_per >= 2) > 0.5,
      paste0(round(100 * mean(n_per == 1), 1), "% have 1 observation; ",
             "mean obs/woman = ", round(mean(n_per), 2)))
  
  # 7. Knots well separated
  add("Knots at least 1 week apart",
      min(diff(kn)) >= 1, paste(names(kn), round(kn, 2), collapse = "; "))
  
  # 8. Extrapolation: target week vs boundary knots and observed data
  add("Target week inside boundary knots",
      ga_impute >= kn[1] && ga_impute <= kn[3],
      paste0("target = ", ga_impute, "; boundary = [",
             round(kn[1], 2), ", ", round(kn[3], 2), "]"))
  add("Target week within observed GA range",
      ga_impute >= min(df$GA_WKS, na.rm = TRUE),
      paste0("observed min = ", round(min(df$GA_WKS, na.rm = TRUE), 2),
             "; share of visits <= target = ",
             round(100 * mean(df$GA_WKS <= ga_impute, na.rm = TRUE), 1), "%"))
  
  # 9. Imputed values plausible and not missing
  if (!is.null(imputed)) {
    add("No missing imputed values", !anyNA(imputed),
        paste(sum(is.na(imputed)), "missing"))
    q <- quantile(imputed, c(0, .01, .5, .99, 1), na.rm = TRUE)
    add("Imputed range plausible (30-150 kg)",
        q[1] > 30 && q[5] < 150,
        paste("min", round(q[1], 1), "| median", round(q[3], 1),
              "| max", round(q[5], 1)))
  }
  
  out <- do.call(rbind, flags)
  n_flag <- sum(out$status == "FLAG")
  
  # still print to the console
  print(out, row.names = FALSE, right = FALSE)
  cat("\n", n_flag, "flag(s) out of", nrow(out), "checks\n")
  
  # draw the table in the Plots window
  if (plot) {
    library(gridExtra)
    library(grid)
    
    show <- out
    names(show) <- c("Check", "Status", "Detail")
    show$Detail <- sapply(show$Detail, function(x) paste(strwrap(x, 55), collapse = "\n"))
    
    # green for PASS, red for FLAG (one fill per row, repeated for each column)
    row_fill <- ifelse(show$Status == "FLAG", "#f8d7da", "#d4edda")
    
    tt <- ttheme_minimal(
      core    = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 9),
                     bg_params = list(fill = rep(row_fill, times = ncol(show)))),
      colhead = list(fg_params = list(hjust = 0, x = 0.05, fontface = "bold", fontsize = 10))
    )
    
    tbl   <- tableGrob(show, rows = NULL, theme = tt)
    title <- textGrob(paste0("Model checks: ", n_flag, " flag(s) out of ", nrow(out)),
                      gp = gpar(fontsize = 13, fontface = "bold"))
    
    grid.newpage()
    grid.draw(arrangeGrob(title, tbl,
                          heights = unit.c(unit(1.5, "lines"), unit(1, "npc") - unit(1.5, "lines"))))
  }
  
  invisible(out)
}

zambia<-imputed_bmi%>%filter(SITE=="Zambia")

weight_imputation<-function(data){

  #data <- zambia
  
  # 1. Percentiles of ga_week < 28 
  df <- data %>% filter(GA_WKS < 28)
  
  # The 25/50/75th percentiles are the knots for the model
  kn <- quantile(df$GA_WKS, probs = c(.25, .50, .75), type = 2, na.rm = TRUE)
  names(kn) <- c("P_25", "P_50", "P_75")
  
  
  # 2. Set knots at P25/P50/P75
  B <- ns(df$GA_WKS, knots = kn["P_50"],Boundary.knots = c(kn["P_25"], kn["P_75"]))
  df$spl1 <- as.numeric(B[, 1])
  df$spl2 <- as.numeric(B[, 2])
  
  # 3. Run the model
  
  ##First try more complex, 2 parameter model
  fit <- tryCatch(
    lme(
      fixed  = mweight ~ spl1 + spl2,
      random = ~ 1 + spl1 + spl2 | PREGID,
      data   = df,
      method = "REML",
      na.action = na.omit,
      control = lmeControl(
        opt = "optim",
        maxIter = 200,
        msMaxIter = 200
      )
    ),
    error = function(e) NULL
  )
  
  #simplify model if initial model is over-parameterized / returns null
  if(is.null(fit)){
    message(paste("Trying simpler random-effects structure: ~1 + spl1 for", unique(df$SITE)))
    
    fit <- tryCatch(
      lme(
        fixed = mweight ~ spl1 + spl2,
        random = ~ 1 + spl1 | PREGID,
        data = df,
        method = "REML",
        na.action = na.omit,
        control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200)
      ),
      error = function(e) NULL
    )
    
  }
  
  # If everything failed
  if (is.null(fit)) {
    message("All model specifications failed for site: ", unique(df$SITE))
    return(NULL)
  }
  
  # 4. pull imputation into data frame
  ids <- unique(df$PREGID)
  
  #this actually applies the model to each pregnancy to give us our imputed weights
  B9 <- ns(9, knots = kn["P_50"],
           Boundary.knots = c(kn["P_25"], kn["P_75"]))
  nd9 <- data.frame(PREGID = ids,
                    spl1   = as.numeric(B9[1, 1]),
                    spl2   = as.numeric(B9[1, 2])
  )
  
  imputation <- data.frame(
    PREGID            = ids,
    weight_imputed_9  = round(predict(fit, newdata = nd9, level = 1), 2)
  )
  
  #run checks on the model
  check_lme_fit(fit, df, kn, ga_impute = 9, imputed = NULL, plot=TRUE)
  
  #print knots
  print(paste0("Knots: ", kn))
  
  #print model details
  print(summary(fit))
  
  #final df
  print(head(imputation))
  
  #return final dataframe to global environment
  imputation
  
}


# To rename variables in a data frame
rename_variables <- function(df, form_prefix) {
  
  # Exclude specific "ID" variables from renaming
  exclude_variables <- c("MOMID", "PREGID", "SCRNID", "INFANTID", "SITE")
  id_variables <- intersect(exclude_variables, names(df))
  var_names <- names(df)[!names(df) %in% id_variables]
  
  # Rename the variables
  new_var_names <- gsub(paste0("^",form_prefix,"_"), "", var_names)  # Remove the first three characters and underscore
  names(df)[!names(df) %in% id_variables] <- new_var_names
  
  return(df)
}

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


#working directory----
setwd("D:/Users/ckalman/Documents/OutcomeDatasets/Analysis/Maternal Weight Gain")

#universal variables----

df_names <- ls(pattern = "^mnh", envir = .GlobalEnv)
UploadDate = "2026-09-18"

#file paths----
path_to_stacked <- paste0("Z:/Stacked Data/", UploadDate)
path_to_outcome <- paste0("Z:/Constructed Data/", UploadDate)
path_to_save    <- paste0("D:/Users/ckalman/Documents/OutcomeDatasets/Analysis/Maternal Weight Gain/data_out")

#****************************************************************************
# PULL IN DATASETS ----
#****************************************************************************

#*******************************************
## Load in the Rda file (after first time) ----
#*******************************************
#*#if datasets not already saved, run the "read in files" section
load(paste0("data_in/input_dataframes_imputedbmi_",UploadDate,".rda"))

#*************************
## Read files (first time) ----
#*************************

#### Pull in CRFs ----
#*******************************************

# Get a list of all CSV files in the folder
# csv_files <- list.files(path = path_to_stacked, pattern = "\\.csv$", full.names = TRUE)
# csv_files
# 
# crfs_needed<-c("mnh05", "mnh23")
# 
# #NOTE: Don't need to read the files in over and over again now that I've created an RDA file.
# #Loop through each CSV file
# #Casey's Notes: Changed this so it only reads in the files we need
# for (file in csv_files) {
#   # Extract the base name of the file (without the path)
#   base_name <- basename(file)
#   base_name
#   # Extract the part of the name before the underscore
#   name_prefix <- str_split(base_name, "_")[[1]][1]
#   name_prefix
#   
#   name_abbrev <- gsub("nh","",name_prefix)%>%toupper()
#   name_abbrev
#   
#   # Read the CSV file into a data frame
#   if(name_prefix %in% c(crfs_needed)){
# 
#     data_frame <- read.csv(file) %>% 
#                   rename_with(toupper)%>%
#                   rename_variables(.,name_abbrev)
# 
#     # Assign the data frame to a variable with the extracted name
#     assign(name_prefix, data_frame)
# 
#   }
# }

# Optional: List all variables in the environment to verify
# ls()
# 
 # save(
 #  list = crfs_needed,
 #  file = (paste0("D:/Users/ckalman/Documents/OutcomeDatasets/Analysis/Maternal Weight Gain/data_in/mnh_dataframes_imputedbmi_",UploadDate,".rda")))

### Pull in constructed datasets----
#*******************************************
# 
# if(UploadDate<'2026-09-18'){
#   
#   mat_enroll <- read_dta(paste0("Z:/Outcome Data/",UploadDate,"/MAT_ENROLL.dta"))%>%rename_with(toupper)
#   mat_endpoints <- read_dta(paste0("Z:/Outcome Data/",UploadDate,"/MAT_ENDPOINTS.dta"))%>%rename_with(toupper)
#   mat_demographic <- read_dta(paste0("Z:/Outcome Data/",UploadDate,"/MAT_DEMOGRAPHIC.dta"))%>%rename_with(toupper)
#   mat_flowchart <- read_dta(paste0("Z:/Outcome Data/",UploadDate,"/MAT_FLOWCHART.dta"))%>%rename_with(toupper)
#   
# } else {
#   
#   mat_studystatus<-read_dta(paste0("Z:/Constructed Data/",UploadDate,"/MAT_STUDYSTATUS.dta"))%>%rename_with(toupper)
#   mat_demographic <- read_dta(paste0("Z:/Constructed Data/",UploadDate,"/MAT_BASELINE.dta"))%>%rename_with(toupper)
#   mat_flowchart <- read.csv(paste0("Z:/Constructed Data/",UploadDate,"/MAT_FLOWCHART.csv"))%>%rename_with(toupper)
#   
#   
#   anc <- c("ENROLL", "ANC20", "ANC28", "ANC32", "ANC36")
#   pnc <- paste0("PNC", c(0, 1, 4, 6, 26, 52))
#   
#   mat_enroll_vars <- c(
#     "SITE", "SCRNID", "MOMID", "PREGID", "ENROLL", "REMAPP_ENROLL",
#     "REMAPP_AIM3_ENROLL", "REMAPP_AIM3_TRI", "ENROLL_SCRN_DATE", "PREG_START_DATE",
#     "EST_CONCEP_DATE_US", "EST_CONCEP_DATE_LMP", "M01_US_OHOSTDAT", "EDD_BOE",
#     "GA_DIFF_DAYS", "BOE_METHOD", "BOE_GA_WKS_ENROLL", "BOE_GA_DAYS_ENROLL",
#     "US_GA_DAYS_ENROLL", "US_GA_WKS_ENROLL", "LMP_GA_DAYS_ENROLL", "LMP_GA_WKS_ENROLL",
#     outer(anc, c("_ONTIME_WINDOW", "_LATE_WINDOW", "_PASS_ONTIME", "_PASS_LATE"), paste0)
#   )
#   
#   mat_endpoints_vars <- c(
#     "SITE", "MOMID", "PREGID", "PREG_END", "PREG_END_GA", "PREG_END_DATE",
#     "PREG_LOSS", "PREG_LOSS_INDUCED", "PREG_LOSS_DEATH",
#     "PREG_END_SOURCE", "CLOSEOUT", "CLOSEOUT_DT", "CLOSEOUT_GA",
#     "CLOSEOUT_TYPE", "CLOSEOUT_DAYS_PP", "MAT_DEATH", "MAT_DEATH_DATE", "MAT_DEATH_GA",
#     "MAT_DEATH_MISSDATE", "MAT_DEATH_INFAGE","PREG_END_PP42_DT", "MAT_DEATH_42", "STOP_DATE",
#     outer(pnc, c("_ONTIME_WINDOW", "_LATE_WINDOW", "_PASS_ONTIME", "_PASS_LATE"), paste0)
#   )
#   
#   mat_enroll <- select(mat_studystatus, any_of(mat_enroll_vars))
#   mat_endpoints <- select(mat_studystatus, any_of(mat_endpoints_vars))
# }

# save(
#   mnh05,
#   mnh23,
#   mat_enroll,
#   mat_endpoints,
#   mat_demographic,
#   mat_flowchart,
#   file = (paste0("D:/Users/ckalman/Documents/OutcomeDatasets/Analysis/Maternal Weight Gain/data_in/input_dataframes_imputedbmi_",UploadDate,".rda")))


#****************************************************************************
# GWG Section----
#****************************************************************************


## 1. DATA CLEANING FOR IMPUTED BMI ----
#***************************************


### a. Merge data in for enrolled women----
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
  filter(PREGID %in% PREGID_vector) %>% #only include those wtih a pregid
  #relocate(SITE, MOMID, PREGID, TYPE_VISIT) %>%
  #rename(TYPE_VISIT = TYPE_VISIT) %>%
  select(MOMID, PREGID, SITE, TYPE_VISIT, ANT_PEDAT, WEIGHT_PERES, WEIGHT_PEPERF, HEIGHT_PEPERF, HEIGHT_PERES)

#join mnh05 to enrollment data
mnh_enroll_05 <- full_join(mat_enroll, mnh05_filtered,
                           by = c("MOMID", "PREGID", "SITE"))


# mnh_enroll_05 %>% distinct (MOMID, PREGID, SITE, .keep_all = TRUE) %>% count(SITE)

### b. Remove NA rows on missing PREGID----
#**********************************
mnh_enroll_05_nomissing <- mnh_enroll_05 %>%
  filter(!is.na(PREGID))


#*****************************************
## 2. Create GWG variables for imputed BMI ----
#*****************************************

### a. Backfill weight from unplanned visit to missing ANC visit----
#* *******************************************************
# Replace -7 values to NA for maternal height and weight
merged_df <- mnh_enroll_05_nomissing %>%
  mutate(WEIGHT_PERES = ifelse(WEIGHT_PERES==-7, NA, WEIGHT_PERES),
         HEIGHT_PERES = ifelse(HEIGHT_PERES==-7, NA, HEIGHT_PERES),
         WEIGHT_PERES = ifelse(WEIGHT_PERES==-5, NA, WEIGHT_PERES),
         HEIGHT_PERES = ifelse(HEIGHT_PERES==-5, NA, HEIGHT_PERES))

# Create an updated type visit where type_visit==13 gets replaced with NA first
# Then we fill it with the updated type visit for that window

merged_df2 <- merged_df %>%
  filter(TYPE_VISIT %in% c(1,2,3,4,5,13))%>%
  #DATE OF VISIT - CONCEPTION DATE = GA AT VISIT. 
  mutate(GA_AT_VISIT = as.integer(floor(as.numeric(as.Date(ANT_PEDAT) - as.Date(PREG_START_DATE)))/7))

weight_df <- merged_df2 %>%
  mutate(TYPE_VISIT_UPDATED = ifelse(TYPE_VISIT==13, NA, TYPE_VISIT)) %>% #set type_visit 13 to NAs
  group_by(MOMID, PREGID, SITE) %>%
  #arrange by date
  arrange(MOMID, PREGID, SITE, as.Date(ANT_PEDAT), TYPE_VISIT) %>% 
  #create columns for QC
  group_by(PREGID)%>%
  mutate(
    visit_list=paste(TYPE_VISIT, collapse=", "),
    visit_count=n()
  )%>%
  ungroup()%>%
  #fills in missing type visits. i.e. if a date falls between visit 1 and visit 3, it is labeled as visit 2
  fill(TYPE_VISIT_UPDATED, .direction = "up") %>% 
  mutate(TYPE_VISIT_UPDATED=ifelse(TYPE_VISIT==13, 
                                   ifelse(TYPE_VISIT_UPDATED-1==0,NA,TYPE_VISIT_UPDATED-1),
                                   TYPE_VISIT_UPDATED))%>%
  fill(TYPE_VISIT_UPDATED, .direction = "down")%>%
  # mutate(TYPE_VISIT_UPDATED_2=case_when(
  #                               GA_AT_VISIT>=34 ~ 5, 
  #                               GA_AT_VISIT<34 & GA_AT_VISIT>=31 ~ 4,
  #                               GA_AT_VISIT<31 & GA_AT_VISIT>=26 ~ 3, 
  #                               GA_AT_VISIT<=20 & TYPE_VISIT==1 ~ 1,
  #                               GA_AT_VISIT<26 & GA_AT_VISIT>=18 ~ 2, 
  #                               GA_AT_VISIT<18 ~ 1,
  #                               TRUE ~ NA
  #                               
  # ))%>%
  # Drop NA rows based on WEIGHT -- drops instances in which we don't have a weight from that visit (Visit 2-5)
  drop_na(WEIGHT_PERES) %>%
  group_by(MOMID, PREGID, SITE, TYPE_VISIT_UPDATED) %>%
  filter(row_number() == 1)


# weight_df%>%
#   filter(grepl("13",visit_list))%>%
#   select(PREGID, GA_AT_VISIT, TYPE_VISIT, TYPE_VISIT_UPDATED, TYPE_VISIT_UPDATED_2, ANT_PEDAT, visit_list, visit_count)%>%
#   View()



### b. Create initial weight & height vars ----
#***************************************
# Create WEIGHT_ENROLL variable from TYPE_VISIT 1
weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(WEIGHT_ENROLL = WEIGHT_PERES[TYPE_VISIT_UPDATED == 1][1],
         HEIGHT_ENROLL = HEIGHT_PERES[TYPE_VISIT_UPDATED == 1][1]) %>%
  ungroup()


### c. Calculating GA days, weeks, and GA flags ----
#* *****************************************
#* ANT_PEDAT == anthropometric assessment date

weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(
    GA_DAYS = as.numeric(difftime(as.Date(ANT_PEDAT), as.Date(PREG_START_DATE), units = "days")),
    GA_WKS = as.numeric(difftime(as.Date(ANT_PEDAT), as.Date(PREG_START_DATE), units = "weeks")),
    GA_WKS_FLAG = case_when(
      GA_WKS >= 0 & GA_WKS <= 42 ~ 0,
      GA_WKS < 0 | GA_WKS > 42 ~ 1,
      TRUE ~ NA_real_
    ),
    GA_WKS = floor(GA_WKS)  # Apply floor directly in mutate
  ) %>%
  ungroup()

#* *****************************************
## 3. Imputation ----
#* *****************************************

imputed_bmi <- weight_df %>%
  select(SITE, SCRNID, MOMID, PREGID, TYPE_VISIT_UPDATED,	PREG_START_DATE, HEIGHT_PERES, WEIGHT_PERES, WEIGHT_ENROLL, GA_WKS)

### Option 1: Export dataset for imputation in SAS ----
#### a. write to file ----
write.csv(imputed_bmi, paste0("data_out/",'GWG_IMPUTE_LONG_', UploadDate, ".csv"))

#### b. run sas code ----

#### c. Read in imputation from SAS ----
#imputed_df <- read.csv("D:/Users/ckalman/Documents/OutcomeDatasets/Analysis/Maternal Weight Gain/data_out/GWG_ImputeObs_wk9_wk14.csv")

#skip to "Merge singleton to weight_df

### Option 2: Imputation in R----
#### a. check number of data points per pregnancy----
##if > 70% of women only have 1 or 2 observations, there will likely be an error
imputed_bmi%>%
  filter(GA_WKS<28)%>%
  select(SITE, PREGID)%>%
  summarise(count=n(), .by=c(SITE, PREGID))%>%
  tabyl(count, SITE)%>%
  #adorn_totals(where="col")%>%
  adorn_percentages(denominator="col")%>%
  adorn_pct_formatting(digits=0, affix_sign=T)%>%
  adorn_ns(position="front")

#sites where there are a large number of pregnancies in which they have only one observation could lead to errors in the model 
#run the imputation and look for warning messages about "singularity"

#### c. Perform imputation by region----

imputed_df <- imputed_bmi %>%
  rename(mweight = WEIGHT_PERES) %>%
  split(.$SITE) %>% #convert df to list of dfs by SITE
  imap(
    ~ weight_imputation(.x) #run the imputation function
    %>% mutate(SITE = .y)
    ) %>%
  list_rbind()

#run this section if you want to check errors/warnings by site
# ghana   <- imputed_bmi%>%
#            filter(SITE=="Ghana")%>%
#            rename(mweight=WEIGHT_PERES)%>%
#            weight_imputation(.)%>%
#            mutate(SITE="Ghana")
# 
# kenya   <- imputed_bmi%>%
#            filter(SITE=="Kenya")%>%
#            rename(mweight=WEIGHT_PERES)%>%
#            weight_imputation(.)%>%
#            mutate(SITE="Kenya")
# 
# zambia  <- imputed_bmi%>%
#            filter(SITE=="Zambia")%>%
#            rename(mweight=WEIGHT_PERES)%>%
#            weight_imputation(.)%>%
#            mutate(SITE="Zambia")
# 
# pakistan <- imputed_bmi%>%
#             filter(SITE=="Pakistan")%>%
#             rename(mweight=WEIGHT_PERES)%>%
#             weight_imputation(.)%>%
#             mutate(SITE="Pakistan")
# 
# indiaSAS <- imputed_bmi%>%
#             filter(SITE=="India-SAS")%>%
#             rename(mweight=WEIGHT_PERES)%>%
#             weight_imputation(.)%>%
#             mutate(SITE="India-SAS")
# 
# indiaCMC <- imputed_bmi%>%
#             filter(SITE=="India-CMC")%>%
#             rename(mweight=WEIGHT_PERES)%>%
#             weight_imputation(.)%>%
#             mutate(SITE="India-CMC")
# 
# 
# imputed_df<-rbind(ghana, kenya, zambia, pakistan, indiaSAS, indiaCMC)

#make sure they are all there
imputed_df%>%
  tabyl(SITE)

colnames(imputed_df)

imputed_df <- imputed_df %>% 
                rename_with(toupper)%>%
                select(SITE,PREGID,WEIGHT_IMPUTED_9
                       #, WEIGHT_IMPUTED_14
                       )

head(imputed_df)
View(imputed_df)

#********************************
## 4.  Merge imputed BMI to weight_df ----
#********************************
weight_df2 <- left_join(weight_df, imputed_df, by = c("SITE","PREGID"))%>%rename_with(toupper)

# weight_df2%>%
#   select(SITE, PREGID, WEIGHT_PERES, WEIGHT_ENROLL, WEIGHT_IMPUTED_9, TYPE_VISIT)%>%
#   View()

weight_df2 <- weight_df2 %>%
  arrange(PREGID, TYPE_VISIT) %>%  # Ensure the data is sorted by participant and visit
  group_by(PREGID) %>%  # Group by participant
  fill(WEIGHT_IMPUTED_9, .direction = "down") %>%  # Fill missing ENROLLMENT_WEIGHT down within each group (each pregnancy/PREGID)
  ungroup()

colnames(weight_df2)

#********************************
## 5.  Merge singleton to weight_df----
#********************************
mat_demographic_sub <- mat_demographic %>%
  select(SCRNID, MOMID, PREGID, SITE, SINGLETON)

weight_df2 <- left_join(weight_df2,mat_demographic_sub, by = c("SCRNID","MOMID","PREGID","SITE"))

#***************************************
## 6. Merge pregnancy endpoint to weight_df----
#***************************************
mat_endpoints_sub <- mat_endpoints %>%
  select(MOMID,PREGID,SITE,PREG_END)

weight_df2 <- left_join(weight_df2,mat_endpoints_sub, by = c("MOMID","PREGID","SITE"))

#*********************************
## 7. Create variables for datasets----
#*********************************
#### a.  Fill  enrollment weight with imputed/recorded data----
weight_df2 <- weight_df2 %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(GA_Enroll_Condition = BOE_GA_WKS_ENROLL[TYPE_VISIT_UPDATED == 1][1]) %>%
  ungroup()%>%

  mutate(
    WEIGHT_ENROLL_IMPUTED = if_else(
                            GA_Enroll_Condition > 9,
                            WEIGHT_IMPUTED_9, #use imputed weight if GA at enrollment is > 9
                            WEIGHT_ENROLL), #otherwise use original measured weight at enrollment
    
    #flag when imputed weight is used
    WEIGHT_IMPUTED_FLAG=case_when(GA_Enroll_Condition<=9~0,
                                  GA_Enroll_Condition>9~1,
                                  TRUE~NA)
  ) %>%
  group_by(SITE, MOMID, PREGID) %>%
  
  mutate(WEIGHT_ENROLL_TEMP = WEIGHT_ENROLL, #WEIGHT_ENROLL_TEMP will be used to preserve the original values
         WEIGHT_ENROLL = WEIGHT_ENROLL_IMPUTED #The new "WEIGHT_ENROLL" will include both imputed and measured weights
         ) %>% 
  
  ungroup()%>%

### b.  BMI4CAT----
  mutate(BMI=(WEIGHT_ENROLL/(HEIGHT_ENROLL^2))*10000,
         
         BMI4CAT=ifelse(BMI>=12 & BMI<18.5,1, # underweight - cmk: replaced >12 with >=12
                        ifelse(BMI>=18.5 & BMI<25,2, # normal weight
                               ifelse(BMI>=25 & BMI<30,3, # overweight
                                      ifelse(BMI >= 30 & BMI <= 50, 4, NA)))),# obese - cmk: replaced <50 to <=50
         BMI_EARLYPREG=factor(BMI4CAT,levels=c(1,2,3,4),
                              labels= c("Underweight", "Normal Weight", "Overweight", "Obesity")),
         ###c. BMI4CAT_MISS----
         BMI4CAT_MISS = case_when(BMI > 50 | BMI < 12 ~ 1,
                                  is.na(BMI) ~ 2,
                                  TRUE ~ NA_real_)
         )%>%
### c.  GWG_TOTAL----

  group_by(PREGID) %>%
  mutate(
    GWG_TOTAL = if_else(
      PREG_END == 1 & TYPE_VISIT_UPDATED == max(TYPE_VISIT_UPDATED, na.rm = TRUE), #calculate GWG at the final visit (max of type_visit)
      WEIGHT_PERES - WEIGHT_ENROLL,
      NA_real_
    ),
    GWG_TOTAL = coalesce(GWG_TOTAL, last(GWG_TOTAL)) #fills in all other rows will the same PREGID with the GWG_TOTAL calculated above
    #GWG_TOTAL = if_else(GWG_TOTAL < 0, NA_real_, GWG_TOTAL) #sets negative weight gain to 0
  ) %>%
  ungroup()%>%

  mutate(
    GWG_TOTAL_FLAG = case_when(
      GWG_TOTAL == 0 ~ 1,
      GWG_TOTAL >= -10 & GWG_TOTAL <= 30 ~ 0,
      GWG_TOTAL < -10 | GWG_TOTAL > 30 ~ 1,
      TRUE ~ NA_real_
    )
  )%>%
  mutate(GWG_TOTAL = if_else(GWG_TOTAL < 0, NA_real_, GWG_TOTAL))#sets negative weight gain to NA
  #Casey's note: This was previously calculated before GWG_TOTAL_FLAG, so the flag wasn't flagging these values. 
  #I also updated the comment to reflect the code (see above)
  #Moving this should only impact the GWG_TOTAL_FLAG calculation and nothing else


####d. IOM_ADEQUACY----

weight_df3 <- weight_df2 %>%
  group_by(SITE, MOMID, PREGID) %>% 
  
  #calculate lower bound of recommend weight gain by BMI category
  mutate(GWG_REC_LO = case_when(
    BMI4CAT == 1 ~ ((2/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.44),
    BMI4CAT == 2 ~ ((2/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.35),
    BMI4CAT == 3 ~ ((1/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.23),
    BMI4CAT == 4 ~ ((0.5/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.17))) %>% 
  mutate(GWG_REC_LO = if_else(PREG_END==1, GWG_REC_LO, NA)) %>% 
  ungroup() %>%

  #Calculate upper bound of recommended weight gain by BMI category
  group_by(SITE, MOMID, PREGID) %>% 
  mutate(GWG_REC_HI = case_when(
    # First piece is weight gain in the first trimester.  2nd piece is weight gain in the 2nd and 3rd tri (after wk 13)
    BMI4CAT == 1 ~ ((2/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.58),
    BMI4CAT == 2 ~ ((2/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.50),
    BMI4CAT == 3 ~ ((1/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.33),
    BMI4CAT == 4 ~ ((0.5/13.86)*(13.86-BOE_GA_WKS_ENROLL)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.27))) %>% 
  mutate(GWG_REC_HI = if_else(PREG_END==1, GWG_REC_HI, NA)) %>% 
  ungroup() %>%
  
  #round values
  mutate(GWG_REC_LO=round(GWG_REC_LO, digits = 2),
         GWG_REC_HI=round(GWG_REC_HI, digits = 2)
         ) %>%

  #correct values that are < 0
  mutate(GWG_REC_LO = if_else(GWG_REC_LO < 0, NA_real_, GWG_REC_LO),
         GWG_REC_HI = if_else(GWG_REC_HI < 0, NA_real_, GWG_REC_HI)
         )%>%

  #mark if weight gain within recommended range
  group_by(SITE, MOMID, PREGID) %>%
  mutate(IOM_ADEQUACY = case_when(
    (BMI4CAT %in% c(1,2,3,4) & GWG_TOTAL < GWG_REC_LO)  ~ 1,
    (BMI4CAT %in% c(1,2,3,4) & (GWG_TOTAL >= GWG_REC_LO & GWG_TOTAL<= GWG_REC_HI))  ~ 2,
    (BMI4CAT %in% c(1,2,3,4) & GWG_TOTAL > GWG_REC_HI)  ~ 3,
    TRUE ~ NA_real_))%>%
  ungroup()%>%

  ###e. GWG_ADEQUACY----  
  mutate(GWG_ADEQUACY = case_when(
    IOM_ADEQUACY == 1 ~ "Inadequate",
    IOM_ADEQUACY == 2 ~ "Adequate",
    IOM_ADEQUACY == 3 ~ "Excessive",
    TRUE ~ NA_character_
  ))

## Create variables for MAT NUTRIENT report
gwg_data <- weight_df3 %>%
  arrange(MOMID,PREGID,TYPE_VISIT_UPDATED) %>%
  group_by(MOMID,PREGID) %>%
  mutate(VISNUM = row_number(),
         VISTOTAL = n(),
         lastentry = if_else(VISNUM == VISTOTAL,1,0)
  ) %>%
  ungroup() %>%
  filter(lastentry == 1)%>%
  
        ### f. GWG_VALID----
  mutate(GWG_VALID = if_else(!is.na(GWG_TOTAL),1,NA_real_),
         ### g. BMI4CAT_D (denom) ----
         BMI4CAT_D = if_else(BMI_EARLYPREG %in% c("Underweight", "Normal Weight", "Overweight", "Obesity"),1,0),#BMI4CAT is numeric, BMI_EARLYPREG is factor, no difference
         ### h. GWG_MISS----
         GWG_MISS = case_when(is.na(GWG_TOTAL_FLAG)~1,
                              GWG_TOTAL_FLAG==1 ~ 2,# GWG lost more than 10kg or gain more than 30kg, or gain 0 kg (most likely one visit)
                              TRUE ~ NA_real_),
         ### i. BMI4CAT_MISS ----
         BMI4CAT_MISS = case_when(BMI > 50 | BMI < 12 ~ 1,
                                  is.na(BMI) ~ 2,
                                  TRUE ~ NA_real_),# BMI4CAT_MISS = 1(out of range),BMI4CAT_MISS = 2(missing weight gain), missing means within normal range
        
         #Fill in NA's where needed
         BMI4CAT_D = if_else(BMI4CAT_MISS == 1,NA_real_,BMI4CAT_D,missing=BMI4CAT_D), 
         BMI = if_else(BMI4CAT_MISS == 1,NA,BMI,missing = BMI),# replace out of range with missing, create a new variable for this
         BMI4CAT = if_else(BMI4CAT_MISS == 1,NA_real_,BMI4CAT,missing = BMI4CAT),
         GWG_VALID = if_else(BMI4CAT_MISS == 1,NA_real_,GWG_VALID,missing = GWG_VALID),# keep a cohort not out of range & not missing, have a clean cohort
         GWG_ADEQUACY = if_else(BMI4CAT_MISS %in% c(1,2) | GWG_MISS %in% c(1,2),NA_character_,GWG_ADEQUACY,missing = GWG_ADEQUACY),
         GWG_TOTAL = if_else(GWG_TOTAL_FLAG == 1, NA_real_, GWG_TOTAL)
  )

temp.df <-gwg_data %>%
  select(SITE,MOMID,PREGID,BMI,BMI4CAT,BMI_EARLYPREG,BMI4CAT_D,BMI4CAT_MISS,GWG_TOTAL,GWG_TOTAL_FLAG,GWG_VALID,GWG_ADEQUACY,IOM_ADEQUACY,PREG_END)


weight_df2%>%
  tabyl(BMI4CAT_MISS, SITE)

gwg_data%>%
  tabyl(BMI4CAT_MISS, SITE)

#************************
# Export intermediary datasets----
#************************
#*
## For PPWC section
gwg_outcome <- weight_df3 %>%
  select(SITE, SCRNID, MOMID, PREGID, TYPE_VISIT_UPDATED,	ANT_PEDAT,HEIGHT_ENROLL,WEIGHT_PERES,WEIGHT_ENROLL,BMI,BMI_EARLYPREG,BMI4CAT,WEIGHT_IMPUTED_FLAG, IOM_ADEQUACY, PREG_END)

old_old_result<-read.csv("Z:/Constructed Data/2026-09-18/GWG_OUTCOME_LONG_ALL.csv")
old_result<-read.csv(paste0("data_out/",'GWG_OUTCOME_LONG_ALL_', UploadDate, ".csv"))
new_result<-gwg_outcome

old_old_result%>%tabyl(BMI4CAT, SITE)
old_result%>%tabyl(BMI4CAT, SITE)
new_result%>%tabyl(BMI4CAT,SITE)

write.csv(gwg_outcome, paste0("data_out/",'GWG_OUTCOME_LONG_ALL_', UploadDate, ".csv"))

## For MAT_GWG
mat_gwg_subset <- gwg_data %>%
  filter(PREG_END==1 & SINGLETON==1) %>%
  select(SITE,MOMID,PREGID,PREG_END,GWG_TOTAL,GWG_ADEQUACY,GWG_VALID,GWG_MISS,BMI,BMI4CAT,BMI4CAT_MISS,BMI4CAT_D, SINGLETON)

write_dta(mat_gwg_subset, paste0("data_out/","MAT_GWG_",UploadDate,".dta"))

colnames(mat_gwg_subset)

#*** end GWG----
#_____----

#PPWC Section----


#*****************************************************************************
#* PRISMA Postpartum Weight Change Outcome 
#* Drafted: 23 October 2025, Precious Williams `williams_pj@gwu.edu`
#* Last updated: 28 September 2026
#* Added instructions for running the code what needs to be done or known
#* Updated no weight change to be 0.25kg (would apply from 9/04 constructed data)
#*****************************************************************************
# This script creates the PRISMA supplemental postpartum weight change
# outcome dataset (MAT_PPWC).
#
# The script:
#   1. Loads maternal enrollment, pregnancy endpoint, flowchart,
#      gestational weight gain, and MNH05/MNH23 data.
#   2. Identifies mothers who were enrolled through pregnancy end.
#   3. Determines whether each postpartum visit was expected based on:
#        - pregnancy end date,
#        - the current data upload date, and
#        - study closeout date.
#   4. Extracts maternal postpartum weights from MNH05.
#   5. Reassigns weight records to postpartum visit windows based on
#      the number of days since pregnancy end.
#   6. Calculates postpartum weight change (PPWC) and percent PPWC
#      relative to enrollment weight.
#   7. Assigns query indicators for missing, invalid, or unusual values.
#   8. Creates one record per mother/pregnancy with postpartum weight
#      outcomes organized by visit.
#   9. Exports the final MAT_PPWC.csv dataset.
# *****************************************************************************



# *****************************************************************************
##Define Variables and Paths ----
# *****************************************************************************

# # Define main directory and file paths
# path_to_data <- paste0("~/OutcomeDatasets/Analysis/Merged_data/", UploadDate)
# # Create the directory if it doesn't exist
# if (!file.exists(path_to_data)) {
#   dir.create(path_to_data, recursive = TRUE)
# }
# 
# path_to_stacked <- paste0("Z:/Stacked Data/", UploadDate)
# path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate)
# 
# # Define the path
# path_to_save <- paste0("~/OutcomeDatasets/Analysis/PP_OUTCOMES/", UploadDate)
# # Expand the path (in case of ~)
# path_to_save <- path.expand(path_to_save)
# # Create the directory if it doesn't exist
# if (!file.exists(path_to_save)) {
#   dir.create(path_to_save, recursive = TRUE)
# }
# 
# # Conditional loads (prefer .rda, else read raw + save .rda)
# # PJW comment: Basically i usually load the dataset and save them as rda files, then reload easily whenever I need
# 
# # MAT_ENDPOINTS
# if (file.exists(file.path(path_to_data, "MAT_ENDPOINTS.rda"))) {
#   load(file.path(path_to_data, "MAT_ENDPOINTS.rda"))   # loads: mat_endpoints
# } else {
#   mat_endpoints <- haven::read_dta(file.path(path_to_tnt, "MAT_ENDPOINTS.dta"))
#   save(mat_endpoints, file = file.path(path_to_data, "MAT_ENDPOINTS.rda"))
# }
# 
# # MAT_ENROLL
# if (file.exists(file.path(path_to_data, "MAT_ENROLL.rda"))) {
#   load(file.path(path_to_data, "MAT_ENROLL.rda"))      # loads: mat_enroll
# } else {
#   mat_enroll <- read.csv(file.path(path_to_tnt, "MAT_ENROLL.csv"))
#   save(mat_enroll, file = file.path(path_to_data, "MAT_ENROLL.rda"))
# }
# 
# # MAT_FLOWCHART
# if (file.exists(file.path(path_to_data, "MAT_FLOWCHART.rda"))) {
#   load(file.path(path_to_data, "MAT_FLOWCHART.rda"))      # loads: mat_enroll
# } else {
#   mat_flowchart <- read.csv(file.path(path_to_tnt, "MAT_FLOWCHART.csv"))
#   save(mat_flowchart, file = file.path(path_to_data, "MAT_FLOWCHART.rda"))
# }

mat_flowchart <- mat_flowchart %>% 
  filter (PREG_END_ENROLLED == 1 & PRISMA_ENROLL == 1)

# GWG_OUTCOME_LONG (note: source file is date-stamped; rda is stable name)
# if (file.exists(file.path(path_to_data, "GWG_OUTCOME_LONG.rda"))) {
#   load(file.path(path_to_data, "GWG_OUTCOME_LONG.rda"))  # loads: gwg_outcome
# } else {
#   gwg_outcome <- read.csv(file.path(path_to_tnt, paste0("GWG_OUTCOME_LONG_ALL", ".csv")))
#   save(gwg_outcome, file = file.path(path_to_data, "GWG_OUTCOME_LONG.rda"))
# }
# 
# # mnh05 merged
# if (file.exists(file.path(path_to_data, "mnh05.rda"))) {
#   load(file.path(path_to_data, "mnh05.rda"))           # loads: mnh05
# } else {
#   mnh05 <- read.csv(file.path(path_to_stacked, "mnh05_merged.csv"))
#   # Inline renaming across ALL data frames (no function)
#   # - Remove leading "M.._" from non-ID cols
#   # - Keep ID variables as-is
#   
#   # Loop through each data frame, excluding specific "ID" variables
#   for (df_name in df_names) {
#     df <- get(df_name)  # Retrieve the data frame by name
#     if (is.data.frame(df)) {  # Check if the object is a data frame
#       assign(df_name, rename_variables(df, "M05"), envir = .GlobalEnv)
#     }
#   }
#   
#   save(mnh05, file = file.path(path_to_data, "mnh05.rda"))
#   
# }
# 
# 
# # mnh23 merged
# if (file.exists(file.path(path_to_data, "mnh23.rda"))) {
#   load(file.path(path_to_data, "mnh23.rda"))           # loads: mnh23
# } else {
#   mnh23 <- read.csv(file.path(path_to_stacked, "mnh23_merged.csv"))
#   # Inline renaming across ALL data frames (no function)
#   # - Remove leading "M.._" from non-ID cols
#   # - Keep ID variables as-is
#   df_names <- ls(pattern = "^mnh", envir = .GlobalEnv)
#   # Define a function to rename variables in a data frame
#   rename_variables <- function(df) {
#     # Exclude specific "ID" variables from renaming
#     exclude_variables <- c("MOMID", "PREGID", "SCRNID", "INFANTID", "SITE")
#     id_variables <- intersect(exclude_variables, names(df))
#     var_names <- names(df)[!names(df) %in% id_variables]
#     
#     # Rename the variables
#     new_var_names <- gsub("^M23_", "", var_names)  # Remove the first three characters and underscore
#     names(df)[!names(df) %in% id_variables] <- new_var_names
#     
#     return(df)
#   }
#   
#   # Loop through each data frame, excluding specific "ID" variables
#   for (df_name in df_names) {
#     df <- get(df_name)  # Retrieve the data frame by name
#     if (is.data.frame(df)) {  # Check if the object is a data frame
#       assign(df_name, rename_variables(df), envir = .GlobalEnv)
#     }
#   }
#   
#   save(mnh23, file = file.path(path_to_data, "mnh23.rda"))
#   
# }



# *****************************************************************************
## 1. Data processing enrolment, closeout information and gwg----
# *****************************************************************************
#mnh23
mnh23_df <- mnh23 %>% 
  mutate(
    STOP_DATE_M23 = clean_date(
      CLOSE_DSSTDAT,
      min_date = ymd("2018-01-01"),
      max_date = ymd(UploadDate)
    )
  ) %>%
  filter(!is.na(STOP_DATE_M23)) %>%
  arrange(SITE, MOMID, PREGID, STOP_DATE_M23) %>%
  group_by(SITE, MOMID, PREGID) %>%
  slice_head(n = 1) %>%         # keep earliest STOP_DATE_M23
  ungroup() %>%
  select(SITE, MOMID, PREGID, STOP_DATE_M23)

##BMI4CAT_ENROLL----
#all baseline gestational weight gain
baseline_gwg <- gwg_outcome %>% 
  filter (TYPE_VISIT_UPDATED == 1) %>% 
  mutate (
    #CMK: replace out of range values with 55/Missing. This was not being done previously. May impact output for PPWC
    BMI = case_when(
            BMI < 12 | BMI > 50 ~ NA_real_,
            TRUE ~ BMI
    ),
    
    BMI4CAT_ENROLL = case_when(
      # keep existing category if already present
      !is.na(BMI4CAT) ~ BMI4CAT,
      # if BMI missing and category missing
      is.na(BMI) ~ 55,
      # derive from BMI
      BMI < 18.5 ~ 1,
      BMI >= 18.5 & BMI < 25 ~ 2,
      BMI >= 25 & BMI < 30 ~ 3,
      BMI >= 30 ~ 4,
      TRUE ~ 55
    )
  ) %>%
  select ( SITE,	MOMID,	PREGID,	
           MAT_WEIGHT_ENROLL_DT = ANT_PEDAT,	
           MAT_HEIGHT_ENROLL = HEIGHT_ENROLL, 
           MAT_WEIGHT_ENROLL = WEIGHT_ENROLL,
           ##BMI ENROLL ----
           BMI_ENROLL = BMI, 
           BMI4CAT_ENROLL,	
           WEIGHT_IMPUTED_IND = WEIGHT_IMPUTED_FLAG)


# Left-bound (first eligible day) for each PNC visit
pnc_late_days <- c(
  PNC0  = 5,
  PNC1  = 14,
  PNC4  = 35,
  PNC6  = 104,
  PNC26 = 279,
  PNC52 = 454
)
#enrolled mothers
#Creates flags for which PNC visits are expected
enrolled_end <- mat_flowchart %>% 
  filter(PRISMA_ENROLL== 1, PREG_END_ENROLLED == 1) %>% 
  select(SITE, MOMID, PREGID, PREG_END_ENROLLED) %>% 
  left_join(mat_endpoints, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(mnh23_df,     by = c("SITE", "MOMID", "PREGID")) %>%
  mutate(
    UploadDate    = ymd(UploadDate),
    DOB           = clean_date(PREG_END_DATE, min_date = ymd("2018-01-01"), max_date = UploadDate),
    DOB           = ymd(DOB),
    STOP_DATE_M23 = ymd(STOP_DATE_M23)
  ) %>%
  { 
    dat <- .
    bind_cols(
      dat,
      purrr::imap_dfc(pnc_late_days, function(days_late, visit) {
        late_window <- dat$DOB + days(days_late)
        
        tibble(
          !!paste0(visit, "_LATE_WINDOW") := late_window,
          !!paste0(visit, "_PNC_EXPECTED") := as.integer(
            !is.na(dat$DOB) &
              !is.na(dat$UploadDate) &
              dat$UploadDate > late_window &
              (is.na(dat$STOP_DATE_M23) | dat$STOP_DATE_M23 > late_window)
          )
        )
      })
    )
  } %>% 
  select (SITE,	MOMID, PREGID, PREG_END, PREG_END_ENROLLED, PREG_END_GA, PREG_END_DATE, 
          PNC0_PNC_EXPECTED, PNC1_PNC_EXPECTED, PNC4_PNC_EXPECTED, STOP_DATE_M23,
          PNC6_PNC_EXPECTED, PNC26_PNC_EXPECTED, PNC52_PNC_EXPECTED, MAT_DEATH, MAT_DEATH_DATE)

# *****************************************************************************
## 2. Data processing mnh05 weight ----
# *****************************************************************************
#### Filter data ----
mnh05_filter <- mnh05 %>%
  filter(
    MAT_VISIT_MNH05 %in% c(1, 2),
    MAT_VITAL_MNH05 == 1,
    TYPE_VISIT %in% c(6:12,14),
    WEIGHT_PEPERF == 1)  %>%
  mutate(
    WEIGHT_PERES = as.numeric(WEIGHT_PERES),
    WEIGHT_PERES = ifelse (WEIGHT_PERES < 0, NA, WEIGHT_PERES)
  )%>% 
  
  select(MOMID, PREGID, SITE, TYPE_VISIT, ANT_PEDAT, WEIGHT_PERES, WEIGHT_PEPERF)

weight_df <- mnh05_filter %>% 
  right_join(enrolled_end, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(baseline_gwg, by = c("SITE", "MOMID", "PREGID"))%>%
  #### Replace -7 and -5 with NA ----
  mutate(
    across(where(is.numeric),
           ~ replace(.x, .x %in% c(-7,-5, -5.0, -7.0), NA))
  )

# Replace -7 and -5 with NA 
# weight_df[weight_df %in% c(-7, -5, -5.0, -7.0,  ".")] <- NA

#### Sort the data----
long_weights <- weight_df %>%
  # Convert ANT_PEDAT and DOB to Date format
  mutate(WEIGHT_DATE = as.Date(ANT_PEDAT),
         DOB = as.Date(PREG_END_DATE),
         MAT_DEATH_DATE = as.Date(MAT_DEATH_DATE),
         # Calculate AGE_INF as the difference in days between ANT_PEDAT and DOB
         AGE_VISIT = as.numeric(WEIGHT_DATE - DOB),
         
         # Update TYPE_VISIT_UPDATE based on the calculated AGE_INF
         TYPE_VISIT_UPDATE = case_when(
           is.na (DOB) ~ TYPE_VISIT,
           # Postnatal visit types
           TYPE_VISIT == 6 | AGE_VISIT < 3 ~ 6,
           AGE_VISIT >= 3 & AGE_VISIT <= 5 ~ 7,
           AGE_VISIT >= 7 & AGE_VISIT <= 14 ~ 8,
           AGE_VISIT >= 28 & AGE_VISIT <= 35 ~ 9,
           AGE_VISIT >= 42 & AGE_VISIT <= 104 ~ 10,
           AGE_VISIT >= 182 & AGE_VISIT <= 279 ~ 11,
           AGE_VISIT > 279 & AGE_VISIT <= 454 ~ 12, #CMK: Changed this from >= 279 to >279. Shouldn't change output
           
           # .5 visits between age-based windows
           AGE_VISIT > 5 & AGE_VISIT < 7 ~ 7.5,
           AGE_VISIT > 14 & AGE_VISIT < 28 ~ 8.5,
           AGE_VISIT > 35 & AGE_VISIT < 42 ~ 9.5,
           AGE_VISIT > 104 & AGE_VISIT < 182 ~ 10.5,
           
           TRUE ~ NA), # Default case
         
         TV_EQ = ifelse(TYPE_VISIT == TYPE_VISIT_UPDATE, 1, 0),
         
         PPWC = case_when(
           !is.na(MAT_WEIGHT_ENROLL) & !is.na(WEIGHT_PERES) ~ WEIGHT_PERES - MAT_WEIGHT_ENROLL,
           TRUE ~ NA), 
         PERCENT_PPWC = case_when(
           !is.na (PPWC) ~ round(PPWC / MAT_WEIGHT_ENROLL * 100, 2),
           TRUE ~ NA_real_
         ),
         #Updated this 9/28/2026 (all outcome datasets before this will have no weight gain at 0)
         PNWR = case_when(
           !is.na(PPWC) & PPWC > 0.25 ~ 0,                  # weight gain > 0.25 kg
           !is.na(PPWC) & PPWC >= -0.25 & PPWC <= 0.25 ~ 1, # no weight change: ±0.25 kg
           !is.na(PPWC) & PPWC < -0.25 ~ 2,                 # weight loss > 0.25 kg
           TRUE ~ NA_real_
         ),
         PPWC_IND_QUERY = case_when(
           !is.na(AGE_VISIT) & AGE_VISIT < 0 ~ 1L,                                 # date error
           MAT_DEATH_DATE <= WEIGHT_DATE & MAT_DEATH == 1 ~ 2L,                    # maternal death
           (!is.na(PERCENT_PPWC) & (PERCENT_PPWC < -30 | PERCENT_PPWC > 50)) |     # % outliers
             (!is.na(PPWC) & (PPWC < -25 | PPWC > 25)) ~ 3L,                       # kg outliers
           is.na(PPWC) | is.na(PERCENT_PPWC) ~ 4L,                                 # missing
           TRUE ~ 0L)) %>%
  # Arrange rows by MOMID, PREGID, and TYPE_VISIT for consistent ordering
  arrange(SITE, MOMID, PREGID, TYPE_VISIT) 


# Filter and arrange the data
# Keep only integer postnatal visit types (6–12) and deduplicate within visit
long_weights_int <- long_weights %>%
  filter(TYPE_VISIT_UPDATE %in% c(6:12)) %>%
  # Prioritize non-missing weights and latest WEIGHT_DATE
  mutate(non_missing_wt = !is.na(WEIGHT_PERES)) %>%
  arrange(SITE, MOMID, PREGID, TYPE_VISIT_UPDATE, desc(non_missing_wt), desc(WEIGHT_DATE)) %>%
  group_by(SITE, MOMID, PREGID, TYPE_VISIT_UPDATE) %>%
  arrange(WEIGHT_DATE, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%         # keep latest WEIGHT_DATE within the visit
  ungroup()

# Dataset PPWC ----
# Pivot to wide: WEIGHT, WEIGHT_DATE, PPWC, and PERCENT_PPWC
weights_wide <- long_weights_int %>%
  # keep only what you need; any_of() avoids errors if some cols aren't present
  select(any_of(c(
    "SITE", "MOMID", "PREGID",
    "TYPE_VISIT_UPDATE",
    "WEIGHT_PERES", "WEIGHT_DATE",
    "PPWC", "PERCENT_PPWC", "PNWR", "PPWC_IND_QUERY"
  ))) %>%
  # rename BEFORE widening so names are consistent
  rename(MAT_WEIGHT = WEIGHT_PERES) %>%
  pivot_wider(
    id_cols    = c(SITE, MOMID, PREGID),
    names_from = TYPE_VISIT_UPDATE,
    values_from = c(MAT_WEIGHT, WEIGHT_DATE, PPWC, PERCENT_PPWC, PNWR, PPWC_IND_QUERY),
    names_glue = "{.value}_V{TYPE_VISIT_UPDATE}"
  ) %>%
  right_join(
    enrolled_end %>%
      select(any_of(c(
        "SITE", "MOMID", "PREGID",
        "PNC0_PNC_EXPECTED", "PNC1_PNC_EXPECTED", "PNC4_PNC_EXPECTED",
        "PNC52_PNC_EXPECTED", "PNC26_PNC_EXPECTED", "PNC6_PNC_EXPECTED",
        "MAT_DEATH", "MAT_DEATH_DATE", "STOP_DATE_M23"
      ))),
    by = c("SITE", "MOMID", "PREGID")
  ) %>%
  left_join(
    baseline_gwg %>%
      select(any_of(c(
        "SITE", "MOMID", "PREGID", "BMI4CAT_ENROLL", "BMI_ENROLL",
        "MAT_WEIGHT_ENROLL", "WEIGHT_IMPUTED_IND", "MAT_WEIGHT_ENROLL_DT"
      ))),
    by = c("SITE", "MOMID", "PREGID")
  ) %>%
  mutate(
    PPWC_IND_QUERY_V6 = case_when(is.na (PPWC_IND_QUERY_V6) & is.na(PPWC_V6) & MAT_DEATH == 1 ~ 2,
                                  is.na (PPWC_IND_QUERY_V6) & is.na(PPWC_V6) & MAT_DEATH != 1 ~ 4,
                                  TRUE ~ PPWC_IND_QUERY_V6),
    PPWC_IND_QUERY_V7 = case_when(is.na (PPWC_IND_QUERY_V7) & is.na(PPWC_V7) & PNC0_PNC_EXPECTED == 1 ~ 4,
                                  is.na(PPWC_V7) & PNC0_PNC_EXPECTED != 1 ~ 77,
                                  TRUE ~ PPWC_IND_QUERY_V7),
    PPWC_IND_QUERY_V8 = case_when(is.na (PPWC_IND_QUERY_V8) & is.na(PPWC_V8) & PNC1_PNC_EXPECTED == 1 ~ 4,
                                  is.na(PPWC_V8) & PNC1_PNC_EXPECTED != 1 ~ 77,
                                  TRUE ~ PPWC_IND_QUERY_V8),
    PPWC_IND_QUERY_V9 = case_when(is.na (PPWC_IND_QUERY_V9) & is.na(PPWC_V9) & PNC4_PNC_EXPECTED == 1 ~ 4,
                                  is.na(PPWC_V9) & PNC4_PNC_EXPECTED != 1 ~ 77,
                                  TRUE ~ PPWC_IND_QUERY_V9),
    PPWC_IND_QUERY_V10 = case_when(is.na (PPWC_IND_QUERY_V10) & is.na(PPWC_V10) & PNC6_PNC_EXPECTED == 1 ~ 4,
                                   is.na(PPWC_V10) & PNC6_PNC_EXPECTED != 1 ~ 77,
                                   TRUE ~ PPWC_IND_QUERY_V10),
    PPWC_IND_QUERY_V11 = case_when(is.na (PPWC_IND_QUERY_V11) & is.na(PPWC_V11) & PNC26_PNC_EXPECTED == 1 ~ 4,
                                   is.na(PPWC_V11) & PNC26_PNC_EXPECTED != 1 ~ 77,
                                   TRUE ~ PPWC_IND_QUERY_V11),
    PPWC_IND_QUERY_V12 = case_when(is.na (PPWC_IND_QUERY_V12) & is.na(PPWC_V12) & PNC52_PNC_EXPECTED == 1 ~ 4,
                                   is.na(PPWC_V12) & PNC52_PNC_EXPECTED != 1 ~ 77,
                                   TRUE ~ PPWC_IND_QUERY_V12),
    
    ##PPWC_PNC*_DENOM----
    PPWC_IPC_DENOM = if_else(MAT_DEATH != 1L, 1L, 0L),
    PPWC_PNC0_DENOM  = if_else(PPWC_IND_QUERY_V7  == 0L | PNC0_PNC_EXPECTED  == 1L, 1L, 0),
    PPWC_PNC1_DENOM  = if_else(PPWC_IND_QUERY_V8  == 0L | PNC1_PNC_EXPECTED  == 1L, 1L, 0),
    PPWC_PNC4_DENOM  = if_else(PPWC_IND_QUERY_V9  == 0L | PNC4_PNC_EXPECTED  == 1L, 1L, 0),
    PPWC_PNC6_DENOM  = if_else(PPWC_IND_QUERY_V10 == 0L | PNC6_PNC_EXPECTED  == 1L, 1L, 0),
    PPWC_PNC26_DENOM = if_else(PPWC_IND_QUERY_V11 == 0L | PNC26_PNC_EXPECTED == 1L, 1L, 0),
    PPWC_PNC52_DENOM = if_else(PPWC_IND_QUERY_V12 == 0L | PNC52_PNC_EXPECTED == 1L, 1L, 0)
  ) %>% 
  select(SITE, MOMID, PREGID, MAT_WEIGHT_ENROLL, WEIGHT_IMPUTED_IND, 
         MAT_WEIGHT_ENROLL_DT, BMI4CAT_ENROLL, BMI_ENROLL, everything())

# *****************************************************************************
# 6. Combined Postpartum Dataset
# *****************************************************************************
#First we rename all the weight variables from V to PNC0 to PNC52
#rename weight- and PPWC-related columns by visit mapping
rename_visits <- c(
  V6 = "IPC", #Casey: V6 is called below but not assigned here
  V7 = "PNC0",
  V8 = "PNC1",
  V9 = "PNC4",
  V10 = "PNC6",
  V11 = "PNC26",
  V12 = "PNC52"
)

weights_renamed <- weights_wide %>%
  rename_with(
    ~ {
      nm <- .
      for (old in names(rename_visits)) {
        nm <- gsub(paste0("_", old, "$"), paste0("_", rename_visits[[old]]), nm)
      }
      nm
    },
    matches("_(V6|V7|V8|V9|V10|V11|V12)$")
  )

#Reorder columns in the correct visit order ---
visit_order <- c("IPC", "PNC0", "PNC1", "PNC4", "PNC6", "PNC26", "PNC52")

# Separate ID variables and measurement variables
id_vars <- c("SITE", "MOMID", "PREGID", "MAT_WEIGHT_ENROLL", "WEIGHT_IMPUTED_IND", 
             "BMI4CAT_ENROLL", "BMI_ENROLL", "PPWC_IPC_DENOM",
             "PPWC_PNC0_DENOM", "PPWC_PNC1_DENOM", "PPWC_PNC4_DENOM",
             "PPWC_PNC6_DENOM", "PPWC_PNC26_DENOM", "PPWC_PNC52_DENOM")

weights_ordered <- weights_renamed %>%
  select(
    all_of(id_vars),
    # Reorder the rest of the columns according to visit order
    tidyselect::matches(
      paste0("(", paste0(visit_order, collapse = "|"), ")$")
    )
  ) %>%
  # Arrange columns internally by visit order
  select(
    all_of(id_vars),
    # build ordered pattern so that all measures per visit appear together
    any_of(unlist(lapply(
      visit_order,
      function(v) grep(paste0("_", v, "$"), names(weights_renamed), value = TRUE)
    )))
  ) 

mat_ppwc <- weights_ordered %>% 
  select (SITE, MOMID, PREGID, MAT_WEIGHT_ENROLL, WEIGHT_IMPUTED_IND, 
          BMI4CAT_ENROLL, BMI_ENROLL, contains("PNC", ignore.case = FALSE)) 

#write.csv(mat_ppwc, paste0(path_to_tnt, "/MAT_PPWC" ,".csv"), na="", row.names=FALSE)
write.csv(mat_ppwc, paste0(path_to_save, "/MAT_PPWC" ,".csv"), na="", row.names=FALSE)

table (mat_ppwc$SITE, mat_ppwc$BMI4CAT_ENROLL)


# *** end PPWC ###----
#______----

##compare columns in the two datasets
gwg_vars_to_fill<-data.frame(colnames = colnames(mat_gwg_subset))%>%
                  full_join(
                    .,
                    data.frame(colnames = colnames(mat_ppwc)),
                    by=c("colnames"),
                    keep=TRUE
                  )%>%
                  rename(vars_mat_gwg_subset="colnames.x",
                         vars_mat_ppwc="colnames.y")%>%
                  filter(is.na(vars_mat_ppwc))%>%
                  pull(vars_mat_gwg_subset)

gwg_vars_to_fill

## MERGE AND EXPORT PPWC + GWG FILE ---- 

mat_ppwc%>%
  count(SITE,MOMID,PREGID)%>%
  filter

nrow(mat_ppwc)

mat_gwg_subset%>%
  count(SITE,MOMID,PREGID)%>%
  filter(n>1)

nrow(mat_gwg_subset)

final_export<-mat_ppwc%>%
              full_join(.,
                        mat_gwg_subset,
                        by=c("SITE","MOMID","PREGID"),
                        #keep=TRUE
                        keep=FALSE
              )
            #%>%
            #   #fill in "not applicable" value when record is present in mat_ppwc but not gwg
            #   mutate(
            #     across(
            #       all_of(setdiff(gwg_vars_to_fill, "GWG_ADEQUACY")),
            #       
            #       ~ case_when(
            #         !is.na(PREGID.y) ~ .x,
            #         cur_column() %in% c("PREG_END", "GWG_MISS", "GWG_VALID", "BMI4CAT", "BMI4CAT_MISS", "BMI4CAT_D", "SINGLETON") ~ 77,
            #         cur_column() %in% c("BMI","GWG_TOTAL") ~ -7,
            #         TRUE ~ NA_real_
            #       )
            #     ) 
            #   )%>%
            # mutate(
            #   GWG_ADEQUACY = if_else(!is.na(PREGID.y), GWG_ADEQUACY, NA_character_)
            # )%>%
            # mutate(
            #   SITE=coalesce(SITE.x, SITE.y),
            #   MOMID=coalesce(MOMID.x, MOMID.y),
            #   PREGID=coalesce(PREGID.x, PREGID.y)
            # )%>%
            # select(-matches("\\.x$"), -matches("\\.y$"))
            
colnames(final_export)

final_export%>%
  filter(is.na(PREGID.x) | is.na(PREGID.y))%>%
  View()

full_join(final_export, mat_flowchart, by=c("SITE","MOMID","PREGID"), keep=TRUE)%>%
  select(matches("^MOMID|^PREGID|^SITE"))%>%
  filter(is.na(PREGID.y))%>%
  View()

#check how numbers changed prior to writing to file
previous_result<-read.csv(paste0(path_to_save,"/MAT_WEIGHT_CHANGE",".csv"))
new_result<-final_export

previous_result%>%
  mutate(SITE=coalesce(SITE.y, SITE.x))%>%
  tabyl(BMI4CAT, SITE)

new_result%>%
  tabyl(BMI4CAT, SITE)

write.csv(final_export, paste0(path_to_save,"/MAT_WEIGHT_CHANGE",".csv"))
write_dta(final_export, paste0(path_to_save,"/MAT_WEIGHT_CHANGE",".dta"))

#QC----
#check for NAs

#compare the joined dataset to the individual datasets

gwg<-read_dta("data_out/MAT_GWG_2026-09-18.dta")
ppwc<-read_csv("data_out/MAT_PPWC.csv")
joint<-read.csv("data_out/MAT_WEIGHT_CHANGE.csv")

gwg%>%
  tabyl(BMI4CAT_D, SITE)

joint%>%tabyl(BMI4CAT_D, SITE) 

ppwc%>%
  tabyl(PPWC_PNC0_DENOM, SITE)

joint%>%
  tabyl(PPWC_PNC0_DENOM, SITE)

joint%>%
  filter(is.na(PPWC_PNC0_DENOM))%>%
  View()

joint%>%
  tabyl(GWG_ADEQUACY, SITE)

#____----
##check imputed vs. observed----
# mat_ppwc <- mat_ppwc %>%
# left_join(
#   baseline_gwg %>%
#     select(SITE, MOMID, PREGID, BMI4CAT_ENROLL, IOM_ADEQUACY),
#   by = c("SITE", "MOMID", "PREGID")
# ) %>%
# mutate(
#   BMI4CAT_ENROLL = factor(BMI4CAT_ENROLL,
#                           levels = 0:3,
#                           labels = c("Underweight", "Normal weight", "Overweight", "Obese")
#   ),
#   IOM_ADEQUACY = factor(IOM_ADEQUACY,
#                         levels = 0:2,
#                         labels = c("Inadequate", "Adequate", "Excessive")
#   ))
# names (mat_ppwc)
# #ggplot for imputed vs observed
# mat_ppwc %>%
#   filter(!is.na(MAT_WEIGHT_ENROLL) & 
#          !is.na(WEIGHT_IMPUTED_IND) &
#          !is.na(BMI4CAT_ENROLL) &
#            BMI4CAT_ENROLL !=55 &
#          MAT_WEIGHT_ENROLL<500 #remove extreme observation
#         ) %>%
#   ggplot(aes(
#              x = factor(WEIGHT_IMPUTED_IND), 
#              y = MAT_WEIGHT_ENROLL,
#              fill = factor(WEIGHT_IMPUTED_IND)
#              )) +
#   geom_boxplot() +
#   scale_fill_manual(values = c("#56B4E9", "#E69F00"),
#                     name = "Imputation",
#                     labels = c("Observed", "Imputed")) +
#   labs(x = "BMI category", y = "Early pregnancy weight (kg)",
#        title = "Comparison of Observed vs Imputed Early Pregnancy Weights") +
#   theme_bw(base_size = 13)+
#   facet_wrap(~BMI4CAT_ENROLL)



