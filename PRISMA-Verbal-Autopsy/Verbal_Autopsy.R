
#*****************************************************************************
#* PRISMA Verbal Autopsy Report: 
#* Drafted: 25 June 2024, Precious Williams
#* Last updated: 19 July 2024

# 
#*****************************************************************************

# clear environment 
rm(list = ls())


library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(openxlsx)
library(haven)
library(dplyr)
library(readr)
library(stringr)


#*****************************************************************************
#* load data
#*****************************************************************************

## UPDATE EACH RUN ## 
UploadDate = "2024-06-28"

#Set your main directory 
path_to_data <- paste0("~/Analysis/Merged_data/", UploadDate)
path_to_save <- paste0("~/Analysis/COD/data/")
path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")

# import forms 
mnh24 <- read_csv (paste0(path_to_data, "/mnh24_merged.csv"))

mnh24_trans <- mnh24 %>%
  rename(
    CLOSE_DSDECOD_1 = CLOSE_DSDECOD,
    CLOSE_DSDECOD_2 = M24_CLOSE_DSDECOD,
    CLOSE_DSSTDAT_1 = CLOSE_DSSTDAT,
    CLOSE_DSSTDAT_2 = M24_CLOSE_DSSTDAT,
    MOMID_1 = MOMID,
    MOMID_2 = M24_MOMID,
    PREGID_1 = PREGID,
    PREGID_2 = M24_PREGID
  )

mnh24_trans_dte <- mnh24_trans %>%
  mutate(
    CLOSE_DSSTDAT_1 = ymd(parse_date_time(CLOSE_DSSTDAT_1, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
    CLOSE_DSSTDAT_2 = ymd(parse_date_time(as.Date(CLOSE_DSSTDAT_2, origin = "1970-01-01"), orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))
  )


# Combine the variables into one unified variable for each pair
mnh24_combined <- mnh24_trans_dte %>%
  mutate(
    CLOSE_DSDECOD = coalesce(CLOSE_DSDECOD_1, CLOSE_DSDECOD_2),
    CLOSE_DSSTDAT = coalesce(CLOSE_DSSTDAT_1, CLOSE_DSSTDAT_2),
    MOMID = coalesce(MOMID_1, MOMID_2),
    PREGID = coalesce(PREGID_1, PREGID_2)
  ) %>%  select(SITE, MOMID, PREGID, INFANTID, DTHDAT = M24_DTHDAT , DTHTIM = M24_DTHTIM , CLOSE_DSDECOD, CLOSE_DSSTDAT ) 

# # import forms 
mat_mortality <- read_dta(paste0(path_to_data,"/mortality_collapsed_wide.dta"))
inf_mortality <- read.csv(paste0(path_to_tnt, "INF_OUTCOMES.csv"))

infant_dob <- read_csv(paste0(path_to_data, "/INFANT_DOB_as_of_2024-06-28.csv"))
mat_enroll <- read_csv (paste0(path_to_tnt,"MAT_ENROLL.csv"))

#Read in the outcome (For Maternal Death)
#where deaths are reported 
#MNH09, MNH10, MNH12, MNH19, MNH23
#So now we have death date, if they have MNH23 and if they are missing death death

mat_death <- mat_mortality %>% 
  filter (MAT_DEATH_MNH04 == 1 | MAT_DEATH_MNH09 == 1|
          MAT_DEATH_MNH10 == 1 | MAT_DEATH_MNH12 == 1|
          MAT_DEATH_M19 == 1 | MAT_DEATH == 1) %>%
  select (SITE = site, MOMID = momid , PREGID = pregid,
          MAT_DEATH_MNH04_DATE, VISIT_DOD_MNH04, MAT_DEATH_MNH04,
          MAT_DEATH_MNH09_DATE, VISIT_DOD_MNH09, MAT_DEATH_MNH09,
          MAT_DEATH_MNH10_DATE, VISIT_DOD_MNH10, MAT_DEATH_MNH10,
          MAT_DEATH_MNH12_DATE, VISIT_DOD_MNH12_DATE, MAT_DEATH_MNH12,
          MAT_DEATH_DATE_M19, MAT_DEATH_M19, MAT_DEATH, 
          CLOSEOUT_DT, CLOSE_DSDECOD = close_dsdecod, DTHDAT = dthdat, DTHTIM = dthtim)

# Standardizing date formats
date_columns <- c("MAT_DEATH_MNH04_DATE", "MAT_DEATH_MNH09_DATE", "MAT_DEATH_MNH10_DATE", "MAT_DEATH_MNH12_DATE", "MAT_DEATH_DATE_M19", "CLOSEOUT_DT", "DTHDAT")

mat_death[date_columns] <- lapply(mat_death[date_columns], function(x) {
  ymd(parse_date_time(x, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))
})

# Creating new columns based on conditions
mat_death_merge <- mat_death %>%
  mutate(
    # Determine the death date based on various conditions
    DEATH_DATE = case_when(
      !is.na(CLOSE_DSDECOD) ~ DTHDAT,  # If CLOSE_DSDECOD is not missing, use DTHDAT
      is.na(CLOSE_DSDECOD) & MAT_DEATH_MNH04 == 1 ~ MAT_DEATH_MNH04_DATE,  # If CLOSE_DSDECOD is missing and MAT_DEATH_MNH04 is 1, use MAT_DEATH_MNH04_DATE
      is.na(CLOSE_DSDECOD) & MAT_DEATH_MNH09 == 1 ~ MAT_DEATH_MNH09_DATE,  # If CLOSE_DSDECOD is missing and MAT_DEATH_MNH09 is 1, use MAT_DEATH_MNH09_DATE
      is.na(CLOSE_DSDECOD) & MAT_DEATH_MNH10 == 1 ~ MAT_DEATH_MNH10_DATE,  # If CLOSE_DSDECOD is missing and MAT_DEATH_MNH10 is 1, use MAT_DEATH_MNH10_DATE
      is.na(CLOSE_DSDECOD) & MAT_DEATH_MNH12 == 1 ~ MAT_DEATH_MNH12_DATE,  # If CLOSE_DSDECOD is missing and MAT_DEATH_MNH12 is 1, use MAT_DEATH_MNH12_DATE
      is.na(CLOSE_DSDECOD) & MAT_DEATH_M19 == 1 ~ MAT_DEATH_DATE_M19,      # If CLOSE_DSDECOD is missing and MAT_DEATH_M19 is 1, use MAT_DEATH_DATE_M19
      TRUE ~ NA_Date_  # If none of the conditions are met, assign NA to DEATH_DATE
    ),
    # Specify the source of the death date
    SOURCE_DDATE = case_when(
      !is.na(CLOSE_DSDECOD) ~ "MNH23",
      is.na(CLOSE_DSDECOD) & MAT_DEATH_MNH04 == 1 ~ "MNH04",
      is.na(CLOSE_DSDECOD) & MAT_DEATH_MNH09 == 1 ~ "MNH09",
      is.na(CLOSE_DSDECOD) & MAT_DEATH_MNH10 == 1 ~ "MNH10",
      is.na(CLOSE_DSDECOD) & MAT_DEATH_MNH12 == 1 ~ "MNH12",
      is.na(CLOSE_DSDECOD) & MAT_DEATH_M19 == 1 ~ "MNH19",
      TRUE ~ NA
    ),
    # Flag if death date is missing or if it matches specific placeholder dates
    DDTH_MISS = if_else(is.na(DEATH_DATE) | DEATH_DATE %in% as.Date(c("1907-07-07", "1905-05-05")), 1, 0), 
    # Flag if CLOSE_DSDECOD is not missing
    MNH23_CMPTE = if_else(!is.na(CLOSE_DSDECOD), 1, 0),
    # Calculate the late date as 42 days after death date if death date is not missing
    LATE_DATE = if_else(DDTH_MISS == 0, DEATH_DATE + 42, NA),
    # Determine if verbal autopsy (VA) is due based on the late date and upload date
    DUE_VA = case_when(
      LATE_DATE <= UploadDate ~ 1,  # If late date is on or before the upload date, VA is due
      LATE_DATE > UploadDate ~ 0,   # If late date is after the upload date, VA is not due
      is.na(LATE_DATE) ~ 55         # If late date is missing, assign 55 as a placeholder value
    )
  )

mat_dth_df <- mat_death_merge %>% select (SITE, MOMID, PREGID, DEATH_DATE, SOURCE_DDATE, DDTH_MISS, MNH23_CMPTE, LATE_DATE, DUE_VA)


# Filter and mutate infant death data
inf_death <- mnh24_combined %>%
  filter(CLOSE_DSDECOD %in% c(3, 6)) %>%
  mutate(
    MNH24_CMPTE = if_else(!is.na(CLOSE_DSDECOD), 1, 0),
    DTHDAT = ymd(parse_date_time(DTHDAT, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
    # Replace invalid DTHTIM values (77:77, 55:55) with NA
    DTHTIM = replace(DTHTIM, DTHTIM %in% c("77:77", "55:55"), NA),
    # Convert DTHTIM to ITime format if not NA
    DTHTIM = if_else(!is.na(DTHTIM), as.ITime(DTHTIM), NA),
    # Replace specific placeholder dates (1907-07-07, 1905-05-05, 2007-07-07) in DTHDAT with NA
    DTHDAT = replace(DTHDAT, DTHDAT %in% c(ymd("1907-07-07"), ymd("1905-05-05"), ymd("2007-07-07")), NA),
    # Combine DTHDAT and DTHTIM into a POSIXct datetime if both are not NA, otherwise use DTHDAT
    DTH_TME = if_else(!is.na(DTHTIM) & !is.na(DTHDAT), as.POSIXct(paste(DTHDAT, DTHTIM), format = "%Y-%m-%d %H:%M:%S"), DTHDAT)) %>% 
  filter (!is.na(INFANTID) & INFANTID != "Not Available")



#Read in the outcome (For Neonatal Death)
infant_dth <- inf_mortality %>% 
              filter (INF_DTH == 1 | STILLBIRTH_20WK == 1) %>% 
              select (SITE, MOMID, PREGID, INFANTID, DEATH_DATETIME, INF_DTH, NEO_DTH_CAT, STILLBIRTH_GESTAGE_CAT, 
                      FETAL_LOSS_DATE, STILLBIRTH_20WK, STILLBIRTH_SIGNS_LIFE, STILLBIRTH_TIMING) %>% 
              distinct(SITE, MOMID, PREGID, INFANTID, .keep_all = TRUE)  %>% 
              filter (!is.na (INFANTID) & INFANTID != "Not Available" &  INFANTID != "" )


mat_edd <- mat_enroll %>% 
  select(SITE, MOMID, PREGID, EDD_BOE)

inf_dth_df <- infant_dth %>% 
  full_join(inf_death, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  left_join(infant_dob, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  left_join(mat_edd, by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(
    DEATH_DATETIME = as.character(DEATH_DATETIME),
    FETAL_LOSS_DATE = as.character(FETAL_LOSS_DATE),
    DTH_TME = as.character(DTH_TME),
    DOB = as.character(DOB),
    DEATH_DATE = as.POSIXct(NA) # Initialize DEATH_DATE with NA of POSIXct type
  ) %>%
  mutate(
    DEATH_DATE = if_else(!is.na(FETAL_LOSS_DATE), as.POSIXct(FETAL_LOSS_DATE, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), DEATH_DATE),
    SOURCE_DDATE = if_else(!is.na(FETAL_LOSS_DATE), "INF_OUT", NA),
    
    DEATH_DATE = if_else(!is.na(DTH_TME) & is.na (DEATH_DATE), as.POSIXct(DTH_TME, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), DEATH_DATE),
    SOURCE_DDATE = if_else(!is.na(DTH_TME) & is.na(SOURCE_DDATE), "MNH24", SOURCE_DDATE),
    
    DEATH_DATE = if_else(!is.na(DEATH_DATETIME) & is.na (DEATH_DATE), as.POSIXct(DEATH_DATETIME, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), DEATH_DATE),
    SOURCE_DDATE = if_else(!is.na(DEATH_DATETIME) & is.na(SOURCE_DDATE), "INF_OUT", SOURCE_DDATE),
    
    DEATH_DATE = if_else((STILLBIRTH_20WK == 1 | CLOSE_DSDECOD == 6) & !is.na(DOB) & is.na (DEATH_DATE), as.POSIXct(DOB, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), DEATH_DATE),
    SOURCE_DDATE = if_else((STILLBIRTH_20WK == 1 | CLOSE_DSDECOD == 6) & !is.na(DOB) & is.na(SOURCE_DDATE), "DOB", SOURCE_DDATE),
    
    DDTH_MISS = if_else(is.na(DEATH_DATE) | DEATH_DATE %in% as.Date(c("1907-07-07", "1905-05-05")), 1, 0), #is death date miss? Yes-1, No-0
   
    DOB = as.POSIXct(DOB, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    
    DOB_Date = as.Date(trunc(DOB, 'days')), 
    
    END_PREG_CALC = round(as.numeric(difftime(DOB_Date, EDD_BOE, units = "days")), 0) + 280,
    
    AGE_DTH = case_when(
      STILLBIRTH_20WK == 1 | CLOSE_DSDECOD == 6 ~ 0,  # If stillbirth or fetal death, then age is 0
     (INF_DTH == 1 | CLOSE_DSDECOD == 3) & !is.na(DOB) & !is.na(DEATH_DATE) ~ round(abs(as.numeric(difftime(DEATH_DATE, DOB, units = "days"))), 0),
      TRUE ~ -5
    ),
    
    DEATH_RSN = case_when(
      STILLBIRTH_20WK == 1 | CLOSE_DSDECOD == 6 ~ 1, #stillbirth
      NEO_DTH_CAT %in% c(11, 12, 13) | (CLOSE_DSDECOD == 3 & AGE_DTH < 28) ~ 2, #neonatal death/death before 28days
     ((INF_DTH == 1 & !(NEO_DTH_CAT %in% c(11, 12, 13))) | CLOSE_DSDECOD == 3) & AGE_DTH >= 28 ~ 3, #livebirth after 28days
      TRUE ~ 55
    ),
    
    #if we know the death date, then create the latest date we would expect VA forms to be filled
    #if stillbirth/fetal death and we have no death date, then use EDD_BOE as benchmark
    LATE_DATE = case_when(
      DDTH_MISS == 0 ~ as.Date (DEATH_DATE) + 42, 
      DDTH_MISS == 1 & DEATH_RSN %in% c(1, 2) ~ as.Date(EDD_BOE) + 42, 
      TRUE ~ NA_Date_
    ),
    # Determine if verbal autopsy (VA) is due based on the late date and upload date
    DUE_VA = case_when(
      LATE_DATE <= UploadDate ~ 1,  # If late date is on or before the upload date, VA is due
      LATE_DATE > UploadDate ~ 0,   # If late date is after the upload date, VA is not due
      is.na(LATE_DATE) ~ 55         # If late date is missing, assign 55 as a placeholder value
    )
  )  %>% 
  select (SITE, INFANTID, MOMID, PREGID, DOB, EDD_BOE, AGE_DTH, END_PREG_CALC, MNH24_CMPTE, DEATH_DATE, CLOSE_DSDECOD, DEATH_DATE_M24 = DTH_TME, SOURCE_DDATE, DDTH_MISS, DEATH_RSN, LATE_DATE, DUE_VA)


#Importing all the COD Files

#Zambia
zambia_mat <- read_csv("~/Analysis/Verbal Autopsy/zambia_mnh27_cod.csv") %>%
  select (MOMID, PREGID, COD = Most.Likely.Cause) %>%
  mutate (SITE = "Zambia")

zambia_mnh28_cod <- read_csv("~/Analysis/Verbal Autopsy/zambia_mnh28_cod.csv") %>%
  select (MOMID, PREGID, INFANTID, COD = Most.Likely.Cause) %>%
  mutate (SITE = "Zambia")

zambia_mnh29_cod <- read_csv("~/Analysis/Verbal Autopsy/zambia_mnh29_cod.csv") %>%
                    select (MOMID, PREGID, INFANTID, COD = Most.Likely.Cause) %>%
                    mutate (SITE = "Zambia")

zambia_inf <- bind_rows(zambia_mnh28_cod,zambia_mnh29_cod)

#Pakistan
pakistan_mat <- read_csv("~/Analysis/Verbal Autopsy/pakistan_mnh27_cod.csv") %>%
  select (MOMID, PREGID, COD = InterVA) %>%
  mutate (SITE = "Pakistan", 
          COD = ifelse (COD == "#N/A" | is.na(COD), "Unknown", COD)) 

pakistan_inf <- read_csv("~/Analysis/Verbal Autopsy/pakistan_inf_cod.csv") %>%
  select (MOMID, PREGID, COD, INFANTID) %>%
  mutate (SITE = "Pakistan", 
          COD = ifelse (COD == "#N/A" | is.na(COD), "Unknown", COD)) 

#Kenya
kenya_mat <- read_csv("~/Analysis/Verbal Autopsy/kenya_mnh27_cod.csv") %>%
  select (MOMID, PREGID, COD) %>%
  mutate (SITE = "Kenya", 
          COD = ifelse (COD == "#N/A" | is.na(COD), "Unknown", COD)) 

kenya_inf <- read_csv("~/Analysis/Verbal Autopsy/kenya_inf_cod.csv") %>%
  select (MOMID, PREGID, COD = CAUSE1, INFANTID) %>%
  mutate (SITE = "Kenya", 
          COD = ifelse (COD == "#N/A" | is.na(COD), "Unknown", COD)) 

#Ghana
ghana_inf <- read_csv("~/Analysis/Verbal Autopsy/ghana_cod_inf.csv") %>%
  select (MOMID = momid, PREGID = pregid, INFANTID = infantid, COD = Most.Likely.Cause) %>%
  mutate (SITE = "Ghana")

india_cmc_inf <- read_csv("~/Analysis/Verbal Autopsy/india_cmc_mnh28_cod.csv")  %>%
  select (MOMID, PREGID, COD = CAUSE1, INFANTID) %>%
  mutate (SITE = "India-CMC")

all_inf_cod_in <- bind_rows(zambia_inf, pakistan_inf, ghana_inf, kenya_inf, india_cmc_inf) %>% distinct(SITE, MOMID, PREGID, INFANTID, .keep_all = TRUE)  
all_inf_cod <- all_inf_cod_in %>% distinct(SITE, MOMID, PREGID, INFANTID, .keep_all = TRUE) %>% 
  select (SITE, INFANTID, COD)

all_mat_cod <- bind_rows(zambia_mat, pakistan_mat, kenya_mat) %>% distinct(SITE, MOMID, PREGID, .keep_all = TRUE)  

data_dictionary <- read_excel("~/Analysis/Dictionary/va_dict.xlsx")


#The function is to rename variables to manually correct SITES variable name errors

# Function to remove duplicate columns
remove_duplicate_columns <- function(df) {
  df <- df[, !duplicated(names(df))]
  return(df)
}

# Function to clean column names to ensure valid UTF-8
clean_column_names <- function(df) {
  names(df) <- names(df) %>%
    str_replace_all("[^[:ascii:]]", "") %>%
    make.names(unique = TRUE)
  return(df)
}

# Function to rename ID variables based on the data dictionary
rename_vars <- function(df, dictionary) {
  for (i in 1:nrow(dictionary)) {
    pattern <- dictionary$`Variable name`[i]
    new_name <- dictionary$`Variable name`[i] # Adjusted to use the pattern as the new name
    if (!is.na(pattern) && !is.na(new_name) && new_name != "") {
      # Skip renaming if the pattern is already a variable in the dataframe
      if (new_name %in% names(df)) {
        next
      }
      
      case_insensitive_pattern <- paste0("(?i)\\b", str_replace_all(pattern, "_", "\\_"), "\\b")
      matched_columns <- names(df)[str_detect(names(df), case_insensitive_pattern)]
      
      # Remove duplicate columns if any
      if (length(matched_columns) > 1) {
        df <- df %>% select(-one_of(matched_columns[-1]))
      }
      
      # Perform renaming on the first matched column if it exists
      if (length(matched_columns) > 0) {
        df <- df %>% rename(!!sym(new_name) := !!sym(matched_columns[1]))
      }
    }
  }
  return(df)
}

# Function to clean and process the form
process_form <- function(df, form_name, dictionary, site) {
  # Clean column names to ensure valid UTF-8
  df <- clean_column_names(df)
  
  # Filter dictionary for the specific form
  dictionary <- dictionary %>% filter(Form == toupper(form_name))
  
  df <- remove_duplicate_columns(df)
  df <- rename_vars(df, dictionary)
  
  required_columns <- c("momid", "pregid", "infantid", "visit_obsstdat", "id10022", "id10019", "id10023_a")
  for (col in required_columns) {
    if (!tolower(col) %in% tolower(colnames(df))) {
      df[[toupper(col)]] <- NA
    }
  }
  
  colnames(df) <- toupper(make.names(colnames(df), unique = TRUE)) # Ensure unique column names
  df <- df %>%
    mutate(FORM = toupper(form_name), SITE = site) %>%  # Add the SITE variable
    select(all_of(toupper(required_columns)), FORM, SITE) %>%
    mutate(across(everything(), as.character))
  
  return(df)
}

# Function to read CSV with cleaning
safe_read_csv <- function(file_path) {
  # Read file with UTF-8 encoding
  df <- read_csv(file_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
  
  # Replace non-ASCII characters with empty string
  df <- df %>%
    mutate(across(everything(), ~str_replace_all(.x, "[^[:ascii:]]", "")))
  
  return(df)
}

# Define the sites, forms, and upload date
sites <- c("Pakistan", "Kenya", "India_CMC", "India_SAS", "Ghana", "Zambia")
forms <- c("mnh27", "mnh28", "mnh29")
upload_date <- "2024-07-02"

# Lists to store processed data
mnh27_list <- list()
mnh28_mnh29_list <- list()

# Iterate over sites and forms
for (site in sites) {
  for (form in forms) {
    file_path <- paste0("~/PRiSMAv2Data/", site, "/", upload_date, "/", form, ".csv")
    
    if (!file.exists(file_path)) {
      message("File not found: ", file_path)
      next
    }
    
    form_data <- safe_read_csv(file_path)
    
    if (is.null(form_data)) next
    
    processed_data <- process_form(form_data, form, data_dictionary, site)
    
    if (is.null(processed_data)) next
    
    if (form == "mnh27") {
      mnh27_list[[paste0(site, "_", form)]] <- processed_data
    } else {
      mnh28_mnh29_list[[paste0(site, "_", form)]] <- processed_data
    }
  }
}

#bind all rows from all SITES mnh27 and mnh28/29 forms
all_mnh27 <- bind_rows(mnh27_list, .id = "source")
all_mnh28_mnh29 <- bind_rows(mnh28_mnh29_list, .id = "source")


#for mnh forms, rename variables and transform VA dod to date time format
mat_form_comp <- all_mnh27 %>%
  mutate(ID10022 = tolower (ID10022),
         ID10019 = tolower (ID10022),
         DATA_COMP = 1,
         SITE = gsub("_", "-", SITE),
         MOMID = ifelse(SITE == "Ghana", substr(PREGID, 1, nchar(PREGID) - 1), MOMID),         
         DEATH_DATE_VA = parse_date_time(ID10023_A, orders = c("%d-%m-%Y","%m/%d/%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y", "%d/%m/%Y", "%b %d %Y %I:%M%p"))) %>% 
  select (-source, -INFANTID)

#for mnh forms, rename variables and transform VA dod to date time format
inf_form_comp <- all_mnh28_mnh29 %>%
  mutate(ID10022 = tolower (ID10022),
         ID10019 = tolower (ID10022),
         DATA_COMP = 1,
         SITE = gsub("_", "-", SITE),
         INFANTID = ifelse(SITE == "Ghana", paste0(PREGID, "1"), INFANTID),
         MOMID = ifelse(SITE == "Ghana", substr(PREGID, 1, nchar(PREGID) - 1), MOMID),         
         DEATH_DATE_VA = parse_date_time(ID10023_A, orders = c("%d-%m-%Y","%m/%d/%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y", "%d/%m/%Y", "%b %d %Y %I:%M%p"))) %>% 
  select (-source)

#remove spaces in between variables to avoid MOMID/PREGID/INFANTID mismatch
inf_dth_df$SITE <- trimws(inf_dth_df$SITE)
inf_dth_df$MOMID <- trimws(inf_dth_df$MOMID)
inf_dth_df$PREGID <- trimws(inf_dth_df$PREGID)
inf_dth_df$INFANTID <- trimws(inf_dth_df$INFANTID)

all_inf_cod$SITE <- trimws(all_inf_cod$SITE)
all_inf_cod$INFANTID <- trimws(all_inf_cod$INFANTID)

# Join inf_dth_df and all_inf_cod by INFANTID, MOMID, and PREGID
merged_inf_df1 <- left_join (inf_dth_df, all_inf_cod, by = c( "SITE", "INFANTID")) %>% 
  distinct(SITE, MOMID, PREGID, INFANTID, .keep_all = TRUE)  
  
# Then join the result with inf_form_comp
all_inf_merged <- left_join(merged_inf_df1, inf_form_comp, by = c("INFANTID", "MOMID", "PREGID", "SITE"))

all_inf_merged <- all_inf_merged %>% 
                      mutate (COD = ifelse (SITE == "India-SAS" & DATA_COMP == 1, "Fresh stillbirth", COD),
                              DEATH_DATE_TRC =  as.Date(trunc(DEATH_DATE, 'days')),
                              MISS_VA =  ifelse (DUE_VA == 1 & is.na(FORM), 1, 0),
                              
                              DDTH_MISS_VA = case_when (DUE_VA == 0 | MISS_VA == 1 ~ 55,
                                                     is.na(DEATH_DATE_VA) | DEATH_DATE_VA %in% as.Date(c("1907-07-07", "1905-05-05")) ~ 1, 
                                                     !is.na(DEATH_DATE_VA) & !(DEATH_DATE_VA %in% as.Date(c("1907-07-07", "1905-05-05"))) ~ 0, 
                                                     TRUE ~ 77), #is death date miss? Yes-1, No-0
                              
                              MISS_COD = ifelse ((DUE_VA == 1 & is.na(COD)), 1, 0), #is cause of date missing? Yes-1, No-0
                              
                              #is dod in VA the same as dod in crfs, Yes, the are equal - 1, No, not equal - 0
                              DOD_EQ =  case_when (is.na (DEATH_DATE_TRC) | is.na (DEATH_DATE_VA) ~ 55, 
                                                   DEATH_DATE_VA == DEATH_DATE_TRC ~ 1,
                                                   DEATH_DATE_VA != DEATH_DATE_TRC ~ 0,
                                                    TRUE ~ NA),
                              #is the adequate form for status at death the right form filled for infants:
                              #Yes, the form is correct - 1, No, not correct - 0
                              FORM_EQ = case_when ( is.na(FORM) & DUE_VA == 1 ~ 55,
                                                    is.na(FORM) & DUE_VA == 0 ~ 77,
                                                    DEATH_RSN %in% c(1,2) & FORM == "MNH28" ~ 1,
                                                    DEATH_RSN == 3 & FORM == "MNH29" ~ 1,
                                                    DEATH_RSN %in% c(1,2) & FORM == "MNH29" ~ 0,
                                                    DEATH_RSN == 3 & FORM == "MNH28" ~ 0,
                                                    TRUE ~ NA),
                              
                              #is the stillbirth classified death correct or is stillbirth misclassified?
                              STILLBIRTH_EQ = case_when( is.na(DEATH_RSN) | is.na(COD) ~ 55,
                                                         DEATH_RSN %in% c(2, 3) & grepl("stillbirth", COD, ignore.case = TRUE) ~ 0,
                                                         DEATH_RSN == 1 & grepl("stillbirth", COD, ignore.case = TRUE) ~ 1,
                                                         DEATH_RSN %in% c(2, 3) & !grepl("stillbirth", COD, ignore.case = TRUE) ~ 1,
                                                         TRUE ~ NA)) %>%
                      select(SITE, INFANTID, MOMID, PREGID, DOB, EDD_BOE, AGE_DTH, 
                             END_PREG_CALC, DEATH_DATE, DEATH_DATE_M24, SOURCE_DDATE, 
                             DDTH_MISS, DDTH_MISS_VA, MNH24_CMPTE, DEATH_RSN, LATE_DATE, DUE_VA, COD, FORM, 
                             DATA_COMP, DEATH_DATE_VA, DEATH_DATE_TRC, MISS_VA, 
                             MISS_COD, DOD_EQ, FORM_EQ, STILLBIRTH_EQ)


# Join mat_dth_df and all_mat_cod by matANTID, MOMID, and PREGID
merged_mat_df1 <- left_join (mat_dth_df, all_mat_cod, by = c( "SITE", "MOMID", "PREGID")) %>% 
  distinct(SITE, MOMID, PREGID, .keep_all = TRUE)  

# Then join the result with mat_form_comp
all_mat_merged <- left_join(merged_mat_df1, mat_form_comp, by = c("MOMID", "PREGID", "SITE"))

# all_mat_merged[all_mat_merged == ""] <- NA_character_

all_mat_merged <- all_mat_merged %>% 
  mutate (MISS_VA =  ifelse (DUE_VA == 1 & is.na(FORM), 1, 0),
          MISS_COD = ifelse ((DUE_VA == 1 & is.na(COD)), 1, 0),
          DDTH_MISS_VA = case_when (DUE_VA == 0 | MISS_VA == 1 ~ 55,
                                    is.na(DEATH_DATE_VA) | DEATH_DATE_VA %in% as.Date(c("1907-07-07", "1905-05-05")) ~ 1, 
                                    !is.na(DEATH_DATE_VA) & !(DEATH_DATE_VA %in% as.Date(c("1907-07-07", "1905-05-05"))) ~ 0, 
                                    TRUE ~ 77), #is death date miss? Yes-1, No-0
          DEATH_DATE_VA_TRC =  as.Date(trunc(DEATH_DATE_VA, 'days')),
          DOD_EQ =  case_when (is.na (DEATH_DATE) | is.na (DEATH_DATE_VA_TRC) ~ 55, 
                               DEATH_DATE_VA_TRC == DEATH_DATE ~ 1,
                               DEATH_DATE_VA_TRC != DEATH_DATE ~ 0,
                               TRUE ~ NA)) %>%
  select(SITE, MOMID, PREGID, DEATH_DATE, SOURCE_DDATE, DDTH_MISS, MNH23_CMPTE,
         LATE_DATE, DUE_VA, COD, FORM, DATA_COMP, DEATH_DATE_VA, DDTH_MISS_VA, MISS_VA, MISS_COD, DOD_EQ)

                                           
write.csv(all_inf_merged, paste0(path_to_tnt, "INF_COD" ,".csv"), na="", row.names=FALSE)
write.csv(all_mat_merged, paste0(path_to_tnt, "MAT_COD" ,".csv"), na="", row.names=FALSE)
write.csv(all_inf_merged, paste0(path_to_save, "INF_COD" ,".csv"), na="", row.names=FALSE)
write.csv(all_mat_merged, paste0(path_to_save, "MAT_COD" ,".csv"), na="", row.names=FALSE)



