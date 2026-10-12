#*******************************************************************************
# Hyperbilirubinemia + PSBI: age-based all-visit version
#
# Refactor of the existing outcome code. Final outcome variable names are retained,
# but TYPE_VISIT is not used to assign PNC visits. Instead:
#   1. Every available IPC, PNC, and hospitalization observation is retained.
#   2. Infant age is calculated from DOB/delivery datetime and the actual visit date/time.
#   3. PNC windows are assigned from age using the late visit windows.
#   4. Repeated and in-between visits remain in the long datasets.
#   5. Wide audit files store repeated/extra visits as pipe-delimited lists.
#   6. Final hyperbilirubinemia and PSBI outcome names are kept consistent with
#      the existing code.
#
# JAUND_CESTDAT is categorical, NOT a date:
#   1 = jaundice appeared <24 hours after birth
#   2 = jaundice appeared >=24 hours after birth
#  77 = no jaundice present
#*******************************************************************************


# AGE-BASED INFANT VISIT CONSTRUCTION
# Replaces TYPE_VISIT-based assignment for TCB/hyperbilirubinemia and PSBI.
# Keeps every observed visit/result in long data and derives scheduled PNC windows
# from infant age at the visit.

library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(stringr)


# TCB / HYPERBILIRUBINEMIA ----
# Helper functions ----
## Date cleaning function (from Flowchart workflow) ----
clean_date <- function(x,
                       invalid_dates = as_date(c("1907-07-07", "2007-07-07",
                                                 "1905-05-05", "1909-07-09",
                                                 "1906-06-06", "1909-09-09")),
                       min_date = NULL,
                       max_date = NULL) {
  if (is.null(max_date)) max_date <- today()
  parsed <- parse_date_time(
    trimws(as.character(x)),
    orders = c("d/m/Y", "d-m-Y", "Y-m-d",
               "d-b-y", "d-m-y",
               "m/d/y", "m-d-y"),
    exact = FALSE
  ) %>% as_date()
  invalid_mask <- is.na(parsed) | parsed %in% invalid_dates
  if (!is.null(min_date)) invalid_mask <- invalid_mask | parsed < min_date
  if (!is.null(max_date)) invalid_mask <- invalid_mask | parsed > max_date
  parsed[invalid_mask] <- NA
  return(parsed)
}

assign_pnc_window <- function(age_days) {
  case_when(
    age_days >= 3   & age_days <= 5   ~ "PNC0",
    age_days >= 7   & age_days <= 14  ~ "PNC1",
    age_days >= 28  & age_days <= 35  ~ "PNC4",
    age_days >= 42  & age_days <= 104 ~ "PNC6",
    age_days >= 182 & age_days <= 279 ~ "PNC26",
    age_days >= 364 & age_days <= 454 ~ "PNC52",
    !is.na(age_days)                  ~ "INBETWEEN",
    TRUE                              ~ NA_character_
  )
}

collapse_list <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = " | ")
}

## Construct ALL TCB observations in long format ----

## IPC TCB observations (MNH11)
tcb_ipc_long <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, LIVEBIRTH, BIRTH_OUTCOME,
         DOB, DELIVERY_DATETIME, GESTAGEBIRTH_ANY, GESTAGEBIRTH_ANY_DAYS) %>%
  left_join(mnh11_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  mutate(
    DOB = clean_date(DOB),
    M11_VISIT_OBSSTDAT = clean_date(M11_VISIT_OBSSTDAT),
    M11_VISIT_OBSSTTIM = replace(M11_VISIT_OBSSTTIM,
                                 M11_VISIT_OBSSTTIM %in% c("77:77", "99:99", "55:55:00"), NA),
    
    # --- Normalize time: pad "H:MM" / "HH:M" to "HH:MM"; keep NA for now ---
    M11_TIME_CLEAN = {
      t <- trimws(as.character(M11_VISIT_OBSSTTIM))
      t <- ifelse(grepl("^\\d{1,2}:\\d{1,2}$", t),
                  sprintf("%02d:%02d",
                          as.integer(sub(":.*", "", t)),
                          as.integer(sub(".*:", "", t))),
                  NA_character_)
      t
    },
    
    DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format = "%Y-%m-%d %H:%M"),
    
    # --- Impute 12:00 (midpoint of day) ONLY when the time is missing AND
    #     the visit date differs from DOB. Same-day visits keep NA time so
    #     hour-of-life is not fabricated on day 0. ---
    M11_TIME_USE = case_when(
      !is.na(M11_TIME_CLEAN)                    ~ M11_TIME_CLEAN,
      is.na(M11_VISIT_OBSSTDAT) | is.na(DOB)    ~ NA_character_,
      M11_VISIT_OBSSTDAT != DOB                 ~ "12:00",
      TRUE                                      ~ NA_character_
    ),
    
    VISIT_DATETIME = as.POSIXct(paste(M11_VISIT_OBSSTDAT, M11_TIME_USE),
                                format = "%Y-%m-%d %H:%M"),
    VISIT_DATE = M11_VISIT_OBSSTDAT,
    
    VISIT_TIME_IMPUTED = as.integer(
      is.na(M11_TIME_CLEAN) &
        !is.na(M11_VISIT_OBSSTDAT) & !is.na(DOB) &
        M11_VISIT_OBSSTDAT != DOB
    ),
    
    # --- Age from datetime when possible ---
    AGE_AT_VISIT_HOURS_FROM_DT = floor(
      as.numeric(difftime(VISIT_DATETIME, DELIVERY_DATETIME, units = "hours"))
    ),
    
    # --- Age from dates only as fallback (covers same-day visits with no time) ---
    AGE_AT_VISIT_DAYS_FROM_DATE = if_else(
      !is.na(VISIT_DATE) & !is.na(DOB) & VISIT_DATE >= DOB,
      as.numeric(VISIT_DATE - DOB),
      NA_real_
    ),
    
    # --- Resolve: prefer datetime, fall back to date-only ---
    AGE_AT_VISIT_HOURS_TOTAL = case_when(
      !is.na(AGE_AT_VISIT_HOURS_FROM_DT) & AGE_AT_VISIT_HOURS_FROM_DT >= 0 ~ AGE_AT_VISIT_HOURS_FROM_DT,
      !is.na(AGE_AT_VISIT_DAYS_FROM_DATE)                                   ~ AGE_AT_VISIT_DAYS_FROM_DATE * 24,
      TRUE                                                                  ~ NA_real_
    ),
    AGE_AT_VISIT_DAYS = as.numeric(AGE_AT_VISIT_HOURS_TOTAL) %/% 24,
    AGE_AT_VISIT_HRS  = as.numeric(AGE_AT_VISIT_HOURS_TOTAL) %% 24,
    
    VISIT_SOURCE = "IPC",
    VISIT_WINDOW = "IPC",
    TCB_ASSESSED = M11_BILIRUBIN_LBPERF,
    TCB_VALUE_RAW = suppressWarnings(as.numeric(M11_TBILIRUBIN_UMOLL_LBORRES)),
    TCB_VALUE = if_else(!is.na(TCB_VALUE_RAW) & TCB_VALUE_RAW >= 0, TCB_VALUE_RAW, NA_real_),
    JAUND_CEOCCUR = M11_JAUND_CEOCCUR,
    YELLOW_CEOCCUR = M11_YELLOW_CEOCCUR,
    JAUND_ONSET = suppressWarnings(as.numeric(M11_JAUND_CESTDAT))
  ) %>%
  select(SITE, MOMID, PREGID, INFANTID, LIVEBIRTH, BIRTH_OUTCOME, DOB,
         DELIVERY_DATETIME, GESTAGEBIRTH_ANY, GESTAGEBIRTH_ANY_DAYS,
         VISIT_SOURCE, VISIT_WINDOW, VISIT_DATE, VISIT_DATETIME,
         VISIT_TIME_IMPUTED,
         AGE_AT_VISIT_DAYS, AGE_AT_VISIT_HRS, AGE_AT_VISIT_HOURS_TOTAL,
         TCB_ASSESSED, TCB_VALUE_RAW, TCB_VALUE,
         JAUND_CEOCCUR, YELLOW_CEOCCUR, JAUND_ONSET)

## PNC clinical observations (MNH13)
pnc_clinical_long <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, DOB) %>%
  right_join(mnh13, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  mutate(
    DOB = clean_date(DOB),
    VISIT_DATE = clean_date(
      M13_VISIT_OBSSTDAT,
      invalid_dates = as_date(c("1907-07-07", "2007-07-07", "1905-05-05",
                                "1909-07-09", "1906-06-06", "1909-09-09",
                                "2028-08-12", "2084-10-25", "2924-03-22",
                                "8024-04-08"))
    ),
    AGE_AT_VISIT_DAYS = case_when(
      !is.na(VISIT_DATE) & !is.na(DOB) & VISIT_DATE >= DOB ~ as.numeric(VISIT_DATE - DOB),
      TRUE ~ NA_real_
    ),
    VISIT_WINDOW = assign_pnc_window(AGE_AT_VISIT_DAYS)
  ) %>%
  select(SITE, MOMID, PREGID, INFANTID, VISIT_DATE, AGE_AT_VISIT_DAYS,
         VISIT_WINDOW, M13_JAUND_CEOCCUR, M13_JAUND_CESTTIM, M13_YELL_CEOCCUR)

## PNC TCB observations (MNH14)
tcb_pnc_long <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, LIVEBIRTH, BIRTH_OUTCOME,
         DOB, DELIVERY_DATETIME, GESTAGEBIRTH_ANY, GESTAGEBIRTH_ANY_DAYS) %>%
  right_join(mnh14, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  mutate(
    DOB = clean_date(DOB),
    VISIT_DATE = clean_date(M14_VISIT_OBSSTDAT),
    M14_TCB_OBSSTTIM = replace(M14_TCB_OBSSTTIM,
                               M14_TCB_OBSSTTIM %in% c("77:77", "99:99", "55:55:00"), NA),
    
    # --- Normalize time: pad "H:MM" / "HH:M" to "HH:MM"; keep NA for now ---
    M14_TIME_CLEAN = {
      t <- trimws(as.character(M14_TCB_OBSSTTIM))
      t <- ifelse(grepl("^\\d{1,2}:\\d{1,2}$", t),
                  sprintf("%02d:%02d",
                          as.integer(sub(":.*", "", t)),
                          as.integer(sub(".*:", "", t))),
                  NA_character_)
      t
    },
    
    DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format = "%Y-%m-%d %H:%M"),
    
    # --- Impute 12:00 (midpoint of day) ONLY when the time is missing AND
    #     the visit date differs from DOB. Same-day visits keep NA time. ---
    M14_TIME_USE = case_when(
      !is.na(M14_TIME_CLEAN)                 ~ M14_TIME_CLEAN,
      is.na(VISIT_DATE) | is.na(DOB)         ~ NA_character_,
      VISIT_DATE != DOB                      ~ "12:00",
      TRUE                                   ~ NA_character_
    ),
    
    VISIT_DATETIME = as.POSIXct(paste(VISIT_DATE, M14_TIME_USE),
                                format = "%Y-%m-%d %H:%M"),
    
    VISIT_TIME_IMPUTED = as.integer(
      is.na(M14_TIME_CLEAN) &
        !is.na(VISIT_DATE) & !is.na(DOB) &
        VISIT_DATE != DOB
    ),
    
    # --- Age from datetime when possible ---
    AGE_AT_VISIT_HOURS_FROM_DT = floor(
      as.numeric(difftime(VISIT_DATETIME, DELIVERY_DATETIME, units = "hours"))
    ),
    
    # --- Age from dates only as fallback ---
    AGE_AT_VISIT_DAYS_FROM_DATE = if_else(
      !is.na(VISIT_DATE) & !is.na(DOB) & VISIT_DATE >= DOB,
      as.numeric(VISIT_DATE - DOB),
      NA_real_
    ),
    
    # --- Resolve: prefer datetime, fall back to date-only ---
    AGE_AT_VISIT_HOURS_TOTAL = case_when(
      !is.na(AGE_AT_VISIT_HOURS_FROM_DT) & AGE_AT_VISIT_HOURS_FROM_DT >= 0 ~ AGE_AT_VISIT_HOURS_FROM_DT,
      !is.na(AGE_AT_VISIT_DAYS_FROM_DATE)                                   ~ AGE_AT_VISIT_DAYS_FROM_DATE * 24,
      TRUE                                                                  ~ NA_real_
    ),
    AGE_AT_VISIT_DAYS = as.numeric(AGE_AT_VISIT_HOURS_TOTAL) %/% 24,
    AGE_AT_VISIT_HRS  = as.numeric(AGE_AT_VISIT_HOURS_TOTAL) %% 24,
    
    VISIT_SOURCE = "PNC",
    VISIT_WINDOW = assign_pnc_window(AGE_AT_VISIT_DAYS),
    TCB_ASSESSED = M14_TCB_VSSTAT,
    TCB_VALUE_RAW = suppressWarnings(as.numeric(M14_TCB_UMOLL_LBORRES)),
    TCB_VALUE = if_else(!is.na(TCB_VALUE_RAW) & TCB_VALUE_RAW >= 0, TCB_VALUE_RAW, NA_real_)
  ) %>%
  left_join(
    pnc_clinical_long %>%
      select(SITE, MOMID, PREGID, INFANTID, VISIT_DATE,
             M13_JAUND_CEOCCUR, M13_JAUND_CESTTIM, M13_YELL_CEOCCUR),
    by = c("SITE", "MOMID", "PREGID", "INFANTID", "VISIT_DATE")
  ) %>%
  mutate(
    JAUND_CEOCCUR = M13_JAUND_CEOCCUR,
    YELLOW_CEOCCUR = M13_YELL_CEOCCUR,
    JAUND_ONSET = NA_real_
  )

## Determine site-level TCB units and convert all actual measurements ----

## MNH14
tcb_pnc_long <- tcb_pnc_long %>%
  group_by(SITE) %>%
  mutate(
    m14_mean = mean(TCB_VALUE, na.rm = TRUE),
    unit = case_when(
      is.nan(m14_mean) ~ NA_character_,
      m14_mean >= 25 ~ "umol/L",
      m14_mean < 25 ~ "mg/dL",
      TRUE ~ NA_character_
    ),
    TCB_VALUE = case_when(
      unit == "umol/L" ~ TCB_VALUE / 17.1,
      TRUE ~ TCB_VALUE
    )
  ) %>%
  ungroup()

m14_subset <- tcb_pnc_long %>% distinct(SITE, unit)
for (i in m14_subset$SITE) {
  out <- m14_subset %>% filter(SITE == i)
  cat(i, "reporting in", out$unit, "in MNH14", "\n")
}

## MNH11
tcb_ipc_long <- tcb_ipc_long %>%
  group_by(SITE) %>%
  mutate(
    m11_mean = mean(TCB_VALUE, na.rm = TRUE),
    unit = case_when(
      is.nan(m11_mean) ~ NA_character_,
      m11_mean >= 25 ~ "umol/L",
      m11_mean < 25 ~ "mg/dL",
      TRUE ~ NA_character_
    ),
    TCB_VALUE = case_when(
      unit == "umol/L" ~ TCB_VALUE / 17.1,
      TRUE ~ TCB_VALUE
    )
  ) %>%
  ungroup()

m11_subset <- tcb_ipc_long %>% distinct(SITE, unit)
for (i in m11_subset$SITE) {
  out <- m11_subset %>% filter(SITE == i)
  cat(i, "reporting in", out$unit, "in MNH11", "\n")
}

## Add clinical PNC visits without a corresponding MNH14 TCB record ----
pnc_clinical_only <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, LIVEBIRTH, BIRTH_OUTCOME,
         DOB, DELIVERY_DATETIME, GESTAGEBIRTH_ANY, GESTAGEBIRTH_ANY_DAYS) %>%
  right_join(pnc_clinical_long, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  anti_join(
    tcb_pnc_long %>% distinct(SITE, MOMID, PREGID, INFANTID, VISIT_DATE),
    by = c("SITE", "MOMID", "PREGID", "INFANTID", "VISIT_DATE")
  ) %>%
  filter(
    (!is.na(M13_JAUND_CEOCCUR) & !M13_JAUND_CEOCCUR %in% c(-5, -7, 77, 99)) |
      (!is.na(M13_YELL_CEOCCUR) & !M13_YELL_CEOCCUR %in% c(-5, -7, 77, 99))
  ) %>%
  mutate(
    VISIT_SOURCE = "PNC",
    VISIT_DATETIME = as.POSIXct(NA),
    DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format = "%Y-%m-%d %H:%M"),
    AGE_AT_VISIT_HRS = NA_real_,
    AGE_AT_VISIT_HOURS_TOTAL = NA_real_,
    TCB_ASSESSED = NA,
    TCB_VALUE_RAW = NA_real_,
    TCB_VALUE = NA_real_,
    JAUND_CEOCCUR = M13_JAUND_CEOCCUR,
    YELLOW_CEOCCUR = M13_YELL_CEOCCUR,
    JAUND_ONSET = NA_real_
  )

## Combine ALL TCB + jaundice observations ----
tcb_long <- bind_rows(
  tcb_ipc_long %>% select(any_of(c(
    "SITE","MOMID","PREGID","INFANTID","LIVEBIRTH","BIRTH_OUTCOME","DOB",
    "DELIVERY_DATETIME","GESTAGEBIRTH_ANY","GESTAGEBIRTH_ANY_DAYS",
    "VISIT_SOURCE","VISIT_WINDOW","VISIT_DATE","VISIT_DATETIME",
    "AGE_AT_VISIT_DAYS","AGE_AT_VISIT_HRS","AGE_AT_VISIT_HOURS_TOTAL",
    "TCB_ASSESSED","TCB_VALUE_RAW","TCB_VALUE",
    "JAUND_CEOCCUR","YELLOW_CEOCCUR","JAUND_ONSET","unit"))),
  
  tcb_pnc_long %>% select(any_of(c(
    "SITE","MOMID","PREGID","INFANTID","LIVEBIRTH","BIRTH_OUTCOME","DOB",
    "DELIVERY_DATETIME","GESTAGEBIRTH_ANY","GESTAGEBIRTH_ANY_DAYS",
    "VISIT_SOURCE","VISIT_WINDOW","VISIT_DATE","VISIT_DATETIME",
    "AGE_AT_VISIT_DAYS","AGE_AT_VISIT_HRS","AGE_AT_VISIT_HOURS_TOTAL",
    "TCB_ASSESSED","TCB_VALUE_RAW","TCB_VALUE",
    "JAUND_CEOCCUR","YELLOW_CEOCCUR","JAUND_ONSET","unit"))),
  
  pnc_clinical_only %>% select(any_of(c(
    "SITE","MOMID","PREGID","INFANTID","LIVEBIRTH","BIRTH_OUTCOME","DOB",
    "DELIVERY_DATETIME","GESTAGEBIRTH_ANY","GESTAGEBIRTH_ANY_DAYS",
    "VISIT_SOURCE","VISIT_WINDOW","VISIT_DATE","VISIT_DATETIME",
    "AGE_AT_VISIT_DAYS","AGE_AT_VISIT_HRS","AGE_AT_VISIT_HOURS_TOTAL",
    "TCB_ASSESSED","TCB_VALUE_RAW","TCB_VALUE",
    "JAUND_CEOCCUR","YELLOW_CEOCCUR","JAUND_ONSET")))
) %>%
  mutate(
    AGE_AT_VISIT_HOURS_USE = case_when(
      !is.na(AGE_AT_VISIT_HOURS_TOTAL) & AGE_AT_VISIT_HOURS_TOTAL >= 0 ~ AGE_AT_VISIT_HOURS_TOTAL,
      !is.na(AGE_AT_VISIT_DAYS) & AGE_AT_VISIT_DAYS >= 0 ~ AGE_AT_VISIT_DAYS * 24,
      TRUE ~ NA_real_
    ),
    AGE_SOURCE = case_when(
      !is.na(AGE_AT_VISIT_HOURS_TOTAL) & AGE_AT_VISIT_HOURS_TOTAL >= 0 ~ "DATETIME",
      !is.na(AGE_AT_VISIT_DAYS) & AGE_AT_VISIT_DAYS >= 0 ~ "DATE_ONLY",
      TRUE ~ "MISSING"
    ),
    TCB_REPORTED = as.integer(!is.na(TCB_VALUE)),
    TCB_AGE_WINDOW = case_when(
      TCB_REPORTED == 1 & AGE_AT_VISIT_HOURS_USE >= 0 & AGE_AT_VISIT_HOURS_USE < 24 ~ "24HR",
      TCB_REPORTED == 1 & AGE_AT_VISIT_HOURS_USE >= 24 & AGE_AT_VISIT_HOURS_USE < 5*24 ~ "5DAY",
      TCB_REPORTED == 1 & AGE_AT_VISIT_HOURS_USE >= 5*24 & AGE_AT_VISIT_HOURS_USE < 14*24 ~ "14DAY",
      TCB_REPORTED == 1 & AGE_AT_VISIT_HOURS_USE >= 14*24 ~ ">=14DAY",
      TCB_REPORTED == 1 & is.na(AGE_AT_VISIT_HOURS_USE) ~ "AGE_MISSING",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(SITE, MOMID, PREGID, INFANTID, VISIT_DATE, VISIT_DATETIME) %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  mutate(VISIT_SEQ = row_number()) %>%
  ungroup()

## Audit ----
cat("\nTCB AVAILABILITY CHECK\n")
cat("Actual usable TCB measurements:", sum(!is.na(tcb_long$TCB_VALUE)), "\n")
cat("Negative raw TCB values converted to missing:",
    sum(tcb_long$TCB_VALUE_RAW < 0, na.rm = TRUE), "\n")
cat("Negative TCB values remaining after cleaning:",
    sum(tcb_long$TCB_VALUE < 0, na.rm = TRUE), "\n")
cat("Actual TCBs with exact datetime age:",
    sum(!is.na(tcb_long$TCB_VALUE) & tcb_long$AGE_SOURCE == "DATETIME"), "\n")
cat("Actual TCBs with date-only age:",
    sum(!is.na(tcb_long$TCB_VALUE) & tcb_long$AGE_SOURCE == "DATE_ONLY"), "\n")
cat("Actual TCBs with missing age:",
    sum(!is.na(tcb_long$TCB_VALUE) & tcb_long$AGE_SOURCE == "MISSING"), "\n")

tcb_reported_check <- tcb_long %>%
  filter(TCB_REPORTED == 1) %>%
  count(VISIT_SOURCE, AGE_SOURCE, TCB_AGE_WINDOW, name = "N_TCB") %>%
  arrange(VISIT_SOURCE, AGE_SOURCE, TCB_AGE_WINDOW)
print(tcb_reported_check)

## Per-observation TCB criteria ----
tcb_long <- tcb_long %>%
  mutate(
    INF_HYPERBILI_TCB15 = case_when(
      TCB_REPORTED == 1 & TCB_VALUE > 15 ~ 1L,
      TCB_REPORTED == 1 ~ 0L,
      TRUE ~ NA_integer_
    ),
    TCB_DAYS = floor(AGE_AT_VISIT_HOURS_USE / 24),
    TCB_HRS = floor(AGE_AT_VISIT_HOURS_USE %% 24)
  )

## AAP and NICE threshold lookup ----
threshold_file <- "~/PRISMA-Public/PRISMA-Infant-Constructed-Outcomes/TCB_threshold_lookup.csv"

if (file.exists(threshold_file)) {
  tcb_thresholds <- read.csv(threshold_file) %>%
    mutate(
      GESTAGEBIRTH_ANY = as.numeric(GESTAGEBIRTH_ANY),
      TCB_DAYS = as.numeric(TCB_DAYS),
      TCB_HRS = as.numeric(TCB_HRS)
    )
} else {
  tcb_thresholds <- tibble(
    GESTAGEBIRTH_ANY = numeric(),
    TCB_DAYS = numeric(),
    TCB_HRS = numeric(),
    TCB_AAP_THRESH = numeric(),
    TCB_NICE_THRESH = numeric()
  )
}

thresholds_needed <- tcb_long %>%
  filter(TCB_REPORTED == 1, !is.na(GESTAGEBIRTH_ANY),
         !is.na(TCB_DAYS), !is.na(TCB_HRS), TCB_DAYS >= 0) %>%
  distinct(GESTAGEBIRTH_ANY, TCB_DAYS, TCB_HRS)

new_thresholds <- thresholds_needed %>%
  anti_join(tcb_thresholds,
            by = c("GESTAGEBIRTH_ANY", "TCB_DAYS", "TCB_HRS"))

if (nrow(new_thresholds) > 0) {
  cat("Calculating", nrow(new_thresholds), "new TCB threshold combinations\n")
  new_thresholds <- new_thresholds %>%
    rowwise() %>%
    mutate(
      # AAP: only valid for GA >= 35 weeks; NA otherwise.
      TCB_AAP_THRESH = ifelse(
        !is.na(GESTAGEBIRTH_ANY) & GESTAGEBIRTH_ANY >= 35,
        tryCatch(
          TSB("P0", paste0(GESTAGEBIRTH_ANY, " weeks"),
              days = TCB_DAYS, hours = TCB_HRS) - 3,
          error = function(e) NA_real_
        ),
        NA_real_
      ),
      # NICE: valid for all GA; floor GA at 23 weeks.
      TCB_NICE_THRESH = tryCatch(
        TSB_NICE("P0", paste0(max(GESTAGEBIRTH_ANY, 23), " weeks"),
                 days = TCB_DAYS, hours = TCB_HRS) - 3,
        error = function(e) NA_real_
      )
    ) %>%
    ungroup()
  
  tcb_thresholds <- bind_rows(tcb_thresholds, new_thresholds) %>%
    distinct(GESTAGEBIRTH_ANY, TCB_DAYS, TCB_HRS, .keep_all = TRUE)
  write.csv(tcb_thresholds, threshold_file, row.names = FALSE)
} else {
  cat("All TCB thresholds already available in lookup\n")
}

# Join thresholds and classify. NOTE: 77L = ineligible (GA < 35 for AAP),
# so it's distinguishable from NA (no data / no threshold).
tcb_long <- tcb_long %>%
  left_join(tcb_thresholds,
            by = c("GESTAGEBIRTH_ANY", "TCB_DAYS", "TCB_HRS")) %>%
  mutate(
    # AAP:
    #   77 = GA < 35 weeks or GA missing (ineligible by design)
    #    1 = TCB >= AAP threshold
    #    0 = TCB < AAP threshold
    #    0 = threshold is NA after TSB() ran (package couldn't produce a value)
    #   NA = no TCB result at all
    INF_HYPERBILI_AAP = case_when(
      TCB_REPORTED == 1 & (is.na(GESTAGEBIRTH_ANY) | GESTAGEBIRTH_ANY < 35) ~ 77L,
      TCB_REPORTED == 1 & is.na(TCB_AAP_THRESH) ~ 0L,   # <- threshold NA after TSB() -> 0
      TCB_REPORTED == 1 & TCB_VALUE >= TCB_AAP_THRESH ~ 1L,
      TCB_REPORTED == 1 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # NICE: same pattern, no GA restriction.
    INF_HYPERBILI_NICE = case_when(
      TCB_REPORTED == 1 & is.na(TCB_NICE_THRESH) ~ 0L,  # <- same rule for NICE
      TCB_REPORTED == 1 & TCB_VALUE >= TCB_NICE_THRESH ~ 1L,
      TCB_REPORTED == 1 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # Age window tags per visit — this is what makes the multi-visit case work
    TCB_WINDOW = case_when(
      !is.na(TCB_VALUE) & !is.na(AGE_AT_VISIT_HOURS_USE) &
        AGE_AT_VISIT_HOURS_USE >= 0     & AGE_AT_VISIT_HOURS_USE < 24    ~ "24HR",
      !is.na(TCB_VALUE) & !is.na(AGE_AT_VISIT_HOURS_USE) &
        AGE_AT_VISIT_HOURS_USE >= 24    & AGE_AT_VISIT_HOURS_USE < 5*24  ~ "5DAY",
      !is.na(TCB_VALUE) & !is.na(AGE_AT_VISIT_HOURS_USE) &
        AGE_AT_VISIT_HOURS_USE >= 5*24  & AGE_AT_VISIT_HOURS_USE <= 14*24 ~ "14DAY",
      !is.na(TCB_VALUE) & !is.na(AGE_AT_VISIT_HOURS_USE) &
        AGE_AT_VISIT_HOURS_USE > 14*24 ~ ">14DAY",
      TRUE ~ NA_character_
    )
  )

## Infant-level TCB outcomes ----
first_positive_age <- function(flag, age) {
  z <- age[flag == 1 & !is.na(flag) & !is.na(age)]
  if (length(z) == 0) NA_real_ else min(z)
}

# Helper: collapse a vector of flags to 1 / 0 / 77 / NA
#   1 if any 1
#   0 if no 1 but at least one non-missing 0
#   77 if no 1 and no 0 but at least one 77  (only used for AAP)
#   NA otherwise (all missing)
collapse_flag <- function(x, allow_77 = FALSE) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_integer_)
  if (any(x == 1L))  return(1L)
  if (any(x == 0L))  return(0L)
  if (allow_77 && any(x == 77L)) return(77L)
  NA_integer_
}

### Criterion 1: TCB >15 mg/dL ----
hyperbili_crit1 <- tcb_long %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  summarise(
    INF_HYPERBILI_TCB15_AGE_DAYS = first_positive_age(INF_HYPERBILI_TCB15, AGE_AT_VISIT_DAYS),
    INF_HYPERBILI_TCB15_AGE_HRS = {
      a <- first_positive_age(INF_HYPERBILI_TCB15, AGE_AT_VISIT_HOURS_USE)
      if (is.na(a)) NA_real_ else a %% 24
    },
    INF_HYPERBILI_TCB15_ANY   = collapse_flag(INF_HYPERBILI_TCB15[TCB_WINDOW %in% c("24HR","5DAY","14DAY")]),
    INF_HYPERBILI_TCB15_24HR  = collapse_flag(INF_HYPERBILI_TCB15[TCB_WINDOW == "24HR"]),
    INF_HYPERBILI_TCB15_5DAY  = collapse_flag(INF_HYPERBILI_TCB15[TCB_WINDOW == "5DAY"]),
    INF_HYPERBILI_TCB15_14DAY = collapse_flag(INF_HYPERBILI_TCB15[TCB_WINDOW == "14DAY"]),
    .groups = "drop"
  )


### Criterion 2: TCB >= AAP threshold ----
hyperbili_crit2 <- tcb_long %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  summarise(
    GESTAGEBIRTH_ANY = first(na.omit(GESTAGEBIRTH_ANY), default = NA_real_),
    INF_HYPERBILI_AAP_AGE_DAYS = first_positive_age(INF_HYPERBILI_AAP, AGE_AT_VISIT_DAYS),
    INF_HYPERBILI_AAP_AGE_HRS = {
      a <- first_positive_age(INF_HYPERBILI_AAP, AGE_AT_VISIT_HOURS_USE)
      if (is.na(a)) NA_real_ else a %% 24
    },
    INF_HYPERBILI_AAP_ANY   = collapse_flag(INF_HYPERBILI_AAP[TCB_WINDOW %in% c("24HR","5DAY","14DAY")], allow_77 = TRUE),
    INF_HYPERBILI_AAP_24HR  = collapse_flag(INF_HYPERBILI_AAP[TCB_WINDOW == "24HR"], allow_77 = TRUE),
    INF_HYPERBILI_AAP_5DAY  = collapse_flag(INF_HYPERBILI_AAP[TCB_WINDOW == "5DAY"], allow_77 = TRUE),
    INF_HYPERBILI_AAP_14DAY = collapse_flag(INF_HYPERBILI_AAP[TCB_WINDOW == "14DAY"], allow_77 = TRUE),
    .groups = "drop"
  )


### Criterion 4: TCB >= NICE threshold ----
hyperbili_crit4 <- tcb_long %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  summarise(
    INF_HYPERBILI_NICE_AGE_DAYS = first_positive_age(INF_HYPERBILI_NICE, AGE_AT_VISIT_DAYS),
    INF_HYPERBILI_NICE_AGE_HRS = {
      a <- first_positive_age(INF_HYPERBILI_NICE, AGE_AT_VISIT_HOURS_USE)
      if (is.na(a)) NA_real_ else a %% 24
    },
    INF_HYPERBILI_NICE_ANY   = collapse_flag(INF_HYPERBILI_NICE[TCB_WINDOW %in% c("24HR","5DAY","14DAY")]),
    INF_HYPERBILI_NICE_24HR  = collapse_flag(INF_HYPERBILI_NICE[TCB_WINDOW == "24HR"]),
    INF_HYPERBILI_NICE_5DAY  = collapse_flag(INF_HYPERBILI_NICE[TCB_WINDOW == "5DAY"]),
    INF_HYPERBILI_NICE_14DAY = collapse_flag(INF_HYPERBILI_NICE[TCB_WINDOW == "14DAY"]),
    .groups = "drop"
  )

## Criterion 3: Clinical jaundice ----
jaundice_long <- tcb_long %>%
  filter(!is.na(AGE_AT_VISIT_DAYS), AGE_AT_VISIT_DAYS <= 35) %>%
  mutate(
    INF_JAUN = case_when(
      JAUND_CEOCCUR == 1 | YELLOW_CEOCCUR == 1 ~ 1L,
      JAUND_CEOCCUR %in% c(0,1) | YELLOW_CEOCCUR %in% c(0,1) ~ 0L,
      TRUE ~ NA_integer_
    ),
    INF_JAUN_DATE = if_else(INF_JAUN == 1, VISIT_DATE, as.Date(NA)),
    INF_JAUN_AGE_DAYS = if_else(INF_JAUN == 1, AGE_AT_VISIT_DAYS, NA_real_),
    INF_JAUN_AGE_HOURS_IPC = if_else(INF_JAUN == 1 & VISIT_SOURCE == "IPC",
                                     AGE_AT_VISIT_HRS, NA_real_),
    INF_JAUN_SEV = case_when(
      INF_JAUN == 1 & (YELLOW_CEOCCUR == 1 | JAUND_ONSET == 1) ~ 1L,
      INF_JAUN == 1 ~ 0L,
      TRUE ~ 0L
    ),
    INF_JAUN_NON_SEV = case_when(
      INF_JAUN == 1 & INF_JAUN_SEV == 0 ~ 1L,
      TRUE ~ 0L
    ),
    DENOM_JAUN = as.integer(
      (JAUND_CEOCCUR %in% c(0,1) | YELLOW_CEOCCUR %in% c(0,1)) &
        AGE_AT_VISIT_DAYS <= 14 #14days!
    )
  )


first_positive_date_j <- function(flag, date) {
  # coerce to Date no matter what comes in
  date <- suppressWarnings(as.Date(date))
  z <- date[!is.na(flag) & flag == 1 & !is.na(date)]
  if (length(z) == 0) return(as.Date(NA))
  suppressWarnings(min(z, na.rm = TRUE))
}

first_positive_age_j <- function(flag, age) {
  age <- suppressWarnings(as.numeric(age))
  z <- age[!is.na(flag) & flag == 1 & !is.na(age)]
  if (length(z) == 0) return(NA_real_)
  min(z)
}

first_positive_hours_j <- function(flag, hours) {
  hours <- suppressWarnings(as.numeric(hours))
  z <- hours[!is.na(flag) & flag == 1 & !is.na(hours)]
  if (length(z) == 0) return(NA_real_)
  min(z)
}
## per-infant (wide) — group_by + summarise collapses visits
hyperbili_crit3 <- jaundice_long %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  summarise(
    
    ## ---- Overall jaundice ----
    INF_JAUN          = as.integer(any(INF_JAUN == 1, na.rm = TRUE)),
    INF_JAUN_DATE     = first_positive_date_j(INF_JAUN, INF_JAUN_DATE),
    INF_JAUN_AGE_DAYS = first_positive_age_j(INF_JAUN, INF_JAUN_AGE_DAYS),
    INF_JAUN_AGE_HRS  = first_positive_hours_j(INF_JAUN, INF_JAUN_AGE_HOURS_IPC),
    
    ## ---- Non-severe jaundice ----
    INF_JAUN_NON_SEV_ANY      = as.integer(any(INF_JAUN_NON_SEV == 1, na.rm = TRUE)),
    INF_JAUN_NON_SEV_DATE     = first_positive_date_j(INF_JAUN_NON_SEV, INF_JAUN_DATE),
    INF_JAUN_NON_SEV_AGE_DAYS = first_positive_age_j(INF_JAUN_NON_SEV, INF_JAUN_AGE_DAYS),
    INF_JAUN_NON_SEV_AGE_HRS  = first_positive_hours_j(INF_JAUN_NON_SEV, INF_JAUN_AGE_HOURS_IPC),
    
    ## ---- Severe jaundice ----
    INF_JAUN_SEV           = as.integer(any(INF_JAUN_SEV == 1, na.rm = TRUE)),
    INF_JAUN_SEV_DATE      = first_positive_date_j(INF_JAUN_SEV, INF_JAUN_DATE),
    INF_JAUN_SEV_AGE_DAYS  = first_positive_age_j(INF_JAUN_SEV, INF_JAUN_AGE_DAYS),
    INF_JAUN_SEV_AGE_HRS   = first_positive_hours_j(INF_JAUN_SEV, INF_JAUN_AGE_HOURS_IPC),
    
    ## ---- Severe within 24hr of onset ----
    INF_JAUN_SEV_24HR = as.integer(any(
      INF_JAUN_SEV == 1 &
        (JAUND_ONSET == 1 |
           (!is.na(AGE_AT_VISIT_HOURS_USE) &
              AGE_AT_VISIT_HOURS_USE >= 0 &
              AGE_AT_VISIT_HOURS_USE < 24)),
      na.rm = TRUE
    )),
    
    ## ---- Severe >24hr after onset ----
    INF_JAUN_SEV_GREATER_24HR = as.integer(any(
      INF_JAUN_SEV == 1 &
        (JAUND_ONSET == 2 |
           (is.na(JAUND_ONSET) &
              !is.na(AGE_AT_VISIT_HOURS_USE) &
              AGE_AT_VISIT_HOURS_USE >= 24)),
      na.rm = TRUE
    )),
    
    ## ---- Denominator ----
    DENOM_JAUN = as.integer(any(DENOM_JAUN == 1, na.rm = TRUE)),
    
    .groups = "drop"
  )

## 10. TCB denominators ----
# Built off the same TCB_WINDOW tag used in hyperbili_crit1/2/4,
# so the denominator and the outcome cannot disagree on window boundaries.
hyperbili_denom <- tcb_long %>%
  mutate(
    TCB_WINDOW = toupper(trimws(as.character(TCB_WINDOW))),
    TCB_VALUE  = suppressWarnings(as.numeric(as.character(TCB_VALUE)))
  ) %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  summarise(
    DENOM_HYPERBILI_24HR = as.integer(
      any(TCB_WINDOW == "24HR" & !is.na(TCB_VALUE))
    ),
    
    DENOM_HYPERBILI_5DAY = as.integer(
      any(TCB_WINDOW == "5DAY" & !is.na(TCB_VALUE))
    ),
    
    DENOM_HYPERBILI_14DAY = as.integer(
      any(TCB_WINDOW == "14DAY" & !is.na(TCB_VALUE))
    ),
    
    DENOM_HYPERBILI_ANY = as.integer(
      any(TCB_WINDOW %in% c("24HR", "5DAY", "14DAY") & !is.na(TCB_VALUE))
    ),
    
    .groups = "drop"
  )

## 11. Denominator audit ----
cat("\nRAW TCB DENOMINATOR CHECK\n")
cat("ANY:", sum(hyperbili_denom$DENOM_HYPERBILI_ANY == 1), "\n")
cat("\n[3] Denominator totals (n = 1)\n")
cat(sprintf("    ANY   : %d\n", sum(hyperbili_denom$DENOM_HYPERBILI_ANY   == 1, na.rm = TRUE)))
cat(sprintf("    24HR  : %d\n", sum(hyperbili_denom$DENOM_HYPERBILI_24HR  == 1, na.rm = TRUE)))
cat(sprintf("    5DAY  : %d\n", sum(hyperbili_denom$DENOM_HYPERBILI_5DAY  == 1, na.rm = TRUE)))
cat(sprintf("    14DAY : %d\n", sum(hyperbili_denom$DENOM_HYPERBILI_14DAY == 1, na.rm = TRUE)))


tcb_age_summary <- tcb_long %>%
  filter(TCB_REPORTED == 1) %>%
  count(TCB_AGE_WINDOW, name = "N_TCB") %>%
  mutate(PERCENT = round(100 * N_TCB / sum(N_TCB), 1))

cat("\nWHERE ALL ACTUAL TCB MEASUREMENTS FALL BY AGE\n")
for (i in seq_len(nrow(tcb_age_summary))) {
  cat(tcb_age_summary$TCB_AGE_WINDOW[i], ":",
      tcb_age_summary$N_TCB[i], "TCBs (",
      tcb_age_summary$PERCENT[i], "%)\n", sep = " ")
}

## 12. Final infant-level dataset ----
hyperbili_all_crit <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, DELIVERY_DATETIME,
         GESTAGEBIRTH_ANY, ADJUD_NEEDED, LIVEBIRTH) %>%
  left_join(hyperbili_crit1, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(hyperbili_crit2 %>% select(-GESTAGEBIRTH_ANY),
            by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(hyperbili_crit3, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(hyperbili_crit4, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(hyperbili_denom, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  mutate(across(
    c(DENOM_HYPERBILI_ANY, DENOM_HYPERBILI_24HR,
      DENOM_HYPERBILI_5DAY, DENOM_HYPERBILI_14DAY),
    ~ replace_na(.x, 0L)
  )) %>%
  mutate(across(c(
    INF_HYPERBILI_TCB15_24HR, INF_HYPERBILI_TCB15_5DAY, INF_HYPERBILI_TCB15_14DAY,
    INF_HYPERBILI_AAP_24HR, INF_HYPERBILI_AAP_5DAY, INF_HYPERBILI_AAP_14DAY,
    INF_HYPERBILI_NICE_24HR, INF_HYPERBILI_NICE_5DAY, INF_HYPERBILI_NICE_14DAY,
    INF_JAUN_NON_SEV_ANY, INF_JAUN_SEV_24HR, INF_JAUN_SEV_GREATER_24HR,
    DENOM_HYPERBILI_ANY, DENOM_HYPERBILI_24HR, DENOM_HYPERBILI_5DAY,
    DENOM_HYPERBILI_14DAY, DENOM_JAUN),
    ~ case_when(
      ADJUD_NEEDED == 1 ~ 55L,
      TRUE ~ as.integer(.x)
    )
  )) %>%
  select(-ADJUD_NEEDED) %>%
  mutate(
    across(
      c(INF_HYPERBILI_TCB15_ANY, INF_HYPERBILI_TCB15_24HR,
        INF_HYPERBILI_TCB15_5DAY, INF_HYPERBILI_TCB15_14DAY),
      ~ case_when(
        is.na(.x) & LIVEBIRTH != 1 ~ 77L,
        is.na(.x) & LIVEBIRTH == 1 ~ 55L,
        TRUE ~ as.integer(.x))
    ),
    across(
      c(INF_HYPERBILI_AAP_ANY, INF_HYPERBILI_AAP_24HR,
        INF_HYPERBILI_AAP_5DAY, INF_HYPERBILI_AAP_14DAY),
      ~ case_when(
        is.na(.x) & LIVEBIRTH != 1 ~ 77L,
        is.na(.x) & LIVEBIRTH == 1 & !is.na(GESTAGEBIRTH_ANY) & GESTAGEBIRTH_ANY < 35 ~ 77L,
        is.na(.x) & LIVEBIRTH == 1 ~ 55L,
        TRUE ~ as.integer(.x))
    ),
    across(
      c(INF_HYPERBILI_NICE_ANY, INF_HYPERBILI_NICE_24HR,
        INF_HYPERBILI_NICE_5DAY, INF_HYPERBILI_NICE_14DAY),
      ~ case_when(
        is.na(.x) & LIVEBIRTH != 1 ~ 77L,
        is.na(.x) & LIVEBIRTH == 1 ~ 55L,
        TRUE ~ as.integer(.x))
    )
  )

## 13. Final checks ----
cat("\nFINAL TCB WORKFLOW CHECK\n")
cat("Actual TCB measurements:", sum(tcb_long$TCB_REPORTED == 1, na.rm = TRUE), "\n")
cat("Negative cleaned TCB values:", sum(tcb_long$TCB_VALUE < 0, na.rm = TRUE), "\n")
cat("TCBs with exact datetime age:",
    sum(tcb_long$TCB_REPORTED == 1 & tcb_long$AGE_SOURCE == "DATETIME", na.rm = TRUE), "\n")
cat("TCBs with date-only age:",
    sum(tcb_long$TCB_REPORTED == 1 & tcb_long$AGE_SOURCE == "DATE_ONLY", na.rm = TRUE), "\n")
cat("TCBs with missing age:",
    sum(tcb_long$TCB_REPORTED == 1 & tcb_long$AGE_SOURCE == "MISSING", na.rm = TRUE), "\n")

# Distribution of the per-observation flags — this is the key diagnostic
cat("\nPER-OBSERVATION FLAG DISTRIBUTIONS\n")
cat("INF_HYPERBILI_TCB15:\n"); print(table(tcb_long$INF_HYPERBILI_TCB15, useNA = "always"))
cat("INF_HYPERBILI_AAP:\n");   print(table(tcb_long$INF_HYPERBILI_AAP,   useNA = "always"))
cat("INF_HYPERBILI_NICE:\n");  print(table(tcb_long$INF_HYPERBILI_NICE,  useNA = "always"))

tcb_long_export <- tcb_long %>% filter (!is.na(TCB_VALUE) & TCB_REPORTED == 1)

## Export ----
# Long datasets: every available visit/result.
write.csv(tcb_long_export, paste0(path_to_save, "INF_TCB_LONG.csv"), row.names = FALSE)
write.csv(hyperbili_all_crit, paste0(path_to_save, "hyperbili_all_crit.csv"), row.names = FALSE)
# write.csv(psbi_outcome, paste0(path_to_tnt, "INF_PSBI_LONG.csv"), row.names = FALSE)

#*****************************************************************************
## PSBI ----
#* Presence of any clinical signs or symptoms as defined by the WHO IMCI criteria, 
#* which are consistent with possible severe bacterial infection from delivery to 59 days. 

# Forms needed: 
# MNH11 
# MNH13 

# Variables needed
#* Has the baby had difficulty in feeding (i.e., feeding poorly or not feeding at all)? [POOR_FEED_CEOCCUR, POOR_FEED_CEOCCUR_MR]
#* Has the baby had convulsions? [CONV_CEOCCUR, CONV_CEOCCUR_MR]
#* Has the baby had fast or difficult breathing? [BREATH_VSORRES_1, BREATH_VSORRES_2, BREATH_CEOCCUR_MR]
#* Does infant have severe chest in-drawing? [CHESTINDRAW_CEOCCUR, CHEST_CEOCCUR]
#* Has the baby had a fever? [TEMP_VSORRES_1, TEMP_VSORRES_2, FEVER_CEOCCUR_MR]
#* Record infant temperature at time of visit. Is the baby cold [TEMP_VSORRES, HYPO_CEOCCUR_MR]
#*****************************************************************************

## PSBI outcomes ----

## Presence of any clinical signs or symptoms as defined by the WHO IMCI
## criteria, consistent with possible severe bacterial infection (PSBI),
## from delivery through 59 days of age.
##
## Forms needed: MNH11 (IPC), MNH13 (PNC), MNH20 (hospitalization)
##
## Variables used:
##   Has the baby had difficulty in feeding (feeding poorly or not at all)?
##     [POOR_FEED_CEOCCUR, POOR_FEED_CEOCCUR_MR]
##   Has the baby had convulsions?
##     [CONV_CEOCCUR, CONV_CEOCCUR_MR]
##   Has the baby had fast or difficult breathing?
##     [BREATH_VSORRES_1, BREATH_VSORRES_2, BREATH_CEOCCUR_MR]
##   Does the infant have severe chest in-drawing?
##     [CHESTINDRAW_CEOCCUR, CHEST_CEOCCUR]
##   Has the baby had a fever?
##     [TEMP_VSORRES_1, TEMP_VSORRES_2, FEVER_CEOCCUR_MR]
##   Record infant temperature at time of visit. Is the baby cold?
##     [TEMP_VSORRES, HYPO_CEOCCUR_MR]

## ---- Helper Functions ----
## Return the earliest age / date / hour among rows where the flag is 1.
## If there are no positive rows, return NA.

first_positive_age <- function(flag, age) {
  age <- suppressWarnings(as.numeric(age))
  z <- age[!is.na(flag) & flag == 1 & !is.na(age)]
  if (length(z) == 0) NA_real_ else min(z)
}

first_positive_date <- function(flag, date) {
  date <- suppressWarnings(as.Date(date))
  z <- date[!is.na(flag) & flag == 1 & !is.na(date)]
  if (length(z) == 0) as.Date(NA) else suppressWarnings(min(z, na.rm = TRUE))
}

first_positive_hours <- function(flag, hours) {
  hours <- suppressWarnings(as.numeric(hours))
  z <- hours[!is.na(flag) & flag == 1 & !is.na(hours)]
  if (length(z) == 0) NA_real_ else min(z)
}


## Long dataset creation ----

## One row per infant per visit. PSBI is evaluated visit by visit, using
## the raw clinical signs from whichever form the visit came from.

## ---- IPC visits ----
## IPC (immediate postnatal care) visits come from MNH11.

psbi_ipc_long <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, DOB, BIRTH_OUTCOME, LIVEBIRTH) %>%
  left_join(mnh11_constructed, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  mutate(
    DOB          = clean_date(DOB),
    VISIT_DATE   = clean_date(M11_VISIT_OBSSTDAT),
    AGE_AT_VISIT = as.numeric(VISIT_DATE - DOB),
    ## Flag visits where the date is missing or before birth.
    INVALID_DATE_INFO = as.integer(AGE_AT_VISIT < 0 | is.na(AGE_AT_VISIT)),
    VISIT_SOURCE = "IPC",
    VISIT_WINDOW = "IPC",
    
    ## In India-SAS, 77 is the default "not done" code; treat it as missing.
    M11_BREATH_VSORRES_1 = if_else(SITE == "India-SAS" & M11_BREATH_VSORRES_1 == 77,
                                   NA_real_, as.numeric(M11_BREATH_VSORRES_1)),
    M11_BREATH_VSORRES_2 = if_else(SITE == "India-SAS" & M11_BREATH_VSORRES_2 == 77,
                                   NA_real_, as.numeric(M11_BREATH_VSORRES_2)),
    
    ## PSBI = 1 if ANY of the WHO IMCI danger signs is present:
    ##   poor feeding, convulsions, fast breathing (>=60 bpm),
    ##   severe chest in-drawing, fever (>=38 C), or low temp (<35.5 C).
    INF_PSBI = case_when(
      M11_POOR_FEED_CEOCCUR == 1 |
        M11_CONV_CEOCCUR == 1 |
        M11_BREATH_VSORRES_1 >= 60 |
        M11_BREATH_VSORRES_2 >= 60 |
        M11_CHESTINDRAW_CEOCCUR == 1 |
        M11_TEMP_VSORRES   >= 38 |
        M11_TEMP_VSORRES_2 >= 38 |
        (M11_TEMP_VSORRES   > 0 & M11_TEMP_VSORRES   < 35.5) |
        (M11_TEMP_VSORRES_2 > 0 & M11_TEMP_VSORRES_2 < 35.5) ~ 1L,
      TRUE ~ 0L
    )
  )

## ---- PNC visits ----
## PNC (postnatal care) visits come from MNH13. Windows are assigned by age.

psbi_pnc_long <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, DOB, BIRTH_OUTCOME, LIVEBIRTH) %>%
  right_join(mnh13, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  mutate(
    DOB          = clean_date(DOB),
    VISIT_DATE   = clean_date(M13_VISIT_OBSSTDAT),
    AGE_AT_VISIT = as.numeric(VISIT_DATE - DOB),
    INVALID_DATE_INFO = as.integer(AGE_AT_VISIT < 0 | is.na(AGE_AT_VISIT)),
    VISIT_SOURCE = "PNC",
    ## Classify the visit into a scheduled PNC window or "INBETWEEN" based on age.
    VISIT_WINDOW = assign_pnc_window(AGE_AT_VISIT),
    
    ## In Ghana, 77 is the default "not done" code; treat it as missing.
    M13_BREATH_VSORRES_1 = if_else(SITE == "Ghana" & M13_BREATH_VSORRES_1 == 77,
                                   NA_real_, as.numeric(M13_BREATH_VSORRES_1)),
    M13_BREATH_VSORRES_2 = if_else(SITE == "Ghana" & M13_BREATH_VSORRES_2 == 77,
                                   NA_real_, as.numeric(M13_BREATH_VSORRES_2)),
    M13_TEMP_VSORRES_1   = if_else(SITE == "Ghana" & M13_TEMP_VSORRES_1 == 77,
                                   NA_real_, as.numeric(M13_TEMP_VSORRES_1)),
    M13_TEMP_VSORRES_2   = if_else(SITE == "Ghana" & M13_TEMP_VSORRES_2 == 77,
                                   NA_real_, as.numeric(M13_TEMP_VSORRES_2)),
    
    ## PSBI = 1 if ANY of the WHO IMCI danger signs is present.
    INF_PSBI = case_when(
      M13_POOR_FEED_CEOCCUR == 1 |
        M13_CONV_CEOCCUR == 1 |
        M13_BREATH_VSORRES_1 >= 60 |
        M13_BREATH_VSORRES_2 >= 60 |
        M13_CHEST_CEOCCUR == 1 |
        M13_TEMP_VSORRES_1 >= 38 |
        M13_TEMP_VSORRES_2 >= 38 |
        (M13_TEMP_VSORRES_1 > 0 & M13_TEMP_VSORRES_1 < 35.5) |
        (M13_TEMP_VSORRES_2 > 0 & M13_TEMP_VSORRES_2 < 35.5) ~ 1L,
      TRUE ~ 0L
    ),
    
    ## Flag cases where a high or low temperature was recorded on the first
    ## measurement but the confirmatory second measurement is missing.
    MISSING_TEMP_PNC_HIGH = as.integer(M13_TEMP_VSORRES_1 >= 38 & M13_TEMP_VSORRES_2 < 0),
    MISSING_TEMP_PNC_LOW  = as.integer(M13_TEMP_VSORRES_1 > 0 &
                                         M13_TEMP_VSORRES_1 < 35.5 &
                                         M13_TEMP_VSORRES_2 < 0),
    MISSING_TEMP_PNC      = as.integer(MISSING_TEMP_PNC_HIGH == 1 | MISSING_TEMP_PNC_LOW == 1)
  )

## which signs prevail more?
psbi_ipc_long %>%
  filter(INF_PSBI == 1) %>%
  summarise(
    Total_PSBI    = n(),
    Poor_feeding  = sum(M11_POOR_FEED_CEOCCUR == 1, na.rm = TRUE),
    Convulsions   = sum(M11_CONV_CEOCCUR == 1, na.rm = TRUE),
    Fast_breath   = sum(M11_BREATH_VSORRES_1 >= 60 | M11_BREATH_VSORRES_2 >= 60, na.rm = TRUE),
    Chest_indraw  = sum(M11_CHESTINDRAW_CEOCCUR == 1, na.rm = TRUE),
    Fever         = sum(M11_TEMP_VSORRES >= 38 | M11_TEMP_VSORRES_2 >= 38, na.rm = TRUE),
    Low_temp      = sum((M11_TEMP_VSORRES > 0 & M11_TEMP_VSORRES < 35.5) |
                          (M11_TEMP_VSORRES_2 > 0 & M11_TEMP_VSORRES_2 < 35.5), na.rm = TRUE)
  )

psbi_pnc_long %>%
  filter(INF_PSBI == 1) %>%
  summarise(
    Total_PSBI    = n(),
    Poor_feeding  = sum(M13_POOR_FEED_CEOCCUR == 1, na.rm = TRUE),
    Convulsions   = sum(M13_CONV_CEOCCUR == 1, na.rm = TRUE),
    Fast_breath   = sum(M13_BREATH_VSORRES_1 >= 60 | M13_BREATH_VSORRES_2 >= 60, na.rm = TRUE),
    Chest_indraw  = sum(M13_CHEST_CEOCCUR == 1, na.rm = TRUE),
    Fever         = sum(M13_TEMP_VSORRES_1 >= 38 | M13_TEMP_VSORRES_2 >= 38, na.rm = TRUE),
    Low_temp      = sum((M13_TEMP_VSORRES_1 > 0 & M13_TEMP_VSORRES_1 < 35.5) |
                          (M13_TEMP_VSORRES_2 > 0 & M13_TEMP_VSORRES_2 < 35.5), na.rm = TRUE)
  )

## ---- Hospital visits ----
## Hospitalization visits come from MNH20. Treated as their own source.

psbi_hospital_long <- inf_baseline %>%
  select(SITE, MOMID, PREGID, INFANTID, DOB, BIRTH_OUTCOME, LIVEBIRTH) %>%
  right_join(mnh20, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  mutate(
    DOB          = clean_date(DOB),
    VISIT_DATE   = clean_date(M20_OBSSTDAT),
    AGE_AT_VISIT = as.numeric(VISIT_DATE - DOB),
    INVALID_DATE_INFO = as.integer(AGE_AT_VISIT < 0 | is.na(AGE_AT_VISIT)),
    VISIT_SOURCE = "HOSPITAL",
    VISIT_WINDOW = "HOSPITAL",
    
    ## PSBI = 1 if ANY of the WHO IMCI danger signs is present.
    INF_PSBI = case_when(
      M20_POOR_FEED_CEOCCUR == 1 |
        M20_CONV_CEOCCUR == 1 |
        M20_RR_VSORRES >= 60 |
        M20_MAX_RR_VSORRES >= 60 |
        M20_CHEST_CEOCCUR == 1 |
        M20_TEMP_VSORRES >= 38 |
        M20_MAX_TEMP_VSORRES >= 38 |
        (M20_TEMP_VSORRES     > 0 & M20_TEMP_VSORRES     < 35.5) |
        (M20_LOW_TEMP_VSORRES > 0 & M20_LOW_TEMP_VSORRES < 35.5) ~ 1L,
      TRUE ~ 0L
    )
  )

## ---- Bind and filter to birth through day 59 ----
## PSBI is only defined for live births between 0 and 59 days of age.

psbi_long <- bind_rows(psbi_ipc_long, psbi_pnc_long, psbi_hospital_long) %>%
  filter(LIVEBIRTH == 1, !is.na(AGE_AT_VISIT), AGE_AT_VISIT >= 0, AGE_AT_VISIT <= 59) %>%
  arrange(SITE, MOMID, PREGID, INFANTID, AGE_AT_VISIT, VISIT_DATE) %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  ## Number visits per infant in chronological order.
  mutate(VISIT_SEQ = row_number()) %>%
  ungroup()


## Adds two kinds of per-visit indicator:
##   HAS_VISIT_*    - does this infant have at least one visit in this window?
##   INF_PSBI_*_FIRST - is this row the FIRST positive visit in this window?
## The *_FIRST markers use cumsum() so only the earliest positive row is
## flagged, which prevents an infant from being counted twice.

psbi_outcome <- psbi_long %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  arrange(AGE_AT_VISIT, VISIT_DATE, .by_group = TRUE) %>%
  mutate(
    ## ---- Visit-presence indicators ----
    ## These let us later distinguish "no visit" (unknown) from
    ## "visited but no PSBI" (confirmed zero).
    HAS_VISIT_ANY      = as.integer(any(!is.na(VISIT_DATE))),
    HAS_VISIT_IPC      = as.integer(any(VISIT_SOURCE == "IPC")),
    HAS_VISIT_PNC0     = as.integer(any(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC0")),
    HAS_VISIT_PNC1     = as.integer(any(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC1")),
    HAS_VISIT_PNC4     = as.integer(any(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC4")),
    HAS_VISIT_PNC6     = as.integer(any(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC6")),
    HAS_VISIT_UNSCHED  = as.integer(any(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "INBETWEEN")),
    HAS_VISIT_HOSPITAL = as.integer(any(VISIT_SOURCE == "HOSPITAL")),
    
    ## ---- First-positive flags ----
    ## Mark only the first visit where PSBI = 1 for each category, so an
    ## infant with multiple PSBI visits is counted once.
    INF_PSBI_FIRST = as.integer(INF_PSBI == 1 & cumsum(INF_PSBI == 1) == 1),
    INF_PSBI_IPC_FIRST = as.integer(
      VISIT_SOURCE == "IPC" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "IPC" & INF_PSBI == 1) == 1
    ),
    INF_PSBI_PNC0_FIRST = as.integer(
      VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC0" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC0" & INF_PSBI == 1) == 1
    ),
    INF_PSBI_PNC1_FIRST = as.integer(
      VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC1" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC1" & INF_PSBI == 1) == 1
    ),
    INF_PSBI_PNC4_FIRST = as.integer(
      VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC4" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC4" & INF_PSBI == 1) == 1
    ),
    INF_PSBI_PNC6_FIRST = as.integer(
      VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC6" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "PNC6" & INF_PSBI == 1) == 1
    ),
    INF_PSBI_UNSCHED_FIRST = as.integer(
      VISIT_SOURCE == "PNC" & VISIT_WINDOW == "INBETWEEN" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "PNC" & VISIT_WINDOW == "INBETWEEN" & INF_PSBI == 1) == 1
    ),
    INF_PSBI_HOSPITAL_FIRST = as.integer(
      VISIT_SOURCE == "HOSPITAL" & INF_PSBI == 1 &
        cumsum(VISIT_SOURCE == "HOSPITAL" & INF_PSBI == 1) == 1
    ),
    
    ## ---- Age-based adjudication for IPC ----
    ## Flag IPC-positive visits where the infant was older than 3 days.
    ## These get recoded to 55 later.
    INF_PSBI_IPC_AGE_GT3 = as.integer(
      INF_PSBI_IPC_FIRST == 1 & !is.na(AGE_AT_VISIT) & AGE_AT_VISIT > 3
    ),
    
    ## ---- Denominator ----
    ## Every live birth is in the denominator.
    INF_PSBI_DENOM = as.integer(BIRTH_OUTCOME == 1)
  ) %>%
  ungroup()


## Collapse to one row per infant. For each PSBI category we keep:
##   - the date and age of the FIRST positive visit
##   - whether any positive visit occurred
##   - whether any visit occurred (for the 0 vs 55 decision)

psbi_outcome_wide <- psbi_outcome %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  summarise(
    DOB = first(DOB),
    
    ## ---- First-positive dates and ages ----
    INF_PSBI_DATE     = first_positive_date(INF_PSBI_FIRST, VISIT_DATE),
    INF_PSBI_AGE_DAYS = first_positive_age(INF_PSBI_FIRST, AGE_AT_VISIT),
    ## Hours are stored as hours-within-day (mod 24).
    INF_PSBI_AGE_HRS  = first_positive_hours(
      INF_PSBI_FIRST,
      as.numeric(difftime(VISIT_DATE, DOB, units = "hours")) %% 24
    ),
    INF_PSBI_IPC_DATE          = first_positive_date(INF_PSBI_IPC_FIRST, VISIT_DATE),
    INF_PSBI_IPC_AGE_DAYS      = first_positive_age(INF_PSBI_IPC_FIRST, AGE_AT_VISIT),
    INF_PSBI_PNC0_DATE         = first_positive_date(INF_PSBI_PNC0_FIRST, VISIT_DATE),
    INF_PSBI_PNC0_AGE_DAYS     = first_positive_age(INF_PSBI_PNC0_FIRST, AGE_AT_VISIT),
    INF_PSBI_PNC1_DATE         = first_positive_date(INF_PSBI_PNC1_FIRST, VISIT_DATE),
    INF_PSBI_PNC1_AGE_DAYS     = first_positive_age(INF_PSBI_PNC1_FIRST, AGE_AT_VISIT),
    INF_PSBI_PNC4_DATE         = first_positive_date(INF_PSBI_PNC4_FIRST, VISIT_DATE),
    INF_PSBI_PNC4_AGE_DAYS     = first_positive_age(INF_PSBI_PNC4_FIRST, AGE_AT_VISIT),
    INF_PSBI_PNC6_DATE         = first_positive_date(INF_PSBI_PNC6_FIRST, VISIT_DATE),
    INF_PSBI_PNC6_AGE_DAYS     = first_positive_age(INF_PSBI_PNC6_FIRST, AGE_AT_VISIT),
    INF_PSBI_UNSCHED_DATE      = first_positive_date(INF_PSBI_UNSCHED_FIRST, VISIT_DATE),
    INF_PSBI_UNSCHED_AGE_DAYS  = first_positive_age(INF_PSBI_UNSCHED_FIRST, AGE_AT_VISIT),
    INF_PSBI_HOSPITAL_DATE     = first_positive_date(INF_PSBI_HOSPITAL_FIRST, VISIT_DATE),
    INF_PSBI_HOSPITAL_AGE_DAYS = first_positive_age(INF_PSBI_HOSPITAL_FIRST, AGE_AT_VISIT),
    
    ## ---- Visit-presence rollups ----
    HAS_VISIT_ANY      = as.integer(any(HAS_VISIT_ANY      == 1, na.rm = TRUE)),
    HAS_VISIT_IPC      = as.integer(any(HAS_VISIT_IPC      == 1, na.rm = TRUE)),
    HAS_VISIT_PNC0     = as.integer(any(HAS_VISIT_PNC0     == 1, na.rm = TRUE)),
    HAS_VISIT_PNC1     = as.integer(any(HAS_VISIT_PNC1     == 1, na.rm = TRUE)),
    HAS_VISIT_PNC4     = as.integer(any(HAS_VISIT_PNC4     == 1, na.rm = TRUE)),
    HAS_VISIT_PNC6     = as.integer(any(HAS_VISIT_PNC6     == 1, na.rm = TRUE)),
    HAS_VISIT_UNSCHED  = as.integer(any(HAS_VISIT_UNSCHED  == 1, na.rm = TRUE)),
    HAS_VISIT_HOSPITAL = as.integer(any(HAS_VISIT_HOSPITAL == 1, na.rm = TRUE)),
    
    ## ---- Positive rollups ----
    INF_PSBI_ANY      = as.integer(any(INF_PSBI_FIRST      == 1, na.rm = TRUE)),
    INF_PSBI_IPC      = as.integer(any(INF_PSBI_IPC_FIRST  == 1, na.rm = TRUE)),
    INF_PSBI_PNC0     = as.integer(any(INF_PSBI_PNC0_FIRST == 1, na.rm = TRUE)),
    INF_PSBI_PNC1     = as.integer(any(INF_PSBI_PNC1_FIRST == 1, na.rm = TRUE)),
    INF_PSBI_PNC4     = as.integer(any(INF_PSBI_PNC4_FIRST == 1, na.rm = TRUE)),
    INF_PSBI_PNC6     = as.integer(any(INF_PSBI_PNC6_FIRST == 1, na.rm = TRUE)),
    INF_PSBI_UNSCHED  = as.integer(any(INF_PSBI_UNSCHED_FIRST == 1, na.rm = TRUE)),
    INF_PSBI_HOSPITAL = as.integer(any(INF_PSBI_HOSPITAL_FIRST == 1, na.rm = TRUE)),
    
    ## ---- Age>3 adjudication rollup ----
    INF_PSBI_IPC_AGE_GT3 = as.integer(any(INF_PSBI_IPC_AGE_GT3 == 1, na.rm = TRUE)),
    
    ## ---- Denominator ----
    INF_PSBI_DENOM = as.integer(any(INF_PSBI_DENOM == 1, na.rm = TRUE)),
    
    .groups = "drop"
  ) %>%
  ## Replace Inf (from max/min on empty groups) with NA.
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)))

## ---- Recode to 0/1/55/77 ----
## For each source-specific flag:
##   1  = had a PSBI-positive visit
##   0  = had a visit in that window, but no PSBI
##   55 = no visit in that window (unknown)
##   77 = no unscheduled or hospital visit (not applicable)
recode_psbi <- function(x, has_visit, no_visit_value = 55L) {
  case_when(x == 1 ~ 1L, has_visit == 1 ~ 0L, TRUE ~ no_visit_value)
}

psbi_outcome_wide <- psbi_outcome_wide %>%
  mutate(
    INF_PSBI_IPC      = recode_psbi(INF_PSBI_IPC,      HAS_VISIT_IPC),
    INF_PSBI_PNC0     = recode_psbi(INF_PSBI_PNC0,     HAS_VISIT_PNC0),
    INF_PSBI_PNC1     = recode_psbi(INF_PSBI_PNC1,     HAS_VISIT_PNC1),
    INF_PSBI_PNC4     = recode_psbi(INF_PSBI_PNC4,     HAS_VISIT_PNC4),
    INF_PSBI_PNC6     = recode_psbi(INF_PSBI_PNC6,     HAS_VISIT_PNC6),
    INF_PSBI_UNSCHED  = recode_psbi(INF_PSBI_UNSCHED,  HAS_VISIT_UNSCHED,  no_visit_value = 77L),
    INF_PSBI_HOSPITAL = recode_psbi(INF_PSBI_HOSPITAL, HAS_VISIT_HOSPITAL, no_visit_value = 77L),
    INF_PSBI_ANY      = recode_psbi(INF_PSBI_ANY,      HAS_VISIT_ANY),
    ## IPC-positive visits at age > 3 days get recoded to 55.
    INF_PSBI_IPC      = if_else(INF_PSBI_IPC == 1 & INF_PSBI_IPC_AGE_GT3 == 1, 55L, INF_PSBI_IPC),
    INF_PSBI_DENOM    = as.integer(INF_PSBI_DENOM)
  )

## ---- Ihme subset ----
## Collapse to one row per infant, keeping the FIRST PSBI-positive visit.
## Recode into the IHME-style categories (1/0/55/77/99).

psbi_outcome_sub <- psbi_outcome %>%
  group_by(SITE, MOMID, PREGID, INFANTID) %>%
  arrange(AGE_AT_VISIT, VISIT_DATE, .by_group = TRUE) %>%
  summarise(
    LIVEBIRTH         = first(LIVEBIRTH),
    ANY_PSBI          = as.integer(any(INF_PSBI == 1, na.rm = TRUE)),
    FIRST_PSBI_AGE    = first_positive_age(INF_PSBI, AGE_AT_VISIT),
    FIRST_PSBI_DATE   = first_positive_date(INF_PSBI, VISIT_DATE),
    INVALID_DATE_INFO = as.integer(any(INF_PSBI == 1 & INVALID_DATE_INFO == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  right_join(
    inf_baseline %>% select(SITE, MOMID, PREGID, INFANTID, LIVEBIRTH),
    by = c("SITE","MOMID","PREGID","INFANTID"),
    suffix = c("", ".base")
  ) %>%
  mutate(LIVEBIRTH = coalesce(LIVEBIRTH, LIVEBIRTH.base)) %>%
  select(-LIVEBIRTH.base) %>%
  mutate(
    ## PSBI before 28 days.
    PSBI_LESS28 = case_when(
      LIVEBIRTH %in% c(0,55) ~ 99L,
      ANY_PSBI == 1 & INVALID_DATE_INFO == 1 ~ 55L,
      ANY_PSBI == 1 & FIRST_PSBI_AGE >= 0 & FIRST_PSBI_AGE < 28 ~ 1L,
      ANY_PSBI == 1 & FIRST_PSBI_AGE >= 28 ~ 77L,
      ANY_PSBI == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    ## PSBI between 28 and 59 days.
    PSBI_GREATER28 = case_when(
      LIVEBIRTH %in% c(0,55) ~ 99L,
      ANY_PSBI == 1 & INVALID_DATE_INFO == 1 ~ 55L,
      ANY_PSBI == 1 & FIRST_PSBI_AGE >= 28 & FIRST_PSBI_AGE <= 59 ~ 1L,
      ANY_PSBI == 1 & FIRST_PSBI_AGE < 28 ~ 77L,
      ANY_PSBI == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    ## PSBI before 59 days (union of the two windows above).
    PSBI_LESS59 = case_when(
      LIVEBIRTH %in% c(0,55) ~ 99L,
      PSBI_LESS28 == 1 | PSBI_GREATER28 == 1 ~ 1L,
      PSBI_LESS28 == 55 | PSBI_GREATER28 == 55 ~ 55L,
      ANY_PSBI == 0 ~ 0L,
      TRUE ~ 77L
    )
  )


