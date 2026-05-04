#*****************************************************************************
#*PRIMSA Flowchart Image Creation
#* Written by: Precious Williams
#* Date Created:  10 March 2026
#* Last updated: 16 March 2026
#* Updates (Current): 
#* 
#*****************************************************************************

library(lubridate)
library(stringr)
library(dplyr)
library(haven)
library(gt)
library(flowchart)
library(magick)

#Step 1 - Load in MAT_FLOWCHART dataset and filter relevant sections ----
#Required paths
UploadDate = "2026-01-30"
path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate)

##Load dataset ----
mat_flowchart <- read.csv(paste0("Z:/Outcome Data/", UploadDate, "/MAT_FLOWCHART.csv"))

#core remapp data from MAT_FLOWCHART
Flowchart_df  <- mat_flowchart %>%
distinct(SITE, SCRNID, MOMID, PREGID, .keep_all = TRUE) #%>%
#filter if remapp cohort
#filter(REMAPP_PRESCRN == 1 |  REMAPP_SCRN == 1)

#Step 2 - Create a function to help with arranging text ----
## Paper-style exclusion text helpers
#This helps create my exclusion description
create_excl  <- function(df,
                        excl_label_col,
                        drop_labels = c("Included", "Not Applicable", "Removed earlier")) {
  df %>%
    filter(!is.na(.data[[excl_label_col]])) %>%
    
    # keep only exclusions (drop Included rows)
    filter(!grepl("^Included", .data[[excl_label_col]])) %>%
    
    # optionally drop labels you don’t want in the paper flow
    filter(!grepl(paste(drop_labels, collapse = "|"),
                  .data[[excl_label_col]],
                  ignore.case = TRUE)) %>%
    
    count(reason = .data[[excl_label_col]], name = "n") %>%
    filter(n > 0) %>%
    arrange(desc(n)) %>%
    mutate(line = paste0(n, " ", reason)) %>%
    pull(line)
}

#Step 3 - Step 1 and 2 exclusion list ----
## 1: Pre-screening exclusions (unchanged) ----
step1_df <- Flowchart_df %>%
  filter(!is.na(PRESCREEN_EXCL_REASON),
         PRESCREEN_EXCL_REASON != 77) %>%
  count(reason = PRESCREEN_EXCL_REASON_LABEL, name = "n")
step1_exc_lines <- step1_df %>%
  filter(n > 0) %>%
  arrange(desc(n)) %>%
  mutate(line = paste0(n, " ", reason)) %>%
  pull(line)
print(step1_exc_lines)

## 2: Screening exclusions (unchanged)
step2_df <- Flowchart_df %>%
  filter(!is.na(SCREEN_EXCL_REASON),
         SCREEN_EXCL_REASON != 77) %>%
  count(reason = SCREEN_EXCL_REASON_LABEL, name = "n")
step2_exc_lines <- step2_df %>%
  filter(n > 0) %>%
  arrange(desc(n)) %>%
  mutate(line = paste0(n, " ", reason)) %>%
  pull(line)
print(step2_exc_lines)

#Step 4 - if you have extra denominators, apply the create_excl function to make your list
## Post-enrollment analysis denominators (A–E)
## (These are “new exclusions after step 2”)
## Each step shows exclusions in a paper way.
#stepA_exc_lines <- create_excl(analysis_denom, "DENOM_A_EXCL_LABEL")


## Build flowchart: Steps 1–2 (same), then A–E as paper steps
flow_core <- Flowchart_df %>%
  as_fc(
    label         = "Women Interviewed (Prescreened)",
    text_fs       = 10,
    text_pattern  = "{label}\n{N}",
    text_color    = "black",
    bg_fill       = "white",
    border_color  = "black"
  ) %>%
  # Step 1: Prescreen
  fc_filter(
    PRESCREEN_PASS == "Yes",
    label            = "Met prescreening criteria",
    show_exc         = TRUE,
    text_fs_exc      = 9,
    text_fs          = 10,
    text_color       = "black",
    bg_fill          = "white",
    border_color     = "black",
    text_color_exc   = "black",
    bg_fill_exc      = "white",
    border_color_exc = "gray50",
    text_pattern     = "{label}\n n={n}",
    text_pattern_exc = "{label}\n n={n}",
    just_exc         = "left",
    offset_exc       = 0.08,
    label_exc        = paste(
      "Did not meet prescreening criteria:",
      paste(step1_exc_lines, collapse = "\n"),
      sep = "\n"
    )
  ) %>%
  # Step 2: Screening enrollment
  fc_filter(
    SCREEN_PASS == "Yes" &
    PRISMA_ENROLL == 1 &
    SCREEN_DENOM == 1,
    label = "Eligible women\nenrolled in PRISMA",
    show_exc         = TRUE,
    text_fs_exc      = 9,
    text_fs          = 10,
    text_color       = "black",
    bg_fill          = "white",
    border_color     = "black",
    text_color_exc   = "black",
    bg_fill_exc      = "white",
    border_color_exc = "gray50",
    text_pattern     = "{label}\n n={n}",
    text_pattern_exc = "{label}\n n={n}",
    just_exc         = "left",
    offset_exc       = 0.08,
    label_exc        = paste(
      "Did not meet screening criteria:",
      paste(step2_exc_lines, collapse = "\n"),
      sep = "\n"
    )
  ) %>%
#Example of adding new denominators included in your analysis
  # Step 3 (A): Enrolled + >/=1 visit after enrollment
  # fc_filter(
  #  DENOM_A_INCLUDED == 1,
  #  label = "Enrolled participants with >/=1 \nfollow-up visit \nafter enrollment",
  #  show_exc         = TRUE,
  # text_fs_exc      = 9,
  # text_fs          = 10,
  # text_color       = "black",
  # bg_fill          = "white",
  # border_color     = "black",
  # text_color_exc   = "black",
  # bg_fill_exc      = "white",
  # border_color_exc = "gray50",
  # text_pattern     = "{label}\n n={n}",
  # text_pattern_exc = "{label}\n n={n}",
  # just_exc         = "left",
  # offset_exc       = 0.08,
  # label_exc        = paste(
  #  "Excluded after enrollment:",
  # paste(stepA_exc_lines, collapse = "\n"),
  # sep = "\n"
  # )
  # ) %>%
  fc_draw()
# Export to PNG
fc_export(
  flow_core,
  filename = "prisma_flowchart.png",
  width = 6500,
  height = 10500,
  units = "px",
  res = 550
)
# Trim whitespace
flowchart_trimmed <- image_read("prisma_flowchart.png") %>%
  image_trim()
image_write(flowchart_trimmed, path = "flowchart_trimmed.png")
#For rmarkdown i use this format to make my very long flowchart fit
#{r flowchart2, out.width='55%', fig.align='center'}
#And I print this in the rmarkdown chunck
#knitr::include_graphics("flowchart_trimmed.png")

