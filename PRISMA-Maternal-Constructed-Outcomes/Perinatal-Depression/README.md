# Maternal Outcome: Perinatal Depression

## Description:
This folder contains Stata code to generate analytical variables for possible Perinatal Depression. More information on perinatal depression can be found in the PRISMA protocol.

#### :pushpin: Originally drafted by:
Stata code: Savannah F. O'Malley (savannah.omalley@gwu.edu)
Grateful for input from Xiaoyan Hu


## :pushpin: Code #1:
**`01_depression_cleaning.do` uses depression [MNH25] data to construct perinatal depression

*Note: this code requires 4 data sets:* 
1. 'mnh25_merged.csv' which contains raw depression data 
2. 'MAT_ENROLL.dta' which is required to calculate visit windows [part 3 of the code]
3. 'MAT_ENDPOINTS.dta' which provides variables for pregnancy end date and closeout date [part 3 of the code]
4. 'Expected_obs.dta' which provides denominators of women expected for each time point
*Note that data sets #2 - 4 are only for generating denominators for reports [part 3 of the code]; individual sites do not need data sets #2 -4 to calculate depression summary scores*

This do file will create three data files:
1. Long data set (per participant - observation) --> useful for monitoring; includes data for non-scheduled visits. This data set is used in codes #2 and #3 below
2. Collapsed data set (one row per participant) --> useful for report; ONLY includes data for three visit types (Enrollment/ANC20, ANC32/ANC36, and PNC6)
3. Collapsed data set with only the variables needed for Maternal Outcomes Report

### Notes:
*Note that the collapsed data sets includes the highest depression score for each visit type; if a woman has multiple forms completed for the same visit type, only the highest (worst) score will be kept.*

*Note that every site will not need the entire code; some code is specific to a site due to differences in the way that the tool has been translated and administered*

### Outcomes:
1. Screening for possible depression at 3 timepoints using a standard cutoff (11+)
2. Screening for possible depression at 3 timepoints using site-specific cutoffs
3. Depression summary score at 3 timepoints

Outcomes are provided in a collapsed data set (one row per participant)

#### Depression at ANC-20 : 
1. DEPR_STND_ANC20
2. DEPR_SITE_ANC20
3. DEPR_SCORE_ANC20 
#### Depression at ANC-32 : 
1. DEPR_STND_ANC32
2. DEPR_SITE_ANC32
3. DEPR_SCORE_ANC32 
#### Depression at PNC-6  : 
1. DEPR_STND_PNC6
2. DEPR_SITE_PNC6
3. DEPR_SCORE_PNC6 


## :pushpin: Code #2:
**`02_depression_covariates.do` uses the long constructed file and merges with maternal demographics file (MAT_DEMOGRAPHICS.csv), and applies pretty labels for tables and figures. Generates the file: 'mnh25_long_covariates.dta'


## :pushpin: Code #3:
**`03_depression_report.RMD` uses 'mnh25_long_covariates.dta' to generate pretty tables and figures in R Markdown
