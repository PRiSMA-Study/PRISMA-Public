# Maternal Outcome: Perinatal Depression

## Description:
This folder contains Stata code to generate analytical variables for possible Perinatal Depression. More information on perinatal depression can be found in the PRISMA protocol.

#### :pushpin: Originally drafted by:
Stata code: Savannah F. O'Malley (savannah.omalley@gwu.edu)

##Codes include:
**`MNH25.do` uses MNH25 form to construct perinatal depression
This do file will create three data files:
1. Long data set (per participant - observation) --> useful for monitoring; includes data for non-scheduled visits
2. Collapsed data set (one row per participant) --> useful for report; ONLY includes data for three visit types (Enrollment/ANC20, ANC32/ANC36, and PNC6)
3. Collapsed data set with only the variables needed for Maternal Outcomes Report

### Notes:
*Note that the collapsed data sets includes the highest depression score for each visit type; if a woman has multiple forms completed for the same visit type, only the highest score will be kept*
*Note that every site will not need the entire code; some code is specific to a site due to differences in the way that the tool has been translated and administered*

##Outcomes:
1. Screening for possible depression at 3 timepoints using a standard cutoff (11+)
2. Screening for possible depression at 3 timepoints using site-specific cutoffs
3. Depression summary score at 3 timepoints
Outcomes are provided in a collapsed data set (one row per participant)

#### Depression at ANC-20 : 
1. DEPR_ANC20_STND
2. DEPR_ANC20_SITE
3. DEPR_ANC20_SCORE 
#### Depression at ANC-32 : 
1. DEPR_ANC32_STND
2. DEPR_ANC32_SITE
3. DEPR_ANC32_SCORE 
#### Depression at PNC-6  : 
1. DEPR_PNC6_STND
2. DEPR_PNC6_SITE
3. DEPR_PNC6_SCORE 
