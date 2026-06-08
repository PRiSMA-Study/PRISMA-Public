# Maternal Outcome: Maternal Near-Miss

## Description:
This folder contains Stata code to generate analytical variables for PRISMA-defined Maternal Near-Miss. 

## Definition:
A woman who has experienced a life-threatening health situation and survived the experience.
Time frame: During pregnancy up to 42 days postpartum.
*Note: The definition of near-miss has been adapted from the WHO maternal near-miss definition.*  

#### :pushpin: Originally drafted by:
Stata code: Savannah F. O'Malley (savannah.omalley@gwu.edu)

## Codes include:

`01_nearmiss_vars.do` Constructs indicators of potentially life-threatening conditions (PLTC). *Run this code first* 

*Note: this file requires the following constructed files: MAT_ENROLL, MAT_ENDPOINTS, MAT_UTERINERUP, MAT_HEMORRHAGE, MAT_INFECTION, MAT_LABOR, MAT_ANEMIA*

`02_nearmiss_outcomes.do` Constructs final indicator variables. *This file additionally requires the HDP.dta. 

