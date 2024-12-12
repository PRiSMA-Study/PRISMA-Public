# Maternal Outcomes: Preterm-Indication-PPROM

## Description
This folder contains two Stata files that generate selected PRISMA maternal outcomes (Preterm Indication and PPROM). More information on the outcomes coded here can be found in the PRISMA protocol.

#### :pushpin: Updated on April 2, 2024 

#### :pushpin: Originally drafted by:
- Stata code: Erin (emoakley@gwu.edu)

## Codes included:
**`mat_BOE_v1.1.do` uses MNH01 and MNH02 data to construct Best Obstetric Estimate of gestational age and Enrollment indicator. 

**`mat_outcomes_PREGEND_v1.1.do` uses the variables constructed above in combination with MNH09 to construct the outcomes and create an analysis dataset.

## Outcomes included in this version:

- PRETERM_ANY (preterm deliveries >=20 weeks & <37 weeks)
- PRETERM_SPON (spontaneous preterm deliveries >=20 weeks & <37 weeks)
- PRETERM_PROV (provider-initiated preterm delivery >=20 weeks & <37 weeks)
- PPROM_PREGEND (preterm premature rupture of membranes - PPROM)

## What data is required:

- MNH01: Ultrasound Exam
- MNH02: Enrollment Status
- MNH09: Maternal Labor and Delivery Outcome

## Additional instructions for PRISMA sites:

Troubleshooting the code:
We hope that you will be able to run the Stata code on your datasets, but we may need to make a few edits to the code to help it run smoothly depending on the structure of your data. First, within GW, we import the data as CSV files. You may need to update the code around data import if you keep the data as a different file type.

Second, within GW, the variables are named with the form number at the beginning of all variable names (except for ID numbers). For example, in form MNH01, the variable for ultrasound date “us_ohostdat” is called “m01_us_ohostdat”.

If needed, you can rename variables to match the Stata code using the line of code below (replace “XX” with the number form you are using):

*rename * mXX_**

Then, you may need to correct the names of identifier variables to be able to merge between forms: 

*rename mXX_momid momid* 

*rename mXX_pregid pregid*

*rename mXX_scrnid scrnid*

