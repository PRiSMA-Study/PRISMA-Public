
# PRISMA Infant Constructed Variables

## Description

This folder two coding files that generate PRISMA infant outcomes and an
accompanying report. This file is an ongoing document that will be
updated as more outcomes are coded and generated. More information on
the outcomes coded here can be found in the PRISMA protocol.

#### :pushpin: Updated on 23 August 2024

#### :pushpin: Originally drafted by:

- R code: Stacie (<stacie.loisate@gwu.edu>)

## Codes included:

**`Infant-Constructed-Variables.R`** reads in the data and generates the
constructed variables.

**`Infant-Constructed-Outcomes-Tables.Rmd`** generates a report with key
metrics for each of the constructed variables generated in the previous
code.

## Outcomes included in this version:

- Low birth-weight
- Pre-term birth
- Size for Gestational Age (SGA)
- Neonatal Mortality
- Infant Mortality
- Stillbirth
- Fetal Loss
- Perinatal Birth Asphyxia
- Hyperbilirubinemia
- PSBI
- Infant growth trajectories (WAZ, WFL, LAZ, HCZ, WFL) 


## What data is required:

- MNH01: Ultrasound Exam
- MNH02: Enrollment Status
- MNH04: ANC Clinical Status
- MNH09: Maternal Labor and Delivery Outcome
- MNH11: Newborn Birth Outcome
- MNH13: Infant PNC Clinical Status
- MNH14: Infant Point-of-Care Diagnostics
- MNH15: Infant Vaccination Status
- MNH24: Infant Closeout
