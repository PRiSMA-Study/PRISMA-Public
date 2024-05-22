
# PRISMA Maternal Infection Variables

## Description

This folder two coding files that generate PRISMA maternal infection outcomes and an accompanying report. This file is an ongoing document that will be
updated as more outcomes are coded and generated. More information on
the outcomes coded here can be found in the PRISMA protocol.

#### :pushpin: Updated on 22 May 2024

#### :pushpin: Originally drafted by:

- R code: Stacie (<stacie.loisate@gwu.edu>)

## Codes included:

**`Maternal-Infection-Variables.R`** reads in the data and generates the
constructed variables.

**`Maternal-Infection-Tables.Rmd`** generates a report with key
metrics for each of the constructed variables generated in the previous
code.

## Outcomes included in this version:
- STIs
  + HIV
  + Syphilis
  + Gonorrhea
  + Chlamydia
  + Genital Ulcers
  + Other STIs
  
- Other infections
  + Malaria
  + Hep B
  + Hep C
  + Covid
  + TB

## What data is required:

- MNH02: Enrollment Status
- MNH04: ANC Clinical Status
- MNH06: Maternal Point of Care Diagnostics
- MNH08: Maternal Lab Results 

