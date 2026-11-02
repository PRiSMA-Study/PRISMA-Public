# MAT_KIDNEY_BASELINE Variable Construction

## Overview

This program creates the PRISMA MNH baseline kidney function dataset (`MAT_KIDNEY_BASELINE`). Using laboratory results from MNH08 and maternal baseline data, it constructs measures of renal function, including serum creatinine, elevated creatinine, and estimated glomerular filtration rate (eGFR).

The final dataset contains the earliest valid creatinine measurement before 28 weeks' gestation for each pregnancy.

---

## Version History

### Original Version

**April 2025**
Erin Oakley

### Current Version

**October 2026**  
Emma K. Cook (emmacook@gwu.edu)

Updates:
- Assumed maintenance and ongoing updates of kidney function variable construction code.

---

## Required Inputs

Required datasets:

- MNH08
- MAT_BASELINE
- MAT_STUDYSTATUS

---

## Important Notes

- Only positive creatinine measurements are included.
- Serum creatinine is converted from μmol/L to mg/dL for eGFR calculation.
- Elevated creatinine is defined as **>77 μmol/L**, based on published pregnancy renal function literature.
- eGFR is calculated using the **2021 CKD-EPI creatinine equation**.
- Maternal age from `MAT_BASELINE` is incorporated into the eGFR calculation.
- When multiple creatinine measurements are available, the earliest valid measurement is selected.
- The final dataset is restricted to creatinine measurements obtained before **28 weeks' gestation**.
- Gestational age at specimen collection is derived using pregnancy start date information from `MAT_STUDYSTATUS`.

### References

- Elevated creatinine threshold: https://pmc.ncbi.nlm.nih.gov/articles/PMC6409397/
- CKD-EPI eGFR equation: https://www.kidney.org/ckd-epi-creatinine-equation-2021

---

## Variables Created

### Kidney Function Measures

- CREATININE
- CREATININE_mgdl
- CREATININE_ELEV
- eGFR
- eGFR_CAT

### Timing Variables

- CREATININE_GA

### Supporting Variables

- MAT_AGE

### Identifiers

- SITE
- MOMID
- PREGID
