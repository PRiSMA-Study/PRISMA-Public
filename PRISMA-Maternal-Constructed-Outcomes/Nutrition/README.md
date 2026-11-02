# MAT_NUTR

Maternal nutrition variable construction code for the PRISMA study.

This folder contains two scripts used to create maternal micronutrient, inflammation, hematologic, and iron deficiency indicators. Iron biomarkers are adjusted using the BRINDA regression-correction approach to account for inflammation and malaria.

---

## Workflow

```text
BBRINDA_iron_adjustment.do
Inputs:  mnh06.dta, mnh08.dta
Output:  mnh08_adj_brinda_malaria.dta

MAT_NUTR.do
Inputs:  mnh07.dta, mnh08.dta, MAT_STUDYSTATUS.dta,
         MAT_BASELINE.dta, mnh08_BRINDA_adj_iron.dta
Outputs: MAT_NUTR_LONG.dta, MAT_NUTR.dta

```

---

## Scripts

### BRINDA_iron_adjustment.do

Creates BRINDA-adjusted ferritin and soluble transferrin receptor (sTfR) values. Originally written by Nazia Binte Ali in R (September 2026). Translated to Stata by Emma Cook (October 2026). 

**Inputs**
- `mnh06.dta`
- `mnh08.dta`

**Key steps**
- Cleans ferritin, CRP, AGP, and sTfR results
- Carries forward malaria tests occurring within 30 days before specimen collection
- Adjusts ferritin for CRP, AGP, and malaria
- Adjusts sTfR for AGP and malaria

**Output**
- `mnh08_BRINDA_adj_iron.dta`

Key variables:
- `ferritin_adj_malaria`
- `stfr_adj_malaria`

---

### MAT_NUTR.do

Constructs maternal nutrition indicators across pregnancy and postpartum visits.

**Inputs**
- `mnh07.dta`
- `mnh08.dta`
- `MAT_STUDYSTATUS.dta`
- `MAT_BASELINE.dta`
- `mnh08_BRINDA_adj_iron.dta`

**Key outputs**
- `MAT_NUTR_LONG.dta` (visit-level)
- `MAT_NUTR.dta` (participant-level)

---

## Indicators Created

### Iron Status
- `FERRITIN_ADJ_15` (<15 µg/L)
- `STFR_ADJ` (>8.3 mg/L)

### Vitamin B12
- `VITB12_COB`
- `VITB12_HOL`

### Folate
- `FOL_SERUM`
- `FOL_RBC`
- `FOL_ANY`

### Iodine Status
- `HIGH_TG`

### Vitamin A Status
- `RBP4`

### Inflammation
- `CRP`
- `AGP`
- `INFLAMMATION`

### Hematologic Measures
- `MCV`
- `HB11`
- `HB10`

---

## Timepoints

Variables are generated for:

- Trimesters: `T1`, `T2`, `T3`
- Visit windows: `ANC20`, `ANC32`, `PNC6`
- Continuous measures: `*_CONT_*`
- Denominators: `*_DENOM`
