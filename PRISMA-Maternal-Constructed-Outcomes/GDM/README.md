# MAT_GDM Variable Construction

## Overview

This program creates the PRISMA maternal gestational diabetes dataset (`MAT_GDM`). It uses laboratory results, antenatal history, hospitalization records, and labor and delivery data to identify overt diabetes, gestational diabetes mellitus (GDM), clinical diagnoses of diabetes, and diabetes treatment during pregnancy.

The program generates both laboratory-based and clinically reported indicators of diabetes and creates analytic denominator and missingness variables for maternal outcomes reporting.

---

## Version History

### Original Version

**April 2024**
Erin Oakley

### Current Version

**October 2026**  
Emma K. Cook (emmacook@gwu.edu)

Major updates include:

- HbA1c threshold corrected to ≥6.5%.
- Revised denominator definitions based on ERS and CRS feedback.
- Exclusion of India-SAS from the 1-hour OGTT assessment.
- Addition of reported pre-existing diabetes diagnosis variables.
- Addition of diabetes treatment variables.
- Addition of alternative diagnostic criteria (WHO, Canadian, ACOG, DIPSI).
- Standardized OGTT selection algorithm using the most recent valid test.
- Inclusion of gestational age and date variables for all laboratory measures.
- Updates for revised MAT_STUDYSTATUS structure.

---

## Required Inputs

Required datasets:

- MNH04
- MNH08
- MNH09
- MNH19
- MAT_STUDYSTATUS

---

## Important Notes

### Overt Diabetes

Overt diabetes is identified using:

#### Laboratory Definition

- HbA1c ≥ 6.5%
- Measured at enrollment or before 20 weeks gestation

#### Clinical History Definition

- Participant reports a prior diagnosis of chronic/pre-existing diabetes on the ANC form

#### Combined Definition

`DIAB_OVERT_ANY` is positive if either:

- HbA1c criteria are met, or
- A prior diagnosis of chronic diabetes is reported

---

### Gestational Diabetes Mellitus (GDM)

Gestational diabetes is identified using the 75g Oral Glucose Tolerance Test (OGTT).

#### Primary PRISMA Definition

Positive if any of the following are met:

- Fasting glucose ≥ 5.1 mmol/L
- 1-hour glucose ≥ 10.0 mmol/L
- 2-hour glucose ≥ 8.5 mmol/L

Women with overt diabetes are excluded from GDM classification.

---

### Alternative Diagnostic Criteria

Additional GDM classifications are generated using:

#### WHO 1999

- Fasting glucose ≥ 7.0 mmol/L
- 2-hour glucose ≥ 7.8 mmol/L

#### Canadian Diabetes Association

- Fasting glucose ≥ 5.3 mmol/L
- 1-hour glucose ≥ 10.6 mmol/L
- 2-hour glucose ≥ 8.9 mmol/L

Requires at least two abnormal values.

#### DIPSI

- 2-hour glucose ≥ 7.8 mmol/L

#### ACOG

- Fasting glucose ≥ 5.3 mmol/L

---

### OGTT Selection

When multiple OGTT records are available:

- Postnatal OGTTs are excluded.
- Invalid tests are removed.
- Exact duplicate results retain a single observation.
- If multiple valid non-duplicate tests exist, the most recent valid test is selected.

---

### Clinical Diagnosis of Gestational Diabetes

Clinical gestational diabetes diagnoses are identified from:

- Labor and delivery records (MNH09)
- Hospitalization records (MNH19)

A combined diagnosis variable is created indicating any documented diagnosis during pregnancy.

---

### Diabetes Treatment

Treatment reported at labor and delivery includes:

- Insulin
- Metformin
- Glyburide

An additional variable identifies receipt of any diabetes treatment.

---

### Denominator Definition

Women are included in the gestational diabetes denominator if:

- Enrolled in the study,
- Have not been classified with overt diabetes,
- Remained pregnant beyond 28 weeks gestation or had a valid OGTT result,
- Were not closed out before the ANC-28 assessment window.
