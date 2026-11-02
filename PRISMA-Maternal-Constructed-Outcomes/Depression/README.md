# MAT_DEPR Variable Construction

## Overview

This program creates the PRISMA maternal depression dataset (`MAT_DEPR`) using data collected on the Maternal Depression Screening Form (MNH25). Depression symptoms are assessed using the **Edinburgh Postnatal Depression Scale (EPDS)**, with site-specific scoring algorithms and validated cutoffs.

The program generates longitudinal depression measures, participant-level summary variables, and outcome variables for maternal outcomes reporting.

---

## Version History

### Original Version

Savannah O'Malley (savannah.omalley@gwu.edu)

### Current Version

**October 2026**  
Emma K. Cook (emmacook@gwu.edu)

Updates:
- Updated for Q3 data lock restructuring.
- Modified to use `MAT_STUDYSTATUS`

---

## Required Inputs
- MNH25
- MAT_STUDYSTATUS

---

## Important Notes

### Depression Assessment

Maternal depression is measured using the **Edinburgh Postnatal Depression Scale (EPDS)**.

- The EPDS consists of 10 questions.
- Responses are recoded to generate standardized symptom scores.
- Total EPDS scores range from 0 to 30.
- Higher scores indicate more severe depressive symptoms.

### Site-Specific Scoring

Because questionnaire administration and response structures differed across sites:

- Kenya uses a unique scoring structure.
- Ghana requires separate recoding logic due to differences in questionnaire wording and response options.
- India-CMC administration changed from self-administered to staff-administered on December 8, 2023.
- Site-specific depression cutoffs are applied when generating `DEPR_SITE`.

### Depression Classifications

Two depression indicators are generated:

#### Standard Cutoff

- Positive depression screen: EPDS score ≥11

#### Site-Specific Cutoffs

- Ghana: ≥11
- India-CMC: ≥8
- India-SAS: ≥10
- Kenya: ≥13
- Pakistan: ≥14
- Zambia: ≥10

### Self-Harm Risk

Participants are classified as being at elevated self-harm risk based on responses to EPDS Question 10.

### Visit Windows

Depression measures are summarized across:

- ANC20 / Enrollment period
- First and second trimester combined
- ANC32 / ANC36 period
- Third trimester
- PNC6 postpartum window

### Longitudinal Summary Measures

The program identifies:

- Worst (highest) depression score observed
- Worst antenatal depression score
- First date meeting depression screening criteria
- First date meeting site-specific depression criteria
