## Description

This folder contains all codes used to generate the maternal outcomes report. 

✏️ Authors: Savannah O'Malley (savannah.omalley@gwu.edu), Erin Oakley (emoakley@email.gwu.edu), and Stacie Loisate (stacie.loisate@gwu.edu)
## Codes to generate outcomes:

**`MAT_INFECTION.R`** Includes infection prevalence at enrollment and incident infections following enrollment for STIs, malaria, Hep E, Zika, Chikungunya, Dengue, Leptospirosis, etc. 

**`MAT_DEPR.do`** Includes continuous scores for depression scores (according to EPDS) at Enrollment/ANC20, ANC32/ANC36, and PNC6. Also includes binary variables yes/no screening for depression at each time point. 

**`MAT_THYR.do`** Maternal thyroid status at enrollment and ANC32.

**`MAT_NEAR_MISS.do`** Indicators of maternal near miss and potentially life-threatening conditions. Includes placenta accrete, placenta abruption, uterin rupture, endometritis, abortion complications, postpartum hemorrhage, ICU, prolonged labor, obstructed labor, malaria, tuberculosis, HIV, organ dysfunction/failure, severe anemia, transfusion, pulmonary edema, hospitalization, hysterectomy, laparotomy, high blood pressure, HDP classification, preeclampsia, severe preeclampsia, ever PLTC, ever near miss.
- *Note: run MAT_NEAR_MISS_1.do before running MAT_NEAR_MISS_2.do*

**`MAT_PLACENTA_PREVIA.do`** Placenta/vasa previa, number of fetuses, cesarean section, placenta accrete, placenta abruption

**`MAT_ENDPOINTS.do`** Pregnancy endpoint, pregnancy end date, gestational age at pregnancy endpoint, closeout, maternal death and timing

**`MAT_MORTALITY.do`** Maternal death, maternal mortality, deaths occurring after 42 days postpartum

**`mat_outcomes_anemia_v1.1.do`** This code reviews all hemoglobin data to construct anemia outcomes at various time points including: all of pregnancy; trimesters 1-3; specific ANC visit windows; specific postpartum visit windows. This code also applies Hb adjustments for smoking and elevation.

**`mat_outcomes_GDM_v1.1.do`** This code constructs indicators for overt diabetes (based on baseline/early pregnancy HbA1c and/or previous diagnosis) and gestational diabetes (based on OGTT).

**`mat_HDP.do`** This code constructs outcomes surrounding hypertensive disorders of pregnancy, including Chronic Hypertension; Gestational Hypertension; Preeclampsia; Preeclampsia Superimposed on Chronic HTN; and Preeclampsia with Severe Features.

**`mat_outcomes_PREGEND.do`** This code constructs maternal outcomes at pregnancy endpoint including: preterm delivery; delivery classification (provider-initiated vs. spontaneous); prolonged and obstructed labor; uterine rupture; cesarean delivery (maternal-level variable); PROM and PPROM; induced labor; along with related variables. This code outputs three datasets: MAT_LABOR; MAT_PRETERM; and MAT_UTERINERUP.

**`mat_HDP_POSTPARTUM.do`** This code constructions postpartum hypertension variables including: 1) high blood pressure immediately post-delivery up to 2 days postpartum; 2) postpartum visit windows (PNC-0, PNC-1, PNC-4, PNC-6); and long-term follow-up visit windows (PNC-26, PNC-52)

**`MAT_HEMORRHAGE.R`** This code constructs variables relating to antepartum and postpartum hemorrhage along with medications and procedures to prevent and/or treat hemorrhage. 


## Code to generate report:

**`Maternal-Constructed-Outcomes-Report.Rmd`** Using the maternal outcomes files, R code for generating a report of maternal constructed outcomes including anemia, depression, infection, hypertensive disorders, and near miss.


