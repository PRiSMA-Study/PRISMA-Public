
# PRISMA Maternal Hemorrhage Outcome

## Description

This folder two coding files that generate PRISMA maternal hemorrhage outcomes and an accompanying report. This file is an ongoing document that will be
updated as more outcomes are coded and generated. More information on
the outcomes coded here can be found in the PRISMA protocol.

#### :pushpin: Updated on 22 May 2024

#### :pushpin: Originally drafted by:

- R code: Stacie (<stacie.loisate@gwu.edu>)

## Codes included:

**`Maternal-Hemorrhage.R`** reads in the data and generates the
constructed variables.

## Outcomes included in this version:
- Antepartum Hemorrhage
- Postpartum Hemorrhage
- Severe postpartum Hemorrhage

## Definitions/logic
- Antepartum hemorrhage required variables & logic
  + Current clinical status: antepartum hemorrhage? [MNH04, APH_CEOCCUR_1-5==1]
  + Current clinical status: antepartum hemorrhage @ at any unscheduled visit [Constructed, APH_UNSCHED_ANY==1]
  + Did the mother experience antepartum hemorrhage? [MNH09, APH_CEOCCUR_6==1]
  + Specify type of labor/delivery or birth complication: APH or PPH or vaginal bleeding (MNH19/Constructed, HEM_HOSP_ANY==1 & TIMING_OHOCAT==1)

- Postpartum hemorrhage required variables & logic
  + Did mother experience postpartum hemorrhage? [MNH09, PPH_CEOCCUR==1]
  + Record estimated blood loss [MNH09, PPH_ESTIMATE_FAORRES>=500]
  + Procedures carried out for PPH, Balloon/condom tamponade [MNH09, PPH_FAORRES_1==1]
  + Procedures carried out for PPH, Surgical interventions [MNH09, PPH_FAORRES_2==1]
  + Procedures carried out for PPH, Brace sutures [MNH09, PPH_FAORRES_3==1]
  + Procedures carried out for PPH, Vessel ligation [MNH09, PPH_FAORRES_4==1]
  + Procedures carried out for PPH, Hysterectomy [MNH09, PPH_FAORRES_5==1]
  + Procedures carried out for PPH, Other [MNH09, PPH_FAORRES_88==1]
  + Did the mother need a transfusion? [MNH09, PPH_TRNSFSN_PROCCUR==1]
  + Specify type of labor/delivery or birth complication: APH or PPH or vaginal bleeding (MNH19/Constructed, HEM_HOSP_ANY==1 & TIMING_OHOCAT==2)
  + Was the mother diagnosed with any of the following birth complications, PPH? [MNH12, BIRTH_COMPL_MHTERM_1==1]

- Severe postpartum hemorrhage required variables & logic
  + Did mother experience postpartum hemorrhage? [MNH09, PPH_CEOCCUR==1]
  + Record estimated blood loss [MNH09, PPH_ESTIMATE_FAORRES>=1000]
  + Procedures carried out for PPH, Balloon/condom tamponade [MNH09, PPH_FAORRES_1==1]
  + Procedures carried out for PPH, Surgical interventions [MNH09, PPH_FAORRES_2==1]
  + Procedures carried out for PPH, Brace sutures [MNH09, PPH_FAORRES_3==1]
  + Procedures carried out for PPH, Vessel ligation [MNH09, PPH_FAORRES_4==1]
  + Procedures carried out for PPH, Hysterectomy [MNH09, PPH_FAORRES_5==1]
  + Procedures carried out for PPH, Other [MNH09, PPH_FAORRES_88==1]
  + Did the mother need a transfusion? [MNH09, PPH_TRNSFSN_PROCCUR==1]
  + Specify type of labor/delivery or birth complication: APH or PPH or vaginal bleeding (MNH19/Constructed, HEM_HOSP_ANY==1 & TIMING_OHOCAT==2)
  + Was the mother diagnosed with any of the following birth complications, PPH? [MNH12, BIRTH_COMPL_MHTERM_1==1]

## What data is required:

- MNH02: Enrollment Status
- MNH04: ANC Clinical Status
- MNH09: Labor and Delivery
- MNH12: PNC Clinical Status
- MNH19: Maternal Hospitalization (optional) 

