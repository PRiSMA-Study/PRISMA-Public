# Fatigue analysis

## Description

This folder contains R code to visualize the answer distribution for each question in fatigue form and also the distribution of subscale score by site.

#### :pushpin: Updated on 2024-11-26
#### :pushpin: Originally drafted by: Savannah O'Malley (savannah.omalley@gwu.edu)

## What data is required:
* MNH26: Maternal Fatigue Questionnaire (Modified FACIT)
* MNH04: PNC clinical care
* MNH09: Labor and Delivery
* MNH19: Hospitalization
* MAT_ENROLL: prepared data set of enrolled mothers
* MAT_ENDPOINTS: prepared data set including all pregnancy endpoints
* MAT_HEMORRHAGE: prepared data set with hemorrhage data 
* MAT_LABOR: prepared dat set which includes C-section data
* MAT_DEMOGRAPHICS: demographic information for enrolled participants
* IDs of test-retest participants and test date, from each site

## Codes included:

**`Data-Prep.do`** read original data, generate new variables and make necessary data transformation. 

**`Test-Retest-Prep.do`** Uses IDs of test-retest participants by site to assemble the test-retest data set

**`Fatigue-Analysis.do`** Conducts the following analyses:

  + Demographics table

  + Internal reliability (cronbach's alpha)

  + Test-retest reliability (intraclass correlation)

**`Fatigue-Figures.R`** Creates figures for presentation and publication

  + Histograms of distibution by site
  + Fatigue by visit type
  + Fatigue over hemoglobin
  + Fatigue by parity
  + Fatigue and depression
