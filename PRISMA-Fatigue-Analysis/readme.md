# Fatigue analysis

## Description

This folder contains R code to visualize the answer distribution for each question in fatigue form and also the distribution of subscale score by site.

#### :pushpin: Updated on 2023-10-26
#### :pushpin: Originally drafted by: Xiaoyan Hu (xyh@gwu.edu)

## What data is required:
* MNH26: Maternal Fatigue Questionnaire (Modified FACIT)

## Codes included:

**`data prep.R`** read original data, generate new variables and make necessary data transformation. 

**`Fatigue Form Review.Rmd`** outputs a pdf with the following figures/tables: 

**1\.** Part I. Within Site Report 
  + Variable list 
  + Answer distribution of all question by each visit for every site.
  
 **2\.** Part II. Between Site Report 
  + Answer distribution of each question at each visit by site.
    
**3\.** Part III. Distribution of fatigue subscale score by site at all time points.
   