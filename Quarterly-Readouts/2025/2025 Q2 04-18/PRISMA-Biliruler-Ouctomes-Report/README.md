## Description

This folder contains all codes used to generate the bili-ruler outcomes report. 

✏️ Author: Alyssa Shapiro (alyssa.shapiro@gwu.edu)

## Codes to generate outcomes:

**`Jaundice Dataset Setup.R`** Creates long and wide INFANT datasets that include MNH11, MNH13, MNH14, and MNH36 (Bili-ruler). Main variables: 
- BILI_FINAL: The one number output that is the average of the two readings from two users, 1-6
- MST_FINAL: Same but for Monk Skin Tone, 1-10
- Visual Inspection result (JAUNDATVISIT, either 0 for no jaundice, 1 for jaundice, and 2 for severe jaundice)
- TCB

## Code to generate report:

**`PRISMA Biliruler Substudy Q2 Report v5.Rmd`** Outputs the PDF file for Bili-ruler Substudy, including: 
- Distribution of Bili-ruler and MST
- Bili-ruler vs TCB
- Matches between 2 staff using Bili-ruler
- Tables Se/Sp of Bili-ruler cutoffs vs TCB cutoffs, ROC and AUC curves


