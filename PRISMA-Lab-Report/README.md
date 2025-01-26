# Lab-Report

## Description

Codes includes two parts:

**`Lab Missingness:`** Report the missingness of lab forms and lab results for each visit window. This is the lab missingness report we run and track monthly.

**`Lab Distribution:`** Visualize the distribution of lab results both during and after pregnancy, and compare the differences in lab results across all sites. This includes code for scatter plots, line plots, bar plots, and histograms. While we’re not running this part for now, I’ve included the code for future use.

#### :pushpin: Updated on 2025-01-26
#### :pushpin: Originally drafted by: Xiaoyan Hu (xyh@gwu.edu)

## File structure

**`1.data_prep.R`**  
1. Data preparation for lab missingness: Load stacked and outcome data, merge data, derive denominators for missingness tables.
2. Prepare data for plots: This section can be omitted if we're only generating missingness tables. It's meant for the scatter plots, line plots, bar plots, and histograms I created earlier in the lab report.

**`2.Lab-Report.RMD`** Generate the lab missingness report.

**`3.Lab-Distribution.RMD`** Codes for scatter plots, line plots, bar plots and histograms.

**`Lab report variables.xlsx`** The Excel file containing all the lab test variables will be included in the missingness report. It should be updated whenever the lab variables in the data dictionary are modified.


