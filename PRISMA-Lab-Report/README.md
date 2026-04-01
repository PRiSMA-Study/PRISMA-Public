# Lab-Report

## Description

Codes includes two parts:

**`Lab Missingness:`** Report the missingness of lab forms and lab results for each visit window. This is the lab missingness report we run and track monthly.

**`Lab Distribution:`** Visualize the distribution of lab results both during and after pregnancy, and compare the differences in lab results across all sites. This includes code for scatter plots, line plots, bar plots, and histograms. While we’re not running this part for now, I’ve included the code for future use.

- Link to meta-data spreadsheet [link](https://docs.google.com/spreadsheets/d/1eh6slRp5IHqcYT5FxUp5ArFbLpB4LpXyRBMuhPmJGHE/edit?gid=1177252973#gid=1177252973)
- Link to lab report update tracker [link](https://docs.google.com/spreadsheets/d/1gWHqU7LFt9kpyz6ebqvfDaFUl3OsT1rRsCw6J0YlzkE/edit?gid=1313345622#gid=1313345622)

#### :pushpin: Updated on 2026-03-31
#### :pushpin: Originally drafted by: Xiaoyan Hu (xyh@gwu.edu)

## File structure

**`1.Lab-Missingness-Data-Prep.R`**  
1. Data preparation for lab missingness: Load stacked and outcome data, merge data, derive denominators for missingness tables.
2. Prepare data for plots: This section can be omitted if we're only generating missingness tables. It's meant for the scatter plots, line plots, bar plots, and histograms I created earlier in the lab report.

**`2.Lab-Missingness-Report.RMD`** Generate the lab missingness report.

**`3.Lab-Distribution.RMD`** Codes for scatter plots, line plots, bar plots and histograms.

**`Lab report variables.xlsx`** The Excel file containing all the lab test variables will be included in the missingness report. It should be updated whenever the lab variables in the data dictionary are modified.


