# PRISMA MNH Maternal Nutrition Report

## Description

This repository contains the code used to generate the **PRISMA MNH Maternal Nutrition Report**.

## Files
- `Nutrition-Figures.do`: Generates figures for report
- `Maternal_Nutrition_Report.Rmd`: Generates report using R markdown

## Troubleshooting

### 1. `row_group_order()` Error

  An update to the **gt** package introduced changes that caused errors in `row_group_order()`.
  
  **Solution:** Add an `id` argument to `tab_row_group()`. Updated June 2026.

### 2. LaTeX / TinyTeX Errors

  If report generation fails due to missing LaTeX dependencies, install or update **TinyTeX**:
  
  ```r
  tinytex::install_tinytex(repository = "ctan")
  ```
  If specific LaTeX packages are missing, install them with:
  
  ```r
  tinytex::tlmgr_install(c("multirow", "wrapfig"))
  ```
