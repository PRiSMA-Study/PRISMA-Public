# PRISMA MNH Maternal Outcomes Report

## Description

This repository contains the code used to generate the **PRISMA MNH Maternal Outcomes Report**.

### Required packages

This code requires version 3.2.1 of `tibble`. 

**Check package version:**
```r 
packageVersion("tibble")
```
**Install correct version if needed:**
```r 
remotes::install_version("tibble", version = "3.2.1")
```

### Inputs
- `MAT_MORTALITY`
- `MAT_HEMORRHAGE`
- `MAT_DEPR`
- `MAT_ANEMIA`
- `MAT_PREGEND`
- `MAT_GDM`
- `MAT_INFECTION`
- `MAT_NEAR_MISS`
- `MAT_THYR`
- `MAT_STUDYSTATUS`
- `MAT_HDP`
- `MAT_POSTPARTUM_HIGHBP`
- `MAT_PLACENTA_PREVIA`
- `MAT_HIGHHB`
- `MAT_SEPSIS`

## Troubleshooting

### `row_group_order()` Error

An update to the **gt** package introduced changes that caused errors in `row_group_order()`.

**Solution:** Add an `id` argument to `tab_row_group()`. Updated June 2026.

### LaTeX / TinyTeX Errors

If report generation fails due to missing LaTeX dependencies, install or update **TinyTeX**:

```r
tinytex::install_tinytex(repository = "ctan")
```
If specific LaTeX packages are missing, install them with:

```r
tinytex::tlmgr_install(c("multirow", "wrapfig"))
```

## Updates

Sepsis added October 28, 2026 (for Q3 data lock [2026-09-18] reports)
