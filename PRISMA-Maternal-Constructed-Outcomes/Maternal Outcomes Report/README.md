# PRISMA MNH Maternal Outcomes Report

## Description

This repository contains the code used to generate the **PRISMA MNH Maternal Outcomes Report**.

## Troubleshooting

### `row_group_order()` Error

A June 2026 update to the **gt** package introduced changes that caused errors in `row_group_order()`.

**Solution:** Add an `id` argument to `tab_row_group()`.

### LaTeX / TinyTeX Errors

If report generation fails due to missing LaTeX dependencies, install or update **TinyTeX**:

```r
tinytex::install_tinytex(repository = "ctan")
```
If specific LaTeX packages are missing, install them with:

```r
tinytex::tlmgr_install(c("multirow", "wrapfig"))
```
