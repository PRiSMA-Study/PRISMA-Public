# PRISMA Mortality

This folder contains R code for processing and harmonizing maternal and infant mortality and cause-of-death (CoD) data across PRISMA study sites. 
The workflow identifies reported deaths, combines information from study forms and constructed outcomes, reviews verbal autopsy (VA) and CoD completeness, and constructs maternal mortality outcomes.


## Workflow

1. **Identify maternal and infant deaths:** Review death indicators and dates across study forms and constructed outcomes, standardize dates, and identify supporting source forms.
2. **Harmonize cause-of-death data:** Process site-specific MNH37 submissions, standardize maternal and infant CoD codes and descriptions, and combine site datasets.
3. **Review form completeness:** Link death records to CoD and VA information, identify missing or incomplete forms, and distinguish permanently missing VA forms from forms that are due or not yet due.
4. **Generate site queries:** Create site-specific Excel workbooks flagging missing death forms, missing CoD information or dates, and VA-related discrepancies.
5. **Construct maternal mortality outcomes:** Classify maternal and pregnancy-related deaths, including outcomes within 42 days postpartum and late outcomes through 365 days postpartum, and retain labeled variables for analysis.

## Outputs

| Output | Description |
|---|---|
| `INF_COD.csv` | Harmonized infant death and cause-of-death dataset |
| `MAT_COD.csv` | Harmonized maternal death and cause-of-death dataset |
| `<SITE>_<UploadDate>_Mortality_Issues.xlsx` | Site-specific mortality data queries and form-completeness checks |
| `MAT_MORTALITY.csv` | Constructed maternal mortality outcomes |
| `MAT_MORTALITY.dta` | Constructed maternal mortality outcomes in Stata format, with labels |

The script saves the CoD and maternal mortality datasets to TnTDrive constructed-data directory. 
Site query workbooks are saved to the configured output directory.

## Running the script

Before running `Mortality.R`, update `UploadDate`, confirm the site-specific dates in `site_upload_date_map`, and verify the input and output paths. 
The script expects the required merged data, constructed datasets, MNH37 submissions, and permanently missing forms workbook to be available in the configured locations.
