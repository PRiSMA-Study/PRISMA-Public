# MAT_BASELINE Variable Construction

## Overview

This program creates the PRISMA MNH maternal baseline constructed dataset (`MAT_BASELINE`). It merges MNH forms MNH00, MNH01, MNH03, MNH04, MNH05, and MNH06, performs data cleaning and quality checks, and constructs standardized demographic, socioeconomic, behavioral, anthropometric, pregnancy, and obstetric history variables.

---

## Required Inputs

Required datasets:

- MAT_STUDYSTATUS
- MNH00: Pre-screening form
- MNH01: Ultrasound exam form
- MNH03: Sociodemographic form
- MNH04: Maternal ANC clinical status form
- MNH05: Maternal anthropometric form
- MNH06: Maternal point of care diagnostics form

**Notes:**
This program uses the user-written Stata command [`str2date`](https://github.com/PRiSMA-Study/PRISMA-Analysis-Emma/blob/309903b70359f479043027c22cba189f7e113ec5/Stata%20ado%20files/str2date.ado) to convert string dates to Stata date format.

Stacked data (mnh00-01, mnh03-06) was processed using user-written Stata command `dataimport` and saved locally in AWS. See [Stata ado files](https://github.com/PRiSMA-Study/PRISMA-Analysis-Emma/tree/309903b70359f479043027c22cba189f7e113ec5/Stata%20ado%20files).

---

## Important Notes

- Wealth quintiles are derived separately within each study site using PCA.
- Free-text responses for household head and occupation are manually reviewed and recoded.
- The program intentionally stops (`exit 9`) if previously unseen free-text responses are identified, requiring manual review and coding before rerunning.
- Maternal age is calculated from date of birth whenever available; estimated age is used otherwise.
- Records with maternal age below the site-specific enrollment minimum should be reviewed. Some sites (e.g., Kenya, Pakistan) permit enrollment of eligible emancipated minors.

---

## Version History

### Previous Versions (through 2026-05-01)

**MAT_DEMOGRAPHIC**  
Xiaoyan Hu and Precious Williams  
Source: [PRISMA Demographic Report (R)](https://github.com/PRiSMA-Study/PRISMA-Public/blob/main/PRISMA-Demographic-Report/1.%20Data%20prep.R)

**MAT_PREVPREG_COMPLICATIONS**  
Erin Oakley (November 2025)  
Source: [mat_PREGHISTORY.do](https://github.com/PRiSMA-Study/PRISMA-Analysis-Erin/blob/591060a34d3ab98583bd211d97634132caa33bf9/mat_PREGHISTORY.do)

**MAT_RISKS**  
Erin Oakley (August 2025)  
Source: [mat_RISKS.do](https://github.com/PRiSMA-Study/PRISMA-Analysis-Erin/blob/591060a34d3ab98583bd211d97634132caa33bf9/mat_RISKS.do)

### Current Version (2026-09-18 --> )
**October 2026**  
Emma K. Cook (emmacook@gwu.edu)

Updates:
- Consolidates MAT_DEMOGRAPHIC, MAT_PREVPREG_COMPLICATIONS, and MAT_RISKS.
- Adds repeat enrollment variables (`ENR_PREG_NUM`, `ENR_PREG_NUM_ORDER`).

