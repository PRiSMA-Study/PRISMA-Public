# MAT_PLACENTA_PREVIA Variable Construction

## Overview

This program creates the PRISMA MNH placenta previa dataset (`MAT_PLACENTA_PREVIA`). It identifies placenta previa using antenatal ultrasound findings (MNH01) and labor and delivery records (MNH09), including cesarean delivery indications and causes of antepartum hemorrhage.

The program also constructs related placental complication variables, including placenta accreta and placental abruption.

---

## Required Inputs

Required datasets:

- MNH01
- MNH02
- MNH09
- MAT_STUDYSTATUS
- MAT_PREGEND

**Notes:**

This program uses the user-written Stata command [`str2date`](https://github.com/PRiSMA-Study/PRISMA-Analysis-Emma/blob/309903b70359f479043027c22cba189f7e113ec5/Stata%20ado%20files/str2date.ado) to convert string dates to Stata date format.

Stacked data (mnh00-01, mnh03-06) was processed using user-written Stata command `dataimport` and saved locally in AWS. See [Stata ado files](https://github.com/PRiSMA-Study/PRISMA-Analysis-Emma/tree/309903b70359f479043027c22cba189f7e113ec5/Stata%20ado%20files).

---

## Important Notes

- Placenta previa is identified from either:
  - Antenatal ultrasound findings indicating placenta previa, or
  - Labor and delivery records documenting placenta/vasa previa.
- Ultrasound classifications distinguish:
  - Normal placental location
  - Low-lying placenta
  - Placenta previa
- Multiple ultrasound assessments are reviewed, and any documented placenta previa is carried forward into the final indicator.
- Labor and delivery ascertainment uses:
  - Cesarean delivery indications for placenta/vasa previa
  - Antepartum hemorrhage attributed to placenta previa
- Placenta accreta and placental abruption are constructed as related placental complication variables.
- Cesarean-specific placenta previa indicators are only applicable among pregnancies delivered by cesarean section.

---

## Variables Created

### Placenta Previa

- PREVIA
- US_PREVIA
- LD_PREVIA
- CES_PREVIA

### Ultrasound Placenta Previa Assessments

- US_PREVIA1
- US_PREVIA2
- US_PREVIA3
- US_PREVIA4
- US_PREVIA5
- US_PREVIA6

### Ultrasound Timing Variables

- US_DAT1
- US_DAT2
- US_DAT3
- US_DAT4
- US_DAT5
- US_DAT6

- US_GA1
- US_GA2
- US_GA3
- US_GA4
- US_GA5
- US_GA6

### Other Placental Complications

- PLACENTA_ACCRETE
- PLACENTA_ABRUPTION

---

## Version History

### Original Version

**May 2024**  
Savannah O'Malley (savannah.omalley@gwu.edu)

### Current Version

**October 2026**  
Emma K. Cook (emmacook@gwu.edu)

Updates:
- Updated to use `MAT_STUDYSTATUS` and `MAT_PREGEND` constructed datasets.
- Ongoing maintenance and review of placenta-related outcome definitions.
