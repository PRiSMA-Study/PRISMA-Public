# MAT_PREGEND Variable Construction

## Overview

This program creates the PRISMA MNH maternal pregnancy endpoint outcomes dataset (`MAT_PREGEND`). It combines information from labor and delivery, hospitalization, and postpartum follow-up forms to construct maternal delivery, preterm birth, labor, membrane rupture, cesarean delivery, induced labor, and maternal complication outcomes.

---

## Version History

### Original Version

**March 2024**  
Erin Oakley (emoakley@gwu.edu)

### Major Updates

- March-October 2024: Development and refinement of preterm birth, PPROM, labor, and delivery classification algorithms.
- May 2024: Added uterine rupture, prolonged labor, and obstructed labor outcomes.
- March 2025: Added PROM and membrane rupture duration (`MEM_HOURS`) for IHME requests.
- August 2025: Added birth location variables.
- January 2025: Updated variable names to align with PRISMA naming conventions.

### Current Version

**October 2026**  
Emma K. Cook (emmacook@gwu.edu)

Updates:
- Transitioned to use `MAT_STUDYSTATUS` as the primary endpoint source dataset.
- Outputs MAT_PREGEND instead of separate MAT_LABOR, MAT_PRETERM, and MAT_UTERINERUP

---

## Required Inputs

Required datasets:

- MAT_STUDYSTATUS
- MNH09: Labor & Delivery
- MNH12: PNC clinical status
- MNH19: Maternal hospitalization form

This program also uses the user-written Stata command [`str2date`](https://github.com/PRiSMA-Study/PRISMA-Analysis-Savannah/blob/main/str2date.ado) to convert string dates to Stata date format.

Additional package:

- `betterbar` (optional; used only for QC figures)

---

## Outcome case reviews
- Check new uterine rupture cases quarterly and share with sites for review
- Code is designed to crash at a certain point to review, especially for cases missing preterm classification outcomes (also related to PPROM)

---

## Important Notes

- Preterm delivery is defined as delivery before 37 completed weeks gestation.
- PPROM and delivery classifications rely heavily on timing of labor onset and rupture of membranes.
- Hospitalization data (MNH19) are incorporated into PPROM, PROM, uterine rupture, and obstructed labor outcomes.
- Uterine rupture is identified using labor and delivery (MNH09), hospitalization (MNH19), and postpartum follow-up (MNH12)

---

## Variables Constructed

### Preterm Delivery (`PRETERM_ANY`)

Delivery before 37 completed weeks of gestation (live birth or stillbirth).

- **Denominator:** Completed pregnancies (`PREG_END == 1`), excluding pregnancy losses <20 weeks gestation (`PREG_LOSS == 0`)
- **Values:** `1=Yes`, `0=No`, `55=Missing`

### Spontaneous Preterm Birth (`PRETERM_SPON`)

Preterm delivery resulting from spontaneous labor or preterm premature rupture of membranes (PPROM).

**Definition confirmed with Dr. Wiley:**
		1. Non-induced labor <37 weeks GA, except in cases where there was artificial 
		rupture of membranes prior to labor onset OR
		2. Spontaneous rupture of membranes < 37 weeks GA, except in cases where 
		labor was induced/augmented before rupture of membranes  
    3. Spontaneous ROM, no labor, vaginal delivery: Dr. Wylie confirmed that we will 
    consider these cases to be spontaneous preterm, allowing vaginal delivery to 
    override the "no labor" detail. However, we will still submit cases >= 24 weeks 
    to be reviewed again by sites 

- **Denominator:** Preterm births (`PRETERM_ANY == 1`), excluding pregnancy losses <20 weeks gestation
- **Values:** `1=Yes`, `0=No`, `55=Missing`

### Provider-Initiated Preterm Birth (`PRETERM_PROV`)

Preterm delivery initiated by a healthcare provider due to a medical, obstetric, or other indication.

**Definition confirmed with Dr. Wiley:**
		1. Induced labor <37 weeks GA, except when there is spontaneous rupture 
		of membranes prior to labor induction/AUGMENTATION OR 
		2. Artificial rupture of membranes <37 weeks GA, except when there is 
		spontaneous labor prior to artificial rupture of membranes OR 
		3. Any cesarean delivery (emergent or planned) < 37 weeks GA not 
		preceded by spontaneous labor or spontaneous rupture of membranes

- **Denominator:** Preterm births (`PRETERM_ANY == 1`), excluding pregnancy losses <20 weeks gestation
- **Values:** `1=Yes`, `0=No`, `55=Missing`

### Provider-Initiated Preterm Indication (`MAT_PRETERM_PROV_IND`)

Composite variable describing the reason for provider-initiated preterm delivery based on indications for labor induction and/or cesarean delivery without prior labor or rupture of membranes.

### Spontaneous Preterm Indication (`MAT_PRETERM_SPON_IND`)

Composite variable describing spontaneous preterm birth subtype:

- Preterm labor
- Preterm premature rupture of membranes (PPROM)
- Preterm birth with unknown sequence of labor and membrane rupture

### Preterm Premature Rupture of Membranes (`PPROM_PREGEND`)

Rupture of membranes before labor onset and before 37 weeks gestation, or clinical diagnosis of PROM.

**Definition confirmed with Dr. Wiley:**
		1. Spontaneous rupture of membranes prior to 37 weeks GA that occurs 
		BEFORE onset of labor.
		ADDED ON 4-1-2024: PPROM can also occur among pregnancies at <20 weeks 
		GA; with this in mind, we will not exclude PREG_LOSS == 1
		ADDED ON 5-13: We will exclude induced abortions from the PPROM outcome

- **Denominator:** Completed pregnancies (`PREG_END == 1`)
- **Values:** `1=Yes`, `0=No`, `55=Missing`

### PROM During Hospitalization (`PROM_HOSP`)

PROM identified from maternal hospitalization records (MNH19).

### Any PPROM (`PPROM_OCCUR`)

Composite variable combining:

- `PPROM_PREGEND`
- `PROM_HOSP`

### Uterine Rupture (`MAT_UTER_RUP`)

Tear in the muscular wall of the uterus during pregnancy or childbirth; the spontaneous tearing of the uterus prior to delivery that may result in the fetus being expelled into the peritoneal cavity; occurring prior to delivery or during labor. 
    
- **Denominator:** Completed pregnancies
- **Values:** `1=Yes`, `0=No`, `55=Missing`

**Component variables**

- `MAT_UTER_RUP_IPC` – identified at pregnancy endpoint (MNH09)
- `MAT_UTER_RUP_PNC` – identified at postpartum visit (MNH12)
- `MAT_UTER_RUP_HOSP` – identified during hospitalization (MNH19)

### Prolonged Labor (`PRO_LABOR`)

Labor lasting ≥24 hours, regardless of parity.

- **Denominator:** Pregnancies with labor
- **Values:** `1=Yes`, `0=No`, `55=Missing`

### Obstructed Labor (`OBS_LABOR`)

Clinician-reported obstructed labor (checkbox)

- **Denominator:** Pregnancies with labor
- **Values:** `1=Yes`, `0=No`, `55=Missing`

### Premature Rupture of Membranes (`PROM`)

Spontaneous rupture of membranes before labor onset or clinical diagnosis during hospitalization.

- **Added:** March 2025 (IHME data request)
- **Denominator:** Completed pregnancies
- **Values:** `1=Yes`, `0=No`, `55=Missing`

### Time from ROM to Delivery (`MEM_HOURS`)

Hours between rupture of membranes and delivery (first infant for multiple gestations).

- **Added:** March 2025 (IHME data request)
- **Denominator:** Pregnancies with rupture of membranes
- **Values:** Continuous

### Induced Labor (`MAT_INDUCED`)

Labor was induced; includes all of the following:
 - Labor induced prior to any contractions or rupture of membranes (ROM)
 - Labor induced following rupture of membranes (PROM or PPROM)
 - Observation where rupture of membranes occurs first (before onset of labor) AND rupture of membranes was artificial
 - Failed inductions, where the CRF reports that the woman did not experience labor, but that labor was induced.

- **Denominator:** Pregnancies with labor and delivery information available
- **Values:** `1=Yes`, `0=No`, `55=Missing`
