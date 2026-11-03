# MAT_THYR

Maternal thyroid function variable construction code for the PRISMA study.

This script creates thyroid function indicators from thyroid-stimulating hormone (TSH) and free thyroxine (Free T4) measurements collected during pregnancy. Outcomes are generated for enrollment, trimester-specific enrollment windows, ANC32, and third trimester.

- **Original:** Savannah O'Malley
- **March 19, 2026:** Updated by Emma Cook

---

## Workflow

```text
MAT_THYR.do

Input:
  - mnh08.dta
  - MAT_STUDYSTATUS.dta

Outputs:
  - MAT_THYR_LONG.dta
  - MAT_THYR.dta
```

---

## Purpose

Construct maternal thyroid function outcomes using trimester-specific reference ranges for:

- Thyroid-stimulating hormone (TSH)
- Free thyroxine (Free T4)

Thyroid status is classified as:

- Normal
- Subclinical hyperthyroid
- Overt hyperthyroid
- Subclinical hypothyroid
- Overt hypothyroid
- Unclassified

---

## Input Variables

From `mnh08.dta`:

- `THYROID_TSH_LBORRES`
- `THYROID_FREET4_LBORRES`
- `THYROID_FREET3_LBORRES`
- `TYPE_VISIT`
- `LBSTDAT`

From `MAT_STUDYSTATUS.dta`:

- `ENROLL`
- `PREG_START_DATE`
- `ANC32_EXP`
- `BOE_GA_DAYS_ENROLL`

---

## Key Processing Steps

1. Clean thyroid laboratory values.
2. Calculate gestational age at specimen collection.
3. Assign trimester:
   - T1: ≤97 days
   - T2: 98-195 days
   - T3: ≥196 days
4. Categorize TSH using trimester-specific reference ranges.
5. Categorize Free T4 using trimester-specific reference ranges.
6. Combine TSH and Free T4 results to classify thyroid function.
7. Create enrollment-, ANC32-, and trimester-specific outcomes.
8. Generate denominator variables indicating valid measurements.

---

## Thyroid Function Categories

| Code | Classification |
|--------|---------------|
| 1 | Normal |
| 2 | Subclinical hyperthyroid |
| 3 | Overt hyperthyroid |
| 4 | Subclinical hypothyroid |
| 5 | Overt hypothyroid |
| 0 | Unclassified |
| 55 | Missing |

---

## Outputs

### MAT_THYR_LONG.dta

Visit-level dataset containing:

- Gestational age
- Trimester
- TSH category
- Free T4 category
- Thyroid function classification

One observation per participant visit.

### MAT_THYR.dta

Participant-level dataset containing:

- `THYR_ENROLL`
- `THYR_ENROLL_T1`
- `THYR_ENROLL_T2`
- `THYR_ANC32`
- `THYR_T3`
- `THYR_EVER`

along with expected-observation and denominator variables.

---

## Key Variables

### Thyroid Outcomes

- `THYR_ENROLL`
- `THYR_ENROLL_T1`
- `THYR_ENROLL_T2`
- `THYR_ANC32`
- `THYR_T3`
- `THYR_EVER`

### Expected Observation Indicators

- `THYR_ENROLL_EXP`
- `THYR_ENROLL_EXP_T1`
- `THYR_ENROLL_EXP_T2`
- `THYR_ANC32_EXP`

### Denominator Variables

- `THYR_ENROLL_DENOM`
- `THYR_ANC32_DENOM`
- `THYR_EVER_DENOM`
