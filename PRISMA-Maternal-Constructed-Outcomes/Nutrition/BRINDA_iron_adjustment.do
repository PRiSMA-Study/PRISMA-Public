*------------------------------------------------------------------------------
* BRINDA adjustment of ferritin and sTfR for inflammation (CRP, AGP)
* and malaria - Stata translation of the original R script
* Original R code: Nazia (naziabinte.ali@gwu.edu) - September 2026
* Updated by E Cook (emmacook@gwu) for MAT_NUTR - September 16, 2026
*------------------------------------------------------------------------------

clear
set more off

*Update Data Date
	gl datadate "2026-09-18" // data date

*Get today's date:
local today: di %td_CCYY_NN_DD daily("`c(current_date)'", "DMY")
global date = subinstr(strltrim("`today'"), " ", "-", .)

**# Directory structure:
	*Emma's local folders in AWS
		gl ldata "D:\Users\emmacook\Documents/data/$datadate"
	*Data folders in TNT Drive
		gl da "Z:/Stacked Data/$datadate"
		*gl out "Z:/Outcome Data/$datadate" // for dates prior to 9/18/2026
		gl out "Z:/Constructed Data/$datadate"
	*Working files folder (TNT Drive)	
		gl emma "Z:/Emma_working_files/Constructed Data_working files/MAT_NUTR"
		gl wrk "$emma/$datadate"
			cap mkdir "$wrk" // creates directory if it doesn't exist	
		gl temp "$wrk/temp"
			cap mkdir "$temp"	
		gl output "$wrk/output"
			cap mkdir "$output"

*==============================================================================
* 1. Load and subset MNH08 (labs)
*==============================================================================
use "$ldata/mnh08.dta", clear
 
keep SITE MOMID PREGID M08_TYPE_VISIT M08_LBSTDAT ///
     M08_FERRITIN_LBORRES M08_CRP_LBORRES M08_AGP_LBORRES M08_TRANSFERRIN_LBORRES

* Negative values (e.g., -5, -7 missing codes) -> missing
foreach v of varlist M08_FERRITIN_LBORRES M08_CRP_LBORRES M08_AGP_LBORRES ///
                     M08_TRANSFERRIN_LBORRES {
	tab `v' if `v' < 0
    replace `v' = . if `v' < 0
}

* Ferritin is in mcg/dL; the ID cutoff is in mcg/L -> multiply by 10
* (sTfR needs no conversion)
replace M08_FERRITIN_LBORRES = M08_FERRITIN_LBORRES * 10

gen DATE = M08_LBSTDAT
replace DATE = . if M08_LBSTDAT==td(07jul1907) | M08_LBSTDAT==td(16oct7023)
gen malaria_record=0

tempfile mnh08
save `mnh08'

*==============================================================================
* 2. Load and subset MNH06 (malaria)
*==============================================================================
use "$ldata/mnh06.dta", clear

keep MOMID PREGID M06_TYPE_VISIT M06_DIAG_VSDAT ///
     M06_MALARIA_POC_LBORRES M06_MALARIA_POC_LBPERF

foreach v of varlist M06_MALARIA_POC_LBORRES M06_MALARIA_POC_LBPERF {
	tab `v' if `v' < 0
    replace `v' = . if `v' < 0
	replace `v' = . if `v' == 55
}

* Recode malaria 77 (not done) as no malaria (0)
replace M06_MALARIA_POC_LBORRES = 0 if M06_MALARIA_POC_LBORRES == 77
 
gen DATE = M06_DIAG_VSDAT
replace DATE = . if M06_DIAG_VSDAT==td(07jul1907)

*only keep valid tests
drop if DATE==. | M06_MALARIA_POC_LBORRES==.

* If participant has multiple malaria values on same day, take positive
bysort PREGID DATE: egen malaria_max = max(M06_MALARIA_POC_LBORRES)
bysort PREGID DATE: keep if _n==1
replace M06_MALARIA_POC_LBORRES = malaria_max 
drop malaria_max


gen MAL_DATE = DATE
gen DATE_30 = DATE + 30
gen malaria_record = 1 
keep MOMID PREGID DATE MAL_DATE DATE_30 malaria_record M06_MALARIA_POC_LBORRES



*==============================================================================
* 3. Append
*============================================================================== 

append using `mnh08'

   quietly count if malaria_record == 0
   local n_lab = r(N)

* 0 = malaria row, 1 = lab row: malaria sorts first on the same date
gen lab_order = (malaria_record == 0)

* One call, so all three carried variables come from the same row
bysort PREGID (DATE lab_order): carryforward ///
    DATE_30 M06_MALARIA_POC_LBORRES MAL_DATE, ///
    gen(DATE_CEILING MALARIA_CF MALARIA_DATE_CF)

*keep only lab rows
	keep if malaria_record==0
	
*restrict to valid 30 day window
replace MALARIA_CF = . if DATE > DATE_CEILING | DATE==.
replace MALARIA_DATE_CF = . if DATE > DATE_CEILING | DATE==.

*check
assert missing(MALARIA_CF) if DATE > DATE_CEILING 
assert missing(MALARIA_DATE_CF) if DATE > DATE_CEILING
  
gen date_distance = DATE - MALARIA_DATE_CF ///
    if !missing(MALARIA_DATE_CF)
	
   assert _N == `n_lab'         // check no lab rows lost or multiplied
   assert inrange(date_distance, 0, 30) if !missing(date_distance)

gen malaria_carried_fwd = (date_distance > 0) ///
    if !missing(date_distance)

label define cf_lbl ///
    0 "Same day as lab draw" ///
    1 "Carried forward from prior date"

label values malaria_carried_fwd cf_lbl

sum date_distance, detail
tab malaria_carried_fwd,m

replace M06_MALARIA_POC_LBORRES = MALARIA_CF
gen M06_DIAG_VSDAT = MALARIA_DATE_CF
format M06_DIAG_VSDAT %td
	
* Visit variable
gen visit = ""
replace visit = "ANC20" if inlist(M08_TYPE_VISIT, 1, 2)
replace visit = "ANC32" if inlist(M08_TYPE_VISIT, 4, 5)
replace visit = "PNC6"  if M08_TYPE_VISIT == 10

*==============================================================================
* 4. Check ferritin median by site
*==============================================================================
tabstat M08_FERRITIN_LBORRES, by(SITE) statistics(n p50)
 
*==============================================================================
* 5. Replace zeros with minimal values (as used in BRINDA)
*==============================================================================
gen M08_CRP_LBORRES_NONZERO         = M08_CRP_LBORRES
gen M08_AGP_LBORRES_NONZERO         = M08_AGP_LBORRES
gen M08_FERRITIN_LBORRES_NONZERO    = M08_FERRITIN_LBORRES
gen M08_TRANSFERRIN_LBORRES_NONZERO = M08_TRANSFERRIN_LBORRES

replace M08_CRP_LBORRES_NONZERO         = 0.024  if M08_CRP_LBORRES == 0
replace M08_AGP_LBORRES_NONZERO         = 0.0304 if M08_AGP_LBORRES == 0
replace M08_FERRITIN_LBORRES_NONZERO    = 0.6    if M08_FERRITIN_LBORRES == 0
replace M08_TRANSFERRIN_LBORRES_NONZERO = 0.088  if M08_TRANSFERRIN_LBORRES == 0
 
*==============================================================================
* 6. 10th percentile by site and visit
*==============================================================================
preserve
    drop if missing(visit)
    collapse (count) n_ferritin    = M08_FERRITIN_LBORRES_NONZERO    ///
             (p10)   p10_ferritin  = M08_FERRITIN_LBORRES_NONZERO    ///
             (count) n_transferrin = M08_TRANSFERRIN_LBORRES_NONZERO ///
             (p10)   p10_transferrin = M08_TRANSFERRIN_LBORRES_NONZERO ///
             (count) n_crp         = M08_CRP_LBORRES_NONZERO         ///
             (p10)   p10_crp       = M08_CRP_LBORRES_NONZERO         ///
             (count) n_agp         = M08_AGP_LBORRES_NONZERO         ///
             (p10)   p10_agp       = M08_AGP_LBORRES_NONZERO,        ///
             by(SITE visit)
    list, sepby(SITE) noobs
restore


*==============================================================================
* 9. BRINDA regression-correction adjustment for CRP, AGP and malaria
*==============================================================================

* --- Manually specified site x visit CRP / AGP reference values -------------
preserve
    clear
    input str10 SITE str5 visit double(CRPref AGPref)
    "Ghana"     "ANC20" 0.39  0.35
    "Ghana"     "ANC32" 0.36  0.27
    "Ghana"     "PNC6"  0.27  0.38
    "India-CMC" "ANC20" 0.9   0.36
    "India-CMC" "ANC32" 1.06  0.29
    "India-CMC" "PNC6"  0.59  0.38
    "India-SAS" "ANC20" 0.30  0.35
    "India-SAS" "ANC32" 0.43  0.28
    "India-SAS" "PNC6"  0.25  0.44
    "Kenya"     "ANC20" 0.703 0.343
    "Kenya"     "ANC32" 0.64  0.27
    "Kenya"     "PNC6"  0.448 0.39
    "Pakistan"  "ANC20" 0.89  0.57
    "Pakistan"  "ANC32" 0.993 0.47
    "Pakistan"  "PNC6"  0.57  0.67
    "Zambia"    "ANC20" 2.15  0.39
    "Zambia"    "ANC32" 1.68  0.35
    "Zambia"    "PNC6"  0.93  0.47
    end
    tempfile refs
    save `refs'
restore

merge m:1 SITE visit using `refs', keep(master match) nogenerate

* --- Log transforms ----------------------------------------------------------
gen lnFerritin = ln(M08_FERRITIN_LBORRES_NONZERO)
gen lnSTFR     = ln(M08_TRANSFERRIN_LBORRES_NONZERO)
gen lnCRP      = ln(M08_CRP_LBORRES_NONZERO)
gen lnAGP      = ln(M08_AGP_LBORRES_NONZERO)

* Complete cases for modeling
gen model = !missing(lnFerritin, lnSTFR, lnCRP, lnAGP, ///
	M06_MALARIA_POC_LBORRES)

gen lnFerritin_adj = .
gen lnSTFR_adj     = .

* --- Fit models and adjust within each site x visit ---------------------------
* Groups with <10 complete cases, or no reference values, stay missing.
levelsof visit, local(visits)
levelsof SITE, local(sites)

foreach s of local sites {
    foreach v of local visits {

        local grp `"SITE == "`s'" & visit == "`v'""'

        quietly count if `grp' & model
        if r(N) < 10 continue

        quietly count if `grp' & !missing(CRPref, AGPref)
        if r(N) == 0 continue

        quietly summarize CRPref if `grp'
        local crpref = r(mean)
        quietly summarize AGPref if `grp'
        local agpref = r(mean)

        * Ferritin model (omitted/collinear terms have _b = 0)
        quietly regress lnFerritin lnCRP lnAGP M06_MALARIA_POC_LBORRES ///
			if `grp' & model
        local bCRP_f = _b[lnCRP]
        local bAGP_f = _b[lnAGP]
        local bMal_f = _b[M06_MALARIA_POC_LBORRES]

        * sTfR model
        quietly regress lnSTFR lnAGP M06_MALARIA_POC_LBORRES if `grp' & model
        local bAGP_s = _b[lnAGP]
        local bMal_s = _b[M06_MALARIA_POC_LBORRES]

        * Ferritin: start with unadjusted, then adjust if CRP/AGP elevated or malaria
        quietly replace lnFerritin_adj = lnFerritin if `grp' ///
            & !missing(lnFerritin, M08_CRP_LBORRES_NONZERO, ///
					   M08_AGP_LBORRES_NONZERO, M06_MALARIA_POC_LBORRES)

        quietly replace lnFerritin_adj = lnFerritin                          ///
            - `bCRP_f' * (lnCRP - ln(`crpref'))                              ///
            - `bAGP_f' * (lnAGP - ln(`agpref'))                              ///
            - `bMal_f' * M06_MALARIA_POC_LBORRES                             ///
            if `grp'                                                         ///
            & !missing(lnFerritin, M08_CRP_LBORRES_NONZERO, ///
					   M08_AGP_LBORRES_NONZERO, M06_MALARIA_POC_LBORRES) ///
            & (M08_CRP_LBORRES_NONZERO > `crpref' | ///
			   M08_AGP_LBORRES_NONZERO > `agpref' | ///
			   M06_MALARIA_POC_LBORRES == 1)

        * sTfR: AGP and malaria only
        quietly replace lnSTFR_adj = lnSTFR if `grp' ///
            & !missing(lnSTFR, M08_AGP_LBORRES_NONZERO, M06_MALARIA_POC_LBORRES)

        quietly replace lnSTFR_adj = lnSTFR                                  ///
            - `bAGP_s' * (lnAGP - ln(`agpref'))                              ///
            - `bMal_s' * M06_MALARIA_POC_LBORRES                             ///
            if `grp'                                                         ///
            & !missing(lnSTFR, M08_AGP_LBORRES_NONZERO, M06_MALARIA_POC_LBORRES) ///
            & (M08_AGP_LBORRES_NONZERO > `agpref' | M06_MALARIA_POC_LBORRES == 1)
    }
}

* Back-transform
gen ferritin_adj_malaria = exp(lnFerritin_adj)
gen stfr_adj_malaria     = exp(lnSTFR_adj)
 
*==============================================================================
* 10. Keep final variables and save
*==============================================================================
keep SITE MOMID PREGID M08_TYPE_VISIT M08_LBSTDAT ///
     ferritin_adj_malaria stfr_adj_malaria
	 
rename M08_TYPE_VISIT TYPE_VISIT
rename M08_LBSTDAT    LBSTDAT

sort SITE MOMID PREGID LBSTDAT
save "$wrk/mnh08_BRINDA_adj_iron.dta", replace


