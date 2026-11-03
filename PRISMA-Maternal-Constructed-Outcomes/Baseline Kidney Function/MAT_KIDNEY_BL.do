*PRISMA Variable Construction - Indicators of Kidney Function
*Purpose: This code constructs indicators of kidney function requested for the 
	*PPHTN analysis
*Original Version: April 25, 2025
*Update: October 12, 2026 by E Cook - new analyst

/*
Inputs:
	MNH08
	MAT_BASELINE
	MAT_STUDYSTATUS
*/


**# Step 0: Copy stacked data from TNT to local AWS environment and convert to 
		  * .dta fomat using `Import Data.dta'

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
		gl emma "Z:/Emma_working_files/Constructed Data_working files/MAT_KIDNEY_BL"
		gl wrk "$emma/$datadate"
			cap mkdir "$wrk" // creates directory if it doesn't exist	
		gl temp "$wrk/temp"
			cap mkdir "$temp"	
		gl output "$wrk/output"
			cap mkdir "$output"


/*************************************************************************

*This file constructs the following variables: 
	
	Creatinine 
		Collected at baseline & ????
		Drawn from Variable: CREAT_umoll_LBORRES (MNH08)
		
	Creatinine - mg/dL (to use in eGFR calculation below)
		Creatinine / 88.42 
		
	Elevated Creatinine 
		Based on this systematic review (cited below):  >77μmol/l (0.87 mg/dl)
		https://pmc.ncbi.nlm.nih.gov/articles/PMC6409397/
			
	Baseline eGFR - estimated glomerular filtration rate
	
	Equation is based on the function below: 
	
eGFRcr = 142 x min(Scr/κ, 1)^α x max(Scr/κ, 1)^-1.200 x 0.9938^Age x 1.012 [if female]
		where:
		Scr = standardized serum creatinine in mg/dL
		κ = 0.7 (females) or 0.9 (males)
		α = -0.241 (female) or -0.302 (male)
		min(Scr/κ, 1) is the minimum of Scr/κ or 1.0
		max(Scr/κ, 1) is the maximum of Scr/κ or 1.0
		Age (years)
	
	Drawn from the website provided by PRISMA PIs: 
	https://www.kidney.org/ckd-epi-creatinine-equation-2021	
	
*/
	
	/////////////////////////////////////////
	
	use "$ldata/mnh08.dta", clear
		
		
	keep if M08_CREAT_UMOLL_LBORRES > 0 
	
	gen CREATININE = M08_CREAT_UMOLL_LBORRES 
	gen CREATININE_DT = M08_RENAL_LBTSTDAT
	format CREATININE_DT %td
	label var CREATININE_DT "Date of creatinine sample"
	
	gen LAB_DT = M08_LBSTDAT
	format LAB_DT %td 
	label var LAB_DT "Date of specimen collection"
	
	gen diff_check = CREATININE_DT - LAB_DT
	tab diff_check
	
	sort diff_check
	list SITE LAB_DT CREATININE_DT diff_check M08_TYPE_VISIT ///
		if diff_check <-30 | diff_check >30
	
	sum CREATININE
	sum CREATININE, d 
	sum CREATININE_DT, format 
	
	gen CREATININE_mgdl = CREATININE / 88.42
	  
	
	/*For reference: 
	The typical range for serum creatinine is:

    For adult men, 0.74 to 1.35 mg/dL (65.4 to 119.3 mmol/L).
    For adult women, 0.59 to 1.04 mg/dL (52.2 to 91.9 mmol/L).
	
	source: 
	https://www.mayoclinic.org/tests-procedures/creatinine-test/about/pac-20384646 
	*/

	sort SITE
	by SITE: sum CREATININE 
	*histogram CREATININE, by(SITE)
	*histogram CREATININE if CREATININE <200, by(SITE)
	
	* Elevated Creatinine: 
	gen CREATININE_ELEV = 0 if CREATININE >0 & CREATININE <=77
	replace CREATININE_ELEV = 1 if CREATININE >77 & CREATININE != . 
	
	label var CREATININE_ELEV "Elevated Creatinine (>77 μmol/L)"
	
	tab CREATININE_ELEV, m 
	
	* merge in demographics data: 
	
	merge m:1 MOMID PREGID using "$out/MAT_BASELINE.dta"
	
	keep if _merge == 3 
	 
	
	*Construct the components of eGFR equation: 
	
	gen scr_k = CREATININE_mgdl / 0.7 
	
	gen scr_k_min_eq = scr_k if scr_k <=1 
	replace scr_k_min_eq = 1 if scr_k >1 
	
	gen scr_k_max_eq = scr_k if scr_k >=1 
	replace scr_k_max_eq = 1 if scr_k <1 


	gen eGFR = 142 * (scr_k_min_eq^(-0.241)) * (scr_k_max_eq^(-1.200)) * ///
		0.9938^(MAT_AGE) * 1.012
	
	sum eGFR

	*histogram eGFR, by(SITE) xline(60,lcolor(red))	xline(90,lcolor(orange))
	
	*construct eGFR categories: 
	gen eGFR_CAT = 0 if eGFR >= 90 & eGFR != . 
	replace eGFR_CAT = 1 if eGFR >=60 & eGFR <90 
	replace eGFR_CAT = 2 if eGFR >=15 & eGFR <60 
	replace eGFR_CAT = 3 if eGFR <15 & eGFR >0 
	
	label define kd 0 "Normal Function (90+)" 1 "Early stage (60-89)" ///
		2 "Kidney disease (15-59)" 3 "Kidney failure (<15)"
	label values eGFR_CAT kd 
	
	label var eGFR_CAT "eGFR Category (0-3;0 is normal)"
	
	tab eGFR_CAT, m 
	
	
	keep SITE MOMID PREGID MAT_AGE CREATININE CREATININE_ELEV ///
		CREATININE_mgdl CREATININE_DT eGFR eGFR_CAT LAB_DT M08_TYPE_VISIT
		
	*merge in enrollment data to get GA at first creatinine measure: 	
	merge m:1 MOMID PREGID using "$out/MAT_STUDYSTATUS.dta", ///
		keepusing(PREG_START_DATE)
		 
	
	drop if _merge <= 2 
	drop _merge 
	
	gen CREATININE_GA = (CREATININE_DT - PREG_START_DATE) / 7 
	
	sum CREATININE_GA
	
	gen CREATININE_GA_check = (LAB_DT - PREG_START_DATE) / 7 
	
	list CREATININE_GA CREATININE_GA_check M08_TYPE_VISIT if ///
		CREATININE_GA <0 | CREATININE_GA >40 & CREATININE_GA!=.
		
	
	*algorithm for fixing the date IF it's an enrollment visit: 
	replace CREATININE_GA = CREATININE_GA_check if CREATININE_GA <0 & ///
		CREATININE_GA_check >0 & CREATININE_GA_check <30 & M08_TYPE_VISIT==1
		
	replace CREATININE_GA = CREATININE_GA_check if CREATININE_GA >40 & ///
		CREATININE_GA_check >0 & CREATININE_GA_check <30 & M08_TYPE_VISIT==1
		
	*get rid of observations without valid GA: 
	replace CREATININE_GA = . if CREATININE_GA <0 
	replace CREATININE_GA = . if CREATININE_GA >40 & M08_TYPE_VISIT==1
		
	*order entries & find baseline: 
		// first, drop those without dates: 
	drop if CREATININE_GA == . 
	
		// next, order by date and number within pregnancies: 
	sort MOMID PREGID CREATININE_GA
	by MOMID PREGID: gen entry_number = _n 
	
		// restrict to first: 
	keep if entry_number==1 
	
		// review: 
	tab CREATININE_GA, m 
	
		// restrict to those in T1 or T2 (regardless of visit type)
	keep if CREATININE_GA < 28 & CREATININE_GA != . 
	
		// final check for duplicates: 
	duplicates tag MOMID PREGID, gen(duplicate)
	
	tab duplicate
	
		// finalize the dataset: 
	keep SITE MOMID PREGID CREATININE CREATININE_mgdl CREATININE_ELEV ///
		CREATININE_GA MAT_AGE eGFR eGFR_CAT  
		
	label var CREATININE "Baseline serum creatinine (umol/L)"
	label var CREATININE_mgdl "Baseline serum creatinine (converted to mg/dL)"
	label var CREATININE_ELEV "Elevated baseline serum creatinine (>77umol/L)"
	label var CREATININE_GA "Gestational age at baseline creatinine measure (<28 weeks)"
	label var MAT_AGE "Maternal age (used for eGFR calculation)"
	label var eGFR "Estimated GFR at baseline creatinine measure"
	label var eGFR_CAT "Estimated GFR category at baseline (0=normal;3=failure)"
	
	tab  eGFR_CAT, m 
		
	*histogram eGFR, by(SITE) xline(60,lcolor(red))	xline(90,lcolor(orange))	
	
*------------------------------------------------------------------------------
* Save
*------------------------------------------------------------------------------
label data "Data date: $datadate; `c(username)' modified `c(current_date)'"
local datalabel: data label

compress
save "$wrk/MAT_KIDNEY_BASELINE.dta", replace
save "$out/MAT_KIDNEY_BASELINE.dta", replace
export delimited using "$out/MAT_KIDNEY_BASELINE.csv", replace
export excel using "$out/MAT_KIDNEY_BASELINE.xlsx", firstrow(variables) replace

	