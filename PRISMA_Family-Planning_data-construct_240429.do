

*Making common identifying variables
foreach crf in 00 01 02 03 04 12 {
	use "$dr2/$date/mnh`crf'_processed.dta", clear
foreach var in SITE SCRNID MOMID PREGID {
	
	
	cap rename M`crf'_`var' `var'
	
}

tempfile mnh`crf'
save `mnh`crf''

}

*Merging together data
use `mnh00', clear

merge 1:m SCRNID using `mnh02', keepusing(SITE SCRNID MOMID PREGID)

drop if _merge==1
drop if MOMID==""
rename _merge m0002

merge 1:1 MOMID PREGID using `mnh03'
rename _merge m000203

preserve 
use `mnh04', clear
local prev M04_PH_PREVN_RPORRES
gen first_preg = 0 if `prev'==-7
replace first_preg = 1 if `prev'==1
replace first_preg = 2 if `prev'==2
replace first_preg = 3 if `prev'>=3 &`prev'!=.
collapse (max) first_preg , by(MOMID PREGID)
tab first_preg
la var first_preg "Number of previous pregnancies"
tempfile parity
save `parity'

restore

merge 1:1 MOMID PREGID using `parity'
rename _merge m00020304

merge 1:m MOMID PREGID using `mnh12'
drop if _merge==1
rename _merge m0002030412


*Keeping the variables we need.
keep SITE SCRNID MOMID PREGID M00_ESTIMATED_AGE M00_SCHOOL_SCORRES ///
	 M00_SCHOOL_YRS_SCORRES M03_MARITAL_SCORRES M03_MARITAL_AGE ///
	 M03_MOBILE_ACCESS_FCORRES first_preg M12_METHOD_FP_FAORRES ///
	 M12_METHOD_FP_SPFY_FAORRES M12_TYPE_VISIT 


	 
save "$da/prisma_fp_data-raw.dta", replace

egen id = group(SCRNID MOMID PREGID)
drop SCRNID MOMID PREGID 
order id, after(SITE)

egen tag=tag(id)

set seed 240428
gen rand =runiform() if tag==1
egen randmax = max(rand),by(id)
keep if randmax<=.1
	 
save "$da/prisma_fp_data-raw-subsample.dta", replace