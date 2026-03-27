**Maternal Thyroid Function 
*Savannah O'Malley (savannah.omalley@gwu.edu)
*By trimester
*ANC20, ANC32


**CHANGE THE BELOW BASED ON WHICH DATA YOU ARE WORKING WITH
global datadate "2026-01-30"

global wrk "Z:\Savannah_working_files\Thyroid/$datadate"
cap mkdir "$wrk"
cd "$wrk"
local date: di %td_CCYY_NN_DD daily("`c(current_date)'", "DMY")
global today = subinstr(strltrim("`date'"), " ", "-", .)
global da "Z:\Stacked Data/$datadate"
global outcomes "Z:\Outcome Data/$datadate"
global gph "$wrk"


/*
**NOTE: requires the following data sets:

"$outcomes/MAT_ENROLL.dta"
"$outcomes/mat_ENDPOINTS.dta"
"Z:\Savannah_working_files\Expected_obs.dta"
*/

**IMPORT DATA
use "D:\Users\savannah.omalley\Documents\data/$datadate/mnh08", clear
rename  M08_* *


keep SITE MOMID PREGID  MAT_VISIT_MNH08  TYPE_VISIT LBSTDAT IODINE_LBORRES THYROID_LBTSTDAT THYROID_LBPERF_1 THYROID_TSH_LBORRES THYROID_LBPERF_2 THYROID_FREET4_LBORRES THYROID_LBPERF_3 THYROID_FREET3_LBORRES

foreach var in  IODINE_LBORRES   THYROID_TSH_LBORRES    THYROID_FREET4_LBORRES    THYROID_FREET3_LBORRES   {
	replace `var' = . if `var' <0 
}




foreach var in   THYROID_LBPERF_1  THYROID_LBPERF_2  THYROID_LBPERF_3 {
	replace `var' = . if  `var' == 55 | `var'==77
}

*Keep only relevant observations:
keep if THYROID_LBPERF_1 == 1  & inlist(TYPE_VISIT,1,4,5)
*drop if TSH was not performed
*keep only observations at relevant visits

label var  THYROID_LBPERF_1 "Performed TSH"
label var  THYROID_TSH_LBORRES "TSH (uIU/mL)"

label var  THYROID_LBPERF_2 "Performed Free T4"
label var  THYROID_FREET4_LBORRES "Free T4 (ng/dL)"

label var  THYROID_LBPERF_3 "Performed Free T3"
label var  THYROID_FREET3_LBORRES "Free T3 (pg/mL)"

label define TYPE_VISIT ///
	1"1. Enrollment" 	///
	2"2. ANC-20" 		///
	3"3. ANC-28" 		///
	4"4. ANC-32" 		///
	5"5. ANC-36" 		///
	6"6. IPC (L&D)" 	///
	7"7. PNC-0" 		///
	8"8. PNC-1" 		///
	9"9. PNC-4" 		///
	10"10. PNC-6" 		///
	11"11. PNC-26" 		///
	12"12. PNC-52" 		///
	13"13. Non-scheduled ANC visit for routine care" 		///
	14"14. Non-scheduled PNC visit for routine care"	
label val TYPE_VISIT TYPE_VISIT


gen uploaddate="$datadate"
gen UploadDate=date(uploaddate, "YMD")
format UploadDate %td

save "$wrk/thyr_allvisits.dta" , replace 

use "$outcomes/MAT_ENROLL.dta", clear 

gen ENROLL_TRIMESTER = 1 if inrange( BOE_GA_DAYS_ENROLL,28,97)
replace ENROLL_TRIMESTER = 2 if inrange( BOE_GA_DAYS_ENROLL,98,195)

keep SITE MOMID PREGID ENROLL ENROLL_TRIMESTER ENROLL_SCRN_DATE PREG_START_DATE EST_CONCEP_DATE_US

merge 1:m  PREGID using "$wrk/thyr_allvisits.dta"
keep if ENROLL == 1
//drop if participant does not exist in MNH08 & no indication that this PREGID was enrolled
tab _merge
//indicates that some have BOE but no MNH08
// some have MNH08 but no BOE

str2date PREG_START_DATE ENROLL_SCRN_DATE EST_CONCEP_DATE_US


gen GA_DAYS =  LBSTDAT - PREG_START_DATE
replace GA_DAYS =. if LBSTDAT < 0
//replace missing if lab date is default value

/// THYROID FUNCTION ///

gen THYR_GA =  LBSTDAT - PREG_START_DATE

	
	gen 	TYPE_VISIT_ACOG = 1 if inrange(THYR_GA,28,139) & TYPE_VISIT == 1
	replace TYPE_VISIT_ACOG = 2 if inrange(THYR_GA,126,181) & TYPE_VISIT == 2
	replace TYPE_VISIT_ACOG = 3 if inrange(THYR_GA,182,216)
	replace TYPE_VISIT_ACOG = 4 if inrange(THYR_GA,217,237)
	replace TYPE_VISIT_ACOG = 5 if inrange(THYR_GA,238,308)
	
	gen query_vistype = 1 if (TYPE_VISIT != TYPE_VISIT_ACOG) 
	label var query_vistype "Visit type does not match"	
	
	

gen 	THYR_TRIMESTER = 1 if THYR_GA <=97
replace THYR_TRIMESTER = 2 if THYR_GA>=98 & THYR_GA<=195
replace THYR_TRIMESTER = 3 if THYR_GA>=196 
replace THYR_TRIMESTER = . if LBSTDAT== .
replace THYR_TRIMESTER = . if inlist(TYPE_VISIT,6,7,8,9,10,11,12,14)
//replace trimester to missing if visit occured during PNC period

/*
Hyperthyroidism = 				low TSH & high Free T4
Subclinical hyperthyroidism = 	low TSH & normal Free T4
Hypothyroidism = 				high TSH & low Free T4
Subclinical hypothyroidism = 	high TSH & normal Free T4

TSH ranges :
1st trimester = 0.6 - 3.4
2nd trimester = 0.37 - 3.6
3rd trimester = 0.38 - 4.04

Free T4 ranges:
1st trimester = 0.8 - 1.2 ng/dL
2nd trimester = 0.6 - 1 ng/dL
3rd trimester = 0.5 - 0.8 ng/dL 
*/



gen TSH = 55
replace TSH = 1 if ///
THYR_TRIMESTER == 1 &  THYROID_TSH_LBORRES<0.6 | ///
THYR_TRIMESTER == 2 &  THYROID_TSH_LBORRES<0.37 | ///
THYR_TRIMESTER == 3 &  THYROID_TSH_LBORRES<0.38

replace TSH = 2 if ///
THYR_TRIMESTER == 1 & THYROID_TSH_LBORRES>=0.6 & THYROID_TSH_LBORRES<3.4  | ///
THYR_TRIMESTER == 2 & THYROID_TSH_LBORRES>=0.37 & THYROID_TSH_LBORRES<3.6 | ///
THYR_TRIMESTER == 3 & THYROID_TSH_LBORRES>=0.38 & THYROID_TSH_LBORRES<4.04  

replace TSH = 3 if ///
THYR_TRIMESTER == 1 &  THYROID_TSH_LBORRES>=3.4 &  THYROID_TSH_LBORRES!=. | ///
THYR_TRIMESTER == 2 &  THYROID_TSH_LBORRES>=3.6 &  THYROID_TSH_LBORRES!=. | ///
THYR_TRIMESTER == 3 &  THYROID_TSH_LBORRES>=4.04 &  THYROID_TSH_LBORRES!=.

tabstat THYROID_TSH_LBORRES, by(SITE) stats (q)

label define TSH ///
1"Low TSH" 2"Normal" 3"High TSH" 55"Missing"
label val TSH TSH

label var TSH ///
"TSH"

gen tsh_abnormal = 1 if ///
inrange(THYROID_TSH_LBORRES,0,0.29) | inrange(THYROID_TSH_LBORRES,4.1,.) 

*check that site medians are reasonable
bysort SITE : egen FREET4_MED = median(THYROID_FREET4_LBORRES)
//median FREE T4 by site
egen FREET4_MED_ALL = median(THYROID_FREET4_LBORRES)
//overall median FREE T4
egen FREET4_STD = std(FREET4_MED)
// divide site by overall median
bigtab  SITE FREET4_STD
cap bigtab SITE FREET4_STD if !inrange(FREET4_STD,-2,2)

/*
graph box THYROID_FREET4_LBORRES,over(SITE) nooutsides title("Free T4") caption("Data from: $datadate")
graph export "$wrk/FREET4.png", replace

graph box THYROID_FREET3_LBORRES,over(SITE) nooutsides title("Free T3") caption("Data from: $datadate")
graph export "$wrk/FREET3.png", replace

graph box THYROID_TSH_LBORRES,over(SITE) nooutsides title("TSH") caption("Data from: $datadate")
graph export "$wrk/TSH.png", replace
*/

 

gen T4 = 55
replace T4 = 1 if ///
THYR_TRIMESTER == 1 &  THYROID_FREET4_LBORRES < 0.8 | ///
THYR_TRIMESTER == 2 &  THYROID_FREET4_LBORRES < 0.6 | ///
THYR_TRIMESTER == 3 &  THYROID_FREET4_LBORRES < 0.5 

replace T4 = 2 if ///
THYR_TRIMESTER == 1 &  ///
THYROID_FREET4_LBORRES >= 0.8 &  THYROID_FREET4_LBORRES < 1.2 | ///
THYR_TRIMESTER == 2 &  ///
THYROID_FREET4_LBORRES >= 0.6 &  THYROID_FREET4_LBORRES < 1 | ///
THYR_TRIMESTER == 3 &  ///
THYROID_FREET4_LBORRES >= 0.5 &  THYROID_FREET4_LBORRES < 0.8 

replace T4 = 3 if ///
THYR_TRIMESTER == 1 &  ///
THYROID_FREET4_LBORRES >= 1.2 &  THYROID_FREET4_LBORRES != . | ///
THYR_TRIMESTER == 2 &  ///
THYROID_FREET4_LBORRES >= 1 &  THYROID_FREET4_LBORRES != . | ///
THYR_TRIMESTER == 3 &  ///
THYROID_FREET4_LBORRES >= 0.8 &  THYROID_FREET4_LBORRES != . 

label define T4 ///
1"Low T4" 2"Normal" 3"High T4" 55"Missing"
label val T4 T4
	

label define THYR ///
-1"Missing" ///
1"Normal" ///
2"Sub hyper" ///
3"Overt hyper" ///
4"Sub hypo" ///
5"Overt hypo" ///
0"Unclassified"

gen 		THYR = -1
replace 	THYR = 1 if TSH == 2
replace 	THYR = 2 if TSH == 1 & T4 == 2
replace 	THYR = 3 if TSH == 1 & T4 == 3
replace 	THYR = 4 if TSH == 3 & T4 == 2
replace 	THYR = 5 if TSH == 3 & T4 == 1
replace 	THYR = 0 if TSH == 3 & T4 == 3
replace 	THYR = 0 if TSH == 1 & T4 == 1
label val 	THYR THYR  


save "$wrk/thyr_short.dta" , replace 

*expected at enrollment?
str2date ENROLL_SCRN_DATE
gen THYR_ENROLL_EXP = 1 if ///
SITE == "Ghana" & ENROLL_SCRN_DATE <= date("2024-09-01", "YMD") | ///
SITE == "India-CMC" & ENROLL_SCRN_DATE <= date("2024-10-17", "YMD") | ///
SITE == "India-SAS" & ENROLL_SCRN_DATE <= date("$datadate", "YMD") | ///
SITE == "Kenya" & ENROLL_SCRN_DATE <= date("2025-01-14", "YMD") | ///
SITE == "Pakistan" & ENROLL_SCRN_DATE <= date("2024-10-03", "YMD") | ///
SITE == "Zambia" & ENROLL_SCRN_DATE <= date("2024-10-11", "YMD") 

label var THYR_ENROLL_EXP "Thyroid test expected at enrollment (before test stop)"


gen		 	THYR_ENROLL = THYR if ///
			TYPE_VISIT ==1 & THYR_ENROLL_EXP == 1 & ///
			inrange(THYR_GA ,28,139) 
			
label var 	THYR_ENROLL "Thyroid at enrollment (any trimester)"

foreach num of numlist 1/2 {
	gen THYR_ENROLL_EXP_T`num' = THYR_ENROLL_EXP if ///
	ENROLL_TRIMESTER == `num'
	
	label var THYR_ENROLL_EXP_T`num' "Expected at T`num'"

	gen THYR_ENROLL_T`num' = THYR if ///
	TYPE_VISIT ==1 & THYR_ENROLL_EXP == 1 & ENROLL_TRIMESTER == `num' & THYR_TRIMESTER == `num'
		//must be enrol visit, expected, enrolled in that trimester, and visit in that trimester
	
	label var THYR_ENROLL_T`num' "Thyroid at enrollment, T`num'"
}


*Expected at ANC32? First we need to merge in "expected" variables:

merge m:1 MOMID PREGID using "Z:\Savannah_working_files\Expected-obs\Expected_obs-$datadate.dta" , nogen force
keep if ENROLL == 1

gen 		THYR_ANC32_EXP = ANC32_EXP
label var 	THYR_ANC32_EXP "Expected at ANC32"

gen 		THYR_ANC32 = THYR if ///
			inlist(TYPE_VISIT,4,5) & inrange(THYR_GA,217,308) 	
label var 	THYR_ANC32 "Thyroid at ANC32"
*NOTE: TOOK OUT REQUIREMENT THYR_ANC32_EXP ==1 
			

gen THYR_T3 = THYR if THYR_ANC32_EXP ==1 & ///
 THYR_TRIMESTER == 3
label var THYR_T3 "Thyroid at T3"


foreach var in THYR_ENROLL THYR_ENROLL_T1 THYR_ENROLL_T2 THYR_ANC32 THYR_T3 {
	replace `var' = . if `var' == -1
}

sort SITE  MOMID PREGID LBSTDAT

preserve 
	
	keep SITE MOMID PREGID LBSTDAT TYPE_VISIT THYR THYR_GA THYR_TRIMESTER THYR_ENROLL_EXP ENROLL_TRIMESTER 
	replace THYR = 55 if THYR == -1
	replace THYR_ENROLL_EXP = 0 if THYR_ENROLL_EXP == . 
	
	save "$wrk/MAT_THYR_LONG.dta", replace
restore
collapse (firstnm) THYR_ENROLL_EXP THYR_ENROLL THYR_ENROLL_EXP_T1 THYR_ENROLL_T1 THYR_ENROLL_EXP_T2 THYR_ENROLL_T2  THYR_ANC32_EXP THYR_ANC32 THYR_T3 ENROLL_TRIMESTER , ///
by(SITE MOMID PREGID)

replace THYR_ENROLL = 55 if THYR_ENROLL_EXP ==1 & THYR_ENROLL == .
replace THYR_ENROLL_T1=55 if ///
	ENROLL_TRIMESTER == 1 & THYR_ENROLL_T1==. & THYR_ENROLL_EXP_T1==1
replace THYR_ENROLL_T2=55 if ///
	ENROLL_TRIMESTER == 2 & THYR_ENROLL_T2==. & THYR_ENROLL_EXP_T2==1
replace THYR_ANC32 = 55 if THYR_ANC32_EXP ==1 & THYR_ANC32 == . 
replace THYR_T3 = 55 if THYR_ANC32_EXP ==1 & THYR_T3 == . 

gen THYR_ENROLL_DENOM = 1 if THYR_ENROLL<55 //non-missing
gen THYR_ANC32_DENOM = 1 if  THYR_ANC32<55 //non-missing

foreach num of numlist 1/2 {
	replace THYR_ENROLL_T`num' = . if ENROLL_TRIMESTER != `num'
	replace THYR_ENROLL_EXP_T`num' = . if  ENROLL_TRIMESTER != `num'
}

foreach var in  THYR_ANC32 THYR_ANC32_DENOM THYR_ANC32_EXP  {
	replace `var'=. if (THYR_ANC32_EXP!=1  & !inrange(THYR_ANC32,0,5) )
}

keep SITE MOMID PREGID THYR_ENROLL_EXP THYR_ENROLL THYR_ENROLL_EXP_T1 THYR_ENROLL_T1 THYR_ENROLL_EXP_T2 THYR_ENROLL_T2 THYR_ANC32_EXP THYR_ANC32 THYR_T3 THYR_ENROLL_DENOM THYR_ANC32_DENOM
label drop THYR
label define THYR ///
55"Missing" ///
1"Normal" ///
2"Subclinical hyperthyroid" ///
3"Overt hyperthyroid" ///
4"Subclinical hypothyroid" ///
5"Overt hypothyroid" ///
0"Unclassified"
label val THYR_ENROLL THYR_ENROLL_T1 THYR_ENROLL_T2 THYR_ANC32 THYR_T3 THYR  

gen THYR_EVER = 1 if ///
inrange(THYR_ENROLL,2,5) | inrange(THYR_ANC32,2,5)
gen THYR_EVER_DENOM = 1 if ///
THYR_ENROLL < 55 | THYR_ANC32 < 55
replace THYR_EVER = 0 if THYR_EVER_DENOM ==1 & THYR_EVER==.
replace THYR_EVER = 55 if ///
(THYR_ENROLL == 55 & THYR_ANC32==55) | ///
(THYR_ENROLL == 55 & THYR_ANC32==.) 
// ever thyroid dysfunction at enroll, ANC20, ANC32, ANC36

label var THYR_ENROLL_EXP "Thyroid test expected at enrollment (before test stop)"
label var THYR_ENROLL "Thyroid status at enrollment (any trimester)"
label var THYR_ENROLL_EXP_T1 "Expected at T1"
label var THYR_ENROLL_T1 "Thyroid at enrollment, T1"
label var THYR_ENROLL_EXP_T2 "Expected at T2"
label var THYR_ENROLL_T2 "Thyroid at enrollment, T2"
label var THYR_ANC32_EXP "Expected at ANC32"
label var THYR_ANC32 "Thyroid at ANC32"
label var THYR_ENROLL_DENOM "Valid result at enrollment"
label var THYR_ANC32_DENOM "Valid result at ANC32"
label var THYR_EVER "Thyroid dysfunction, ever"
label var THYR_EVER_DENOM "Thyroid test, ever"
label var THYR_T3 "Thyroid at T3"

sort SITE MOMID PREGID
order SITE MOMID PREGID THYR_ENROLL_EXP THYR_ENROLL_DENOM THYR_ENROLL THYR_ENROLL_EXP_T1 THYR_ENROLL_T1 THYR_ENROLL_EXP_T2 THYR_ENROLL_T2 THYR_ANC32_EXP THYR_ANC32_DENOM THYR_ANC32 THYR_EVER_DENOM THYR_EVER

label data "Data date: $datadate; `c(username)' modified `c(current_date)'"
local datalabel: data label
disp "`datalabel'" //displays dataset label you just assigned

save "$wrk/MAT_THYR-$today.dta" , replace

*Review and save to outcomes folder:
*save "$outcomes/MAT_THYR.dta" , replace //keep commented until ready to add to outcomes folder

