**Maternal Thyroid Function 
*Savannah O'Malley (savannah.omalley@gwu.edu)
*By trimester
*ANC20, ANC32


**CHANGE THE BELOW BASED ON WHICH DATA YOU ARE WORKING WITH
global datadate "2025-04-18"

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

foreach var in  IODINE_LBORRES THYROID_LBPERF_1  THYROID_TSH_LBORRES  THYROID_LBPERF_2  THYROID_FREET4_LBORRES  THYROID_LBPERF_3  THYROID_FREET3_LBORRES   {
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

keep SITE MOMID PREGID ENROLL ENROLL_TRIMESTER ENROLL_SCRN_DATE PREG_START_DATE 

merge 1:m  PREGID using "$wrk/thyr_allvisits.dta"
keep if ENROLL == 1
//drop if participant does not exist in MNH08 & no indication that this PREGID was enrolled
tab _merge
//indicates that some have BOE but no MNH08
// some have MNH08 but no BOE

str2date PREG_START_DATE ENROLL_SCRN_DATE


gen GA_DAYS =  LBSTDAT - PREG_START_DATE
replace GA_DAYS =. if LBSTDAT < 0
//replace missing if lab date is default value

/// THYROID FUNCTION ///

gen THYR_GA_DAYS =  LBSTDAT - PREG_START_DATE

gen THYR_TRIMESTER = 1 if THYR_GA_DAYS <=97
replace THYR_TRIMESTER = 2 if THYR_GA_DAYS>=98 & THYR_GA_DAYS<=195
replace THYR_TRIMESTER = 3 if THYR_GA_DAYS>=196 
replace THYR_TRIMESTER = . if  LBSTDAT==.
replace THYR_TRIMESTER =. if TYPE_VISIT >=6 & TYPE_VISIT<=12 | TYPE_VISIT==14
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
bigtab SITE FREET4_STD if !inrange(FREET4_STD,-2,2)

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

gen THYR = -1
replace THYR = 1 if TSH == 2
replace THYR = 2 if TSH == 1 & T4 == 2
replace THYR = 3 if TSH == 1 & T4 == 3
replace THYR = 4 if TSH == 3 & T4 == 2
replace THYR = 5 if TSH == 3 & T4 == 1
replace THYR = 0 if TSH == 3 & T4 == 3
replace THYR = 0 if TSH == 1 & T4 == 1
label val THYR THYR  


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

gen THYR_ENROLL = THYR if TYPE_VISIT ==1 & THYR_ENROLL_EXP == 1
label var THYR_ENROLL "Thyroid at enrollment (any trimester)"

foreach num of numlist 1/2 {
	gen THYR_ENROLL_EXP_T`num' = THYR_ENROLL_EXP if ///
	ENROLL_TRIMESTER == `num'
	
	label var THYR_ENROLL_EXP_T`num' "Expected at T`num'"

	gen THYR_ENROLL_T`num' = THYR if ///
	TYPE_VISIT ==1 & THYR_ENROLL_EXP == 1 & ENROLL_TRIMESTER == `num'
	
	label var THYR_ENROLL_T`num' "Thyroid at enrollment, T`num'"
}


*Expected at ANC32? First we need to merge in "expected" variables:

merge m:1 MOMID PREGID using "Z:\Savannah_working_files\Expected_obs-$datadate.dta" , nogen force
keep if ENROLL == 1

gen THYR_ANC32_EXP = ANC32_EXP
label var THYR_ANC32_EXP "Expected at ANC32"
gen THYR_ANC32 = THYR if THYR_ANC32_EXP ==1 & ///
TYPE_VISIT >= 4 & TYPE_VISIT <= 5
label var THYR_ANC32 "Thyroid at ANC32"


foreach var in THYR_ENROLL THYR_ENROLL_T1 THYR_ENROLL_T2 THYR_ANC32 {
	replace `var' = . if `var' == -1
}

sort SITE  MOMID PREGID LBSTDAT
collapse (firstnm) THYR_ENROLL_EXP THYR_ENROLL THYR_ENROLL_EXP_T1 THYR_ENROLL_T1 THYR_ENROLL_EXP_T2 THYR_ENROLL_T2  THYR_ANC32_EXP THYR_ANC32 ENROLL_TRIMESTER , ///
by(SITE MOMID PREGID)

replace THYR_ENROLL = 55 if THYR_ENROLL_EXP ==1 & THYR_ENROLL == .
replace THYR_ENROLL_T1=55 if ///
	ENROLL_TRIMESTER == 1 & THYR_ENROLL_T1==. & THYR_ENROLL_EXP_T1==1
replace THYR_ENROLL_T2=55 if ///
	ENROLL_TRIMESTER == 2 & THYR_ENROLL_T2==. & THYR_ENROLL_EXP_T2==1
replace THYR_ANC32 = 55 if THYR_ANC32_EXP ==1 & THYR_ANC32 == . 

gen THYR_ENROLL_DENOM = 1 if THYR_ENROLL_EXP==1 & THYR_ENROLL<55
gen THYR_ANC32_DENOM = 1 if THYR_ANC32_EXP==1 & THYR_ANC32<55

foreach num of numlist 1/2 {
	replace THYR_ENROLL_T`num' = . if ENROLL_TRIMESTER != `num'
	replace THYR_ENROLL_EXP_T`num' = . if  ENROLL_TRIMESTER != `num'
}

foreach var in  THYR_ANC32 THYR_ANC32_DENOM THYR_ANC32_EXP  {
	replace `var'=. if (THYR_ANC32_EXP!=1  & !inrange(THYR_ANC32,0,5) )
}

keep SITE MOMID PREGID THYR_ENROLL_EXP THYR_ENROLL THYR_ENROLL_EXP_T1 THYR_ENROLL_T1 THYR_ENROLL_EXP_T2 THYR_ENROLL_T2 THYR_ANC32_EXP THYR_ANC32 THYR_ENROLL_DENOM THYR_ANC32_DENOM
label drop THYR
label define THYR ///
55"Missing" ///
1"Normal" ///
2"Subclinical hyperthyroid" ///
3"Overt hyperthyroid" ///
4"Subclinical hypothyroid" ///
5"Overt hypothyroid" ///
0"Unclassified"
label val THYR_ENROLL THYR_ENROLL_T1 THYR_ENROLL_T2 THYR_ANC32 THYR  

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

sort SITE MOMID PREGID
order SITE MOMID PREGID THYR_ENROLL_EXP THYR_ENROLL_DENOM THYR_ENROLL THYR_ENROLL_EXP_T1 THYR_ENROLL_T1 THYR_ENROLL_EXP_T2 THYR_ENROLL_T2 THYR_ANC32_EXP THYR_ANC32_DENOM THYR_ANC32 THYR_EVER_DENOM THYR_EVER

label data "Data date: $datadate; `c(username)' modified `c(current_date)'"
local datalabel: data label
disp "`datalabel'" //displays dataset label you just assigned

save "$wrk/MAT_THYR-$today.dta" , replace

*Review and save to outcomes folder:
*save "$outcomes/MAT_THYR.dta" , replace //keep commented until ready to add to outcomes folder

/*
****SANKEY PLOT ****
**DRAFT
use "$wrk/thyr_allvisits.dta", clear

**Create a sankey plot
gen THYR_ENROLL = THYR if TYPE_VISIT == 1 
gen THYR_ANC20  = THYR if TYPE_VISIT == 2 
gen THYR_ANC32  = THYR if TYPE_VISIT == 4  
gen THYR_ANC36  = THYR if TYPE_VISIT == 5

sort SITE MOMID PREGID THYROID_LBTSTDAT

collapse (firstnm) THYR_ENROLL THYR_ANC20 THYR_ANC32 THYR_ANC36 , by(SITE MOMID PREGID) 

replace THYR_ANC20 = 55 if THYR_ANC20 == -1 | THYR_ANC20 == . 
replace THYR_ANC20 = THYR_ENROLL if THYR_ANC20 == 55 & THYR_ENROLL!=.
//replace THYR_ANC20 with enrollment if missing at anc 20
replace THYR_ANC32 = 55 if THYR_ANC32 == -1 | THYR_ANC32 == . 
replace THYR_ANC32 = THYR_ANC36 if THYR_ANC32 == 55 & THYR_ANC36!=.

gen newid = _n

collapse (count) newid, by(THYR_ANC20 THYR_ANC32 )

foreach var in  THYR_ANC20 THYR_ANC32  {
	replace `var' = 55 if `var' == -1
	replace `var' = 55 if `var' == . 
}

label define THYR_miss ///
55"Missing" ///
1"Normal" ///
2"Subclinical hyperthyroid" ///
3"Overt hyperthyroid" ///
4"Subclinical hypothyroid" ///
5"Overt hypothyroid"
label val THYR_ANC20 THYR_ANC32 THYR_miss

	sankey_plot  THYR_ANC20 THYR_ANC32 , wide width(newid) ///
		fillcolor(%50) ///
		xlabel("",nogrid) gap(0.1) tight ///
		title("Thyroid function")
		graph export "$wrk/ThyroidFunctionANC.png", ///
		as(png) name("Graph")

		*/

/*

global TSH_QUERY 5
global TSH_UNIT "uIU/m"
global TSH_T1_LL 0.6
global TSH_T1_UL 3.4
global TSH_T2_LL 0.37
global TSH_T2_UL 3.6
global TSH_T3_LL 0.38
global TSH_T3_UL 4.04

global FREET4_QUERY 2.25
global FREET4_UNIT "ng/dL"
global FREET4_T1_LL 
global FREET4_T1_UL
global FREET4_T2_LL
global FREET4_T2_UL
global FREET4_T3_LL
global FREET4_T3_UL



putdocx begin
putdocx paragraph
putdocx text ("Report of thyroid stimulating hormone (TSH), free T4, and free T3 , by site") , bold underline font (, 18)
putdocx paragraph
putdocx text ("Date run: 2024-08-14; Date of data upload: 2024-07-26")
putdocx paragraph
putdocx text ("Section 1: thyroid stimulating hormone (TSH)") , bold  font (, 16)
	putdocx paragraph 
	putdocx text ("Reference ranges for TSH for Trimester 1: 0.6-3.4 uIU/mL")
	putdocx paragraph
///TSH///
	* TRIMESTER 1
	hist THYROID_TSH_LBORRES if THYR_TRIMESTER == 1, by(SITE, col(1) note("Red lines: reference ranges") title("TSH, Trimester 1") subtitle("All values")) xline(0.6,lcolor(red)) xline(3.4,lcolor(red)) color(navy%50)
	graph export "$gph/TSH_T1_All.png", replace 
	putdocx image "$gph/TSH_T1_All.png", height(3)

	putdocx paragraph
	
	hist THYROID_TSH_LBORRES if THYR_TRIMESTER == 1 & THYROID_TSH_LBORRES<5, by(SITE, col(1) note("Red lines: reference ranges") title("TSH, Trimester 1") subtitle("TSH < 5 uIU/mL")) xline(0.6,lcolor(red)) xline(3.4,lcolor(red)) color(navy%50)
	graph export "$gph/TSH_T1_LT5.png", replace 
	putdocx image "$gph/TSH_T1_LT5.png", height(3)
	putdocx sectionbreak
	
	* TRIMESTER2
	putdocx paragraph
	putdocx text (("Reference ranges for TSH for Trimester 2: 0.37-3.6 uIU/mL"))
	putdocx paragraph
	hist THYROID_TSH_LBORRES if THYR_TRIMESTER == 2, by(SITE, col(1) note("Red lines: reference ranges") title("TSH, Trimester 2") subtitle("All values")) xline(0.37,lcolor(red)) xline(3.6,lcolor(red)) color(navy%50)
	graph export "$gph/TSH_T2_All.png", replace
	
	putdocx image "$gph/TSH_T2_All.png"
	putdocx paragraph
	hist THYROID_TSH_LBORRES if THYR_TRIMESTER == 2 & THYROID_TSH_LBORRES<5, by(SITE, col(1) note("Red lines: reference ranges") title("TSH, Trimester 2") subtitle("TSH < 5 uIU/mL")) xline(0.37,lcolor(red)) xline(3.6,lcolor(red)) color(navy%50)
	graph export "$gph/TSH_T2_LT5.png", replace
	putdocx image "$gph/TSH_T2_LT5.png"
	putdocx sectionbreak
	
	* TRIMESTER3
	putdocx paragraph
	putdocx text (("Reference ranges for TSH for Trimester 3: 0.38-4.04 uIU/mL"))
	putdocx paragraph
	hist THYROID_TSH_LBORRES if THYR_TRIMESTER == 3, by(SITE, col(1) note("Red lines: reference ranges") title("TSH, Trimester 3") subtitle("All values")) xline(0.38,lcolor(red)) xline(4.04,lcolor(red)) color(navy%50)
	graph export "$gph/TSH_T3_All.png", replace
	putdocx image "$gph/TSH_T3_All.png"
	putdocx paragraph
	
	hist THYROID_TSH_LBORRES if THYR_TRIMESTER == 3 & THYROID_TSH_LBORRES<5, by(SITE, col(1) note("Red lines: reference ranges") title("TSH, Trimester 3") subtitle("TSH < 5 uIU/mL")) xline(0.38,lcolor(red)) xline(4.04,lcolor(red)) color(navy%50) xsize(10) ysize(5)
	graph export "$gph/TSH_T3_LT5.png", replace
	putdocx image "$gph/TSH_T3_LT5.png"
	putdocx sectionbreak
	
///FREE T4///
	putdocx paragraph
	putdocx text ("Section 2: free T4") , bold  font (, 16)
	* TRIMESTER 1
	putdocx paragraph
	putdocx text (("Reference ranges for Free T4 for Trimester 1: 0.8-1.2 ng/dL"))
	putdocx paragraph
	hist THYROID_FREET4_LBORRES if THYR_TRIMESTER == 1, by(SITE, col(1) note("Red lines: reference ranges") title("Free T4, Trimester 1") subtitle("All values")) xline(0.8,lcolor(red)) xline(1.2,lcolor(red)) color(olive%50)
	graph export "$gph/FREET4_T1_All.png", replace
	putdocx image "$gph/FREET4_T1_All.png"
	putdocx paragraph
	
	hist THYROID_FREET4_LBORRES if THYR_TRIMESTER == 1 & THYROID_FREET4_LBORRES<2.25, by(SITE, col(1) note("Red lines: reference ranges") title("Free T4, Trimester 1") subtitle("Free T4 < 2.25 ng/dL")) xline(0.8,lcolor(red)) xline(1.2,lcolor(red)) color(olive%50)
	graph export "$gph/FREET4_T1_LT2.png", replace
	putdocx image "$gph/FREET4_T1_LT2.png"
	putdocx sectionbreak
	
		* TRIMESTER2
	putdocx paragraph
	putdocx text ("Reference ranges for Free T4 for Trimester 2: 0.6-1 ng/dL")
	putdocx paragraph
	hist THYROID_FREET4_LBORRES if THYR_TRIMESTER == 2, by(SITE, col(1) note("Red lines: reference ranges") title("Free T4, Trimester 2") subtitle("All values")) xline(0.6,lcolor(red)) xline(1,lcolor(red)) color(olive%50)
	graph export "$gph/FREET4_T2_All.png", replace
	putdocx image "$gph/FREET4_T2_All.png"
	putdocx paragraph
	
	hist THYROID_FREET4_LBORRES if THYR_TRIMESTER == 2 & THYROID_FREET4_LBORRES<2.25, by(SITE, col(1) note("Red lines: reference ranges") title("Free T4, Trimester 2") subtitle("Free T4 < 2.25 ng/dL")) xline(0.6,lcolor(red)) xline(1,lcolor(red)) color(olive%50)
	graph export "$gph/FREET4_T2_LT2.png", replace
	putdocx image  "$gph/FREET4_T2_LT2.png"
	putdocx sectionbreak
	
		* TRIMESTER3
	putdocx paragraph
	putdocx text ("Reference ranges for Free T4 for Trimester 3: 0.5-0.8 ng/dL")
	putdocx paragraph
	hist THYROID_FREET4_LBORRES if THYR_TRIMESTER == 3, by(SITE, col(1) note("Red lines: reference ranges") title("Free T4, Trimester 3") subtitle("All values")) xline(0.5,lcolor(red)) xline(0.8,lcolor(red)) color(olive%50)
	graph export "$gph/FREET4_T3_All.png", replace
	putdocx image "$gph/FREET4_T3_All.png"
	putdocx paragraph
	
	hist THYROID_FREET4_LBORRES if THYR_TRIMESTER == 3 & THYROID_FREET4_LBORRES<2.25, by(SITE, col(1) note("Red lines: reference ranges") title("Free T4, Trimester 3") subtitle("Free T4 < 2.25 ng/dL")) xline(0.5,lcolor(red)) xline(0.8,lcolor(red)) color(olive%50)
	graph export "$gph/FREET4_T3_LT2.png", replace
	putdocx image "$gph/FREET4_T3_LT2.png"
	putdocx sectionbreak
	
///FREE T3 ///
	putdocx paragraph
	putdocx text ("Section 3: free T3") , bold  font (, 16)
	putdocx paragraph

	* TRIMESTER 1
	putdocx text ("Reference ranges for Free T4 for Trimester 1: 4.1-4.4 pg/mL")
	putdocx paragraph
	hist THYROID_FREET3_LBORRES if THYR_TRIMESTER == 1, by(SITE, col(1) note("Red lines: reference ranges") title("Free T3, Trimester 1") subtitle("All values")) xline(4.1,lcolor(red)) xline(4.4,lcolor(red)) color(gs4%50)
	graph export "$gph/FREET3_T1_All.png", replace
	putdocx image "$gph/FREET3_T1_All.png"
	putdocx paragraph
	
	hist THYROID_FREET3_LBORRES if THYR_TRIMESTER == 1 & THYROID_FREET3_LBORRES<5, by(SITE, col(1) note("Red lines: reference ranges") title("Free T3, Trimester 1") subtitle("Free T3 < 5 pg/mL")) xline(4.1,lcolor(red)) xline(4.4,lcolor(red)) color(gs4%50)
	graph export "$gph/FREET3_T1_LT5.png", replace
	putdocx image  "$gph/FREET3_T1_LT5.png"
	putdocx sectionbreak
	
	* TRIMESTER 2
	putdocx paragraph
	putdocx text ("Reference ranges for Free T4 for Trimester 2: 4-4.2 pg/mL")
	putdocx paragraph
	hist THYROID_FREET3_LBORRES if THYR_TRIMESTER == 2, by(SITE, col(1) note("Red lines: reference ranges") title("Free T3, Trimester 2") subtitle("All values")) xline(4,lcolor(red)) xline(4.2,lcolor(red)) color(gs4%50)
	graph export "$gph/FREET3_T2_All.png", replace
	putdocx image "$gph/FREET3_T2_All.png"
	putdocx paragraph
	
	hist THYROID_FREET3_LBORRES if THYR_TRIMESTER == 2 & THYROID_FREET3_LBORRES<5, by(SITE, col(1) note("Red lines: reference ranges") title("Free T3, Trimester 2") subtitle("Free T3 < 5 pg/mL")) xline(4,lcolor(red)) xline(4.2,lcolor(red)) color(gs4%50)
	graph export "$gph/FREET3_T2_LT5.png", replace
	putdocx image "$gph/FREET3_T2_LT5.png"
	putdocx sectionbreak
	
		* TRIMESTER 3
	putdocx paragraph
	putdocx text ("Reference ranges for Free T4 for Trimester 3: unknown; we have shown 4-4.2 pg/mL (trimester 2 values)")
	putdocx paragraph	
	hist THYROID_FREET3_LBORRES if THYR_TRIMESTER == 3, by(SITE, col(1) note("Reference ranges for Trimester 3 unknown, shown are the reference ranges for trimester 2") title("Free T3, Trimester 3") subtitle("All values")) xline(4,lcolor(red)) xline(4.2,lcolor(red)) color(gs4%50)
	graph export "$gph/FREET3_T3_All.png", replace
	putdocx image "$gph/FREET3_T3_All.png"
	putdocx paragraph
	
	hist THYROID_FREET3_LBORRES if THYR_TRIMESTER == 3 & THYROID_FREET3_LBORRES<5, by(SITE, col(1) note("Reference ranges for Trimester 3 unknown, shown are the reference ranges for trimester 2") title("Free T3, Trimester 3") subtitle("Free T3 < 5 pg/mL")) xline(4,lcolor(red)) xline(4.2,lcolor(red)) color(gs4%50)
	graph export "$gph/FREET3_T3_LT5.png", replace
	putdocx image "$gph/FREET3_T3_LT5.png"

	putdocx save "$gph/Thyroid-Report-2024-08-14.docx", replace

*/		
		
