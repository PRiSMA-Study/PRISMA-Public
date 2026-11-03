**Placenta Previa
*Author: Savannah O'Malley (savannah.omalley@gwu.edu)
*Begun May 30, 2024

*Updated 3/19/26 by Emma Cook
*Updated 10/19/2026 by Emma Cook

/* Placenta previa can be flagged during ANC ultrasounds, 
	or reported as an indication for C-section at MNH 		*/

/*
Inputs:
	MNH01
	MNH09
	MAT_STUDYSTATUS (if using data prior to 9/18/2026, use MAT_ENROLL + MAT_ENDPOINTS)
	MAT_PREGEND (if using data prior to 9/18/2026, use MAT_LABOR)
	
	
This code uses the user written command str2date	
*/

**# Step 0: Copy stacked data from TNT to local AWS environment and convert to 
* 			.dta fomat using `Import Data.dta'

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
		gl emma "Z:/Emma_working_files/Constructed Data_working files/MAT_PLACENTA_PREVIA"
		gl wrk "$emma/$datadate"
			cap mkdir "$wrk" // creates directory if it doesn't exist	
		gl temp "$wrk/temp"
			cap mkdir "$temp"	
		gl output "$wrk/output"
			cap mkdir "$output"


**from MNH01: PREVIA_PERES_FTS3

	use "$ldata/mnh01.dta", clear
	rename M01_* *
	save "$wrk/mnh01.dta", replace
	
	use "$ldata/mnh02.dta", clear
	rename M02_* *
	keep SITE SCRNID MOMID PREGID
	duplicates tag SITE SCRNID, gen(dup)
	drop if dup!=0 & MOMID==""
	merge 1:m SITE SCRNID using "$wrk/mnh01.dta", gen(m01_merge)
	merge m:1 MOMID PREGID using "$out/MAT_STUDYSTATUS", gen(studystat_merge)
		*for data prior to 9-18-26, use MAT_ENROLL
	keep if ENROLL==1
	
	tab m01_merge,m
	tab studystat_merge,m 
	save "$wrk/mnh01.dta", replace 
 	
*Previa identified at ultrasound (any time)
	*PREVIA_PERES_FTS* - Previa assessment, by fetus
		* 1, Neither low-lying nor previa 
		* 2, Low-lying (placental edge within 2 cm of internal os but does not reach os)
		* 3, Previa (placenta partially or completely covering internal os)

	gen US_PREVIA = 0 if inlist(PREVIA_PERES_FTS1 ,1,2,3)
	foreach num of numlist 1/4 {
		gen PREVIA_FTS`num' =  PREVIA_PERES_FTS`num'
		replace PREVIA_FTS`num'=. if inlist(PREVIA_PERES_FTS`num', 55, 77,99)
		
		
		replace US_PREVIA = 0 if ///
		inlist(PREVIA_PERES_FTS`num',1,2) & US_PREVIA ==.
			*replace to zero if previously missing but result available for fetus 2-4
		replace US_PREVIA = 1 if PREVIA_PERES_FTS`num' == 3
			*option 3 is placenta previa
	}
	 
		//check
		bigtab PREVIA_FTS1 PREVIA_FTS2 PREVIA_FTS3 if ///
		US_PREVIA==0 //no previa
		
		bigtab PREVIA_FTS1 PREVIA_FTS2 PREVIA_FTS3 if ///
		US_PREVIA==1 //at least 1 previa
	
	
	
	str2date US_OHOSTDAT PREG_START_DATE
	
	gen US_DAT = US_OHOSTDAT
	replace US_DAT =. if US_DAT < 0 
		*if default date
	replace US_DAT = . if US_DAT > date( "$datadate" , "YMD")
		*if US date is after upload date
	*gen PREVIA_M01_DATE = US_DAT if PREVIA_M01 == 1
	gen US_GA = US_DAT - PREG_START_DATE

	sort SITE MOMID PREGID US_DAT
	bysort SITE MOMID PREGID (US_DAT TYPE_VISIT) : gen visnum = _n
	keep SITE MOMID PREGID ENROLL US_DAT US_PREVIA US_GA visnum
	save "$wrk/MNH01-previa-long.dta", replace 
	
	reshape wide US_DAT US_PREVIA US_GA, i(SITE MOMID PREGID ENROLL) j(visnum)
	
	save "$wrk/MNH01-previa-wide.dta", replace
	
**from MNH09: 
	*indication for C-section is previa: CES_PRINDC_INF1_18 
	*contributing to antepartum hemorrhage: APH_FAORRES_2
	
	use "$ldata/mnh09.dta", clear
	rename M09_* *
	
	gen CES_PREVIA = 1 if ///
	CES_PRINDC_INF1_18 == 1 | CES_PRINDC_INF2_18 == 1 | ///
	CES_PRINDC_INF3_18 == 1 | CES_PRINDC_INF4_18 == 1
	label var CES_PREVIA "Indication for C-section: placenta/vasa previa"
	
	label var APH_FAORRES_2 "Placenta previa contributed to antepartum hemorrhage"
	
	gen LD_PREVIA = 1 if CES_PREVIA == 1 | APH_FAORRES_2 == 1
	//Previa if indicated for C-section or contributed to APH 
	label var LD_PREVIA "Placenta/vasa previa recorded during L&D"
	
	save "$wrk/MNH09.dta", replace
	
	merge 1:m MOMID PREGID using "$wrk/MNH01-previa-wide.dta", ///
	generate(_merge2)
	keep if ENROLL == 1
	 
	gen US_PREVIA = max(US_PREVIA1, US_PREVIA2, US_PREVIA3, US_PREVIA4, ///
		US_PREVIA5, US_PREVIA6)
	gen PREVIA = 1 if US_PREVIA==1 | LD_PREVIA==1
	label var PREVIA ///
	"Placenta/vasa previa identified in an ultrasound or at L&D"
	
	forvalues i=1/4 {
		gen INF_DELIV`i' = DELIV_DSSTDAT_INF`i'
		replace INF_DELIV`i' = . if INF_DELIV`i' < 0
	}
	gen DOB = min(INF_DELIV1, INF_DELIV2, INF_DELIV3, INF_DELIV4)
	label var DOB "DOB of infant born first"
	format  DOB %td
	
	merge m:1 MOMID PREGID using "$out/MAT_PREGEND.dta", ///
		keepusing(MAT_CES_ANY)
	*for data prior to 9-18-26, use MAT_LABOR
	
	*merge m:1 MOMID PREGID using "$datadate/MAT_PREGEND.dta", ///
	keepusing(MAT_CES_ANY)
	
	*Placenta accrete
	tab CES_PRINDC_INF1_17 CES_PRINDC_INF2_17
	gen PLACENTA_ACCRETE=0 if MAT_CES_ANY==1
	replace PLACENTA_ACCRETE = 1 if ///
	MAT_CES_ANY==1 & ///
	(CES_PRINDC_INF1_17 == 1 | CES_PRINDC_INF2_17 == 1 | ///
	CES_PRINDC_INF3_17 == 1 | CES_PRINDC_INF4_17 == 1)	
	**if c-section was done because of placenta accrete
		**denominator should be those with a c-section?
	

	
	*Placenta abruption
	gen PLACENTA_ABRUPTION = 1 if ///
	APH_FAORRES_1 == 1 	   | /// placental abruption leading to APH
	CES_PRINDC_INF1_6 == 1 | /// placental abruption indicated for C-section
	CES_PRINDC_INF2_6 == 1 | /// placental abruption indicated for C-section
	CES_PRINDC_INF3_6 == 1 | /// placental abruption indicated for C-section
	CES_PRINDC_INF4_6 == 1   // placental abruption indicated for C-section
	
	tab PLACENTA_ABRUPTION SITE
	

	

	

	merge  m:1 MOMID PREGID using "$out/MAT_STUDYSTATUS.dta",nogen keepusing(PREG_END PREG_LOSS MAT_DEATH PREG_END_DATE PREG_END_GA)
	*for data prior to 9-18-26, use MAT_ENDPOINTS
	
	replace CES_PREVIA = 0 if MAT_CES_ANY == 1 & CES_PREVIA ==.
		//cesarean but not due to previa
	replace CES_PREVIA = 77 if MAT_CES_ANY != 1
		//77 is no Cesarean
	
	gen SB_LB = 1 if PREG_END==1 & PREG_LOSS!=1 & MAT_DEATH!=1
	//still births and livebirths
		//all preg endpoints, minus pregnancy losses and maternal deaths

		keep SITE MOMID PREGID CES_PREVIA LD_PREVIA US_PREVIA1 US_DAT1 US_GA1 US_PREVIA2 US_DAT2 US_GA2 US_PREVIA3 US_DAT3 US_GA3 US_PREVIA4 US_DAT4 US_GA4 US_PREVIA5 US_DAT5 US_GA5 US_PREVIA6 US_DAT6 US_GA6 US_PREVIA PREVIA PLACENTA_ACCRETE PLACENTA_ABRUPTION SB_LB 
	  
	 	
*------------------------------------------------------------------------------
* Save
*------------------------------------------------------------------------------
label data "Data date: $datadate; `c(username)' modified `c(current_date)'"
local datalabel: data label

compress
save "$wrk/MAT_PLACENTA_PREVIA.dta", replace
save "$out/MAT_PLACENTA_PREVIA.dta", replace
export delimited using "$out/MAT_PLACENTA_PREVIA.csv", replace
export excel using "$out/MAT_PLACENTA_PREVIA.xlsx", firstrow(variables) replace
	
	
