**Placenta Previa
*Author: Savannah O'Malley (savannah.omalley@gwu.edu)
*Begun May 30, 2024

/*
NOTE: below are the required files:

"$outcomes/MAT_ENROLL.dta"
"$outcomes/MAT_LABOR.dta"
"$outcomes/MAT_ENDPOINTS.dta"

*/

	**Update below based on date
	global datadate "2025-04-18"
	**Set up directories and paths
	
	cap mkdir "Z:\Savannah_working_files\Placenta-Previa/$datadate\"
	//auto-creates the folder if it does not already exist
	
	global wrk "Z:\Savannah_working_files\Placenta-Previa/$datadate"
	cd "$wrk"		
	
	global da "Z:\Stacked Data/$datadate"

	global outcomes "Z:\Outcome Data/$datadate"

	**Need: MNH01, MNH09

**from MNH01: PREVIA_PERES_FTS3

	import delimited "$da/mnh01_merged.csv", ///
	bindquote(strict) case(upper) clear

	rename M01_* *
	save "$wrk/mnh01.dta", replace
	
	import delimited "$da/mnh02_merged.csv", ///
	bindquote(strict) case(upper) clear

	rename M02_* *
	keep SITE SCRNID MOMID PREGID
	duplicates tag SITE SCRNID, gen(dup)
	drop if dup!=0 & MOMID==""
	merge 1:m SITE SCRNID using "$wrk/mnh01.dta", nogen
	merge m:1 MOMID PREGID using "$outcomes/MAT_ENROLL", nogen
	keep if ENROLL==1
	save "$wrk/mnh01.dta", replace

	

	foreach num of numlist 1/4 {
		gen PREVIA_FTS`num' =  PREVIA_PERES_FTS`num'
		replace PREVIA_FTS`num'=. if inlist(PREVIA_PERES_FTS`num', 55, 77,99)
	}
	gen PREVIA_M01 = max( PREVIA_FTS1, PREVIA_FTS2, PREVIA_FTS3, PREVIA_FTS4)
	replace PREVIA_M01=-5 if PREVIA_M01==.
	recode PREVIA_M01(1=0) (2=0) //1 and 2 are not previa
	recode PREVIA_M01(3=1) //only a "3" is previa
	
	//check
	bigtab PREVIA_FTS1 PREVIA_FTS2 PREVIA_FTS3 if ///
	PREVIA_M01==0 //no previa
	
	bigtab PREVIA_FTS1 PREVIA_FTS2 PREVIA_FTS3 if ///
	PREVIA_M01==1 //at least 1 previa
	
	bigtab PREVIA_PERES_FTS1 PREVIA_PERES_FTS2 PREVIA_PERES_FTS3 if ///
	PREVIA_M01==-5 //all missing
	
	gen US_DAT = date(US_OHOSTDAT, "YMD")
	gen PREVIA_M01_DATE = US_DAT if PREVIA_M01 == 1
	

	sort SITE MOMID PREGID US_DAT
	collapse (max) PREVIA_M01 ENROLL FETUS_CT_PERES_US (firstnm)  PREVIA_M01_DATE, by(SITE MOMID PREGID)
	label var PREVIA_M01 "Previa identified MNH01"
	save "$wrk/MNH01-previa.dta", replace
	
	
**from MNH09: 
	*indication for C-section is previa: CES_PRINDC_INF1_18 
	*contributing to antepartum hemorrhage: APH_FAORRES_2
	
	import delimited "$da/mnh09_merged.csv", ///
	bindquote(strict) case(upper) clear

	rename M09_* *
	
	
	gen CES_PREVIA = 1 if ///
	CES_PRINDC_INF1_18 == 1 | CES_PRINDC_INF2_18 == 1 | ///
	CES_PRINDC_INF3_18 == 1 | CES_PRINDC_INF4_18 == 1
	label var CES_PREVIA "Indication for C-section: placenta/vasa previa"
	
	label var APH_FAORRES_2 "Placenta previa contributed to antepartum hemorrhage"
	
	gen PREVIA_M09 = 1 if CES_PREVIA == 1 | APH_FAORRES_2 == 1
	//Previa if indicated for C-section or contributed to APH 
	label var PREVIA_M09 "Placenta/vasa previa recorded during L&D"
	
	save "$wrk/MNH09.dta", replace
	
	merge 1:1 MOMID PREGID using "$wrk/MNH01-previa.dta", ///
	generate(_merge2)
	keep if ENROLL == 1
	
	
	gen PREVIA = 1 if PREVIA_M01==1 | PREVIA_M09==1
	label var PREVIA ///
	"Placenta/vasa previa identified in an ultrasound or at L&D"
	
	forvalues i=1/4 {
		gen INF_DELIV`i' = date(DELIV_DSSTDAT_INF`i' , "YMD")
		replace INF_DELIV`i' = . if INF_DELIV`i' < 0
	}
	gen DOB = min(INF_DELIV1, INF_DELIV2, INF_DELIV3, INF_DELIV4)
	label var DOB "DOB of infant born first"
	format PREVIA_M01_DATE DOB %td
	
	merge 1:1 MOMID PREGID using "$outcomes/MAT_LABOR.dta", ///
	keepusing(MAT_CES_ANY)
	
	*Placenta accrete
	tab CES_PRINDC_INF1_17 CES_PRINDC_INF2_17
	gen PLACENTA_ACCRETE=0 if MAT_CES_ANY==1
	replace PLACENTA_ACCRETE = 1 if ///
	MAT_CES_ANY==1 & ///
	(CES_PRINDC_INF1_17 == 1 | CES_PRINDC_INF2_17 == 1 | ///
	CES_PRINDC_INF3_17 == 1 | CES_PRINDC_INF4_17 == 1)	
	**if c-section was done because of placenta accrete
	//denominator should be those with a c-section?
	

	
	*Placenta abruption
	gen PLACENTA_ABRUPTION = 1 if ///
	APH_FAORRES_1 ==1 | /// placental abruption leading to APH
	CES_PRINDC_INF1_6 == 1 | /// placental abruption leading to C-section
	CES_PRINDC_INF2_6 == 1 | /// placental abruption leading to C-section
	CES_PRINDC_INF3_6 == 1 | /// placental abruption leading to C-section
	CES_PRINDC_INF4_6 == 1  // placental abruption leading to C-section
	
	tab PLACENTA_ABRUPTION SITE
	
	keep SITE MOMID  PREGID  FETUS_CT_PERES_US ///
	PREVIA PREVIA_M01 PREVIA_M01_DATE PREVIA_M09 APH_FAORRES_2 CES_PREVIA ///
	DOB FETUS_CT_PERES_US INFANTS_FAORRES ///
	PLACENTA_ABRUPTION PLACENTA_ACCRETE MAT_CES_ANY
	

	merge  1:1 MOMID PREGID using "$outcomes/MAT_ENDPOINTS.dta",nogen
	
	gen SB_LB = 1 if PREG_END==1 & PREG_LOSS!=1 & MAT_DEATH!=1
	//still births and livebirths
		//all preg endpoints, minus pregnancy losses and maternal deaths
		
	keep SITE MOMID PREGID FETUS_CT_PERES_US ///
	SB_LB  MAT_CES_ANY PREVIA PLACENTA_ABRUPTION PLACENTA_ACCRETE
	
	save "$wrk/MAT_PLACENTA_PREVIA.dta", replace
	*Review and save:
	*save "$outcomes/MAT_PLACENTA_PREVIA.dta", replace
	
