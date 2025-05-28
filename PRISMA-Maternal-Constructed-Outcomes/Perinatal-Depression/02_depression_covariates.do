*Depression file: add demographics & covariates


***********************************
***Part 1: Directories and data import
***********************************
* Update each new data date:
global datadate "2025-04-18"


global runqueries = 1
//logical: export queries?


*******************************************************************
global da "Z:\Stacked Data/$datadate"
global outcomes "Z:\Outcome Data/$datadate"

global wrk "Z:\Savannah_working_files\depression/$datadate"
// make sure this is a secure location, as we will save data files here
cap mkdir "$wrk"
cd "$wrk"

*Save queries:
global queries "Z:\Savannah_working_files\depression/$datadate\queries"
	cap mkdir "$queries" //create the folder if it doesn't exist
	//save query reports here

*Save reports:
global reports "Z:\Savannah_working_files\depression/$datadate\reports"
	cap mkdir "$reports" //create the folder if it doesn't exist
	//save output reports here
	
	*Get today's date:
local date: di %td_CCYY_NN_DD daily("`c(current_date)'", "DMY")
global today = subinstr(strltrim("`date'"), " ", "-", .)
*******************************************************************

*Bring in MAT_DEMOGRAPHICS:

cap use "$wrk/MAT_DEMOGRAPHIC.dta"
if _rc > 0 {
	use "$outcomes/MAT_DEMOGRAPHIC.dta", clear
	save "$wrk/MAT_DEMOGRAPHIC.dta", replace
}


/*	Relevant demographics:


	- Maternal Age
	- Marital Status
	- Maternal BMI 
	- Maternal Education
	- Substance Usage (alcohol/tobacco/betel nut)
	- Paid work?
	- Wealth Quintile?
	- Previous Miscarriage/Stillbirth
	- Parity

*/

	use "$wrk/MAT_DEMOGRAPHIC.dta"
	label var MAT_AGE "Maternal age at enrollment"
	sum MAT_AGE
	return list //find the max
	local ceiling = `r(max)' + 1 //the "ceiling" is max + 1 
	
	egen MAT_AGE_CAT = cut(MAT_AGE), at (0 20 30 `ceiling') icodes
		//creates categories as 0,1, 2-ceiling
	label define MAT_AGE_CAT 0"<20" 1"20-29" 2"30+" 55"Missing", replace
	label val MAT_AGE_CAT MAT_AGE_CAT
	tabstat MAT_AGE, by(MAT_AGE_CAT) stats(min max)
	
	label var 	 MARRY_STATUS "Marital status"
	label define MARRY_STATUS ///
					1 "Married" ///
					2 "Cohabitating" ///
					3 "Divorced" ///
					4 "Widowed" ///
					5 "Single" ///
					55 "Missing", replace
	label val 	 MARRY_STATUS MARRY_STATUS				

	label var BMI_ENROLL "BMI at enrollment"
	sum BMI_ENROLL
	return list //find the max
	local ceiling = `r(max)' + 1 //the "ceiling" is max + 1 
	
	egen BMI_ENROLL_CAT = cut(BMI_ENROLL), at (0 18.5 25 30 `ceiling') icodes
		//creates categories as 0,1, 2-ceiling
	label define BMI_ENROLL_CAT 0"<18.5" 1"18.5-24.9" 2"25-29.9" 3"30+" 55"Missing", replace
	label val BMI_ENROLL_CAT BMI_ENROLL_CAT
	label var BMI_ENROLL_CAT "BMI at enrollment"
	tabstat BMI_ENROLL, by(BMI_ENROLL_CAT) stats(min max)
	
	label var SCHOOL_YRS "Years of schooling"
	
	label var DRINK "Alcohol usage"
	
	label var CHEW_TOBACCO "Chew tobacco"
	
	label var CHEW_BETELNUT "Chew betelnut"
	
	label var PAID_WORK "Paid work"
	
	label var MISCARRIAGE "Previous miscarriage"
	
	label define yes_no_miss 1"Yes" 0"No" 55"Missing" 88"Other", replace
	label val DRINK CHEW_TOBACCO CHEW_BETELNUT SMOKE PAID_WORK MISCARRIAGE ///
		yes_no_miss
	
	label var PARITY "Parity"
	*code parity categories as 0/1/2+
	
	sum PARITY
	return list //find the max
	local ceiling = `r(max)' + 1 //the "ceiling" is max + 1 
	
	egen PARITY_CAT = cut(PARITY), at (0 1 2 `ceiling') icodes
		//creates categories as 0,1, 2-ceiling
	label define PARITY_CAT 0"0" 1"1" 2"2+" 55"Missing", replace
	label val PARITY_CAT PARITY_CAT
	label var PARITY_CAT  "Parity"
	
	*check it worked:
	bigtab PARITY PARITY_CAT
	
	
	save "$wrk/MAT_DEMOGRAPHICS_LABELED", replace
	
	
	merge 1:m SITE MOMID PREGID using "$wrk/mnh25"
	tab _merge SITE
	
	
	for var  MAT_AGE_CAT MARRY_STATUS BMI_ENROLL_CAT DRINK CHEW_TOBACCO CHEW_BETELNUT PAID_WORK MISCARRIAGE PARITY_CAT SMOKE : replace X = 55 if X == .
	for var  MAT_AGE_CAT MARRY_STATUS BMI_ENROLL_CAT DRINK CHEW_TOBACCO CHEW_BETELNUT PAID_WORK MISCARRIAGE PARITY_CAT SMOKE : decode X  , gen(X_STR)
	
	tab1  MAT_AGE_CAT MARRY_STATUS BMI_ENROLL_CAT DRINK CHEW_TOBACCO CHEW_BETELNUT SMOKE PAID_WORK MISCARRIAGE PARITY_CAT
	
	*create visit type string for visualizations:
	gen VISTYPE = "ENROLL/ANC20" if inlist(TYPE_VISIT ,1,2)
	replace VISTYPE = "ANC32/36" if inlist(TYPE_VISIT ,4,5)
	replace VISTYPE = "PNC6" if inlist(TYPE_VISIT ,10)
	
	
*************************************************************************	
**Who is missing assessments?
*************************************************************************
	replace QUERY_MISS_DEPSCORE=1 if epds_score==.
	replace QUERY_MISS_DEPSCORE=0 if inrange( epds_score,0,30)
	label define QUERY_MISS_DEPSCORE 1"Missing depression score" 0"Valid depression score"
	label val QUERY_MISS_DEPSCORE QUERY_MISS_DEPSCORE
	
*Who missed both enrollment and ANC20 visit?	
	gen anc20 = 1 if inlist(TYPE_VISIT,1,2) & QUERY_MISS_DEPSCORE == 0
	bysort MOMID PREGID: egen ANC20 = max(anc20)
	replace ANC20 = 0 if ANC20 ==.
	label define ANC20 1"Valid ANC20 score" 0"Missing ANC20", replace
	label val ANC20 ANC20
*Who missed both ANC32 & ANC 36?	
	gen anc32 = 1 if inlist(TYPE_VISIT,4,5) & QUERY_MISS_DEPSCORE == 0
	bysort MOMID PREGID: egen ANC32 = max(anc32)
*Who missed PNC6?	
	gen pnc6 = 1 if TYPE_VISIT==10 & QUERY_MISS_DEPSCORE == 0
	bysort MOMID PREGID: egen PNC6 = max(pnc6)
	
	drop anc20 anc32 pnc6

	*merge in who is "expected" (passed the window without closing out)	
	merge m:1 MOMID PREGID using "Z:\Savannah_working_files\Expected_obs-$datadate.dta", nogenerate keepusing(ANC20_EXP ANC32_EXP PNC6_EXP)

*************************************************************************	
	
	*create a numbering of visits per woman:
	bysort MOMID PREGID ( DATE TYPE_VISIT) : gen NUMVISIT = _n
	label var NUMVISIT "Visit order"
	
	save "$wrk/mnh25_long_covariates.dta", replace
	
	*If available: merge in singleton/twin/triplet
	merge m:1 MOMID PREGID using "$outcomes/MAT_PLACENTA_PREVIA.dta", nogen
	
	merge m:1 SITE MOMID PREGID using "$outcomes/MAT_HDP.dta", nogen keepusing(HTN_ANY)
	merge m:1 SITE MOMID PREGID using "$outcomes/MAT_GDM.dta", nogen keepusing(DIAB_OVERT_ANY)
	merge m:1 SITE MOMID PREGID using "$outcomes/MAT_risks.dta", nogen keepusing(WEALTH_QUINT)
	merge m:1 SITE MOMID PREGID using "$outcomes/MAT_PREVPREG_COMPLICATIONS.dta", nogen
	
	
	
	save "$wrk/mnh25_long_covariates.dta", replace
	
	
	
	
preserve
table1_mc if  NUMVISIT==1 & ANC20 == 1, by(SITE) ///
vars(MAT_AGE conts \ MARRY_STATUS cat \ BMI_ENROLL_CAT cat \ SCHOOL_YRS conts \ PARITY_CAT cat \  MISCARRIAGE cat \ PAID_WORK cat \ DRINK cat \ CHEW_TOBACCO cat \ CHEW_BETELNUT cat) ///
format(%2.0f) extraspace clear total(before) percformat(%5.1f)  percsign("") iqrmiddle(",") sdleft(" [±") sdright("]") gsdleft(" [×/") gsdright("]") onecol 
drop pvalue
table1_mc_dta2docx using "$reports/Table1_Demographics-$today.docx", replace land tablenum("Table 1") tabletitle("Characteristics by site, run $today")
restore

preserve
table1_mc if  NUMVISIT==1 & ANC20_EXP == 1, by(ANC20) ///
vars(SITE cat   \ MAT_AGE conts \ MARRY_STATUS cat \ BMI_ENROLL_CAT cat \ SCHOOL_YRS conts \ PARITY_CAT cat \  MISCARRIAGE cat \ PAID_WORK cat \ DRINK cat \ CHEW_TOBACCO cat \ CHEW_BETELNUT cat) ///
format(%2.0f) extraspace clear  percformat(%5.1f)  percsign("") iqrmiddle(",") sdleft(" [±") sdright("]") gsdleft(" [×/") gsdright("]") onecol 
table1_mc_dta2docx using "$reports/Table1_Demographics_byMiss-$today.docx", replace land tablenum("Table 1") tabletitle("Characteristics by missing, run $today")
restore
	
