**Gestational weight gain analysis**
**Begun on June 24, 2024
*Savannah O'Malley (savannah.omalley@gwu.edu)

/*
Note: data set requires the following codes:

"$outcomes/MAT_ENROLL"
"$outcomes/MAT_ENDPOINTS"
"$outcomes/MAT_PLACENTA_PREVIA.dta"

*/


***Variables:
*** - BMI at enrollment == BMI4CAT
*** - Total weight gain == GWG_TOTAL
*** - GWG adequacy == GWG_ADEQUACY


clear
global wrk "Z:\Savannah_working_files\Maternal Nutrition"
cd "$wrk"
**CHANGE THE BELOW BASED ON WHICH DATA YOU ARE WORKING WITH
global datadate "2025-04-18"
global outcomes "Z:\Outcome Data/$datadate"

global da "Z:\Stacked Data/$datadate"



import delimited "$outcomes/GWG_OUTCOME_LONG.csv", case(upper) clear
/*
Notes on this data set:
- all pregnancies with an endpoint (all PREG_END==1)
- imputed early pregnancy values (BMI)
- includes non-singleton pregnancies
- all visit types 
*/
drop V1 SCRNID

**MANY ARE STRING BUT WE NEED THEM TO BE FLOATS
foreach var in IOM_ADEQUACY BMI4CAT BMI GWG_TOTAL PREG_END TYPE_VISIT_UPDATED {
	cap replace `var' = "" if inlist(`var', "NA", "Inf")
	*before destring, replace values with "" if the values are "NA" or "Inf"
	cap destring `var' , replace
	*destring, replacing variable
}


gen 			GWG_ADEQUACY = 1 if IOM_ADEQUACY == 0
replace 		GWG_ADEQUACY = 2 if IOM_ADEQUACY == 1
replace 		GWG_ADEQUACY = 3 if IOM_ADEQUACY == 2
label define 	GWG_ADEQUACY 1"Inadequate" 2"Adequate" 3"Excessive"
label val 		GWG_ADEQUACY GWG_ADEQUACY
bigtab 			GWG_ADEQUACY IOM_ADEQUACY


rename BMI4CAT BMI4CAT_OLD
gen 		BMI4CAT = 1 if BMI4CAT_OLD == 0
replace 	BMI4CAT = 2 if BMI4CAT_OLD == 1
replace 	BMI4CAT = 3 if BMI4CAT_OLD == 2
replace 	BMI4CAT = 4 if BMI4CAT_OLD == 3
label define BMI4CAT ///
	1"Underweight" 2"Normal" ///
	3"Overweight" 4"Obese", replace
label val 	BMI4CAT BMI4CAT
tab SITE	BMI4CAT


*graph box GWG_TOTAL if GWG_TOTAL<50 & GWG_TOTAL>-10 & PREG_END==1, over(GWG_ADEQUACY) by(BMI4CAT,col(4))

bysort BMI4CAT: tabstat GWG_TOTAL, by(GWG_ADEQUACY) stats(n min  p50 max)

tabstat GWG_TOTAL, by(SITE) stats(n p10 p50 p90)

gen DATE=date( M05_ANT_PEDAT, "YMD")
format DATE %td

bysort MOMID PREGID (TYPE_VISIT DATE): 	gen VISNUM 	= _n
*by MOMID PREGID combination, number entries, sorted by type_visit / date

bysort MOMID PREGID : 					gen VISTOTAL = _N
* the total # of entries for each MOMID PREGID combination

*we only want to keep the last entry
gen lastentry = 1 if VISNUM == VISTOTAL

*	browse MOMID PREGID TYPE_VISIT DATE VISNUM VISTOTAL lastentry  
sort SITE PREGID TYPE_VISIT
keep if lastentry == 1


*confirm no duplicates
isid  MOMID PREGID

keep SITE MOMID PREGID GWG_TOTAL GWG_ADEQUACY  BMI4CAT BMI PREG_END 
gen GWG_VALID = 1 if GWG_TOTAL != .

gen BMI4CAT_D = 1 if inlist(BMI4CAT, 1, 2, 3, 4)
*participant has a valid BMI category 


label var BMI4CAT "BMI cat at enrollment"
label var GWG_ADEQUACY "GWG adequacy (IOM)"

**NEW denominator: all pregnancies with an end (PREG_END==1)

gen GWG_MISS = 1 if GWG_TOTAL==.
//option 1 = total gwg is missing
replace GWG_MISS = 2 if (GWG_TOTAL<-10 | GWG_TOTAL>30) & GWG_TOTAL != .
//option 2 = total gwg exists but implausible GWG values

 *now code missing BMI 
gen BMI4CAT_MISS = 1 if BMI>50 & BMI !=. 
replace BMI4CAT_MISS = 1 if BMI<12 
//scenario 1: implausible BMI at enrollment
	//note that 'BMI' is early pregnancy BMI

replace BMI4CAT_MISS = 2 if BMI == .
//scenario 2: missing BMI at enrollment

replace BMI4CAT_D=. if BMI4CAT_MISS == 1
*Replace denominator as missing if BMI was implausible 

*now replace BMI to missing if it is above 50
*so that outliers do not mess up averages
replace BMI = . if BMI4CAT_MISS == 1
replace BMI4CAT = . if BMI4CAT_MISS == 1

merge 1:1 MOMID PREGID using "$outcomes/MAT_PLACENTA_PREVIA.dta", keepusing(FETUS_CT_PERES_US) 
keep if _merge == 3
drop _merge


foreach var in FETUS_CT_PERES_US {
	count  if `var' !=1 
	return list 
	global nonsingleton=`r(N)'
	**record number of non-singleton pregnancies
	
	count if `var'==1
	return list
	global singleton=`r(N)'
	*record number of singleton pregnancies
}

gen SINGLETON = FETUS_CT_PERES_US
label var  SINGLETON "Number of infants"

*keep if FETUS_CT_PERES_US == 1
**keep only if singleton pregnancy


label define 	GWG_ADEQUACY 1"Inadequate" 2"Adequate" 3"Excessive",replace
label val 		GWG_ADEQUACY GWG_ADEQUACY

foreach var in GWG_TOTAL GWG_ADEQUACY GWG_VALID {
	replace `var' = . if  GWG_MISS == 1 |  GWG_MISS == 2 
	*replace all as missing if GWG was missing/implausible
}

keep  MOMID PREGID BMI BMI4CAT_MISS BMI4CAT_D BMI4CAT PREG_END GWG_MISS GWG_VALID GWG_ADEQUACY  GWG_TOTAL SINGLETON

	merge 1:1 MOMID PREGID using "$outcomes/MAT_ENDPOINTS.dta", keepusing (SITE ) 
	keep if _merge == 3
	drop _merge
	
	order SITE MOMID PREGID PREG_END ///
	GWG_TOTAL GWG_ADEQUACY GWG_VALID GWG_MISS ///
	BMI BMI4CAT BMI4CAT_MISS BMI4CAT_D 

	**Checks:

	disp as text "Number nonsingleton = "   as result $nonsingleton
	disp as text "Number expected = "  as result $singleton
	
	count if GWG_MISS<.
	assert `r(N)' < 50
	
	count if BMI4CAT_MISS <. 
	assert `r(N)' < 50
	
label data "Data date: $datadate; `c(username)' modified `c(current_date)'"
local datalabel: data label
disp "`datalabel'" //displays dataset label you just assigned

	save "$wrk/MAT_GWG-$datadate-updated.dta" , replace
	
	*save in outcome folder once reviewed:
	*save "$outcomes/MAT_GWG.dta", replace
