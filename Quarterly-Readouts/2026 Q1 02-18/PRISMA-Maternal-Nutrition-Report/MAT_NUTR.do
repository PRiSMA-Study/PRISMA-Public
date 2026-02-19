**Maternal nutritional characteristics
**Micronutrient deficiencies & MCV
**Prepared by : Savannah O'Malley (savannah.omalley@gwu.edu)

**UPDATED Jan 3 2025 with new variable naming convention

**UPDATED Jan 2026 to add trimester variables

/*
Note: the file needs to call in the following files:

"$outcomes/MAT_ENROLL.dta"
"$outcomes/mat_ENDPOINTS.dta"
"expected-obs-$datadate"
"$outcomes/MAT_DEMOGRAPHIC"

*/

*******************************************************************


**Step 1: CHANGE THE BELOW BASED ON WHICH DATA YOU ARE WORKING WITH
global datadate "2026-01-30"
	*data upload date
global latest_report_date "2026-01-30"
	*date of latest quarterly report (full datasets)
global runquery 0
	*logical; run queries for this data set?
	
	
*******************************************************************

*SET DIRECTORIES
global da "Z:\Stacked Data/$datadate"
global outcomes "Z:\Outcome Data/$datadate"
global latest_out "Z:\Outcome Data/$latest_report_date"
local date: di %td_CCYY_Mon_DD daily("`c(current_date)'", "DMY")
global today = subinstr(strltrim("`date'"), " ", "-", .)
disp "$today"

global savannah "D:\Users\savannah.omalley\Documents"

cap mkdir "$savannah/nutrition"
cd "$savannah/nutrition"
global wrk "$savannah/nutrition/$datadate"
	cap mkdir "$wrk"
	*make this directory if it does not exist 

global queries "$wrk/queries"
	cap mkdir "$queries" //make this folder if it does not exist
	
*******************************************************************
*Import & clean data

use "$savannah/data/$datadate/mnh07.dta", clear
str2date M07_MAT_SPEC_COLLECT_DAT
gen DATE = M07_MAT_SPEC_COLLECT_DAT
replace DATE = . if DATE <0
label var M07_MAT_BLOOD_SPEC_1 "EDTA"
label var M07_MAT_BLOOD_SPEC_2 "Sodium heparin"
label var M07_MAT_BLOOD_SPEC_3 "Serum separator"
label var M07_MAT_BLOOD_SPEC_4 "Trace free"
label var M07_MAT_BLOOD_SPEC_5 "Lithium heparin"
label var M07_MAT_BLOOD_SPEC_6 "Fluoride"
label var M07_MAT_BLOOD_SPEC_77 "No blood collected"
label var M07_MAT_BLOOD_SPEC_88 "Other blood collected"
label define yesno 1"Yes" 0"No", replace
label val M07_MAT_BLOOD_SPEC_1 M07_MAT_BLOOD_SPEC_2 M07_MAT_BLOOD_SPEC_3 M07_MAT_BLOOD_SPEC_4 M07_MAT_BLOOD_SPEC_5 M07_MAT_BLOOD_SPEC_6 M07_MAT_BLOOD_SPEC_77 M07_MAT_BLOOD_SPEC_88 yesno
	
gen bloodcollected = .
for var M07_MAT_BLOOD_SPEC_1 M07_MAT_BLOOD_SPEC_2 M07_MAT_BLOOD_SPEC_3 M07_MAT_BLOOD_SPEC_4 M07_MAT_BLOOD_SPEC_5 M07_MAT_BLOOD_SPEC_6  M07_MAT_BLOOD_SPEC_88 : replace bloodcollected = 1 if X == 1	

label var bloodcollected "Blood collected"

keep SITE MOMID PREGID M07_FORMCOMPLDAT_MNH07 M07_LB_REMAPP3 M07_LB_REMAPP3_TRI M07_MAT_BLOOD_SPEC_1 M07_MAT_BLOOD_SPEC_2 M07_MAT_BLOOD_SPEC_3 M07_MAT_BLOOD_SPEC_4 M07_MAT_BLOOD_SPEC_5 M07_MAT_BLOOD_SPEC_6 M07_MAT_BLOOD_SPEC_77 M07_MAT_BLOOD_SPEC_88 M07_MAT_BLOOD_SPEC_2 M07_MAT_BLOOD_SPEC_3 M07_MAT_BLOOD_SPEC_4 M07_MAT_BLOOD_SPEC_5 M07_MAT_BLOOD_SPEC_6 M07_MAT_BLOOD_SPEC_77 M07_MAT_BLOOD_SPEC_88 M07_MAT_HELM_SPEC_COLLECT M07_MAT_TB_SPEC_COLLECT M07_MAT_URINE_COLLECT_YN M07_MAT_VISIT_MNH07 M07_MAT_VISIT_OTHR_MNH07 M07_MAT_VITAL_MNH07 M07_TYPE_VISIT DATE M07_MAT_SPEC_COLLECT_DAT bloodcollected

order SITE MOMID PREGID M07_TYPE_VISIT DATE bloodcollected
rename M07_TYPE_VISIT TYPE_VISIT

*in case of duplicates, take whichever form was submitted later (assume this is a correction)
duplicates tag SITE MOMID PREGID TYPE_VISIT DATE, gen(dup)
bysort PREGID (M07_FORMCOMPLDAT_MNH07): gen formnum = _n
list SITE PREGID DATE M07_FORMCOMPLDAT_MNH07 dup formnum if dup > 0 
drop if dup >0 & formnum>1
drop dup formnum

save "$wrk/mnh07.dta", replace
	
use "$savannah/data/$datadate/mnh08.dta", clear 

cap drop V1 

rename  M08_* *

#delimit ;
keep SITE MOMID PREGID  MAT_VISIT_MNH08 MAT_VISIT_OTHR_MNH08 TYPE_VISIT LBSTDAT  LB_REMAPP*
CBC_MCV_LBORRES 
CBC_HCT_LBORRES
CBC_LBPERF_9
CBC_HB_LBORRES
MN_LBPERF_1
VITB12_COB_LBORRES
VITB12_COB_LBTSTDAT
MN_LBPERF_2
VITB12_HOL_LBORRES
VITB12_HOL_LBTSTDAT
MN_LBPERF_3
FOLATE_PLASMA_NMOLL_LBORRES
MN_LBPERF_4
MN_LBPERF_5
IRON_HEP_LBORRES
IRON_HEP_LBTSTDAT
MN_LBPERF_6
IRON_TOT_UGDL_LBORRES
IRON_TIBC_LBTSTDAT
MN_LBPERF_7
VITA_UGDL_LBORRES
VITA_LBTSTDAT
MN_LBPERF_8
FERRITIN_LBORRES 
MN_LBPERF_9
IODINE_LBORRES
MN_LBPERF_10
TRANSFERRIN_LBORRES 
MN_LBPERF_11
RBP4_LBORRES
MN_LBPERF_12
CRP_LBORRES 
MN_LBPERF_13
AGP_LBORRES 
MN_LBPERF_14
HRP_LBORRES
QUANSYS_LBTSTDAT
MN_LBPERF_15
FOLATE_RBC_NMOLL_LBORRES
FOLATE_RBC_LBTSTDAT
 RBC_LBPERF_2
 RBC_THALA_*
 MALBL_*
 BLEAD_*
 PLACMAL_* 
 SCHISTO_*
 HELM_*
 RBC_MORPH_* 
 BLD_MORPH_*
 RBC_MORPH_LBORRES
 WBC_MORPH_LBORRES
 PL_MORPH_LBORRES
 PARA_MORPH_LBORRES
;

order SITE MOMID PREGID  MAT_VISIT_MNH08 MAT_VISIT_OTHR_MNH08 TYPE_VISIT LBSTDAT  
CBC_MCV_LBORRES 
CBC_HCT_LBORRES
CBC_LBPERF_9
CBC_HB_LBORRES
MN_LBPERF_1
VITB12_COB_LBORRES
VITB12_COB_LBTSTDAT
MN_LBPERF_2
VITB12_HOL_LBORRES
VITB12_HOL_LBTSTDAT
MN_LBPERF_3
FOLATE_PLASMA_NMOLL_LBORRES
MN_LBPERF_4
MN_LBPERF_5
IRON_HEP_LBORRES
IRON_HEP_LBTSTDAT
MN_LBPERF_6
IRON_TOT_UGDL_LBORRES
IRON_TIBC_LBTSTDAT
MN_LBPERF_7
VITA_UGDL_LBORRES
VITA_LBTSTDAT
MN_LBPERF_8
FERRITIN_LBORRES
MN_LBPERF_9
IODINE_LBORRES
MN_LBPERF_10
TRANSFERRIN_LBORRES
MN_LBPERF_11
RBP4_LBORRES
MN_LBPERF_12
CRP_LBORRES
MN_LBPERF_13
AGP_LBORRES
MN_LBPERF_14
HRP_LBORRES
QUANSYS_LBTSTDAT
MN_LBPERF_15
FOLATE_RBC_NMOLL_LBORRES
FOLATE_RBC_LBTSTDAT
RBC_LBPERF_2
 RBC_THALA_*
;
#delimit cr


**Set default values to missing
foreach var in  ///
MN_LBPERF_1 CBC_MCV_LBORRES CBC_HCT_LBORRES VITB12_COB_LBORRES  MN_LBPERF_2  VITB12_HOL_LBORRES  MN_LBPERF_3  FOLATE_PLASMA_NMOLL_LBORRES  MN_LBPERF_8  FERRITIN_LBORRES SF_ADJ STFR_ADJ  MN_LBPERF_9  IODINE_LBORRES  MN_LBPERF_10 IRON_HEP_LBORRES IRON_TOT_UGDL_LBORRES  TRANSFERRIN_LBORRES  MN_LBPERF_11  RBP4_LBORRES  MN_LBPERF_12  CRP_LBORRES  MN_LBPERF_13  AGP_LBORRES VITA_UGDL_LBORRES HRP_LBORRES  FOLATE_RBC_NMOLL_LBORRES RBC_THALA_1 RBC_THALA_10 RBC_THALA_11 RBC_THALA_12 RBC_THALA_13 RBC_THALA_14 RBC_THALA_15 RBC_THALA_16 RBC_THALA_17 RBC_THALA_18 RBC_THALA_19 RBC_THALA_2 RBC_THALA_3 RBC_THALA_4 RBC_THALA_5 RBC_THALA_6 RBC_THALA_7 RBC_THALA_8 RBC_THALA_9 RBC_THALA_LBORRES PL_MORPH_LBORRES WBC_MORPH_LBORRES PARA_MORPH_LBORRES RBC_MORPH_LBORRES  {
	cap replace `var'= "" if `var'=="NA"
	cap destring `var', replace
}

*Replace default values of -5 and -7
foreach var in  MN_LBPERF_1 CBC_MCV_LBORRES CBC_HCT_LBORRES VITB12_COB_LBORRES  MN_LBPERF_2  VITB12_HOL_LBORRES  MN_LBPERF_3  FOLATE_PLASMA_NMOLL_LBORRES  MN_LBPERF_8  FERRITIN_LBORRES  MN_LBPERF_9  IODINE_LBORRES  MN_LBPERF_10 IRON_HEP_LBORRES IRON_TOT_UGDL_LBORRES  TRANSFERRIN_LBORRES  MN_LBPERF_11  RBP4_LBORRES  MN_LBPERF_12  CRP_LBORRES  MN_LBPERF_13  AGP_LBORRES VITA_UGDL_LBORRES HRP_LBORRES  FOLATE_RBC_NMOLL_LBORRES RBC_THALA_1 RBC_THALA_10 RBC_THALA_11 RBC_THALA_12 RBC_THALA_13 RBC_THALA_14 RBC_THALA_15 RBC_THALA_16 RBC_THALA_17 RBC_THALA_18 RBC_THALA_19 RBC_THALA_2 RBC_THALA_3 RBC_THALA_4 RBC_THALA_5 RBC_THALA_6 RBC_THALA_7 RBC_THALA_8 RBC_THALA_9 RBC_THALA_LBORRES BLEAD_LBORRES   SCHISTO_STOOL_CT_1 SCHISTO_STOOL_CT_2 SCHISTO_STOOL_CT_3 SCHISTO_STOOL_CT_4 SCHISTO_STOOL_CT_5  {
	replace `var' = . if `var' <0 
}


**these are categorical variables, 0/1/55/77
foreach var in  MN_LBPERF_1     MN_LBPERF_2    MN_LBPERF_3    MN_LBPERF_8    MN_LBPERF_9    MN_LBPERF_10      MN_LBPERF_11    MN_LBPERF_12    MN_LBPERF_13       RBC_THALA_1 RBC_THALA_10 RBC_THALA_11 RBC_THALA_12 RBC_THALA_13 RBC_THALA_14 RBC_THALA_15 RBC_THALA_16 RBC_THALA_17 RBC_THALA_18 RBC_THALA_19 RBC_THALA_2 RBC_THALA_3 RBC_THALA_4 RBC_THALA_5 RBC_THALA_6 RBC_THALA_7 RBC_THALA_8 RBC_THALA_9 RBC_THALA_LBORRES  {
	replace `var' = . if  `var' == 55 | `var'==77
}
 
*Label vars
label var MN_LBPERF_2 "Performed holotranscobalamin test"
label var MN_LBPERF_3 "Performed folate-blood serum test"
label var MN_LBPERF_8 "Performed ferritin test"
label var MN_LBPERF_9 "Performed Tg test"
label var MN_LBPERF_10 "Performed serum transferrin receptor test"
label var MN_LBPERF_11 "Performed RBP4 test"
label var MN_LBPERF_12 "Performed CRP test"
label var MN_LBPERF_13 "Performed AGP test"
label var CBC_MCV_LBORRES "Mean corpuscular volume (fL)"
label var VITB12_HOL_LBORRES "Holotranscobalamin (fmol/mL)"
label var VITB12_COB_LBORRES "Total cobalamin (pg/mL)"
label var FOLATE_PLASMA_NMOLL_LBORRES "Folate (blood serum) nmol/L"
label var IODINE_LBORRES "Tg (ug/L)"
label var TRANSFERRIN_LBORRES "Serum transferrin receptor (mg/L)"
label var FERRITIN_LBORRES "Ferritin (ug/L)"
label var RBC_THALA_1 "SS" //sickle cell
label var RBC_THALA_2 "SC" //sickle cell
label var RBC_THALA_3 "SE" //sickle cell
label var RBC_THALA_4 "CC" //other hemoglobinopathy
label var RBC_THALA_5 "SD-Punjab" //sickle cell
label var RBC_THALA_6 "SBthal" //thalassemia & sickle cell
label var RBC_THALA_7 "EBthal" //thalassemia
label var RBC_THALA_8 "CBthal" //thalassemia
label var RBC_THALA_9 "CD-Punjab ED-Punjab" //other hemoglobinopathy
label var RBC_THALA_10 "D-D-Punjab" //other hemoglobinopathy
label var RBC_THALA_11 "D-PunjabBthal"  //thalassemia
label var RBC_THALA_12 "Thalassemia major" //thalassemia
label var RBC_THALA_13 "Thalassemia intermedia" //thalassemia
label var RBC_THALA_14 "Alpha thalassemia" //thalassemia
label var RBC_THALA_15 "F" //persistent fetal hemoglobin
label var RBC_THALA_16 "SA" //sickle cell trait
label var RBC_THALA_17 "Thalassemia minor/trait"
label var RBC_THALA_18 "AC" //other hemoglobinopathy
label var RBC_THALA_19 "Other hemoglobinopathy or thalassemia"

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
	14"14. Non-scheduled PNC visit for routine care" , replace
	label val TYPE_VISIT TYPE_VISIT

	
	label define MAT_VISIT ///
	1"1. Yes in person" ///
	2"2. Yes by phone" ///
	3"3. No; medical issue" ///
	4"4. No, temp. absent" ///
	5"5. No, temp. refused" ///
	6"6. No, moved away" ///
	7"7. No, withdrew consent" ///
	8"8. No, woman died" ///
	88"88. Other" , replace
	label val MAT_VISIT_MNH08 MAT_VISIT

	gen uploaddate="$datadate"
	gen UploadDate=date(uploaddate, "YMD")
	format UploadDate %td
	drop uploaddate
	
	str2date LBSTDAT
	gen DATE= LBSTDAT
	format DATE %td
	replace DATE=. if DATE<0
	
	*which observations had a lab performed?
	gen labperformed = .
	for var CBC_LBPERF_9 MN_LBPERF_1 MN_LBPERF_2 MN_LBPERF_3 MN_LBPERF_4 MN_LBPERF_5 MN_LBPERF_6 MN_LBPERF_7 MN_LBPERF_8 MN_LBPERF_9 MN_LBPERF_10 MN_LBPERF_11 MN_LBPERF_12 MN_LBPERF_13 MN_LBPERF_14 MN_LBPERF_15 RBC_LBPERF_2 BLD_MORPH_LBPERF_1 BLD_MORPH_LBPERF_2 BLD_MORPH_LBPERF_3 BLD_MORPH_LBPERF_4 BLEAD_LBPERF_1 HELM_LBPERF_1 MALBL_LBPERF_1 PLACMAL_LBPERF_1 SCHISTO_LBPERF_1 SCHISTO_LBPERF_STOOL : replace labperformed = 1 if X == 1
	
	merge m:1 SITE MOMID PREGID TYPE_VISIT DATE using "$wrk/mnh07", gen(merge_mnh07)
	
*query: visit incomplete/phone visit, but test was performed
		if $runquery == 1 {
		levelsof(SITE) if labperformed==1 & inlist(MAT_VISIT_MNH08,2,4,5,6) , local(sitelev) clean
		foreach site of local sitelev {
			export excel SITE MOMID PREGID LBSTDAT MAT_VISIT_MNH08 TYPE_VISIT labperformed using "$queries/`site'-nutrition-queries-$datadate.xlsx"  if SITE=="`site'" & labperformed==1 & inlist(MAT_VISIT_MNH08,2,4,5,6)  , sheet("labs-incomplete-visit",modify)  firstrow(variables) 
		}
	}
	
	
	*Drop visits not completed & no labs performed:
	keep if MAT_VISIT_MNH08 == 1 | labperformed == 1
	
	*identify duplicates:
	
	duplicates tag MOMID PREGID LBSTDAT, gen(dup)
	label var dup "Duplicates by PREGID x date"
	tab SITE dup 
	tab SITE dup  if inrange(DATE,0,.)
	
	if $runquery == 1 {
		levelsof(SITE) if dup>0 , local(sitelev) clean
		foreach site of local sitelev {
			export excel SITE MOMID PREGID LBSTDAT MAT_VISIT_MNH08 TYPE_VISIT using "$queries/`site'-nutrition-queries-$datadate.xlsx"  if SITE=="`site'" & dup>0  , sheet("duplicates",modify)  firstrow(variables) 
		}
	}
	
	bysort MOMID PREGID DATE (TYPE_VISIT) : gen VISNUM=_n
	tab VISNUM SITE
	*keep if VISNUM==1
	
	save "$wrk/mnh08_allvisits-$datadate.dta" , replace 

	use "$latest_out/MAT_DEMOGRAPHIC.dta", clear
	cap isid MOMID PREGID 
	//does this combo uniquely identify all obs?
		//use capture to not break the code
	if _rc ==459 {
		//459 is the error code for duplicates
		duplicates tag MOMID PREGID, gen(dup)
		bysort MOMID PREGID (ENROLL_SCRN_DATE) : gen nvals = _n
		drop if dup == 1 & nvals == 2
		//if duplicate, drop the second screen
		drop dup nvals
		notes : MAT_DEMOGRAPHIC duplicates dropped  
	}
	notes : MAT_DEMOGRAPHIC from "$datadate". TS
	isid MOMID PREGID
	save "$wrk/MAT_DEMOGRAPHIC.dta", replace
	
	
	cap use "$outcomes/MAT_ENROLL.dta" , clear
	
	if _rc > 0 {
		*if no MAT_ENROLL dta file:
		import excel "$outcomes/MAT_ENROLL.xlsx", ///
		sheet("Sheet 1") firstrow case(upper) clear
		
		cap isid MOMID PREGID 
		//does this combo uniquely identify all obs?
		//use capture to not break the code
			if _rc == 459 {
				//459 is the error code for duplicates
				duplicates tag MOMID PREGID, gen(dup)
				bysort MOMID PREGID (ENROLL_SCRN_DATE) : gen nvals = _n
				drop if dup == 1 & nvals == 2
				//if duplicate, drop the second screen
				drop dup nvals
			}
			
			
	 save "$outcomes/MAT_ENROLL.dta"	
	 
	}

	keep SITE MOMID PREGID ENROLL PREG_START_DATE ANC20_PASS_LATE ANC36_PASS_LATE
	merge 1:m  PREGID using "$wrk/mnh08_allvisits-$datadate.dta"
	
	if $runquery == 1 {
		levelsof(SITE) if ENROLL!=1 , local(sitelev) clean
		foreach site of local sitelev {
			export excel SITE MOMID PREGID ENROLL  using "$queries/`site'-nutrition-queries-$datadate.xlsx"  if SITE=="`site'" & ENROLL!=1 , sheet("not-enrolled",modify)  firstrow(variables) 
		}
	}
	
	keep if ENROLL == 1
	//drop if no indication that this PREGID was enrolled
	tab _merge
	//indicates that some have ENROLL but no MNH08
	
	
	merge m:1 MOMID PREGID using "$wrk/MAT_DEMOGRAPHIC.dta", ///
	gen(merge_demo)
	

	keep if ENROLL == 1
	drop dup
	duplicates tag PREGID TYPE_VISIT LBSTDAT, gen(dup)
	drop if dup == 1 & ( CBC_LBPERF_9!=1 & MN_LBPERF_8!=1)
	
	cap drop _merge 
	merge m:1 SITE MOMID PREGID using "$outcomes/MAT_ENDPOINTS.dta", ///
		keepusing(PREG_END_DATE)
	
*Calculate gestational age at lab test date
	str2date PREG_START_DATE
	gen 	GA_DAYS =  LBSTDAT - PREG_START_DATE
	replace GA_DAYS = . if LBSTDAT < 0
	replace GA_DAYS = . if GA_DAYS > 1000
	//replace missing if lab date is default value or  extremely high
	
	replace GA_DAYS = . if inlist(TYPE_VISIT, 6,7,8,9,10,11,12,14)
	replace GA_DAYS = . if LBSTDAT > PREG_END_DATE
*Calculate trimester
	gen 	TRIMESTER = 1 if GA_DAYS <=97
	replace TRIMESTER = 2 if GA_DAYS>=98 & GA_DAYS<=195
	replace TRIMESTER = 3 if GA_DAYS>=196 & GA_DAYS<=308 // 44 weeks
	
	//replace missing if missing or default value
	*Trimester missing if PNC
	replace TRIMESTER = 55 if inlist(TYPE_VISIT,6,7,8,9,10,11,12,14) //PNC
	replace TRIMESTER = 55 if LBSTDAT > PREG_END_DATE //PNC visit
	
	replace TRIMESTER = 55 if LBSTDAT==. | LBSTDAT < 0 //invalid dates
	replace TRIMESTER = 55 if PREG_START_DATE ==. //invalid
	replace TRIMESTER = 55 if GA_DAYS < 0 //invalid
	
	replace TRIMESTER = 55 if TRIMESTER == .
	
	
	label define TRIMESTER ///
	1"1st" 2"2nd" 3"3rd" 55"PNC/missing/invalid"
	label val TRIMESTER TRIMESTER	
	
	**Look at GA concisely by trimester
	tabstat GA_DAYS , by (TRIMESTER) statistics(n min max )
	
	gen PP_DAYS = LBSTDAT-PREG_END_DATE if ///
		inlist(TYPE_VISIT,6,7,8,9,10,11,12,14)
	label var PP_DAYS "Days postpartum"
	
	**#Mean corpuscular volumne (MCV)
	*reference: perinatology.com/Reference/Reference%20Ranges/Mean%20corpuscular%20volume.htm
//DIFFERENTIATES MICROCYTIC, NORMOCYTIC, MACROCYTIC ANEMIA
	
	
	gen 	MCV = 1 if /// microcytic
	TRIMESTER==1 & CBC_MCV_LBORRES>0 & CBC_MCV_LBORRES<85   | ///
	TRIMESTER==2 & CBC_MCV_LBORRES>0 & CBC_MCV_LBORRES<85.8 | ///
	TRIMESTER==3 & CBC_MCV_LBORRES>0 & CBC_MCV_LBORRES<82.4 | ///
	((LBSTDAT>PREG_END_DATE) & CBC_MCV_LBORRES>0 & CBC_MCV_LBORRES<80)

	replace MCV = 2 if /// normocytic
	TRIMESTER==1 & CBC_MCV_LBORRES>=85 & CBC_MCV_LBORRES<=97.8 | ///
	TRIMESTER==2 & CBC_MCV_LBORRES>=85.8 & CBC_MCV_LBORRES<=99.4 | ///
	TRIMESTER==3 & CBC_MCV_LBORRES>=82.4 & CBC_MCV_LBORRES<=100.4 | ///
	((LBSTDAT>PREG_END_DATE) & CBC_MCV_LBORRES>=80 & CBC_MCV_LBORRES<=93)

	replace MCV = 3 if /// macrocytic
	TRIMESTER==1 & CBC_MCV_LBORRES>97.8 & CBC_MCV_LBORRES!=. | ///
	TRIMESTER==2 & CBC_MCV_LBORRES>99.4 & CBC_MCV_LBORRES!=. | ///
	TRIMESTER==3 & CBC_MCV_LBORRES>100 & CBC_MCV_LBORRES!=. | ///
	((LBSTDAT>PREG_END_DATE) & CBC_MCV_LBORRES>93 & CBC_MCV_LBORRES!=.)

	
**#MICRONUTRIENTS (not quansys)
	*1. Total serum cobalamin
	*2. Holotranscobalamin
	*3. Folate: blood serum
	*4. Zinc
	*5. Iron: hepcidin
	*6. Iron: total iron-binding capacity
	*7. Vitamin A: serum retinol
	*8. Folate - RBC


	**#1. Total serum cobalamin (pg/mL)
	*from ERS (different unit)
	*deficient <150 pmol/L == 203pg/mL
	*insufficient 150-220 pmol/L == 203-298 pg/mL
	*NOTE that a pmol/L will appear in the report 
	*for ease of interpretation and comparing with other literature

	gen VITB12_COB = 1 if ///
	VITB12_COB_LBORRES<203 	
	//deficient
	replace VITB12_COB = . if VITB12_COB_LBORRES<0
	//replace missing if default value	
	replace VITB12_COB = 2 if ///
	VITB12_COB_LBORRES>=203 &  VITB12_COB_LBORRES<=298
	//insufficient 
	replace VITB12_COB = 3 if ///
	VITB12_COB_LBORRES>298
	//sufficient
	
	replace VITB12_COB =. if VITB12_COB_LBORRES==.
	replace VITB12_COB =. if ///
	(TRIMESTER == . & TYPE_VISIT<=5) //ANC but no trimester	
	label var VITB12_COB "Serum Vitamin B12: total cobalamin (pg/mL)"

	tabstat VITB12_COB_LBORRES, by(VITB12_COB) statistics(min mean max n)	
	tab VITB12_COB SITE, m
	
	label define low_normal_high 1"Low" 2"Normal" 3"High"
	
	**#2. Holotranscobalamin
		*Trimester 1: > 35.5
		*Trimester 2: > 35.1
		*Trimester 3: > 30
		*PNC: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3127504/ 
				*40-150
	
	tabstat VITB12_HOL_LBORRES if VITB12_HOL_LBORRES>0, ///
	by(SITE) stat(n min p50 max)	
	**!! likely units issue between sites; have notified Abby
	
	gen VITB12_HOL  = 1 if ///
	TRIMESTER == 1 & VITB12_HOL_LBORRES < 35.5 | ///
	TRIMESTER == 2 & VITB12_HOL_LBORRES < 35.1 | ///
	TRIMESTER == 3 & VITB12_HOL_LBORRES < 30  | ///
	((LBSTDAT>PREG_END_DATE) & VITB12_HOL_LBORRES < 40) 
	replace VITB12_HOL = 2 if ///
	TRIMESTER == 1 & VITB12_HOL_LBORRES >= 35.5 | ///
	TRIMESTER == 2 & VITB12_HOL_LBORRES >= 35.1 | ///
	TRIMESTER == 3 & VITB12_HOL_LBORRES >= 30 | ///
	((LBSTDAT>PREG_END_DATE)  & VITB12_HOL_LBORRES >= 40)
	
	replace VITB12_HOL = . if VITB12_HOL_LBORRES == . 
	replace VITB12_HOL = . if TYPE_VISIT<=5 & TRIMESTER==.
	label var VITB12_HOL "Serum Vit B12: HoloTC"


	tab VITB12_HOL SITE, col
		
	**#3. Folate: blood serum
	*All trimesters:
		*<6.8 deficient
		*6.8-13.4 possibly deficient
		*13.5-45.3 normal
		*>45.2 elevated
		*according to WHO 2015	
	
	label define FOL_SERUM 1"Deficient" 2"Possibly deficient" ///
							3"Normal" 4"Elevated", replace
	
	gen FOL_SERUM = 1 if ///
	FOLATE_PLASMA_NMOLL_LBORRES < 6.8 
	replace FOL_SERUM = . if FOLATE_PLASMA_NMOLL_LBORRES < 0 
	//replace missing if default value 
	replace FOL_SERUM = 2 if ///
	FOLATE_PLASMA_NMOLL_LBORRES>=6.8 & FOLATE_PLASMA_NMOLL_LBORRES<13.5 
	
	replace FOL_SERUM = 3 if ///
	FOLATE_PLASMA_NMOLL_LBORRES>=13.5 & FOLATE_PLASMA_NMOLL_LBORRES<=45.3 
	replace FOL_SERUM = 4 if ///
	FOLATE_PLASMA_NMOLL_LBORRES>45.3
	label val FOL_SERUM FOL_SERUM
	
	replace FOL_SERUM = . if FOLATE_PLASMA_NMOLL_LBORRES == . 
	replace FOL_SERUM = . if (TRIMESTER == . & TYPE_VISIT<=5)
	label var FOL_SERUM "Folate - blood serum (nmol/L)"
	label var FOLATE_PLASMA_NMOLL_LBORRES "Folate - blood serum (nmol/L)"

	
	
	**#4. Folate: red blood cell (RBC) (nmol/L)
		*All trimesters: < 226.5 nmol/L (according to WHO 2015)
		*Prevent NTDs: 906 nmol/L 
			* https://pmc.ncbi.nlm.nih.gov/articles/PMC5779552/
	
	*Problem: Zambia does not perform CBC at ANC32
	*to fix this we will "Carry forward" the ANC28 value
	
	gen hct_anc28 = CBC_HCT_LBORRES if TYPE_VISIT == 3
	sort SITE PREGID TYPE_VISIT
	bysort PREGID (TYPE_VISIT): carryforward hct_anc28  if TYPE_VISIT==4 & CBC_HCT_LBORRES==., gen(hct_anc28_carry)
	
	gen FOL_RBC_CALC = FOLATE_RBC_NMOLL_LBORRES/(CBC_HCT_LBORRES/100) if ///
	inrange(FOLATE_RBC_NMOLL_LBORRES,0,.) & inrange(CBC_HCT_LBORRES,0,.)
	replace FOL_RBC_CALC = FOLATE_RBC_NMOLL_LBORRES/(hct_anc28_carry/100) if ///
	inrange(FOLATE_RBC_NMOLL_LBORRES,0,.) & CBC_HCT_LBORRES==.
	
	gen 	FOL_RBC = 1 if FOL_RBC_CALC < 226.5  		//deficient
	replace FOL_RBC = 2 if FOL_RBC_CALC>=226.5  		//suboptimal
	replace FOL_RBC = 3 if inrange(FOL_RBC_CALC,906,.) 	//prevent NTDs
	
	replace FOL_RBC = . if FOL_RBC_CALC < 0
	replace FOL_RBC = . if FOL_RBC_CALC == .
	replace FOL_RBC = . if TRIMESTER==. & TYPE_VISIT<=5

	label var FOL_RBC "Folate - RBC (nmol/L)"
	label var FOLATE_RBC_NMOLL_LBORRES "Folate - RBC (nmol/L)"
	
	
	
	**#Folate: any (serum/RBC)
	
	gen 			FOL_ANY = FOL_SERUM
	replace 		FOL_ANY = FOL_RBC if FOL_RBC!=.
	

	
**#QUANSYS
	*1. Ferritin //iron
	*2. Iodine (Thyroglobulin) 
	*3. Serum transferrin receptor (sTfR) //iron 
	*4. Retinol binding protein 4 (RBP4) //vitamin a deficiency
	*5. C-reactive protein (CRP) //inflammation
	*6. Alpha 1-acid glycoprotein (AGP) //inflammation
	*7. Histidine-rich protein 2 //malaria


	**#1. Ferritin
	
	
	**Question for how many ferritin values are in the data set?
	egen FERRITIN_TEST_NUM = total(!missing( FERRITIN_LBORRES ) ), by(MOMID PREGID)
	tab FERRITIN_TEST_NUM, m
		
	//VERSION 1: UNADJUSTED (DIFFERENT CUTOFF BASED ON INFLAMMATION STATUS)
	
	gen INFLAMMATION = 1 if CRP_LBORRES!=. | AGP_LBORRES!=.
	replace INFLAMMATION = 2 if ///
	CRP_LBORRES>5 & CRP_LBORRES!=. | AGP_LBORRES>1 & AGP_LBORRES!=.
	//INFLAMMATION IF EITHER CRP > 5 MG/L OR AGP > 1 G/L
	label define normal_high 1"Normal" 2"High"
	label val INFLAMMATION normal_high
	
	rename 	FERRITIN_LBORRES FERRITIN_LBORRES_OLD
	gen 	FERRITIN_LBORRES = FERRITIN_LBORRES*10 
			//convert units (from ug/dL --> ug/L)
			//to match literature/common cutoffs
	
	gen 	FERRITIN70 = 1 if FERRITIN_LBORRES!=.
	replace FERRITIN70 = 2 if ///
			(INFLAMMATION == 2 & FERRITIN_LBORRES<70) | /// if inflammation
			(INFLAMMATION == 1 & FERRITIN_LBORRES<15) 	// no inflammation
			//NOTE THAT <70 IS FOR UNADJUSTED, HIGH INFLAMMATION

	
	//concisely check the coding is correct
	tabstat FERRITIN_LBORRES if INFLAMMATION == 1, by( FERRITIN70) stats(min max)
	tabstat FERRITIN_LBORRES if INFLAMMATION == 2, by( FERRITIN70) stats(min max)
	
	label val FERRITIN70  low
	
	gen FERRITIN_D = 1 if FERRITIN70 ==1 | FERRITIN70==2
	


	**#2. Iodine (thyroglobulin)
	**Note that high Tg indicates either low iodine or excess iodine
	**Low iodine is more likely in our context
	**the reference range is not determined

	gen HIGH_TG = 1 if  IODINE_LBORRES!=.
	replace HIGH_TG = 2 if ///
	IODINE_LBORRES>=43.5 &  IODINE_LBORRES!=.
	label var HIGH_TG "2= Indicates >= 43.5 ug/L Tg"
	// Reference value from multi-country study of pregnant women 
	// conducted by Sara Stinca et al. 2017
	// doi: 10.1210/jc.2016-2829
	
	
	**#3. Serum transferrin receptor (sTfR) (mg/L)
	*Trimester 1: 1.49-3.61 mg/L
	*Trimester 2: 2.93-4.98
	*Trimester 3: 3.52-5.94
	
	gen STFR = 1 if ///
	TRIMESTER == 1 & TRANSFERRIN_LBORRES < 1.49 | /// STFR_ADJ is the adjusted variable
	TRIMESTER == 2 & TRANSFERRIN_LBORRES < 2.93 | ///
	TRIMESTER == 3 & TRANSFERRIN_LBORRES < 3.52 | ///
	((LBSTDAT > PREG_END_DATE) & TRANSFERRIN_LBORRES < 1.41 )
	replace STFR = . if TRANSFERRIN_LBORRES < 0 
	
	label var STFR "sTfR (high=iron deficiency)"
	
	replace STFR = 2 if ///
	TRIMESTER == 1 & ///
	(TRANSFERRIN_LBORRES>=1.49 & TRANSFERRIN_LBORRES<3.61) | ///
	TRIMESTER == 2 & ///
	(TRANSFERRIN_LBORRES>=2.93 & TRANSFERRIN_LBORRES<4.98) | ///
	TRIMESTER == 3 & ///
	(TRANSFERRIN_LBORRES>=3.52 & TRANSFERRIN_LBORRES<5.94) | ///
	((LBSTDAT > PREG_END_DATE) == 10 & ///
	(TRANSFERRIN_LBORRES>=1.41 & TRANSFERRIN_LBORRES<3.52) )
	
	replace STFR = 3 if ///
	TRIMESTER == 1 & TRANSFERRIN_LBORRES >= 3.61 | ///
	TRIMESTER == 2 & TRANSFERRIN_LBORRES >= 4.98 | ///
	TRIMESTER == 3 & TRANSFERRIN_LBORRES >= 5.94 | ///
	((LBSTDAT > PREG_END_DATE) & TRANSFERRIN_LBORRES >= 3.52)
	replace STFR = . if TRANSFERRIN_LBORRES == . 
	
	bys TRIMESTER : tabstat TRANSFERRIN_LBORRES, by(STFR) ///
	stats (n min max)
	// concisely check that cutoffs worked
	
	*Trimester 1: 1.49-3.61 mg/L
	*Trimester 2: 2.93-4.98
	*Trimester 3: 3.52-5.94
	
	

	**#4. Retinol binding protein 4 (RBP4)
	*Reference values from ERS		
	//note that RBP4 is not adjusted in BRINDA package
		
	gen RBP4 = 1 if RBP4_LBORRES < 0.35
	//severe deficiency

	replace RBP4 = 2 if RBP4_LBORRES>=0.35 & RBP4_LBORRES<0.7
	//moderate deficiency
	
	replace RBP4 = 3 if RBP4_LBORRES>=0.7 & RBP4_LBORRES<1.05
	//mild
	
	replace RBP4 = 4 if RBP4_LBORRES>=1.05 & RBP4_LBORRES!=.

	replace RBP4 = . if RBP4_LBORRES < 0
	replace RBP4 = . if RBP4_LBORRES == . 
	replace RBP4 = . if (TRIMESTER == . & TYPE_VISIT <=5)
	
	label var RBP4 "RBP4, severe: <0.35, moderate: <0.7, mild: <1.05"
	
	
	

	**#5. C-reactive protein (CRP) (mg/L)
	*Use the same cutoffs as for ReMAPP healthy cohort criteria
	gen 	CRP = 1 if CRP_LBORRES <= 5 
	replace CRP = 2 if CRP_LBORRES > 5 & CRP_LBORRES!=. 
	*replace CRP = . if TRIMESTER ==. & TYPE_VISIT<=5
	label var CRP "CRP, 1: less than 5mg/L, 2: >5mg/L"
	
	tabstat CRP_LBORRES, by(CRP) statistics(min max)
	
	
	**#6. Alpha 1-acid glycoprotein (g/L)
	*Use the same cutoffs as for ReMAPP healthy cohort criteria	
	gen AGP = 1 if AGP_LBORRES <= 1 
	replace AGP = 2 if AGP_LBORRES >1 & AGP_LBORRES !=.
	*replace AGP = . if TRIMESTER ==. & TYPE_VISIT<=5
	
	label var AGP "AGP, 1: less than 1g/L, 2: > 1g/L"
	
	tabstat AGP_LBORRES, by(AGP) statistics(min max)
	
	
	
	**#7. Histidine-rich protein 2	
	*Reference range: >= 40.84 pg/mL is high (== malaria)
	*Note that PRISMA reports values in ug/mL
	*Quansys reports in ug/L, sites need to convert to ug/mL
	
	
	
	foreach num of numlist 4/6 {
		gen HRP_`num' = 0 if !missing( HRP_LBORRES)
		replace HRP_`num' = 1 if  !missing( HRP_LBORRES) & HRP_LBORRES >= 0.000`num'
		label define  HRP_`num' 1"above 0.000`num'"
		label val  HRP_`num'  HRP_`num'
	}
	
	
	gen QUANSYS_TEST = . 
foreach v in FERRITIN TRANSFERRIN CRP AGP IODINE RBP4 {
	
	replace QUANSYS_TEST = 1 if !inlist(`v'_LBORRES, -5,-7) & `v'_LBORRES!=.
	}
	

	
**#Hemoglobin: 
	gen HB_LBORRES = CBC_HB_LBORRES if CBC_HB_LBORRES != .
	destring HB_LBORRES, replace 
	replace HB_LBORRES = . if HB_LBORRES < 0  
	replace HB_LBORRES = . if HB_LBORRES >= 99 
	
	sum HB_LBORRES
	
	// adjust for altitude: 
	replace HB_LBORRES = HB_LBORRES - 0.8 if SITE == "Kenya" | SITE == "Zambia"	
	// adjust for smoking: 
		replace HB_LBORRES = HB_LBORRES - 0.3 if SMOKE == 1 
	label var HB_LBORRES "Hb, adjusted for altitude & smoking"
	
	gen HB11 = 0 if HB_LBORRES!=.
	replace HB11 = 1 if HB_LBORRES < 11
	label define HB11 0"Hb > 11" 1"Hb < 11"
	label val HB11 HB11
	
	gen HB10 = 0 if HB_LBORRES!=.
	replace HB10 = 1 if HB_LBORRES < 10
	label define HB10 0"Hb > 10" 1"Hb < 10"
	label val HB10 HB10
	

	**# Loop for different time points:
	
	foreach var in 	VITB12_COB CRP AGP INFLAMMATION FERRITIN70 HIGH_TG STFR RBP4 MCV FOL_ANY QUANSYS_TEST {
		
		foreach num of numlist 1/3 {
		cap gen `var'_T`num' = `var' if TRIMESTER == `num'
		cap label var `var'_T`num' "`var' in Trimester `num'"
		
		cap gen `var'_CONT_T`num' = `var'_LBORRES if TRIMESTER == `num'
		cap label var `var'_CONT_T`num' "`var' (continuous), T `num'"
		
	}
	
	*Enroll/ANC20
	gen `var'_ANC20 = `var' if ///
					inlist(TYPE_VISIT,1,2) & inrange(GA_DAYS,28,181)
	
	cap gen `var'_CONT_ANC20 = `var'_LBORRES if ///
					inlist(TYPE_VISIT,1,2) & inrange(GA_DAYS,28,181)
	
	
	*ANC32/36
	gen `var'_ANC32 = `var' if ///
					inlist(TYPE_VISIT,4,5) & inrange(GA_DAYS,217,310)
	cap gen `var'_CONT_ANC32 = `var'_LBORRES if ///
					inlist(TYPE_VISIT,4,5) & inrange(GA_DAYS,217,310)
	
	*PNC6
	gen `var'_PNC6 = `var' if ///
					TYPE_VISIT == 10 & inrange(PP_DAYS,42,104)
	cap gen `var'_CONT_PNC6  = `var'_LBORRES if ///
					inlist(TYPE_VISIT,10) & inrange(PP_DAYS,42,104)		
	
	
	}
	
	gen STFR_CONT_ANC20 = TRANSFERRIN_LBORRES if ///
					inlist(TYPE_VISIT,1,2) & inrange(GA_DAYS,28,181)
	gen STFR_CONT_ANC32 = TRANSFERRIN_LBORRES if ///
					inlist(TYPE_VISIT,4,5) & inrange(GA_DAYS,217,310)
	gen STFR_CONT_PNC6  = TRANSFERRIN_LBORRES if ///
					inlist(TYPE_VISIT,10) & inrange(PP_DAYS,42,104)
					
	gen FERRITIN_CONT_ANC20 = FERRITIN_LBORRES if ///
					inlist(TYPE_VISIT,1,2) & inrange(GA_DAYS,28,181)
	gen FERRITIN_CONT_ANC32 = FERRITIN_LBORRES if ///
					inlist(TYPE_VISIT,4,5) & inrange(GA_DAYS,217,310)
	gen FERRITIN_CONT_PNC6  = FERRITIN_LBORRES if ///
					inlist(TYPE_VISIT,10) & inrange(PP_DAYS,42,104)				
	
	foreach num of numlist 1/3 {
		
		
		gen FERRITIN_CONT_T`num' = FERRITIN_LBORRES if TRIMESTER == `num'
		label var FERRITIN_CONT_T`num' "Ferritin (continuous), T `num'"
		
		gen STFR_CONT_T`num' = TRANSFERRIN_LBORRES if TRIMESTER == `num'
		label var STFR_CONT_T`num' "sTfR (continuous), T `num'"
	}
	
	
	label define MCV 1"Microcytic" 2"Normal" 3"Macrocytic" 
	label val MCV MCV_ANC20 MCV_ANC32 MCV_T* MCV
	
	
	label define COB 1"Deficient" 2"Insufficient" 3"Normal"
	label val VITB12_COB VITB12_COB_ANC20 VITB12_COB_ANC32 VITB12_COB_PNC6 VITB12_COB_T* COB 
	
	
	
	label define FOL_RBC 1"Deficient" 2"Insufficient" 3"Optimal", replace
	label val FOL_RBC  FOL_RBC
	
	label define 	FOL_ANY ///
						1"Deficient" ///
						2"Insufficient/possibly deficient" ///
						3"Normal/optimal" ///
						4"Elevated", replace
	label val 		FOL_ANY FOL_ANY_T* FOL_ANY_ANC20 FOL_ANY_ANC32 FOL_ANY_PNC6 FOL_ANY
	label var FOL_ANY "Folate status (serum/RBC; RBC prioritized)"
	
	label val STFR STFR_T* STFR_ANC20 STFR_ANC32 STFR_PNC6 low_normal_high
	
	
	label define RBP4 1"Severe deficiency" 2"Moderate" 3"Mild" 4"None"
	label val RBP4 RBP4_ANC20 RBP4_ANC32 RBP4_PNC6 RBP4_T* RBP4 
	
	label define low 2"low ferritin", replace
	label val FERRITIN70 FERRITIN70_T*  FERRITIN70_ANC20 FERRITIN70_ANC32 FERRITIN70_PNC6 low
	
	
	merge m:1 SITE MOMID PREGID using "$outcomes/MAT_ENROLL.dta", keepusing(REMAPP_ENROLL) nogenerate
	
	
	
	preserve 
	
	keep SITE MOMID PREGID LBSTDAT TYPE_VISIT  GA_DAYS TRIMESTER PP_DAYS MCV VITB12_COB CRP_LBORRES AGP_LBORRES  CRP AGP INFLAMMATION FERRITIN_LBORRES FERRITIN70 IODINE_LBORRES HIGH_TG  TRANSFERRIN_LBORRES STFR RBP4_LBORRES RBP4  HB_LBORRES VITB12_COB_LBORRES CBC_MCV_LBORRES MCV FOL_ANY FOLATE_PLASMA_NMOLL_LBORRES FOLATE_RBC_NMOLL_LBORRES
	
	label var LBSTDAT "Specimen collection date"
	label var TYPE_VISIT "Visit type"
	label var GA_DAYS "GA in days"
	label var TRIMESTER "Trimester"
	label var CRP_LBORRES "CRP (mg/L)"
	label var AGP_LBORRES "AGP (g/L)"
	label var INFLAMMATION "Inflammation by either CRP or AGP"
	label var FERRITIN_LBORRES "Ferritin (ug/L)"
	label var FERRITIN70 "Low ferritin; <70 if inflammation; <15 if no inflammation"
	label var RBP4_LBORRES "RBP4 (umol/L)"
	label var MCV "Micro/normal/macrocytic"
	
	
	order SITE MOMID PREGID  LBSTDAT TYPE_VISIT  GA_DAYS TRIMESTER PP_DAYS  VITB12_COB_LBORRES VITB12_COB CRP_LBORRES AGP_LBORRES  CRP AGP INFLAMMATION FERRITIN_LBORRES FERRITIN70 IODINE_LBORRES HIGH_TG  TRANSFERRIN_LBORRES STFR RBP4_LBORRES RBP4 CBC_MCV_LBORRES MCV  HB_LBORRES 
	
	save "$wrk/MAT_NUTR_LONG.dta" , replace 
	restore
	

cap mkdir "$savannah/Maternal Outcomes\ReMAPP Aim 3/$datadate/"
*save "$savannah/Maternal Outcomes\ReMAPP Aim 3/$datadate/MAT_NUTR_LONG.dta", replace

**#Create a collapsed data file

sort SITE  MOMID PREGID LBSTDAT
collapse (firstnm) ///
		ENROLL  REMAPP ///
		MCV_*  ///
		VITB12_COB_*  ///
		FERRITIN70_* FERRITIN_CONT_*  ///
		HIGH_TG_* ///
		STFR_* ///
		RBP4_* ///
		CRP_*  ///
		AGP_*  ///
		INFLAMMATION_* /// 
		QUANSYS_TEST_* ///
		FOL_ANY_* , ///
		by(SITE MOMID PREGID)

*save "$outcomes/MAT_NUTR-CONTN-MEASURES.dta" , replace

drop *_LBORRES *_LBTSTDAT


keep if ENROLL==1

*Label variables
label define FERRITIN70 1"Not low" 2"Ferritin <70ug/L",replace
label val FERRITIN70_T1 FERRITIN70_T2 FERRITIN70_T3 FERRITIN70_ANC20 FERRITIN70_ANC32 FERRITIN70_PNC6 FERRITIN70

label define HIGH_TG 1"Normal" 2"High",replace
label val HIGH_TG_T1 HIGH_TG_T2 HIGH_TG_T3 HIGH_TG_ANC20 HIGH_TG_ANC32 HIGH_TG_PNC6 HIGH_TG

label define STFR 1"Low" 2"Normal" 3"High",replace
label val STFR_ANC20 STFR_ANC32 STFR

label define RBP4 1"Severe deficiency" 2"Moderate" 3"Mild" 4"None",replace
label val RBP4_ANC20 RBP4_ANC32 RBP4 

label define INFL 1"Normal" 2"High",replace
label val CRP_ANC20 CRP_ANC32 AGP_ANC20 AGP_ANC32 INFL

label define VITB12 1"Deficient" 2"Insufficient" 3"Sufficient",replace
label val VITB12_COB_ANC20 VITB12_COB_ANC32 VITB12




label data "All participants. Data date: $datadate; `c(username)' modified `c(current_date)'"
local datalabel: data label
disp "`datalabel'" //displays dataset label you just assigned

*save "$outcomes/MAT_NUTR_ALL.dta", replace


**#Calculate denominators based on expected 
merge 1:1 MOMID PREGID using "Z:\Savannah_working_files\Expected-obs/Expected_obs-$datadate.dta" , gen(merge_exp)

rename TRI_1_EXP T1_EXP
rename TRI_2_EXP T2_EXP
rename TRI_3_EXP T3_EXP


foreach var in MCV VITB12_COB FERRITIN70 HIGH_TG STFR RBP4 CRP AGP INFLAMMATION FOL_ANY {
	
	foreach time in "ANC20" "ANC32" "PNC6" "T1" "T2" "T3" {
		
		replace `var'_`time' = . if  `var'_`time' == 55
		
		cap drop `var'_`time'_DENOM
		gen `var'_`time'_DENOM = 1 if !missing( `var'_`time')
		label var `var'_`time'_DENOM "Nonmissing `var' at `time'"
		
		replace `var'_`time' = 55 if ///
					`time'_EXP == 1 & `var'_`time' == .
		
		replace `var'_`time'_DENOM = 55 if ///
					`time'_EXP == 1 & `var'_`time'_DENOM == .
					
	}
	
	}

	foreach var in MCV VITB12_COB FERRITIN70 HIGH_TG STFR RBP4 CRP AGP INFLAMMATION FOL_ANY {
	*for nutrition analytes: if value at T1, not expected at T2:
		replace `var'_T2=. if `var'_T2==55 & `var'_T1<55
		replace `var'_T2_DENOM = . if `var'_T2_DENOM==55 & `var'_T1<55
	}
	

*MCV denominators (expected or observed):  
gen 		MCV_T1_EXP_DENOM = 1 if MCV_T1_DENOM==1 | T1_EXP==1
label var 	MCV_T1_EXP_DENOM "Expected or observed in T1"

gen 		MCV_T2_EXP_DENOM = 1 if MCV_T2_DENOM==1 | T2_EXP==1
replace 	MCV_T2_EXP_DENOM = . if ///
				MCV_T1_DENOM==1 & MCV_T2_DENOM != 1
			//not expected if reported in T1	
label var 	MCV_T2_EXP_DENOM "Expected or observed in T2"

gen 		MCV_T3_EXP_DENOM = 1 if MCV_T3_DENOM==1 | T3_EXP==1
label var 	MCV_T3_EXP_DENOM "Expected or observed in T3"  
  
*Quansys denominators (expected or observed):  
gen 		QUANSYS_T1_DENOM = 1 if QUANSYS_TEST_T1==1 | T1_EXP==1
label var 	QUANSYS_T1_DENOM "Expected or observed in T1"

gen 		QUANSYS_T2_DENOM = 1 if QUANSYS_TEST_T2==1 | T2_EXP==1
replace 	QUANSYS_T2_DENOM = . if ///
				QUANSYS_TEST_T1==1 & QUANSYS_TEST_T2 != 1
			//not expected if reported in T1	
label var 	QUANSYS_T2_DENOM "Expected or observed in T2"

gen 		QUANSYS_T3_DENOM = 1 if QUANSYS_TEST_T3==1 | T3_EXP==1
label var 	QUANSYS_T3_DENOM "Expected or observed in T3"


*B12 denominators (expected or observed):
gen 		VITB12_T1_EXP_DENOM = 1 if ///
				VITB12_COB_T1_DENOM == 1 | T1_EXP == 1
label var 	VITB12_T1_EXP_DENOM	"Expected or observed in T1"

gen 		VITB12_T2_EXP_DENOM = 1 if ///
				VITB12_COB_T2_DENOM == 1 | T2_EXP == 1
replace 	VITB12_T2_EXP_DENOM = . if ///
				VITB12_COB_T1_DENOM == 1 & VITB12_COB_T2_DENOM != 1
				//not expected if reported in T1
label var  	VITB12_T2_EXP_DENOM "Expected or observed in T2"

gen 		VITB12_T3_EXP_DENOM = 1 if ///
				VITB12_COB_T3_DENOM == 1 | T3_EXP == 1
label var 	VITB12_T3_EXP_DENOM	"Expected or observed in T3"


*Folate denominators (expected or observed):
gen 		FOL_ANY_T1_EXP_DENOM = 1 if ///
				FOL_ANY_T1_DENOM == 1 | T1_EXP == 1
label var 	FOL_ANY_T1_EXP_DENOM	"Expected or observed in T1"

gen 		FOL_ANY_T2_EXP_DENOM = 1 if ///
				FOL_ANY_T2_DENOM == 1 | T2_EXP == 1
replace 	FOL_ANY_T2_EXP_DENOM = . if ///
				FOL_ANY_T1_DENOM == 1 & FOL_ANY_T2_DENOM != 1
				//not expected if reported in T1
label var  	FOL_ANY_T2_EXP_DENOM "Expected or observed in T2"

gen 		FOL_ANY_T3_EXP_DENOM = 1 if ///
				FOL_ANY_T3_DENOM == 1 | T3_EXP == 1
label var 	FOL_ANY_T3_EXP_DENOM	"Expected or observed in T3"
  
keep SITE MOMID PREGID ENROLL REMAPP_ENROLL MCV_* VITB12_* FERRITIN* HIGH_TG* STFR* RBP4* CRP* AGP* INFLAMMATION* QUANSYS_T* FOL_ANY_* *_EXP

order *_ANC20_DENOM *_ANC20 *_CONT_ANC20 *_ANC32_DENOM *_ANC32 *_CONT_ANC32 *_T1_DENOM *_T1 *_T2_DENOM *_T2 *_T3_DENOM *_T3 *_PNC6_DENOM *_PNC6 *_CONT_PNC6

order SITE MOMID PREGID ENROLL REMAPP *_EXP   QUANSYS_TEST* FERRITIN* STFR* CRP* AGP* INFLAMMATION* HIGH_TG* RBP4*   MCV_* VITB12_* FOL_ANY_* 


foreach analyte in  "STFR" "RBP4" "CRP" "AGP" {
	
	foreach time in "ANC20" "ANC32" "PNC6" "T1" "T2" "T3" {
	label var `analyte'_`time' "`analyte' at `time'"
	label var `analyte'_`time'_DENOM "valid measurement of `analyte' at `time'"
	label var `analyte'_CONT_`time' "continuous value of `analyte' at `time'"
	}
	
}


foreach time in "ANC20" "ANC32" "PNC6"  "T1" "T2" "T3" {
	label var MCV_`time' "MCV at `time'"
	label var MCV_`time'_DENOM "valid measurement of MCV at `time'"

	label var VITB12_COB_`time' "B12 at `time'"
	label var VITB12_COB_`time'_DENOM "valid B12 at `time'"
	label var VITB12_COB_CONT_`time' "continuous value of B12 at `time'"
	
	label var FERRITIN70_`time'  "Low ferritin at `time' (<70 if inflammation)"
	label var FERRITIN70_`time'_DENOM "valid ferritin at `time'"
	label var FERRITIN_CONT_`time' "continuous ferritin at `time'"
	
	label var HIGH_TG_`time' "High tg at `time'"
	label var HIGH_TG_`time'_DENOM "valid tg at `time'"
	
	label var INFLAMMATION_`time' "Inflammation at `time'"
	label var INFLAMMATION_`time'_DENOM "valid inflammation at `time'"
	
	label var FOL_ANY_`time' "Folate status at `time'"
	
	label var QUANSYS_TEST_`time' "quansys test performed at `time'"
	}
	 
	
	label var T1_EXP "Expected in T1"
	label var T2_EXP "Expected in T2"
	label var T3_EXP "Expected in T3"
	
	label define enrolled 0"Not enrolled" 1"Enrolled"
	label val ENROLL REMAPP_ENROLL enrolled
	
	label var ENROLL "Enrolled in PRISMA"
	label var REMAPP_ENROLL "Enrolled in ReMAPP"
	

label data "Data date: $datadate; `c(username)' modified `c(current_date)'"
local datalabel: data label
disp "`datalabel'" //displays dataset label you just assigned

cap drop uploaddate 
cap drop UploadDate 
cap drop merge_exp
	
	save "$wrk/MAT_NUTR.dta" , replace
	save "$outcomes/MAT_NUTR.dta" , replace

	
	
