**Maternal nutritional characteristics
**Micronutrient deficiencies & MCV
**Prepared by : Savannah O'Malley (savannah.omalley@gwu.edu)

**UPDATED Jan 3 2024 with new variable naming convention

/*
Note: the file needs to call in the following files:

"$outcomes/MAT_ENROLL.dta"
"$outcomes/mat_ENDPOINTS.dta"

*/





**CHANGE THE BELOW BASED ON WHICH DATA YOU ARE WORKING WITH
global datadate "2025-04-18"
global da "Z:\Stacked Data/$datadate"
global outcomes "Z:\Outcome Data/$datadate"
local date: di %td_CCYY_NN_DD daily("`c(current_date)'", "DMY")
global today = subinstr(strltrim("`date'"), " ", "-", .)
disp "$today"

**#**SET DIRECTORIES
global savannah "D:\Users\savannah.omalley\Documents"

cap mkdir "$savannah/nutrition"
cd "$savannah/nutrition"
global wrk "$savannah/nutrition/$datadate"
	cap mkdir "$wrk"


global queries "$wrk/queries"
	cap mkdir "$queries" //make this folder if it does not exist
	
	global runquery 1


import excel "Z:\Stacked Data/$datadate/mnh07_merged.xlsx", ///
sheet("Sheet 1") firstrow case(upper) clear
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

save "$wrk/mnh07.dta", replace
	
use "$savannah/data/$datadate/mnh08.dta", ///
 clear 

cap drop V1 

rename  M08_* *

#delimit ;
keep SITE MOMID PREGID  MAT_VISIT_MNH08 MAT_VISIT_OTHR_MNH08 TYPE_VISIT LBSTDAT  LB_REMAPP*
CBC_MCV_LBORRES 
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
ZINC_LBORRES
ZINC_LBTSTDAT
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
ZINC_LBORRES
ZINC_LBTSTDAT
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
MN_LBPERF_1 CBC_MCV_LBORRES  VITB12_COB_LBORRES  MN_LBPERF_2  VITB12_HOL_LBORRES  MN_LBPERF_3  FOLATE_PLASMA_NMOLL_LBORRES  MN_LBPERF_8  FERRITIN_LBORRES SF_ADJ STFR_ADJ  MN_LBPERF_9  IODINE_LBORRES  MN_LBPERF_10 IRON_HEP_LBORRES IRON_TOT_UGDL_LBORRES  TRANSFERRIN_LBORRES  MN_LBPERF_11  RBP4_LBORRES  MN_LBPERF_12  CRP_LBORRES  MN_LBPERF_13  AGP_LBORRES VITA_UGDL_LBORRES HRP_LBORRES ZINC_LBORRES FOLATE_RBC_NMOLL_LBORRES RBC_THALA_1 RBC_THALA_10 RBC_THALA_11 RBC_THALA_12 RBC_THALA_13 RBC_THALA_14 RBC_THALA_15 RBC_THALA_16 RBC_THALA_17 RBC_THALA_18 RBC_THALA_19 RBC_THALA_2 RBC_THALA_3 RBC_THALA_4 RBC_THALA_5 RBC_THALA_6 RBC_THALA_7 RBC_THALA_8 RBC_THALA_9 RBC_THALA_LBORRES PL_MORPH_LBORRES WBC_MORPH_LBORRES PARA_MORPH_LBORRES RBC_MORPH_LBORRES  {
	cap replace `var'= "" if `var'=="NA"
	cap destring `var', replace
}

*Replace default values of -5 and -7
foreach var in  MN_LBPERF_1 CBC_MCV_LBORRES  VITB12_COB_LBORRES  MN_LBPERF_2  VITB12_HOL_LBORRES  MN_LBPERF_3  FOLATE_PLASMA_NMOLL_LBORRES  MN_LBPERF_8  FERRITIN_LBORRES  MN_LBPERF_9  IODINE_LBORRES  MN_LBPERF_10 IRON_HEP_LBORRES IRON_TOT_UGDL_LBORRES  TRANSFERRIN_LBORRES  MN_LBPERF_11  RBP4_LBORRES  MN_LBPERF_12  CRP_LBORRES  MN_LBPERF_13  AGP_LBORRES VITA_UGDL_LBORRES HRP_LBORRES ZINC_LBORRES FOLATE_RBC_NMOLL_LBORRES RBC_THALA_1 RBC_THALA_10 RBC_THALA_11 RBC_THALA_12 RBC_THALA_13 RBC_THALA_14 RBC_THALA_15 RBC_THALA_16 RBC_THALA_17 RBC_THALA_18 RBC_THALA_19 RBC_THALA_2 RBC_THALA_3 RBC_THALA_4 RBC_THALA_5 RBC_THALA_6 RBC_THALA_7 RBC_THALA_8 RBC_THALA_9 RBC_THALA_LBORRES BLEAD_LBORRES   SCHISTO_STOOL_CT_1 SCHISTO_STOOL_CT_2 SCHISTO_STOOL_CT_3 SCHISTO_STOOL_CT_4 SCHISTO_STOOL_CT_5  {
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
	keep if MAT_VISIT_MNH08 <= 2 | labperformed == 1
	
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

	use "$outcomes/MAT_DEMOGRAPHIC.dta", clear
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
	
	import excel "Z:\Savannah_working_files\Maternal Nutrition/$datadate/mnh08_adj-CRP_AGP-$datadate.xlsx", sheet("Sheet1") firstrow case(upper) clear
	drop if M08_LBSTDAT==""
	str2date M08_LBSTDAT
	duplicates tag PREGID M08_TYPE_VISIT M08_LBSTDAT, gen(dup)
	drop if dup == 1 & SF_ADJ==.
	isid PREGID M08_LBSTDAT M08_TYPE_VISIT
	rename M08_* *
	drop dup LBSTDAT_STR
	save "$wrk/MNH08_BRINDA", replace
	
	use "$outcomes/MAT_ENROLL.dta" , clear

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
	merge 1:1 SITE MOMID PREGID TYPE_VISIT LBSTDAT using "$wrk/MNH08_BRINDA", gen(merge_BRINDA)
	**# explore non-merging between files - should we merge earlier?
	
*Calculate gestational age at lab test date
	str2date PREG_START_DATE
	gen GA_DAYS =  LBSTDAT - PREG_START_DATE
	
	replace GA_DAYS =. if LBSTDAT < 0
	
	replace GA_DAYS=. if GA_DAYS > 1000
	//replace missing if lab date is default value or  extremely high
	
*Calculate trimester
	gen TRIMESTER = 1 if GA_DAYS <=97
	replace TRIMESTER = 2 if GA_DAYS>=98 & GA_DAYS<=195
	replace TRIMESTER = 3 if GA_DAYS>=196 & GA_DAYS<=300
	
	//replace missing if missing or default value
	replace TRIMESTER = 55 if  ///
	LBSTDAT==. |LBSTDAT < 0
	replace TRIMESTER = 55 if PREG_START_DATE ==.
	replace TRIMESTER = 55 if GA_DAYS < 0 
	replace TRIMESTER = 55 if TYPE_VISIT >=6 & TYPE_VISIT <=12
	replace TRIMESTER = 55 if TYPE_VISIT == 14
	replace TRIMESTER = 55 if TRIMESTER == .
	
	
	
	*replace TRIMESTER =4 if ///
	*TYPE_VISIT >=6 & TYPE_VISIT<=12 | TYPE_VISIT==14
	//replace "4th" trimester if visit type is L&D or PNC visit
	
	label define TRIMESTER ///
	1"1st" 2"2nd" 3"3rd" 55"PNC/missing/invalid"
	label val TRIMESTER TRIMESTER	
	
	**Look at GA concisely by trimester
	tabstat GA_DAYS , by (TRIMESTER) statistics(n min max )

	/*
	gen INFAGE_DAYS = date(LBSTDAT, "YMD") - PREG_END_DATE
	replace INFAGE_DAYS = . if date(LBSTDAT, "YMD") < 0 | ///
	PREG_END_DATE == .
	*/

	
	**#Mean corpuscular volumne (MCV)
//DIFFERENTIATES MICROCYTIC, NORMOCYTIC, MACROCYTIC ANEMIA
	gen MCV = 1 if ///
	TRIMESTER==1 & CBC_MCV_LBORRES>0 & CBC_MCV_LBORRES<85   | ///
	TRIMESTER==2 & CBC_MCV_LBORRES>0 & CBC_MCV_LBORRES<85.8 | ///
	TRIMESTER==3 & CBC_MCV_LBORRES>0 & CBC_MCV_LBORRES<82.4 | ///
	TYPE_VISIT == 10 & CBC_MCV_LBORRES>0 & CBC_MCV_LBORRES<80

	replace MCV = 2 if ///
	TRIMESTER==1 & CBC_MCV_LBORRES>=85 & CBC_MCV_LBORRES<=97.8 | ///
	TRIMESTER==2 & CBC_MCV_LBORRES>=85.8 & CBC_MCV_LBORRES<=99.4 | ///
	TRIMESTER==3 & CBC_MCV_LBORRES>=82.4 & CBC_MCV_LBORRES<=100.4 | ///
	TYPE_VISIT==10 & CBC_MCV_LBORRES>=80 & CBC_MCV_LBORRES<=93

	replace MCV=3 if ///
	TRIMESTER==1 & CBC_MCV_LBORRES>97.8 & CBC_MCV_LBORRES!=. | ///
	TRIMESTER==2 & CBC_MCV_LBORRES>99.4 & CBC_MCV_LBORRES!=. | ///
	TRIMESTER==3 & CBC_MCV_LBORRES>100 & CBC_MCV_LBORRES!=. | ///
	TYPE_VISIT==10 & CBC_MCV_LBORRES>93 & CBC_MCV_LBORRES!=.

	gen MCV_ANC20 = MCV if TYPE_VISIT<=2
	gen MCV_ANC20_DENOM = 1 if MCV_ANC20 <=3
	gen MCV_ANC32 = MCV if TYPE_VISIT>=4 & TYPE_VISIT <=5
	*gen  MCV_ANC32 = MCV if inlist(TYPE_VISIT, 4, 5)
	gen MCV_ANC32_DENOM = 1 if MCV_ANC32<=3
	gen MCV_PNC6 = MCV if TYPE_VISIT==10
	gen MCV_PNC6_DENOM = 1 if MCV_PNC6<=3

	gen MCV_T1 = MCV if TRIMESTER ==1
	gen MCV_T2 = MCV if TRIMESTER ==2
	gen MCV_T3 = MCV if TRIMESTER ==3
	
	
	label define MCV 1"Microcytic" 2"Normal" 3"Macrocytic" 
	label val MCV MCV_ANC20 MCV_ANC32 MCV_T1 MCV_T2 MCV_T3 MCV


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
	
	*Now create the timepoint vars
	gen VITB12_COB_ANC20 = VITB12_COB if TYPE_VISIT <= 2
	gen VITB12_COB_ANC32 = VITB12_COB if ///
	TYPE_VISIT >= 4 & TYPE_VISIT <= 5
	gen VITB12_COB_PNC6 = VITB12_COB if TYPE_VISIT==10
	
	gen VITB12_COB_ANC20_DENOM = 1 if ///
	VITB12_COB_ANC20 >= 0 & VITB12_COB_ANC20 <= 3
	gen VITB12_COB_ANC32_DENOM = 1 if ///
	VITB12_COB_ANC32 >= 0 & VITB12_COB_ANC32 <= 3
	gen VITB12_COB_PNC6_DENOM = 1 if ///
	VITB12_COB_PNC6 >= 0 & VITB12_COB_PNC6 <= 3

	label define COB 1"Deficient" 2"Insufficient" 3"Normal"
	label val VITB12_COB VITB12_COB_ANC20 VITB12_COB_ANC32 VITB12_COB_PNC6 COB
	
	gen VITB12_CONT_ANC20 = VITB12_COB_LBORRES if inlist(TYPE_VISIT,1,2)
	gen VITB12_CONT_ANC32 = VITB12_COB_LBORRES if inlist(TYPE_VISIT,4,5)
	gen VITB12_CONT_PNC6 = VITB12_COB_LBORRES if inlist(TYPE_VISIT,10)
	
	tab VITB12_COB SITE,col //prevalence deficiency by site
	
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
	TYPE_VISIT==10 & VITB12_HOL_LBORRES < 40
	replace VITB12_HOL = 2 if ///
	TRIMESTER == 1 & VITB12_HOL_LBORRES >= 35.5 | ///
	TRIMESTER == 2 & VITB12_HOL_LBORRES >= 35.1 | ///
	TRIMESTER == 3 & VITB12_HOL_LBORRES >= 30 | ///
	TYPE_VISIT==10  & VITB12_HOL_LBORRES >= 40
	replace VITB12_HOL = . if VITB12_HOL_LBORRES == . 
	replace VITB12_HOL = . if TYPE_VISIT<=5 & TRIMESTER==.
	label var VITB12_HOL "Serum Vit B12: HoloTC"

		
	gen VITB12_HOL_ANC20 = VITB12_HOL if TYPE_VISIT <= 2
	gen VITB12_HOL_ANC32 = VITB12_HOL if ///
	TYPE_VISIT >=4 & TYPE_VISIT <= 5
	gen VITB12_HOL_PNC6 = VITB12_HOL if TYPE_VISIT == 10
		
	gen VITB12_HOL_ANC20_D = 1 if ///
	VITB12_HOL_ANC20>=1 & VITB12_HOL_ANC20 <=2
	gen VITB12_HOL_ANC32_D = 1 if ///
	VITB12_HOL_ANC32>=1 & VITB12_HOL_ANC32 <=2
	gen VITB12_HOL_PNC6_D = 1 if ///
	VITB12_HOL_PNC6>=1 & VITB12_HOL_PNC6<=2
	
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

	gen FOL_SERUM_ANC20 = FOL_SERUM if TYPE_VISIT <= 2 
	gen FOL_SERUM_ANC32 = FOL_SERUM if TYPE_VISIT >= 4 & TYPE_VISIT <= 5
	gen FOL_SERUM_PNC6 = FOL_SERUM if TYPE_VISIT==10
	
	gen FOL_SERUM_ANC20_D = 1 if ///
	FOL_SERUM_ANC20>=1 & FOL_SERUM_ANC20<=4
	gen FOL_SERUM_ANC32_D = 1 if ///
	FOL_SERUM_ANC32>=1 & FOL_SERUM_ANC32<=4
	gen FOL_SERUM_PNC6_D = 1 if ///
	FOL_SERUM_PNC6>=1 & FOL_SERUM_PNC6<=4
	
	**#4. Zinc (ug/dL) **ReMAPP Aim 3**
	*Trimester 1: 57-88
	*Trimester 2: 51-80
	*Trimester 3: 50-77
	*cite: Abbassi-Ghanavati 2009
	
	//note that zinc is not adjusted by BRINDA package
	
	gen ZINC = 1 if ///
	TRIMESTER == 1 & ZINC_LBORRES < 57 | ///
	TRIMESTER == 2 & ZINC_LBORRES < 51 | ///
	TRIMESTER == 3 & ZINC_LBORRES < 50
	replace ZINC = . if ZINC_LBORRES < 0 
	//missing if default value
	replace ZINC = 2 if ///
	TRIMESTER == 1 & ZINC_LBORRES>=57 & ZINC_LBORRES<=88 | ///
	TRIMESTER == 2 & ZINC_LBORRES>=51 & ZINC_LBORRES<=80 | ///
	TRIMESTER == 3 & ZINC_LBORRES>=50 & ZINC_LBORRES<=77
	replace ZINC = 3 if ///
	TRIMESTER == 1 & ZINC_LBORRES > 88 | ///
	TRIMESTER == 2 & ZINC_LBORRES > 80 | ///
	TRIMESTER == 3 & ZINC_LBORRES > 77 
	replace ZINC = . if ZINC_LBORRES == . 
	//replace missing if missing test result
	label var ZINC "Zinc (ug/dL)"
	
	label val ZINC low_normal_high
	//note: currently very little data, not in the reports
	
	**#5. Iron: hepcidin **ReMAPP Aim 3**
		*Trimester 1: 3-12.9
		*Trimester 2: 4.4-14.4
		*Trimester 3: 3.1-13.0
		
	tabstat IRON_HEP_LBORRES,by(SITE) stats(n p5 p25 p50 p75 p95)
	sum IRON_HEP_LBORRES if SITE=="Pakistan"
	return list
	global MEAN_HEP_PAK = `r(mean)'
	disp $MEAN_HEP_PAK
	
	if $MEAN_HEP_PAK > 100 {
	rename IRON_HEP_LBORRES IRON_HEP_LBORRES_OLD 
	gen IRON_HEP_LBORRES = IRON_HEP_LBORRES_OLD
	replace  IRON_HEP_LBORRES = IRON_HEP_LBORRES_OLD/1000 
	}
	

	
		
	gen HEP = 1 if /// low
	TRIMESTER == 1 & IRON_HEP_LBORRES < 3 | ///
	TRIMESTER == 2 & IRON_HEP_LBORRES < 4.4 | ///
	TRIMESTER == 3 & IRON_HEP_LBORRES < 3.1

	replace HEP = 2 if /// normal
	TRIMESTER==1 & IRON_HEP_LBORRES>=3 & IRON_HEP_LBORRES< 12.9 | ///
	TRIMESTER==2 & IRON_HEP_LBORRES>=4.4 & IRON_HEP_LBORRES<14.4 | /// 
	TRIMESTER==3 & IRON_HEP_LBORRES>=3.1 & IRON_HEP_LBORRES<13	
		
	replace HEP = 3 if /// high
	TRIMESTER == 1 & IRON_HEP_LBORRES>12.9 | ///
	TRIMESTER == 2 & IRON_HEP_LBORRES>14.4 | ///
	TRIMESTER == 3 & IRON_HEP_LBORRES>13
		
	replace HEP = . if IRON_HEP_LBORRES < 0
	replace HEP = . if IRON_HEP_LBORRES == . 
	label var HEP "Hepcidin"
	label val HEP low_normal_high
	//note: currently very little data, not in the reports
	

	
	**#6. TIBC (ug/dL) **ReMAPP Aim 3**
		*Trimester 1: 278-403
		*Trimester 2: 302-519
		*Trimester 3: 359-609
		*cite: Abbassi-Ghanavati 2009 & Perinatology
	tabstat IRON_TOT_UGDL_LBORRES, by(SITE) stat(min p50 max  n)	
	tabstat IRON_TOT_UGDL_LBORRES if IRON_TOT_UGDL_LBORRES>0, ///
	by(SITE) stat(n min p50 max)	
	**!! noting that there is likely a units issue with total iron binding capacity between sites; have notified Abby
	
	
	gen TIBC = 1 if ///
	TRIMESTER == 1 & IRON_TOT_UGDL_LBORRES < 278 | ///
	TRIMESTER == 2 & IRON_TOT_UGDL_LBORRES < 302 | ///
	TRIMESTER == 3 & IRON_TOT_UGDL_LBORRES < 359 
	replace TIBC = . if IRON_TOT_UGDL_LBORRES < 0
	
	replace TIBC = 2 if ///
	TRIMESTER == 1 & ///
	(IRON_TOT_UGDL_LBORRES>=278 & IRON_TOT_UGDL_LBORRES<403) | ///
	TRIMESTER == 2 & ///
	(IRON_TOT_UGDL_LBORRES>=302 & IRON_TOT_UGDL_LBORRES<519) | ///
	TRIMESTER == 3 & ///
	(IRON_TOT_UGDL_LBORRES>=359 & IRON_TOT_UGDL_LBORRES<609)
	
	replace TIBC = 3 if ///
	TRIMESTER == 1 & IRON_TOT_UGDL_LBORRES > 403 | ///
	TRIMESTER == 2 & IRON_TOT_UGDL_LBORRES > 519 | ///
	TRIMESTER == 3 & IRON_TOT_UGDL_LBORRES > 609 
	replace TIBC = . if IRON_TOT_UGDL_LBORRES == . 
	label val TIBC low_normal_high
	label var TIBC "Total iron-binding capacity"
	
	**#7. Vitamin A: serum retinol (ug/dL) **ReMAPP Aim 3**
		*Trimester 1: 32-47
		*Trimester 2: 35-44
		*Trimester 3: 29-42
		*cite: Abbassi-Ghanavati 2009
		
		*note updated recommendation from ERS:
		*<1.05 umol/L --> 29.4 ug/dL
		*<0.70 umol/L --> 19.6 ug/dL
		*<0.35 umol/L --> 9.8 ug/dL
		
		//note that BRINDA does not adjust serum retinol for WRA
	
	**!!temporary fix 
	sum VITA_UGDL_LBORRES if SITE == "Kenya"
	return list 
	global VITA_KY = `r(mean)'
	
	if $VITA_KY > 300 {
		gen convert_kenya = "yes" if SITE=="Kenya" & VITA_UGDL_LBORRES!=.
		rename VITA_UGDL_LBORRES VITA_UGDL_LBORRES_OLD
		gen VITA_UGDL_LBORRES=VITA_UGDL_LBORRES_OLD
		replace VITA_UGDL_LBORRES= VITA_UGDL_LBORRES_OLD/10 if SITE=="Kenya"
	}
	
	*hist VITA_UGDL_LBORRES if VITA_UGDL_LBORRES<., by(SITE, col(1))
	
	gen VITA = 4 if  VITA_UGDL_LBORRES >=29.4 &  VITA_UGDL_LBORRES<.
	replace VITA = 3 if  VITA_UGDL_LBORRES < 29.4  // 1.05 umol/L MILD
	replace VITA = 2  if  VITA_UGDL_LBORRES < 19.6  //		MODERATE
	replace VITA = 1 if  VITA_UGDL_LBORRES < 9.8  // 0.35 umol/L SEVERE
	
	replace VITA = . if VITA_UGDL_LBORRES == . 
	replace VITA = . if VITA_UGDL_LBORRES < 0 
	label define VITA 1"<0.35" 2"0.35-0.7" 3"0.7-1.05" 4">=1.05"
	label val VITA VITA
	label var VITA "Vitamin A: serum retinol (ug/dL)"
	
	**Note that <0.70 umol/L or 19.6 ug/dL is a common cutoff for VAD
	
	**#8. Folate: red blood cell (RBC) (nmol/L)
		*All trimesters: < 226.5 nmol/L (according to WHO 2015)
	
	gen FOL_RBC_CALC = FOLATE_RBC_NMOLL_LBORRES/(CBC_HCT_LBORRES/100) if ///
	inrange(FOLATE_RBC_NMOLL_LBORRES,0,.) & inrange(CBC_HCT_LBORRES,0,.)
	
	gen FOL_RBC = 1 if ///
	FOLATE_RBC_NMOLL_LBORRES < 226.5 
	replace FOL_RBC = . if FOLATE_RBC_NMOLL_LBORRES < 0
	replace FOL_RBC = 2 if ///
	FOLATE_RBC_NMOLL_LBORRES>=226.5 
	replace FOL_RBC = . if FOLATE_RBC_NMOLL_LBORRES == .
	replace FOL_RBC = . if TRIMESTER==. & TYPE_VISIT<=5

	label var FOL_RBC "Folate - RBC (nmol/L)"
	label var FOLATE_RBC_NMOLL_LBORRES "Folate - RBC (nmol/L)"
	
	
	gen FOL_RBC_ANC20 = FOL_RBC if TYPE_VISIT <= 2
	gen FOL_RBC_ANC32 = FOL_RBC if TYPE_VISIT >=4 & TYPE_VISIT <= 5
	gen FOL_RBC_PNC6 = FOL_RBC if TYPE_VISIT == 10
		
	gen FOL_RBC_ANC20_D =1 if FOL_RBC_ANC20>=1 & FOL_RBC_ANC20<=3
	gen FOL_RBC_ANC32_D =1 if FOL_RBC_ANC32>=1 & FOL_RBC_ANC32<=3
	gen FOL_RBC_PNC6_D =1 if FOL_RBC_PNC6>=1 & FOL_RBC_PNC6<=3
	
**#QUANSYS
	*1. Ferritin //iron
	*2. Iodine (Thyroglobulin) 
	*3. Serum transferrin receptor (sTfR) //iron 
	*4. Retinol binding protein 4 (RBP4) //vitamin a deficiency
	*5. C-reactive protein (CRP) //inflammation
	*6. Alpha 1-acid glycoprotein (AGP) //inflammation
	*7. Histidine-rich protein 2 //malaria


	**#1. Ferritin
	
	//The following lines are not needed because ferritin has been corrected previously in the mnh08_adjusted.csv file
	//corrects units issue at Ghana, CMC,  Kenya, Zambia
		*replace FERRITIN_LBORRES=FERRITIN_LBORRES*10 if ///
		*(SITE == "Ghana" |  SITE == "India-CMC" | SITE=="Kenya")

	**Question for how many ferritin values are in the data set?
	egen FERRITIN_TEST_NUM = total(!missing( FERRITIN_LBORRES ) ), by(MOMID PREGID)
	tab FERRITIN_TEST_NUM, m
	
	
/*
	**unresolved units issues with ferritin in Kenya
	*This has been resolved as of the 2025-01-10 data upload
	replace FERRITIN_LBORRES =. if SITE=="Kenya"
	replace SF_ADJ =. if SITE=="Kenya"
*/
		
	//VERSION 1: UNADJUSTED (DIFFERENT CUTOFF BASED ON INFLAMMATION STATUS)
	
	gen INFLAMMATION = 1 if CRP_LBORRES!=. | AGP_LBORRES!=.
	replace INFLAMMATION = 2 if ///
	CRP_LBORRES>5 & CRP_LBORRES!=. | AGP_LBORRES>1 & AGP_LBORRES!=.
	//INFLAMMATION IF EITHER CRP > 5 MG/L OR AGP > 1 G/L
	label define normal_high 1"Normal" 2"High"
	label val INFLAMMATION normal_high
	
	rename FERRITIN_LBORRES FERRITIN_LBORRES_OLD
	gen FERRITIN_LBORRES = FERRITIN_LBORRES*10
	gen FERRITIN_70 = 1 if FERRITIN_LBORRES!=.
	replace FERRITIN_70 = 2 if ///
	(INFLAMMATION == 2 & FERRITIN_LBORRES<70) | ///
	(INFLAMMATION == 1 & FERRITIN_LBORRES<15)
	//NOTE THAT <70 IS FOR UNADJUSTED, HIGH INFLAMMATION
	label define low 2"low ferritin"

	
	//concisely check the coding is correct
	tabstat FERRITIN_LBORRES if INFLAMMATION == 1, by( FERRITIN_70) stats(min max)
	tabstat FERRITIN_LBORRES if INFLAMMATION == 2, by( FERRITIN_70) stats(min max)
	
	/*
	gen FERRITIN_15 = 1 if SF_ADJ!=. // SF_ADJ is adjusted serum ferritin
	replace FERRITIN_15 = 2 if SF_ADJ<15	
	//NOTE THAT <15 IS FOR VALUES THAT HAVE BEEN ADJUSTED FOR INFLAMMATION
	tabstat SF_ADJ , by( FERRITIN_15) stats(min max)
	*/
	label val FERRITIN_70  low
	
	gen FERRITIN_D = 1 if FERRITIN_70 ==1 | FERRITIN_70==2
	
	gen FERRITIN70_ANC20 = FERRITIN_70 if TYPE_VISIT <= 2
	gen FERRITIN70_ANC32 = FERRITIN_70 if TYPE_VISIT >= 4 & TYPE_VISIT<= 5
	gen FERRITIN70_PNC6 = FERRITIN_70 if TYPE_VISIT == 10 
	
	/*
	gen FERRITIN15_ANC20 = FERRITIN_15 if TYPE_VISIT <= 2
	gen FERRITIN15_ANC32 = FERRITIN_15 if TYPE_VISIT >= 4 & TYPE_VISIT<= 5
	gen FERRITIN15_PNC6 = FERRITIN_15 if TYPE_VISIT == 10 
	*/
	gen INFLAMMATION_ANC20 = INFLAMMATION if TYPE_VISIT <=2
	gen INFLAMMATION_ANC32 = INFLAMMATION if TYPE_VISIT>=4 & TYPE_VISIT<=5
	gen INFLAMMATION_PNC6 = INFLAMMATION if TYPE_VISIT==10
	

	gen FERRITIN_ANC20_DENOM = 1 if ///
	FERRITIN70_ANC20 == 1 | FERRITIN70_ANC20 == 2
	gen FERRITIN_ANC32_DENOM = 1 if ///
	FERRITIN70_ANC32 == 1 | FERRITIN70_ANC32 == 2
	gen FERRITIN_PNC6_DENOM = 1 if ///
	FERRITIN70_PNC6 == 1 | FERRITIN70_PNC6 == 2
	
	gen FERRITIN_CONT_ANC20 = FERRITIN_LBORRES if inlist(TYPE_VISIT,1,2)
	gen FERRITIN_CONT_ANC32 = FERRITIN_LBORRES if inlist(TYPE_VISIT,4,5)
	gen FERRITIN_CONT_PNC6 = FERRITIN_LBORRES if inlist(TYPE_VISIT,10)
	
	/*
	gen SF_ADJ_CONT_ANC20 = SF_ADJ if inlist(TYPE_VISIT,1,2)
	gen SF_ADJ_CONT_ANC32 = SF_ADJ if inlist(TYPE_VISIT,4,5)
	gen SF_ADJ_CONT_PNC6 = SF_ADJ if inlist(TYPE_VISIT,10)
	*/
	gen INFLAMMATION_ANC20_DENOM = 1 if ///
	INFLAMMATION_ANC20 == 1 | INFLAMMATION_ANC20 == 2
	gen INFLAMMATION_ANC32_DENOM = 1 if ///
	INFLAMMATION_ANC32 == 1 | INFLAMMATION_ANC32 == 2
	gen INFLAMMATION_PNC6_DENOM = 1 if ///
	INFLAMMATION_PNC6 == 1 | INFLAMMATION_PNC6 == 2


	**#2. Iodine (thyroglobulin)
	**Note that high Tg indicates either low iodine or excess iodine
	**Low iodine is more likely in our context
	**the reference range is not determined

	gen HIGH_TG_44 = 1 if  IODINE_LBORRES!=.
	replace HIGH_TG_44 = 2 if ///
	IODINE_LBORRES>=43.5 &  IODINE_LBORRES!=.
	label var HIGH_TG_44 "2= Indicates >= 43.5 ug/L Tg"
	// Reference value from multi-country study of pregnant women 
	// conducted by Sara Stinca et al. 2017
	// doi: 10.1210/jc.2016-2829
	gen HIGH_TG_ANC20 = HIGH_TG_44 if TYPE_VISIT <= 2
	gen HIGH_TG_ANC32 = HIGH_TG_44 if TYPE_VISIT>=4 & TYPE_VISIT <= 5
	gen HIGH_TG_PNC6 = HIGH_TG_44 if TYPE_VISIT == 10	
	
	gen HIGH_TG_ANC20_DENOM  = 1 if ///
	HIGH_TG_ANC20 == 1 | HIGH_TG_ANC20 == 2
	
	gen HIGH_TG_ANC32_DENOM  = 1 if ///
	HIGH_TG_ANC32 == 1 | HIGH_TG_ANC32 == 2
	
	gen HIGH_TG_PNC6_DENOM  = 1 if ///
	HIGH_TG_PNC6==1 | HIGH_TG_PNC6 == 2
	
	**#3. Serum transferrin receptor (sTfR) (mg/L)
	*Trimester 1: 1.49-3.61 mg/L
	*Trimester 2: 2.93-4.98
	*Trimester 3: 3.52-5.94
	
	gen STFR = 1 if ///
	TRIMESTER == 1 & TRANSFERRIN_LBORRES < 1.49 | /// STFR_ADJ is the adjusted variable
	TRIMESTER == 2 & TRANSFERRIN_LBORRES < 2.93 | ///
	TRIMESTER == 3 & TRANSFERRIN_LBORRES < 3.52 | ///
	TYPE_VISIT == 10 & TRANSFERRIN_LBORRES < 1.41
	replace STFR = . if TRANSFERRIN_LBORRES < 0 
	
	label var STFR "sTfR (high=iron deficiency)"
	
	replace STFR = 2 if ///
	TRIMESTER == 1 & ///
	(TRANSFERRIN_LBORRES>=1.49 & TRANSFERRIN_LBORRES<3.61) | ///
	TRIMESTER == 2 & ///
	(TRANSFERRIN_LBORRES>=2.93 & TRANSFERRIN_LBORRES<4.98) | ///
	TRIMESTER == 3 & ///
	(TRANSFERRIN_LBORRES>=3.52 & TRANSFERRIN_LBORRES<5.94) | ///
	TYPE_VISIT == 10 & ///
	(TRANSFERRIN_LBORRES>=1.41 & TRANSFERRIN_LBORRES<3.52)
	
	replace STFR = 3 if ///
	TRIMESTER == 1 & TRANSFERRIN_LBORRES >= 3.61 | ///
	TRIMESTER == 2 & TRANSFERRIN_LBORRES >= 4.98 | ///
	TRIMESTER == 3 & TRANSFERRIN_LBORRES >= 5.94 | ///
	TYPE_VISIT == 10 & TRANSFERRIN_LBORRES >= 3.52
	replace STFR = . if TRANSFERRIN_LBORRES == . 
	
	bys TRIMESTER : tabstat TRANSFERRIN_LBORRES, by(STFR) ///
	stats (min max)
	// concisely check that cutoffs worked
	*Trimester 1: 1.49-3.61 mg/L
	*Trimester 2: 2.93-4.98
	*Trimester 3: 3.52-5.94
	
	
	label val STFR low_normal_high

	gen STFR_ANC20 = STFR if TYPE_VISIT <= 2
	gen STFR_ANC32 = STFR if TYPE_VISIT >= 4 & TYPE_VISIT <= 5
	gen STFR_PNC6 = STFR if TYPE_VISIT == 10 
	
	gen STFR_ANC20_DENOM = 1 if STFR_ANC20>=1 & STFR_ANC20<=3
	gen STFR_ANC32_DENOM = 1 if STFR_ANC32>=1 & STFR_ANC32<=3
	gen STFR_PNC6_DENOM = 1 if STFR_PNC6>=1 & STFR_PNC6<=3
	
	gen STFR_CONT_ANC20 = TRANSFERRIN_LBORRES if inlist(TYPE_VISIT,1,2)
	gen STFR_CONT_ANC32 = TRANSFERRIN_LBORRES if inlist(TYPE_VISIT,4,5)
	gen STFR_CONT_PNC6 = TRANSFERRIN_LBORRES if inlist(TYPE_VISIT,10)
	
	**#4. Retinol binding protein 4 (RBP4)
	*Reference values from ERS		
	//note that RBP4 is not adjusted in BRINDA package
		
	gen RBP4 = 1 if RBP4_LBORRES < 0.30
	//severe deficiency

	replace RBP4 = 2 if RBP4_LBORRES>=0.3 & RBP4_LBORRES<0.7
	//moderate deficiency
	
	replace RBP4 = 3 if RBP4_LBORRES>=0.7 & RBP4_LBORRES<1.05
	//mild
	
	replace RBP4 = 4 if RBP4_LBORRES>=1.05 & RBP4_LBORRES!=.

	replace RBP4 = . if RBP4_LBORRES < 0
	replace RBP4 = . if RBP4_LBORRES == . 
	replace RBP4 = . if (TRIMESTER == . & TYPE_VISIT <=5)
	
	label var RBP4 "RBP4, severe: <0.3, moderate: <0.7, mild: <1.05"
	
	gen RBP4_CONT_ANC20 = RBP4_LBORRES if inlist(TYPE_VISIT,1,2)
	gen RBP4_CONT_ANC32 = RBP4_LBORRES if inlist(TYPE_VISIT,4,5)
	gen RBP4_CONT_PNC6 = RBP4_LBORRES if inlist(TYPE_VISIT,10)
	
	gen RBP4_ANC20 = RBP4 if TYPE_VISIT <= 2
	gen RBP4_ANC32 = RBP4 if TYPE_VISIT >=4 & TYPE_VISIT <=5
	gen RBP4_PNC6 = RBP4 if TYPE_VISIT == 10
	
	gen RBP4_ANC20_DENOM = 1 if RBP4_ANC20>=1 & RBP4_ANC20 <=4
	gen RBP4_ANC32_DENOM = 1 if RBP4_ANC32>=1 & RBP4_ANC32 <=4
	gen RBP4_PNC6_DENOM = 1 if RBP4_PNC6>=1 & RBP4_PNC6<=4
	
	label define RBP4 1"Severe deficiency" 2"Moderate" 3"Mild" 4"None"
	label val RBP4 RBP4_ANC20 RBP4_ANC32 RBP4_PNC6 RBP4 

	**#5. C-reactive protein (CRP) (mg/L)
	*Use the same cutoffs as for ReMAPP healthy cohort criteria
	gen CRP = 1 if CRP_LBORRES <= 5 
	replace CRP = 2 if CRP_LBORRES > 5 & CRP_LBORRES!=. 
	*replace CRP = . if TRIMESTER ==. & TYPE_VISIT<=5
	label var CRP "CRP, 1: less than 5mg/L, 2: >5mg/L"
	
	tabstat CRP_LBORRES, by(CRP) statistics(min max)
	
	gen CRP_ANC20 = CRP if TYPE_VISIT <= 2 
	gen CRP_ANC32 = CRP if TYPE_VISIT >=4 & TYPE_VISIT <=5
	gen CRP_PNC6 = CRP if TYPE_VISIT == 10 
	
	gen CRP_ANC20_DENOM = 1 if CRP_ANC20 >=1 & CRP_ANC20 <=3
	gen CRP_ANC32_DENOM = 1 if CRP_ANC32 >=1 & CRP_ANC32 <=3
	gen CRP_PNC6_DENOM = 1 if CRP_PNC6 >= 1 & CRP_PNC6 <= 3
	
	gen CRP_CONT_ANC20 = CRP_LBORRES if inlist(TYPE_VISIT,1,2)
	gen CRP_CONT_ANC32 = CRP_LBORRES if inlist(TYPE_VISIT,4,5)
	gen CRP_CONT_PNC6 = CRP_LBORRES if inlist(TYPE_VISIT,10)
	
	**#6. Alpha 1-acid glycoprotein (g/L)
	*Use the same cutoffs as for ReMAPP healthy cohort criteria	
	gen AGP = 1 if AGP_LBORRES <= 1 
	replace AGP = 2 if AGP_LBORRES >1 & AGP_LBORRES !=.
	*replace AGP = . if TRIMESTER ==. & TYPE_VISIT<=5
	
	label var AGP "AGP, 1: less than 1g/L, 2: > 1g/L"
	
	tabstat AGP_LBORRES, by(AGP) statistics(min max)
	
	gen AGP_ANC20 = AGP if TYPE_VISIT <=2 
	gen AGP_ANC32 = AGP if TYPE_VISIT >=4 & TYPE_VISIT <=5
	gen AGP_PNC6 = AGP if TYPE_VISIT == 10
	
	gen AGP_ANC20_DENOM = 1 if AGP_ANC20>=1 & AGP_ANC20<=2
	gen AGP_ANC32_DENOM = 1 if AGP_ANC32>=1 & AGP_ANC32<=2
	gen AGP_PNC6_DENOM = 1 if AGP_PNC6>=1 & AGP_PNC6<=2
	
	gen AGP_CONT_ANC20 = AGP_LBORRES if inlist(TYPE_VISIT,1,2)
	gen AGP_CONT_ANC32 = AGP_LBORRES if inlist(TYPE_VISIT,4,5)
	gen AGP_CONT_PNC6 = AGP_LBORRES if inlist(TYPE_VISIT,10)
	
	**#7. Histidine-rich protein 2	
	*Reference range: >= 40.84 pg/mL is high (== malaria)
	*Note that PRISMA reports values in ug/mL
	*Quansys reports in ug/L, sites need to convert to ug/mL
	
	/*
	gen HRP = 1 if HRP_LBORRES<.000004084
	*note that lower limit of detection is 0.000204 ug/mL
	replace HRP = 2 if HRP_LBORRES >=.00004084 & HRP_LBORRES !=.
	replace HRP = . if TRIMESTER == . & TYPE_VISIT<=5
	label val HRP low_normal_high
	label var HRP "HRP2, high: malaria"
	
	gen HRP_ANC20 = HRP if TYPE_VISIT <= 2
	gen HRP_ANC32 = HRP if TYPE_VISIT >= 4 & TYPE_VISIT <= 5
	gen HRP_PNC6 = HRP if TYPE_VISIT == 10 
	
	gen HRP_ANC20_D = 1 if HRP_ANC20>=1 & HRP_ANC20<=3
	gen HRP_ANC32_D = 1 if HRP_ANC32>=1 & HRP_ANC32<=3
	gen HRP_PNC6_D = 1 if HRP_PNC6>=1 & HRP_PNC6<=3
*/
	
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
	gen QUANSYS_TEST_ANC20 = QUANSYS_TEST if inlist(TYPE_VISIT, 1 , 2)
	gen QUANSYS_TEST_ANC32 = QUANSYS_TEST if inlist(TYPE_VISIT, 4 , 5)
	gen QUANSYS_TEST_PNC6 = QUANSYS_TEST if TYPE_VISIT==10
	

	
**#Hemoglobin: 
	gen HB_LBORRES = CBC_HB_LBORRES if CBC_HB_LBORRES != .
	destring HB_LBORRES, replace 
	replace HB_LBORRES = . if HB_LBORRES < 0  // clean 9,690 observations
	replace HB_LBORRES = . if HB_LBORRES >= 99 // clean 0 observation
	
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
	
**#Blood lead **ReMAPP Aim 3**
	gen LEAD5 = 1 if BLEAD_LBORRES<5
	replace LEAD5= 2 if BLEAD_LBORRES>=5 & BLEAD_LBORRES <.
	label define LEAD5 1"<5 ug/dL" 2">=5 ug/dL"
	label val LEAD5 LEAD5
	tab LEAD5 SITE, col
	tab LEAD5 TRIMESTER, col
	
	gen LEAD10 = 1 if BLEAD_LBORRES <10
	replace LEAD10 = 2 if BLEAD_LBORRES>=10 & BLEAD_LBORRES<.
	label define LEAD10 1"<10 ug/dL" 2">=10 ug/dL"
	label val LEAD10 LEAD10
	tab LEAD10 SITE,col
	
	
	foreach var in HEP TIBC  VITA ZINC LEAD10 {
		foreach num of numlist 1/3 {
		gen `var'_T`num' = `var' if TRIMESTER == `num'
		label var `var'_T`num' "`var' in Trimester `num'"
	}
	}
	
	merge m:1 MOMID PREGID using "$outcomes/MAT_ENROLL.dta", keepusing(M01_US_OHOSTDAT) nogenerate
	
	*Who is also in ReMAPP?
	gen remappdate = "2022-12-28" if SITE =="Ghana"
	replace remappdate = "2023-04-03" if SITE=="Kenya"
	replace remappdate = "2022-12-15" if SITE=="Zambia"
	replace remappdate = "2022-09-22" if SITE=="Pakistan"
	replace remappdate = "2023-06-20" if SITE=="India-CMC"
	replace remappdate = "2023-08-15" if SITE=="India-SAS"
	gen remappend = "2024-04-05" if SITE=="Pakistan"
	replace remappend = "2024-10-29" if SITE=="Ghana"
	gen ReMAPPEnd =date(remappend,"YMD")
	gen ReMAPPDate=date(remappdate, "YMD")
	format ReMAPPDate ReMAPPEnd %td
	drop remappdate remappend
	gen REMAPP = 0
	replace REMAPP = 1 if date(ENROLL_SCRN_DATE,"YMD")>=ReMAPPDate
	*keep if M01_US_OHOSTDAT>=ReMAPPDate
	replace REMAPP=0 if date(ENROLL_SCRN_DATE,"YMD")>ReMAPPEnd & SITE=="Pakistan"
	replace REMAPP=0 if date(ENROLL_SCRN_DATE,"YMD")>ReMAPPEnd & SITE=="Ghana"
	
	drop ReMAPPEnd ReMAPPDate
	
	
	
save "$wrk/MAT_NUTR_LONG.dta" , replace 
cap mkdir "$savannah/Maternal Outcomes\ReMAPP Aim 3/$datadate/"
save "$savannah/Maternal Outcomes\ReMAPP Aim 3/$datadate/MAT_NUTR_LONG.dta", replace

**#Create a collapsed data file

sort SITE  MOMID PREGID LBSTDAT
collapse (firstnm) ENROLL  REMAPP MCV_ANC20 MCV_ANC20_DENOM  MCV_ANC32  MCV_ANC32_DENOM  VITB12_COB_ANC20 VITB12_COB_ANC32 VITB12_COB_ANC20_DENOM VITB12_COB_ANC32_DENOM FERRITIN70_ANC20 FERRITIN70_ANC32   FERRITIN_ANC20_DENOM FERRITIN_ANC32_DENOM HIGH_TG_ANC20 HIGH_TG_ANC32 HIGH_TG_ANC20_DENOM  HIGH_TG_ANC32_DENOM  STFR_ANC20 STFR_ANC32 STFR_ANC20_DENOM STFR_ANC32_DENOM RBP4_ANC20 RBP4_ANC32 RBP4_ANC20_DENOM RBP4_ANC32_DENOM CRP_ANC20 CRP_ANC32 CRP_ANC20_DENOM CRP_ANC32_DENOM AGP_ANC20 AGP_ANC32 AGP_ANC20_DENOM AGP_ANC32_DENOM  MCV_PNC6 MCV_PNC6_DENOM VITB12_COB_PNC6 VITB12_COB_PNC6_DENOM   FERRITIN70_PNC6  FERRITIN_PNC6_DENOM HIGH_TG_PNC6 HIGH_TG_PNC6_DENOM  STFR_PNC6 STFR_PNC6_DENOM RBP4_PNC6 RBP4_PNC6_DENOM CRP_PNC6 CRP_PNC6_DENOM AGP_PNC6 AGP_PNC6_DENOM  	INFLAMMATION_ANC20 INFLAMMATION_ANC20_DENOM INFLAMMATION_ANC32 INFLAMMATION_ANC32_DENOM INFLAMMATION_PNC6 INFLAMMATION_PNC6_DENOM QUANSYS_TEST_ANC20 QUANSYS_TEST_ANC32 QUANSYS_TEST_PNC6 ///
FERRITIN_CONT_ANC20 FERRITIN_CONT_ANC32 FERRITIN_CONT_PNC6 CRP_CONT_ANC20 CRP_CONT_ANC32 CRP_CONT_PNC6 AGP_CONT_ANC20 AGP_CONT_ANC32 AGP_CONT_PNC6 STFR_CONT_* RBP4_CONT_* VITB12_CONT_* , ///
by(SITE MOMID PREGID)

*save "$outcomes/MAT_NUTR-CONTN-MEASURES.dta" , replace


keep if ENROLL==1

*Label variables
label define FERRITIN70 1"Not low" 2"Ferritin <70ug/L",replace
label val FERRITIN70_ANC20 FERRITIN70_ANC32  FERRITIN70_PNC6 FERRITIN70
label var FERRITIN70_ANC20 "Ferritin, unadjusted"
label var FERRITIN70_ANC32 "Ferritin, unadjusted"

/*
label define FERRITIN15 1"Not low" 2"Ferritin <15ug/L",replace
label val FERRITIN15_ANC20 FERRITIN15_ANC32 FERRITIN15
label var FERRITIN15_ANC20 "Ferritin, adjusted"
label var FERRITIN15_ANC32 "Ferritin, adjusted"
*/
label define HIGH_TG 1"Normal" 2"High",replace
label val HIGH_TG_ANC20 HIGH_TG_ANC32 HIGH_TG

label define STFR 1"Low" 2"Normal" 3"High",replace
label val STFR_ANC20 STFR_ANC32 STFR

label define RBP4 1"Severe deficiency" 2"Moderate" 3"Mild" 4"None",replace
label val RBP4_ANC20 RBP4_ANC32 RBP4 

label define INFL 1"Normal" 2"High",replace
label val CRP_ANC20 CRP_ANC32 AGP_ANC20 AGP_ANC32 INFL

*label define HRP 1"Normal" 2"High",replace
*label val HRP_ANC20 HRP_ANC32 HRP

label define VITB12 1"Deficient" 2"Insufficient" 3"Sufficient",replace
label val VITB12_COB_ANC20 VITB12_COB_ANC32 VITB12




label data "All participants. Data date: $datadate; `c(username)' modified `c(current_date)'"
local datalabel: data label
disp "`datalabel'" //displays dataset label you just assigned

*save "$outcomes/MAT_NUTR_ALL.dta", replace

merge 1:1 MOMID PREGID using "Z:\Savannah_working_files\Expected_obs-$datadate.dta" , gen(merge_exp)

*incorporate the "expected" indicators into the denominators

	foreach var in MCV_ANC20 MCV_ANC20_DENOM VITB12_COB_ANC20 VITB12_COB_ANC20_DENOM  FERRITIN70_ANC20  FERRITIN_ANC20_DENOM HIGH_TG_ANC20 HIGH_TG_ANC20_DENOM  STFR_ANC20 STFR_ANC20_DENOM RBP4_ANC20 RBP4_ANC20_DENOM CRP_ANC20 CRP_ANC20_DENOM AGP_ANC20 AGP_ANC20_DENOM  INFLAMMATION_ANC20 INFLAMMATION_ANC20_DENOM QUANSYS_TEST_ANC20   {
		*replace `var' = . if ANC20_EXP != 1
		*set to  missing if not expected at this time point yet (e.g., not passed window)
		replace `var' = 55 if ANC20_EXP == 1 & `var' == .
		*set to 55, missing, if expected but not available
	}

	foreach var in MCV_ANC32 MCV_ANC32_DENOM AGP_ANC32 AGP_ANC32_DENOM CRP_ANC32 CRP_ANC32_DENOM  FERRITIN70_ANC32 FERRITIN_ANC32_DENOM  HIGH_TG_ANC32 HIGH_TG_ANC32_DENOM   RBP4_ANC32 RBP4_ANC32_DENOM STFR_ANC32 STFR_ANC32_DENOM  VITB12_COB_ANC32 VITB12_COB_ANC32_DENOM  INFLAMMATION_ANC32 INFLAMMATION_ANC32_DENOM QUANSYS_TEST_ANC32  {
		*replace `var'= . if ANC32_EXP!=1
		replace `var' = 55 if ANC32_EXP==1 & `var' == .
	}

	foreach var in VITB12_COB_PNC6 VITB12_COB_PNC6_DENOM  FERRITIN70_PNC6  FERRITIN_PNC6_DENOM HIGH_TG_PNC6 HIGH_TG_PNC6_DENOM  STFR_PNC6 STFR_PNC6_DENOM RBP4_PNC6 RBP4_PNC6_DENOM CRP_PNC6 CRP_PNC6_DENOM AGP_PNC6 AGP_PNC6_DENOM MCV_PNC6 MCV_PNC6_DENOM INFLAMMATION_PNC6 INFLAMMATION_PNC6_DENOM QUANSYS_TEST_PNC6  {
		*replace `var' = . if PNC6_EXP!=1
		replace `var' = 55 if PNC6_EXP==1 & `var' == . 
	}
  
  
keep SITE MOMID PREGID ENROLL REMAPP MCV_* VITB12_* FERRITIN* HIGH_TG* STFR* RBP4* CRP* AGP* INFLAMMATION* QUANSYS_TEST* ANC20_EXP ANC32_EXP PNC6_EXP
order SITE MOMID PREGID ENROLL REMAPP MCV_* VITB12_* FERRITIN* HIGH_TG* STFR* RBP4* CRP* AGP* INFLAMMATION* QUANSYS_TEST* ANC20_EXP ANC32_EXP PNC6_EXP 

foreach analyte in  "STFR" "RBP4" "CRP" "AGP" {
	
	foreach time in "ANC20" "ANC32" "PNC6" {
	label var `analyte'_`time' "`analyte' at `time'"
	label var `analyte'_`time'_DENOM "valid measurement of `analyte' at `time'"
	label var `analyte'_CONT_`time' "continuous value of `analyte' at `time'"
	}
	
}


foreach time in "ANC20" "ANC32" "PNC6" {
	label var MCV_`time' "MCV at `time'"
	label var MCV_`time'_DENOM "valid measurement of MCV at `time'"

	label var VITB12_COB_`time' "B12 at `time'"
	label var VITB12_COB_`time'_DENOM "valid B12 at `time'"
	label var VITB12_CONT_`time' "continuous value of B12 at `time'"
	
	label var FERRITIN70_`time'  "Low ferritin at `time' (<70 if inflammation)"
	label var FERRITIN_`time'_DENOM "valid ferritin at `time'"
	label var FERRITIN_CONT_`time' "continuous ferritin at `time'"
	
	label var HIGH_TG_`time' "High tg at `time'"
	label var HIGH_TG_`time'_DENOM "valid tg at `time'"
	
	label var INFLAMMATION_`time' "Inflammation at `time'"
	label var INFLAMMATION_`time'_DENOM "valid inflammation at `time'"
	label var QUANSYS_TEST_`time' "quansys test performed at `time'"
	}

label data "Data date: $datadate; `c(username)' modified `c(current_date)'"
local datalabel: data label
disp "`datalabel'" //displays dataset label you just assigned

cap drop uploaddate 
cap drop UploadDate 
cap drop merge_exp
	
	save "$wrk/MAT_NUTR-$datadate.dta" , replace
	save "$outcomes/MAT_NUTR.dta" , replace
	
**Final section is optional, to investigate multiple etiologies	
	import delimited "$outcomes/MAT_INFECTION.csv", ///
		bindquote(strict) varnames(1) case(upper) clear 
	merge 1:1 MOMID PREGID using "$outcomes/MAT_NUTR_ALL.dta"

	save "$wrk/NUTR_INFECTIONS.dta"

	import delimited "$outcomes/MAT_RBC.csv", ///
	bindquote(strict) varnames(1) case(upper) clear 

	merge 1:1 MOMID PREGID using "$wrk/NUTR_INFECTIONS.dta", gen(RBC_MERGE)
	replace RBC_THALASSEMIA ="" if RBC_THALASSEMIA=="NA"
	replace RBC_G6PD="" if RBC_G6PD=="NA"
	destring RBC_G6PD, replace
	replace RBC_THALASSEMIA = "0" if RBC_THALASSEMIA =="Normal"
	replace RBC_THALASSEMIA = "1" if RBC_THALASSEMIA =="Disease"

	destring RBC_THALASSEMIA,replace

	gen ferritin = 0 if FERRITIN70_ANC20 == 1
	replace ferritin = 1 if FERRITIN70_ANC20 == 2
	
	gen b12 = 0 if inlist(VITB12_COB_ANC20 ,3)
	replace b12 = 1 if inlist(VITB12_COB_ANC20 ,1,2)
	
	gen agp = 0 if AGP_ANC20 == 1
	replace agp = 1 if AGP_ANC20 == 2
	
	gen crp = 0 if CRP_ANC20 == 1
	replace crp = 1 if CRP_ANC20 == 2
	
	gen rbp4 = 0 if RBP4_ANC20 == 4
	replace rbp4 = 1 if inlist(RBP4_ANC20,1,2,3)
	
	egen etiologies = rowtotal(ferritin b12 agp crp rbp4 RBC_THALASSEMIA RBC_G6PD HIV_POSITIVE_ENROLL SYPH_POSITIVE_ENROLL MAL_POSITIVE_ENROLL HBV_POSITIVE_ENROLL HCV_POSITIVE_ENROLL)
	
	gen etiologies_bin 
	
	
	gen ferritin_str = "iron_deficient" if FERRITIN70_ANC20 == 2
	
	
	gen b12_str = "b12_insuff_def" if inlist(VITB12_COB_ANC20 ,1,2)
	
	gen agp_str ="high_agp" if AGP_ANC20 == 2
	
	gen crp_str = "high_crp" if CRP_ANC20 == 2
	
	gen rbp4_str =  "VAD" if inlist(RBP4_ANC20,1,2,3)
	
	foreach infection in HIV SYPH MAL HBV HCV {
		gen `infection'_str = "`infection'" if `infection'_POSITIVE_ENROLL ==1
	}
	
	foreach genetic in G6PD THALASSEMIA {
		gen `genetic'_str ="`genetic'" if RBC_`genetic' == 1
	}
	
	egen etiology_string = concat(ferritin_str b12_str agp_str crp_str rbp4_str HIV_str SYPH_str MAL_str HBV_str HCV_str G6PD_str THALASSEMIA_str), punct(" ")
	
*Save in outcomes folder once reviewed:
*save "$outcomes/MAT_NUTR.dta" , replace

	**check if SITE / MOMID / PREGID is missing for any participants
	foreach var in SITE MOMID PREGID {
		qui tab `var' if `var' =="", miss
		disp as text "Missing `var' = " as result _col(20) r(N)
	}
missvarstr SITE MOMID PREGID



/*
**#CREATE MAT_NUTR_RFA.DTA
**currently only for ANC20 and ANC32 risk factors

//create indicators 
	//1 = yes at that value
	//0 = otherwise
foreach time in ANC20 ANC32 {
	
	
foreach var of varlist 	 HIGH_TG_`time'  {
		
		foreach num of numlist 1/2 55{
		
        gen `var'_`num' = 0  if `time'_EXP==1 
		*expected at that time point
		
        replace `var'_`num' = 1 if `var' == `num'
            
        }
        
        label var `var'_55 "Missing at `time'"
		label var `var'_1 "Normal TG at `time'"
		label var `var'_2 "High TG at `time'"
       
        }	
	
foreach var of varlist  CRP_`time' AGP_`time' HRP_`time'   {          

        foreach num of numlist 1/2 55 {
            
        gen `var'_`num' = 0 if `time'_EXP==1
        replace `var'_`num' = 1 if `var' == `num'
            
        }
        
        label var `var'_55 "Missing at `time'"
        label var `var'_1 "Normal at `time'"
        label var `var'_2 "High at `time'"
        
        }	

		
foreach var of varlist 	FERRITIN70_`time' FERRITIN15_`time'  {
		
		foreach num of numlist 1/2 55{
		
        gen `var'_`num' = 0  if `time'_EXP==1
        replace `var'_`num' = 1 if `var' == `num'
            
        }
        
        label var `var'_55 "Missing at `time'"
        label var `var'_1 "Normal at `time'"
        label var `var'_2 "Deficient at `time'"
       
        }	
		
foreach var of varlist 	 MCV_`time'  {
	foreach num of numlist 1/3 55{
		
        gen `var'_`num' = 0  if `time'_EXP==1
        replace `var'_`num' = 1 if `var' == `num'
            
        }
        
        label var `var'_55 "Missing at `time'"
		label var `var'_1 "Microcytic at `time'"
        label var `var'_2 "Normocytic at `time'"
        label var `var'_3 "Macrocytic at `time'"
       
        }	
	
	foreach var of varlist 	 VITB12_COB_`time'  {
	foreach num of numlist 1/3 55{
		
        gen `var'_`num' = 0  if `time'_EXP==1
        replace `var'_`num' = 1 if `var' == `num'
            
        }
        
        label var `var'_55 "Missing at `time'"
		label var `var'_1 "Deficient at `time'"
        label var `var'_2 "Insufficient at `time'"
        label var `var'_3 "Sufficient at `time'"
       
        }	
	
	
		foreach var of varlist 	 STFR_`time'  {
		foreach num of numlist 1/3 55{
		
        gen `var'_`num' = 0  if `time'_EXP==1
        replace `var'_`num' = 1 if `var' == `num'
            
        }
        
        label var `var'_55 "Missing at `time'"
		label var `var'_1 "Low at `time'"
        label var `var'_2 "Normal at `time'"
        label var `var'_3 "High at `time'"
       
        }
		
		foreach var of varlist 	 RBP4_`time'  {
		foreach num of numlist 1/4 55{
		
        gen `var'_`num' = 0  if `time'_EXP==1
        replace `var'_`num' = 1 if `var' == `num'
            
        }
        
        label var `var'_55 "Missing at `time'"
		label var `var'_1 "Severe deficiency at `time'"
		label var `var'_2 "Moderate deficiency at `time'"
        label var `var'_3 "Mild deficiency at `time'"
        label var `var'_4 "Sufficient at `time'"
       
        }
	

}	


*keep SITE MOMID PREGID HIGH_TG_ANC20_0-RBP4_ANC32_55

save "$outcomes/MAT_NUTR_RFA.dta", replace


**# Merge with Anemia file

	use "Z:\Erin_working_files\data\ANEMIA_all_long.dta"  , clear
	keep if TEST_TYPE=="CBC"
	rename TEST_GA GA_DAYS
	drop if TYPE_VISIT==.
	keep if !missing( HB_LBORRES)
	
	merge 1:m MOMID PREGID TYPE_VISIT GA_DAYS using ///
	"$wrk/mnh08_allvisits-$datadate.dta", gen(anemiamerge) force
	keep if anemiamerge==3
	
	*Code contributing causes to anemia
	
	*Low ferritin
	gen FERRITIN_LT70 = "Fe_LT70" if FERRITIN_70 == 2
	*Low B12
	gen B12 = "B12_Def" if inlist(VITB12_COB,1,2)
	*replace B12 = "B12_In" if  VITB12_COB==2
	
	*VAD
	gen VAD = "VAD" if inlist(RBP4,1,2,3)
	*replace VAD = "VAD_mod" if RBP4==2
	*replace VAD = "VAD_mild" if RBP4==3
	
	*Inflammation
	gen INF = "Inflam" if INFLAMMATION==2
	
	egen factors_nutr=concat(FERRITIN_LT70 B12 VAD INF ), punct(" ")
	replace factors ="None" if factors=="" & !missing(FERRITIN_70)
	
	*Microcytic anemia:
	groups factors_nutr if HB11==1 & MCV==1 & !missing(FERRITIN_70), ///
	order(h) select(10) miss
	
	*Normocytic anemia:
	groups factors_nutr if HB11==1 & MCV==2 & !missing(FERRITIN_70), ///
	order(h) select(10) miss
	
	*Macrocytic anemia:
	groups factors_nutr if HB11==1 & MCV==3 & !missing(FERRITIN_70), ///
	order(h) select(10) miss
	
	*Non-anemic:
	groups factors_nutr if HB11==0  & !missing(FERRITIN_70), ///
	order(h) select(10) miss
	
	
	
	save "$wrk/NUTR_ANEMIA_LONG.dta", replace
	
	import delimited "Z:\Outcome Data\2024-06-28\MAT_INFECTION.csv", ///
	bindquote(strict) case(upper) clear 
	merge 1:m MOMID PREGID using "$wrk/NUTR_ANEMIA_LONG.dta"
	
	*Malaria
	gen MALARIA = "Malaria" if ///
	MAL_POSITIVE_ENROLL==1 | MAL_POSITIVE_ANY_VISIT==1
	
	*HIV
	gen HIV = "HIV" if ///
	HIV_POSITIVE_ENROLL==1 |  HIV_POSITIVE_ANY_VISIT==1
	
	gen THAL = "Thalassemia" if ANY_THAL==1
	
	egen factors=concat(FERRITIN_LT70 B12 VAD INF MALARIA HIV ) if HB11==1, punct(" ")
	replace factors ="None" if factors=="" & !missing(FERRITIN_70)
	
	
	**Look at the results:
	
	*Microcytic anemia:
	groups factors if HB11==1 & MCV==1 & !missing(FERRITIN_70), ///
	order(h) select(10) miss
		*shows me the top 10 results
		*only for those with a nonmissing ferritin
		
	**Check by continent	
	groups factors if HB11==1 & MCV==1 & !missing(FERRITIN_70) & inlist(SITE, "Ghana", "Kenya", "Zambia"), order(h) select(10) miss

	groups factors if HB11==1 & MCV==1 & !missing(FERRITIN_70) & inlist(SITE, "India-CMC", "India-SAS", "Pakistan"), order(h) select(10) miss
		
	*Macrocytic anemia:
	groups factors if HB11==1 & MCV==3 & !missing(FERRITIN_70), ///
	order(h) select(10) miss	
	
	save "$wrk/NUTR_ANEMIA_LONG.dta", replace
	

	
	
	
