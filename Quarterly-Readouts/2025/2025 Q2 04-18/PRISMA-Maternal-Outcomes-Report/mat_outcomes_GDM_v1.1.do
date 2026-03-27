*PRISMA Maternal Variable Construction Code - GDM
*Purpose: This code drafts variable construction code for maternal outcome
	*variables for the PRISMA study - Gestational Diabetes
*Original Version: April 11, 2024
*Update 1: Fix HbA1c measure (should be >=6.5%)
*Update 2: Updates to denominator based on feedback from ERS & CRS
*Update 3: Exclude India-SAS from 1hr OGTT (not being completed)
*Update 4: Add variables for GDM analysis: DIAB_OVERT_DX & treatment at L&D
*Update 5: Construct additional therholds for GDM (support future analysis)
*Update 6: Edit the denominator to accomodate early OGTTs (July 15, 2024)
*Update 7: Upates to file pathways after MAT_ENDPOINTS code was updated (October 1, 2024)
*Update 8: Update to the variable naming convention (January 10, 2025)
*Update 9: Update to always select the same OGTT (most recent valid test) (May 13, 2025)

clear
set more off
cap log close

*Directory structure:

	// Erin's folders: 
global dir  "D:\Users\emoakley\Documents\Maternal Outcome Construction" 
global log "$dir/logs"
global do "$dir/do"
global output "$dir/output"

	// Stacked Data Folders (TNT Drive)
global dadate "2025-04-18"
global da "Z:/Stacked Data/$dadate" // 
globa OUT "Z:/Outcome Data/$dadate"


	// Working Files Folder (TNT-Drive)
global wrk "Z:/Erin_working_files/data"

global date "250513" 

log using "$log/mat_outcome_construct_gdm_$date", replace

/*
Savannah's setup codes:


	// Stacked Data Folders (TNT Drive)
global dadate "2025-05-02"
global da "Z:/Stacked Data/$dadate" // 
global OUT "Z:\Savannah_working_files\GDM/$dadate"



	// Working Files Folder (TNT-Drive)
global wrk "$OUT"
cap mkdir "Z:\Savannah_working_files\GDM/$dadate" //make this folder if it does not exist

local date: di %td_CCYY_NN_DD daily("`c(current_date)'", "DMY")
global today = subinstr(strltrim("`date'"), " ", "-", .)
disp "$today"

*/


/* Gestational Diabetes - Measured & Clinical at multiple time points 

	DIAB_OVERT
		Defined: HbA1c percent at enrollment >=6.5%
		Denominator: All enrolled women 
		Response options: yes(1); no(0); missing (55)
		Timing: Enrollment OR <20 weeks GA 
		
	DIAB_OVERT_DX 
		Defined: Reported dx of preexisting CHRONIC diabetes at enrollment 
		Denominator" All enrolled women
		Response options: yes(1); no(0); missing (55)
		Timing: Enrollment OR first available data from ANC form 
		
	DIAB_OVERT_ANY 
		Defined: Woman has preexisting CHRONIC diabetes based on HbA1c or reported dx
		Denominator" All enrolled women
		Response options: yes(1); no(0); missing (55)
		Timing: Enrollment OR first available data from ANC form 
		
	DIAB_GEST_ANY 
		Defined: Results from pretest, 1-hour, and 2-hour 75g oral glucose 
		tolerance test at ANC-28 or prior to delivery if visit missed (fasting 
		≥ 5.1 mmol/L OR 1-hr ≥ 10.0 mmol/L OR 2-hr ≥ 8.5 mmol/L).
		Denominator: Women w/o pre-existing/overt diabetes >=20 weeks GA 
		Response options: yes(1); no(0); missing (55)
		Timing: ANC-28 or later
		
	DIAB_GEST_FASTING 
		Defined: Results from pretest 75g oral glucose tolerance test at 
		ANC-28 or prior to delivery if visit missed (fasting ≥ 5.1 mmol/L OR 
		1-hr ≥ 10.0 mmol/L OR 2-hr ≥ 8.5 mmol/L).
		Denominator Women w/o pre-existing/overt diabetes >=20 weeks GA 
		Response options: yes(1); no(0); missing (55)
		Timing: ANC-28 or later
		
	DIAB_GEST_1HR
		Defined: Results from 1-hour 75g oral glucose tolerance test at 
		ANC-28 or prior to delivery if visit missed (1-hr ≥ 10.0 mmol/L).
		Denominator Women w/o pre-existing/overt diabetes >=20 weeks GA 
		Response options: yes(1); no(0); missing (55)
		Timing: ANC-28 or later	
	
	
	DIAB_GEST_2HR
		Defined: Results from pretest, 1-hour, and 2-hour 75g oral glucose 
		tolerance test at ANC-28 or prior to delivery if visit missed (fasting 
		≥ 5.1 mmol/L OR 1-hr ≥ 10.0 mmol/L OR 2-hr ≥ 8.5 mmol/L).
		Denominator: Women w/o pre-existing/overt diabetes >=20 weeks GA 
		Response options: yes(1); no(0); missing (55)
		Timing: ANC-28 or later
		
	DIAB_GEST_DX
		Defined: Clinical dx of gestational diabetes, as indicated in 
		L&D form (MNH09) and/or hospitalization form (MNH19)
		Denominator: Women w/o pre-existing/overt diabetes >=20 weeks GA 
		Response options: yes(1); no(0); missing (55)
		Timing: ANC-28 or later
		
	DIAB_OVERT_DX (ADDED 7-10)
		Defined: Participant reports EVER diagnosed with pre-existing/ 
		chronic diabetes (Reported at ANC - MNH04)
		Denominator: All enrolled women
		Response options: yes(1); no(0); missing (55)
		Timing: Expect these details from ANC at enrollment/ earliest ANC visit
		
	DIAB_TREAT
		Defined: Treatment for gestational diabetes reported at L&D 
		Denominator: All completed pregnancies
		Response options: yes(1); no(0); missing (55); NA (77)
		Timing: L&D 
		
	DIAB_TREAT_INS
		Defined: Treatment for gestational diabetes reported at L&D - insulin
		Denominator: All completed pregnancies
		Response options: yes(1); no(0); missing (55); NA (77)
		Timing: L&D 
		
	DIAB_TREAT_MET
		Defined: Treatment for gestational diabetes reported at L&D - metformin
		Denominator: All completed pregnancies
		Response options: yes(1); no(0); missing (55); NA (77)
		Timing: L&D 
		
	DIAB_TREAT_GLY
		Defined: Treatment for gestational diabetes reported at L&D - glyburide
		Denominator: All completed pregnancies
		Response options: yes(1); no(0); missing (55); NA (77)
		Timing: L&D 
		
*/

				
	*CRF of interest: MNH08 - Lab Results - ANC/PNC

		*Variables needed: 
			*momid / pregid (identifiers)
			*for blood glucose test: 
				*BGLUC_LBTSTDAT - date of blood glucose test 
				*BGLUC_LBPERF_1
				*BGLUC_PRETEST_MMOLL_LBORRES
				*BGLUC_LBPERF_2
				*BGLUC_ORAL_1HR_MMOLL_LBORRES
				*BGLUC_LBPERF_3
				*BGLUC_ORAL_2HR_MMOLL_LBORRES
			*for HbA1c test:
				*HBA1C_LBTSTDAT - date of test
				*HBA1C_TEST_YN
				*HBA1C_LBORRES
				*HBA1C_PRCNT
			*TYPE_VISIT - type of visit (1-14)
			
	clear		
	import delimited "$da/mnh08_merged", bindquote(strict)
	
	*clean up: 
	drop if momid == "" | pregid == ""
	
	
	*prepare momid/pregid for merging: 
	rename momid momid_old
	gen momid = ustrtrim(momid_old)
	
	rename pregid pregid_old
	gen pregid = ustrtrim(pregid_old)
	
	rename site site_old
	gen site = ustrtrim(site_old)
	
	drop momid_old pregid_old
	
	*merge in BOE-PREG_START_DATE to create GA at lab variable:
	*merge m:1 momid pregid using "$wrk/BOE", keepusing(PREG_START_DATE)
	
	preserve 
	clear 
	import delimited "$OUT/MAT_ENROLL", bindquote(strict) case(preserve)
	save "$OUT/MAT_ENROLL", replace 
	restore 
	
		rename momid MOMID 
		rename pregid PREGID 
	merge m:1 MOMID PREGID using "$OUT/MAT_ENROLL", keepusing(PREG_START_DATE)
	
	drop if _merge == 2 
	
	drop _merge 
	
	*update var format PREG_START_DATE
	rename PREG_START_DATE STR_PREG_START_DATE // no longer needed as of 11-1
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td 
 
	
	*Create visit type variables: 
	*visit type:  
	gen TYPE_VISIT_08 = m08_type_visit  
	
	*** Create visit type label: 
	label define vistype 1 "1-Enrollment" 2 "2-ANC-20" 3 "3-ANC-28" ///
		4 "4-ANC-32" 5 "5-ANC-36" 6 "6-IPC" 7 "7-PNC-0" 8 "8-PNC-1" ///
		9 "9-PNC-4" 10 "10-PNC-6" 11 "11-PNC-26" 12 "12-PNC-52" ///
		13 "13-ANC-Unsched" 14 "14-PNC-Unsched" 
	
	*replace TYPE_VISIT_08 = "." if TYPE_VISIT_08 == "NA"
	*destring TYPE_VISIT_08, replace 
	label var TYPE_VISIT_08 "MNH08 Visit Type"
	format TYPE_VISIT_08 %14.0g
	label values TYPE_VISIT_08 vistype
	tab TYPE_VISIT_08, m 
	
	*convert to dates - overall form date:
	gen LBSTDAT = date(m08_lbstdat, "YMD") if m08_lbstdat != "1907-07-07"
	format LBSTDAT %td
	label var LBSTDAT "Date of collection for labs"
	
	*create an indicator for GA at dx:
	gen LBS_GA = LBSTDAT - PREG_START_DATE
	label var LBS_GA "GA at lab date"
	
	sum LBS_GA
	
	///////////////////////////////////////////////////////
	*Start with HbA1c measures - construct overt diabetes:
	
		*first, check adherence with enrollment visit measure:
	tab TYPE_VISIT m08_hba1c_test_yn, m 
	tab LBS_GA m08_hba1c_test_yn, m 
	
	*HbA1c date: 
	gen HBA1C_LBTSTDAT = date(m08_hba1c_lbtstdat, "YMD") if ///
		m08_hba1c_lbtstdat != "1907-07-07" & ///
		m08_hba1c_lbtstdat != "1905-05-05"
	format HBA1C_LBTSTDAT %td
	label var HBA1C_LBTSTDAT "Date of HbA1c test"
	
	sum HBA1C_LBTSTDAT, format 
	
	*Any results with missing date?
	tab m08_hba1c_test_yn if HBA1C_LBTSTDAT ==., m 
	
		*If no HbA1c test date, use the overall lab date:
		replace HBA1C_LBTSTDAT = LBSTDAT if m08_hba1c_test_yn == 1 & ///
			HBA1C_LBTSTDAT == .
			
	*GA at HbA1c test: -- this seems to be a poor indicator of correct GA: 
	/*gen HBA1C_GA = HBA1C_LBTSTDAT - PREG_START_DATE if HBA1C_LBTSTDAT !=. & ///
		PREG_START_DATE !=. 
	label var HBA1C_GA "GA at HbA1c test"
	*/
	
	gen HBA1C_GA = LBS_GA 
	label var HBA1C_GA "GA at sample collection - visit with HbA1c"
	
	*Keep if Enrollment Visit OR test occurred at <20 weeks GA (140 days)
	keep if TYPE_VISIT_08 == 1 | (HBA1C_GA < 140 & HBA1C_GA > 0)
	
	*create an indicator for entry number for each person by date: 
	sort MOMID PREGID LBSTDAT TYPE_VISIT
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "MNH08 Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "MNH08 Total number of entries"
	
	*Create indicator for Overt Diabetes (HbA1c >=6.5%)
		gen HBA1C_PRCNT = m08_hba1c_prcnt
		label var HBA1C_PRCNT "HbA1c result (%)"
		
		sum HBA1C_PRCNT, d 

		gen DIAB_OVERT = 0 if HBA1C_PRCNT >= 0 & HBA1C_PRCNT <6.5
		replace DIAB_OVERT = 1 if HBA1C_PRCNT >=6.5 & HBA1C_PRCNT !=.
		replace DIAB_OVERT = 55 if HBA1C_PRCNT == . | HBA1C_PRCNT < 0 
		label var DIAB_OVERT "Overt diabetes (by HbA1c)"
		
		tab HBA1C_PRCNT DIAB_OVERT, m 
		
	*Check on GA at HbA1c vs. result: 
	tab HBA1C_GA DIAB_OVERT, m 
	
	sort HBA1C_GA
	
	list site TYPE_VISIT_08 HBA1C_LBTSTDAT HBA1C_GA PREG_START_DATE ///
		HBA1C_PRCNT DIAB_OVERT if HBA1C_GA <0
		
	list site TYPE_VISIT_08 HBA1C_LBTSTDAT LBSTDAT HBA1C_GA PREG_START_DATE ///
		HBA1C_PRCNT DIAB_OVERT if HBA1C_GA >=140 & HBA1C_GA != . 
	

	*Prepare HbA1c dataset:
	
	rename TYPE_VISIT_08 TYPE_VISIT_odm
	rename LBSTDAT LBSTDAT_odm
	
	rename site SITE
	
	keep MOMID PREGID SITE TYPE_VISIT_odm LBSTDAT_odm HBA1C_LBTSTDAT ///
		HBA1C_GA ENTRY_NUM ENTRY_TOTAL HBA1C_PRCNT DIAB_OVERT
	
	save "$wrk/dm_mnh08", replace 
	
	
	rename * *_
	
	rename MOMID_ MOMID 
	rename PREGID_ PREGID
	rename ENTRY_NUM_ ENTRY_NUM 
	
	tab ENTRY_NUM, m 
 
		
	reshape wide TYPE_VISIT_odm LBSTDAT_odm HBA1C_LBTSTDAT ///
		HBA1C_GA ENTRY_TOTAL HBA1C_PRCNT DIAB_OVERT, ///
		i(MOMID PREGID SITE) j(ENTRY_NUM)  
		
	*Create comprehensive indicators of Overt Diabetes in wide: 

	gen DIAB_OVERT = . 
	label var DIAB_OVERT "Ever HbA1c >=6.5 at enrollment or visit <20 weeks"
	
	gen DIAB_OVERT_20 = .
	label var DIAB_OVERT_20 "Ever HbA1c >=6.5 at a visit <20 weeks GA"
	
	gen HBA1C_COUNT = 0 
	label var HBA1C_COUNT "Count of HbA1c tests at enrollment or <20 weeks GA"
	
	gen HBA1C_PRCNT = .
	label var HBA1C_PRCNT "HbA1c result (%)"
	
	*update: use a global macro to automatically set the max number of entries: 
		sum ENTRY_TOTAL_1
		
		return list 
		
		global i = r(max)
	
	foreach num of numlist 1/$i {
		
	// count of tests with results:
	replace HBA1C_COUNT = HBA1C_COUNT + 1 if DIAB_OVERT_`num' == 1 | ///
		DIAB_OVERT_`num' == 0 
	
	// any HbA1c: 
	replace DIAB_OVERT = 0 if DIAB_OVERT_`num' == 0 & DIAB_OVERT != 1
	replace DIAB_OVERT = 1 if DIAB_OVERT_`num' == 1 
	replace DIAB_OVERT = 55 if DIAB_OVERT_`num' == 55 & DIAB_OVERT == . 
	
	// HbA1c measure - use higher (more severe) measure if more than one: 
	replace HBA1C_PRCNT = HBA1C_PRCNT_`num' if ///
		(DIAB_OVERT_`num' == 0 | DIAB_OVERT_`num' == 1) & ///
		(HBA1C_PRCNT == . | (HBA1C_PRCNT_`num' > HBA1C_PRCNT & HBA1C_PRCNT !=.))
	
	// HbA1c <20 weeks: 
	replace DIAB_OVERT_20 = 0 if DIAB_OVERT_`num' == 0 & DIAB_OVERT_20 != 1 ///
		& HBA1C_GA_`num' >= 0 & HBA1C_GA_`num' < 140 
	replace DIAB_OVERT_20 = 1 if DIAB_OVERT_`num' == 1 ///
		& HBA1C_GA_`num' >= 0 & HBA1C_GA_`num' < 140 
	replace DIAB_OVERT_20 = 55 if DIAB_OVERT_`num' == 55 & DIAB_OVERT_20 == . ///
		& HBA1C_GA_`num' >= 0 & HBA1C_GA_`num' < 140 
		
	// missing dates & measures after 20 weeks: 
	replace DIAB_OVERT_20 = 77 if DIAB_OVERT_20 == . & ///
		HBA1C_GA_`num' == . & (DIAB_OVERT_`num' == 1 | DIAB_OVERT_`num' == 0)
		
	replace DIAB_OVERT_20 = 77 if DIAB_OVERT_20 == . & ///
		HBA1C_GA_`num' >= 140 & HBA1C_GA_`num' !=. & ///
		(DIAB_OVERT_`num' == 1 | DIAB_OVERT_`num' == 0)
		
	replace DIAB_OVERT_20 = 77 if DIAB_OVERT_20 == . & ///
		HBA1C_GA_`num' <0 & HBA1C_GA_`num' != . & ///
		(DIAB_OVERT_`num' == 1 | DIAB_OVERT_`num' == 0)
	
	// missing date & measure
	replace DIAB_OVERT_20 = 77 if DIAB_OVERT_20 == . & ///
		(HBA1C_GA_`num' <0 | HBA1C_GA_`num' ==. | HBA1C_GA_`num' >=140) & ///
		(DIAB_OVERT_`num' == 55)
	
	}
	
	tab DIAB_OVERT DIAB_OVERT_20, m 
	
	sum HBA1C_PRCNT
	sort DIAB_OVERT
	by DIAB_OVERT: sum HBA1C_PRCNT
	
	tab HBA1C_COUNT, m 
	tab HBA1C_COUNT SITE, m 
		
	save "$wrk/dm_mnh08_wide", replace 
	
	/////////////////////////////////////
	* Overt Diabetes by DX
	////////////////////////////////////
	
	*Review data to construct overt diabetes dx (prior to study):
	/*	DIAB_OVERT_DX
		Defined: Participant reports EVER diagnosed with pre-existing/ 
		chronic diabetes (Reported at ANC - MNH04)
		Denominator: All enrolled women
		Response options: yes(1); no(0); missing (55)
		Timing: Expect these details from ANC at enrollment/ earliest ANC visit
	*/
	clear 
	*ANC form: 
	import delimited "$da/mnh04_merged", bindquote(strict)
	
	*clean up: 
	drop if momid == "" | pregid == ""
	
	*prepare momid/pregid for merging: 
	rename momid momid_old
	gen MOMID = ustrtrim(momid_old)
	
	rename pregid pregid_old
	gen PREGID = ustrtrim(pregid_old)
	
	drop momid_old pregid_old	
	
	*DIAB_OVERT_DX 
	tab m04_diabetes_ever_mhoccur, m 
	
	gen DIAB_OVERT_DX = m04_diabetes_ever_mhoccur
	replace DIAB_OVERT_DX = . if DIAB_OVERT_DX == 77 | DIAB_OVERT_DX == 99 | ///
		DIAB_OVERT_DX == . 
		
	label var DIAB_OVERT_DX "DX of gestational diabetes at hospitalization"
	
	tab DIAB_OVERT_DX, m 
	
	keep if DIAB_OVERT_DX != . 
	
	keep MOMID PREGID DIAB_OVERT_DX
	
	*create an indicator for entry number for each person by date: 
	sort MOMID PREGID DIAB_OVERT_DX
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "MNH04 Entry Number"
	
	tab ENTRY_NUM, m 	
	
	list if ENTRY_NUM >=2
	
	*collapse to max: 
	drop ENTRY_NUM 
	collapse (max) DIAB_OVERT_DX, by (MOMID PREGID)

	tab DIAB_OVERT_DX, m 
	label var DIAB_OVERT_DX "Participant reports diagnosed with pre-existing diabetes"
	
	save "$wrk/dm_dx", replace 
	
	clear 
	
	///////////////////////////////////////////////////////////////////////////
	*Gestational Diabetes 
	///////////////////////////////////////////////////////////////////////////
	
	clear		
	import delimited "$da/mnh08_merged", bindquote(strict)
	
	*clean up: 
	drop if momid == "" | pregid == ""
	
	*prepare momid/pregid for merging: 
	rename momid momid_old
	gen momid = ustrtrim(momid_old)
	
	rename pregid pregid_old
	gen pregid = ustrtrim(pregid_old)
	
	rename site site_old
	gen site = ustrtrim(site_old)
	
	drop momid_old pregid_old

	*merge in BOE-PREG_START_DATE to create GA at lab variable:
	*merge m:1 momid pregid using "$wrk/BOE", keepusing(PREG_START_DATE)
		rename momid MOMID 
		rename pregid PREGID 
	merge m:1 MOMID PREGID using "$OUT/MAT_ENROLL", keepusing(PREG_START_DATE)
	
	drop if _merge == 2 
	
	drop _merge 
	
	*update var format PREG_START_DATE // no longer needed as of 11-1
	rename PREG_START_DATE STR_PREG_START_DATE
	
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td 
	
	*Create visit type variables: 
	*visit type:  
	gen TYPE_VISIT_08 = m08_type_visit  
	
	*** Create visit type label: 
	label define vistype 1 "1-Enrollment" 2 "2-ANC-20" 3 "3-ANC-28" ///
		4 "4-ANC-32" 5 "5-ANC-36" 6 "6-IPC" 7 "7-PNC-0" 8 "8-PNC-1" ///
		9 "9-PNC-4" 10 "10-PNC-6" 11 "11-PNC-26" 12 "12-PNC-52" ///
		13 "13-ANC-Unsched" 14 "14-PNC-Unsched" 
	
	*replace TYPE_VISIT_08 = "." if TYPE_VISIT_08 == "NA"
	*destring TYPE_VISIT_08, replace 
	label var TYPE_VISIT_08 "MNH08 Visit Type"
	format TYPE_VISIT_08 %14.0g
	label values TYPE_VISIT_08 vistype
	tab TYPE_VISIT_08, m 
	
	*convert to dates - overall form date:
	gen LBSTDAT = date(m08_lbstdat, "YMD") if m08_lbstdat != "1907-07-07" & ///
		m08_lbstdat != "1905-05-05"
	format LBSTDAT %td
	label var LBSTDAT "Date of collection for labs"
	
	*create an indicator for GA at dx:
	gen LBS_GA = LBSTDAT - PREG_START_DATE
	label var LBS_GA "GA at lab date"
	
	sum LBS_GA
	
	///////////////////////////////////////////////////////
	*Review variables for blood glucose tests: 
	
	gen BGLUC_PRETEST_MMOLL_LBORRES = m08_bgluc_pretest_mmoll_lborres
	label var BGLUC_PRETEST_MMOLL_LBORRES "Pre-test (fasting) glucose"
	
	tab BGLUC_PRETEST_MMOLL_LBORRES TYPE_VISIT_08, m 
	
	gen BGLUC_ORAL_1HR_MMOLL_LBORRES = m08_bgluc_oral_1hr_mmoll_lborres
	label var BGLUC_ORAL_1HR_MMOLL_LBORRES  "Oral glucose tolerance test - 1-hr"
	
	gen BGLUC_ORAL_2HR_MMOLL_LBORRES = m08_bgluc_oral_2hr_mmoll_lborres
	label var BGLUC_ORAL_2HR_MMOLL_LBORRES  "Oral glucose tolerance test - 2-hr"	
	
	gen BGLUC_RESULT = 0 
	label var BGLUC_RESULT "Visit has any blood glucose test result"
	
	foreach var of varlist BGLUC_PRETEST_MMOLL_LBORRES ///
		BGLUC_ORAL_1HR_MMOLL_LBORRES BGLUC_ORAL_2HR_MMOLL_LBORRES {
	
		replace BGLUC_RESULT = 1 if `var' >= 0 & `var' < 99 
	
		}
		
	tab TYPE_VISIT_08 BGLUC_RESULT , m 
	
	*restrict to visit type 3 OR participant reports blood-glucose results at  
	*another ANC visit: 
	
	keep if TYPE_VISIT_08 == 3 | BGLUC_RESULT == 1 
	

	*review enrollment visit observation: 
	list LBS_GA BGLUC_RESULT BGLUC_PRETEST_MMOLL_LBORRES BGLUC_ORAL_1HR_MMOLL_LBORRES ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES if TYPE_VISIT_08 == 1 
		
	*since there are 2 enrollment visits with blood glucose test result, restrict to
	*observations > 20 weeks GA if visit type 1 (enrollment), 2 (ANC20), or 
	*13(unscheduled ANC):
	drop if LBS_GA <(20*7) & LBS_GA != . & ///
		(TYPE_VISIT_08 == 1 | TYPE_VISIT_08 == 2 | TYPE_VISIT_08 == 13) 
		
		
	*create an indicator for entry number for each person by date: 
	sort MOMID PREGID LBSTDAT TYPE_VISIT
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "MNH08 Entry Number"
	
	tab ENTRY_NUM, m 
	tab ENTRY_NUM site, m

	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "MNH08 Total number of entries"
		
		
	//////////////////////////
	*construct GDM indicators:
	
	/// fasting threshold: ≥ 5.1 mmol/L 
	gen DIAB_GEST_FASTING = 0 if BGLUC_PRETEST_MMOLL_LBORRES >= 0 & ///	
		BGLUC_PRETEST_MMOLL_LBORRES < 5.1 
	replace DIAB_GEST_FASTING = 1 if BGLUC_PRETEST_MMOLL_LBORRES >= 5.1 & ///
		BGLUC_PRETEST_MMOLL_LBORRES <99
	replace DIAB_GEST_FASTING = 55 if BGLUC_PRETEST_MMOLL_LBORRES < 0 | ///
		BGLUC_PRETEST_MMOLL_LBORRES == . 
	label var DIAB_GEST_FASTING "GDM by fasting blood glucose test (pre-test)"
	
	tab BGLUC_PRETEST_MMOLL_LBORRES DIAB_GEST_FASTING, m 
	
	
	/// 1-hr threshold: ≥ 10.0 mmol/L  
	gen DIAB_GEST_1HR = 0 if BGLUC_ORAL_1HR_MMOLL_LBORRES >= 0 & ///	
		BGLUC_ORAL_1HR_MMOLL_LBORRES < 10
	replace DIAB_GEST_1HR = 1 if BGLUC_ORAL_1HR_MMOLL_LBORRES >= 10 & ///
		BGLUC_ORAL_1HR_MMOLL_LBORRES <99
	replace DIAB_GEST_1HR = 55 if BGLUC_ORAL_1HR_MMOLL_LBORRES < 0 | ///
		BGLUC_ORAL_1HR_MMOLL_LBORRES == . 
	label var DIAB_GEST_1HR "GDM by oral glucose tolerance test - 1 hour"
	
	tab BGLUC_ORAL_1HR_MMOLL_LBORRES DIAB_GEST_1HR, m 
	
	
	/// 2-hr threshold: ≥ 8.5 mmol/L)
	gen DIAB_GEST_2HR = 0 if BGLUC_ORAL_2HR_MMOLL_LBORRES >= 0 & ///	
		BGLUC_ORAL_2HR_MMOLL_LBORRES < 8.5
	replace DIAB_GEST_2HR = 1 if BGLUC_ORAL_2HR_MMOLL_LBORRES >= 8.5 & ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES <99
	replace DIAB_GEST_2HR = 55 if BGLUC_ORAL_2HR_MMOLL_LBORRES < 0 | ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES == . 
	label var DIAB_GEST_2HR "GDM by oral glucose tolerance test - 2 hours"
	
	tab BGLUC_ORAL_2HR_MMOLL_LBORRES DIAB_GEST_2HR, m 
	
	
	//////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////
		* ADDITIONAL VARIABLES: WHO 1999 THRESHOLDS * 
		* Fasting: >= 7.0
		* 2 hour: >= 7.8
	
	/// fasting threshold: ≥ 7.0 mmol/L 
	gen DIAB_GEST_FASTING_WHO = 0 if BGLUC_PRETEST_MMOLL_LBORRES >= 0 & ///	
		BGLUC_PRETEST_MMOLL_LBORRES < 7.0
	replace DIAB_GEST_FASTING_WHO = 1 if BGLUC_PRETEST_MMOLL_LBORRES >= 7.0 & ///
		BGLUC_PRETEST_MMOLL_LBORRES <99
	replace DIAB_GEST_FASTING_WHO = 55 if BGLUC_PRETEST_MMOLL_LBORRES < 0 | ///
		BGLUC_PRETEST_MMOLL_LBORRES == . 
	label var DIAB_GEST_FASTING_WHO "GDM by fasting blood glucose test (pre-test) - WHO 1999"
	
	tab BGLUC_PRETEST_MMOLL_LBORRES DIAB_GEST_FASTING_WHO, m 
	
	
	/// 2-hr threshold: ≥ 7.8 mmol/L)
	gen DIAB_GEST_2HR_WHO = 0 if BGLUC_ORAL_2HR_MMOLL_LBORRES >= 0 & ///	
		BGLUC_ORAL_2HR_MMOLL_LBORRES < 7.8
	replace DIAB_GEST_2HR_WHO = 1 if BGLUC_ORAL_2HR_MMOLL_LBORRES >= 7.8 & ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES <99
	replace DIAB_GEST_2HR_WHO = 55 if BGLUC_ORAL_2HR_MMOLL_LBORRES < 0 | ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES == . 
	label var DIAB_GEST_2HR_WHO "GDM by oral glucose tolerance test - 2 hours - WHO 1999"
	
	tab BGLUC_ORAL_2HR_MMOLL_LBORRES DIAB_GEST_2HR_WHO, m 

	
	//////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////
		* ADDITIONAL VARIABLES: Canadian Diabetes Association THRESHOLDS * 
		* Fasting: >= 5.3
		* 1-hour: >= 10.6
		* 2 hour: >= 8.9	
	
	/// fasting threshold: ≥ 5.3 mmol/L 
	gen DIAB_GEST_FASTING_CAN = 0 if BGLUC_PRETEST_MMOLL_LBORRES >= 0 & ///	
		BGLUC_PRETEST_MMOLL_LBORRES < 5.3 
	replace DIAB_GEST_FASTING_CAN = 1 if BGLUC_PRETEST_MMOLL_LBORRES >= 5.3 & ///
		BGLUC_PRETEST_MMOLL_LBORRES <99
	replace DIAB_GEST_FASTING_CAN = 55 if BGLUC_PRETEST_MMOLL_LBORRES < 0 | ///
		BGLUC_PRETEST_MMOLL_LBORRES == . 
	label var DIAB_GEST_FASTING_CAN "GDM by fasting blood glucose test (pre-test) - Canadian"
	
	tab BGLUC_PRETEST_MMOLL_LBORRES DIAB_GEST_FASTING_CAN, m 
	
	
	/// 1-hr threshold: ≥ 10.6 mmol/L  
	gen DIAB_GEST_1HR_CAN = 0 if BGLUC_ORAL_1HR_MMOLL_LBORRES >= 0 & ///	
		BGLUC_ORAL_1HR_MMOLL_LBORRES < 10.6
	replace DIAB_GEST_1HR_CAN = 1 if BGLUC_ORAL_1HR_MMOLL_LBORRES >= 10.6 & ///
		BGLUC_ORAL_1HR_MMOLL_LBORRES <99
	replace DIAB_GEST_1HR_CAN = 55 if BGLUC_ORAL_1HR_MMOLL_LBORRES < 0 | ///
		BGLUC_ORAL_1HR_MMOLL_LBORRES == . 
	label var DIAB_GEST_1HR_CAN "GDM by oral glucose tolerance test - 1 hour - Canadian"
	
	tab BGLUC_ORAL_1HR_MMOLL_LBORRES DIAB_GEST_1HR_CAN, m 
	
	
	/// 2-hr threshold: ≥ 8.9 mmol/L)
	gen DIAB_GEST_2HR_CAN = 0 if BGLUC_ORAL_2HR_MMOLL_LBORRES >= 0 & ///	
		BGLUC_ORAL_2HR_MMOLL_LBORRES < 8.9
	replace DIAB_GEST_2HR_CAN = 1 if BGLUC_ORAL_2HR_MMOLL_LBORRES >= 8.9 & ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES <99
	replace DIAB_GEST_2HR_CAN = 55 if BGLUC_ORAL_2HR_MMOLL_LBORRES < 0 | ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES == . 
	label var DIAB_GEST_2HR_CAN "GDM by oral glucose tolerance test - 2 hours - Canadian"
	
	tab BGLUC_ORAL_2HR_MMOLL_LBORRES DIAB_GEST_2HR_CAN, m 
	
	//////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////
		* ADDITIONAL VARIABLES: DIPSI THRESHOLDS * 
		* 2 hour: >= 7.8		
	
	/// 2-hr threshold: ≥ 7.8 mmol/L)
	gen DIAB_GEST_2HR_DIPSI = 0 if BGLUC_ORAL_2HR_MMOLL_LBORRES >= 0 & ///	
		BGLUC_ORAL_2HR_MMOLL_LBORRES < 7.8
	replace DIAB_GEST_2HR_DIPSI = 1 if BGLUC_ORAL_2HR_MMOLL_LBORRES >= 7.8 & ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES <99
	replace DIAB_GEST_2HR_DIPSI = 55 if BGLUC_ORAL_2HR_MMOLL_LBORRES < 0 | ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES == . 
	label var DIAB_GEST_2HR_DIPSI "GDM by oral glucose tolerance test - 2 hours - DIPSI"
	
	tab BGLUC_ORAL_2HR_MMOLL_LBORRES DIAB_GEST_2HR_DIPSI, m 
	
	//////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////
		* ADDITIONAL VARIABLES: ACOG THRESHOLDS * 
		* Fasting: >= 5.3
	
	/// fasting threshold: ≥ 5.3 mmol/L 
	gen DIAB_GEST_FASTING_ACOG = 0 if BGLUC_PRETEST_MMOLL_LBORRES >= 0 & ///	
		BGLUC_PRETEST_MMOLL_LBORRES < 5.3 
	replace DIAB_GEST_FASTING_ACOG = 1 if BGLUC_PRETEST_MMOLL_LBORRES >= 5.3 & ///
		BGLUC_PRETEST_MMOLL_LBORRES <99
	replace DIAB_GEST_FASTING_ACOG = 55 if BGLUC_PRETEST_MMOLL_LBORRES < 0 | ///
		BGLUC_PRETEST_MMOLL_LBORRES == . 
	label var DIAB_GEST_FASTING_ACOG "GDM by fasting blood glucose test (pre-test) - ACOG"
	
	
	tab BGLUC_PRETEST_MMOLL_LBORRES DIAB_GEST_FASTING_ACOG, m 
	
	*Prepare GDM dataset:
	
	rename LBS_GA DIAB_GA 
	label var DIAB_GA "Gestational age at GDM blood glucose test"
	
	rename TYPE_VISIT_08 TYPE_VISIT_gdm
	rename LBSTDAT LBSTDAT_gdm
	
	keep MOMID PREGID site TYPE_VISIT_gdm LBSTDAT_gdm ENTRY_NUM ENTRY_TOTAL ///
		DIAB_GA DIAB_GEST_FASTING DIAB_GEST_1HR DIAB_GEST_2HR ///
		BGLUC_PRETEST_MMOLL_LBORRES BGLUC_ORAL_1HR_MMOLL_LBORRES ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES BGLUC_RESULT *_CAN *_ACOG *_WHO *_DIPSI 
	
	save "$wrk/gdm_mnh08", replace 
	
	* UPDATE INCORPORATED ON MAY 13 2025: 
	
	* First, drop any postnatal OGTTs: 
	merge m:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing(PREG_END PREG_END_DATE)
	
	drop if _merge == 2 
	drop _merge 
	
	* identify postnatal OGTTs: 
	gen POSTNATAL_OGTT = 1 if LBSTDAT_gdm >= PREG_END_DATE & PREG_END==1 & ///
		LBSTDAT_gdm != . & PREG_END_DATE != . 
		
	tab POSTNATAL_OGTT TYPE_VISIT_gdm, m 
	
	list site PREG_END_DATE LBSTDAT_gdm PREG_END BGLUC_RESULT if ///
		POSTNATAL_OGTT == 1 & TYPE_VISIT_gdm==3
		
	tab POSTNATAL_OGTT ENTRY_TOTAL, m 
		
	*DROP postnatal OGTTs IF the participant has more than one entry: 
	drop if POSTNATAL_OGTT == 1 & ENTRY_TOTAL > 1 
	
	tab ENTRY_NUM, m 
	
	*review those with multiple entries:
	sort MOMID PREGID 
	list MOMID PREGID site TYPE_VISIT_gdm LBSTDAT_gdm DIAB_GA ENTRY_NUM if ///
	ENTRY_TOTAL>1

	*drop entry numbers & recalculcate
	drop ENTRY_NUM ENTRY_TOTAL
	
	*create an indicator for entry number for each person by date: 
	sort MOMID PREGID LBSTDAT TYPE_VISIT
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "MNH08 Entry Number"
	
	tab ENTRY_NUM, m 
	tab ENTRY_NUM site, m

	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "MNH08 Total number of entries"
	
	*create an indicator for multiple VALID tests: 
	gen OGTT_TRUE = 0 
	replace OGTT_TRUE = 1 if BGLUC_RESULT==1 & ///
		((BGLUC_PRETEST_MMOLL_LBORRES>0 & BGLUC_PRETEST_MMOLL_LBORRES!=.) | ///
		(BGLUC_ORAL_2HR_MMOLL_LBORRES>0 & BGLUC_ORAL_2HR_MMOLL_LBORRES!=.) | ///
		(BGLUC_ORAL_1HR_MMOLL_LBORRES>0 & BGLUC_ORAL_1HR_MMOLL_LBORRES!=.))
	
	sort MOMID PREGID OGTT_TRUE 
	
	duplicates tag MOMID PREGID OGTT_TRUE, gen(OGTT_TRUE_COUNT)
	
	replace OGTT_TRUE_COUNT = OGTT_TRUE_COUNT + 1 if OGTT_TRUE==1
	
	tab OGTT_TRUE_COUNT, m 
	tab OGTT_TRUE_COUNT ENTRY_TOTAL, m 
	
	* check on those with multiple entries: multiple true results
	list PREGID site BGLUC_RESULT BGLUC_PRETEST_MMOLL_LBORRES ///
		ENTRY_NUM ENTRY_TOTAL LBSTDAT_gdm PREG_END_DATE if OGTT_TRUE_COUNT==2 
		
	* check on those with multiple entries: mix of true & empty results 
	list PREGID site BGLUC_RESULT BGLUC_PRETEST_MMOLL_LBORRES ///
		ENTRY_NUM ENTRY_TOTAL LBSTDAT_gdm PREG_END_DATE ///
		OGTT_TRUE OGTT_TRUE_COUNT  if ENTRY_TOTAL==2 & ///
		OGTT_TRUE_COUNT <2
		
		
	*create an indicator for EXACT DUPLICATE results with different dates: 
	sort MOMID PREGID OGTT_TRUE BGLUC_PRETEST_MMOLL_LBORRES ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES BGLUC_ORAL_1HR_MMOLL_LBORRES
	
	duplicates tag MOMID PREGID OGTT_TRUE BGLUC_PRETEST_MMOLL_LBORRES ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES BGLUC_ORAL_1HR_MMOLL_LBORRES ///
		if OGTT_TRUE==1, gen(DUP_RESULT)
		
	tab OGTT_TRUE_COUNT DUP_RESULT
	
	////////////////////////////
	*SELECTION DECISION: 1-3: 
	
	*1. IF there is a multiple entry with at least one true OGTT, keep the 
	* true entry only: 
	drop if ENTRY_TOTAL == 2 & OGTT_TRUE_COUNT == 1 & OGTT_TRUE == 1

	*2. IF results are exact duplicates, keep the EARLIEST entry: 
	drop if ENTRY_TOTAL == 2 & OGTT_TRUE == 1 & OGTT_TRUE_COUNT == 2 & ///
		DUP_RESULT ==1 & ENTRY_NUM == 1 
		
	*3. IF the results are both valid and NOT exact duplicates, keep the 
	* MOST RECENT entry (assuming a repeated test): 
	drop if ENTRY_TOTAL == 2 & OGTT_TRUE ==1 & OGTT_TRUE_COUNT == 2 & ///
		DUP_RESULT == 0 & ENTRY_NUM ==1 
		
		
	/////////////////////////////////
		
	*review for remaining duplicates: 
	drop ENTRY_TOTAL ENTRY_NUM 
	
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	
	tab ENTRY_TOTAL, m 
	
	*No duplicates: okay to proceed.
	
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	gen ENTRY_NUM = ENTRY_TOTAL
	
	
	rename * *_
	
	rename MOMID_ MOMID  
	rename PREGID_ PREGID  
	rename ENTRY_NUM_ ENTRY_NUM 
	
	tab ENTRY_NUM 

		
	reshape wide TYPE_VISIT_gdm LBSTDAT_gdm ///
		DIAB_GA DIAB_GEST_FASTING* DIAB_GEST_1HR* DIAB_GEST_2HR* ///
		BGLUC_PRETEST_MMOLL_LBORRES BGLUC_ORAL_1HR_MMOLL_LBORRES ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES BGLUC_RESULT ENTRY_TOTAL, ///
		i(MOMID PREGID site) j(ENTRY_NUM)  
		
	*Create comprehensive indicators of Gestational Diabetes in wide:
	
	*first, merge in indicator for Overt Diabetes
	merge 1:1 MOMID PREGID using "$wrk/dm_mnh08_wide"
	
	drop if _merge == 2 
	drop _merge 
	
	*then merge in indicator for preexisting diabetes dx: 
	merge 1:1 MOMID PREGID using "$wrk/dm_dx", keepusing(DIAB_OVERT_DX)
	
	drop if _merge == 2 
	drop _merge 
	

	
	*create combined indicators for GDM: 	
	gen BGLUC_COUNT = 0 
	label var BGLUC_COUNT "Count of blood glucose test entries"
	
	gen DIAB_GEST_FASTING = 77 if DIAB_OVERT == 1 | DIAB_OVERT_DX == 1 
	gen DIAB_GEST_1HR = 77 if DIAB_OVERT == 1 | DIAB_OVERT_DX == 1 
	gen DIAB_GEST_2HR = 77 if DIAB_OVERT == 1 | DIAB_OVERT_DX == 1 
	
	label var DIAB_GEST_FASTING "GDM by fasting blood glucose test (excludes overt DM)"
	label var DIAB_GEST_1HR "GDM by 1-hour blood glucose test (excludes overt DM)"
	label var DIAB_GEST_2HR "GDM by 2-hour blood glucose test (excludes overt DM)"
	
	gen DIAB_GEST_FASTING_GA = . 
	gen DIAB_GEST_1HR_GA = .
	gen DIAB_GEST_2HR_GA = . 
	
	label var DIAB_GEST_FASTING_GA "GDM by fasting blood glucose test - GA at test"
	label var DIAB_GEST_1HR_GA "GDM by 1-hour blood glucose test - GA at test"
	label var DIAB_GEST_2HR_GA "GDM by 2-hour blood glucose test - GA at test"
	
	gen BGLUC_PRETEST_MMOLL_LBORRES = . 
	gen BGLUC_ORAL_1HR_MMOLL_LBORRES = . 
	gen BGLUC_ORAL_2HR_MMOLL_LBORRES = .
	
	label var BGLUC_PRETEST_MMOLL_LBORRES "Fasting blood glucose test (mmol/L)" 
	label var BGLUC_ORAL_1HR_MMOLL_LBORRES "1-hour blood glucose test (mmol/L)" 
	label var BGLUC_ORAL_2HR_MMOLL_LBORRES "2-hour blood glucose test (mmol/L)" 	
	
	foreach num of numlist 1 {
		
	// count of tests with results:
	replace BGLUC_COUNT = BGLUC_COUNT + 1 if BGLUC_RESULT_`num' == 1 & ///
		DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	
	// fasting: 
	replace DIAB_GEST_FASTING = 0 if DIAB_GEST_FASTING_`num' == 0 & ///
		DIAB_GEST_FASTING != 1 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	replace DIAB_GEST_FASTING = 1 if DIAB_GEST_FASTING_`num' == 1 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	replace DIAB_GEST_FASTING = 55 if DIAB_GEST_FASTING_`num' == 55 & ///
		DIAB_GEST_FASTING == . & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
		
	// timing 
	replace DIAB_GEST_FASTING_GA = DIAB_GA_`num' if DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 & ///
		((DIAB_GEST_FASTING_`num' == 0 & DIAB_GEST_FASTING_GA == .) | ///
		(DIAB_GEST_FASTING_`num' == 1 & ///
		(DIAB_GEST_FASTING_GA == . | DIAB_GEST_FASTING_GA > DIAB_GA_`num')))
		
	// continuous result: 
	replace BGLUC_PRETEST_MMOLL_LBORRES = BGLUC_PRETEST_MMOLL_LBORRES_`num' if ///
		BGLUC_PRETEST_MMOLL_LBORRES == . | ///
		(BGLUC_PRETEST_MMOLL_LBORRES_`num' > BGLUC_PRETEST_MMOLL_LBORRES & ///
		 BGLUC_PRETEST_MMOLL_LBORRES_`num' !=.)
		
	
	// 1 hour 
	replace DIAB_GEST_1HR = 0 if DIAB_GEST_1HR_`num' == 0 & DIAB_GEST_1HR != 1 ///
		& DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	replace DIAB_GEST_1HR = 1 if DIAB_GEST_1HR_`num' == 1 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	replace DIAB_GEST_1HR = 55 if DIAB_GEST_1HR_`num' == 55 & ///
		DIAB_GEST_1HR == . & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
		
	// timing 
	replace DIAB_GEST_1HR_GA = DIAB_GA_`num' if DIAB_OVERT != 1 & DIAB_OVERT_DX != 1  & ///
		((DIAB_GEST_1HR_`num' == 0 & DIAB_GEST_1HR_GA == .) | ///
		(DIAB_GEST_1HR_`num' == 1 & ///
		(DIAB_GEST_1HR_GA == . | DIAB_GEST_1HR_GA > DIAB_GA_`num')))
		
	// continuous result: 
	replace BGLUC_ORAL_1HR_MMOLL_LBORRES = BGLUC_ORAL_1HR_MMOLL_LBORRES_`num' if ///
		BGLUC_ORAL_1HR_MMOLL_LBORRES == . | ///
		(BGLUC_ORAL_1HR_MMOLL_LBORRES_`num' > BGLUC_ORAL_1HR_MMOLL_LBORRES & ///
		 BGLUC_ORAL_1HR_MMOLL_LBORRES_`num' != .)
		
	
	// 2 hour 
	replace DIAB_GEST_2HR = 0 if DIAB_GEST_2HR_`num' == 0 & DIAB_GEST_2HR != 1 ///
		& DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	replace DIAB_GEST_2HR = 1 if DIAB_GEST_2HR_`num' == 1 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	replace DIAB_GEST_2HR = 55 if DIAB_GEST_2HR_`num' == 55 & ///
		DIAB_GEST_2HR == . & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
		
	// timing 
	replace DIAB_GEST_2HR_GA = DIAB_GA_`num' if DIAB_OVERT != 1 & DIAB_OVERT_DX != 1  & ///
		((DIAB_GEST_2HR_`num' == 0 & DIAB_GEST_2HR_GA == .) | ///
		(DIAB_GEST_2HR_`num' == 1 & ///
		(DIAB_GEST_2HR_GA == . | DIAB_GEST_2HR_GA > DIAB_GA_`num')))
		
	// continuous result: 
	replace BGLUC_ORAL_2HR_MMOLL_LBORRES = BGLUC_ORAL_2HR_MMOLL_LBORRES_`num' if ///
		BGLUC_ORAL_2HR_MMOLL_LBORRES == . | ///
		(BGLUC_ORAL_2HR_MMOLL_LBORRES_`num' > BGLUC_ORAL_2HR_MMOLL_LBORRES & ///
		 BGLUC_ORAL_2HR_MMOLL_LBORRES_`num'!= .	)
		
	}
	
	tab BGLUC_COUNT, m 
	
	*Loop extra variables - additional thresholds: 
	
	gen BGLUC_COUNT_ADD = 0 
	
	foreach let in WHO CAN DIPSI ACOG {
	
	gen DIAB_GEST_FASTING_`let' = .
	gen DIAB_GEST_1HR_`let' = . 
	gen DIAB_GEST_2HR_`let' = . 
	
	}
	
	foreach num of numlist 1 {
		
	// count of tests with results:
	replace BGLUC_COUNT_ADD = BGLUC_COUNT + 1 if BGLUC_RESULT_`num' == 1 & ///
		DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
		
	foreach let in WHO CAN ACOG {
	
	// fasting: 
	replace DIAB_GEST_FASTING_`let' = 0 if DIAB_GEST_FASTING_`let'_`num' == 0 & ///
		DIAB_GEST_FASTING_`let' != 1 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	replace DIAB_GEST_FASTING_`let' = 1 if DIAB_GEST_FASTING_`let'_`num' == 1 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	replace DIAB_GEST_FASTING_`let' = 55 if DIAB_GEST_FASTING_`let'_`num' == 55 & ///
		DIAB_GEST_FASTING_`let' == . & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	
		
	}
	
	foreach let in CAN {		
	
	// 1 hour 
	replace DIAB_GEST_1HR_`let' = 0 if DIAB_GEST_1HR_`let'_`num' == 0 & DIAB_GEST_1HR_`let' != 1 ///
		& DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	replace DIAB_GEST_1HR_`let' = 1 if DIAB_GEST_1HR_`let'_`num' == 1 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	replace DIAB_GEST_1HR_`let' = 55 if DIAB_GEST_1HR_`let'_`num' == 55 & ///
		DIAB_GEST_1HR_`let' == . & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
		
		
	}
		
	foreach let in WHO CAN DIPSI {	
	
	// 2 hour 
	replace DIAB_GEST_2HR_`let' = 0 if DIAB_GEST_2HR_`let'_`num' == 0 & DIAB_GEST_2HR_`let' != 1 ///
		& DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	replace DIAB_GEST_2HR_`let' = 1 if DIAB_GEST_2HR_`let'_`num' == 1 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	replace DIAB_GEST_2HR_`let' = 55 if DIAB_GEST_2HR_`let'_`num' == 55 & ///
		DIAB_GEST_2HR_`let' == . & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	
		 
	}
		
	}
	

	
	// construct combined variable 
	// N/A if overt diabetes: 
	gen DIAB_GEST_ANY = 77 if DIAB_OVERT == 1 | DIAB_OVERT_DX == 1 
	// 0 if all are 0: 
	replace DIAB_GEST_ANY = 0 if DIAB_GEST_FASTING == 0 & DIAB_GEST_1HR == 0 & ///
		DIAB_GEST_2HR == 0 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 & site != "India-SAS"
	// 0 if all are 0 - INDIA SAS SHOULD ONLY INCLUDE pretest + 1 hr : 
	replace DIAB_GEST_ANY = 0 if DIAB_GEST_FASTING == 0 &  ///
		DIAB_GEST_2HR == 0 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 & site == "India-SAS"
	// 1 if any are 1: 
	replace DIAB_GEST_ANY = 1 if DIAB_GEST_FASTING == 1 | DIAB_GEST_1HR == 1 | ///
		DIAB_GEST_2HR == 1 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1
	// missing if does not meet other categories above & 1 or more are missing: 
	replace DIAB_GEST_ANY = 55 if DIAB_GEST_ANY == . & ///
		(DIAB_GEST_FASTING == 55 | DIAB_GEST_1HR == 55 | ///
		DIAB_GEST_2HR == 55) & DIAB_OVERT != 1	& DIAB_OVERT_DX != 1
		
	label var DIAB_GEST_ANY "GDM by any blood-glucose test (fasting,1hr,2hr)"
		
	foreach var of varlist DIAB_GEST_FASTING DIAB_GEST_1HR DIAB_GEST_2HR {
	tab `var' DIAB_GEST_ANY, m 
	}
	
	
	// construct combined variable for different thresholds: 
	
	foreach let in WHO CAN { 
	
	// N/A if overt diabetes: 
	gen DIAB_GEST_ANY_`let' = 77 if DIAB_OVERT == 1 | DIAB_OVERT_DX == 1
	
	}
	
	/////////
	/// WHO: 
	
	// 0 if all are 0: 
	replace DIAB_GEST_ANY_WHO = 0 if DIAB_GEST_FASTING_WHO == 0 & ///
		DIAB_GEST_2HR_WHO == 0 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1 
	// 1 if any are 1: 
	replace DIAB_GEST_ANY_WHO = 1 if DIAB_GEST_FASTING_WHO == 1 | ///
		DIAB_GEST_2HR_WHO == 1 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1
	// missing if does not meet other categories above & 1 or more are missing: 
	replace DIAB_GEST_ANY_WHO = 55 if DIAB_GEST_ANY_WHO == . & ///
		(DIAB_GEST_FASTING_WHO == 55 | ///
		DIAB_GEST_2HR_WHO == 55) & DIAB_OVERT != 1	& DIAB_OVERT_DX != 1
		
	tab DIAB_GEST_ANY_WHO, m 
	
	/////////
	/// Canada: 
	
	// 0 if all are 0: 
	replace DIAB_GEST_ANY_CAN = 0 if DIAB_GEST_FASTING_CAN == 0 & ///
		DIAB_GEST_1HR_CAN == 0 & DIAB_GEST_2HR_CAN == 0 & DIAB_OVERT != 1 & DIAB_OVERT_DX != 1
		
	// 0 if sum of results is 1 
	replace DIAB_GEST_ANY_CAN = 0 if ///
		((DIAB_GEST_FASTING_CAN + DIAB_GEST_2HR_CAN + DIAB_GEST_1HR_CAN == 1) | ///
		(DIAB_GEST_FASTING_CAN + DIAB_GEST_2HR_CAN + DIAB_GEST_1HR_CAN == 0)) ///
		& DIAB_OVERT != 1 & DIAB_OVERT_DX != 1
		
	// 1 if any are 1: 
	replace DIAB_GEST_ANY_CAN = 1 if ///
		((DIAB_GEST_FASTING_CAN + DIAB_GEST_2HR_CAN + DIAB_GEST_1HR_CAN == 2) | ///
		(DIAB_GEST_FASTING_CAN + DIAB_GEST_2HR_CAN + DIAB_GEST_1HR_CAN == 3)) ///
		& DIAB_OVERT != 1 & DIAB_OVERT_DX != 1
		
	// missing if does not meet other categories above & 1 or more are missing: 
	replace DIAB_GEST_ANY_CAN = 55 if DIAB_GEST_ANY_CAN == . & ///
		(DIAB_GEST_FASTING_CAN == 55 | DIAB_GEST_1HR_CAN == 55 | ///
		DIAB_GEST_2HR_CAN == 55) & DIAB_OVERT != 1	& DIAB_OVERT_DX != 1
		
	tab DIAB_GEST_ANY_CAN, m 
	
	label var DIAB_GEST_FASTING_WHO "GDM by fasting blood glucose test (pre-test) - WHO 1999"
	label var DIAB_GEST_2HR_WHO "GDM by oral glucose tolerance test - 2 hours - WHO 1999"
	label var DIAB_GEST_FASTING_ACOG "GDM by fasting blood glucose test (pre-test) - ACOG"
	label var DIAB_GEST_2HR_DIPSI "GDM by oral glucose tolerance test - 2 hours - DIPSI"
	label var DIAB_GEST_1HR_CAN "GDM by oral glucose tolerance test - 1 hour - Canadian"
	label var DIAB_GEST_FASTING_CAN "GDM by fasting blood glucose test (pre-test) - Canadian"	
	label var DIAB_GEST_2HR_CAN "GDM by oral glucose tolerance test - 2 hours - Canadian"
	
	label var DIAB_GEST_ANY_CAN "GDM by Canadian Diabetes Association criteria (2+positive)"
	label var DIAB_GEST_ANY_WHO "GDM by WHO criteria (any test)"
	
	
	*review observations: 
	list DIAB_GEST_ANY DIAB_GEST_FASTING_1 DIAB_GEST_1HR_1 DIAB_GEST_2HR_1 ///
		 if DIAB_GEST_ANY != DIAB_GEST_FASTING | ///
		 DIAB_GEST_ANY != DIAB_GEST_1HR | ///
		 DIAB_GEST_ANY != DIAB_GEST_2HR 
		 
	tab DIAB_GEST_ANY DIAB_OVERT, m  
	
	drop DIAB_OVERT DIAB_OVERT_DX DIAB_GEST_1HR*WHO DIAB_GEST_1HR*ACOG ///
		DIAB_GEST_2HR*ACOG DIAB_GEST_FASTING*DIPSI DIAB_GEST_1HR*DIPSI 
		
	
	
	save "$wrk/gdm_mnh08_wide", replace 	
	clear 
	
	*Review data to construct gestational diabetes: 
	/*
	DIAB_GEST_DX
		Defined: Clinical dx of gestational diabetes, as indicated in 
		L&D form (MNH09) and/or hospitalization form (MNH19)
		Denominator: Women w/o pre-existing/overt diabetes >=20 weeks GA 
		Response options: yes(1); no(0); missing (55)
		Timing: ANC-28 or later
	*/
	clear 
	*First draw from MN19 - Hospitalization:
	import delimited "$da/mnh19_merged", bindquote(strict)
	
	*from var "LD_COMPL_MHTERM" -- option 3=Gestational Diabetes (dx l&d complication)
	tab m19_ld_compl_mhterm_3, m 
	
	*note: 1 observation with recorded GDM dx during hospitalization: 
	gen DIAB_GEST_DX_HOSP = m19_ld_compl_mhterm_3 if m19_ld_compl_mhterm_3 != . & ///
		m19_ld_compl_mhterm_3 != 77
		
	label var DIAB_GEST_DX_HOSP "DX of gestational diabetes at hospitalization"
	
	*clean up: 
	drop if momid == "" | pregid == ""
	
	*prepare momid/pregid for merging: 
	rename momid momid_old
	gen MOMID = ustrtrim(momid_old)
	
	rename pregid pregid_old
	gen PREGID = ustrtrim(pregid_old)
	
	drop momid_old pregid_old
	
	gen DIAB_GEST_DX_HOSP_DT = date(m19_ohostdat, "YMD") if ///
		m19_ohostdat != "1907-07-07" 
	replace DIAB_GEST_DX_HOSP_DT = date(m19_mat_est_ohostdat, "YMD") if ///
		m19_ohostdat == "1907-07-07" & m19_mat_est_ohostdat != "1907-07-07" & ///
		m19_mat_est_ohostdat != "1905-05-05"
		
	*if missing consider alternative date: ADMIT_OHOSTDAT
	replace DIAB_GEST_DX_HOSP_DT = date(m19_admit_ohostdat, "YMD") if ///
		DIAB_GEST_DX_HOSP_DT == . & m19_admit_ohostdat != "1907-07-07" & ///
		m19_admit_ohostdat != "1905-05-05"
		
	format DIAB_GEST_DX_HOSP_DT %td
		
	sum DIAB_GEST_DX_HOSP_DT, format
	
	tab site if DIAB_GEST_DX_HOSP_DT == . 
	
	
	*reshape to wide to accomodate multiple hospitalizations:
	*create an indicator for entry number for each person by date: 
	sort MOMID PREGID DIAB_GEST_DX_HOSP_DT
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "MNH19 Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create a macro with max entry number:
	sum ENTRY_NUM
	return list 
	
	global i = r(max)
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "MNH19 Total number of entries"
	
	keep MOMID PREGID DIAB_GEST_DX_HOSP_DT DIAB_GEST_DX_HOSP ///
		ENTRY_NUM ENTRY_TOTAL
	
	reshape wide DIAB_GEST_DX_HOSP DIAB_GEST_DX_HOSP_DT ///
		ENTRY_TOTAL, ///
		i(MOMID PREGID) j(ENTRY_NUM)  
	
	
	*create a comprehensive variable: 
	gen DIAB_GEST_DX_HOSP = . 
	label var DIAB_GEST_DX_HOSP "DX of gestational diabetes at hospitalization"
	
	gen DIAB_GEST_DX_HOSP_DT = .
	format DIAB_GEST_DX_HOSP_DT %td
	label var DIAB_GEST_DX_HOSP_DT "Date of gestational diabetes at hospitalization"
		
	foreach num of numlist 1/$i {
	    
	replace DIAB_GEST_DX_HOSP = DIAB_GEST_DX_HOSP`num' if ///
		DIAB_GEST_DX_HOSP == . | ///
		(DIAB_GEST_DX_HOSP == 0 & DIAB_GEST_DX_HOSP`num' == 1)
	
	replace DIAB_GEST_DX_HOSP_DT = DIAB_GEST_DX_HOSP_DT`num' if ///
		DIAB_GEST_DX_HOSP`num' == 1 &  ///
		(DIAB_GEST_DX_HOSP_DT == . | ///
		(DIAB_GEST_DX_HOSP_DT > DIAB_GEST_DX_HOSP_DT`num' & ///
		DIAB_GEST_DX_HOSP_DT!=.))
	
	}
		
	*prep final dataset:
	keep MOMID PREGID DIAB_GEST_DX_HOSP DIAB_GEST_DX_HOSP_DT 
	
	save "$wrk/gdm_dx_mnh19", replace 
	
	
	*Second, draw from MNH09 - L&D 
	clear 
	import delimited "$da/mnh09_merged", bindquote(strict)
	
	tab m09_gest_diab_mhoccur, m 
	
	gen DIAB_GEST_DX_IPC = m09_gest_diab_mhoccur 
	
	replace DIAB_GEST_DX_IPC = 55 if m09_gest_diab_mhoccur == 77 | ///
		m09_gest_diab_mhoccur == 99 | m09_gest_diab_mhoccur == . 
		
	drop if momid == "" | pregid == ""
	
	label var DIAB_GEST_DX_IPC "DX of Gestational Diabetes recorded at L&D"
	tab DIAB_GEST_DX_IPC, m 
	
	*Construct treatment variables: 
	
	/*
	DIAB_TREAT
		Defined: Treatment for gestational diabetes reported at L&D 
		Denominator: All completed pregnancies
		Response options: yes(1); no(0); missing (55); NA (77)
		Timing: L&D 
		
	DIAB_TREAT_INS
		Defined: Treatment for gestational diabetes reported at L&D - insulin
		Denominator: All completed pregnancies
		Response options: yes(1); no(0); missing (55); NA (77)
		Timing: L&D 
		
	DIAB_TREAT_MET
		Defined: Treatment for gestational diabetes reported at L&D - metformin
		Denominator: All completed pregnancies
		Response options: yes(1); no(0); missing (55); NA (77)
		Timing: L&D 
		
	DIAB_TREAT_GLY
		Defined: Treatment for gestational diabetes reported at L&D - glyburide
		Denominator: All completed pregnancies
		Response options: yes(1); no(0); missing (55); NA (77)
		Timing: L&D 
	*/
	
	* Insulin 
	gen DIAB_TREAT_INS = m09_gest_diab_proccur_1 
	replace DIAB_TREAT_INS = 0 if m09_gest_diab_proccur_1 == 77 
	replace DIAB_TREAT_INS = 55 if m09_gest_diab_proccur_1 == . 
	label var DIAB_TREAT_INS "Treatment for gestational diabetes reported at L&D - insulin"
	
	* Metformin 
	gen DIAB_TREAT_MET = m09_gest_diab_proccur_2 
	replace DIAB_TREAT_MET = 0 if m09_gest_diab_proccur_2 == 77
	replace DIAB_TREAT_MET = 55 if m09_gest_diab_proccur_2 == . 
	label var DIAB_TREAT_MET "Treatment for gestational diabetes reported at L&D - metformin"	
	
	* Glyburide 
	gen DIAB_TREAT_GLY = m09_gest_diab_proccur_3
	replace DIAB_TREAT_GLY = 0 if m09_gest_diab_proccur_3 == 77
	replace DIAB_TREAT_GLY = 55 if m09_gest_diab_proccur_3 == . 
	label var DIAB_TREAT_GLY "Treatment for gestational diabetes reported at L&D - glyburide"	
	
	* Any treatment: 
	gen DIAB_TREAT = 0 if DIAB_TREAT_INS == 0 & DIAB_TREAT_MET == 0 & ///
		DIAB_TREAT_GLY == 0 
	replace DIAB_TREAT = 1 if DIAB_TREAT_INS == 1 | DIAB_TREAT_MET == 1 | ///
		DIAB_TREAT_GLY == 1 
	replace DIAB_TREAT = 55 if DIAB_TREAT == . 
		
	label var DIAB_TREAT "Treatment for gestational diabetes reported at L&D"
	
	tab DIAB_TREAT, m 
	tab DIAB_TREAT_INS, m 
	tab DIAB_TREAT_MET, m 
	tab DIAB_TREAT_GLY, m 
	
	keep momid pregid DIAB_GEST_DX_IPC DIAB_TREAT DIAB_TREAT_INS DIAB_TREAT_MET ///
		DIAB_TREAT_GLY 
		
	***merge to construct the comprehensive DX variable 
		//prepare momid/pregid for merging: 
	rename momid momid_old
	gen MOMID = ustrtrim(momid_old)
	
	rename pregid pregid_old
	gen PREGID = ustrtrim(pregid_old)
	
	drop momid_old pregid_old
	
	order MOMID PREGID, first
	
	merge 1:1 MOMID PREGID using "$wrk/gdm_dx_mnh19"
	
		// compare: 
	tab DIAB_GEST_DX_IPC DIAB_GEST_DX_HOSP, m 
	
		// make final var: 
	gen DIAB_GEST_DX = DIAB_GEST_DX_IPC 
	replace DIAB_GEST_DX = 1 if DIAB_GEST_DX_HOSP == 1
	replace DIAB_GEST_DX = 0 if DIAB_GEST_DX_HOSP == 0 & DIAB_GEST_DX_IPC != 1
	
	replace DIAB_GEST_DX = 55 if DIAB_GEST_DX_HOSP == . & DIAB_GEST_DX_IPC == . 
	
	label var DIAB_GEST_DX "Ever diagnosed with gestational diabetes"
	
	tab DIAB_GEST_DX, m
	
	drop _merge 
	
		// save final DX dataset:
	save "$wrk/gdm_dx_all", replace
	
	clear 
	
	

	///////////////////////////////////////////////
	///////////////////////////////////////////////
	*Merge in indicators to construct denominators:
	
	use "$OUT/MAT_ENROLL"
	
	// format date: 
	*update var format PREG_START_DATE // no longer needed as of 11-1
	rename PREG_START_DATE STR_PREG_START_DATE
	
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td 
	
	
	// restrict to enrolled cohort
	keep if ENROLL == 1 

	
	// Merge in Overt Diabetes:
	merge 1:1 MOMID PREGID using "$wrk/dm_mnh08_wide"
	
	drop if _merge == 2 
	drop _merge 
	
	// Merge in GDM: 
	merge 1:1 MOMID PREGID using "$wrk/gdm_mnh08_wide"
	
	drop if _merge == 2 
	drop _merge
	
	// Merge in Clinical DX of GDM:
	merge 1:1 MOMID PREGID using "$wrk/gdm_dx_all"
	
	drop if _merge == 2 
	drop _merge 
	
	// Merge in Prior DX of Overt Diabetes:
	merge 1:1 MOMID PREGID using "$wrk/dm_dx"
	
	drop if _merge == 2 
	drop _merge
	
	order MOMID PREGID SITE, first 
	
	//////////////////////////////
	*Finalize denominator variables: 
	
	
	//// OVER DIABETES: 
	tab ENROLL DIAB_OVERT, m 

	gen DIAB_OVERT_MISS = 0 if DIAB_OVERT == 1 | DIAB_OVERT == 0 | DIAB_OVERT_DX == 1 
	replace DIAB_OVERT_MISS = 1 if DIAB_OVERT == 55 & DIAB_OVERT_MISS !=0
	replace DIAB_OVERT_MISS = 2 if ENROLL == 1 & DIAB_OVERT == . 
	
	replace DIAB_OVERT = 55 if DIAB_OVERT == . 
	
	label define overtdiab 0 "0-Non-missing" 1 "1-Missing HbA1c test result" ///
		2 "2-Missing enrollment lab or other lab <20 weeks"
	label var DIAB_OVERT_MISS "Reason missing - Overt Diabetes"
	label values DIAB_OVERT_MISS overtdiab
		
	tab ENROLL DIAB_OVERT, m 
	tab ENROLL DIAB_OVERT_MISS, m 
	tab DIAB_OVERT DIAB_OVERT_MISS, m 
	
	tab DIAB_OVERT DIAB_OVERT_DX, m 
	tab DIAB_OVERT_DX DIAB_OVERT_MISS, m 
	
	
	// CONSTRUCT COMBINED VARIABLE FOR OVERT DIABETES: 
	gen DIAB_OVERT_ANY = DIAB_OVERT 
	replace DIAB_OVERT_ANY = 1 if DIAB_OVERT_DX == 1 
	replace DIAB_OVERT_ANY = 55 if DIAB_OVERT_ANY == . 
	
	label var DIAB_OVERT_ANY "Overt diabetes by HbA1c or preexisting diagnosis"
	
	tab DIAB_OVERT_ANY
	tab DIAB_OVERT_MISS DIAB_OVERT_ANY, m 


	
	//// GESTATIONAL DIABETES: 
	tab DIAB_GEST_ANY site if DIAB_OVERT != 1 & DIAB_OVERT_DX != 1, m 
	
	*ADDED 6-18: add indicator to remove pregnancies that ended at <28 weeks GA: 
	
	merge 1:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing (PREG_END ///
		PREG_END_GA CLOSEOUT CLOSEOUT_GA MAT_DEATH MAT_DEATH_GA ///
		PREG_LOSS_DEATH)

	drop if _merge ==2 
	drop _merge
	
	gen PREG_END_28 = 0 
	replace PREG_END_28 = 1 if PREG_END == 1 & PREG_END_GA <(28*7) & PREG_END_GA != . 
	replace PREG_END_28 = 1 if PREG_END==1 & PREG_LOSS_DEATH == 1 & ///
		MAT_DEATH_GA <(28*7) & MAT_DEATH_GA != . 
	
	*ADDED 6-18: add indicator to remove early closeouts <28 weeks GA:
	gen CLOSEOUT_28 = 0 
	replace CLOSEOUT_28 = 1 if CLOSEOUT == 1 & CLOSEOUT_GA <(28*7) & ///
		CLOSEOUT_GA != . 
		
	label var PREG_END_28 "=1 if pregnancy ended before 28 weeks GA"
	label var CLOSEOUT_28 "=1 if closeout prior to 28 weeks GA"
	
	
	*create indicator for completed ANC-28 late window (> 30 weeks GA = 210 days)
	gen COMPLETE_ANC28 = 0
	replace COMPLETE_ANC28 = 1 if (date("$dadate", "YMD") >= PREG_START_DATE + 216 & ///
		PREG_END_28 == 0 & CLOSEOUT_28 == 0) | ///
		(DIAB_GEST_ANY == 1 | DIAB_GEST_ANY == 0) 
	label var COMPLETE_ANC28 "Denominator - completed ANC-28 visit window w/ continued preg OR has OGTT measures"
		
	tab COMPLETE_ANC28, m 
	
	tab COMPLETE_ANC28 PREG_END_28, m 
	
	rename COMPLETE_ANC28 DIAB_GEST_DENOM 

	
		*COMPARE TO STACIE'S WINDOW VARS: 
	tab ANC28_PASS_ONTIME DIAB_GEST_DENOM if PREG_END_28==0, m 
	tab ANC28_PASS_LATE DIAB_GEST_DENOM if PREG_END_28==0, m 
	
	*review denominator & set missingness variables -  DIAB_GEST_ANY 
	tab DIAB_GEST_ANY if DIAB_GEST_DENOM==1 & DIAB_OVERT!=1 & DIAB_OVERT_DX != 1, m 	

		
	gen DIAB_GEST_ANY_MISS = 0 if (DIAB_GEST_ANY == 1 | DIAB_GEST_ANY == 0) & ///
		(DIAB_GEST_DENOM==1 & DIAB_OVERT!=1 & DIAB_OVERT_DX != 1)
		
	replace DIAB_GEST_ANY_MISS = 1 if DIAB_GEST_ANY == 55 & ///
		(DIAB_GEST_DENOM==1 & DIAB_OVERT!=1 & DIAB_OVERT_DX != 1)
		
	replace DIAB_GEST_ANY_MISS = 2 if DIAB_GEST_ANY == . & ///
		(DIAB_GEST_DENOM==1 & DIAB_OVERT!=1 & DIAB_OVERT_DX != 1)
		
	replace DIAB_GEST_ANY_MISS = 77 if (DIAB_OVERT ==1 | DIAB_OVERT_DX == 1) & DIAB_GEST_DENOM == 1
	
	replace DIAB_GEST_ANY = 55 if DIAB_GEST_ANY_MISS == 2 
	replace DIAB_GEST_ANY = 77 if (DIAB_OVERT == 1 | DIAB_OVERT_DX == 1) & DIAB_GEST_DENOM == 1
	
	label define gdmmiss 0 "0-Non-missing" 1 "1-Missing test results" ///
		2 "2-Missing visit lab result at/after ANC-28" 77 "77-Overt diabetes (N/A)"
	
	label values DIAB_GEST_ANY_MISS gdmmiss
	label var DIAB_GEST_ANY_MISS "Reason missing - Gestational Diabetes any"
	
	tab DIAB_GEST_ANY_MISS DIAB_GEST_ANY, m 
	tab DIAB_GEST_ANY_MISS DIAB_GEST_ANY if DIAB_GEST_DENOM==1, m 
	

	
	*review denominator & set missingness variables -  test-specific

	foreach var of varlist DIAB_GEST_FASTING DIAB_GEST_1HR DIAB_GEST_2HR {
		
	tab `var' if DIAB_GEST_DENOM==1 & DIAB_OVERT!=1 & DIAB_OVERT_DX != 1, m 	
		
	gen `var'_MISS = 0 if (`var' == 1 | `var' == 0) & ///
		(DIAB_GEST_DENOM==1 & DIAB_OVERT!=1 & DIAB_OVERT_DX != 1)
		
	replace `var'_MISS = 1 if `var' == 55 & ///
		(DIAB_GEST_DENOM==1 & DIAB_OVERT!=1 & DIAB_OVERT_DX != 1)
		
	replace `var'_MISS = 2 if `var' == . & ///
		(DIAB_GEST_DENOM==1 & DIAB_OVERT!=1 & DIAB_OVERT_DX != 1)
		
	replace `var'_MISS = 77 if (DIAB_OVERT ==1 | DIAB_OVERT_DX == 1) & DIAB_GEST_DENOM == 1
	
	replace `var' = 55 if `var'_MISS == 2 
	replace `var' = 77 if DIAB_GEST_DENOM == 1 & (DIAB_OVERT == 1 | DIAB_OVERT_DX == 1)
	
	label values `var'_MISS gdmmiss
	
	}
	
	label var DIAB_GEST_FASTING_MISS "Reason missing - Fasting glucose/pretest"
	label var DIAB_GEST_1HR_MISS "Reason missing - 1-hr OGTT"
	label var DIAB_GEST_2HR_MISS "Reason missing - 2-hr OGTT"
	
	*for test-specific variables:
	foreach var of varlist DIAB_GEST_FASTING DIAB_GEST_1HR DIAB_GEST_2HR {
	replace `var' = 55 if `var' == . & DIAB_GEST_ANY_MISS == 2 
	}
	
	*for continuous variables, replace with missing:
	foreach var of varlist BGLUC_PRETEST_MMOLL_LBORRES ///
		BGLUC_ORAL_1HR_MMOLL_LBORRES BGLUC_ORAL_2HR_MMOLL_LBORRES {
	replace `var' = . if `var' <0
		}
	
	*compare to dx of gestational diabetes recorded: 
	tab DIAB_GEST_FASTING DIAB_GEST_DX, m 
	tab DIAB_GEST_1HR DIAB_GEST_DX, m 
	tab DIAB_GEST_2HR DIAB_GEST_DX, m 
	tab DIAB_GEST_ANY DIAB_GEST_DX, m 
	
	*restrict to variables for output:
	keep MOMID PREGID SITE ENROLL DIAB_OVERT_ANY DIAB_OVERT DIAB_OVERT_20 ///
		DIAB_OVERT_MISS HBA1C_PRCNT HBA1C_COUNT DIAB_GEST_FASTING ///
		DIAB_GEST_FASTING_GA DIAB_GEST_1HR DIAB_GEST_1HR_GA DIAB_GEST_2HR ///
		DIAB_GEST_2HR_GA DIAB_GEST_ANY DIAB_GEST_ANY_MISS BGLUC_COUNT ///
		DIAB_GEST_DENOM DIAB_GEST_FASTING_MISS DIAB_GEST_1HR_MISS ///
		DIAB_GEST_2HR_MISS BGLUC_PRETEST_MMOLL_LBORRES ///
		BGLUC_ORAL_1HR_MMOLL_LBORRES BGLUC_ORAL_2HR_MMOLL_LBORRES ///
		DIAB_GEST_DX DIAB_GEST_DX_IPC DIAB_GEST_DX_HOSP DIAB_GEST_DX_HOSP_DT ///
		DIAB_OVERT_DX PREG_END DIAB_TREAT DIAB_TREAT_INS DIAB_TREAT_MET ///
		DIAB_TREAT_GLY ///
		DIAB_GEST_*_WHO DIAB_GEST_*_ACOG DIAB_GEST_*_DIPSI DIAB_GEST_*_CAN 
		
		
	* Renaming per the new variable naming convention: 
	rename	DIAB_GEST_FASTING_WHO	DIAB_GEST_WHO_FASTING
	rename	DIAB_GEST_2HR_WHO	DIAB_GEST_WHO_2HR
	rename	DIAB_GEST_ANY_WHO	DIAB_GEST_WHO_ANY
	rename	DIAB_GEST_2HR_DIPSI	DIAB_GEST_DIPSI_2HR
	rename	DIAB_GEST_FASTING_CAN	DIAB_GEST_CAN_FASTING
	rename	DIAB_GEST_1HR_CAN	DIAB_GEST_CAN_1HR
	rename	DIAB_GEST_2HR_CAN	DIAB_GEST_CAN_2HR
	rename	DIAB_GEST_ANY_CAN	DIAB_GEST_CAN_ANY
	rename	DIAB_GEST_FASTING_ACOG	DIAB_GEST_ACOG_FASTING
	
	save "$wrk/GDM", replace 
	
	tab DIAB_GEST_ANY DIAB_GEST_DENOM if SITE == "India-SAS", m 
	
	*CHECK ON VARS: 
	sort DIAB_OVERT
	by DIAB_OVERT: sum HBA1C_PRCNT
	
	sort DIAB_GEST_FASTING
	by DIAB_GEST_FASTING: sum BGLUC_PRETEST_MMOLL_LBORRES

	sort DIAB_GEST_1HR
	by DIAB_GEST_1HR: sum BGLUC_ORAL_1HR_MMOLL_LBORRES
	
	sort DIAB_GEST_2HR
	by DIAB_GEST_2HR: sum BGLUC_ORAL_2HR_MMOLL_LBORRES
	
	
	*** Review medications:
	
		// Overt diabetes: 
		
	foreach var of varlist DIAB_TREAT_INS DIAB_TREAT_MET ///
		DIAB_TREAT_GLY DIAB_TREAT {
	tab `var' if DIAB_OVERT == 1 & DIAB_OVERT_DX == 1 & PREG_END==1, m 
	}
	
	foreach var of varlist DIAB_TREAT_INS DIAB_TREAT_MET ///
		DIAB_TREAT_GLY DIAB_TREAT {
	tab `var' if DIAB_OVERT == 0 & DIAB_OVERT_DX == 1 & PREG_END==1, m 
	}
	
	foreach var of varlist DIAB_TREAT_INS DIAB_TREAT_MET ///
		DIAB_TREAT_GLY DIAB_TREAT {
	tab `var' if DIAB_OVERT == 1 & PREG_END==1 & ///
		(DIAB_OVERT_DX == 0 | DIAB_OVERT_DX == 55 | DIAB_OVERT_DX == .), m 
	}
	
		// Gestational diabetes: 
		
	foreach var of varlist DIAB_TREAT_INS DIAB_TREAT_MET ///
		DIAB_TREAT_GLY DIAB_TREAT {
	tab `var' if DIAB_GEST_FASTING ==1 & PREG_END==1 & DIAB_GEST_DENOM==1, m 
	}
	
	foreach var of varlist DIAB_TREAT_INS DIAB_TREAT_MET ///
		DIAB_TREAT_GLY DIAB_TREAT {
	tab `var' if DIAB_GEST_1HR ==1 & PREG_END==1 & DIAB_GEST_DENOM==1, m 
	}
	
	foreach var of varlist DIAB_TREAT_INS DIAB_TREAT_MET ///
		DIAB_TREAT_GLY DIAB_TREAT {
	tab `var' if DIAB_GEST_2HR == 1 & PREG_END==1 & DIAB_GEST_DENOM==1, m 
	}
	
	foreach var of varlist DIAB_TREAT_INS DIAB_TREAT_MET ///
		DIAB_TREAT_GLY DIAB_TREAT {
	tab `var' if DIAB_GEST_ANY == 1 & PREG_END==1 & DIAB_GEST_DENOM==1, m 
	}
	
	
	*Review dx criteria by site: 
	foreach var of varlist *_WHO_* *_ACOG_* *_DIPSI_* *_CAN_* ///
		DIAB_GEST_FASTING DIAB_GEST_1HR DIAB_GEST_2HR DIAB_GEST_ANY {
		
	tab `var' if PREG_END==1 & DIAB_GEST_DENOM==1 & `var'!=55 & `var'!=. & DIAB_OVERT!=1,  m 
	
	tab `var' SITE if PREG_END==1 & DIAB_GEST_DENOM==1 & `var'!=55 & `var'!=. & DIAB_OVERT!=1, m 
	 
	}
	
	
	
	drop ENROLL PREG_END
	
	foreach var of varlist DIAB_GEST_ANY DIAB_GEST_FASTING DIAB_GEST_1HR ///
		DIAB_GEST_2HR {
	
	tab `var' DIAB_GEST_DENOM, m 
	
		}
		

	*check a few conditions:
		*assert will "crash" the do file if these conditions not met
	
	assert DIAB_GEST_DENOM == 1 if inlist(DIAB_GEST_ANY,0,1)
		*check that denominator == 1 if there is a 0/1 result for DIAB_GEST_ANY
	
	assert !inlist(DIAB_GEST_ANY,0,1) if DIAB_OVERT_ANY == 1
		*check that if DIAB_OVERT_ANY ==1, NO result for DIAB_GEST_ANY

	* Save a clean copy of GDM to the outcome folder: 
	tab 
	save "$OUT/MAT_GDM", replace 
	
	
	
