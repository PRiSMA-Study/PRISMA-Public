*PRISMA Maternal Variable Construction Code - Anemia
*Purpose: This code drafts variable construction code for maternal outcome
	*variables for the PRISMA study - Anemia outcomes
*Original Version: March 6, 2024 by E Oakley (emoakley@gwu.edu)
*Update: March 25, 2024 by E Oakley (incorporate feedback from Dr. Wiley)
*Split to separate file: March 27, 2024 by E Oakley
*Update: April 2, 2024 by E Oakley (incorporate adjustments from Xiaoyan)
*Update: April 3, 2024 by E Oakley (incorporate denominator feedback from Dr. Smith)
*Update: May 15, 2024 by E Oakley (incorporate feedback from Chris & ERS)
*Update: June 28, 2024 by E Oakley (minor updates to data structure)
*Update: July 2, 2024 by E Oakley (update PNC 6 window to 14+6 -- 104 days PP)
*Update: October 1, 2024 by E Oakley (update file path to accomodate 9-20 data)
*Update: January 10, 2025 by E Oakley (minor updates to var names per convention)
*Update: January 14, 2025 by E Oakley (fix to statement to make GA for T1 <98 (not <=98); update variable names for PREG_START_DATE and ENROLL_SCRN_DATE)
*Update: March 10, 2025 by E Oakley (correction to Hb values <0 after elevation adjustment)
*Update: May 27, 2025 by E Oakley (add onset dates for anemia levels)
*Update: May 28, 2025 by E Oakley (revise construction for "any anemia" variable)
*Update: November 19, 2025 by E Oakley (minor updates to the long dataset to prepare for sharing)

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
global dadate "2025-10-31" // this should be considered the date the data was most recently updated & will be used to calculate whether different gestational windows are completed
global da "Z:/Stacked Data/$dadate"

global OUT "D:\Users\emoakley\Documents\Outcome Data/$dadate"


	// Working Files Folder (TNT-Drive)
global wrk "D:\Users\emoakley\Documents\working files-25-11-09"

global date "251119" 

log using "$log/mat_outcome_construct_anemia_$date", replace


/* Maternal Anemia - Measured & Clinical at multiple time points 

	MAT_ANEMIA_ANY Low hemoglobin levels and/or diagnosis of anemia in pregnancy, at labor and delivery, and through 6 months postpartum. 
	
	MAT_ANEMIA_M Low hemoglobin levels in pregnancy, at labor and delivery, and through 6 months postpartum. Based on results from point-of-care hemoglobin tests or complete blood count. 
	
	MAT_ANEMIA_DX Diagnosis of anemia in pregnancy, at labor and delivery, and through 6 months postpartum. 
	
	MAT_ANEMIA_M_ANC Low hemoglobin levels throughout pregnancy, classified as mild (10-10.9 g/dL), moderate (7-9.9 g/dL), or severe (<7 g/dL).
	
		*See also by measure type: MAT_ANEMIA_CBC_ANC MAT_ANEMIA_POC_ANC
		*See also by trimester: MAT_ANEMIA_M_ANC_T1 MAT_ANEMIA_M_ANC_T2 MAT_ANEMIA_M_ANC_T3
		*See also by trimester & measure type: 
			MAT_ANEMIA_CBC_ANC_T1 MAT_ANEMIA_CBC_ANC_T2 MAT_ANEMIA_CBC_ANC_T3
			MAT_ANEMIA_POC_ANC_T1 MAT_ANEMIA_POC_ANC_T2 MAT_ANEMIA_POC_ANC_T3
	
	MAT_ANEMIA_M_IPC Low hemoglobin levels at labor and delivery, classified as mild (10-10.9 g/dL), moderate (7-9.9 g/dL), or severe (<7 g/dL).
	
		*See also by measure type: MAT_ANEMIA_CBC_IPC MAT_ANEMIA_POC_IPC
	
	MAT_ANEMIA_M_PNC6 Low hemoglobin levels in the postpartum period, classified as mild (11-11.9 g/dL), moderate (8-10.9 g/dL), or severe (<8 g/dL).
	
		*See also by measure type: MAT_ANEMIA_CBC_PNC6 MAT_ANEMIA_POC_PNC6 
		*See also outcome including late visit window: MAT_ANEMIA_M_PNC6L 
	
	MAT_ANEMIA_M_PNC26 Low hemoglobin levels in the postpartum period, classified as mild (11-11.9 g/dL), moderate (8-10.9 g/dL), or severe (<8 g/dL).
	
		*See also by measure type: MAT_ANEMIA_CBC_PNC26 MAT_ANEMIA_POC_PNC26 
		*See also outcome including late visit window: MAT_ANEMIA_M_PNC26L 

*/


/* Incorporating hemoglobin measure adjustments (added on 4-2-2024):

	Per the REMAPP protocol, we will adjust Hb measures at the individual 
	level as follows:
	
	1. Altitude: UPDATED BASED ON NEW WHO GUIDELINES: 
	We will adjust for altitude at the site level as follows:
		Kenya site: -0.8 units (adjustment for altitude >=1,000 to <1500)
		Zambia site: -0.8 units (adjustment for altitude >=1,000 to <1500)
	
	2. Smoking: For any smokers (cigarette, pipe, or cigar) identified based 
	on a positive response for var SMOKE_OECOCCUR in MNH03, we will subtract 
	0.3 units from hemoglobin measure at the individual level. 

*/

/* New adjustments to the code to set an algorithm for test type (added on 5-15-2024):

	Per feedback from ERS and CS:
	
	Anemia during any window of interest (ex: all of pregnancy, trimester 1, 
	trimester 2, etc.) will now be selected as follows:
		1. First, select the lowest measure  in the window (regardless of test type)
		2. If the lowest measure is a CBC test -> finalize this as the lowest measure
		3. If the lowest measure is a POC test -> check for an accompanying CBC 
		test within 7 days of POC tests
		  3a. If no CBC test within 7 days -> finalize the low POC test as the 
		      lowest measures
		  3b. If there is a CBC test within 7 days -> exclude the low POC test 
		      and restart at step 1. 
	
	This code also corrects for early pregnancy losses (not accounted for in 
	earlier versions of the code when reported only in MNH04/MNH19).
	
*/



/* New additions: date of onset (added on 5-27-2025):

	Date of first recorded reading for the following: 
	
		1. Any anemia (mild, moderate, or severe)
		2. Moderate/severe anemia 
		3. Severe anemia 
		
	We will apply the same rule, where we consider POCs ONLY in cases where 
	there is no CBC within 7 days. See code starting on line 3950. 
	
*/

	*Gather relevant variables:
	
	/////////////////////////////////
	*First create "smoker" indicator: 
	
	import delimited "$da/mnh03_merged", bindquote(strict)
	
	// SMOKE 
	tab m03_smoke_oecoccur, m 
	gen SMOKE = m03_smoke_oecoccur 
	replace SMOKE = . if m03_smoke_oecoccur == 77 
	label var SMOKE "Smoking status (binary)"
	
	rename momid MOMID 
	rename pregid PREGID
	
	// check for duplicates:
	drop if MOMID == "NA" | PREGID == "NA" | MOMID == "" | PREGID == "" 
	
	duplicates tag MOMID PREGID, gen(duplicate)
	tab duplicate, m 
	
	keep MOMID PREGID SMOKE 
	
	save "$wrk/SMOKE", replace 

	clear 

	
	////////////////////////////////////////////
	*Next compile HB datasets from all sources - MNH06:
	import delimited "$da/mnh06_merged", bindquote(strict)
	
	
	*Timepoint 1: ANC/PNC - MNH06 - POC 

		*Variables needed: 
			*MOMID / PREGID (identifiers)
			*TEST_DATE - visit/diagnosis date
			*TYPE_VISIT - type of visit (1-14)
			*HB_POC_LBPERF - POC hemoglobin test completed (Hemocue)
			*HB_POC_LBORRES - Result 
			*HB_POC_PEMETHOD - POC assessment method 
	
	*clean up: 
	drop if momid == "" | pregid == ""
	
	rename momid MOMID_old
	gen MOMID = ustrtrim(MOMID_old)
	
	rename pregid PREGID_old
	gen PREGID = ustrtrim(PREGID_old)
	
	drop MOMID_old PREGID_old
	
	*convert to dates:
	gen TEST_DATE = date(m06_diag_vsdat, "YMD") if m06_diag_vsdat != "1907-07-07"
	format TEST_DATE %td
	label var TEST_DATE "Date of HB test"	
	
	*create label for visit type 
	rename m06_type_visit TYPE_VISIT 
	label var TYPE_VISIT "Visit Type"
	label define vistype 1 "1-Enrollment" 2 "2-ANC-20" 3 "3-ANC-28" ///
		4 "4-ANC-32" 5 "5-ANC-36" 6 "6-IPC" 7 "7-PNC-0" 8 "8-PNC-1" ///
		9 "9-PNC-4" 10 "10-PNC-6" 11 "11-PNC-26" 12 "12-PNC-52" ///
		13 "13-ANC-Unsched" 14 "14-PNC-Unsched" 
	label values TYPE_VISIT vistype
	tab TYPE_VISIT, m 	
	
	*additional variables:
	gen HB_POC_LBPERF = m06_hb_poc_lbperf 
	label var HB_POC_LBPERF "POC HB test was performed (y/n)"
	
	gen HB_POC_PEMETHOD = m06_hb_poc_pemethod 
	label var HB_POC_PEMETHOD "Method of POC HB assessment"
	
	*Construct: 
	gen HB_POC_LBORRES = m06_hb_poc_lborres if m06_hb_poc_lborres != .
	destring HB_POC_LBORRES, replace 
	replace HB_POC_LBORRES = . if HB_POC_LBORRES < 0  // clean 19,443 observations
	replace HB_POC_LBORRES = . if HB_POC_LBORRES >= 99 // clean 1 observation
	label var HB_POC_LBORRES "POC hemoglobin results"
	sum HB_POC_LBORRES
	
	rename HB_POC_LBORRES HB_LBORRES
	
	*Create a variable for test type: 
	gen TEST_TYPE = "POC"
	label var TEST_TYPE "Type of Test (POC/CBC)"
	
	*restrict to entries with HB results:
	keep if HB_POC_LBPERF == 1
	
	order MOMID PREGID site TEST_DATE TEST_TYPE TYPE_VISIT HB_LBORRES ///
		HB_POC_LBPERF HB_POC_PEMETHOD 

	keep MOMID PREGID site TEST_DATE TEST_TYPE TYPE_VISIT HB_LBORRES ///
		HB_POC_LBPERF HB_POC_PEMETHOD 
		
	gen TEST_CRF = 6
	
	save "$wrk/anemia_mnh06", replace 	
	clear 
	
	
	////////////////////////////////////////////
	*Next compile HB datasets from all sources - MNH08:
	import delimited "$da/mnh08_merged", bindquote(strict)
	
	
	*Timepoint 2: ANC/PNC - MNH08 - CBC

		*Variables needed: 
			*MOMID / PREGID (identifiers)
			*TEST_DATE - visit/diagnosis date
			*TYPE_VISIT - type of visit (1-14)
			*HB_CBC_LBPERF - CBC test completed
			*HB_LBORRES - Result 
			
			
	*clean up: 
	drop if momid == "" | pregid == ""
	
	rename momid MOMID_old
	gen MOMID = ustrtrim(MOMID_old)
	
	rename pregid PREGID_old
	gen PREGID = ustrtrim(PREGID_old)
	
	drop MOMID_old PREGID_old
	
	*convert to dates:
	gen TEST_DATE = date(m08_lbstdat, "YMD") if m08_lbstdat != "1907-07-07"
	format TEST_DATE %td
	label var TEST_DATE "Date of HB test"	
	
	*create label for visit type 
	rename m08_type_visit TYPE_VISIT 
	label var TYPE_VISIT "Visit Type"
	label define vistype 1 "1-Enrollment" 2 "2-ANC-20" 3 "3-ANC-28" ///
		4 "4-ANC-32" 5 "5-ANC-36" 6 "6-IPC" 7 "7-PNC-0" 8 "8-PNC-1" ///
		9 "9-PNC-4" 10 "10-PNC-6" 11 "11-PNC-26" 12 "12-PNC-52" ///
		13 "13-ANC-Unsched" 14 "14-PNC-Unsched" 
	label values TYPE_VISIT vistype
	tab TYPE_VISIT, m 	
	
	*additional variables:
	gen HB_CBC_LBPERF = m08_cbc_lbperf_1
	label var HB_CBC_LBPERF "CBC HB test was performed (y/n)"
	
	*Construct: 
	gen HB_LBORRES = m08_cbc_hb_lborres if m08_cbc_hb_lborres != .
	destring HB_LBORRES, replace 
	replace HB_LBORRES = . if HB_LBORRES < 0  // clean 9,690 observations
	replace HB_LBORRES = . if HB_LBORRES >= 99 // clean 0 observation
	label var HB_LBORRES "CBC hemoglobin results"
	sum HB_LBORRES
	
	*Create a variable for test type: 
	gen TEST_TYPE = "CBC"
	label var TEST_TYPE "Type of Test (POC/CBC)"
	
	*restrict to entries with HB results:
	keep if HB_CBC_LBPERF == 1
	
	order MOMID PREGID site TEST_DATE TEST_TYPE TYPE_VISIT HB_LBORRES ///
		HB_CBC_LBPERF 

	keep MOMID PREGID site TEST_DATE TEST_TYPE TYPE_VISIT HB_LBORRES ///
		HB_CBC_LBPERF
		
	gen TEST_CRF=8
	
	save "$wrk/anemia_mnh08", replace 		

	clear 
	
////////////////////////////////////////////
	*Next compile HB datasets from hospitalization form - MNH19:
	import delimited "$da/mnh19_merged", bindquote(strict)
	
	
	*Timepoint 3: Hosptialization MNH19 - POC 

		*Variables needed: 
			*MOMID / PREGID (identifiers)
			*TEST_DATE - visit/diagnosis date
			*TIME_HOSP - timing of hospitalization 
			*HB_POC_LBPERF - POC hemoglobin test completed (Hemocue)
			*HB_POC_LBORRES - Result 
			*HB_POC_PEMETHOD - POC assessment method 
	
	*clean up: 
	drop if momid == "" | pregid == ""
	
	rename momid MOMID_old
	gen MOMID = ustrtrim(MOMID_old)
	
	rename pregid PREGID_old
	gen PREGID = ustrtrim(PREGID_old)
	
	drop MOMID_old PREGID_old
	
	*convert to dates:
	gen TEST_DATE = date(m19_hb_poc_lbtstdat, "YMD") if m19_hb_poc_lbtstdat != "1907-07-07"
	format TEST_DATE %td
	label var TEST_DATE "Date of HB test"	
	
	*create label for hospitalization timing:  
	rename m19_timing_ohocat TIME_HOSP
	label var TIME_HOSP "Timing of hospitalization"
	label define hosptype 1 "1-Antenatal" 2 "2-Postnatal" 77 "77-N/A"
	label values TIME_HOSP hosptype
	tab TIME_HOSP, m 	
	
	*additional variables:
	gen HB_POC_LBPERF = m19_hb_poc_lbperf 
	label var HB_POC_LBPERF "POC HB test was performed (y/n)"
	
	gen HB_POC_PEMETHOD = m19_hb_poc_lbmethod 
	label var HB_POC_PEMETHOD "Method of POC HB assessment"
	
	*Construct: 
	gen HB_POC_LBORRES = m19_hb_poc_lborres if m19_hb_poc_lborres != .
	destring HB_POC_LBORRES, replace 
	replace HB_POC_LBORRES = . if HB_POC_LBORRES < 0  // clean 336 observations
	replace HB_POC_LBORRES = . if HB_POC_LBORRES >= 99 // clean 0 observations
	label var HB_POC_LBORRES "POC hemoglobin results"
	sum HB_POC_LBORRES
	
	rename HB_POC_LBORRES HB_LBORRES
	
	*Create a variable for test type: 
	gen TEST_TYPE = "POC"
	label var TEST_TYPE "Type of Test (POC/CBC)"
	
	*restrict to entries with HB results:
	keep if HB_POC_LBPERF == 1
	
	order MOMID PREGID site TEST_DATE TEST_TYPE TIME_HOSP HB_LBORRES ///
		HB_POC_LBPERF HB_POC_PEMETHOD 

	keep MOMID PREGID site TEST_DATE TEST_TYPE TIME_HOSP HB_LBORRES ///
		HB_POC_LBPERF HB_POC_PEMETHOD 
		
	gen TEST_CRF=19
	
	save "$wrk/anemia_mnh19_poc", replace 	
	clear 
	
	
	*Timepoint 4: Hosptialization MNH19 - CBC 

		*Variables needed: 
			*MOMID / PREGID (identifiers)
			*TEST_DATE - visit/diagnosis date
			*TIME_HOSP - timing of hospitalization 
			*HB_CBC_LBPERF - CBC hemoglobin test completed
			*HB_LBORRES - Result 
			
	import delimited "$da/mnh19_merged" 
	
	*clean up: 
	drop if momid == "" | pregid == ""
	
	rename momid MOMID_old
	gen MOMID = ustrtrim(MOMID_old)
	
	rename pregid PREGID_old
	gen PREGID = ustrtrim(PREGID_old)
	
	drop MOMID_old PREGID_old
	
	*convert to dates:
	gen TEST_DATE = date(m19_cbc_lbdat, "YMD") if m19_cbc_lbdat != "1907-07-07"
	format TEST_DATE %td
	label var TEST_DATE "Date of HB test"	
	
	*create label for hospitalization timing:  
	rename m19_timing_ohocat TIME_HOSP
	label var TIME_HOSP "Timing of hospitalization"
	label define hosptype 1 "1-Antenatal" 2 "2-Postnatal" 77 "77-N/A"
	label values TIME_HOSP hosptype
	tab TIME_HOSP, m 	
	
	*additional variables:
	gen HB_CBC_LBPERF = m19_cbc_spcperf
	label var HB_CBC_LBPERF "CBC HB test was performed (y/n)"
	
	*Construct: 
	gen HB_LBORRES = m19_cbc_hb_lborres if m19_cbc_hb_lborres != .
	destring HB_LBORRES, replace 
	replace HB_LBORRES = . if HB_LBORRES < 0  // clean 336 observations
	replace HB_LBORRES = . if HB_LBORRES >= 99 // clean 0 observations
	label var HB_LBORRES "Hemoglobin results"
	sum HB_LBORRES
	
	*Create a variable for test type: 
	gen TEST_TYPE = "CBC"
	label var TEST_TYPE "Type of Test (POC/CBC)"
	
	*restrict to entries with HB results:
	keep if HB_CBC_LBPERF == 1
	
	order MOMID PREGID site TEST_DATE TEST_TYPE TIME_HOSP HB_LBORRES ///
		HB_CBC_LBPERF 

	keep MOMID PREGID site TEST_DATE TEST_TYPE TIME_HOSP HB_LBORRES ///
		HB_CBC_LBPERF 
		
	gen TEST_CRF=19
	
	save "$wrk/anemia_mnh19_cbc", replace 	
	clear 	
	
	
/////////////////////////////////////	
	*Append files together:
	
		/// CBC - MNH08 
	use "$wrk/anemia_mnh08"
	
		/// POC - MNH06 
	append using "$wrk/anemia_mnh06"
	
		/// Hospitalization CBC 
	append using "$wrk/anemia_mnh19_cbc"
	
		/// Hospitalization POC 
	append using "$wrk/anemia_mnh19_poc"
	
		/// order file by person & date 
	sort MOMID PREGID TEST_DATE
	
		/// create indicator for number of entries per person: 
	sort MOMID PREGID TEST_DATE TYPE_VISIT TIME_HOSP
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "HB Test Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of HB tests"
	
	*Check on indicators: 
	list MOMID PREGID ENTRY_NUM TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT ///
		TIME_HOSP if ENTRY_TOTAL > 16
	
	
///////////////////////////////////
	////////////// Update Dataset: 
	
	////////////////////////
	*Merge in Enrolled data: 
	
	merge m:1 MOMID PREGID using "$OUT/MAT_ENROLL", gen(ENROLL_merge)
	
	*update var format PREG_START_DATE // no longer needed for the 10-18 data: 
	rename PREG_START_DATE STR_PREG_START_DATE
	
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td 
	
	////////////////////////////////////////////////////////////////////
	*TEMPORARY MEASURE: REMOVE PREG_START_DATE FOR ERROR CASE: KEARC00074
	replace PREG_START_DATE = . if PREG_START_DATE == date("20240407", "YMD") & ///
		MOMID == "KEARC00074"
	////////////////////////////////////////////////////////////////////
	
	tab ENROLL_merge, m 
	
	tab ENROLL, m 
		
	*Check enrollment date for people with no HB tests yet:
	tab ENROLL_SCRN_DATE if ENROLL_merge == 2

	
	*DECISION POINT: We will drop where there is no MNH02/Enrollment Indicator:
	drop if ENROLL == . 
	
	 
	///////////////////////
	*Merge in Smoking Data:
	merge m:1 MOMID PREGID using "$wrk/SMOKE", gen(SMOKE_merge)
	
	replace SMOKE = 77 if SMOKE == . 
	
	drop if SMOKE_merge == 2
	
	
	///////////////////////
	*Merge in PREGEND data:
	
	merge m:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", gen(END_merge) ///
		keepusing(PREG_END PREG_END_DATE PREG_END_GA ///
				  CLOSEOUT CLOSEOUT_TYPE CLOSEOUT_DT CLOSEOUT_GA ///
				  MAT_DEATH MAT_DEATH_DATE MAT_DEATH_GA STOP_DATE)
	
	drop if END_merge == 2 
	
	
	//////////////////////
	*Fix site variable:
	replace site = SITE if site == ""
	
	replace PREG_END = 0 if PREG_END == .
	
	tab site, m 
	
	drop *_merge
	
	
	/////////////////////////////////////////////
	*Review instances of a measure with no date:
	list site TEST_DATE TEST_TYPE HB_LBORRES if TEST_DATE == . & HB_LBORRES != .
	
	

///////////////////////////////////////
	///////////////// * Construction 
	
		*create an indicator for GA at test:
	gen TEST_GA = TEST_DATE - PREG_START_DATE
	label var TEST_GA "GA at test (in days)"
	
	sum TEST_GA	
	
	replace TEST_GA = . if TEST_DATE > PREG_END_DATE & TEST_DATE!=. & ///
		PREG_END_DATE!=. 

	
		*CHECK ON NEGATIVE TESTS:
	list MOMID site PREG_START_DATE ENROLL_SCRN_DATE TEST_DATE TEST_GA ///
		TEST_TYPE TYPE_VISIT TIME_HOSP PREG_END_DATE if TEST_GA <0
		
		////////////////////
		* * * * FIX DATES
		gen FIXED_TEST_DATE = 1 if (MOMID == "KA3cafbe4e-3868-4d8c-81e7-0e0ea7faeb07" & ///
			TEST_DATE == date("03112022", "DMY") & TEST_GA < 0) | ///
			(MOMID == "PFb6b0ad08-56d0-48a4-8bdd-e1f4ccd7b9ac" & ///
			TEST_DATE == date("29062022", "DMY") & TEST_GA < 0) | ///
			(MOMID == "Z3-025-0927" & ///
			TEST_DATE == date("11042020", "DMY") & TEST_GA < 0) | ///
			(MOMID == "Z3-025-1177" & ///
			TEST_DATE == date("23022023", "DMY") & TEST_GA < 0 ) | ///
			(MOMID == "Z3-025-1209" & ///
			TEST_DATE == date("12012023", "DMY") & TEST_GA < 0 ) | ///
			(MOMID == "Z3-025-1254" & ///
			TEST_DATE == date("23012023", "DMY") & TEST_GA < 0 ) | ///
			(MOMID == "Z3-025-1387" & ///
			TEST_DATE == date("18012023", "DMY") & TEST_GA < 0 ) | ///
			(MOMID == "Z3-025-1418" & ///
			TEST_DATE == date("15012023", "DMY") & TEST_GA < 0 ) | ///
			(MOMID == "Z3-025-1010" & ///
			TEST_DATE == date("22032924", "DMY") & TEST_GA > 50000 ) | ///
			(MOMID == "Z3-025-1677" & ///
			TEST_DATE == date("09052224", "DMY") & TEST_GA > 5000 )				
			
			
		label var FIXED_TEST_DATE "=1 if fixed test date due to negative GA"
		tab FIXED_TEST_DATE, m 

			
		
		replace TEST_DATE = date("03112023", "DMY") if MOMID == "KA3cafbe4e-3868-4d8c-81e7-0e0ea7faeb07" & ///
			TEST_DATE == date("03112022", "DMY") & TEST_GA < 0
			
		replace TEST_DATE = date("29062023", "DMY") if MOMID == "PFb6b0ad08-56d0-48a4-8bdd-e1f4ccd7b9ac" & ///
			TEST_DATE == date("29062022", "DMY") & TEST_GA < 0 
			
		replace TEST_DATE = date("11042023", "DMY") if MOMID == "Z3-025-0927" & ///
			TEST_DATE == date("11042020", "DMY") & TEST_GA < 0 
			
		replace TEST_DATE = date("23022024", "DMY") if MOMID == "Z3-025-1177" & ///
			TEST_DATE == date("23022023", "DMY") & TEST_GA < 0 
			
		replace TEST_DATE = date("12012024", "DMY") if (MOMID == "Z3-025-1209" & ///
			TEST_DATE == date("12012023", "DMY") & TEST_GA < 0 )
			
		replace TEST_DATE = date("23012024", "DMY") if (MOMID == "Z3-025-1254" & ///
			TEST_DATE == date("23012023", "DMY") & TEST_GA < 0 ) 
			
		replace TEST_DATE = date("18012024", "DMY") if (MOMID == "Z3-025-1387" & ///
			TEST_DATE == date("18012023", "DMY") & TEST_GA < 0 ) 
			
		replace TEST_DATE = date("15012024", "DMY") if (MOMID == "Z3-025-1418" & ///
			TEST_DATE == date("15012023", "DMY") & TEST_GA < 0 )
			
		replace TEST_DATE = date("22032024", "DMY") if (MOMID == "Z3-025-1010" & ///
			TEST_DATE == date("22032924", "DMY") & TEST_GA > 50000 )
			
		replace TEST_DATE = date("09052024", "DMY") if (MOMID == "Z3-025-1677" & ///
			TEST_DATE == date("09052224", "DMY") & TEST_GA > 5000 )		
			
		* * * * FIX GA VAR FOR FIXED DATES: 
		replace TEST_GA = TEST_DATE - PREG_START_DATE if FIXED_TEST_DATE==1
		
		*RECHECK FIXES: 
		list MOMID site PREG_START_DATE ENROLL_SCRN_DATE TEST_DATE TEST_GA ///
		TEST_TYPE TYPE_VISIT TIME_HOSP PREG_END_DATE if FIXED_TEST_DATE == 1
		
		* * * * Address irresolvable dates:
		replace TEST_GA = . if TEST_GA < 0
		replace TEST_DATE = . if TEST_DATE==date("19050505", "YMD") | ///
			TEST_DATE==date("19070707", "YMD")
		
	
	*create an indicator for days PP at dx: 
	
		// remove missing pregnancy end dates: 
		replace PREG_END_GA = . if PREG_END_DATE == date("07071907", "DMY")
		replace PREG_END_DATE = . if PREG_END_DATE == date("07071907", "DMY")
	
	gen TEST_PP = TEST_DATE - PREG_END_DATE if TEST_DATE >= PREG_END_DATE & ///
		PREG_END == 1 
	label var TEST_PP "Days postpartum at test (in days)"
	
	tab TEST_PP, m 
	
	list MOMID TEST_PP TEST_DATE PREG_END_DATE if TEST_PP>500 & TEST_PP!=.

	///////////////////////////////////////////
	* Create a final indicator for test timing: 
	
	sort MOMID PREGID TEST_DATE
	
		// Timing 0 (ANC) if Test Date occurs after conception & before pregnancy end date
		*** For pregnancies with no enddate yet, we will cap at TEST_GA <=308 days
	gen TEST_TIMING = 0 if TEST_DATE > PREG_START_DATE & ///
		((TEST_DATE < PREG_END_DATE & PREG_END == 1) | ///
		(PREG_END == 0 & TEST_GA <=308))
		
		// Timing 1 (IPC) if Test Date occurs on the same day as pregnancy end date 
		**** For those marked as an IPC visit, we should allow for pregnancy end date +/- 1 day
	replace TEST_TIMING = 1 if PREG_END == 1 & ///
		(TEST_DATE == PREG_END_DATE | ///
		((TEST_DATE == PREG_END_DATE + 1 | TEST_DATE == PREG_END_DATE - 1) ///
		& TYPE_VISIT == 6))
	
		// Timing 2 (PNC) if Test Date occurs after pregnancy end date 
	replace TEST_TIMING = 2 if TEST_DATE > PREG_END_DATE & PREG_END == 1 
	
		// N/A (77) if no labs 
	replace TEST_TIMING = 77 if TEST_TYPE == "" & HB_LBORRES == . 
	
	list MOMID PREGID site TEST_DATE TEST_GA TEST_PP TYPE_VISIT TIME_HOSP ///
		PREG_START_DATE PREG_END_DATE ENROLL PREG_END PREG_END_GA if TEST_TIMING == . 
		
	*Address tests with missing GA information: 
		// If PNC visit & after GA 280 --> set to postnatal timing: 
		replace TEST_TIMING = 2 if TEST_TIMING == . & TYPE_VISIT >= 7 & ///
			TYPE_VISIT <= 12 & TEST_GA > 280
			
		// If ANC visit & occurred before pregnancy endpoint --> set to anc timing:
		replace TEST_TIMING = 0 if TEST_TIMING == . & ((TYPE_VISIT >= 1 & ///
			TYPE_VISIT <= 6) | TYPE_VISIT==13) & PREG_START_DATE == . & ///
			TEST_DATE < PREG_END_DATE & TEST_DATE != . & PREG_END_DATE != . 
			
		// If IPC visit & occurred at pregnancy endpoint --> set to ipc timing:
		replace TEST_TIMING = 1 if TEST_TIMING == . & TYPE_VISIT == 6 & ///
			PREG_START_DATE == . & ///
			TEST_DATE == PREG_END_DATE & TEST_DATE != . & PREG_END_DATE != . 
	
		// Timing Unknown (99) if there is no PREG_START_DATE and no PREG_END_DATE 
		// If no PREG_START_DATE and no PREG_END_DATE --> set timing to unknown 
		replace TEST_TIMING = 99 if TEST_TIMING == . & PREG_START_DATE == . & ///
			PREG_END_DATE == . 
			
	*Address tests with missing date: 
	// place in pregnancy if no test date, but ANC visit:
	replace TEST_TIMING = 0 if TEST_TIMING == . & TEST_DATE == . & ///
		((TYPE_VISIT >=1 & TYPE_VISIT <=5) | TYPE_VISIT == 13)
		
	list MOMID TEST_TIMING TEST_GA TEST_DATE TYPE_VISIT PREG_START_DATE PREG_END_DATE ///
		if TEST_TIMING == . 

		tab TEST_TIMING, m 
		
		*labels for timing variable: 
		label define testiming 0 "0 ANC" 1 "1 IPC" 2 "2 PNC" 77 "77 No test" ///
			99 "99 Missing dates"
		label values TEST_TIMING testiming 
		label var TEST_TIMING "Timing of the HB test (0-ANC/ 1-IPC/ 2-PNC)"
	
		*check on observations in late pregnancy; could be misclassified if 
		*there is no pregnancy end date: 
		list MOMID PREGID TEST_DATE TEST_TYPE TEST_GA TYPE_VISIT TIME_HOSP ///
			PREG_END PREG_END_DATE PREG_END_GA if ///
			TEST_TIMING == 0 & (TEST_GA > 280 | (TYPE_VISIT >=7 & TYPE_VISIT <=12))
			

			
	* APPLY ALTITUDE ADJUSTMENTS: 
		// per new WHO guidelines, we will apply the following new adjustments:
	
		*1000–1499m elevation = -0.8 g/dL adjustment 
		*This adjustment is appropriate for: Kenya and Zambia 
	replace HB_LBORRES = HB_LBORRES - 0.8 if site == "Kenya" | site == "Zambia"	
	
	
	* APPLY SMOKING ADJUSTMENTS: 
		// merge in smoking status:
		merge m:1 MOMID PREGID using "$wrk/SMOKE"
		drop if _merge == 2 
		drop _merge 
		
		// adjust for smoking: 
		replace HB_LBORRES = HB_LBORRES - 0.3 if SMOKE == 1 
		

	* Added on 3-10-25: Check for negative values after adjustment: 
	list site HB_LBORRES SMOKE if HB_LBORRES<0
	* Note: These observations presented with an Hb value of <1 prior to 
	* adjustment. This is extremely unlikley/not biologically plausible. We 
	* will set these individual measures to missing "." and remove from the 
	* algorithm for identifying the lowest value in pregnancy and by trimester.
	replace HB_LBORRES =. if HB_LBORRES<0 & (site == "Kenya" | ///
		site == "Zambia" | SMOKE==1)
	
	
	* Save a copy of: All data --> long
	label var TEST_CRF "CRF where Hb test was reported"
	replace TEST_CRF = 77 if TEST_CRF ==.
	
	replace HB_CBC_LBPERF = 0 if HB_CBC_LBPERF==. 
	replace HB_POC_LBPERF = 0 if HB_POC_LBPERF==.
	
	replace HB_POC_PEMETHOD = 99 if HB_POC_PEMETHOD == 77 & HB_POC_LBPERF==1
	
	tab HB_POC_PEMETHOD HB_POC_LBPERF, m 
	
	save "$wrk/ANEMIA_all_long", replace 
	
	preserve 
	
	keep MOMID PREGID SITE TEST_CRF TEST_DATE TEST_TYPE ///
		HB_CBC_LBPERF HB_POC_LBPERF HB_POC_PEMETHOD ///
		HB_LBORRES TYPE_VISIT TIME_HOSP ENTRY_NUM ENTRY_TOTAL TEST_GA ///
		FIXED_TEST_DATE TEST_PP TEST_TIMING PREG_START_DATE PREG_END_DATE
		
	*CHECKS: 
	list if TEST_TIMING==.
	
	replace TEST_TIMING=99 if TEST_TIMING==. 
	
	drop PREG_END_DATE PREG_START_DATE
		
	order MOMID PREGID SITE TEST_CRF TEST_DATE TEST_TYPE ///
		HB_CBC_LBPERF HB_POC_LBPERF HB_POC_PEMETHOD ///
		HB_LBORRES ENTRY_NUM ENTRY_TOTAL TYPE_VISIT TIME_HOSP  ///
		TEST_GA FIXED_TEST_DATE TEST_PP TEST_TIMING

	
	save "$OUT/ANEMIA_all_long", replace 
	
	restore 
	
//////////////////////////////////////////////////////////////////////////
 * * * * TRIMESTER 1 * * * * 
/////////////////////////////////////////////////////////////////////////

	clear 
	use "$wrk/ANEMIA_all_long"

	*CONSTRUCT: first trimester anemia 
	*GA for 1st trimester: Day 0 thruogh Day 97 
	
	*First, restrict to window: must be within T1 AND must be a test during pregnancy 
	keep if TEST_TIMING == 0 & TEST_GA >=0 & TEST_GA <98 
			
	*For reshaping, restrict to needed variables:
	keep MOMID PREGID TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP 
	
	/////
	///// Order the tests within the window: 
		/// order file by person & date 
	sort MOMID PREGID TEST_DATE
	
		/// create indicator for number of entries per person: 
	sort MOMID PREGID TEST_DATE TYPE_VISIT TIME_HOSP
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "HB Test Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of HB tests"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)
	/////
	/////
	
	
	*Next, convert to wide:

	reshape wide TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP ///
		, i(MOMID PREGID) j(ENTRY_NUM) 
		
	*Create indicator for lowest CBC test: 	
	
	gen HB_LOW_CBC_T1 = .
	gen HB_LOW_CBC_T1_DT = .
		format HB_LOW_CBC_T1_DT %td
	gen HB_LOW_CBC_T1_GA = . 
	
	foreach num of numlist 1/$i {
			
		replace HB_LOW_CBC_T1_DT = TEST_DATE`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_T1 == . | (HB_LOW_CBC_T1 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_T1_GA = TEST_GA`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_T1 == . | (HB_LOW_CBC_T1 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_T1 = HB_LBORRES`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_T1 == . | (HB_LOW_CBC_T1 > HB_LBORRES`num'))
	
	}
	
	tab HB_LOW_CBC_T1, m 
	
	list if HB_LOW_CBC_T1 == . 
	
	
	*Create indicator for lowest test of any type: 	
	
	gen HB_LOW_T1 = .
	gen HB_LOW_T1_DT = .
		format HB_LOW_T1_DT %td
	gen HB_LOW_T1_GA = . 
	
	gen HB_LOW_T1_TEST = ""
	
	foreach num of numlist 1/$i {
			
		* overall loop for all tests: 
		replace HB_LOW_T1_DT = TEST_DATE`num' if  ///
			(HB_LOW_T1 == . | (HB_LOW_T1 > HB_LBORRES`num'))
		
		replace HB_LOW_T1_GA = TEST_GA`num' if  ///
			(HB_LOW_T1 == . | (HB_LOW_T1 > HB_LBORRES`num'))
			
		replace HB_LOW_T1_TEST = TEST_TYPE`num' if  ///
			(HB_LOW_T1 == . | (HB_LOW_T1 > HB_LBORRES`num'))
		
		replace HB_LOW_T1 = HB_LBORRES`num' if  ///
			(HB_LOW_T1 == . | (HB_LOW_T1 > HB_LBORRES`num'))
			
	}
	
	tab HB_LOW_T1, m 
	tab HB_LOW_T1_TEST, m 
	
	
	foreach num of numlist 1/$i {
		
		*ensure we default to CBC if the two tests are equal: 
		replace HB_LOW_T1_DT = TEST_DATE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_T1_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_T1 
		
		replace HB_LOW_T1_GA = TEST_GA`num' if  ///
			TEST_TYPE`num' == "CBC" & HB_LOW_T1_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_T1 
			
		replace HB_LOW_T1_TEST = TEST_TYPE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_T1_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_T1 
			
	}
	
	
	
	////////////////////////
	* * * ADDRESS POC TESTS: 
	
	* PREP: Create a count of overall number of POC tests:
	gen POC_COUNT = 0 
	label var POC_COUNT "Number of POC tests per participant"
	
	foreach num of numlist 1/$i {
		
	replace POC_COUNT = POC_COUNT + 1 if TEST_TYPE`num' == "POC"
	
	}
	
	sum POC_COUNT
	return list 
	global z = r(max)
	
	*use max number of POC tests as max number of times to repeat: 
	foreach num of numlist 1/$z {
	
		* * * Step 1: Set date boundaries for any low test that is POC 
		gen POC_UPBOUND = HB_LOW_T1_DT + 7 if HB_LOW_T1_TEST == "POC"
		gen POC_LOWBOUND = HB_LOW_T1_DT - 7 if HB_LOW_T1_TEST == "POC"
		format POC_UPBOUND POC_LOWBOUND %td
		
		* * * Step 2: Run loop to identify any CBC test within the bounds 
		
		gen CBC_CONCURRENT = 0 if HB_LOW_T1_TEST == "POC"
		
		foreach num of numlist 1/$i {
		
		replace CBC_CONCURRENT = 1 if HB_LOW_T1_TEST == "POC" & ///
			TEST_TYPE`num' == "CBC" & ///
			TEST_DATE`num' >= POC_LOWBOUND & TEST_DATE`num' <= POC_UPBOUND
		
		}
		
		*Check the loop:
		list HB_LOW_T1 HB_LOW_T1_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if CBC_CONCURRENT == 1 
			
		* * * Step 3: Drop the low POC test if concurrent CBC test:
		
		foreach num of numlist 1/$i {
		
		replace HB_LBORRES`num' = . if HB_LBORRES`num' == HB_LOW_T1 & ///
			TEST_TYPE`num' == "POC" & CBC_CONCURRENT == 1 & ///
			TEST_DATE`num' == HB_LOW_T1_DT
		
		}
		
		* * * Step 4: Update the loop for those with concurrent CBC
		replace HB_LOW_T1 = . if CBC_CONCURRENT == 1 
		
		foreach num of numlist 1/$i {
				
			replace HB_LOW_T1_DT = TEST_DATE`num' if  ///
				(HB_LOW_T1 == . | (HB_LOW_T1 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_T1_GA = TEST_GA`num' if  ///
				(HB_LOW_T1 == . | (HB_LOW_T1 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
				
			replace HB_LOW_T1_TEST = TEST_TYPE`num' if  ///
				(HB_LOW_T1 == . | (HB_LOW_T1 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_T1 = HB_LBORRES`num' if  ///
				(HB_LOW_T1 == . | (HB_LOW_T1 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
		
		}
		
		tab HB_LOW_T1, m 
		tab HB_LOW_T1_TEST, m 
		
		*Re-check the loops:
		list HB_LOW_T1 HB_LOW_T1_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if CBC_CONCURRENT == 1 
			
		*Review those with a POC test only: 
		list HB_LOW_T1 HB_LOW_T1_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if HB_LOW_T1_TEST == "POC"
			
		drop CBC_CONCURRENT POC_UPBOUND POC_LOWBOUND	
		
	}
		
	* * * * Trimester 1 clean up: restrict dataset:
	
	keep MOMID PREGID ENTRY_TOTAL POC_COUNT HB_LOW_CBC* HB_LOW_T* 
	
	order POC_COUNT, after(ENTRY_TOTAL) 
	
	label var HB_LOW_CBC_T1 "Lowest HB test by CBC - trimester 1"
	label var HB_LOW_CBC_T1_DT "Date of lowest HB test by CBC - trimester 1"
	label var HB_LOW_CBC_T1_GA "GA at lowest HB test by CBC - trimester 1"
	
	label var HB_LOW_T1 "Lowest HB test - trimester 1"
	label var HB_LOW_T1_DT "Date of lowest HB test - trimester 1"
	label var HB_LOW_T1_GA "GA at lowest HB test - trimester 1"
	label var HB_LOW_T1_TEST "Test type for lowest HB measure - trimester 1"
	
	* CONSTRUCT ANEMIA MEASURES HERE: 
	
		*Trimester 1: 
			*>=110 = no anemia
			*100-<110 = mild anemia 
			*70 - <100 = moderate anemia 
			*<70 = severe anemia 
	
	gen ANEMIA_CBC_T1 = 0 if HB_LOW_CBC_T1 >= 11.0 & HB_LOW_CBC_T1 != .
	replace ANEMIA_CBC_T1 = 1 if HB_LOW_CBC_T1 >= 10.0 & HB_LOW_CBC_T1 <11.0
	replace ANEMIA_CBC_T1 = 2 if HB_LOW_CBC_T1 >= 7.0 & HB_LOW_CBC_T1 <10.0
	replace ANEMIA_CBC_T1 = 3 if HB_LOW_CBC_T1 <7.0 & HB_LOW_CBC_T1 != .
	
	label var ANEMIA_CBC_T1 "Most severe anemia status in trimester 1 - CBC only"
	
	gen ANEMIA_CBC_T1_MISS = 0 if ANEMIA_CBC_T1 != . 
	replace ANEMIA_CBC_T1_MISS = 1 if ANEMIA_CBC_T1 == . 
	
	label define anem_miss 0 "0-Non-missing" 1 "1-No valid test result" ///
		2 "2-No labs in window" 3 "3-GA information missing"
		
	label values ANEMIA_CBC_T1_MISS anem_miss 
	label var ANEMIA_CBC_T1_MISS "Missing reason - Anemia T1 (CBC)"
	
	*gen ANEMIA_CBC_T1_H // consult with Xiaoyan before constructing 
	
	gen ANEMIA_T1 = 0 if HB_LOW_T1 >= 11.0 & HB_LOW_T1 != .
	replace ANEMIA_T1 = 1 if HB_LOW_T1 >= 10.0 & HB_LOW_T1 <11.0
	replace ANEMIA_T1 = 2 if HB_LOW_T1 >= 7.0 & HB_LOW_T1 <10.0
	replace ANEMIA_T1 = 3 if HB_LOW_T1 <7.0 & HB_LOW_T1 != .
	
	label var ANEMIA_T1 "Most severe anemia status in trimester 1"
	
	gen ANEMIA_T1_MISS = 0 if ANEMIA_T1 != . 
	replace ANEMIA_T1_MISS = 1 if ANEMIA_T1 == . 
		
	label values ANEMIA_T1_MISS anem_miss 
	label var ANEMIA_T1_MISS "Missing reason - Anemia T1"
	
	*gen ANEMIA_T1_H // consult with Xiaoyan before constructing 
	
	
	save "$wrk/ANEMIA_t1", replace 
			
			
//////////////////////////////////////////////////////////////////////////
 * * * * TRIMESTER 2 * * * * 
/////////////////////////////////////////////////////////////////////////

	clear 
	use "$wrk/ANEMIA_all_long"

	*CONSTRUCT: second trimester anemia 
	*GA for 2nd trimester: Day 98 thruogh Day 195
	
	*First, restrict to window: must be during T2 AND must be a test during pregnancy 
	keep if TEST_TIMING == 0 & TEST_GA >=98 & TEST_GA <=195
		
	*For reshaping, restrict to needed variables:
	keep MOMID PREGID TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP 
	
	/////
	///// Order the tests within the window: 
		/// order file by person & date 
	sort MOMID PREGID TEST_DATE
	
		/// create indicator for number of entries per person: 
	sort MOMID PREGID TEST_DATE TYPE_VISIT TIME_HOSP
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "HB Test Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of HB tests"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)
	/////
	/////
	
	
	*Next, convert to wide:

	reshape wide TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP ///
		, i(MOMID PREGID) j(ENTRY_NUM) 
		
	*Create indicator for lowest CBC test: 	
	
	gen HB_LOW_CBC_T2 = .
	gen HB_LOW_CBC_T2_DT = .
		format HB_LOW_CBC_T2_DT %td
	gen HB_LOW_CBC_T2_GA = . 
	
	foreach num of numlist 1/$i {
			
		replace HB_LOW_CBC_T2_DT = TEST_DATE`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_T2 == . | (HB_LOW_CBC_T2 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_T2_GA = TEST_GA`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_T2 == . | (HB_LOW_CBC_T2 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_T2 = HB_LBORRES`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_T2 == . | (HB_LOW_CBC_T2 > HB_LBORRES`num'))
	
	}
	
	tab HB_LOW_CBC_T2, m 
	
	list if HB_LOW_CBC_T2 == . 
	
	
	*Create indicator for lowest test of any type: 	
	
	gen HB_LOW_T2 = .
	gen HB_LOW_T2_DT = .
		format HB_LOW_T2_DT %td
	gen HB_LOW_T2_GA = . 
	
	gen HB_LOW_T2_TEST = ""
	
	foreach num of numlist 1/$i {
			
		* overall loop for all tests: 
		replace HB_LOW_T2_DT = TEST_DATE`num' if  ///
			(HB_LOW_T2 == . | (HB_LOW_T2 > HB_LBORRES`num'))
		
		replace HB_LOW_T2_GA = TEST_GA`num' if  ///
			(HB_LOW_T2 == . | (HB_LOW_T2 > HB_LBORRES`num'))
			
		replace HB_LOW_T2_TEST = TEST_TYPE`num' if  ///
			(HB_LOW_T2 == . | (HB_LOW_T2 > HB_LBORRES`num'))
		
		replace HB_LOW_T2 = HB_LBORRES`num' if  ///
			(HB_LOW_T2 == . | (HB_LOW_T2 > HB_LBORRES`num'))
			
	}
	
	tab HB_LOW_T2, m 
	tab HB_LOW_T2_TEST, m 
	
	
	foreach num of numlist 1/$i {
		
		*ensure we default to CBC if the two tests are equal: 
		replace HB_LOW_T2_DT = TEST_DATE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_T2_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_T2 
		
		replace HB_LOW_T2_GA = TEST_GA`num' if  ///
			TEST_TYPE`num' == "CBC" & HB_LOW_T2_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_T2 
			
		replace HB_LOW_T2_TEST = TEST_TYPE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_T2_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_T2 
			
	}
	
	
	
	////////////////////////
	* * * ADDRESS POC TESTS: 
	
	* PREP: Create a count of overall number of POC tests:
	gen POC_COUNT = 0 
	label var POC_COUNT "Number of POC tests per participant"
	
	foreach num of numlist 1/$i {
		
	replace POC_COUNT = POC_COUNT + 1 if TEST_TYPE`num' == "POC"
	
	}
	
	sum POC_COUNT
	return list 
	global z = r(max)
	
	*use max number of POC tests as max number of times to repeat: 
	foreach num of numlist 1/$z {
	
		* * * Step 1: Set date boundaries for any low test that is POC 
		gen POC_UPBOUND = HB_LOW_T2_DT + 7 if HB_LOW_T2_TEST == "POC"
		gen POC_LOWBOUND = HB_LOW_T2_DT - 7 if HB_LOW_T2_TEST == "POC"
		format POC_UPBOUND POC_LOWBOUND %td
		
		* * * Step 2: Run loop to identify any CBC test within the bounds 
		
		gen CBC_CONCURRENT = 0 if HB_LOW_T2_TEST == "POC"
		
		foreach num of numlist 1/$i {
		
		replace CBC_CONCURRENT = 1 if HB_LOW_T2_TEST == "POC" & ///
			TEST_TYPE`num' == "CBC" & ///
			TEST_DATE`num' >= POC_LOWBOUND & TEST_DATE`num' <= POC_UPBOUND
		
		}
		
		*Check the loop:
		list HB_LOW_T2 HB_LOW_T2_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if CBC_CONCURRENT == 1 
			
		* * * Step 3: Drop the low POC test if concurrent CBC test:
		
		foreach num of numlist 1/$i {
		
		replace HB_LBORRES`num' = . if HB_LBORRES`num' == HB_LOW_T2 & ///
			TEST_TYPE`num' == "POC" & CBC_CONCURRENT == 1 & ///
			TEST_DATE`num' == HB_LOW_T2_DT
		
		}
		
		* * * Step 4: Update the loop for those with concurrent CBC
		replace HB_LOW_T2 = . if CBC_CONCURRENT == 1 
		
		foreach num of numlist 1/$i {
				
			replace HB_LOW_T2_DT = TEST_DATE`num' if  ///
				(HB_LOW_T2 == . | (HB_LOW_T2 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_T2_GA = TEST_GA`num' if  ///
				(HB_LOW_T2 == . | (HB_LOW_T2 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
				
			replace HB_LOW_T2_TEST = TEST_TYPE`num' if  ///
				(HB_LOW_T2 == . | (HB_LOW_T2 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_T2 = HB_LBORRES`num' if  ///
				(HB_LOW_T2 == . | (HB_LOW_T2 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
		
		}
		
		tab HB_LOW_T2, m 
		tab HB_LOW_T2_TEST, m 
		
		*Re-check the loops:
		list HB_LOW_T2 HB_LOW_T2_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if CBC_CONCURRENT == 1 
			
		*Review those with a POC test only: 
		list HB_LOW_T2 HB_LOW_T2_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if HB_LOW_T2_TEST == "POC"
			
		drop CBC_CONCURRENT POC_UPBOUND POC_LOWBOUND	
		
	}
		
	* * * * Trimester 2 clean up: restrict dataset:
	
	keep MOMID PREGID ENTRY_TOTAL POC_COUNT HB_LOW_CBC* HB_LOW_T* 
	
	order POC_COUNT, after(ENTRY_TOTAL) 
	
	label var HB_LOW_CBC_T2 "Lowest HB test by CBC - trimester 2"
	label var HB_LOW_CBC_T2_DT "Date of lowest HB test by CBC - trimester 2"
	label var HB_LOW_CBC_T2_GA "GA at lowest HB test by CBC - trimester 2"
	
	label var HB_LOW_T2 "Lowest HB test - trimester 2"
	label var HB_LOW_T2_DT "Date of lowest HB test - trimester 2"
	label var HB_LOW_T2_GA "GA at lowest HB test - trimester 2"
	label var HB_LOW_T2_TEST "Test type for lowest HB measure - trimester 2"
	
	* CONSTRUCT ANEMIA MEASURES HERE: 
	
		*Trimester 2: 
			*>=105 = no anemia
			*95-<105 = mild anemia 
			*70 - <95 = moderate anemia 
			*<70 = severe anemia 
	
	gen ANEMIA_CBC_T2 = 0 if HB_LOW_CBC_T2 >= 10.5 & HB_LOW_CBC_T2 != .
	replace ANEMIA_CBC_T2 = 1 if HB_LOW_CBC_T2 >= 9.5 & HB_LOW_CBC_T2 <10.5
	replace ANEMIA_CBC_T2 = 2 if HB_LOW_CBC_T2 >= 7.0 & HB_LOW_CBC_T2 <9.5
	replace ANEMIA_CBC_T2 = 3 if HB_LOW_CBC_T2 <7.0 & HB_LOW_CBC_T2 != .
	
	label var ANEMIA_CBC_T2 "Most severe anemia status in trimester 2 - CBC only"
	
	gen ANEMIA_CBC_T2_MISS = 0 if ANEMIA_CBC_T2 != . 
	replace ANEMIA_CBC_T2_MISS = 1 if ANEMIA_CBC_T2 == . 
	
	label define anem_miss 0 "0-Non-missing" 1 "1-No valid test result" ///
		2 "2-No labs in window" 3 "3-GA information missing"
		
	label values ANEMIA_CBC_T2_MISS anem_miss 
	label var ANEMIA_CBC_T2_MISS "Missing reason - Anemia T2 (CBC)"
	
	*gen ANEMIA_CBC_T2_H // consult with Xiaoyan before constructing 
	
	gen ANEMIA_T2 = 0 if HB_LOW_T2 >= 10.5 & HB_LOW_T2 != .
	replace ANEMIA_T2 = 1 if HB_LOW_T2 >= 9.5 & HB_LOW_T2 <10.5
	replace ANEMIA_T2 = 2 if HB_LOW_T2 >= 7.0 & HB_LOW_T2 <9.5
	replace ANEMIA_T2 = 3 if HB_LOW_T2 <7.0 & HB_LOW_T2 != .
	
	label var ANEMIA_T2 "Most severe anemia status in trimester 2"
	
	gen ANEMIA_T2_MISS = 0 if ANEMIA_T2 != . 
	replace ANEMIA_T2_MISS = 1 if ANEMIA_T2 == . 
		
	label values ANEMIA_T2_MISS anem_miss 
	label var ANEMIA_T2_MISS "Missing reason - Anemia T2"
	
	*gen ANEMIA_T2_H // consult with Xiaoyan before constructing 
	
	
	save "$wrk/ANEMIA_t2", replace 		

			
			
//////////////////////////////////////////////////////////////////////////
 * * * * TRIMESTER 3 * * * * 
/////////////////////////////////////////////////////////////////////////

	clear 
	use "$wrk/ANEMIA_all_long"

	*CONSTRUCT: third trimester anemia 
	*GA for 3rd trimester: Day 196 through pregnancy end
	
	*First, restrict to window: must be during T3 AND must be a test during pregnancy 
	keep if TEST_TIMING == 0 & TEST_GA >=196 & TEST_GA !=.
		
	*For reshaping, restrict to needed variables:
	keep MOMID PREGID TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP 
	
	/////
	///// Order the tests within the window: 
		/// order file by person & date 
	sort MOMID PREGID TEST_DATE
	
		/// create indicator for number of entries per person: 
	sort MOMID PREGID TEST_DATE TYPE_VISIT TIME_HOSP
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "HB Test Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of HB tests"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)
	/////
	/////
	
	
	*Next, convert to wide:

	reshape wide TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP ///
		, i(MOMID PREGID) j(ENTRY_NUM) 
		
	*Create indicator for lowest CBC test: 	
	
	gen HB_LOW_CBC_T3 = .
	gen HB_LOW_CBC_T3_DT = .
		format HB_LOW_CBC_T3_DT %td
	gen HB_LOW_CBC_T3_GA = . 
	
	foreach num of numlist 1/$i {
			
		replace HB_LOW_CBC_T3_DT = TEST_DATE`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_T3 == . | (HB_LOW_CBC_T3 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_T3_GA = TEST_GA`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_T3 == . | (HB_LOW_CBC_T3 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_T3 = HB_LBORRES`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_T3 == . | (HB_LOW_CBC_T3 > HB_LBORRES`num'))
	
	}
	
	tab HB_LOW_CBC_T3, m 
	
	*list if HB_LOW_CBC_T3 == . // many more POC only tests for Trimester 3
	
	
	*Create indicator for lowest test of any type: 	
	
	gen HB_LOW_T3 = .
	gen HB_LOW_T3_DT = .
		format HB_LOW_T3_DT %td
	gen HB_LOW_T3_GA = . 
	
	gen HB_LOW_T3_TEST = ""
	
	foreach num of numlist 1/$i {
			
		* overall loop for all tests: 
		replace HB_LOW_T3_DT = TEST_DATE`num' if  ///
			(HB_LOW_T3 == . | (HB_LOW_T3 > HB_LBORRES`num'))
		
		replace HB_LOW_T3_GA = TEST_GA`num' if  ///
			(HB_LOW_T3 == . | (HB_LOW_T3 > HB_LBORRES`num'))
			
		replace HB_LOW_T3_TEST = TEST_TYPE`num' if  ///
			(HB_LOW_T3 == . | (HB_LOW_T3 > HB_LBORRES`num'))
		
		replace HB_LOW_T3 = HB_LBORRES`num' if  ///
			(HB_LOW_T3 == . | (HB_LOW_T3 > HB_LBORRES`num'))
			
	}
	
	tab HB_LOW_T3, m 
	tab HB_LOW_T3_TEST, m 
	
	
	foreach num of numlist 1/$i {
		
		*ensure we default to CBC if the two tests are equal: 
		replace HB_LOW_T3_DT = TEST_DATE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_T3_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_T3 
		
		replace HB_LOW_T3_GA = TEST_GA`num' if  ///
			TEST_TYPE`num' == "CBC" & HB_LOW_T3_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_T3 
			
		replace HB_LOW_T3_TEST = TEST_TYPE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_T3_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_T3 
			
	}
	
	
	
	////////////////////////
	* * * ADDRESS POC TESTS: 
	
	* PREP: Create a count of overall number of POC tests:
	gen POC_COUNT = 0 
	label var POC_COUNT "Number of POC tests per participant"
	
	foreach num of numlist 1/$i {
		
	replace POC_COUNT = POC_COUNT + 1 if TEST_TYPE`num' == "POC"
	
	}
	
	sum POC_COUNT
	return list 
	global z = r(max)
	
	*use max number of POC tests as max number of times to repeat: 
	foreach num of numlist 1/$z {
	
		* * * Step 1: Set date boundaries for any low test that is POC 
		gen POC_UPBOUND = HB_LOW_T3_DT + 7 if HB_LOW_T3_TEST == "POC"
		gen POC_LOWBOUND = HB_LOW_T3_DT - 7 if HB_LOW_T3_TEST == "POC"
		format POC_UPBOUND POC_LOWBOUND %td
		
		* * * Step 2: Run loop to identify any CBC test within the bounds 
		
		gen CBC_CONCURRENT = 0 if HB_LOW_T3_TEST == "POC"
		
		foreach num of numlist 1/$i {
		
		replace CBC_CONCURRENT = 1 if HB_LOW_T3_TEST == "POC" & ///
			TEST_TYPE`num' == "CBC" & ///
			TEST_DATE`num' >= POC_LOWBOUND & TEST_DATE`num' <= POC_UPBOUND
		
		}
		
		*Check the loop:
		list HB_LOW_T3 HB_LOW_T3_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if CBC_CONCURRENT == 1 
			
		* * * Step 3: Drop the low POC test if concurrent CBC test:
		
		foreach num of numlist 1/$i {
		
		replace HB_LBORRES`num' = . if HB_LBORRES`num' == HB_LOW_T3 & ///
			TEST_TYPE`num' == "POC" & CBC_CONCURRENT == 1 & ///
			TEST_DATE`num' == HB_LOW_T3_DT
		
		}
		
		* * * Step 4: Update the loop for those with concurrent CBC
		replace HB_LOW_T3 = . if CBC_CONCURRENT == 1 
		
		foreach num of numlist 1/$i {
				
			replace HB_LOW_T3_DT = TEST_DATE`num' if  ///
				(HB_LOW_T3 == . | (HB_LOW_T3 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_T3_GA = TEST_GA`num' if  ///
				(HB_LOW_T3 == . | (HB_LOW_T3 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
				
			replace HB_LOW_T3_TEST = TEST_TYPE`num' if  ///
				(HB_LOW_T3 == . | (HB_LOW_T3 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_T3 = HB_LBORRES`num' if  ///
				(HB_LOW_T3 == . | (HB_LOW_T3 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
		
		}
		
		tab HB_LOW_T3, m 
		tab HB_LOW_T3_TEST, m 
		
		*Re-check the loops:
		list HB_LOW_T3 HB_LOW_T3_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if CBC_CONCURRENT == 1 
			
		*Review those with a POC test only: 
		list HB_LOW_T3 HB_LOW_T3_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if HB_LOW_T3_TEST == "POC"
			
		drop CBC_CONCURRENT POC_UPBOUND POC_LOWBOUND	
		
	}
		
	* * * * Trimester 3 clean up: restrict dataset:
	
	keep MOMID PREGID ENTRY_TOTAL POC_COUNT HB_LOW_CBC* HB_LOW_T* 
	
	order POC_COUNT, after(ENTRY_TOTAL) 
	
	label var HB_LOW_CBC_T3 "Lowest HB test by CBC - trimester 3"
	label var HB_LOW_CBC_T3_DT "Date of lowest HB test by CBC - trimester 3"
	label var HB_LOW_CBC_T3_GA "GA at lowest HB test by CBC - trimester 3"
	
	label var HB_LOW_T3 "Lowest HB test - trimester 3"
	label var HB_LOW_T3_DT "Date of lowest HB test - trimester 3"
	label var HB_LOW_T3_GA "GA at lowest HB test - trimester 3"
	label var HB_LOW_T3_TEST "Test type for lowest HB measure - trimester 3"
	
	* CONSTRUCT ANEMIA MEASURES HERE: 
	
		*Trimester 3: 
			*>=110 = no anemia
			*100-<110 = mild anemia 
			*70 - <100 = moderate anemia 
			*<70 = severe anemia 
	
	gen ANEMIA_CBC_T3 = 0 if HB_LOW_CBC_T3 >= 11.0 & HB_LOW_CBC_T3 != .
	replace ANEMIA_CBC_T3 = 1 if HB_LOW_CBC_T3 >= 10.0 & HB_LOW_CBC_T3 <11.0
	replace ANEMIA_CBC_T3 = 2 if HB_LOW_CBC_T3 >= 7.0 & HB_LOW_CBC_T3 <10.0
	replace ANEMIA_CBC_T3 = 3 if HB_LOW_CBC_T3 <7.0 & HB_LOW_CBC_T3 != .
	
	label var ANEMIA_CBC_T3 "Most severe anemia status in trimester 3 - CBC only"
	
	gen ANEMIA_CBC_T3_MISS = 0 if ANEMIA_CBC_T3 != . 
	replace ANEMIA_CBC_T3_MISS = 1 if ANEMIA_CBC_T3 == . 
	
	label define anem_miss 0 "0-Non-missing" 1 "1-No valid test result" ///
		2 "2-No labs in window" 3 "3-GA information missing"
		
	label values ANEMIA_CBC_T3_MISS anem_miss 
	label var ANEMIA_CBC_T3_MISS "Missing reason - Anemia T3 (CBC)"
	
	*gen ANEMIA_CBC_T3_H // consult with Xiaoyan before constructing 
	
	gen ANEMIA_T3 = 0 if HB_LOW_T3 >= 11.0 & HB_LOW_T3 != .
	replace ANEMIA_T3 = 1 if HB_LOW_T3 >= 10.0 & HB_LOW_T3 <11.0
	replace ANEMIA_T3 = 2 if HB_LOW_T3 >= 7.0 & HB_LOW_T3 <10.0
	replace ANEMIA_T3 = 3 if HB_LOW_T3 <7.0 & HB_LOW_T3 != .
	
	label var ANEMIA_T3 "Most severe anemia status in trimester 3"
	
	gen ANEMIA_T3_MISS = 0 if ANEMIA_T3 != . 
	replace ANEMIA_T3_MISS = 1 if ANEMIA_T3 == . 
		
	label values ANEMIA_T3_MISS anem_miss 
	label var ANEMIA_T3_MISS "Missing reason - Anemia T3"
	
	*gen ANEMIA_T3_H // consult with Xiaoyan before constructing 
	
	
	save "$wrk/ANEMIA_t3", replace 				
			
			
//////////////////////////////////////////////////////////////////////////
 * * * * Any Time in Pregnancy * * * * 
/////////////////////////////////////////////////////////////////////////

	/* NOTE THIS VARIABLE IS NO LONGER CONSTRUCTED USING THE CODE BELOW; 
	   INSTEAD WE WILL USE THE WORST ANEMIA STATUS FROM ACROSS THE THREE 
	   TRIMSTERS (RATHER THAN THE WORST HB MEASURE IN PREGNANCY).
	   
	   THIS RESOLVES TWO ISSUES: FIRST, IT CREATES AN ARTIFICIAL BARRIER 
	   BETWEEN TRIMESTERS (ex: a CBC taken at 27+6 weeks won't be able to 
	   trump a POC taken at 28+1 weeks) TO ENSURE CONSISTENCY ACROSS OUTCOMES. 
	   
	   THE SECOND ISSUE RESOLVED IS ENSURING THAT WE ARE SELECTING THE 
	   WORST ANEMIA LEVEL, RATHER THAN THE WORST HB. BECAUSE OF DIFFERENCES 
	   IN DX THRESHOLDS BY TRIMESTER, THE LOWEST HB IS NOT NECESSARILY THE 
	   WORST ANEMIA STATUS (ex: if HB in T2 is 10.5, this person is not 
	   anemic, but if HB for the same person in T3 is 10.6, this person 
	   IS anemic, but the algorithm as originally written would select the 
	   T2 measure of 10.5).
	   
	clear 
	use "$wrk/ANEMIA_all_long"

	*CONSTRUCT: any anemia in pregnancy
	*No GA restrictions; we will keep any ANC measure (TEST_TIMING=0), 
	*although we will exclude IPC measurse (TEST_TIMING=1)
	
	*First, restrict to window: must be a test during pregnancy 
	keep if TEST_TIMING == 0 
		
	*For reshaping, restrict to needed variables:
	keep MOMID PREGID TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP ///
		PREG_START_DATE
	
	/////
	///// Order the tests within the window: 
		/// order file by person & date 
	sort MOMID PREGID TEST_DATE
	
		/// create indicator for number of entries per person: 
	sort MOMID PREGID TEST_DATE TYPE_VISIT TIME_HOSP
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "HB Test Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of HB tests"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)
	/////
	/////
	
	
	*Next, convert to wide:

	reshape wide TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP ///
		, i(MOMID PREGID PREG_START_DATE) j(ENTRY_NUM) 
		
	*Create indicator for lowest CBC test: 	
	
	gen HB_LOW_CBC_ANC = .
	gen HB_LOW_CBC_ANC_DT = .
		format HB_LOW_CBC_ANC_DT %td
	gen HB_LOW_CBC_ANC_GA = . 
	
	foreach num of numlist 1/$i {
			
		replace HB_LOW_CBC_ANC_DT = TEST_DATE`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_ANC == . | (HB_LOW_CBC_ANC > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_ANC_GA = TEST_GA`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_ANC == . | (HB_LOW_CBC_ANC > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_ANC = HB_LBORRES`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_ANC == . | (HB_LOW_CBC_ANC > HB_LBORRES`num'))
	
	}
	
	tab HB_LOW_CBC_ANC, m 
	
	
	*Create indicator for lowest test of any type: 	
	
	gen HB_LOW_ANC = .
	gen HB_LOW_ANC_DT = .
		format HB_LOW_ANC_DT %td
	gen HB_LOW_ANC_GA = . 
	
	gen HB_LOW_ANC_TEST = ""
	
	foreach num of numlist 1/$i {
			
		* overall loop for all tests: 
		replace HB_LOW_ANC_DT = TEST_DATE`num' if  ///
			(HB_LOW_ANC == . | (HB_LOW_ANC > HB_LBORRES`num'))
		
		replace HB_LOW_ANC_GA = TEST_GA`num' if  ///
			(HB_LOW_ANC == . | (HB_LOW_ANC > HB_LBORRES`num'))
			
		replace HB_LOW_ANC_TEST = TEST_TYPE`num' if  ///
			(HB_LOW_ANC == . | (HB_LOW_ANC > HB_LBORRES`num'))
		
		replace HB_LOW_ANC = HB_LBORRES`num' if  ///
			(HB_LOW_ANC == . | (HB_LOW_ANC > HB_LBORRES`num'))
			
	}
	
	tab HB_LOW_ANC, m 
	tab HB_LOW_ANC_TEST, m 
	
	
	foreach num of numlist 1/$i {
		
		*ensure we default to CBC if the two tests are equal: 
		replace HB_LOW_ANC_DT = TEST_DATE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_ANC_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_ANC 
		
		replace HB_LOW_ANC_GA = TEST_GA`num' if  ///
			TEST_TYPE`num' == "CBC" & HB_LOW_ANC_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_ANC 
			
		replace HB_LOW_ANC_TEST = TEST_TYPE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_ANC_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_ANC 
			
	}
	
	
	
	////////////////////////
	* * * ADDRESS POC TESTS: 
	
	* PREP: Create a count of overall number of POC tests:
	gen POC_COUNT = 0 
	label var POC_COUNT "Number of POC tests per participant"
	
	foreach num of numlist 1/$i {
		
	replace POC_COUNT = POC_COUNT + 1 if TEST_TYPE`num' == "POC"
	
	}
	
	sum POC_COUNT
	return list 
	global z = r(max)
	
	*use max number of POC tests as max number of times to repeat: 
	foreach num of numlist 1/$z {
	
		* * * Step 1: Set date boundaries for any low test that is POC 
		gen POC_UPBOUND = HB_LOW_ANC_DT + 7 if HB_LOW_ANC_TEST == "POC"
		gen POC_LOWBOUND = HB_LOW_ANC_DT - 7 if HB_LOW_ANC_TEST == "POC"
		format POC_UPBOUND POC_LOWBOUND %td
		
		* * * Step 2: Run loop to identify any CBC test within the bounds 
		
		gen CBC_CONCURRENT = 0 if HB_LOW_ANC_TEST == "POC"
		
		foreach num of numlist 1/$i {
		
		replace CBC_CONCURRENT = 1 if HB_LOW_ANC_TEST == "POC" & ///
			TEST_TYPE`num' == "CBC" & ///
			TEST_DATE`num' >= POC_LOWBOUND & TEST_DATE`num' <= POC_UPBOUND
		
		}
		
		*Check the loop:
		list HB_LOW_ANC HB_LOW_ANC_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if CBC_CONCURRENT == 1 
			
		* * * Step 3: Drop the low POC test if concurrent CBC test:
		
		foreach num of numlist 1/$i {
		
		replace HB_LBORRES`num' = . if HB_LBORRES`num' == HB_LOW_ANC & ///
			TEST_TYPE`num' == "POC" & CBC_CONCURRENT == 1 & ///
			TEST_DATE`num' == HB_LOW_ANC_DT
		
		}
		
		* * * Step 4: Update the loop for those with concurrent CBC
		replace HB_LOW_ANC = . if CBC_CONCURRENT == 1 
		
		foreach num of numlist 1/$i {
				
			replace HB_LOW_ANC_DT = TEST_DATE`num' if  ///
				(HB_LOW_ANC == . | (HB_LOW_ANC > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_ANC_GA = TEST_GA`num' if  ///
				(HB_LOW_ANC == . | (HB_LOW_ANC > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
				
			replace HB_LOW_ANC_TEST = TEST_TYPE`num' if  ///
				(HB_LOW_ANC == . | (HB_LOW_ANC > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_ANC = HB_LBORRES`num' if  ///
				(HB_LOW_ANC == . | (HB_LOW_ANC > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
		
		}
		
		tab HB_LOW_ANC, m 
		tab HB_LOW_ANC_TEST, m 
		
		*Re-check the loops:
		list HB_LOW_ANC HB_LOW_ANC_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if CBC_CONCURRENT == 1 
			
		*Review those with a POC test only: 
		list HB_LOW_ANC HB_LOW_ANC_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if HB_LOW_ANC_TEST == "POC"
			
		drop CBC_CONCURRENT POC_UPBOUND POC_LOWBOUND	
		
	}
	
	list if HB_LOW_ANC < 7 & HB_LOW_ANC != . & HB_LOW_ANC_TEST == "POC"
		
	* * * * ANC period clean up: restrict dataset:
	
	keep MOMID PREGID ENTRY_TOTAL POC_COUNT HB_LOW_CBC* HB_LOW_A* PREG_START_DATE
	
	order POC_COUNT, after(ENTRY_TOTAL) 
	
	label var HB_LOW_CBC_ANC "Lowest HB test by CBC - all of pregnancy"
	label var HB_LOW_CBC_ANC_DT "Date of lowest HB test by CBC - all of pregnancy"
	label var HB_LOW_CBC_ANC_GA "GA at lowest HB test by CBC - all of pregnancy"
	
	label var HB_LOW_ANC "Lowest HB test - all of pregnancy"
	label var HB_LOW_ANC_DT "Date of lowest HB test - all of pregnancy"
	label var HB_LOW_ANC_GA "GA at lowest HB test - all of pregnancy"
	label var HB_LOW_ANC_TEST "Test type for lowest HB measure - all of pregnancy"
	
	* CONSTRUCT ANEMIA MEASURES HERE: 
	
		*Trimester 1 or 3: 
			*>=110 = no anemia
			*100-<110 = mild anemia 
			*70 - <100 = moderate anemia 
			*<70 = severe anemia 
			
		*Trimester 2: 
			*>=105 = no anemia
			*95-<105 = mild anemia 
			*70 - <95 = moderate anemia 
			*<70 = severe anemia 
			
	gen ANEMIA_CBC_ANC = . 
	
	//// set globals for GA criteria
	global trimester13 ///
	"(HB_LOW_CBC_ANC_GA < 98 | HB_LOW_CBC_ANC_GA >=196) & HB_LOW_CBC_ANC_GA != ."
	global trimester2 ///
	"HB_LOW_CBC_ANC_GA >= 98 & HB_LOW_CBC_ANC_GA <196 & HB_LOW_CBC_ANC_GA != ."
	
	//// miminum HB occurs in trimester 1 or 3: 
	replace ANEMIA_CBC_ANC = 0 if HB_LOW_CBC_ANC >= 11.0 & HB_LOW_CBC_ANC != . & ///
		$trimester13
	replace ANEMIA_CBC_ANC = 1 if HB_LOW_CBC_ANC >= 10.0 & HB_LOW_CBC_ANC <11.0 & ///
		$trimester13
	replace ANEMIA_CBC_ANC = 2 if HB_LOW_CBC_ANC >= 7.0 & HB_LOW_CBC_ANC <10.0 & ///
		$trimester13
	replace ANEMIA_CBC_ANC = 3 if HB_LOW_CBC_ANC <7.0 & HB_LOW_CBC_ANC != . & ///
		$trimester13

	//// miminum HB occurs in trimester 2:  
	replace ANEMIA_CBC_ANC = 0 if HB_LOW_CBC_ANC >= 10.5 & HB_LOW_CBC_ANC != . & ///
		$trimester2
	replace ANEMIA_CBC_ANC = 1 if HB_LOW_CBC_ANC >= 9.5 & HB_LOW_CBC_ANC <10.5 & ///
		$trimester2
	replace ANEMIA_CBC_ANC = 2 if HB_LOW_CBC_ANC >= 7.0 & HB_LOW_CBC_ANC <9.5 & ///
		$trimester2
	replace ANEMIA_CBC_ANC = 3 if HB_LOW_CBC_ANC <7.0 & HB_LOW_CBC_ANC != . & ///
		$trimester2
		
	label var ANEMIA_CBC_ANC "Most severe anemia status in pregnancy (CBC)"
		
	*CHECKS: 
	tab HB_LOW_CBC_ANC ANEMIA_CBC_ANC if HB_LOW_CBC_ANC_GA >= 98 & ///
		HB_LOW_CBC_ANC_GA <196
		
	tab HB_LOW_CBC_ANC ANEMIA_CBC_ANC if ///
		(HB_LOW_CBC_ANC_GA < 98 | HB_LOW_CBC_ANC_GA >=196) & ///
		HB_LOW_CBC_ANC != .
	
	*Missing indicator: 
	gen ANEMIA_CBC_ANC_MISS = 0 if ANEMIA_CBC_ANC != . 
	replace ANEMIA_CBC_ANC_MISS = 1 if ANEMIA_CBC_ANC == . & PREG_START_DATE != . 
	replace ANEMIA_CBC_ANC_MISS = 3 if ANEMIA_CBC_ANC == . & PREG_START_DATE == . 	
	
	label define anem_miss 0 "0-Non-missing" 1 "1-No valid test result" ///
		2 "2-No labs in window" 3 "3-GA information missing"	
	
	label values ANEMIA_CBC_ANC_MISS anem_miss 
	label var ANEMIA_CBC_ANC_MISS "Missing reason - Anemia ANC (CBC)"
	
	*gen ANEMIA_CBC_ANC_H // consult with Xiaoyan before constructing 
	
	
	//// update globals for GA criteria -  all tests (not restricted to CBC)
	global trimester13 ///
	"(HB_LOW_ANC_GA < 98 | HB_LOW_ANC_GA >=196) & HB_LOW_ANC_GA != ."
	global trimester2 ///
	"HB_LOW_ANC_GA >= 98 & HB_LOW_ANC_GA <196 & HB_LOW_ANC_GA != ."
	
	gen ANEMIA_ANC = .
	
	//// miminum HB occurs in trimester 1 or 3: 
	replace ANEMIA_ANC = 0 if HB_LOW_ANC >= 11.0 & HB_LOW_ANC != . & ///
		$trimester13
	replace ANEMIA_ANC = 1 if HB_LOW_ANC >= 10.0 & HB_LOW_ANC <11.0 & ///
		$trimester13
	replace ANEMIA_ANC = 2 if HB_LOW_ANC >= 7.0 & HB_LOW_ANC <10.0 & ///
		$trimester13
	replace ANEMIA_ANC = 3 if HB_LOW_ANC <7.0 & HB_LOW_ANC != . & ///
		$trimester13

	//// miminum HB occurs in trimester 2:  
	replace ANEMIA_ANC = 0 if HB_LOW_ANC >= 10.5 & HB_LOW_ANC != . & ///
		$trimester2
	replace ANEMIA_ANC = 1 if HB_LOW_ANC >= 9.5 & HB_LOW_ANC <10.5 & ///
		$trimester2
	replace ANEMIA_ANC = 2 if HB_LOW_ANC >= 7.0 & HB_LOW_ANC <9.5 & ///
		$trimester2
	replace ANEMIA_ANC = 3 if HB_LOW_ANC <7.0 & HB_LOW_ANC != . & ///
		$trimester2
	
	label var ANEMIA_ANC "Most severe anemia status - all of pregnancy"
	
	gen ANEMIA_ANC_MISS = 0 if ANEMIA_ANC != . 
	replace ANEMIA_ANC_MISS = 1 if ANEMIA_ANC == . & PREG_START_DATE != . 
	replace ANEMIA_ANC_MISS = 3 if ANEMIA_ANC == . & PREG_START_DATE == . 
		
	label values ANEMIA_ANC_MISS anem_miss 
	label var ANEMIA_ANC_MISS "Missing reason - Anemia ANC"
	
	tab ANEMIA_ANC, m 
	tab HB_LOW_ANC_TEST, m 
	
	*gen ANEMIA_ANC_H // consult with Xiaoyan before constructing 
	
	
	save "$wrk/ANEMIA_ANC", replace 		
	
	
	
//////////////////////////////////////////////////////////////////////////
 * * * * Add code for HIGHEST HB measure in pregnancy * * * * 
/////////////////////////////////////////////////////////////////////////

	clear 
	use "$wrk/ANEMIA_all_long"

	*IDENTIFY highest HB measure in pregnancy
	*No GA restrictions; we will keep any ANC measure (TEST_TIMING=0), 
	*although we will exclude IPC measurse (TEST_TIMING=1)
	
	*First, restrict to window: must be a test during pregnancy 
	keep if TEST_TIMING == 0 
		
	*For reshaping, restrict to needed variables:
	keep MOMID PREGID TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP ///
		PREG_START_DATE
	
	/////
	///// Order the tests within the window: 
		/// order file by person & date 
	sort MOMID PREGID TEST_DATE
	
		/// create indicator for number of entries per person: 
	sort MOMID PREGID TEST_DATE TYPE_VISIT TIME_HOSP
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "HB Test Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of HB tests"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)
	/////
	/////
	
	
	*Next, convert to wide:

	reshape wide TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP ///
		, i(MOMID PREGID PREG_START_DATE) j(ENTRY_NUM) 
		
	*Create indicator for HIGHEST CBC test: 	
	
	gen HB_HIGH_CBC_ANC = .
	gen HB_HIGH_CBC_ANC_DT = .
		format HB_HIGH_CBC_ANC_DT %td
	gen HB_HIGH_CBC_ANC_GA = . 
	
	foreach num of numlist 1/$i {
			
		replace HB_HIGH_CBC_ANC_DT = TEST_DATE`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_HIGH_CBC_ANC == . | (HB_HIGH_CBC_ANC < HB_LBORRES`num' & HB_LBORRES`num'!=.))
		
		replace HB_HIGH_CBC_ANC_GA = TEST_GA`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_HIGH_CBC_ANC == . | (HB_HIGH_CBC_ANC < HB_LBORRES`num' & HB_LBORRES`num'!=.))
		
		replace HB_HIGH_CBC_ANC = HB_LBORRES`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_HIGH_CBC_ANC == . | (HB_HIGH_CBC_ANC < HB_LBORRES`num' & HB_LBORRES`num'!=.))
	
	}
	
	tab HB_HIGH_CBC_ANC, m 
	
	
	
	*Create indicator for HIGHEST test of any type: 	
	
	gen HB_HIGH_ANC = .
	gen HB_HIGH_ANC_DT = .
		format HB_HIGH_ANC_DT %td
	gen HB_HIGH_ANC_GA = . 
	
	gen HB_HIGH_ANC_TEST = ""
	
	foreach num of numlist 1/$i {
			
		* overall loop for all tests: 
		replace HB_HIGH_ANC_DT = TEST_DATE`num' if  ///
			(HB_HIGH_ANC == . | (HB_HIGH_ANC < HB_LBORRES`num' & HB_LBORRES`num'!=.))
		
		replace HB_HIGH_ANC_GA = TEST_GA`num' if  ///
			(HB_HIGH_ANC == . | (HB_HIGH_ANC < HB_LBORRES`num' & HB_LBORRES`num'!=.))
			
		replace HB_HIGH_ANC_TEST = TEST_TYPE`num' if  ///
			(HB_HIGH_ANC == . | (HB_HIGH_ANC < HB_LBORRES`num' & HB_LBORRES`num'!=.))
		
		replace HB_HIGH_ANC = HB_LBORRES`num' if  ///
			(HB_HIGH_ANC == . | (HB_HIGH_ANC < HB_LBORRES`num' & HB_LBORRES`num'!=.))
			
	}
	
	tab HB_HIGH_ANC, m 
	tab HB_HIGH_ANC_TEST, m 
	
	
	foreach num of numlist 1/$i {
		
		*ensure we default to CBC if the two tests are equal: 
		replace HB_HIGH_ANC_DT = TEST_DATE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_HIGH_ANC_TEST == "POC" & ///
			HB_LBORRES`num' == HB_HIGH_ANC 
		
		replace HB_HIGH_ANC_GA = TEST_GA`num' if  ///
			TEST_TYPE`num' == "CBC" & HB_HIGH_ANC_TEST == "POC" & ///
			HB_LBORRES`num' == HB_HIGH_ANC 
			
		replace HB_HIGH_ANC_TEST = TEST_TYPE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_HIGH_ANC_TEST == "POC" & ///
			HB_LBORRES`num' == HB_HIGH_ANC 
			
	}
	
	
	
	////////////////////////
	* * * ADDRESS POC TESTS: 
	
	* PREP: Create a count of overall number of POC tests:
	gen POC_COUNT = 0 
	label var POC_COUNT "Number of POC tests per participant"
	
	foreach num of numlist 1/$i {
		
	replace POC_COUNT = POC_COUNT + 1 if TEST_TYPE`num' == "POC"
	
	}
	
	sum POC_COUNT
	return list 
	global z = r(max)
	
	*use max number of POC tests as max number of times to repeat: 
	foreach num of numlist 1/$z {
	
		* * * Step 1: Set date boundaries for any low test that is POC 
		gen POC_UPBOUND = HB_HIGH_ANC_DT + 7 if HB_HIGH_ANC_TEST == "POC"
		gen POC_LOWBOUND = HB_HIGH_ANC_DT - 7 if HB_HIGH_ANC_TEST == "POC"
		format POC_UPBOUND POC_LOWBOUND %td
		
		* * * Step 2: Run loop to identify any CBC test within the bounds 
		
		gen CBC_CONCURRENT = 0 if HB_HIGH_ANC_TEST == "POC"
		
		foreach num of numlist 1/$i {
		
		replace CBC_CONCURRENT = 1 if HB_HIGH_ANC_TEST == "POC" & ///
			TEST_TYPE`num' == "CBC" & ///
			TEST_DATE`num' >= POC_LOWBOUND & TEST_DATE`num' <= POC_UPBOUND
		
		}
		
		*Check the loop:
		list HB_HIGH_ANC HB_HIGH_ANC_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if CBC_CONCURRENT == 1 
			
		* * * Step 3: Drop the high POC test if concurrent CBC test:
		
		foreach num of numlist 1/$i {
		
		replace HB_LBORRES`num' = . if HB_LBORRES`num' == HB_HIGH_ANC & ///
			TEST_TYPE`num' == "POC" & CBC_CONCURRENT == 1 & ///
			TEST_DATE`num' == HB_HIGH_ANC_DT
		
		}
		
		* * * Step 4: Update the loop for those with concurrent CBC
		replace HB_HIGH_ANC = . if CBC_CONCURRENT == 1 
		
		foreach num of numlist 1/$i {
				
			replace HB_HIGH_ANC_DT = TEST_DATE`num' if  ///
				(HB_HIGH_ANC == . | (HB_HIGH_ANC < HB_LBORRES`num' & HB_LBORRES`num'!=.)) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_HIGH_ANC_GA = TEST_GA`num' if  ///
				(HB_HIGH_ANC == . | (HB_HIGH_ANC < HB_LBORRES`num' & HB_LBORRES`num'!=.)) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
				
			replace HB_HIGH_ANC_TEST = TEST_TYPE`num' if  ///
				(HB_HIGH_ANC == . | (HB_HIGH_ANC < HB_LBORRES`num' & HB_LBORRES`num'!=.)) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_HIGH_ANC = HB_LBORRES`num' if  ///
				(HB_HIGH_ANC == . | (HB_HIGH_ANC < HB_LBORRES`num' & HB_LBORRES`num'!=.)) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
		
		}
		
		tab HB_HIGH_ANC, m 
		tab HB_HIGH_ANC_TEST, m 
		
		*Re-check the loops:
		list HB_HIGH_ANC HB_HIGH_ANC_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if CBC_CONCURRENT == 1 
			
		*Review those with a POC test only: 
		list HB_HIGH_ANC HB_HIGH_ANC_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if HB_HIGH_ANC_TEST == "POC"
			
		drop CBC_CONCURRENT POC_UPBOUND POC_LOWBOUND	
		
	}
	
	list if HB_HIGH_ANC > 13 & HB_HIGH_ANC != . & HB_HIGH_ANC_TEST == "POC"
		
	* * * * ANC period clean up: restrict dataset:
	
	keep MOMID PREGID ENTRY_TOTAL POC_COUNT HB_HIGH_CBC* HB_HIGH_A* PREG_START_DATE
	
	order POC_COUNT, after(ENTRY_TOTAL) 
	
	label var HB_HIGH_CBC_ANC "Highest HB test by CBC - all of pregnancy"
	label var HB_HIGH_CBC_ANC_DT "Date of highest HB test by CBC - all of pregnancy"
	label var HB_HIGH_CBC_ANC_GA "GA at highest HB test by CBC - all of pregnancy"
	
	label var HB_HIGH_ANC "Highest HB test - all of pregnancy"
	label var HB_HIGH_ANC_DT "Date of highest HB test - all of pregnancy"
	label var HB_HIGH_ANC_GA "GA at highest HB test - all of pregnancy"
	label var HB_HIGH_ANC_TEST "Test type for lowest HB measure - all of pregnancy"
	
	
	save "$wrk/ANEMIA_ANC_HIGH-HB", replace 
	*/		
			
//////////////////////////////////////////////////////////////////////////
 * * * * ANC-20 visit window * * * * 
/////////////////////////////////////////////////////////////////////////

	clear 
	use "$wrk/ANEMIA_all_long"

	*CONSTRUCT: ANC-20 visit window anemia
	*Window includes  18-22 weeks: Day 126 - Day 160 (assume 18+0 through 22+6)
	
	*First, restrict to window: must be a test during pregnancy AND between 126 & 160 days
	keep if TEST_TIMING == 0 & TEST_GA >= 126 & TEST_GA <= 160
		
	*For reshaping, restrict to needed variables:
	keep MOMID PREGID TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP 
	
	/////
	///// Order the tests within the window: 
		/// order file by person & date 
	sort MOMID PREGID TEST_DATE
	
		/// create indicator for number of entries per person: 
	sort MOMID PREGID TEST_DATE TYPE_VISIT TIME_HOSP
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "HB Test Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of HB tests"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)
	/////
	/////
	
	
	*Next, convert to wide:

	reshape wide TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP ///
		, i(MOMID PREGID) j(ENTRY_NUM) 
		
	*Create indicator for lowest CBC test: 	
	
	gen HB_LOW_CBC_ANC20 = .
	gen HB_LOW_CBC_ANC20_DT = .
		format HB_LOW_CBC_ANC20_DT %td
	gen HB_LOW_CBC_ANC20_GA = . 
	
	foreach num of numlist 1/$i {
			
		replace HB_LOW_CBC_ANC20_DT = TEST_DATE`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_ANC20 == . | (HB_LOW_CBC_ANC20 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_ANC20_GA = TEST_GA`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_ANC20 == . | (HB_LOW_CBC_ANC20 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_ANC20 = HB_LBORRES`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_ANC20 == . | (HB_LOW_CBC_ANC20 > HB_LBORRES`num'))
	
	}
	
	tab HB_LOW_CBC_ANC20, m 
	
	
	*Create indicator for lowest test of any type: 	
	
	gen HB_LOW_ANC20 = .
	gen HB_LOW_ANC20_DT = .
		format HB_LOW_ANC20_DT %td
	gen HB_LOW_ANC20_GA = . 
	
	gen HB_LOW_ANC20_TEST = ""
	
	foreach num of numlist 1/$i {
			
		* overall loop for all tests: 
		replace HB_LOW_ANC20_DT = TEST_DATE`num' if  ///
			(HB_LOW_ANC20 == . | (HB_LOW_ANC20 > HB_LBORRES`num'))
		
		replace HB_LOW_ANC20_GA = TEST_GA`num' if  ///
			(HB_LOW_ANC20 == . | (HB_LOW_ANC20 > HB_LBORRES`num'))
			
		replace HB_LOW_ANC20_TEST = TEST_TYPE`num' if  ///
			(HB_LOW_ANC20 == . | (HB_LOW_ANC20 > HB_LBORRES`num'))
		
		replace HB_LOW_ANC20 = HB_LBORRES`num' if  ///
			(HB_LOW_ANC20 == . | (HB_LOW_ANC20 > HB_LBORRES`num'))
			
	}
	
	tab HB_LOW_ANC20, m 
	tab HB_LOW_ANC20_TEST, m 
	
	
	foreach num of numlist 1/$i {
		
		*ensure we default to CBC if the two tests are equal: 
		replace HB_LOW_ANC20_DT = TEST_DATE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_ANC20_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_ANC20 
		
		replace HB_LOW_ANC20_GA = TEST_GA`num' if  ///
			TEST_TYPE`num' == "CBC" & HB_LOW_ANC20_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_ANC20 
			
		replace HB_LOW_ANC20_TEST = TEST_TYPE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_ANC20_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_ANC20 
			
	}
	
	
	
	////////////////////////
	* * * ADDRESS POC TESTS: 
	
	* PREP: Create a count of overall number of POC tests:
	gen POC_COUNT = 0 
	label var POC_COUNT "Number of POC tests per participant"
	
	foreach num of numlist 1/$i {
		
	replace POC_COUNT = POC_COUNT + 1 if TEST_TYPE`num' == "POC"
	
	}
	
	sum POC_COUNT
	return list 
	global z = r(max)
	
	*use max number of POC tests as max number of times to repeat: 
	foreach num of numlist 1/$z {
	
		* * * Step 1: Set date boundaries for any low test that is POC 
		gen POC_UPBOUND = HB_LOW_ANC20_DT + 7 if HB_LOW_ANC20_TEST == "POC"
		gen POC_LOWBOUND = HB_LOW_ANC20_DT - 7 if HB_LOW_ANC20_TEST == "POC"
		format POC_UPBOUND POC_LOWBOUND %td
		
		* * * Step 2: Run loop to identify any CBC test within the bounds 
		
		gen CBC_CONCURRENT = 0 if HB_LOW_ANC20_TEST == "POC"
		
		foreach num of numlist 1/$i {
		
		replace CBC_CONCURRENT = 1 if HB_LOW_ANC20_TEST == "POC" & ///
			TEST_TYPE`num' == "CBC" & ///
			TEST_DATE`num' >= POC_LOWBOUND & TEST_DATE`num' <= POC_UPBOUND
		
		}
		
		*Check the loop:
		list HB_LOW_ANC20 HB_LOW_ANC20_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if CBC_CONCURRENT == 1 
			
		* * * Step 3: Drop the low POC test if concurrent CBC test:
		
		foreach num of numlist 1/$i {
		
		replace HB_LBORRES`num' = . if HB_LBORRES`num' == HB_LOW_ANC20 & ///
			TEST_TYPE`num' == "POC" & CBC_CONCURRENT == 1 & ///
			TEST_DATE`num' == HB_LOW_ANC20_DT
		
		}
		
		* * * Step 4: Update the loop for those with concurrent CBC
		replace HB_LOW_ANC20 = . if CBC_CONCURRENT == 1 
		
		foreach num of numlist 1/$i {
				
			replace HB_LOW_ANC20_DT = TEST_DATE`num' if  ///
				(HB_LOW_ANC20 == . | (HB_LOW_ANC20 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_ANC20_GA = TEST_GA`num' if  ///
				(HB_LOW_ANC20 == . | (HB_LOW_ANC20 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
				
			replace HB_LOW_ANC20_TEST = TEST_TYPE`num' if  ///
				(HB_LOW_ANC20 == . | (HB_LOW_ANC20 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_ANC20 = HB_LBORRES`num' if  ///
				(HB_LOW_ANC20 == . | (HB_LOW_ANC20 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
		
		}
		
		tab HB_LOW_ANC20, m 
		tab HB_LOW_ANC20_TEST, m 
		
		*Re-check the loops:
		list HB_LOW_ANC20 HB_LOW_ANC20_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if CBC_CONCURRENT == 1 
			
		*Review those with a POC test only: 
		list HB_LOW_ANC20 HB_LOW_ANC20_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if HB_LOW_ANC20_TEST == "POC"
			
		drop CBC_CONCURRENT POC_UPBOUND POC_LOWBOUND	
		
	}
	
	list if HB_LOW_ANC20 < 7 & HB_LOW_ANC20 != . & HB_LOW_ANC20_TEST == "POC"
		
	* * * * ANC20 period clean up: restrict dataset:
	
	keep MOMID PREGID ENTRY_TOTAL POC_COUNT HB_LOW_CBC* HB_LOW_A*
	
	order POC_COUNT, after(ENTRY_TOTAL) 
	
	label var HB_LOW_CBC_ANC20 "Lowest HB test by CBC - ANC-20 Visit Window"
	label var HB_LOW_CBC_ANC20_DT "Date of lowest HB test by CBC - ANC-20 Visit Window"
	label var HB_LOW_CBC_ANC20_GA "GA at lowest HB test by CBC - ANC-20 Visit Window"
	
	label var HB_LOW_ANC20 "Lowest HB test - ANC-20 Visit Window"
	label var HB_LOW_ANC20_DT "Date of lowest HB test - ANC-20 Visit Window"
	label var HB_LOW_ANC20_GA "GA at lowest HB test - ANC-20 Visit Window"
	label var HB_LOW_ANC20_TEST "Test type for lowest HB measure - ANC-20 Visit Window"
	
	* CONSTRUCT ANEMIA MEASURES HERE: 
	
		*Trimester 1 or 3: 
			*>=110 = no anemia
			*100-<110 = mild anemia 
			*70 - <100 = moderate anemia 
			*<70 = severe anemia 
			
		*Trimester 2: 
			*>=105 = no anemia
			*95-<105 = mild anemia 
			*70 - <95 = moderate anemia 
			*<70 = severe anemia 
			
	gen ANEMIA_CBC_ANC20 = . 
	
	//// set globals for GA criteria
	global trimester13 ///
	"(HB_LOW_CBC_ANC20_GA < 98 | HB_LOW_CBC_ANC20_GA >=196) & HB_LOW_CBC_ANC20_GA != ."
	global trimester2 ///
	"HB_LOW_CBC_ANC20_GA >= 98 & HB_LOW_CBC_ANC20_GA <196 & HB_LOW_CBC_ANC20_GA != ."
	
	//// miminum HB occurs in trimester 1 or 3: 
	replace ANEMIA_CBC_ANC20 = 0 if HB_LOW_CBC_ANC20 >= 11.0 & HB_LOW_CBC_ANC20 != . & ///
		$trimester13
	replace ANEMIA_CBC_ANC20 = 1 if HB_LOW_CBC_ANC20 >= 10.0 & HB_LOW_CBC_ANC20 <11.0 & ///
		$trimester13
	replace ANEMIA_CBC_ANC20 = 2 if HB_LOW_CBC_ANC20 >= 7.0 & HB_LOW_CBC_ANC20 <10.0 & ///
		$trimester13
	replace ANEMIA_CBC_ANC20 = 3 if HB_LOW_CBC_ANC20 <7.0 & HB_LOW_CBC_ANC20 != . & ///
		$trimester13

	//// miminum HB occurs in trimester 2:  
	replace ANEMIA_CBC_ANC20 = 0 if HB_LOW_CBC_ANC20 >= 10.5 & HB_LOW_CBC_ANC20 != . & ///
		$trimester2
	replace ANEMIA_CBC_ANC20 = 1 if HB_LOW_CBC_ANC20 >= 9.5 & HB_LOW_CBC_ANC20 <10.5 & ///
		$trimester2
	replace ANEMIA_CBC_ANC20 = 2 if HB_LOW_CBC_ANC20 >= 7.0 & HB_LOW_CBC_ANC20 <9.5 & ///
		$trimester2
	replace ANEMIA_CBC_ANC20 = 3 if HB_LOW_CBC_ANC20 <7.0 & HB_LOW_CBC_ANC20 != . & ///
		$trimester2
		
	label var ANEMIA_CBC_ANC20 "Most severe anemia status at ANC-20 visit window (CBC)"
		
	*CHECKS: 
	tab HB_LOW_CBC_ANC20 ANEMIA_CBC_ANC20 if HB_LOW_CBC_ANC20_GA >= 98 & ///
		HB_LOW_CBC_ANC20_GA <196
		
	tab HB_LOW_CBC_ANC20 ANEMIA_CBC_ANC20 if ///
		(HB_LOW_CBC_ANC20_GA < 98 | HB_LOW_CBC_ANC20_GA >=196) & ///
		HB_LOW_CBC_ANC20 != .
	
	*Missing indicator: 
	gen ANEMIA_CBC_ANC20_MISS = 0 if ANEMIA_CBC_ANC20 != . 
	replace ANEMIA_CBC_ANC20_MISS = 1 if ANEMIA_CBC_ANC20 == .  
	
	label define anem_miss 0 "0-Non-missing" 1 "1-No valid test result" ///
		2 "2-No labs in window" 3 "3-GA information missing"	
	
	label values ANEMIA_CBC_ANC20_MISS anem_miss 
	label var ANEMIA_CBC_ANC20_MISS "Missing reason - Anemia ANC-20 Visit Window (CBC)"
	
	*gen ANEMIA_CBC_ANC20_H // consult with Xiaoyan before constructing 
	
	
	//// update globals for GA criteria -  all tests (not restricted to CBC)
	global trimester13 ///
	"(HB_LOW_ANC20_GA < 98 | HB_LOW_ANC20_GA >=196) & HB_LOW_ANC20_GA != ."
	global trimester2 ///
	"HB_LOW_ANC20_GA >= 98 & HB_LOW_ANC20_GA <196 & HB_LOW_ANC20_GA != ."
	
	gen ANEMIA_ANC20 = .
	
	//// miminum HB occurs in trimester 1 or 3: 
	replace ANEMIA_ANC20 = 0 if HB_LOW_ANC20 >= 11.0 & HB_LOW_ANC20 != . & ///
		$trimester13
	replace ANEMIA_ANC20 = 1 if HB_LOW_ANC20 >= 10.0 & HB_LOW_ANC20 <11.0 & ///
		$trimester13
	replace ANEMIA_ANC20 = 2 if HB_LOW_ANC20 >= 7.0 & HB_LOW_ANC20 <10.0 & ///
		$trimester13
	replace ANEMIA_ANC20 = 3 if HB_LOW_ANC20 <7.0 & HB_LOW_ANC20 != . & ///
		$trimester13

	//// miminum HB occurs in trimester 2:  
	replace ANEMIA_ANC20 = 0 if HB_LOW_ANC20 >= 10.5 & HB_LOW_ANC20 != . & ///
		$trimester2
	replace ANEMIA_ANC20 = 1 if HB_LOW_ANC20 >= 9.5 & HB_LOW_ANC20 <10.5 & ///
		$trimester2
	replace ANEMIA_ANC20 = 2 if HB_LOW_ANC20 >= 7.0 & HB_LOW_ANC20 <9.5 & ///
		$trimester2
	replace ANEMIA_ANC20 = 3 if HB_LOW_ANC20 <7.0 & HB_LOW_ANC20 != . & ///
		$trimester2
	
	label var ANEMIA_ANC20 "Most severe anemia status - ANC-20 Visit Window"
	
	gen ANEMIA_ANC20_MISS = 0 if ANEMIA_ANC20 != . 
	replace ANEMIA_ANC20_MISS = 1 if ANEMIA_ANC20 == . 
		
	label values ANEMIA_ANC20_MISS anem_miss 
	label var ANEMIA_ANC20_MISS "Missing reason - Anemia ANC-20 Visit Window"
	
	tab ANEMIA_ANC20, m 
	tab HB_LOW_ANC20_TEST, m 
	
	*gen ANEMIA_ANC20_H // consult with Xiaoyan before constructing 
	
	
	save "$wrk/ANEMIA_ANC20_window", replace 			
			
			
//////////////////////////////////////////////////////////////////////////
 * * * * ANC-32 visit window * * * * 
/////////////////////////////////////////////////////////////////////////

	clear 
	use "$wrk/ANEMIA_all_long"

	*CONSTRUCT: ANC-32 visit window anemia
	*Window includes  31-33 weeks: Day 217 - Day 237 (assume 31+0 through 33+6)
	
	*First, restrict to window: must be a test during pregnancy AND between 217 & 237 days
	keep if TEST_TIMING == 0 & TEST_GA >= 217 & TEST_GA <= 237
		
	*For reshaping, restrict to needed variables:
	keep MOMID PREGID TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP 
	
	/////
	///// Order the tests within the window: 
		/// order file by person & date 
	sort MOMID PREGID TEST_DATE
	
		/// create indicator for number of entries per person: 
	sort MOMID PREGID TEST_DATE TYPE_VISIT TIME_HOSP
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "HB Test Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of HB tests"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)
	/////
	/////
	
	
	*Next, convert to wide:

	reshape wide TEST_GA TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP ///
		, i(MOMID PREGID) j(ENTRY_NUM) 
		
	*Create indicator for lowest CBC test: 	
	
	gen HB_LOW_CBC_ANC32 = .
	gen HB_LOW_CBC_ANC32_DT = .
		format HB_LOW_CBC_ANC32_DT %td
	gen HB_LOW_CBC_ANC32_GA = . 
	
	foreach num of numlist 1/$i {
			
		replace HB_LOW_CBC_ANC32_DT = TEST_DATE`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_ANC32 == . | (HB_LOW_CBC_ANC32 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_ANC32_GA = TEST_GA`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_ANC32 == . | (HB_LOW_CBC_ANC32 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_ANC32 = HB_LBORRES`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_ANC32 == . | (HB_LOW_CBC_ANC32 > HB_LBORRES`num'))
	
	}
	
	tab HB_LOW_CBC_ANC32, m 
	
	
	*Create indicator for lowest test of any type: 	
	
	gen HB_LOW_ANC32 = .
	gen HB_LOW_ANC32_DT = .
		format HB_LOW_ANC32_DT %td
	gen HB_LOW_ANC32_GA = . 
	
	gen HB_LOW_ANC32_TEST = ""
	
	foreach num of numlist 1/$i {
			
		* overall loop for all tests: 
		replace HB_LOW_ANC32_DT = TEST_DATE`num' if  ///
			(HB_LOW_ANC32 == . | (HB_LOW_ANC32 > HB_LBORRES`num'))
		
		replace HB_LOW_ANC32_GA = TEST_GA`num' if  ///
			(HB_LOW_ANC32 == . | (HB_LOW_ANC32 > HB_LBORRES`num'))
			
		replace HB_LOW_ANC32_TEST = TEST_TYPE`num' if  ///
			(HB_LOW_ANC32 == . | (HB_LOW_ANC32 > HB_LBORRES`num'))
		
		replace HB_LOW_ANC32 = HB_LBORRES`num' if  ///
			(HB_LOW_ANC32 == . | (HB_LOW_ANC32 > HB_LBORRES`num'))
			
	}
	
	tab HB_LOW_ANC32, m 
	tab HB_LOW_ANC32_TEST, m 
	
	
	foreach num of numlist 1/$i {
		
		*ensure we default to CBC if the two tests are equal: 
		replace HB_LOW_ANC32_DT = TEST_DATE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_ANC32_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_ANC32 
		
		replace HB_LOW_ANC32_GA = TEST_GA`num' if  ///
			TEST_TYPE`num' == "CBC" & HB_LOW_ANC32_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_ANC32 
			
		replace HB_LOW_ANC32_TEST = TEST_TYPE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_ANC32_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_ANC32 
			
	}
	
	
	
	////////////////////////
	* * * ADDRESS POC TESTS: 
	
	* PREP: Create a count of overall number of POC tests:
	gen POC_COUNT = 0 
	label var POC_COUNT "Number of POC tests per participant"
	
	foreach num of numlist 1/$i {
		
	replace POC_COUNT = POC_COUNT + 1 if TEST_TYPE`num' == "POC"
	
	}
	
	sum POC_COUNT
	return list 
	global z = r(max)
	
	*use max number of POC tests as max number of times to repeat: 
	foreach num of numlist 1/$z {
	
		* * * Step 1: Set date boundaries for any low test that is POC 
		gen POC_UPBOUND = HB_LOW_ANC32_DT + 7 if HB_LOW_ANC32_TEST == "POC"
		gen POC_LOWBOUND = HB_LOW_ANC32_DT - 7 if HB_LOW_ANC32_TEST == "POC"
		format POC_UPBOUND POC_LOWBOUND %td
		
		* * * Step 2: Run loop to identify any CBC test within the bounds 
		
		gen CBC_CONCURRENT = 0 if HB_LOW_ANC32_TEST == "POC"
		
		foreach num of numlist 1/$i {
		
		replace CBC_CONCURRENT = 1 if HB_LOW_ANC32_TEST == "POC" & ///
			TEST_TYPE`num' == "CBC" & ///
			TEST_DATE`num' >= POC_LOWBOUND & TEST_DATE`num' <= POC_UPBOUND
		
		}
		
		*Check the loop:
		list HB_LOW_ANC32 HB_LOW_ANC32_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if CBC_CONCURRENT == 1 
			
		* * * Step 3: Drop the low POC test if concurrent CBC test:
		
		foreach num of numlist 1/$i {
		
		replace HB_LBORRES`num' = . if HB_LBORRES`num' == HB_LOW_ANC32 & ///
			TEST_TYPE`num' == "POC" & CBC_CONCURRENT == 1 & ///
			TEST_DATE`num' == HB_LOW_ANC32_DT
		
		}
		
		* * * Step 4: Update the loop for those with concurrent CBC
		replace HB_LOW_ANC32 = . if CBC_CONCURRENT == 1 
		
		foreach num of numlist 1/$i {
				
			replace HB_LOW_ANC32_DT = TEST_DATE`num' if  ///
				(HB_LOW_ANC32 == . | (HB_LOW_ANC32 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_ANC32_GA = TEST_GA`num' if  ///
				(HB_LOW_ANC32 == . | (HB_LOW_ANC32 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
				
			replace HB_LOW_ANC32_TEST = TEST_TYPE`num' if  ///
				(HB_LOW_ANC32 == . | (HB_LOW_ANC32 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_ANC32 = HB_LBORRES`num' if  ///
				(HB_LOW_ANC32 == . | (HB_LOW_ANC32 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
		
		}
		
		tab HB_LOW_ANC32, m 
		tab HB_LOW_ANC32_TEST, m 
		
		*Re-check the loops:
		list HB_LOW_ANC32 HB_LOW_ANC32_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if CBC_CONCURRENT == 1 
			
		*Review those with a POC test only: 
		list HB_LOW_ANC32 HB_LOW_ANC32_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 TEST_TYPE3 TEST_DATE3 ///
			HB_LBORRES3 if HB_LOW_ANC32_TEST == "POC"
			
		drop CBC_CONCURRENT POC_UPBOUND POC_LOWBOUND	
		
	}
	
	list if HB_LOW_ANC32 < 7 & HB_LOW_ANC32 != . & HB_LOW_ANC32_TEST == "POC"
		
	* * * * ANC32 period clean up: restrict dataset:
	
	keep MOMID PREGID ENTRY_TOTAL POC_COUNT HB_LOW_CBC* HB_LOW_A*
	
	order POC_COUNT, after(ENTRY_TOTAL) 
	
	label var HB_LOW_CBC_ANC32 "Lowest HB test by CBC - ANC-32 Visit Window"
	label var HB_LOW_CBC_ANC32_DT "Date of lowest HB test by CBC - ANC-32 Visit Window"
	label var HB_LOW_CBC_ANC32_GA "GA at lowest HB test by CBC - ANC-32 Visit Window"
	
	label var HB_LOW_ANC32 "Lowest HB test - ANC-32 Visit Window"
	label var HB_LOW_ANC32_DT "Date of lowest HB test - ANC-32 Visit Window"
	label var HB_LOW_ANC32_GA "GA at lowest HB test - ANC-32 Visit Window"
	label var HB_LOW_ANC32_TEST "Test type for lowest HB measure - ANC-32 Visit Window"
	
	* CONSTRUCT ANEMIA MEASURES HERE: 
	
		*Trimester 1 or 3: 
			*>=110 = no anemia
			*100-<110 = mild anemia 
			*70 - <100 = moderate anemia 
			*<70 = severe anemia 
			
		*Trimester 2: 
			*>=105 = no anemia
			*95-<105 = mild anemia 
			*70 - <95 = moderate anemia 
			*<70 = severe anemia 
			
	gen ANEMIA_CBC_ANC32 = . 
	
	//// set globals for GA criteria
	global trimester13 ///
	"(HB_LOW_CBC_ANC32_GA < 98 | HB_LOW_CBC_ANC32_GA >=196) & HB_LOW_CBC_ANC32_GA != ."
	global trimester2 ///
	"HB_LOW_CBC_ANC32_GA >= 98 & HB_LOW_CBC_ANC32_GA <196 & HB_LOW_CBC_ANC32_GA != ."
	
	//// miminum HB occurs in trimester 1 or 3: 
	replace ANEMIA_CBC_ANC32 = 0 if HB_LOW_CBC_ANC32 >= 11.0 & HB_LOW_CBC_ANC32 != . & ///
		$trimester13
	replace ANEMIA_CBC_ANC32 = 1 if HB_LOW_CBC_ANC32 >= 10.0 & HB_LOW_CBC_ANC32 <11.0 & ///
		$trimester13
	replace ANEMIA_CBC_ANC32 = 2 if HB_LOW_CBC_ANC32 >= 7.0 & HB_LOW_CBC_ANC32 <10.0 & ///
		$trimester13
	replace ANEMIA_CBC_ANC32 = 3 if HB_LOW_CBC_ANC32 <7.0 & HB_LOW_CBC_ANC32 != . & ///
		$trimester13

	//// miminum HB occurs in trimester 2:  
	replace ANEMIA_CBC_ANC32 = 0 if HB_LOW_CBC_ANC32 >= 10.5 & HB_LOW_CBC_ANC32 != . & ///
		$trimester2
	replace ANEMIA_CBC_ANC32 = 1 if HB_LOW_CBC_ANC32 >= 9.5 & HB_LOW_CBC_ANC32 <10.5 & ///
		$trimester2
	replace ANEMIA_CBC_ANC32 = 2 if HB_LOW_CBC_ANC32 >= 7.0 & HB_LOW_CBC_ANC32 <9.5 & ///
		$trimester2
	replace ANEMIA_CBC_ANC32 = 3 if HB_LOW_CBC_ANC32 <7.0 & HB_LOW_CBC_ANC32 != . & ///
		$trimester2
		
	label var ANEMIA_CBC_ANC32 "Most severe anemia status at ANC-32 visit window (CBC)"
		
	*CHECKS: 
	tab HB_LOW_CBC_ANC32 ANEMIA_CBC_ANC32 if HB_LOW_CBC_ANC32_GA >= 98 & ///
		HB_LOW_CBC_ANC32_GA <196
		
	tab HB_LOW_CBC_ANC32 ANEMIA_CBC_ANC32 if ///
		(HB_LOW_CBC_ANC32_GA < 98 | HB_LOW_CBC_ANC32_GA >=196) & ///
		HB_LOW_CBC_ANC32 != .
	
	*Missing indicator: 
	gen ANEMIA_CBC_ANC32_MISS = 0 if ANEMIA_CBC_ANC32 != . 
	replace ANEMIA_CBC_ANC32_MISS = 1 if ANEMIA_CBC_ANC32 == .  
	
	label define anem_miss 0 "0-Non-missing" 1 "1-No valid test result" ///
		2 "2-No labs in window" 3 "3-GA information missing"	
	
	label values ANEMIA_CBC_ANC32_MISS anem_miss 
	label var ANEMIA_CBC_ANC32_MISS "Missing reason - Anemia ANC-32 Visit Window (CBC)"
	
	*gen ANEMIA_CBC_ANC32_H // consult with Xiaoyan before constructing 
	
	
	//// update globals for GA criteria -  all tests (not restricted to CBC)
	global trimester13 ///
	"(HB_LOW_ANC32_GA < 98 | HB_LOW_ANC32_GA >=196) & HB_LOW_ANC32_GA != ."
	global trimester2 ///
	"HB_LOW_ANC32_GA >= 98 & HB_LOW_ANC32_GA <196 & HB_LOW_ANC32_GA != ."
	
	gen ANEMIA_ANC32 = .
	
	//// miminum HB occurs in trimester 1 or 3: 
	replace ANEMIA_ANC32 = 0 if HB_LOW_ANC32 >= 11.0 & HB_LOW_ANC32 != . & ///
		$trimester13
	replace ANEMIA_ANC32 = 1 if HB_LOW_ANC32 >= 10.0 & HB_LOW_ANC32 <11.0 & ///
		$trimester13
	replace ANEMIA_ANC32 = 2 if HB_LOW_ANC32 >= 7.0 & HB_LOW_ANC32 <10.0 & ///
		$trimester13
	replace ANEMIA_ANC32 = 3 if HB_LOW_ANC32 <7.0 & HB_LOW_ANC32 != . & ///
		$trimester13

	//// miminum HB occurs in trimester 2:  
	replace ANEMIA_ANC32 = 0 if HB_LOW_ANC32 >= 10.5 & HB_LOW_ANC32 != . & ///
		$trimester2
	replace ANEMIA_ANC32 = 1 if HB_LOW_ANC32 >= 9.5 & HB_LOW_ANC32 <10.5 & ///
		$trimester2
	replace ANEMIA_ANC32 = 2 if HB_LOW_ANC32 >= 7.0 & HB_LOW_ANC32 <9.5 & ///
		$trimester2
	replace ANEMIA_ANC32 = 3 if HB_LOW_ANC32 <7.0 & HB_LOW_ANC32 != . & ///
		$trimester2
	
	label var ANEMIA_ANC32 "Most severe anemia status - ANC-32 Visit Window"
	
	gen ANEMIA_ANC32_MISS = 0 if ANEMIA_ANC32 != . 
	replace ANEMIA_ANC32_MISS = 1 if ANEMIA_ANC32 == . 
		
	label values ANEMIA_ANC32_MISS anem_miss 
	label var ANEMIA_ANC32_MISS "Missing reason - Anemia ANC-32 Visit Window"
	
	tab ANEMIA_ANC32, m 
	tab HB_LOW_ANC32_TEST, m 
	
	*gen ANEMIA_ANC32_H // consult with Xiaoyan before constructing 
	
	
	save "$wrk/ANEMIA_ANC32_window", replace 		
	

//////////////////////////////////////////////////////////////////////////
 * * * * PNC-6 visit window * * * * 
/////////////////////////////////////////////////////////////////////////

	clear 
	use "$wrk/ANEMIA_all_long"

	*CONSTRUCT: PNC-6 visit window anemia     
	*Window includes  6-14 weeks postpartum: Day 42 - Day 104 postpartum
	
	
	*First, restrict to window: must be a test after pregnancy, 
	*pregnancy must be completed (PREG_END) AND between 42 and 140 days PP: 
	keep if TEST_TIMING == 2 & TEST_PP >= 42 & TEST_PP <=140 & PREG_END == 1 
		
	*For reshaping, restrict to needed variables:
	keep MOMID PREGID TEST_PP PREG_END PREG_END_DATE PREG_END_GA ///
		TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP 

	
	/////
	///// Order the tests within the window: 
		/// order file by person & date 
	sort MOMID PREGID TEST_DATE
	
		/// create indicator for number of entries per person: 
	sort MOMID PREGID TEST_DATE TYPE_VISIT TIME_HOSP
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "HB Test Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of HB tests"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)
	/////
	/////
	
	
	*Next, convert to wide:

	reshape wide TEST_PP TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP ///
		, i(MOMID PREGID PREG_END* ) j(ENTRY_NUM) 
		
	*Create indicator for lowest CBC test: 	
	
	gen HB_LOW_CBC_PNC6 = .
	gen HB_LOW_CBC_PNC6_DT = .
		format HB_LOW_CBC_PNC6_DT %td
	gen HB_LOW_CBC_PNC6_PP = . 
	
	foreach num of numlist 1/$i {
			
		replace HB_LOW_CBC_PNC6_DT = TEST_DATE`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_PNC6 == . | (HB_LOW_CBC_PNC6 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_PNC6_PP = TEST_PP`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_PNC6 == . | (HB_LOW_CBC_PNC6 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_PNC6 = HB_LBORRES`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_PNC6 == . | (HB_LOW_CBC_PNC6 > HB_LBORRES`num'))
	
	}
	
	tab HB_LOW_CBC_PNC6, m 
	
	
	*Create indicator for lowest test of any type: 	
	
	gen HB_LOW_PNC6 = .
	gen HB_LOW_PNC6_DT = .
		format HB_LOW_PNC6_DT %td
	gen HB_LOW_PNC6_PP = . 
	
	gen HB_LOW_PNC6_TEST = ""
	
	foreach num of numlist 1/$i {
			
		* overall loop for all tests: 
		replace HB_LOW_PNC6_DT = TEST_DATE`num' if  ///
			(HB_LOW_PNC6 == . | (HB_LOW_PNC6 > HB_LBORRES`num'))
		
		replace HB_LOW_PNC6_PP = TEST_PP`num' if  ///
			(HB_LOW_PNC6 == . | (HB_LOW_PNC6 > HB_LBORRES`num'))
			
		replace HB_LOW_PNC6_TEST = TEST_TYPE`num' if  ///
			(HB_LOW_PNC6 == . | (HB_LOW_PNC6 > HB_LBORRES`num'))
		
		replace HB_LOW_PNC6 = HB_LBORRES`num' if  ///
			(HB_LOW_PNC6 == . | (HB_LOW_PNC6 > HB_LBORRES`num'))
			
	}
	
	tab HB_LOW_PNC6, m 
	tab HB_LOW_PNC6_TEST, m 
	
	
	foreach num of numlist 1/$i {
		
		*ensure we default to CBC if the two tests are equal: 
		replace HB_LOW_PNC6_DT = TEST_DATE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_PNC6_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_PNC6 
		
		replace HB_LOW_PNC6_PP = TEST_PP`num' if  ///
			TEST_TYPE`num' == "CBC" & HB_LOW_PNC6_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_PNC6 
			
		replace HB_LOW_PNC6_TEST = TEST_TYPE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_PNC6_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_PNC6 
			
	}
	
	
	
	////////////////////////
	* * * ADDRESS POC TESTS: 
	
	* PREP: Create a count of overall number of POC tests:
	gen POC_COUNT = 0 
	label var POC_COUNT "Number of POC tests per participant"
	
	foreach num of numlist 1/$i {
		
	replace POC_COUNT = POC_COUNT + 1 if TEST_TYPE`num' == "POC"
	
	}
	
	sum POC_COUNT
	return list 
	global z = r(max)
	
	*use max number of POC tests as max number of times to repeat: 
	foreach num of numlist 1/$z {
	
		* * * Step 1: Set date boundaries for any low test that is POC 
		gen POC_UPBOUND = HB_LOW_PNC6_DT + 7 if HB_LOW_PNC6_TEST == "POC"
		gen POC_LOWBOUND = HB_LOW_PNC6_DT - 7 if HB_LOW_PNC6_TEST == "POC"
		format POC_UPBOUND POC_LOWBOUND %td
		
		* * * Step 2: Run loop to identify any CBC test within the bounds 
		
		gen CBC_CONCURRENT = 0 if HB_LOW_PNC6_TEST == "POC"
		
		foreach num of numlist 1/$i {
		
		replace CBC_CONCURRENT = 1 if HB_LOW_PNC6_TEST == "POC" & ///
			TEST_TYPE`num' == "CBC" & ///
			TEST_DATE`num' >= POC_LOWBOUND & TEST_DATE`num' <= POC_UPBOUND
		
		}
		
		*Check the loop:
		list HB_LOW_PNC6 HB_LOW_PNC6_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if CBC_CONCURRENT == 1 
			
		* * * Step 3: Drop the low POC test if concurrent CBC test:
		
		foreach num of numlist 1/$i {
		
		replace HB_LBORRES`num' = . if HB_LBORRES`num' == HB_LOW_PNC6 & ///
			TEST_TYPE`num' == "POC" & CBC_CONCURRENT == 1 & ///
			TEST_DATE`num' == HB_LOW_PNC6_DT
		
		}
		
		* * * Step 4: Update the loop for those with concurrent CBC
		replace HB_LOW_PNC6 = . if CBC_CONCURRENT == 1 
		
		foreach num of numlist 1/$i {
				
			replace HB_LOW_PNC6_DT = TEST_DATE`num' if  ///
				(HB_LOW_PNC6 == . | (HB_LOW_PNC6 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_PNC6_PP = TEST_PP`num' if  ///
				(HB_LOW_PNC6 == . | (HB_LOW_PNC6 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
				
			replace HB_LOW_PNC6_TEST = TEST_TYPE`num' if  ///
				(HB_LOW_PNC6 == . | (HB_LOW_PNC6 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_PNC6 = HB_LBORRES`num' if  ///
				(HB_LOW_PNC6 == . | (HB_LOW_PNC6 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
		
		}
		
		tab HB_LOW_PNC6, m 
		tab HB_LOW_PNC6_TEST, m 
		
		*Re-check the loops:
		list HB_LOW_PNC6 HB_LOW_PNC6_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if CBC_CONCURRENT == 1 
			
		*Review those with a POC test only: 
		list HB_LOW_PNC6 HB_LOW_PNC6_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if HB_LOW_PNC6_TEST == "POC"
			
		drop CBC_CONCURRENT POC_UPBOUND POC_LOWBOUND	
		
	}
	
	list if HB_LOW_PNC6 < 7 & HB_LOW_PNC6 != . & HB_LOW_PNC6_TEST == "POC"
		
	* * * * PNC6 period clean up: restrict dataset:
	
	keep MOMID PREGID ENTRY_TOTAL POC_COUNT HB_LOW_CBC* HB_LOW_P*
	
	order POC_COUNT, after(ENTRY_TOTAL) 
	
	label var HB_LOW_CBC_PNC6 "Lowest HB test by CBC - PNC-6 Visit Window"
	label var HB_LOW_CBC_PNC6_DT "Date of lowest HB test by CBC - PNC-6 Visit Window"
	label var HB_LOW_CBC_PNC6_PP "Days postpartum at lowest HB test by CBC - PNC-6 Visit Window"
	
	label var HB_LOW_PNC6 "Lowest HB test - PNC-6 Visit Window"
	label var HB_LOW_PNC6_DT "Date of lowest HB test - PNC-6 Visit Window"
	label var HB_LOW_PNC6_PP "Days postpartum at lowest HB test - PNC-6 Visit Window"
	label var HB_LOW_PNC6_TEST "Test type for lowest HB measure - PNC-6 Visit Window"
	
	* CONSTRUCT ANEMIA MEASURES HERE: 
	
		*Postpartum
			*>=120 = no anemia
			*110-<120 = mild anemia 
			*80 - <110 = moderate anemia 
			*<80 = severe anemia 
			
	gen ANEMIA_CBC_PNC6 = . 
	
	//// miminum HB postpartum anemia status criteria: 
	replace ANEMIA_CBC_PNC6 = 0 if HB_LOW_CBC_PNC6 >= 12.0 & HB_LOW_CBC_PNC6 != . 
	replace ANEMIA_CBC_PNC6 = 1 if HB_LOW_CBC_PNC6 >= 11.0 & HB_LOW_CBC_PNC6 <12.0
	replace ANEMIA_CBC_PNC6 = 2 if HB_LOW_CBC_PNC6 >= 8.0 & HB_LOW_CBC_PNC6 <11.0
	replace ANEMIA_CBC_PNC6 = 3 if HB_LOW_CBC_PNC6 <8.0 & HB_LOW_CBC_PNC6 != .
		
	label var ANEMIA_CBC_PNC6 "Most severe anemia status at PNC-6 visit window (CBC)"
		
	*Missing indicator: 
	gen ANEMIA_CBC_PNC6_MISS = 0 if ANEMIA_CBC_PNC6 != . 
	replace ANEMIA_CBC_PNC6_MISS = 1 if ANEMIA_CBC_PNC6 == .  
	
	label define anem_miss 0 "0-Non-missing" 1 "1-No valid test result" ///
		2 "2-No labs in window" 3 "3-Days postpartum information missing"	
	
	label values ANEMIA_CBC_PNC6_MISS anem_miss 
	label var ANEMIA_CBC_PNC6_MISS "Missing reason - Anemia PNC-6 Visit Window (CBC)"
	
	*gen ANEMIA_CBC_PNC6_H // consult with Xiaoyan before constructing 
		
	gen ANEMIA_PNC6 = .
	
	//// miminum HB postpartum
	replace ANEMIA_PNC6 = 0 if HB_LOW_PNC6 >= 12.0 & HB_LOW_PNC6 != . 
	replace ANEMIA_PNC6 = 1 if HB_LOW_PNC6 >= 11.0 & HB_LOW_PNC6 <12.0 
	replace ANEMIA_PNC6 = 2 if HB_LOW_PNC6 >= 8.0 & HB_LOW_PNC6 <11.0 
	replace ANEMIA_PNC6 = 3 if HB_LOW_PNC6 <8.0 & HB_LOW_PNC6 != . 


	label var ANEMIA_PNC6 "Most severe anemia status - PNC-6 Visit Window"
	
	gen ANEMIA_PNC6_MISS = 0 if ANEMIA_PNC6 != . 
	replace ANEMIA_PNC6_MISS = 1 if ANEMIA_PNC6 == . 
		
	label values ANEMIA_PNC6_MISS anem_miss 
	label var ANEMIA_PNC6_MISS "Missing reason - Anemia PNC-6 Visit Window"
	
	tab ANEMIA_PNC6, m 
	tab HB_LOW_PNC6_TEST, m 
	
	*gen ANEMIA_PNC6_H // consult with Xiaoyan before constructing 
	
	
	save "$wrk/ANEMIA_PNC6_window", replace 		
	
	
//////////////////////////////////////////////////////////////////////////
 * * * * PNC-26 visit window * * * * 
/////////////////////////////////////////////////////////////////////////

	clear 
	use "$wrk/ANEMIA_all_long"

	*CONSTRUCT: PNC-26 visit window anemia     
	*Window includes  26-39 weeks postpartum: Day 182 - Day 279 postpartum
	
	
	*First, restrict to window: must be a test after pregnancy, 
	*pregnancy must be completed (PREG_END) AND between 182 and 279 days PP: 
	keep if TEST_TIMING == 2 & TEST_PP >= 182 & TEST_PP <=279 & PREG_END == 1 
		
	*For reshaping, restrict to needed variables:
	keep MOMID PREGID TEST_PP PREG_END PREG_END_DATE PREG_END_GA ///
		TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP 

	
	/////
	///// Order the tests within the window: 
		/// order file by person & date 
	sort MOMID PREGID TEST_DATE
	
		/// create indicator for number of entries per person: 
	sort MOMID PREGID TEST_DATE TYPE_VISIT TIME_HOSP
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "HB Test Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of HB tests"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)
	/////
	/////
	
	
	*Next, convert to wide:

	reshape wide TEST_PP TEST_DATE TEST_TYPE HB_LBORRES TYPE_VISIT TIME_HOSP ///
		, i(MOMID PREGID PREG_END* ) j(ENTRY_NUM) 
		
	*Create indicator for lowest CBC test: 	
	
	gen HB_LOW_CBC_PNC26 = .
	gen HB_LOW_CBC_PNC26_DT = .
		format HB_LOW_CBC_PNC26_DT %td
	gen HB_LOW_CBC_PNC26_PP = . 
	
	foreach num of numlist 1/$i {
			
		replace HB_LOW_CBC_PNC26_DT = TEST_DATE`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_PNC26 == . | (HB_LOW_CBC_PNC26 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_PNC26_PP = TEST_PP`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_PNC26 == . | (HB_LOW_CBC_PNC26 > HB_LBORRES`num'))
		
		replace HB_LOW_CBC_PNC26 = HB_LBORRES`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_LOW_CBC_PNC26 == . | (HB_LOW_CBC_PNC26 > HB_LBORRES`num'))
	
	}
	
	tab HB_LOW_CBC_PNC26, m 
	
	
	*Create indicator for lowest test of any type: 	
	
	gen HB_LOW_PNC26 = .
	gen HB_LOW_PNC26_DT = .
		format HB_LOW_PNC26_DT %td
	gen HB_LOW_PNC26_PP = . 
	
	gen HB_LOW_PNC26_TEST = ""
	
	foreach num of numlist 1/$i {
			
		* overall loop for all tests: 
		replace HB_LOW_PNC26_DT = TEST_DATE`num' if  ///
			(HB_LOW_PNC26 == . | (HB_LOW_PNC26 > HB_LBORRES`num'))
		
		replace HB_LOW_PNC26_PP = TEST_PP`num' if  ///
			(HB_LOW_PNC26 == . | (HB_LOW_PNC26 > HB_LBORRES`num'))
			
		replace HB_LOW_PNC26_TEST = TEST_TYPE`num' if  ///
			(HB_LOW_PNC26 == . | (HB_LOW_PNC26 > HB_LBORRES`num'))
		
		replace HB_LOW_PNC26 = HB_LBORRES`num' if  ///
			(HB_LOW_PNC26 == . | (HB_LOW_PNC26 > HB_LBORRES`num'))
			
	}
	
	tab HB_LOW_PNC26, m 
	tab HB_LOW_PNC26_TEST, m 
	
	
	foreach num of numlist 1/$i {
		
		*ensure we default to CBC if the two tests are equal: 
		replace HB_LOW_PNC26_DT = TEST_DATE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_PNC26_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_PNC26 
		
		replace HB_LOW_PNC26_PP = TEST_PP`num' if  ///
			TEST_TYPE`num' == "CBC" & HB_LOW_PNC26_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_PNC26 
			
		replace HB_LOW_PNC26_TEST = TEST_TYPE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_LOW_PNC26_TEST == "POC" & ///
			HB_LBORRES`num' == HB_LOW_PNC26 
			
	}
	
	
	
	////////////////////////
	* * * ADDRESS POC TESTS: 
	
	* PREP: Create a count of overall number of POC tests:
	gen POC_COUNT = 0 
	label var POC_COUNT "Number of POC tests per participant"
	
	foreach num of numlist 1/$i {
		
	replace POC_COUNT = POC_COUNT + 1 if TEST_TYPE`num' == "POC"
	
	}
	
	sum POC_COUNT
	return list 
	global z = r(max)
	
	*use max number of POC tests as max number of times to repeat: 
	foreach num of numlist 1/$z {
	
		* * * Step 1: Set date boundaries for any low test that is POC 
		gen POC_UPBOUND = HB_LOW_PNC26_DT + 7 if HB_LOW_PNC26_TEST == "POC"
		gen POC_LOWBOUND = HB_LOW_PNC26_DT - 7 if HB_LOW_PNC26_TEST == "POC"
		format POC_UPBOUND POC_LOWBOUND %td
		
		* * * Step 2: Run loop to identify any CBC test within the bounds 
		
		gen CBC_CONCURRENT = 0 if HB_LOW_PNC26_TEST == "POC"
		
		foreach num of numlist 1/$i {
		
		replace CBC_CONCURRENT = 1 if HB_LOW_PNC26_TEST == "POC" & ///
			TEST_TYPE`num' == "CBC" & ///
			TEST_DATE`num' >= POC_LOWBOUND & TEST_DATE`num' <= POC_UPBOUND
		
		}
		
		/*Check the loop:
		list HB_LOW_PNC26 HB_LOW_PNC26_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 if CBC_CONCURRENT == 1 
		*/
			
		* * * Step 3: Drop the low POC test if concurrent CBC test:
		
		foreach num of numlist 1/$i {
		
		replace HB_LBORRES`num' = . if HB_LBORRES`num' == HB_LOW_PNC26 & ///
			TEST_TYPE`num' == "POC" & CBC_CONCURRENT == 1 & ///
			TEST_DATE`num' == HB_LOW_PNC26_DT
		
		}
		
		* * * Step 4: Update the loop for those with concurrent CBC
		replace HB_LOW_PNC26 = . if CBC_CONCURRENT == 1 
		
		foreach num of numlist 1/$i {
				
			replace HB_LOW_PNC26_DT = TEST_DATE`num' if  ///
				(HB_LOW_PNC26 == . | (HB_LOW_PNC26 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_PNC26_PP = TEST_PP`num' if  ///
				(HB_LOW_PNC26 == . | (HB_LOW_PNC26 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
				
			replace HB_LOW_PNC26_TEST = TEST_TYPE`num' if  ///
				(HB_LOW_PNC26 == . | (HB_LOW_PNC26 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_LOW_PNC26 = HB_LBORRES`num' if  ///
				(HB_LOW_PNC26 == . | (HB_LOW_PNC26 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
		
		}
		
		tab HB_LOW_PNC26, m 
		tab HB_LOW_PNC26_TEST, m 
		
		/*Re-check the loops:
		list HB_LOW_PNC26 HB_LOW_PNC26_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 if CBC_CONCURRENT == 1 
			
		*Review those with a POC test only: 
		list HB_LOW_PNC26 HB_LOW_PNC26_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 if HB_LOW_PNC26_TEST == "POC"
		*/	
		drop CBC_CONCURRENT POC_UPBOUND POC_LOWBOUND	
		
	}
	
	list if HB_LOW_PNC26 < 7 & HB_LOW_PNC26 != . & HB_LOW_PNC26_TEST == "POC"
		
	* * * * PNC26 period clean up: restrict dataset:
	
	keep MOMID PREGID ENTRY_TOTAL POC_COUNT HB_LOW_CBC* HB_LOW_P*
	
	order POC_COUNT, after(ENTRY_TOTAL) 
	
	label var HB_LOW_CBC_PNC26 "Lowest HB test by CBC - PNC-26 Visit Window"
	label var HB_LOW_CBC_PNC26_DT "Date of lowest HB test by CBC - PNC-26 Visit Window"
	label var HB_LOW_CBC_PNC26_PP "Days postpartum at lowest HB test by CBC - PNC-26 Visit Window"
	
	label var HB_LOW_PNC26 "Lowest HB test - PNC-26 Visit Window"
	label var HB_LOW_PNC26_DT "Date of lowest HB test - PNC-26 Visit Window"
	label var HB_LOW_PNC26_PP "Days postpartum at lowest HB test - PNC-26 Visit Window"
	label var HB_LOW_PNC26_TEST "Test type for lowest HB measure - PNC-26 Visit Window"
	
	* CONSTRUCT ANEMIA MEASURES HERE: 
	
		*Postpartum
			*>=120 = no anemia
			*110-<120 = mild anemia 
			*80 - <110 = moderate anemia 
			*<80 = severe anemia 

			
	gen ANEMIA_CBC_PNC26 = . 
	
	//// miminum HB postpartum anemia status criteria: 
	replace ANEMIA_CBC_PNC26 = 0 if HB_LOW_CBC_PNC26 >= 12.0 & HB_LOW_CBC_PNC26 != . 
	replace ANEMIA_CBC_PNC26 = 1 if HB_LOW_CBC_PNC26 >= 11.0 & HB_LOW_CBC_PNC26 <12.0
	replace ANEMIA_CBC_PNC26 = 2 if HB_LOW_CBC_PNC26 >= 8.0 & HB_LOW_CBC_PNC26 <11.0
	replace ANEMIA_CBC_PNC26 = 3 if HB_LOW_CBC_PNC26 <8.0 & HB_LOW_CBC_PNC26 != .
		
	label var ANEMIA_CBC_PNC26 "Most severe anemia status at PNC-26 visit window (CBC)"
		
	*Missing indicator: 
	gen ANEMIA_CBC_PNC26_MISS = 0 if ANEMIA_CBC_PNC26 != . 
	replace ANEMIA_CBC_PNC26_MISS = 1 if ANEMIA_CBC_PNC26 == .  
	
	label define anem_miss 0 "0-Non-missing" 1 "1-No valid test result" ///
		2 "2-No labs in window" 3 "3-Days postpartum information missing"	
	
	label values ANEMIA_CBC_PNC26_MISS anem_miss 
	label var ANEMIA_CBC_PNC26_MISS "Missing reason - Anemia PNC-26 Visit Window (CBC)"
	
	*gen ANEMIA_CBC_PNC26_H // consult with Xiaoyan before constructing 
		
	gen ANEMIA_PNC26 = .
	
	//// miminum HB postpartum
	replace ANEMIA_PNC26 = 0 if HB_LOW_PNC26 >= 12.0 & HB_LOW_PNC26 != . 
	replace ANEMIA_PNC26 = 1 if HB_LOW_PNC26 >= 11.0 & HB_LOW_PNC26 <12.0 
	replace ANEMIA_PNC26 = 2 if HB_LOW_PNC26 >= 8.0 & HB_LOW_PNC26 <11.0 
	replace ANEMIA_PNC26 = 3 if HB_LOW_PNC26 <8.0 & HB_LOW_PNC26 != . 


	label var ANEMIA_PNC26 "Most severe anemia status - PNC-26 Visit Window"
	
	gen ANEMIA_PNC26_MISS = 0 if ANEMIA_PNC26 != . 
	replace ANEMIA_PNC26_MISS = 1 if ANEMIA_PNC26 == . 
		
	label values ANEMIA_PNC26_MISS anem_miss 
	label var ANEMIA_PNC26_MISS "Missing reason - Anemia PNC-26 Visit Window"
	
	tab ANEMIA_PNC26, m 
	tab HB_LOW_PNC26_TEST, m 
	
	*gen ANEMIA_PNC26_H // consult with Xiaoyan before constructing 
	
	
	save "$wrk/ANEMIA_PNC26_window", replace 		
	
	
	
//////////////////////////////////////	
//////////////////////////////////////	
//////////////////////////////////////	
*Merge files & set denominator:

	clear
	
	* Start with enrollment indicator:
	
	use "$OUT/MAT_ENROLL"
	
		*update var format PREG_START_DATE // no longer needed as of 10-18 data
		rename PREG_START_DATE STR_PREG_START_DATE
		
		gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
		format PREG_START_DATE %td 
		
		////////////////////////////////////////////////////////////////////
		*TEMPORARY MEASURE: REMOVE PREG_START_DATE FOR ERROR CASE: KEARC00074
		replace PREG_START_DATE = . if PREG_START_DATE == date("20240407", "YMD") & ///
			MOMID == "KEARC00074"
		////////////////////////////////////////////////////////////////////		
		
	*Merge in PREG_END dataset:
	
	merge 1:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing(PREG_END PREG_END_DATE ///
		PREG_END_GA PREG_LOSS PREG_LOSS_INDUCED PREG_LOSS_DEATH MAT_DEATH ///
		 MAT_DEATH_DATE MAT_DEATH_GA CLOSEOUT CLOSEOUT_DT CLOSEOUT_GA ///
		 CLOSEOUT_TYPE STOP_DATE)
	
		*send indicators to 0 for those not in the PREG_END dataset:
		foreach var of varlist PREG_END MAT_DEATH CLOSEOUT {
		replace `var' = 0 if _merge == 1 | `var' == .
		
		tab `var', m 
		}
		
		*clean up:
		replace CLOSEOUT_GA = . if CLOSEOUT_GA <0 & CLOSEOUT == 1 
		
		*drop those not enrolled: 
		drop if ENROLL==0 | ENROLL == . 	
		
		drop _merge 
		
	*Merge in the smoking indicator:
	merge 1:1 MOMID PREGID using "$wrk/SMOKE", keepusing(SMOKE)
	
		*send indicators to 99 for those not in the SMOKE dataset:
		foreach var of varlist SMOKE {
		replace `var' = 99 if _merge == 1
		}
		
		*drop those not enrolled: 
		drop if _merge == 2 
		drop _merge 		
		
	*Merge in anemia datasets:
	
	foreach l in ANEMIA_t1 ANEMIA_t2 ANEMIA_t3 ANEMIA_ANC20_window ///
		ANEMIA_ANC32_window ANEMIA_PNC6_window ANEMIA_PNC26_window {
		
	merge 1:1 MOMID PREGID using "$wrk/`l'"
	
	drop if _merge == 2 
	
	gen `l'_NONE = 1 if _merge == 1 
	
	drop _merge 
		
		}
		
	* * * * * NEW CODE ADDED ON 5-28: SELECT FROM TRIMESTER MEASURES 
	* TO DETERMINE ANEMIA IN ALL OF PREGNANCY: 
	
	* ALL TESTS: 
	
	foreach var of varlist ANEMIA_T1 ANEMIA_T2 ANEMIA_T3 {
	    
	replace `var' = -55 if `var' == 55 
	replace `var' = -77 if `var' == 77 
		
	}
	
	gen anem_max = max(ANEMIA_T1, ANEMIA_T2, ANEMIA_T3) 
	
		gen anem_max_t1 = 1 if anem_max == ANEMIA_T1 & anem_max !=.
		gen anem_max_t2 = 1 if anem_max == ANEMIA_T2 & anem_max !=. 
		gen anem_max_t3 = 1 if anem_max == ANEMIA_T3 & anem_max !=. 
		
		egen anem_max_points= rowtotal(anem_max_t1 anem_max_t2 anem_max_t3) ///
			if anem_max !=.
	
	gen hb_min = min(HB_LOW_T1, HB_LOW_T2, HB_LOW_T3) if anem_max_t2 ==1 
	replace hb_min = min(HB_LOW_T1, HB_LOW_T3) if anem_max_t2==. 
	
		gen hb_min_t1 = 1 if hb_min == HB_LOW_T1 & hb_min !=. & anem_max_t1 == 1
		gen hb_min_t2 = 1 if hb_min == HB_LOW_T2 & hb_min !=. & anem_max_t2 == 1 
		gen hb_min_t3 = 1 if hb_min == HB_LOW_T3 & hb_min !=. & anem_max_t3 == 1 
		
		egen hb_min_points= rowtotal(hb_min_t1 hb_min_t2 hb_min_t3) ///
			if hb_min !=.
			
	gen anemia_anc_timing = .
	
	foreach num of numlist 1/3 {
	
    * If only one worst anemia status: choose that trimester
	replace anemia_anc_timing = `num' if anem_max_t`num' == 1 & anem_max_points == 1 
	
	* If multiple trimesters with the same anemia status: choose the trimester with lowest HB 
	replace anemia_anc_timing = `num' if anem_max_t`num' == 1 & anem_max_points >1 & ///
		hb_min_points == 1 & hb_min_t`num' == 1 & anem_max_points!= . 
		
	* If multiple trimesters with the same anemia status & hb: choose the earlier trimester 
	replace anemia_anc_timing = `num' if anem_max_t`num' == 1 & anem_max_points >1 & ///
		hb_min_t`num' == 1 & hb_min_points > 1 & anem_max_points != . & ///
		hb_min_points != . & ///
		anemia_anc_timing == . 
		
	}
	
	tab anemia_anc_timing, m 
	
	gen ANEMIA_ANC = . 
	gen ANEMIA_ANC_MISS = .
	gen HB_LOW_ANC = .
	gen HB_LOW_ANC_DT = .
	format HB_LOW_ANC_DT %td 
	gen HB_LOW_ANC_GA = .
	gen HB_LOW_ANC_TEST = ""
	
	foreach num of numlist 1/3 {
	
	replace ANEMIA_ANC = ANEMIA_T`num' if anemia_anc_timing == `num'
	replace ANEMIA_ANC_MISS = ANEMIA_T`num'_MISS if anemia_anc_timing == `num'
	replace HB_LOW_ANC = HB_LOW_T`num' if anemia_anc_timing == `num'
	replace HB_LOW_ANC_DT = HB_LOW_T`num'_DT if anemia_anc_timing == `num'
	replace HB_LOW_ANC_GA = HB_LOW_T`num'_GA if anemia_anc_timing == `num'
	replace HB_LOW_ANC_TEST = HB_LOW_T`num'_TEST if anemia_anc_timing == `num'
	
	}
	
	tab ANEMIA_ANC, m 
	
	
	
	* CBC TESTS
	
	foreach var of varlist ANEMIA_CBC_T1 ANEMIA_CBC_T2 ANEMIA_CBC_T3 {
	    
	replace `var' = -55 if `var' == 55 
	replace `var' = -77 if `var' == 77 
		
	}
	
	gen anem_max_CBC = max(ANEMIA_CBC_T1, ANEMIA_CBC_T2, ANEMIA_CBC_T3) 
	
		gen anem_max_CBC_T1 = 1 if anem_max_CBC == ANEMIA_CBC_T1 & anem_max_CBC !=.
		gen anem_max_CBC_T2 = 1 if anem_max_CBC == ANEMIA_CBC_T2 & anem_max_CBC !=. 
		gen anem_max_CBC_T3 = 1 if anem_max_CBC == ANEMIA_CBC_T3 & anem_max_CBC !=. 
		
		egen anem_max_CBC_points= rowtotal(anem_max_CBC_T1 anem_max_CBC_T2 anem_max_CBC_T3) ///
			if anem_max_CBC!=.
	
	gen hb_min_CBC = min(HB_LOW_CBC_T1, HB_LOW_CBC_T2, HB_LOW_CBC_T3) if anem_max_CBC_T2 ==1 
	replace hb_min_CBC = min(HB_LOW_CBC_T1, HB_LOW_CBC_T3) if anem_max_CBC_T2==. 
	
		gen hb_min_CBC_T1 = 1 if hb_min_CBC == HB_LOW_CBC_T1 & hb_min_CBC !=. & anem_max_CBC_T1 == 1
		gen hb_min_CBC_T2 = 1 if hb_min_CBC == HB_LOW_CBC_T2 & hb_min_CBC !=. & anem_max_CBC_T2 == 1 
		gen hb_min_CBC_T3 = 1 if hb_min_CBC == HB_LOW_CBC_T3 & hb_min_CBC !=. & anem_max_CBC_T3 == 1 
		
		egen hb_min_CBC_points= rowtotal(hb_min_CBC_T1 hb_min_CBC_T2 hb_min_CBC_T3) ///
			if hb_min_CBC !=.
			
	gen anemia_anc_timing_CBC = .
	
	foreach num of numlist 1/3 {
	
    * If only one worst anemia status: choose that trimester
	replace anemia_anc_timing_CBC = `num' if anem_max_CBC_T`num' == 1 & anem_max_CBC_points == 1 
	
	* If multiple trimesters with the same anemia status: choose the trimester with lowest HB 
	replace anemia_anc_timing_CBC = `num' if anem_max_CBC_T`num' == 1 & anem_max_CBC_points >1 & ///
		hb_min_CBC_points == 1 & hb_min_CBC_T`num' == 1 & anem_max_CBC_points!= . 
		
	* If multiple trimesters with the same anemia status & hb: choose the earlier trimester 
	replace anemia_anc_timing_CBC = `num' if anem_max_CBC_T`num' == 1 & anem_max_CBC_points >1 & ///
		hb_min_CBC_T`num' == 1 & hb_min_CBC_points > 1 & anem_max_CBC_points != . & ///
		hb_min_CBC_points != . & ///
		anemia_anc_timing_CBC == . 
		
	}
	
	tab anemia_anc_timing_CBC, m 
	
	gen ANEMIA_CBC_ANC  = . 
	gen ANEMIA_CBC_ANC_MISS = .
	gen HB_LOW_CBC_ANC = .
	gen HB_LOW_CBC_ANC_DT = .
	format HB_LOW_CBC_ANC_DT %td 
	gen HB_LOW_CBC_ANC_GA  = .
	
	foreach num of numlist 1/3 {
	
	replace ANEMIA_CBC_ANC = ANEMIA_CBC_T`num' if anemia_anc_timing_CBC == `num'
	replace ANEMIA_CBC_ANC_MISS = ANEMIA_CBC_T`num'_MISS if anemia_anc_timing_CBC == `num'
	replace HB_LOW_CBC_ANC = HB_LOW_CBC_T`num' if anemia_anc_timing_CBC == `num'
	replace HB_LOW_CBC_ANC_DT = HB_LOW_CBC_T`num'_DT if anemia_anc_timing_CBC == `num'
	replace HB_LOW_CBC_ANC_GA = HB_LOW_CBC_T`num'_GA if anemia_anc_timing_CBC == `num'
	
	}
	
	tab ANEMIA_CBC_ANC, m 
	
	* CHECK:
	list ANEMIA_T1 HB_LOW_T1 ANEMIA_T2 HB_LOW_T2 ANEMIA_T3 HB_LOW_T3 ///
		anemia_anc_timing ANEMIA_ANC HB_LOW_ANC 
	
	
	list ANEMIA_CBC_T1 HB_LOW_CBC_T1 ANEMIA_CBC_T2 HB_LOW_CBC_T2 ANEMIA_CBC_T3 ///
		HB_LOW_CBC_T3 anemia_anc_timing_CBC ANEMIA_CBC_ANC HB_LOW_CBC_ANC 	 
	 
	 
	 drop anem_max* hb_min* anemia_anc_timin* 


		
/////////////////////////////
/////////////////////////////
/////////////////////////////

*Variables for all denoms:
	gen MISS_PREG_START_DATE = 0 
		replace MISS_PREG_START_DATE = 1 if PREG_START_DATE == . 
		label var MISS_PREG_START_DATE "Missing BOE/estimated conception date"
		
		tab MISS_PREG_START_DATE, m 

	
 *Denominator: Trimester 1
	*Recruited in Trimester 1 (<14 weeks GA)
	*Completed Trimester 1 (>=14 weeks GA)
	*Exclude: 
		*Closeouts at < 98 days with no labs
		*Deaths at <98 days with no labs 
		*Pregnancy losses at <98 days with no labs 
		
		// indicator for enrolled during trimester 1:
		gen ENROLL_T1 = 0 if BOE_GA_DAYS != . 
		replace ENROLL_T1 = 1 if BOE_GA_DAYS < 98 & BOE_GA_DAYS != . 
		label var ENROLL_T1 "Participant enrolled during 1st trimester"
		
		
			//check: 
			tab BOE_GA_DAYS ENROLL_T1, m 
			tab ENROLL_T1
		
		// indicator for TRI1 completed: 
		gen COMPLETE_T1 = 0 
		replace COMPLETE_T1 = 1 if date("$dadate", "YMD") >= PREG_START_DATE + 98 & ///
			PREG_START_DATE != . 
		label var COMPLETE_T1 "Denominator - completed trimester 1"
		
		tab COMPLETE_T1, m 
		tab COMPLETE_T1 ENROLL_T1, m 
		
		// indicator for proceeded through study until the end of the window: 
		gen PROCEED_T1 = 0 
		replace PROCEED_T1 = 1 if CLOSEOUT == 0 & MAT_DEATH == 0 
		
		replace PROCEED_T1 = 1 if CLOSEOUT == 1 & CLOSEOUT_GA >= 98
		
		replace PROCEED_T1 = 1 if MAT_DEATH == 1 & MAT_DEATH_GA >= 98
		
		replace PROCEED_T1 = 99 if MISS_PREG_START_DATE == 1 
		
			*Check remaining 0s: 
		list PROCEED_T1 CLOSEOUT CLOSEOUT_GA MAT_DEATH MAT_DEATH_GA if ///
			PROCEED_T1 == 0
			
		label var PROCEED_T1 "Women proceeded in study through the end of window"
			
		// indicator for continuous pregnancy through window: 
		gen PREG_CONT_T1 = 0 
		replace PREG_CONT_T1 = 1 if PREG_END == 0 
		replace PREG_CONT_T1 = 1 if PREG_END == 1 & PREG_END_GA >= 98 & PREG_END_GA != .
		
		replace PREG_CONT_T1 = 99 if MISS_PREG_START_DATE == 1 | ///
			(PREG_END == 1 & PREG_END_GA == .)
			
			*Check remaining 0s:
		list PREG_CONT_T1 PREG_END PREG_END_GA PREG_START_DATE PREG_END_DATE ///
			PREG_LOSS PREG_LOSS_INDUCED if PREG_CONT_T1 == 0 
	
		
		*Set denominator: 
		gen ANEMIA_T1_DENOM = 0 
		replace ANEMIA_T1_DENOM = 1 if ENROLL_T1 == 1 & COMPLETE_T1 == 1 & ///
			PROCEED_T1 == 1 & PREG_CONT_T1 == 1
			
			// NOTE: We will keep in denominator if the person has any labs:
		replace ANEMIA_T1_DENOM = 1 if HB_LOW_T1 != . 
		
			// Those who are in the denominator, but missing, we will add to missing & missing indicator 
		replace ANEMIA_T1 = 55 if ANEMIA_T1_DENOM == 1 & ANEMIA_T1 == . 
		
			// Those who are in the denom but had no labs at all = 2 
		replace ANEMIA_T1_MISS = 2 if ANEMIA_T1_DENOM == 1 & ANEMIA_T1 == 55 & ///
			ANEMIA_T1_MISS != 1 
			
		*CHECKS: 
		tab ANEMIA_T1_MISS ANEMIA_T1, m 
		tab ANEMIA_T1_MISS ANEMIA_T1_DENOM,m 
			
		label var ANEMIA_T1_DENOM "Denominator for Trimester 1 - Anemia"
		
		
 *Denominator: Trimester 2
	*Completed Trimester 2 (>=28 weeks GA)
	*Exclude: 
		*Closeouts at < 196 days with no labs
		*Deaths at <196 days with no labs 
		*Pregnancy losses at <98 days with no labs 
		
		// indicator for TRI2 completed: 
		gen COMPLETE_T2 = 0 
		replace COMPLETE_T2 = 1 if date("$dadate", "YMD") >= PREG_START_DATE + 196 & ///
			PREG_START_DATE != . 
		label var COMPLETE_T2 "Denominator - completed trimester 1"
		
		tab COMPLETE_T2, m  
		
		// indicator for proceeded through study until the end of the window: 
		gen PROCEED_T2 = 0 
		replace PROCEED_T2 = 1 if CLOSEOUT == 0 & MAT_DEATH == 0 
		
		replace PROCEED_T2 = 1 if CLOSEOUT == 1 & CLOSEOUT_GA >= 196
		
		replace PROCEED_T2 = 1 if MAT_DEATH == 1 & MAT_DEATH_GA >= 196
		
		replace PROCEED_T2 = 99 if MISS_PREG_START_DATE == 1 
		
			*Check remaining 0s: 
		list PROCEED_T2 CLOSEOUT CLOSEOUT_GA MAT_DEATH MAT_DEATH_GA if ///
			PROCEED_T2 == 0
			
		label var PROCEED_T2 "Women proceeded in study through the end of window"
			
		// indicator for continuous pregnancy through window: 
		gen PREG_CONT_T2 = 0 
		replace PREG_CONT_T2 = 1 if PREG_END == 0 
		replace PREG_CONT_T2 = 1 if PREG_END == 1 & PREG_END_GA >= 196 & PREG_END_GA != .
		
		replace PREG_CONT_T2 = 99 if MISS_PREG_START_DATE == 1 | ///
			(PREG_END == 1 & PREG_END_GA == .)
			
			*Check remaining 0s:
		list PREG_CONT_T2 PREG_END PREG_END_GA PREG_START_DATE PREG_END_DATE ///
			PREG_LOSS PREG_LOSS_INDUCED if PREG_CONT_T2 == 0 & ///
			PREG_CONT_T1 == 1
	
		
		*Set denominator: 
		gen ANEMIA_T2_DENOM = 0 
		replace ANEMIA_T2_DENOM = 1 if COMPLETE_T2 == 1 & ///
			PROCEED_T2 == 1 & PREG_CONT_T2 == 1
			
			// NOTE: We will keep in denominator if the person has any labs:
		replace ANEMIA_T2_DENOM = 1 if HB_LOW_T2 != . 
		
			// Those who are in the denominator, but missing, we will add to missing & missing indicator 
		replace ANEMIA_T2 = 55 if ANEMIA_T2_DENOM == 1 & ANEMIA_T2 == . 
		
			// Those who are in the denom but had no labs at all = 2 
		replace ANEMIA_T2_MISS = 2 if ANEMIA_T2_DENOM == 1 & ANEMIA_T2 == 55 & ///
			ANEMIA_T2_MISS != 1 
			
		*CHECKS: 
		tab ANEMIA_T2_MISS ANEMIA_T2, m 
		tab ANEMIA_T2_MISS ANEMIA_T2_DENOM,m 
			
		label var ANEMIA_T2_DENOM "Denominator for Trimester 2 - Anemia"
		
		
*Denominator: Trimester 3
	*Completed Trimester 3 (>=40 weeks GA)
	*Exclude: 
		*Closeouts at < 280 days with no labs
		*Deaths at < 280 days with no labs 
		
		// indicator for TRI3 completed: 
		gen COMPLETE_T3 = 0 
		replace COMPLETE_T3 = 1 if date("$dadate", "YMD") >= PREG_START_DATE + 280 & ///
			PREG_START_DATE != . 
		label var COMPLETE_T3 "Denominator - completed trimester 3"
		
		tab COMPLETE_T3, m  
		
		// indicator for proceeded through study until the end of pregnancy (for T3)
		gen PROCEED_T3 = 0 
		replace PROCEED_T3 = 1 if CLOSEOUT == 0 & MAT_DEATH == 0 
		
		replace PROCEED_T3 = 1 if CLOSEOUT == 1 & CLOSEOUT_GA >= 280 
		
		replace PROCEED_T3 = 1 if MAT_DEATH == 1 & MAT_DEATH_GA >= 280
		
		replace PROCEED_T3 = 99 if MISS_PREG_START_DATE == 1 
		
			*Check remaining 0s: 
		list PROCEED_T3 CLOSEOUT CLOSEOUT_GA MAT_DEATH MAT_DEATH_GA if ///
			PROCEED_T3 == 0
			
		label var PROCEED_T3 "Women proceeded in study through the end of window"
			
		// indicator for pregnancy continued into T3
		gen PREG_CONT_T3 = 0 
		replace PREG_CONT_T3 = 1 if PREG_END == 0 
		replace PREG_CONT_T3 = 1 if PREG_END == 1 & PREG_END_GA >= 196 & PREG_END_GA != .
		
		replace PREG_CONT_T3 = 99 if MISS_PREG_START_DATE == 1 | ///
			(PREG_END == 1 & PREG_END_GA == .)
			
			*Check remaining 0s:
		list PREG_CONT_T3 PREG_END PREG_END_GA PREG_START_DATE PREG_END_DATE ///
			PREG_LOSS PREG_LOSS_INDUCED if PREG_CONT_T3 == 0 
	
		
		*Set denominator: 
		gen ANEMIA_T3_DENOM = 0 
		replace ANEMIA_T3_DENOM = 1 if COMPLETE_T3 == 1 & ///
			PROCEED_T3 == 1 & PREG_CONT_T3 == 1
			
			// NOTE: We will keep in denominator if the person has any labs:
		replace ANEMIA_T3_DENOM = 1 if HB_LOW_T3 != . 
		
			// Those who are in the denominator, but missing, we will add to missing & missing indicator 
		replace ANEMIA_T3 = 55 if ANEMIA_T3_DENOM == 1 & ANEMIA_T3 == . 
		
			// Those who are in the denom but had no labs at all = 2 
		replace ANEMIA_T3_MISS = 2 if ANEMIA_T3_DENOM == 1 & ANEMIA_T3 == 55 & ///
			ANEMIA_T3_MISS != 1 
			
		*CHECKS: 
		tab ANEMIA_T3_MISS ANEMIA_T3, m 
		tab ANEMIA_T3_MISS ANEMIA_T3_DENOM,m 
			
		label var ANEMIA_T3_DENOM "Denominator for Trimester 3 - Anemia"
		
		
		/////////////////////////////////////////////
		*Reasons for NOT being in the denominator:
		gen ANEMIA_T3_DENOM_NOT = 0 if ANEMIA_T3_DENOM == 1 
			// 1=pregnancy ended prior to 3rd trimester: 
		replace ANEMIA_T3_DENOM_NOT = 1 if ANEMIA_T3_DENOM == 0 & ///
			ANEMIA_T3_DENOM_NOT == . & PREG_END == 1 & PREG_END_GA < 196 & ///
			PREG_END_GA != . 
			// 2=closeout/stop prior to end of 3rd trimester: 
		replace ANEMIA_T3_DENOM_NOT = 2 if ANEMIA_T3_DENOM == 0 & ///
			ANEMIA_T3_DENOM_NOT == . & ((CLOSEOUT == 1 & PREG_END == 0 & ///
			CLOSEOUT_GA <= 280) | ///
			(MAT_DEATH==1 & PREG_END==1 & PREG_LOSS_DEATH == 1 & MAT_DEATH_GA <= 280))
			// 3=not yet completed study window: 
		replace ANEMIA_T3_DENOM_NOT = 3 if ANEMIA_T3_DENOM == 0 & ///
			ANEMIA_T3_DENOM_NOT == . & COMPLETE_T3 == 0 & ///
			PREG_START_DATE != . 
			// 4=missing GA info
		replace ANEMIA_T3_DENOM_NOT = 4 if ANEMIA_T3_DENOM ==  0 & ///
			ANEMIA_T3_DENOM_NOT == . & PREG_START_DATE == . 
			// 5=pregnancy end or maternal death with unknown timing: 
		replace ANEMIA_T3_DENOM_NOT = 5 if ANEMIA_T3_DENOM == 0 & ///
			ANEMIA_T3_DENOM_NOT == . & ///
			((PREG_END == 1 & PREG_END_DATE == .) | ///
			(MAT_DEATH == 1 & MAT_DEATH_DATE == . & PREG_END_DATE == .))
			// 6=suspected date error
		replace ANEMIA_T3_DENOM_NOT = 6 if ANEMIA_T3_DENOM == 0 & ///
			ANEMIA_T3_DENOM_NOT == . & ///
			(CLOSEOUT_GA < PREG_END_GA)
		
		// review those with deaths/closeouts before the window ends: 
		tab ANEMIA_T3_DENOM_NOT ANEMIA_T3_DENOM, m 
		list if ANEMIA_T3_DENOM_NOT == . 
		
		
		label define t3anem 0 "0-In denominator" 1 "1-Pregnancy ended prior to T3" ///
			2 "2-Closeout prior to end of T3" 3 "3-Not yet completed T3" ///
			4 "4-Missing US GA info" 5 "5-End/Death with missing date" ///
			6 "6-Date error (closeout)"
			
		label var ANEMIA_T3_DENOM_NOT "Reason not in the Anemia T3 denominator"
		
		label values ANEMIA_T3_DENOM_NOT t3anem
		
		tab ANEMIA_T2_DENOM ANEMIA_T3_DENOM,m 
		
			*review reason missing by trimester: 
		tab ANEMIA_T3_DENOM_NOT ANEMIA_T2_DENOM, m 
		tab ANEMIA_T3_DENOM_NOT ANEMIA_T1_DENOM, m 
		
 *Denominator: PNC 6
	*Completed PNC-6 visit window (>=84 days PP)
	*Exclude: 
		*Closeouts at < 84 days pp with no labs
		*Deaths at < 84 days pp with no labs 
		
		// indicator for PNC6 window completed  
		gen COMPLETE_PNC6 = 0 
		replace COMPLETE_PNC6 = 1 if date("$dadate", "YMD") >= PREG_END_DATE + 84 & ///
			PREG_START_DATE != . 
		label var COMPLETE_PNC6 "Denominator - completed PNC 6 window"
		
		tab COMPLETE_PNC6, m  
		
		// indicator for proceeded through study until the end of the window: 
		gen PROCEED_PNC6 = 0 
		replace PROCEED_PNC6 = 1 if CLOSEOUT == 0 & MAT_DEATH == 0 & PREG_END == 1 
		
		replace PROCEED_PNC6 = 1 if CLOSEOUT == 1 & PREG_END ==1 & ///
			CLOSEOUT_DT >=  PREG_END_DATE + 84 & CLOSEOUT_DT != . & ///
			PREG_END_DATE != . 
		
		replace PROCEED_PNC6 = 1 if MAT_DEATH == 1 & PREG_END==1 & ///
			MAT_DEATH_DATE >=  PREG_END_DATE + 84 & MAT_DEATH_DATE != . & ///
			PREG_END_DATE != .
		
		replace PROCEED_PNC6 = 99 if PREG_END_DATE == . 
		
			*Check remaining 0s: 
		list PROCEED_PNC6 CLOSEOUT CLOSEOUT_GA PREG_END_DATE CLOSEOUT_DT ///
			MAT_DEATH MAT_DEATH_GA if ///
			PROCEED_PNC6 == 0
			
		label var PROCEED_PNC6 "Women proceeded in study through the end of window"
		
		*Set denominator: 
		gen ANEMIA_PNC6_DENOM = 0 
		replace ANEMIA_PNC6_DENOM = 1 if COMPLETE_PNC6 == 1 & ///
			PROCEED_PNC6 == 1 
			
			// NOTE: We will keep in denominator if the person has any labs:
		replace ANEMIA_PNC6_DENOM = 1 if HB_LOW_PNC6 != . 
		
			// Those who are in the denominator, but missing, we will add to missing & missing indicator 
		replace ANEMIA_PNC6 = 55 if ANEMIA_PNC6_DENOM == 1 & ANEMIA_PNC6 == . 
		
			// Those who are in the denom but had no labs at all = 2 
		replace ANEMIA_PNC6_MISS = 2 if ANEMIA_PNC6_DENOM == 1 & ANEMIA_PNC6 == 55 & ///
			ANEMIA_PNC6_MISS != 1 
			
			// Those who are unknown days PP (i.e., preg end date missing)
		replace ANEMIA_PNC6_MISS = 3 if PREG_END==1 & PREG_END_DATE == . & ///
			ANEMIA_PNC6_MISS != 1 
			
		*CHECKS: 
		tab ANEMIA_PNC6 ANEMIA_PNC6_DENOM, m 
		tab ANEMIA_PNC6_MISS ANEMIA_PNC6_DENOM,m  
			
		label var ANEMIA_PNC6_DENOM "Denominator for PNC6 - Anemia"	
		
		
 *Denominator: PNC 26
	*Completed PNC-26 visit window (>=273 days PP)
	*Exclude: 
		*Closeouts at < 273 days pp with no labs
		*Deaths at < 273 days pp with no labs 
		*Pregnancy losses at <20 weeks (no longer followed)
		
		// indicator for PNC26 window completed  
		gen COMPLETE_PNC26 = 0 
		replace COMPLETE_PNC26 = 1 if date("$dadate", "YMD") >= PREG_END_DATE + 273 & ///
			PREG_START_DATE != . 
		label var COMPLETE_PNC26 "Denominator - completed PNC 26 window"
		
		tab COMPLETE_PNC26, m  
		
		// indicator for proceeded through study until the end of the window: 
		gen PROCEED_PNC26 = 0 
		replace PROCEED_PNC26 = 1 if CLOSEOUT == 0 & MAT_DEATH == 0 & PREG_END == 1 
		
		replace PROCEED_PNC26 = 1 if CLOSEOUT == 1 & PREG_END ==1 & ///
			CLOSEOUT_DT >=  PREG_END_DATE + 273 & CLOSEOUT_DT != . & ///
			PREG_END_DATE != . 
		
		replace PROCEED_PNC26 = 1 if MAT_DEATH == 1 & PREG_END==1 & ///
			MAT_DEATH_DATE >=  PREG_END_DATE + 273 & MAT_DEATH_DATE != . & ///
			PREG_END_DATE != .
		
		replace PROCEED_PNC26 = 99 if PREG_END_DATE == . 
		
			*Check remaining 0s: 
		list PROCEED_PNC26 CLOSEOUT CLOSEOUT_GA PREG_END_DATE CLOSEOUT_DT ///
			MAT_DEATH MAT_DEATH_GA if ///
			PROCEED_PNC26 == 0
			
		label var PROCEED_PNC26 "Women proceeded in study through the end of window"
		
		*Set denominator: 
		gen ANEMIA_PNC26_DENOM = 0 
		replace ANEMIA_PNC26_DENOM = 1 if COMPLETE_PNC26 == 1 & ///
			PROCEED_PNC26 == 1 & PREG_LOSS == 0 
			
			// NOTE: We will keep in denominator if the person has any labs:
		replace ANEMIA_PNC26_DENOM = 1 if HB_LOW_PNC26 != . 
		
			// Those who are in the denominator, but missing, we will add to missing & missing indicator 
		replace ANEMIA_PNC26 = 55 if ANEMIA_PNC26_DENOM == 1 & ANEMIA_PNC26 == . 
		
			// Those who are in the denom but had no labs at all = 2 
		replace ANEMIA_PNC26_MISS = 2 if ANEMIA_PNC26_DENOM == 1 & ANEMIA_PNC26 == 55 & ///
			ANEMIA_PNC26_MISS != 1 
			
			// Those who are unknown days PP (i.e., preg end date missing)
		replace ANEMIA_PNC26_MISS = 3 if PREG_END==1 & PREG_END_DATE == . & ///
			ANEMIA_PNC26_MISS != 1 
			
		*CHECKS: 
		tab ANEMIA_PNC26 ANEMIA_PNC26_DENOM, m 
		tab ANEMIA_PNC26_MISS ANEMIA_PNC26_DENOM,m  
			
		label var ANEMIA_PNC26_DENOM "Denominator for PNC26 - Anemia"	
		
		
* Fix missing indicator for ANEMIA_ANC overall (denom = PREG_END): 

	tab ANEMIA_ANC if PREG_END==1, m 
	
	replace ANEMIA_ANC=55 if ANEMIA_ANC == . 
	
	*No lab forms: 
	replace ANEMIA_ANC_MISS = 2 if ANEMIA_ANC == 55 & PREG_END==1 & ///
		ANEMIA_ANC_MISS == . 
		
	tab ANEMIA_ANC_MISS, m 
	tab ANEMIA_ANC_MISS if PREG_END==1, m 
		
		
	*UPDATE for final dataset: 
	
	drop *_NONE ENTRY_TOTAL POC_COUNT
	
	drop ENROLL_SCRN_DATE ENROLL BOE_GA_DAYS PREG_START_DATE ///
	EDD_BOE PREG_END PREG_END_GA PREG_END_DATE PREG_LOSS PREG_LOSS_INDUCED ///
	CLOSEOUT CLOSEOUT_DT CLOSEOUT_GA CLOSEOUT_TYPE MAT_DEATH MAT_DEATH_DATE ///
	MAT_DEATH_GA STOP_DATE
	
	drop BOE_* ENROLL_* *_WINDOW *_PASS_*
	
	
	/* Update for Xiaoyan -- add HB_LEVEL_ANC outcome - REMOVED ON 11-9-2025: 
	
	*1st merge in high HB dataset (highest HB in pregnancy):
	merge 1:1 MOMID PREGID using "$wrk/ANEMIA_ANC_HIGH-HB"
	
	
		gen HB_LEVEL_ANC = 1 if ANEMIA_ANC == 3 // severe anemia 
		replace HB_LEVEL_ANC = 2 if ANEMIA_ANC == 2 // moderate anemia 
		replace HB_LEVEL_ANC = 3 if ANEMIA_ANC == 1 // mild anemia 
		replace HB_LEVEL_ANC = 4 if ANEMIA_ANC == 0 & HB_HIGH_ANC <=13 // no anemia-normal 
		replace HB_LEVEL_ANC = 5 if ANEMIA_ANC == 0 & HB_HIGH_ANC >13 & HB_HIGH_ANC <15 // no anemia - ever high > 13
		replace HB_LEVEL_ANC = 6 if ANEMIA_ANC == 0 & HB_HIGH_ANC >=15 & HB_HIGH_ANC !=. // no anemia - ever high > 15
		
		replace HB_LEVEL_ANC = 55 if ANEMIA_ANC == 55 
		
		tab HB_LEVEL_ANC ANEMIA_ANC, m 
		
		label var HB_LEVEL_ANC "Summary of HB level in pregnancy (anemia prioritized over high HB)"
		
		label define hbl 1 "1-Severe anemia" 2 "2-Moderate anemia" 3 "3-Mild anemia" ///
			4 "4-Normal" 5 "5-High HB 13-<15" 6 "6-High HB >=15"
			
		label values  HB_LEVEL_ANC hbl
		
		tab HB_LEVEL_ANC
		
		drop  _merge PREG_START_DATE ENTRY_TOTAL POC_COUNT 
		
		*/
		
		drop REMAPP_*
		


	**** FINALIZE DATASET & SAVE: 
	save "$wrk/ANEMIA", replace 
	
	
	//////////////////////////////////////////////////////////////////////////
	* Added on 5-27-2025: Dates for each anemia level: 
	clear 
	
	use  "$wrk/ANEMIA_all_long"
	
	// restrict to pregnancy tests
	keep if TEST_TIMING == 0 
	
	// drop if: HB level is missing or date is missing: 
	drop if HB_LBORRES== . | TEST_DATE == . 
	
	* Run anemia loops: 
	
	//// set globals for GA criteria
	global trimester13 ///
	"(TEST_GA < 98 | TEST_GA >=196) & TEST_GA != ."
	global trimester2 ///
	"TEST_GA >= 98 & TEST_GA <196 & TEST_GA != ."
	
	gen ANEMIA_LEVEL = .
	
	//// Hb occurs in trimester 1 or 3: 
	replace ANEMIA_LEVEL = 0 if HB_LBORRES >= 11.0 & HB_LBORRES != . & ///
		$trimester13
	replace ANEMIA_LEVEL = 1 if HB_LBORRES >= 10.0 & HB_LBORRES <11.0 & ///
		$trimester13
	replace ANEMIA_LEVEL = 2 if HB_LBORRES >= 7.0 & HB_LBORRES <10.0 & ///
		$trimester13
	replace ANEMIA_LEVEL = 3 if HB_LBORRES <7.0 & HB_LBORRES != . & ///
		$trimester13

	//// HB occurs in trimester 2:  
	replace ANEMIA_LEVEL = 0 if HB_LBORRES >= 10.5 & HB_LBORRES != . & ///
		$trimester2
	replace ANEMIA_LEVEL = 1 if HB_LBORRES >= 9.5 & HB_LBORRES <10.5 & ///
		$trimester2
	replace ANEMIA_LEVEL = 2 if HB_LBORRES >= 7.0 & HB_LBORRES <9.5 & ///
		$trimester2
	replace ANEMIA_LEVEL = 3 if HB_LBORRES <7.0 & HB_LBORRES != . & ///
		$trimester2
	
	label var ANEMIA_LEVEL "Anemia status"
	
	tab ANEMIA_LEVEL, m 
	
	gen TRIMESTER = 0 
	replace TRIMESTER = 1 if TEST_GA < 98 & TEST_GA != . 
	replace TRIMESTER = 2 if TEST_GA >= 98 & TEST_GA <196 & TEST_GA != .
	replace TRIMESTER = 3 if TEST_GA >=196 & TEST_GA != . 
	
	tab TRIMESTER, m
	
	* Generate a count of POC tests: 
	sort MOMID PREGID TEST_DATE 
	by MOMID PREGID: gen poc_num = _n if TEST_TYPE== "POC"
	
	sum poc_num 
	return list 
	
	global pocmax = r(max)
	
	foreach num of numlist 1/$pocmax {
	
	*Process: keep only POC values that are NOT within 7 days of a CBC test: 
	sort MOMID PREGID TEST_DATE 
	by MOMID PREGID: gen hb_num = _n 
	
	sum hb_num 
	return list 
	
	global max = r(max)
	
	keep site MOMID PREGID TEST_DATE TEST_GA TEST_TYPE ANEMIA_LEVEL TRIMESTER hb_num 
	
	*Create dataset to merge back in as "previous test"
	preserve 
	
	foreach var of varlist 	TEST_DATE TEST_GA TEST_TYPE ANEMIA_LEVEL hb_num TRIMESTER {
	    
	rename `var' `var'_prev
	
	}

	save "$wrk/temp/hb_entries_prev", replace 
	
	restore 
	
	*Create dataset to merge back in as next test"
	preserve 

	foreach var of varlist 	TEST_DATE TEST_GA TEST_TYPE ANEMIA_LEVEL hb_num TRIMESTER {
	    
	rename `var' `var'_next
	
	}

	save "$wrk/temp/hb_entries_next", replace 
	
	restore 
	
	gen hb_num_prev = hb_num - 1 
	gen hb_num_next = hb_num + 1 
	
	*merge in "previous test" indicators: 
	merge 1:1 MOMID PREGID hb_num_prev using "$wrk/temp/hb_entries_prev"
	
	drop if _merge == 2 
	drop _merge 
	
	*merge in "next test" indicators: 
	merge 1:1 MOMID PREGID hb_num_next using "$wrk/temp/hb_entries_next"
	
	drop if _merge == 2 
	drop _merge 
	
	list PREGID TEST_DATE hb_num TEST_DATE_prev hb_num_prev TEST_DATE_next hb_num_next
	
	gen POC_WITHIN7 = 1 if TEST_TYPE == "POC" & ///
		TEST_TYPE_prev == "CBC" & abs(TEST_DATE - TEST_DATE_prev) <=7 & ///
		TEST_DATE != . & TEST_DATE_prev != . & ///
		TRIMESTER == TRIMESTER_prev
		
	replace POC_WITHIN7 = 1 if TEST_TYPE == "POC" & ///
		TEST_TYPE_next == "CBC" & abs(TEST_DATE - TEST_DATE_next) <=7 & ///
		TEST_DATE != . & TEST_DATE_next != . & ///
		TRIMESTER == TRIMESTER_next
		
	drop if POC_WITHIN7 == 1 
	
	drop *_prev *_next POC_WITHIN7 hb_num
	
	}
	
	* find date of earliest test: any anemia:
	
	keep if ANEMIA_LEVEL >=1 
	
	preserve 
	
	sort MOMID PREGID TEST_DATE
	by MOMID PREGID: gen hb_num = _n
	
	keep if hb_num == 1
	
	rename TEST_DATE ANEMIA_ANY_FIRST_DT
	rename TEST_TYPE ANEMIA_ANY_FIRST_TYPE
	rename TEST_GA ANEMIA_ANY_FIRST_GA
	rename ANEMIA_LEVEL ANEMIA_ANY_FIRST_LEVEL 
	rename TRIMESTER ANEMIA_ANY_FIRST_TRIMESTER
	
	drop hb_num site
	
	save "$wrk/temp/anemia_any_dates", replace 
	
	restore 
	
	keep if ANEMIA_LEVEL >=2
	
	preserve 
	
	sort MOMID PREGID TEST_DATE
	by MOMID PREGID: gen hb_num = _n
	
	keep if hb_num == 1
	
	rename TEST_DATE ANEMIA_MODSEV_FIRST_DT
	rename TEST_TYPE ANEMIA_MODSEV_FIRST_TYPE
	rename TEST_GA ANEMIA_MODSEV_FIRST_GA
	rename ANEMIA_LEVEL ANEMIA_MODSEV_FIRST_LEVEL 
	rename TRIMESTER ANEMIA_MODSEV_FIRST_TRIMESTER
	
	drop hb_num site
	
	save "$wrk/temp/anemia_modsev_dates", replace 
	
	restore 
	
	keep if ANEMIA_LEVEL >=3
	
	preserve 
	
	sort MOMID PREGID TEST_DATE
	by MOMID PREGID: gen hb_num = _n
	
	keep if hb_num == 1
	
	rename TEST_DATE ANEMIA_SEV_FIRST_DT
	rename TEST_TYPE ANEMIA_SEV_FIRST_TYPE
	rename TEST_GA ANEMIA_SEV_FIRST_GA
	rename ANEMIA_LEVEL ANEMIA_SEV_FIRST_LEVEL 
	rename TRIMESTER ANEMIA_SEV_FIRST_TRIMESTER
	
	drop hb_num site
	
	save "$wrk/temp/anemia_sev_dates", replace 
	
	restore 
	
	clear 
	
	use "$wrk/ANEMIA"
	
	merge 1:1 MOMID PREGID using "$wrk/temp/anemia_any_dates"
	
	tab _merge, m 
	drop _merge 
	
	merge 1:1 MOMID PREGID using "$wrk/temp/anemia_modsev_dates"

	tab _merge, m 
	drop _merge 
	
	merge 1:1 MOMID PREGID using "$wrk/temp/anemia_sev_dates"
	
	tab _merge, m 
	drop _merge 
	
	list ANEMIA_T1 ANEMIA_T2 ANEMIA_T3 ANEMIA_SEV_FIRST_DT ANEMIA_SEV_FIRST_GA ///
		ANEMIA_SEV_FIRST_TRIMESTER if ANEMIA_ANC==3 
		
	list ANEMIA_T1 ANEMIA_T2 ANEMIA_T3 ANEMIA_SEV_FIRST_DT ANEMIA_SEV_FIRST_GA ///
		ANEMIA_SEV_FIRST_TRIMESTER ANEMIA_MODSEV_FIRST_DT ANEMIA_MODSEV_FIRST_GA ///
		ANEMIA_MODSEV_FIRST_TRIMESTER if ANEMIA_ANC==3 
		
	tab 	
		
	save "$OUT/MAT_ANEMIA", replace
	
	
	
	/* LEFT OFF HERE 
	
	
	foreach num of numlist 2/$max {
	    
	preserve 
	
	keep if hb_num == `num'
	
	foreach var of varlist TEST_DATE TEST_GA TEST_TYPE ANEMIA_LEVEL hb_num {
	   
	rename `var' `var'_next 
	
	}
	
	save "$da/temp/hb_entries_`num'", replace 
	
	restore 
	
	}
	
	foreach num of numlist 1/$maxlt {	
	
	preserve 
	
	keep if hb_num == `num' 
	
	foreach var of varlist TEST_DATE TEST_GA TEST_TYPE ANEMIA_LEVEL hb_num {
	   
	rename `var' `var'_before 
	
	}
	
	save "$da/temp/hb_entries_`num'_before", replace 
	
	restore 
		
	}
	
	gen hb_num_next = hb_num + 1 
	
	foreach num of numlist 2/$max {
	    
	merge 1:1 site MOMID PREGID hb_num_next using "$da/temp/hb_entries_`num'"
	
	drop _merge 
		
	}
	
	
	
	*Identify earliest date for "any anemia"
		// first drop non-anemic entries:
	drop if ANEMIA_LEVEL == 0 
	
		// second, sort remaining tests by date: 
	sort MOMID PREGID TEST_DATE 
	by MOMID PREGID: gen hb_num = _n 
	
	preserve 
	keep if hb_num == 1 
	gen ANEMIA_ANY_DATE = 
		
	label values ANEMIA_ANC32_MISS anem_miss 
	label var ANEMIA_ANC32_MISS "Missing reason - Anemia ANC-32 Visit Window"
	
	tab ANEMIA_ANC32, m 
	tab HB_LOW_ANC32_TEST, m 
	
	tab 

			
	**** FINALIZE DATASET & SAVE: 
	
	save "$OUT/MAT_ANEMIA", replace 	
	
*/
	
	/*for codebook: 
	preserve 
		describe, replace clear
		list
		export excel using "$wrk/anemia_vars.xls", replace first(var)
	restore
	*/
	
	tab
	*STOP HERE BEFORE PROCEEDING TO SANKEY PLOT (IF NEEDED)
	
	use "$OUT/MAT_ANEMIA"
	
	/////////////////////////////////////////
	* DRAFT FIGURE: 
	////////////////////////////////////////
	*use "$wrk/ANEMIA"
	merge 1:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing(PREG_END)
	
	drop if _merge == 2 
	
	keep if PREG_END == 1 & ANEMIA_T1_DENOM==1 == 1
	
	**** also: drop 1 pregnancy with closeout date not matching with pregnancy 
	**** end information:
	drop if ANEMIA_T3_DENOM_NOT == 6 
	
	gen newid = _n
	
	sum newid, d 

	tab ANEMIA_T1 ANEMIA_T1_MISS if ANEMIA_T1_DENOM==1, m 
	tab ANEMIA_T2 ANEMIA_T2_MISS if ANEMIA_T2_DENOM==1, m 
	
		*** Examine those who fall out of the denominator:
		tab ANEMIA_T3_DENOM_NOT ANEMIA_T1_DENOM, m 
		tab ANEMIA_T3_DENOM_NOT ANEMIA_T2_DENOM, m 
		
		foreach num of numlist 1/3 {
		
		replace ANEMIA_T`num' = 77 if ANEMIA_T`num' == . & ///
		ANEMIA_T`num'_DENOM == 0 & ANEMIA_T3_DENOM_NOT >= 1 & ///
		ANEMIA_T3_DENOM_NOT <= 5 
		
		}
		
	tab ANEMIA_T1, m 
	tab ANEMIA_T2, m 
	tab ANEMIA_T3, m 
 
	
	preserve 
	
	keep newid ANEMIA_T1 ANEMIA_T2 ANEMIA_T3 
	
	label define anem 0 "No anemia" 1 "Mild anemia" 2 "Moderate anemia" 3 "Severe anemia" ///
		55 "Missing" 77 "Pregnancy ended" 
		
	label values ANEMIA_T1 ANEMIA_T2 ANEMIA_T3 ///
		anem
	
	collapse (count) newid, by(ANEMIA_T1 ANEMIA_T2 ANEMIA_T3)
	

	sankey_plot ANEMIA_T1 ANEMIA_T2 ANEMIA_T3, wide width(newid) ///
		fillcolor(%50) ///
		xlabel("",nogrid) gap(0.1) tight ///
		title("Anemia Status by Trimester") ///
		subtitle ("Completed pregnancies recruited in Trimester 1")	
	
	
	
	
		*STOP HERE BEFORE PROCEEDING TO SANKEY PLOT - POSTPARTUM
	clear 
	use "$OUT/MAT_ANEMIA"
	
	/////////////////////////////////////////
	* DRAFT FIGURE: 
	////////////////////////////////////////
	*use "$wrk/ANEMIA"
	merge 1:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing(PREG_END PNC6_PASS_LATE)
	
	drop if _merge == 2 
	drop _merge 
	
	keep if PREG_END == 1 & ANEMIA_T1_DENOM==1 == 1 & PNC6_PASS_LATE == 1 
	
	**** also: drop 1 pregnancy with closeout date not matching with pregnancy 
	**** end information:
	drop if ANEMIA_T3_DENOM_NOT == 6 
	
	gen newid = _n
	
	sum newid, d 

	tab ANEMIA_T1 ANEMIA_T1_MISS if ANEMIA_T1_DENOM==1, m 
	tab ANEMIA_T2 ANEMIA_T2_MISS if ANEMIA_T2_DENOM==1, m 
	
		*** Examine those who fall out of the denominator:
		tab ANEMIA_T3_DENOM_NOT ANEMIA_T1_DENOM, m 
		tab ANEMIA_T3_DENOM_NOT ANEMIA_T2_DENOM, m 
		
		foreach num of numlist 1/3 {
		
		replace ANEMIA_T`num' = 77 if ANEMIA_T`num' == . & ///
		ANEMIA_T`num'_DENOM == 0 & ANEMIA_T3_DENOM_NOT >= 1 & ///
		ANEMIA_T3_DENOM_NOT <= 5 
		
		}
		
	tab ANEMIA_T1, m 
	tab ANEMIA_T2, m 
	tab ANEMIA_T3, m 
 
	
	preserve 
	
	keep newid ANEMIA_T1 ANEMIA_T2 ANEMIA_T3 ANEMIA_PNC6
	
	replace ANEMIA_PNC6 = 55 if ANEMIA_PNC6==.
	
	label define anem 0 "No anemia" 1 "Mild anemia" 2 "Moderate anemia" 3 "Severe anemia" ///
		55 "Missing" 77 "Pregnancy ended" 
		
	label values ANEMIA_T1 ANEMIA_T2 ANEMIA_T3 ANEMIA_PNC6 ///
		anem
	
	collapse (count) newid, by(ANEMIA_T1 ANEMIA_T2 ANEMIA_T3 ANEMIA_PNC6)
	

	sankey_plot ANEMIA_T1 ANEMIA_T2 ANEMIA_T3 ANEMIA_PNC6, wide width(newid) ///
		fillcolor(%50) ///
		xlabel("",nogrid) gap(0.1) tight ///
		title("Anemia Status by Trimesters through PNC6") ///
		subtitle ("Completed pregnancies recruited in Trimester 1")	
	
	
	
	
	
