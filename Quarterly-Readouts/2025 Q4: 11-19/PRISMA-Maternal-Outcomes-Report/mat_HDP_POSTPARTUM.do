*PRISMA Maternal Variable Construction Code
*Purpose: This code constructs hypertensive disorders in the postpartum period
*Original Version: July 19, 2024 by E Oakley (emoakley@gwu.edu)
*Update: October 23, 2024 by E Oakley (HDP_GROUP_POSTP has been moved to main code; this code is revised to focus on high BP in postpartum windows only)
*Update: June 15, 2025 - construct in early PNC windows (0, 1, 4)

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
global da "Z:/Stacked Data/2025-10-31" // change date here as needed

	// Working Files Folder (TNT-Drive)
global wrk "D:\Users\emoakley\Documents\working files-25-11-09" // set pathway here for where you want to save output data files (i.e., constructed analysis variables)

global OUT "D:\Users\emoakley\Documents\Outcome Data\2025-10-31" // UPDATE DATE AS NEEDED


global date "251111" // today's date

log using "$log/mat_outcome_HDP_postpartum_$date", replace

*************************************************************************

* * * Start by reviewing blood pressure measures in the postpartum period: 
	
	use "$wrk/BP_all_long"

	* Restrict to completed pregnancies: 
	keep if PREG_END == 1 
	
	
	* For this, we want the following:
		* BP measures collected by PRISMA staff in the postpartum period (based on visit date)
		* Post-L&D form (MNH10) which specifies BP after delivery (TIMING==2)
		* Hospitalizations in the postpartum period 
		
	keep if (VISIT_DATE > PREG_END_DATE) | ///
		(TIMING == 2) | (HOSP_TIMING == 2 & VISIT_DATE==.)
	
	* drop observations with visit type in pregnancy & missing date:  
	drop if VISIT_DATE == . & ((TYPE_VISIT >= 1 & TYPE_VISIT <=6) | TYPE_VISIT == 13)
	
	*Checks: review type visit: 
	tab TYPE_VISIT, m 
	
	*Drop GA indicator: 
	drop VISIT_GA 
	
	*Construct Days PP indicator: 
	gen VISIT_PP = VISIT_DATE - PREG_END_DATE 
	
	label var VISIT_PP "Days postpartum at visit"
	
	tab VISIT_PP, m 
	
	drop if VISIT_PP <-1 
	drop if VISIT_PP >= 500
	
	tab TIMING if VISIT_DATE == . 

	sort SITE
	histogram VISIT_PP if VISIT_PP < (52*7) & VISIT_PP >=0 
	
	/////
	///// Order the BP measures: 
		/// order file by person & date 
	sort MOMID PREGID VISIT_DATE TIMING

	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "BP Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of BP measures (postpartum)"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)
	/////
	/////
	
	*Create variables for high High BP & Severe High BP using the mean: 
	
	gen HIGH_BP_SYS = 0 if BP_SYS_VSORRES >0 & BP_SYS_VSORRES < 140
	replace HIGH_BP_SYS = 1 if BP_SYS_VSORRES >=140 & BP_SYS_VSORRES != . 
	
	gen SEVHIGH_BP_SYS = 0 if BP_SYS_VSORRES >0 & BP_SYS_VSORRES < 160 
	replace SEVHIGH_BP_SYS = 1 if BP_SYS_VSORRES >=160 & BP_SYS_VSORRES != .
	
	
	gen HIGH_BP_DIA = 0 if BP_DIA_VSORRES >0 & BP_DIA_VSORRES < 90
	replace HIGH_BP_DIA = 1 if BP_DIA_VSORRES >=90 & BP_DIA_VSORRES != . 
	
	gen SEVHIGH_BP_DIA = 0 if BP_DIA_VSORRES >0 & BP_DIA_VSORRES < 110 
	replace SEVHIGH_BP_DIA = 1 if BP_DIA_VSORRES >=110 & BP_DIA_VSORRES != .

	
	label var HIGH_BP_SYS "High systolic BP measure at the visit (mean)"
	label var HIGH_BP_DIA "High diastolic BP measure at the visit (mean)"
	label var SEVHIGH_BP_SYS "Severe high systolic BP measure at the visit (mean)"
	label var SEVHIGH_BP_DIA "Severe high diastolic BP measure at the visit (mean)"
	
	sort HIGH_BP_SYS
	
	by HIGH_BP_SYS: sum BP_SYS_VSORRES
	
	sort SEVHIGH_BP_SYS
	
	by SEVHIGH_BP_SYS: sum BP_SYS_VSORRES
	
	sort HIGH_BP_DIA
	
	by HIGH_BP_DIA: sum BP_DIA_VSORRES
	
	sort SEVHIGH_BP_DIA
	
	by SEVHIGH_BP_DIA: sum BP_DIA_VSORRES
	
	keep MOMID PREGID TIMING TYPE_VISIT VISIT_DATE ///
		ENTRY_NUM ENTRY_TOTAL BP_DIA_VSORRES BP_SYS_VSORRES ///
		HIGH_BP_SYS SEVHIGH_BP_SYS HIGH_BP_DIA SEVHIGH_BP_DIA VISIT_PP 
	
	*Next, convert to wide:

	reshape wide TIMING TYPE_VISIT VISIT_DATE  ///
		HIGH* SEVHIGH* BP_DIA_VSORRES BP_SYS_VSORRES VISIT_PP ///
		, i(MOMID PREGID ENTRY_TOTAL) j(ENTRY_NUM) 
		
	sum ENTRY_TOTAL
		
	*COUNTS: any BP measure:
	
	gen HIGH_BP_COUNT = 0 
	label var HIGH_BP_COUNT "Number of High BP readings"
	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_COUNT = HIGH_BP_COUNT + 1 if ///
		(HIGH_BP_DIA`num' == 1 | HIGH_BP_SYS`num' == 1)
		
	
	}
	
	tab HIGH_BP_COUNT, m 
	
	rename HIGH_BP_COUNT HIGH_BP_COUNT_PPALL
	
	gen SEVHIGH_BP_COUNT = 0 
	label var SEVHIGH_BP_COUNT "Number of severe high BP readings"
	
	foreach num of numlist 1/$i {
	
	replace SEVHIGH_BP_COUNT = SEVHIGH_BP_COUNT + 1 if ///
		(SEVHIGH_BP_DIA`num' == 1 | SEVHIGH_BP_SYS`num' == 1)
	
	}	

	tab SEVHIGH_BP_COUNT, m 
	
	rename SEVHIGH_BP_COUNT SEVHIGH_BP_COUNT_PPALL
	
	
	*COUNTS: High BP within 1-2 days of delivery:
	
	gen HIGH_BP_COUNT_PP1 = 0 
	label var HIGH_BP_COUNT_PP1 "Number of High BP readings at 1-2 days PP OR at post-delivery measure"
	
	gen ENTRY_TOTAL_PP1 = 0 
	label var ENTRY_TOTAL_PP1 "Total number of bp readings at 1-2 days PP OR at post-delivery measure"
	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_COUNT_PP1 = HIGH_BP_COUNT_PP1 + 1 if ///
		((VISIT_PP`num' >= 1 & VISIT_PP`num' <= 2) | TIMING`num'==2) & ///
		(HIGH_BP_DIA`num' == 1 | HIGH_BP_SYS`num' == 1)
		
	replace ENTRY_TOTAL_PP1 = ENTRY_TOTAL_PP1 + 1 if  ///
		((VISIT_PP`num' >= 1 & VISIT_PP`num' <= 2) | TIMING`num'==2) & ///
		(HIGH_BP_DIA`num' !=. | HIGH_BP_SYS`num' !=. ) 
	
	}
	
	tab ENTRY_TOTAL_PP1, m 
	tab HIGH_BP_COUNT_PP1, m 


	
	gen SEVHIGH_BP_COUNT_PP1 = 0 
	label var SEVHIGH_BP_COUNT_PP1 "Number of severe high BP readings at 1-2 days PP  OR at post-delivery measure"
	
	foreach num of numlist 1/$i {
	
	replace SEVHIGH_BP_COUNT_PP1 = SEVHIGH_BP_COUNT_PP1 + 1 if ///
		((VISIT_PP`num' >= 1 & VISIT_PP`num' <= 2) | TIMING`num'==2) & ///
		(SEVHIGH_BP_DIA`num' == 1 | SEVHIGH_BP_SYS`num' == 1)
	
	}	

	tab SEVHIGH_BP_COUNT_PP1, m 
	
	
	* * * * PNC-0: 3-5 days
	*COUNTS: High BP within 5-5 days of delivery:
	
	gen HIGH_BP_COUNT_PNC0 = 0 
	label var HIGH_BP_COUNT_PNC0 "Number of High BP readings at PNC-0 (3-5 days)"
	
	gen ENTRY_TOTAL_PNC0 = 0 
	label var ENTRY_TOTAL_PNC0 "Total number of BP readings at PNC-0 (3-5 day)"
	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_COUNT_PNC0 = HIGH_BP_COUNT_PNC0 + 1 if VISIT_PP`num' >=3 & ///
		VISIT_PP`num' <=5 & ///
		(HIGH_BP_DIA`num' == 1 | HIGH_BP_SYS`num' == 1)
		
	replace ENTRY_TOTAL_PNC0 = ENTRY_TOTAL_PNC0 + 1 if VISIT_PP`num' >=3 & ///
		VISIT_PP`num' <=5 & ///
		(HIGH_BP_DIA`num' !=. | HIGH_BP_SYS`num' !=. ) 
	
	}
	
	tab HIGH_BP_COUNT_PNC0, m 
	
	gen SEVHIGH_BP_COUNT_PNC0 = 0 
	label var SEVHIGH_BP_COUNT_PNC0 "Number of severe high BP readings at PNC-0 (3-5 days)"
	
	foreach num of numlist 1/$i {
	
	replace SEVHIGH_BP_COUNT_PNC0 = SEVHIGH_BP_COUNT_PNC0 + 1 if VISIT_PP`num' >=3 & ///
		VISIT_PP`num' <=5 & ///
		(SEVHIGH_BP_DIA`num' == 1 | SEVHIGH_BP_SYS`num' == 1)
	
	}	

	tab SEVHIGH_BP_COUNT_PNC0, m 
	
	
	* * * * PNC-1: 7-14 days
	*COUNTS: High BP within  7-14 days of delivery:
	
	gen HIGH_BP_COUNT_PNC1 = 0 
	label var HIGH_BP_COUNT_PNC1 "Number of High BP readings at PNC-1 (7-14 days)"
	
	gen ENTRY_TOTAL_PNC1 = 0 
	label var ENTRY_TOTAL_PNC1 "Total number of BP readings at PNC-1 (7-14 day)"

	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_COUNT_PNC1 = HIGH_BP_COUNT_PNC1 + 1 if VISIT_PP`num' >=7 & ///
		VISIT_PP`num' <=14 & ///
		(HIGH_BP_DIA`num' == 1 | HIGH_BP_SYS`num' == 1)
		
	replace ENTRY_TOTAL_PNC1 = ENTRY_TOTAL_PNC1 + 1 if VISIT_PP`num' >=7 & ///
		VISIT_PP`num' <=14 & ///
		(HIGH_BP_DIA`num' !=. | HIGH_BP_SYS`num' !=. ) 
	
	}
	
	tab HIGH_BP_COUNT_PNC1, m 
	
	gen SEVHIGH_BP_COUNT_PNC1 = 0 
	label var SEVHIGH_BP_COUNT_PNC1 "Number of severe high BP readings at PNC-1 (7-14 days)"
	
	foreach num of numlist 1/$i {
	
	replace SEVHIGH_BP_COUNT_PNC1 = SEVHIGH_BP_COUNT_PNC1 + 1 if VISIT_PP`num' >=7 & ///
		VISIT_PP`num' <=14 & ///
		(SEVHIGH_BP_DIA`num' == 1 | SEVHIGH_BP_SYS`num' == 1)
	
	}	

	tab SEVHIGH_BP_COUNT_PNC1, m 
	

	* * * * PNC-4: 28-35 days
	*COUNTS: High BP within  28-35 days of delivery:
	
	gen HIGH_BP_COUNT_PNC4 = 0 
	label var HIGH_BP_COUNT_PNC4 "Number of High BP readings at PNC-4 (28-35 days)"
	
	gen ENTRY_TOTAL_PNC4 = 0 
	label var ENTRY_TOTAL_PNC4 "Total number of BP readings at PNC-4 (28-35 days)"
	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_COUNT_PNC4 = HIGH_BP_COUNT_PNC4 + 1 if VISIT_PP`num' >=28 & ///
		VISIT_PP`num' <=35 & ///
		(HIGH_BP_DIA`num' == 1 | HIGH_BP_SYS`num' == 1)
		
	replace ENTRY_TOTAL_PNC4 = ENTRY_TOTAL_PNC4 + 1 if VISIT_PP`num' >=28 & ///
		VISIT_PP`num' <=35 & ///
		(HIGH_BP_DIA`num' !=. | HIGH_BP_SYS`num' !=. ) 
	
	}
	
	tab HIGH_BP_COUNT_PNC4, m 
	
	gen SEVHIGH_BP_COUNT_PNC4 = 0 
	label var SEVHIGH_BP_COUNT_PNC4 "Number of severe high BP readings at PNC-4 (28-35 days)"
	
	foreach num of numlist 1/$i {
	
	replace SEVHIGH_BP_COUNT_PNC4 = SEVHIGH_BP_COUNT_PNC4 + 1 if VISIT_PP`num' >=28 & ///
		VISIT_PP`num' <=35 & ///
		(SEVHIGH_BP_DIA`num' == 1 | SEVHIGH_BP_SYS`num' == 1)
	
	}	

	tab SEVHIGH_BP_COUNT_PNC4, m 
	
	/* Original code was for 2-42 days (now updating to PNC visit windows 0-4)
	*COUNTS: High BP within 3-42 days of delivery:
	
	gen HIGH_BP_COUNT_PP42 = 0 
	label var HIGH_BP_COUNT_PP42 "Number of High BP readings at 2-42 days PP"
	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_COUNT_PP42 = HIGH_BP_COUNT_PP42 + 1 if VISIT_PP`num' >=2 & ///
		VISIT_PP`num' <=42 & ///
		(HIGH_BP_DIA`num' == 1 | HIGH_BP_SYS`num' == 1)
	
	}
	
	tab HIGH_BP_COUNT_PP42, m 
	
	gen SEVHIGH_BP_COUNT_PP42 = 0 
	label var SEVHIGH_BP_COUNT_PP42 "Number of severe high BP readings at 2-42 days PP"
	
	foreach num of numlist 1/$i {
	
	replace SEVHIGH_BP_COUNT_PP42 = SEVHIGH_BP_COUNT_PP42 + 1 if VISIT_PP`num' >=2 & ///
		VISIT_PP`num' <=42 & ///
		(SEVHIGH_BP_DIA`num' == 1 | SEVHIGH_BP_SYS`num' == 1)
	
	}	

	tab SEVHIGH_BP_COUNT_PP42, m 

	
	*COUNTS: High BP after postpartum period (43+ days of delivery):
	
	gen ENTRY_TOTAL_PPLATE = 0 
	label var ENTRY_TOTAL_PPLATE "Total number of BP readings at more than 42 days PP"
	
	gen HIGH_BP_COUNT_PPLATE = 0 
	label var HIGH_BP_COUNT_PPLATE "Number of High BP readings at more than 42 days PP"
	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_COUNT_PPLATE = HIGH_BP_COUNT_PPLATE + 1 if VISIT_PP`num' >42 & ///
		VISIT_PP`num' !=. & ///
		(HIGH_BP_DIA`num' == 1 | HIGH_BP_SYS`num' == 1)
		
	replace ENTRY_TOTAL_PPLATE = ENTRY_TOTAL_PPLATE + 1 if VISIT_PP`num' >42 & ///
		VISIT_PP`num' !=. & ///
		(HIGH_BP_DIA`num' !=. | HIGH_BP_SYS`num' !=. ) 
	
	}
	
	tab HIGH_BP_COUNT_PPLATE, m 
	
	gen SEVHIGH_BP_COUNT_PPLATE = 0 
	label var SEVHIGH_BP_COUNT_PPLATE "Number of severe high BP readings at more than 42 days PP"
	
	foreach num of numlist 1/$i {
	
	replace SEVHIGH_BP_COUNT_PPLATE = SEVHIGH_BP_COUNT_PPLATE + 1 if VISIT_PP`num' >42 & ///
		VISIT_PP`num' !=. & ///
		(SEVHIGH_BP_DIA`num' == 1 | SEVHIGH_BP_SYS`num' == 1)
	
	}	

	tab SEVHIGH_BP_COUNT_PPLATE, m 
	*/ 
	
///////////////////////////////////////////////////////////////////////////
 * * * * Construct by visit window: 
//////////////////////////////////////////////////////////////////////////

	*COUNTS: High BP in PNC-6 Window: 
	
	gen ENTRY_TOTAL_PNC6 = 0 
	label var ENTRY_TOTAL_PNC6 "Total number of BP readings in PNC-6 window"
	
	gen HIGH_BP_COUNT_PNC6 = 0 
	label var HIGH_BP_COUNT_PNC6 "Number of High BP readings in PNC-6 window"
	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_COUNT_PNC6 = HIGH_BP_COUNT_PNC6 + 1 if VISIT_PP`num' >=42 & ///
		VISIT_PP`num' <=104 & ///
		(HIGH_BP_DIA`num' == 1 | HIGH_BP_SYS`num' == 1)
		
	replace ENTRY_TOTAL_PNC6 = ENTRY_TOTAL_PNC6 + 1 if VISIT_PP`num' >=42 & ///
		VISIT_PP`num' <=104 & ///
		(HIGH_BP_DIA`num' !=. | HIGH_BP_SYS`num' !=. ) 
	
	}
	
	tab ENTRY_TOTAL_PNC6, m 
	
	tab HIGH_BP_COUNT_PNC6, m 
	
	gen SEVHIGH_BP_COUNT_PNC6 = 0 
	label var SEVHIGH_BP_COUNT_PNC6 "Number of severe high BP readings in PNC-6 window"
	
	foreach num of numlist 1/$i {
	
	replace SEVHIGH_BP_COUNT_PNC6 = SEVHIGH_BP_COUNT_PNC6 + 1 if VISIT_PP`num' >=42 & ///
		VISIT_PP`num' <=104 & ///
		(SEVHIGH_BP_DIA`num' == 1 | SEVHIGH_BP_SYS`num' == 1)
	
	}	

	tab SEVHIGH_BP_COUNT_PNC6, m 
	
	
	*COUNTS: High BP in PNC-6 EARLY Window: 
	
	gen ENTRY_TOTAL_PNC6_E = 0 
	label var ENTRY_TOTAL_PNC6_E "Total number of BP readings in PNC-6 window - early"
	
	gen HIGH_BP_COUNT_PNC6_E = 0 
	label var HIGH_BP_COUNT_PNC6_E "Number of High BP readings in PNC-6 window - early"
	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_COUNT_PNC6_E = HIGH_BP_COUNT_PNC6_E + 1 if VISIT_PP`num' >=35 & ///
		VISIT_PP`num' <=104 & ///
		(HIGH_BP_DIA`num' == 1 | HIGH_BP_SYS`num' == 1)
		
	replace ENTRY_TOTAL_PNC6_E = ENTRY_TOTAL_PNC6_E + 1 if VISIT_PP`num' >=35 & ///
		VISIT_PP`num' <=104 & ///
		(HIGH_BP_DIA`num' !=. | HIGH_BP_SYS`num' !=. ) 
	
	}
	
	tab ENTRY_TOTAL_PNC6_E, m 
	
	tab HIGH_BP_COUNT_PNC6_E, m 
	
	gen SEVHIGH_BP_COUNT_PNC6_E = 0 
	label var SEVHIGH_BP_COUNT_PNC6_E "Number of severe high BP readings in PNC-6 window - early"
	
	foreach num of numlist 1/$i {
	
	replace SEVHIGH_BP_COUNT_PNC6_E = SEVHIGH_BP_COUNT_PNC6_E + 1 if VISIT_PP`num' >=35 & ///
		VISIT_PP`num' <=104 & ///
		(SEVHIGH_BP_DIA`num' == 1 | SEVHIGH_BP_SYS`num' == 1)
	
	}	

	tab SEVHIGH_BP_COUNT_PNC6_E, m 

	
	*COUNTS: High BP in PNC-26 Window: 
	
	gen ENTRY_TOTAL_PNC26 = 0 
	label var ENTRY_TOTAL_PNC26 "Total number of BP readings in PNC-26 window"
	
	gen HIGH_BP_COUNT_PNC26 = 0 
	label var HIGH_BP_COUNT_PNC26 "Number of High BP readings in PNC-26 window"
	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_COUNT_PNC26 = HIGH_BP_COUNT_PNC26 + 1 if VISIT_PP`num' >=182 & ///
		VISIT_PP`num' <=279 & ///
		(HIGH_BP_DIA`num' == 1 | HIGH_BP_SYS`num' == 1)
		
	replace ENTRY_TOTAL_PNC26 = ENTRY_TOTAL_PNC26 + 1 if VISIT_PP`num' >=182 & ///
		VISIT_PP`num' <=279 & ///
		(HIGH_BP_DIA`num' !=. | HIGH_BP_SYS`num' !=. ) 
	
	}
	
	tab ENTRY_TOTAL_PNC26, m 
	
	tab HIGH_BP_COUNT_PNC26, m 
	
	gen SEVHIGH_BP_COUNT_PNC26 = 0 
	label var SEVHIGH_BP_COUNT_PNC26 "Number of severe high BP readings in PNC-6 window"
	
	foreach num of numlist 1/$i {
	
	replace SEVHIGH_BP_COUNT_PNC26 = SEVHIGH_BP_COUNT_PNC26 + 1 if VISIT_PP`num' >=182 & ///
		VISIT_PP`num' <=279 & ///
		(SEVHIGH_BP_DIA`num' == 1 | SEVHIGH_BP_SYS`num' == 1)
	
	}	

	tab SEVHIGH_BP_COUNT_PNC26, m 
	
	
	*COUNTS: High BP in PNC-52 Window: 
	
	gen ENTRY_TOTAL_PNC52 = 0 
	label var ENTRY_TOTAL_PNC52 "Total number of BP readings in PNC-52 window"
	
	gen HIGH_BP_COUNT_PNC52 = 0 
	label var HIGH_BP_COUNT_PNC52 "Number of High BP readings in PNC-52 window"
	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_COUNT_PNC52 = HIGH_BP_COUNT_PNC52 + 1 if VISIT_PP`num' >=364 & ///
		VISIT_PP`num' <=454 & ///
		(HIGH_BP_DIA`num' == 1 | HIGH_BP_SYS`num' == 1)
		
	replace ENTRY_TOTAL_PNC52 = ENTRY_TOTAL_PNC52 + 1 if VISIT_PP`num' >=364 & ///
		VISIT_PP`num' <=454 & ///
		(HIGH_BP_DIA`num' !=. | HIGH_BP_SYS`num' !=. ) 
	
	}
	
	tab ENTRY_TOTAL_PNC52, m 
	
	tab HIGH_BP_COUNT_PNC52, m 
	
	gen SEVHIGH_BP_COUNT_PNC52 = 0 
	label var SEVHIGH_BP_COUNT_PNC52 "Number of severe high BP readings in PNC-6 window"
	
	foreach num of numlist 1/$i {
	
	replace SEVHIGH_BP_COUNT_PNC52 = SEVHIGH_BP_COUNT_PNC52 + 1 if VISIT_PP`num' >=364 & ///
		VISIT_PP`num' <=454 & ///
		(SEVHIGH_BP_DIA`num' == 1 | SEVHIGH_BP_SYS`num' == 1)
	
	}	

	tab SEVHIGH_BP_COUNT_PNC52, m 
	
	
	* save a copy of high BP tabulations: 
	save "$wrk/BP_postpartum_wide", replace 
	
	keep MOMID PREGID ENTRY_TOTAL HIGH_BP_COUNT* SEVHIGH_BP_COUNT* ///
		ENTRY_TOTAL_PP1 ///
		ENTRY_TOTAL_PNC0* HIGH_BP_COUNT_PNC0* SEVHIGH_BP_COUNT_PNC0* ///
		ENTRY_TOTAL_PNC1* HIGH_BP_COUNT_PNC1* SEVHIGH_BP_COUNT_PNC1* ///
		ENTRY_TOTAL_PNC4* HIGH_BP_COUNT_PNC4* SEVHIGH_BP_COUNT_PNC4* ///
		ENTRY_TOTAL_PNC6* HIGH_BP_COUNT_PNC6* SEVHIGH_BP_COUNT_PNC6* ///
		ENTRY_TOTAL_PNC26 HIGH_BP_COUNT_PNC26 SEVHIGH_BP_COUNT_PNC26 ///
		ENTRY_TOTAL_PNC52 HIGH_BP_COUNT_PNC52 SEVHIGH_BP_COUNT_PNC52 
	
	
	* save a copy of high BP tabulations: 
	save "$wrk/BP_tabulation_postpartum", replace 
	
	
* * * Second, look at treatment for high BP in the postpartum period: 
	
	* Hospitalizations in the postpartum period: 
	clear 
	import delimited "$da/mnh19_merged", bindquote(strict)
	
	rename momid MOMID 
	rename pregid PREGID 
	
	* merge in pregnancy endpoint data: 
	merge m:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing(PREG_END_DATE PREG_END)
		
		// restrict to completed pregnancies: 
		keep if PREG_END==1
		
		drop if _merge == 2 
		
		drop _merge 
	
	* restrict to hospitalizations in the postpartum period: 
		//create timing indicator: 
	gen HOSP_TIMING = m19_timing_ohocat
	label define hospt 1 "1-ANC" 2 "2-PNC" 77 "77-N/A"
	label values HOSP_TIMING hospt
	label var HOSP_TIMING "Timing of hospitalization"
	
		// create date indicator: 
	gen HOSP_DATE = date(m19_ohostdat, "YMD")
	replace HOSP_DATE = date(m19_mat_est_ohostdat, "YMD") if HOSP_DATE == . 
	replace HOSP_DATE = date(m19_admit_ohostdat, "YMD") if HOSP_DATE == . 
	
	replace HOSP_DATE = . if HOSP_DATE == date("07071907", "MDY")
	
	format HOSP_DATE %td 
	label var HOSP_DATE "Date of hospitalization"
	
	tab HOSP_DATE, m 
	
		// restrict: 
	keep if m19_timing_ohocat == 2 /// post-natal period timing OR 
		| (HOSP_DATE > PREG_END_DATE & HOSP_DATE != . & PREG_END_DATE != .) 
		// hospital date is after pregnancy end date 
		
	
	* Treatment for HDPs during hospitalization in the postpartum period: 
	tab m19_hpd_htn_cmoccur_1, m 
	
	gen HDP_TREAT_HOSP = 0  if m19_hpd_htn_cmoccur_1 == 1 | m19_hpd_htn_cmoccur_1 == 0 | ///
		m19_hpd_htn_cmoccur_77==1
	
	foreach num of numlist  3/10 {
	    
	replace HDP_TREAT_HOSP = 1 if m19_hpd_htn_cmoccur_`num' == 1 
	
	}
	
	label var HDP_TREAT_HOSP "Treated for HDP w/ a medication of interest during hospitalization"
	tab HDP_TREAT_HOSP, m 	
	
	*Check for Duplicates: 
	duplicates tag MOMID PREGID, gen(duplicate)
	tab duplicate, m 
	
	rename duplicate ENTRY_TOTAL 
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	
	*restrict to needed variables & reshape to wide 
	keep MOMID PREGID HOSP_DATE HOSP_TIMING PREG_END PREG_END_DATE ///
		HDP_TREAT_HOSP ENTRY_TOTAL 
		
	/// order file by person & date to order hospitalizations: 
	sort MOMID PREGID HOSP_DATE HOSP_TIMING 
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "Hospitalization Entry Number"
	
	tab ENTRY_NUM, m 
	
	reshape wide HOSP_DATE HOSP_TIMING HDP_TREAT_HOSP, ///
		i(MOMID PREGID ENTRY_TOTAL PREG_END PREG_END_DATE) j(ENTRY_NUM)
		
	sum ENTRY_TOTAL 
	return list 
	
	global i = r(max)
	
	foreach num of numlist 1/$i {
		
	label var HOSP_TIMING`num' "Hospitalization timing - postpartum event `num'"
	label var HOSP_DATE`num' "Hospitalization date - postpartum event `num'"
	
	label var HDP_TREAT_HOSP`num' "Treated for HDP w/ a medication of interest during hospitalization"
	
	}
	
	
	save "$wrk/HDP_treated_hosp_postpartum", replace 

	
	clear 
	
///////////////////////////////////////////////////////////
      * * * * Compile postpartum HDP Outcomes * * * * 
///////////////////////////////////////////////////////////

* * * Start with completed pregnancies: 
	use "$OUT/MAT_ENDPOINTS"
	
	keep if PREG_END==1
	
	*drop _merge 
	
* * * Merge in original HDP information: 
	
	merge 1:1 MOMID PREGID using "$OUT/MAT_HDP", keepusing(HDP_GROUP)
	
	tab HDP_GROUP, m 
	
	drop _merge 
	
* * * Merge in postpartum blood pressure readings: 
	
	merge 1:1 MOMID PREGID using "$wrk/BP_tabulation_postpartum"
	
	drop _merge 
	
* * * Merge in Hospitalizations 

	merge 1:1 MOMID PREGID using "$wrk/HDP_treated_hosp_postpartum"
	
	drop _merge 
	
			
	* Create a constructed outcome: 
		// postpartum hypertension: 2+ high BP; 1+ severe high BP; 1+ high BP 
		// with medication: 
	gen POSTP_HTN = 0 if HIGH_BP_COUNT_PPALL == 0 | HIGH_BP_COUNT_PPALL == 1 
	
	replace POSTP_HTN = 1 if HIGH_BP_COUNT_PPALL >= 2 & HIGH_BP_COUNT_PPALL !=.
	
	replace POSTP_HTN = 1 if SEVHIGH_BP_COUNT_PPALL >= 1 & SEVHIGH_BP_COUNT_PPALL != . 
	
	replace POSTP_HTN = 1 if HIGH_BP_COUNT_PPALL >= 1 & (HDP_TREAT_HOSP1 == 1 | ///
		HDP_TREAT_HOSP2 == 1)
		
	tab POSTP_HTN, m 
	
	tab POSTP_HTN HDP_GROUP, m 

	/*
	// postpartum hypertension outcome: 
	gen POSTP_HTN_42 = 0 if HIGH_BP_COUNT_PPLATE == 0 & ENTRY_TOTAL_PPLATE >= 1 & ///
		ENTRY_TOTAL_PPLATE != . 
	replace POSTP_HTN_42 = 1 if HIGH_BP_COUNT_PPLATE >= 1 & HIGH_BP_COUNT_PPLATE != . 
	
	label var POSTP_HTN_42 "Any high BP measure after 42 days postpartum"
	
	tab POSTP_HTN_42, m 
	
	tab POSTP_HTN_42 if PNC6_PASS_LATE == 1, m 
	*/
	
	
	* merge in closeout details: 
	merge 1:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing(STOP_DATE)
	
	drop if _merge == 2 


	* Set denominators for PNC High BP measures: 
	
	// PNC-6: early window: 
	gen POSTP_HTN_PNC6_E = 0 if ENTRY_TOTAL_PNC6_E >= 1 & ENTRY_TOTAL_PNC6_E != .  
	replace POSTP_HTN_PNC6_E = 1 if HIGH_BP_COUNT_PNC6_E >= 1 & ///
		HIGH_BP_COUNT_PNC6_E != . 
		
	label var POSTP_HTN_PNC6_E "High blood pressure at 5-14 weeks postpartum"
	
	tab POSTP_HTN_PNC6_E, m 
	
	gen POSTP_HTN_PNC6_E_DENOM = 0 
	replace POSTP_HTN_PNC6_E_DENOM = 1 if POSTP_HTN_PNC6_E != . | ///
			PNC6_PASS_LATE == 1
			
	replace POSTP_HTN_PNC6_E = 55 if POSTP_HTN_PNC6_E_DENOM == 1 & ///
		POSTP_HTN_PNC6_E == 1 
		
	
	// PNC 6, 26, 53: late windows: 
	
	foreach num of numlist 0 1 4 6 26 52 {
		
	gen POSTP_HTN_PNC`num' = 0 if ENTRY_TOTAL_PNC`num' >= 1 & ENTRY_TOTAL_PNC`num' != .  
	
	replace POSTP_HTN_PNC`num' = 1 if HIGH_BP_COUNT_PNC`num' >= 1 & ///
		HIGH_BP_COUNT_PNC`num' != . 
	
	tab POSTP_HTN_PNC`num', m 
	
	gen POSTP_HTN_PNC`num'_DENOM = 0 
	replace POSTP_HTN_PNC`num'_DENOM = 1 if POSTP_HTN_PNC`num' != . | ///
			PNC`num'_PASS_LATE == 1 
			
	replace POSTP_HTN_PNC`num'_DENOM = 0 if ///
		STOP_DATE < PNC`num'_LATE_WINDOW & STOP_DATE != . & ///
		POSTP_HTN_PNC`num'== . 
			
	replace POSTP_HTN_PNC`num' = 55 if POSTP_HTN_PNC`num'_DENOM == 1 & ///
		POSTP_HTN_PNC`num' == .
		
	replace POSTP_HTN_PNC`num' = 77 if (HDP_GROUP == 1 | HDP_GROUP == 4) /// chronic HTN (preexisting)
		& PNC`num'_PASS_LATE == 1

	replace POSTP_HTN_PNC`num'_DENOM = 0 if POSTP_HTN_PNC`num' == 77
	
	tab POSTP_HTN_PNC`num' POSTP_HTN_PNC`num'_DENOM, m 

	}
	
	label var POSTP_HTN_PNC0 "High blood pressure at 3-5 days postpartum"
	label var POSTP_HTN_PNC1 "High blood pressure at 7-14 days postpartum"
	label var POSTP_HTN_PNC4 "High blood pressure at 4-5 weeks postpartum"
	label var POSTP_HTN_PNC6 "High blood pressure at 6-14 weeks postpartum"	
	label var POSTP_HTN_PNC26 "High blood pressure at 26-39 weeks postpartum"	
	label var POSTP_HTN_PNC52 "High blood pressure at 52-64 weeks postpartum"	

	
	* Save a postpartum high-BP dataset: 
	
	*keep if PNC6_PASS_LATE == 1 

	
	keep MOMID PREGID SITE POSTP_HTN_PNC6 POSTP_HTN_PNC6_DENOM ///
		POSTP_HTN_PNC26 POSTP_HTN_PNC26_DENOM POSTP_HTN_PNC52 ///
		POSTP_HTN_PNC52_DENOM HDP_GROUP ///
		HIGH_BP_COUNT_PP1 SEVHIGH_BP_COUNT_PP1 ENTRY_TOTAL_PP1 ///
		POSTP_HTN_PNC0 POSTP_HTN_PNC0_DENOM POSTP_HTN_PNC1 ///
		POSTP_HTN_PNC1_DENOM POSTP_HTN_PNC4 POSTP_HTN_PNC4_DENOM 
		
	foreach num of numlist 0 1 4 6 26 52 {
		
	tab POSTP_HTN_PNC`num' if POSTP_HTN_PNC`num'_DENOM==1, m 
	
	tab POSTP_HTN_PNC`num' HDP_GROUP if POSTP_HTN_PNC`num'_DENOM==1, m 
		
	}
	
	drop HDP_GROUP 
	
	save "$OUT/MAT_POSTPARTUM_HIGHBP", replace 
