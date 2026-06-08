*PRISMA Maternal Variable Construction Code - High Hb
*Purpose: This code drafts variable construction code for maternal outcome
	*variables for the PRISMA study - High Hemoglobin Outcomes
*Note that this code is based on the datasets prepared in the maternal anemia 
	*outcome code for PRISMA (mat_outcomes_anemia_v1.1), which MUST be 
	*run first for this code to run. 
*Original Version: May 8, 2026 by E Oakley (emoakley@gwu.edu)

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
global dadate "2026-05-01" // this should be considered the date the data was most recently updated & will be used to calculate whether different gestational windows are completed
global da "Z:/Stacked Data/$dadate"

global OUT "Z:\Outcome Data/$dadate"


	// Working Files Folder (TNT-Drive)
global wrk "D:\Users\emoakley\Documents\Outcome Data\2026-05-01"

global date "260508" 

log using "$log/mat_outcome_construct_highHB_$date", replace


/* Maternal High HB Outcomes

	HB_HIGH_CBC_T1	- highest HB value in first trimester (CBC only)
	
	HB_HIGH_T1	- highest HB value in first trimester (including standalone POCs)
	
	HB_HIGH_CBC_T2	- highest HB value in second trimester (CBC only)
	
	HB_HIGH_T2	- highest HB value in second trimester (including standalone POCs)
	
	HB_HIGH_CBC_T3	- highest HB value in third trimester (CBC only)
	
	HB_HIGH_T3	- highest HB value in third trimester (including standalone POCs)
	
	HB_HIGH_CBC_ANC  - highest HB value in pregnancy (CBC only)

	HB_HIGH_ANC - highest HB value in pregnancy (including standalone POCs)
	
*/

	
//////////////////////////////////////////////////////////////////////////
 * * * * TRIMESTER 1 * * * * 
/////////////////////////////////////////////////////////////////////////

	clear 
	use "$wrk/ANEMIA_all_long"

	*CONSTRUCT: first trimester high HB 
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
		
	*Create indicator for highest CBC test: 	
	
	gen HB_HIGH_CBC_T1 = .
	gen HB_HIGH_CBC_T1_DT = .
		format HB_HIGH_CBC_T1_DT %td
	gen HB_HIGH_CBC_T1_GA = . 
	
	foreach num of numlist 1/$i {
			
		replace HB_HIGH_CBC_T1_DT = TEST_DATE`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_HIGH_CBC_T1 == . | (HB_HIGH_CBC_T1 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
		
		replace HB_HIGH_CBC_T1_GA = TEST_GA`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_HIGH_CBC_T1 == . | (HB_HIGH_CBC_T1 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
		
		replace HB_HIGH_CBC_T1 = HB_LBORRES`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_HIGH_CBC_T1 == . | (HB_HIGH_CBC_T1 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
	
	}
	
	tab HB_HIGH_CBC_T1, m 
	
	list if HB_HIGH_CBC_T1 == . 
	
	
	*Create indicator for HIGHest test of any type: 	
	
	gen HB_HIGH_T1 = .
	gen HB_HIGH_T1_DT = .
		format HB_HIGH_T1_DT %td
	gen HB_HIGH_T1_GA = . 
	
	gen HB_HIGH_T1_TEST = ""
	
	foreach num of numlist 1/$i {
			
		* overall loop for all tests: 
		replace HB_HIGH_T1_DT = TEST_DATE`num' if  ///
			(HB_HIGH_T1 == . | (HB_HIGH_T1 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
		
		replace HB_HIGH_T1_GA = TEST_GA`num' if  ///
			(HB_HIGH_T1 == . | (HB_HIGH_T1 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
			
		replace HB_HIGH_T1_TEST = TEST_TYPE`num' if  ///
			(HB_HIGH_T1 == . | (HB_HIGH_T1 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
		
		replace HB_HIGH_T1 = HB_LBORRES`num' if  ///
			(HB_HIGH_T1 == . | (HB_HIGH_T1 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
			
	}
	
	tab HB_HIGH_T1, m 
	tab HB_HIGH_T1_TEST, m 
	
	
	foreach num of numlist 1/$i {
		
		*ensure we default to CBC if the two tests are equal: 
		replace HB_HIGH_T1_DT = TEST_DATE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_HIGH_T1_TEST == "POC" & ///
			HB_LBORRES`num' == HB_HIGH_T1 
		
		replace HB_HIGH_T1_GA = TEST_GA`num' if  ///
			TEST_TYPE`num' == "CBC" & HB_HIGH_T1_TEST == "POC" & ///
			HB_LBORRES`num' == HB_HIGH_T1 
			
		replace HB_HIGH_T1_TEST = TEST_TYPE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_HIGH_T1_TEST == "POC" & ///
			HB_LBORRES`num' == HB_HIGH_T1 
			
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
	
		* * * Step 1: Set date boundaries for any HIGH test that is POC 
		gen POC_UPBOUND = HB_HIGH_T1_DT + 7 if HB_HIGH_T1_TEST == "POC"
		gen POC_LOWBOUND = HB_HIGH_T1_DT - 7 if HB_HIGH_T1_TEST == "POC"
		format POC_UPBOUND POC_LOWBOUND %td
		
		* * * Step 2: Run loop to identify any CBC test within the bounds 
		
		gen CBC_CONCURRENT = 0 if HB_HIGH_T1_TEST == "POC"
		
		foreach num of numlist 1/$i {
		
		replace CBC_CONCURRENT = 1 if HB_HIGH_T1_TEST == "POC" & ///
			TEST_TYPE`num' == "CBC" & ///
			TEST_DATE`num' >= POC_LOWBOUND & TEST_DATE`num' <= POC_UPBOUND
		
		}
		
		*Check the loop:
		list HB_HIGH_T1 HB_HIGH_T1_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if CBC_CONCURRENT == 1 
			
		* * * Step 3: Drop the HIGH POC test if concurrent CBC test:
		
		foreach num of numlist 1/$i {
		
		replace HB_LBORRES`num' = . if HB_LBORRES`num' == HB_HIGH_T1 & ///
			TEST_TYPE`num' == "POC" & CBC_CONCURRENT == 1 & ///
			TEST_DATE`num' == HB_HIGH_T1_DT
		
		}
		
		* * * Step 4: Update the loop for those with concurrent CBC
		replace HB_HIGH_T1 = . if CBC_CONCURRENT == 1 
		
		foreach num of numlist 1/$i {
				
			replace HB_HIGH_T1_DT = TEST_DATE`num' if  ///
				(HB_HIGH_T1 == . | (HB_HIGH_T1 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_HIGH_T1_GA = TEST_GA`num' if  ///
				(HB_HIGH_T1 == . | (HB_HIGH_T1 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
				
			replace HB_HIGH_T1_TEST = TEST_TYPE`num' if  ///
				(HB_HIGH_T1 == . | (HB_HIGH_T1 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_HIGH_T1 = HB_LBORRES`num' if  ///
				(HB_HIGH_T1 == . | (HB_HIGH_T1 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
		
		}
		
		tab HB_HIGH_T1, m 
		tab HB_HIGH_T1_TEST, m 
		
		*Re-check the loops:
		list HB_HIGH_T1 HB_HIGH_T1_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if CBC_CONCURRENT == 1 
			
		*Review those with a POC test only: 
		list HB_HIGH_T1 HB_HIGH_T1_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if HB_HIGH_T1_TEST == "POC"
			
		drop CBC_CONCURRENT POC_UPBOUND POC_LOWBOUND	
		
	}
		
	* * * * Trimester 1 clean up: restrict dataset:
	
	keep MOMID PREGID ENTRY_TOTAL POC_COUNT HB_HIGH_CBC* HB_HIGH_T* 
	
	order POC_COUNT, after(ENTRY_TOTAL) 
	
	label var HB_HIGH_CBC_T1 "HIGHest HB test by CBC - trimester 1"
	label var HB_HIGH_CBC_T1_DT "Date of HIGHest HB test by CBC - trimester 1"
	label var HB_HIGH_CBC_T1_GA "GA at HIGHest HB test by CBC - trimester 1"
	
	label var HB_HIGH_T1 "HIGHest HB test - trimester 1"
	label var HB_HIGH_T1_DT "Date of HIGHest HB test - trimester 1"
	label var HB_HIGH_T1_GA "GA at HIGHest HB test - trimester 1"
	label var HB_HIGH_T1_TEST "Test type for HIGHest HB measure - trimester 1"
	
	
	* CONSTRUCT HIGH HB STATUS MEASURES HERE: 
	
		*Trimester 1: 
			*>=150 = Very high Hb
			*130 - <150 = High Hb
			*110 - <130 = Normal Hb 
			*100 - <110 = Low Hb (mild anemia) 
			*70 -  <100 = Low Hb (moderate anemia)
			*<70 = Very low Hb (severe anemia) 
	
	gen HBCAT_CBC_T1 = 1 if  HB_HIGH_CBC_T1 >= 15.0 & HB_HIGH_CBC_T1 != .
	replace HBCAT_CBC_T1 = 2 if HB_HIGH_CBC_T1 >= 13.0 & HB_HIGH_CBC_T1 <15.0
	replace HBCAT_CBC_T1 = 3 if HB_HIGH_CBC_T1 >= 11.0 & HB_HIGH_CBC_T1 <13.0
	replace HBCAT_CBC_T1 = 4 if HB_HIGH_CBC_T1 >= 10.0 & HB_HIGH_CBC_T1 <11.0
	replace HBCAT_CBC_T1 = 5 if HB_HIGH_CBC_T1 >= 7.0 & HB_HIGH_CBC_T1 <10.0
	replace HBCAT_CBC_T1 = 6 if HB_HIGH_CBC_T1 <7.0 & HB_HIGH_CBC_T1 != .
	
	label var HBCAT_CBC_T1 "Highest HB level status in trimester 1 - CBC only"
	
	tab HBCAT_CBC_T1 
	
	histogram HB_HIGH_CBC_T1, by(HBCAT_CBC_T1)
	
	sort HBCAT_CBC_T1 
	by HBCAT_CBC_T1: sum HB_HIGH_CBC_T1
	
	gen HBCAT_CBC_T1_MISS = 0 if HBCAT_CBC_T1 != . 
	replace HBCAT_CBC_T1_MISS = 1 if HBCAT_CBC_T1 == . 
	
	label define anem_miss 0 "0-Non-missing" 1 "1-No valid test result" ///
		2 "2-No labs in window" 3 "3-GA information missing"
		
	label values HBCAT_CBC_T1_MISS anem_miss 
	label var HBCAT_CBC_T1_MISS "Missing reason - High HB T1 (CBC)"
	
	gen HBCAT_T1 = 1 if  HB_HIGH_T1 >= 15.0 & HB_HIGH_T1 != .
	replace HBCAT_T1 = 2 if HB_HIGH_T1 >= 13.0 & HB_HIGH_T1 <15.0
	replace HBCAT_T1 = 3 if HB_HIGH_T1 >= 11.0 & HB_HIGH_T1 <13.0
	replace HBCAT_T1 = 4 if HB_HIGH_T1 >= 10.0 & HB_HIGH_T1 <11.0
	replace HBCAT_T1 = 5 if HB_HIGH_T1 >= 7.0 & HB_HIGH_T1 <10.0
	replace HBCAT_T1 = 6 if HB_HIGH_T1 <7.0 & HB_HIGH_T1 != .
	
	label var HBCAT_T1 "Highest HB level status in trimester 1"
	
	gen HBCAT_T1_MISS = 0 if HBCAT_T1 != . 
	replace HBCAT_T1_MISS = 1 if HBCAT_T1 == . 
		
	label values HBCAT_T1_MISS anem_miss 
	label var HBCAT_T1_MISS "Missing reason -High HB T1"
	
	sort HBCAT_T1 
	by HBCAT_T1: sum HB_HIGH_T1
	
	save "$wrk/HIGHHB_t1", replace 
	

	
//////////////////////////////////////////////////////////////////////////
 * * * * TRIMESTER 2 * * * * 
/////////////////////////////////////////////////////////////////////////

	clear 
	use "$wrk/ANEMIA_all_long"

	*CONSTRUCT: second trimester high HB 
	*GA for 2nd trimester: Day 98 thruogh Day 195
	
	*First, restrict to window: must be within T2 AND must be a test during pregnancy 
	keep if TEST_TIMING == 0 & TEST_GA >=98 & TEST_GA <196
			
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
		
	*Create indicator for highest CBC test: 	
	
	gen HB_HIGH_CBC_T2 = .
	gen HB_HIGH_CBC_T2_DT = .
		format HB_HIGH_CBC_T2_DT %td
	gen HB_HIGH_CBC_T2_GA = . 
	
	foreach num of numlist 1/$i {
			
		replace HB_HIGH_CBC_T2_DT = TEST_DATE`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_HIGH_CBC_T2 == . | (HB_HIGH_CBC_T2 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
		
		replace HB_HIGH_CBC_T2_GA = TEST_GA`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_HIGH_CBC_T2 == . | (HB_HIGH_CBC_T2 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
		
		replace HB_HIGH_CBC_T2 = HB_LBORRES`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_HIGH_CBC_T2 == . | (HB_HIGH_CBC_T2 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
	
	}
	
	tab HB_HIGH_CBC_T2, m 
	
	list if HB_HIGH_CBC_T2 == . 
	
	
	*Create indicator for HIGHest test of any type: 	
	
	gen HB_HIGH_T2 = .
	gen HB_HIGH_T2_DT = .
		format HB_HIGH_T2_DT %td
	gen HB_HIGH_T2_GA = . 
	
	gen HB_HIGH_T2_TEST = ""
	
	foreach num of numlist 1/$i {
			
		* overall loop for all tests: 
		replace HB_HIGH_T2_DT = TEST_DATE`num' if  ///
			(HB_HIGH_T2 == . | (HB_HIGH_T2 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
		
		replace HB_HIGH_T2_GA = TEST_GA`num' if  ///
			(HB_HIGH_T2 == . | (HB_HIGH_T2 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
			
		replace HB_HIGH_T2_TEST = TEST_TYPE`num' if  ///
			(HB_HIGH_T2 == . | (HB_HIGH_T2 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
		
		replace HB_HIGH_T2 = HB_LBORRES`num' if  ///
			(HB_HIGH_T2 == . | (HB_HIGH_T2 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
			
	}
	
	tab HB_HIGH_T2, m 
	tab HB_HIGH_T2_TEST, m 
	
	
	foreach num of numlist 1/$i {
		
		*ensure we default to CBC if the two tests are equal: 
		replace HB_HIGH_T2_DT = TEST_DATE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_HIGH_T2_TEST == "POC" & ///
			HB_LBORRES`num' == HB_HIGH_T2 
		
		replace HB_HIGH_T2_GA = TEST_GA`num' if  ///
			TEST_TYPE`num' == "CBC" & HB_HIGH_T2_TEST == "POC" & ///
			HB_LBORRES`num' == HB_HIGH_T2 
			
		replace HB_HIGH_T2_TEST = TEST_TYPE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_HIGH_T2_TEST == "POC" & ///
			HB_LBORRES`num' == HB_HIGH_T2 
			
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
	
		* * * Step 1: Set date boundaries for any HIGH test that is POC 
		gen POC_UPBOUND = HB_HIGH_T2_DT + 7 if HB_HIGH_T2_TEST == "POC"
		gen POC_LOWBOUND = HB_HIGH_T2_DT - 7 if HB_HIGH_T2_TEST == "POC"
		format POC_UPBOUND POC_LOWBOUND %td
		
		* * * Step 2: Run loop to identify any CBC test within the bounds 
		
		gen CBC_CONCURRENT = 0 if HB_HIGH_T2_TEST == "POC"
		
		foreach num of numlist 1/$i {
		
		replace CBC_CONCURRENT = 1 if HB_HIGH_T2_TEST == "POC" & ///
			TEST_TYPE`num' == "CBC" & ///
			TEST_DATE`num' >= POC_LOWBOUND & TEST_DATE`num' <= POC_UPBOUND
		
		}
		
		*Check the loop:
		list HB_HIGH_T2 HB_HIGH_T2_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if CBC_CONCURRENT == 1 
			
		* * * Step 3: Drop the HIGH POC test if concurrent CBC test:
		
		foreach num of numlist 1/$i {
		
		replace HB_LBORRES`num' = . if HB_LBORRES`num' == HB_HIGH_T2 & ///
			TEST_TYPE`num' == "POC" & CBC_CONCURRENT == 1 & ///
			TEST_DATE`num' == HB_HIGH_T2_DT
		
		}
		
		* * * Step 4: Update the loop for those with concurrent CBC
		replace HB_HIGH_T2 = . if CBC_CONCURRENT == 1 
		
		foreach num of numlist 1/$i {
				
			replace HB_HIGH_T2_DT = TEST_DATE`num' if  ///
				(HB_HIGH_T2 == . | (HB_HIGH_T2 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_HIGH_T2_GA = TEST_GA`num' if  ///
				(HB_HIGH_T2 == . | (HB_HIGH_T2 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
				
			replace HB_HIGH_T2_TEST = TEST_TYPE`num' if  ///
				(HB_HIGH_T2 == . | (HB_HIGH_T2 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_HIGH_T2 = HB_LBORRES`num' if  ///
				(HB_HIGH_T2 == . | (HB_HIGH_T2 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
		
		}
		
		tab HB_HIGH_T2, m 
		tab HB_HIGH_T2_TEST, m 
		
		*Re-check the loops:
		list HB_HIGH_T2 HB_HIGH_T2_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if CBC_CONCURRENT == 1 
			
		*Review those with a POC test only: 
		list HB_HIGH_T2 HB_HIGH_T2_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if HB_HIGH_T2_TEST == "POC"
			
		drop CBC_CONCURRENT POC_UPBOUND POC_LOWBOUND	
		
	}
		
	* * * * Trimester 2 clean up: restrict dataset:
	
	keep MOMID PREGID ENTRY_TOTAL POC_COUNT HB_HIGH_CBC* HB_HIGH_T* 
	
	order POC_COUNT, after(ENTRY_TOTAL) 
	
	label var HB_HIGH_CBC_T2 "HIGHest HB test by CBC - trimester 2"
	label var HB_HIGH_CBC_T2_DT "Date of HIGHest HB test by CBC - trimester 2"
	label var HB_HIGH_CBC_T2_GA "GA at HIGHest HB test by CBC - trimester 2"
	
	label var HB_HIGH_T2 "HIGHest HB test - trimester 2"
	label var HB_HIGH_T2_DT "Date of HIGHest HB test - trimester 2"
	label var HB_HIGH_T2_GA "GA at HIGHest HB test - trimester 2"
	label var HB_HIGH_T2_TEST "Test type for HIGHest HB measure - trimester 2"
	
	
	* CONSTRUCT HIGH HB STATUS MEASURES HERE: 
	
		*Trimester 2: 
			*>=150 = Very high Hb
			*130 - <150 = High Hb
			*105 - <130 = Normal Hb 
			*95 -  <105 = Low Hb (mild anemia) 
			*70 -  <95 = Low Hb (moderate anemia)
			*<70 = Very low Hb (severe anemia) 
	
	gen HBCAT_CBC_T2 = 1 if  HB_HIGH_CBC_T2 >= 15.0 & HB_HIGH_CBC_T2 != .
	replace HBCAT_CBC_T2 = 2 if HB_HIGH_CBC_T2 >= 13.0 & HB_HIGH_CBC_T2 <15.0
	replace HBCAT_CBC_T2 = 3 if HB_HIGH_CBC_T2 >= 10.5 & HB_HIGH_CBC_T2 <13.0
	replace HBCAT_CBC_T2 = 4 if HB_HIGH_CBC_T2 >= 9.5 & HB_HIGH_CBC_T2 <10.5
	replace HBCAT_CBC_T2 = 5 if HB_HIGH_CBC_T2 >= 7.0 & HB_HIGH_CBC_T2 <9.5
	replace HBCAT_CBC_T2 = 6 if HB_HIGH_CBC_T2 <7.0 & HB_HIGH_CBC_T2 != .
	
	label var HBCAT_CBC_T2 "Highest HB level status in trimester 2 - CBC only"
	
	tab HBCAT_CBC_T2 
	
	histogram HB_HIGH_CBC_T2, by(HBCAT_CBC_T2)
	
	sort HBCAT_CBC_T2 
	by HBCAT_CBC_T2: sum HB_HIGH_CBC_T2
	
	gen HBCAT_CBC_T2_MISS = 0 if HBCAT_CBC_T2 != . 
	replace HBCAT_CBC_T2_MISS = 1 if HBCAT_CBC_T2 == . 
	
	label define anem_miss 0 "0-Non-missing" 1 "1-No valid test result" ///
		2 "2-No labs in window" 3 "3-GA information missing"
		
	label values HBCAT_CBC_T2_MISS anem_miss 
	label var HBCAT_CBC_T2_MISS "Missing reason - High HB T2 (CBC)"
	
	gen HBCAT_T2 = 1 if  HB_HIGH_T2 >= 15.0 & HB_HIGH_T2 != .
	replace HBCAT_T2 = 2 if HB_HIGH_T2 >= 13.0 & HB_HIGH_T2 <15.0
	replace HBCAT_T2 = 3 if HB_HIGH_T2 >= 10.5 & HB_HIGH_T2 <13.0
	replace HBCAT_T2 = 4 if HB_HIGH_T2 >= 9.5 & HB_HIGH_T2 <10.5
	replace HBCAT_T2 = 5 if HB_HIGH_T2 >= 7.0 & HB_HIGH_T2 <9.5
	replace HBCAT_T2 = 6 if HB_HIGH_T2 <7.0 & HB_HIGH_T2 != .
	
	label var HBCAT_T2 "Highest HB level status in trimester 2"
	
	gen HBCAT_T2_MISS = 0 if HBCAT_T2 != . 
	replace HBCAT_T2_MISS = 1 if HBCAT_T2 == . 
		
	label values HBCAT_T2_MISS anem_miss 
	label var HBCAT_T2_MISS "Missing reason -High HB T2"
	
	sort HBCAT_T2 
	by HBCAT_T2: sum HB_HIGH_T2
	
	save "$wrk/HIGHHB_t2", replace 

			
	
//////////////////////////////////////////////////////////////////////////
 * * * * TRIMESTER 3 * * * * 
/////////////////////////////////////////////////////////////////////////

	clear 
	use "$wrk/ANEMIA_all_long"

	*CONSTRUCT: third trimester high HB 
	*GA for 3rd trimester: Day 196 through Delivery 
	
	*First, restrict to window: must be within T3 AND must be a test during pregnancy 
	keep if TEST_TIMING == 0 & TEST_GA >=196 & TEST_GA!=. 
			
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
		
	*Create indicator for highest CBC test: 	
	
	gen HB_HIGH_CBC_T3 = .
	gen HB_HIGH_CBC_T3_DT = .
		format HB_HIGH_CBC_T3_DT %td
	gen HB_HIGH_CBC_T3_GA = . 
	
	foreach num of numlist 1/$i {
			
		replace HB_HIGH_CBC_T3_DT = TEST_DATE`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_HIGH_CBC_T3 == . | (HB_HIGH_CBC_T3 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
		
		replace HB_HIGH_CBC_T3_GA = TEST_GA`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_HIGH_CBC_T3 == . | (HB_HIGH_CBC_T3 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
		
		replace HB_HIGH_CBC_T3 = HB_LBORRES`num' if TEST_TYPE`num' == "CBC" & ///
			(HB_HIGH_CBC_T3 == . | (HB_HIGH_CBC_T3 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
	
	}
	
	tab HB_HIGH_CBC_T3, m 
	
	list if HB_HIGH_CBC_T3 == . 
	
	
	*Create indicator for HIGHest test of any type: 	
	
	gen HB_HIGH_T3 = .
	gen HB_HIGH_T3_DT = .
		format HB_HIGH_T3_DT %td
	gen HB_HIGH_T3_GA = . 
	
	gen HB_HIGH_T3_TEST = ""
	
	foreach num of numlist 1/$i {
			
		* overall loop for all tests: 
		replace HB_HIGH_T3_DT = TEST_DATE`num' if  ///
			(HB_HIGH_T3 == . | (HB_HIGH_T3 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
		
		replace HB_HIGH_T3_GA = TEST_GA`num' if  ///
			(HB_HIGH_T3 == . | (HB_HIGH_T3 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
			
		replace HB_HIGH_T3_TEST = TEST_TYPE`num' if  ///
			(HB_HIGH_T3 == . | (HB_HIGH_T3 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
		
		replace HB_HIGH_T3 = HB_LBORRES`num' if  ///
			(HB_HIGH_T3 == . | (HB_HIGH_T3 < HB_LBORRES`num')) & HB_LBORRES`num'!=.
			
	}
	
	tab HB_HIGH_T3, m 
	tab HB_HIGH_T3_TEST, m 
	
	
	foreach num of numlist 1/$i {
		
		*ensure we default to CBC if the two tests are equal: 
		replace HB_HIGH_T3_DT = TEST_DATE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_HIGH_T3_TEST == "POC" & ///
			HB_LBORRES`num' == HB_HIGH_T3 
		
		replace HB_HIGH_T3_GA = TEST_GA`num' if  ///
			TEST_TYPE`num' == "CBC" & HB_HIGH_T3_TEST == "POC" & ///
			HB_LBORRES`num' == HB_HIGH_T3 
			
		replace HB_HIGH_T3_TEST = TEST_TYPE`num' if ///
			TEST_TYPE`num' == "CBC" & HB_HIGH_T3_TEST == "POC" & ///
			HB_LBORRES`num' == HB_HIGH_T3 
			
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
	
		* * * Step 1: Set date boundaries for any HIGH test that is POC 
		gen POC_UPBOUND = HB_HIGH_T3_DT + 7 if HB_HIGH_T3_TEST == "POC"
		gen POC_LOWBOUND = HB_HIGH_T3_DT - 7 if HB_HIGH_T3_TEST == "POC"
		format POC_UPBOUND POC_LOWBOUND %td
		
		* * * Step 2: Run loop to identify any CBC test within the bounds 
		
		gen CBC_CONCURRENT = 0 if HB_HIGH_T3_TEST == "POC"
		
		foreach num of numlist 1/$i {
		
		replace CBC_CONCURRENT = 1 if HB_HIGH_T3_TEST == "POC" & ///
			TEST_TYPE`num' == "CBC" & ///
			TEST_DATE`num' >= POC_LOWBOUND & TEST_DATE`num' <= POC_UPBOUND
		
		}
		
		*Check the loop:
		list HB_HIGH_T3 HB_HIGH_T3_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if CBC_CONCURRENT == 1 
			
		* * * Step 3: Drop the HIGH POC test if concurrent CBC test:
		
		foreach num of numlist 1/$i {
		
		replace HB_LBORRES`num' = . if HB_LBORRES`num' == HB_HIGH_T3 & ///
			TEST_TYPE`num' == "POC" & CBC_CONCURRENT == 1 & ///
			TEST_DATE`num' == HB_HIGH_T3_DT
		
		}
		
		* * * Step 4: Update the loop for those with concurrent CBC
		replace HB_HIGH_T3 = . if CBC_CONCURRENT == 1 
		
		foreach num of numlist 1/$i {
				
			replace HB_HIGH_T3_DT = TEST_DATE`num' if  ///
				(HB_HIGH_T3 == . | (HB_HIGH_T3 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_HIGH_T3_GA = TEST_GA`num' if  ///
				(HB_HIGH_T3 == . | (HB_HIGH_T3 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
				
			replace HB_HIGH_T3_TEST = TEST_TYPE`num' if  ///
				(HB_HIGH_T3 == . | (HB_HIGH_T3 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
			
			replace HB_HIGH_T3 = HB_LBORRES`num' if  ///
				(HB_HIGH_T3 == . | (HB_HIGH_T3 > HB_LBORRES`num')) & ///
				HB_LBORRES`num' != . & CBC_CONCURRENT == 1 
		
		}
		
		tab HB_HIGH_T3, m 
		tab HB_HIGH_T3_TEST, m 
		
		*Re-check the loops:
		list HB_HIGH_T3 HB_HIGH_T3_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if CBC_CONCURRENT == 1 
			
		*Review those with a POC test only: 
		list HB_HIGH_T3 HB_HIGH_T3_DT TEST_TYPE1 TEST_DATE1 HB_LBORRES1 ///
			TEST_TYPE2 TEST_DATE2 HB_LBORRES2 ///
			if HB_HIGH_T3_TEST == "POC"
			
		drop CBC_CONCURRENT POC_UPBOUND POC_LOWBOUND	
		
	}
		
	* * * * Trimester 3 clean up: restrict dataset:
	
	keep MOMID PREGID ENTRY_TOTAL POC_COUNT HB_HIGH_CBC* HB_HIGH_T* 
	
	order POC_COUNT, after(ENTRY_TOTAL) 
	
	label var HB_HIGH_CBC_T3 "HIGHest HB test by CBC - trimester 3"
	label var HB_HIGH_CBC_T3_DT "Date of HIGHest HB test by CBC - trimester 3"
	label var HB_HIGH_CBC_T3_GA "GA at HIGHest HB test by CBC - trimester 3"
	
	label var HB_HIGH_T3 "HIGHest HB test - trimester 3"
	label var HB_HIGH_T3_DT "Date of HIGHest HB test - trimester 3"
	label var HB_HIGH_T3_GA "GA at HIGHest HB test - trimester 3"
	label var HB_HIGH_T3_TEST "Test type for HIGHest HB measure - trimester 3"
	
	
	* CONSTRUCT HIGH HB STATUS MEASURES HERE: 
	
		*Trimester 3: 
			*>=150 = Very high Hb
			*130 - <150 = High Hb
			*110 - <130 = Normal Hb 
			*100 - <110 = Low Hb (mild anemia) 
			*70 -  <100 = Low Hb (moderate anemia)
			*<70 = Very low Hb (severe anemia) 
	
	gen HBCAT_CBC_T3 = 1 if  HB_HIGH_CBC_T3 >= 15.0 & HB_HIGH_CBC_T3 != .
	replace HBCAT_CBC_T3 = 2 if HB_HIGH_CBC_T3 >= 13.0 & HB_HIGH_CBC_T3 <15.0
	replace HBCAT_CBC_T3 = 3 if HB_HIGH_CBC_T3 >= 11.0 & HB_HIGH_CBC_T3 <13.0
	replace HBCAT_CBC_T3 = 4 if HB_HIGH_CBC_T3 >= 10.0 & HB_HIGH_CBC_T3 <11.0
	replace HBCAT_CBC_T3 = 5 if HB_HIGH_CBC_T3 >= 7.0 & HB_HIGH_CBC_T3 <10.0
	replace HBCAT_CBC_T3 = 6 if HB_HIGH_CBC_T3 <7.0 & HB_HIGH_CBC_T3 != .
	
	label var HBCAT_CBC_T3 "Highest HB level status in trimester 3 - CBC only"
	
	tab HBCAT_CBC_T3 
	
	histogram HB_HIGH_CBC_T3, by(HBCAT_CBC_T3)
	
	sort HBCAT_CBC_T3 
	by HBCAT_CBC_T3: sum HB_HIGH_CBC_T3
	
	gen HBCAT_CBC_T3_MISS = 0 if HBCAT_CBC_T3 != . 
	replace HBCAT_CBC_T3_MISS = 1 if HBCAT_CBC_T3 == . 
	
	label define anem_miss 0 "0-Non-missing" 1 "1-No valid test result" ///
		2 "2-No labs in window" 3 "3-GA information missing"
		
	label values HBCAT_CBC_T3_MISS anem_miss 
	label var HBCAT_CBC_T3_MISS "Missing reason - High HB T3 (CBC)"
	
	gen HBCAT_T3 = 1 if  HB_HIGH_T3 >= 15.0 & HB_HIGH_T3 != .
	replace HBCAT_T3 = 2 if HB_HIGH_T3 >= 13.0 & HB_HIGH_T3 <15.0
	replace HBCAT_T3 = 3 if HB_HIGH_T3 >= 11.0 & HB_HIGH_T3 <13.0
	replace HBCAT_T3 = 4 if HB_HIGH_T3 >= 10.0 & HB_HIGH_T3 <11.0
	replace HBCAT_T3 = 5 if HB_HIGH_T3 >= 7.0 & HB_HIGH_T3 <10.0
	replace HBCAT_T3 = 6 if HB_HIGH_T3 <7.0 & HB_HIGH_T3 != .
	
	label var HBCAT_T3 "Highest HB level status in trimester 3"
	
	gen HBCAT_T3_MISS = 0 if HBCAT_T3 != . 
	replace HBCAT_T3_MISS = 1 if HBCAT_T3 == . 
		
	label values HBCAT_T3_MISS anem_miss 
	label var HBCAT_T3_MISS "Missing reason -High HB T3"
	
	sort HBCAT_T3 
	by HBCAT_T3: sum HB_HIGH_T3
	
	save "$wrk/HIGHHB_t3", replace 
	



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
		
	*Merge in HIGH HB datasets:
	
	foreach l in HIGHHB_t1 HIGHHB_t2 HIGHHB_t3 {
		
	merge 1:1 MOMID PREGID using "$wrk/`l'"
	
	drop if _merge == 2 
	
	gen `l'_NONE = 1 if _merge == 1 
	
	drop _merge 
		
		}
		
		
//////////////////////////////////////////////////////////////////////////
 * * * * Any Time in Pregnancy * * * * 
/////////////////////////////////////////////////////////////////////////
	
	* ALL TESTS: 
	
	foreach var of varlist HBCAT_T1 HBCAT_T2 HBCAT_T3 {
	    
	replace `var' = -55 if `var' == 55 
	replace `var' = -77 if `var' == 77 
		
	}
	
	gen hbcat_min = min( HBCAT_T1, HBCAT_T2, HBCAT_T3 ) 
	
		gen hbcat_min_t1 = 1 if hbcat_min == HBCAT_T1 & hbcat_min !=.
		gen hbcat_min_t2 = 1 if hbcat_min == HBCAT_T2 & hbcat_min !=. 
		gen hbcat_min_t3 = 1 if hbcat_min == HBCAT_T3 & hbcat_min !=. 
		
		egen hbcat_min_points= rowtotal(hbcat_min_t1 hbcat_min_t2 hbcat_min_t3) ///
			if hbcat_min !=.
	
	gen hb_max = max(HB_HIGH_T1, HB_HIGH_T2, HB_HIGH_T3) if hbcat_min_t2 ==1 
	replace hb_max = max(HB_HIGH_T1, HB_HIGH_T3) if hbcat_min_t2==. 
	
		gen hb_max_t1 = 1 if hb_max == HB_HIGH_T1 & hb_max !=. & hbcat_min_t1 == 1
		gen hb_max_t2 = 1 if hb_max == HB_HIGH_T2 & hb_max !=. & hbcat_min_t2 == 1 
		gen hb_max_t3 = 1 if hb_max == HB_HIGH_T3 & hb_max !=. & hbcat_min_t3 == 1 
		
		egen hb_max_points= rowtotal(hb_max_t1 hb_max_t2 hb_max_t3) ///
			if hb_max !=.
			
	gen hbcat_anc_timing = .
	
	foreach num of numlist 1/3 {
	
    * If only one highest HB status: choose that trimester
	replace hbcat_anc_timing = `num' if hbcat_min_t`num' == 1 & hbcat_min_points == 1 
	
	* If multiple trimesters with the same HB status: choose the trimester with HIGHEST HB 
	replace hbcat_anc_timing = `num' if hbcat_min_t`num' == 1 & hbcat_min_points >1 & ///
		hb_max_points == 1 & hb_max_t`num' == 1 & hbcat_min_points!= . 
		
	* If multiple trimesters with the same HB status & hb: choose the earlier trimester 
	replace hbcat_anc_timing = `num' if hbcat_min_t`num' == 1 & hbcat_min_points >1 & ///
		hb_max_t`num' == 1 & hb_max_points > 1 & hbcat_min_points != . & ///
		hb_max_points != . & ///
		hbcat_anc_timing == . 
		
	}
	
	tab hbcat_anc_timing, m 
	
	gen HBCAT_ANC = . 
	gen HBCAT_ANC_MISS = .
	gen HB_HIGH_ANC = .
	gen HB_HIGH_ANC_DT = .
	format HB_HIGH_ANC_DT %td 
	gen HB_HIGH_ANC_GA = .
	gen HB_HIGH_ANC_TEST = ""
	
	foreach num of numlist 1/3 {
	
	replace HBCAT_ANC = HBCAT_T`num' if hbcat_anc_timing == `num'
	replace HBCAT_ANC_MISS = HBCAT_T`num'_MISS if hbcat_anc_timing == `num'
	replace HB_HIGH_ANC = HB_HIGH_T`num' if hbcat_anc_timing == `num'
	replace HB_HIGH_ANC_DT = HB_HIGH_T`num'_DT if hbcat_anc_timing == `num'
	replace HB_HIGH_ANC_GA = HB_HIGH_T`num'_GA if hbcat_anc_timing == `num'
	replace HB_HIGH_ANC_TEST = HB_HIGH_T`num'_TEST if hbcat_anc_timing == `num'
	
	}
	
	tab HBCAT_ANC, m 
	
	tab HBCAT_ANC HBCAT_T1, m 
	tab HBCAT_ANC HBCAT_T2, m 
	tab HBCAT_ANC HBCAT_T3, m 

	
	* CBC TESTS
	
	foreach var of varlist HBCAT_CBC_T1 HBCAT_CBC_T2 HBCAT_CBC_T3 {
	    
	replace `var' = -55 if `var' == 55 
	replace `var' = -77 if `var' == 77 
		
	}
	
	gen hbcat_min_CBC = min(HBCAT_CBC_T1, HBCAT_CBC_T2, HBCAT_CBC_T3) 
	
		gen hbcat_min_CBC_T1 = 1 if hbcat_min_CBC == HBCAT_CBC_T1 & hbcat_min_CBC !=.
		gen hbcat_min_CBC_T2 = 1 if hbcat_min_CBC == HBCAT_CBC_T2 & hbcat_min_CBC !=. 
		gen hbcat_min_CBC_T3 = 1 if hbcat_min_CBC == HBCAT_CBC_T3 & hbcat_min_CBC !=. 
		
		egen hbcat_min_CBC_points= rowtotal(hbcat_min_CBC_T1 hbcat_min_CBC_T2 hbcat_min_CBC_T3) ///
			if hbcat_min_CBC!=.
	
	gen hb_min_CBC = max(HB_HIGH_CBC_T1, HB_HIGH_CBC_T2, HB_HIGH_CBC_T3) if hbcat_min_CBC_T2 ==1 
	replace hb_min_CBC = max(HB_HIGH_CBC_T1, HB_HIGH_CBC_T3) if hbcat_min_CBC_T2==. 
	
		gen hb_min_CBC_T1 = 1 if hb_min_CBC == HB_HIGH_CBC_T1 & hb_min_CBC !=. & hbcat_min_CBC_T1 == 1
		gen hb_min_CBC_T2 = 1 if hb_min_CBC == HB_HIGH_CBC_T2 & hb_min_CBC !=. & hbcat_min_CBC_T2 == 1 
		gen hb_min_CBC_T3 = 1 if hb_min_CBC == HB_HIGH_CBC_T3 & hb_min_CBC !=. & hbcat_min_CBC_T3 == 1 
		
		egen hb_min_CBC_points= rowtotal(hb_min_CBC_T1 hb_min_CBC_T2 hb_min_CBC_T3) ///
			if hb_min_CBC !=.
			
	gen hbcat_anc_timing_CBC = .
	
	foreach num of numlist 1/3 {
	
    * If only one worst anemia status: choose that trimester
	replace hbcat_anc_timing_CBC = `num' if hbcat_min_CBC_T`num' == 1 & hbcat_min_CBC_points == 1 
	
	* If multiple trimesters with the same anemia status: choose the trimester with HIGHest HB 
	replace hbcat_anc_timing_CBC = `num' if hbcat_min_CBC_T`num' == 1 & hbcat_min_CBC_points >1 & ///
		hb_min_CBC_points == 1 & hb_min_CBC_T`num' == 1 & hbcat_min_CBC_points!= . 
		
	* If multiple trimesters with the same anemia status & hb: choose the earlier trimester 
	replace hbcat_anc_timing_CBC = `num' if hbcat_min_CBC_T`num' == 1 & hbcat_min_CBC_points >1 & ///
		hb_min_CBC_T`num' == 1 & hb_min_CBC_points > 1 & hbcat_min_CBC_points != . & ///
		hb_min_CBC_points != . & ///
		hbcat_anc_timing_CBC == . 
		
	}
	
	tab hbcat_anc_timing_CBC, m 
	
	gen HBCAT_CBC_ANC  = . 
	gen HBCAT_CBC_ANC_MISS = .
	gen HB_HIGH_CBC_ANC = .
	gen HB_HIGH_CBC_ANC_DT = .
	format HB_HIGH_CBC_ANC_DT %td 
	gen HB_HIGH_CBC_ANC_GA  = .
	
	foreach num of numlist 1/3 {
	
	replace HBCAT_CBC_ANC = HBCAT_CBC_T`num' if hbcat_anc_timing_CBC == `num'
	replace HBCAT_CBC_ANC_MISS = HBCAT_CBC_T`num'_MISS if hbcat_anc_timing_CBC == `num'
	replace HB_HIGH_CBC_ANC = HB_HIGH_CBC_T`num' if hbcat_anc_timing_CBC == `num'
	replace HB_HIGH_CBC_ANC_DT = HB_HIGH_CBC_T`num'_DT if hbcat_anc_timing_CBC == `num'
	replace HB_HIGH_CBC_ANC_GA = HB_HIGH_CBC_T`num'_GA if hbcat_anc_timing_CBC == `num'
	
	}
	
	tab HBCAT_CBC_ANC, m 
	
	tab HBCAT_CBC_ANC HBCAT_CBC_T1, m 
	tab HBCAT_CBC_ANC HBCAT_CBC_T2, m 
	tab HBCAT_CBC_ANC HBCAT_CBC_T3, m
	 
	
	* CHECK:
	list HBCAT_T1 HB_HIGH_T1 HBCAT_T2 HB_HIGH_T2 HBCAT_T3 HB_HIGH_T3 ///
		hbcat_anc_timing HBCAT_ANC HB_HIGH_ANC 
	
	
	list HBCAT_CBC_T1 HB_HIGH_CBC_T1 HBCAT_CBC_T2 HB_HIGH_CBC_T2 HBCAT_CBC_T3 ///
		HB_HIGH_CBC_T3 hbcat_anc_timing_CBC HBCAT_CBC_ANC HB_HIGH_CBC_ANC 	 
	 
	 
	 drop hbcat_min* hb_min* 

		
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
		gen HBCAT_T1_DENOM = 0 
		replace HBCAT_T1_DENOM = 1 if ENROLL_T1 == 1 & COMPLETE_T1 == 1 & ///
			PROCEED_T1 == 1 & PREG_CONT_T1 == 1
			
			// NOTE: We will keep in denominator if the person has any labs:
		replace HBCAT_T1_DENOM = 1 if HB_HIGH_T1 != . 
		
			// Those who are in the denominator, but missing, we will add to missing & missing indicator 
		replace HBCAT_T1 = 55 if HBCAT_T1_DENOM == 1 & HBCAT_T1 == . 
		
			// Those who are in the denom but had no labs at all = 2 
		replace HBCAT_T1_MISS = 2 if HBCAT_T1_DENOM == 1 & HBCAT_T1 == 55 & ///
			HBCAT_T1_MISS != 1 
			
		*CHECKS: 
		tab HBCAT_T1_MISS HBCAT_T1, m 
		tab HBCAT_T1_MISS HBCAT_T1_DENOM,m 
			
		label var HBCAT_T1_DENOM "Denominator for Trimester 1 - High HB Catgory"
		
		
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
		gen HBCAT_T2_DENOM = 0 
		replace HBCAT_T2_DENOM = 1 if COMPLETE_T2 == 1 & ///
			PROCEED_T2 == 1 & PREG_CONT_T2 == 1
			
			// NOTE: We will keep in denominator if the person has any labs:
		replace HBCAT_T2_DENOM = 1 if HB_HIGH_T2 != . 
		
			// Those who are in the denominator, but missing, we will add to missing & missing indicator 
		replace HBCAT_T2 = 55 if HBCAT_T2_DENOM == 1 & HBCAT_T2 == . 
		
			// Those who are in the denom but had no labs at all = 2 
		replace HBCAT_T2_MISS = 2 if HBCAT_T2_DENOM == 1 & HBCAT_T2 == 55 & ///
			HBCAT_T2_MISS != 1 
			
		*CHECKS: 
		tab HBCAT_T2_MISS HBCAT_T2, m 
		tab HBCAT_T2_MISS HBCAT_T2_DENOM,m 
			
		label var HBCAT_T2_DENOM "Denominator for Trimester 2 - High HB Category"
		
		
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
		gen HBCAT_T3_DENOM = 0 
		replace HBCAT_T3_DENOM = 1 if COMPLETE_T3 == 1 & ///
			PROCEED_T3 == 1 & PREG_CONT_T3 == 1
			
			// NOTE: We will keep in denominator if the person has any labs:
		replace HBCAT_T3_DENOM = 1 if HB_HIGH_T3 != . 
		
			// Those who are in the denominator, but missing, we will add to missing & missing indicator 
		replace HBCAT_T3 = 55 if HBCAT_T3_DENOM == 1 & HBCAT_T3 == . 
		
			// Those who are in the denom but had no labs at all = 2 
		replace HBCAT_T3_MISS = 2 if HBCAT_T3_DENOM == 1 & HBCAT_T3 == 55 & ///
			HBCAT_T3_MISS != 1 
			
		*CHECKS: 
		tab HBCAT_T3_MISS HBCAT_T3, m 
		tab HBCAT_T3_MISS HBCAT_T3_DENOM,m 
			
		label var HBCAT_T3_DENOM "Denominator for Trimester 3 - High HB Category"
		
		
		/////////////////////////////////////////////
		*Reasons for NOT being in the denominator:
		gen HBCAT_T3_DENOM_NOT = 0 if HBCAT_T3_DENOM == 1 
			// 1=pregnancy ended prior to 3rd trimester: 
		replace HBCAT_T3_DENOM_NOT = 1 if HBCAT_T3_DENOM == 0 & ///
			HBCAT_T3_DENOM_NOT == . & PREG_END == 1 & PREG_END_GA < 196 & ///
			PREG_END_GA != . 
			// 2=closeout/stop prior to end of 3rd trimester: 
		replace HBCAT_T3_DENOM_NOT = 2 if HBCAT_T3_DENOM == 0 & ///
			HBCAT_T3_DENOM_NOT == . & ((CLOSEOUT == 1 & PREG_END == 0 & ///
			CLOSEOUT_GA <= 280) | ///
			(MAT_DEATH==1 & PREG_END==1 & PREG_LOSS_DEATH == 1 & MAT_DEATH_GA <= 280))
			// 3=not yet completed study window: 
		replace HBCAT_T3_DENOM_NOT = 3 if HBCAT_T3_DENOM == 0 & ///
			HBCAT_T3_DENOM_NOT == . & COMPLETE_T3 == 0 & ///
			PREG_START_DATE != . 
			// 4=missing GA info
		replace HBCAT_T3_DENOM_NOT = 4 if HBCAT_T3_DENOM ==  0 & ///
			HBCAT_T3_DENOM_NOT == . & PREG_START_DATE == . 
			// 5=pregnancy end or maternal death with unknown timing: 
		replace HBCAT_T3_DENOM_NOT = 5 if HBCAT_T3_DENOM == 0 & ///
			HBCAT_T3_DENOM_NOT == . & ///
			((PREG_END == 1 & PREG_END_DATE == .) | ///
			(MAT_DEATH == 1 & MAT_DEATH_DATE == . & PREG_END_DATE == .))
			// 6=suspected date error
		replace HBCAT_T3_DENOM_NOT = 6 if HBCAT_T3_DENOM == 0 & ///
			HBCAT_T3_DENOM_NOT == . & ///
			(CLOSEOUT_GA < PREG_END_GA)
		
		// review those with deaths/closeouts before the window ends: 
		tab HBCAT_T3_DENOM_NOT HBCAT_T3_DENOM, m 
		list if HBCAT_T3_DENOM_NOT == . 
		
		
		label define t3anem 0 "0-In denominator" 1 "1-Pregnancy ended prior to T3" ///
			2 "2-Closeout prior to end of T3" 3 "3-Not yet completed T3" ///
			4 "4-Missing US GA info" 5 "5-End/Death with missing date" ///
			6 "6-Date error (closeout)"
			
		label var HBCAT_T3_DENOM_NOT "Reason not in the High HB Category T3 denominator"
		
		label values HBCAT_T3_DENOM_NOT t3anem
		
		tab HBCAT_T2_DENOM HBCAT_T3_DENOM,m 
		
			*review reason missing by trimester: 
		tab HBCAT_T3_DENOM_NOT HBCAT_T2_DENOM, m 
		tab HBCAT_T3_DENOM_NOT HBCAT_T1_DENOM, m 
		

* Fix missing indicator for HBCAT_ANC overall (denom = PREG_END): 

	tab HBCAT_ANC if PREG_END==1, m 
	
	replace HBCAT_ANC=55 if HBCAT_ANC == . 
	
	*No lab forms: 
	replace HBCAT_ANC_MISS = 2 if HBCAT_ANC == 55 & PREG_END==1 & ///
		HBCAT_ANC_MISS == . 
		
	tab HBCAT_ANC_MISS, m 
	tab HBCAT_ANC_MISS if PREG_END==1, m 
		
		
	*UPDATE for final dataset: 
	
	drop *_NONE ENTRY_TOTAL POC_COUNT M01_US_OHOSTDAT STR_PREG_START_DATE ///
		EST_CONCEP_DATE_US EST_CONCEP_DATE_LMP GA_DIFF_DAYS US_GA_WKS_ENROLL ///
		US_GA_DAYS_ENROLL LMP_GA_WKS_ENROLL LMP_GA_DAYS_ENROLL INVALID_SCRN_DATE ///
		PREG_LOSS_DEATH hb_max hb_max_t1 hb_max_t2 hb_max_t3 hb_max_points ///
		hbcat_anc_timing hbcat_anc_timing_CBC MISS_PREG_START_DATE
	
	drop ENROLL_SCRN_DATE ENROLL BOE_GA_DAYS PREG_START_DATE ///
	EDD_BOE PREG_END PREG_END_GA PREG_END_DATE PREG_LOSS PREG_LOSS_INDUCED ///
	CLOSEOUT CLOSEOUT_DT CLOSEOUT_GA CLOSEOUT_TYPE MAT_DEATH MAT_DEATH_DATE ///
	MAT_DEATH_GA STOP_DATE
	
	drop BOE_* ENROLL_* *_WINDOW *_PASS_*
	
		
		drop REMAPP_*


	**** FINALIZE DATASET & SAVE: 
	save "$wrk/MAT_HIGHHB", replace 
	
	
	save "$OUT/MAT_HIGHHB", replace

