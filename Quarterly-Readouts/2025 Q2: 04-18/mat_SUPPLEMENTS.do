*PRISMA Variable Construction - Supplements reported at ANC
*Purpose: This code constructs supplement variables by trimester, as requested by IHME
*Original Version: October 31, 2024
*Update: January 15, 2025 (for the 1-10-2025 data)

clear
set more off
cap log close

*Directory structure:

	// Erin's folders: 
global dir  "D:\Users\emoakley\Documents\Maternal Outcome Construction" 
global log "$dir/logs"
global do "$dir/do"
global output "$dir/output"

	// Stacked Data Folders (TNT Drive) - raw data 
	
global dadate "2025-04-18"
global da "Z:/Stacked Data/$dadate" // change date here as needed

	// Working Files Folder (TNT-Drive)
global wrk "Z:/Erin_working_files/data" // set pathway here 

global OUT "Z:\Outcome Data/$dadate"

global date "250512" // today's date

log using "$log/construct_supplements_$date", replace

/*************************************************************************

*This file constructs the following variables: 
	
	MAT_FOLIC_T1, T2, T3
		Received folic acid supplement in T1,2,3
	
	MAT_IRON_T1, T2, T3 
		Received oral iron supplement in T1,2,3
	
	MAT_IFA_T1, T2, T3
		Received iron folic acid supplement in T1,2,3
	
	MAT_IVIRON_T1, T2, T3
		Received IV iron in T1,2,3
	
	MAT_CALCIUM_T1, T2, T3
		Received calcium supplement in T1,2,3
	
	MAT_VITA_T1, T2, T3
		Received Vitamin A supplement in T1,2,3
	
	MAT_ZINC_T1, T2, T3
		Received zinc supplement in T1,2,3
	
	MAT_MMS_T1, T2, T3
		Received MMS supplement in T1,2,3
		
	MAT_NON_IRON_T1, T2, T3 
		Received any non-iron supplement in T1,2,3
		(includes folic acid, calcium, zinc, vitamin A)
			
*/
	
	/////////////////////////////////////////
	
	* Import data: 
	
	import delimited "$da/mnh04_merged", varn(1) case(preserve) bindquote(strict)
	
	rename M04_* *
		
	tab FOLIC_ACID_CMOCCUR, m 
	
	tab IRON_ORAL_CMOCCUR, m 
	
		tab IRON_ORAL_DOSAGE, m 
	
	tab IFA_CMOCCUR, m 
	
		tab IFA_DOSAGE
	
	tab IRON_IV_CMOCCUR, m 
	
		tab IRON_IV_TYPE, m 
		tab IRON_IV_DOSAGE, m 
	
	tab CALCIUM_CMOCCUR, m 
	
	tab VITAMIN_A_CMOCCUR, m 
	
	tab ZINC_CMOCCUR, m 
	
	tab MICRONUTRIENT_CMOCCUR, m 
	
	
	gen MAT_FOLIC = FOLIC_ACID_CMOCCUR
	gen MAT_IRON = IRON_ORAL_CMOCCUR 
	gen MAT_IFA = IFA_CMOCCUR 
	gen MAT_IVIRON = IRON_IV_CMOCCUR
	gen MAT_CALCIUM = CALCIUM_CMOCCUR 
	gen MAT_VITA = VITAMIN_A_CMOCCUR
	gen MAT_ZINC = ZINC_CMOCCUR 
	gen MAT_MMS = MICRONUTRIENT_CMOCCUR
	
	*Check on weird missingness: 
	tab MAT_IRON SITE, m 
	tab MAT_IVIRON SITE, m 
	
	*set to unknown for missingness in Zambia: 
	replace MAT_IRON = 55 if MAT_IRON == . 
	replace MAT_IVIRON = 55 if MAT_IVIRON == .
	
	*drop incomplete visits: 
	keep if MAT_VISIT_MNH04 == 1 | MAT_VISIT_MNH04 == 2 
	
	*pull in MAT_ENROLL for visit GA 
	merge m:1 MOMID PREGID using "$OUT/MAT_ENROLL"
	
	drop if _merge == 1 // drop the un-enrolled
	
	drop _merge 
	
	*visit GA: 
	gen VISIT_DATE = date(ANC_OBSSTDAT, "YMD")
	label var VISIT_DATE "Date of ANC visit"
	
	rename PREG_START_DATE PREG_START_DATE_string 
	
	gen PREG_START_DATE = date(PREG_START_DATE_string, "YMD")
	
	gen VISIT_GA = VISIT_DATE - PREG_START_DATE 
	label var VISIT_GA "Gestational age at visit (days)"
	
	sum VISIT_GA
	
	tab VISIT_GA 
	
	replace VISIT_GA = . if VISIT_GA <0
	replace VISIT_GA = . if VISIT_GA > 316
	
	*review visit type if GA is missing: 
	tab TYPE_VISIT if VISIT_GA == . 
	
	
	* Trimester variables: 
	
	gen VISIT_TRIMESTER = 1 if VISIT_GA >0 & VISIT_GA < 98 
	replace VISIT_TRIMESTER = 2 if VISIT_GA >=98 & VISIT_GA <196 
	replace VISIT_TRIMESTER = 3 if VISIT_GA >=196 & VISIT_GA != . 
	
	tab VISIT_TRIMESTER, m 
	
	label var VISIT_TRIMESTER "Visit trimester"
	
	*address visit trimester if GA is missing:
		
		*ANC-20 can be placed in T2 (visit window is 126-181 days)
		replace VISIT_TRIMESTER = 2 if TYPE_VISIT == 2 & VISIT_GA == . 
		
		*ANC-32 and ANC-36 can be placed in T3 (visit windows span 217-272 days)
		replace VISIT_TRIMESTER = 3 if (TYPE_VISIT == 4 | TYPE_VISIT == 5) & ///
			VISIT_GA == .
			
		*Enrollment visit can be estimated using the GA at enrollment: 
		replace VISIT_TRIMESTER = 1 if TYPE_VISIT == 1 & VISIT_GA == . & ///
			BOE_GA_DAYS_ENROLL >=0 & BOE_GA_DAYS_ENROLL <98
			
		replace VISIT_TRIMESTER = 2 if TYPE_VISIT == 1 & VISIT_GA == . & ///
			BOE_GA_DAYS_ENROLL >=98 & BOE_GA_DAYS_ENROLL <140
			
		*Remaining are unknown: 
		replace VISIT_TRIMESTER = 55 if VISIT_TRIMESTER == . & VISIT_GA == . 
		
		*review outcome variables for those missing trimester value: 
		list TYPE_VISIT MAT_* if VISIT_TRIMESTER == 55
	
	
	* Prep for collapse: 
	
	foreach var of varlist MAT_FOLIC MAT_IRON MAT_IFA MAT_IVIRON ///
		MAT_CALCIUM MAT_VITA MAT_ZINC MAT_MMS {
	
	replace `var' = 55 if `var' == . 
	
	replace `var' = -55 if `var' == 99
	replace `var' = -55 if `var' == 77 
	replace `var' = -55 if `var' == 55
	
		}
	
	*By trimester: Collapse max 
	
	foreach num of numlist 1/3 55 {

	preserve 
	
	keep if VISIT_TRIMESTER == `num'
	
	keep SITE MOMID PREGID MAT_FOLIC MAT_IRON MAT_IFA MAT_IVIRON MAT_CALCIUM ///
		MAT_VITA MAT_ZINC MAT_MMS IRON_ORAL_DOSAGE IFA_DOSAGE 
	
	collapse (max) MAT_FOLIC MAT_IRON MAT_IFA MAT_IVIRON MAT_CALCIUM ///
		MAT_VITA MAT_ZINC MAT_MMS IRON_ORAL_DOSAGE IFA_DOSAGE, by(SITE MOMID PREGID)
		
	foreach var of varlist MAT_FOLIC MAT_IRON MAT_IFA MAT_IVIRON MAT_CALCIUM ///
		MAT_VITA MAT_ZINC MAT_MMS IRON_ORAL_DOSAGE IFA_DOSAGE {
		    
	rename `var' `var'_T`num'
	
		}
		
	*clean up dosage variables - oral iron supplement: 
	replace IRON_ORAL_DOSAGE_T`num' = -7 if MAT_IRON_T`num' == 0 & ///
		(IRON_ORAL_DOSAGE_T`num' == -5 | IRON_ORAL_DOSAGE_T`num' == 55 | ///
		 IRON_ORAL_DOSAGE_T`num' == 77 | IRON_ORAL_DOSAGE_T`num' == 0)

	replace IRON_ORAL_DOSAGE_T`num' = -5 if MAT_IRON_T`num' == -55 
		
	replace IRON_ORAL_DOSAGE_T`num' = -5 if MAT_IRON_T`num' == 1 & ///
		(IRON_ORAL_DOSAGE_T`num' == -7 | IRON_ORAL_DOSAGE_T`num' == 0)
		
	*clean up dosage variables - IFA supplement: 
	replace IFA_DOSAGE_T`num' = -7 if MAT_IFA_T`num' == 0 & ///
		(IFA_DOSAGE_T`num' == -5 | IFA_DOSAGE_T`num' == 55 | ///
		 IFA_DOSAGE_T`num' == 77 | IFA_DOSAGE_T`num' == 0)

	replace IFA_DOSAGE_T`num' = -5 if MAT_IFA_T`num' == -55 
		
	replace IFA_DOSAGE_T`num' = -5 if MAT_IFA_T`num' == 1 & ///
		(IFA_DOSAGE_T`num' == -7 | IFA_DOSAGE_T`num' == 0)
		
		
	label var MAT_FOLIC_T`num' "Received folic acid supplements at ANC (during Trimester `num')"
	label var MAT_IRON_T`num' "Received oral iron supplements at ANC (during Trimester `num')"
	label var MAT_IFA_T`num' "Received iron folic acid supplements at ANC (during Trimester `num')"
	label var MAT_IVIRON_T`num' "Received IV Iron at ANC (during Trimester `num')"
	label var MAT_CALCIUM_T`num' "Received calcium supplement at ANC (during Trimester `num')"
	label var MAT_VITA_T`num' "Received Vitamin A supplement at ANC (during Trimester `num')"
	label var MAT_ZINC_T`num' "Received zinc supplement at ANC (during Trimester `num')"
	label var MAT_MMS_T`num' "Received MMS at ANC (during Trimester `num')"
	
	label var IRON_ORAL_DOSAGE_T`num' "Maximum iron dosage received for oral iron supplements (Trimester `num')"
	label var IFA_DOSAGE_T`num' "Maximum iron dosage received for oral IFA supplements (Trimester `num')"
	
	
	* Mother received any non-iron supplement: 
	gen MAT_NON_IRON_T`num' = 55
	replace MAT_NON_IRON_T`num' = 0 if ///
		MAT_FOLIC_T`num' == 0 & MAT_CALCIUM_T`num' == 0 & ///
		MAT_VITA_T`num' == 0 & MAT_ZINC_T`num' == 0 
	replace MAT_NON_IRON_T`num' = 1 if ///
		MAT_FOLIC_T`num' == 1 | MAT_CALCIUM_T`num' == 1 | ///
		MAT_VITA_T`num' == 1 | MAT_ZINC_T`num' == 1
		
	label var MAT_NON_IRON_T`num' "Received any non-iron supplement at ANC (during Trimester `num')"
	
	
	*Mother did not receieve any supplement: 
	gen MAT_NO_SUPP_T`num' = 55
	
	replace MAT_NO_SUPP_T`num' = 1 if ///
		MAT_FOLIC_T`num' == 0 & MAT_CALCIUM_T`num' == 0 & ///
		MAT_VITA_T`num' == 0 & MAT_ZINC_T`num' == 0 & ///
		MAT_IRON_T`num' == 0 & MAT_IFA_T`num' == 0 & ///
		MAT_IVIRON_T`num' == 0 & MAT_MMS_T`num' == 0 
		
	replace MAT_NO_SUPP_T`num' = 0 if ///
		MAT_FOLIC_T`num' == 1 | MAT_CALCIUM_T`num' == 1 | ///
		MAT_VITA_T`num' == 1 | MAT_ZINC_T`num' == 1 | ///
		MAT_IRON_T`num' == 1 | MAT_IFA_T`num' == 1 | ///
		MAT_IVIRON_T`num' == 1 | MAT_MMS_T`num' == 1 		
		
	label var MAT_NO_SUPP_T`num' "Mother did not receive any supplements at ANC (during Trimester `num')"
		
	
	save "$wrk/supplements_T`num'", replace
	
	restore 
	
	
	}
	
	
	clear 
	
	use "$OUT/MAT_ENROLL"
	
	keep SITE MOMID PREGID ENROLL BOE_GA_DAYS_ENROLL 
	
	label var ENROLL "Enrolled indicator"
	label var BOE_GA_DAYS_ENROLL "Gestational age at enrollment (days)"
	
	merge 1:1 MOMID PREGID using "$wrk/supplements_T1"
	
	foreach var of varlist MAT_FOLIC_T1 MAT_IRON_T1 MAT_IFA_T1 MAT_IVIRON_T1 ///
		MAT_CALCIUM_T1 MAT_VITA_T1 MAT_ZINC_T1 MAT_MMS_T1 MAT_NON_IRON_T1 ///
		MAT_NO_SUPP_T1 {
	
	replace `var' = 55 if `var' == -55 
	replace `var' = 77 if `var' == . & _merge == 1
	
	tab `var', m 
 	
		}

	drop _merge 
		
	merge 1:1 MOMID PREGID using "$wrk/supplements_T2"
	
	foreach var of varlist MAT_FOLIC_T2 MAT_IRON_T2 MAT_IFA_T2 MAT_IVIRON_T2 ///
		MAT_CALCIUM_T2 MAT_VITA_T2 MAT_ZINC_T2 MAT_MMS_T2 MAT_NON_IRON_T2 ///
		MAT_NO_SUPP_T2 {
	
	replace `var' = 55 if `var' == -55 
	replace `var' = 77 if `var' == . & _merge == 1
	
	tab `var', m 
 	
		}
	
	drop _merge 
		
	merge 1:1 MOMID PREGID using "$wrk/supplements_T3"
	
	foreach var of varlist MAT_FOLIC_T3 MAT_IRON_T3 MAT_IFA_T3 MAT_IVIRON_T3 ///
		MAT_CALCIUM_T3 MAT_VITA_T3 MAT_ZINC_T3 MAT_MMS_T3  MAT_NON_IRON_T3 ///
		MAT_NO_SUPP_T3 {
	
	replace `var' = 55 if `var' == -55 
	replace `var' = 77 if `var' == . & _merge == 1
	
	tab `var', m 
 	
		}	
	
	drop _merge 
	
	*We have one observation with valid info in the unknown timing group; 
	*we'll review the additional data for this person here:
	
	list if PREGID=="KEARC00074_P1"
	
	*Will discuss with ERS how to treat this person
	
	tab IRON_ORAL_DOSAGE_T1 MAT_IRON_T1, m 
	tab IFA_DOSAGE_T1 MAT_IFA_T1, m 
	
	tab IRON_ORAL_DOSAGE_T2 MAT_IRON_T2, m 
	tab IFA_DOSAGE_T2 MAT_IFA_T2, m 
	
	tab IRON_ORAL_DOSAGE_T3 MAT_IRON_T3, m 
	tab IFA_DOSAGE_T3 MAT_IFA_T3, m 
	
	tab 
	
	save "$OUT/MAT_ANC_SUPPLEMENT", replace
	
	
	
	
	