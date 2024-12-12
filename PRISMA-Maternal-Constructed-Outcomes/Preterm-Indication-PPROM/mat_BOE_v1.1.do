*PRISMA Maternal Variable Construction Code
*Purpose: This code drafts variable construction code for maternal outcome
	*variables for the PRISMA study - EDD_BOE & EST_CONCEP_DATE
*Original Version: March 6, 2024 by E Oakley (emoakley@gwu.edu)
*Update: March 25, 2024 by E Oakley (incorporate feedback from Dr. Wiley)
*Update: Split into a separate .do file for BOE only on March 29, 2024

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
global da "Z:/Stacked Data/2024-03-22" // change date here as needed

	// Working Files Folder (TNT-Drive)
global wrk "Z:/Erin_working_files/data" // set pathway here for where you want to save output data files (i.e., constructed analysis variables)

global date "240401" // today's date

log using "$log/mat_outcome_construct_BOE_$date", replace

/*************************************************************************
	*Variables constructed in this do file:
	
	*Best obstetric estimate (EDD_BOE)
		Gestational age estimate based on agreement between LMP and ultrasound. 	
		denominator: All women enrolled with a completed MNH01	
		format: dd-mm-yyyy	
					
	*Estimate date of conception/ pregnancy start date (EST_CONCEP_DATE)
		Calculated date: EDD_BOE - 280 
		format: dd-mm-yyyy
		
	*Enrollment indicator (ENROLL)
		Observation meets all criteria for enrollment as recorded in 
		MNH02
		
*/


*Best obstetric estimate (EDD_BOE)
	*Gestational age estimate based on agreement between LMP and ultrasound. 	
	*denominator: All women enrolled with a completed MNH01	
	*format: dd-mm-yyyy	

	*Input variables:
		// MNH01
	*ESTIMATED_EDD_SCDAT	Estimated due date (EDD) by LMP:
	*US_EDD_BRTHDAT_FTS1	1st fetus: EDD by ultrasound at TODAY’s visit:
	*GA_LMP_DAYS_SCORRES	Estimated GA by LMP:  days
	*US_GA_DAYS_AGE_FTS1	1st fetus: GA by ultrasound at TODAY’s visit:  days
	
		// constructed from MNH01
	*diff_lmp_us			EDD by LMP - EDD by US (days)
	
	import delimited "$da/mnh01_merged"
	
	
	*TO DO: MERGE IN MOM ID PREG ID USING SCREENING ID FROM MNH02 
	
	/* 
	
	Some study sites have not entered the identifiers "momid" and "pregid" into 
	the MNH01 screening form. Therefore, we need a file that includes momid, 
	pregid, AND the third identifier (scrnid) to crosswalk between scrnid and 
	the identifiers we will use to merge moving forward. 
	
	To do this, we need to match Stacie's R code (copied below) to create an 
	"ENROLL" indicator for all participants that meet criteria in MNH02 and 
	then use MNH02 to match scrnid to momid and pregid
	
	# pull all enrolled participants
	enrolled_ids <- mnh02 %>% 
	  mutate(ENROLL = ifelse(M02_AGE_IEORRES == 1 & 
							   M02_PC_IEORRES == 1 & 
							   M02_CATCHMENT_IEORRES == 1 & 
							   M02_CATCH_REMAIN_IEORRES == 1 & 
							   M02_CONSENT_IEORRES == 1, 1, 0)) %>% 
	  select(SITE, SCRNID, MOMID, PREGID,ENROLL, M02_AGE_IEORRES, M02_PC_IEORRES, 
	  M02_CATCHMENT_IEORRES,M02_CATCH_REMAIN_IEORRES, M02_CONSENT_IEORRES) %>% 
	  filter(ENROLL == 1) 
  */
  
	preserve 
	
		clear 
		
		*Use MNH02 to get the enrolled indicator:
		import delimited "$da/mnh02_merged", case(upper)
		
		*generate the ENROLL variable (1 = enrolled, 0 = not enrolled)
		gen ENROLL = 0 
		replace ENROLL = 1 if M02_AGE_IEORRES == 1 & M02_PC_IEORRES == 1 & ///
		   M02_CATCHMENT_IEORRES == 1 & M02_CATCH_REMAIN_IEORRES == 1 & ///
		   M02_CONSENT_IEORRES == 1
		   
		label var ENROLL "Enrolled in study (MNH02)"
		
		*rename variables & prep scrnid to merge: 
		rename MOMID momid_2
		rename PREGID pregid_2 
		rename SCRNID scrnid 
		
		keep scrnid momid_2 pregid_2 ENROLL M02_SCRN_OBSSTDAT
		
		rename scrnid scrnid_old 
		gen scrnid = ustrtrim(scrnid_old)
		drop scrnid_old 
		order scrnid, first
		
		*address duplicate scrnids 
		duplicates tag scrnid, gen(duplicate)
		tab duplicate
		
		*check on duplicates (comment this out if not needed)
		*list scrnid momid_2 pregid_2 duplicate M02_SCRN_OBSSTDAT ENROLL if duplicate >0
		
		*drop empty entries:
		drop if scrnid == ""  & momid_2 == "" & pregid_2 == ""
		
		*drop duplicate scrnids with no accompanying momid / pregid 
		drop if duplicate >=1 & momid_2 == "NA" & pregid_2 == "NA"
		
		*TEMPORARY MEASURE: drop second observation of 1 remaining duplicate: 
		drop if duplicate ==1 & momid_2 == "AA-4640" & M02_SCRN_OBSSTDAT == "2024-03-11"
		
		*final check: 
		drop duplicate 
		
		duplicates tag scrnid, gen(duplicate)
		tab duplicate
		
		drop duplicate 
		
		save "$wrk/ENROLL", replace 
		
	restore 
	
	*return to MNH01: 
	
	*restrict to enrollment visits: 
	tab m01_type_visit, m 
	tab m01_type_visit site, m 
	keep if m01_type_visit == 1
	
	*Combine enrolled indicator with MNH02: 
	merge m:1 scrnid using "$wrk/ENROLL"
	
		*Correct IDs for Zambia & other missing sites
		replace momid = momid_2 if _merge == 3 & momid == "NA"
		replace pregid = pregid_2 if _merge == 3 & pregid == "NA"
	
	*drop unmatched entries from using file: 
	drop if _merge == 2 
	
	
	*create components 
		// estimated due date by LMP
	gen ESTIMATED_EDD_SCDAT = date(m01_estimated_edd_scdat, "YMD")
	format ESTIMATED_EDD_SCDAT %d 
	label var ESTIMATED_EDD_SCDAT "Estimated due date (EDD) by LMP"
	
		// estimated GA by LMP
	gen GA_LMP_WKS_SCORRES = m01_ga_lmp_weeks_scorres 
	label var GA_LMP_WKS_SCORRES "Estimated GA by LMP: weeks"
	tab GA_LMP_WKS_SCORRES, m 
	
	gen GA_LMP_DAYS_SCORRES = m01_ga_lmp_days_scorres
	label var GA_LMP_DAYS_SCORRES "Estimated GA by LMP:  days"
	tab GA_LMP_DAYS_SCORRES, m 
	
		// calculated GA by LMP 
	gen GA_LMP = (GA_LMP_WKS_SCORRES * 7) + GA_LMP_DAYS_SCORRES if ///
		GA_LMP_WKS_SCORRES >= 0 & GA_LMP_WKS_SCORRES < = 60 & ///
		GA_LMP_DAYS_SCORRES >= 0 & GA_LMP_DAYS_SCORRES <= 6
		
	tab GA_LMP, m 
	label var GA_LMP "Gestational age by LMP at enrollment visit"
	
		// estimated due date by US 
		
	*Note: Stacie confirmed that OBs in the PRISMA study suggest that we 
	*take the EDD from the largest fetus for multiple pregnancies. This is 
	*addressed in the code below:
	
	gen US_EDD_BRTHDAT = date(m01_us_edd_brthdat_fts1, "YMD")
	format US_EDD_BRTHDAT %d
	label var US_EDD_BRTHDAT "EDD by ultrasound at TODAY’s visit"
		
		foreach num of numlist 2/4 {
		// replace with US-EDD of the largest fetus (i.e., the earliest EDD)
	replace US_EDD_BRTHDAT = date(m01_us_edd_brthdat_fts`num', "YMD") if ///
		date(m01_us_edd_brthdat_fts`num', "YMD") < US_EDD_BRTHDAT & ///
		m01_us_edd_brthdat_fts`num' != "1907-07-07" 
		
		// replace with US-EDD of subsequent fetus if unavailable for first
	replace US_EDD_BRTHDAT = date(m01_us_edd_brthdat_fts`num', "YMD") if ///
		US_EDD_BRTHDAT ==date("19070707", "YMD") & ///
		m01_us_edd_brthdat_fts`num' != "1907-07-07" 
		}
		
	/*CHECK ON EDDS FOR MULTIPLE BIRTHS: 
	list US_EDD_BRTHDAT  m01_us_edd_brthdat_fts1  m01_us_edd_brthdat_fts2 ///
		 m01_us_edd_brthdat_fts3  m01_us_edd_brthdat_fts4 ///
		 if m01_us_edd_brthdat_fts2 != "1907-07-07" 
	*/
	
		// estimated GA by US
	foreach num of numlist 1/4 {
		
		// Sites other than India-CMC and India-SAS
	gen US_GA_AGE_FTS`num' =  (m01_us_ga_wks_age_fts`num' * 7) + ///
		m01_us_ga_days_age_fts`num' if ///
		m01_us_ga_days_age_fts`num' >= 0 & m01_us_ga_days_age_fts1 <=6 & ///
		m01_us_ga_wks_age_fts`num' >= 0 & m01_us_ga_wks_age_fts`num' <=60 & ///
		site != "India-CMC" & site != "India-SAS"
		
		// India-CMC and India-SAS - use calculcated instead per request from site: 
	gen CAL_GA_AGE_FTS`num' = (m01_cal_ga_wks_age_fts`num' * 7) + ///
		m01_cal_ga_days_age_fts`num' if ///
		m01_cal_ga_days_age_fts`num' >= 0 & m01_cal_ga_days_age_fts1 <=6 & ///
		m01_cal_ga_wks_age_fts`num' >= 0 & m01_cal_ga_wks_age_fts`num' <=60 & ///
		(site == "India-CMC" | site == "India-SAS")
		
	}
	
	
	gen GA_US = max(US_GA_AGE_FTS1, US_GA_AGE_FTS2, US_GA_AGE_FTS3,US_GA_AGE_FTS4) if ///
		site != "India-CMC" & site != "India-SAS"
		
	replace GA_US = max(CAL_GA_AGE_FTS1, CAL_GA_AGE_FTS2, CAL_GA_AGE_FTS3, ///
		CAL_GA_AGE_FTS4) if (site == "India-CMC" | site == "India-SAS")
	
	tab GA_US, m 
	label var GA_US "Gestational age by US/AJOG Calc at enrollment visit"

	/*CHECK ON GAS FOR MULTIPLE BIRTHS: 
	list GA_US US_GA_AGE_FTS1 US_GA_AGE_FTS2 US_GA_AGE_FTS3 US_GA_AGE_FTS4 ///
		m01_us_ga_days_age_fts1 m01_us_ga_wks_age_fts1 ///
		m01_us_ga_days_age_fts2 m01_us_ga_wks_age_fts2 ///
		if (m01_us_ga_days_age_fts2 >= 0 & m01_us_ga_days_age_fts2 <=6) | ///
		(m01_us_ga_wks_age_fts2 >=0 & m01_us_ga_wks_age_fts2 <= 60)
	*/

		// difference of dates (by actual calendar date)
	
	gen diff_lmp_us_dates = abs(ESTIMATED_EDD_SCDAT - US_EDD_BRTHDAT) if ///
		ESTIMATED_EDD_SCDAT > date("01012022", "DMY") & ///
		ESTIMATED_EDD_SCDAT != . & ///
		US_EDD_BRTHDAT > date("01012022", "DMY") & ///
		US_EDD_BRTHDAT != .
	label var diff_lmp_us_dates "EDD by LMP - EDD by US (dates) - absolute value"
	tab diff_lmp_us_dates, m 
	
	/* CHECK ON PROBLEM DATES: 
	list ESTIMATED_EDD_SCDAT US_EDD_BRTHDAT diff_lmp_us_dates GA_LMP GA_US if ///
		(diff_lmp_us_dates < -60 | diff_lmp_us_dates > 60) & ///
		diff_lmp_us_dates != . 
	*/
		
	gen diff_lmp_us_dates_error = 0 
	replace diff_lmp_us_dates_error = 1 if ///
		(diff_lmp_us_dates < -60 | diff_lmp_us_dates > 60) & ///
		diff_lmp_us_dates != . 
	label var diff_lmp_us_dates_error "EDD by LMP and US are >60 days apart"
		
		// difference of GA (by calculated GA)
	
	gen diff_lmp_us_GA = abs(GA_LMP - GA_US) if GA_LMP >= 0 & GA_LMP <=400 & ///
		GA_US >= 0 & GA_US <=400
	label var diff_lmp_us_GA "GA by LMP - GA by US (at enrollment)"
	tab diff_lmp_us_GA, m 
	
	gen diff_lmp_us_GA_error = 0 
	replace diff_lmp_us_GA_error = 1 if ///
		(diff_lmp_us_GA < -60 | diff_lmp_us_GA > 60) & ///
		diff_lmp_us_GA != . 
	label var diff_lmp_us_GA_error "GA at enrollment by LMP and US are >60 days apart"
	
	tab diff_lmp_us_GA_error diff_lmp_us_dates_error, m 
	
	
	*Set criteria:
	gen BOE_GA_DAYS = . 
	label var BOE_GA_DAYS "Gestational age at enrollment by BOE (days)"
	
	gen BOE_METHOD = . 
	label var BOE_METHOD "Method used for BOE (1=US/Calc 2=LMP)"
	
	///////////////////////////////
	*If GA by LMP <9 weeks 0 days:
	
		*If GA by LMP - GA by US ≤ 5 days, == EDD by LMP 
	replace BOE_GA_DAYS = GA_LMP if diff_lmp_us_GA <= 5 & ///
		GA_LMP != . &  GA_US != . & ///
		GA_LMP < 63 
		
	replace BOE_METHOD = 2 if diff_lmp_us_GA <= 5 & ///
		GA_LMP != . &  GA_US != . & ///
		GA_LMP < 63 		
	
		*If GA by LMP - GA by US > 5 days, == EDD by US
	replace BOE_GA_DAYS = GA_US if diff_lmp_us_GA > 5 & ///
		GA_LMP != . & GA_US != . & ///
		GA_LMP < 63 	
		
	replace BOE_METHOD = 1 if diff_lmp_us_GA > 5 & ///
		GA_LMP != . & GA_US != . & ///
		GA_LMP < 63 	
		
	sum BOE_GA_DAYS
	sum BOE_METHOD
	
	/////////////////////////////////
	*If GA by LMP ≥9 weeks 0 days and ≤15 weeks 6 days:		
		*If GA by LMP - GA by US ≤ 7 days, BOE = EDD by LMP
	replace BOE_GA_DAYS = GA_LMP if diff_lmp_us_GA <= 7 & ///
		GA_LMP != . &  GA_US != . & ///
		GA_LMP >= 63 & GA_LMP < 112
		
	replace BOE_METHOD = 2 if diff_lmp_us_GA <= 7 & ///
		GA_LMP != . &  GA_US != . & ///
		GA_LMP >= 63 & GA_LMP < 112
		
		*If GA by LMP - GA by US > 7 days, == EDD by US
	replace BOE_GA_DAYS = GA_US if diff_lmp_us_GA > 7 & ///
		GA_LMP != . & GA_US != . & ///
		GA_LMP >= 63 & GA_LMP < 112 
		
	replace BOE_METHOD = 1 if diff_lmp_us_GA > 7 & ///
		GA_LMP != . & GA_US != . & ///
		GA_LMP >= 63 & GA_LMP < 112
	
	sum BOE_GA_DAYS
	sum BOE_METHOD
	
	/////////////////////////////////
	*If GA by LMP ≥ 16 weeks 0 days:
		*If GA by LMP - GA by US ≤ 10 days, == EDD by LMP
	replace BOE_GA_DAYS = GA_LMP if diff_lmp_us_GA <= 10 & ///
		GA_LMP != . &  GA_US != . & ///
		GA_LMP >= 112
		
	replace BOE_METHOD = 2 if diff_lmp_us_GA <= 10 & ///
		GA_LMP != . &  GA_US != . & ///
		GA_LMP >= 112
		
		*If GA by LMP - GA by US > 10 days, == EDD by US 
	replace BOE_GA_DAYS = GA_US if diff_lmp_us_GA > 10 & ///
		GA_LMP != . & GA_US != . & ///
		GA_LMP >= 112 
		
	replace BOE_METHOD = 1 if diff_lmp_us_GA > 10 & ///
		GA_LMP != . & GA_US != . & ///
		GA_LMP >= 112
		
	sum BOE_GA_DAYS
	sum BOE_METHOD
	
	*If GA by LMP is unknown, == EDD by US
	replace BOE_GA_DAYS = GA_US if GA_LMP == . 
	
	replace BOE_METHOD = 1 if GA_LMP == . & GA_US != . 
	
	sum BOE_GA_DAYS
	sum BOE_METHOD
	
	
	*Create weeks:
	gen BOE_GA_WKS = BOE_GA_DAYS / 7 
	label var BOE_GA_WKS "Gestational age at enrollment by BOE (weeks)"
	
	sum BOE_GA_WKS
	
	*Calculate dates by anchoring with ultrasound form date:
	gen US_OHOSTDAT = date(m01_us_ohostdat, "YMD") if ///
		m01_us_ohostdat != "1907-07-07"
	format US_OHOSTDAT %td 
	label var US_OHOSTDAT "Ultrasound Date"
	
	sum US_OHOSTDAT, format
	
	*Back-calculate estimated date of conception
	gen EST_CONCEP_DATE = US_OHOSTDAT - BOE_GA_DAYS 
	format EST_CONCEP_DATE %td 
	label var EST_CONCEP_DATE "Estimated date of conception (BOE)"
	
	*Create final EDD_BOE: 
	*Per Fouzia's code: add 280 days to EST_CONCEP_DATE to generate EDD based on BOE
	gen EDD_BOE = EST_CONCEP_DATE + 280
	format EDD_BOE %td
	label var EDD_BOE "Best obstetric estimate of EDD"
	
	sum EDD_BOE, format
	 
	
///////////////////////////////////////////
*Address duplicates: 
	
	duplicates tag momid pregid, gen(duplicate)
	tab duplicate, m 
	tab duplicate ENROLL, m 
	tab duplicate site, m 
	
	drop if duplicate > 0 & (momid == "NA" | momid == "") 
	
	*review for exact duplicates: 
	duplicates tag, gen(exact)
	tab exact, m 
	
	*create an indicator of total exact duplicates per person: 
	sort momid pregid *
	quietly by momid pregid :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	*drop any additional entries of exact duplicates: 
	drop if exact == 1 & ENTRY_NUM > 1 

//////////////////////////////////////////
*Save variables on BOE to a working file: 

	keep site momid pregid EDD_BOE EST_CONCEP_DATE US_OHOSTDAT BOE_GA_DAYS ///
		BOE_GA_WKS GA_US GA_LMP ESTIMATED_EDD_SCDAT US_EDD_BRTHDAT ENROLL
		
	save "$wrk/BOE", replace 

 gen ENROLL_ONTIME = EST_CONCEP_DATE + 139
 gen ENROLL_LATE = EST_CONCEP_DATE + 139
 gen ANC20_ONTIME = EST_CONCEP_DATE + 160
 gen ANC20_LATE = EST_CONCEP_DATE + 181
 gen ANC28_ONTIME = EST_CONCEP_DATE + 216
 gen ANC28_LATE = EST_CONCEP_DATE + 216
 gen ANC32_ONTIME = EST_CONCEP_DATE + 237
 gen ANC32_LATE = EST_CONCEP_DATE + 237
 gen ANC36_ONTIME = EST_CONCEP_DATE + 272
 gen ANC36_LATE = EST_CONCEP_DATE + 272

 foreach var in ENROLL_ONTIME ENROLL_LATE ANC20_ONTIME ANC20_LATE ANC28_ONTIME ANC28_LATE ANC32_ONTIME ANC32_LATE ANC36_ONTIME ANC36_LATE  {
 	replace `var' = . if ENROLL != 1 | EST_CONCEP_DATE ==.
	format `var' %td
 }
	
	save "$wrk/BOE", replace 
