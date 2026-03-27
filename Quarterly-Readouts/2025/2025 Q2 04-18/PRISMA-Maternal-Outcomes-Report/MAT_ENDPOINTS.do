*PRISMA Maternal Variable Construction Code
*Purpose: This code drafts variable construction code for maternal outcome
	*variables for the PRISMA study. This file focuses on outcomes collected 
	*at the endpoint of pregnancy.
*Original Version: March 6, 2024 by E Oakley (emoakley@gwu.edu)
*Update: March 25, 2024 by E Oakley (incorporate feedback from Dr. Wiley)
*Update: April 1, 2024 by E Oakley (incorporate feedback from Dr. Wiley)
*Update: April 8, 2024 by E Oakley (incorporate feedback from PRISMA sites)
*Update: May 13, 2024 by E Oakley (pull data for early pregnancy loss-MNH04)
*Update: May 16, 2024 by E Oakley (add indicator for withdraw from study)
*Update: June 27, 2024 by E Oakley (update definition & denominator for prolonged labor)
*Update: July 1, 2024 by E Oakley (update preterm classification for problem cases (no labor+vaginal delivery))
*Update: July 9, 2024 by E Oakley (update to files merged for maternal mortality endpoints)
*Update: September 16, 2024 by E Oakley (split into separate file for mat_endpoints only)
*Update: October 09, 2024 by S O'Malley (fixed error handling induced abortions)
*Update: December 17, 2024 by S O'Malley (Cleaned up and added to Github)
*Update: January-February, 2025 by S O'Malley (updated variable names & improved handling of duplicates)

clear
set more off
cap log close

*Directory structure:

	// Savannah's folders: 
global dadate "2025-05-16" // SYNAPSE/UPLOAD DATE 
global runquery = 1
global dir  "Z:\Savannah_working_files\Endpoints/$dadate" 
*global log "$dir/logs"
*global do "$dir/do"
global output "$dir"

	// Stacked Data Folders (TNT Drive)


global da "Z:/Stacked Data/$dadate" // update date as needed

global OUT "Z:\Savannah_working_files\Endpoints/$dadate"


	// Working Files Folder (TNT-Drive)
global wrk "Z:\Savannah_working_files\Endpoints/$dadate" // set pathway here for where you want to save output data files (i.e., constructed analysis variables)
	
	cap mkdir "$wrk" //make this folder if it does not exist
	
global queries "Z:\Savannah_working_files\Endpoints/$dadate\queries"
	cap mkdir "$queries" //make this folder if it does not exist



*log using "$log/mat_outcome_construct_endpoints_$date", replace

/*************************************************************************
	*Variables constructed in this do file:
					
	PREG_END - =1 for all completed pregnancies, including:
		- deliveries (MNH09)
		- pregnancy losses <20 weeks (MNH09 or MNH04 or MNH19)
		- induced abortion
		- deaths prior to delivery 
	
	PREG_END_DATE 
	
	PREG_END_GA (in days)
	
	CLOSEOUT 
	
	CLOSEOUT_DT
	
	CLOSEOUT_TYPE
	
	CLOSEOUT_GA (if during pregnancy)
	
	MAT_DEATH (pulled from mortality dataset)
	
	MAT_DEATH_DATE 
	
	MAT_DEATH_GA (if prior to delivery)
	
	STOP_DATE date of either a closeout or death where we no longer expect ANY 
		new data for the mother.	
		
*/


*Step 0: check for duplicates in MAT_ENROLL:
		
	clear 
	import delimited "Z:/Outcome Data/$dadate/MAT_ENROLL", case(upper)
	keep if ENROLL==1
	duplicates tag MOMID PREGID,gen(dup)
	assert dup == 0 
	bysort MOMID PREGID (PREG_START_DATE) : gen ENROLL_NUM = _n
	tab ENROLL_NUM
	keep if ENROLL_NUM == 1
	drop ENROLL_NUM dup
	count
	save "$wrk/MAT_ENROLL", replace 
	clear	

//////////////////////////////////////////
	*Added step prior to opening MMH09: search for pregnancy losses reported in 
	*MNH04 (ANC form): 
	
	import delimited "$da/mnh04_merged", bindquote(strict)
	
	*clean up: 
	drop if momid == "" | pregid == ""
	
	rename momid momid_old
	gen momid = ustrtrim(momid_old)
	
	rename pregid pregid_old
	gen pregid = ustrtrim(pregid_old)
	
	drop momid_old pregid_old
	
	*review fetal loss variables:
	tab m04_prg_dsdecod, m 
	tab m04_fetal_loss_dsstdat m04_prg_dsdecod, m 
	tab m04_fetal_loss_dsdecod, m 
	
	*construct outcomes: 
		// pregnancy loss recorded
	gen PREG_LOSS_MNH04 = 1 if m04_prg_dsdecod == 2 
	label var PREG_LOSS_MNH04 "Pregnancy loss recorded in MNH04"
	
		// date of pregnancy loss
	gen PREG_END_DATE_MNH04 = date(m04_fetal_loss_dsstdat , "YMD") if ///
		PREG_LOSS_MNH04 == 1 & m04_fetal_loss_dsstdat != "1907-07-07" & ///
		m04_fetal_loss_dsstdat != "1905-05-05"
	format PREG_END_DATE_MNH04 %td
	label var PREG_END_DATE_MNH04 "Date of pregnancy loss recorded in MNH04"
	
	replace m04_mat_visit_mnh04 = 88 if site=="Kenya" & m04_mat_visit_mnh04 == 8
		// Maternal death before delivery (fetal demise concurrent)
	gen PREG_DEATH_MNH04 = 1 if m04_prg_dth_dsdecod == 2 & ///
		(m04_mat_visit_mnh04 == 8 | m04_mat_vital_mnh04 == 2)
		
		// date of maternal death before delivery 
	gen PREG_DEATH_DATE_MNH04 = date(m04_dthdat,"YMD") if ///
		PREG_DEATH_MNH04 == 1
	format PREG_DEATH_DATE_MNH04 %td 
	label var PREG_DEATH_DATE_MNH04 "Date of maternal death with concurrent fetal demise"
	
	replace PREG_END_DATE_MNH04 = PREG_DEATH_DATE_MNH04 if ///
		PREG_END_DATE_MNH04 == . & PREG_DEATH_MNH04 == 1 
		
	tab PREG_DEATH_MNH04, m 
	
		// type of pregnancy loss:
	gen LOSS_TYPE = m04_fetal_loss_dsdecod if m04_fetal_loss_dsdecod <4 
	replace LOSS_TYPE = 4 if PREG_DEATH_MNH04 == 1 & LOSS_TYPE == . 
	label var LOSS_TYPE "Type of pregnancy loss (1=SpAb; 2=IndAb; 3=SB; 4=Unborn)"
	label define loss 1 "1-Spontaneous abortion" 2 "2-Induced Abortion" ///
		3 "3-Stillbirth" 4 "4-Fetal demise at maternal death"
	label values LOSS_TYPE loss 
 	
	tab PREG_LOSS_MNH04 LOSS_TYPE, m 
	
	* Checks from Code Review: 
	
	tab m04_fetal_loss_dsdecod, m 
	tab m04_prg_dsdecod, m 
	
	tab m04_fetal_loss_dsdecod m04_prg_dsdecod, m 
	
	* look for losses w/ missing dates: 
	list site PREG_END_DATE_MNH04 PREG_LOSS_MNH04 LOSS_TYPE if PREG_LOSS_MNH04 == 1 | ///
		LOSS_TYPE != . 
	
	tab m04_anc_obsstdat if PREG_LOSS_MNH04 == 1 & PREG_END_DATE_MNH04==.
	
	/*
	replace PREG_END_DATE_MNH04 = date(m04_anc_obsstdat, "YMD") if ///
		PREG_LOSS_MNH04 == 1 & PREG_END_DATE_MNH04 == . 
	*/
	
	gen PREG_END_DATE_MNH04_ESTIMATED = date(m04_anc_obsstdat, "YMD") if ///
		PREG_LOSS_MNH04 == 1 & PREG_END_DATE_MNH04 == . 
		
	format PREG_END_DATE_MNH04_ESTIMATED %td 
	
	**Add date last seen, if maternal death is recorded
	gen LASTALIVE = date(m04_anc_obsstdat, "YMD") if ///
	m04_mat_visit_mnh04<=2 & m04_mat_vital_mnh04==1
	
	bysort momid pregid: ///
	egen VISIT_LAST_ALIVE_MNH04=max( LASTALIVE)
	
	format VISIT_LAST_ALIVE_MNH04 LASTALIVE %td
	
	preserve 
		
		collapse (max) VISIT_LAST_ALIVE_MNH04, ///
		by(site momid pregid)
		save "$wrk/MNH04_LASTALIVE", replace
		
	restore
	
	
	*Restrict to pregnancy losses/deaths before delivery: 
	keep if PREG_LOSS_MNH04 == 1 | PREG_DEATH_MNH04 == 1 | LOSS_TYPE != . 
	
	*check for duplicates:
	duplicates tag momid pregid, gen(duplicate)
	
	tab duplicate, m 
	
	*review duplicates:
	sort pregid 
	list site pregid PREG_LOSS_MNH04 PREG_END_DATE_MNH04 ///
		PREG_DEATH_MNH04 LOSS_TYPE m04_type_visit m04_anc_obsstdat ///
		if duplicate >= 1 
	
	*there are a few observations (all in Ghana) where the pregnancy loss is 
	*recorded on multiple forms with different visit types (ex: visit type 2 and 
	*also visit type 13 (unscheduled). This isn't a problem as long as the loss
	*has the same date of pregnancy endpoint. we review these "exact" duplicate 
	*details below:
	duplicates tag momid pregid PREG_LOSS_MNH04 PREG_END_DATE_MNH04, gen(exact)
	tab exact, m 
	
	*since these duplicates all record the same details, we will drop the 
	*unscheduled entries:
	drop if (exact==1 | exact==2) & m04_type_visit == 13
	drop if (exact==2) & m04_type_visit == 3 & ///
		pregid=="DA9a934372-e0d9-4bd0-9bd7-9221aab4f7051" 		
	
	
	keep site momid pregid PREG_LOSS_MNH04 PREG_END_DATE_MNH04 ///
		PREG_DEATH_MNH04 PREG_DEATH_DATE_MNH04 LOSS_TYPE ///
		PREG_END_DATE_MNH04_ESTIMATED VISIT_LAST_ALIVE_MNH04
		
	rename site site_MNH04
	
	*save a copy of this file:
	save "$wrk/preg-losses_MNH04", replace 
	 
	clear
	
	*Added step prior to opening MMH09: search for pregnancy losses & relevant 
	*outcomes reported in MNH19 (Hospitalization form): 
	
	import delimited "$da/mnh19_merged", bindquote(strict) clear
	
	/* Variables needed from hospitalization:
		
		Pregnancy endpoint - PREG_FAORRES
	
	*/
	
	* Review pregnancy endpoints recorded in the form: 
	tab m19_preg_faorres m19_preg_dsterm, m 

	gen PREGEND_HOSP = 0 if m19_preg_dsterm==1 
	replace PREGEND_HOSP = 1 if m19_preg_dsterm==2 
	label var PREGEND_HOSP "Pregnancy endpoint recorded during ANC hospitalization"
	
	tab PREGEND_HOSP, m 
	
	*Type of pregnancy loss: 
	/* values for consistency with ANC preg loss var: 
		label define loss 1 "1-Spontaneous abortion" 2 "2-Induced Abortion" ///
		3 "3-Stillbirth" 4 "4-Fetal demise at maternal death"
	*/
	gen LOSS_TYPE_HOSP = 1 if PREGEND_HOSP == 1 & m19_preg_faorres == 1 
	replace LOSS_TYPE_HOSP = 2 if PREGEND_HOSP == 1 & m19_preg_faorres == 2
	replace LOSS_TYPE_HOSP = 3 if PREGEND_HOSP == 1 & m19_preg_faorres == 3
	label var LOSS_TYPE_HOSP "Pregnancy loss recorded in hospitalization form"
	
	gen LIVEBIRTH_HOSP = 1 if PREGEND_HOSP == 1 & m19_preg_faorres == 4
	replace LIVEBIRTH_HOSP = 0 if PREGEND_HOSP == 1 & m19_preg_faorres != 4
	label var LIVEBIRTH_HOSP "Livebirth recorded in hospitalization form"

	label define loss 1 "1-Spontaneous abortion" 2 "2-Induced Abortion" ///
	3 "3-Stillbirth" 4 "4-Fetal demise at maternal death"
	label values LOSS_TYPE_HOSP loss

	
	tab LOSS_TYPE_HOSP PREGEND_HOSP, m 
	
	*Confirm that pregend w/o loss type are marked as livebirths: 
	tab LOSS_TYPE LIVEBIRTH_HOSP if PREGEND_HOSP == 1, m 
	
	*indicator for early pregnancy loss < 13 weeks: 
	gen LOSS_EARLY = 0 if LOSS_TYPE_HOSP >= 1 & LOSS_TYPE_HOSP <= 3 
	replace LOSS_EARLY = 1 if m19_primary_mhterm == 1 // early pregnancy loss as main reason for hospital visit
	label var LOSS_EARLY "Hospitalized for early pregnancy loss"
	
	tab LOSS_EARLY LOSS_TYPE_HOSP, m 
	
		// update PREGEND_HOSP variable to account for early pregnancy loss:
		replace PREGEND_HOSP = 1 if LOSS_EARLY == 1 
	
	gen PREGEND_HOSP_DT =  date(m19_ohostdat, "YMD") if PREGEND_HOSP == 1 | LOSS_EARLY == 1 
	replace PREGEND_HOSP_DT = date(m19_mat_est_ohostdat, "YMD") if /// 
		(PREGEND_HOSP == 1 | LOSS_EARLY == 1) & ///
		PREGEND_HOSP_DT == . 
	format PREGEND_HOSP_DT %td
	label var PREGEND_HOSP_DT "Date of hospitalization with recorded pregnancy endpoint"
	
	tab PREGEND_HOSP_DT PREGEND_HOSP, m 
	tab PREGEND_HOSP_DT LOSS_TYPE, m 
	tab PREGEND_HOSP_DT LIVEBIRTH_HOSP, m 
	
	gen MAT_DEATH_M19 = 1 if m19_mat_arrival_dsdecod ==2 | m19_visit_faorres ==5 | m19_admit_dsterm==3 | ( m19_dthdat!="1907-07-07" & m19_dthdat!= "1909-09-09" & m19_dthdat!="1905-05-05")
	gen MAT_DEATH_DATE_M19 = date(m19_dthdat, "YMD") if MAT_DEATH_M19 == 1
	gen FORMCOMPL_DTHDAT_M19 = date(m19_formcompldat_mnh19, "YMD") if MAT_DEATH_M19 == 1
	label var FORMCOMPL_DTHDAT_M19 "date form was completed and recorded mother death"
	gen lastalive = date(m19_formcompldat_mnh19, "YMD") if MAT_DEATH_M19 !=1
	
	bysort momid pregid : egen LASTALIVE_M19=max(lastalive)
	format MAT_DEATH_DATE_M19 FORMCOMPL_DTHDAT_M19 LASTALIVE_M19 %td
	
	*restrict to the outcomes that we need to prep hospitalization dataset: 
	rename site site_MNH19 
	
	
	preserve
		collapse (max) LASTALIVE_M19, ///
		by(site momid pregid)
		
		save "$wrk/MNH19_LASTALIVE", replace
	restore
	
		// keep if any outcomes of interest: 
	keep if PREGEND_HOSP == 1 | LOSS_EARLY == 1 | MAT_DEATH_M19 ==1
	
		// restrict to constructed variables: 
	keep momid pregid site_MNH19 PREGEND_HOSP PREGEND_HOSP_DT LOSS_TYPE_HOSP ///
		LIVEBIRTH_HOSP LOSS_EARLY MAT_DEATH_M19 MAT_DEATH_DATE_M19 FORMCOMPL_DTHDAT_M19 LASTALIVE_M19
	
		// check for duplicates:
	duplicates tag momid pregid, gen(duplicate)
	tab duplicate, m 
	
	

		if $runquery == 1 {
		levelsof(site) if duplicate !=0 , local(sitelev) clean
		foreach site of local sitelev {
			export excel site_MNH19 momid pregid  using "$queries/`site'-endpoints-queries-$dadate.xlsx"  if site=="`site'" & duplicate >= 1 , sheet("duplicate-MNH19",modify)  firstrow(variables) 
		}
	}
	
	
		sum duplicate 
		if r(max) > 0  {
			*if duplicate exists, collapse
		
	collapse (min)  PREGEND_HOSP_DT  ///
		(max) PREGEND_HOSP LOSS_TYPE_HOSP LIVEBIRTH_HOSP ///
			LOSS_EARLY LASTALIVE_M19 /// 
		(firstnm) MAT_DEATH_M19 MAT_DEATH_DATE_M19 FORMCOMPL_DTHDAT_M19, ///
		by(site_MNH19 momid pregid)
		}
	

	cap drop duplicate
	
	
	isid momid pregid //confirm no duplicates
	
	save "$wrk/endpoint_outcomes_MNH19", replace 
	
	clear 
	
	/////////////////////////
	*Now can open MNH09: 
	*MNH09: 
	import delimited "$da/mnh09_merged", bindquote(strict) clear
	
	tab site, m 
	
	
	*clean up dataset: 
	keep if site == "Ghana" | site == "India-CMC" | site == "Kenya" | ///
		site == "Pakistan" | site == "Zambia" | site == "India-SAS"
		
	format momid %38s
	recast str38 momid, force
	
	gen PREG_END_SOURCE = 1 
	label var PREG_END_SOURCE "Source of information for PREG_END (1=M09;2=M04 only;3=M19 only;4=Death only)"
	

	///////////////////////////////////////////////
	* * * * Determine pregnancy end date * * * *
	///////////////////////////////////////////////
	
	*to create a comprehensive variable for preg end date, merge in MNH04 
	*variables:
	merge 1:1 momid pregid using "$wrk/preg-losses_MNH04.dta", gen(MNH04_ANY)
	tab MNH04_ANY
	 
	*** we will retain all observations, even if there is no MNH09 
	replace site = site_MNH04 if site == ""
	
	gen MNH04_ONLY = 0 
	replace MNH04_ONLY = 1 if MNH04_ANY == 2
	label var MNH04_ONLY "=1 if endpoint reported in MNH04 (but not MNH09)"
	
	replace PREG_END_SOURCE = 2 if MNH04_ONLY == 1 
	
	replace MNH04_ANY = 0 if MNH04_ANY == 1 
	replace MNH04_ANY = 1 if MNH04_ANY == 2 | MNH04_ANY == 3
	
	tab PREG_END_SOURCE, m 


	
	*also need to merge in outcomes as reported at hospitalization: 
	merge 1:1 momid pregid using "$wrk/endpoint_outcomes_MNH19", gen(MNH19_ANY)
	tab MNH19_ANY
	*** we will retain all observations, even if there is no MNH19 
	replace site = site_MNH19 if site == ""
	
	gen MNH19_ONLY = 0 
	replace MNH19_ONLY = 1 if MNH19_ANY == 2
	label var MNH19_ONLY "=1 if endpoint reported in MN19 (but not MNH09)"
	
	replace PREG_END_SOURCE = 3 if MNH19_ONLY == 1 
	
	replace MNH19_ANY = 0 if MNH19_ANY == 1 
	replace MNH19_ANY = 1 if MNH19_ANY == 2 | MNH19_ANY == 3
	
	tab PREG_END_SOURCE
	
	
	
	// PREG_END_DATE
	*Date of pregnancy outcome (take first infant): 
	gen PREG_END_DATE = date(m09_deliv_dsstdat_inf1, "YMD") if ///
		m09_deliv_dsstdat_inf1 != "1907-07-07"
	format PREG_END_DATE %td
	label var PREG_END_DATE "Date of pregnancy outcome (1st infant)"
	sum PREG_END_DATE, format 	
	 
	*incorporate pregnancy losses in MNH04, MNH19: 
		// from MNH04 only - loss date: 
	replace PREG_END_DATE = PREG_END_DATE_MNH04 if PREG_LOSS_MNH04 == 1 & ///
		PREG_END_DATE == . & PREG_END_DATE_MNH04 !=  date("1907-07-07","YMD")
		
		// from MNH04 only - death prior to delivery: 
	replace PREG_END_DATE = PREG_DEATH_DATE_MNH04 if PREG_DEATH_MNH04 == 1 & ///
		PREG_END_DATE == . & PREG_DEATH_DATE_MNH04 !=  date("1907-07-07","YMD")
		
		// from hospital form (estimated by date of hospitalization)
	replace PREG_END_DATE = PREGEND_HOSP_DT if PREGEND_HOSP == 1 & ///
		(MNH19_ONLY == 1 | PREG_END_DATE == .) & PREGEND_HOSP_DT !=  ///
		date("1907-07-07","YMD")
		
	tab PREG_END_DATE if PREG_LOSS_MNH04 == 1, m 
	tab PREG_END_DATE if PREGEND_HOSP == 1, m 
	
	// Review people with a missing pregend date: look for observations with 
	// NO information on pregnancy endpoint (n=75 observations in 9-6 data)
	list site PREG_END_SOURCE m09_mat_visit_mnh09 m09_mat_vital_mnh09 ///
		m09_deliv_dsstdat_inf1 m09_birth_dsterm_inf1 ///
		PREG_LOSS_MNH04 PREG_END_DATE_MNH04 ///
		PREGEND_HOSP PREGEND_HOSP_DT  if PREG_END_DATE == .
		
		gen PREG_END_DATE_MISS = 1 if PREG_END_DATE ==. 
		
	gen MNH04_ESTIMATES = 1 if PREG_END_DATE_MNH04_ESTIMATED != . 
	
	tab MNH04_ESTIMATES 
	

	// Merge in BOE data & ENROLLMENT INDICATOR 
	
	
	
	rename momid MOMID 
	rename pregid PREGID 
	
	merge 1:1 MOMID PREGID using "$wrk/MAT_ENROLL"
	
	tab _merge site, m 
	
	replace site = SITE if site == ""
	
	
	*** TEMPORARY MEASURE: keep only if in ENROLL dataset AND L&D/MNH04/MNH19:
	
	if $runquery == 1 {
		
		levelsof(site) if _merge ==1 , local(sitelev) clean
			*which sites have unenrolled participants?
		foreach site of local sitelev {
			export excel site MOMID PREGID PREG_END_SOURCE using ///
			"$queries/`site'-endpoints-queries-$dadate.xlsx"  if ///
			site=="`site'" & _merge == 1 , ///
			sheet("Not-enrolled",modify) firstrow(variables) 
		}
		
	}
	
	keep if _merge == 3 // keep only those Enrolled & with MNH 04/09/19
	
	drop _merge 
	
	rename PREG_START_DATE 	STR_PREG_START_DATE
	
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td
	
	sum PREG_START_DATE, format 
		
*Calculate GA at pregnancy endpoint: 
	
	gen PREG_END_GA = PREG_END_DATE - PREG_START_DATE
	label var PREG_END_GA "GA at pregnancy endpoint (days)"
	
	sum PREG_END_GA
	
	gen PREG_END_GA_WK = PREG_END_GA / 7 
	
	
	
	* REVIEW GA AT ESTIMATED ENDPOINT (for pregnancy losses with missing date): 
	gen PREG_END_GA_EST = PREG_END_DATE_MNH04_ESTIMATED - PREG_START_DATE if ///
		MNH04_ESTIMATES==1 
		
	gen PREG_END_GA_EST_WK = PREG_END_GA_EST / 7 
		
	list SITE MNH04_ESTIMATES PREG_END_GA_WK PREG_END_GA_EST_WK if ///
		MNH04_ESTIMATES==1
		
		*For now, we will use the visit date where loss was reported 
		*to estimate GA at pregnancy endpoint:
			
		replace PREG_END_GA = PREG_END_GA_EST if ///
			PREG_END_DATE == . & MNH04_ESTIMATES == 1 
			
		replace PREG_END_GA_WK = PREG_END_GA_EST_WK if ///
			PREG_END_DATE == . & MNH04_ESTIMATES == 1 
			
		replace PREG_END_DATE = PREG_END_DATE_MNH04_ESTIMATED if ///
			PREG_END_DATE == . & MNH04_ESTIMATES == 1 
			
		replace PREG_END_DATE_MISS = . if MNH04_ESTIMATES == 1 & ///
			PREG_END_DATE != . 
	
	
	*Check on negative case: 
	list MOMID PREGID SITE PREG_START_DATE PREG_END_DATE PREG_END_GA ///
		ENROLL_SCRN_DATE ///
		if PREG_END_GA <0 & PREG_END_GA != .
		
	
	*Check on pregnancies by GA at endpoint: are there any sites filling MNH09 
	*for pregnancies <20 weeks GA?
	*twoway histogram PREG_END_GA, w(1) by(site) ///
	*xline(140) color(black) xsize(20) ysize(10)
	
	
	gen PREG_LOSS = 0 if PREG_END_GA >=140 & PREG_END_GA !=.
	replace PREG_LOSS = 1 if PREG_END_GA < 140 & PREG_END_GA >=0
	replace PREG_LOSS = 0 if PREG_END_GA == . // we will consider this to be "0" for table output 
	label var PREG_LOSS "Pregnancy endpoint <20 weeks GA"
	
	tab PREG_LOSS
	
	tab PREG_LOSS PREG_LOSS_MNH04, m 
	tab PREG_LOSS LOSS_TYPE, m 
	
	
	*review of outcomes:
	list site PREG_LOSS PREG_LOSS_MNH04 LOSS_TYPE ///
	PREGEND_HOSP LOSS_TYPE_HOSP LOSS_EARLY ///
	PREG_END_GA PREG_END_GA_WK ///
	PREG_END_DATE PREG_END_DATE_MNH04 m09_birth_dsterm_inf1 ///
	if PREG_LOSS == 1 | PREG_LOSS_MNH04 == 1 | PREGEND_HOSP == 1 
	
	*revise the "Loss Type" variable, correcting for GA by BOE & incorporating 
	*hospitalization variables:
	 // If GA at endpoint is <20 weeks, this is a spontaneous abortion: 
	 **UPDATE OCT 9 2024 BY SFO:
		*previously missing induced abortion specification, added here
		*only reassign to miscarriage/stillbirth if NOT an induced abortion
	replace LOSS_TYPE = 1 if PREG_END_GA >= 0 & PREG_END_GA <= 139 & ///	
		(LOSS_TYPE == 3 | (PREGEND_HOSP ==1 & LIVEBIRTH_HOSP==0 & LOSS_TYPE_HOSP!=2))
		
	replace LOSS_TYPE = 1 if PREG_END_GA >= 0 & PREG_END_GA <=139 & ///
		(PREGEND_HOSP==. & LOSS_TYPE == . & LOSS_TYPE_HOSP!=2)
		
	 // If the GA at endpoint is >=20 weeks, this is a stillbirth: 
	replace LOSS_TYPE = 3 if PREG_END_GA >= 140 & PREG_END_GA != . & ///
		(LOSS_TYPE == 1 | (PREGEND_HOSP ==1 & LIVEBIRTH_HOSP==0 & LOSS_TYPE_HOSP!=2))
		
		
	*generate an indicator for induced abortion:
	**UPDATE OCT 9 2024 BY SFO:
	* if indicated induced abortion in hospitalization: loss_type==2
	gen PREG_LOSS_INDUCED = 0 
	replace PREG_LOSS_INDUCED = 1 if LOSS_TYPE == 2 | LOSS_TYPE_HOSP==2
	label var PREG_LOSS_INDUCED "Pregnancy loss - induced abortion (any GA)"
	
	tab PREG_LOSS_INDUCED PREG_LOSS, m 
	
	sort LOSS_TYPE 
	by LOSS_TYPE: tab PREG_END_GA_WK
	bysort LOSS_TYPE: sum PREG_END_GA_WK
	tabstat PREG_END_GA_WK, by(LOSS_TYPE) stats(n min p50 p95 max) missing
	 

	* Review loss types: 
	
	tab PREG_END_GA PREG_LOSS, m 
	tab PREG_LOSS site, m 	
	
**# return here - what to do with PREG_END_GA > 43?
	
	//////////////////////////////////////////////////////////////////////////
	** REVIEW: Observations with an MNH09 & no end date - Part I **
	//////////////////////////////////////////////////////////////////////////

	tab PREG_END_DATE_MISS, m 
	
	
		/*Items to review: 
			Part I (here): 
			Visit completion status for MNH09 form?
			
			Part II (below, after closeout form is processed): 
			How many people have a closeout form?
			What is the time period between MNH09 form date & closeout date?
			What is the GA at MNH09 form fill date? Is it at/after 42 weeks?
			Do any of these observations have PNC forms?
			
		*/
		
		tab m09_mat_visit_mnh09 if PREG_END_DATE_MISS==1, m 
		list m09_mat_visit_othr_mnh09 if PREG_END_DATE_MISS==1
		
		*Check for information from other forms for the incomplete: 
		tab MNH19_ANY MNH04_ANY if PREG_END_DATE_MISS ==1
			// as of 9-18: No additional info from these forms; this appears 
			// to be JUST an MNH09 issue.
		
		gen MNH09_VISIT_INCOMPLETE = 1 if ///
			PREG_END_DATE_MISS == 1 & ///
			((m09_mat_visit_mnh09 >= 3 & m09_mat_visit_mnh09 <=7) |  ///
			m09_mat_visit_mnh09 == 88)
			
		label var MNH09_VISIT_INCOMPLETE "MNH09 marked as not completed"
		
		tab MNH09_VISIT_INCOMPLETE, m 
		
			*calculate: form completed date & ga: 
			gen FORM_DATE = date(m09_formcompldat_mnh09, "YMD")
			format FORM_DATE %td
			gen FORM_GA = FORM_DATE - PREG_START_DATE if MNH09_VISIT_INCOMPLETE == 1 | ///
				PREG_END_DATE_MISS == 1 
				
			sum FORM_GA
		
		*Check on some key variables for this group:
			*Mother's vital status			MAT_VITAL_MNH09
			*Delivery location				MAT_LD_OHOLOC
			*Primary data source 			MAT_LD_SRCE
			*Experienced labor				LABOR_MHOCCUR
			*Infant 1 ID					INFANTID_INF1
			*Infant 1 date of delivery		DELiV_DSSTDAT_INF1
			*Infant 1 birth outcome 		BIRTH_DSTERM_INF1
			*Infant 2 ID					INFANTID_INF2
			*Infant 2 date of delivery		DELiV_DSSTDAT_INF2		
			*Infant 2 birth outcome 		BIRTH_DSTERM_INF2	
			*Postpartum hemorrhage			PPH_CEOCCUR
			*Maternal death at L&D (date)	MAT_DEATH_DTHDAT
			*Form Date 						FORMCOMPLDAT_MNH09
			*GA at Form Date				calculated: FORM_GA 
			
			*see also: comments 			COVAL_MNH09
			
		sort SITE FORM_DATE
		
		list SITE m09_mat_vital_mnh09 m09_mat_ld_oholoc m09_mat_ld_srce ///
			m09_labor_mhoccur m09_infantid_inf1 m09_deliv_dsstdat_inf1 ///
			m09_birth_dsterm_inf1 m09_infantid_inf2 m09_deliv_dsstdat_inf2 ///
			m09_birth_dsterm_inf2 m09_pph_ceoccur m09_mat_death_dthdat ///
			FORM_DATE FORM_GA MNH09_VISIT_INCOMPLETE if ///
			MNH09_VISIT_INCOMPLETE == 1 | PREG_END_DATE_MISS == 1 
			
		list SITE m09_coval_mnh09 if  ///
			MNH09_VISIT_INCOMPLETE == 1 | PREG_END_DATE_MISS == 1 
			 
			
		*More comprehensive indicator: form marked as incomplete OR 
		*date & birth outcome not recorded (and no info from MNH04/19)
		gen MNH09_FORM_INCOMPLETE = 1 if (MNH09_VISIT_INCOMPLETE == 1 | ///
			(PREG_END_DATE_MISS == 1 & m09_birth_dsterm_inf1==77)) & ///
			MNH04_ANY == 0 & MNH19_ANY == 0 
			
		tab MNH09_FORM_INCOMPLETE, m 
		label var MNH09_FORM_INCOMPLETE "Incomplete MNH09 form (visit incomplete; no date/outcome)"
		**note: in Kenya, we would expect a number of MNH09 incomplete 
			*likely pending closeout form
			
	//////////////////////////////////////////////////////////////////////////
	** Construct the PREG_END indicator: **
	//////////////////////////////////////////////////////////////////////////			
			
	gen PREG_END = 0 
		// reported date OR outcome in MNH09 
	replace PREG_END = 1 if PREG_END_SOURCE == 1 & ///
		MNH09_FORM_INCOMPLETE == . & ///
		(PREG_END_DATE != . | m09_birth_dsterm_inf1 == 1 | m09_birth_dsterm_inf1 == 2)
		
	replace PREG_END = 1 if PREG_END_SOURCE == 2 & PREG_LOSS_MNH04 ==1 	
	
	replace PREG_END = 1 if PREG_END_SOURCE == 3 & PREGEND_HOSP == 1 

	
	label var PREG_END "Completed pregnancies (with MNH09 completed OR loss in MNH04/19)"	
	
	replace m09_mat_visit_mnh09 = 88 if site=="Kenya" & m09_mat_visit_mnh09 == 8
	**Indicator for maternal death and last vsit seen alive:
	gen MAT_DEATH_MNH09 = 1 if ///
	m09_mat_vital_mnh09 == 2 | m09_mat_visit_mnh09 == 8 | ///
	(m09_mat_death_dthdat!= "1907-07-07" & ///
	m09_mat_death_dthdat!= "1905-05-05" & ///
	m09_mat_death_dthdat != "")
	label var MAT_DEATH_MNH09 "death identified at mnh09"
	gen MAT_DEATH_MNH09_DATE = date(m09_mat_death_dthdat, "YMD") if ///
	MAT_DEATH_MNH09 == 1
	format   MAT_DEATH_MNH09_DATE %td

	gen VISIT_DOD_MNH09 = date(m09_formcompldat_mnh09, "YMD") if ///
	MAT_DEATH_MNH09 == 1
	label var VISIT_DOD_MNH09 "date death was recorded"

	gen VISIT_LAST_ALIVE_MNH09 = date(m09_formcompldat_mnh09, "YMD") if ///
	MAT_DEATH_MNH09 !=1
	
	preserve 
		collapse (max) VISIT_LAST_ALIVE_MNH09 (min) VISIT_DOD_MNH09 , ///
		by(SITE MOMID PREGID)
		save "$wrk/MNH09_LASTALIVE", replace
	restore
	
	//////////////////////////////////////////////////////////////////////////
	** FINALIZE ANALYSIS DATASET - from MNH09 **
	//////////////////////////////////////////////////////////////////////////
	
	*export an analysis data set:
	
	order SITE MOMID PREGID PREG_LOSS PREG_END_DATE PREG_END_GA PREG_END ///
		LOSS_TYPE PREG_LOSS_INDUCED PREG_END_DATE_MISS MNH09_FORM_INCOMPLETE ///
		m09_mat_visit_mnh09 m09_mat_visit_othr_mnh09 FORM_DATE FORM_GA ///
		m09_coval_mnh09 PREG_END_SOURCE MNH04_ANY MNH19_ANY 
	
	keep SITE MOMID PREGID PREG_LOSS PREG_END_DATE PREG_END_GA PREG_END ///
		LOSS_TYPE PREG_LOSS_INDUCED PREG_END_DATE_MISS MNH09_FORM_INCOMPLETE ///
		m09_mat_visit_mnh09 m09_mat_visit_othr_mnh09 FORM_DATE FORM_GA ///
		m09_coval_mnh09 PREG_END_SOURCE MNH04_ANY MNH19_ANY ///
		MAT_DEATH_MNH09 MAT_DEATH_MNH09_DATE VISIT_DOD_MNH09 VISIT_LAST_ALIVE_MNH09 PREG_DEATH_MNH04 PREG_DEATH_DATE_MNH04 PREG_END_DATE_MNH04_ESTIMATED VISIT_LAST_ALIVE_MNH04 FORMCOMPL_DTHDAT_M19 LASTALIVE_M19
		 
	save "$wrk/maternal_endpoints_MNH09", replace 
	
///////////////////////////////////////////////////////////////////////
*** INCORPORATE CLOSEOUT FORM:

	clear 
	
	import delimited "$da/mnh23_merged", bindquote(strict)
	
	*Review variables:
	tab m23_close_dsdecod, m 
	
	*** closeout indicator: 
	gen CLOSEOUT = 1 
	
	*** closeout type:
	gen CLOSEOUT_TYPE = m23_close_dsdecod
	
	label define co 1 "1-Follow-up complete (1 yr)" 2 "2-Follow-up complete (42 days)" ///
		3 "3-Death" 4 "4-Withdraw" 5 "5-Terminated from study" 6 "6-Loss to follow-up"
	label values CLOSEOUT_TYPE co 
	tab CLOSEOUT_TYPE, m 
	
	
	gen CLOSEOUT_DT = date(m23_close_dsstdat, "YMD") if ///
		!inlist(m23_close_dsstdat, "1905-05-05", "1907-07-07") 
	format CLOSEOUT_DT %td
	sum CLOSEOUT_DT,format
	tab site CLOSEOUT_TYPE if CLOSEOUT_DT<0
	
	
	label var CLOSEOUT "Participant closed out of study"
	label var CLOSEOUT_TYPE "Type of closeout"
	label var CLOSEOUT_DT "Date of closeout"
	
	gen MAT_DEATH_M23 = 1 if ///
	m23_close_dsdecod == 3 | !inlist(m23_dthdat, "1905-05-05", "1907-07-07")
	gen MAT_DEATH_DATE_M23 = date(m23_dthdat, "YMD") if MAT_DEATH_M23 == 1
	
	
	
	*restrict to needed variables:
	keep site momid pregid ///
	CLOSEOUT CLOSEOUT_TYPE CLOSEOUT_DT ///
	MAT_DEATH_M23 MAT_DEATH_DATE_M23 m23_acc_ddorres
	
	*check for duplicates: 
	duplicates tag momid pregid, gen(duplicate)
	tab duplicate
	
	list if duplicate >= 1 ,sepby(pregid)
	
	tab  CLOSEOUT_TYPE if duplicate >= 1
	
	**#stop & address duplicates: 
	assert CLOSEOUT_TYPE !=3 if duplicate >= 1 
	*duplicates are okay as long as there are no deaths

	
	if $runquery == 1 {
		levelsof(site) if duplicate >= 1 , local(sitelev) clean
		foreach site of local sitelev {
			export excel site momid pregid CLOSEOUT_DT using "$queries/`site'-endpoints-queries-$dadate.xlsx"  if site=="`site'" & duplicate >= 1 , sheet("duplicate-closeout",modify)  firstrow(variables) 
		}
	}

	bysort pregid (CLOSEOUT_DT) : gen dup_num = _n
	drop if dup_num!=1
	duplicates tag momid pregid, gen(duplicates2)
	tab duplicates2
	drop duplicates duplicates2 dup_num
	
	*Merge in info to construct GA at closeout 	
	rename momid MOMID 
	rename pregid PREGID 

	
	merge 1:1 MOMID PREGID using "$OUT/MAT_ENROLL"
	
	tab _merge site, m 
	
	replace site = SITE if site == ""
	
	keep if _merge == 1 | _merge == 3 
	
	drop _merge 
	
	rename PREG_START_DATE STR_PREG_START_DATE
	
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td
	
	*Merge in info to add pregnancy endpoint: 
	
	merge 1:1 MOMID PREGID using "$wrk/maternal_endpoints_MNH09", keepusing(PREG_END_DATE PREG_END)
	
	keep if _merge == 1 | _merge == 3 
	
	drop _merge 
	
	*Generate a GA at closeout: 

	
	gen CLOSEOUT_GA = CLOSEOUT_DT - PREG_START_DATE if ///
		CLOSEOUT == 1 & CLOSEOUT_DT != . & PREG_START_DATE != . & ///
		((CLOSEOUT_DT < PREG_END_DATE & PREG_END == 1) | ///
		PREG_END == .)
		
		// For closeouts with no pregnancy endpoint & CLOSEOUT_GA >294 days, remove CLOSEOUT_GA 
		replace CLOSEOUT_GA = . if CLOSEOUT_GA >294
		// clean if unknown closeout timing: 
		replace CLOSEOUT_GA = . if CLOSEOUT_GA <0
		
		label var CLOSEOUT_GA "GA at closeout (if during pregnancy)"
		
	list site CLOSEOUT PREG_START_DATE CLOSEOUT_DT ///
		CLOSEOUT_GA PREG_END PREG_END_DATE CLOSEOUT_TYPE if CLOSEOUT == 1 
		
	*** Save a closeout dataset:
	rename site site_MNH23 
	
	keep site_MNH23 MOMID PREGID CLOSEOUT CLOSEOUT_TYPE CLOSEOUT_DT CLOSEOUT_GA m23_acc_ddorres MAT_DEATH_M23 MAT_DEATH_DATE_M23
		
	save "$wrk/CLOSEOUT", replace 
	
	clear 
	
	
	**Pull maternal death from MNH10
	import delimited "$da/mnh10_merged", bindquote(strict) clear
	rename m10_* *
	
	replace mat_visit_mnh10 = 8 if site=="Kenya" & mat_visit_mnh10 == 8
	gen MAT_DEATH_MNH10 = 1 if ///
	mat_vital_mnh10 == 2 |  mat_visit_mnh10 == 8 | ///
	(mat_death_dthdat!= "1907-07-07" & mat_death_dthdat!= "1905-05-05" & ///
	mat_death_dthdat !="")
	label var MAT_DEATH_MNH10 "death identified in MNH10"

	gen MAT_DEATH_MNH10_DATE = date(mat_death_dthdat, "YMD") if ///
	MAT_DEATH_MNH10 ==1 
	label var MAT_DEATH_MNH10_DATE "Date of death"
	format MAT_DEATH_MNH10_DATE %td
	
	gen VISIT_DOD_MNH10 = date(visit_obsstdat, "YMD") if ///
	MAT_DEATH_MNH10 == 1 
	label var VISIT_DOD_MNH10 "date death was recorded"
	
	gen VISIT_LAST_ALIVE_MNH10 = date(visit_obsstdat, "YMD") if ///
	MAT_DEATH_MNH10 != 1
	
	format VISIT_DOD_MNH10 MAT_DEATH_MNH10_DATE VISIT_LAST_ALIVE_MNH10 %td
	keep site momid pregid MAT_DEATH_MNH10 MAT_DEATH_MNH10_DATE VISIT_DOD_MNH10 VISIT_LAST_ALIVE_MNH10
	save "$wrk/mnh10.dta" , replace
	
	
**Bring in MNH12
	import delimited "$da/mnh12_merged", bindquote(strict) clear
	rename m12_* *
	
	replace mat_visit_mnh12=88 if site=="Kenya" & mat_visit_mnh12==8
	gen MAT_DEATH_MNH12=1 if mat_visit_mnh12==8 | mat_vital_mnh12==2 | ///
	(mat_death_dthdat!="1907-07-07" & mat_death_dthdat!="1905-05-05" & ///
	mat_death_dthdat != "" )
	label var MAT_DEATH_MNH12 "maternal death identified in mnh12"

	gen MAT_DEATH_MNH12_DATE = date( mat_death_dthdat , "YMD") if ///
	MAT_DEATH_MNH12 == 1


	gen VISIT_DOD_MNH12_DATE = date( visit_obsstdat , "YMD") if ///
	MAT_DEATH_MNH12 ==1
	label var VISIT_DOD_MNH12_DATE "date death was recorded"

	gen VISIT_LAST_ALIVE_MNH12 = date(visit_obsstdat , "YMD") if ///
	MAT_DEATH_MNH12 !=1
	label var VISIT_LAST_ALIVE_MNH12 "date last seen alive during mnh12"
	
	format MAT_DEATH_MNH12_DATE VISIT_DOD_MNH12_DATE VISIT_LAST_ALIVE_MNH12 %td

	**COLLAPSE
	collapse (min) MAT_DEATH_MNH12 MAT_DEATH_MNH12_DATE VISIT_DOD_MNH12_DATE ///
	(max) VISIT_LAST_ALIVE_MNH12 , by(site momid pregid)
	save  "$wrk/mnh12_collapsed.dta" , replace
	
	
/////////////////////////////////////////////////////////////
***Incorporate new form : MNH37
/////////////////////////////////////////////////////////////
	import delimited "Z:\SynapseCSVs\Ghana/$dadate/mnh37.csv", ///
	bindquote(strict) varnames(1) case(upper) clear
	gen SITE="Ghana"
	cap tab VA_TYPE
	if _rc > 0 {
		keep SITE MOMID PREGID FINAL_MAT_DAT 
	}
	else  {
		keep SITE MOMID PREGID FINAL_MAT_DAT VA_TYPE
	}
	
	save "$wrk/mnh37_GH.dta",replace

	
	import delimited "Z:\SynapseCSVs\India_CMC/$dadate/mnh37.csv", ///
	bindquote(strict) varnames(1) case(upper) clear
	gen SITE="India-CMC"
	rename FINAL_MAT_DAT FINAL_MAT_DAT_STR
	gen FINAL_MAT_DAT=date(FINAL_MAT_DAT,"YMD")
	cap tab VA_TYPE
	if _rc > 0 {
		keep SITE MOMID PREGID FINAL_MAT_DAT 
	}
	else  {
		keep SITE MOMID PREGID FINAL_MAT_DAT VA_TYPE
		keep if VA_TYPE==1
	}
	
	
	save "$wrk/mnh37_CMC.dta",replace
	
	
	import delimited "Z:\SynapseCSVs\India_SAS/$dadate/mnh37.csv", ///
	bindquote(strict) varnames(1) case(upper) clear
	gen SITE="India-SAS"
	rename FINAL_MAT_DAT FINAL_MAT_DAT_STR
	gen FINAL_MAT_DAT=date(FINAL_MAT_DAT,"YMD")
	cap tab VA_TYPE
	if _rc > 0 {
		keep SITE MOMID PREGID FINAL_MAT_DAT 
	}
	else  {
		keep SITE MOMID PREGID FINAL_MAT_DAT VA_TYPE
		keep if VA_TYPE==1
	}
	
	save "$wrk/mnh37_SAS.dta",replace
	
	
	import delimited "Z:\SynapseCSVs\Kenya/$dadate/mnh37.csv", ///
	bindquote(strict) varnames(1) case(upper) clear
	gen SITE="Kenya"
	rename FINAL_MAT_DAT FINAL_MAT_DAT_STR
	gen FINAL_MAT_DAT=date(FINAL_MAT_DAT,"YMD")
	cap tab VA_TYPE
	if _rc > 0 {
		keep SITE MOMID PREGID FINAL_MAT_DAT 
	}
	else  {
		keep SITE MOMID PREGID FINAL_MAT_DAT VA_TYPE
		keep if VA_TYPE==1
	}
	
	save "$wrk/mnh37_KY.dta",replace
	
	
	import delimited "Z:\SynapseCSVs\Pakistan/$dadate/mnh37.csv", ///
	bindquote(strict) varnames(1) case(upper) clear
	gen SITE="Pakistan"
	rename FINAL_MAT_DAT FINAL_MAT_DAT_STR
	gen FINAL_MAT_DAT=date(FINAL_MAT_DAT,"DMY")
	cap tab VA_TYPE
	if _rc > 0 {
		keep SITE MOMID PREGID FINAL_MAT_DAT 
	}
	else  {
		keep SITE MOMID PREGID FINAL_MAT_DAT VA_TYPE
		keep if VA_TYPE==1
	}
	
	save "$wrk/mnh37_PK.dta",replace
	
	
	import delimited "Z:\SynapseCSVs\Zambia/$dadate/mnh37.csv", ///
	bindquote(strict) varnames(1) case(upper) clear
	gen SITE="Zambia"
	rename FINAL_MAT_DAT FINAL_MAT_DAT_STR
	gen FINAL_MAT_DAT=date(FINAL_MAT_DAT,"DMY")
	cap tab VA_TYPE
	if _rc > 0 {
		keep SITE MOMID PREGID FINAL_MAT_DAT 
	}
	else  {
		keep SITE MOMID PREGID FINAL_MAT_DAT VA_TYPE
		keep if VA_TYPE==1
	}
	
	save "$wrk/mnh37_ZM.dta",replace
	
	use "$wrk/mnh37_GH.dta", clear
	append using "$wrk/mnh37_CMC.dta"
	append using "$wrk/mnh37_SAS.dta"
	append using "$wrk/mnh37_KY.dta"
	append using "$wrk/mnh37_PK.dta"
	append using "$wrk/mnh37_ZM.dta"
	
	format FINAL_MAT_DAT %td
	save "$wrk/mnh37.dta", replace
	
	
///////////////////////////////////////////////////////////////////////
*** MERGE IN PREGNANCY ENDPOINT DATASET WITH CLOSEOUT & DEATH INDICATORS 

	use "$wrk/maternal_endpoints_MNH09" , clear
	
	*merge in closeout: 
	merge 1:1 MOMID PREGID using "$wrk/CLOSEOUT"
	drop _merge 
	rename MOMID PREGID , lower
	merge 1:1 momid pregid using "$wrk/mnh10.dta",nogenerate
	merge 1:1 momid pregid using "$wrk/mnh12_collapsed.dta", nogenerate
	merge 1:1 momid pregid using "$wrk/endpoint_outcomes_MNH19.dta", nogenerate
	
	replace CLOSEOUT = 0 if CLOSEOUT == . 
	replace PREG_END = 0 if CLOSEOUT == 1 & PREG_END == . 
	
	
	
	*merge 1:1 momid pregid using "$OUT/MAT_MORTALITY", keepusing(MAT_DEATH_DATE MAT_DEATH)
	
	gen MAT_DEATH = 1 if ///
	PREG_DEATH_MNH04 ==1 | MAT_DEATH_MNH09 == 1 | ///
	MAT_DEATH_MNH10 ==1 | MAT_DEATH_MNH12==1 | ///
	MAT_DEATH_M19 ==1 | MAT_DEATH_M23 == 1
	list SITE site_MNH23 site site_MNH19 PREG_DEATH_MNH04 MAT_DEATH_MNH09 MAT_DEATH_MNH10 MAT_DEATH_MNH12 MAT_DEATH_M19 MAT_DEATH_M23 if MAT_DEATH==1
	*mostly coming from MNH23

	*browse if PREG_END==.  & CLOSEOUT !=1 & MAT_DEATH !=1
	*drop if no pregnancy endpoint AND no closeout AND no death: 	
	drop if PREG_END==.  & CLOSEOUT !=1 & MAT_DEATH !=1
	**these are participants who have mnh10 or 12 but no preg end
	
	cap drop _merge 
	
	*fix after merge in deaths:
	rename momid MOMID 
	rename pregid PREGID 
	
	
	*Merge in info to construct GA: 	
	
	merge 1:1 MOMID PREGID using "$wrk/MAT_ENROLL"
	
	tab _merge site, m 
	
	replace site = SITE if site == ""
	
	keep if _merge == 3 & ENROLL == 1
	
	drop _merge 
	
	rename PREG_START_DATE STR_PREG_START_DATE
	
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td	
	
	
	*#Generate GA at maternal death for cases where:
		*Date is available 
		*Pregnancy was not reported as ended in MNH09/MNH04/MNH19 
		
		foreach var in PREG_DEATH_DATE_MNH04 MAT_DEATH_MNH09_DATE MAT_DEATH_MNH10_DATE MAT_DEATH_MNH12_DATE MAT_DEATH_DATE_M19 MAT_DEATH_DATE_M23 {
			replace `var' = . if `var' <0
			format `var' %td
		}
		gen MAT_DEATH_DATE = min( PREG_DEATH_DATE_MNH04, MAT_DEATH_MNH09_DATE, MAT_DEATH_MNH10_DATE, MAT_DEATH_MNH12_DATE, MAT_DEATH_DATE_M19, MAT_DEATH_DATE_M23)
		gen MAT_DEATH_MISSDATE = 1 if MAT_DEATH_DATE==. & MAT_DEATH == 1
		
		
		replace SITE = site_MNH23 if SITE == ""
replace SITE = site if SITE == ""
replace SITE = site_MNH19 if SITE == ""
		
		list SITE MOMID PREG_DEATH_MNH04 MAT_DEATH_MNH09 MAT_DEATH_M23 MAT_DEATH_MNH10 MAT_DEATH_MNH12 MAT_DEATH_M19 if MAT_DEATH_MISSDATE==1
		

		
		
if $runquery == 1 {
		levelsof(SITE) if MAT_DEATH_MISSDATE==1 , local(sitelev) clean
		foreach site of local sitelev {
			export excel SITE MOMID PREGID PREG_DEATH_MNH04  MAT_DEATH_MNH09 MAT_DEATH_MNH10 MAT_DEATH_MNH12 MAT_DEATH_M19 MAT_DEATH_M23  using "$queries/`site'-endpoints-queries-$dadate.xlsx"  if SITE=="`site'" & MAT_DEATH_MISSDATE==1 , sheet("missing-DoD",modify)  firstrow(variables) 
		}
	}
		
		**#stop and check how many are missing death dates
		stop and check how many are missing death dates
		
		*n=1 in Ghana 09-06 data set
		
		*n= 1 in Ghana and n=1 in Kenya in the 10-04 data set
		*both recorded in MNH12
		
		*in 11-15 data set, 1 in Ghana, 1 in Kenya, 1 in Zambia
		*all are postpartum deaths, follow the below workflow:
		
			
		*check when last seen alive and date when death was recorded
		
		list SITE VISIT_LAST_ALIVE_MNH09 VISIT_LAST_ALIVE_MNH10 VISIT_LAST_ALIVE_MNH12 VISIT_DOD_MNH09 VISIT_DOD_MNH10 VISIT_DOD_MNH12_DATE LASTALIVE_M19 if MAT_DEATH_MISSDATE==1, abbr(15)
		
	/*
	gen LASTALIVE = ///
	max(VISIT_LAST_ALIVE_MNH09, VISIT_LAST_ALIVE_MNH10, VISIT_LAST_ALIVE_MNH12)
	gen VISIT_DOD = ///
	min( VISIT_DOD_MNH09, VISIT_DOD_MNH10, VISIT_DOD_MNH12_DATE)
	
	
	gen MIDPOINT_LASTALIVE_DEAD= ((VISIT_DOD-LASTALIVE)/2) + LASTALIVE
		
	format LASTALIVE MIDPOINT_LASTALIVE_DEAD VISIT_DOD %td	
	list SITE LASTALIVE MIDPOINT_LASTALIVE_DEAD VISIT_DOD if MAT_DEATH_MISSDATE==1
	
	*show the momids at each site with a missing date
	levelsof(SITE) if MAT_DEATH_MISSDATE==1,local(sitelev) clean
	foreach l of local sitelev {
		qui levelsof(MOMID) if MAT_DEATH_MISSDATE==1 & SITE=="`l'", local(momlevels) clean
		foreach m of local momlevels {
			di "`l': `m'"
		}	
	}

	*/	

	**Find midpoint between date last seen alive & date death reported
		preserve
			keep if MAT_DEATH_MISSDATE==1
			
			drop  VISIT_LAST_ALIVE_MNH04 LASTALIVE_M19 VISIT_LAST_ALIVE_MNH09
			
			gen momid=MOMID
			gen pregid=PREGID
			
			
			merge 1:1 momid pregid using "$wrk/MNH04_LASTALIVE"
			keep if _merge==1 | MAT_DEATH_MISSDATE==1
			drop _merge
			
			merge 1:1 MOMID PREGID using "$wrk/MNH09_LASTALIVE"
			keep if _merge==1 | MAT_DEATH_MISSDATE==1
			drop _merge
			
			merge 1:1 momid pregid using "$wrk/MNH19_LASTALIVE"
			keep if _merge==1 | MAT_DEATH_MISSDATE==1
			drop _merge
			
			
			list VISIT_LAST_ALIVE_MNH04 VISIT_LAST_ALIVE_MNH09 LASTALIVE_M19 VISIT_LAST_ALIVE_MNH10 VISIT_LAST_ALIVE_MNH12 CLOSEOUT_DT
			
		gen VISIT_DOD = min(VISIT_DOD_MNH09, VISIT_DOD_MNH10, VISIT_DOD_MNH12_DATE, FORMCOMPL_DTHDAT_M19)	
		format VISIT_DOD %td
		label var VISIT_DOD "date of first visit where mother was recorded died"
			
		gen LASTALIVE=max(VISIT_LAST_ALIVE_MNH04, VISIT_LAST_ALIVE_MNH09, LASTALIVE_M19, VISIT_LAST_ALIVE_MNH10, VISIT_LAST_ALIVE_MNH12)
		format LASTALIVE %td
		gen MIDPOINT_LASTALIVE_DEAD = ///
		((CLOSEOUT_DT-LASTALIVE)/2) + LASTALIVE
		replace MIDPOINT_LASTALIVE_DEAD = ///
		((VISIT_DOD-LASTALIVE)/2) + LASTALIVE if MIDPOINT_LASTALIVE_DEAD ==.
		format MIDPOINT_LASTALIVE_DEAD %td
		list SITE LASTALIVE  MIDPOINT_LASTALIVE_DEAD CLOSEOUT_DT VISIT_DOD
		
		save "$wrk/MAT_DEATH_MISSDATE", replace
	
	restore
	
	merge 1:1 MOMID PREGID using "$wrk/MAT_DEATH_MISSDATE", keepusing(MIDPOINT_LASTALIVE_DEAD)

	
	replace MAT_DEATH_DATE= MIDPOINT_LASTALIVE_DEAD if MAT_DEATH_DATE==.
		
		
	gen MAT_DEATH_GA = MAT_DEATH_DATE - PREG_START_DATE if MAT_DEATH == 1 & ///
		MAT_DEATH_DATE != . & PREG_START_DATE != . & PREG_END != 1 
		
		 
		
	tab MAT_DEATH_GA if MAT_DEATH==1, m 
	gen MAT_DEATH_INFAGE = MAT_DEATH_DATE - PREG_END_DATE if ///
	MAT_DEATH==1 & MAT_DEATH_DATE != . & PREG_END_DATE != . & PREG_END == 1
	bigtab PREG_END  MAT_DEATH_GA MAT_DEATH_INFAGE if MAT_DEATH==1
	
	format MAT_DEATH_DATE %td
	sort SITE MOMID
	*identify any maternal deaths during pregnancy & not yet documented:
	list SITE PREG_START_DATE  CLOSEOUT_DT CLOSEOUT_GA MAT_DEATH_DATE ///
		MAT_DEATH_GA PREG_END PREG_END_DATE PREG_END_GA MNH04_ANY MNH19_ANY ///
		MNH09_FORM_INCOMPLETE if MAT_DEATH == 1  , sepby(SITE) 
		
	* Deaths with no reported pregnancy endpoint are (for now) reported 
	* as completed pregnancies. We will set the date to the date of maternal 
	* death, in lieu of other information: 
	gen PREG_LOSS_DEATH = 1 if MAT_DEATH == 1 & ///
		PREG_END != 1 & PREG_END_DATE == . & MAT_DEATH_GA <290
		
	*Incorporate deaths into variable "PREGEND"
	replace PREG_END_DATE = MAT_DEATH_DATE if MAT_DEATH == 1 & ///
		PREG_END != 1 & PREG_END_DATE == . & MAT_DEATH_GA <290
		
	replace PREG_END_GA = PREG_END_DATE - PREG_START_DATE if ///
		PREG_END_DATE != . & PREG_START_DATE != . & PREG_END_GA == . 

	replace PREG_END=1 if PREG_LOSS_DEATH == 1 
	
	*RE_FIX negative case: 
	list MOMID PREGID SITE PREG_START_DATE PREG_END_DATE PREG_END_GA ///
		ENROLL_SCRN_DATE ///
		if PREG_END_GA <0 & PREG_END_GA != .
		


	
	*review info: 
	*histogram PREG_END_GA, by(site,col(1)) percent
	bigtab site PREG_END_GA MAT_DEATH if PREG_END_GA>300 & PREG_END_GA<.
	
	if $runquery == 1 {
		levelsof(SITE) if PREG_END_GA>300 & PREG_END_GA<., local(sitelev) clean
		foreach site of local sitelev {
			export excel SITE MOMID PREGID PREG_END_GA PREG_END_SOURCE using "$queries/`site'-endpoints-queries-$dadate.xlsx"  if SITE=="`site'" & PREG_END_GA>300 & PREG_END_GA<. , sheet("High-PREG_END_GA",modify)  firstrow(variables) 
		}
	}
	
	histogram PREG_END_GA if MAT_DEATH == 1 & MAT_DEATH_DATE == PREG_END_DATE, width(1)
	
	*review details for people missing dates: 
	tab CLOSEOUT_GA MAT_DEATH if PREG_END == 1 & PREG_END_DATE == . 
	
	
	*Generate a comprehensive variable for "STOP_DATE" where participants 
	*should not EVER be included after this date:
	
		// overall stop date: 
	gen STOP_DATE = CLOSEOUT_DT if CLOSEOUT == 1 
	replace STOP_DATE = MAT_DEATH_DATE if MAT_DEATH == 1 & MAT_DEATH_DATE != . 
	format STOP_DATE %td
	
	label var STOP_DATE "Participant no longer followed after this date (closeout or death)"
	
	tab STOP_DATE if CLOSEOUT == 1 | MAT_DEATH == 1, m 
	
		* people missing a stop date:
		rename MOMID momid 
		rename PREGID pregid
		
		list site momid pregid CLOSEOUT CLOSEOUT_TYPE CLOSEOUT_GA ///
		MAT_DEATH MAT_DEATH_DATE MAT_DEATH_GA if STOP_DATE == . & ///
		(CLOSEOUT == 1 | MAT_DEATH == 1)
		
	replace STOP_DATE = . if STOP_DATE == date("19070707", "YMD") | ///
		STOP_DATE == date("19050505", "YMD") 


	*Final variable fixes: 
	replace PREG_LOSS_DEATH =0 if PREG_LOSS_DEATH == . 
	label var PREG_LOSS_DEATH "Maternal death prior to delivery"
	
	tab PREG_LOSS_DEATH, m 
	
	replace PREG_END = 0 if PREG_END== .
		
	replace MAT_DEATH = 0 if MAT_DEATH == . 
	
	replace PREG_LOSS = 0 if PREG_LOSS == . 
		
	replace PREG_LOSS_INDUCED = 0 if PREG_LOSS_INDUCED == . 
		
	replace PREG_LOSS_DEATH = 0 if PREG_LOSS_DEATH == . 
	
	replace PREG_END_SOURCE = 4 if PREG_LOSS_DEATH == 1 & MNH04_ANY == . & ///
		MNH19_ANY == . & PREG_END_SOURCE != 1 
	
	label define srce 1 "MNH09" 2 "MNH04 only" 3 "MNH19 only" 4 "Death only"
	label values PREG_END_SOURCE srce
	
	tab PREG_END_SOURCE if PREG_END==1, m 
	
	//////////////////////////////////////////////////////////////////////////
	** REVIEW: Observations with an MNH09 & no end date - Part II **
	//////////////////////////////////////////////////////////////////////////

	tab PREG_END_DATE_MISS, m 
	
	
		/*Items to review: 
		Part I (above): 
			Visit completion status for MNH09 form?
			
			Part II (here): 
			How many people have a closeout form?
			What is the time period between MNH09 form date & closeout date?
			What is the GA at MNH09 form fill date? Is it at/after 42 weeks?
			Do any of these observations have PNC forms?
			
		*/
		
	tab CLOSEOUT SITE if MNH09_FORM_INCOMPLETE == 1 
	
	gen MNH09_CLOSEOUT_DIFF = CLOSEOUT_DT - FORM_DATE if ///
		MNH09_FORM_INCOMPLETE == 1 
	tab MNH09_CLOSEOUT_DIFF SITE
	
	list MNH09_CLOSEOUT_DIFF CLOSEOUT_DT FORM_DATE if ///
		MNH09_FORM_INCOMPLETE == 1 
		
	preserve 
	
		keep if MNH09_FORM_INCOMPLETE == 1  
		
		*subset the data for further review: 
		
		save "$wrk/REVIEW_EMPTY_MNH09_FORMS", replace 
		
	restore 
	
	keep if CLOSEOUT == 1 | PREG_END == 1 
	
		
	//////////////////////////////////////////////////////////////////////////
	** REVIEW: Observations with an MNH09 & no end date **
	//////////////////////////////////////////////////////////////////////////
	
	rename momid MOMID 
	rename pregid PREGID
	
	order SITE MOMID PREGID PREG_END PREG_END_GA PREG_END_DATE ///
			PREG_LOSS PREG_LOSS_INDUCED PREG_LOSS_DEATH ///
			CLOSEOUT CLOSEOUT_DT CLOSEOUT_GA CLOSEOUT_TYPE ///
			MAT_DEATH MAT_DEATH_DATE MAT_DEATH_GA ///
			STOP_DATE PREG_END_SOURCE 
			
		keep SITE MOMID PREGID PREG_END PREG_END_GA PREG_END_DATE ///
			PREG_LOSS PREG_LOSS_INDUCED PREG_LOSS_DEATH ///
			CLOSEOUT CLOSEOUT_DT CLOSEOUT_GA CLOSEOUT_TYPE ///
			MAT_DEATH MAT_DEATH_DATE MAT_DEATH_GA ///
			STOP_DATE PREG_END_SOURCE  MAT_DEATH_MISSDATE MAT_DEATH_INFAGE
			
			
	
merge 1:1 MOMID PREGID using "$OUT/MAT_ENROLL.dta", gen(ENROLL_MERGE)
keep if ENROLL_MERGE==3
drop ENROLL_MERGE
	
	*replace PREG_END_DATE = date(EDD_BOE, "YMD") if PREG_END_DATE==. 
//if no pregnancy end date or DOB

//step 2: calculate window of time up to 42 days after pregnancy end date
	cap drop PREG_END_PP42_DT
	gen PREG_END_PP42_DT = PREG_END_DATE + 42 if PREG_END_DATE>0 & PREG_END_DATE!=.
	replace PREG_END_PP42_DT = date(EDD_BOE, "YMD") + 42 if PREG_END_DATE == . 
	//if no DOB
	format PREG_END_DATE PREG_END_PP42_DT MAT_DEATH_DATE %td

	
	*Calculate GA or infant age at time of death
	gen DEATH_GA = MAT_DEATH_DATE-date(PREG_START_DATE, "YMD") if ///
	MAT_DEATH == 1
	replace DEATH_GA = . if MAT_DEATH_DATE > PREG_END_DATE
	replace MAT_DEATH_GA = DEATH_GA if MAT_DEATH_GA ==. & DEATH_GA != .
	assert DEATH_GA == MAT_DEATH_GA
	list SITE DEATH_GA PREG_START_DATE PREG_END_DATE PREG_END MAT_DEATH_GA MAT_DEATH_MISSDATE  if DEATH_GA != MAT_DEATH_GA
	// this will flag if any differences
	drop DEATH_GA
	//if no differences, safe to drop
	
//step 3: code if within 42 days
	gen MAT_DEATH_42 = 1 if /// occured during pregnancy - 42 days pp
	MAT_DEATH_DATE <= PREG_END_PP42_DT
	replace MAT_DEATH_42 =2 if /// occured >42 days pp
	MAT_DEATH_DATE > PREG_END_PP42_DT & MAT_DEATH_INFAGE<=365
	replace MAT_DEATH_42 = 55 if /// missing
	MAT_DEATH_DATE<0
	//negative values are the default values, meaning DoD unknown
	replace MAT_DEATH_42 = 55 if ///
	MAT_DEATH==1 & MAT_DEATH_DATE==.

	label define MAT_DEATH_42 ///
	1"Preg-related death" 2"Late preg-related death" 55"Missing DoD"
	label val MAT_DEATH_42 MAT_DEATH_42		
	
	bigtab MAT_DEATH_42 MAT_DEATH_GA  MAT_DEATH_INFAGE if MAT_DEATH==1
	
	
		if $runquery == 1 {
		
	*pull IDs for weird closeout situations
	
	gen CLOSEOUT_DAYS_PP = CLOSEOUT_DT- PREG_END_DATE
	
	*Situation #1: 
	*closeout < 40 days and closeout type == " 2-Follow-up complete (42 days)"
	tab SITE if CLOSEOUT_TYPE==2 & CLOSEOUT_DAYS_PP <40
	
	
	levelsof(SITE) if CLOSEOUT_TYPE==2 & CLOSEOUT_DAYS_PP <40 , local(sitelev) clean
		foreach site of local sitelev {
		export excel SITE MOMID PREGID PREG_END CLOSEOUT_DAYS_PP CLOSEOUT_TYPE using "$queries/`site'-endpoints-queries-$dadate.xlsx"  if SITE=="`site'" & CLOSEOUT_TYPE==2 & CLOSEOUT_DAYS_PP <40 , sheet("Closeout-LT40",modify)  firstrow(variables)  
	}
	
	
	*Situation #2:
	*closeout type == 2 and PREG_END==0
	tab SITE if CLOSEOUT_TYPE==2 & PREG_END==0
	
	levelsof(SITE) if CLOSEOUT_TYPE==2 & PREG_END==0 , local(sitelev) clean
		foreach site of local sitelev {
		export excel SITE MOMID PREGID PREG_END CLOSEOUT_DAYS_PP CLOSEOUT_TYPE using "$queries/`site'-endpoints-queries-$dadate.xlsx"  if SITE=="`site'"  & PREG_END==0 & (CLOSEOUT_TYPE==2 | CLOSEOUT_TYPE == 1) , sheet("No-Preg-End",modify)  firstrow(variables) 
	}
	
		
	*SITUATION #3:
	*Closeout_type == "1-Follow-up complete (1 yr)" but closeout days postparum < 300 days
	bigtab SITE CLOSEOUT_TYPE if CLOSEOUT_TYPE==1 & CLOSEOUT_DAYS_PP<300
	
	
	levelsof(SITE) if CLOSEOUT_TYPE==1 & CLOSEOUT_DAYS_PP<300 & PREG_END==1 , local(sitelev) clean
		foreach site of local sitelev {
		export excel SITE MOMID PREGID PREG_END CLOSEOUT_DAYS_PP CLOSEOUT_TYPE ///
		using "$queries/`site'-endpoints-queries-$dadate.xlsx"  if ///
		SITE=="`site'" & (CLOSEOUT_TYPE==1 & CLOSEOUT_DAYS_PP<300 & PREG_END==1) , ///
		sheet("Closeout-LT300",modify)  firstrow(variables) 
	}
	
	
	*Situation 4: the closeout type is not specified
	levelsof(SITE) if CLOSEOUT_TYPE >10 & CLOSEOUT == 1, local(sitelev) clean
	foreach site of local sitelev {
		export excel SITE MOMID PREGID CLOSEOUT_TYPE using ///
		"$queries/`site'-endpoints-queries-$dadate.xlsx"  if ///
		SITE=="`site'" & CLOSEOUT_TYPE>10 & CLOSEOUT == 1, ///
		sheet("Miss-closeout-reason", modify) firstrow(variables)
	}
	
	}
			
	* ADD IN PNC VISIT WINDOWS: 
	
	// construct empty vars: 
	foreach let in PNC0 PNC1 PNC4 PNC6 PNC26 PNC52 {
	    
	gen `let'_ONTIME_WINDOW = .
	format `let'_ONTIME_WINDOW %td
	label var `let'_ONTIME_WINDOW "Date of last day of on-time `let' window"
	
	gen `let'_LATE_WINDOW = .
	format `let'_LATE_WINDOW %td
	label var `let'_LATE_WINDOW "Date of last day of late `let' window"
	
	gen `let'_PASS_LATE = 0 if PREG_END==1
	label var `let'_PASS_LATE "Indicator for participant passed the late `let' window"
	
	gen `let'_PASS_ONTIME = 0 if PREG_END==1
	label var `let'_PASS_ONTIME "Indicator for participant passed the on-time `let' window"
		
	}
	
	

	

	
	*Windows below drawn for consistency with the monitoring report: 
	*https://docs.google.com/spreadsheets/d/11VutJeWo5lH2RlSpgPHv7hrHqNt2BCXrZm8XRjdS3j0/edit?usp=sharing 
	
	// PNC0: 
		// On-time: =DOB + 5
		// Late: =DOB + 5
	replace PNC0_ONTIME_WINDOW = PREG_END_DATE + 5 if PREG_END==1 & PREG_END_DATE != . 
	replace PNC0_LATE_WINDOW = PREG_END_DATE + 5 if PREG_END==1 & PREG_END_DATE != . 
	
	// PNC1: 
		// On-time: =DOB + 14
		// Late: =DOB + 14
	replace PNC1_ONTIME_WINDOW = PREG_END_DATE + 14 if PREG_END==1 & PREG_END_DATE != . 
	replace PNC1_LATE_WINDOW = PREG_END_DATE + 14 if PREG_END==1 & PREG_END_DATE != . 
	
	// PNC4: 
		// On-time: =DOB + 35
		// Late: =DOB + 35
	replace PNC4_ONTIME_WINDOW = PREG_END_DATE + 35 if PREG_END==1 & PREG_END_DATE != . 
	replace PNC4_LATE_WINDOW = PREG_END_DATE + 35 if PREG_END==1 & PREG_END_DATE != . 
	
	
	// PNC6: 
		// On-time: =DOB + 55
		// Late: =DOB + 104
	replace PNC6_ONTIME_WINDOW = PREG_END_DATE + 55 if PREG_END==1 & PREG_END_DATE != . 
	replace PNC6_LATE_WINDOW = PREG_END_DATE + 104 if PREG_END==1 & PREG_END_DATE != . 
	
	
	// PNC26: 
		// On-time: =DOB + 202
		// Late: =DOB + 279
	replace PNC26_ONTIME_WINDOW = PREG_END_DATE + 202 if PREG_END==1 & PREG_END_DATE != . 
	replace PNC26_LATE_WINDOW = PREG_END_DATE + 279 if PREG_END==1 & PREG_END_DATE != . 
	
	
	// PNC52: 
		// On-time: =DOB + 384
		// Late: =DOB + 454
	replace PNC52_ONTIME_WINDOW = PREG_END_DATE + 384 if PREG_END==1 & PREG_END_DATE != . 
	replace PNC52_LATE_WINDOW = PREG_END_DATE + 454 if PREG_END==1 & PREG_END_DATE != . 
	
		
	
	// finalize indicators: 
	foreach let in PNC0 PNC1 PNC4 PNC6 PNC26 PNC52 {	
	
		// on-time: 
	replace `let'_PASS_ONTIME = 1 if `let'_ONTIME_WINDOW < date("$dadate" ,"YMD") & ///
		`let'_ONTIME_WINDOW != . 
	
		// late: 
	replace `let'_PASS_LATE = 1 if `let'_LATE_WINDOW < date("$dadate" ,"YMD") & ///
		`let'_LATE_WINDOW != . 	
	
	}		
	
	
	list PREG_END PREG_END_DATE PNC26_LATE_WINDOW PNC26_PASS_LATE if PNC26_PASS_LATE == 1 
	list PREG_END PREG_END_DATE PNC26_LATE_WINDOW PNC26_PASS_LATE if PNC26_PASS_LATE == 0 
	list PREG_END PREG_END_DATE PNC26_LATE_WINDOW PNC26_PASS_LATE if PNC26_PASS_LATE == . 
	
	*missing site variables
	drop SITE
	merge 1:1 MOMID PREGID using "$OUT/MAT_ENROLL.dta", keepusing(SITE)
	keep if _merge==3
	tab SITE if PREG_END==., miss
	
	drop _merge 
	assert PREG_END_GA == . if PREG_END == 0
	assert PREG_END_DATE == . if PREG_END == 0
	
	cap assert PREG_END_GA <. & PREG_END_DATE <.  if PREG_END == 1
	if _rc != 0 { 
		*if error code is captured, run below code
		
		if $runquery == 1 {
		
	levelsof(SITE) if PREG_END_DATE ==. & PREG_END == 1, local(sitelev) clean 
	*which sites have this error?
		
		foreach site of local sitelev {
			*for each site with the error, export the following excel:
					
		export excel SITE MOMID PREGID PREG_END PREG_END_SOURCE using ///
		"$queries/`site'-endpoints-queries-$dadate.xlsx" if ///
		SITE=="`site'" & PREG_END_DATE ==. & PREG_END == 1, ///
		sheet("Miss-PREG_END_DATE",modify)  firstrow(variables) 
			
		}
		
		}
		
	}

	keep SITE MOMID PREGID PREG_END PREG_END_GA PREG_END_DATE PREG_LOSS PREG_LOSS_INDUCED PREG_LOSS_DEATH CLOSEOUT CLOSEOUT_DT CLOSEOUT_GA CLOSEOUT_TYPE MAT_DEATH MAT_DEATH_DATE MAT_DEATH_GA STOP_DATE PREG_END_SOURCE MAT_DEATH_MISSDATE MAT_DEATH_INFAGE PREG_END_PP42_DT MAT_DEATH_42 CLOSEOUT_DAYS_PP PNC0_* PNC1_* PNC4_* PNC6_* PNC26_*  PNC52_* 
	save "$OUT/MAT_ENDPOINTS", replace 
	
	*Save to outcomes folder once reviewed:
	*save "Z:\Outcome Data/$dadate\MAT_ENDPOINTS",replace

	
	
	if $runquery == 1 {
		
		use "Z:\Savannah_working_files\Endpoints/$dadate\REVIEW_EMPTY_MNH09_FORMS.dta", clear
		
		gen formfill_window = FORM_DATE + 60 if ///
			FORM_DATE != . & CLOSEOUT == 0 
			format formfill_window %td
		
		levelsof(SITE) if formfill_window < date("$dadate","YMD"), local(sitelev) clean 
		
		foreach site of local sitelev {
			
			preserve
			
			sort FORM_DATE
			export excel SITE momid pregid CLOSEOUT FORM_DATE using ///
			"$queries/`site'-endpoints-queries-$dadate.xlsx" if ///
			SITE=="`site'" & formfill_window < date("$dadate","YMD"), ///
			sheet("Empty-MNH09",modify)  firstrow(variables) 
			
			restore

			}
		
	}
	
	
	
	clear 
	
	/*
//////////////////////////////////////////////
* * * For 9-16: Review empty MNH09s * * * 
//////////////////////////////////////////////

	* MNH10: 
	
	import delimited "$da/mnh10_merged", bindquote(strict)
	
	merge 1:1 momid pregid using "$wrk/REVIEW_EMPTY_MNH09_FORMS"
	
	keep if _merge == 3
	
	list m10*
	
	clear 
	
	
	* MNH11: 
	
	import delimited "$da/mnh11_merged", bindquote(strict)
	
	merge m:1 momid pregid using "$wrk/REVIEW_EMPTY_MNH09_FORMS"
	
	keep if _merge == 3
	
	list m11*
	
	clear 	
	
	
	* MNH12: 
	
	import delimited "$da/mnh12_merged", bindquote(strict)
	
	merge m:1 momid pregid using "$wrk/REVIEW_EMPTY_MNH09_FORMS"
	
	keep if _merge == 3
	
	list m12*
	
	clear 	
	
	
	* Any Infant Outcome
	
	import delimited "$OUT/INF_OUTCOMES", bindquote(strict)
	
	merge m:1 momid pregid using "$wrk/REVIEW_EMPTY_MNH09_FORMS"
	
	keep if _merge == 3
	*/
	
	
