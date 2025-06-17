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
*Update: September 17, 2024 by E Oakley (split up PREG_END file; this file now creates outcomes ONLY)
*Update: October 8, 2024 by E Oakley (add separate induced labor outcome)
*Update: October 22, 2024 by E Oakley (update to induced labor outcome)
*Update: October 23, 2024 by E Oakley (create induced labor missing-ness indicator)
*Update: January 10, 2025 by E Oakley (updates to variable names per convention)
*Update: March 11, 2025 by E Oakley (add outcomes for IHME: PROM; MEM_HOURS)


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
global dadate "2025-04-18" // SYNAPSE/UPLOAD DATE 

global da "Z:/Stacked Data/$dadate" // update date as needed

global OUT "Z:/Outcome Data/$dadate"

	// Working Files Folder (TNT-Drive)
global wrk "Z:/Erin_working_files/data" // set pathway here for where you want to save output data files (i.e., constructed analysis variables)

global date "250428" // today's date

log using "$log/mat_outcome_construct_preg_end_$date", replace



*NOTE: This file is now updated to create outcomes ONLY. For the construction
*of completed pregnancy variables, including PREG_END, PREG_END_DATE, 
*PREG_END_GA, and PNC visit windows see the .do file: mat_outcomes_MAT_ENDPOINTS

/*************************************************************************
	*Variables constructed in this do file:
					
	*Preterm delivery (PRETERM_ANY)
		Delivery prior to 37 completed weeks of gestation of a birth (live or
			stillbirth). 
		denominator: Completed pregnancies (PREG_END==1), excluding those with 
			pregnancy loss at <20 weeks GA (PREG_LOSS==0)
		format: 1, Yes; 0, No; 55, Missing
				
	*Preterm birth classification - spontaneous (PRETERM_SPON)
		Spontaneous preterm: Defined as delivery <37 weeks that occurs either 
			secondary to preterm labor or preterm premature rupture of membranes.	
		denominator: Among all preterm births (PRETERM_ANY ==1), excluding those with 
			pregnancy loss at <20 weeks GA (PREG_LOSS==0)
		format: 1, Yes; 0, No; 55, Missing
				
	*Preterm birth classification - provider-initiated (PRETERM_PROV)	
		Provider-initiated preterm: Medical or obstetric complication or other 
			reason that the health care provider initiates delivery at <37 
			completed weeks gestation.	
		denominator: Among all preterm births (PRETERM_ANY ==1), excluding those with 
			pregnancy loss at <20 weeks GA (PREG_LOSS==0)
		format: 1, Yes; 0, No; 55, Missing
		
	*New variables added on 4-08-2024 based on PRISMA site feedback:
	
		*Provider-initiated preterm indication 
			A composite outcome describing the reason for provider-initiated 
			preterm delivery (i.e., specific indication reported for 
			labor induction and/or cesarean delivery w/o prior ROM or labor).
		
		*Spontaneous preterm indication 
			A composite outcome describing onset of spontaneous preterm 
			delivery as: preterm labor (i.e., labor onset <37 weeks GA and 
			preceeds ROM); preterm premature rupture of membranes (i.e., ROM 
			<37 weeks GA and preceeds preterm labor); preterm with unknown 
			order of events (i.e., a preterm birth missing data on the timing 
			of labor vs. ROM).
		
	*Preterm premature rupture of membranes - PPROM_PREGEND 	
		Rupture of membranes before the onset of labor, occurring before <37 
			weeks of gestation OR clinical diagnosis of premature rupture of
			membranes.			
		denominator: Among all completed pregnancies (PREG_END==1)
		format: 1, Yes; 0, No; 55, Missing	
		
	*PROM_HOSP 
		Same as above, but recorded as a dx while the mother is hospitalized 
		during pregnancy (drawn from MNH19, rather than from timing variables 
		recorded in MNH09)
		
	*PPROM_OCCUR 
		Same as above -- combines PPROM_PREGEND & PROM_HOSP 
		
	*Uterine Rupture  - MAT_UTER_RUP 
		Definition: Tear in the muscular wall of the uterus during pregnancy or 
		childbirth; the spontaneous tearing of the uterus prior to delivery that 
		may result in the fetus being expelled into the peritoneal cavity; 
		occurring prior to delivery or during labor. 
		Values: 0=No,1=Yes,55=Missing
		Denominator: completed pregnancies
		Variables of interest drawn from: MNH09; MNH12 (PNC-0); MNH19 (hosp)
		Subvariables constructed:
			*MAT_UTER_RUP_IPC -- recorded at pregnancy endpoint/MNH09 
			*MAT_UTER_RUP_PNC -- recorded at PNC/MNH12 
			*MAT_UTER_RUP_HOSP -- recorded during hospitalization 
		
	*Prolonged labor - PRO_LABOR 
		Definition (UPDATED 6-27): Prolonged labor is defined as labor 
		lasting ≥24 hours (regardless of parity)
		Values: 0=No,1=Yes,55=Missing
		Denominator (UPDATED 6-27): all pregnancies with labor 
		Variables of interest drawn from: MNH09
		
	*Obstructed labor - OBS_LABOR
		Definition: Obstructed labor based on check-box response -- 
		requested for G3 study. 
		Values: 0=No, 1=Yes, 55=Missing 
		Denominator (UPDATED 6-27): all pregnancies with labor 
		Variables of interest drawn from: MNH09 
		
* * * Added on 3-11-2025 for IHME data request:
	*Premature rupture of membranes - PROM
		Definition: Spontaneous rupture of membranes prior to the onset of 
		labor OR clinically diagnosed during hospitalization -- requested for 
		G3 study
		Values: 0=No, 1=Yes, 55=Missing 
		Denominator: completed pregnancies
		
	*Hours between ROM and delivery - MEM_HOURS 
		Definition: Hours between the time/date of rupture of membranes to the 
		time/date of delivery (1st infant if multiples) -- requested for G3 study
		Values: continuous
		Denominator: all pregnancies with ROM 
	
	
*/

		
//////////////////////////////////////////
	
*Added step prior to opening MMH09: search for relevant 
*outcomes reported in MNH19 (Hospitalization form): 
	
	import delimited "$da/mnh19_merged", bindquote(strict)
	
	/* Variables needed from hospitalization:
	
		DX of PROM - LD_COMPL_MHTERM_1 -- would need to check on date of 
		hospitalization to confirm if PROM or PPROM 
	
		DX of Uterine Rupture - LD_COMPL_MHTERM_7
	
		DX of Obstructed labor - LD_COMPL_MHTERM_6 
	
	*/
	
	*PROM (we will review for cases <37 weeks GA after merging)
	tab m19_ld_compl_mhterm_1, m 
	tab m19_primary_mhterm m19_ld_compl_mhterm_1, m // note: this var doesn't really account for most missing info
	
	gen PROM_HOSP = m19_ld_compl_mhterm_1 
	replace PROM_HOSP = 55 if m19_ld_compl_mhterm_1  == 77
	
	label var PROM_HOSP "Dx of PROM during hospitalization (MNH19)"
	
	gen PROM_HOSP_DT = date(m19_ohostdat, "YMD") if PROM_HOSP == 1 
	replace PROM_HOSP_DT = date(m19_mat_est_ohostdat, "YMD") if PROM_HOSP == 1 & ///
		PROM_HOSP_DT == . 
	format PROM_HOSP_DT %td
	label var PROM_HOSP_DT "Date of hospitalization - PROM Dx"
	tab PROM_HOSP_DT PROM_HOSP, m 
	
	
	* Uterine rupture:
	tab m19_ld_compl_mhterm_7, m 
	
	gen MAT_UTER_RUP_HOSP = m19_ld_compl_mhterm_7 
	replace MAT_UTER_RUP_HOSP = 55 if m19_ld_compl_mhterm_7 == 77 
	
	label var MAT_UTER_RUP_HOSP "Dx of uterine rupture during hospitalization (MNH19)"
	
	* Obstructed labor: 
	tab m19_ld_compl_mhterm_6, m 
	
	gen OBS_LABOR_HOSP = m19_ld_compl_mhterm_6 
	replace OBS_LABOR_HOSP = 55 if m19_ld_compl_mhterm_6 == 77 
	
	label var OBS_LABOR_HOSP "Dx of obstructed labor during hospitalization (MNH19)"	
	
	
	*restrict to the outcomes that we need to prep hospitalization dataset: 
	rename site site_MNH19 
	
	tab PROM_HOSP
	tab MAT_UTER_RUP_HOSP 
	tab OBS_LABOR_HOSP
	
	
		// keep if any outcomes of interest: 
	keep if PROM_HOSP == 1 | MAT_UTER_RUP_HOSP == 1 | OBS_LABOR_HOSP == 1 
	
		// restrict to constructed variables: 
	keep momid pregid site_MNH19 PROM_HOSP PROM_HOSP_DT MAT_UTER_RUP_HOSP ///
		OBS_LABOR_HOSP 
	
		// check for duplicates:
	duplicates tag momid pregid, gen(duplicate)
	tab duplicate, m 
	
	drop duplicate
	
		*no duplicates for hospitalizations with pregnancy complications/
		*endpoint information; proceed 
	
	save "$wrk/endpoint_outcomes_pregend_MNH19", replace 
	
	clear 
	
/////////////////////////
*Now can open MNH09: 
*MNH09: 
import delimited "$da/mnh09_merged", bindquote(strict)
	
	tab site, m 
	
	
	*clean up dataset: 
	keep if site == "Ghana" | site == "India-CMC" | site == "Kenya" | ///
		site == "Pakistan" | site == "Zambia" | site == "India-SAS"
		
	format momid %38s
	recast str38 momid, force
	
	
///////////////////////////////////////////////////
* * * Bring in needed datasets: enroll & endpoints 
///////////////////////////////////////////////////
	
	// Merge in ENROLLMENT INDICATOR:
	preserve 
		
		clear 
		import delimited "$OUT/MAT_ENROLL"
		
		save "$wrk/MAT_ENROLL", replace 
		
	restore
	
	rename momid MOMID 
	rename pregid PREGID 
	
	merge 1:1 MOMID PREGID using "$OUT/MAT_ENROLL"
	
	tab _merge site, m 
	
	replace site = SITE if site == ""
	
	// Merge in ENDPPINT INDICATOR: 
	drop _merge 
	
	drop EST_CONCEP_DATE_US EST_CONCEP_DATE_LMP
	
	merge 1:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", gen(merge)
	
	
	*** L&D outcomes dataset: keep only if in enrollment/BOE dataset AND has a 
	*** pregnancy endpoint: 
	
	keep if ENROLL==1 & PREG_END==1
	
	drop merge 
	
	rename PREG_START_DATE 	STR_PREG_START_DATE
	
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td
	
	sum PREG_START_DATE, format 
	
	*review of potential input variables:
	tab m09_ptb_mhoccur m09_hdp_htn_mhoccur_3, m 
	tab m09_labor_mhoccur m09_ptb_mhoccur, m 
	
	*Components: 
	
	// LABOR_ANY Did the participant experience labor: 
	gen LABOR_ANY = 0 if m09_labor_mhoccur == 0 
	replace LABOR_ANY = 1 if m09_labor_mhoccur == 1
	replace LABOR_ANY = 55 if m09_labor_mhoccur >= 55 | m09_labor_mhoccur == . 
	label var LABOR_ANY "=1 if participant experienced labor"
	
	tab m09_labor_mhoccur LABOR_ANY, m 
	
	
	// LABOR_INDUCED 
	gen LABOR_INDUCED = 0 if m09_induced_proccur == 0 
	replace LABOR_INDUCED = 1 if m09_induced_proccur == 1
	replace LABOR_INDUCED = 55 if m09_induced_proccur >= 77 | m09_induced_proccur == 55
	label var LABOR_INDUCED "=1 if labor was induced"
	
	tab m09_induced_proccur LABOR_INDUCED, m 
	
	// LABOR_SPON
	gen LABOR_SPON = 0 if m09_induced_proccur == 1 // 0 if induced
	replace LABOR_SPON = 0 if m09_labor_mhoccur == 0 // 0 if no labor
	replace LABOR_SPON = 1 if m09_induced_proccur == 0 & ///
		m09_labor_mhoccur == 1 // 1 if labor occurred & not induced
	replace LABOR_SPON = 55 if m09_induced_proccur >=55 | m09_labor_mhoccur >=55
	label var LABOR_SPON "=1 if labor was spontaneous"	
	
	tab LABOR_INDUCED LABOR_SPON, m 
	
	// CES_ANY
	tab m09_deliv_prroute_inf1, m 
	
	gen CES_ANY = 0 if m09_deliv_prroute_inf1 == 1 // 1=vaginal delivery 
	replace CES_ANY = 1 if m09_deliv_prroute_inf1 == 2 // 2=cesarean delivery
	replace CES_ANY = 0 if m09_deliv_prroute_inf1 == 3 // 3=death prior to del
	replace CES_ANY = 55 if m09_deliv_prroute_inf1 >= 55 |  m09_deliv_prroute_inf1 ==. // missing
		// add additional infants: 
	foreach num of numlist 2/4 {
	replace CES_ANY = 1 if m09_deliv_prroute_inf`num' == 2 // cesarean delivery 
	}
	label var CES_ANY "=1 if any cesarean delivery"
	tab CES_ANY, m 
	
	// CES_PLAN 
	gen CES_PLAN = 0 if CES_ANY == 0 // not a cesarean delivery
	
	replace CES_PLAN = 55 if CES_ANY == 55 // unknown mode of delivery
	
	replace CES_PLAN = 0 if CES_ANY == 1 & ( m09_ces_proccur_inf1 == 1 | ///
		m09_ces_faorres_inf2 == 1 | m09_ces_faorres_inf3 == 1 | ///
		m09_ces_faorres_inf4 == 1) // cesarean-emergent (any fetus)
		
	replace CES_PLAN = 1 if CES_ANY == 1 & m09_ces_proccur_inf1 == 2 & ///
		m09_ces_faorres_inf2 != 1 & m09_ces_faorres_inf3 != 1 & ///
		m09_ces_faorres_inf4 != 1 // cesarean-planned 
		
	replace CES_PLAN = 55 if CES_ANY == 1 & (m09_ces_proccur_inf1 == 77 | ///
		m09_ces_proccur_inf1 == 99 | m09_ces_proccur_inf1 == 55) &  ///
		(m09_ces_faorres_inf2 != 1 & ///
		m09_ces_faorres_inf2 != 2 & m09_ces_faorres_inf3 != 1 & ///
		m09_ces_faorres_inf3 != 2 & m09_ces_faorres_inf4 != 1 & ///
		m09_ces_faorres_inf4 != 2) // missing information on classification
		
	*review missing:
	list CES_ANY CES_PLAN m09_ces_proccur_inf1  m09_ces_faorres_inf2 ///
	m09_ces_faorres_inf3 m09_ces_faorres_inf4 if CES_PLAN == . 
		
	label var CES_PLAN "=1 if planned cesarean delivery (all infants)"
	
	tab CES_PLAN, m 
	
	// CES_EMERGENT 
	gen CES_EMERGENT = 0 if CES_ANY == 0 // not a cesarean 
	
	replace CES_EMERGENT = 55 if CES_ANY == 55 // unknown mode of delivery 
	
	replace CES_EMERGENT = 1 if CES_ANY == 1 & ( m09_ces_proccur_inf1 == 1 | ///
		m09_ces_faorres_inf2 == 1 | m09_ces_faorres_inf3 == 1 | ///
		m09_ces_faorres_inf4 == 1) // cesarean-emergent (any fetus)
		
	replace CES_EMERGENT = 0 if CES_ANY == 1 & m09_ces_proccur_inf1 == 2 & ///
		m09_ces_faorres_inf2 != 1 & m09_ces_faorres_inf3 != 1 & ///
		m09_ces_faorres_inf4 != 1 // cesarean-planned 
		
	replace CES_EMERGENT = 55 if CES_ANY == 1 & (m09_ces_proccur_inf1 == 77 | ///
		m09_ces_proccur_inf1 == 99 | m09_ces_proccur_inf1 == 55) & ///
		(m09_ces_faorres_inf2 != 1 & ///
		m09_ces_faorres_inf2 != 2 & m09_ces_faorres_inf3 != 1 & ///
		m09_ces_faorres_inf3 != 2 & m09_ces_faorres_inf4 != 1 & ///
		m09_ces_faorres_inf4 != 2) // missing information on classification
		
	label var CES_EMERGENT "=1 if emergent cesarean delivery (any infant)"
	
		tab CES_EMERGENT, m
		tab CES_PLAN CES_EMERGENT, m 
		
	* Check based on code review:
	
		tab m09_deliv_prroute_inf1 m09_deliv_prroute_inf2, m 
		// concern if infant 1 = c-section and infant 2 = vaginal delivery; 
		// added to queries 
		
	// MEM_SPON 
	gen MEM_SPON = 1 if m09_membrane_rupt_mhterm == 1 // spontaneous rupture
	replace MEM_SPON = 0 if m09_membrane_rupt_mhterm == 2 // induced rupture 
	replace MEM_SPON = 0 if m09_membrane_rupt_mhterm == 77 // cesarean bef rupture 
	replace MEM_SPON = 55 if m09_membrane_rupt_mhterm == 55  | ///
		m09_membrane_rupt_mhterm == . | m09_membrane_rupt_mhterm == 99 | ///
		m09_membrane_rupt_mhterm == 9 // unknown/missing 
	label var MEM_SPON "=1 if spontaneous rupture of membranes"
	
	tab m09_membrane_rupt_mhterm MEM_SPON, m 
	
	// MEM_ART 
	gen MEM_ART = 1 if m09_membrane_rupt_mhterm == 2 // induced rupture 
	replace MEM_ART = 0 if m09_membrane_rupt_mhterm == 1 // spontaneous rupture 
	replace MEM_ART = 0 if m09_membrane_rupt_mhterm == 77 // cesarean bef rupture 
	replace MEM_ART = 55 if m09_membrane_rupt_mhterm == 55 | ///
		m09_membrane_rupt_mhterm == . | m09_membrane_rupt_mhterm == 99 | ///
		m09_membrane_rupt_mhterm == 9 // unknown/missing 
	label var MEM_ART "=1 if artificial rupture of membranes"
	
	tab MEM_ART MEM_SPON, m 
	
	// MEM_CES 
	gen MEM_CES = 0 if MEM_SPON == 1 | MEM_ART == 1 // spontaenous or induced 
	replace MEM_CES = 1 if m09_membrane_rupt_mhterm == 77 // cesarean bef rupture 
	replace MEM_CES = 55 if m09_membrane_rupt_mhterm == 55 | ///
		m09_membrane_rupt_mhterm == . | m09_membrane_rupt_mhterm == 99 | ///
		m09_membrane_rupt_mhterm == 9 // unknown/missing 
	label var MEM_CES "=1 if rupture of membranes c-section related"
	
	tab MEM_CES, m 
	
	tab MEM_CES CES_ANY, m 
		
*Preterm birth classification (PRETERM_ANY)
	
	sum PREG_END_GA
	
	gen PREG_END_GA_WK = PREG_END_GA / 7 

	*Check on pregnancies by GA at endpoint: are there any sites filling MNH09 
	*for pregnancies <20 weeks GA?
	twoway histogram PREG_END_GA, w(1) by(site) ///
	xline(140) color(black) xsize(20) ysize(10)
	
	gen PRETERM_ANY = 0 if PREG_END_GA >= 259 & PREG_END_GA != . & ///
		PREG_LOSS_INDUCED != 1 
		
	replace PRETERM_ANY = 1 if PREG_END_GA < 259 & PREG_END_GA >= 140 & ///
		PREG_LOSS_INDUCED != 1 
		
	replace PRETERM_ANY = 55 if (PREG_END_GA == . | PREG_END_GA <0) & ///
		PREG_LOSS_INDUCED != 1 
		
	replace PRETERM_ANY = 77 if PREG_LOSS == 1 | (PREG_END_GA <140 & ///
		PREG_LOSS_DEATH ==1) // remove pregnancy losses/deaths <20 weeks GA
	
	replace PRETERM_ANY = 77 if PREG_LOSS_INDUCED == 1 // remove induced abortions
	
	label var PRETERM_ANY "Preterm pregnancy endpoint (by any classification)"
	
	*Check: 
	tab PREG_END_GA PRETERM_ANY, m 
	tab PREG_END_GA PREG_LOSS, m 
	
	list PREG_END_GA PREG_LOSS PREG_LOSS_DEATH PREG_LOSS_INDUCED if ///
		PRETERM_ANY==. 
		
	
	*Create "missing" indicator for the maternal outcomes report:
	gen PRETERM_ANY_MISS = 0 if PRETERM_ANY != 55
	
	replace PRETERM_ANY_MISS = 1 if PRETERM_ANY == 55 & PREG_START_DATE ==.
	replace PRETERM_ANY_MISS = 2 if PRETERM_ANY == 55 & PREG_END_DATE ==.
	replace PRETERM_ANY_MISS = 3 if PRETERM_ANY == 55 & PREG_START_DATE == . & ///
		PREG_END_DATE ==.
	
	// add the "investigate" tab: 
	replace PRETERM_ANY_MISS = 4 if PRETERM_ANY==55 & PREG_START_DATE!=. & ///
		PREG_END_DATE !=. 
	
	tab PRETERM_ANY_MISS, m 
	
	label define pt_any_m 0 "0-Non-missing" 1 "1=Missing BOE" ///
		2 "2-Missing end date" 3 "3-Missing BOE and end date" 4 "4-Other (INVESTIGATE)"
	label values PRETERM_ANY_MISS pt_any_m 
	label var PRETERM_ANY_MISS "Reason missing - PRETERM ANY"
	
	
*Review Timing Variables: Onset of Labor:
	*review variables:
	*list m09_labor_mhstdat m09_labor_mhsttim
	*Generate a date-time variable: 
	gen date_time_string = m09_labor_mhstdat + " " + m09_labor_mhsttim
	gen  double LABOR_DTT = clock(date_time_string, "YMDhm")
	format LABOR_DTT %tc
	sum LABOR_DTT, format
	
	// check on observations with missing timing: 
	list LABOR_DTT m09_labor_mhstdat m09_labor_mhsttim m09_labor_mhoccur ///
		if LABOR_DTT == . 
		
	label var LABOR_DTT "Date & time of labor onset"
		
*Review Timing Variables: Rupture of Membranes: 
	*review variables:
	*list m09_membrane_rupt_mhstdat m09_membrane_rupt_mhsttim
	*Generate a date-time variable: 
	gen date_time_string2 = m09_membrane_rupt_mhstdat + " "  ///
		+ m09_membrane_rupt_mhsttim
	gen  double MEM_DTT = clock(date_time_string2, "YMDhm") if ///
		m09_membrane_rupt_mhstdat != "1907-07-07"
	format MEM_DTT %tc
	sum MEM_DTT, format
	
	// check on observations with missing timing: 
	list MEM_DTT m09_membrane_rupt_mhstdat m09_membrane_rupt_mhsttim ///
		m09_membrane_rupt_mhterm if MEM_DTT == . 		
		
	label var MEM_DTT "Date & time of ROM"

*Examine difference (in hours): 
	generate LABOR_MEM_HOURS = hours(LABOR_DTT - MEM_DTT)
	label var LABOR_MEM_HOURS "Difference in Hours ROM to Labor Onset (neg means labor 1st)"
	sum LABOR_MEM_HOURS 
	
	generate MEM_LABOR_HOURS = hours(MEM_DTT - LABOR_DTT)
	label var MEM_LABOR_HOURS "Difference in Hours Labor Onset to ROM (neg means ROM 1st)"
	sum MEM_LABOR_HOURS
	
	
	// checks on observations with missing information: 
	
	*list LABOR_MEM_HOURS MEM_LABOR_HOURS LABOR_DTT MEM_DTT PREG_END_DATE
	
	list LABOR_MEM_HOURS MEM_LABOR_HOURS LABOR_DTT MEM_DTT PREG_END_DATE if ///
		(MEM_LABOR_HOURS > 500 | MEM_LABOR_HOURS < -500) & MEM_LABOR_HOURS !=.
	
	*Review difference in timing visually: 
	preserve 
		replace MEM_LABOR_HOURS = . if MEM_LABOR_HOURS > 200
		replace MEM_LABOR_HOURS = . if MEM_LABOR_HOURS < -500
	twoway histogram MEM_LABOR_HOURS, w(4) by(site) ///
	xline(0, lcolor(blue)) xline(-24, lcolor(red)) xsize(20) ysize(10)
	restore 
	
*Indicator for timing: 

	gen MEM_FIRST = 1 if MEM_DTT < LABOR_DTT & MEM_DTT != . & LABOR_DTT != .
	replace MEM_FIRST = 0 if MEM_DTT > LABOR_DTT & MEM_DTT != . & LABOR_DTT != . 
	replace MEM_FIRST = 0 if MEM_DTT == LABOR_DTT & MEM_DTT != . & LABOR_DTT != . 
	label var MEM_FIRST "ROM occurred prior to labor onset"
	tab MEM_FIRST, m 
	
	gen LABOR_FIRST = 1 if MEM_DTT > LABOR_DTT & MEM_DTT != . & LABOR_DTT != .
	replace LABOR_FIRST = 0 if MEM_DTT < LABOR_DTT & MEM_DTT != . & LABOR_DTT != . 
	replace LABOR_FIRST = 1 if MEM_DTT == LABOR_DTT & MEM_DTT != . & LABOR_DTT != .
	label var LABOR_FIRST "Labor onset occurred prior/at the same time as ROM"
	tab LABOR_FIRST, m 
	
	tab MEM_FIRST LABOR_FIRST, m 
	
	*Review Missing Data: 
	tab LABOR_ANY MEM_CES if MEM_FIRST ==. | LABOR_FIRST ==., m 
	
	
/*Outcome definitions confirmed with Dr. Wiley:

	Spontaneous preterm:
		1. Non-induced labor <37 weeks GA, except in cases where there was artificial 
		rupture of membranes prior to labor onset OR
		2. Spontaneous rupture of membranes < 37 weeks GA, except in cases where 
		labor was induced/augmented before rupture of membranes  
		
	Provider-initiated preterm: 
		1. Induced labor <37 weeks GA, except when there is spontaneous rupture 
		of membranes prior to labor induction/AUGMENTATION OR 
		2. Artificial rupture of membranes <37 weeks GA, except when there is 
		spontaneous labor prior to artificial rupture of membranes OR 
		3. Any cesarean delivery (emergent or planned) < 37 weeks GA not 
		preceded by spontaneous labor or spontaneous rupture of membranes
	
	PPROM: 
		1. Spontaneous rupture of membranes prior to 37 weeks GA that occurs 
		BEFORE onset of labor.
		ADDED ON 4-1-2024: PPROM can also occur among pregnancies at <20 weeks 
		GA; with this in mind, we will not exclude PREG_LOSS == 1
		ADDED ON 5-13: We will exclude induced abortions from the PPROM outcome
		
*/

///////////////////////////////////////////////////////
*Preterm classification typologies variable:
	
	*preterm deliveries: 
	gen PRETERM_CLASS = 0 if PRETERM_ANY == 1 
	label var PRETERM_CLASS "Preterm classification typologies"
	
	*all deliveries: 
	gen DELIVERY_CLASS = 0 if PRETERM_ANY == 1 | PRETERM_ANY == 0 
	label var DELIVERY_CLASS "Delivery typologies (spontaneous/initiated)"
	
	gen DELIVERY_ANY = 1 if PRETERM_ANY == 1 | PRETERM_ANY == 0 


//////////////////////////////////////////////////////
*Updated 9-17-2024:
*Birth classification - spontaneous (DELIVERY_SPON)
	*Will be subset to PRETERM_SPON after assigning all observations: 

	*preterm deliveries: 
	gen PRETERM_SPON = 0 if PRETERM_ANY == 0 
	replace PRETERM_SPON = 55 if PRETERM_ANY == 55 
	replace PRETERM_SPON = 77 if PRETERM_ANY == 77 
	
	*all deliveries: 
	gen DELIVERY_SPON = 0 if PRETERM_ANY == 1 | PRETERM_ANY == 0 
	replace DELIVERY_SPON = 55 if PRETERM_ANY == 55
	replace DELIVERY_SPON = 77 if PRETERM_ANY == 77 
	
	////////////////
	// SPONTANEOUS: 
	// spontaneous ROM prior to onset of labor: 
	replace DELIVERY_SPON = 1 if  ///
		MEM_SPON == 1 & MEM_FIRST == 1
	
	replace DELIVERY_CLASS = 1 if ///
		MEM_SPON == 1 & MEM_FIRST == 1 
	
	// spontaneous labor prior to ROM
	replace DELIVERY_SPON = 1 if ///
		LABOR_SPON == 1 & LABOR_FIRST == 1
		
	replace DELIVERY_CLASS = 2 if ///
		LABOR_SPON == 1 & LABOR_FIRST == 1
		
	// spontaneous labor with ROM at C-section & Cesarean delivery
	replace DELIVERY_SPON = 1 if  ///
		LABOR_SPON == 1 & MEM_CES == 1 & MEM_FIRST == . & CES_ANY == 1
		
	replace DELIVERY_CLASS = 3 if  ///
		LABOR_SPON == 1 & MEM_CES == 1 & MEM_FIRST == . & CES_ANY == 1	
		
	// spontaneous ROM & no labor (c-section)
	replace DELIVERY_SPON = 1 if  ///
		MEM_SPON == 1 & LABOR_ANY == 0 & CES_ANY == 1 & MEM_FIRST == . 
		
	replace DELIVERY_CLASS = 4 if  ///
		MEM_SPON == 1 & LABOR_ANY == 0 & CES_ANY == 1 & MEM_FIRST == . 
		
	// spontaneous ROM & Spontaneous Labor (timing is missing)
	replace DELIVERY_SPON = 1 if  ///
		MEM_SPON == 1 & LABOR_ANY == 1 & LABOR_SPON == 1 & MEM_FIRST == . 
		
	replace DELIVERY_CLASS = 5 if  ///
		MEM_SPON == 1 & LABOR_ANY == 1 & LABOR_SPON == 1 & MEM_FIRST == . 
		
	tab DELIVERY_CLASS DELIVERY_SPON, m 
	
	//Similar to DELIVERY_CLASS=23 below: No ROM - Spontaneous labor - vaginal delivery 
	 // (we will consider provider-initiated based on this info)
	replace DELIVERY_SPON = 1 if  DELIVERY_CLASS == 0 & ///
		LABOR_ANY == 1 & LABOR_SPON==1 & CES_ANY == 0 & MEM_CES==1 & LABOR_FIRST == . 
		
	replace DELIVERY_CLASS = 6 if  DELIVERY_CLASS == 0 & ///
		LABOR_ANY == 1 & LABOR_SPON==1 & CES_ANY == 0 & MEM_CES==1 & LABOR_FIRST == . 
	
	///////////////////////
	// PROVIDER-INITIATED: 
	//  induced labor prior to ROM		
	replace DELIVERY_SPON = 0 if ///
		LABOR_INDUCED == 1 & LABOR_FIRST == 1 
		
	replace DELIVERY_CLASS = 11 if ///
		LABOR_INDUCED == 1 & LABOR_FIRST == 1 
		
	// 	artificial ROM prior to labor		
	replace DELIVERY_SPON = 0 if  ///
		MEM_ART == 1 & MEM_FIRST == 1 
		
	replace DELIVERY_CLASS = 12 if  ///
		MEM_ART == 1 & MEM_FIRST == 1 
		
	//  cesarean with no labor or spontaneous ROM 
	replace DELIVERY_SPON = 0 if  ///
		CES_ANY == 1 & LABOR_ANY== 0 & (MEM_CES == 1 | MEM_ART == 1)
		
	replace DELIVERY_CLASS = 13 if  ///
		CES_ANY == 1 & LABOR_ANY== 0 & (MEM_CES == 1 | MEM_ART == 1)
		
	// cesarean with induced labor - cesarean-related ROM 
	replace DELIVERY_SPON = 0 if  ///
		CES_ANY == 1 & LABOR_INDUCED == 1 & LABOR_ANY == 1 & ///
		MEM_CES == 1 & LABOR_FIRST == . 
		
	replace DELIVERY_CLASS = 14 if  ///
		CES_ANY == 1 & LABOR_INDUCED == 1 & LABOR_ANY == 1 & ///
		MEM_CES == 1 & LABOR_FIRST == . 
		
	// cesarean with induced labor + artificial ROM 
	replace DELIVERY_SPON = 0 if  ///
		LABOR_INDUCED == 1 & MEM_ART == 1 & LABOR_ANY == 0 & CES_ANY == 0
		
	replace DELIVERY_CLASS = 15 if  ///
		LABOR_INDUCED == 1 & MEM_ART == 1 & LABOR_ANY == 0 & CES_ANY == 0
		
	tab DELIVERY_CLASS DELIVERY_SPON, m
	
	// induced labor + artificial ROM - missing timing
	replace DELIVERY_SPON = 0 if  ///
		LABOR_INDUCED == 1 & (MEM_ART == 1 | MEM_CES == 1) & ///
		(LABOR_MEM_HOURS == . ) & ///
		LABOR_ANY==1
		
	replace DELIVERY_CLASS = 16 if  ///
		LABOR_INDUCED == 1 & (MEM_ART == 1 | MEM_CES == 1) & ///
		(LABOR_MEM_HOURS == . ) & ///
		LABOR_ANY==1 & DELIVERY_CLASS == 0
		
	//Similar to the one immediately below (23): Artificial ROM - No labor - vaginal delivery 
	 // (we will consider provider-initiated based on this info)
	replace DELIVERY_SPON = 0 if  DELIVERY_CLASS == 0 & ///
		LABOR_ANY == 0 & CES_ANY == 0 & MEM_ART == 1 & MEM_FIRST == . 
		
	replace DELIVERY_CLASS = 17 if  DELIVERY_CLASS == 0 & ///
		LABOR_ANY == 0 & CES_ANY == 0 & MEM_ART == 1 & MEM_FIRST == . 
		
	//Similar to the one immediately below (23): No ROM - Induced labor - vaginal delivery 
	 // (we will consider provider-initiated based on this info)
	replace DELIVERY_SPON = 0 if  DELIVERY_CLASS == 0 & ///
		LABOR_ANY == 0 & LABOR_INDUCED == 1 & MEM_CES == 1 & CES_ANY == 0 
		
	replace DELIVERY_CLASS = 18 if  DELIVERY_CLASS == 0 & ///
		LABOR_ANY == 0 & LABOR_INDUCED == 1 & MEM_CES == 1 & CES_ANY == 0 
		
	//Spontaneous ROM - No labor - vaginal delivery 
	**** NOTE: Dr. Wylie confirmed that we will consider these cases to be 
	**** spontaneous preterm, allowing vaginal delivery to override the 
	**** "no labor" detail. However, we will still submit cases >= 24 weeks 
	**** to be reviewed again by sites 
	replace DELIVERY_SPON = 1 if  ///
		LABOR_ANY == 0 & CES_ANY == 0 & MEM_SPON == 1 & MEM_FIRST == . & ///
		(LABOR_INDUCED == 0 | LABOR_INDUCED==55)
		
	replace DELIVERY_CLASS = 23 if  ///
		LABOR_ANY == 0 & CES_ANY == 0 & MEM_SPON == 1 & MEM_FIRST == . & ///
		(LABOR_INDUCED == 0 | LABOR_INDUCED==55)
		
	gen DELIVERY_SPON_REVIEW = 1 if  ///
		LABOR_ANY == 0 & CES_ANY == 0 & MEM_SPON == 1 & MEM_FIRST == . & ///
		PREG_END_GA >= (24*7) & PREG_END_GA != . 
		
	list site LABOR_ANY CES_ANY MEM_SPON PREG_END_GA ///
		DELIVERY_SPON_REVIEW if DELIVERY_CLASS == 23 
		
	tab DELIVERY_CLASS DELIVERY_SPON, m
	 
	///////////////////////
	// UNKNOWN: 
	// missing information - Labor 1st and Labor Type Missing: 
	replace DELIVERY_SPON = 55 if DELIVERY_ANY == 1 & ///
		(LABOR_SPON == 55 & LABOR_FIRST==1 & CES_PLAN == 0)
		
	replace DELIVERY_CLASS = 21 if DELIVERY_ANY == 1 & ///
		(LABOR_SPON == 55 & LABOR_FIRST==1 & CES_PLAN == 0)
		
		// Also: missing information - Labor 1st, type of labor missing, no ROM 
	replace DELIVERY_SPON = 55 if DELIVERY_ANY == 1 & ///
		(LABOR_ANY==1 & LABOR_SPON == 55 & MEM_CES == 1 & DELIVERY_CLASS==0)
		
	replace DELIVERY_CLASS = 21 if DELIVERY_ANY == 1 & ///
		(LABOR_ANY==1 & LABOR_SPON == 55 & MEM_CES == 1 & DELIVERY_CLASS==0)

		// Also: missing information - Labor 1st and Labor Type Missing, c-section 
	replace DELIVERY_SPON = 55 if DELIVERY_ANY == 1 & DELIVERY_CLASS == 0 ///
		& (LABOR_SPON == 55 & LABOR_ANY==1 & MEM_CES == 1 & CES_ANY == 1)
		
	replace DELIVERY_CLASS = 21 if DELIVERY_ANY == 1 & DELIVERY_CLASS == 0 ///
		& (LABOR_SPON == 55 & LABOR_ANY==1 & MEM_CES == 1 & CES_ANY == 1)
		
		tab DELIVERY_CLASS DELIVERY_SPON, m
		 
	// missing information - MEM_CES=1 but not a cesarean delivery & no labor  
	replace DELIVERY_SPON = 55 if DELIVERY_ANY == 1 & DELIVERY_CLASS == 0 & ///
		MEM_CES == 1 & CES_ANY == 0 & LABOR_ANY==0 & ///
		(LABOR_FIRST==. | LABOR_FIRST >0)
		
	replace DELIVERY_CLASS = 22 if DELIVERY_ANY == 1 & DELIVERY_CLASS == 0 & ///
		MEM_CES == 1 & CES_ANY == 0 & LABOR_ANY==0 & ///
		(LABOR_FIRST==. | LABOR_FIRST >0)
		
		tab DELIVERY_CLASS DELIVERY_SPON, m
		
		
	// missing information - Artificial ROM - No labor / no induction - vaginal delivery 
	replace DELIVERY_SPON = 55 if DELIVERY_ANY == 1 & ///
		LABOR_ANY == 0 & LABOR_INDUCED == 0 & CES_ANY == 0 & ///
		MEM_ART == 1 & MEM_FIRST == . 
		
	replace DELIVERY_CLASS = 24 if DELIVERY_ANY == 1 & ///
		LABOR_ANY == 0 & LABOR_INDUCED == 0 & CES_ANY == 0 & ///
		MEM_ART == 1 & MEM_FIRST == . 
		
		tab DELIVERY_CLASS DELIVERY_SPON, m
		
	// missing information - Unknown if the woman labored
	replace DELIVERY_SPON = 55 if DELIVERY_ANY == 1 & ///
		LABOR_ANY == 55 & DELIVERY_CLASS==0
		
	replace DELIVERY_CLASS = 25 if DELIVERY_ANY == 1 & ///
		LABOR_ANY == 55 & DELIVERY_CLASS==0
		
		tab DELIVERY_CLASS DELIVERY_SPON, m
		
	// missing information - Unknown rupture of membranes
	replace DELIVERY_SPON = 55 if DELIVERY_ANY == 1 & ///
		MEM_SPON == 55  & LABOR_ANY == 1 & DELIVERY_CLASS == 0  
		
	replace DELIVERY_CLASS = 26 if DELIVERY_ANY == 1 & ///
		MEM_SPON == 55  & LABOR_ANY == 1 & DELIVERY_CLASS == 0 
		
		tab DELIVERY_CLASS DELIVERY_SPON, m
		
	// missing information - Unknown timing
	replace DELIVERY_SPON = 55 if DELIVERY_ANY == 1 & ///
		((LABOR_INDUCED==1 & MEM_SPON==1 & LABOR_MEM_HOURS==.) | ///
		(LABOR_INDUCED==0 & MEM_SPON==0 & LABOR_MEM_HOURS==.)) & ///
		DELIVERY_CLASS==0
		
	replace DELIVERY_CLASS = 27 if DELIVERY_ANY == 1 & ///
		((LABOR_INDUCED==1 & MEM_SPON==1 & LABOR_MEM_HOURS==.) | ///
		(LABOR_INDUCED==0 & MEM_SPON==0 & LABOR_MEM_HOURS==.)) & ///
		DELIVERY_CLASS==0
		
	// missing information - Unknown timing AND Unknown labor type: 
	replace DELIVERY_SPON = 55 if DELIVERY_ANY == 1 & ///
		LABOR_ANY==1 & LABOR_INDUCED==55 & LABOR_MEM_HOURS==. & DELIVERY_CLASS==0
		
	replace DELIVERY_CLASS = 27 if DELIVERY_ANY == 1 & ///
		LABOR_ANY==1 & LABOR_INDUCED==55 & LABOR_MEM_HOURS==. & DELIVERY_CLASS==0
		
		tab DELIVERY_CLASS DELIVERY_SPON, m	
		
		// additional timing problems - ROM first, but marked as N/A 
	replace DELIVERY_SPON = 55 if DELIVERY_ANY == 1 & DELIVERY_CLASS == 0 & ///
		LABOR_ANY == 1 & MEM_CES == 1 & ///
		MEM_FIRST == 1 	
		
	replace DELIVERY_CLASS = 27 if DELIVERY_ANY == 1 & DELIVERY_CLASS == 0 & ///
		LABOR_ANY == 1 & MEM_CES == 1 & ///
		MEM_FIRST == 1 
		
		// =28 if stillbirth reported in MNH04/19 but no MNH09 yet: 
	replace DELIVERY_SPON = 55 if DELIVERY_CLASS == 0 & ///
		m09_birth_dsterm_inf1 == . & m09_deliv_dsstdat_inf1 == "" & ///
		(PREG_END_SOURCE == 2 | PREG_END_SOURCE == 3 | PREG_END_SOURCE == 4) & ///
		PREG_END == 1 & PREG_END_GA >=140 & PREG_END_GA !=. 
		
	replace DELIVERY_CLASS = 28 if DELIVERY_CLASS == 0 & ///
		m09_birth_dsterm_inf1 == . & m09_deliv_dsstdat_inf1 == "" & ///
		(PREG_END_SOURCE == 2 | PREG_END_SOURCE == 3 | PREG_END_SOURCE == 4) & ///
		PREG_END == 1 & PREG_END_GA >=140 & PREG_END_GA !=.
		
		// =29 if maternal death >=20 weeks with no delivery info reported yet 
	replace DELIVERY_SPON = 55 if PREG_LOSS_DEATH == 1 & ///
		PREG_END_GA_WK >= 20 & PREG_END_GA_WK != . & PREG_END_SOURCE == 4 
		
	replace DELIVERY_CLASS = 29 if PREG_LOSS_DEATH == 1 & ///
		PREG_END_GA_WK >= 20 & PREG_END_GA_WK != . & PREG_END_SOURCE == 4  
	
	tab DELIVERY_CLASS DELIVERY_SPON, m
	tab DELIVERY_CLASS DELIVERY_SPON if DELIVERY_ANY==1, m
	
	list site PREG_END_GA_WK MEM_SPON MEM_ART MEM_CES LABOR_ANY LABOR_INDUCED ///
		LABOR_SPON LABOR_MEM_HOURS CES_ANY MEM_FIRST m09_birth_dsterm_inf1 ///
		MEM_FIRST LABOR_FIRST ///
		if DELIVERY_CLASS == 0 & DELIVERY_ANY == 1 

		
	label define ptbtypes 1 "Spontaneous ROM prior to labor" ///
		2 "Spontaneous labor prior to ROM" ///
		3 "Spontaneous labor - ROM at cesarean delivery" ///
		4 "Spontaneous ROM - no labor - cesarean delivery" ///
		5 "Spontaneous ROM & labor - missing timing" ///
		6 "Spontaneous labor & no ROM - vaginal delivery" ///
		11 "Induced labor prior to ROM" ///
		12 "Artificial ROM prior to labor" ///
		13 "Cesarean with no labor or spontaneous ROM" ///
		14 "Induced with no labor - cesarean delivery" ///
		15 "Induced with no labor - artifical ROM - cesarean delivery" ///
		16 "Artificial ROM & induced labor - missing timing" ///
		17 "Artificial ROM-no labor-vaginal delivery" ///
		18 "Induced labor-no ROM-vaginal delivery" ///
		21 "Missing info: Labor occurred first - missing labor type" ///
		22 "Missing info: N/A ROM-No labor-vaginal delivery" ///
		23 "Missing info: No labor - vaginal delivery" ///
		24 "Missing info: Artificial ROM - no labor - vaginal delivery" ///
		25 "Missing info: Unknown if labor" ///
		26 "Missing info: Unknown if spontaneous ROM" ///
		27 "Missing info: Missing/problem timing info" /// 
		28 "Missing info: Stillbirth - missing MNH09" ///
		29 "Missing info: Maternal death in late pregnancy with no delivery info"
		
	label values DELIVERY_CLASS ptbtypes
	
	label var DELIVERY_SPON "Delivery classification - spontaneous"
	
	tab DELIVERY_CLASS DELIVERY_SPON, m 

	
	// make preterm spontaneous observations: 
	replace PRETERM_SPON = DELIVERY_SPON if PRETERM_ANY == 1 
	replace PRETERM_SPON = 0 if PRETERM_ANY == 0 
	
	replace PRETERM_CLASS = DELIVERY_CLASS if PRETERM_ANY == 1 
	label var PRETERM_SPON "Preterm birth classification - spontaneous"
	
	label values PRETERM_CLASS ptbtypes
	
	*check variables: 
	list PREG_LOSS PREG_END_GA PREG_END_GA_WK PREG_LOSS_DEATH ///
		if PRETERM_CLASS == . & PRETERM_SPON == 77
		
	list PRETERM_CLASS PRETERM_SPON PREG_END_GA PREG_END_GA_WK if ///
		PREG_LOSS_INDUCED == 1 
		
	*Check on missing data:
	list MEM_SPON MEM_ART MEM_CES LABOR_SPON LABOR_INDUCED LABOR_ANY ///
		CES_ANY CES_EMERGENT CES_PLAN MEM_FIRST LABOR_FIRST PREG_LOSS ///
		PREG_END_GA_WK if PRETERM_SPON == . 
		
	*Check on missing data:
	list MEM_SPON MEM_ART MEM_CES LABOR_SPON LABOR_INDUCED LABOR_ANY ///
		CES_ANY CES_EMERGENT CES_PLAN MEM_FIRST LABOR_FIRST PREG_LOSS ///
		PREG_END_GA_WK if PRETERM_CLASS == 0


		
*Delivery classification - provider-initiated (DELIVERY_PROV)

	gen DELIVERY_PROV = 0 if DELIVERY_ANY == 1  
	replace DELIVERY_PROV = 55 if DELIVERY_ANY == 55 | DELIVERY_SPON == 55
	replace DELIVERY_PROV = 77 if DELIVERY_SPON == 77 
	
	replace DELIVERY_PROV = 0 if DELIVERY_SPON == 1 
	
	replace DELIVERY_PROV = 1 if DELIVERY_SPON == 0 & ///
		(DELIVERY_CLASS == 11 | DELIVERY_CLASS == 12 | DELIVERY_CLASS == 13 | ///
		 DELIVERY_CLASS == 14 | DELIVERY_CLASS == 15 | DELIVERY_CLASS == 16 | ///
		 DELIVERY_CLASS == 17 | DELIVERY_CLASS == 18 | DELIVERY_CLASS == 23)
		 
		
	label var DELIVERY_PROV "Delivery classification - provider-initiated"
	
		
*Preterm birth classification - provider-initiated (PRETERM_PROV)

	gen PRETERM_PROV = 0 if PRETERM_ANY == 0 
	replace PRETERM_PROV = 55 if PRETERM_ANY == 55 | PRETERM_SPON == 55
	replace PRETERM_PROV = 77 if PRETERM_SPON == 77 
	
	replace PRETERM_PROV = 0 if PRETERM_SPON == 1 
	
	replace PRETERM_PROV = 1 if PRETERM_SPON == 0 & ///
		(PRETERM_CLASS == 11 | PRETERM_CLASS == 12 | PRETERM_CLASS == 13 | ///
		 PRETERM_CLASS == 14 | PRETERM_CLASS == 15 | PRETERM_CLASS == 16 | ///
		 PRETERM_CLASS == 17 | PRETERM_CLASS == 18 | PRETERM_CLASS == 23)
		 
		
	label var PRETERM_PROV "Preterm birth classification - provider-initiated"
	

	*CHECKS:
	tab PRETERM_ANY if PREG_LOSS == 0, m 
	tab PRETERM_PROV PRETERM_SPON, m 
	tab PRETERM_CLASS PRETERM_PROV, m 
	tab PRETERM_CLASS PRETERM_SPON, m 
	
	tab PRETERM_CLASS if PRETERM_ANY ==1, m 
	tab PRETERM_SPON if PRETERM_ANY == 1, m 
	tab PRETERM_PROV if PRETERM_ANY == 1, m 
	
	*Create a missing indicator - Overall deliveries 
	gen DELIVERY_PROV_MISS = 0 if DELIVERY_PROV != 55 & DELIVERY_PROV !=77
	*same as PRETERM_ANY_MISS: 
	replace DELIVERY_PROV_MISS = 1 if DELIVERY_PROV == 55 & PRETERM_ANY_MISS == 1 
	replace DELIVERY_PROV_MISS = 2 if DELIVERY_PROV == 55 & PRETERM_ANY_MISS == 2
	replace DELIVERY_PROV_MISS = 3 if DELIVERY_PROV == 55 & PRETERM_ANY_MISS == 3
	replace DELIVERY_PROV_MISS = 4 if DELIVERY_PROV == 55 & PRETERM_ANY_MISS == 4
	*other missing beyond DELIVERY_ANY_MISS:
	replace DELIVERY_PROV_MISS = 5 if DELIVERY_PROV == 55 & ((DELIVERY_CLASS >= 20 & ///
		DELIVERY_CLASS <23) | (DELIVERY_CLASS >23 & DELIVERY_CLASS <30))
	
	label define pt_prov_miss 0 "0-Non-missing" 1 "1=Missing BOE" ///
		2 "2-Missing end date" 3 "3-Missing BOE and end date" ///
		4 "4-Missing GA for another reason (INVESTIGATE)" ///
		5 "4-Missing labor/ROM info-MNH09"
	label values DELIVERY_PROV_MISS pt_prov_miss 
	label var DELIVERY_PROV_MISS "Reason missing - DELIVERY PROV"

	tab DELIVERY_PROV_MISS DELIVERY_PROV, m 
	
	*Create a missing indicator - Preterm
	gen PRETERM_PROV_MISS = 0 if PRETERM_PROV != 55 & PRETERM_PROV !=77
	*same as PRETERM_ANY_MISS: 
	replace PRETERM_PROV_MISS = 1 if PRETERM_PROV == 55 & PRETERM_ANY_MISS == 1 
	replace PRETERM_PROV_MISS = 2 if PRETERM_PROV == 55 & PRETERM_ANY_MISS == 2
	replace PRETERM_PROV_MISS = 3 if PRETERM_PROV == 55 & PRETERM_ANY_MISS == 3
	replace PRETERM_PROV_MISS = 4 if PRETERM_PROV == 55 & PRETERM_ANY_MISS == 4
	*other missing beyond PRETERM_ANY_MISS:
	replace PRETERM_PROV_MISS = 5 if PRETERM_PROV == 55 & ((PRETERM_CLASS >= 20 & ///
		PRETERM_CLASS <23) | (PRETERM_CLASS >23 & PRETERM_CLASS <30))
	
	label values PRETERM_PROV_MISS pt_prov_miss 
	label var PRETERM_PROV_MISS "Reason missing - PRETERM PROV"

	tab PRETERM_PROV_MISS PRETERM_PROV, m 
	
		tab PRETERM_PROV_MISS if PREG_LOSS_INDUCED == 1, m  
	
	
	*Construct: Post-term delivery indicator: 
	gen POST_TERM_41_DEL = 0 if PREG_END_GA >=  259 & PREG_END_GA < 287 & ///
		DELIVERY_ANY == 1 
	replace POST_TERM_41_DEL = 1 if PREG_END_GA >= 287 & PREG_END_GA != .  & ///
		DELIVERY_ANY == 1  
		
	replace POST_TERM_41_DEL = 77 if POST_TERM_41_DEL == . & ///
		(PRETERM_ANY == 1 | PREG_LOSS == 1 | ///
		PREG_LOSS_DEATH == 1)
	
	gen POST_TERM_42_DEL = 0 if PREG_END_GA >= 259 & PREG_END_GA < 294  & ///
		DELIVERY_ANY == 1 
	replace POST_TERM_42_DEL = 1 if PREG_END_GA >= 294 & PREG_END_GA != . & ///
		DELIVERY_ANY == 1 
		
	replace POST_TERM_42_DEL = 77 if POST_TERM_42_DEL == . & ///
		(PRETERM_ANY == 1 | PREG_LOSS == 1 | ///
		PREG_LOSS_DEATH == 1)
	
	label var POST_TERM_41_DEL "Post-term vs. term deliveries (>=41 weeks)"
	label var POST_TERM_42_DEL "Post-term vs. term deliveries (>=42 weeks)"
	
	
	///////////////////////////////////////////////////////////////////////////
	** MAKE A BAR GRAPH **
	///////////////////////////////////////////////////////////////////////////
	
	*create numeric variable for site:
	gen site_num = 1 if site == "Ghana"
	replace site_num = 2 if site == "India-CMC"
	replace site_num = 3 if site == "Kenya"
	replace site_num = 4 if site == "Pakistan"
	replace site_num = 5 if site == "Zambia"
	replace site_num = 6 if site == "India-SAS"
	
	tab site_num, m 
	
	sort site_num 
	by site_num: tab PRETERM_ANY 
	by site_num: tab PRETERM_PROV
	
	by site_num: tab PRETERM_PROV if PRETERM_PROV != 55 & PRETERM_ANY == 1
	
	label define sites 1 "Ghana" 2 "India-CMC" 3 "Kenya" 4 "Pakistan" 5 "Zambia" ///
		6 "India-SAS"
	label values site_num sites
	
	label var PRETERM_ANY "Preterm pregnancy endpoint (<37 weeks)"
	
	global pct `" 0 "0%" .05 "5%" .1 "10%" .15 "15%" .2 "20%" .25 "25%" .3 "30%" .35 "35%" .4  "40%" "'	
  
	betterbar ///
	PRETERM_ANY if PRETERM_ANY !=55 & PREG_LOSS == 0 & PREG_LOSS_INDUCED == 0 ///
	, over(site_num) bar format(%9.2f) ci n xlab($pct) ///
	legend(c(4)) ysize(17) xsize(43)
	
	graph export "$output/PRETERM_ANY_$date.jpg", replace 

		
	label var PRETERM_PROV "Provider-initiated Preterm (among all preterm)"
	global pct2 `" 0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%"  "'	
	
	betterbar ///
	PRETERM_PROV if PRETERM_ANY == 1 & PRETERM_PROV != 55 & PREG_LOSS_INDUCED == 0 ///
	, over(site_num) bar format(%9.2f) n xlab($pct2) ///
	legend(c(4)) ysize(17) xsize(45)
	
	graph export "$output/PRETERM_PROV_$date.jpg", replace 
	
	
	///////////////////////////////////////////////////////////////////////////
	** END MAKE A BAR GRAPH **
	///////////////////////////////////////////////////////////////////////////
	
*Addition of variables on Provider-Initiated and Spontaneous Indication (i.e., 
	*underlying reason for preterm delivery classification): 
	
	*Provider initiated indication for induced labor: 
		*review input variables:
			// 1=post-term
		tab PRETERM_PROV m09_induced_prindc_1, m 
			// 2=non-reassuring fetal heart rate
		tab PRETERM_PROV m09_induced_prindc_2, m 		
			// 3=prior stillbirth
		tab PRETERM_PROV m09_induced_prindc_3, m 
			// 4=macrosomia
		tab PRETERM_PROV m09_induced_prindc_4, m 
			// 5=Oligohydramnios
		tab PRETERM_PROV m09_induced_prindc_5, m 
			// 6=IUGR
		tab PRETERM_PROV m09_induced_prindc_6, m 
			// 7=Hypertension
		tab PRETERM_PROV m09_induced_prindc_7, m 
			// 8=Diabetes
		tab PRETERM_PROV m09_induced_prindc_8, m 
			// 9=Cardiac disease
		tab PRETERM_PROV m09_induced_prindc_9, m 
			// 10=elective
		tab PRETERM_PROV m09_induced_prindc_10, m 
			// 11=ROM
		tab PRETERM_PROV m09_induced_prindc_11, m 
			// 88=Other
		tab PRETERM_PROV m09_induced_prindc_88, m 
			// 99=DK
		tab PRETERM_PROV m09_induced_prindc_99, m 
		
		gen INDICATION_NUM = 0 if PRETERM_PROV == 1 & LABOR_INDUCED == 1 
		
		foreach num of numlist 1/11 88 {
		
		replace INDICATION_NUM = INDICATION_NUM +1 if ///
			m09_induced_prindc_`num' == 1 & PRETERM_PROV == 1 & ///
			LABOR_INDUCED == 1 
		
		}
		
		label var INDICATION_NUM "Number of indications given for induced labor (provider-initiated pt)"
		tab INDICATION_NUM, m 
		
	gen PRETERM_PROV_IND = 55 if PRETERM_PROV == 1 
	label var PRETERM_PROV_IND "Indication for provider-initiated preterm"
	
	foreach num of numlist 1/11 88 {
	replace PRETERM_PROV_IND = `num' if m09_induced_prindc_`num' == 1 ///
		& PRETERM_PROV == 1 & LABOR_INDUCED == 1 & INDICATION_NUM == 1
	}
	
	replace PRETERM_PROV_IND = 89 if INDICATION_NUM >= 2 & INDICATION_NUM != . & ///
		PRETERM_PROV == 1 & LABOR_INDUCED == 1 
		
	tab PRETERM_PROV_IND, m 
		
		
	*Provider initiated indication for cesarean: 		
		tab PRETERM_PROV m09_ces_prindc_inf1_1, m 
		
		gen INDCES_NUM = 0 if PRETERM_PROV == 1 & LABOR_ANY ==0 & ///
			LABOR_INDUCED == 0 & CES_ANY == 1 
		label var INDCES_NUM "Number of indications given for cesarean (provider-initiated pt)"
		
		foreach num of numlist 1/16 88 {
		
		replace INDCES_NUM = INDCES_NUM +1 if ///
			m09_ces_prindc_inf1_`num' == 1 & PRETERM_PROV == 1 & ///
			LABOR_INDUCED == 0 & LABOR_ANY == 0 & CES_ANY == 1  
		
		}
		
		*incorporate c-section indication into combined variable for 
		*provider-initiated preterm indication: 
		
	foreach num of numlist 1/16 {
	replace PRETERM_PROV_IND = (`num'+ 20) if m09_ces_prindc_inf1_`num' == 1 ///
		& PRETERM_PROV == 1 & LABOR_INDUCED == 0 & LABOR_ANY == 0 & ///
		CES_ANY == 1 & INDCES_NUM == 1 
	}
	
	foreach num of numlist 88 {
	replace PRETERM_PROV_IND = (`num') if m09_ces_prindc_inf1_`num' == 1 ///
		& PRETERM_PROV == 1 & LABOR_INDUCED == 0 & LABOR_ANY == 0 & ///
		CES_ANY == 1 & INDCES_NUM == 1 
	}
	
	replace PRETERM_PROV_IND = 89 if INDCES_NUM >= 2 & INDCES_NUM != . & ///
		PRETERM_PROV == 1 & LABOR_INDUCED == 0 & LABOR_ANY == 0 & ///
		CES_ANY == 1  
		
	tab PRETERM_PROV_IND, m 
	
		*combine "Postterm"
	replace PRETERM_PROV_IND = 1 if PRETERM_PROV_IND == 21
		*combine non-reassuring fetal heart rate 
	replace PRETERM_PROV_IND = 2 if PRETERM_PROV_IND == 27 
		*combine Macrosomia
	replace PRETERM_PROV_IND = 4 if PRETERM_PROV_IND == 31
	
		*set missing to 555 so that it will show up last:
	replace PRETERM_PROV_IND = 555 if PRETERM_PROV_IND == 55 

	label define indications 1 "Post-term" 2 "Non-reassuring fetal heartrate" ///
		3 "Prior stillbirth" 4 "Macrosomia" 5 "Oligohydramnios" ///
		6 "IUGR" 7 "Hypertension" 8 "Diabetes" 9 "Cardiac disease" ///
		10 "Elective induction" 11 "Rupture of membranes" ///
		22 "Previous cesarean" 23 "Failure to progress" 24 "Failed induction" ///
		25 "Failed vacuum/forceps" 26 "Abruption/bleeding" ///
		28 "Cord prolapse" 29 "Breech presentation" 30 "Shoulder Dystocia" ///
		32 "Preeclampsia/eclampsia" 33 "Fetal anomaly" 34 "Herpes" ///
		35 "PMTCT" 36 "Elective cesarean" 88 "Other indication" ///
		89 "Multiple indications" 555 "No indication provided"
	label values PRETERM_PROV_IND indications 
	
	tab PRETERM_PROV_IND if PRETERM_PROV == 1 
	tab PRETERM_PROV_IND site if PRETERM_PROV == 1 
	
	tab PRETERM_PROV_IND PRETERM_CLASS 
	
	////////////////////////////////////////////////////////////////
	*review data for observations with inconsistent indications:
	
	rename MOMID momid 
	rename PREGID pregid 
	
	*11= Rupture of membranes 
	list momid pregid site PRETERM_PROV ///
		MEM_ART MEM_CES MEM_SPON MEM_DTT ///
		LABOR_ANY LABOR_INDUCED LABOR_DTT ///
		if PRETERM_PROV_IND == 11
	*1=Post-term 
	list momid pregid site PRETERM_PROV ///
		MEM_ART MEM_CES MEM_SPON MEM_DTT ///
		LABOR_ANY LABOR_INDUCED LABOR_DTT ///
		if PRETERM_PROV_IND == 1
	
	*Pull suspcious cases: 
	tab PRETERM_CLASS, m 
	
	*Scenarios for PRETERM_SPON_IND 
	gen PRETERM_SPON_IND = 55 if PRETERM_SPON == 1 
	label var PRETERM_SPON_IND "Indication for spontaneous preterm delivery"
	
		// 1=Preterm Labor: 
	replace PRETERM_SPON_IND = 1 if PRETERM_CLASS == 2
	replace PRETERM_SPON_IND = 1 if PRETERM_CLASS == 3 
	
		// 2=ROM: 
	replace PRETERM_SPON_IND = 2 if PRETERM_CLASS == 1 
	replace PRETERM_SPON_IND = 2 if PRETERM_CLASS == 4 
	replace PRETERM_SPON_IND = 2 if PRETERM_CLASS == 23 // added on 7-1-2024
	
		// 55=Unknown
	replace PRETERM_SPON_IND = 55 if PRETERM_CLASS == 5
	
	label define sponind 1 "Preterm labor" 2 "PPROM" 55 "Unknown"
	
	label values PRETERM_SPON_IND sponind 
		 
	tab PRETERM_SPON_IND, m 
	tab PRETERM_SPON_IND PRETERM_SPON, m 
		 
*Preterm premature rupture of membranes - PPROM (PPROM_OCCUR)
	
	*Generate GA at membrane rupture: 
	gen MEM_GA = dofc(MEM_DTT) - PREG_START_DATE 
	tab MEM_GA, m 
	
	gen MEM_GA_WK = MEM_GA / 7 
	tab MEM_GA_WK, m 
	
	tab MEM_GA_WK PREG_LOSS, m 

	*PPROM_PREGEND
	gen PPROM_PREGEND = .
	label var PPROM_PREGEND "PPROM at L&D (MNH09)"
	
	// Unknown PPROM if unknown membrane rupture method or timing: 
	replace PPROM_PREGEND = 55 if (MEM_SPON == 55 | (MEM_SPON == 1 & MEM_DTT == .))  /// 
		&  m09_membrane_rupt_mhterm != . 
	
	// Unknown PPROM if Spontaneous ROM at <37 weeks, but unknown if labored  
	replace PPROM_PREGEND = 55 if MEM_SPON == 1 & LABOR_ANY == 55 & ///
		MEM_GA_WK < 37 & MEM_GA_WK != . 
		
	// Unknown PPROM if Spontaneous ROM at <37 weeks, but labor timing missing
	replace PPROM_PREGEND = 55 if MEM_SPON == 1 & LABOR_ANY == 1 & ///
		LABOR_DTT == . & (MEM_GA_WK < 37 & MEM_GA_WK != .)
		
	// Unknown PPROM if Spontaneous ROM at <37 weeks, no labor but induced, and vaginal delivery: 
	replace PPROM_PREGEND = 55 if MEM_SPON == 1 & LABOR_ANY == 0 & LABOR_INDUCED ==1 & ///
		LABOR_DTT == . & (MEM_GA_WK < 37 & MEM_GA_WK != .) & CES_ANY == 0 
		
	// Unknown PPROM if pregnancy loss only documented in MNH04/19 or death & not MNH09 
	replace PPROM_PREGEND = 55 if ///
		PREG_LOSS_INDUCED == 0 & PREG_END_SOURCE >= 2 & PREG_END_SOURCE <=4 
	
	// NOT PPROM if labor occurs first: 
	replace PPROM_PREGEND = 0 if LABOR_FIRST == 1
	
	// NOT PPROM if artificial rupture of membranes / no rupture 
	replace PPROM_PREGEND = 0 if (MEM_ART == 1 | MEM_CES == 1)
	
	// NOT PPROM if spontaneous ROM after 37 weeks GA:
	replace PPROM_PREGEND = 0 if MEM_SPON == 1 & MEM_GA_WK >= 37 & MEM_GA_WK !=.
	
	// PPROM if Spontaneous ROM prior to labor onset: 
	replace PPROM_PREGEND = 1 if MEM_SPON == 1 & MEM_FIRST == 1 & ///
		MEM_GA_WK < 37 & MEM_GA_WK != . 
		
	// PPROM if Spontaneous ROM prior to a cesarean delivery (no labor): 
	replace PPROM_PREGEND = 1 if MEM_SPON == 1 & MEM_FIRST == . & ///
		LABOR_ANY == 0 & CES_ANY == 1 & MEM_GA_WK < 37 & MEM_GA_WK !=.
		
	// PPROM if Spontaneous ROM & no labor/no induced labor (no cesarean -- most suspicious, but includes some instances of pregnancy loss <20wks): 
	replace PPROM_PREGEND = 1 if MEM_SPON == 1 & MEM_FIRST == . & ///
		LABOR_ANY == 0 & LABOR_INDUCED == 0 & CES_ANY == 0 & MEM_GA_WK < 37 & MEM_GA_WK !=.
		
	// N/A if induced abortion: 
	replace PPROM_PREGEND = 77 if PREG_LOSS_INDUCED == 1 & m09_membrane_rupt_mhterm == . 
		
	tab PPROM_PREGEND, m 

	
	list PPROM_PREGEND m09_membrane_rupt_mhterm MEM_SPON MEM_GA_WK ///
		MEM_FIRST LABOR_ANY ///
		LABOR_INDUCED LABOR_DTT PREG_END_DATE PREG_END_GA CES_ANY ///
		PREG_LOSS PREG_LOSS_INDUCED PREG_LOSS_DEATH if PPROM_PREGEND == . 
		
		
	// 
	
	*review: hours between ROM and labor onset for cases of PPROM with 
	*spontaneous labor: 
	sum LABOR_MEM_HOURS if PPROM == 1
	histogram LABOR_MEM_HOURS if PPROM==1 & LABOR_SPON == 1, w(4) xline(24)
	graph export "$output/PPROM_HOURS_$date.jpg", replace
		
	tab PPROM_PREGEND PRETERM_ANY, m 
	
	// review observations: 
	list PPROM_PREGEND PRETERM_ANY MEM_DTT LABOR_DTT PREG_END_DATE ///
		MEM_GA PREG_END_GA if ///
		PPROM_PREGEND == 1 & PRETERM_ANY == 0 
		
	list PPROM_PREGEND PRETERM_ANY MEM_DTT LABOR_SPON LABOR_DTT PREG_END_DATE ///
		MEM_GA PREG_END_GA if ///
		PPROM_PREGEND == 1 
		
	// create a "timing" indicator to describe PPROM scenarios:	
	gen PPROM_PREGEND_TIMING = 0 if PPROM_PREGEND == 1 & LABOR_MEM_HOURS > 0 ///
		& LABOR_MEM_HOURS < 1 & LABOR_SPON == 1
		
	replace PPROM_PREGEND_TIMING = 1 if PPROM_PREGEND == 1 & LABOR_MEM_HOURS >=1 & ///
		LABOR_MEM_HOURS < 3 & LABOR_SPON == 1
	
	replace PPROM_PREGEND_TIMING = 2 if PPROM_PREGEND == 1 & LABOR_MEM_HOURS >=3 & ///
		LABOR_MEM_HOURS < 6 & LABOR_SPON == 1
		
	replace PPROM_PREGEND_TIMING = 3 if PPROM_PREGEND == 1 & LABOR_MEM_HOURS >=6 & ///
		LABOR_MEM_HOURS < 12 & LABOR_SPON == 1
		
	replace PPROM_PREGEND_TIMING = 4 if PPROM_PREGEND == 1 & LABOR_MEM_HOURS >=12 & ///
		LABOR_MEM_HOURS < 24 & LABOR_SPON == 1
		
	replace PPROM_PREGEND_TIMING = 5 if PPROM_PREGEND == 1 & LABOR_MEM_HOURS >=24 & ///
		LABOR_MEM_HOURS!=. & LABOR_SPON == 1
		
	replace PPROM_PREGEND_TIMING = 77 if PPROM_PREGEND == 1 & ///
		LABOR_INDUCED == 1 
		
	replace PPROM_PREGEND_TIMING = 88 if PPROM_PREGEND == 1 & ///
		LABOR_ANY == 0 & (LABOR_INDUCED==0 | LABOR_INDUCED==55) & CES_ANY == 1 
		
	replace PPROM_PREGEND_TIMING = 99 if PPROM_PREGEND == 1 & ///
		LABOR_ANY == 0 & LABOR_INDUCED==0 & CES_ANY == 0 	
		
	label define pprom 0 "0-Labor onset <1hr" 1 "1-Labor onset 1-<3hrs" ///
		2 "2-Labor onset 3-<6hrs" 3 "3-Labor onset 6-<12hrs" ///
		4 "4-Labor onset 12-<24hrs" 5 "Labor onset >24hrs" ///
		77 "77-Induced labor" ///
		88 "88-Cesarean" ///
		99 "99-No labor"
	label values PPROM_PREGEND_TIMING pprom 
	label var PPROM_PREGEND_TIMING "PPROM - detailed scenarios"
		
	tab PPROM_PREGEND_TIMING PPROM_PREGEND, m 
	tab PPROM_PREGEND_TIMING
	
	*review cases with no labor: 
	tab PREG_END_GA if PPROM_PREGEND_TIMING == 99
	
	tab PRETERM_CLASS PPROM_PREGEND, m 
	tab PRETERM_SPON_IND PPROM_PREGEND, m 

	
	*Create missing reason indicator for maternal outcomes report: 
	gen PPROM_PREGEND_MISS = 0 if PPROM_PREGEND != 55 
	
	// Unknown PPROM if unknown membrane rupture method: 
	replace PPROM_PREGEND_MISS = 1 if MEM_SPON == 55 & PPROM_PREGEND == 55 & ///
		m09_membrane_rupt_mhterm != . 
	
	// Unknown PPROM if spontaenous ROM with missing date/time: 
	replace PPROM_PREGEND_MISS = 2 if MEM_SPON == 1 & MEM_DTT == . 
	
	// Unknown PPROM if Spontaneous ROM <37 weeks, but unknown if labored
	replace PPROM_PREGEND_MISS = 3 if MEM_SPON == 1 & LABOR_ANY == 55 & ///
		(MEM_GA_WK < 37 & MEM_GA_WK != .)
		
	// Unknown PPROM if Spontaneous ROM <37 weeks, but timing of labor is unknown 
	replace PPROM_PREGEND_MISS = 3 if MEM_SPON == 1 & ///
		((LABOR_ANY == 1 & LABOR_DTT == .) | ///
		(LABOR_ANY == 0 & LABOR_INDUCED == 1 & LABOR_DTT == . & CES_ANY == 0)) ///
		& (MEM_GA_WK < 37 & MEM_GA_WK != .)
		
	// Unknown PPROM if pregnancy loss documented in MNH04/19 but not MNH09: 
	replace PPROM_PREGEND_MISS = 4 if m09_membrane_rupt_mhterm == . & ///
		PREG_LOSS_INDUCED == 0 & PPROM_PREGEND == 55
		
	// Not applicable PPROM if induced abortion: 
	replace PPROM_PREGEND_MISS = 5 if m09_membrane_rupt_mhterm == . & ///
		PREG_LOSS_INDUCED == 1 & PPROM_PREGEND == 77
	
	label var PPROM_PREGEND_MISS "Reason missing - PPROM PREGEND"
	
	label define pprom_missed 0 "0-Non-missing" 1 "1-Type of ROM" 2 "2-Timing of ROM" ///
		3 "3-Labor info" 4 "4-No MNH09 (loss)" 5 "5-Excluded: induced abortion"
	label values PPROM_PREGEND_MISS pprom_missed 
	
	tab PPROM_PREGEND_MISS PPROM_PREGEND, m 
	 
	*review for consistency: 
	tab PRETERM_SPON_IND PPROM_PREGEND, m 
	tab PPROM_PREGEND_TIMING PRETERM_SPON_IND,m 
	
	
	*check on PPROM observations NOT marked as spontaneous preterm:
	list site PRETERM_ANY PRETERM_SPON PRETERM_SPON_IND ///
		PRETERM_PROV PRETERM_PROV_IND LABOR_ANY CES_ANY ///
		MEM_SPON MEM_FIRST LABOR_ANY LABOR_INDUCED m09_birth_dsterm_inf1 ///
		m09_macer_ceoccur_inf1 m09_fhr_vstat_inf1 m09_cry_ceoccur_inf1 ///
		PREG_END_GA PREG_END_GA_WK ///
		if PPROM_PREGEND == 1 & PRETERM_SPON_IND == . 
		 
	
		
		* As of 5/7/2024:
			* 7 total observations picked up
			* 3 are observations that experienced PPROM but not Preterm Delivery
			* 4 are observations that experiened PPROM but no labor at all & 
				* no c-section (need further review). 
				* Of these 4, 2 appear to be early stillbirths at <22 weeks GA. 
				* We'll need to confirm if this type of outcome is expected 
				* for an early stillbirth (pregnancy loss with no labor), and 
				* if this should be considered as PPROM.
				
		* As of 6/17/2024:
			* 15 total observations picked up
			* 5 are observations that experienced PPROM but not Preterm Delivery (GA at delivery >=37)
			* 10 are observations that experiened PPROM but no labor at all & 
				* no c-section (need further review). 
				* Of these 10, 2 appear to be early stillbirths at <22 weeks GA. 
				* Of these 10, 1 is an ~early stillbirths at 24 weeks GA. 
				* We'll need to confirm if this type of outcome is expected 
				* for an early stillbirth (pregnancy loss with no labor), and 
				* if this should be considered as PPROM.
				
		* As of 7/1/2024:
			* Based on updates proposed by Dr. Wylie, we will consider observations 
			* with "no labor" and "vaginal delivery" to be spontaneous preterm 
			* if spontaneous ROM; therefore, the 10 problem cases above are 
			* resolved. 5 remain: observations that experienced PPROM but not 
			* Preterm Delivery (GA at delivery >=37)
			
		*Update for 7-26 dataset (processed 8/15):
			*We find 7 observations:
				*5 are observations that experienced PPROM but not Preterm Delivery (GA >=37)
				*The remaining 2 are PPROM with pregnancy loss at <20 weeks GA (i.e., not a preterm birth)
				
		*Update for 9-6 dataset (processed 9/18):
			*We find 6 observations:
				*3 are observations that experienced PPROM but not Preterm Delivery (GA >=37)
				*The remaining 3 are PPROM with pregnancy loss at <20 weeks GA (i.e., not a preterm birth)
				
				
		*Update for 9-20 dataset (processed 10/2):
			*We find 6 observations:
				*3 are observations that experienced PPROM but not Preterm Delivery (GA >=37)
				*The remaining 3 are PPROM with pregnancy loss at <20 weeks GA (i.e., not a preterm birth)
				
		*Update for 1-10-25 dataset (processed 1/15):
			*We find 10 observations:
				*6 are observations that experienced PPROM but not Preterm Delivery (GA >=37)
				*The remaining 4 are PPROM with pregnancy loss at <20 weeks GA (i.e., not a preterm birth)
			
				
	***** INCORPORATE ANY OBSERVATIONS WITH PROM DX DURING HOSPITALIZATION AT 
	***** <37 weeks:
	
		// merge in hospitalization form data: 
	merge 1:1 momid pregid using "$wrk/endpoint_outcomes_pregend_MNH19"
	drop _merge 
	
	gen PROM_HOSP_GA = PROM_HOSP_DT - PREG_START_DATE if PROM_HOSP == 1 
	label var PROM_HOSP_GA "GA at dx of PROM during hospitalization" 
	
	*Review cases dx of PROM during hospitalization: 
	list site PPROM_PREGEND PRETERM_CLASS PREG_END_DATE PREG_END_GA ///
		PROM_HOSP PROM_HOSP_DT PROM_HOSP_GA ///
		if PROM_HOSP == 1 
		
	*Create Combined Variable: 
		// start from MNH09 variable: 
	gen PPROM_OCCUR = PPROM_PREGEND 
	
		// incorporate PROM cases with dx at < 37 weeks: 
	replace PPROM_OCCUR = 1 if PROM_HOSP == 1 & PROM_HOSP_GA <= 258 & ///
		PROM_HOSP_GA != . & PROM_HOSP_GA <= PREG_END_GA
		
		// 0 if PROM dx at >= 37 weeks 
	replace PPROM_OCCUR = 0 if PPROM_OCCUR == 55 & ///
		(PROM_HOSP == 0 | ///
		(PROM_HOSP == 1 & PROM_HOSP_GA > 258 & PROM_HOSP_GA != .)) 
	
		
	label var PPROM_OCCUR "PPROM at L&D (MNH09) or Dx of PROM at <37 weeks (MNH19)"
	
	tab PPROM_OCCUR, m 
	
	*Update missing indicator: 
	gen PPROM_OCCUR_MISS = PPROM_PREGEND_MISS 
	replace PPROM_OCCUR_MISS = 0 if PPROM_OCCUR == 1 | PPROM_OCCUR == 0 
	replace PPROM_OCCUR_MISS = 4 if PROM_HOSP == 55 & PPROM_OCCUR == 55 
	
	tab PPROM_OCCUR_MISS PPROM_OCCUR, m 
	
	label values PPROM_OCCUR_MISS pprom_missed 
	

	///////////////////////////////////////////////////////////////////////////
	** MAKE A BAR GRAPH **
	///////////////////////////////////////////////////////////////////////////
	
	global pct3 `" 0 "0%" .01 "1%" .02 "2%" .03 "3%" .04 "4%" .05 "5%" "'	
  
	betterbar ///
	PPROM_PREGEND if PPROM_PREGEND !=55 & PPROM_PREGEND != 77 ///
	, over(site_num) bar format(%9.2f) n xlab($pct3) ///
	legend(c(4)) ysize(17) xsize(40)
	
	graph export "$output/PPROM_PREGEND_$date.jpg", replace
	
	betterbar ///
	PPROM_OCCUR if PPROM_OCCUR !=55  & PPROM_OCCUR != 77 ///
	, over(site_num) bar format(%9.2f) n xlab($pct3) ///
	legend(c(4)) ysize(17) xsize(50)
	
	graph export "$output/PPROM_OCCUR_$date.jpg", replace
	
	///////////////////////////////////////////////////////////////////////////
	** END MAKE A BAR GRAPH **
	///////////////////////////////////////////////////////////////////////////
	
* * * PROM - added on 3-11-2025; as requested by IHME
	*PROM_PREGEND
	gen PROM_PREGEND = .
	label var PROM_PREGEND "PROM at L&D (MNH09)"
	
	// Unknown PROM if unknown membrane rupture method or timing: 
	replace PROM_PREGEND = 55 if (MEM_SPON == 55 | (MEM_SPON == 1 & MEM_DTT == .))  /// 
		&  m09_membrane_rupt_mhterm != . 
	
	// Unknown PROM if Spontaneous ROM but unknown if labored  
	replace PROM_PREGEND = 55 if MEM_SPON == 1 & LABOR_ANY == 55 
		
	// Unknown PROM if Spontaneous ROM but labor timing missing
	replace PROM_PREGEND = 55 if MEM_SPON == 1 & LABOR_ANY == 1 & ///
		LABOR_DTT == . 
		
	// Unknown PROM if Spontaneous ROM, no labor but induced, and vaginal delivery: 
	replace PROM_PREGEND = 55 if MEM_SPON == 1 & LABOR_ANY == 0 & LABOR_INDUCED ==1 & ///
		LABOR_DTT == . & CES_ANY == 0 
		
	// Unknown PROM if pregnancy loss only documented in MNH04/19 or death & not MNH09 
	replace PROM_PREGEND = 55 if ///
		PREG_LOSS_INDUCED == 0 & PREG_END_SOURCE >= 2 & PREG_END_SOURCE <=4 
	
	// NOT PROM if labor occurs first: 
	replace PROM_PREGEND = 0 if LABOR_FIRST == 1
	
	// NOT PROM if artificial rupture of membranes / no rupture 
	replace PROM_PREGEND = 0 if (MEM_ART == 1 | MEM_CES == 1)
	
	// PROM if Spontaneous ROM prior to labor onset: 
	replace PROM_PREGEND = 1 if MEM_SPON == 1 & MEM_FIRST == 1 
		
	// PROM if Spontaneous ROM prior to a cesarean delivery (no labor): 
	replace PROM_PREGEND = 1 if MEM_SPON == 1 & MEM_FIRST == . & ///
		LABOR_ANY == 0 & CES_ANY == 1 
		
	// PROM if Spontaneous ROM & no labor/no induced labor (no cesarean -- most suspicious, but includes some instances of pregnancy loss <20wks): 
	replace PROM_PREGEND = 1 if MEM_SPON == 1 & MEM_FIRST == . & ///
		LABOR_ANY == 0 & LABOR_INDUCED == 0 & CES_ANY == 0 
		
	replace PROM_PREGEND = 1 if MEM_SPON == 1 & MEM_FIRST == . & ///
		LABOR_ANY == 0 & LABOR_INDUCED == 55 & CES_ANY == 0 
		 
		
	// N/A if induced abortion: 
	replace PROM_PREGEND = 77 if PREG_LOSS_INDUCED == 1 & m09_membrane_rupt_mhterm == . 
		
	tab PROM_PREGEND, m 

	
	list PROM_PREGEND m09_membrane_rupt_mhterm MEM_SPON MEM_GA_WK ///
		MEM_FIRST LABOR_ANY ///
		LABOR_INDUCED LABOR_DTT PREG_END_DATE PREG_END_GA CES_ANY ///
		PREG_LOSS PREG_LOSS_INDUCED PREG_LOSS_DEATH if PROM_PREGEND == . 
		
	*Check against PPROM:
	tab PROM_PREGEND PPROM_PREGEND, m 
	
	*NOTE: One observation of PROM is unknown for PPROM as of 3-11; this is 
	*because the date & time of ROM is unknown; we know that spontanenous ROM 
	*occurrred with (reportedly) not labor, but we do not know if it was preterm.
	
	*Layer in clinical dx of PROM at hospitalization: 
	gen PROM = PROM_PREGEND 
	replace PROM = 1 if PROM_HOSP ==1 
	
	label var PROM "Premature rupture of membranes (MNH09 OR clinical Dx)"
	
	tab PROM, m 
	
	
	//////////////////////////////////////////////////////////////////////////
	
	*stop 
	
	*data checks for re-running the code on new data:
	
	tab PREG_END_GA PRETERM_ANY, m 
	tab PRETERM_ANY site, m 
	
	tab PREG_END_GA PREG_LOSS, m 
	tab PREG_LOSS site, m 	
	
	tab PRETERM_PROV PRETERM_SPON, m 
	
	tab PRETERM_PROV_IND PRETERM_PROV, m 
	tab PRETERM_SPON_IND PRETERM_SPON, m 
	
	tab PRETERM_PROV_MISS PRETERM_ANY, m 
	
	tab PRETERM_SPON_IND PPROM_OCCUR, m 
		
	//////////////////////////////////////////////////////////////////////////
	** Outcomes added on 5-13-2024 **
	//////////////////////////////////////////////////////////////////////////	
	
	/* variables constructed below:
	
	MAT_UTER_RUP - Uterine Rupture 
		Definition: Tear in the muscular wall of the uterus during pregnancy or 
		childbirth; the spontaneous tearing of the uterus prior to delivery that 
		may result in the fetus being expelled into the peritoneal cavity; 
		occurring prior to delivery or during labor. 
		Values: 0=No,1=Yes,55=Missing
		Denominator: completed pregnancies
		Variables of interest drawn from: MNH09; MNH12 (PNC-0); MNH19 (hosp)
		Subvariables constructed:
			*MAT_UTER_RUP_IPC -- recorded at pregnancy endpoint/MNH09 
			*MAT_UTER_RUP_PNC -- recorded at PNC/MNH12 
			*MAT_UTER_RUP_HOSP -- recorded during hospitalization 
		
	PRO_LABOR - prolonged labor 
		Definition (UPDATED 6-27): Prolonged labor is defined as labor 
		lasting ≥24 hours (regardless of parity)
		Values: 0=No,1=Yes,55=Missing
		Denominator (UPDATED 6-27): all pregnancies with labor 
		Variables of interest drawn from: MNH09
		 
	OBS_LABOR - obstructed labor
		Definition: Obstructed labor based on check-box response -- 
		requested for G3 study. 
		Values: 0=No, 1=Yes, 55=Missing 
		Denominator (UPDATED 6-27): all pregnancies with labor 
		Variables of interest drawn from: MNH09 
		
	MEM_HOURS - hours between ROM & delivery 
		Definition: Hours between date/time of ROM and date/time of delivery
		(for first infant if multiple birth)
		Values: continuous 
		Denominator: all completed pregnancies with ROM 
		Variables of interest drawn from: MNH09 
		
	*/
	
	
	* Ruptured uterus 
		// variable: did mother have ruptured uterus
	tab m09_rupt_uterus_ceoccur, m 
		// variable: uterine rupture as complication to PPH: 
	tab m09_pph_comp_3 m09_pph_ceoccur, m 
	
	gen MAT_UTER_RUP_IPC = m09_rupt_uterus_ceoccur 
	replace MAT_UTER_RUP_IPC = 55 if m09_rupt_uterus_ceoccur == 77 | ///
		m09_rupt_uterus_ceoccur == . | m09_rupt_uterus_ceoccur == 99 
		// add additional var: 
	replace MAT_UTER_RUP_IPC = 1 if m09_pph_comp_3 == 1 
	label var MAT_UTER_RUP_IPC "Ruptured uterus at L&D"
	
	tab MAT_UTER_RUP_IPC, m 
	
	gen MAT_UTER_RUP_IPC_MISS = 0 if MAT_UTER_RUP_IPC == 1 | MAT_UTER_RUP_IPC == 0 
	
	replace MAT_UTER_RUP_IPC_MISS = 1 if ///
		MAT_UTER_RUP_IPC == 55 & m09_rupt_uterus_ceoccur != .
		
	replace MAT_UTER_RUP_IPC_MISS = 2 if ///
		MAT_UTER_RUP_IPC == 55 & ///
		PREG_END_SOURCE >= 2 & PREG_END_SOURCE <=3 
		
	replace MAT_UTER_RUP_IPC_MISS = 4 if ///
		MAT_UTER_RUP_IPC == 55 & ///
		PREG_END_SOURCE ==4  
		
	label define urmiss 0 "0-Non-missing" 1 "1-Missing info" ///
		2 "2-Loss in MNH04/19 only" 4 "4-Maternal death with no info on uterine rupture"
	label values MAT_UTER_RUP_IPC_MISS urmiss 
	
	label var MAT_UTER_RUP_IPC_MISS "Reason missing - Uterine rupture at L&D"
	
	tab MAT_UTER_RUP_IPC_MISS MAT_UTER_RUP_IPC, m 
	

	****** Incorporate hospitalization variable into a combined outcome:
	gen MAT_UTER_RUP = MAT_UTER_RUP_IPC 
	
	replace MAT_UTER_RUP = 1 if MAT_UTER_RUP_HOSP == 1 
	replace MAT_UTER_RUP = 0 if MAT_UTER_RUP_IPC == 55 & ///
		MAT_UTER_RUP_HOSP == 0 
		
		*check on updates:
		tab MAT_UTER_RUP MAT_UTER_RUP_HOSP, m 
		
	label var MAT_UTER_RUP "Uterine rupture at L&D, Hospitalization, or PNC"
	
	tab MAT_UTER_RUP, m 
	
	
	*Obstructed labor 
		// variable: did mother have obstructed labor 
	tab m09_obstructed_labor_ceoccur, m 
	
	gen OBS_LABOR = m09_obstructed_labor_ceoccur 
	replace OBS_LABOR = 55 if m09_obstructed_labor_ceoccur == . | ///
		m09_obstructed_labor_ceoccur  == 99 | m09_obstructed_labor_ceoccur  == 77 
		
	tab OBS_LABOR LABOR_ANY, m 
	
		*cleanup: we can consider those with "missing" info on 
		*obstructed labor to be "0" if the participant did not experience 
		*labor: 
		replace OBS_LABOR = 0 if OBS_LABOR == 55 & LABOR_ANY == 0 
		
		*cleanup: we can consider those with pregnancy loss/induced abortion to be
		*N/A for thisoutcome:
		replace OBS_LABOR = 77 if PREG_LOSS == 1 | PREG_LOSS_INDUCED == 1 
		
		*Also N/A will be maternal deaths in early pregnancy (<20 weeks)
		replace OBS_LABOR = 77 if PREG_LOSS_DEATH == 1 & PREG_END_GA_WK < 20 & ///
			PREG_END_GA_WK != . 
		
		*cleanup: we can consider those with pregnancy loss/induced abortion to be
		*N/A for this outcome:
		replace OBS_LABOR = 77 if LABOR_ANY == 0 
		
		*incorporate hospitalization: 
		replace OBS_LABOR = 1 if OBS_LABOR_HOSP == 1 & OBS_LABOR != 77
		replace OBS_LABOR = 0 if OBS_LABOR == 55 & OBS_LABOR_HOSP == 0 & ///
			PREG_END_SOURCE == 3
		
	label var OBS_LABOR "Obstructed labor"
	tab OBS_LABOR, m 
	
	
	*ADDED 6-27: Generate a denominator variable for "LABOR_DENOM"
	gen LABOR_DENOM = LABOR_ANY
	replace LABOR_DENOM = 1 if OBS_LABOR == 1 
	replace LABOR_DENOM = 55 if LABOR_ANY == . 

	replace LABOR_DENOM = 0 if PREG_END_GA_WK <20 & PREG_END_GA_WK != . 
	
	label var LABOR_DENOM "Participant experienced any labor"
	
	tab LABOR_DENOM, m 
	
	
		// construct missing indicator for obstructed albor: 
	gen OBS_LABOR_MISS = 0 if OBS_LABOR == 1 | OBS_LABOR == 0 
	
	// missing info: 
	replace OBS_LABOR_MISS = 1 if OBS_LABOR == 55 & PREG_LOSS == 0 
	
	// exclude: pregnancy loss/induced abortion
	replace OBS_LABOR_MISS = 2 if OBS_LABOR == 77 & (PREG_LOSS == 1 | PREG_LOSS_INDUCED == 1) 
	
	// exclude: maternal death in early pregnancy
	replace OBS_LABOR_MISS = 3  if PREG_LOSS_DEATH == 1 & PREG_END_GA_WK < 20 & ///
			PREG_END_GA_WK != . 
	
	// exclude: no labor: 
	replace OBS_LABOR_MISS = 4 if OBS_LABOR == 77 & (LABOR_ANY == 0 & LABOR_DENOM == 0 & PREG_LOSS == 0 & PREG_LOSS_INDUCED == 0 & CES_ANY==1)
	
		// MISSING if: pregnancy ends without labor BUT vaginal delivery reported 
	replace OBS_LABOR = 55 if (LABOR_ANY == 0 & PREG_LOSS == 0 & ///
		PREG_LOSS_INDUCED==0 & (CES_ANY==0 | CES_ANY==55)) & OBS_LABOR != 1
		
	replace OBS_LABOR_MISS = 5 if (LABOR_ANY == 0 & PREG_LOSS == 0 & ///
		PREG_LOSS_INDUCED==0 & (CES_ANY==0 | CES_ANY==55)) & OBS_LABOR != 1
		
	tab OBS_LABOR, m 
	tab OBS_LABOR_MISS OBS_LABOR, m  
		
	label define oblm1 0 "0-Non-missing" 1 "1-Missing info" ///
		2 "2-Pregnancy loss <20 or induced abortion" ///
		3 "3-Maternal death in early pregnancy <20 weeks" ///
		4 "4-Pregnancy ends without labor" ///
		5 "5-Conflicting delivery info"
	label values OBS_LABOR_MISS oblm1 
	label var OBS_LABOR_MISS "Reason missing-Obstructed labor"
	
	tab OBS_LABOR_MISS OBS_LABOR, m 
	
	tab OBS_LABOR_MISS if PREG_LOSS_DEATH==1
	
	tab OBS_LABOR_MISS OBS_LABOR, m 
	
	
	*Prolonged labor 
	
		// first create a date-time variable for delivery (1st infant)
	gen date_time_string3 = m09_deliv_dsstdat_inf1 + " " + m09_deliv_dssttim_inf1
	gen  double DELIV_DTT = clock(date_time_string3, "YMDhm")
	format DELIV_DTT %tc
	sum DELIV_DTT, format	
	
	label var DELIV_DTT "Date & time of delivery (1st infant)"

		// next, examine time difference in hours between labor onset & delivery 
	generate LABOR_HOURS = hours(DELIV_DTT - LABOR_DTT)
	label var LABOR_HOURS "Hours of labor before delivery (1st infant)"
	sum LABOR_HOURS 
	
	*** review variables with negative labor time: 
	list DELIV_DTT LABOR_DTT PREG_END_GA_WK if LABOR_HOURS <0 & ///
		LABOR_HOURS != . 
		
	gen TIME_ERROR = 0
	replace TIME_ERROR = 1 if DELIV_DTT < LABOR_DTT & DELIV_DTT != . & ///
		LABOR_DTT != . 
	label var TIME_ERROR "=1 if date/time of labor onset if after date/time of delivery"
	
	tab TIME_ERROR, m 
	
	tab TIME_ERROR site, m
	
	*** review variables with too long of labor:  
	gen TIME_ERROR_LONG = 0
	replace TIME_ERROR_LONG = 1 if LABOR_HOURS > 168 & LABOR_HOURS != . 
	label var TIME_ERROR_LONG "=1 if date/time variables indicate labor lasted more than 7 days"
	
	tab TIME_ERROR_LONG, m 
	
	tab TIME_ERROR_LONG site, m 
	
	*** minor data cleaning: 
	replace LABOR_HOURS = . if TIME_ERROR == 1 | TIME_ERROR_LONG == 1 
	
	sum LABOR_HOURS
	
	histogram LABOR_HOURS, xline(24) percent
	
	/* List errors by site for query team (6-13-2024)
	sort site 
	
	preserve 
	
		keep if TIME_ERROR == 1 
		keep site momid pregid LABOR_DTT DELIV_DTT 
		
		export excel "$wrk/Labor_Timing_Review_240613", firstrow(variables)
		
	restore 
	
	preserve 
	
		keep if TIME_ERROR_LONG == 1 
		keep site momid pregid LABOR_DTT DELIV_DTT 
		
		export excel "$wrk/Labor_Timing_Review_2_240613", firstrow(variables)

	restore 
		
	*/
	
	
		// merge in parity information: 
	/*	
	drop _merge
	merge 1:1 momid pregid using "$wrk/DEMO", keepusing(PARITY)
	
	keep if _merge == 1 | _merge == 3 
	
	*fix observations with missing partiy:
	replace PARITY = 55 if PARITY == . 
	
	drop _merge 
	*/
	
	*Variable construction:
	*UPDATED 6-27 : prolonged labor outcome is now >=24 hours for any parity: 
	
		// No prolonged labor if no labor at all: 
	gen PRO_LABOR = 0 if LABOR_ANY == 0 
	
		// No prolonged labor <= 24 hour labor:
	replace PRO_LABOR = 0 if LABOR_ANY == 1 & LABOR_HOURS <=24 & LABOR_HOURS >=0  
	
		// update for those with obstructed labor even if LABOR_ANY==55: 
	replace PRO_LABOR = 0 if LABOR_ANY == 55 & LABOR_HOURS <=24 & LABOR_HOURS >=0 & ///
		OBS_LABOR==1
		
		// Prolonged labor if > 24 hour labor: 
	replace PRO_LABOR = 1 if LABOR_ANY == 1 & LABOR_HOURS >24 & LABOR_HOURS!=. 
	
		// update for those with obstructed labor even if LABOR_ANY==55: 
	replace PRO_LABOR = 1 if LABOR_ANY == 55 & LABOR_HOURS >24 & LABOR_HOURS!=. & ///
		OBS_LABOR==1

	gen PRO_LABOR_MISS = 0 if PRO_LABOR == 1 | PRO_LABOR == 0 
		
		// Missing if: unknown if labor:
	replace PRO_LABOR = 55 if (LABOR_DENOM == 55 & LABOR_ANY == 55) ///
		| (LABOR_ANY == . & PREG_LOSS ==0 & PREG_END_SOURCE == 1) 
	
	replace PRO_LABOR_MISS = 1 if (LABOR_DENOM == 55 & LABOR_ANY == 55) ///
		| (LABOR_ANY == . & PREG_LOSS ==0 & PREG_END_SOURCE == 1)
	
		// Missing if: time missing/unknown: 
	replace PRO_LABOR = 55 if (LABOR_ANY == 1 & LABOR_HOURS == . & ///
		TIME_ERROR == 0 & TIME_ERROR_LONG == 0)  | ///
		(OBS_LABOR == 1 & LABOR_HOURS == . & ///
		TIME_ERROR == 0 & TIME_ERROR_LONG == 0)
		
	replace PRO_LABOR_MISS = 2 if (LABOR_ANY == 1 & LABOR_HOURS == . & ///
		TIME_ERROR == 0 & TIME_ERROR_LONG == 0 ) | ///
		(OBS_LABOR == 1 & LABOR_HOURS == . & ///
		TIME_ERROR == 0 & TIME_ERROR_LONG == 0)
		
		// Missing if: time contains errors: 
	replace PRO_LABOR = 55 if LABOR_ANY == 1 & LABOR_HOURS == . & ///
		(TIME_ERROR == 1 | TIME_ERROR_LONG == 1)
		
	replace PRO_LABOR_MISS = 3 if LABOR_ANY == 1 & LABOR_HOURS == . & ///
		(TIME_ERROR == 1 | TIME_ERROR_LONG == 1)
		
		// Not applicable if: pregnancy loss <20 weeks OR induced abortion 
	replace PRO_LABOR = 77 if (PREG_LOSS == 1 | PREG_LOSS_INDUCED == 1)
		
	replace PRO_LABOR_MISS = 4 if (PREG_LOSS == 1 | PREG_LOSS_INDUCED == 1)
		
		// Not applicable if: maternal death in early pregnancy: 
	replace PRO_LABOR = 77 if (PREG_LOSS_DEATH==1 & PREG_END_GA_WK <20 & PREG_END_GA_WK != .) 
		
	replace PRO_LABOR_MISS = 5 if (PREG_LOSS_DEATH==1 & PREG_END_GA_WK <20 & PREG_END_GA_WK != .)  
		
		// Not applicable if: pregnancy ends without labor
	replace PRO_LABOR = 77 if (LABOR_ANY == 0 & LABOR_DENOM == 0 & PREG_LOSS == 0 & CES_ANY==1)
		
	replace PRO_LABOR_MISS = 6 if (LABOR_ANY == 0 & LABOR_DENOM == 0 & PREG_LOSS == 0 & PREG_LOSS_INDUCED == 0 & CES_ANY==1)
	
		// MISSING if: pregnancy ends without labor BUT vaginal delivery reported 
	replace PRO_LABOR = 55 if (LABOR_ANY == 0 & PREG_LOSS == 0 & ///
		PREG_LOSS_INDUCED==0 & (CES_ANY==0 | CES_ANY==55))
		
	replace PRO_LABOR_MISS = 7 if (LABOR_ANY == 0 & PREG_LOSS == 0 & ///
		PREG_LOSS_INDUCED==0 & (CES_ANY==0 | CES_ANY==55))
		
	tab PRO_LABOR, m 
	tab PRO_LABOR_MISS PRO_LABOR, m  
	
	
	*CHECKS ON MISSING DATA:
	list PRO_LABOR LABOR_ANY LABOR_INDUCED LABOR_SPON m09_labor_mhstdat ///
		m09_labor_mhsttim m09_deliv_dsstdat_inf1 m09_deliv_dssttim_inf1 ///
		LABOR_HOURS PREG_LOSS PREG_LOSS_INDUCED PREG_END_GA  if ///
		PRO_LABOR == .  
	
	label var PRO_LABOR "Prolonged labor (>24 hours)"
	label var PRO_LABOR_MISS "Reason missing: Prolonged labor"
	
	label define plab1 0 "0-Non-missing" 1 "1-Unknown if labor" ///
		2 "2-Missing timing" 3 "3-Time errors" ///
		4 "4-Pregnancy loss <20 or induced abortion" ///
		5 "5-Maternal death in early pregnancy (<20wks)" ///
		6 "6-Pregnancy ends without labor" ///
		7 "7-Conflicting delivery info"
		
	
	label values PRO_LABOR_MISS plab1 
	
	tab PRO_LABOR_MISS, m 
	tab PRO_LABOR_MISS PRO_LABOR , m 
	
	tab PRO_LABOR_MISS OBS_LABOR_MISS,m 
	
	* Construct a continuous variable for the time between ROM and delivery 
	* (per IHME request)
 
	gen MEM_HOURS = hours(DELIV_DTT - MEM_DTT) if MEM_DTT != . & DELIV_DTT!=. 
	replace MEM_HOURS = . if MEM_HOURS >800 | MEM_HOURS <0
	label var MEM_HOURS "Hours between rupture of membranes and delivery"
	sum MEM_HOURS, d 
	histogram MEM_HOURS
	graph box MEM_HOURS if MEM_HOURS<200, over(SITE)
	
	list SITE MEM_DTT DELIV_DTT MEM_HOURS MEM_SPON MEM_ART MEM_CES
	
	
	//////////////////////////////////////////////////////////////////////////
	** Outcomes added on 10-15-2024 **
	//////////////////////////////////////////////////////////////////////////		
	
	/* variables constructed below: 
	
	MAT_INDUCED - Labor was induced
		0=Spontaenous labor 
		1=Induced labor 
		55=Missing if induced 
		66=Conflicting info (on ROM)
		77=Not applicable - no labor AND not induced 
	
	MAT_INDUCED_TYPE - Codes 5 scenarios of induced labor: 
		1=labor induced by artificial rupture of membranes
		2=labor induced by another method prior to rupture of membranes 
		3=labor induced after spontaneous PPROM or PROM 
		4=labor induced, unknown type
		5=labor induced, unknown timing of events
		
	MAT_INDUCED_MISS = Missingness indicator for induced labor 
	
	*/
	
	*Construct variable: MAT_INDUCED 
		// 0 if spontaneous labor: 
	gen MAT_INDUCED = 0 if LABOR_SPON == 1 
		// not applicable if cesarean delivery with no labor/induction 
	replace MAT_INDUCED = 77 if LABOR_ANY == 0 & LABOR_INDUCED == 0 & ///
		CES_ANY==1
		// missing if: unknown if labor was induced / unknown if labor 
	replace MAT_INDUCED = 55 if LABOR_INDUCED == 55 | (LABOR_ANY == 55 & ///
		LABOR_INDUCED == 0)
		
		// missing if: no labor, no induction, vaginal delivery: 
	replace MAT_INDUCED = 55 if LABOR_ANY==0 & LABOR_INDUCED==0 & ///
		MEM_ART==0 & CES_ANY==0 
		
		// missing if: no labor, no induction, unknown delivery mode
	replace MAT_INDUCED = 55 if LABOR_ANY==0 & LABOR_INDUCED==0 & CES_ANY==55
		
		// Labor induced (as reported in variable):
	replace MAT_INDUCED = 1 if LABOR_INDUCED == 1 
		// Labor induced if ROM occurs first & artificial ROM is reported 
	replace MAT_INDUCED = 1 if MEM_FIRST == 1 & MEM_ART == 1 
		// Labor induced if no labor & artificial ROM is reported 
	replace MAT_INDUCED = 1 if MEM_ART == 1 & LABOR_ANY == 0 
	
		// Flag conflicting info: 
	replace MAT_INDUCED = 66 if LABOR_INDUCED == 1 & ///
		m09_induced_prtrt_1 == 1 & MEM_SPON == 1 & MEM_FIRST == 1 
		
		// replace with 77 if NOT A DELIVERY (GA<20)
	replace MAT_INDUCED = 77 if DELIVERY_ANY !=1  
	
	label var MAT_INDUCED "Labor was induced"
	
	tab MAT_INDUCED, m 
	
	*Construct the missingness indicator:
	gen MAT_INDUCED_MISS = 0 if MAT_INDUCED == 1 | MAT_INDUCED == 0 
	
		// missing if unknown if labor 
	replace MAT_INDUCED_MISS = 1 if MAT_INDUCED == 55 & LABOR_ANY == 55 & LABOR_INDUCED ==0
		
		// missing if unknown if induced 
	replace MAT_INDUCED_MISS = 2 if MAT_INDUCED == 55 & LABOR_INDUCED == 55 
	
		// missing if conflicting info on ROM -- consider the same as above
	replace MAT_INDUCED_MISS = 2 if MAT_INDUCED == 66 
	
		// missing if no labor, no induction, vaginal delivery:
	replace MAT_INDUCED_MISS = 3 if LABOR_ANY==0 & LABOR_INDUCED==0 & ///
		MEM_ART==0 & CES_ANY==0  
		
		// missing if no labor, no induction, unknown delivery mode:
	replace MAT_INDUCED_MISS = 3 if LABOR_ANY==0 & LABOR_INDUCED==0 & ///
		MEM_ART==0 & CES_ANY==55  
	
		// excluded if pregnancy loss <20 weeks 
	replace MAT_INDUCED_MISS = 4 if MAT_INDUCED == 77 & DELIVERY_ANY !=1 
		
		// excluded if never induced and never labor 
	replace MAT_INDUCED_MISS = 5 if MAT_INDUCED == 77 & DELIVERY_ANY == 1 
	
	label define indmiss 0 "0-Nonmissing" 1 "1-Unknown if labor" ///
		2 "2-Unknown if induced" 3 "3-Conflicting labor information" ///
		4 "4-Pregnancy loss <20 weeks" ///
		5 "5-Cesarean without labor/induction"
	
	label values MAT_INDUCED_MISS indmiss
	
	tab MAT_INDUCED_MISS MAT_INDUCED, m 
		
	
	*Construct variable: MAT_INDUCED_TYPE 
		// 1= induced via artificial ROM: 
	gen MAT_INDUCED_TYPE = 1 if MAT_INDUCED == 1 & ///
		m09_induced_prtrt_1 == 1 
		// 1= induction not reported but artificial ROM reported first: 
	replace MAT_INDUCED_TYPE = 1 if MAT_INDUCED == 1 & ///
		MEM_ART == 1 & MEM_FIRST == 1 
		// 1= induction reported, no labor, artificial ROM reported: 
	replace MAT_INDUCED_TYPE = 1 if MAT_INDUCED == 1 & ///
		MEM_ART == 1 & LABOR_ANY == 0 & LABOR_INDUCED == 1 & MEM_FIRST ==.
		// 1= artificial ROM reported, no labor, no timing 
	replace MAT_INDUCED_TYPE = 1 if MAT_INDUCED == 1 & ///
		MAT_INDUCED_TYPE == . & ///
		MEM_ART == 1 & LABOR_ANY == 0 & MEM_FIRST ==. & ///
		(LABOR_INDUCED == 0 | LABOR_INDUCED == 55)
	
		// 2= induction by another method PRIOR to ROM:
			*2=oxytocin; 3=misoprostol; 4=foley; 88=other
	replace MAT_INDUCED_TYPE = 2 if MAT_INDUCED == 1 & ///
		LABOR_FIRST == 1 & ///
		(m09_induced_prtrt_2 == 1 | m09_induced_prtrt_3 == 1 | ///
		 m09_induced_prtrt_4 == 1 | m09_induced_prtrt_88 == 1 ) & ///
		 m09_induced_prtrt_1 != 1 
		
		// 2= induction by another method PRIOR to N/A ROM: 
	replace MAT_INDUCED_TYPE = 2 if MAT_INDUCED == 1 & ///
		LABOR_FIRST == . & LABOR_INDUCED == 1 & MEM_CES == 1 & ///
		(m09_induced_prtrt_2 == 1 | m09_induced_prtrt_3 == 1 | ///
		 m09_induced_prtrt_4 == 1 | m09_induced_prtrt_88 == 1 ) & ///
		 m09_induced_prtrt_1 != 1  
		 
		 // 3= induction following PROM or PPROM: 
	replace MAT_INDUCED_TYPE = 3 if MAT_INDUCED == 1 & ///
		MEM_FIRST == 1 & MEM_SPON == 1 & m09_induced_prtrt_1 != 1 
	
		// 4= Missing information on induction method 
	replace MAT_INDUCED_TYPE = 4 if MAT_INDUCED == 1 & ///
		MAT_INDUCED_TYPE == . & LABOR_INDUCED == 1 & ///
		m09_induced_prtrt_1 == 0 & m09_induced_prtrt_2 == 0 & ///
		m09_induced_prtrt_3 == 0 & m09_induced_prtrt_4 == 0 & ///
		(m09_induced_prtrt_88 == 0 | m09_induced_prtrt_88 == 99) & ///
		(m09_induced_prtrt_99 == 0 | m09_induced_prtrt_99 == 1 | ///
		 m09_induced_prtrt_99 == 55 | m09_induced_prtrt_99 == 77)
		 
		// 4= Missing information on induction method 2 -- all 77s 
	replace MAT_INDUCED_TYPE = 4 if MAT_INDUCED == 1 & ///
		MAT_INDUCED_TYPE == . & LABOR_INDUCED == 1 & ///
		m09_induced_prtrt_1 == 77 & m09_induced_prtrt_2 == 77 & ///
		m09_induced_prtrt_3 == 77 & m09_induced_prtrt_4 == 77 & ///
		m09_induced_prtrt_88 == 77 & m09_induced_prtrt_99 == 1
		
		// 5= Missing information on timing of events 
			// induced, but no labor 
	replace MAT_INDUCED_TYPE = 5 if MAT_INDUCED == 1 & MAT_INDUCED_TYPE==. ///
		& LABOR_INDUCED == 1 & LABOR_ANY == 0 & LABOR_FIRST == . & ///
		MEM_SPON == 1 
			// missing timing - induced labor spontaneous ROM 
	replace MAT_INDUCED_TYPE = 5 if MAT_INDUCED == 1 & MAT_INDUCED_TYPE==. ///
		& LABOR_INDUCED == 1 & LABOR_ANY==1 & LABOR_FIRST == . & MEM_SPON == 1 
			// missing ROM information 
	replace MAT_INDUCED_TYPE = 5 if MAT_INDUCED == 1 & MAT_INDUCED_TYPE==. ///
		& LABOR_INDUCED == 1 & LABOR_ANY==1 & LABOR_FIRST == . & ///
		MEM_SPON == 55 & m09_induced_prtrt_1 != 1
			// ROM is N/A but occurred first (likely error) 
	replace MAT_INDUCED_TYPE = 5 if MAT_INDUCED == 1 & MAT_INDUCED_TYPE==. ///
		& LABOR_INDUCED == 1 & LABOR_ANY==1 & MEM_CES==1 ///
		& MEM_FIRST == 1 & CES_ANY == 0 
		
		//6=special case: Spontaenous ROM + no labor - unknown induction timing
	replace MAT_INDUCED_TYPE = 6 if MAT_INDUCED==1 & LABOR_INDUCED == 1 & ///
		LABOR_ANY == 0 & LABOR_FIRST == . & MEM_SPON == 1 & DELIVERY_PROV == 0 & ///
		DELIVERY_CLASS == 4
	
	tab MAT_INDUCED_TYPE MAT_INDUCED, m 
	
	* examine missing cases: 
	list LABOR_ANY LABOR_SPON LABOR_INDUCED MEM_ART MEM_SPON MEM_CES ///
		LABOR_FIRST MEM_FIRST TIME_ERROR CES_ANY if ///
		MAT_INDUCED == 1 & MAT_INDUCED_TYPE == . 
		
	* examine method of induced labor: 
	list LABOR_INDUCED m09_induced_prtrt_1 m09_induced_prtrt_2 ///
		m09_induced_prtrt_3 m09_induced_prtrt_4 m09_induced_prtrt_88 ///
		m09_induced_prtrt_99 if ///
		MAT_INDUCED == 1 & MAT_INDUCED_TYPE == .
		
	label define induct 1 "1 Induced by Artificial ROM" ///
		2 "2 Induced another way before ROM" ///
		3 "3 Induced following PROM/PPROM" ///
		4 "4 Unknown induction method" ///
		5 "5 Unknown induction timing" ///
		6 "6 Spontaenous ROM + no labor - unknown induction timing"
		
	label values MAT_INDUCED_TYPE induct
	
	tab MAT_INDUCED_TYPE DELIVERY_PROV, m 
	tab MAT_INDUCED_TYPE PPROM_OCCUR, m 
	
	
	* examine conflicting cases: 
	list LABOR_ANY LABOR_SPON LABOR_INDUCED MEM_ART MEM_SPON MEM_CES ///
		LABOR_FIRST MEM_FIRST TIME_ERROR CES_ANY ///
		m09_induced_prtrt_1 m09_induced_prtrt_2 ///
		m09_induced_prtrt_3 m09_induced_prtrt_4 m09_induced_prtrt_88 ///
		m09_induced_prtrt_99 ///
		if MAT_INDUCED_TYPE == 1 & DELIVERY_PROV==0
		
	list LABOR_ANY LABOR_SPON LABOR_INDUCED MEM_ART MEM_SPON MEM_CES ///
		LABOR_FIRST MEM_FIRST TIME_ERROR CES_ANY DELIVERY_CLASS ///
		m09_induced_prtrt_1 m09_induced_prtrt_2 ///
		m09_induced_prtrt_3 m09_induced_prtrt_4 m09_induced_prtrt_88 ///
		m09_induced_prtrt_99 ///
		if MAT_INDUCED_TYPE == 4 & DELIVERY_PROV==0

	tab MAT_INDUCED MAT_INDUCED_TYPE, m 
	
	tab MAT_INDUCED_TYPE
	label var MAT_INDUCED_TYPE "Type of induction"
	
	* Review induction indications - first create variables:
	gen INDUCED_IND_POSTTERM = 0 if LABOR_INDUCED==1
		replace INDUCED_IND_POSTTERM = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_1==1
		label var INDUCED_IND_POSTTERM "Induced labor: Post-term"
	gen INDUCED_IND_HEART = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_HEART = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_2==1
		label var INDUCED_IND_HEART "Induced labor: Nonreassuring fetal heart rate"
	gen INDUCED_IND_PRIORSB = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_PRIORSB = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_3==1
		label var INDUCED_IND_PRIORSB "Induced labor: Prior stillbirth"
	gen INDUCED_IND_MACRO = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_MACRO = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_4==1
		label var INDUCED_IND_MACRO "Induced labor: Macrosomia"
	gen INDUCED_IND_OLIGO = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_OLIGO = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_5==1
		label var INDUCED_IND_OLIGO "Induced labor: Oligohydramnios"
	gen INDUCED_IND_IUGR = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_IUGR = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_6==1
		label var INDUCED_IND_IUGR "Induced labor: IUGR"
	gen INDUCED_IND_HTN = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_HTN = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_7==1
		label var INDUCED_IND_HTN "Induced labor: Chronic or gestational HTN"
	gen INDUCED_IND_DM = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_DM = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_8==1
		label var INDUCED_IND_DM "Induced labor: Chronic or gestational diabetes"
	gen INDUCED_IND_CARDIAC = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_CARDIAC = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_9==1
		label var INDUCED_IND_CARDIAC "Induced labor: Cardiac disease"
	gen INDUCED_IND_ELECT = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_ELECT = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_10==1
		label var INDUCED_IND_ELECT "Induced labor: Elective"
	gen INDUCED_IND_ROM = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_ROM = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_11==1
		label var INDUCED_IND_ROM "Induced labor: Rupture of membranes"
	gen INDUCED_IND_IUFD = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_IUFD = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_12==1
		label var INDUCED_IND_IUFD "Induced labor: Intrauterine fetal demise"
	gen INDUCED_IND_PREEC = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_PREEC = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_13==1
		label var INDUCED_IND_PREEC "Induced labor: Preeclampsia"
	gen INDUCED_IND_CHORIO = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_CHORIO = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_14==1
		label var INDUCED_IND_CHORIO "Induced labor: Chorioamnionitis"
	gen INDUCED_IND_MOVE = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_MOVE = 1 if LABOR_INDUCED==1 & ///
		m09_induced_prindc_15==1
		label var INDUCED_IND_MOVE "Induced labor: Decreased fetal movement"
	
	
	* Review induction indications - recode other-specify: 
	
	list m09_induced_spfy_prindc if m09_induced_spfy_prindc != "n/a" & ///
		m09_induced_spfy_prindc != "77" & m09_induced_prindc_88==1
	
		* OE coding: 
		gen INDUCED_IND_OTHER = 0 if LABOR_INDUCED==1 
		replace INDUCED_IND_OTHER =1 if m09_induced_prindc_88==1  & ///
			m09_induced_spfy_prindc != "n/a" & m09_induced_spfy_prindc != "77"
		
	*New indication: bleeding/hemorrhage: 
	gen spfy_bleeding = 1 if ///
		strpos(m09_induced_spfy_prindc, "Bleeding") >0 | ///
		strpos(m09_induced_spfy_prindc, "PLACENTAL ABRUPTION / APH") >0 | ///
		strpos(m09_induced_spfy_prindc, "Aph") >0 | ///
		strpos(m09_induced_spfy_prindc, "APH") >0 | ///
		strpos(m09_induced_spfy_prindc, ///
		"Pani Ki Theli Phat Gai Thi Blood A Gaya Tha") >0 
		
		*generate the bleeding indication: 
		gen INDUCED_IND_BLEEDING = 0 if LABOR_INDUCED == 1 
		replace INDUCED_IND_BLEEDING = 1 if spfy_bleeding==1
	
		*recode "other" for those fully reassigned 
		list spfy_bleeding INDUCED_IND_BLEEDING m09_induced_spfy_prindc if ///
			INDUCED_IND_BLEEDING == 1
		replace INDUCED_IND_OTHER = 0 if INDUCED_IND_BLEEDING==1
		
	
	*Oligo: 
	gen spfy_oligo = 1 if strpos(m09_induced_spfy_prindc, "anhydromnios") >0
	
		*recode the oligo indications: 
		replace INDUCED_IND_OLIGO = 1 if spfy_oligo==1
		
		*recode "other" for those fully reassigned 
		list spfy_oligo INDUCED_IND_OLIGO m09_induced_spfy_prindc if ///
			spfy_oligo==1

	*Decreased fetal movement: 
	gen spfy_move = 1 if strpos(m09_induced_spfy_prindc, "Decrease FM") >0 | ///
		strpos(m09_induced_spfy_prindc, "Decreased fetal movements") >0 | ///
		strpos(m09_induced_spfy_prindc, "decreased fetal movements") >0 
		
		*recode movement indications: 
		replace INDUCED_IND_MOVE = 1 if spfy_move == 1 
		
		*recode "other" for those fully reassigned 
		list spfy_move INDUCED_IND_MOVE m09_induced_spfy_prindc if ///
			spfy_move==1
		replace INDUCED_IND_OTHER = 0 if spfy_move == 1 
		
	*IUFD 
	gen spfy_iufd = 1 if strpos(m09_induced_spfy_prindc, "FHS absent") >0 | ///
		strpos(m09_induced_spfy_prindc, "Miscarriage") >0 
		
		*recode IUFD indications: 
		replace INDUCED_IND_IUFD = 1 if spfy_iufd == 1 
		
		*recode "other" for those fully reassigned 
		*(& check against birth outcome)
		list spfy_iufd INDUCED_IND_IUFD m09_induced_spfy_prindc ///
			m09_birth_dsterm_inf1 if spfy_iufd==1
		replace INDUCED_IND_OTHER = 0 if spfy_iufd == 1		
		
		
	*Rupture of membranes 
	gen spfy_rom = 1 if strpos(m09_induced_spfy_prindc, ///
		"Pani Ki Theli Phat Gai Thi Blood A Gaya Tha") >0 | ///
		strpos(m09_induced_spfy_prindc, "Prom") >0 
		
		*recode rupture of membranes indication: 
		replace INDUCED_IND_ROM = 1 if spfy_rom == 1 
		
		*recode "other" for those fully reassigned 
		*(&check against data)
		list spfy_rom INDUCED_IND_ROM m09_induced_spfy_prindc ///
			INDUCED_IND_OTHER MAT_INDUCED_TYPE if spfy_rom==1
		replace INDUCED_IND_OTHER = 0 if spfy_rom == 1
		
	*Hypertension 
	gen spfy_htn = 1 if strpos(m09_induced_spfy_prindc, "PIH") >0
	
		*recode HTN indications: 
		replace INDUCED_IND_HTN = 1 if spfy_htn == 1 
		
		*recode "other" for those fully reassigned 
		list spfy_htn INDUCED_IND_HTN m09_induced_spfy_prindc if ///
			spfy_htn==1
		replace INDUCED_IND_OTHER = 0 if spfy_htn == 1
	
	*IUGR 
	gen spfy_iugr = 1 if ///
		strpos(m09_induced_spfy_prindc, "growth restriction") >0 | ///
		strpos(m09_induced_spfy_prindc, "SGA") >0 | ///
		strpos(m09_induced_spfy_prindc, "Small for gestational age") >0 | ///
		strpos(m09_induced_spfy_prindc, "growth restriction") >0 
		
		*recode IUGR indications: 
		replace INDUCED_IND_IUGR = 1 if spfy_iugr == 1 
		
		*recode "other" for those fully reassigned 
		list spfy_iugr INDUCED_IND_IUGR m09_induced_spfy_prindc if ///
			spfy_iugr==1
		replace INDUCED_IND_OTHER = 0 if spfy_iugr == 1
		
	*Other-fetal 
	gen spfy_fetal = 1 if ///
		strpos(m09_induced_spfy_prindc, "anomalous") > 0 | ///
		strpos(m09_induced_spfy_prindc, "Meconium") > 0 | ///
		strpos(m09_induced_spfy_prindc, "multiple pregnancy") > 0 | ///
		strpos(m09_induced_spfy_prindc, "OTHER FETAL CONDITION") > 0 | ///
		strpos(m09_induced_spfy_prindc, "echogenic bowel loops") > 0 
		
		*generate a category for other fetal indication 
		gen INDUCED_IND_OTHER_FETAL = 0 if LABOR_INDUCED==1
		replace INDUCED_IND_OTHER_FETAL = 1 if spfy_fetal == 1 
		
		*recode "other" for those fully reassigned 
		list spfy_fetal INDUCED_IND_OTHER_FETAL m09_induced_spfy_prindc if ///
			spfy_fetal==1
		replace INDUCED_IND_OTHER = 0 if spfy_fetal == 1
	
	*Other-maternal 
	gen spfy_maternal = 1 if ///
		strpos(m09_induced_spfy_prindc, "Cholestasis") > 0 | ///
		strpos(m09_induced_spfy_prindc, "Obesity") > 0 | ///
		strpos(m09_induced_spfy_prindc, "OTHER MATERNAL CONDITION") > 0 	
		
		*generate a category for other maternal indication 
		gen INDUCED_IND_OTHER_MAT = 0 if LABOR_INDUCED==1
		replace INDUCED_IND_OTHER_MAT = 1 if spfy_maternal == 1 
		
		*recode "other" for those fully reassigned 
		list spfy_maternal INDUCED_IND_OTHER_MAT m09_induced_spfy_prindc if ///
			spfy_maternal==1
		replace INDUCED_IND_OTHER = 0 if spfy_maternal == 1
		
	*Problem observations - suggests augmented labor: 
	gen spfy_augment = 1 if ///
		strpos(m09_induced_spfy_prindc, "DELAYED FIRST STAGE") >0 | ///
		strpos(m09_induced_spfy_prindc, "Failure to progress") >0 | ///
		strpos(m09_induced_spfy_prindc, "Mild labor pain") >0 | ///
		strpos(m09_induced_spfy_prindc, "Non progess of labour") >0 | ///
		strpos(m09_induced_spfy_prindc, "Non Progress Of Labor") >0	| ///
		strpos(m09_induced_spfy_prindc, "Npl") >0	| ///
		strpos(m09_induced_spfy_prindc, "Pain not continuously") >0	
	
	*create main indicator: 
	gen INDUCED_IND_AUGMENT = 0 if LABOR_INDUCED==1 
	replace INDUCED_IND_AUGMENT = 1 if spfy_augment==1 
	
	*check against data: 
	list spfy_augment m09_induced_spfy_prindc MAT_INDUCED_TYPE ///
		PREG_END_GA_WK if spfy_augment==1
	
	*We'll leave these as "other" for now.
	
	*create an indicator for unknown indication: 	
	gen INDUCED_IND_UNKNOWN = 0 if LABOR_INDUCED==1 
	replace INDUCED_IND_UNKNOWN = 1 if  LABOR_INDUCED==1 & ///
		INDUCED_IND_BLEEDING==0 & INDUCED_IND_CARDIAC == 0 & ///
		INDUCED_IND_CHORIO == 0 & INDUCED_IND_DM == 0 & ///
		INDUCED_IND_ELECT == 0 & INDUCED_IND_HEART == 0 & ///
		INDUCED_IND_HTN == 0 & INDUCED_IND_IUFD == 0 & ///
		INDUCED_IND_IUGR == 0 & INDUCED_IND_MACRO == 0 & ///
		INDUCED_IND_MOVE == 0 & INDUCED_IND_OLIGO == 0 & ///
		INDUCED_IND_OTHER == 0 & INDUCED_IND_OTHER_FETAL == 0 & ///
		INDUCED_IND_OTHER_MAT == 0 & INDUCED_IND_POSTTERM == 0 & ///
		INDUCED_IND_PREEC == 0 & INDUCED_IND_PRIORSB == 0 & ///
		INDUCED_IND_ROM == 0 
		
	egen INDUCED_IND_COUNT = rowtotal(INDUCED_IND_BLEEDING ///
		INDUCED_IND_CARDIAC INDUCED_IND_CHORIO INDUCED_IND_DM ///
		INDUCED_IND_ELECT INDUCED_IND_HEART INDUCED_IND_HTN ///
		INDUCED_IND_IUFD INDUCED_IND_IUGR INDUCED_IND_MACRO INDUCED_IND_MOVE ///
		INDUCED_IND_OLIGO INDUCED_IND_OTHER INDUCED_IND_OTHER_MAT ///
		INDUCED_IND_OTHER_FETAL INDUCED_IND_POSTTERM INDUCED_IND_PREEC ///
		INDUCED_IND_PRIORSB INDUCED_IND_ROM) if LABOR_INDUCED==1
		
	tab INDUCED_IND_COUNT INDUCED_IND_UNKNOWN, m 
		
	label var INDUCED_IND_AUGMENT "Problem observation: indication suggests augmented (not induced)"	
		
	label var INDUCED_IND_BLEEDING "Induced labor: Vaginal bleeding or APH"
	label var INDUCED_IND_OTHER "Induced labor: Other/unclassified"
	label var INDUCED_IND_OTHER_FETAL "Induced labor: Other fetal indication"
	label var INDUCED_IND_OTHER_MAT "Induced labor: Other maternal indication"
	label var INDUCED_IND_UNKNOWN "Induced labor: Missing/unknown indication"
	label var INDUCED_IND_COUNT "Number of indications for induced labor"
	
	tab INDUCED_IND_UNKNOWN SITE, m 
	
	tab INDUCED_IND_AUGMENT SITE, m 
	
	
	
	//////////////////////////////////////////////////////////////////////////
	** FINALIZE ANALYSIS DATASET - from MNH09 **
	//////////////////////////////////////////////////////////////////////////
	
	*export an analysis data set:
	order site momid pregid PREG_LOSS PREG_END_DATE PREG_END_GA* PREG_END ///
		PREG_END_SOURCE PREG_LOSS_INDUCED PREG_LOSS_DEATH  ///
		PRETERM_ANY PRETERM_ANY_MISS PRETERM_PROV PRETERM_PROV_MISS ///
		PRETERM_SPON PRETERM_CLASS ///
		PPROM_PREGEND PPROM_PREGEND_MISS PPROM_PREGEND_TIMING  ///
		PPROM_OCCUR PPROM_OCCUR_MISS PROM MEM_HOURS ///
		PRETERM_PROV_IND PRETERM_SPON_IND POST_TERM_41_DEL POST_TERM_42_DEL ///
		DELIVERY_CLASS DELIVERY_PROV DELIVERY_PROV_MISS DELIVERY_SPON ///
		LABOR_MEM_HOURS LABOR_ANY ///
		MEM_SPON MEM_ART MEM_CES CES_ANY CES_EMERGENT CES_PLAN ///
		MAT_UTER_RUP_IPC MAT_UTER_RUP_IPC_MISS MAT_UTER_RUP_HOSP ///
		MAT_UTER_RUP OBS_LABOR OBS_LABOR_MISS PRO_LABOR PRO_LABOR_MISS ///
		LABOR_HOURS LABOR_DENOM  ///
		MAT_INDUCED MAT_INDUCED_TYPE INDUCED_IND_* MAT_INDUCED_MISS
		
		drop INDUCED_IND_AUGMENT
	
	keep site momid pregid PREG_LOSS PREG_END_DATE PREG_END_GA* PREG_END ///
		PREG_END_SOURCE PREG_LOSS_INDUCED PREG_LOSS_DEATH ///
		PRETERM_ANY PRETERM_ANY_MISS PRETERM_PROV PRETERM_PROV_MISS ///
		PRETERM_SPON PRETERM_CLASS ///
		PPROM_PREGEND PPROM_PREGEND_MISS PPROM_PREGEND_TIMING ///
		PPROM_OCCUR PPROM_OCCUR_MISS PROM MEM_HOURS ///
		PRETERM_PROV_IND PRETERM_SPON_IND POST_TERM_41_DEL POST_TERM_42_DEL ///
		DELIVERY_CLASS DELIVERY_PROV DELIVERY_PROV_MISS DELIVERY_SPON ///
		LABOR_MEM_HOURS LABOR_ANY ///
		MEM_SPON MEM_ART MEM_CES CES_ANY CES_EMERGENT CES_PLAN ///
		MAT_UTER_RUP_IPC MAT_UTER_RUP_IPC_MISS MAT_UTER_RUP_HOSP ///
		MAT_UTER_RUP OBS_LABOR OBS_LABOR_MISS PRO_LABOR PRO_LABOR_MISS ///
		LABOR_HOURS LABOR_DENOM  ///
		MAT_INDUCED MAT_INDUCED_TYPE INDUCED_IND_* MAT_INDUCED_MISS
		 
	save "$wrk/maternal_outcomes_MNH09", replace 
	
	
///////////////////////////////////////////////////////////////////////
*** INCORPORATE ADDITIONAL DETAILS ON UTERINE RUPTURE REPORTED AT PNC: 
	
	///////// For uterine rupture, we also need to look at PNC: 
	clear 
	
	import delimited "$da/mnh12_merged", bindquote(strict)
	
	tab m12_birth_compl_mhterm_3, m 
	
	gen MAT_UTER_RUP_PNC = m12_birth_compl_mhterm_3 // uterine rupture 
	replace MAT_UTER_RUP_PNC = 55 if m12_birth_compl_mhterm_3 == 77 | ///
		m12_birth_compl_mhterm_3 == 99
	
	label var MAT_UTER_RUP_PNC "Uterine rupture dx at PNC follow-up"
	
	gen PNC_DT = date(m12_visit_obsstdat, "YMD")
	format PNC_DT %td
	label var PNC_DT "Date of PNC visit"
	sum PNC_DT, format 
	
	*reshape to wide 
	sort PNC_DT 
	duplicates tag momid pregid, gen(ENTRY_TOTAL)
	
	*create label for visit type 
	rename m12_type_visit TYPE_VISIT_PNC 
	label var TYPE_VISIT_PNC "MNH12 Visit Type"
	label define vistype 1 "1-Enrollment" 2 "2-ANC-20" 3 "3-ANC-28" ///
		4 "4-ANC-32" 5 "5-ANC-36" 6 "6-IPC" 7 "7-PNC-0" 8 "8-PNC-1" ///
		9 "9-PNC-4" 10 "10-PNC-6" 11 "11-PNC-26" 12 "12-PNC-52" ///
		13 "13-ANC-Unsched" 14 "14-PNC-Unsched" 
	label values TYPE_VISIT_PNC vistype
	tab TYPE_VISIT_PNC, m 
	
	*create an indicator for entry number for each person by date: 
	sort momid pregid PNC_DT TYPE_VISIT_PNC
	quietly by momid pregid :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "MNH12 Entry Number"
	
	tab ENTRY_NUM, m 
	
	sum ENTRY_NUM 
	return list
	
	global i = r(max)
	
	keep momid pregid PNC_DT TYPE_VISIT_PNC MAT_UTER_RUP_PNC ENTRY_NUM ENTRY_TOTAL 
	
	*reshape to wide:
	rename * *_ 
	
	rename momid_ momid 
	rename pregid_ pregid 
	rename ENTRY_NUM_ ENTRY_NUM 
		
	reshape wide PNC_DT TYPE_VISIT_PNC MAT_UTER_RUP_PNC ENTRY_TOTAL ///
	, i(momid pregid) j(ENTRY_NUM) 	
	
	gen MAT_UTER_RUP_PNC = . 
	gen MAT_UTER_RUP_PNC_DT = . 
	format MAT_UTER_RUP_PNC_DT %td 
	
	label var MAT_UTER_RUP_PNC "Uterine rupture dx at PNC visit(s)"
	label var MAT_UTER_RUP_PNC_DT "Date of dx for uterine rupture at PNC"
	
	foreach num of numlist 1/$i {
	
	*if first entry: 
	replace MAT_UTER_RUP_PNC = MAT_UTER_RUP_PNC_`num' if ///
		 MAT_UTER_RUP_PNC == . 
		 
	replace MAT_UTER_RUP_PNC_DT = PNC_DT_`num' if ///
		MAT_UTER_RUP_PNC_DT == . 
	
	*take first non-missing 0:
	replace MAT_UTER_RUP_PNC_DT = PNC_DT_`num' if ///
		MAT_UTER_RUP_PNC_`num' == 0 & ///
		 MAT_UTER_RUP_PNC == 55
		
	replace MAT_UTER_RUP_PNC = 0 if ///
		MAT_UTER_RUP_PNC_`num' == 0 & ///
		MAT_UTER_RUP_PNC == 55 
	
	*1 at any time takes priority:
	replace MAT_UTER_RUP_PNC_DT = PNC_DT_`num' if ///
		MAT_UTER_RUP_PNC_`num' == 1 & ///
		 MAT_UTER_RUP_PNC != 1
		
	replace MAT_UTER_RUP_PNC = 1 if ///
		MAT_UTER_RUP_PNC_`num' == 1 & ///
		MAT_UTER_RUP_PNC != 1
	
	}
	
	tab MAT_UTER_RUP_PNC, m 
	tab MAT_UTER_RUP_PNC_DT MAT_UTER_RUP_PNC, m 
	
	*restrict to final dataset: 
	keep momid pregid MAT_UTER_RUP_PNC MAT_UTER_RUP_PNC_DT 
	
	save "$wrk/endpoint_outcomes_MNH12", replace 
	
	clear
	
	
	//////////////////////////////////////////////////////////////////////////
	* * * Combine & Finalize * * *
	//////////////////////////////////////////////////////////////////////////		
	
	use "$wrk/maternal_outcomes_MNH09"
	
	* merge in MNH-12 to finalize uterine rupture outcome: 
	merge 1:1 momid pregid using "$wrk/endpoint_outcomes_MNH12"
	
	*for now: restrict to only pregnancies with a recorded endpoint 
	*(i.e., drop PNC forms where we don't yet have pregnancy endpoint on file): 
	gen MNH12_ANY = 1 if _merge == 2 | _merge == 3 
	replace MNH12_ANY = 0 if _merge == 1
	label var MNH12_ANY "Observation has any MNH12 observations"
	
	drop if _merge == 2 
	drop _merge 
	
	* update uterine rupture outcome:
	replace MAT_UTER_RUP = 1 if MAT_UTER_RUP_PNC == 1 
	replace MAT_UTER_RUP = 0 if MAT_UTER_RUP_PNC == 0 & ///
		MAT_UTER_RUP == 55 
	tab MAT_UTER_RUP, m 
	
	* update the missing indicator: 
	gen MAT_UTER_RUP_MISS = MAT_UTER_RUP_IPC_MISS 
	replace MAT_UTER_RUP_MISS = 0 if MAT_UTER_RUP == 1 | MAT_UTER_RUP == 0 
	
	* missing in PNC form + L&D form: 
	replace MAT_UTER_RUP_MISS = 3 if MAT_UTER_RUP == 55 & ///
		MAT_UTER_RUP_PNC == 55 & MAT_UTER_RUP_IPC == 55 & ///
		PREG_END_SOURCE == 1 
		
	* maternal death before delivery with missing info -- needs to be added below: 
	replace MAT_UTER_RUP_MISS = 4 if MAT_UTER_RUP == 55 & ///
		PREG_LOSS_DEATH == 1 & PREG_END_SOURCE == 4
	
	label define rupturemiss 0 "0-Non-missing" 1 "1-Missing L&D info (MNH09)" ///
		2 "2-Loss in MNH04/19 only" 3 "3-Missing L&D + PNC info (MNH12)" ///
		4 "4-Maternal death w/ missing info"
	label values MAT_UTER_RUP_MISS rupturemiss 
	
	label var MAT_UTER_RUP_MISS "Reason missing - Uterine Rupture"
	
	tab MAT_UTER_RUP_MISS MAT_UTER_RUP, m 

	
	order MAT_UTER_RUP* MNH12_ANY, last 
	

	/////////////////////////////////////////////////
	***** Review appropriate denominators by outcome:
	
		*PPROM - can occur at ANY gestational age; therefore, N/A is only induced abortion 
		tab PPROM_OCCUR, m 
		tab PPROM_OCCUR PREG_LOSS, m 
		tab PPROM_OCCUR PREG_LOSS_DEATH, m 
		tab PPROM_OCCUR PREG_LOSS_INDUCED, m 

				
		
		*Uterine rupture - can occur at ANY gestational age and as abortion complication; therefore, no N/A
		tab  MAT_UTER_RUP, m 
		tab  MAT_UTER_RUP PREG_LOSS, m 
		tab  MAT_UTER_RUP PREG_LOSS_DEATH, m 
		tab  MAT_UTER_RUP PREG_LOSS_INDUCED, m 
			
	
		*Delivery indicator: 
		gen PREG_END_DELIV = 0 
		replace PREG_END_DELIV = 1 if PREG_END==1 & PREG_LOSS == 0 & ///
			PREG_LOSS_INDUCED ==0 & PREG_LOSS_DEATH == 0 & ///
			(PREG_END_GA_WK >=20 & PREG_END_GA_WK != .)
		label var PREG_END_DELIV "Denominator - all deliveries"
		
		tab PREG_END_DELIV, m 
		
		
		// PRETERM_PROV -- all deliveries (>=20 weeks GA)
		tab PRETERM_PROV if PREG_END_DELIV==1, m 	

		
		// PRETERM_SPON- all deliveries (>=20 weeks GA)
		tab PRETERM_SPON if PREG_END_DELIV==1, m 
				
		
		// PRO_LABOR -- all deliveries (>=20 weeks GA) that experienced labor: 
		tab PRO_LABOR if PREG_END_DELIV==1 & LABOR_DENOM==1, m 
		
		
		// OBS_LABOR -- all deliveries (>=20 weeks GA) that experienced labor:
		tab OBS_LABOR if PREG_END_DELIV==1 & LABOR_DENOM==1, m 
		
		tab LABOR_DENOM PREG_END_DELIV, m 
		
		
	
	////////////////////////////////////////
	*Prep final dataset for outcomes folder: 
	
	rename momid MOMID
	rename pregid PREGID
	rename site SITE
	
	* RENAMING PER CONVENTIONS - ADDED ON JAN 10, 2025: 
	rename CES_ANY MAT_CES_ANY 
	rename CES_EMERGENT	MAT_CES_EMERGENT
	rename CES_PLAN	MAT_CES_PLAN
	rename DELIVERY_PROV MAT_DELIVERY_PROV
	rename DELIVERY_PROV_MISS MAT_DELIVERY_PROV_MISS
	rename DELIVERY_SPON MAT_DELIVERY_SPON
	rename DELIVERY_CLASS MAT_DELIVERY_CLASS
	
	rename LABOR_DENOM PRO_LABOR_DENOM 
	gen OBS_LABOR_DENOM = PRO_LABOR_DENOM 
	
	label var PRO_LABOR_DENOM "Denominator for prolonged labor outcome: women who experienced labor"
	label var OBS_LABOR_DENOM "Denominator for obstructed labor outcome: women who experienced labor"
	
	rename PRETERM_ANY	MAT_PRETERM_ANY
	rename PRETERM_ANY_MISS	MAT_PRETERM_ANY_MISS
	rename PRETERM_PROV	MAT_PRETERM_PROV
	rename PRETERM_PROV_MISS	MAT_PRETERM_PROV_MISS
	rename PRETERM_PROV_IND	MAT_PRETERM_PROV_IND
	rename PRETERM_SPON	MAT_PRETERM_SPON
	rename PRETERM_SPON_IND	MAT_PRETERM_SPON_IND
	rename PRETERM_CLASS	MAT_PRETERM_CLASS
	rename POST_TERM_41_DEL	MAT_POST_TERM_41_DEL
	rename POST_TERM_42_DEL	MAT_POST_TERM_42_DEL
	
	
	* * * * * Incorporating outcome updates: 
	* Per discussions with ERS and BW, we will remove the following sites from 
	* the prolonged labor outcome per feedback from sites that the length of 
	* induced labor is often not recorded. Therefore, we remove Ghana and 
	* India-CMC from the Prolonged Labor Outcome: 
	
		tab PRO_LABOR PRO_LABOR_MISS, m 
		
	replace PRO_LABOR = 777 if SITE == "Ghana" | SITE == "India-CMC"
	replace PRO_LABOR_MISS = 8 if SITE == "Ghana" | SITE == "India-CMC"
	
	
	label define plab_2 0 "0-Non-missing" 1 "1-Unknown if labor" ///
		2 "2-Missing timing" 3 "3-Time errors" ///
		4 "4-Pregnancy loss <20 or induced abortion" ///
		5 "5-Maternal death in early pregnancy (<20wks)" ///
		6 "6-Pregnancy ends without labor" ///
		7 "7-Conflicting delivery info" ///
		8 "8-Site N/A due to data collection differences"
		
	
	label values PRO_LABOR_MISS plab_2 	
	
	tab PRO_LABOR_MISS, m 
	
	tab OBS_LABOR if OBS_LABOR_DENOM==1, m 
	tab PRO_LABOR if PRO_LABOR_DENOM==1, m 
	
	replace PRO_LABOR_DENOM=0 if PRO_LABOR == 777
	
		replace PRO_LABOR_DENOM = 1 if PRO_LABOR==55 & PRO_LABOR_MISS==7 
		replace OBS_LABOR_DENOM = 1 if OBS_LABOR==55 & OBS_LABOR_MISS==5
	
	tab OBS_LABOR if OBS_LABOR_DENOM==1, m 
	tab PRO_LABOR if PRO_LABOR_DENOM==1, m 
	
	tab PRO_LABOR_MISS PRO_LABOR, m 
	tab OBS_LABOR_MISS OBS_LABOR, m 
	
	tab PRO_LABOR_MISS PRO_LABOR if PRO_LABOR_DENOM==1, m 
	tab OBS_LABOR_MISS OBS_LABOR if OBS_LABOR_DENOM==1, m 
		
	
		
	save "$wrk/PREGEND"	, replace 
	
	
	*tab 
	
	preserve 
	
 		
		keep if PREG_END == 1 
		
		
		order SITE MOMID PREGID MAT_PRETERM_ANY MAT_PRETERM_ANY_MISS MAT_PRETERM_PROV ///
			MAT_PRETERM_PROV_MISS MAT_PRETERM_PROV_IND MAT_PRETERM_SPON ///
			MAT_PRETERM_SPON_IND MAT_PRETERM_CLASS PPROM_OCCUR PPROM_OCCUR_MISS ///
			PREG_END_DELIV MAT_POST_TERM_41_DEL MAT_POST_TERM_42_DEL ///
			PROM MEM_HOURS 

			
		keep SITE MOMID PREGID  MAT_PRETERM_ANY MAT_PRETERM_ANY_MISS MAT_PRETERM_PROV ///
			MAT_PRETERM_PROV_MISS MAT_PRETERM_PROV_IND MAT_PRETERM_SPON ///
			MAT_PRETERM_SPON_IND MAT_PRETERM_CLASS PPROM_OCCUR PPROM_OCCUR_MISS ///
			PREG_END_DELIV MAT_POST_TERM_41_DEL MAT_POST_TERM_42_DEL
			
		foreach var of varlist MAT_PRETERM_ANY MAT_PRETERM_ANY_MISS MAT_PRETERM_PROV ///
			MAT_PRETERM_PROV_MISS MAT_PRETERM_PROV_IND MAT_PRETERM_SPON ///
			MAT_PRETERM_SPON_IND MAT_PRETERM_CLASS PPROM_OCCUR PPROM_OCCUR_MISS /// 
			PREG_END_DELIV MAT_POST_TERM_41_DEL MAT_POST_TERM_42_DEL {
				
		tab `var', m 
	
			}
			
	
		
	save "$OUT/MAT_PRETERM", replace 

	restore 
	
	
	preserve 
 		
		keep if PREG_END == 1 
		
		
		//// Finalize: 
	
		order SITE MOMID PREGID PREG_END_DELIV PRO_LABOR_DENOM PRO_LABOR ///
			PRO_LABOR_MISS OBS_LABOR_DENOM OBS_LABOR OBS_LABOR_MISS LABOR_ANY ///
			LABOR_HOURS MEM_SPON MEM_ART MEM_CES PROM MEM_HOURS ///
			MAT_DELIVERY_CLASS MAT_DELIVERY_PROV MAT_DELIVERY_SPON MAT_DELIVERY_PROV_MISS ///
			MAT_CES_ANY MAT_CES_EMERGENT MAT_CES_PLAN ///
			MAT_INDUCED MAT_INDUCED_TYPE INDUCED_IND_* MAT_INDUCED_MISS

		keep SITE MOMID PREGID PREG_END_DELIV PRO_LABOR_DENOM PRO_LABOR ///
			PRO_LABOR_MISS OBS_LABOR_DENOM OBS_LABOR OBS_LABOR_MISS LABOR_ANY ///
			LABOR_HOURS MEM_SPON MEM_ART MEM_CES PROM MEM_HOURS ///
			MAT_DELIVERY_CLASS MAT_DELIVERY_PROV MAT_DELIVERY_SPON MAT_DELIVERY_PROV_MISS ///
			MAT_CES_ANY MAT_CES_EMERGENT MAT_CES_PLAN ///
			MAT_INDUCED MAT_INDUCED_TYPE INDUCED_IND_* MAT_INDUCED_MISS
			
	foreach var of varlist PREG_END_DELIV PRO_LABOR_DENOM PRO_LABOR ///
			PRO_LABOR_MISS OBS_LABOR_DENOM OBS_LABOR OBS_LABOR_MISS LABOR_ANY ///
			MAT_DELIVERY_CLASS MAT_DELIVERY_PROV MAT_DELIVERY_SPON MAT_DELIVERY_PROV_MISS ///
			MEM_SPON MEM_ART MEM_CES PROM MEM_HOURS ///
			MAT_CES_ANY MAT_CES_EMERGENT MAT_CES_PLAN ///
			MAT_INDUCED MAT_INDUCED_TYPE INDUCED_IND_* MAT_INDUCED_MISS {
				
	tab  `var' PREG_END_DELIV, m 
	
			}
			
	
		
	save "$OUT/MAT_LABOR", replace 

	restore 
	
	preserve 
	
		keep if PREG_END == 1 
		
		*address dates:
		replace MAT_UTER_RUP_PNC_DT = . if MAT_UTER_RUP_PNC != 1
		
		
		//// Finalize: 
	
		order SITE MOMID PREGID MAT_UTER_RUP MAT_UTER_RUP_MISS ///
			MAT_UTER_RUP_IPC MAT_UTER_RUP_HOSP MAT_UTER_RUP_PNC ///
			MAT_UTER_RUP_PNC_DT

		keep SITE MOMID PREGID MAT_UTER_RUP MAT_UTER_RUP_MISS ///
			MAT_UTER_RUP_IPC MAT_UTER_RUP_HOSP MAT_UTER_RUP_PNC ///
			MAT_UTER_RUP_PNC_DT
			
	foreach var of varlist MAT_UTER_RUP MAT_UTER_RUP_MISS ///
			MAT_UTER_RUP_IPC MAT_UTER_RUP_HOSP MAT_UTER_RUP_PNC ///
			MAT_UTER_RUP_PNC_DT  {
				
	tab  `var', m 
	
			}
			
	
		
	save "$OUT/MAT_UTERINERUP", replace 	
	
	restore 
	
	
	
	
	//////////////////////////////////////////////////////////////////////////
	/*Create output reports for suspcious cases: 
	
	*PPROM: 
	tab PPROM_PREGEND_TIMING,m 
	
	gen comment = ""
	
	*cases with PPROM <20 weeks GA 
	list site PPROM_PREGEND PREG_END_GA if PPROM_PREGEND == 1 & PREG_END_GA <(20*7) 
	
	replace comment= "PPROM with gestational age <20 weeks at pregnancy endpoint" ///
		if PPROM_PREGEND == 1 & PREG_END_GA <(20*7) 
		
	*cases with PPROM >7 days before pregnancy endpoint 
	list site PPROM_PREGEND LABOR_MEM_HOURS if PPROM_PREGEND == 1 & ///
		LABOR_MEM_HOURS > 168 & LABOR_MEM_HOURS != . 
	
	replace comment= "PPROM where ROM occurred more than 7 days before pregnancy end date" ///
		if PPROM_PREGEND == 1 & LABOR_MEM_HOURS > 168 & LABOR_MEM_HOURS !=. & comment == ""
		
	*cases with no labor that end in vaginal delivery 
	list site PRETERM_PROV PRETERM_CLASS LABOR_ANY CES_ANY PPROM_PREGEND if PRETERM_CLASS == 23 | ///
		PRETERM_CLASS == 24
		
	replace comment= "No labor but pregnancy ends in vaginal delivery" ///
		if comment =="" & (PRETERM_CLASS == 23 | PRETERM_CLASS == 24)
	
	*cases where ROM is associated with cesarean, but ends in a vaginal delivery
	list site PRETERM_PROV PRETERM_CLASS MEM_CES CES_ANY PPROM_PREGEND if ///
		PRETERM_ANY ==1 & CES_ANY == 0 & MEM_CES == 1 
		
	replace comment= "ROM is cesarean-related but pregnancy ends in a vaginal delivery" if ///
		comment=="" & PRETERM_ANY ==1 & CES_ANY == 0 & MEM_CES == 1 		
		
		
	tab comment site
	
	keep site momid pregid PRETERM_ANY PRETERM_SPON PRETERM_PROV ///
		PPROM_PREGEND comment 
		
	keep if comment != ""
	sort comment 
	
	export excel "$output/review/PPROM_Preterm_Review_$date.xlsx", firstrow(variables)
		
		
		
