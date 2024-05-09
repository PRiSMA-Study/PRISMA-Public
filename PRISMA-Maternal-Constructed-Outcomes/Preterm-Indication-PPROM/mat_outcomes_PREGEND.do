*PRISMA Maternal Variable Construction Code
*Purpose: This code drafts variable construction code for maternal outcome
	*variables for the PRISMA study. This file focuses on outcomes collected 
	*at the endpoint of pregnancy.
*Original Version: March 6, 2024 by E Oakley (emoakley@gwu.edu)
*Update: March 25, 2024 by E Oakley (incorporate feedback from Dr. Wiley)
*Update: April 1, 2024 by E Oakley (incorporate feedback from Dr. Wiley)
*Update: April 8, 2024 by E Oakley (incorporate feedback from PRISMA sites)

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
global da "Z:/Stacked Data/2024-04-19" // update date as needed

	// Working Files Folder (TNT-Drive)
global wrk "Z:/Erin_working_files/data" // set pathway here for where you want to save output data files (i.e., constructed analysis variables)

global date "240422" // today's date

log using "$log/mat_outcome_construct_preg_end_$date", replace

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
			
		
		*Spontaneous preterm indication 
			
		
	*Preterm premature rupture of membranes - PPROM_PREGEND 	
		Rupture of membranes before the onset of labor, occurring before <37 
			weeks of gestation OR clinical diagnosis of premature rupture of
			membranes.			
		denominator: Among all completed pregnancies (PREG_END==1)
		format: 1, Yes; 0, No; 55, Missing	
		
*/

		
//////////////////////////////////////////

	*MNH09: 
	import delimited "$da/mnh09_merged"
	
	tab site, m 
	
	*clean up dataset: 
	keep if site == "Ghana" | site == "India-CMC" | site == "Kenya" | ///
		site == "Pakistan" | site == "Zambia" | site == "India-SAS"
		
	format momid %38s
	recast str38 momid, force
	
	*review of potential input variables:
	tab m09_ptb_mhoccur m09_hdp_htn_mhoccur_3, m 
	tab m09_labor_mhoccur m09_ptb_mhoccur, m 
	
	*Components: 
	
	// LABOR_ANY Did the participant experience labor: 
	gen LABOR_ANY = 0 if m09_labor_mhoccur == 0 
	replace LABOR_ANY = 1 if m09_labor_mhoccur == 1
	replace LABOR_ANY = 55 if m09_labor_mhoccur >= 77 
	label var LABOR_ANY "=1 if participant experienced labor"
	
	// LABOR_INDUCED 
	gen LABOR_INDUCED = 0 if m09_induced_proccur == 0 
	replace LABOR_INDUCED = 1 if m09_induced_proccur == 1
	replace LABOR_INDUCED = 55 if m09_induced_proccur >= 77 
	label var LABOR_INDUCED "=1 if labor was induced"
	
	// LABOR_SPON
	gen LABOR_SPON = 0 if m09_induced_proccur == 1 // 0 if induced
	replace LABOR_SPON = 0 if m09_labor_mhoccur == 0 // 0 if no labor
	replace LABOR_SPON = 1 if m09_induced_proccur == 0 & ///
		m09_labor_mhoccur == 1 // 1 if labor occurred & not induced
	replace LABOR_SPON = 55 if m09_induced_proccur >=77 | m09_labor_mhoccur >=77
	label var LABOR_SPON "=1 if labor was spontaneous"	
	
	tab LABOR_INDUCED LABOR_SPON, m 
	
	// CES_ANY
	gen CES_ANY = 0 if m09_deliv_prroute_inf1 == 1 // 1=vaginal delivery 
	replace CES_ANY = 1 if m09_deliv_prroute_inf1 == 2 // 2=cesarean delivery
	replace CES_ANY = 0 if m09_deliv_prroute_inf1 == 3 // 3=death prior to del
	replace CES_ANY = 55 if m09_deliv_prroute_inf1 >= 77 // missing
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
		m09_ces_proccur_inf1 == 99) & (m09_ces_faorres_inf2 != 1 & ///
		m09_ces_faorres_inf2 != 2 & m09_ces_faorres_inf3 != 1 & ///
		m09_ces_faorres_inf3 != 2 & m09_ces_faorres_inf4 != 1 & ///
		m09_ces_faorres_inf4 != 2) // missing information on classification
		
	label var CES_PLAN "=1 if planned cesarean delivery (all infants)"
	
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
		m09_ces_proccur_inf1 == 99) & (m09_ces_faorres_inf2 != 1 & ///
		m09_ces_faorres_inf2 != 2 & m09_ces_faorres_inf3 != 1 & ///
		m09_ces_faorres_inf3 != 2 & m09_ces_faorres_inf4 != 1 & ///
		m09_ces_faorres_inf4 != 2) // missing information on classification
		
	label var CES_EMERGENT "=1 if emergent cesarean delivery (any infant)"
		
	// MEM_SPON 
	gen MEM_SPON = 1 if m09_membrane_rupt_mhterm == 1 // spontaneous rupture
	replace MEM_SPON = 0 if m09_membrane_rupt_mhterm == 2 // induced rupture 
	replace MEM_SPON = 0 if m09_membrane_rupt_mhterm == 77 // cesarean bef rupture 
	replace MEM_SPON = 55 if m09_membrane_rupt_mhterm == 55  | ///
		m09_membrane_rupt_mhterm == . | m09_membrane_rupt_mhterm == 99 // unknown/missing 
	label var MEM_SPON "=1 if spontaneous rupture of membranes"
	
	// MEM_ART 
	gen MEM_ART = 1 if m09_membrane_rupt_mhterm == 2 // induced rupture 
	replace MEM_ART = 0 if m09_membrane_rupt_mhterm == 1 // spontaneous rupture 
	replace MEM_ART = 0 if m09_membrane_rupt_mhterm == 77 // cesarean bef rupture 
	replace MEM_ART = 55 if m09_membrane_rupt_mhterm == 55 | ///
		m09_membrane_rupt_mhterm == . | m09_membrane_rupt_mhterm == 99 // unknown/missing 
	label var MEM_ART "=1 if artificial rupture of membranes"
	
	tab MEM_ART MEM_SPON, m 
	
	// MEM_CES 
	gen MEM_CES = 0 if MEM_SPON == 1 | MEM_ART == 1 // spontaenous or induced 
	replace MEM_CES = 1 if m09_membrane_rupt_mhterm == 77 // cesarean bef rupture 
	replace MEM_CES = 55 if m09_membrane_rupt_mhterm == 55 | ///
		m09_membrane_rupt_mhterm == . | m09_membrane_rupt_mhterm == 99 // unknown/missing 
	label var MEM_CES "=1 if rupture of membranes c-section related"
	
	tab MEM_CES, m 
	
	// PREG_END_DATE
	*Date of pregnancy outcome (take first infant): 
	gen PREG_END_DATE = date(m09_deliv_dsstdat_inf1, "YMD") if ///
		m09_deliv_dsstdat_inf1 != "1907-07-07"
	format PREG_END_DATE %td
	label var PREG_END_DATE "Date of pregnancy outcome (1st infant)"
	sum PREG_END_DATE, format 

	// Merge in BOE data: 
	merge 1:1 momid pregid using "$wrk/BOE.dta", keepusing(EST_CONCEP_DATE EDD_BOE ENROLL)
	
	tab _merge site, m 
	
	gen PREG_END = 0 
	replace PREG_END = 1 if _merge ==3 | _merge ==1 
	label var PREG_END "Completed pregnancies (with MNH09 completed)"
	
	*** TEMPORARY MEASURE: keep only if in BOE dataset AND L&D form:
	keep if _merge == 3
	
		
*Preterm birth classification (PRETERM_ANY)
	
	gen PREG_END_GA = PREG_END_DATE - EST_CONCEP_DATE
	label var PREG_END_GA "GA at pregnancy endpoint (days)"
	
	sum PREG_END_GA
	
	*Check on pregnancies by GA at endpoint: are there any sites filling MNH09 
	*for pregnancies <20 weeks GA?
	twoway histogram PREG_END_GA, w(1) by(site) ///
	xline(140) color(black) xsize(20) ysize(10)
	
	
	gen PREG_LOSS = 0 if PREG_END_GA >=140 & PREG_END_GA !=.
	replace PREG_LOSS = 1 if PREG_END_GA < 140 & PREG_END_GA >=0
	replace PREG_LOSS = 0 if PREG_END_GA == . // we will consider this to be "0" for table output 
	label var PREG_LOSS "Pregnancy endpoint <20 weeks GA"
	
	tab PREG_LOSS
	
	gen PRETERM_ANY = 0 if PREG_END_GA >= 259 & PREG_END_GA != .
	replace PRETERM_ANY = 1 if PREG_END_GA < 259 & PREG_END_GA >= 140
	replace PRETERM_ANY = 0 if PREG_LOSS == 1 // remove pregnancies <20 weeks GA
	replace PRETERM_ANY = 55 if PREG_END_GA == . | PREG_END_GA <0 
	label var PRETERM_ANY "Preterm pregnancy endpoint (by any classification)"
	
	*Check: 
	tab PREG_END_GA PRETERM_ANY, m 
	tab PREG_END_GA PREG_LOSS, m 
	
	*Create "missing" indicator for the maternal outcomes report:
	gen PRETERM_ANY_MISS = 0 if PRETERM_ANY != 55
	
	replace PRETERM_ANY_MISS = 1 if PRETERM_ANY == 55 & EST_CONCEP_DATE ==.
	replace PRETERM_ANY_MISS = 2 if PRETERM_ANY == 55 & PREG_END_DATE ==.
	replace PRETERM_ANY_MISS = 3 if PRETERM_ANY == 55 & EST_CONCEP_DATE == . & ///
		PREG_END_DATE ==.
	
	tab PRETERM_ANY_MISS, m 
	
	label define pt_any_miss 0 "0-Non-missing" 1 "1=Missing BOE" ///
		2 "2-Missing end date" 3 "3-Missing BOE and end date" 
	label values PRETERM_ANY_MISS pt_any_miss 
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
		
*/

///////////////////////////////////////////////////////
*Preterm classification typologies variable:
	
	gen PRETERM_CLASS = 0 if PRETERM_ANY == 1 
	label var PRETERM_CLASS "Preterm classification typologies"

//////////////////////////////////////////////////////
*Preterm birth classification - spontaneous (PRETERM_SPON)

	gen PRETERM_SPON = 0 if PRETERM_ANY == 0 
	replace PRETERM_SPON = 55 if PRETERM_ANY == 55 
	
	////////////////
	// SPONTANEOUS: 
	// preterm + spontaneous ROM prior to onset of labor: 
	replace PRETERM_SPON = 1 if PRETERM_ANY == 1 & ///
		MEM_SPON == 1 & MEM_FIRST == 1
	
	replace PRETERM_CLASS = 1 if PRETERM_ANY == 1 & ///
		MEM_SPON == 1 & MEM_FIRST == 1 
	
	// preterm + spontaneous labor prior to ROM
	replace PRETERM_SPON = 1 if PRETERM_ANY == 1 & ///
		LABOR_SPON == 1 & LABOR_FIRST == 1
		
	replace PRETERM_CLASS = 2 if  PRETERM_ANY == 1 & ///
		LABOR_SPON == 1 & LABOR_FIRST == 1
		
	// preterm + spontaneous labor with ROM at C-section & Cesarean delivery
	replace PRETERM_SPON = 1 if PRETERM_ANY == 1 & ///
		LABOR_SPON == 1 & MEM_CES == 1 & MEM_FIRST == . & CES_ANY == 1
		
	replace PRETERM_CLASS = 3 if PRETERM_ANY == 1 & ///
		LABOR_SPON == 1 & MEM_CES == 1 & MEM_FIRST == . & CES_ANY == 1	
		
	// preterm + Spontaneous ROM & no labor (c-section)
	replace PRETERM_SPON = 1 if PRETERM_ANY == 1 & ///
		MEM_SPON == 1 & LABOR_ANY == 0 & CES_ANY == 1 & MEM_FIRST == . 
		
	replace PRETERM_CLASS = 4 if PRETERM_ANY == 1 & ///
		MEM_SPON == 1 & LABOR_ANY == 0 & CES_ANY == 1 & MEM_FIRST == . 
		
	// preterm + Spontaneous ROM & Spontaneous Labor (timing is missing)
	replace PRETERM_SPON = 1 if PRETERM_ANY == 1 & ///
		MEM_SPON == 1 & LABOR_ANY == 1 & LABOR_SPON == 1 & MEM_FIRST == . 
		
	replace PRETERM_CLASS = 5 if PRETERM_ANY == 1 & ///
		MEM_SPON == 1 & LABOR_ANY == 1 & LABOR_SPON == 1 & MEM_FIRST == . 
		
	tab PRETERM_CLASS PRETERM_SPON, m 
	
	///////////////////////
	// PROVIDER-INITIATED: 
	// 	preterm + induced labor prior to ROM		
	replace PRETERM_SPON = 0 if PRETERM_ANY == 1 & ///
		LABOR_INDUCED == 1 & LABOR_FIRST == 1 
		
	replace PRETERM_CLASS = 11 if PRETERM_ANY == 1 & ///
		LABOR_INDUCED == 1 & LABOR_FIRST == 1 
		
	// 	preterm + artificial ROM prior to labor		
	replace PRETERM_SPON = 0 if PRETERM_ANY == 1 & ///
		MEM_ART == 1 & MEM_FIRST == 1 
		
	replace PRETERM_CLASS = 12 if PRETERM_ANY == 1 & ///
		MEM_ART == 1 & MEM_FIRST == 1 
		
	// preterm + cesarean with no labor or spontaneous ROM 
	replace PRETERM_SPON = 0 if PRETERM_ANY == 1 & ///
		CES_ANY == 1 & LABOR_ANY== 0 & (MEM_CES == 1 | MEM_ART == 1)
		
	replace PRETERM_CLASS = 13 if PRETERM_ANY == 1 & ///
		CES_ANY == 1 & LABOR_ANY== 0 & (MEM_CES == 1 | MEM_ART == 1)
		
	// preterm + cesarean with induced labor - cesarean-related ROM 
	replace PRETERM_SPON = 0 if PRETERM_ANY == 1 & ///
		CES_ANY == 1 & LABOR_INDUCED == 1 & LABOR_ANY == 1 & ///
		MEM_CES == 1 & LABOR_FIRST == . 
		
	replace PRETERM_CLASS = 14 if PRETERM_ANY == 1 & ///
		CES_ANY == 1 & LABOR_INDUCED == 1 & LABOR_ANY == 1 & ///
		MEM_CES == 1 & LABOR_FIRST == . 
		
	// preterm + cesarean with induced labor + artificial ROM 
	replace PRETERM_SPON = 0 if PRETERM_ANY == 1 & ///
		LABOR_INDUCED == 1 & MEM_ART == 1 & LABOR_ANY == 0 & CES_ANY == 0
		
	replace PRETERM_CLASS = 15 if PRETERM_ANY == 1 & ///
		LABOR_INDUCED == 1 & MEM_ART == 1 & LABOR_ANY == 0 & CES_ANY == 0
		
	tab PRETERM_CLASS PRETERM_SPON, m

	///////////////////////
	// UNKNOWN: 
	// missing information - Labor 1st and Labor Type Missing: 
	replace PRETERM_SPON = 55 if PRETERM_ANY == 1 & ///
		(LABOR_SPON == 55 & LABOR_FIRST==1 & CES_PLAN == 0)
		
	replace PRETERM_CLASS = 21 if PRETERM_ANY == 1 & ///
		(LABOR_SPON == 55 & LABOR_FIRST==1 & CES_PLAN == 0)
		 
	// missing information - MEM_CES=1 but not a cesarean delivery: 
	replace PRETERM_SPON = 55 if PRETERM_ANY == 1 & ///
		MEM_CES == 1 & CES_ANY == 0 & LABOR_FIRST==.
		
	replace PRETERM_CLASS = 22 if PRETERM_ANY == 1 & ///
		MEM_CES == 1 & CES_ANY == 0 & LABOR_FIRST==.
		
	// missing information - Spontaneous ROM - No labor - vaginal delivery 
	replace PRETERM_SPON = 55 if PRETERM_ANY == 1 & ///
		LABOR_ANY == 0 & CES_ANY == 0 & MEM_SPON == 1 & MEM_FIRST == . 
		
	replace PRETERM_CLASS = 23 if PRETERM_ANY == 1 & ///
		LABOR_ANY == 0 & CES_ANY == 0 & MEM_SPON == 1 & MEM_FIRST == . 
		
	// missing information - Artificial ROM - No labor / no induction - vaginal delivery 
	replace PRETERM_SPON = 55 if PRETERM_ANY == 1 & ///
		LABOR_ANY == 0 & LABOR_INDUCED == 0 & CES_ANY == 0 & ///
		MEM_ART == 1 & MEM_FIRST == . 
		
	replace PRETERM_CLASS = 24 if PRETERM_ANY == 1 & ///
		LABOR_ANY == 0 & LABOR_INDUCED == 0 & CES_ANY == 0 & ///
		MEM_ART == 1 & MEM_FIRST == . 
		
	// missing information - Unknown if the woman labored
	replace PRETERM_SPON = 55 if PRETERM_ANY == 1 & ///
		LABOR_ANY == 55 
		
	replace PRETERM_CLASS = 25 if PRETERM_ANY == 1 & ///
		LABOR_ANY == 55 
		
	// missing information - Unknown rupture of membranes
	replace PRETERM_SPON = 55 if PRETERM_ANY == 1 & ///
		MEM_SPON == 55  & LABOR_ANY == 1 & CES_ANY == 0 
		
	replace PRETERM_CLASS = 26 if PRETERM_ANY == 1 & ///
		MEM_SPON == 55  & LABOR_ANY == 1 & CES_ANY == 0 
	
	
	tab PRETERM_CLASS PRETERM_SPON, m
		
	label define ptbtypes 1 "Spontaneous ROM prior to labor" ///
		2 "Spontaneous labor prior to ROM" ///
		3 "Spontaneous labor - ROM at cesarean delivery" ///
		4 "Spontaneous ROM - no labor - cesarean delivery" ///
		5 "Spontaneous ROM & labor - missing timing" ///
		11 "Induced labor prior to ROM" ///
		12 "Artificial ROM prior to labor" ///
		13 "Cesarean with no labor or spontaneous ROM" ///
		14 "Induced with no labor - cesarean delivery" ///
		15 "Induced with no labor - artifical ROM - cesarean delivery" ///
		21 "Missing info: Labor occurred first - missing labor type" ///
		22 "Missing info: ROM cesarean-related - vaginal delivery" ///
		23 "Missing info: No labor - vaginal delivery" ///
		24 "Missing info: Artificial ROM - no labor - vaginal delivery" ///
		25 "Missing info: Unknown if labor" ///
		26 "Missing info: Unknown if spontaneous ROM"
		
	label values PRETERM_CLASS ptbtypes 
	
	label var PRETERM_SPON "Preterm birth classification - spontaneous"
	
	tab PRETERM_CLASS PRETERM_SPON, m 
		
	*Check on missing data:
	list MEM_SPON MEM_ART MEM_CES LABOR_SPON LABOR_INDUCED LABOR_ANY ///
		CES_ANY CES_EMERGENT CES_PLAN MEM_FIRST LABOR_FIRST PREG_LOSS ///
		if PRETERM_SPON == . 
	

*Preterm birth classification - provider-initiated (PRETERM_PROV)

	gen PRETERM_PROV = 0 if PRETERM_ANY == 0 
	replace PRETERM_PROV = 55 if PRETERM_ANY == 55 | PRETERM_SPON == 55
	
	replace PRETERM_PROV = 0 if PRETERM_SPON == 1 
	
	replace PRETERM_PROV = 1 if PRETERM_SPON == 0 & ///
		(PRETERM_CLASS == 11 | PRETERM_CLASS == 12 | PRETERM_CLASS == 13 | ///
		 PRETERM_CLASS == 14 | PRETERM_CLASS == 15)
		 
		
	label var PRETERM_PROV "Preterm birth classification - provider-initiated"
	

	*CHECKS:
	tab PRETERM_ANY if PREG_LOSS == 0, m 
	tab PRETERM_PROV PRETERM_SPON, m 
	tab PRETERM_CLASS PRETERM_PROV, m 
	tab PRETERM_CLASS PRETERM_SPON, m 
	
	tab PRETERM_CLASS if PRETERM_ANY ==1, m 
	tab PRETERM_SPON if PRETERM_ANY == 1, m 
	tab PRETERM_PROV if PRETERM_ANY == 1, m 
	
	*Create a missing indicator:
	gen PRETERM_PROV_MISS = 0 if PRETERM_PROV != 55
	*same as PRETERM_ANY_MISS: 
	replace PRETERM_PROV_MISS = 1 if PRETERM_PROV == 55 & PRETERM_ANY_MISS == 1 
	replace PRETERM_PROV_MISS = 2 if PRETERM_PROV == 55 & PRETERM_ANY_MISS == 2
	replace PRETERM_PROV_MISS = 3 if PRETERM_PROV == 55 & PRETERM_ANY_MISS == 3
	*other missing beyond PRETERM_ANY_MISS:
	replace PRETERM_PROV_MISS = 4 if PRETERM_PROV == 55 & PRETERM_CLASS >= 20 & ///
		PRETERM_CLASS <29
	
	label define pt_prov_miss 0 "0-Non-missing" 1 "1=Missing BOE" ///
		2 "2-Missing end date" 3 "3-Missing BOE and end date" ///
		4 "4-Missing labor/ROM info-MNH09"
	label values PRETERM_PROV_MISS pt_prov_miss 
	label var PRETERM_PROV_MISS "Reason missing - PRETERM PROV"

	tab PRETERM_PROV_MISS PRETERM_PROV, m 
	
		
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
	PRETERM_ANY if PRETERM_ANY !=55 & PREG_LOSS == 0 ///
	, over(site_num) bar format(%9.2f) ci n xlab($pct) ///
	legend(c(4)) ysize(17) xsize(43)
	
	graph export "$output/PRETERM_ANY_$date.jpg", replace 

		
	label var PRETERM_PROV "Provider-initiated Preterm (among all preterm)"
	global pct2 `" 0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%"  "'	
	
	betterbar ///
	PRETERM_PROV if PRETERM_ANY == 1 & PRETERM_PROV != 55 ///
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
	
		// 55=Unknown
	replace PRETERM_SPON_IND = 55 if PRETERM_CLASS == 5
	
	label define sponind 1 "Preterm labor" 2 "PPROM" 55 "Unknown"
	
	label values PRETERM_SPON_IND sponind 
		 
	tab PRETERM_SPON_IND, m 
		 
*Preterm premature rupture of membranes - PPROM (PPROM_OCCUR)
	
	*Generate GA at membrane rupture: 
	gen MEM_GA = dofc(MEM_DTT) - EST_CONCEP_DATE 
	tab MEM_GA, m 
	
	gen MEM_GA_WK = MEM_GA / 7 
	tab MEM_GA_WK, m 
	
	tab MEM_GA_WK PREG_LOSS, m 

	*PPROM_PREGEND
	gen PPROM_PREGEND = .
	label var PPROM_PREGEND "PPROM at L&D (MNH09)"
	
	// Unknown PPROM if unknown membrane rupture method or timing: 
	replace PPROM_PREGEND = 55 if MEM_SPON == 55 | (MEM_SPON == 1 & MEM_DTT == .) 
	
	// Unknown PPROM if Spontaneous ROM between 20 & 37 weeks, but unknown if labored  
	replace PPROM_PREGEND = 55 if MEM_SPON == 1 & LABOR_ANY == 55 & ///
		MEM_GA_WK < 37 & MEM_GA_WK != . 
	
	// NOT PPROM if labor occurs first: 
	replace PPROM_PREGEND = 0 if LABOR_FIRST == 1
	
	// NOT PPROM if artificial rupture of membranes / no rupture before cesarean 
	replace PPROM_PREGEND = 0 if (MEM_ART == 1 | MEM_CES == 1)
	
	// NOT PPROM if spontaneous ROM after 37 weeks GA:
	replace PPROM_PREGEND = 0 if MEM_SPON == 1 & MEM_GA_WK >= 37 & MEM_GA_WK !=.
	
	// PPROM if Spontaneous ROM prior to labor onset: 
	replace PPROM_PREGEND = 1 if MEM_SPON == 1 & MEM_FIRST == 1 & ///
		MEM_GA_WK < 37 & MEM_GA_WK != . 
		
	// PPROM if Spontaneous ROM prior to a cesarean delivery (no labor): 
	replace PPROM_PREGEND = 1 if MEM_SPON == 1 & MEM_FIRST == . & ///
		LABOR_ANY == 0 & CES_ANY == 1 & MEM_GA_WK < 37 & MEM_GA_WK !=.
		
	// PPROM if Spontaneous ROM & no labor (no cesarean -- most suspicious, but includes some instances of pregnancy loss <20wks): 
	replace PPROM_PREGEND = 1 if MEM_SPON == 1 & MEM_FIRST == . & ///
		LABOR_ANY == 0 & CES_ANY == 0 & MEM_GA_WK < 37 & MEM_GA_WK !=.
		
	tab PPROM_PREGEND, m 
	
	list PPROM_PREGEND MEM_SPON MEM_DTT MEM_GA_WK MEM_FIRST LABOR_ANY ///
		LABOR_INDUCED LABOR_DTT PREG_END_DATE CES_ANY if PPROM_PREGEND == . 
	
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
		LABOR_ANY == 0 & LABOR_INDUCED == 0 & CES_ANY == 1 
		
	replace PPROM_PREGEND_TIMING = 99 if PPROM_PREGEND == 1 & ///
		LABOR_ANY == 0 & LABOR_INDUCED == 0 & CES_ANY == 0 	
		
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
	
	*Create missing reason indicator for maternal outcomes report: 
	gen PPROM_PREGEND_MISS = 0 if PPROM_PREGEND != 55 
	
	// Unknown PPROM if unknown membrane rupture method: 
	replace PPROM_PREGEND_MISS = 1 if MEM_SPON == 55
	
	// Unknown PPROM if spontaenous ROM with missing date/time: 
	replace PPROM_PREGEND_MISS = 2 if MEM_SPON == 1 & MEM_DTT == . 
	
	// Unknown PPROM if Spontaneous ROM <37 weeks, but unknown if labored
	replace PPROM_PREGEND_MISS = 3 if MEM_SPON == 1 & LABOR_ANY == 55 & ///
		(MEM_GA_WK < 37 & MEM_GA_WK != .)
	
	label var PPROM_PREGEND_MISS "Reason missing - PPROM PREGEND"
	
	label define pprom_miss 0 "0-Non-missing" 1 "1-Type of ROM" 2 "2-Timing of ROM" ///
		3 "3-Labor info"
	label values PPROM_PREGEND_MISS pprom_miss 
	
	tab PPROM_PREGEND_MISS PPROM_PREGEND, m 
	
	*review for consistency: 
	tab PRETERM_SPON_IND PPROM_PREGEND, m 
	tab PPROM_PREGEND_TIMING PRETERM_SPON_IND,m 

	///////////////////////////////////////////////////////////////////////////
	** MAKE A BAR GRAPH **
	///////////////////////////////////////////////////////////////////////////
	
	global pct3 `" 0 "0%" .01 "1%" .02 "2%" .03 "3%" .04 "4%" .05 "5%" "'	
  
	betterbar ///
	PPROM_PREGEND if PPROM_PREGEND !=55 ///
	, over(site_num) bar format(%9.2f) n xlab($pct3) ///
	legend(c(4)) ysize(17) xsize(40)
	
	graph export "$output/PPROM_PREGEND_$date.jpg", replace
	
	///////////////////////////////////////////////////////////////////////////
	** END MAKE A BAR GRAPH **
	///////////////////////////////////////////////////////////////////////////
		
	stop 
	
	*data checks for re-running the code on new data:
	
	tab PREG_END_GA PRETERM_ANY, m 
	tab PRETERM_ANY site, m 
	
	tab PREG_END_GA PREG_LOSS, m 
	tab PREG_LOSS site, m 	
	
	tab PRETERM_PROV PRETERM_SPON, m 
	
	tab PRETERM_PROV_IND PRETERM_PROV, m 
	tab PRETERM_SPON_IND PRETERM_SPON, m 
	
	tab PRETERM_PROV_MISS PRETERM_ANY, m 
	
	
	
	//////////////////////////////////////////////////////////////////////////
	** FINALIZE ANALYSIS DATASET **
	//////////////////////////////////////////////////////////////////////////
	
	*export an analysis data set:
	order site momid pregid PREG_LOSS PREG_END_DATE PREG_END_GA PREG_END ///
		PRETERM_ANY PRETERM_ANY_MISS PRETERM_PROV PRETERM_PROV_MISS ///
		PRETERM_SPON PRETERM_CLASS PPROM_PREGEND PPROM_PREGEND_MISS ///
		PPROM_PREGEND_TIMING PRETERM_PROV_IND PRETERM_SPON_IND ///
		LABOR_MEM_HOURS LABOR_ANY LABOR_INDUCED LABOR_SPON ///
		MEM_SPON MEM_ART MEM_CES CES_ANY CES_EMERGENT CES_PLAN
	
	keep site momid pregid PREG_LOSS PREG_END_DATE PREG_END_GA PREG_END ///
		PRETERM_ANY PRETERM_ANY_MISS PRETERM_PROV PRETERM_PROV_MISS ///
		PRETERM_SPON PRETERM_CLASS PPROM_PREGEND PPROM_PREGEND_MISS ///
		PPROM_PREGEND_TIMING PRETERM_PROV_IND PRETERM_SPON_IND ///
		LABOR_MEM_HOURS LABOR_ANY LABOR_INDUCED LABOR_SPON ///
		MEM_SPON MEM_ART MEM_CES CES_ANY CES_EMERGENT CES_PLAN
		 
	save "$wrk/maternal_outcomes_MNH09", replace 
	
	tab 
	
	
	//////////////////////////////////////////////////////////////////////////
	*Create output reports for suspcious cases: 
	
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
		
		
		
