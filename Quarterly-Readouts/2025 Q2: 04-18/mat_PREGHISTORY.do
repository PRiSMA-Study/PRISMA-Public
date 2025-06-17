*PRISMA Variable Construction - Pregnancy history
*Purpose: This code constructs obstetric history/history of pregnancy 
*complications outcome, as requested by IHME
*Original Version: October 25, 2024
*Update: January 15, 2024 (update to dates for the 1-10-2025 data)

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

global date "250428" // today's date

log using "$log/construct_previous_pregcomplications_$date-v2", replace

/*************************************************************************

*This file constructs the following variables: 
	
	MAT_PREVPREG_PPH
	
	MAT_PREVPREG_APH
	
	MAT_PREVPREG_PRETERM 
	
	MAT_PREVPREG_POSTTERM
	
	MAT_PREVPREG_GHTN
	
	MAT_PREVPREG_PREEC
	
	MAT_PREVPREG_GDM 
	
	MAT_PREVPREG_PROM
	
	MAT_PREVPREG_PROLABOR
	
	MAT_PREVPREG_CES_PLAN 
	
	MAT_PREVPREG_CES_UNPLAN 
	
	MAT_PREVPREG_CES_ANY 
	
	MAT_PREVPREG_IUGR 
	
	MAT_PREVPREG_LBW 
	
	MAT_PREVPREG_MACRO
	
	MAT_PREVPREG_OLIGO
	
	MAT_PREVPREG_ANOMALY
			
*/
	
	/////////////////////////////////////////
	
	* Import data: 
	
	import delimited "$da/mnh04_merged", varn(1) case(preserve) bindquote(strict)
	
	rename M04_* *
	
	tab PH_PREV_RPORRES, m 
	
	*previous PPH
	gen MAT_PREVPREG_PPH = 77 if PH_PREV_RPORRES == 0 
	replace MAT_PREVPREG_PPH = 0 if PPH_RPORRES == 0 & PH_PREV_RPORRES != 0 
	replace MAT_PREVPREG_PPH = 1 if PPH_RPORRES == 1 & PH_PREV_RPORRES != 0 
	replace MAT_PREVPREG_PPH = 55 if (PPH_RPORRES == 55 | PPH_RPORRES == 99) & ///
		MAT_PREVPREG_PPH ==.
	replace MAT_PREVPREG_PPH = 55 if PH_PREV_RPORRES == 1 & PPH_RPORRES == 77
		
	tab MAT_PREVPREG_PPH PH_PREV_RPORRES, m 
	tab MAT_PREVPREG_PPH PPH_RPORRES, m 
	
	label var MAT_PREVPREG_PPH "Reported PPH in previous pregnancy"
	
	*previous APH 
	gen MAT_PREVPREG_APH = 77 if PH_PREV_RPORRES == 0 
	replace MAT_PREVPREG_APH = 0 if APH_RPORRES == 0 & PH_PREV_RPORRES != 0 
	replace MAT_PREVPREG_APH = 1 if APH_RPORRES == 1 & PH_PREV_RPORRES != 0 
	replace MAT_PREVPREG_APH = 55 if (APH_RPORRES == 55 | APH_RPORRES == 99) & ///
		MAT_PREVPREG_APH ==.
	replace MAT_PREVPREG_APH = 55 if PH_PREV_RPORRES == 1 & APH_RPORRES == 77
		
	tab MAT_PREVPREG_APH PH_PREV_RPORRES, m 
	tab MAT_PREVPREG_APH APH_RPORRES, m 
	
	label var MAT_PREVPREG_APH "Reported APH in previous pregnancy"
	
	*previous complications loop: 
	
	foreach let in PRETERM POSTTERM GEST_HTN PREECLAMPSIA GEST_DIAB ///
		PREMATURE_RUPTURE OBSTR_LABOR INTERUTER_GROWTH LOWBIRTHWT ///
		MACROSOMIA OLIGOHYDRAMNIOS {
	
	gen MAT_PREVPREG_`let' = 77 if PH_PREV_RPORRES == 0 
	
	replace MAT_PREVPREG_`let' = 0 if `let'_RPORRES == 0 & PH_PREV_RPORRES != 0 
	replace MAT_PREVPREG_`let' = 1 if `let'_RPORRES == 1 & PH_PREV_RPORRES != 0 
	
	replace MAT_PREVPREG_`let' = 55 if (`let'_RPORRES == 55 | ///
		`let'_RPORRES == 77 | `let'_RPORRES == 99) & PH_PREV_RPORRES ==1
	
		}
		
	rename MAT_PREVPREG_GEST_HTN MAT_PREVPREG_GHTN 
	rename MAT_PREVPREG_PREECLAMPSIA MAT_PREVPREG_PREEC 
	rename MAT_PREVPREG_GEST_DIAB MAT_PREVPREG_GDM
	rename MAT_PREVPREG_PREMATURE_RUPTURE MAT_PREVPREG_PROM
	rename MAT_PREVPREG_OBSTR_LABOR MAT_PREVPREG_PROLABOR
	rename MAT_PREVPREG_INTERUTER_GROWTH MAT_PREVPREG_IUGR
	rename MAT_PREVPREG_LOWBIRTHWT MAT_PREVPREG_LBW
	rename MAT_PREVPREG_MACROSOMIA MAT_PREVPREG_MACRO
	rename MAT_PREVPREG_OLIGOHYDRAMNIOS MAT_PREVPREG_OLIGO
	
	label var MAT_PREVPREG_PRETERM "Preterm delivery in previous pregnancy"
	label var MAT_PREVPREG_POSTTERM "Postterm delivery in previous pregnancy"
	label var MAT_PREVPREG_GHTN "GHTN in previous pregnancy"
	label var MAT_PREVPREG_PREEC "Preeclampsia/eclampsia in previous pregnancy"
	label var MAT_PREVPREG_GDM "Gestational diabetes in previous pregnancy"
	label var MAT_PREVPREG_PROM "PROM in previous pregnancy"
	label var MAT_PREVPREG_PROLABOR "Prolonged or obstructed labor in previous pregnancy"
	label var MAT_PREVPREG_IUGR "Intrauterine growth restriction in previous pregnancy"
	label var MAT_PREVPREG_LBW "Low birthweight baby in previous pregnancy"
	label var MAT_PREVPREG_MACRO "Macrosomia in previous pregnancy"
	label var MAT_PREVPREG_OLIGO "Oligohydramnios in previous pregnancy"
	
	*fetal anomalies: 
	tab MALFORM_MHOCCUR, m 
	
	gen MAT_PREVPREG_ANOMALY = 77 if PH_PREV_RPORRES == 0 
	replace MAT_PREVPREG_ANOMALY = 0 if MALFORM_MHOCCUR==0 & PH_PREV_RPORRES !=0 
	replace MAT_PREVPREG_ANOMALY = 1 if MALFORM_MHOCCUR==1 & PH_PREV_RPORRES !=0 
	replace MAT_PREVPREG_ANOMALY = 55 if PH_PREV_RPORRES == 1 & ///
		(MALFORM_MHOCCUR == 55 | MALFORM_MHOCCUR == 77 | MALFORM_MHOCCUR == 99)
		
	label var MAT_PREVPREG_ANOMALY "Baby with fetal anomaly in previous pregnancy"
		
	*history of unplanned c-section: 
	tab UNPL_CESARIAN_PROCCUR, m 
	
	gen MAT_PREVPREG_CES_UNPLAN = 77 if PH_PREV_RPORRES == 0 
	replace MAT_PREVPREG_CES_UNPLAN  = 0 if UNPL_CESARIAN_PROCCUR == 0 & ///
		PH_PREV_RPORRES != 0 
	replace MAT_PREVPREG_CES_UNPLAN  = 1 if UNPL_CESARIAN_PROCCUR == 1 & ///
		PH_PREV_RPORRES != 0 
	replace MAT_PREVPREG_CES_UNPLAN = 55 if PH_PREV_RPORRES == 1 & ///
		(UNPL_CESARIAN_PROCCUR == 55 | UNPL_CESARIAN_PROCCUR == 77 | ///
		 UNPL_CESARIAN_PROCCUR == 99 | UNPL_CESARIAN_PROCCUR == .)
		 
	label var MAT_PREVPREG_CES_UNPLAN "History of unplanned c-section(s)"
	
	
	* Updates to c-section variables implemented on 1-15-2024: 
	
	*history of planned c-section: 
	tab PL_CESARIAN_PROCCUR, m 
	
	gen MAT_PREVPREG_CES_PLAN  = 77 if PH_PREV_RPORRES == 0 
	replace MAT_PREVPREG_CES_PLAN   = 0 if PL_CESARIAN_PROCCUR == 0 & ///
		PH_PREV_RPORRES != 0 
	replace MAT_PREVPREG_CES_PLAN   = 1 if PL_CESARIAN_PROCCUR == 1 & ///
		PH_PREV_RPORRES != 0 
	replace MAT_PREVPREG_CES_PLAN  = 55 if PH_PREV_RPORRES == 1 & ///
		(PL_CESARIAN_PROCCUR == 55 | PL_CESARIAN_PROCCUR == 77 | ///
		 PL_CESARIAN_PROCCUR == 99 | PL_CESARIAN_PROCCUR == .)
		 
	label var MAT_PREVPREG_CES_PLAN  "History of planned c-section(s)"
	
	tab MAT_PREVPREG_CES_PLAN, m 
	
	
	*history of ANY c-section: 	
	tab CESARIAN_RPORRES, m 
	
	gen MAT_PREVPREG_CES_ANY  = 77 if PH_PREV_RPORRES == 0 
	replace MAT_PREVPREG_CES_ANY   = 0 if CESARIAN_RPORRES == 0 & ///
		PH_PREV_RPORRES != 0 
	replace MAT_PREVPREG_CES_ANY   = 1 if CESARIAN_RPORRES == 1 & ///
		PH_PREV_RPORRES != 0 
	replace MAT_PREVPREG_CES_ANY  = 55 if PH_PREV_RPORRES == 1 & ///
		(CESARIAN_RPORRES == 55 | CESARIAN_RPORRES == 77 | ///
		 CESARIAN_RPORRES == 99 | CESARIAN_RPORRES == .)
		 
	label var MAT_PREVPREG_CES_ANY  "History of ANY c-section(s)"
	
	tab MAT_PREVPREG_CES_ANY MAT_PREVPREG_CES_UNPLAN, m 
	
		// fix to MAT_PREVPREG_CES_ANY: 
	replace MAT_PREVPREG_CES_ANY = 1 if MAT_PREVPREG_CES_UNPLAN == 1 & ///
		MAT_PREVPREG_CES_ANY == 0 
	replace MAT_PREVPREG_CES_ANY = 1 if MAT_PREVPREG_CES_PLAN == 1 & ///
		MAT_PREVPREG_CES_ANY == 0 	
		
	
	
	* restrict to variables with information: 
	
	drop if PH_PREV_RPORRES == 77 & MAT_PREVPREG_ANOMALY == . & ///
		MAT_PREVPREG_APH == . & MAT_PREVPREG_CES_UNPLAN == . & ///
		MAT_PREVPREG_GDM == . & MAT_PREVPREG_GHTN == . & ///
		MAT_PREVPREG_IUGR == . & MAT_PREVPREG_LBW == . & ///
		MAT_PREVPREG_MACRO == . & MAT_PREVPREG_OLIGO == . & ///
		MAT_PREVPREG_POSTTERM == . & MAT_PREVPREG_PPH == . & ///
		MAT_PREVPREG_PREEC == . & MAT_PREVPREG_PRETERM == . & ///
		MAT_PREVPREG_PROLABOR == . & MAT_PREVPREG_PROM == . & ///
		MAT_PREVPREG_CES_ANY == . & MAT_PREVPREG_CES_PLAN == .
		
	* Check for duplicate obstetric history entries: 
	duplicates tag MOMID PREGID, gen(dup)
	
	tab dup, m 
	
	*Check for EXACT duplicates: 
	keep MOMID PREGID SITE PH_PREV_RPORRES MAT_PREVPREG_* dup 
	
	duplicates tag MOMID PREGID PH_PREV_RPORRES MAT_PREVPREG_*, gen(exact)
	
	tab exact dup, m 
	
	* some are exact duplicates; others have some differences (review below)
	sort MOMID PREGID 
	
	list if exact == 0 & dup == 1
	
	*Yes some people have conflicting pregnancy histories; we will condense the 
	*variables by taking the max (we will set unknowns to negative to be over-written 
	*if other information is available)
	
	foreach var of varlist PH_PREV_RPORRES MAT_PREVPREG_* {
	
	replace `var' = -55 if `var' == 55
	replace `var' = -77 if `var' == 77
	replace `var' = -99 if `var' == . 
		
	}
	
	*collapse: 
	collapse (max) PH_PREV_RPORRES MAT_PREVPREG_*, by(MOMID PREGID SITE)
	
	
	label var MAT_PREVPREG_APH "Reported APH in previous pregnancy"
	label var MAT_PREVPREG_PPH "Reported PPH in previous pregnancy"
	label var MAT_PREVPREG_PRETERM "Preterm delivery in previous pregnancy"
	label var MAT_PREVPREG_POSTTERM "Postterm delivery in previous pregnancy"
	label var MAT_PREVPREG_GHTN "GHTN in previous pregnancy"
	label var MAT_PREVPREG_PREEC "Preeclampsia/eclampsia in previous pregnancy"
	label var MAT_PREVPREG_GDM "Gestational diabetes in previous pregnancy"
	label var MAT_PREVPREG_PROM "PROM in previous pregnancy"
	label var MAT_PREVPREG_PROLABOR "Prolonged or obstructed labor in previous pregnancy"
	label var MAT_PREVPREG_IUGR "Intrauterine growth restriction in previous pregnancy"
	label var MAT_PREVPREG_LBW "Low birthweight baby in previous pregnancy"
	label var MAT_PREVPREG_MACRO "Macrosomia in previous pregnancy"
	label var MAT_PREVPREG_OLIGO "Oligohydramnios in previous pregnancy"
	label var MAT_PREVPREG_ANOMALY "Baby with fetal anomaly in previous pregnancy"
	label var MAT_PREVPREG_CES_UNPLAN "History of unplanned c-section(s)"	
	
	label var MAT_PREVPREG_CES_ANY "History of ANY c-section(s)"
	label var MAT_PREVPREG_CES_PLAN "History of planned c-section"
	
	foreach var of varlist MAT_PREVPREG_* {
	
	replace `var' = 55 if `var' == -55 
	replace `var' = 77 if `var' == -77 
	replace `var' = . if `var' == -99
	
	tab `var' PH_PREV_RPORRES, m 
		
	}
	
	
	* Review consistency of c-section variables in new upload/data structure: 
	tab MAT_PREVPREG_CES_ANY MAT_PREVPREG_CES_UNPLAN, m 
	
		* if ANY c-section = 0, then we can replace unplanned c-section = 0 if 
		* currently missing:
	replace MAT_PREVPREG_CES_UNPLAN = 0 if MAT_PREVPREG_CES_ANY == 0 & ///
		MAT_PREVPREG_CES_UNPLAN == 55 
		
		* if ANY c-section = 1 and unplanned c-section = 0 and planned c-section
		* = 0, then we can set to missing for unplanned c-section (since we 
		* don't know if it's planned or unplanned)
	replace MAT_PREVPREG_CES_UNPLAN = 55 if MAT_PREVPREG_CES_UNPLAN == 0 & ///
		MAT_PREVPREG_CES_PLAN == 0 & MAT_PREVPREG_CES_ANY == 1 
		
	tab MAT_PREVPREG_CES_ANY, m 
	tab MAT_PREVPREG_CES_ANY MAT_PREVPREG_CES_UNPLAN, m 
	tab MAT_PREVPREG_CES_ANY MAT_PREVPREG_CES_PLAN, m 
	
	
	drop PH_PREV_RPORRES 
	

	
	foreach var of varlist MAT_PREVPREG_* {
		
	tab `var' SITE, m 
		
	}
	
	save "$OUT/MAT_PREVPREG_COMPLICATIONS", replace
	
	
	
	