*PRISMA Variable Construction - Birth Position & Mode of Delivery Details by Infant
*Purpose: This code constructs risk factor variables related to birth position & mode of delivery 
*Original Version: July 24, 2024
*Update: Clarify denominators & create denom-specific variables

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

global OUT "Z:/Outcome Data/$dadate"

global date "250428" // today's date

log using "$log/construct_birthposition_$date", replace

/*************************************************************************

*This file constructs the following variables: 
	
	Birth position: 
		INF_PRES Fetal presentation:
			1=Cephalic vertex
			2=Breech
			3=Transverse lie 
			4=Brow or face 
			5=Other
			55=Missing/unknown
		INF_PRES_CEPH		=1 if cephalic vertex 
		INF_PRES_BREECH		=1 if breech
		INF_PRES_TRANS		=1 if transverse lie 
		INF_PRES_BROW 		=1 if brow or face 
		INF_PRES_OTHER		=1 if other birth presentation
		INF_PRES_MISS		=1 if missing/unknown 
		
	Delivery mode: 
		INF_MODE_CES 			=1 if cesarean delivery
		INF_MODE_VAG 			=1 if vaginal delivery
		INF_MODE_VAG_ASSISTED 	=1 if assisted vaginal delivery
		INF_MODE_VAG_FORCEPS	=1 if forceps-assisted vaginal delivery 	
			
*/
	
	/////////////////////////////////////////
	
	* All information is drawn from MNH09 delivery form; below we manually reshape it 
	* from wide to long to get infant-level outcomes: 
	* /
	import delimited "$da/mnh09_merged", varn(1) case(preserve) bindquote(strict)
		
		keep MOMID PREGID M09_INFANTID_INF* M09_PRESENT_FAORRES* M09_DELIV_PRROUTE_INF* ///
			M09_VAG_PRROUTE_INF* SITE

		
		///// Loop by infant number: 
		
		foreach num of numlist 1/4 {
		
		preserve 
		
		keep MOMID PREGID M09_INFANTID_INF`num' M09_PRESENT_FAORRES_INF`num' ///
			M09_DELIV_PRROUTE_INF`num' M09_VAG_PRROUTE_INF`num' SITE
			
		rename M09_INFANTID_INF`num' INFANTID 
		
		rename M09_PRESENT_FAORRES_INF`num' PRESENT_FAORRES
		rename M09_DELIV_PRROUTE_INF`num' DELIV_PRROUTE
		rename M09_VAG_PRROUTE_INF`num' VAG_PRROUTE
		
		keep MOMID PREGID INFANTID PRESENT_FAORRES DELIV_PRROUTE VAG_PRROUTE SITE
		
		drop if INFANTID == "" | INFANTID == "55" | INFANTID == "77" | INFANTID == "n/a"
		
		save "$wrk/position_inf`num'", replace 
		
		restore 
		
		}
		*/
		
		*Stack the data together: 
		
		clear 
		
		use "$wrk/position_inf1"
		
		gen INFANT_NUM = 1 
		
		append using "$wrk/position_inf2", gen(inf2)
		
		replace INFANT_NUM = 2 if inf2==1
		
		append using "$wrk/position_inf3", gen(inf3)
		
		replace INFANT_NUM = 3 if inf3==1
		
		/* no quads in current dataset: 
		*append using "$wrk/position_inf4", gen(inf4)
		
		*replace INFANT_NUM = 4 if inf4==1
		*/
		
		tab DELIV_PRROUTE if INFANT_NUM==1, m 
		tab DELIV_PRROUTE if INFANT_NUM==2, m 
		tab DELIV_PRROUTE if INFANT_NUM==3, m 
		tab DELIV_PRROUTE if INFANT_NUM==4, m 
	
	////////////////////////////
	*Construct: Birth position: 
	tab PRESENT_FAORRES, m 
	
	gen INF_PRES = PRESENT_FAORRES
	
	label var INF_PRES "Fetal presentation (1=cephalic; 2=breech)"
	
		replace INF_PRES = 5 if INF_PRES == 88 // recode other as 5 
		
		replace INF_PRES = 55 if INF_PRES == 77 | INF_PRES == 99 | INF_PRES == .
		
	label define pres 1 "1-Cephalic" 2 "2-Breech" 3 "3-Transverse" 4 "4-Brow or face" ///
		5 "5-Other" 55 "55-Missing/unknown"
		
	label values INF_PRES pres 
	
	tab INF_PRES, m 
	
	foreach var of varlist INF_PRES {
	
	foreach num of numlist 1/5 {
	
	gen `var'_`num' = 0 
	replace `var'_`num' = 1 if `var' == `num'
	
	replace `var'_`num' = 55 if `var' == 55 
	
	tab `var' `var'_`num', m 
	
	}
	}
	
	rename INF_PRES_1 INF_PRES_CEPH
	rename INF_PRES_2 INF_PRES_BREECH 
	rename INF_PRES_3 INF_PRES_TRANS 
	rename INF_PRES_4 INF_PRES_BROW 
	rename INF_PRES_5 INF_PRES_OTHER 
	
	label var INF_PRES_CEPH "Fetal presentation: cephalic vertex"
	label var INF_PRES_BREECH "Fetal presentation: breech"
	label var INF_PRES_TRANS "Fetal presentation: transverse lie"
	label var INF_PRES_BROW "Fetal presentation: brow or face"
	label var INF_PRES_OTHER "Fetal presentation: other"
	
	////////////////////////////
	*Construct: Infant-level delivery mode:  
	tab DELIV_PRROUTE, m 	
	
	gen INF_MODE_CES = 0 if DELIV_PRROUTE == 1 // vaginal delivery 
	replace INF_MODE_CES = 1 if DELIV_PRROUTE == 2 // c-section 
	replace INF_MODE_CES = 77 if DELIV_PRROUTE == 3 //N/A if maternal death prior to delivery 
	replace INF_MODE_CES = 55 if DELIV_PRROUTE == 55 | DELIV_PRROUTE == 77 | DELIV_PRROUTE == . 
	
	label var INF_MODE_CES "Delivery mode (infant): Cesarean"
	
	gen INF_MODE_VAG = 0 if DELIV_PRROUTE == 2 // c-section 
	replace INF_MODE_VAG = 1 if DELIV_PRROUTE == 1 // vaginal delivery 
	replace INF_MODE_VAG = 77 if DELIV_PRROUTE == 3 // N/A if maternal death prior to delivery 
	replace INF_MODE_VAG = 55 if DELIV_PRROUTE == 55 | DELIV_PRROUTE == 77 | DELIV_PRROUTE == . 
	
	label var INF_MODE_VAG "Delivery mode (infant): Vaginal"
	
	tab DELIV_PRROUTE INF_MODE_CES, m 
	tab DELIV_PRROUTE INF_MODE_VAG, m 
	
	
	//////////////////////////
	*Construct: Assisted vaginal deliveries & Forceps assisted deliveries
	tab VAG_PRROUTE, m 
	
	// any assisted: 
	gen INF_MODE_VAG_ASSISTED = 0 if INF_MODE_CES == 1 
	replace INF_MODE_VAG_ASSISTED = 55 if INF_MODE_VAG == 55 
	replace INF_MODE_VAG_ASSISTED = 77 if INF_MODE_VAG == 77 
	
	replace INF_MODE_VAG_ASSISTED = 0 if VAG_PRROUTE == 1 // spontaneous 
	replace INF_MODE_VAG_ASSISTED = 1 if VAG_PRROUTE == 2 // assisted breech 
	replace INF_MODE_VAG_ASSISTED = 1 if VAG_PRROUTE == 3 // vacuum 
	replace INF_MODE_VAG_ASSISTED = 1 if VAG_PRROUTE == 4 // forceps 
	replace INF_MODE_VAG_ASSISTED = 1 if VAG_PRROUTE == 88 // other method 
	replace INF_MODE_VAG_ASSISTED = 55 if INF_MODE_VAG == 1 & ///
		(VAG_PRROUTE == 99 | VAG_PRROUTE == 77 | VAG_PRROUTE == 55)
		
	tab INF_MODE_VAG INF_MODE_VAG_ASSISTED, m 
	
	label var INF_MODE_VAG_ASSISTED "Any assisted vaginal delivery"
	
	// forceps-assisted
	gen INF_MODE_VAG_FORCEPS = 0 if INF_MODE_CES == 1 
	replace INF_MODE_VAG_FORCEPS = 55 if INF_MODE_VAG == 55 
	replace INF_MODE_VAG_FORCEPS = 77 if INF_MODE_VAG == 77 
	
	replace INF_MODE_VAG_FORCEPS = 0 if VAG_PRROUTE == 1 // spontaneous 
	replace INF_MODE_VAG_FORCEPS = 0 if VAG_PRROUTE == 2 // assisted breech 
	replace INF_MODE_VAG_FORCEPS = 0 if VAG_PRROUTE == 3 // vacuum 
	replace INF_MODE_VAG_FORCEPS = 1 if VAG_PRROUTE == 4 // forceps 
	replace INF_MODE_VAG_FORCEPS = 0 if VAG_PRROUTE == 88 // other method 
	replace INF_MODE_VAG_FORCEPS = 55 if INF_MODE_VAG == 1 & ///
		(VAG_PRROUTE == 99 | VAG_PRROUTE == 77 | VAG_PRROUTE == 55)
		
	tab INF_MODE_VAG INF_MODE_VAG_FORCEPS, m 
	
	label var INF_MODE_VAG_FORCEPS "Forceps-assisted vaginal delivery"
	
	
	keep MOMID PREGID SITE INFANTID INF_PRES INF_PRES_CEPH INF_PRES_BREECH ///
		INF_PRES_TRANS INF_PRES_BROW INF_PRES_OTHER INF_MODE_CES ///
		INF_MODE_VAG INF_MODE_VAG_ASSISTED INF_MODE_VAG_FORCEPS
		
	* Merge in indicator for deliveries: 
	
	merge m:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing(PREG_END ///
		PREG_END_GA)

	keep if PREG_END==1 
	
	keep if PREG_END_GA >= 140 & PREG_END_GA != . 
	
	drop if _merge == 2 
	
	drop _merge 
	
	*CREATE DENOMINATORS: 
	gen INF_PRES_DENOM = 0 
	replace INF_PRES_DENOM = 1 if PREG_END_GA >=140 & PREG_END_GA != . & PREG_END==1 
	label var INF_PRES_DENOM "Denominator for infant presentation (deliveries >= 20 weeks GA)"
	
	gen INF_MODE_DENOM = 0 
	replace INF_MODE_DENOM = 1 if PREG_END_GA >=140 & PREG_END_GA != . & PREG_END==1 
	label var INF_MODE_DENOM "Denominator for infant delivery mode (deliveries >= 20 weeks GA)"
	
	drop PREG_END PREG_END_GA 
	
	* Save file to main folder: 
	
	save "$OUT/INF_PRESENTATION", replace 
	