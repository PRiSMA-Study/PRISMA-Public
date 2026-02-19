	global datadate "2026-01-30"
	
	global wrk "Z:\Savannah_working_files\Maternal Nutrition/$datadate"
	
	cap mkdir "$wrk"
	//make a new directory with data date, if it doesn't exist
	
	cd "$wrk"
	global da "Z:\Stacked Data/$datadate"
	global data "D:\Users\savannah.omalley\Documents\data/$datadate"
	global ids "Z:\Savannah_working_files\Maternal Nutrition\Aim 3\New-IDs"
	global outcomes "Z:\Outcome Data/$datadate"
	local date: di %td_CCYY_NN_DD daily("`c(current_date)'", "DMY")
	global today = subinstr(strltrim("`date'"), " ", "-", .)
	disp "$today"
	
	use "D:\Users\savannah.omalley\Documents\nutrition/$datadate/MAT_NUTR_LONG.dta", clear
	keep if inrange(LBSTDAT,0,.)
	cap gen DATE = LBSTDAT
	*only looking for observations with data on iron, inflammation, or b12:
	keep if inrange( FERRITIN_LBORRES ,0,.)  | inrange( TRANSFERRIN_LBORRES,0,.) | inrange( CRP_LBORRES,0,.) | inrange( AGP_LBORRES,0,.) | inrange(VITB12_COB_LBORRES,0,.) | inrange( RBP4_LBORRES,0,.) | inlist(FOL_ANY,0,1,2,3)
	*keep only needed variables:
	keep  SITE MOMID PREGID DATE TYPE_VISIT RBP4 VITB12_COB FERRITIN_70   STFR CRP AGP INFLAMMATION FOL_ANY
	gen DATE_60 = DATE + 60
	label var DATE_60 "carryforward status until this date"
	save "$wrk/IRON_INF_VIT.dta", replace
	
	
	cap use "$wrk/ANEMIA_all_long.dta",clear 
	if _rc != 0 {
		use "$outcomes/ANEMIA_all_long.dta", clear
		
		save "$wrk/ANEMIA_all_long.dta", replace
	}
	
	keep SITE MOMID PREGID TEST_DATE TEST_TYPE TYPE_VISIT HB_LBORRES TEST_GA TEST_PP TEST_TIMING
	gen DATE = TEST_DATE
	keep if !missing(DATE)
	keep if !missing(HB_LBORRES)
	keep if TEST_TYPE=="CBC" 
		*For ReMAPP, we only want CBC
	bysort MOMID PREGID DATE (TEST_TYPE HB_LBORRES) : gen hb_num=_n
		*sorts alphabetically by TEST_TYPE (CBC will come first)
		*sorts by HB_LBORRES with the lowest HB coming first
	
	list MOMID  DATE TEST_TYPE HB_LBORRES hb_num in 1/10,sepby(MOMID)
	
	*For our purpose, keep only the first sorted hemoglobin measurement
	keep if hb_num == 1
	
	duplicates tag MOMID PREGID DATE,gen(dup)
	assert dup <1
	
	drop hb_num dup
	count
	//n= 66,235 in June 27 dataset
	
	merge 1:1 SITE MOMID PREGID DATE TYPE_VISIT using "$wrk/IRON_INF_VIT.dta"
	
	*carry forward micronutrients 60 days:
	bysort PREGID (DATE): carryforward DATE_60, gen(DATE_CEILING)
		*first, carryforward the date ceiling
	label var DATE_CEILING "Carryforward until this date"
	label define CARRIED 1"carried forward" 0"original"
	foreach var in FERRITIN_70 STFR RBP4 CRP AGP INFLAMMATION VITB12_COB FOL_ANY {
		bysort PREGID (DATE): carryforward `var'  if DATE<=DATE_CEILING, gen(`var'_carry)
		*carry forward MN status if the HB date is <= 60 days from MN date
		*sorted by date
		
		
		gen 	`var'_CARRIED = 0 if `var' !=.
		replace `var'_CARRIED = 1 if `var'_carry !=. & `var' ==.
			*indicator if variable was carried forward
		label val `var'_CARRIED CARRIED
	}
	
	gen date = DATE if inrange( FERRITIN_70 ,0,.)  | inrange( STFR ,0,.) | inrange( CRP ,0,.) | inrange( AGP ,0,.) | inrange( VITB12_COB ,0,.) | inrange( RBP4 ,0,.) |inrange(FOL_ANY,0,.)
	bysort PREGID (DATE): carryforward date, gen(DATE_FLOOR)
		*carry forward date floor 
	format DATE DATE_60 DATE_CEILING DATE_FLOOR %td
	label var DATE_FLOOR "Carryforward from this date"
	
	list SITE MOMID DATE DATE_60 DATE_FLOOR  DATE_CEILING FERRITIN_70 FERRITIN_70_carry in 1/20, sepby(MOMID)
	
	
	gen completecase = 1 if !missing( HB_LBORRES ) & ///
	!missing( CRP_carry ) & !missing( AGP_carry ) & ///
	!missing( FERRITIN_70_carry ) & !missing( STFR_carry ) & ///
	!missing( RBP4_carry ) & !missing( VITB12_COB_carry )
	
	gen completequansys = 1 if !missing( HB_LBORRES ) & ///
	!missing( CRP_carry ) & !missing( AGP_carry ) & ///
	!missing( FERRITIN_70_carry ) & !missing( STFR_carry ) & ///
	!missing( RBP4_carry )
	
	gen completeb12 = 1 if ///
		!missing( HB_LBORRES ) & !missing( VITB12_COB_carry)
	for var completecase completequansys completeb12: replace X = 0 if X !=1
	
		label define complete 1"complete"
		label val completecase completequansys completeb12 complete
	*tab:
	for var completecase completequansys completeb12 : tab X SITE if !missing(HB_LBORRES), col
	
	assert missing(FERRITIN_70_carry ) if !inrange(DATE, DATE_FLOOR, DATE_CEILING)
		* logic check: assert that the value is missing if the hb date is not within the micronutrient date range
	
	gen TRIMESTER = 1 if inrange(TEST_GA,28,97)
	replace TRIMESTER = 2 if TEST_GA>=98 & TEST_GA<=195
	replace TRIMESTER = 3 if TEST_GA>=196 & TEST_GA<=300
	gen ANY_ANEMIA = 0 if !missing(HB_LBORRES) & !missing(TRIMESTER)
	replace ANY_ANEMIA = 1 if inrange(HB_LBORRES,0,10.999) & inlist(TRIMESTER,1,3)
	replace ANY_ANEMIA = 1 if inrange(HB_LBORRES,0,10.499) & inlist(TRIMESTER,2)
	label define ANY_ANEMIA 1"Anemic" 0"Not anemic"
	label val ANY_ANEMIA ANY_ANEMIA
	
	
	recast str40 PREGID
	save "$wrk/Hb-MN-Carried-Forward-CBC-only.dta", replace
	drop _merge
	merge m:1 SITE MOMID PREGID using "$outcomes/MAT_ENROLL",keepusing(REMAPP_ENROLL)
	drop _merge
	keep SITE MOMID PREGID TEST_DATE TEST_TYPE TYPE_VISIT HB_LBORRES REMAPP_ENROLL TEST_GA TEST_PP  DATE FERRITIN_70_carry STFR_carry RBP4_carry CRP_carry AGP_carry CRP AGP INFLAMMATION_carry VITB12_COB_carry FOL_ANY FOL_ANY_carry 
	keep if !missing(HB_LBORRES)
	count

	save "$outcomes/Hb-MN-Carried-Forward-CBC-only.dta", replace
	
	
