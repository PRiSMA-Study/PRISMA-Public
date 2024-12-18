** FACIT-Fatigue validation analysis **
**Savannah O'Malley (savannah.omalley@gwu.edu)

/*
Notes:
This file requires the following maternal outcome data sets:

	MAT_HEMORRHAGE.csv
	MAT_DEMOGRAPHIC.dta
	MAT_ENROLL.xlsx
	MAT_ENDPOINTS.dta
	MAT_LABOR.dta

This file requires the following stacked data files:

	mnh26_merged.xlsx
	mnh04_merged.xlsx
	mnh09_merged.csv
	mnh19_merged.xlsx


*/
  
**Part 1: Set paths & read in the data (MNH 26)
	
	global datadate "2024-11-15"
	global da "Z:\Stacked Data/$datadate"
	
	global outcomes "Z:\Outcome Data/$datadate"
		//most recent data set
	global latest_out "Z:\Outcome Data\2024-09-20" 
		//latest full set of outcome files; update as needed
	
	global wrk "Z:\Savannah_working_files\Fatigue"
	global output "Z:\Savannah_working_files\Fatigue\output"
	global ids "Z:\Savannah_working_files\Fatigue\Test_Retest_IDs"
	cd "$wrk"

	**Step 1: bring in the maternal outcomes using most recent files:
	
	import delimited "$latest_out/MAT_HEMORRHAGE.csv", ///
	bindquote(strict) case(upper) clear 
	save "$wrk/MAT_HEMORRHAGE.dta",replace
		
	import delimited "Z:\Outcome Data\2024-11-15\MAT_DEMOGRAPHIC.csv", ///
	bindquote(strict) case(upper) clear
	save "$wrk/MAT_DEMOGRAPHIC", replace
	
	
	**# Calculate Fatigue for all data
	import excel "$da/mnh26_merged.xlsx", sheet("Sheet 1") firstrow clear
	rename M26_* *
	

	
order SITE MOMID PREGID FTGE_OBSTDAT TYPE_VISIT MAT_VITAL_MNH26 MAT_VISIT_MNH26 MAT_VISIT_OTHR_MNH26 FTGE_AN2 FTGE_HI7 FTGE_HI12 FTGE_AN1V FTGE_AN3 FTGE_AN4 FTGE_AN5 FTGE_AN7 FTGE_AN8 FTGE_AN12 FTGE_AN14 FTGE_AN15 FTGE_AN16 FTGE_ASSIST COYN_MNH26 FORMCOMPLDAT_MNH26 FORMCOMPLID_MNH26 COVAL_MNH26
	label var FTGE_AN2 "1. tired"
	label var FTGE_HI7 "2. fatigued"
	label var FTGE_HI12 "3. weak"
	label var FTGE_AN1V "4. washed out"
	label var FTGE_AN3 "5. trouble starting things"
	label var FTGE_AN4 "6. trouble finishing things"
	label var FTGE_AN5 "7. have energy"
	label var FTGE_AN7 "8. able to do usual activities"
	label var FTGE_AN8 "9. need sleep during day"
	label var FTGE_AN12 "10. too tired to eat"
	label var FTGE_AN14 "11. need help usual activities"
	label var FTGE_AN15 "12. frustrated"
	label var FTGE_AN16 "13. limit social"
	
	foreach var in FTGE_AN5 FTGE_AN7 {
		destring `var', replace
		recode `var' (66=.) (77=.) (55=.)
	}
	//for these reverse-coded items, they do not need to be corrected
	//only destring and replace default values as missing

	foreach v of varlist  FTGE_AN2 FTGE_HI7 FTGE_HI12 FTGE_AN1V FTGE_AN3 FTGE_AN4 FTGE_AN8 FTGE_AN12 FTGE_AN14 FTGE_AN15 FTGE_AN16 {
		recode `v' (77=.) (66=.) (55=.)
		gen 	`v'_R =(4- `v')
		label var `v'_R "`: variable label `v''"
	}
	//these items are coded negatively, subtract each value from 4
	
	order FTGE_AN2_R FTGE_HI7_R FTGE_HI12_R FTGE_AN1V_R FTGE_AN3_R FTGE_AN4_R FTGE_AN5 FTGE_AN7 FTGE_AN8_R FTGE_AN12_R FTGE_AN14_R FTGE_AN15_R FTGE_AN16_R, after( MAT_VISIT_OTHR_MNH26)
	order FTGE_AN2 FTGE_HI7 FTGE_HI12 FTGE_AN1V FTGE_AN3 FTGE_AN4 FTGE_AN8 FTGE_AN12 FTGE_AN14 FTGE_AN15 FTGE_AN16, after(COVAL_MNH26)
	
	//get the number of items answered
	egen Q_ANSWERED = anycount( FTGE_AN2_R FTGE_HI7_R FTGE_HI12_R FTGE_AN1V_R FTGE_AN3_R FTGE_AN4_R FTGE_AN5 FTGE_AN7 FTGE_AN8_R FTGE_AN12_R FTGE_AN14_R FTGE_AN15_R FTGE_AN16_R), values( 0 1 2 3 4)

	egen fatigue = rowtotal( FTGE_AN2_R FTGE_HI7_R FTGE_HI12_R FTGE_AN1V_R FTGE_AN3_R FTGE_AN4_R FTGE_AN5 FTGE_AN7 FTGE_AN8_R FTGE_AN12_R FTGE_AN14_R FTGE_AN15_R FTGE_AN16_R)

	gen FATIGUE=(fatigue*13)/Q_ANSWERED
	
	gen FATIGUE_VALIDSCORE = 0
	replace FATIGUE_VALIDSCORE = 1 if FATIGUE>=0 & FATIGUE<.
	gen SCORE_ANC20 = 1 if ///
	FATIGUE_VALIDSCORE==1 & inlist(TYPE_VISIT,1,2)
	gen SCORE_ANC32 = 1 if ///
	FATIGUE_VALIDSCORE==1 & inlist(TYPE_VISIT,4,5)
	gen SCORE_PNC6 = 1 if ///
	FATIGUE_VALIDSCORE==1 & inlist(TYPE_VISIT,10)

	gen Date=FTGE_OBSTDAT
	format Date %td
	
	preserve
		import excel "$outcomes/MAT_ENROLL.xlsx", sheet("Sheet 1") firstrow clear
		save "$outcomes/MAT_ENROLL.dta", replace
	restore
	
	
	
	merge m:1 MOMID PREGID using "$outcomes/MAT_ENROLL.dta", ///
	keepusing(SITE M02_SCRN_OBSSTDAT M01_US_OHOSTDAT ENROLL EST_CONCEP_DATE) ///
	gen(ENROLLmerge)
	
	keep if ENROLL == 1
	 
	drop ENROLLmerge
	
	*Identify when ReMAPP started - when we expect Fatigue assessments
	gen remappdate = "2022-12-28" if SITE =="Ghana"
	replace remappdate = "2023-04-14" if SITE=="Kenya"
	replace remappdate = "2022-12-15" if SITE=="Zambia"
	replace remappdate = "2022-09-22" if SITE=="Pakistan"
	replace remappdate = "2023-06-20" if SITE=="India-CMC"
	replace remappdate = "2023-08-15" if SITE=="India-SAS"
	gen remappend = "2024-04-05" if SITE=="Pakistan"
	gen ReMAPPEnd =date(remappend,"YMD")
	gen ReMAPPDate=date(remappdate, "YMD")
	format ReMAPPDate %td
	gen ReMAPP = 1 if M01_US_OHOSTDAT>=ReMAPPDate
	*keep if M01_US_OHOSTDAT>=ReMAPPDate
	replace ReMAPP=. if M01_US_OHOSTDAT>=ReMAPPEnd & SITE=="Pakistan"
	
	duplicates tag MOMID PREGID FTGE_OBSTDAT, gen(duplicate)
	tab duplicate SITE
	
	/*
	**temporary: export duplicate events to send to sites
	foreach site in "Ghana" "Kenya" "Zambia"  {
	export excel SITE MOMID TYPE_VISIT FTGE_OBSTDAT  MAT_VISIT_MNH26 duplicate using "$wrk/Duplicate Fatigue assessments `site'.xlsx" if  duplicate!=0 &   FATIGUE>=0 & SITE == "`site'", sheet("Duplicate_MNH26") replace firstrow(variables)
	} 
	
	*/
	
	**!!temporary fix while waiting on sites to clear duplicates:
	
	bysort MOMID PREGID FTGE_OBSTDAT (TYPE_VISIT ) : gen VISNUM=_n
	bysort MOMID PREGID FTGE_OBSTDAT (TYPE_VISIT ) : gen VISTOTAL=_N
	tab VISNUM SITE
	list SITE MOMID FTGE_OBSTDAT TYPE_VISIT MAT_VISIT_MNH26 FATIGUE if VISTOTAL !=1,sepby(SITE)
	keep if VISNUM==1
	drop if TYPE_VISIT==.
	duplicates tag MOMID PREGID FTGE_OBSTDAT, gen(dup_check2)
	tab dup_check2
	drop dup* VISNUM VISTOTAL
	gen DATE = FTGE_OBSTDAT
	gen FATIGUE_GA = FTGE_OBSTDAT - EST_CONCEP_DATE
	replace FATIGUE_GA=. if FATIGUE_GA<0
	replace FATIGUE_GA=. if FATIGUE_GA>400
	
	*Query: were fatigue assessments timed correctly? 
		*does visit type match the visit window?
	gen GA_CHECK = 1 if TYPE_VISIT==1 & !inrange(FATIGUE_GA,28,181)
	replace  GA_CHECK = 1 if TYPE_VISIT==2 & !inrange(FATIGUE_GA,126,181)
	replace  GA_CHECK = 1 if TYPE_VISIT==3 & !inrange(FATIGUE_GA,182,216)
	replace  GA_CHECK = 1 if TYPE_VISIT==4 & !inrange(FATIGUE_GA,217,237)
	replace  GA_CHECK = 1 if TYPE_VISIT==5 & !inrange(FATIGUE_GA,238,272)
	

	
	hist FATIGUE_GA if TYPE_VISIT<=10, by(TYPE_VISIT,col(1))

	*recode site to make trends easier to see:
	gen SITE_CONTINENT = 1 if SITE=="Ghana"
	replace SITE_CONTINENT = 2 if SITE=="Kenya"
	replace SITE_CONTINENT = 3 if SITE=="Zambia"
	replace SITE_CONTINENT = 4 if SITE=="India-CMC"
	replace SITE_CONTINENT = 5 if SITE=="India-SAS"
	replace SITE_CONTINENT = 6 if SITE=="Pakistan"
	label define SITE_CONTINENT ///
		1"Ghana" 2"Kenya" 3"Zambia" ///
		4"India-CMC" 5"India-SAS" 6"Pakistan"
	label val SITE_CONTINENT SITE_CONTINENT
	bigtab SITE SITE_CONTINENT
	
	merge m:1 MOMID PREGID using "$outcomes/MAT_ENDPOINTS.dta", ///
	force gen(endpoints_merge) ///
	keepusing(PREG_END_DATE ANC20_PASS_LATE ANC36_PASS_LATE PNC6_PASS_LATE)
	
	*Calculate days postpartum (infant age)
	gen FATIGUE_INFAGE = FTGE_OBSTDAT-PREG_END_DATE if PREG_END_DATE !=. 
	*set infant age to missing if during pregnancy
	replace FATIGUE_INFAGE =. if ///
	FTGE_OBSTDAT<PREG_END_DATE & inlist(TYPE_VISIT,1,2,3,4,5,13)
	*Set gestational age to missing if visit date after pregnancy end date
	replace FATIGUE_GA =. if ///
	FTGE_OBSTDAT>PREG_END_DATE & inlist(TYPE_VISIT,10,14)
	
	
	replace GA_CHECK = 1 if TYPE_VISIT ==10 & !inrange(FATIGUE_INFAGE,42,104)
		
	gen EXP_TYPE_VISIT = 1 if inrange(FATIGUE_GA,28,181)
	replace EXP_TYPE_VISIT = 2 if inrange(FATIGUE_GA,126,181)
	replace EXP_TYPE_VISIT = 3 if inrange(FATIGUE_GA,182,216)
	replace EXP_TYPE_VISIT = 4 if inrange(FATIGUE_GA,217,237)
	replace EXP_TYPE_VISIT = 5 if inrange(FATIGUE_GA,238,272)
	replace EXP_TYPE_VISIT = 7 if inrange(FATIGUE_INFAGE,3,5)
	replace EXP_TYPE_VISIT = 8 if inrange(FATIGUE_INFAGE,7,14)
	replace EXP_TYPE_VISIT = 9 if inrange(FATIGUE_INFAGE,28,35)
	replace EXP_TYPE_VISIT = 10 if inrange(FATIGUE_INFAGE,42,104)
	replace EXP_TYPE_VISIT = 11 if inrange(FATIGUE_INFAGE,182,279)
	replace EXP_TYPE_VISIT = 12 if inrange(FATIGUE_INFAGE,364,454)
	replace EXP_TYPE_VISIT = 14 if ///
	(FTGE_OBSTDAT>PREG_END_DATE) & EXP_TYPE_VISIT==. & (FTGE_OBSTDAT!=. & PREG_END_DATE!=. )
	
	gen CHECK = 1 if EXP_TYPE_VISIT!=TYPE_VISIT
	gen Note="too early" if TYPE_VISIT<EXP_TYPE_VISIT
	replace Note="too late" if TYPE_VISIT>EXP_TYPE_VISIT
	
	*do not flag the non-completed visits, as we don't expect these dates to match the windows:
	replace GA_CHECK=. if MAT_VISIT_MNH26>=3
	
	gen TYPE_VISIT_1_MIN = 28
	gen TYPE_VISIT_1_MAX = 181 //use the same maximum window as ANC20 since these can be combined
	gen TYPE_VISIT_2_MIN = 126
	gen TYPE_VISIT_2_MAX = 181
	
	gen TYPE_VISIT_3_MIN = 182
	gen TYPE_VISIT_3_MAX = 216
	gen TYPE_VISIT_4_MIN = 217
	gen TYPE_VISIT_4_MAX = 237
	
	gen TYPE_VISIT_5_MIN = 238
	gen TYPE_VISIT_5_MAX = 272
	
	gen TYPE_VISIT_10_MIN = 42
	gen TYPE_VISIT_10_MAX = 104
	
	/*
	**Optional Query: export IDs with wrong visit window/dates:
	foreach site in "Ghana" "India-CMC"  "India-SAS" "Kenya" "Pakistan"  {	
		foreach num of numlist 1 2 4 5  {
		cap	export excel SITE MOMID PREGID TYPE_VISIT FATIGUE_GA  TYPE_VISIT_`num'_MIN TYPE_VISIT_`num'_MAX  using "$output/Check dates `site'.xlsx" if GA_CHECK==1  & TYPE_VISIT==`num' &  SITE == "`site'", ///
	sheet("TYPE_VISIT `num'", modify)  firstrow(variables) 
		}
		
		cap	export excel SITE MOMID PREGID TYPE_VISIT FATIGUE_INFAGE TYPE_VISIT_10_MIN TYPE_VISIT_10_MAX using "$output/Check dates `site'.xlsx" if GA_CHECK==1  & TYPE_VISIT==10 &  SITE == "`site'", ///
	sheet("TYPE_VISIT 10", modify)  firstrow(variables) 

	}
	
	
	*EXPORT DATES FOR ZAMBIA:
	foreach num of numlist 1 2 4 5 10 {
		cap	export excel SITE MOMID PREGID TYPE_VISIT FTGE_OBSTDAT  using "$output/Check dates Zambia.xlsx" if GA_CHECK==1  & TYPE_VISIT==`num' &  SITE == "Zambia", ///
	sheet("TYPE_VISIT `num'", modify)  firstrow(variables) 
		}
	*/
	save "$wrk/mnh26.dta" , replace //save long data
	
	/*
	**Next: identify those with missing forms
	gen vistype = "ANC20" if TYPE_VISIT<=2
	replace vistype = "ANC20" if TYPE_VISIT == 13 & FATIGUE_GA <= 180
	replace vistype = "ANC32" if inlist(TYPE_VISIT,4,5)
	replace vistype = "ANC32" if ///
	TYPE_VISIT == 13 & FATIGUE_GA > 180 & FTGE_OBSTDAT < PREG_END_DATE
	replace vistype = "PNC6" if TYPE_VISIT == 10
	replace vistype = "PNC6" if TYPE_VISIT == 14 & ///
	(FTGE_OBSTDAT - PREG_END_DATE >= 42 ) & (FTGE_OBSTDAT - PREG_END_DATE <= 104 )
	
	drop if vistype=="" & FATIGUE_INFAGE > 104
	//these are past the late visit window for PNC 6 or no fatigue score
	drop if TYPE_VISIT==3
	//THESE ARE NOT EXPECTED
	
	list SITE MOMID FTGE_OBSTDAT TYPE_VISIT  FATIGUE_GA FATIGUE_INFAGE if vistype=="",abbreviate(15)
	//note a few outstanding issues at Kenya, 
		//have emailed 10/29/2024
	foreach let in "ANC20" "ANC32" "PNC6" {
		gen `let' = 1 if vistype=="`let'"
	}
	collapse (max) ANC20 ANC32 PNC6 , by(SITE MOMID PREGID)
	
	foreach let in "ANC20" "ANC32" "PNC6" {
		label var `let' "Fatigue form filled for `let'"
	}
	merge 1:1 MOMID PREGID using "Z:\Savannah_working_files\Expected_obs-2024-10-18.dta", gen(expect)
	
	
	**who is expected but missing?
	gen remappdate = "2022-12-28" if SITE =="Ghana"
	replace remappdate = "2023-04-14" if SITE=="Kenya"
	replace remappdate = "2022-12-15" if SITE=="Zambia"
	replace remappdate = "2022-09-22" if SITE=="Pakistan"
	replace remappdate = "2023-06-20" if SITE=="India-CMC"
	replace remappdate = "2023-08-15" if SITE=="India-SAS"
	gen remappend = "2024-04-05" if SITE=="Pakistan"
	gen ReMAPPEnd =date(remappend,"YMD")
	gen ReMAPPDate=date(remappdate, "YMD")
	format ReMAPPDate %td
	gen ReMAPP = 1 if M01_US_OHOSTDAT>=ReMAPPDate
	*keep if M01_US_OHOSTDAT>=ReMAPPDate
	replace ReMAPP=. if M01_US_OHOSTDAT>=ReMAPPEnd & SITE=="Pakistan"
	
	foreach let in "ANC20" "ANC32" "PNC6" {
		tab  `let' SITE if `let'_EXP == 1 & ReMAPP==1,miss col
	}
	
	/*
	**export missing IDs:
	foreach site in "Ghana" "India-CMC" "India-SAS" "Kenya" "Pakistan" "Zambia" {
	
	foreach let in "ANC20" "ANC32" "PNC6" { 
		cap export excel SITE MOMID PREGID `let'_EXP `let'  ///
		using "$output/Missing Fatigue `site'.xlsx" if ///
		`let'_EXP == 1 & `let' ==. & ReMAPP==1 &  SITE == "`site'", ///
		sheet("`let'", modify)  firstrow(variables)
	}
	
	}
	
	*/
	*/
	
**#Get the first 250 with a fatigue analysis
	use "$wrk/mnh26.dta", clear
	
	**keep if valid fatigue score
	keep if FATIGUE >=0 & FATIGUE != .
	
	**keep if valid date (non-default date and non-missing)
	keep if FTGE_OBSTDAT > 0 & FTGE_OBSTDAT !=.
	
	*keep only if it was administered early, to give time for each participant to have 3 assessments
	keep if TYPE_VISIT<=2
	
	**create visit sequence for each mom based on visit date
	by MOMID ( FTGE_OBSTDAT ) , sort: gen VISITNUM = _n
	
	order SITE MOMID PREGID FTGE_OBSTDAT VISITNUM
	
	**keep only her first visit
	keep if VISITNUM==1
	
	sort SITE FTGE_OBSTDAT
	
	**create sequence based on fatigue administration date by each site
		*if same date, sort by PREGID
	by SITE (FTGE_OBSTDAT PREGID) , sort: gen FATIGUENUM = _n 
	
	*keep the first 250 from each site
	keep if FATIGUENUM <= 250
	
	*keep only the IDs
	keep  MOMID PREGID  
	*create an indicator that these are the 1st 250
	gen ENROLL250=1

	
	merge 1:m MOMID PREGID using "$wrk/mnh26.dta", nogen
	keep if ENROLL250==1
	
	*save a data set of only the first 250
	save "$wrk/FATIGUE_250.dta",replace
	
	*create a wide version for matching to anemia and depression
	keep SITE MOMID PREGID FATIGUE DATE
	bysort MOMID PREGID (DATE): gen VISNUM=_n
	reshape wide FATIGUE DATE, i(SITE MOMID PREGID) j(VISNUM)
	save "$wrk/FATIGUE_250_WIDE.dta", replace
	
	
	use "$wrk/FATIGUE_250.dta", clear
	
	encode SITE, gen(SITE_NUM)
	tab SITE SITE_NUM
	
	
**#Merge in Anemia and Depression for convergent validity
	
	merge 1:1 MOMID PREGID DATE TYPE_VISIT using ///
	"Z:\Savannah_working_files\MNH25\data\mnh25.dta", gen(DEPRMERGE)
	keep if ENROLL == 1
	drop DEPRMERGE
	save "$wrk/FATIGUE_250.dta", replace
	
	
	use "Z:\Erin_working_files\data\ANEMIA_all_long.dta", clear
	
	keep MOMID PREGID site TEST_DATE TEST_TYPE TYPE_VISIT HB_LBORRES SITE TEST_GA TEST_TIMING
	
	*by each PREGID-date combination, prioritize CBC, take the lowest hb within each date
	bysort MOMID PREGID TEST_DATE (TEST_TYPE HB_LBORRES) : gen hb_num=_n

	*For fatigue purpose, keep only the first sorted hemoglobin measurement
	keep if hb_num == 1
	
	merge m:1 MOMID PREGID using ///
	"$wrk/FATIGUE_250_WIDE.dta", gen(anemia_merge) force
	keep if anemia_merge==3
	drop anemia_merge
	
	*CHECK IF THERE IS A FATIGUE DATE WITHIN 7 DAYS OF THE HB DATE
	*IF YES, ASSIGN "DATE" TO EQUAL THE FATIGUE DATE (FOR MERGING)
	gen fatiguematch=.
	gen DATE =.
	foreach num of numlist 1/5 {
		replace fatiguematch = `num' if ///
		(TEST_DATE - DATE`num' <= 7) & (TEST_DATE - DATE`num' >= -7) 
		replace DATE = DATE`num' if ///
		(TEST_DATE - DATE`num' <= 7) & (TEST_DATE - DATE`num' >= -7) & DATE==.
	}
	
	*keep only if there's a match within 7 days:
	keep if DATE!=.
	format DATE %td
	
	duplicates tag MOMID PREGID TYPE_VISIT DATE,gen(dup)
	*duplicates because sometimes there are two observations within 7 days
	*in this case take the closest one
	
	*by each mother/date combination, sort by test type then absolute # days difference
	gen test_diff = abs(TEST_DATE-DATE)
	
	bysort MOMID PREGID TYPE_VISIT DATE (TEST_TYPE test_diff) : gen rank=_n
	
	list MOMID TEST_DATE TEST_TYPE rank test_diff fatiguematch DATE* if dup>0, separator(2)
	
	keep if rank==1
	
	drop FATIGUE1 FATIGUE2 FATIGUE3 FATIGUE4 FATIGUE5 DATE1 DATE2 DATE3 DATE4 DATE5 
	keep SITE MOMID PREGID TEST_DATE TEST_TYPE TYPE_VISIT HB_LBORRES DATE
	merge 1:1 MOMID PREGID TYPE_VISIT DATE using ///
	"$wrk/FATIGUE_250.dta", nogen force
	keep if ENROLL250 ==1 
	

	*merge 1:1 MOMID PREGID Date TYPE_VISIT using "Z:\Savannah_working_files\Maternal Nutrition\mnh08_allvisits-$datadate.dta", generate(NUTR_merge) force
	keep if ENROLL250==1
	
	save "$wrk/FATIGUE_250.dta", replace
	
	cap drop momid pregid 
	cap drop fatigue 
	cap drop Date
	cap drop remappdate
	cap drop remappend
	
	/*
**#Reshape wide (optional)
	
	use "$wrk/FATIGUE_250.dta", clear
	
	replace FATIGUE_INFAGE = . if FTGE_OBSTDAT < PREG_END_DATE
	gen FATIGUE_ANC20 = FATIGUE if TYPE_VISIT<=2
	replace FATIGUE_ANC20 = FATIGUE if TYPE_VISIT == 13 & FATIGUE_GA <= 180
	gen FATIGUE_ANC32 = FATIGUE if inlist(TYPE_VISIT,4,5)
	replace FATIGUE_ANC32 = FATIGUE if ///
	TYPE_VISIT == 13 & FATIGUE_GA > 180 & FTGE_OBSTDAT < PREG_END_DATE
	gen FATIGUE_PNC6 = FATIGUE if TYPE_VISIT == 10
	replace FATIGUE_PNC6 = FATIGUE if TYPE_VISIT == 14 & ///
	(FTGE_OBSTDAT - PREG_END_DATE >= 42 ) & (FTGE_OBSTDAT - PREG_END_DATE <= 104 )
	
	gen HB_ANC20 = HB_LBORRES if FATIGUE_ANC20 !=.
	gen HB_ANC32 = HB_LBORRES if FATIGUE_ANC32 !=.
	gen HB_PNC6 = HB_LBORRES if FATIGUE_PNC6 !=.
	
	gen DEPR_ANC20 = dep_sum if FATIGUE_ANC20 !=.
	gen DEPR_ANC32 = dep_sum if FATIGUE_ANC32 !=.
	gen DEPR_PNC6 = dep_sum if FATIGUE_PNC6 !=.
	
	*sort by lowest hemoglobin reading 
	bysort MOMID PREGID (HB_ANC20 FATIGUE_ANC20) : gen ANC20_sort = _n if FATIGUE_ANC20!=.
	bysort MOMID PREGID (HB_ANC32 FATIGUE_ANC32) : gen ANC32_sort = _n if FATIGUE_ANC32!=.
	bysort MOMID PREGID (HB_PNC6 FATIGUE_PNC6) : gen PNC6_sort = _n if FATIGUE_PNC6!=.
	
	gen vistype= "ANC20" if FATIGUE_ANC20 !=.
	replace vistype = "ANC32" if FATIGUE_ANC32 !=.
	replace vistype = "PNC6" if FATIGUE_PNC6 !=.
	
	keep if ANC20_sort ==1 | ANC32_sort ==1 | PNC6_sort ==1
	
	keep SITE MOMID PREGID MAT_VISIT_MNH26 HB_LBORRES DATE FTGE_AN2_R FTGE_HI7_R FTGE_HI12_R FTGE_AN1V_R FTGE_AN3_R FTGE_AN4_R FTGE_AN5 FTGE_AN7 FTGE_AN8_R FTGE_AN12_R FTGE_AN14_R FTGE_AN15_R FTGE_AN16_R FATIGUE dep_sum vistype
	
	reshape wide HB_LBORRES MAT_VISIT_MNH26 DATE FTGE_AN2_R FTGE_HI7_R FTGE_HI12_R FTGE_AN1V_R FTGE_AN3_R FTGE_AN4_R FTGE_AN5 FTGE_AN7 FTGE_AN8_R FTGE_AN12_R FTGE_AN14_R FTGE_AN15_R FTGE_AN16_R FATIGUE dep_sum , i(SITE MOMID PREGID) j(vistype) string
	
	rename *ANC20 *_ANC20
	rename *ANC32 *_ANC32
	rename *PNC6 *_PNC6
	
	merge 1:1 MOMID PREGID using "Z:\Savannah_working_files\Expected_obs-$datadate.dta", generate(expect)
	keep if expect==3
	drop expect
	cap drop _merge
	
	save "$wrk/FATIGUE_250_FINAL_WIDE.dta", replace	
	*/
	
**#merge in demographics csv

	use "$wrk/MAT_DEMOGRAPHIC", clear
	foreach var in HH_HEAD_FEMALE HH_SIZE TOILET_IMPROVED TOILET_SHARED WATER_IMPROVED PHONE_ACCESS HH_SMOKE AGE AGE18 BMI_ENROLL BMI_LEVEL BMI_INDEX GA_WKS_ENROLL SCHOOL_YRS MARRIED MARRY_STATUS MARRY_AGE HEIGHT_INDEX SINGLETON EDUCATED GRAVIDITY PRIMIGRAVIDA PARITY NULLIPAROUS PAID_WORK SMOKE CHEW_TOBACCO CHEW_BETELNUT DRINK FOLIC UNDER_NET BIRTH_FACILITY BIRTH_LOC_DECISION_MAKER NUM_FETUS MISCARRIAGE NUM_MISCARRIAGE MUAC {
	cap replace `var' = "" if `var' == "NA"
	cap destring `var', replace
	}
	sum PARITY
	return list
	local max = `r(max)' + 1
	
	egen PARITY_CAT = cut(PARITY), at (0 1 2 `max') icodes
	label define PARITY_CAT 0"0" 1"1" 2"2+"
	label val PARITY_CAT PARITY_CAT
	
	*grand parity
	egen GPARITY = cut(PARITY), at (0 1 5 `max') icodes
	label define GPARITY 0"0" 1"1-4" 2"5+", replace
	label val GPARITY GPARITY
	bigtab GPARITY PARITY
	
	gen CHEW_ANY = 0 if inlist( CHEW_TOBACCO,0,1)
	replace CHEW_ANY = 1 if CHEW_TOBACCO==1 | CHEW_BETELNUT==1
	
	bigtab PARITY_CAT PARITY

	
	merge 1:m MOMID PREGID using "$wrk/FATIGUE_250.dta", ///
	nogen force
	keep if ENROLL250==1
		
	save "$wrk/FATIGUE_250.dta", replace
	
**#Pull items from MNH04
	*bed rest, prior pregnancy loss, did mother desire current pregnancy
	import excel "$da/mnh04_merged.xlsx", ///
	sheet("Sheet 1") firstrow clear
	rename M04_* *
	
	*previous stillbirth
	gen STILLBIRTH_M04  = STILLBIRTH_RPORRES if ///
	inlist(STILLBIRTH_RPORRES,0,1)
	replace STILLBIRTH_M04 = 0 if PH_OTH_RPORRES==0
	replace STILLBIRTH_M04 = 0 if PH_PREV_RPORRES==0
	
	*Previous miscarriage
	gen MISCARRIAGE_M04 = MISCARRIAGE_RPORRES if ///
	inlist( MISCARRIAGE_RPORRES,0,1)
	replace MISCARRIAGE_M04 = 0 if PH_OTH_RPORRES==0
	replace MISCARRIAGE_M04 = 0 if PH_PREV_RPORRES==0
	
	**Bedrest
	gen BEDREST = 1 	if HTN_CMTRT_1 == 1  |  HTN_CMTRT_2 == 1
	
	//bedrest for hypertension
	replace BEDREST = 1 if HPD_OECTRT_1 == 1 |  HPD_OECTRT_2 == 1
	//bedrest for Hypertensive disorders of pregnancy
	replace BEDREST = 1 if PTL_OECTRT_1 == 1 |  PTL_OECTRT_2 == 1
	//bedrest for preterm labor risk
	label var BEDREST "Mother on bedrest - MNH04"
	
	**Did she desire the current pregnancy?
	replace CURREPREGN_DESIRE_YN = . if ///
	CURREPREGN_DESIRE_YN == 55 | CURREPREGN_DESIRE_YN == 77
	label var CURREPREGN_DESIRE_YN "Mother desired current pregnancy"
	label define CURREPREGN_DESIRE_YN 0"No" 1"Yes"
	label val CURREPREGN_DESIRE_YN CURREPREGN_DESIRE_YN
	
	sort SITE MOMID ANC_OBSSTDAT
	collapse (max) BEDREST STILLBIRTH_RPORRES MISCARRIAGE_M04 STILLBIRTH_M04 ///
	(firstnm) CURREPREGN_DESIRE_YN, by(SITE MOMID PREGID)
	save "$wrk/mnh04_collapsed.dta" , replace
	
**#Bedrest variables from MNH09 (L&D) 
	import delimited "$da/mnh09_merged.csv", ///
	bindquote(strict) case(upper) clear 
	rename M09_* *
	gen BEDREST_MNH09 = 1 if ///
	HDP_HTN_PROCCUR_1 == 1 |  HDP_HTN_PROCCUR_2 == 1
	keep SITE MOMID PREGID BEDREST_MNH09
	save "$wrk/mnh09.dta" , replace
	
**#Bedrest variables from MNH19 (Hospitalization)
	import excel "$da/mnh19_merged.xlsx", ///
	sheet("Sheet 1") firstrow clear
	rename M19_* *
	
	*Because hospitalization could occur during pregnancy or postpartum, 
		*we need to identify timing of each visit
	merge m:1 MOMID PREGID using "$outcomes/MAT_ENDPOINTS.dta",nogen
	
	gen HOSP_DATE_RECORD = OHOSTDAT
	replace HOSP_DATE_RECORD = . if HOSP_DATE_RECORD < 0
	
	gen HOSP_DATE_EST = MAT_EST_OHOSTDAT
	replace HOSP_DATE_EST = . if HOSP_DATE_EST < 0 
	
	gen HOSP_DATE = HOSP_DATE_RECORD
	replace HOSP_DATE = HOSP_DATE_EST if HOSP_DATE_RECORD == . 
	
	gen ANC= 1 if (HOSP_DATE < PREG_END_DATE) & PREG_END_DATE!=. & HOSP_DATE!=.
	replace ANC = 1 if PREG_END ==0 
	replace ANC = 1 if PREG_END ==. 
	gen PNC= 1 if (HOSP_DATE > PREG_END_DATE) & ///
	PREG_END_DATE!=. & HOSP_DATE!=.
	
	keep if ANC==1

	
	gen BEDREST_MNH19 = 1 if HPD_HTN_CMOCCUR_1 == 1 | HPD_HTN_CMOCCUR_2 == 1
	
	collapse (max) BEDREST_MNH19 , by(SITE MOMID PREGID)
	save "$wrk/mnh19.dta" , replace

**#Add MNH04/09/19 to the fatigue file	
	use "$wrk/FATIGUE_250.dta", clear
	cap drop STILLBIRTH MISCARRIAGE
	cap drop BEDREST
	merge m:1 PREGID using "$wrk/mnh04_collapsed.dta",nogen force
	keep if ENROLL250==1
	
	merge m:1 PREGID using "$wrk/mnh09.dta",nogen force
	keep if ENROLL250==1
	
	merge m:1 PREGID using "$wrk/mnh19.dta",nogen force
	keep if ENROLL250==1
	count
	
	*create a summary variable of bedrest:
	rename BEDREST BEDREST_MNH04
	gen BEDREST = 0
	replace BEDREST = 1 if ///
	BEDREST_MNH04==1 | BEDREST_MNH09==1 | BEDREST_MNH19==1
	
	by PREGID (DATE ) , sort: gen VISITNUM_ALL = _n if FATIGUE>=0
	
**#Add hemorrhage and labor:	

	
	merge m:1 MOMID PREGID using "$wrk/MAT_HEMORRHAGE.dta", ///
	keepusing(HEM_PPH HEM_PPH_SEV) generate(pph)
	keep if ENROLL250==1
	
	merge m:1 MOMID PREGID using "$wrk/MAT_LABOR.dta", ///
	generate(labor)
	keep if ENROLL250==1
	
	*Set missing "." to "55" to denote missingness
	foreach var in MARRIED EDUCATED PARITY_CAT GPARITY BMI_LEVEL MISCARRIAGE_M04 STILLBIRTH_M04 PAID_WORK CHEW_ANY DRINK HB10 BEDREST CES_ANY HEM_PPH HEM_PPH_SEV CURREPREGN_DESIRE_YN {
		replace `var' = 55 if `var'==.
	}
	
	
	label var MARRIED "Married/cohabiting"
	label var AGE "Age at enrolment"
	label var EDUCATED "Ever attended school"
	label var PARITY_CAT "Parity"
	label var GPARITY "Grand parity"
	label var MISCARRIAGE_M04 "Previous miscarriage"
	label var STILLBIRTH_M04 "Previous stillbirth"
	label var BMI_LEVEL "BMI category at enrolment"
	label define BMI_LEVEL ///
	1"Underweight" 2"Normal" 3"Overweight" 4"Obese" 55"Missing",replace
	label val BMI_LEVEL BMI_LEVEL

	label var PAID_WORK "Has paid work"
	label var CHEW_ANY "Chews tobacco or betel nut"
	label var DRINK "Drink alcohol"
	label var CURREPREGN_DESIRE_YN "Desired current pregnancy"
	label var HB10 "Hemoglobin < 10 g/dL"
	label var BEDREST "On bedrest"
	label var CES_ANY "Cesarean section"
	label var HEM_PPH "Postpartum hemorrhage"
	label var HEM_PPH_SEV "Severe postpartum hemorrhage"

	label define yesnomiss 1"Yes" 0"No" 55"Missing", replace
	label val MARRIED EDUCATED MISCARRIAGE_M04 STILLBIRTH_M04 CHEW_ANY DRINK BEDREST CES_ANY HEM_PPH HEM_PPH_SEV CURREPREGN_DESIRE_YN PAID_WORK yesnomiss
	
	gen FATIGUE_GA_WK =round(FATIGUE_GA/7)
	
	save "$wrk/FATIGUE_250.dta", replace
