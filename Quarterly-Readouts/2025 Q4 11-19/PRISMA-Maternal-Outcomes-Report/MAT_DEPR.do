**MNH25 - Perinatal Depression**
**Savannah O'Malley (savannah.omalley@gwu.edu)


***********************************
******Table of Contents************
/*
1. Set directories and import data
2. Basic data cleaning
3. Code analytical variables for long and collapsed data sets
4. Code analytical variables for Maternal Outcomes report data set
*/

/*
*NOTE: THIS FILE REQUIRES:

"$outcomes/MAT_ENROLL.dta"
"$outcomes/MAT_ENDPOINTS.dta"
"Z:\Savannah_working_files\Expected_obs.dta"
*/

*Output: MAT_DEPR.dta

***********************************
**#Part 1: Directories and data import
***********************************
* Update each session:
global datadate "2025-10-31"
//update with data date in format YYYY-MM-DD

global runqueries = 0
//set this depending on whether you want to run queries or no

*******************************************************************
global da "Z:\Stacked Data/$datadate"
global outcomes "Z:\Outcome Data/$datadate"


global wrk "Z:\Savannah_working_files\depression/$datadate"
// make sure this is a secure location as we will save data files here
cap mkdir "$wrk"
//This will make the folder if it does not yet exist
cd "$wrk"

*Save queries:
global queries "Z:\Savannah_working_files\depression/$datadate\queries"
	cap mkdir "$queries" //create the folder if it doesn't exist
	//save query reports here

	*Get today's date:
local date: di %td_CCYY_NN_DD daily("`c(current_date)'", "DMY")
global today = subinstr(strltrim("`date'"), " ", "-", .)
*******************************************************************

**import data - stacked file
clear
import delimited "$da/mnh25_merged.csv", bindquote(strict) case(upper) 
rename M25_* *
	*remove the M25_ prefix

**************************************************
**#Part 2: Basic Data cleaning
**************************************************
label define TYPE_VISIT ///
	1"Enrollment" 	///
	2"ANC-20" 		///
	3"ANC-28" 		///
	4"ANC-32" 		///
	5"ANC-36" 		///
	6"IPC (L&D)" 	///
	7"PNC-0" 		///
	8"PNC-1" 		///
	9"PNC-4" 		///
	10"PNC-6" 		///
	11"PNC-26" 		///
	12"PNC-52" 		///
	13"Non-scheduled visit for routine care" 		///
	14"Non-scheduled PNC visit for routine care"	
label val TYPE_VISIT TYPE_VISIT


label define MAT_VISIT 					///
	1"Yes, visit conducted in person" 	///
	2"Yes, visit conducted by phone" 	///
	3"No, woman unable to complete visit due to medical issue" 	///
	4"No, woman temporarily absent" 	///
	5"No, woman temporarily refused" 	///
	6"No, woman permanently moved out of study area" 			///
	7"No, woman withdrew consent" 		///
	8"No, woman died" 					///
	88"Other, specify" 					///
	77"Not applicable" 					///
	55"Missing" 						///
	66"Refused to answer" 				///
	99"Don't know"						
label val MAT_VISIT_MNH25 MAT_VISIT
label var MAT_VISIT_MNH25 "Was the visit completed?"

	**#Query: duplicates by date
gen DATE = date(OBSSTDAT, "YMD")
format DATE %td

duplicates tag MOMID PREGID OBSSTDAT if MAT_VISIT_MNH25 <=2, gen(date_dup)
	*there should not be duplicates on the same date
	
	*The below will export queries by site if duplicates on same date
if $runqueries == 1 {
	
		levelsof(SITE) if date_dup > 0 & date_dup <., local(sitelev) clean
		foreach site of local sitelev {
			export excel SITE MOMID PREGID OBSSTDAT MAT_VISIT_MNH25 TYPE_VISIT  using "$queries/`site'-depression-queries-$datadate.xlsx"  if SITE=="`site'" & (date_dup > 0 & date_dup <. ), sheet("Duplicates",modify)  firstrow(variables) 
			disp as result "`site' complete"
		}
		
}

bysort MOMID PREGID OBSSTDAT (TYPE_VISIT) : gen visnum = _n
drop if visnum != 1 & date_dup > 0 & date_dup < . 
drop date_dup visnum

*Check if all merge with the enrolled file 

merge m:1 MOMID PREGID using "$outcomes/MAT_ENROLL.dta", gen(checkenroll) keepusing(SITE ENROLL PREG_START_DATE)
assert SITE != ""

str2date   FORMCOMPLDAT_MNH25 
	**str2date is a user-defined function to convert strings to dates
	*see str2date.ado on Savannah's Github, or convert to date on your own
str2date   PREG_START_DATE

	**#Query: not enrolled
if $runqueries == 1 {
	
		levelsof(SITE) if ENROLL == ., local(sitelev) clean
		foreach site of local sitelev {
			export excel SITE MOMID PREGID OBSSTDAT   using "$queries/`site'-depression-queries-$datadate.xlsx"  if SITE=="`site'" & ENROLL == ., sheet("Not-enrolled",modify)  firstrow(variables) 
			disp as result "`site' complete"
		}
		
}

drop if ENROLL == . 
drop checkenroll 

merge m:1 MOMID PREGID using "$outcomes/MAT_ENDPOINTS.dta", nogen keepusing(SITE PREG_END PREG_END_DATE)
assert SITE != ""

gen depr_ga = DATE - PREG_START_DATE
replace depr_ga = . if ///
	DATE < 0 | PREG_START_DATE < 0 | inlist(TYPE_VISIT,10,14)
replace depr_ga = . if MAT_VISIT_MNH25 >=3	
	
gen depr_pp = DATE - PREG_END_DATE
replace depr_pp = . if ///
		DATE < 0 | PREG_END_DATE < 0 | inlist(TYPE_VISIT,1,2,3,4,5,13)
replace depr_pp = . if MAT_VISIT_MNH25 >=3

	*GAs < 28 or > 310 should be queried
gen outofrange = 1 if  depr_ga < 28 | (depr_ga > 310 & depr_ga <.)
replace outofrange = 1 if depr_pp < 42 | ///
	(depr_pp > 104 & depr_pp <. )
assert outofrange == . if MAT_VISIT_MNH25 >= 3
count if outofrange== 1
	
	**#Query: Unexpected visit types
	if $runqueries == 1 {
		*query for unexpected visit types
		levelsof(SITE) if inlist(TYPE_VISIT,6,7,8,9,11,12), local(sitelev) clean
		foreach site of local sitelev {
			export excel SITE MOMID PREGID TYPE_VISIT OBSSTDAT   using "$queries/`site'-depression-queries-$datadate.xlsx"  if SITE=="`site'" & inlist(TYPE_VISIT,6,7,8,9,11,12), sheet("Unexpected visit type",modify)  firstrow(variables) 
			disp as result "`site' complete"
		}
		
}
	
	drop if inlist(TYPE_VISIT,6,7,8,9,11,12)
	**#Query: GA out of range
	if $runqueries == 1 {
		*query for GA out of range
		levelsof(SITE) if outofrange == 1, local(sitelev) clean
		foreach site of local sitelev {
			export excel SITE MOMID PREGID TYPE_VISIT OBSSTDAT PREG_START_DATE  using "$queries/`site'-depression-queries-$datadate.xlsx"  if SITE=="`site'" & outofrange == 1, sheet("Out of range",modify)  firstrow(variables) 
			disp as result "`site' complete"
		}
		
}

drop if outofrange == 1
drop outofrange

gen 	 ga_check = 1 if TYPE_VISIT==1 & !inrange(depr_ga,28,181)
replace  ga_check = 1 if TYPE_VISIT==2 & !inrange(depr_ga,28,181)
replace  ga_check = 1 if TYPE_VISIT==3 & !inrange(depr_ga,182,216)
replace  ga_check = 1 if TYPE_VISIT==4 & !inrange(depr_ga,217,310)
replace  ga_check = 1 if TYPE_VISIT==5 & !inrange(depr_ga,238,310)
replace  ga_check = 1 if TYPE_VISIT ==10 & !inrange(depr_pp,42,104)
replace  ga_check = . if MAT_VISIT_MNH25 >= 3
tab 	 ga_check SITE


	gen ga_check_note = "GW has no DOB on file" if ///
	PREG_END_DATE == . & inlist(TYPE_VISIT,10,14)
	replace ga_check_note = "Before PREG_END_DATE" if ///
	depr_pp <0 & inrange(TYPE_VISIT, 10,14) & ga_check == 1 
	
	**anc windows:
	replace ga_check_note = ///
	"GW records indicate this visit falls during ANC20" if ///
	inrange(depr_ga,28,181) & ga_check == 1 & ga_check_note == ""
	
	replace ga_check_note = ///
	"GW records indicate this visit falls during ANC28" if ///
	inrange(depr_ga,182,216) & ga_check == 1 & ga_check_note == ""

	replace ga_check_note = ///
	"GW records indicate this visit falls during ANC32" if ///
	inrange(depr_ga,217,237) & ga_check == 1 & ga_check_note == ""
	
	replace ga_check_note = ///
	"GW records indicate this visit falls during ANC36" if ///
	inrange(depr_ga,238,310) & ga_check == 1 & ga_check_note == ""
	
	**postpartum windows:
	replace ga_check_note = "Too early < 42 days pp" if ///
	inrange(depr_pp,0,41) & TYPE_VISIT == 10 & ga_check == 1  & ga_check_note == ""
	replace ga_check_note = "Too late > 104 days pp" if ///
	inrange(depr_pp,105,400) & TYPE_VISIT == 10 & ga_check == 1  & ga_check_note == ""
	
	replace ga_check_note = ///
	"GW records indicate this visit falls during PNC" if ///
	inrange(depr_pp,0,1000) & ga_check == 1 & !inlist(TYPE_VISIT,10,14) & ga_check_note == ""
	
	replace ga_check_note = "No visit date" if ///
	(DATE == . | DATE < 0 ) & ga_check == 1 & ga_check_note == ""

	**#Query: export IDs with wrong visit window/dates:
	if $runqueries == 1 {	
	
		
	preserve
	sort TYPE_VISIT MOMID
	levelsof(SITE) if ga_check==1, clean local(sitelev)
		foreach site of local sitelev  {
		
	export excel SITE MOMID PREGID TYPE_VISIT DATE FORMCOMPLDAT_MNH25 PREG_START_DATE depr_ga ga_check_note  using "$queries/`site'-depression-queries-$datadate.xlsx" if (ga_check==1 | ga_check_note != "")  &  SITE == "`site'", ///
	sheet("type visit error", modify)  firstrow(variables) 
	disp as result "`site' completed"
	}
	
	restore
	}
	
	drop if ga_check == 1
	
	drop ga_check
	drop ga_check_note
	

***********************************	
**# Part 3: Code depression score
***********************************	
foreach var in EPDS0101 EPDS0102 EPDS0103 EPDS0104 EPDS0105 EPDS0106 EPDS0107 EPDS0108 EPDS0109 EPDS0110 {
	gen `var'_R = `var'
	**this is a loop for each variable before the bracket { 
	**creates a "_recode" variable so we can make changes but preserve the original data 
}

/*
foreach var in m25_epds0103_recode m25_epds0106_recode m25_epds0107_recode ///
 m25_epds0108_recode m25_epds0109_recode m25_epds0110_recode {
	replace `var'="77" if `var'=="NA"
	**replace with numbers to allow for destring
	**note that SITEs should only be using 77
	destring `var' , replace
}
*/
	**#Codes for India, Pakistan, and Zambia
**CHANGE SCORES FOR INDIA CMC AND SAS, PAKISTAN AND ZAMBIA - BASED ON CRF
foreach var in EPDS0101_R EPDS0102_R EPDS0104_R {
	recode `var' (1=0) (2=1) (3=2) (4=3) (77=.) (55=.) if SITE!="Ghana" & SITE!="Kenya"
	**change score so that the  option #1 == score of 0,  option #2== score of 1, etc.
	**question 1,2,4 are positively worded
	**Ghana and Kenya are each a special case
	**NOTE that this DROPS any observations ==77, may need to update code for query report
}

foreach var in EPDS0103_R EPDS0105_R EPDS0106_R EPDS0107_R EPDS0108_R EPDS0109_R EPDS0110_R {
	recode `var' (1=3) (2=2) (3=1) (4=0) (77=.) (55=.) if SITE!="Ghana"  & SITE!="Kenya"
	**change score so that the 1 option == score of 3, 2 option == score of 2, etc.
	*questions 3, 5-10 are negatively worded
}


******************************************************
	**# Kenya-specific code
**all questions in the scale are coded the same 

**SITES: DO NOT RUN IF SITE ! = "Kenya"
foreach var in EPDS0101_R EPDS0102_R EPDS0103_R EPDS0104_R EPDS0105_R EPDS0106_R EPDS0107_R EPDS0108_R EPDS0109_R EPDS0110_R {
	recode `var' (1=0)  (2=1) (3=2) (4=3) (77=.) (55=.) (777=.) if SITE=="Kenya"
	**change score so that the option #1 == score of 0,  option #2 == score of 1, etc.
}

**END Kenya- Specific code
******************************************************

******************************************************
	**# Ghana-specific code
*Ghana's  questions are asked differently, see CRF and data dictionary
**SITES: DO NOT RUN IF SITE ! = "Ghana"
**Questions 1-2 which are written in a positive way:
	**"1. In the past 7 days I have been able to laugh..."
	**"2. In the past 7 days I have looked forward with enjoyment to things"
	**THEREFORE, if participant responded "no" --> indicates symptom of depression and should be given a higher score
	**a participant responded "yes" --> no symptom of depression, lower score
foreach var in EPDS0101 EPDS0102  {
	replace `var'_R=. if SITE=="Ghana"
	*original recode is wrong, drop it
	replace `var'_R=3 if `var'_N ==2 & SITE=="Ghana"
	*no, not at all (positive question)
	replace `var'_R=2 if `var'_N ==1 & SITE=="Ghana"
	*no, not very often (positive question)
	replace `var'_R=1 if `var'_Y ==1 & SITE=="Ghana"
	*yes, some of the time (positive question)
	replace `var'_R=0 if `var'_Y ==2 & SITE=="Ghana"
	*yes, most of the time (positive question)
	replace `var'_R=. if `var' ==77 | `var' ==55
}

**Questions #3-10 are written in a negative way
	**THEREFORE, a "no" response indicates the participant does not report this depression symptom --> should be given a LOWER score
	**a "yes" indicates participant reported a depression symptom--> coded a HIGHER score 
	**e.g., Question #3 "In the past 7 days I have blamed myself unnecessarily when things went wrong"
foreach var in EPDS0103 EPDS0104 EPDS0105 EPDS0106 EPDS0107 EPDS0108 EPDS0109 EPDS0110 {
	replace `var'_R=. if SITE=="Ghana"
	*original recode is wrong, drop it
	replace `var'_R=3 if `var'_Y==2 & SITE=="Ghana"
	*yes, most of the time (yes to a negative question)
	replace `var'_R=2 if `var'_Y==1 & SITE=="Ghana"
	*yes, some of the time (yes to a negative question)
	replace `var'_R=1 if `var'_N==1 & SITE=="Ghana"
	*no, not very often (no to a negative question)
	replace `var'_R=0 if `var'_N==2 & SITE=="Ghana"
	*no, not at all (no to a negative question)
}

**END GHANA-SPECIFIC CODE**
***************************************

	**#Generate summary variable and label responses
*Label the responses
**note that the responses are not the same across sites, but 3--> higher frequency/worse intensity of depression symptoms
label define depression_responses ///
0"0, Less/infrequent symptom" 3"3, Worse/frequent symptom"
label val EPDS0101_R EPDS0102_R EPDS0103_R EPDS0104_R EPDS0105_R EPDS0106_R EPDS0107_R EPDS0108_R EPDS0109_R EPDS0110_R depression_responses

 egen Q_ANSWERED=anycount( EPDS0101_R EPDS0102_R EPDS0103_R EPDS0104_R EPDS0105_R EPDS0106_R EPDS0107_R EPDS0108_R EPDS0109_R EPDS0110_R), values(0 1 2 3)
 //this codes the number of questions that were answered 
 
**GEN SUMMARY VARIABLE
egen dep=rowtotal( EPDS0101_R EPDS0102_R EPDS0103_R EPDS0104_R EPDS0105_R EPDS0106_R EPDS0107_R EPDS0108_R EPDS0109_R EPDS0110_R) , missing 
gen epds_score = round(((dep/Q_ANSWERED)*10),0.1)

gen selfharm_risk = 1 if inrange( EPDS0110_R,2,3)
replace selfharm_risk = 0 if inrange( EPDS0110_R,0,1)
label var selfharm_risk "Participant screened for self-harm risk according to Q10"

	**#query: missing depression score
gen QUERY_MISS_DEPSCORE=1 if epds_score==. 
label var QUERY_MISS_DEPSCORE "Missing depression score"

	**#QC check if sum score matches the auto-calculated score
gen dep_check = epds_score - EPDS01_SCORRES if EPDS01_SCORRES < 55
gen EPDS01_SCORRES_MISS = 1 if ///
	inlist(EPDS01_SCORRES,55,66,77,88,99) & inrange(Q_ANSWERED,1,10)


gen QUERY_SCORE_DIFFER = 1 if dep_check!=0 & dep_check!=.
label var QUERY_SCORE_DIFFER "Manual differs auto score depression"

	**#query: depression score differs from auto-calculated
if $runqueries == 1 {
	
		levelsof(SITE) if QUERY_SCORE_DIFFER == 1, local(sitelev) clean
		foreach site of local sitelev {
			export excel SITE MOMID PREGID OBSSTDAT Q_ANSWERED dep_check   using "$queries/`site'-depression-queries-$datadate.xlsx"  if SITE=="`site'" & QUERY_SCORE_DIFFER == 1, sheet("Differ-Dep-Score",modify)  firstrow(variables) 
			disp as result "`site' complete"
		}
		
	**#Query: Missing score but questions are answered:	
		levelsof(SITE) if EPDS01_SCORRES_MISS == 1, local(sitelev) clean
		foreach site of local sitelev {
		
		export excel SITE MOMID PREGID OBSSTDAT Q_ANSWERED EPDS01_SCORRES   using "$queries/`site'-depression-queries-$datadate.xlsx"  if SITE=="`site'" & EPDS01_SCORRES_MISS == 1, sheet("Missing-Score", modify)  firstrow(variables) 
			disp as result "`site' complete"
		}
		
}


	tabstat dep_check if dep_check!=0 & dep_check!=., ///
	by(SITE) statistics(count min max)
**note discrepancies by SITE



**NOTE that India-CMC administered the form differently before and after Dec 8, 2023
**create a variable for before and after
**SITES: DO NOT RUN IF SITE ! = "India-CMC"
gen cmc_admin=0 if OBSSTDAT<"2023-12-08" & SITE=="India-CMC"
replace cmc_admin=1 if OBSSTDAT>="2023-12-08" & SITE=="India-CMC"
label define cmc_admin 0"self-administered" 1"staff-administered"
label val cmc_admin cmc_admin
label var cmc_admin "CMC self- or staff- administered; changed Dec 8 2023"
	*Note that the distribution of values by assessment differs substantially

gen 		DEPR_STND = -5 
replace 	DEPR_STND = -2 if MAT_VISIT_MNH25>=3
			*visit not completed
replace 	DEPR_STND = -1 if MAT_VISIT_MNH25<=2 & epds_score==.
			*visit completed, but no EPDS score
replace 	DEPR_STND = 0 if epds_score != . 
replace 	DEPR_STND = 1 if inrange(epds_score, 11,30)
label var 	DEPR_STND "Standard score"
	
gen 		DEPR_SITE = -5 
replace 	DEPR_SITE = -2 if MAT_VISIT_MNH25>=3
			*visit not completed
replace 	DEPR_SITE = -1 if MAT_VISIT_MNH25<=2 & epds_score==.
			*visit completed, but no EPDS score
replace 	DEPR_SITE = 0 if epds_score != . 
replace 	DEPR_SITE = 1 if ///
			SITE=="Ghana" 		& inrange(epds_score,11,.) | ///
			SITE=="India-CMC" 	& inrange(epds_score,8,.)  | ///
			SITE=="India-SAS" 	& inrange(epds_score,10,.) | ///
			SITE=="Kenya" 		& inrange(epds_score,13,.) | ///
			SITE=="Pakistan" 	& inrange(epds_score,14,.) | ///
			SITE=="Zambia" 		& inrange(epds_score,10,.)  	

	notes : Data date: $datadate TS
	label data "Depression form, all $datadate data, long"
	
	keep SITE MOMID PREGID DATE TYPE_VISIT depr_ga depr_pp epds_score selfharm_risk DEPR_STND DEPR_SITE 
	save "$wrk/MNH25.dta", replace
	drop if epds_score==.
	bysort MOMID PREGID DATE (TYPE_VISIT) : gen VISNUM=_n
	keep if VISNUM==1
	drop VISNUM
	rename *, upper
	label data "Depression form, nonmissing $datadate data, long"
	save "$wrk/MAT_DEPR_LONG.dta", replace

**COLLAPSE to get one row per participant
 use "$wrk/MAT_DEPR_LONG.dta",clear
 
 **Worst depression score date
	gsort PREGID -EPDS_SCORE +DATE
		*sorts EPDS score in descending order (by PREGID)
		*if two entries with same score, sort ascending by date
	by PREGID : gen worst = _n if  inrange(EPDS_SCORE,0,.)
	gen DEPR_SCORE_WORST_DT = DATE if worst == 1 & inrange(EPDS_SCORE, 0,.)
	gen DEPR_SCORE_WORST = EPDS_SCORE if worst == 1 & inrange(EPDS_SCORE, 0,.)
	sort SITE PREGID DATE
	*browse SITE PREGID TYPE_VISIT DEPR_SCORE_WORST epds_score worst
	drop worst 
	
	*worst depression score date in ANC
	gen ANC = 1 if inrange(DEPR_GA,28,310) & inrange(EPDS_SCORE,0,.)
	gsort PREGID ANC -EPDS_SCORE +DATE
	by PREGID : gen worst_anc = _n if ANC == 1
	gen DEPR_SCORE_WORST_ANC_DT = DATE if worst_anc == 1
	format DEPR_SCORE_WORST_ANC_DT %td
	gen DEPR_SCORE_WORST_ANC = EPDS_SCORE if worst_anc == 1
	sort SITE PREGID DATE
	*browse SITE PREGID TYPE_VISIT DEPR_SCORE_WORST DEPR_SCORE_WORST_ANC epds_score worst_anc
	drop worst_anc
	
	*first time woman screened for depression? (standard cutoff)
	*gen DEPR_STND = 1 if inrange(epds_score, 11,.)
	gsort PREGID +DATE 
	bysort PREGID (DEPR_STND DATE) : gen first_depr = _n if DEPR_STND == 1
	gen DEPR_STND_FIRST_DT = DATE if first_depr == 1
	drop first_depr
	
	*first time woman screened for depression? (site cutoff)
	
	gsort PREGID -DEPR_SITE +DATE
	by PREGID : gen first_depr = _n if DEPR_SITE == 1
	gen  DEPR_SITE_FIRST_DT  = DATE if first_depr == 1
	drop first_depr 
	
	sort SITE PREGID DATE
	
	keep SITE MOMID PREGID DATE TYPE_VISIT DEPR_GA DEPR_PP EPDS_SCORE DEPR_STND DEPR_SITE DEPR_SCORE_WORST_DT DEPR_SCORE_WORST DEPR_SCORE_WORST_ANC_DT DEPR_SCORE_WORST_ANC DEPR_STND_FIRST_DT DEPR_SITE_FIRST_DT
	
	save "$wrk/MAT_DEPR_LONG.dta", replace
	
	**# Create variables for the report
**Enroll/ANC-20
gen 		DEPR_STND_ANC20 = DEPR_STND if TYPE_VISIT <= 2
label var 	DEPR_STND_ANC20 "Standard cutoff"

gen 		DEPR_SITE_ANC20 = DEPR_SITE if TYPE_VISIT <= 2
label var 	DEPR_SITE_ANC20 "SITE-specific cutoff"

gen 		DEPR_SCORE_ANC20 = EPDS_SCORE if TYPE_VISIT<=2

**ANC-32/ANC-36
gen 		DEPR_STND_ANC32 = DEPR_STND if inlist(TYPE_VISIT, 4,5)
label var 	DEPR_STND_ANC32 "Standard cutoff"

gen 		DEPR_SITE_ANC32 = DEPR_SITE if inlist(TYPE_VISIT, 4,5)
label var 	DEPR_SITE_ANC32 "SITE-specific cutoff"

gen 		DEPR_SCORE_ANC32 = EPDS_SCORE if inlist(TYPE_VISIT, 4,5)

**PNC-6
gen 		DEPR_STND_PNC6 = DEPR_STND if TYPE_VISIT==10 
label var 	DEPR_STND_PNC6 "Standard cutoff"

gen 		DEPR_SITE_PNC6 = DEPR_SITE if TYPE_VISIT==10
label var 	DEPR_SITE_PNC6 "SITE-specific cutoff"

gen 		DEPR_SCORE_PNC6 = EPDS_SCORE if TYPE_VISIT==10 
	
**We want to keep only the highest score
collapse (max) DEPR_STND_ANC20 DEPR_SITE_ANC20 DEPR_SCORE_ANC20 ///
			DEPR_STND_ANC32 DEPR_SITE_ANC32 DEPR_SCORE_ANC32 ///
			DEPR_STND_PNC6 DEPR_SITE_PNC6 DEPR_SCORE_PNC6 ///
		(firstnm) DEPR_SCORE_WORST DEPR_SCORE_WORST_DT ///
			DEPR_SCORE_WORST_ANC_DT DEPR_SCORE_WORST_ANC ///
			DEPR_STND_FIRST_DT DEPR_SITE_FIRST_DT , ///
		by(SITE MOMID PREGID)
 
  
notes replace _dta in 1 : worst depression score for $datadate data TS

save "$wrk/mnh25_collapsed.dta", replace


**************************************************
**# Part 4: Analytical variables for maternal outcomes data set
**************************************************
	
	use "$outcomes/MAT_ENROLL.dta", clear
	keep SITE MOMID PREGID ENROLL
	merge 1:1 MOMID PREGID using "$wrk/mnh25_collapsed.dta", nogen
	keep if ENROLL == 1


	merge 1:1 MOMID PREGID using "Z:\Savannah_working_files\Expected-obs\Expected_obs-$datadate.dta", nogen

	assert SITE != ""
		//check not missing SITE for any observations

		
	**#ANC-20
	**denominator for data completeness table
	gen DEPR_ANC20_EXP = ANC20_EXP
		*ANC20_EXP is based on the woman's visit window & her closeout date
	*Note one issue: Ghana started testing depression late
	*If a woman's ANC20 late visit window passed before the depression start date, set that woman as 'not expected'
	
	replace DEPR_ANC20_EXP = 0 if ANC20_LATE_WINDOW < date("2023-06-15", "YMD") & SITE == "Ghana"
	gen DEPR_MISS_ANC20_DENOM = 1 if DEPR_ANC20_EXP ==1 | inrange(DEPR_STND_ANC20,0,1)
		**those expected and/or have data
	gen DEPR_ANC20_DENOM = 1 if ///
	(DEPR_STND_ANC20 ==0 | DEPR_STND_ANC20==1) 
	label var DEPR_ANC20_DENOM ///
	"Denominator of those who have valid depression score for ANC20"
	**NUMERATORS
	gen DEPR_STND_ANC20_NUM = 1 if DEPR_STND_ANC20==1 
	label var DEPR_STND_ANC20_NUM ///
	"Numerator of those screening for depression at ANC20, Standard cutoff"
	gen DEPR_SITE_ANC20_NUM = 1 if DEPR_SITE_ANC20==1 
	label var DEPR_SITE_ANC20_NUM ///
	"Numerator of those screening for depression at ANC20, SITE cutoff"
	
	**MISSING
	gen DEPR_MISS_ANC20 =DEPR_STND_ANC20 if DEPR_STND_ANC20<0
	replace DEPR_MISS_ANC20 = -2 if DEPR_ANC20_EXP ==1 & DEPR_STND_ANC20==.
		*-2 is visit not completed; we will code this if she's expected but there's no score for her
	replace DEPR_MISS_ANC20 = . if DEPR_MISS_ANC20_DENOM !=1
		*if no data and not expected, we don't care about missingness reason
	label define MISS -2"Visit not completed" -1"No summary score"
	label val DEPR_MISS_ANC20 MISS



	**#ANC-32

	* "full" denominator for missingness table in the report
	gen DEPR_ANC32_EXP = ANC32_EXP
	replace DEPR_ANC32_EXP = 0 if ANC36_LATE_WINDOW < date("2023-06-15", "YMD") & SITE == "Ghana"
	gen DEPR_MISS_ANC32_DENOM =1 if DEPR_ANC32_EXP == 1 | inrange(DEPR_STND_ANC32,0,1)
		**have data and/or are expected
		
	*denominator of those with valid data
	gen DEPR_ANC32_DENOM = 1 if ///
	(DEPR_STND_ANC32 == 0 | DEPR_STND_ANC32 == 1) 
	label var DEPR_ANC32_DENOM ///
	"Denominator of those who have valid depression score for ANC32"
	**NUMERATORS
	gen DEPR_STND_ANC32_NUM =1 if ///
	DEPR_STND_ANC32 ==1 
	label var DEPR_STND_ANC32_NUM ///
	"Numerator of those screening for depression at ANC32, standard cutoff"
	gen DEPR_SITE_ANC32_NUM =1 if ///
	DEPR_SITE_ANC32 ==1 
	label var DEPR_SITE_ANC32_NUM ///
	"Numerator of those screening for depression at ANC32, SITE cutoff"
	**MISSING
	gen DEPR_MISS_ANC32 = DEPR_STND_ANC32 if DEPR_STND_ANC32<0
	replace DEPR_MISS_ANC32 = -2 if  DEPR_ANC32_EXP == 1 & DEPR_STND_ANC32==.
	replace  DEPR_MISS_ANC32=. if DEPR_MISS_ANC32_DENOM !=1
		*if no data and not expected, we don't care about missingness reason
	label val DEPR_MISS_ANC32 MISS
	

	**#PNC-6
	**DENOMINATOR
	*full denominator for the report
	gen DEPR_PNC6_EXP = PNC6_EXP
	replace DEPR_PNC6_EXP = 0 if SITE == "Ghana" & PNC6_LATE_WINDOW < date("2023-06-15", "YMD")
	gen DEPR_MISS_PNC6_DENOM = 1 if PNC6_EXP == 1 | inrange(DEPR_STND_PNC6,0,1)
		// those who have data or are expected
	
	*denominator of those who have data
	gen DEPR_PNC6_DENOM = 1 if ///
	(DEPR_STND_PNC6 == 0 | DEPR_STND_PNC6 == 1)
	label var DEPR_PNC6_DENOM ///
	"Denominator of those who have valid depression scores for PNC6"
	**NUMERATORS
	gen DEPR_STND_PNC6_NUM = 1 if DEPR_STND_PNC6 == 1 
	label var DEPR_STND_PNC6_NUM ///
	"Numerator of those screening for depression at PNC6, standard cutoff"
	gen DEPR_SITE_PNC6_NUM = 1 if DEPR_SITE_PNC6 == 1 
	label var DEPR_SITE_PNC6_NUM ///
	"Numerator of those screening for depression at PNC6, SITE cutoff"
	**MISSING 
	gen DEPR_MISS_PNC6 = DEPR_STND_PNC6 if DEPR_STND_PNC6 < 0
	replace DEPR_MISS_PNC6 = -2 if PNC6_EXP ==1 & DEPR_STND_PNC6==.
	replace DEPR_MISS_PNC6 = . if DEPR_MISS_PNC6_DENOM!=1
	label val DEPR_MISS_PNC6 MISS


	**#ANC-ever
	gen DEPR_ANC_EVER_DENOM =1 if ///
	DEPR_ANC20_DENOM ==1 | ///
	DEPR_ANC32_DENOM ==1
	label var DEPR_ANC_EVER_DENOM ///
	"Denominator of those with a valid depression score at ANC20 or 32"
	gen DEPR_STND_ANC_EVER_NUM = 1 if ///
	 DEPR_STND_ANC20_NUM ==1 |  DEPR_STND_ANC32_NUM ==1
	 label var DEPR_STND_ANC_EVER_NUM ///
	 "Numerator of those screening for depression (stnd score) at ANC20 or 32"
	gen DEPR_SITE_ANC_EVER_NUM = 1 if ///
	 DEPR_SITE_ANC20_NUM ==1 | DEPR_SITE_ANC32_NUM==1
	 label var DEPR_SITE_ANC_EVER_NUM ///
	 "Numerator of those screening for depression(site score) at ANC20 or 32"


	**#Depression- ever
	gen DEPR_EVER_DENOM = 1 if ///
	DEPR_ANC20_DENOM ==1 | DEPR_ANC32_DENOM==1 | DEPR_PNC6_DENOM==1
	label var DEPR_EVER_DENOM ///
	"Denominator of any who have a valid depression score, any time point"
	gen DEPR_STND_EVER_NUM = 1 if ///
	DEPR_STND_ANC_EVER_NUM == 1 | DEPR_STND_PNC6_NUM == 1
	label var DEPR_STND_EVER_NUM ///
	"Numerator of any who ever screened for possible depression, std cutoff"
	gen DEPR_SITE_EVER_NUM = 1 if ///
	DEPR_SITE_ANC_EVER_NUM == 1 | DEPR_SITE_PNC6_NUM == 1
	label var DEPR_SITE_EVER_NUM ///
	"Numerator of any who ever screened for possible depression, SITE cutoff"




*drop SITE DEPR_STND_ANC20 DEPR_SITE_ANC20 DEPR_STND_ANC32 DEPR_SITE_ANC32 DEPR_STND_PNC6 DEPR_SITE_PNC6
order DEPR_SCORE_ANC32, after( DEPR_MISS_ANC20)
order DEPR_SCORE_PNC6, after( DEPR_MISS_ANC32)
foreach var in DEPR_STND_ANC20 DEPR_SITE_ANC20 DEPR_STND_ANC32 DEPR_SITE_ANC32 DEPR_STND_PNC6 DEPR_SITE_PNC6 {
	replace `var'=55 if `var' == -2 | `var' == -1
}

save "$wrk/mnh25_MaternalOutcomes.dta", replace

**Finally, save a smaller dataset for the outcomes folder

keep SITE MOMID PREGID DEPR_STND_ANC20 DEPR_SITE_ANC20 DEPR_SCORE_ANC20 DEPR_STND_ANC32 DEPR_SITE_ANC32 DEPR_STND_PNC6 DEPR_SITE_PNC6  DEPR_ANC20_DENOM DEPR_STND_ANC20_NUM DEPR_SITE_ANC20_NUM DEPR_MISS_ANC20 DEPR_SCORE_ANC32 DEPR_MISS_ANC20_DENOM  DEPR_ANC32_DENOM DEPR_STND_ANC32_NUM DEPR_SITE_ANC32_NUM DEPR_MISS_ANC32 DEPR_SCORE_PNC6 DEPR_MISS_ANC32_DENOM  DEPR_PNC6_DENOM DEPR_STND_PNC6_NUM DEPR_SITE_PNC6_NUM DEPR_MISS_PNC6 DEPR_MISS_PNC6_DENOM DEPR_ANC_EVER_DENOM DEPR_STND_ANC_EVER_NUM DEPR_SITE_ANC_EVER_NUM DEPR_EVER_DENOM DEPR_STND_EVER_NUM DEPR_SITE_EVER_NUM

label data "Depression score, $datadate, `c(username)', $today"
notes replace _dta in 1 : Outcome dataset for $datadate data TS
assert SITE != ""

sort SITE MOMID PREGID
order SITE MOMID PREGID  DEPR_SCORE_ANC20 DEPR_SCORE_ANC32 DEPR_SCORE_PNC6 DEPR_STND_ANC20 DEPR_STND_ANC32 DEPR_STND_PNC6 DEPR_SITE_ANC20 DEPR_SITE_ANC32 DEPR_SITE_PNC6 DEPR_MISS_ANC20_DENOM DEPR_MISS_ANC20 DEPR_ANC20_DENOM DEPR_STND_ANC20_NUM DEPR_SITE_ANC20_NUM DEPR_MISS_ANC32_DENOM DEPR_MISS_ANC32 DEPR_ANC32_DENOM DEPR_STND_ANC32_NUM DEPR_SITE_ANC32_NUM DEPR_MISS_PNC6_DENOM DEPR_MISS_PNC6 DEPR_PNC6_DENOM DEPR_STND_PNC6_NUM DEPR_SITE_PNC6_NUM DEPR_ANC_EVER_DENOM DEPR_STND_ANC_EVER_NUM DEPR_SITE_ANC_EVER_NUM DEPR_EVER_DENOM DEPR_STND_EVER_NUM DEPR_SITE_EVER_NUM

foreach time in "ANC20" "ANC32" "PNC6" {
	
	foreach cutoff in "STND" "SITE" {
		
	label var DEPR_SCORE_`time' ///
		"Worst (highest) depression score at `time'"
	label var DEPR_`cutoff'_`time' ///
		"Depression (`cutoff' cutoff) at `time'"
	}
	
	label var DEPR_MISS_`time'_DENOM ///
		"Denom of expected or has data at `time'"
	label var DEPR_MISS_`time' "Reasons for missing at `time'"
	label var DEPR_`time'_DENOM "Has data at `time'"
}


save "$wrk/MAT_DEPR-$datadate.dta", replace
*Review and save to outcome folder:
*save "$outcomes/MAT_DEPR.dta" , replace
	
