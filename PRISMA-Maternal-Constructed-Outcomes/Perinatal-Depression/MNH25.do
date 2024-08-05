**MNH25 - Perinatal Depression**
**Savannah O'Malley (savannah.omalley@gwu.edu)


***********************************
******Table of Contents************
/*
1. Set directories and import data
2. Code analytical variables for long and collapsed data sets
3. Code analytical variables for Maternal Outcomes report data set
*/



/*
*NOTE: THIS FILE REQUIRES:

"$outcomes/MAT_ENROLL.dta"
"$outcomes/MAT_ENDPOINTS.dta"
"Z:\Savannah_working_files\Expected_obs.dta"
*/
***********************************
***Part 1: Directories and data import
***********************************
* Set folders:
global datadate "2024-06-28"
//update with data date

global da "Z:\Stacked Data/$datadate"
//update with newest data

global outcomes "Z:\Outcome Data/$datadate"



cd "Z:\Savannah_working_files\MNH25\data"
global wrk "Z:\Savannah_working_files\MNH25\data"
// make sure this is a secure location as we will save data files here

**import data - stacked file
import delimited "$da/mnh25_merged.csv", bindquote(strict) case(upper) clear 
rename M25_* *

**************************************************
***Part 2: Analytical variables for long data set
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

**# Code depression score
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
	recode `var' (1=0)  (2=1) (3=2) (4=3) (77=.) (55=.) if SITE=="Kenya"
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
**note that the responses are not the same across SITEs, but 3--> higher frequency/worse intensity of depression symptoms
label define depression_responses ///
0"0, Less/infrequent symptom" 3"3, Worse/frequent symptom"
label val EPDS0101_R EPDS0102_R EPDS0103_R EPDS0104_R EPDS0105_R EPDS0106_R EPDS0107_R EPDS0108_R EPDS0109_R EPDS0110_R depression_responses

 egen Q_ANSWERED=anycount( EPDS0101_R EPDS0102_R EPDS0103_R EPDS0104_R EPDS0105_R EPDS0106_R EPDS0107_R EPDS0108_R EPDS0109_R EPDS0110_R), values(0 1 2 3)
 //this codes the number of questions that were answered 
 
**GEN SUMMARY VARIABLE
egen dep=rowtotal( EPDS0101_R EPDS0102_R EPDS0103_R EPDS0104_R EPDS0105_R EPDS0106_R EPDS0107_R EPDS0108_R EPDS0109_R EPDS0110_R) , missing 
gen dep_sum = (dep/Q_ANSWERED) *10



gen QUERY_MISS_DEPSCORE=1 if dep_sum==. 
label var QUERY_MISS_DEPSCORE "Missing depression score"

**QC check if sum score matches the autocalculated score
gen dep_check = dep_sum - EPDS01_SCORRES    

gen QUERY_SCORE_DIFFER = 1 if dep_check!=0 & dep_check!=.
label var QUERY_SCORE_DIFFER "Manual differs auto score depression"


	tabstat dep_check if dep_check!=0 & dep_check!=., ///
	by(SITE) statistics(count mean)
**note discrepancies by SITE



**NOTE that India-CMC administered the form differently before and after Dec 8, 2023
**create a variable for before and after
**SITES: DO NOT RUN IF SITE ! = "India-CMC"
gen cmc_admin=0 if OBSSTDAT<"2023-12-08" & SITE=="India-CMC"
replace cmc_admin=1 if OBSSTDAT>="2023-12-08" & SITE=="India-CMC"
label define cmc_admin 0"self-administered" 1"staff-administered"
label val cmc_admin cmc_admin
label var cmc_admin "CMC self- or staff- administered; changed Dec 8 2023"

**# Create variables for the report
**ANC-20
gen DEPR_ANC20_STND = -5 if ///
TYPE_VISIT<=2  
//all expected obs
//temporarily code as -5 because we will collapse (max) and we want to preserve 0/1 if it's available 
//we can fix -5, -2, -1 --> 55 after collapsing depending on our needs
replace DEPR_ANC20_STND = -2 if ///
TYPE_VISIT<=2  &  MAT_VISIT_MNH25>=3
//reason for missing: visit not completed
replace DEPR_ANC20_STND = -1 if ///
TYPE_VISIT<=2  &  MAT_VISIT_MNH25<=2 & dep_sum==.
//reason for missing: visit completed, but the summary score is not available
replace DEPR_ANC20_STND = 0 if ///
TYPE_VISIT<=2  & dep_sum !=.
replace DEPR_ANC20_STND = 1 if ///
TYPE_VISIT<=2  & dep_sum !=. & dep_sum>=11 
label var DEPR_ANC20_STND "Standard cutoff"

gen DEPR_ANC20_SITE = -5 if ///
TYPE_VISIT<=2
replace DEPR_ANC20_SITE = -2 if ///
TYPE_VISIT<=2  &  MAT_VISIT_MNH25>=3
//reason for missing: visit not completed
replace DEPR_ANC20_SITE = -1 if ///
TYPE_VISIT<=2  &  MAT_VISIT_MNH25<=2 & dep_sum==.
replace DEPR_ANC20_SITE = 0 if ///
TYPE_VISIT<=2  & dep_sum !=.
replace DEPR_ANC20_SITE  = 1 if /// 
SITE=="Ghana" & dep_sum>=11  & dep_sum!=.  & TYPE_VISIT<=2 | ///
SITE=="India-CMC" & dep_sum>=8  & dep_sum!=. & TYPE_VISIT<=2 | ///
SITE=="India-SAS" & dep_sum>=10 & dep_sum!=. & TYPE_VISIT<=2 | ///
SITE=="Kenya" & dep_sum>=13     & dep_sum!=. & TYPE_VISIT<=2 | ///
SITE=="Pakistan" & dep_sum>=14  & dep_sum!=. & TYPE_VISIT<=2 | ///
SITE=="Zambia" & dep_sum>=10    & dep_sum!=. & TYPE_VISIT<=2
label var DEPR_ANC20_SITE "SITE-specific cutoff"

gen DEPR_ANC20_SCORE=dep_sum if TYPE_VISIT<=2

**ANC-32 (also includes ANC-36)
	gen DEPR_ANC32_STND = -5 if ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5  
	//all expected obs
	//temporarily code as -5 because we will collapse (max) and we want to preserve 0/1 if it's available 
	replace DEPR_ANC32_STND = -2 if ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5  & MAT_VISIT_MNH25>=3
	replace DEPR_ANC32_STND = -1 if ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5  & MAT_VISIT_MNH25<=2 & dep_sum==.
	replace DEPR_ANC32_STND = 0 if ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5 & dep_sum !=.
	replace DEPR_ANC32_STND = 1 if ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5  & dep_sum !=. & dep_sum>=11 
	label var DEPR_ANC32_STND "Standard cutoff"

	gen DEPR_ANC32_SITE = -5 if ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5
	 replace DEPR_ANC32_SITE = -2 if ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5  & MAT_VISIT_MNH25>=3
	replace DEPR_ANC32_SITE = -1 if ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5  & MAT_VISIT_MNH25<=2 & dep_sum==.
	replace DEPR_ANC32_SITE = 0 if ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5  & dep_sum !=.
	replace DEPR_ANC32_SITE  = 1 if /// 
	SITE=="Ghana" & dep_sum>=11 	& dep_sum!=. & ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5  | ///
	SITE=="India-CMC" & dep_sum>=8 	& dep_sum!=. & ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5  | ///
	SITE=="India-SAS" & dep_sum>=10 & dep_sum!=. & ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5  | ///
	SITE=="Kenya" & dep_sum>=13 	& dep_sum!=. & ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5  | ///
	SITE=="Pakistan" & dep_sum>=14 	& dep_sum!=. & ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5  | ///
	SITE=="Zambia" & dep_sum>=10 	& dep_sum!=. & ///
	TYPE_VISIT>=4 & TYPE_VISIT<=5  
	label var DEPR_ANC32_SITE "SITE-specific cutoff"
	gen DEPR_ANC32_SCORE =dep_sum if TYPE_VISIT>=4 & TYPE_VISIT<=5  

**PNC-6
	gen DEPR_PNC6_STND = -5 if ///
	TYPE_VISIT==10 
	//all expected obs
	//temporarily code as -5 because we will collapse (max) and we want to preserve 0/1 if it's available 
	replace DEPR_PNC6_STND = -2 if ///
	TYPE_VISIT==10 & MAT_VISIT_MNH25>=3
	replace DEPR_PNC6_STND = -1 if ///
	TYPE_VISIT==10 & MAT_VISIT_MNH25<=2 & dep_sum==.
	replace DEPR_PNC6_STND = 0 if ///
	TYPE_VISIT==10 & dep_sum !=.
	replace DEPR_PNC6_STND = 1 if ///
	TYPE_VISIT==10  & dep_sum !=. & dep_sum>=11 
	label var DEPR_PNC6_STND "Standard cutoff"

	gen DEPR_PNC6_SITE = -5 if ///
	TYPE_VISIT==10
	replace DEPR_PNC6_SITE = -2 if ///
	TYPE_VISIT==10 & MAT_VISIT_MNH25>=3
	replace DEPR_PNC6_SITE = -1 if ///
	TYPE_VISIT==10 & MAT_VISIT_MNH25<=2 & dep_sum==.
	replace DEPR_PNC6_SITE = 0 if ///
	TYPE_VISIT==10  & dep_sum !=.
	replace DEPR_PNC6_SITE  = 1 if /// 
	SITE=="Ghana" & dep_sum>=11 & dep_sum!=. & TYPE_VISIT==10  | ///
	SITE=="India-CMC" & dep_sum>=8 	& dep_sum!=. & TYPE_VISIT==10  | ///
	SITE=="India-SAS" & dep_sum>=10 & dep_sum!=. & TYPE_VISIT==10  | ///
	SITE=="Kenya" & dep_sum>=13 	& dep_sum!=. & TYPE_VISIT==10  | ///
	SITE=="Pakistan" & dep_sum>=14 	& dep_sum!=. & TYPE_VISIT==10  | ///
	SITE=="Zambia" & dep_sum>=10 	& dep_sum!=. & TYPE_VISIT==10 
	label var DEPR_PNC6_SITE "SITE-specific cutoff"
	gen DEPR_PNC6_SCORE = dep_sum if TYPE_VISIT==10 


save "$wrk/mnh25_update.dta", replace

**COLLAPSE to get one row per participant
**We want to keep only the highest score
collapse (max) DEPR_ANC20_STND DEPR_ANC20_SITE DEPR_ANC20_SCORE DEPR_ANC32_STND DEPR_ANC32_SITE DEPR_ANC32_SCORE DEPR_PNC6_STND DEPR_PNC6_SITE DEPR_PNC6_SCORE, by(SITE MOMID PREGID)

save "$wrk/mnh25_collapsed.dta", replace


**************************************************
**# Part 3: Analytical variables for maternal outcomes data set
**************************************************
	rename MOMID PREGID, upper
	merge 1:1 MOMID PREGID using "$outcomes/MAT_ENROLL.dta",nogen
	keep if ENROLL == 1

	merge 1:1 MOMID PREGID using "$outcomes/MAT_ENDPOINTS.dta", ///
	gen(merge_PREGEND)
	keep if ENROLL == 1

	merge 1:1 MOMID PREGID using "Z:\Savannah_working_files\Expected_obs.dta", nogen


**********ANC-20***********

	gen DEPR_ANC20_D = 1 if ///
	(DEPR_ANC20_STND ==0 | DEPR_ANC20_STND==1) & ANC20_EXP==1
	label var DEPR_ANC20_D ///
	"Denominator of those who have valid depression score for ANC20"
	**NUMERATORS
	gen DEPR_ANC20_STND_N = 1 if DEPR_ANC20_STND==1 & ANC20_EXP==1
	label var DEPR_ANC20_STND_N ///
	"Numerator of those screening for depression at ANC20, Standard cutoff"
	gen DEPR_ANC20_SITE_N = 1 if DEPR_ANC20_SITE==1 & ANC20_EXP==1
	label var DEPR_ANC20_SITE_N ///
	"Numerator of those screening for depression at ANC20, SITE cutoff"
	**MISSING
	gen DEPR_ANC20_MISS =DEPR_ANC20_STND if DEPR_ANC20_STND<0
	replace DEPR_ANC20_MISS = -2 if ANC20_EXP ==1 & DEPR_ANC20_STND==.
	replace DEPR_ANC20_MISS = . if ANC20_EXP !=1
	label define MISS -2"Visit not completed" -1"No summary score"
	label val DEPR_ANC20_MISS MISS

	gen DEPR_ANC20_MISS_D = 1 if ANC20_EXP ==1
	//denominator for data completeness table

**********ANC-32***********

	gen DEPR_ANC32_D = 1 if ///
	(DEPR_ANC32_STND == 0 | DEPR_ANC32_STND == 1) & ANC32_EXP == 1
	label var DEPR_ANC32_D ///
	"Denominator of those who have valid depression score for ANC32"
	**NUMERATORS
	gen DEPR_ANC32_STND_N =1 if ///
	DEPR_ANC32_STND ==1 & ANC32_EXP == 1
	label var DEPR_ANC32_STND_N ///
	"Numerator of those screening for depression at ANC32, standard cutoff"
	gen DEPR_ANC32_SITE_N =1 if ///
	DEPR_ANC32_SITE ==1 & ANC32_EXP == 1
	label var DEPR_ANC32_SITE_N ///
	"Numerator of those screening for depression at ANC32, SITE cutoff"
	**MISSING
	gen DEPR_ANC32_MISS = DEPR_ANC32_STND if DEPR_ANC32_STND<0
	replace DEPR_ANC32_MISS = -2 if  ANC32_EXP == 1 & DEPR_ANC32_STND==.
	replace  DEPR_ANC32_MISS=. if ANC32_EXP !=1
	label val DEPR_ANC32_MISS MISS
	gen DEPR_ANC32_MISS_D =1 if ANC32_EXP == 1

**********PNC-6***********
	**DENOMINATOR
	gen DEPR_PNC6_D = 1 if ///
	(DEPR_PNC6_STND == 0 | DEPR_PNC6_STND == 1) & PNC6_EXP == 1
	label var DEPR_PNC6_D ///
	"Denominator of those who have valid depression scores for PNC6"
	**NUMERATORS
	gen DEPR_PNC6_STND_N = 1 if DEPR_PNC6_STND == 1 & PNC6_EXP == 1
	label var DEPR_PNC6_STND_N ///
	"Numerator of those screening for depression at PNC6, standard cutoff"
	gen DEPR_PNC6_SITE_N = 1 if DEPR_PNC6_SITE == 1 & PNC6_EXP == 1
	label var DEPR_PNC6_SITE_N ///
	"Numerator of those screening for depression at PNC6, SITE cutoff"
	**MISSING 
	gen DEPR_PNC6_MISS = DEPR_PNC6_STND if DEPR_PNC6_STND < 0
	replace DEPR_PNC6_MISS = -2 if PNC6_EXP ==1 & DEPR_PNC6_STND==.
	replace DEPR_PNC6_MISS = . if PNC6_EXP!=1
	label val DEPR_PNC6_MISS MISS

	gen DEPR_PNC6_MISS_D = 1 if PNC6_EXP == 1

**********ANC, EVER***********
	gen DEPR_ANC_EVER_D =1 if ///
	DEPR_ANC20_D ==1 | ///
	DEPR_ANC32_D ==1
	label var DEPR_ANC_EVER_D ///
	"Denominator of those with a valid depression score at ANC20 or 32"
	gen DEPR_ANC_EVER_STND_N = 1 if ///
	 DEPR_ANC20_STND_N ==1 |  DEPR_ANC32_STND_N ==1
	 label var DEPR_ANC_EVER_STND_N ///
	 "Numerator of those with a valid depression score at ANC20 or 32"
	gen DEPR_ANC_EVER_SITE_N = 1 if ///
	 DEPR_ANC20_SITE_N ==1 | DEPR_ANC32_SITE_N==1
	 label var DEPR_ANC_EVER_SITE_N ///
	 "Numerator of those with a valid depression score at ANC20 or 32"


**********EVER***********
	gen DEPR_EVER_D = 1 if ///
	DEPR_ANC20_D ==1 | DEPR_ANC32_D==1 | DEPR_PNC6_D==1
	label var DEPR_EVER_D ///
	"Denominator of any who have a valid depression score, any time point"
	gen DEPR_EVER_STND_N = 1 if ///
	DEPR_ANC_EVER_STND_N == 1 | DEPR_PNC6_STND_N == 1
	label var DEPR_EVER_STND_N ///
	"Numerator of any who ever screened for possible depression, std cutoff"
	gen DEPR_EVER_SITE_N = 1 if ///
	DEPR_ANC_EVER_SITE_N == 1 | DEPR_PNC6_SITE_N == 1
	label var DEPR_EVER_SITE_N ///
	"Numerator of any who ever screened for possible depression, SITE cutoff"




*drop SITE DEPR_ANC20_STND DEPR_ANC20_SITE DEPR_ANC32_STND DEPR_ANC32_SITE DEPR_PNC6_STND DEPR_PNC6_SITE
order DEPR_ANC32_SCORE, after( DEPR_ANC20_MISS)
order DEPR_PNC6_SCORE, after( DEPR_ANC32_MISS)
foreach var in DEPR_ANC20_STND DEPR_ANC20_SITE DEPR_ANC32_STND DEPR_ANC32_SITE DEPR_PNC6_STND DEPR_PNC6_SITE {
	replace `var'=55 if `var' == -2 | `var' == -1
}

save "$wrk/mnh25_MaternalOutcomes.dta", replace

keep SITE MOMID PREGID DEPR_ANC20_STND DEPR_ANC20_SITE DEPR_ANC20_SCORE DEPR_ANC32_STND DEPR_ANC32_SITE DEPR_PNC6_STND DEPR_PNC6_SITE  DEPR_ANC20_D DEPR_ANC20_STND_N DEPR_ANC20_SITE_N DEPR_ANC20_MISS DEPR_ANC32_SCORE DEPR_ANC20_MISS_D  DEPR_ANC32_D DEPR_ANC32_STND_N DEPR_ANC32_SITE_N DEPR_ANC32_MISS DEPR_PNC6_SCORE DEPR_ANC32_MISS_D  DEPR_PNC6_D DEPR_PNC6_STND_N DEPR_PNC6_SITE_N DEPR_PNC6_MISS DEPR_PNC6_MISS_D DEPR_ANC_EVER_D DEPR_ANC_EVER_STND_N DEPR_ANC_EVER_SITE_N DEPR_EVER_D DEPR_EVER_STND_N DEPR_EVER_SITE_N



save "$outcomes/MAT_DEPR.dta" , replace
