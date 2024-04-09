**MNH25 - Perinatal Depression**
**Savannah O'Malley (savannah.omalley@gwu.edu)


***********************************
******Table of Contents************
/*
1. Set directories and import data
2. Code analytical variables for long and collapsed data sets
3. Code analytical variables for Maternal Outcomes report data set
*/
***********************************
  
***Part 1: Directories and data import

* Set folders:
global da "Z:\Stacked Data\2024-04-05"
//update with newest data
global datadate "05-apr-2024"
//update with data date
global wrk "Z:\Savannah_working_files\MNH25\data"
// make sure this is a secure location as we will save data files here
cd "Z:\Savannah_working_files\MNH25\data"

**import data - stacked file
import delimited "$da/mnh25_merged.csv", bindquote(strict) clear 


**************************************************
***Part 2: Analytical variables for long data set
**************************************************
label define type_visit ///
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
label val m25_type_visit type_visit
label var m25_type_visit "Indicate visit when depression screening was administered"

label define mat_visit 					///
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
label val m25_mat_visit_mnh25 mat_visit
label var m25_mat_visit_mnh25 "Was the visit completed?"

foreach var in m25_epds0101 m25_epds0102 m25_epds0103 m25_epds0104 ///
m25_epds0105 m25_epds0106 m25_epds0107 m25_epds0108 m25_epds0109 m25_epds0110 {
	gen `var'_recode=`var'
	**this is a loop for each variable before the bracket { 
	**creates a "_recode" variable so we can make changes but preserve the original data 
}

/*
foreach var in m25_epds0103_recode m25_epds0106_recode m25_epds0107_recode ///
 m25_epds0108_recode m25_epds0109_recode m25_epds0110_recode {
	replace `var'="77" if `var'=="NA"
	**replace with numbers to allow for destring
	**note that sites should only be using 77
	destring `var' , replace
}
*/
**CHANGE SCORES FOR INDIA CMC AND SAS, PAKISTAN AND ZAMBIA - BASED ON CRF
foreach var in m25_epds0101_recode m25_epds0102_recode m25_epds0104_recode {
	recode `var' (1=0) (2=1) (3=2) (4=3) (77=.) if site!="Ghana" & site!="Kenya"
	**change score so that the  option #1 == score of 0,  option #2== score of 1, etc.
	**question 1,2,4 are positively worded
	**Ghana and Kenya are each a special case
	**NOTE that this DROPS any observations ==77, may need to update code for query report
}

foreach var in m25_epds0103_recode m25_epds0105_recode m25_epds0106_recode m25_epds0107_recode m25_epds0108_recode m25_epds0109_recode m25_epds0110_recode {
	recode `var' (1=3) (2=2) (3=1) (4=0) (77=.) if site!="Ghana"  & site!="Kenya"
	**change score so that the 1 option == score of 3, 2 option == score of 2, etc.
	*questions 3, 5-10 are negatively worded
}


******************************************************
**KENYA-SPECIFIC CODE; 
**all questions in the scale are coded the same 

**SITES: DO NOT RUN IF SITE ! = "Kenya"
foreach var in m25_epds0101_recode m25_epds0102_recode m25_epds0103_recode m25_epds0104_recode m25_epds0105_recode m25_epds0106_recode m25_epds0107_recode m25_epds0108_recode m25_epds0109_recode m25_epds0110_recode {
	recode `var' (1=0)  (2=1) (3=2) (4=3) (77=.) if site=="Kenya"
	**change score so that the option #1 == score of 0,  option #2 == score of 1, etc.
}

**END Kenya- Specific code
******************************************************

******************************************************
**Ghana-specific code: their questions are asked differently, see CRF and data dictionary
**SITES: DO NOT RUN IF SITE ! = "Ghana"
**Questions 1-2 which are written in a positive way:
	**"1. In the past 7 days I have been able to laugh..."
	**"2. In the past 7 days I have looked forward with enjoyment to things"
	**THEREFORE, if participant responded "no" --> indicates symptom of depression and should be given a higher score
	**a participant responded "yes" --> no symptom of depression, lower score
foreach var in m25_epds0101 m25_epds0102  {
	replace `var'_recode=. if site=="Ghana"
	*original recode is wrong, drop it
	replace `var'_recode=3 if `var'_n ==2 & site=="Ghana"
	*no, not at all (positive question)
	replace `var'_recode=2 if `var'_n ==1 & site=="Ghana"
	*no, not very often (positive question)
	replace `var'_recode=1 if `var'_y ==1 & site=="Ghana"
	*yes, some of the time (positive question)
	replace `var'_recode=0 if `var'_y ==2 & site=="Ghana"
	*yes, most of the time (positive question)
	replace `var'_recode=. if `var' ==77 
}

**Questions #3-10 are written in a negative way
	**THEREFORE, a "no" response indicates the participant does not report this depression symptom --> should be given a LOWER score
	**a "yes" indicates participant reported a depression symptom--> coded a HIGHER score 
	**e.g., Question #3 "In the past 7 days I have blamed myself unnecessarily when things went wrong"
foreach var in m25_epds0103 m25_epds0104 m25_epds0105 m25_epds0106 ///
m25_epds0107 m25_epds0108 m25_epds0109 m25_epds0110 {
	replace `var'_recode=. if site=="Ghana"
	*original recode is wrong, drop it
	replace `var'_recode=3 if `var'_y==2 & site=="Ghana"
	*yes, most of the time (yes to a negative question)
	replace `var'_recode=2 if `var'_y==1 & site=="Ghana"
	*yes, some of the time (yes to a negative question)
	replace `var'_recode=1 if `var'_n==1
	*no, not very often (no to a negative question)
	replace `var'_recode=0 if `var'_n==2
	*no, not at all (no to a negative question)
}

**END GHANA-SPECIFIC CODE**
***************************************


*Label the responses
**note that the responses are not the same across sites, but 3--> higher frequency/worse intensity of depression symptoms
label define depression_responses ///
0"0, Less/infrequent symptom" 3"3, Worse/frequent symptom"
label val m25_epds0101_recode m25_epds0102_recode m25_epds0103_recode ///
m25_epds0104_recode m25_epds0105_recode m25_epds0106_recode ///
m25_epds0107_recode m25_epds0108_recode m25_epds0109_recode ///
m25_epds0110_recode depression_responses

**GEN SUMMARY VARIABLE
egen dep_sum=rowtotal( m25_epds0101_recode m25_epds0102_recode m25_epds0103_recode m25_epds0104_recode m25_epds0105_recode m25_epds0106_recode m25_epds0107_recode m25_epds0108_recode m25_epds0109_recode m25_epds0110_recode) , missing 

gen QUERY_MISS_DEPSCORE=1 if dep_sum==. 
label var QUERY_MISS_DEPSCORE "Missing depression score"

**QC check if sum score matches the autocalculated score
gen dep_check = dep_sum - m25_epds01_scorres    

gen QUERY_SCORE_DIFFER = 1 if dep_check!=0 & dep_check!=.
label var QUERY_SCORE_DIFFER "Manual differs auto score depression"

bysort site: sum dep_check
  *note any discrepancies by site

tab site if dep_check!=0 & dep_check!=.

**NOTE that India-CMC administered the form differently before and after Dec 8, 2023
**create a variable for before and after
**SITES: DO NOT RUN IF SITE ! = "India-CMC"
gen cmc_admin=0 if m25_obsstdat<"2023-12-08" & site=="India-CMC"
replace cmc_admin=1 if m25_obsstdat>="2023-12-08" & site=="India-CMC"
label define cmc_admin 0"self-administered" 1"staff-administered"
label val cmc_admin cmc_admin
label var cmc_admin "CMC self- or staff- administered; changed Dec 8 2023"

**ANC-20
gen DEPR_ANC20_STND = -5 if ///
m25_type_visit<=2  
//all expected obs
//temporarily code as -5 because we will collapse (max) and we want to preserve 0/1 if it's available 
//we can fix -5, -2, -1 --> 55 after collapsing depending on our needs
replace DEPR_ANC20_STND = -2 if ///
m25_type_visit<=2  &  m25_mat_visit_mnh25>=3
//reason for missing: visit not completed
replace DEPR_ANC20_STND = -1 if ///
m25_type_visit<=2  &  m25_mat_visit_mnh25<=2 & dep_sum==.
//reason for missing: visit completed, but the summary score is not available
replace DEPR_ANC20_STND = 0 if ///
m25_type_visit<=2  & dep_sum !=.
replace DEPR_ANC20_STND = 1 if ///
m25_type_visit<=2  & dep_sum !=. & dep_sum>=11 
label var DEPR_ANC20_STND "Standard cutoff"

gen DEPR_ANC20_SITE = -5 if ///
m25_type_visit<=2
replace DEPR_ANC20_SITE = -2 if ///
m25_type_visit<=2  &  m25_mat_visit_mnh25>=3
//reason for missing: visit not completed
replace DEPR_ANC20_SITE = -1 if ///
m25_type_visit<=2  &  m25_mat_visit_mnh25<=2 & dep_sum==.
replace DEPR_ANC20_SITE = 0 if ///
m25_type_visit<=2  & dep_sum !=.
replace DEPR_ANC20_SITE  = 1 if /// 
site=="Ghana" & dep_sum>=11  & dep_sum!=.  & m25_type_visit<=2 | ///
site=="India-CMC" & dep_sum>=8  & dep_sum!=. & m25_type_visit<=2 | ///
site=="India-SAS" & dep_sum>=10 & dep_sum!=. & m25_type_visit<=2 | ///
site=="Kenya" & dep_sum>=13     & dep_sum!=. & m25_type_visit<=2 | ///
site=="Pakistan" & dep_sum>=14  & dep_sum!=. & m25_type_visit<=2 | ///
site=="Zambia" & dep_sum>=10    & dep_sum!=. & m25_type_visit<=2
label var DEPR_ANC20_SITE "Site-specific cutoff"

gen DEPR_ANC20_SCORE=dep_sum if m25_type_visit<=2

**ANC-32 (also includes ANC-36)
gen DEPR_ANC32_STND = -5 if ///
m25_type_visit>=4 & m25_type_visit<=5  
//all expected obs
//temporarily code as -5 because we will collapse (max) and we want to preserve 0/1 if it's available 
replace DEPR_ANC32_STND = -2 if ///
m25_type_visit>=4 & m25_type_visit<=5  & m25_mat_visit_mnh25>=3
replace DEPR_ANC32_STND = -1 if ///
m25_type_visit>=4 & m25_type_visit<=5  & m25_mat_visit_mnh25<=2 & dep_sum==.
replace DEPR_ANC32_STND = 0 if ///
m25_type_visit>=4 & m25_type_visit<=5 & dep_sum !=.
replace DEPR_ANC32_STND = 1 if ///
m25_type_visit>=4 & m25_type_visit<=5  & dep_sum !=. & dep_sum>=11 
label var DEPR_ANC32_STND "Standard cutoff"

gen DEPR_ANC32_SITE = -5 if ///
m25_type_visit>=4 & m25_type_visit<=5
 replace DEPR_ANC32_SITE = -2 if ///
m25_type_visit>=4 & m25_type_visit<=5  & m25_mat_visit_mnh25>=3
replace DEPR_ANC32_SITE = -1 if ///
m25_type_visit>=4 & m25_type_visit<=5  & m25_mat_visit_mnh25<=2 & dep_sum==.
replace DEPR_ANC32_SITE = 0 if ///
m25_type_visit>=4 & m25_type_visit<=5  & dep_sum !=.
replace DEPR_ANC32_SITE  = 1 if /// 
site=="Ghana" & dep_sum>=11 	& dep_sum!=. & m25_type_visit>=4 & m25_type_visit<=5  | ///
site=="India-CMC" & dep_sum>=8 	& dep_sum!=. & m25_type_visit>=4 & m25_type_visit<=5  | ///
site=="India-SAS" & dep_sum>=10 & dep_sum!=. & m25_type_visit>=4 & m25_type_visit<=5  | ///
site=="Kenya" & dep_sum>=13 	& dep_sum!=. & m25_type_visit>=4 & m25_type_visit<=5  | ///
site=="Pakistan" & dep_sum>=14 	& dep_sum!=. & m25_type_visit>=4 & m25_type_visit<=5  | ///
site=="Zambia" & dep_sum>=10 	& dep_sum!=. & m25_type_visit>=4 & m25_type_visit<=5  
label var DEPR_ANC32_SITE "Site-specific cutoff"
gen DEPR_ANC32_SCORE =dep_sum if m25_type_visit>=4 & m25_type_visit<=5  

**PNC-6
gen DEPR_PNC6_STND = -5 if ///
m25_type_visit==10 
//all expected obs
//temporarily code as -5 because we will collapse (max) and we want to preserve 0/1 if it's available 
replace DEPR_PNC6_STND = -2 if ///
m25_type_visit==10 & m25_mat_visit_mnh25>=3
replace DEPR_PNC6_STND = -1 if ///
m25_type_visit==10 & m25_mat_visit_mnh25<=2 & dep_sum==.
replace DEPR_PNC6_STND = 0 if ///
m25_type_visit==10 & dep_sum !=.
replace DEPR_PNC6_STND = 1 if ///
m25_type_visit==10  & dep_sum !=. & dep_sum>=11 
label var DEPR_PNC6_STND "Standard cutoff"

gen DEPR_PNC6_SITE = -5 if ///
m25_type_visit==10
replace DEPR_PNC6_SITE = -2 if ///
m25_type_visit==10 & m25_mat_visit_mnh25>=3
replace DEPR_PNC6_SITE = -1 if ///
m25_type_visit==10 & m25_mat_visit_mnh25<=2 & dep_sum==.
replace DEPR_PNC6_SITE = 0 if ///
m25_type_visit==10  & dep_sum !=.
replace DEPR_PNC6_SITE  = 1 if /// 
site=="Ghana" & dep_sum>=11 & dep_sum!=. & m25_type_visit==10  | ///
site=="India-CMC" & dep_sum>=8 	& dep_sum!=. & m25_type_visit==10  | ///
site=="India-SAS" & dep_sum>=10 & dep_sum!=. & m25_type_visit==10  | ///
site=="Kenya" & dep_sum>=13 	& dep_sum!=. & m25_type_visit==10  | ///
site=="Pakistan" & dep_sum>=14 	& dep_sum!=. & m25_type_visit==10  | ///
site=="Zambia" & dep_sum>=10 	& dep_sum!=. & m25_type_visit==10 
label var DEPR_PNC6_SITE "Site-specific cutoff"
gen DEPR_PNC6_SCORE = dep_sum if m25_type_visit==10 


save "$wrk/mnh25.dta", replace

**COLLAPSE to get one row per participant
**We want to keep only the highest score
collapse (max) DEPR_ANC20_STND DEPR_ANC20_SITE DEPR_ANC20_SCORE DEPR_ANC32_STND DEPR_ANC32_SITE DEPR_ANC32_SCORE DEPR_PNC6_STND DEPR_PNC6_SITE DEPR_PNC6_SCORE, by(site momid pregid)

save "$wrk/mnh25_collapsed.dta", replace


**************************************************
***Part 3: Analytical variables for Maternal Outcomes Data Set
**************************************************

**********ANC-20***********
**DENOMINATOR
gen DEPR_ANC20_D = 1 if ///
DEPR_ANC20_STND ==0 | DEPR_ANC20_STND==1
label var DEPR_ANC20_D "Denominator of those who have valid depression score for ANC20"
**NUMERATORS
gen DEPR_ANC20_STND_N = 1 if  DEPR_ANC20_STND==1
label var DEPR_ANC20_STND_N ///
"Numerator of those screening for depression at ANC20, Standard cutoff"
gen DEPR_ANC20_SITE_N = 1 if  DEPR_ANC20_SITE==1
label var DEPR_ANC20_SITE_N ///
"Numerator of those screening for depression at ANC20, site cutoff"
**MISSING
gen DEPR_ANC20_MISS =DEPR_ANC20_STND if DEPR_ANC20_STND<0
label define MISS -2"Visit not completed" -1"No summary score"
label val DEPR_ANC20_MISS MISS

**********ANC-32***********
**DENOMINATOR
gen DEPR_ANC32_D = 1 if ///
DEPR_ANC32_STND == 0 | DEPR_ANC32_STND == 1
label var DEPR_ANC32_D ///
"Denominator of those who have valid depression score for ANC32"
**NUMERATORS
gen DEPR_ANC32_STND_N =1 if ///
DEPR_ANC32_STND ==1
label var DEPR_ANC32_STND_N ///
"Numerator of those screening for depression at ANC32, standard cutoff"
gen DEPR_ANC32_SITE_N =1 if ///
DEPR_ANC32_SITE ==1
label var DEPR_ANC32_SITE_N ///
"Numerator of those screening for depression at ANC32, site cutoff"
**MISSING
gen DEPR_ANC32_MISS = DEPR_ANC32_STND if DEPR_ANC32_STND<0
label val DEPR_ANC32_MISS MISS

**********PNC-6***********
**DENOMINATOR
gen DEPR_PNC6_D = 1 if ///
DEPR_PNC6_STND == 0 | DEPR_PNC6_STND == 1
label var DEPR_PNC6_D ///
"Denominator of those who have valid depression scores for PNC6"
**NUMERATORS
gen DEPR_PNC6_STND_N = 1 if DEPR_PNC6_STND == 1
label var DEPR_PNC6_STND_N ///
"Numerator of those screening for depression at PNC6, standard cutoff"
gen DEPR_PNC6_SITE_N = 1 if DEPR_PNC6_SITE == 1
label var DEPR_PNC6_SITE_N ///
"Numerator of those screening for depression at PNC6, site cutoff"
**MISSING 
gen DEPR_PNC6_MISS = DEPR_PNC6_STND if DEPR_PNC6_STND < 0
label val DEPR_PNC6_MISS MISS


**********ANC, EVER***********
gen DEPR_ANC_EVER_D =1 if ///
DEPR_ANC20_STND ==0  | DEPR_ANC20_STND==1 | ///
DEPR_ANC32_STND ==0  | DEPR_ANC32_STND==1
label var DEPR_ANC_EVER_D ///
"Denominator of those with a valid depression score at ANC20 or 32"
gen DEPR_ANC_EVER_STND_N = 1 if ///
 DEPR_ANC20_STND ==1 |  DEPR_ANC32_STND ==1
 label var DEPR_ANC_EVER_STND_N ///
 "Numerator of those with a valid depression score at ANC20 or 32"
gen DEPR_ANC_EVER_SITE_N = 1 if ///
 DEPR_ANC20_SITE ==1 | DEPR_ANC32_SITE==1
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
"Numerator of any who screened for possible depression, standard cutoff, any time point"
gen DEPR_EVER_SITE_N = 1 if ///
DEPR_ANC_EVER_SITE_N == 1 | DEPR_PNC6_SITE_N == 1
label var DEPR_EVER_SITE_N ///
"Numerator of any who screened for possible depression, site cutoff, any time point"

gen SITE=site
order SITE, before(site)

drop site DEPR_ANC20_STND DEPR_ANC20_SITE DEPR_ANC32_STND DEPR_ANC32_SITE DEPR_PNC6_STND DEPR_PNC6_SITE
order DEPR_ANC32_SCORE, after( DEPR_ANC20_MISS)
order DEPR_PNC6_SCORE, after( DEPR_ANC32_MISS)

save "$wrk/mnh25_MaternalOutcomes.dta", replace
