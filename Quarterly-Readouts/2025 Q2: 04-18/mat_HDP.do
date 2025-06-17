*PRISMA Maternal Variable Construction Code
*Purpose: This code drafts variable construction code for Hypertensive Disorders of Pregnancy
*Original Version: June 20, 2024 by E Oakley (emoakley@gwu.edu)
*Update: July 2, 2024 by E Oakley (Define chronic HTN as 2+ high BP measures with the FIRST at <20 weeks (but no restriction on the second))
*Update: July 22, 2024 by E Oakley (add postpartum eclampsia outcome)
*Update: September 24, 2024 by E Oakley (incorporate code review; adjust definition of PE with severe features to include any severe hypertension (regardless of proteinuria status))
*Update: October 16, 2024 by E Oakley (reconstruct separate indicator for organ failure & run with new data)
*Update: October 29, 2024 by E Oakley (add postpartum seizures within 7 days to PE with severe features)
*Update: January 10, 2025 by E Oakley (update variable names per convention & add denominators)

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
global da "Z:/Stacked Data/2025-04-18" // change date here as needed

	// Working Files Folder (TNT-Drive)
global wrk "Z:/Erin_working_files/data" // set pathway here for where you want to save output data files (i.e., constructed analysis variables)

global OUT "Z:\Outcome Data\2025-04-18" // UPDATE DATE AS NEEDED

global date "250505" // today's date

log using "$log/mat_outcome_construct_HDP_$date", replace

/*************************************************************************
	*Variables constructed in this do file:
	
	
	/////////////////////////////
	*Finalized outcome variables: 
	HDP_GROUP - final status of HDPs at ANC through delivery 
		0=No HDP
		1=Chronic HTN 
		2=Gestational HTN
		3=Preeclampsia
		4=Preeclampsia superimposed on Chronic HTN 
		5=Preeclampsia with severe features 
		55=Missing information
		77=Pregnancy ended at <20 weeks GA
	
	HTN_ANY - binary indicator of HTN (chronic) <20 weeks GA
	
	Symptoms of PE with severe features:
		SEVERE_FEAT_SEIZURES	PE with severe features: eclampsia/seizures
		SEVERE_FEAT_HELLP		PE with severe features: HELLP
		SEVERE_FEAT_ORGAN		PE with severe features: organ dysfunction
		SEVERE_FEAT_SEVHIGH		PE with severe features: 1+ Severe high BP
		SEVERE_FEAT_SEVHYP		PE with severe features: Dx with severe high BP
		SEVERE_FEAT_PULMED		PE with severe features: pulmonary edema
		SEVERE_FEAT_VISUAL		PE with severe features: visual symptoms
		SEVERE_FEAT_EPIPAIN		PE with severe features: epigastric pain
		SEVERE_FEAT_HEAD		PE with severe features: severe headache
		SEVERE_FEAT_POSTP_ECLAMPSIA	PE with severe features: postpartum eclampsia
	
	HDP_GROUP_MISS - Reason missing: HDP Group
	
	HIGH_BP_SEVERE_ANY	All participants who ever had severe high BP during pregnancy (regardless of HDP group) - includes both measured and diagnosed

	HIGH_BP_SEVERE_DX	Among those who ever had severe high BP: those with a dx of severe high BP (checkbox)

	HIGH_BP_SEVERE_MEAS	Among those who ever had severe high BP: those with measured severe high BP
	
	
	/////////////////////////////
	*Binary outcome variables (added 9-25-2024): 
	* Note: variables below are NOT mutually exclusive (unlike the variable HDP_GROUP above):
	
	CHTN - participant met the criteria for chronic HTN at any time
	
	GHTN - participant met the criteria for gestational HTN at any time 
	
	PREEC - participant met the criteria for preeclampsia at any time 
	
	PREEC_SEVERE - participant met the criteria for preeclamspia with severe features at any time 
	
	
	//////////////////////////////////
	*Blood pressure reading variables: 
	
	ENTRY_TOTAL - Total number of BP measures at/after 20 weeks 
	
	HIGH_BP_COUNT - Number of High BP readings at/after 20 weeks
	
	HIGH_BP_SEV_COUNT - Number of severe high BP readings at/after 20 weeks
	
	BP_COUNT_PRISMA - Number of BP readings by PRISMA staff (MNH06) at/after 20 weeks
	
	BP_COUNT_IPC - Number of BP readings at L&D (MNH09) at/after 20 weeks 
	
	BP_COUNT_HOSP - Number of BP readings recorded in hospitalization at/after 20 weeks
	
	
	///////////////////////////////
	*Proteinuria reading variables: 
	
	UA_PROT_LBORRES - Highest protein reading at/after 20 weeks
	
	UA_PROT_TESTTYPE - Form reported highest protein reading at/after 20 weeks (MNH08 or Hospital form or L&D form)
	
	UA_PROT_DATE - Date of highest protein reading 
	
	UA_PROT_GA - GA at highest protein reading 
	
	UA_PROT_PRIOR20_COUNT - Total number of proteinuria measures at BASELINE (<20 weeks)
continuous

	UA_PROT_PRIOR20_LBORRES - Highest protein reading at BASELINE (<20 weeks)
	
	UA_PROT_PRIOR20_TESTTYPE - Form reported highest protein reading  at BASELINE (<20 weeks) (MNH08 or Hospital form or L&D form)
	
	UA_PROT_PRIOR20_DATE - Date of highest protein reading at BASELINE (<20 weeks)
	
	UA_PROT_PRIOR20_GA - GA at highest protein reading at BASELINE (<20 weeks)
	
	
	//////////////////////////////////////
	*Intermediate indicators for outcomes: 
	GHYP_IND - Qualification for gestational hypertension (mutually exclusive categories, where participants are assigned first to 1, then 2, etc.)
	
	PREEC_IND - Qualification for preeclampsia (mutually exclusive categories, where participants are assigned first to 1, then 2, etc.)
	
	PREEC_SUP_IND - Qualification for preeclampsia superimposed on HTN (mutually exclusive categories, where participants are assigned first to 1, then 2, etc.)
	
	PREEC_SEV_IND - Qualification for preeclampsia with severe features (mutually exclusive categories, where participants are assigned first to 1, then 2, etc.)
	
		
*/

///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
////// * * Part I: Pull variables from CRFs * * //////
///////////////////////////////////////////////////////
	
	////////////////////////////////////////////
	*POC form (MNH06) - BP measures: 
	import delimited "$da/mnh06_merged", bindquote(strict)
	
	rename momid MOMID 
	rename pregid PREGID 

	
	drop if MOMID == "" | PREGID == ""
	
	
	*Visit type 
	gen TYPE_VISIT = m06_type_visit
	tab TYPE_VISIT, m 
	
	*** Create visit type label: 
	label define vistype 1 "1-Enrollment" 2 "2-ANC-20" 3 "3-ANC-28" ///
		4 "4-ANC-32" 5 "5-ANC-36" 6 "6-IPC" 7 "7-PNC-0" 8 "8-PNC-1" ///
		9 "9-PNC-4" 10 "10-PNC-6" 11 "11-PNC-26" 12 "12-PNC-52" ///
		13 "13-ANC-Unsched" 14 "14-PNC-Unsched" 
	
	label var TYPE_VISIT "MNH06 Visit Type"
	label values TYPE_VISIT vistype
	tab TYPE_VISIT, m 
	
	*generate visit date: 
	gen VISIT_DATE = date(m06_diag_vsdat, "YMD") if ///
		m06_diag_vsdat != "1907-07-07" & m06_diag_vsdat != "1905-05-05" 
		
	*fix visit date errors: 	
	replace VISIT_DATE = date("20240322", "YMD") if VISIT_DATE == date("29240322", "YMD")
	replace VISIT_DATE = date("20240509", "YMD") if VISIT_DATE == date("22240509", "YMD")
	
	format VISIT_DATE %td 
	sum VISIT_DATE, format	
	
	*BP Taken: 
	tab m06_bp_vsstat, m 
	
		gen BP_VSSTAT = m06_bp_vsstat 
		gen BP_VSSTAT_2 = m06_bp_vsstat_2 
		gen BP_VSSTAT_3 = m06_bp_vsstat_3
	
	foreach num of numlist 1/3 {
	
	gen BP_SYS_VSORRES_`num' = m06_bp_sys_vsorres_`num' if ///
		m06_bp_sys_vsorres_`num' >= 0 & m06_bp_sys_vsorres_`num' < 900
		
	gen BP_DIA_VSORRES_`num' = m06_bp_dia_vsorres_`num' if ///
		m06_bp_dia_vsorres_`num' >= 0 & m06_bp_dia_vsorres_`num' < 900
	
	}
	
	*restrict to needed dataset: 
	
	keep MOMID PREGID TYPE_VISIT VISIT_DATE BP_VSSTA* ///
		BP_SYS_VSORRES* BP_DIA_VSORRES* 
		
		
	save "$wrk/BP_mnh06", replace 
		
	clear 
	
	
	////////////////////////////////////////////
	*ANC Form (MNH04) - HDP diagnoses: 
	import delimited "$da/mnh04_merged", bindquote(strict)
	
	rename momid MOMID 
	rename pregid PREGID 
	
	drop if MOMID == "" | PREGID == ""
	
	
	*Visit type 
	gen TYPE_VISIT = m04_type_visit
	tab TYPE_VISIT, m 
	
	*** Create visit type label:	
	label var TYPE_VISIT "MNH04 Visit Type"
	label define vistype 1 "1-Enrollment" 2 "2-ANC-20" 3 "3-ANC-28" ///
		4 "4-ANC-32" 5 "5-ANC-36" 6 "6-IPC" 7 "7-PNC-0" 8 "8-PNC-1" ///
		9 "9-PNC-4" 10 "10-PNC-6" 11 "11-PNC-26" 12 "12-PNC-52" ///
		13 "13-ANC-Unsched" 14 "14-PNC-Unsched" 
		
	label values TYPE_VISIT vistype
	tab TYPE_VISIT, m 
	
	*generate visit date: 
	gen VISIT_DATE = date(m04_anc_obsstdat, "YMD") if ///
		m04_anc_obsstdat != "1907-07-07"
		
	*fix visit date errors: 	
	replace VISIT_DATE = date("20240509", "YMD") if VISIT_DATE == date("22240509", "YMD")
	replace VISIT_DATE = . if VISIT_DATE == date("20281124", "YMD")
	
	
	format VISIT_DATE %td 
	sum VISIT_DATE, format
	
	* Variables needed: 
	
	*ever hypertension (chronic): 
	gen HTN_EVER_MHOCCUR = m04_htn_ever_mhoccur
	label var HTN_EVER_MHOCCUR "Ever dx with chronic hypertension"
	
	*treatment for chronic hypertension: 
	gen HTN_CMOCCUR = m04_htn_cmoccur 
	label var HTN_CMOCCUR "Currently treated for chronic hypertension"
	
	*medication for chronic hypertension (per outcome def):
		*magnesium sulfate: HTN_CMTRT_3
		*hydralazine: HTN_CMTRT_4
		*methyldopa (aldomet): HTN_CMTRT_7
		*atenolol: HTN_CMTRT_10
		*nifedipine: HTN_CMTRT_5
		*betamethasone: HTN_CMTRT_8
		*labetalol: HTN_CMTRT_6
		*dexamethasone: HTN_CMTRT_9
		
	gen HTN_CHRONIC_TREAT = 0 if HTN_EVER_MHOCCUR == 1 
	
	foreach num of numlist 3 4 7 10 5 8 6 9 {
	    
	replace HTN_CHRONIC_TREAT = 1 if m04_htn_cmtrt_`num' == 1 
	
	}
	
	label var HTN_CHRONIC_TREAT "Treated for chronic hypertension w/ a medication of interest"
	tab HTN_CHRONIC_TREAT HTN_EVER_MHOCCUR, m 
	

	//////////////////////////////////
	*HDP diagnoses:
	
	tab m04_hpd_mhterm, m 
	
	gen HDP_MHOCCUR = m04_hpd_mhoccur
	label var HDP_MHOCCUR "Hypertensive disorders of pregnancy (any)"
	tab HDP_MHOCCUR, m 
	
	gen HDP_DX_GHYP = 0 if HDP_MHOCCUR == 1 | HDP_MHOCCUR == 0 
	replace HDP_DX_GHYP = 1 if m04_hpd_mhterm == 1 
	label var HDP_DX_GHYP "DX of gestational hypertension"
	
	gen HDP_DX_PREEC = 0 if HDP_MHOCCUR == 1 | HDP_MHOCCUR == 0 
	replace HDP_DX_PREEC = 1 if m04_hpd_mhterm == 2 
	label var HDP_DX_PREEC "DX of preeclampsia"
	
	gen HDP_DX_HTN = 0 if HDP_MHOCCUR == 1 | HDP_MHOCCUR == 0 
	replace HDP_DX_HTN = 1 if m04_hpd_mhterm == 3 
	label var HDP_DX_HTN "DX of chronic hypertension"	
	
	gen HDP_DX_ECL = 0 if HDP_MHOCCUR == 1 | HDP_MHOCCUR == 0 
	replace HDP_DX_ECL = 1 if m04_hpd_mhterm == 4 
	label var HDP_DX_ECL "DX of eclampsia"	
	
	gen HDP_DX_DK = 0 if HDP_MHOCCUR == 1 | HDP_MHOCCUR == 0 
	replace HDP_DX_DK = 1 if m04_hpd_mhterm == 99
	label var HDP_DX_DK "DX of HDP unspecified"	
	
	list HDP_MHOCCUR HDP_DX_GHYP HDP_DX_HTN HDP_DX_PREEC HDP_DX_ECL HDP_DX_DK ///
		if HDP_MHOCCUR==1
	
	
	*treatment for HDP diagnosis: 
	gen HDP_TREAT = 0 if HDP_MHOCCUR == 1 
	
	foreach num of numlist 3 4 7 10 5 8 6 9 {
	    
	replace HDP_TREAT = 1 if m04_hpd_oectrt_`num' == 1 
	
	}
	
	label var HDP_TREAT "Treated for HDP w/ a medication of interest"
	tab HDP_TREAT HDP_MHOCCUR, m 	
	
	
	*** merge in for GA at visit:	
	merge m:1 MOMID PREGID using "$OUT/MAT_ENROLL", keepusing(PREG_START_DATE)
	
	tab _merge 
	
	drop if _merge == 2
	drop _merge 
	
	*update var format PREG_START_DATE
	rename PREG_START_DATE STR_PREG_START_DATE
	
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td 
	
	*** merge in date for pregnancy ended:
	merge m:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing(PREG_END ///
		PREG_END_DATE PREG_END_GA)
	
	drop if _merge == 2
	drop _merge 
	
	gen VISIT_GA = VISIT_DATE - PREG_START_DATE if ///
		(PREG_END==0 | PREG_END==.) | ///
		(PREG_END==1 & VISIT_DATE <= PREG_END_DATE & PREG_END_DATE !=.)
	
	sum VISIT_GA
	
	replace VISIT_GA = . if VISIT_GA <0 | VISIT_GA > 400
	
	tab VISIT_GA TYPE_VISIT,m 	
	
	
	//// Keep preexisting chronic HTN variables:
	
	preserve 
	
	rename TYPE_VISIT HTN_TYPE_VISIT
	rename VISIT_DATE HTN_VISIT_DATE
	rename VISIT_GA HTN_VISIT_GA 
	
	keep MOMID PREGID HTN_* 
	
	drop if HTN_EVER_MHOCCUR == 77 & HTN_CMOCCUR == 77 & HTN_CHRONIC_TREAT == .
	
	save "$wrk/HTN_mnh04", replace 
	
	restore 
	
	
	//// Restrict to needed variables for HDP diagnoses: 
	
	rename TYPE_VISIT HDP_TYPE_VISIT
	rename VISIT_DATE HDP_VISIT_DATE
	rename VISIT_GA HDP_VISIT_GA 	
	
	keep MOMID PREGID HDP_* 
	
	save "$wrk/HDP_mnh04", replace 	
	
	clear 
	
	
	////////////////////////////////////////////
	*L&D (MNH09) - HDP diagnoses & measures: 
	import delimited "$da/mnh09_merged", bindquote(strict)
	
	rename momid MOMID 
	rename pregid PREGID 
	
	drop if MOMID == "" | PREGID == ""	
	
	
	* flag delivery location (for review): 
	gen MAT_LD_OHOLOC = m09_mat_ld_oholoc
	
	label var MAT_LD_OHOLOC "Location of delivery"
	
	label define loc  1 "1-Facility" 2 "2-Home" 88 "88-Other"
	label values MAT_LD_OHOLOC loc
	
	tab MAT_LD_OHOLOC site, m 
	
	
	* HDP_HTN_MHOCCUR - diagnoses 
	foreach num of numlist 1/3 77 99 {
	
	gen HDP_HTN_MHOCCUR_`num' = m09_hdp_htn_mhoccur_`num'
	tab HDP_HTN_MHOCCUR_`num', m 
	
	}
	
	label var HDP_HTN_MHOCCUR_1 "DX of chronic hypertension at L&D"
	label var HDP_HTN_MHOCCUR_2 "DX of gestational hypertension at L&D"
	label var HDP_HTN_MHOCCUR_3 "DX of preeclampsia at L&D"
	label var HDP_HTN_MHOCCUR_77 "No dx of HDP at L&D"
	label var HDP_HTN_MHOCCUR_99 "Unknown info on HDP at L&D"
	
	ren HDP_HTN_MHOCCUR_1 HDP_DX_HTN 
	ren HDP_HTN_MHOCCUR_2 HDP_DX_GHYP 
	ren HDP_HTN_MHOCCUR_3 HDP_DX_PREEC
	ren HDP_HTN_MHOCCUR_77 HDP_DX_NONE 
	ren HDP_HTN_MHOCCUR_99 HDP_DX_DK
	
	* Code severe features: 
	
	gen SEVERE_SEIZURES = m09_preeclampsia_ceoccur_1 if ///
		m09_preeclampsia_ceoccur_1 == 0 | m09_preeclampsia_ceoccur_1 == 1 
		
	gen SEVERE_HELLP = m09_preeclampsia_ceoccur_2 if ///
		m09_preeclampsia_ceoccur_2 == 0 | m09_preeclampsia_ceoccur_2 == 1 
		
	gen SEVERE_SEVHYP = m09_preeclampsia_ceoccur_3 if ///
		m09_preeclampsia_ceoccur_3 == 0 | m09_preeclampsia_ceoccur_3 == 1 
		
	gen SEVERE_PE = m09_preeclampsia_ceoccur_4 if ///
		m09_preeclampsia_ceoccur_4 == 0 | m09_preeclampsia_ceoccur_4== 1 
		
	gen SEVERE_VISUAL = m09_preeclampsia_ceoccur_5 if ///
		m09_preeclampsia_ceoccur_5 == 0 | m09_preeclampsia_ceoccur_5== 1 
		
	gen SEVERE_ANY = 0 if HDP_DX_PREEC == 1 | HDP_DX_GHYP == 1 | ///
		HDP_DX_HTN == 1 | HDP_DX_DK == 1 
	replace SEVERE_ANY = 1 if SEVERE_SEIZURES == 1 | SEVERE_HELLP == 1 | ///
		SEVERE_SEVHYP == 1 | SEVERE_PE == 1 | SEVERE_VISUAL == 1 
		
	label var SEVERE_SEIZURES "Severe features: seizures or eclampsia"
	label var SEVERE_HELLP "Severe features: HELLP syndrome"
	label var SEVERE_SEVHYP "Severe features: severe hypertension"
	label var SEVERE_PE "Severe features: pulmonary edema"
	label var SEVERE_VISUAL "Severe features: visual symptoms"
	label var SEVERE_ANY "Any severe feature(s) of preeclampsia"
	
	foreach var of varlist SEVERE_* {
	
	tab `var', m 
	
	}
	
	
	*BP Taken: 
	tab m09_bp_vsstat, m 

	
		gen BP_VSSTAT = m09_bp_vsstat 
	
	foreach num of numlist 1/3 {
	
	gen BP_SYS_VSORRES_`num' = m09_bp_sys_vsorres_`num' if ///
		m09_bp_sys_vsorres_`num' >= 0 & m09_bp_sys_vsorres_`num' < 900
		
	gen BP_DIA_VSORRES_`num' = m09_bp_dia_vsorres_`num' if ///
		m09_bp_dia_vsorres_`num' >= 0 & m09_bp_dia_vsorres_`num' < 900
	
	}	
	
	
	*info source: HDP_HTN_SRCE 
	foreach num of numlist 1/3 {
	gen HDP_HTN_SRCE_`num' = m09_hdp_htn_srce_`num'
	}
	
	label var HDP_HTN_SRCE_1 "Source of info at L&D: maternal recall"
	label var HDP_HTN_SRCE_2 "Source of info at L&D: facilty-based or participant record"
	label var HDP_HTN_SRCE_3 "Source of info at L&D: study assessment"
	
	
	*treatment for HDP diagnosis: 
	gen HDP_TREAT_LD = 0  
	
	foreach num of numlist 3 4 7 10 5 8 6 9 {
	    
	replace HDP_TREAT = 1 if m09_hdp_htn_proccur_`num' == 1 
	
	}
	
	label var HDP_TREAT_LD "Treated for HDP w/ a medication of interest at L&D"
	tab HDP_TREAT_LD, m 		
	
	
	*treatment for HDP diagnosis -- new variable: 
	tab m09_hdp_ld_mhterm, m 
	
	gen HDP_TREAT2_LD = 0  if m09_hdp_ld_mhterm == 1 | m09_hdp_ld_mhterm == 0 
	
	foreach num of numlist 1/5 {
	    
	replace HDP_TREAT2_LD = 1 if m09_hdp_ld_oectrt_`num' == 1 
	
	}
	
	label var HDP_TREAT2_LD "Treated for HDP w/ a medication of interest at L&D"
	tab HDP_TREAT2_LD, m 		
	
	/////////////////////////////
	* Protein at L&D:
	
	gen UA_LD = m09_labor_ua_spcperf
	label var UA_LD "Urinanalysis done at L&D"
	tab UA_LD, m 
	
	gen UA_LD_BLOOD = m09_labor_ua_blood
	label var UA_LD_BLOOD "UA sample contaminated with blood at L&D"
	tab UA_LD_BLOOD, m 
	
	gen UA_LD_MEM = m09_labor_ua_prot_mem
	label var UA_LD_MEM "UA sample AFTER ROM at L&D"
	tab UA_LD_MEM, m 
	
	gen UA_PROT_LBORRES = m09_labor_ua_prot_lborres
	label var UA_PROT_LBORRES "Proteinuria result"
	tab UA_PROT_LBORRES, m 

	
	/////////////////////////////
	* Other symptoms of illness:
	
	gen SYMPTOM_HEADACHE_LD = m09_headache_ceoccur
	label var SYMPTOM_HEADACHE_LD "Severe headache at L&D"
	
	tab SYMPTOM_HEADACHE_LD, m 
	
	gen SYMPTOM_SEIZURE_LD = m09_othr_ceoccur_5 
	label var SYMPTOM_SEIZURE_LD "Seizure at L&D"
	
	tab SYMPTOM_SEIZURE_LD, m 
	
	gen SYMPTOM_PROTEIN_LD = m09_othr_ceoccur_8
	label var SYMPTOM_PROTEIN_LD "Excess protein in urine at L&D"
	
	tab SYMPTOM_PROTEIN_LD, m 
	
	gen SYMPTOM_VISION_LD = m09_othr_ceoccur_11
	label var SYMPTOM_VISION_LD "Vision changes at L&D"
	
	tab SYMPTOM_VISION_LD, m 	
	
	
	/////////////////////////////
	* Other indications of HDPs:
	
	gen INDUCED_HDP = m09_induced_prindc_7
	label var INDUCED_HDP "Induced labor due to HTN or Gestational HTN"
	tab INDUCED_HDP, m 
	
	gen CES_PREEC = m09_ces_prindc_inf1_12 
	
	foreach num of numlist 1/4 {
	
	replace CES_PREEC = 1 if m09_ces_prindc_inf`num'_12 == 1
	 
	}
	
	label var CES_PREEC "Cesarean due to preeclampsia/eclampsia"
	tab CES_PREEC, m 
	
	/////////////////////////////////////////////////////////////////////
	*Set pregnancy end date as a proxy for visit date for these measures:
	gen VISIT_DATE = date(m09_deliv_dsstdat_inf1, "YMD") if ///
		m09_deliv_dsstdat_inf1 != "1907-07-07"
	format VISIT_DATE %td 
	
	sum VISIT_DATE, format
	
	
	/////////////////////////////
	* Restrict to variables of interest: 
	
	keep MOMID PREGID MAT_LD_OHOLOC HDP* SEVERE* SYMPTOM* INDUCED_HDP CES_PREEC ///
		UA_* BP_* VISIT_DATE 
	
	save "$wrk/HDP_mnh09", replace 
	
	
	*** NOTE: Based on feedback from ERS, we want to consider each BP measure at 
	*** L&D to be a distinct instance (i.e., we will not combine them). This in
	*** mind, we need to create a separate, long dataset for MNH09 entries: 
	
	keep MOMID PREGID BP_* VISIT_DATE 
	
	drop BP_VSSTAT 
	
	reshape long BP_SYS_VSORRES_ BP_DIA_VSORRES_, i(MOMID PREGID VISIT_DATE) j(ENTRY_NUM)
	
	rename BP_SYS_VSORRES_ BP_SYS_VSORRES_1
	rename BP_DIA_VSORRES_ BP_DIA_VSORRES_1
	
	save "$wrk/BP_mnh09_long", replace 
	
	clear 
	
	////////////////////////////////////////////
	*Post-L&D - HDP diagnoses & measures: 
	import delimited "$da/mnh10_merged", bindquote(strict)
	
	rename momid MOMID 
	rename pregid PREGID 
	
	drop if MOMID == "" | PREGID == ""	
	
	
	*Post-L&D symptoms: 
	
	gen SYMPTOM_HEADACHE_PLD = m10_headache_ceoccur
	label var SYMPTOM_HEADACHE_PLD "Severe headache post-L&D"
	
	tab SYMPTOM_HEADACHE_PLD, m 
	
	gen SYMPTOM_SEIZURE_PLD = m10_seizure_ceoccur
	label var SYMPTOM_SEIZURE_PLD "Seizure post-L&D"
	
	tab SYMPTOM_SEIZURE_PLD, m 
	
	gen SYMPTOM_EPIPAIN_PLD = m10_epigastr_pain_ceoccur
	label var SYMPTOM_EPIPAIN_PLD "Epi-gastric pain post-L&D"
	
	tab SYMPTOM_EPIPAIN_PLD, m 
	
	*BP Taken: 
	tab m10_bp_vsstat, m 
	
		gen BP_VSSTAT = m10_bp_vsstat 
	
	foreach num of numlist 1/3 {
	
	gen BP_SYS_VSORRES_`num' = m10_bp_sys_vsorres_`num' if ///
		m10_bp_sys_vsorres_`num' >= 0 & m10_bp_sys_vsorres_`num' < 900
		
	gen BP_DIA_VSORRES_`num' = m10_bp_dia_vsorres_`num' if ///
		m10_bp_dia_vsorres_`num' >= 0 & m10_bp_dia_vsorres_`num' < 900
	
	}	

	
	/////////////////////////////////////////////////////////////////////
	*Set interview date as a proxy for visit date for these measures:
	gen VISIT_DATE = date(m10_visit_obsstdat, "YMD") if ///
		m10_visit_obsstdat != "1907-07-07" & m10_visit_obsstdat != "1905-05-05"
	format VISIT_DATE %td 
	
		/*** TEMP CLEANING - this was used for annual meeting for the sake of 
		*** completeness, but ideally to be revised by the site based on 
		*** queries.
		replace VISIT_DATE = date("20230718", "YMD") if ///
			VISIT_DATE == date("20130718", "YMD")
		*/
	
	sum VISIT_DATE, format	
	
	
	keep MOMID PREGID VISIT_DATE BP_* SYMPTOM* 
	
	save "$wrk/HDP_mnh10", replace 
	
	*** NOTE: Based on feedback from ERS, we want to consider each BP measure at 
	*** L&D (and right after) to be a distinct instance (i.e., we will not 
	*** combine them). This in mind, we need to create a separate, long dataset 
	*** for MNH10 entries: 
	keep MOMID PREGID VISIT_DATE BP_*
	
	drop BP_VSSTAT 
	
	reshape long BP_SYS_VSORRES_ BP_DIA_VSORRES_, i(MOMID PREGID VISIT_DATE) j(ENTRY_NUM)
	
	rename BP_SYS_VSORRES_ BP_SYS_VSORRES_1
	rename BP_DIA_VSORRES_ BP_DIA_VSORRES_1
	
	save "$wrk/BP_mnh10_long", replace 
	
	clear
	////////////////////////////////////////////
	*Lab form (MNH08) - Proteinuria measures: 
	import delimited "$da/mnh08_merged", bindquote(strict)
	
	rename momid MOMID 
	rename pregid PREGID 
	
	drop if MOMID == "" | PREGID == ""
	
	
	*Visit type 
	gen TYPE_VISIT = m08_type_visit
	tab TYPE_VISIT, m 
	
	*** Create visit type label: 
	label define vistype 1 "1-Enrollment" 2 "2-ANC-20" 3 "3-ANC-28" ///
		4 "4-ANC-32" 5 "5-ANC-36" 6 "6-IPC" 7 "7-PNC-0" 8 "8-PNC-1" ///
		9 "9-PNC-4" 10 "10-PNC-6" 11 "11-PNC-26" 12 "12-PNC-52" ///
		13 "13-ANC-Unsched" 14 "14-PNC-Unsched" 
	
	label var TYPE_VISIT "MNH08 Visit Type"
	label values TYPE_VISIT vistype
	tab TYPE_VISIT, m 
	
	*generate visit date: 
	gen VISIT_DATE = date(m08_lbstdat, "YMD") if ///
		m08_lbstdat != "1907-07-07" & m08_lbstdat != "1905-05-05" 
		
	*fix visit date errors: 	
	replace VISIT_DATE = date("20240322", "YMD") if VISIT_DATE == date("29240322", "YMD")
	replace VISIT_DATE = date("20240509", "YMD") if VISIT_DATE == date("22240509", "YMD")
	
	format VISIT_DATE %td 
	sum VISIT_DATE, format
	
	
	/////////////////////////////
	* Pull urinalysis variables:
	
	gen UA_PROT_LBORRES = m08_ua_prot_lborres 
	gen UA_LEUK_LBORRES = m08_ua_leuk_lborres
	gen UA_NITRITE_LBORRES = m08_ua_nitrite_lborres 
	
		label var UA_PROT_LBORRES "Protein"
		label var UA_LEUK_LBORRES "Leukocytes"
		label var UA_NITRITE_LBORRES "Nitrates"
		
		label define prot 0 "0: Neg" 1 "1: Trace" 2 "2: 1+" 3 "3: 2+" ///
			4 "4: 3+" 5 "5: 4+"
			
		label define leuk 0 "0: Neg" 1 "1: Trace" 2 "2: 1+" 3 "3: 2+" ///
			4 "4: 3+"
		
		label define nit 0 "0: Neg" 1 "1: Pos"
		
		label values UA_PROT_LBORRES prot 
		label values UA_LEUK_LBORRES leuk 
		label values UA_NITRITE_LBORRES nit
		
		
	foreach var of varlist UA_PROT_LBORRES UA_NITRITE_LBORRES UA_LEUK_LBORRES {
		tab `var', m 
	}
	
	
	*finalize dataset: 
	keep MOMID PREGID UA_* TYPE_VISIT VISIT_DATE
	
	save "$wrk/UA_mnh08", replace 
	

	clear 
	
	
	////////////////////////////////////////////
	*Hospitalization Form (MNH19):
		* Proteinuria measures
		* BP measures 
		* DX of any hypertensive disoders 
		
	import delimited "$da/mnh19_merged", bindquote(strict)
	
	rename momid MOMID 
	rename pregid PREGID 
	
	drop if MOMID == "" | PREGID == "" 
	
	gen VISIT_DATE = date(m19_ohostdat, "YMD") if m19_ohostdat != "1907-07-07"
	replace VISIT_DATE = date(m19_mat_est_ohostdat, "YMD") if VISIT_DATE == . & ///
		m19_mat_est_ohostdat != "1907-07-07" & m19_mat_est_ohostdat != "1905-05-05"

	label var VISIT_DATE "Estimated hospitalization date"
	
	gen HOSPITAL = 1 
	label var HOSPITAL "Records from the hospitalization form"
	
	gen HOSP_TIMING = m19_timing_ohocat
	label define hospt 1 "1-ANC" 2 "2-PNC" 77 "77-N/A"
	label values HOSP_TIMING hospt
	label var HOSP_TIMING "Timing of hospitalization"
	
	tab HOSP_TIMING, m 
	
	* Record BP measures: 
	
	gen BP_SYS_VSORRES_1 = m19_bp_sys_vsorres if m19_bp_sys_vsorres > 0 
	gen BP_DIA_VSORRES_1 = m19_bp_dia_vsorres if m19_bp_dia_vsorres > 0 
	
	gen BP_SYS_VSORRES_2 = m19_bp_gt120_sys_vsorres if m19_bp_gt120_sys_vsorres >0
	gen BP_DIA_VSORRES_2 = m19_bp_gt120_dia_vsorres if m19_bp_gt120_dia_vsorres >0
	
	gen BP_SYS_VSORRES_3 = m19_bp_gt90_sys_vsorres if m19_bp_gt90_sys_vsorres >0
	gen BP_DIA_VSORRES_3 = m19_bp_gt90_dia_vsorres if m19_bp_gt90_dia_vsorres >0	
	
	gen BP_SYS_VSORRES_4 = m19_bp_lt90_sys_vsorres if m19_bp_lt90_sys_vsorres >0 
	gen BP_DIA_VSORRES_4 = m19_bp_lt90_dia_vsorres if m19_bp_lt90_dia_vsorres >0
	
	
	//////////////////////////////
	* Record symptoms potentially indicating severe features of preeclampsia: 
	
	gen SYMPTOM_HEADACHE_HOSP = m19_headache_ceoccur
	gen SYMPTOM_VISION_HOSP = m19_blur_vision_ceoccur
	gen SYMPTOM_SEIZURE_HOSP = m19_seizure_ceoccur 
	gen SYMPTOM_EPIPAIN_HOSP = m19_epigastr_pain_ceoccur 
	
	
	//////////////////////////////	
	*Record diagnoses of HDPs: 
	* HDP_HTN_MHOCCUR - diagnoses 
	foreach num of numlist 1/3 77 99 {
	
	gen HDP_HTN_MHOCCUR_`num' = m19_hdp_htn_mhoccur_`num'
	tab HDP_HTN_MHOCCUR_`num', m 
	
	}
	
	label var HDP_HTN_MHOCCUR_1 "DX of chronic hypertension at hospitalization"
	label var HDP_HTN_MHOCCUR_2 "DX of gestational hypertension at hospitalization"
	label var HDP_HTN_MHOCCUR_3 "DX of preeclampsia at hospitalization"
	label var HDP_HTN_MHOCCUR_77 "No dx of HDP at hospitalization"
	label var HDP_HTN_MHOCCUR_99 "Unknown info on HDP at hospitalization"
	
	ren HDP_HTN_MHOCCUR_1 HDP_DX_HTN 
	ren HDP_HTN_MHOCCUR_2 HDP_DX_GHYP 
	ren HDP_HTN_MHOCCUR_3 HDP_DX_PREEC
	ren HDP_HTN_MHOCCUR_77 HDP_DX_NONE 
	ren HDP_HTN_MHOCCUR_99 HDP_DX_DK
	
	* Code severe features: 
	
	gen SEVERE_SEIZURES = m19_preeclampsia_ceoccur_1 if ///
		m19_preeclampsia_ceoccur_1 == 0 | m19_preeclampsia_ceoccur_1 == 1 
		
	gen SEVERE_HELLP = m19_preeclampsia_ceoccur_2 if ///
		m19_preeclampsia_ceoccur_2 == 0 | m19_preeclampsia_ceoccur_2 == 1 
		
	gen SEVERE_SEVHYP = m19_preeclampsia_ceoccur_3 if ///
		m19_preeclampsia_ceoccur_3 == 0 | m19_preeclampsia_ceoccur_3 == 1 
		
	gen SEVERE_PE = m19_preeclampsia_ceoccur_4 if ///
		m19_preeclampsia_ceoccur_4 == 0 | m19_preeclampsia_ceoccur_4== 1 
		
	gen SEVERE_VISUAL = m19_preeclampsia_ceoccur_5 if ///
		m19_preeclampsia_ceoccur_5 == 0 | m19_preeclampsia_ceoccur_5== 1 
		
	gen SEVERE_ANY = 0 if HDP_DX_PREEC == 1 | HDP_DX_GHYP == 1 | ///
		HDP_DX_HTN == 1 | HDP_DX_DK == 1 
	replace SEVERE_ANY = 1 if SEVERE_SEIZURES == 1 | SEVERE_HELLP == 1 | ///
		SEVERE_SEVHYP == 1 | SEVERE_PE == 1 | SEVERE_VISUAL == 1 
		
	label var SEVERE_SEIZURES "Severe features: seizures or eclampsia"
	label var SEVERE_HELLP "Severe features: HELLP syndrome"
	label var SEVERE_SEVHYP "Severe features: severe hypertension"
	label var SEVERE_PE "Severe features: pulmonary edema"
	label var SEVERE_VISUAL "Severe features: visual symptoms"
	label var SEVERE_ANY "Any severe feature(s) of preeclampsia"
	
	foreach var of varlist SEVERE_* {
	
	tab `var', m 
	
	}
	
	
	*treatment for HDP diagnosis at hospitalization -- new variable: 
	tab m19_hpd_htn_cmoccur_1, m 
	
	gen HDP_TREAT_HOSP = 0  if m19_hpd_htn_cmoccur_1 == 1 | m19_hpd_htn_cmoccur_1 == 0 | ///
		m19_hpd_htn_cmoccur_77==1
	
	foreach num of numlist  3/10 {
	    
	replace HDP_TREAT_HOSP = 1 if m19_hpd_htn_cmoccur_`num' == 1 
	
	}
	
	label var HDP_TREAT_HOSP "Treated for HDP w/ a medication of interest during hospitalization"
	tab HDP_TREAT_HOSP, m 	
	
	
	/////////////////////////////
	*Urinalysis variables: 
	
	gen UA_DATE = date(m19_ua_dip_lbtstdat, "YMD") if m19_ua_dip_lbtstdat != "1907-07-07"
	format UA_DATE %td 
	label var UA_DATE "Date of urinalysis during hospitalization"
	
	gen UA_PROT_LBORRES = m19_ua_prot_lborres 
	gen UA_LEUK_LBORRES = m19_ua_leuk_lborres
	gen UA_NITRITE_LBORRES = m19_ua_nitrite_lborres
	
		label var UA_PROT_LBORRES "Protein"
		label var UA_LEUK_LBORRES "Leukocytes"
		label var UA_NITRITE_LBORRES "Nitrates"
		
		label define prot 0 "0: Neg" 1 "1: 1+" 2 "2: 2+" 3 "3: 3+" ///
			77 "77: NA"
			
		label define leuk 0 "0: Neg" 1 "1: 1+" 2 "2: 2+" 3 "3: 3+" ///
			77 "77: NA"
		
		label define nit 0 "0: Neg" 1 "1: 1+" 2 "2: 2+" 3 "3: 3+" ///
			77 "77: NA"
		
		label values UA_PROT_LBORRES prot 
		label values UA_LEUK_LBORRES leuk 
		label values UA_NITRITE_LBORRES nit
		
		
	foreach var of varlist UA_PROT_LBORRES UA_NITRITE_LBORRES UA_LEUK_LBORRES {
		tab `var', m 
	}
	
	**** Other DX records:
	gen PRIMARY_DX_HPD = 0 if m19_primary_mhterm >= 1 & m19_primary_mhterm <= 6
	replace PRIMARY_DX_HPD = 55 if m19_primary_mhterm == 88 | m19_primary_mhterm == 77 
	replace PRIMARY_DX_HPD = 1 if m19_primary_mhterm == 3
	label var PRIMARY_DX_HPD "Primary diagnosis is a hypertensive disorder"
	
	tab m19_primary_mhterm PRIMARY_DX_HPD, m 

	foreach num of numlist 1/10 {
	
	gen PRIMARY_DX_HDP_`num' = m19_htn_mhterm_`num'
	replace PRIMARY_DX_HDP_`num' = 0 if m19_htn_mhterm_`num' == 77 & ///
		PRIMARY_DX_HPD == 0
		
	tab PRIMARY_DX_HDP_`num'
	
	}
	
	rename PRIMARY_DX_HDP_1 PRIMARY_DX_HDP_HTN
	rename PRIMARY_DX_HDP_2 PRIMARY_DX_HDP_GHYP
	rename PRIMARY_DX_HDP_3 PRIMARY_DX_HDP_PREEC
	rename PRIMARY_DX_HDP_4 PRIMARY_DX_HDP_SEIZURES
	rename PRIMARY_DX_HDP_5 PRIMARY_DX_HDP_HELLP
	rename PRIMARY_DX_HDP_6 PRIMARY_DX_HDP_SEVHYP
	rename PRIMARY_DX_HDP_7 PRIMARY_DX_HDP_PPECL
	rename PRIMARY_DX_HDP_8 PRIMARY_DX_HDP_PULMED
	rename PRIMARY_DX_HDP_9 PRIMARY_DX_HDP_VIS
	rename PRIMARY_DX_HDP_10 PRIMARY_DX_HDP_STROKE
	
	
	*Record other pulmonary edema: 
	gen HOSP_PULMED = 0 if m19_dx_othr_mhterm_4 != 77 
	replace HOSP_PULMED = 1 if m19_dx_othr_mhterm_4 == 1 
	
	/////
	///// Order the HOSP forms: 
		/// order file by person & date 
	sort MOMID PREGID VISIT_DATE HOSP_TIMING
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "Hosp Entry Number"
	
	
	keep MOMID PREGID VISIT_DATE HOSPITAL HOSP_TIMING BP_* SYMPTOM_* ///
		HDP_* SEVERE* UA_* PRIMARY_* HOSP_PULMED ENTRY_NUM
	
	**** save the file: 
	save "$wrk/HDP_mnh19", replace 
	
	
	**** reshape files for BP & Proteinuria 
	
	preserve 
	
	keep MOMID PREGID HOSPITAL HOSP_TIMING BP* VISIT_DATE ENTRY_NUM
	
	reshape long BP_SYS_VSORRES_ BP_DIA_VSORRES_, ///
		i(MOMID PREGID HOSPITAL ENTRY_NUM VISIT_DATE HOSP_TIMING) j(BP_MEASURE_NUM)
		
	rename BP_SYS_VSORRES_ BP_SYS_VSORRES_1 
	rename BP_DIA_VSORRES_ BP_DIA_VSORRES_1 
	
	
	*review timings for people without a visit date: 
	tab HOSP_TIMING if VISIT_DATE == ., m 
	
	
	save "$wrk/BP_mnh19_long", replace 
	
	restore 
	
	keep MOMID PREGID VISIT_DATE HOSPITAL HOSP_TIMING ENTRY_NUM UA_* 
	
	save "$wrk/UA_mnh19", replace 
	
	
	
///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
/// * * Part II: Construct Analysis Variables * * ///
///////////////////////////////////////////////////////	
	
	
*** Create a long BP dataset:
	
	clear 
	
	use "$wrk/BP_mnh06"
	
	gen TIMING = 0 
	
	append using "$wrk/BP_mnh09_long", gen(append_09)
	
	append using "$wrk/BP_mnh10_long", gen(append_10)
	
	append using "$wrk/BP_mnh19_long", gen(append_19)
	
	
		/// restrict to BP variables: 
		keep MOMID PREGID TYPE_VISIT VISIT_DATE BP_* append_09 ///
			append_10 append_19 TIMING HOSP_TIMING
			
	replace TIMING = 1 if append_09 == 1 
	replace TIMING = 2 if append_10 == 1 
	replace TIMING = 3 if append_19 == 1 
	
	label define timing 0 "0-PRISMA staff" 1 "1-L&D" 2 "2-Post-L&D" ///
		3 "3-Hospitalization"
	
	label values TIMING timing 
	
	label var TIMING "Timing of BP measure"
	
	tab TIMING, m 
	
	*DATA CLEANING FOR ANNUAL MEETING DATASET: 
	
	gen CLEANED = 0 
	
	foreach num of numlist 1/3 {
	    
	// drop individual measures where SYSTOLIC is > 220 
	replace CLEANED = 1 if BP_SYS_VSORRES_`num' > 220 & BP_SYS_VSORRES_`num' != . 
	replace BP_SYS_VSORRES_`num' = . if BP_SYS_VSORRES_`num' > 220 & ///
		BP_SYS_VSORRES_`num' != . 
	
	// drop individual measures where DIASTOLIC is > 160
	replace CLEANED = 1 if BP_DIA_VSORRES_`num' > 160 & BP_DIA_VSORRES_`num' != . 
	replace BP_DIA_VSORRES_`num' = . if BP_DIA_VSORRES_`num' > 160 & ///
		BP_DIA_VSORRES_`num' != . 
		
	}

	*Bring together blood pressure variables when multiple taken in the same 
	*instance (i.e., PRISMA staff take 3 measures within 1 minute of each 
	*other -- for these, we will take the mean of the three)
	
	foreach let in SYS DIA {
    egen BP_`let'_VSORRES = rowmean(BP_`let'_VSORRES_1   ///
		BP_`let'_VSORRES_2   BP_`let'_VSORRES_3) 
        }
	
	/*CHECKS: 
	list BP_DIA_VSORRES BP_DIA_VSORRES_1 BP_DIA_VSORRES_2 BP_DIA_VSORRES_3 if ///
		BP_DIA_VSORRES != . 
	list BP_DIA_VSORRES BP_DIA_VSORRES_1 BP_DIA_VSORRES_2 BP_DIA_VSORRES_3 if ///
		BP_DIA_VSORRES_2 != . 
	*/
	

	
	*** merge in for GA at visit:	
	merge m:1 MOMID PREGID using "$OUT/MAT_ENROLL", keepusing(PREG_START_DATE SITE)
	
	drop if _merge == 2
	drop _merge 
	
	*update var format PREG_START_DATE
	rename PREG_START_DATE STR_PREG_START_DATE
	
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td 
	
	*** merge in date for pregnancy ended:
	merge m:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing(PREG_END ///
		PREG_END_DATE PREG_END_GA)
	
	drop if _merge == 2
	drop _merge 
	
	gen VISIT_GA = VISIT_DATE - PREG_START_DATE if ///
		(PREG_END==0 | PREG_END==.) | ///
		(PREG_END==1 & VISIT_DATE <= PREG_END_DATE & PREG_END_DATE !=.)
	
	sum VISIT_GA
	
	replace VISIT_GA = . if VISIT_GA <0 | VISIT_GA > 400
	
	tab VISIT_GA TYPE_VISIT,m 
	
	order MOMID PREGID TIMING TYPE_VISIT VISIT_DATE VISIT_GA, first 
	
	//////////
	* Save a long BP dataset: 
	
	save "$wrk/BP_all_long", replace 
	
	/**** histograms for data cleaning: 
		// diastolic 
	histogram BP_DIA_VSORRES, width(1) percent xline(150)
	
	gen BP_DIA_VSORRES_OVER = 0 if BP_DIA_VSORRES != . 
	replace BP_DIA_VSORRES_OVER = 1 if BP_DIA_VSORRES >= 150 & BP_DIA_VSORRES != . 
	
	tab BP_DIA_VSORRES SITE if BP_DIA_VSORRES_OVER==1
	tab 	
		// systolic
	histogram BP_SYS_VSORRES, width(1) percent xline(200)
	
	gen BP_SYS_VSORRES_OVER = 0 if BP_SYS_VSORRES != . 
	replace BP_SYS_VSORRES_OVER = 1 if BP_SYS_VSORRES >= 200 & BP_SYS_VSORRES != . 
	
	tab BP_SYS_VSORRES SITE if BP_SYS_VSORRES_OVER==1	
	
	list SITE TIMING TYPE_VISIT BP_SYS_VSORRES BP_DIA_VSORRES if ///
		BP_SYS_VSORRES_OVER==1
	
	list SITE TIMING TYPE_VISIT BP_DIA_VSORRES BP_DIA_VSORRES_1 ///
		BP_DIA_VSORRES_2 BP_DIA_VSORRES_3 if ///
		BP_DIA_VSORRES_OVER==1
	*/
	
	//////////////////////////////////////////////////////////////////////
	/////////
	*For main outcomes, we'll restrict to ANC measures & L&D form (MNH09) & ///
		*hospital forms during ANC if date is missing: 
	
	keep if (VISIT_DATE >= PREG_START_DATE & VISIT_DATE <= PREG_END_DATE & PREG_END==1) | /// for completed pregnancies, measures BEFORE end date
			(VISIT_DATE >= PREG_START_DATE & PREG_END_DATE == . & (PREG_END==0 | PREG_END ==.)) /// for ongoing pregnancies, measures AFTER PREG_START_DATE 
			| TIMING == 1 /// include ANY measures recorded in MNH09 (TIMING==1)
			| (HOSP_TIMING == 1 & VISIT_DATE==.) // include ANY measures in MNN19 with a missing date that are recorded in 
		
		/// Split into 2 datasets for <20 weeks and >= 20 weeks:
		/// below, we preserve & save a dataset with all observations of BP <20 weeks GA 
		preserve 
		
			keep if VISIT_GA < (20*7) & VISIT_GA !=.
			
			save "$wrk/BP_all_long_less20", replace 
			
		restore 
		
		
		
	////////////////////////////////////////////////////////////////////	
		/// Review for gestational hypertension: focus on >=20 weeks: 
		/// we will also include hospitalizations with missing timing: 
		keep if VISIT_GA >= (20*7) | /// Keep all observations of BP >= 20 weeks GA
			(VISIT_GA==. & HOSP_TIMING == 1) // for now, also keep hospital forms marked as during ANC, only in instances where the hospital form was NOT dated.
			
			*PAUSE FOR REVIEW: 
			* Which observations drawn from the hospital form are 
			* marked as "PNC" but register as "during pregnancy" based on 
			* visit date and visit GA?
			
			list SITE VISIT_DATE VISIT_GA BP_SYS_VSORRES BP_DIA_VSORRES ///
			PREG_END PREG_END_GA PREG_END_DATE if HOSP_TIMING == 2
			
			*In general, as of the 6-28 data, most of these mis-matching 
			*observations have visit date/visit GA VERY close to the pregnancy 
			*end date (within 1-2 days). Suspect this may be due to the 
			*hospitalization surrounding the pregnancy endpoint; sites might not 
			*be clear about how to fill the "timing" variable for a 
			*hospitalization that encompasses IPC. 
			
		
	/////
	///// Order the BP measures: 
		/// order file by person & date 
	sort MOMID PREGID VISIT_DATE TIMING

	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "BP Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of BP measures"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)
	

	/////
	/////
	
	*Create variables for high High BP & Severe High BP using the mean: 
	
	gen HIGH_BP_SYS = 0 if BP_SYS_VSORRES >0 & BP_SYS_VSORRES <= 140
	replace HIGH_BP_SYS = 1 if BP_SYS_VSORRES >140 & BP_SYS_VSORRES != . 
	
	gen SEVHIGH_BP_SYS = 0 if BP_SYS_VSORRES >0 & BP_SYS_VSORRES <= 160 
	replace SEVHIGH_BP_SYS = 1 if BP_SYS_VSORRES >160 & BP_SYS_VSORRES != .
	
	
	gen HIGH_BP_DIA = 0 if BP_DIA_VSORRES >0 & BP_DIA_VSORRES <= 90
	replace HIGH_BP_DIA = 1 if BP_DIA_VSORRES >90 & BP_DIA_VSORRES != . 
	
	gen SEVHIGH_BP_DIA = 0 if BP_DIA_VSORRES >0 & BP_DIA_VSORRES <= 110 
	replace SEVHIGH_BP_DIA = 1 if BP_DIA_VSORRES >110 & BP_DIA_VSORRES != .

	
	label var HIGH_BP_SYS "High systolic BP measure at the visit (mean)"
	label var HIGH_BP_DIA "High diastolic BP measure at the visit (mean)"
	label var SEVHIGH_BP_SYS "Severe high systolic BP measure at the visit (mean)"
	label var SEVHIGH_BP_DIA "Severe high diastolic BP measure at the visit (mean)"
	
	sort HIGH_BP_SYS
	
	by HIGH_BP_SYS: sum BP_SYS_VSORRES
	
	sort SEVHIGH_BP_SYS
	
	by SEVHIGH_BP_SYS: sum BP_SYS_VSORRES
	
	sort HIGH_BP_DIA
	
	by HIGH_BP_DIA: sum BP_DIA_VSORRES
	
	sort SEVHIGH_BP_DIA
	
	by SEVHIGH_BP_DIA: sum BP_DIA_VSORRES
	
	keep MOMID PREGID TIMING TYPE_VISIT VISIT_DATE VISIT_GA ///
		ENTRY_NUM ENTRY_TOTAL BP_DIA_VSORRES BP_SYS_VSORRES ///
		HIGH_BP_SYS SEVHIGH_BP_SYS HIGH_BP_DIA SEVHIGH_BP_DIA 
	
	*Next, convert to wide:

	reshape wide TIMING TYPE_VISIT VISIT_DATE VISIT_GA  ///
		HIGH* SEVHIGH* BP_DIA_VSORRES BP_SYS_VSORRES ///
		, i(MOMID PREGID ENTRY_TOTAL) j(ENTRY_NUM) 
		
	sum ENTRY_TOTAL
		
	*COUNTS: any BP measure:
	
	gen HIGH_BP_COUNT = 0 
	label var HIGH_BP_COUNT "Number of High BP readings"
	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_COUNT = HIGH_BP_COUNT + 1 if ///
		(HIGH_BP_DIA`num' == 1 | HIGH_BP_SYS`num' == 1)
	
	}
	
	tab HIGH_BP_COUNT, m 
	
	gen HIGH_BP_SEV_COUNT = 0 
	label var HIGH_BP_SEV_COUNT "Number of severe high BP readings"
	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_SEV_COUNT = HIGH_BP_SEV_COUNT + 1 if ///
		(SEVHIGH_BP_DIA`num' == 1 | SEVHIGH_BP_SYS`num' == 1)
	
	}	

	tab HIGH_BP_SEV_COUNT, m 
	
	**** record measure types:
	
	gen BP_COUNT_PRISMA = 0 
	label var BP_COUNT_PRISMA "Number of BP readings by PRISMA staff (MNH06)"
	
	foreach num of numlist 1/$i {
	
	replace BP_COUNT_PRISMA = BP_COUNT_PRISMA + 1 if ///
		(TIMING`num' == 0 )
	
	}
	
	gen BP_COUNT_IPC = 0 
	label var BP_COUNT_IPC "Number of BP readings at L&D or post-L&D outcome (MNH09 or MNH10)"
	
	foreach num of numlist 1/$i {
	
	replace BP_COUNT_IPC = BP_COUNT_IPC + 1 if ///
		(TIMING`num' == 1 | TIMING`num'==2)
	
	}
	
	gen BP_COUNT_HOSP = 0 
	label var BP_COUNT_HOSP "Number of BP readings recorded in hospitalization"
	
	foreach num of numlist 1/$i {
	
	replace BP_COUNT_HOSP = BP_COUNT_HOSP + 1 if ///
		(TIMING`num' == 3)
	
	}
	
	* save a copy of high BP tabulations: 
	save "$wrk/BP_GTE20_wide", replace 
	
	keep MOMID PREGID ENTRY_TOTAL HIGH_BP_COUNT HIGH_BP_SEV_COUNT ///
	BP_COUNT_PRISMA BP_COUNT_IPC BP_COUNT_HOSP 
	
	
	* save a copy of high BP tabulations: 
	save "$wrk/BP_tabulation", replace 
	 
	
	
//////////////////////////////////////////////////////////////

	*Construct a chronic HTN dataset: 
	
	/////////////////////////
	* First, we'll look at measured BP at <20 weeks GA: 
	clear 
	use "$wrk/BP_all_long_less20"
	
	/////
	///// Order the BP measures: 
		/// order file by person & date 
	sort MOMID PREGID VISIT_DATE TIMING
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "BP Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of BP measures"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)
	/////
	/////
	
	*Create variables for high High BP & Severe High BP using the mean: 
	
	gen HIGH_BP_SYS = 0 if BP_SYS_VSORRES >0 & BP_SYS_VSORRES <= 140
	replace HIGH_BP_SYS = 1 if BP_SYS_VSORRES >140 & BP_SYS_VSORRES != . 
	
	gen SEVHIGH_BP_SYS = 0 if BP_SYS_VSORRES >0 & BP_SYS_VSORRES <= 160 
	replace SEVHIGH_BP_SYS = 1 if BP_SYS_VSORRES >160 & BP_SYS_VSORRES != .
	
	
	gen HIGH_BP_DIA = 0 if BP_DIA_VSORRES >0 & BP_DIA_VSORRES <= 90
	replace HIGH_BP_DIA = 1 if BP_DIA_VSORRES >90 & BP_DIA_VSORRES != . 
	
	gen SEVHIGH_BP_DIA = 0 if BP_DIA_VSORRES >0 & BP_DIA_VSORRES <= 110 
	replace SEVHIGH_BP_DIA = 1 if BP_DIA_VSORRES >110 & BP_DIA_VSORRES != .

	
	label var HIGH_BP_SYS "High systolic BP measure at the visit (mean)"
	label var HIGH_BP_DIA "High diastolic BP measure at the visit (mean)"
	label var SEVHIGH_BP_SYS "Severe high systolic BP measure at the visit (mean)"
	label var SEVHIGH_BP_DIA "Severe high diastolic BP measure at the visit (mean)"
	
	sort HIGH_BP_SYS
	
	by HIGH_BP_SYS: sum BP_SYS_VSORRES
	
	sort SEVHIGH_BP_SYS
	
	by SEVHIGH_BP_SYS: sum BP_SYS_VSORRES
	
	sort HIGH_BP_DIA
	
	by HIGH_BP_DIA: sum BP_DIA_VSORRES
	
	sort SEVHIGH_BP_DIA
	
	by SEVHIGH_BP_DIA: sum BP_DIA_VSORRES
	
	keep MOMID PREGID TIMING TYPE_VISIT VISIT_DATE VISIT_GA ///
		ENTRY_NUM ENTRY_TOTAL HIGH_BP_SYS SEVHIGH_BP_SYS ///
		HIGH_BP_DIA SEVHIGH_BP_DIA BP_DIA_VSORRES BP_SYS_VSORRES ///
		HOSP_TIMING
	
	*Next, convert to wide:

	reshape wide TIMING TYPE_VISIT VISIT_DATE VISIT_GA  ///
		HIGH* SEVHIGH* HOSP_TIMING BP_DIA_VSORRES BP_SYS_VSORRES ///
		, i(MOMID PREGID ENTRY_TOTAL) j(ENTRY_NUM) 
		
	sum ENTRY_TOTAL
		
	*COUNTS: any BP measure:
	
	gen HIGH_BP_COUNT = 0 
	label var HIGH_BP_COUNT "Number of High BP readings"
	
	gen HIGH_BP_SEV_COUNT = 0 
	label var HIGH_BP_SEV_COUNT "Number of severe high BP readings"
	
	foreach num of numlist 1/$i {
	
	replace HIGH_BP_COUNT = HIGH_BP_COUNT + 1 if ///
		(HIGH_BP_DIA`num' == 1 | HIGH_BP_SYS`num' == 1)
		
	replace HIGH_BP_SEV_COUNT = HIGH_BP_SEV_COUNT + 1 if ///
		(SEVHIGH_BP_SYS`num' == 1 | SEVHIGH_BP_DIA`num' == 1)
	
	}
	
	tab HIGH_BP_COUNT, m 
	
	*review entries: 
	list if HIGH_BP_COUNT >1
	
	keep MOMID PREGID ENTRY_TOTAL HIGH_BP_COUNT HIGH_BP_SEV_COUNT
	
	rename HIGH_BP_COUNT HIGH_BP_COUNT_LESS20 
	rename HIGH_BP_SEV_COUNT HIGH_BP_SEV_COUNT_LESS20
	
	save "$wrk/BP_tabulation_less20", replace
	clear 
	
	
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

	/////////////////////////
	* Then, we'll look at PREEXISTING dx of chronic HTN: 
	
	use "$wrk/HTN_mnh04"
	
	gen HTN_CHRONIC = 0 if HTN_EVER_MHOCCUR == 0 
	replace HTN_CHRONIC = 1 if HTN_EVER_MHOCCUR == 1 
	replace HTN_CHRONIC = 55 if HTN_EVER_MHOCCUR != 0 & HTN_EVER_MHOCCUR != 1
	
	label var HTN_CHRONIC "Chronic hypertension dx (before enrollment)"
	
	tab HTN_CHRONIC, m 
	tab HTN_CHRONIC_TREAT, m 
	 replace HTN_CHRONIC_TREAT = 55 if HTN_CHRONIC == 1 & HTN_CHRONIC_TREAT == 55
	 replace HTN_CHRONIC_TREAT = 77 if HTN_CHRONIC_TREAT == . 
	 
	tab HTN_CHRONIC HTN_CHRONIC_TREAT, m 
	
	gen HTN_HISTORY = HTN_CHRONIC 
	replace HTN_HISTORY = 1 if HTN_CHRONIC_TREAT == 1 
	
	label var HTN_HISTORY "Participant dx or currently treated for hypertension before enrollment"
	
	tab HTN_HISTORY, m 
	
	gen HTN_HISTORY_IND = 1 if HTN_CHRONIC_TREAT ==1 // here, we want the MINIMUM value to be prioritized 
	replace HTN_HISTORY_IND = 2 if HTN_CHRONIC ==1 & HTN_HISTORY_IND ==.
	
	tab HTN_HISTORY HTN_HISTORY_IND, m 
	
	
	**** deduplicate: 
		/// order file by person & date 
	sort MOMID PREGID HTN_VISIT_DATE
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of entries"
	
	*this is an appropriate use for collapse:
	
	keep MOMID PREGID HTN_VISIT_DATE HTN_TYPE_VISIT HTN_HISTORY HTN_HISTORY_IND
	
	replace HTN_HISTORY = -55 if HTN_HISTORY == 55
	
	collapse (max) HTN_HISTORY (min) HTN_HISTORY_IND, by(MOMID PREGID)
		
	label var HTN_HISTORY "Participant dx or currently treated for hypertension before enrollment"	
	
	replace HTN_HISTORY = 55 if HTN_HISTORY == -55
	
	tab HTN_HISTORY, m 
	tab HTN_HISTORY_IND, m 
	label var HTN_HISTORY_IND "Chronic hypertension based on: 1=current treatment or 2=reported dx"
	
	save "$wrk/History_chronic_htn", replace 
	
	
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
	
	/////////////////////////
	*Then, we'll look at dx of chronic htn in early pregnancy: 
	clear 
	use "$wrk/HDP_mnh04"
	
	tab HDP_DX_HTN, m 
	
	keep if HDP_VISIT_GA <(20*7) & HDP_VISIT_GA >= 0 
	
		// among visits <20 weeks GA: 
		tab HDP_MHOCCUR, m 
		tab HDP_DX_HTN, m 
		tab HDP_DX_GHYP, m 
		tab HDP_DX_PREEC HDP_TYPE_VISIT, m 
		tab HDP_DX_DK, m 
		tab HDP_DX_ECL HDP_TYPE_VISIT, m 
		
	replace HDP_MHOCCUR = -55 if HDP_MHOCCUR == 77 | HDP_MHOCCUR == 99
		
	foreach var of varlist  HDP_DX_HTN  HDP_DX_GHYP HDP_DX_PREEC HDP_DX_DK HDP_DX_ECL {
	
	replace `var' = -55 if HDP_MHOCCUR == -55
	
	}
	
	replace HDP_TREAT = 0 if HDP_TREAT == . 
	
	///////////////////////////
	*** review for duplicates: 

	**** deduplicate: 
		/// order file by person & date 
	sort MOMID PREGID HDP_VISIT_DATE
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of entries"
	
	*** who are the people with 6+ entries? 
	list MOMID HDP_* ENTRY_TOTAL ENTRY_NUM if ENTRY_TOTAL >=6
	
	*** collapse this dataset to one entry per person
 
	keep MOMID PREGID HDP_MHOCCUR HDP_DX_HTN HDP_DX_GHYP HDP_DX_PREEC ///
		HDP_DX_DK HDP_TREAT HDP_DX_ECL 
	
	collapse (max) HDP_*, by(MOMID PREGID)
	///////////////////////////////
	
	tab HDP_MHOCCUR, m 
	
	foreach var of varlist HDP_* {
	replace `var' = 55 if `var' == -55
	}
	
	save "$wrk/New_dx_htn_less20", replace 
	
	clear 
	

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

	*Merge together a wide HTN dataset:
		*This dataset will be built on MAT_ENROLL (to ensure all participants 
		*in the final dataset are enrolled) and will include the following 
		*mechanisms for identifying cases of CHTN: 
		
			*Diagnosis reported at enrollment: 
				*working dataset: "$wrk/History_chronic_htn"
				*variable name: HTN_HISTORY
			
			*HTN medications taken at enrollment: 
				*working dataset: "$wrk/History_chronic_htn"
				*variable name: HTN_HISTORY
			
			*New diagnosis of HTN (or another HDP) at <20 weeks GA: 
				*working dataset: "$wrk/New_dx_htn_less20"
				*variable names: HDP_MHOCCUR HDP_DX_HTN  HDP_DX_GHYP HDP_DX_PREEC HDP_DX_DK HDP_DX_ECL 
				
			*New HTN medications at <20 weeks GA: 
				*working dataset: "$wrk/New_dx_htn_less20"
				*variable names: HDP_TREAT 				
			
			*1+ severe high BP readings at <20 weeks GA: 
				*working dataset: "$wrk/BP_tabulation_less20"
				*variable name: HIGH_BP_SEV_COUNT_LESS20
			
			*2+ high BP readings: 
				*working dataset: "$wrk/BP_tabulation_less20"
				*variable name: HIGH_BP_COUNT_LESS20 
				*working dataset 2: "$wrk/BP_tabulation"
				*variable name: HIGH_BP_COUNT 
		
		
		
	use "$OUT/MAT_ENROLL"
	
	keep if ENROLL == 1  
	
	*update var format PREG_START_DATE
	rename PREG_START_DATE STR_PREG_START_DATE
	
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td 
	
	* Merge in: 
	merge 1:1 MOMID PREGID using "$wrk/BP_tabulation_less20"
	
		drop if _merge == 2 
		drop _merge 
	
	merge 1:1 MOMID PREGID using "$wrk/History_chronic_htn"
	
		drop if _merge == 2 
		drop _merge 
		
	merge 1:1 MOMID PREGID using "$wrk/New_dx_htn_less20"
	
		drop if _merge == 2 
		drop _merge 
		
	merge 1:1 MOMID PREGID using "$wrk/BP_tabulation", keepusing(HIGH_BP_COUNT)
	
		drop if _merge == 2 
		drop _merge  
		
	* Review all possible criteria:
	
	tab HIGH_BP_COUNT_LESS20 HIGH_BP_COUNT, m 
	tab HTN_HISTORY, m 
	tab HDP_MHOCCUR, m 
	
		tab HDP_DX_HTN, m 
		tab HDP_DX_GHYP, m 
		tab HDP_DX_PREEC, m 
		tab HDP_DX_DK, m 
		tab HDP_DX_ECL, m 
		
	tab HDP_TREAT, m 
	
	gen HTN_ANY = 0 if HTN_HISTORY == 0 & ///
		HDP_MHOCCUR == 0 & HDP_TREAT == 0 
		
	*Updating the criteria below (7-2-2024): per CRS, we will consider chronic 
	*HTN IF the participant had at least 1 high BP reading at <20 weeks GA, 
	*while second reading can be any time in pregnancy: 
		
	replace HTN_ANY = 1 if ///
		(HIGH_BP_COUNT_LESS20 >=2 & HIGH_BP_COUNT_LESS20!=. ) | /// at least 2 before 20 weeks 
		(HIGH_BP_COUNT_LESS20 >=1 & HIGH_BP_COUNT_LESS20!= . & ///
		  HIGH_BP_COUNT >= 1 & HIGH_BP_COUNT != . ) | /// 2+ with first <20 weeks 
		(HIGH_BP_SEV_COUNT_LESS20 >=1 & HIGH_BP_SEV_COUNT_LESS20 != .) | /// any severe high BP <20
		HTN_HISTORY == 1 | HDP_MHOCCUR == 1 | HDP_TREAT == 1 
		
	replace HTN_ANY = 55 if HTN_ANY == . 
		
	tab HTN_ANY, m 
	
	label var HTN_ANY "Any report of chronic HTN/HDP at <20 weeks GA"
	
	order HTN_ANY, after(PREGID)
	
	rename HIGH_BP_COUNT HIGH_BP_COUNT_AFT20 
	
	*Fix labels:
	label var HDP_MHOCCUR "Any NEW dx of an HDP at <20 weeks GA"
	label var HDP_DX_GHYP "Any NEW dx of gestational hypertension at <20 weeks GA"
	label var HDP_DX_HTN "Any NEW dx of hypertension at <20 weeks GA"
	label var HDP_DX_PREEC "Any NEW dx of preeclampsia at <20 weeks GA"
	label var HDP_DX_DK "Any new dx of HDP (unspecified) at <20 weeks GA"
	
	label var HDP_TREAT "New treatment initiated for an HDP at <20 weeks GA"
	
	
	* CREATE A SUMMARY VARIABLE FOR HOW SOMEONE IS CATEGORIZED INTO CHTN: 
		* 1=MEASURED based on 2+ high BP readings: 
	gen HTN_ANY_IND = 1 if ///
		(HIGH_BP_COUNT_LESS20 >=2 & HIGH_BP_COUNT_LESS20!=. ) | /// at least 2 before 20 weeks 
		(HIGH_BP_COUNT_LESS20 >=1 & HIGH_BP_COUNT_LESS20!= . & ///
		  HIGH_BP_COUNT_AFT20  >= 1 & HIGH_BP_COUNT_AFT20  != . ) // 2+ with first <20 weeks 
 
		 * 2= MEASURED based on 1+ severe high BP reading: 
	replace HTN_ANY_IND = 2 if HTN_ANY_IND ==. & ///
		(HIGH_BP_SEV_COUNT_LESS20 >=1 & HIGH_BP_SEV_COUNT_LESS20 != .) // any severe high BP <20
		
		* 3 = NEWLY PRESCRIBED TREATMENT of HDP at <20 weeks GA 
	replace HTN_ANY_IND = 3 if HTN_ANY_IND == . & ///
		HDP_TREAT == 1 
		
		* 4 = NEWLY DIAGNOSED HDP at <20 weeks GA (without treatment)
	replace HTN_ANY_IND = 4 if HTN_ANY_IND == . & ///
		HDP_MHOCCUR == 1 
		
		* 5 = CURRENTLY TREATED for existing dx of chronic HTN at <20 weeks 
	replace HTN_ANY_IND = 5 if HTN_ANY_IND == . & ///
		HTN_HISTORY == 1 & HTN_HISTORY_IND == 1 		
		
		* 6 = Self-report of chronic htn only (not currently treated): 
	replace HTN_ANY_IND = 6 if HTN_ANY_IND == . & ///
		HTN_HISTORY == 1 & HTN_HISTORY_IND == 2 			
	
	tab HTN_ANY_IND HTN_ANY, m 
	
	tab HTN_ANY_IND SITE, m 
	
	label define htnind 1 "Measured (2+ high bp readings)" ///
		2 "Measured (1+ severe high bp readings)" 3 "New prescription for high bp" ///
		4 "New dx of hypertension at <20 weeks GA (no treatment)" ///
		5 "Currently treated for existing dx of HTN at <20wks" ///
		6 "Reported DX of existing HTN at <20 wks (no treatment)"
		
	label values HTN_ANY_IND htnind
	
	
	save "$wrk/HTN_final", replace 
	

	
/////////////////////////////////////////////////	
* * * * * * Review dxs given at ANC/Hospitalization 
* * * * * * (multiple entries per person)

	*Prep DX and Treatment info collected in MNH04: 
	clear 
	use "$wrk/HDP_mnh04"
	
	append using "$wrk/HDP_mnh19", gen(hospmerge)
	
	replace HDP_VISIT_DATE = VISIT_DATE if hospmerge == 1 
	
	merge m:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing(PREG_END ///
		PREG_END_DATE PREG_END_GA)
		
	drop if _merge == 2 
	drop _merge 
	
	
		* REVIEW IF THERE ARE ANY DXs WITH A MISSING DATE:
		gen HDP_CHECK = 0 
		replace HDP_CHECK = 1 if HDP_DX_GHYP==1 | HDP_DX_HTN==1 | ///
			HDP_DX_PREEC==1 | HDP_DX_DK == 1 | HDP_DX_ECL == 1 | HDP_TREAT ==1 | ///
			HDP_TREAT_HOSP == 1 
			
		replace HDP_CHECK = 2 if HDP_CHECK ==1 & HDP_VISIT_DATE == . 
		
		*** NOTE: as of 9-27, 6 observations with any diagnosis that are missing a date: 
		tab HDP_CHECK if HDP_CHECK==2
		list HOSP_TIMING  HDP_TYPE_VISIT HDP_DX_GHYP HDP_DX_HTN HDP_DX_PREEC ///
			HDP_DX_DK HDP_DX_ECL HDP_TREAT HDP_TREAT_HOSP PREG_END_GA if HDP_CHECK == 2 
		
		* all are hospitalization forms without a date, but reported during the 
		* ANC period. To further review, I'll create an indicator for "pregnancy"
		* lasted at least 20 weeks to help us differentiate between diagnoses that 
		* MUST have happened at <20 weeks: 
		
		gen HDP_CHECK_PREGLOSS = 1 if HDP_CHECK == 2 & PREG_END_GA < 140 & ///
			PREG_END_GA > 0 & PREG_END == 1 
		label var HDP_CHECK_PREGLOSS "Missing date of HDP dx AND pregnancy spanned <20 weeks GA"
		
		tab HDP_CHECK HDP_CHECK_PREGLOSS, m 
		
		*As of 9-27, no observations fall in this category. 
		
	
	**** drop hospitalizations that occurred AFTER pregnancy endpoint:
	
	// if visit date is AFTER pregnancy outcome date: 
	drop if HOSPITAL==1 & VISIT_DATE > PREG_END_DATE & ///
		PREG_END==1 & VISIT_DATE != . & PREG_END_DATE != . 
		
	// if hospitalization is marked as PNC & visit date is missing: 	
	drop if HOSPITAL == 1 & VISIT_DATE == . & HOSP_TIMING == 2 
	
	list if HOSPITAL==1 & VISIT_DATE == . 
	
	// drop variables we don't need for now: 
	drop BP_* UA_*
	
	**** add PREG_START_DATE for VISIT GA: 
	*** merge in for GA at visit:	
	merge m:1 MOMID PREGID using "$OUT/MAT_ENROLL", keepusing(PREG_START_DATE)
	
	drop if _merge == 2
	drop _merge 
	
	*update var format PREG_START_DATE
	rename PREG_START_DATE STR_PREG_START_DATE
	
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td 
	
	**** Calculate Visit GA (for estimate on dx date) 
	
	replace HDP_VISIT_GA = HDP_VISIT_DATE - PREG_START_DATE 
	label var HDP_VISIT_GA "GA at HDP visit/dx decision"
	
	tab HDP_VISIT_GA, m 
	
	replace HDP_VISIT_GA = . if HDP_VISIT_GA <0 | HDP_VISIT_GA > 400 
	
	
	**** RESTRICT to entries >=20 weeks GA: 
	
	keep if HDP_VISIT_GA >= (20*7) | ///
		(HOSP_TIMING == 1 & HDP_VISIT_GA == . & HDP_CHECK_PREGLOSS != 1)
	
	
///////////////////////////////////////////////////////////////////////////////

	/////////////////////////////////
	**** Make loops for each outcome: 
	
	* Below, the loop creates a set of three indicators for each dx 
	* identified in MNH04 (ANC) and MNH19 (hospitalization) in the following 
	* variables:
		*Chronic Hypertension (HTN)
			*In MNH04: HDP_DX_HTN 
			*In MNH19:  HDP_DX_HTN OR PRIMARY_DX_HDP_HTN
		*Gestational Hypertension (GHYP)
			*In MNH04: HDP_DX_GHYP 
			*In MNH19:  HDP_DX_GHYP OR PRIMARY_DX_HDP_GHYP
		*Preeclampsia (PREEC)
			*In MNH04: HDP_DX_PREEC 
			*In MNH19:  HDP_DX_PREEC OR PRIMARY_DX_HDP_PREEC
	
	*Within each category (HTN, GHYP, PREEC), the loop identifies the dx with 
	*the earliest date across all forms and constructs the following 
	*comprehensive variables: 
		* HDP_DX_`let' - final status from all MNH04 & MNH19 entries
		* HDP_DX_`let'_DATE - dx date if ever positive dx in MNH04 & MNH19 entries
		* HDP_DX_`let'_GA - dx ga if ever positive dx inM NH04 & MNH19 entries
		
	*On request, I also added a variable for dx in MNH04 vs. MNH19 on 9-27-2024:
		* FIRST_DX_`let'_FORM = 4 or 19 
		* EVER_DX_`let'_M04 = ever diangosed in MNH04 
		* EVER_DX_`let'_M19 = ever diagnosed in MNH19
	
	foreach let in HTN GHYP PREEC {
		
	preserve 
	
	keep MOMID PREGID HDP_VISIT_DATE HDP_VISIT_GA HDP_DX_`let' ///
		PRIMARY_DX_HDP_`let' hospmerge
		
	drop if HDP_DX_`let'== . & PRIMARY_DX_HDP_`let' ==. 

	
	/// order file by person & date to find earliest DX: 
	sort MOMID PREGID HDP_VISIT_GA 
	
	/// create indicator to number the of entries for each person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "DX Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of DX measures"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)	
	
	reshape wide HDP_VISIT_DATE HDP_DX_`let' HDP_VISIT_GA PRIMARY_DX_HDP_`let' ///
		hospmerge, ///
		i(MOMID PREGID ENTRY_TOTAL) j(ENTRY_NUM)
		
	gen HDP_DX_`let'_DATE = . 
	format HDP_DX_`let'_DATE %td
	
	gen HDP_DX_`let'_GA = . 
	
	gen HDP_DX_`let' = . 
	
	gen FIRST_DX_`let' = . 
	gen EVER_DX_`let'_M04 = . 
	gen EVER_DX_`let'_M19 = .
		
	foreach num of numlist 1/$i {
		
	*ID the first dx: 
		// if MNH04: 
		replace FIRST_DX_`let' = 4 if ///
		(HDP_DX_`let'`num' == 1 | PRIMARY_DX_HDP_`let'`num'==1) ///
		& (HDP_DX_`let'_DATE == . | ///
		HDP_VISIT_DATE`num' < HDP_DX_`let'_DATE) & ///
		hospmerge`num' == 0 
		
		// if MNH19: 
		replace FIRST_DX_`let' = 19 if ///
		(HDP_DX_`let'`num' == 1 | PRIMARY_DX_HDP_`let'`num'==1) ///
		& (HDP_DX_`let'_DATE == . | ///
		HDP_VISIT_DATE`num' < HDP_DX_`let'_DATE) & ///
		hospmerge`num' == 1 
		
	*replace overall visit date with the entry visit date if the dx is 
	*recorded for the first time (date==.) OR if the dx is recorded for a 
	*date that is prior to other dates recorded	
	replace HDP_DX_`let'_DATE = HDP_VISIT_DATE`num' if ///
		(HDP_DX_`let'`num' == 1 | PRIMARY_DX_HDP_`let'`num'==1) ///
		& (HDP_DX_`let'_DATE == . | ///
		   HDP_VISIT_DATE`num' < HDP_DX_`let'_DATE)
		   
	*replace overall dx GA with the entry dx GA if the dx is 
	*recorded for the first time (GA==.) OR if the dx is recorded for an 
	*earlier GA recorded 
	replace HDP_DX_`let'_GA = HDP_VISIT_GA`num' if ///
		(HDP_DX_`let'`num' == 1 | PRIMARY_DX_HDP_`let'`num'==1) ///
		& (HDP_DX_`let'_GA == . | ///
		   HDP_VISIT_GA`num' < HDP_DX_`let'_GA)
	
	*record the value of the entry dx is no dx information recorded yet 
	replace HDP_DX_`let' = HDP_DX_`let'`num' if HDP_DX_`let' == . 
	
	*record 55 if no other dx information has been recorded 
	replace HDP_DX_`let' = 55 if HDP_DX_`let'`num' > 2 & ///
		   HDP_DX_`let' != 1 & HDP_DX_`let' != 0 
		   
	*record 1 if entry number for DX is ever =1
	replace HDP_DX_`let' = 1 if HDP_DX_`let'`num' == 1 | ///
		PRIMARY_DX_HDP_`let'`num'==1
		
	*record the form if positive: 
	replace EVER_DX_`let'_M04 = 1 if hospmerge`num' == 0 & ///
		(HDP_DX_`let'`num' == 1)
	
	replace EVER_DX_`let'_M19 = 1 if hospmerge`num' == 1 & ///
		(HDP_DX_`let'`num' == 1 | PRIMARY_DX_HDP_`let'`num'==1)
	
	}
	
	* CHECKS: 
	foreach num of numlist 1/$i {
		
	tab HDP_DX_`let' HDP_DX_`let'`num', m 
	tab HDP_DX_`let' PRIMARY_DX_HDP_`let'`num', m 
	
	}
	
	tab HDP_DX_`let' FIRST_DX_`let', m 
	tab EVER_DX_`let'_M04 EVER_DX_`let'_M19 if HDP_DX_`let' == 1, m 
	
	tab HDP_DX_`let'_GA, m 
	
	sum HDP_DX_`let'_GA
	
	keep MOMID PREGID HDP_DX_`let' HDP_DX_`let'_DATE HDP_DX_`let'_GA ///
		FIRST_DX_`let' EVER_DX_`let'_M04 EVER_DX_`let'_M19
		
	save "$wrk/`let'_dx_ANC", replace 
	
	restore 
	
	}
	
	/////////////////////////////////
	**** Make loops for severe features: 
	
		*Three possible variables: 
		*HELLP
		*SEVHYP
		*VIS

	*renaming for consistency: 
	rename SEVERE_VISUAL SEVERE_VIS 
	
	* Below, the loop creates a set of three indicators for each severe feature 
	* identified in MNH19 (hospitalization) in the following 
	* variables:
		*HELLP syndrome 
			*In MNH19: SEVERE_HELLP PRIMARY_DX_HDP_HELLP
		*Severe gestational hypertension 
			*In MNH19: SEVERE_SEVHYP PRIMARY_DX_HDP_SEVHYP
		*Visual symptoms 
			*In MNH19: SEVERE_VIS PRIMARY_DX_HDP_VIS
	
	*Within each category (HELLP, SEVHYP, VIS), the loop identifies the dx with 
	*the earliest date across all forms and constructs the following 
	*comprehensive variables: 
		* HDP_`let' - final status from all MNH19 entries
		* HDP_`let'_DATE - dx date if ever positive dx in all MNH19 entries
		* HDP_`let'_GA - dx ga if ever positive dx in all MNH19 entries
	
	
	foreach let in HELLP SEVHYP VIS {
		
	preserve 
	
	keep MOMID PREGID HDP_VISIT_DATE HDP_VISIT_GA ///
		SEVERE_`let' PRIMARY_DX_HDP_`let'
		
	drop if SEVERE_`let' ==. & PRIMARY_DX_HDP_`let'==.

	
	/// order file by person & date to find earliest DX: 
	sort MOMID PREGID HDP_VISIT_GA 
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "DX Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of DX measures"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)	
	
	reshape wide HDP_VISIT_DATE HDP_VISIT_GA ///
		SEVERE_`let' PRIMARY_DX_HDP_`let', ///
		i(MOMID PREGID ENTRY_TOTAL) j(ENTRY_NUM)
		
	gen HDP_`let'_DATE = . 
	format HDP_`let'_DATE %td
	
	gen HDP_`let'_GA = . 
	
	gen HDP_`let' = . 
		
	foreach num of numlist 1/$i {
		
	replace HDP_`let'_DATE = HDP_VISIT_DATE`num' if ///
		(SEVERE_`let'`num' == 1 | PRIMARY_DX_HDP_`let'`num'==1) ///
		& (HDP_`let'_DATE == . | ///
		   HDP_VISIT_DATE`num' < HDP_`let'_DATE)
		   
	replace HDP_`let'_GA = HDP_VISIT_GA`num' if ///
		(SEVERE_`let'`num' == 1 | PRIMARY_DX_HDP_`let'`num'==1) ///
		& (HDP_`let'_GA == . | ///
		   HDP_VISIT_GA`num' < HDP_`let'_GA)
		   
	replace HDP_`let' = SEVERE_`let'`num' if HDP_`let' == . 
	
	replace HDP_`let' = 55 if SEVERE_`let'`num' > 2 & ///
		   HDP_`let' != 1 & HDP_`let' != 0 
		   
	replace HDP_`let' = 1 if (SEVERE_`let'`num' == 1 | ///
		PRIMARY_DX_HDP_`let'`num'==1)
	
	}
	
	* CHECKS: 
	foreach num of numlist 1/$i {
		
	tab HDP_`let' SEVERE_`let'`num', m 
	tab HDP_`let' PRIMARY_DX_HDP_`let'`num', m 
	
	}
	
	
	tab HDP_`let'_GA, m 
	
	sum HDP_`let'_GA
	
	keep MOMID PREGID HDP_`let' HDP_`let'_DATE HDP_`let'_GA 
		
	save "$wrk/`let'_dx_ANC", replace 
	
	restore 
	
	}
	
	/////////////////////////////////
	**** Make loops for severe features: 
	
		*Two possible variables: 
		*SEIZURES
		*PE -- pulmonary edema 
		
	* Below, the loop creates a set of three indicators for each severe feature 
	* identified in MNH19 (hospitalization) in the following 
	* variables:
		*Seizures 
			*In MNH19: SEVERE_SEIZURES PRIMARY_DX_HDP_SEIZURES SYMPTOM_SEIZURES_HOSP
		*Pulmonary Edema (PE) 
			*In MNH19: SEVERE_PE PRIMARY_DX_HDP_PE SYMPTOM_PE_HOSP
	
	*Within each category (SEIZURES, PE), the loop identifies the dx with 
	*the earliest date across all forms and constructs the following 
	*comprehensive variables: 
		* HDP_`let' - final status from all MNH19 entries
		* HDP_`let'_DATE - dx date if ever positive dx in all MNH19 entries
		* HDP_`let'_GA - dx ga if ever positive dx in all MNH19 entries

	*renaming for consistency: 
	rename SYMPTOM_SEIZURE_HOSP SYMPTOM_SEIZURES_HOSP
	
	rename PRIMARY_DX_HDP_PULMED PRIMARY_DX_HDP_PE
	rename HOSP_PULMED SYMPTOM_PE_HOSP
	
	foreach let in SEIZURES PE {
		
	preserve 
	
	keep MOMID PREGID HDP_VISIT_DATE HDP_VISIT_GA SYMPTOM_`let'_HOSP ///
		SEVERE_`let' PRIMARY_DX_HDP_`let'
		
	drop if SYMPTOM_`let'_HOSP== . & SEVERE_`let' ==. & PRIMARY_DX_HDP_`let'==.

	
	/// order file by person & date to find earliest DX: 
	sort MOMID PREGID HDP_VISIT_GA 
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "DX Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of DX measures"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)	
	
	reshape wide HDP_VISIT_DATE HDP_VISIT_GA SYMPTOM_`let'_HOSP ///
		SEVERE_`let' PRIMARY_DX_HDP_`let', ///
		i(MOMID PREGID ENTRY_TOTAL) j(ENTRY_NUM)
		
	gen HDP_`let'_DATE = . 
	format HDP_`let'_DATE %td
	
	gen HDP_`let'_GA = . 
	
	gen HDP_`let' = . 
		
	foreach num of numlist 1/$i {
		
	replace HDP_`let'_DATE = HDP_VISIT_DATE`num' if ///
		(SEVERE_`let'`num' == 1 | PRIMARY_DX_HDP_`let'`num'==1 | ///
		 SYMPTOM_`let'_HOSP`num'==1) ///
		& (HDP_`let'_DATE == . | ///
		   HDP_VISIT_DATE`num' < HDP_`let'_DATE)
		   
	replace HDP_`let'_GA = HDP_VISIT_GA`num' if ///
		(SEVERE_`let'`num' == 1 | PRIMARY_DX_HDP_`let'`num'==1 | ///
		 SYMPTOM_`let'_HOSP`num'==1) ///
		& (HDP_`let'_GA == . | ///
		   HDP_VISIT_GA`num' < HDP_`let'_GA)
		   
	replace HDP_`let' = SEVERE_`let'`num' if HDP_`let' == . 
	
	replace HDP_`let' = 55 if SEVERE_`let'`num' > 2 & ///
		   HDP_`let' != 1 & HDP_`let' != 0 
		   
	replace HDP_`let' = 1 if (SEVERE_`let'`num' == 1 | ///
		PRIMARY_DX_HDP_`let'`num'==1 | SYMPTOM_`let'_HOSP`num'==1)
	
	}
	
	* CHECKS: 
	foreach num of numlist 1/$i {
		
	tab HDP_`let' SEVERE_`let'`num', m 
	tab HDP_`let' PRIMARY_DX_HDP_`let'`num', m 
	tab HDP_`let' SYMPTOM_`let'_HOSP`num', m 
	
	}
	
	
	tab HDP_`let'_GA, m 
	
	sum HDP_`let'_GA
	
	keep MOMID PREGID HDP_`let' HDP_`let'_DATE HDP_`let'_GA 
		
	save "$wrk/`let'_dx_ANC", replace 
	
	restore 
	
	}
	
	/////////////////////////////////
	**** Make loops for treatment: 
		
	* Below, the chunk of code creates a set of indicators for ever treated for 
	* HDP with a medication of interest, based on the following variables:
		*HDP_TREAT 
			*Drawn from MNH04
		*HDP_TREAT_HOSP
			*Drawn from MNH19 
	
	*The loops ID the earliest date across all forms and constructs the following 
	*comprehensive variables: 
		* HDP_TREAT - ever treated for HDPs at/after 20 weeks GA 
		* HDP_TREAT_DATE - date first treated for HDP
		* HDP_TREAT_GA - GA first treated for HDP 
		* FIRST_HDP_TREAT - form first treated for HDP 
		* EVER_HDP_TREAT_M04 - ever treated for HDP in MNH04 
		* EVER_HDP_TREAT_M19 - ever treated for HDP in MNH19 
		
	preserve 
	
	keep MOMID PREGID HDP_VISIT_DATE HDP_VISIT_GA HDP_TREAT HDP_TREAT_HOSP hospmerge
		
	drop if HDP_TREAT== . & HDP_TREAT_HOSP ==. 

	
	/// order file by person & date to find earliest DX: 
	sort MOMID PREGID HDP_VISIT_GA 
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "DX Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of DX measures"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)	
	
	reshape wide HDP_VISIT_DATE HDP_TREAT HDP_VISIT_GA HDP_TREAT_HOSP hospmerge, ///
		i(MOMID PREGID ENTRY_TOTAL) j(ENTRY_NUM)
		
	gen HDP_TREAT_DATE = . 
	format HDP_TREAT_DATE %td
	
	gen HDP_TREAT_GA = . 
	
	gen HDP_TREAT = . 
		
	gen FIRST_HDP_TREAT = .
	
	gen EVER_HDP_TREAT_M04 = . 
	gen EVER_HDP_TREAT_M19 = . 
		
	foreach num of numlist 1/$i {
		
		// if form 4: 
	replace FIRST_HDP_TREAT = 4 if ///
		(HDP_TREAT`num' == 1 | HDP_TREAT_HOSP`num'==1) ///
		& (HDP_TREAT_DATE == . | ///
		   HDP_VISIT_DATE`num' < HDP_TREAT_DATE) & hospmerge`num' == 0 
		// if form 19: 
	replace FIRST_HDP_TREAT = 19 if ///
		(HDP_TREAT`num' == 1 | HDP_TREAT_HOSP`num'==1) ///
		& (HDP_TREAT_DATE == . | ///
		   HDP_VISIT_DATE`num' < HDP_TREAT_DATE) & hospmerge`num' == 1 	
		
	replace HDP_TREAT_DATE = HDP_VISIT_DATE`num' if ///
		(HDP_TREAT`num' == 1 | HDP_TREAT_HOSP`num'==1) ///
		& (HDP_TREAT_DATE == . | ///
		   HDP_VISIT_DATE`num' < HDP_TREAT_DATE)
		   
	replace HDP_TREAT_GA = HDP_VISIT_GA`num' if ///
		(HDP_TREAT`num' == 1 | HDP_TREAT_HOSP`num'==1) ///
		& (HDP_TREAT_GA == . | ///
		   HDP_VISIT_GA`num' < HDP_TREAT_GA)
		   
	replace HDP_TREAT = HDP_TREAT`num' if HDP_TREAT == . 
	
	replace HDP_TREAT = 55 if HDP_TREAT`num' > 2 & ///
		  HDP_TREAT != 1 & HDP_TREAT != 0 
		   
	replace HDP_TREAT = 1 if HDP_TREAT`num' == 1 | ///
		HDP_TREAT_HOSP`num'==1
	
	replace EVER_HDP_TREAT_M04 = 1 if (HDP_TREAT`num' == 1 | ///
		HDP_TREAT_HOSP`num'==1) & hospmerge`num' == 0 
		
	replace EVER_HDP_TREAT_M19 = 1 if (HDP_TREAT`num' == 1 | ///
		HDP_TREAT_HOSP`num'==1) & hospmerge`num' == 1 
	
	}
	
	* CHECKS: 
	foreach num of numlist 1/$i {
		
	tab HDP_TREAT HDP_TREAT`num', m 
	tab HDP_TREAT HDP_TREAT_HOSP`num', m 
	
	}
	
	
	tab HDP_TREAT_GA, m 
	
	sum HDP_TREAT_GA
	
	keep MOMID PREGID HDP_TREAT HDP_TREAT_DATE HDP_TREAT_GA FIRST_HDP_TREAT ///
		EVER_HDP_TREAT_M04 EVER_HDP_TREAT_M19
		
	tab EVER_HDP_TREAT_M04 EVER_HDP_TREAT_M19, m 
	tab HDP_TREAT FIRST_HDP_TREAT, m 
		
	save "$wrk/HDP_TREAT_dx_ANC", replace 
	
	restore 
	
	
	/////////////////////////////////
	**** Make loops for other HDP dxs: 
	
	* Below, the loop creates a set of indicators for "other" HDP dxs 
	* including: 
		*HDP_MHOCCUR - Any HDP 
			*Drawn from MNH04
		*HDP_DX_DK - Any HDP, but DK which one 
			*Drawn from MNH04 OR MNH19 
		*HDP_DX_ECL - Eclampsia (dx at ANC)
			*Drawn from MNH04
	
	*The loops ID the earliest date across all MNH04/19 forms and constructs the 
	*following comprehensive variables: 
		* HDP_`let' - diagnosis 
		* HDP_`let'_DATE - first date of dx 
		* HDP_`let'_GA - GA at first dx 
		* FIRST_DX_`let' - form that dx was first recorded 
		* EVER_DX_`let'_M04 - ever dx with this category in MNH04 
		* EVER_DX_`let'_M19 - ever dx with this caetgory in MNH19 
		
	
	foreach let in MHOCCUR DX_DK DX_ECL {
		
	preserve 
	
	keep MOMID PREGID HDP_VISIT_DATE HDP_VISIT_GA HDP_`let' hospmerge 
		
	drop if HDP_`let' ==. 

	
	/// order file by person & date to find earliest DX: 
	sort MOMID PREGID HDP_VISIT_GA 
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "DX Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of DX measures"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)	
	
	reshape wide HDP_VISIT_DATE HDP_VISIT_GA HDP_`let' hospmerge, ///
		i(MOMID PREGID ENTRY_TOTAL) j(ENTRY_NUM)
		
	gen HDP_`let'_DATE = . 
	format HDP_`let'_DATE %td
	
	gen HDP_`let'_GA = . 
	
	gen HDP_`let' = . 
	
	gen FIRST_DX_`let' = . 
	gen EVER_DX_`let'_M04 = . 
	gen EVER_DX_`let'_M19 = .
		
	foreach num of numlist 1/$i {
		
		// if form mnh04 
	replace FIRST_DX_`let' = 4 if hospmerge`num' == 0 & ///
		(HDP_`let'`num' == 1) ///
		& (HDP_`let'_DATE == . | ///
		   HDP_VISIT_DATE`num' < HDP_`let'_DATE)
		// if form mnh19 
	replace FIRST_DX_`let' = 19 if hospmerge`num' == 1 & ///
		(HDP_`let'`num' == 1) ///
		& (HDP_`let'_DATE == . | ///
		   HDP_VISIT_DATE`num' < HDP_`let'_DATE)
		
	replace HDP_`let'_DATE = HDP_VISIT_DATE`num' if ///
		(HDP_`let'`num' == 1) ///
		& (HDP_`let'_DATE == . | ///
		   HDP_VISIT_DATE`num' < HDP_`let'_DATE)
		   
	replace HDP_`let'_GA = HDP_VISIT_GA`num' if ///
		(HDP_`let'`num' == 1) ///
		& (HDP_`let'_GA == . | ///
		   HDP_VISIT_GA`num' < HDP_`let'_GA)
		   
	replace HDP_`let' = HDP_`let'`num' if HDP_`let' == . 
	
	replace HDP_`let' = 55 if HDP_`let'`num' > 2 & ///
		  HDP_`let' != 1 & HDP_`let' != 0 
		   
	replace HDP_`let' = 1 if HDP_`let'`num' == 1 
	
	replace EVER_DX_`let'_M04 = 1 if HDP_`let'`num' == 1 & hospmerge`num' == 0 
	replace EVER_DX_`let'_M19 = 1 if HDP_`let'`num' == 1 & hospmerge`num' == 1 
	
	}
	
	* CHECKS: 
	foreach num of numlist 1/$i {
		
	tab HDP_`let' HDP_`let'`num', m 
	
	}
	
	
	tab HDP_`let'_GA, m 
	
	sum HDP_`let'_GA
	
	keep MOMID PREGID HDP_`let' HDP_`let'_DATE HDP_`let'_GA FIRST_DX_`let' ///
		EVER_DX_`let'_M04 EVER_DX_`let'_M19 
		
	tab HDP_`let' FIRST_DX_`let', m 
		
	save "$wrk/HDP_`let'_dx_ANC", replace 
	
	restore 
	
	}
	

	/////////////////////////////////
	**** Make loops for other HDP primary dx options: 
	
	* Below, the loop creates a set of indicators for additional primary 
	* diagnosis options related to HDPs from the hospitalization form, 
	* including: 
		*Postpartum eclampsia 
			*MNH19: PRIMARY_DX_HDP_PPECL 
		*Stroke 
			*MNH19: PRIMARY_DX_HDP_STROKE 
	
	*The loops ID the earliest date across all MNH19 forms and constructs the 
	*following comprehensive variables: 
		* HDP_`let' - diagnosis 
		* HDP_`let'_DATE - first date of dx 
		* HDP_`let'_GA - GA at first dx 
	
	foreach let in PPECL STROKE {
		
	preserve 
	
	keep MOMID PREGID HDP_VISIT_DATE HDP_VISIT_GA PRIMARY_DX_HDP_`let'
		
	drop if PRIMARY_DX_HDP_`let' ==. 

	
	/// order file by person & date to find earliest DX: 
	sort MOMID PREGID HDP_VISIT_GA 
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "DX Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of DX measures"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)	
	
	reshape wide HDP_VISIT_DATE HDP_VISIT_GA PRIMARY_DX_HDP_`let', ///
		i(MOMID PREGID ENTRY_TOTAL) j(ENTRY_NUM)
		
	gen HDP_`let'_DATE = . 
	format HDP_`let'_DATE %td
	
	gen HDP_`let'_GA = . 
	
	gen HDP_`let' = . 
		
	foreach num of numlist 1/$i {
		
	replace HDP_`let'_DATE = HDP_VISIT_DATE`num' if ///
		(PRIMARY_DX_HDP_`let'`num' == 1) ///
		& (HDP_`let'_DATE == . | ///
		   HDP_VISIT_DATE`num' < HDP_`let'_DATE)
		   
	replace HDP_`let'_GA = HDP_VISIT_GA`num' if ///
		(PRIMARY_DX_HDP_`let'`num' == 1) ///
		& (HDP_`let'_GA == . | ///
		   HDP_VISIT_GA`num' < HDP_`let'_GA)
		   
	replace HDP_`let' = PRIMARY_DX_HDP_`let'`num' if HDP_`let' == . 
	
	replace HDP_`let' = 55 if PRIMARY_DX_HDP_`let'`num' > 2 & ///
		  HDP_`let' != 1 & HDP_`let' != 0 
		   
	replace HDP_`let' = 1 if PRIMARY_DX_HDP_`let'`num' == 1 
	
	}
	
	* CHECKS: 
	foreach num of numlist 1/$i {
		
	tab HDP_`let' PRIMARY_DX_HDP_`let'`num', m 
	
	}
	
	
	tab HDP_`let'_GA, m 
	
	sum HDP_`let'_GA
	
	keep MOMID PREGID HDP_`let' HDP_`let'_DATE HDP_`let'_GA 
		
	save "$wrk/HDP_`let'_dx_ANC", replace 
	
	restore 
	
	}	
	
	
	/////////////////////////////////
	**** Make loops for other severe symptoms: 
	
	* Below, the loop creates a set of indicators for additional severe 
	* symptoms related to preeclampsia from the hospitalization form, 
	* including: 
		*Epigastric pain 
			*MNH19: SYMPTOM_EPIPAIN_HOSP 
		*Severe headache 
			*MNH19: SYMPTOM_HEADACHE_HOSP 
	
	*The loops ID the earliest date across all MNH19 forms and constructs the 
	*following comprehensive variables: 
		* SYMPTOM_`let' - diagnosis 
		* SYMPTOM_`let'_DATE - first date of dx 
		* SYMPTOM_`let'_GA - GA at first dx 

	
	foreach let in EPIPAIN HEADACHE {
		
	preserve 
	
	keep MOMID PREGID HDP_VISIT_DATE HDP_VISIT_GA SYMPTOM_`let'_HOSP
		
	drop if SYMPTOM_`let'_HOSP ==. 

	
	/// order file by person & date to find earliest DX: 
	sort MOMID PREGID HDP_VISIT_GA 
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "DX Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of DX measures"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)	
	
	reshape wide HDP_VISIT_DATE HDP_VISIT_GA SYMPTOM_`let'_HOSP, ///
		i(MOMID PREGID ENTRY_TOTAL) j(ENTRY_NUM)
		
	gen SYMPTOM_`let'_DATE = . 
	format SYMPTOM_`let'_DATE %td
	
	gen SYMPTOM_`let'_GA = . 
	
	gen SYMPTOM_`let' = . 
		
	foreach num of numlist 1/$i {
		
	replace SYMPTOM_`let'_DATE = HDP_VISIT_DATE`num' if ///
		(SYMPTOM_`let'_HOSP`num' == 1) ///
		& (SYMPTOM_`let'_DATE == . | ///
		   HDP_VISIT_DATE`num' < SYMPTOM_`let'_DATE)
		   
	replace SYMPTOM_`let'_GA = HDP_VISIT_GA`num' if ///
		(SYMPTOM_`let'_HOSP`num' == 1) ///
		& (SYMPTOM_`let'_GA == . | ///
		   HDP_VISIT_GA`num' < SYMPTOM_`let'_GA)
		   
	replace SYMPTOM_`let' = SYMPTOM_`let'_HOSP`num' if SYMPTOM_`let' == . 
	
	replace SYMPTOM_`let' = 55 if SYMPTOM_`let'_HOSP`num' > 2 & ///
		  SYMPTOM_`let' != 1 & SYMPTOM_`let' != 0 
		   
	replace SYMPTOM_`let' = 1 if SYMPTOM_`let'_HOSP`num' == 1 
	
	}
	
	* CHECKS: 
	foreach num of numlist 1/$i {
		
	tab SYMPTOM_`let' SYMPTOM_`let'_HOSP`num', m 
	
	}
	
	
	tab SYMPTOM_`let'_GA, m 
	
	sum SYMPTOM_`let'_GA
	
	keep MOMID PREGID SYMPTOM_`let' SYMPTOM_`let'_DATE SYMPTOM_`let'_GA 
		
	save "$wrk/SYMPTOM_`let'_dx_HOSP", replace 
	
	restore 
	
	}	
	
	
	clear 
/////////////////////////////////////////////////	
* * * * * * Review proteinuria measures

	use "$wrk/UA_mnh08"
	
	gen UA_TIMING = 1 
	label var UA_TIMING "Timing of urinalysis"
	label define uat 1 "1-MNH08" 2 "2-Hospitalization" 3 "3-L&D"
	label values UA_TIMING uat
	
	append using "$wrk/UA_mnh19", gen(HOSP)
	
	tab UA_PROT_LBORRES if HOSP==1, m 
	
	replace UA_TIMING = 2 if HOSP == 1 
	
	* FLAG: MNH19 form variable does not include trace, so we need to adjust the 
	* variables as follows:
	
		// remove values >3 (not in MNH19 CRF)
	replace UA_PROT_LBORRES = 77 if UA_PROT_LBORRES >3 & HOSP==1
	
	replace UA_PROT_LBORRES = UA_PROT_LBORRES + 1 if ///
		UA_PROT_LBORRES >=1 & UA_PROT_LBORRES <=3 & HOSP == 1 
	
	replace VISIT_DATE = UA_DATE if HOSP == 1 & VISIT_DATE == . 
	
	append using "$wrk/HDP_mnh09", gen(LD)
	
	replace UA_TIMING = 3 if LD == 1 
	
	* FLAG: MNH09 form variable does not include trace, so we need to adjust the 
	* variables as follows:
	
		// remove values >3 (not in MNH09 CRF)
	replace UA_PROT_LBORRES = 77 if UA_PROT_LBORRES >3 & LD==1
	
	replace UA_PROT_LBORRES = UA_PROT_LBORRES + 1 if ///
		UA_PROT_LBORRES >=1 & UA_PROT_LBORRES <=3 & LD == 1 
	
	tab UA_TIMING, m 
	
	order UA_TIMING, after(PREGID)
	
	tab UA_PROT_LBORRES UA_TIMING, m 
	
	tab UA_PROT_LBORRES if UA_TIMING == 1 
	tab UA_PROT_LBORRES if UA_TIMING == 2 
	tab UA_PROT_LBORRES if UA_TIMING == 3 
	
	
	////////////////////////////////////////////////////
	/// order file by person & date to find max proteinuria measure 
	
	keep MOMID PREGID UA_TIMING VISIT_DATE UA_* HOSP LD 
	
	*** merge in for GA at visit:	
	merge m:1 MOMID PREGID using "$OUT/MAT_ENROLL", keepusing(PREG_START_DATE)
	
	drop if _merge == 2
	drop _merge 
	
	*update var format PREG_START_DATE
	rename PREG_START_DATE STR_PREG_START_DATE
	
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td 
	
	gen VISIT_GA = VISIT_DATE - PREG_START_DATE
	
	replace VISIT_GA = . if VISIT_GA <0 | VISIT_GA >400
	
	
	* Separate out any protein measures at <20 weeks GA: 
	
	gen UA_LESS20 = 0 
	
	replace UA_LESS20 = 1 if VISIT_GA < (20*7) & VISIT_GA != . 
	
	
	preserve 
	
		keep if UA_LESS20 == 1 
		
		save "$wrk/UA_before20w", replace 
		
	restore 
	
	keep if UA_LESS20 == 0 
	
	sort MOMID PREGID VISIT_DATE
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "DX Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of DX measures"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)	
	
	reshape wide UA_* VISIT_DATE VISIT_GA HOSP LD, ///
		i(MOMID PREGID ENTRY_TOTAL) j(ENTRY_NUM)	
		
		
	gen UA_PROT_LBORRES = -99
	label var UA_PROT_LBORRES "Highest protein reading"
	
	gen UA_PROT_TESTTYPE = .
	label var UA_PROT_TESTTYPE "Form reported highest protein reading"
	gen UA_PROT_DATE = . 
		format UA_PROT_DATE %td 
		label var UA_PROT_DATE "Date of highest protein reading"
	gen UA_PROT_GA = . 
	label var UA_PROT_GA "GA at highest protein reading"	
	
	foreach num of numlist 1/$i {
		
	replace UA_PROT_LBORRES`num' = -55 if UA_PROT_LBORRES`num' == 55 | ///
		UA_PROT_LBORRES`num' == . | UA_PROT_LBORRES`num' == 77
		
	replace UA_PROT_TESTTYPE = UA_TIMING`num' if ///
		UA_PROT_LBORRES`num'> UA_PROT_LBORRES		
		
	replace UA_PROT_DATE = VISIT_DATE`num' if ///
		UA_PROT_LBORRES`num'> UA_PROT_LBORRES	
		
	replace UA_PROT_GA = VISIT_GA`num' if ///
		UA_PROT_LBORRES`num'> UA_PROT_LBORRES	
		
	replace UA_PROT_LBORRES = UA_PROT_LBORRES`num' if ///
		UA_PROT_LBORRES`num'> UA_PROT_LBORRES
		
	
		
	}
	
	label values UA_PROT_LBORRES prot
	tab UA_PROT_LBORRES, m 
	
	label values UA_PROT_TESTTYPE uat
	
	tab UA_PROT_LBORRES UA_PROT_TESTTYPE , m 
	
	keep MOMID PREGID ENTRY_TOTAL UA_PROT_LBORRES UA_PROT_DATE UA_PROT_GA ///
		UA_PROT_TESTTYPE
		
	save "$wrk/UA_tabulated", replace 

	clear 
	
	///// Calculate for <20 weeks: 
	use "$wrk/UA_before20w"
	
	sort MOMID PREGID VISIT_DATE
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "DX Entry Number"
	
	tab ENTRY_NUM, m 
	
	*create an indicator of total entries per person: 
	duplicates tag MOMID PREGID, gen(ENTRY_TOTAL)
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	tab ENTRY_TOTAL, m 
	label var ENTRY_TOTAL "Total number of DX measures"
	
	sum ENTRY_TOTAL
	return list 
	
	global i = r(max)	
	
	drop HOSP LD
	
	reshape wide UA_* VISIT_DATE VISIT_GA, ///
		i(MOMID PREGID ENTRY_TOTAL) j(ENTRY_NUM)	
		
		
	gen UA_PROT_LBORRES = -99
	label var UA_PROT_LBORRES "Highest protein reading"
	
	gen UA_PROT_TESTTYPE = .
	label var UA_PROT_TESTTYPE "Form reported highest protein reading"
	gen UA_PROT_DATE = . 
		format UA_PROT_DATE %td 
		label var UA_PROT_DATE "Date of highest protein reading"
	gen UA_PROT_GA = . 
	label var UA_PROT_GA "GA at highest protein reading"
	
	
	foreach num of numlist 1/$i {
		
	replace UA_PROT_LBORRES`num' = -55 if UA_PROT_LBORRES`num' == 55 | ///
		UA_PROT_LBORRES`num' == . | UA_PROT_LBORRES`num' == 77
		
	replace UA_PROT_TESTTYPE = UA_TIMING`num' if ///
		UA_PROT_LBORRES`num'> UA_PROT_LBORRES		
		
	replace UA_PROT_DATE = VISIT_DATE`num' if ///
		UA_PROT_LBORRES`num'> UA_PROT_LBORRES	
		
	replace UA_PROT_GA = VISIT_GA`num' if ///
		UA_PROT_LBORRES`num'> UA_PROT_LBORRES	
		
	replace UA_PROT_LBORRES = UA_PROT_LBORRES`num' if ///
		UA_PROT_LBORRES`num'> UA_PROT_LBORRES
		
	}
	
	label values UA_PROT_LBORRES prot
	tab UA_PROT_LBORRES, m 
	
	label values UA_PROT_TESTTYPE uat
	
	
	keep MOMID PREGID ENTRY_TOTAL UA_PROT_LBORRES UA_PROT_DATE UA_PROT_GA ///
		UA_PROT_TESTTYPE
		
	rename UA_* UA_*_BL
	rename ENTRY_TOTAL UA_PROT_PRIOR20_COUNT
	rename UA_PROT_LBORRES UA_PROT_PRIOR20_LBORRES
	rename UA_PROT_DATE UA_PROT_PRIOR20_DATE
	rename UA_PROT_GA UA_PROT_PRIOR20_GA
	rename UA_PROT_TESTTYPE UA_PROT_PRIOR20_TESTTYPE
	
	*tab UA_PROT_LBORRES UA_PROT_TESTTYPE, m 
		
	save "$wrk/UA_tabulated_less20", replace
	

///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
/////// * * Part III: Compile & Reduce * * ///////
///////////////////////////////////////////////////////		
	clear 
	
	
	*Start with enrolled indicator: 
	use "$OUT/MAT_ENROLL"
	
	*update var format PREG_START_DATE
	rename PREG_START_DATE STR_PREG_START_DATE
	
	gen PREG_START_DATE = date(STR_PREG_START_DATE, "YMD")
	format PREG_START_DATE %td 
	
	keep MOMID PREGID ENROLL PREG_START_DATE SITE
	
	* merge in chronic hypertension file: 
	merge 1:1 MOMID PREGID using "$wrk/HTN_final", keepusing(HTN_ANY HIGH_BP_SEV_COUNT_LESS20 HTN_ANY_IND)
	
		drop if _merge == 2 
		drop _merge 
	
	* merge in BP measures at >=20 weeks: 
	merge 1:1 MOMID PREGID using "$wrk/BP_tabulation"
	
		drop if _merge == 2 
		drop _merge 
		
	* merge in UA measures (>=20 weeks): 
	merge 1:1 MOMID PREGID using "$wrk/UA_tabulated"
	
		drop if _merge == 2 
		drop _merge 
		
	*merge in BL UA measures (<20 weeks):
	merge 1:1 MOMID PREGID using "$wrk/UA_tabulated_less20"
	
		drop if _merge == 2 
		drop _merge 
		
	* merge in DXes at ANC/Hospitalization: 

	
	foreach let in HTN GHYP PREEC HDP_TREAT SEIZURES PE HELLP SEVHYP ///
		VIS HDP_MHOCCUR HDP_DX_DK HDP_DX_ECL HDP_PPECL HDP_STROKE {
	
	merge 1:1 MOMID PREGID using "$wrk/`let'_dx_ANC"
	
		drop if _merge == 2 
		drop _merge 
		
		}
		
	merge 1:1 MOMID PREGID using "$wrk/SYMPTOM_EPIPAIN_dx_HOSP"
		
		drop if _merge == 2 
		drop _merge 
		
	merge 1:1 MOMID PREGID using "$wrk/SYMPTOM_HEADACHE_dx_HOSP"
		
		drop if _merge == 2 
		drop _merge 

	rename HDP_* HDP_*_ANC
	
	rename SYMPTOM_* SYMPTOM_*_HOSP
	
	/////////////////////////
	****** Merge in L&D form: 
	merge 1:1 MOMID PREGID using "$wrk/HDP_mnh09", keepusing(MAT_* ///
		HDP_* SEVERE_* SYMPTOM* INDUCED_HDP CES_PREEC VISIT_DATE)
		
		drop if _merge == 2 
		drop _merge 
		
	****** Merge in final pregnancy end info: 
	merge 1:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing(PREG_END ///
		PREG_END_DATE PREG_END_GA PREG_LOSS PREG_LOSS_INDUCED ///
		PREG_LOSS_DEATH)
		
		drop if _merge == 2 
		drop _merge 
		
	///////////////////////////////////////	
	**** restrict to completed pregnancies:
	
		keep if PREG_END == 1  
		
	* review variables:
	
		*fix var name: 
		rename FIRST_DX_DX_DK FIRST_DX_DK
	
	foreach let in HTN GHYP PREEC DK {
		
	tab HDP_DX_`let'_ANC HDP_DX_`let', m 
	
	* set to L&D dx first: 
	gen `let'_EVER = HDP_DX_`let'
	
	* then take value from ANC if =1 
	replace `let'_EVER = 1 if HDP_DX_`let'_ANC == 1 
	
	
	* Timing variable where: 
		*1=ANC - MN04 
		*2=ANC - MNH19 
		*3=LD Form - MNH09 
	gen `let'_TIMING = 1 if HDP_DX_`let'_ANC == 1 & FIRST_DX_`let' ==4
	replace `let'_TIMING = 2 if HDP_DX_`let'_ANC == 1 & FIRST_DX_`let' ==19		
	replace `let'_TIMING = 3 if HDP_DX_`let'_ANC != 1 & HDP_DX_`let' == 1 
	
	tab `let'_EVER `let'_TIMING, m 
		
	}
	
	
	**** Create Groups of Interest: 
	
		// chronic hypertension: 
	gen HDP_GROUP = 0 if HTN_ANY == 0 
	replace HDP_GROUP = 55 if HTN_ANY == 55
	replace HDP_GROUP = 1 if HTN_ANY == 1 
	
		// gestational hypertension (excludes chronic): 
			// 2+ high BP measures: 
	replace HDP_GROUP = 2 if HIGH_BP_COUNT >= 2 & HIGH_BP_COUNT != . & ///
		HTN_ANY != 1
		
	gen GHYP_IND = 1 if HIGH_BP_COUNT >= 2 & HIGH_BP_COUNT != . & ///
		HTN_ANY != 1
		
			// 1+ severe high BP measure: 
	replace HDP_GROUP = 2 if HIGH_BP_SEV_COUNT >= 1 & HIGH_BP_SEV_COUNT != . & ///
		HTN_ANY != 1 
		
	replace GHYP_IND = 2 if HIGH_BP_SEV_COUNT >= 1 & HIGH_BP_SEV_COUNT != . & ///
		HTN_ANY != 1 & GHYP_IND == . 
		
			// 1+ high BP AND medication: 
	replace HDP_GROUP = 2 if HIGH_BP_COUNT >=1 & HIGH_BP_COUNT != . & ///
		HTN_ANY != 1 & ///
		(HDP_TREAT_ANC == 1 | HDP_TREAT_LD == 1 | HDP_TREAT2_LD == 1)
		
	replace GHYP_IND = 3 if HIGH_BP_COUNT >=1 & HIGH_BP_COUNT != . & ///
		HTN_ANY != 1 & GHYP_IND == . & ///
		(HDP_TREAT_ANC == 1 | HDP_TREAT_LD == 1 | HDP_TREAT2_LD == 1)
		
			// Diagnosis of Gestational Hypertension at >= 20 weeks GA 
	replace HDP_GROUP = 2 if GHYP_EVER == 1 & HTN_ANY != 1
	
	replace GHYP_IND = 4 if GHYP_EVER == 1 & HTN_ANY != 1 & GHYP_IND == .
	
			// Diagnosis of Unknown Hypertensive Disorder OR chronic HTN 
			// at >= 20 weeks GA 
	replace HDP_GROUP = 2 if (DK_EVER == 1 | HTN_EVER == 1) ///
		& HTN_ANY != 1
	
	replace GHYP_IND = 5 if (DK_EVER == 1 | HTN_EVER == 1) & ///
		HTN_ANY != 1 & GHYP_IND == .		
	
	tab HDP_GROUP, m 
	tab GHYP_IND HDP_GROUP, m 
	
		label define ghypind 1 "1-2+ high bp measures" 2 "2-1+ severe bp" ///
			3 "3-1+ high BP & meds" 4 "4-Dx of GHYP" 5 "5-Dx of other HTN" 
		label values GHYP_IND ghypind
	
	*Gestational hypertension indicator: 
	gen GHTN = 0 if HDP_GROUP == 0 // none if HDP_GROUP == 0 
	replace GHTN = 55 if HDP_GROUP == 55 // Missing if unknown HDP status at baseline 
	replace GHTN = 77 if HTN_ANY == 1 // N/A if underlying CHTN 
	replace GHTN = 1 if HDP_GROUP == 2 // 1 if meets criteria for GHTN 
	
	label var GHTN "Gestational hypertension"
	
	tab GHTN, m 

	
	// preeclampsia:
	
		////////////
		* create an indicator for baseline + protein: 
		
		gen PROT_BL = 0 if UA_PROT_PRIOR20_LBORRES == 0 | UA_PROT_PRIOR20_LBORRES == 1 
		replace PROT_BL = 1 if UA_PROT_PRIOR20_LBORRES >= 2 & UA_PROT_PRIOR20_LBORRES <=5
		
		tab UA_PROT_PRIOR20_LBORRES PROT_BL, m 
		
		*Compare to protein readings >= 20 weeks:
		tab UA_PROT_LBORRES PROT_BL, m 
		////////////
	
		*** gestational hypertension + proteinuria >=1+ at >= 20 weeks, negative/trace at BL
	replace HDP_GROUP = 3 if HTN_ANY!= 1 & ///	
		HDP_GROUP == 2 & UA_PROT_LBORRES >=2 & UA_PROT_LBORRES <=5 & ///
		PROT_BL == 0 
		
	gen PREEC_IND = 1 if HTN_ANY!= 1 & ///	
		HDP_GROUP == 3 & UA_PROT_LBORRES >=2 & UA_PROT_LBORRES <=5 & ///
		PROT_BL == 0 
		
		*** gestational hypertension + proteinuria >=1+ at >= 20 weeks, no BL
	replace HDP_GROUP = 3 if HTN_ANY!= 1 & ///	
		HDP_GROUP == 2 & UA_PROT_LBORRES >=2 & UA_PROT_LBORRES <=5 & ///
		PROT_BL == . 
		
	replace PREEC_IND = 2 if HTN_ANY!= 1 & ///	
		HDP_GROUP == 3 & UA_PROT_LBORRES >=2 & UA_PROT_LBORRES <=5 & ///
		PROT_BL == . 
		
		*** gestational hypertension + proteinuria >=2+ at >= 20 weeks, 1+ at BL
	replace HDP_GROUP = 3 if HTN_ANY!= 1 & ///	
		HDP_GROUP == 2 & UA_PROT_LBORRES >=3 & UA_PROT_LBORRES <=5 & ///
		UA_PROT_PRIOR20_LBORRES==2
		
	replace PREEC_IND = 3 if HTN_ANY!= 1 & ///	
		HDP_GROUP == 3 & UA_PROT_LBORRES >=3 & UA_PROT_LBORRES <=5 & ///
		UA_PROT_PRIOR20_LBORRES==2
		
		/*** gestational hypertension + proteinuria 1+ at BL and 1+ in pregnancy  --- THIS TYPE OF CASE IS EXCLUDED: NOT INCIDENT CASE OF PROTEINURIA
	replace HDP_GROUP = 3 if HTN_ANY!= 1 & ///	
		HDP_GROUP == 2 & UA_PROT_LBORRES ==2 &  ///
		UA_PROT_PRIOR20_LBORRES==2
		
	replace PREEC_IND = 4 if HTN_ANY!= 1 & ///	
		HDP_GROUP == 3 & UA_PROT_LBORRES ==2 &  ///
		UA_PROT_PRIOR20_LBORRES==2
		*/
		
		*** gestational hypertension + protein SYMPTOM at L&D
	replace HDP_GROUP = 3 if HTN_ANY!= 1 & ///	
		HDP_GROUP == 2 & SYMPTOM_PROTEIN_LD ==1
		
	replace PREEC_IND = 5 if HTN_ANY!= 1 & ///	
		HDP_GROUP == 3 & SYMPTOM_PROTEIN_LD ==1 & PREEC_IND == . 
		
		*** Dx of preeclampsia
	replace HDP_GROUP = 3 if HTN_ANY!= 1 & ///	
		PREEC_EVER == 1 
		
	replace PREEC_IND = 6 if HTN_ANY!= 1 & ///	
		HDP_GROUP == 3 & PREEC_EVER==1 & PREEC_IND == . 
		
	tab PREEC_IND HDP_GROUP, m 
	
	label define preecind 1 "1-GH+Prot, BL0/trace" 2 "2-GH+Prot, no BL" ///
		3 "3-GH+Prot >=2+, 1+BL" 4 "4-GH+Prot 1+, 1+BL" ///
		5 "5-GH+Prot symp L&D" 6 "6-Dx of Preeclampsia"
		
	label values PREEC_IND preecind 
	
	tab PREEC_IND HDP_GROUP, m 
	

	// preeclampsia superimposed on HTN: 	
		*** chronic hypertension + proteinuria >=1+ , neg/trace baseline 
	replace HDP_GROUP = 4 if HTN_ANY== 1 & ///	
		UA_PROT_LBORRES >=2 & UA_PROT_LBORRES <=5 & PROT_BL == 0 
		
	gen PREEC_SUP_IND = 1 if HTN_ANY== 1 & ///	
		UA_PROT_LBORRES >=2 & UA_PROT_LBORRES <=5 & PROT_BL == 0 
		
	// preeclampsia superimposed on HTN: 	
		*** chronic hypertension + proteinuria >=1+ , no baseline 
	replace HDP_GROUP = 4 if HTN_ANY== 1 & ///	
		UA_PROT_LBORRES >=2 & UA_PROT_LBORRES <=5 & PROT_BL == . 
		
	replace PREEC_SUP_IND = 2 if HTN_ANY== 1 & ///	
		UA_PROT_LBORRES >=2 & UA_PROT_LBORRES <=5 & PROT_BL == . 
		
	// preeclampsia superimposed on HTN: 	
		*** chronic hypertension + proteinuria >=2+ , 1+ at BL 
	replace HDP_GROUP = 4 if HTN_ANY== 1 & ///	
		UA_PROT_LBORRES >=3 & UA_PROT_LBORRES <=5 & UA_PROT_PRIOR20_LBORRES ==2  
		
	replace PREEC_SUP_IND = 3 if HTN_ANY== 1 & ///	
		UA_PROT_LBORRES >=3 & UA_PROT_LBORRES <=5 & UA_PROT_PRIOR20_LBORRES ==2 
		
	// preeclampsia superimposed on HTN: 	
		*** chronic hypertension + proteinuria >=1+ , 1+ at BL --- THIS TYPE OF CASE IS EXCLUDED: NOT INCIDENT CASE OF PROTEINURIA 
	/*replace HDP_GROUP = 4 if HTN_ANY== 1 & ///	
		UA_PROT_LBORRES ==2 & UA_PROT_PRIOR20_LBORRES ==2  
		
	replace PREEC_SUP_IND = 4 if HTN_ANY== 1 & ///	
		UA_PROT_LBORRES ==2 & UA_PROT_PRIOR20_LBORRES ==2   
	*/
		
	
		*** gestational hypertension + protein SYMPTOM at L&D
	replace HDP_GROUP = 4 if HTN_ANY== 1 & ///	
		SYMPTOM_PROTEIN_LD ==1
		
	replace PREEC_SUP_IND = 5 if HTN_ANY== 1 & ///	
		SYMPTOM_PROTEIN_LD ==1 & PREEC_SUP_IND == . 
		
		*** Dx of preeclampsia
	replace HDP_GROUP = 4 if HTN_ANY== 1 & ///	
		PREEC_EVER == 1 
		
	replace PREEC_SUP_IND = 6 if HTN_ANY== 1 & ///	
		PREEC_EVER==1 & PREEC_SUP_IND == . 
		
	tab PREEC_SUP_IND HDP_GROUP, m 	
	
	label define suppreecind 1 "1-CH+Prot, BL0/trace" 2 "2-CH+Prot, no BL" ///
		3 "3-CH+Prot >=2+, 1+BL" 4 "4-CH+Prot 1+, 1+BL" ///
		5 "5-CH+Prot symp L&D" 6 "6-Dx of Preeclampsia"
		
	label values PREEC_SUP_IND suppreecind 
	
	tab PREEC_SUP_IND HDP_GROUP, m 
	
	
		// preeclampsia with severe features: 	
		
		///////////////////////////////////////////////////////////
		*SOME severe features are sufficient to escalate to PE with 
		*severe features on their own, including:
			
			*Eclampsia/seizures
			*HELLP
			*Severe High BP at/after 20 weeks GA 	
	
		// eclampsia, seizures: 
	gen PREEC_SEV_IND = 1 if HDP_SEIZURES_ANC == 1 | SEVERE_SEIZURES == 1 | ///
		SYMPTOM_SEIZURE_LD == 1 | HDP_DX_ECL_ANC == 1
		
	tab HDP_GROUP PREEC_SEV_IND, m 
	
	replace HDP_GROUP = 5 if  HDP_SEIZURES_ANC == 1 | SEVERE_SEIZURES == 1 | ///
		SYMPTOM_SEIZURE_LD == 1 	
	
		// HELLP syndrome: 
	replace PREEC_SEV_IND = 2 if PREEC_SEV_IND==. & (HDP_HELLP_ANC == 1 | SEVERE_HELLP == 1) 
	
	tab HDP_GROUP PREEC_SEV_IND, m 
	
	replace HDP_GROUP = 5 if HDP_HELLP_ANC == 1 | SEVERE_HELLP == 1 
		
			*Severe High BP measured 
		replace PREEC_SEV_IND = 4 if PREEC_SEV_IND==. & ///
			HIGH_BP_SEV_COUNT >=1 & HIGH_BP_SEV_COUNT != . & ///
			 (HIGH_BP_SEV_COUNT_LESS20 == 0 | HIGH_BP_SEV_COUNT_LESS20 ==.)
			
		replace HDP_GROUP = 5 if ///
			HIGH_BP_SEV_COUNT >=1 & HIGH_BP_SEV_COUNT != . & ///
			 (HIGH_BP_SEV_COUNT_LESS20 == 0 | HIGH_BP_SEV_COUNT_LESS20 ==.)			
			
			*Severe High BP dx
		replace PREEC_SEV_IND = 5 if PREEC_SEV_IND==. & ///
			(HDP_SEVHYP_ANC == 1 | SEVERE_SEVHYP == 1)
			
		replace HDP_GROUP = 5 if (HDP_SEVHYP_ANC == 1 | SEVERE_SEVHYP == 1)			
			
			
		///////////////////////////////////////////////////	
		*OTHER features should be among High BP cases only: 
			*Organ dysfunction

			
/////////////////////////////////////////////////////////////////////
* NOTE: Updated on 10-16-2024; we will now reconstruct organ failure 
* variable SEPARATELY from the near-miss outcome to support flow/
* efficiency of outcomes construction process.
////////////////////////////////////////////////////////////////////

	preserve 
		
	clear 
		
	* PULLED FROM SAVANNAH'S NEAR-MISS CODE: 
	
	// MNN09 
	
	import delimited "$da/mnh09_merged.csv", ///
	bindquote(strict) case(upper) clear

	rename M09_* *
	
	tab1 ORG_FAIL_MHOCCUR_1 ORG_FAIL_MHOCCUR_2 ORG_FAIL_MHOCCUR_3 ///
		ORG_FAIL_MHOCCUR_4 ORG_FAIL_MHOCCUR_5 ORG_FAIL_MHOCCUR_6 ///
		ORG_FAIL_MHOCCUR_7 ORG_FAIL_MHOCCUR_77 ORG_FAIL_MHOCCUR_88 ///
		ORG_FAIL_MHOCCUR_99 ORG_FAIL_SPFY_MHTERM
	
	gen ORG_FAIL_HRT_M09 = ORG_FAIL_MHOCCUR_1
	gen ORG_FAIL_RESP_M09 = ORG_FAIL_MHOCCUR_2
	gen ORG_FAIL_RENAL_M09 = ORG_FAIL_MHOCCUR_3
	gen ORG_FAIL_LIVER_M09 = ORG_FAIL_MHOCCUR_4
	gen ORG_FAIL_NEUR_M09 = ORG_FAIL_MHOCCUR_5
	gen ORG_FAIL_UTER_M09 = ORG_FAIL_MHOCCUR_6
	gen ORG_FAIL_HEM_M09 = ORG_FAIL_MHOCCUR_7
	 
	label var ORG_FAIL_HRT_M09 "Heart"
	label var ORG_FAIL_RESP_M09 "Respiratory"
	label var ORG_FAIL_RENAL_M09 "Renal (kidney)"
	label var ORG_FAIL_LIVER_M09 "Liver/hepatic"
	label var ORG_FAIL_NEUR_M09 "Neurological"
	label var ORG_FAIL_UTER_M09 "Uterine"
	label var ORG_FAIL_HEM_M09 "Coagulation/hematologic"
	label var ORG_FAIL_MHOCCUR_77 "No organ failure, M09"
	label var ORG_FAIL_MHOCCUR_88 "Other specify: M09"
	label var ORG_FAIL_MHOCCUR_99 "Don't know, M09"

	*Indicator for if any organ dysfunction/failure was reported in MNH09
	gen MAT_DYS_M09 = 0 
	replace MAT_DYS_M09 = 1 if ///
	ORG_FAIL_MHOCCUR_1 == 1 | ORG_FAIL_MHOCCUR_2 == 1 | ///
	ORG_FAIL_MHOCCUR_3 == 1 | ORG_FAIL_MHOCCUR_4 == 1 | ///
	ORG_FAIL_MHOCCUR_5 == 1 | ORG_FAIL_MHOCCUR_6 == 1 | ///
	ORG_FAIL_MHOCCUR_7 == 1 | ORG_FAIL_MHOCCUR_88 == 1 
	label var MAT_DYS_M09 "Organ dysfunction recorded MNH09"
	
	list ORG_FAIL_MHOCCUR_1 ORG_FAIL_MHOCCUR_2 ORG_FAIL_MHOCCUR_3 ORG_FAIL_MHOCCUR_4 ORG_FAIL_MHOCCUR_5 ORG_FAIL_MHOCCUR_6 ORG_FAIL_MHOCCUR_7  ORG_FAIL_MHOCCUR_88 ORG_FAIL_MHOCCUR_99 if MAT_DYS_M09 ==1	
	
	gen CHK_ORG_FAIL_SPFY = 1 if ///
	ORG_FAIL_MHOCCUR_88 == 1 | ORG_FAIL_SPFY_MHTERM!="n/a"
	label var CHK_ORG_FAIL_SPFY ///
	"Organ failure specified, examine responses"
	list ORG_FAIL_MHOCCUR_1 ORG_FAIL_MHOCCUR_2 ORG_FAIL_MHOCCUR_3 ORG_FAIL_MHOCCUR_4 ORG_FAIL_MHOCCUR_5 ORG_FAIL_MHOCCUR_6 ORG_FAIL_MHOCCUR_7  ORG_FAIL_MHOCCUR_88  if CHK_ORG_FAIL_SPFY ==1
	**check these responses to see if they are captured elsewhere
	
	label var ORG_FAIL_SRCE_1 "Maternal recall"
	label var ORG_FAIL_SRCE_2 "Facility or participant record"
	label var ORG_FAIL_SRCE_3 "Study assessment"

	gen MISS_SOURCE = 1 if MAT_DYS == 1 & ///
	(ORG_FAIL_SRCE_1!=1 & ORG_FAIL_SRCE_2!=1 & ORG_FAIL_SRCE_3!=1)
	label var MISS_SOURCE "Missing info source organ failure"
	**creates a missing indicator if:
		**(recorded that woman experienced organ failure) AND 
		**(there is no information source listed)
		
		rename MISS_SOURCE MISS_SOURCE_M09
		rename ORG_FAIL_MHOCCUR_77 ORG_FAIL_MHOCCUR_77_M09
		rename ORG_FAIL_MHOCCUR_88 ORG_FAIL_MHOCCUR_88_M09
		rename ORG_FAIL_MHOCCUR_99 ORG_FAIL_MHOCCUR_99_M09
		
	keep MOMID PREGID *_M09 ORG_FAIL_MHOCCUR_77 ORG_FAIL_MHOCCUR_88 ///
		ORG_FAIL_MHOCCUR_99 MISS_SOURCE 
		
	save "$wrk/organ_failure_mnh09", replace 
	
	
	// MNH19 
	import delimited "$da/mnh19_merged.csv", ///
	bindquote(strict) case(upper) clear

	rename M19_* *
	
	*Gen new vars for readability
	gen ORG_FAIL_HRT_M19 = 1 if ORGAN_FAIL_MHTERM_1 == 1
	gen ORG_FAIL_RESP_M19 = 1 if ORGAN_FAIL_MHTERM_2 == 1
	gen ORG_FAIL_RENAL_M19 = 1 if ORGAN_FAIL_MHTERM_3 == 1
	gen ORG_FAIL_LIVER_M19 = 1 if ORGAN_FAIL_MHTERM_4 == 1 
	gen ORG_FAIL_NEUR_M19 = 1 if ORGAN_FAIL_MHTERM_5 == 1 
	gen ORG_FAIL_UTER_M19 = 1 if ORGAN_FAIL_MHTERM_6 == 1
	gen ORG_FAIL_HEM_M19 = 1 if ORGAN_FAIL_MHTERM_7 == 1
	gen ORG_FAIL_OTHR_M19 = 1 if ORGAN_FAIL_MHTERM_88 == 1 
	gen ORG_FAIL_SPFY_M19 = ORGAN_FAIL_SPFY_MHTERM if ///
	ORGAN_FAIL_SPFY_MHTERM != "n/a"

	
	gen MAT_DYS_M19 = 0
	replace MAT_DYS_M19 = 1 if ///
	ORGAN_FAIL_MHTERM_1 == 1 | /// heart failure
	ORGAN_FAIL_MHTERM_2 == 1 | /// respiratory
	ORGAN_FAIL_MHTERM_3 == 1 | /// renal/kidney
	ORGAN_FAIL_MHTERM_4 == 1 | /// liver/hepatic
	ORGAN_FAIL_MHTERM_5 == 1 | /// neurological
	ORGAN_FAIL_MHTERM_6 == 1 | /// uterine
	ORGAN_FAIL_MHTERM_7 == 1 | /// coagulation/hematologic
	ORGAN_FAIL_MHTERM_88 == 1 // other/specify
	label var MAT_DYS_M19 "Organ dysfunction MNH19"
	
	gen CHK_ORG_FAIL_SPFY = 1  if ///
	ORGAN_FAIL_MHTERM_88 == 1 | ORGAN_FAIL_SPFY_MHTERM != "n/a"
	label var CHK_ORG_FAIL_SPFY "Manually check if failure is specified"
	
	*construct date: 
	
	gen ORG_FAIL_DATE_M19 = date(OHOSTDAT, "YMD") if OHOSTDAT != "1907-07-07" & ///
		 OHOSTDAT != "1905-05-05" 
		 
	replace ORG_FAIL_DATE = date(MAT_EST_OHOSTDAT, "YMD") if ///
		ORG_FAIL_DATE == . & MAT_EST_OHOSTDAT != "1907-07-07" & ///
		MAT_EST_OHOSTDAT != "1905-05-05" 
		
	replace ORG_FAIL_DATE = date(ADMIT_OHOSTDAT, "YMD") if ///
		ORG_FAIL_DATE == . & ADMIT_OHOSTDAT != "1907-07-07" & ///
		ADMIT_OHOSTDAT != "1905-05-05" 
		
	*Restrict to cases reporting any organ dysfunction: 
	keep if MAT_DYS_M19 == 1 
	
	*Rename for consistency: 
	rename ORGAN_FAIL_MHTERM_88 ORGAN_FAIL_MHTERM_88_M09
	rename ORGAN_FAIL_MHTERM_99 ORGAN_FAIL_MHTERM_99_M09 
	
	keep MOMID PREGID MAT_DYS_M19 ORG_FAIL_* ORGAN_FAIL_MHTERM_88_M09 ///
		ORGAN_FAIL_MHTERM_99_M09 ORG_FAIL_DATE
		
	save "$wrk/organ_failure_mnh19", replace 
	
	
	// MNH12 
	import delimited "$da/mnh12_merged.csv", ///
	bindquote(strict) case(upper) clear
	
	tab1 M12_ORG_FAIL_MHOCCUR_1 M12_ORG_FAIL_MHOCCUR_2 ///
	M12_ORG_FAIL_MHOCCUR_3 M12_ORG_FAIL_MHOCCUR_4  M12_ORG_FAIL_MHOCCUR_88
	gen ORG_FAIL_HRT_M12 = 1 if M12_ORG_FAIL_MHOCCUR_1 == 1 
	gen ORG_FAIL_RESP_M12 = 1 if M12_ORG_FAIL_MHOCCUR_2==1
	gen ORG_FAIL_RENAL_M12 = 1 if M12_ORG_FAIL_MHOCCUR_3==1
	gen ORG_FAIL_LIVER_M12 = 1 if M12_ORG_FAIL_MHOCCUR_4==1
	gen ORG_FAIL_OTHR_M12 = 1 if M12_ORG_FAIL_MHOCCUR_88==1
	
	rename M12_ORG_FAIL_MHTERM ORG_FAIL_SPFY_M12 

	gen MAT_DYS_M12 = 1 if ///
	ORG_FAIL_HRT_M12 == 1 | ORG_FAIL_RESP_M12 == 1 | ///
	ORG_FAIL_RENAL_M12 == 1 | ORG_FAIL_LIVER_M12 == 1 | ///
	ORG_FAIL_OTHR_M12 == 1 
	label var MAT_DYS_M12 "Organ failure M12"
	
	*Review the "other-specify cases"
	list ORG_FAIL_SPFY_M12  if M12_ORG_FAIL_MHOCCUR_88==1 

	list M12_ORG_FAIL_MHOCCUR_1 M12_ORG_FAIL_MHOCCUR_2 M12_ORG_FAIL_MHOCCUR_3 M12_ORG_FAIL_MHOCCUR_4  M12_ORG_FAIL_MHOCCUR_88 ORG_FAIL_SPFY_M12 if MAT_DYS_M12==1
	
	* Take date reported for organ failure: 
	gen ORG_FAIL_DATE_M12 = date(M12_VISIT_OBSSTDAT, "YMD")
	format ORG_FAIL_DATE_M12 %d
	label var ORG_FAIL_DATE_M12 "Visit date when organ failure reported (postpartum)"
	
	* Create an ordered visit number variable for PNC visits: 
		* first keep if visit complete: 
		keep if M12_MAT_VISIT_MNH12 == 1 | M12_MAT_VISIT_MNH12 == 2 
		
		*sort
		sort MOMID PREGID ORG_FAIL_DATE
		*create indicator for number of entries per person: 
		quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
		tab ENTRY_NUM, m 
	
		replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
		label var ENTRY_NUM "Completed PNC Visit Number"
	
	*restrict to any cases of organ failure: 
	keep if MAT_DYS_M12 == 1 
	
	list MOMID PREGID ORG_FAIL_DATE_M12 M12_TYPE_VISIT ENTRY_NUM 
	
	rename ENTRY_NUM ENTRY_NUM_M12
	rename M12_TYPE_VISIT TYPE_VISIT_M12

	keep MOMID PREGID MAT_DYS_M12 ORG_FAIL_* TYPE_VISIT_M12 ENTRY_NUM_M12
	
	save "$wrk/organ_failure_mnh12", replace 
	
	restore 	
	
	
	*MERGE IN ORGAN FAILURE DATASETS: 
	
	foreach let in mnh09 mnh19 mnh12 {
		
	merge 1:1 MOMID PREGID using "$wrk/organ_failure_`let'.dta"
		
	drop if _merge == 2 
	drop _merge 
	
	}
	
	* Create a combined variable for organ dysfunction : 
	
	gen MAT_DYS = 1 if MAT_DYS_M09 == 1 | MAT_DYS_M12 == 1 | MAT_DYS_M19 == 1 
	
		// review observations: 
	list MAT_DYS_M09 PREG_END_DATE  MAT_DYS_M19 ORG_FAIL_DATE_M19 ///
		 MAT_DYS_M12 ORG_FAIL_DATE_M12  if MAT_DYS==1 
	
	* Create a combined timing variable for organ dysfunction : 
		// here, we prioritize at L&D: 
	gen MAT_DYS_DATE = PREG_END_DATE if MAT_DYS_M09 == 1
	format MAT_DYS_DATE %d 
	
		// then, hospitalizations during pregnancy: 
	replace MAT_DYS_DATE = ORG_FAIL_DATE_M19 if MAT_DYS_M19 == 1 & ///
		MAT_DYS_DATE == . & ORG_FAIL_DATE_M19 <= PREG_END_DATE 
		
		// then, reported at PNC: 
	replace MAT_DYS_DATE = ORG_FAIL_DATE_M12 if MAT_DYS_M12 == 1 & MAT_DYS_DATE == . 
	
		// then, hospitalizations postpartum if the date is EARLIER than PNC12: 
	replace MAT_DYS_DATE = ORG_FAIL_DATE_M19 if MAT_DYS_M19 == 1 & ///
		((MAT_DYS_DATE == . & ORG_FAIL_DATE_M19 > PREG_END_DATE) | ///
		 (ORG_FAIL_DATE_M19 < MAT_DYS_DATE & ORG_FAIL_DATE_M19 > PREG_END_DATE))
	
	* Review dates & organ failures: 
	tab MAT_DYS_DATE MAT_DYS, m 
	
	* Calculate GA at organ failure: 
	gen MAT_DYS_GA = MAT_DYS_DATE - PREG_START_DATE if MAT_DYS==1 & ///
		MAT_DYS_DATE <=PREG_END_DATE & MAT_DYS_DATE != . & PREG_END_DATE != . 
		
	tab MAT_DYS_GA, m 
	
	tab MAT_DYS_GA PREG_END_GA
	
	* Calculate days postpartum at organ failure 
	gen MAT_DYS_PP = MAT_DYS_DATE - PREG_END_DATE if MAT_DYS==1 & ///
		MAT_DYS_DATE >= PREG_END_DATE & MAT_DYS_DATE != . & PREG_END_DATE != . 
		
	tab MAT_DYS_PP, m 
	
	*Create a timing variable: 
		// during pregnancy: 
	gen MAT_DYS_TIMING = 0 if MAT_DYS == 1 & MAT_DYS_GA < PREG_END_GA & ///
		MAT_DYS_GA != . & PREG_END_GA != . 
	
		// during IPC: 
	replace MAT_DYS_TIMING = 1 if MAT_DYS == 1 & MAT_DYS_GA == PREG_END_GA 
	
		// during postpartum period (up to 42 days)
	replace MAT_DYS_TIMING = 2 if MAT_DYS == 1 & MAT_DYS_PP >= 1 & ///
		MAT_DYS_PP<= 42 
		
		// late postpartum (after 42 days postpartum)
	replace MAT_DYS_TIMING = 3 if MAT_DYS == 1 & MAT_DYS_PP > 42 & MAT_DYS_PP!=.
	
		// unknown timing 
	replace MAT_DYS_TIMING = 55 if MAT_DYS == 1 & MAT_DYS_DATE == . 
	
	label var MAT_DYS_TIMING "Timing of organ dysfunction (0=preg;1=IPC;2=PP42;3=PP43+)"
	
	tab MAT_DYS_TIMING MAT_DYS, m 
	
		* Review PNC info for those in the late postpartum period: 
		sort MAT_DYS_TIMING
		list PREG_END_DATE MAT_DYS_DATE MAT_DYS_TIMING MAT_DYS_PP ///
			MAT_DYS_M09 MAT_DYS_M12 TYPE_VISIT_M12 ENTRY_NUM_M12 if MAT_DYS==1 
	
	*Code below will take organ dysfunction IF it occurs during pregnancy or 
	*within 42 days postpartum (i.e., MAT_DYS_TIMING is 0 or 1 or 2)
	
	tab HDP_GROUP MAT_DYS, m  
	tab HDP_GROUP MAT_DYS_TIMING, m 
	
	replace PREEC_SEV_IND = 3 if PREEC_SEV_IND==. & MAT_DYS == 1 & ///
		(MAT_DYS_TIMING >=0 & MAT_DYS_TIMING <=2) & ///
		HDP_GROUP >= 2 & HDP_GROUP <=4 
		
	replace HDP_GROUP = 5 if MAT_DYS == 1 & MAT_DYS == 1 & ///
		(MAT_DYS_TIMING >=0 & MAT_DYS_TIMING <=2) & ///
		HDP_GROUP >= 2 & HDP_GROUP <=4 
	
		
		*Finally, some features should be among PE cases only, including: 
			*Pulmonary edema; 
			*Visual symptoms; 
			*epigastric pain 
			*severe headache 
	
			*Pulmonary Edema
			
				*review:
				tab HDP_GROUP SEVERE_PE, m 
				tab HDP_GROUP HDP_PE_ANC, m 
			
		replace PREEC_SEV_IND = 6 if (HDP_GROUP == 4 | HDP_GROUP==3) & ///
			(HDP_PE_ANC == 1 | SEVERE_PE == 1)
			
		replace HDP_GROUP = 5 if (HDP_GROUP == 4 | HDP_GROUP==3) & ///
			(HDP_PE_ANC == 1 | SEVERE_PE == 1) 
			
			*Visual symptoms
		replace PREEC_SEV_IND = 7 if (HDP_GROUP == 4 | HDP_GROUP==3) & ///
			(HDP_VIS_ANC == 1 | SEVERE_VISUAL == 1 | SYMPTOM_VISION_LD == 1)
			
		replace HDP_GROUP = 5 if (HDP_GROUP == 4 | HDP_GROUP==3) & ///
			(HDP_VIS_ANC == 1 | SEVERE_VISUAL == 1 | SYMPTOM_VISION_LD == 1)
			 
			*Epigastric pain
		replace PREEC_SEV_IND = 8 if (HDP_GROUP == 4 | HDP_GROUP==3) & ///
			(SYMPTOM_EPIPAIN_HOSP==1)
			
		replace HDP_GROUP = 5 if (HDP_GROUP == 4 | HDP_GROUP==3) & ///
			(SYMPTOM_EPIPAIN_HOSP==1) 			
		
		
			*Severe headache
		replace PREEC_SEV_IND = 9 if (HDP_GROUP == 4 | HDP_GROUP==3) & ///
			(SYMPTOM_HEADACHE_HOSP==1 | SYMPTOM_HEADACHE_LD==1)
			
		replace HDP_GROUP = 5 if (HDP_GROUP == 4 | HDP_GROUP==3) & ///
			(SYMPTOM_HEADACHE_HOSP==1 | SYMPTOM_HEADACHE_LD==1) 			
			
	
		label define severeind 1 "1-Dx Eclampsia/Seizures" 2 "2-Dx HELLP" ///
			3 "3-GH/PE w/ organ dysf" 4 "4-Severe BP measured" ///
			5 "5-Dx severe BP" 6 "6-PE w/ pulm edema" ///
			7 "7-PE w/ visual" 8 "8-PE w/ epig pain" 9 "9-PE w/ severe headache"
			
		label values PREEC_SEV_IND severeind 
		
		tab PREEC_SEV_IND HDP_GROUP
		
	////////////////////////////////////////////////////////////////
	* create non-mutually exclusive indicators for severe features:
	
	foreach let in SEIZURES HELLP ORGAN SEVHIGH SEVHYP PULMED VISUAL ///
		EPIPAIN HEAD {
			
	gen SEVERE_FEAT_`let' = 0 if HDP_GROUP == 5 
	
		}
	
		replace SEVERE_FEAT_SEIZURES = 1 if HDP_SEIZURES_ANC == 1 | ///
			SEVERE_SEIZURES == 1 | SYMPTOM_SEIZURE_LD == 1 
			
		replace SEVERE_FEAT_HELLP = 1 if HDP_HELLP_ANC == 1 | SEVERE_HELLP == 1
		
		* Edit: organ dysfunction must be within pregnancy or 42 days pp: 
		replace SEVERE_FEAT_ORGAN = 1 if MAT_DYS == 1 & HDP_GROUP == 5 & ///
			(MAT_DYS_TIMING >= 0 & MAT_DYS_TIMING <= 2)
		
		replace SEVERE_FEAT_SEVHIGH = 1 if HDP_GROUP == 5 & ///
			(HIGH_BP_SEV_COUNT >=1 & HIGH_BP_SEV_COUNT != .)
		
		replace SEVERE_FEAT_SEVHYP = 1 if HDP_GROUP == 5 & ///
			(HDP_PE_ANC == 1 | SEVERE_PE == 1)
			
		replace SEVERE_FEAT_PULMED = 1 if HDP_GROUP == 5 & ///
			(HDP_PE_ANC == 1 | SEVERE_PE == 1)
		
		replace SEVERE_FEAT_VISUAL = 1 if HDP_GROUP == 5 & ///
			(HDP_VIS_ANC == 1 | SEVERE_VISUAL == 1 | SYMPTOM_VISION_LD == 1)
		
		replace SEVERE_FEAT_EPIPAIN = 1 if HDP_GROUP == 5 & ///
			(SYMPTOM_EPIPAIN_HOSP==1) 	
		
		replace SEVERE_FEAT_HEAD = 1 if HDP_GROUP == 5 & ///
			(SYMPTOM_HEADACHE_HOSP==1 | SYMPTOM_HEADACHE_LD==1) 	
			
		
		label var SEVERE_FEAT_SEIZURES "PE with severe features: eclampsia/seizures"
		label var SEVERE_FEAT_HELLP "PE with severe features: HELLP"
		label var SEVERE_FEAT_SEVHIGH "PE with severe features: 1+ Severe high BP"
		label var SEVERE_FEAT_SEVHYP "PE with severe features: Dx with severe high BP"
		label var SEVERE_FEAT_ORGAN "PE with severe features: organ dysfunction"
		label var SEVERE_FEAT_PULMED "PE with severe features: pulmonary edema"
		label var SEVERE_FEAT_VISUAL "PE with severe features: visual symptoms"
		label var SEVERE_FEAT_EPIPAIN "PE with severe features: epigastric pain"
		label var SEVERE_FEAT_HEAD "PE with severe features: severe headache"
			
	
	label define hdps 0 "0-No HDP" 1 "1-Chronic HTN" 2 "2-Gest Hyper" ///
		3 "3-Preeclampsia" 4 "4-Superimposed Preeclampsia" ///
		5 "5-Severe features" 55 "55-Missing info" 77 "77-Pregnancy loss"
		
	label values HDP_GROUP hdps
	
	*** Checks against indication variables:
	
	tab  HDP_GROUP  INDUCED_HDP, m 
	
	tab  HDP_GROUP  CES_PREEC, m 
	
		*9 observations with preeclampsia indication but NO dx/criteria met: 
		list 
	
	tab PREG_END_GA HDP_GROUP, m 
	
	tab HDP_GROUP
	
	
	*PREECLAMPSIA indicator: 
	gen PREECLAMPSIA = 0 if HDP_GROUP >=0 & HDP_GROUP <=2 // none if HDP_GROUP == 0 OR =1 OR =2
	replace PREECLAMPSIA = 55 if HDP_GROUP == 55 // Missing if unknown HDP status at baseline & no PE 
	replace PREECLAMPSIA = 1 if HDP_GROUP == 3 | HDP_GROUP == 4 | HDP_GROUP==5 // 1 if meets criteria for Preeclampsia 
	
	label var PREECLAMPSIA "Preeclampsia"
	
	tab PREECLAMPSIA, m 	
	tab PREECLAMPSIA GHTN, m
	tab PREECLAMPSIA HTN_ANY, m 
	
	
	*PREECLAMPSIA_SEV indicator: 
	gen PREECLAMPSIA_SEV = 0 if HDP_GROUP >= 0 & HDP_GROUP <=4 // none if HDP_GROUP <5 
	replace PREECLAMPSIA_SEV = 55 if HDP_GROUP == 55 // Missing if unknown HDP status at baseline & no PE 
	replace PREECLAMPSIA_SEV = 1 if HDP_GROUP == 5 // 1 if meets criteria for Preeclampsia 
	
	label var PREECLAMPSIA_SEV "Preeclampsia with severe features"
	
	tab PREECLAMPSIA_SEV, m 	
	tab PREECLAMPSIA PREECLAMPSIA_SEV, m 
	
	
	*Who should be considered "ineligible" for HDP: 
	
		// first, any pregnancies that end prior to 20 weeks:
		
	replace HDP_GROUP = 77 if PREG_END == 1 & PREG_END_GA < 140 & ///
		HDP_GROUP == 0 
	
	*Who should be considered "missing" for HDP?
	
	// missing blood pressure readings: 
	tab ENTRY_TOTAL HDP_GROUP, m 
	
		replace HDP_GROUP = 55 if  HDP_GROUP == 0  & ///
			(ENTRY_TOTAL ==. | ENTRY_TOTAL < 2 )
		
		
	// missing chronic HTN information: 
	tab HTN_ANY HDP_GROUP, m 
	
		replace HDP_GROUP = 55 if HTN_ANY == 55 & HDP_GROUP == 0 
		
	tab HDP_GROUP, m 
	
	// update missing data for GHTN, PREECLAMPSIA and PREECLAMPSIA_SEV indicators:
	replace GHTN = 55 if GHTN == 0 & HDP_GROUP == 55
	replace PREECLAMPSIA = 55 if PREECLAMPSIA == 0 & HDP_GROUP == 55
	replace PREECLAMPSIA_SEV = 55 if PREECLAMPSIA_SEV == 0 & HDP_GROUP == 55
	
	****** Create missing indicator: 
	gen HDP_GROUP_MISS = 1 if HDP_GROUP == 55 & (ENTRY_TOTAL ==. | ENTRY_TOTAL < 2 )
	replace HDP_GROUP_MISS = 2 if HTN_ANY == 55 & HDP_GROUP == 55
	replace HDP_GROUP_MISS = 3 if HDP_GROUP == 77 
	
	label define hdpmiss 1 "1- <2 BP measures" 2 "2- Unknown HTN at enrollment" ///
		3 "3-Preg loss <20w"
		
	label values HDP_GROUP_MISS hdpmiss
	label var HDP_GROUP_MISS "Reason missing: HDP Group"
	
	tab HDP_GROUP_MISS HDP_GROUP, m 
	
	tab HDP_GROUP SITE, m 
	
	/* Make a bar graph: date of pregnancy endpoint if any HTN disorders: 
	
	preserve 
	
	keep if HDP_GROUP >= 1 
	
	sum PREG_END_DATE 
	
	gen annual_meeting = date("20231101", "YMD")
	
	twoway histogram PREG_END_DATE, w(1) by(SITE) ///
	xline(23315) color(black) xsize(20) ysize(10)
	
	twoway histogram PREG_END_DATE, ///
	xline(23315) xsize(20) ysize(10)
	
	restore 
	
	*/
	
	////////////////////////////////////
	* Review cases: 1 high BP + proteinuria: 
	
	gen HIGH_BP_1 = 0 if HDP_GROUP == 0 
	replace HIGH_BP_1 = 1 if HDP_GROUP == 0 & HIGH_BP_COUNT == 1 
	
	label var HIGH_BP_1 "Participant with 1 high BP & no dx of other HDPs"
	
	
		// variables of interest: 
	tab UA_PROT_PRIOR20_LBORRES UA_PROT_LBORRES if HIGH_BP_1 == 1, m 
	
	tab SITE if HIGH_BP_1==1
	
	tab PREG_END_GA if HIGH_BP_1==1
	
		// generate severe features for this group:
		
	foreach let in SEIZURES HELLP ORGAN SEVHIGH SEVHYP PULMED VISUAL ///
		EPIPAIN HEAD {
			
	gen SEVERE_FEAT_TEST_`let' = 0 if HIGH_BP_1==1 
	
		}
	
		replace SEVERE_FEAT_TEST_SEIZURES = 1 if (HDP_SEIZURES_ANC == 1 | ///
			SEVERE_SEIZURES == 1 | SYMPTOM_SEIZURE_LD == 1) & HIGH_BP_1==1 
			
		replace SEVERE_FEAT_TEST_HELLP = 1 if HIGH_BP_1==1  & ///
			(HDP_HELLP_ANC == 1 | SEVERE_HELLP == 1)
		
		replace SEVERE_FEAT_TEST_ORGAN = 1 if MAT_DYS == 1 & HIGH_BP_1==1  
		
		replace SEVERE_FEAT_TEST_SEVHIGH = 1 if HIGH_BP_1==1  & ///
			(HIGH_BP_SEV_COUNT >=1 & HIGH_BP_SEV_COUNT != .)
		
		replace SEVERE_FEAT_TEST_SEVHYP = 1 if HIGH_BP_1==1  & ///
			(HDP_PE_ANC == 1 | SEVERE_PE == 1)
			
		replace SEVERE_FEAT_TEST_PULMED = 1 if HIGH_BP_1==1  & ///
			(HDP_PE_ANC == 1 | SEVERE_PE == 1)
		
		replace SEVERE_FEAT_TEST_VISUAL = 1 if HIGH_BP_1==1  & ///
			(HDP_VIS_ANC == 1 | SEVERE_VISUAL == 1 | SYMPTOM_VISION_LD == 1)
		
		replace SEVERE_FEAT_TEST_EPIPAIN = 1 if HIGH_BP_1==1  & ///
			(SYMPTOM_EPIPAIN_HOSP==1) 	
		
		replace SEVERE_FEAT_TEST_HEAD = 1 if HIGH_BP_1==1  & ///
			(SYMPTOM_HEADACHE_HOSP==1 | SYMPTOM_HEADACHE_LD==1) 
			
	foreach var of varlist SEVERE_FEAT_TEST_* {
		
	tab `var'
	
	tab `var' if (UA_PROT_PRIOR20_LBORRES == 0 | UA_PROT_PRIOR20_LBORRES == 1) & ///
		(UA_PROT_LBORRES >= 2 & UA_PROT_LBORRES <=5)
		
	}
	
	* Check against indications: 
	tab CES_PREEC HIGH_BP_1 if PROT_BL==0 & UA_PROT_LBORRES>2, m 
	tab INDUCED_HDP HIGH_BP_1, m 
	
	
	////////////////////////////////////
	* Review cases: 2+ high BP OR severe high BP OR 1 high BP + medication with 
		*no proteinuria BUT with any severe feature: 
	
	gen GHYP_REVIEW = 1 if HDP_GROUP == 2 & (GHYP_IND == 1 | GHYP_IND == 2 | ///
		GHYP_IND == 3)
	
	label var GHYP_REVIEW "Case w/ gestational hypertension, no protein, & severe features"	
	
	
		// generate severe features for this group:
		
	drop SEVERE_FEAT_TEST*
		
	foreach let in SEIZURES HELLP ORGAN SEVHIGH SEVHYP PULMED VISUAL ///
		EPIPAIN HEAD {
			
	gen SEVERE_FEAT_TEST_`let' = 0 if GHYP_REVIEW==1 
	
		}
	
		replace SEVERE_FEAT_TEST_SEIZURES = 1 if (HDP_SEIZURES_ANC == 1 | ///
			SEVERE_SEIZURES == 1 | SYMPTOM_SEIZURE_LD == 1) & GHYP_REVIEW==1 
			
		replace SEVERE_FEAT_TEST_HELLP = 1 if GHYP_REVIEW==1  & ///
			(HDP_HELLP_ANC == 1 | SEVERE_HELLP == 1)
		
		replace SEVERE_FEAT_TEST_ORGAN = 1 if MAT_DYS == 1 & GHYP_REVIEW==1  
		
		replace SEVERE_FEAT_TEST_SEVHIGH = 1 if GHYP_REVIEW==1  & ///
			(HIGH_BP_SEV_COUNT >=1 & HIGH_BP_SEV_COUNT != .)
		
		replace SEVERE_FEAT_TEST_SEVHYP = 1 if GHYP_REVIEW==1  & ///
			(HDP_PE_ANC == 1 | SEVERE_PE == 1)
			
		replace SEVERE_FEAT_TEST_PULMED = 1 if GHYP_REVIEW==1  & ///
			(HDP_PE_ANC == 1 | SEVERE_PE == 1)
		
		replace SEVERE_FEAT_TEST_VISUAL = 1 if GHYP_REVIEW==1  & ///
			(HDP_VIS_ANC == 1 | SEVERE_VISUAL == 1 | SYMPTOM_VISION_LD == 1)
		
		replace SEVERE_FEAT_TEST_EPIPAIN = 1 if GHYP_REVIEW==1  & ///
			(SYMPTOM_EPIPAIN_HOSP==1) 	
		
		replace SEVERE_FEAT_TEST_HEAD = 1 if GHYP_REVIEW==1  & ///
			(SYMPTOM_HEADACHE_HOSP==1 | SYMPTOM_HEADACHE_LD==1) 
			
			
	gen SEVERE_FEAT_TESTCOUNT = 0 if GHYP_REVIEW==1 
			
	foreach var of varlist SEVERE_FEAT_TEST_* {
		
	tab `var' 
	
	replace SEVERE_FEAT_TESTCOUNT = SEVERE_FEAT_TESTCOUNT + 1 if ///
		`var' == 1 
		
	}	
	
	tab SEVERE_FEAT_TESTCOUNT, m 
	
	drop SEVERE_FEAT_TEST* 
	
	* * * * Create a new outcome for Savannah's near-miss analysis:
		* HIGH_BP_SEVERE_ANY 
		* All participants who ever had severe high BP during pregnancy 
		* (regardless of HDP group)
		
	gen HIGH_BP_SEVERE_ANY = 55 if HDP_GROUP == 55 
	replace HIGH_BP_SEVERE_ANY = 0 if HDP_GROUP == 0 
	replace HIGH_BP_SEVERE_ANY = 1 if ///
		(HIGH_BP_SEV_COUNT_LESS20 >=1 & HIGH_BP_SEV_COUNT_LESS20 != .) | ///
		(HIGH_BP_SEV_COUNT >=1 & HIGH_BP_SEV_COUNT !=.) | ///
		HDP_SEVHYP_ANC == 1 | SEVERE_SEVHYP == 1 
	replace HIGH_BP_SEVERE_ANY = 0 if HIGH_BP_SEVERE_ANY == . & ///
		((HIGH_BP_SEV_COUNT_LESS20 ==0 | HIGH_BP_SEV_COUNT_LESS20 == .) & ///
		(HIGH_BP_SEV_COUNT ==0 | HIGH_BP_SEV_COUNT ==.) & ///
	(HDP_SEVHYP_ANC == 0 | HDP_SEVHYP_ANC == . | HDP_SEVHYP_ANC == 55) & ///
	(SEVERE_SEVHYP == 0 | SEVERE_SEVHYP == . | SEVERE_SEVHYP == 55))
	
	tab HIGH_BP_SEVERE_ANY, m 
	label var HIGH_BP_SEVERE_ANY "All who ever had severe high BP during preg (regardless of HDP group)"
	
	*If DX of severe high BP: 
	gen HIGH_BP_SEVERE_DX = 0 if HIGH_BP_SEVERE_ANY == 1 
	replace HIGH_BP_SEVERE_DX = 1 if HDP_SEVHYP_ANC == 1 | SEVERE_SEVHYP == 1
	
	label var HIGH_BP_SEVERE_DX "Among ever severe high BP: those with dx of severe high BP (checkbox)"

	gen HIGH_BP_SEVERE_MEAS = 0 if HIGH_BP_SEVERE_ANY == 1 
	replace HIGH_BP_SEVERE_MEAS = 1 if ///
		(HIGH_BP_SEV_COUNT_LESS20 >=1 & HIGH_BP_SEV_COUNT_LESS20 != .) | ///
		(HIGH_BP_SEV_COUNT >=1 & HIGH_BP_SEV_COUNT !=.)
		
	label var HIGH_BP_SEVERE_MEAS "Among ever severe high BP: those with measured severe high BP"
	
	foreach var of varlist HDP_GROUP HIGH_BP_SEV_COUNT HIGH_BP_SEV_COUNT_LESS20 ///
		HDP_SEVHYP_ANC SEVERE_SEVHYP GHYP_IND PREEC_IND PREEC_SUP_IND ///
		PREEC_SEV_IND SEVERE_FEAT_SEVHIGH SEVERE_FEAT_SEVHYP {
	
	tab `var' HIGH_BP_SEVERE_ANY, m 
	
		}
		
	
	/////////////////////////////////////////////////////////////////////
	
	* UPDATE ON JULY 22, 2024: INCORPORATE POSTPARTUM ECLAMPSIA DX: 
	* UPDATE - move code into the final file from its original place 
	* (mat_HDP_POSTPARTUM)
	
//////////////////////////////////////////////////////////////////////////

	preserve 
	
	* Examine diagnoses in the postpartum period: 
	
	* Hospitalizations in the postpartum period: 
	clear 
	import delimited "$da/mnh19_merged", bindquote(strict)
	
	rename momid MOMID 
	rename pregid PREGID 
	
	* merge in pregnancy endpoint data: 
	merge m:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing(PREG_END_DATE PREG_END)
		
		// restrict to completed pregnancies: 
		keep if PREG_END==1
		
		drop if _merge == 2 
		
		drop _merge 
	
	* restrict to hospitalizations in the postpartum period: 
		//create timing indicator: 
	gen HOSP_TIMING = m19_timing_ohocat
	label define hospt 1 "1-ANC" 2 "2-PNC" 77 "77-N/A"
	label values HOSP_TIMING hospt
	label var HOSP_TIMING "Timing of hospitalization"
	
		// create date indicator: 
	gen HOSP_DATE = date(m19_ohostdat, "YMD")
	replace HOSP_DATE = date(m19_mat_est_ohostdat, "YMD") if HOSP_DATE == . 
	replace HOSP_DATE = date(m19_admit_ohostdat, "YMD") if HOSP_DATE == . 
	
	replace HOSP_DATE = . if HOSP_DATE == date("07071907", "MDY")
	
	format HOSP_DATE %td 
	label var HOSP_DATE "Date of hospitalization"
	
	tab HOSP_DATE, m 
	
		// restrict: 
	keep if m19_timing_ohocat == 2 /// post-natal period timing OR 
		| (HOSP_DATE > PREG_END_DATE & HOSP_DATE != . & PREG_END_DATE != .) 
		// hospital date is after pregnancy end date 
		
		
	* Review DXs in the postpartum period: 
	
	* Severe symptoms: 
		
		gen SYMPTOM_HEADACHE = m19_headache_ceoccur
		label var SYMPTOM_HEADACHE "Severe headache symptom during postpartum hosp"
		tab SYMPTOM_HEADACHE, m 
		
		gen SYMPTOM_SEIZURE = m19_seizure_ceoccur
		label var SYMPTOM_SEIZURE "Seizure symptom during postpartum hosp"
		tab SYMPTOM_SEIZURE, m 
		
		gen SYMPTOM_EPIPAIN = m19_epigastr_pain_ceoccur 
		label var SYMPTOM_EPIPAIN "Severe epigastric pain symptom during postpartum hosp"
		tab SYMPTOM_EPIPAIN, m 
		
		gen SYMPTOM_VISION = m19_blur_vision_ceoccur 
		label var SYMPTOM_VISION "Blurred vision symptom during postpartum hosp"
		tab SYMPTOM_VISION, m 
		
	* HDP diagnosis checkboxes: 
	
		gen CHTN_PP = m19_hdp_htn_mhoccur_1 
		label var CHTN_PP "Chronic hypertension dx in postpartum"
		
		gen GHTN_PP = m19_hdp_htn_mhoccur_2
		label var GHTN_PP "Gestational hypertension dx in postpartum"
		
		gen PREEC_PP = m19_hdp_htn_mhoccur_3
		label var PREEC_PP "Preeclamspia dx in postpartum"
		
		foreach var of varlist CHTN_PP GHTN_PP PREEC_PP {
		
		replace `var' = 0 if `var' == 77 
		
		tab `var', m 
		
		}
		
	* Severe features checkboxes: 
	
		gen SEVERE_SEIZURE = m19_preeclampsia_ceoccur_1
		label var SEVERE_SEIZURE "Severe feature: seizure/eclampsia during postpartum hosp"
		tab SEVERE_SEIZURE, m 
	
		gen SEVERE_HELLP = m19_preeclampsia_ceoccur_2
		label var SEVERE_HELLP "Severe feature: HELLP during postpartum hosp"
		tab SEVERE_HELLP, m 
				
		gen SEVERE_SEVHYP = m19_preeclampsia_ceoccur_3 
		label var SEVERE_SEVHYP "Severe feature: severe hyper during postpartum hosp"
		tab SEVERE_SEVHYP, m 
		
		gen SEVERE_PULMED = m19_preeclampsia_ceoccur_4 
		label var SEVERE_PULMED "Severe feature: pulmonary edema during postpartum hosp"
		tab SEVERE_PULMED, m 
		
		gen SEVERE_VISION = m19_preeclampsia_ceoccur_5 
		label var SEVERE_VISION "Severe feature: vision changes during postpartum hosp"
		tab SEVERE_VISION, m 
		
	foreach var of varlist SEVERE_* {
	
	replace `var' = 0 if `var' == 77 
	
	}
	
	
	* Treatment for HDPs during hospitalization in the postpartum period: 
	tab m19_hpd_htn_cmoccur_1, m 
	
	gen HDP_TREAT_HOSP = 0  if m19_hpd_htn_cmoccur_1 == 1 | m19_hpd_htn_cmoccur_1 == 0 | ///
		m19_hpd_htn_cmoccur_77==1
	
	foreach num of numlist  3/10 {
	    
	replace HDP_TREAT_HOSP = 1 if m19_hpd_htn_cmoccur_`num' == 1 
	
	}
	
	label var HDP_TREAT_HOSP "Treated for HDP w/ a medication of interest during hospitalization"
	tab HDP_TREAT_HOSP, m 	
	
	* Primary DX: 
	
	gen PRIMARY_DX_CHTN = m19_htn_mhterm_1
	gen PRIMARY_DX_GHTN = m19_htn_mhterm_2
	gen PRIMARY_DX_PREEC = m19_htn_mhterm_3
	gen PRIMARY_DX_SEIZURES = m19_htn_mhterm_4
	gen PRIMARY_DX_HELLP = m19_htn_mhterm_5
	gen PRIMARY_DX_SEVHYP = m19_htn_mhterm_6
	gen PRIMARY_DX_PPEC = m19_htn_mhterm_7
	gen PRIMARY_DX_PULMED = m19_htn_mhterm_8
	gen PRIMARY_DX_VISUAL = m19_htn_mhterm_9
	gen PRIMARY_DX_STROKE = m19_htn_mhterm_10
	gen PRIMARY_DX_HDPOTHER = m19_htn_mhterm_88
	
	foreach var of varlist PRIMARY_DX_* {
	
	replace `var' = 0 if `var' == 77 
	
	tab `var', m 
	
	}
	
	*Check for Duplicates: 
	duplicates tag MOMID PREGID, gen(duplicate)
	tab duplicate, m 
	
	rename duplicate ENTRY_TOTAL 
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	
	*restrict to needed variables & reshape to wide 
	keep MOMID PREGID HOSP_DATE HOSP_TIMING PREG_END PREG_END_DATE ///
		SYMPTOM_* CHTN_PP GHTN_PP PREEC_PP SEVERE_* HDP_TREAT_HOSP ///
		PRIMARY_DX_* ENTRY_TOTAL 
		
	/// order file by person & date to order hospitalizations: 
	sort MOMID PREGID HOSP_DATE HOSP_TIMING 
	
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "Hospitalization Entry Number"
	
	tab ENTRY_NUM, m 
	
	reshape wide HOSP_DATE HOSP_TIMING SYMPTOM_* ///
		CHTN_PP GHTN_PP PREEC_PP SEVERE_* HDP_TREAT_HOSP PRIMARY_DX_*, ///
		i(MOMID PREGID ENTRY_TOTAL PREG_END PREG_END_DATE) j(ENTRY_NUM)
		
	sum ENTRY_TOTAL 
	return list 
	
	global i = r(max)
	
	foreach num of numlist 1/$i {
		
	label var HOSP_TIMING`num' "Hospitalization timing - postpartum event `num'"
	label var HOSP_DATE`num' "Hospitalization date - postpartum event `num'"
	
	label var HDP_TREAT_HOSP`num' "Treated for HDP w/ a medication of interest during hospitalization"
	label var SEVERE_VISION`num' "Severe feature: vision changes during postpartum hosp"
	label var SEVERE_PULMED`num' "Severe feature: pulmonary edema during postpartum hosp"
	label var SEVERE_SEVHYP`num' "Severe feature: severe hyper during postpartum hosp"
	label var SEVERE_HELLP`num' "Severe feature: HELLP during postpartum hosp"
	label var SEVERE_SEIZURE`num' "Severe feature: seizure/eclampsia during postpartum hosp"
	label var PREEC_PP`num' "Preeclamspia dx in postpartum"
	label var GHTN_PP`num' "Gestational hypertension dx in postpartum"
	label var CHTN_PP`num' "Chronic hypertension dx in postpartum"
	label var SYMPTOM_VISION`num' "Blurred vision symptom during postpartum hosp"
	label var SYMPTOM_EPIPAIN`num' "Severe epigastric pain symptom during postpartum hosp"
	label var SYMPTOM_SEIZURE`num' "Seizure symptom during postpartum hosp"
	label var SYMPTOM_HEADACHE`num' "Severe headache symptom during postpartum hosp"
	
	label var PRIMARY_DX_CHTN`num' "Primary DX: chronic hypertension"
	label var PRIMARY_DX_GHTN`num' "Primary DX: gestational hypertension"
	label var PRIMARY_DX_PREEC`num' "Primary DX: preeclamspia"
	label var PRIMARY_DX_SEIZURES`num' "Primary DX: eclampsia/seizures"
	label var PRIMARY_DX_HELLP`num' "Primary DX: HELLP"
	label var PRIMARY_DX_SEVHYP`num' "Primary DX: severe hypertension"
	label var PRIMARY_DX_PPEC`num' "Primary DX: postpartum eclampsia"
	label var PRIMARY_DX_PULMED`num' "Primary DX: pulmonary edema"
	label var PRIMARY_DX_VISUAL`num' "Primary DX: visual changes"
	label var PRIMARY_DX_STROKE`num' "Primary DX: stroke (HDP-related)"
	label var PRIMARY_DX_HDPOTHER`num' "Primary DX: Other HDP dx"
	
	}
	
	
	save "$wrk/HDP_hosp_postpartum", replace 
	
	
* * * Review symptoms & diagnoses at PNC: 

	clear 
	import delimited "$da/mnh12_merged", bindquote(strict)
	
	rename momid MOMID 
	rename pregid PREGID 
	
	* merge in pregnancy endpoint data: 
	merge m:1 MOMID PREGID using "$OUT/MAT_ENDPOINTS", keepusing(PREG_END_DATE PREG_END)	
	
	drop if _merge == 2 
	drop if PREG_END != 1 
	
	* type visit: 
	gen TYPE_VISIT = m12_type_visit 
	
	label define vistype 7 "7-PNC-0" 8 "8-PNC-1" 9 "9-PNC-4" 10 "10-PNC-6" ///
		11 "11-PNC-26" 12 "12-PNC-52" 14 "14-PNC-Non-scheduled"
		
	label values TYPE_VISIT vistype 
	
	label var TYPE_VISIT "Visit type - PNC"
	
	* visit date: 
	gen VISIT_DATE = date(m12_visit_obsstdat, "YMD") if ///
		m12_visit_obsstdat != "1907-07-07" & m12_visit_obsstdat != "1905-05-05"
	format VISIT_DATE %td 
	
	label var VISIT_DATE "Date of visit - PNC"
	
	gen VISIT_PP = VISIT_DATE - PREG_END_DATE if VISIT_DATE != . & PREG_END_DATE != . 
	label var VISIT_PP "Days postpartum at visit"
	
	sum VISIT_PP

		// minor cleaning: 
	replace VISIT_PP = . if VISIT_DATE < PREG_END_DATE & PREG_END_DATE != . 
	replace VISIT_DATE = . if VISIT_DATE < PREG_END_DATE & PREG_END_DATE != . 
		// fix date typo: 
	replace VISIT_DATE = date("20240322", "YMD") if VISIT_DATE == date("29240322", "YMD")
	replace VISIT_PP = VISIT_DATE - PREG_END_DATE if VISIT_DATE != . & PREG_END_DATE != . & ///
		VISIT_PP > 1000
		
	sum VISIT_PP
	
	sort TYPE_VISIT 
	
	by TYPE_VISIT: sum VISIT_PP

	* Severe symptoms: 
	gen SYMPTOM_SEIZURE = m12_seizure_ceoccur
	gen SYMPTOM_VISION = m12_blur_vision_ceoccur
	gen SYMPTOM_HEADACHE = m12_headache_ceoccur
	gen SYMPTOM_EPIPAIN = m12_epigastr_ceoccur
	
	label var SYMPTOM_SEIZURE "Symptom at PNC: Seizure"
	label var SYMPTOM_VISION "Symptom at PNC: Blurred vision"
	label var SYMPTOM_HEADACHE "Symptom at PNC: Severe headache"
	label var SYMPTOM_EPIPAIN "Symptom at PNC: Epigastric pain"
	
	rename SYMPTOM_* SYMPTOM_*_PNC
	
	*Birth complication dx: 
	
		// postpartum eclampsia
	gen POSTPART_DX_PPEC = m12_birth_compl_mhterm_2 
	tab POSTPART_DX_PPEC, m 
	
	
		// pulmonary edema 
	gen  POSTPART_DX_PULMED = m12_pulm_edema_mhoccur
	tab POSTPART_DX_PULMED, m 
	
	gen POSTPART_DX_PULMED_DATE = date(m12_pulm_edema_mhstdat, "YMD")
	format POSTPART_DX_PULMED_DATE %td 
		
	label var POSTPART_DX_PPEC "Postpartum dx: postpartum eclampsia"
	label var POSTPART_DX_PULMED "Postpartum dx: pulmonary edema"
	label var POSTPART_DX_PULMED_DATE "Date of pulmonary edema"
	
	 // Finalize dataset: 
	keep MOMID PREGID TYPE_VISIT VISIT_DATE VISIT_PP SYMPTOM_* ///
		POSTPART_DX_PPEC POSTPART_DX_PULMED POSTPART_DX_PULMED_DATE 
		
	// keep forms with relevant info: 
	keep if POSTPART_DX_PPEC == 1 | POSTPART_DX_PPEC == 0 | ///
		POSTPART_DX_PULMED == 1 | POSTPART_DX_PULMED == 0 | ///
		SYMPTOM_SEIZURE == 1 | SYMPTOM_SEIZURE == 0 

	
	// review number of observations per person: 
	sort MOMID PREGID VISIT_DATE TYPE_VISIT 
	
	duplicates tag MOMID PREGID, gen(duplicate)
	tab duplicate, m 
	
	rename duplicate ENTRY_TOTAL 
	replace ENTRY_TOTAL = ENTRY_TOTAL + 1 
	
	*reshape to wide 
		/// create indicator for number of entries per person: 
	quietly by MOMID PREGID :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "PNC Entry Number"
	
	tab ENTRY_NUM, m 
	
	reshape wide TYPE_VISIT VISIT_DATE VISIT_PP SYMPTOM_* POSTPART_DX_*, ///
		i(MOMID PREGID ENTRY_TOTAL) j(ENTRY_NUM)
		
	sum ENTRY_TOTAL 
	return list 
	
	global i = r(max)
	
	foreach num of numlist 1/$i {
		
	label var TYPE_VISIT`num' "PNC visit type"
	label var VISIT_DATE`num' "PNC visit date"
	label var VISIT_PP`num' "Days postpartum at PNC visit"
		
	label var SYMPTOM_SEIZURE_PNC`num' "Symptom at PNC: Seizure"
	label var SYMPTOM_VISION_PNC`num' "Symptom at PNC: Blurred vision"
	label var SYMPTOM_HEADACHE_PNC`num' "Symptom at PNC: Severe headache"
	label var SYMPTOM_EPIPAIN_PNC`num' "Symptom at PNC: Epigastric pain"
	
	label var POSTPART_DX_PPEC`num' "Postpartum dx: postpartum eclampsia"
	label var POSTPART_DX_PULMED`num' "Postpartum dx: pulmonary edema"
	label var POSTPART_DX_PULMED_DATE`num' "Date of pulmonary edema"	
	
	}
	
	
	save "$wrk/HDP_pnc", replace 	
	
	clear 
	
		///////////////////////////////////////////////////////////
			  * * * * Compile postpartum HDP Outcomes * * * * 
		///////////////////////////////////////////////////////////

* * * Start with completed pregnancies: 
	use "$OUT/MAT_ENDPOINTS"
	
	keep if PREG_END==1
	*drop _merge 
	
* * * Merge in post-L&D symptoms (as recorded in MNH10)
	
	merge 1:1 MOMID PREGID using "$wrk/HDP_mnh10", keepusing(VISIT_DATE SYMPTOM*)
	
	drop if _merge == 2 
	
	drop _merge 
		
	rename VISIT_DATE VISIT_DATE_PLD
	
* * * Merge in Hospitalizations 

	merge 1:1 MOMID PREGID using "$wrk/HDP_hosp_postpartum"
	
	drop _merge 
	
* * * Merge in PNC visits 

	merge 1:1 MOMID PREGID using "$wrk/HDP_pnc"
	
	drop _merge 
	
* * * Construct variables - postpartum HDPs: 

	// Diagnosis CHTN (from hospitalization only)
	gen POSTP_DX_CHTN = CHTN_PP1
	gen POSTP_DX_CHTN_DT = HOSP_DATE1 if CHTN_PP1==1
	
	replace POSTP_DX_CHTN = 1 if CHTN_PP2 == 1 
	replace POSTP_DX_CHTN_DT = HOSP_DATE2 if (CHTN_PP1 == 0 | CHTN_PP1 == .) ///
		& CHTN_PP2 == 1 
	format POSTP_DX_CHTN_DT %td
		
	gen POSTP_DX_CHTN_TIMING = 3 if CHTN_PP1==1 | CHTN_PP2 == 1 
	
	tab POSTP_DX_CHTN, m 
	
	// Diagnosis GHTN (from hospitalization only)
	gen POSTP_DX_GHTN = GHTN_PP1
	gen POSTP_DX_GHTN_DT = HOSP_DATE1 if GHTN_PP1==1
	
	replace POSTP_DX_GHTN = 1 if GHTN_PP2 == 1 
	replace POSTP_DX_GHTN_DT = HOSP_DATE2 if (GHTN_PP1 == 0 | GHTN_PP1 == .) ///
		& GHTN_PP2 == 1 	
	format POSTP_DX_GHTN_DT %td 
		
	gen POSTP_DX_GHTN_TIMING = 3 if GHTN_PP1 == 1 | GHTN_PP2 == 1 
	
	tab POSTP_DX_GHTN, m 
	
	// Diagnosis Preeclampsia (from hospitalization only)
	gen POSTP_DX_PREEC = PREEC_PP1
	gen POSTP_DX_PREEC_DT = HOSP_DATE1 if PREEC_PP1==1
	
	replace POSTP_DX_PREEC = 1 if PREEC_PP2 == 1 
	replace POSTP_DX_PREEC_DT = HOSP_DATE2 if (PREEC_PP1 == 0 | PREEC_PP1 == .) ///
		& PREEC_PP2 == 1 	
	format POSTP_DX_PREEC_DT %td 
		
	gen POSTP_DX_PREEC_TIMING = 3 if PREEC_PP1 == 1 | PREEC_PP2 == 1 	
	
	tab POSTP_DX_PREEC, m 
	
	// Diagnosis HELLP syndrome (from hospitalization only)
	gen POSTP_DX_HELLP = SEVERE_HELLP1
	replace POSTP_DX_HELLP = 1 if PRIMARY_DX_HELLP1==1
	gen POSTP_DX_HELLP_DT = HOSP_DATE1 if SEVERE_HELLP1==1 | PRIMARY_DX_HELLP1==1
	
	replace POSTP_DX_HELLP = 1 if SEVERE_HELLP2 == 1 | PRIMARY_DX_HELLP2 == 1
	replace POSTP_DX_HELLP_DT = HOSP_DATE2 if (SEVERE_HELLP1 == 0 | SEVERE_HELLP1 == .) ///
		& (PRIMARY_DX_HELLP1 == 0 | PRIMARY_DX_HELLP1 == .) & ///
		 (PRIMARY_DX_HELLP2 == 1 | SEVERE_HELLP2==1)
	format POSTP_DX_HELLP_DT %td 
		
	gen POSTP_DX_HELLP_TIMING = 3 if POSTP_DX_HELLP==1
	
	tab POSTP_DX_HELLP, m 
	
	// Diagnosis Severe Hypertension (from hospitalization only)
	gen POSTP_DX_SEVHYP = SEVERE_SEVHYP1
	replace POSTP_DX_SEVHYP = 1 if PRIMARY_DX_SEVHYP1==1
	gen POSTP_DX_SEVHYP_DT = HOSP_DATE1 if SEVERE_SEVHYP1==1 | PRIMARY_DX_SEVHYP1==1
	
	replace POSTP_DX_SEVHYP = 1 if SEVERE_SEVHYP2 == 1 | PRIMARY_DX_SEVHYP2 == 1
	replace POSTP_DX_SEVHYP_DT = HOSP_DATE2 if (SEVERE_SEVHYP1 == 0 | SEVERE_SEVHYP1 == .) ///
		& (PRIMARY_DX_SEVHYP1 == 0 | PRIMARY_DX_SEVHYP1 == .) & ///
		 (PRIMARY_DX_SEVHYP2 == 1 | SEVERE_SEVHYP2==1)
	format POSTP_DX_SEVHYP_DT %td 
		
	gen POSTP_DX_SEVHYP_TIMING = 3 if POSTP_DX_SEVHYP==1
	
	tab POSTP_DX_SEVHYP, m 
	
	
	// PE with severe features (visual changes, severe epi pain, severe headache, PE)
	*(hospital only)
	gen POSTP_DX_PREECSEV = 1 if ///
		PREEC_PP1 == 1 & ///
		(SYMPTOM_VISION1==1 | SEVERE_VISION1==1 | PRIMARY_DX_VISUAL1==1 | ///
		 SYMPTOM_EPIPAIN1==1 | SYMPTOM_HEADACHE1==1 | SEVERE_PULMED1==1)
	replace POSTP_DX_PREECSEV = 0 if PREEC_PP1 == 0 | ///
		(PREEC_PP1!= 1 & SYMPTOM_VISION1!=1 & SEVERE_VISION1!=1 & PRIMARY_DX_VISUAL1!=1 & ///
		 SYMPTOM_EPIPAIN1!=1 & SYMPTOM_HEADACHE1!=1 & SEVERE_PULMED1!=1)
	
	gen POSTP_DX_PREECSEV_DT = HOSP_DATE1 if POSTP_DX_PREECSEV == 1
	
	replace POSTP_DX_PREECSEV = 1 if PREEC_PP2 == 1 & ///
		(SYMPTOM_VISION2==1 | SEVERE_VISION2==1 | PRIMARY_DX_VISUAL2==1 | ///
		 SYMPTOM_EPIPAIN2==1 | SYMPTOM_HEADACHE2==1 | SEVERE_PULMED2==1)
		
	replace POSTP_DX_PREECSEV_DT = HOSP_DATE2 if POSTP_DX_PREECSEV_DT == . & ///
		PREEC_PP2 == 1 & (SYMPTOM_VISION2==1 | SEVERE_VISION2==1 | PRIMARY_DX_VISUAL2==1 | ///
		 SYMPTOM_EPIPAIN2==1 | SYMPTOM_HEADACHE2==1 | SEVERE_PULMED2==1)
	
	format POSTP_DX_PREECSEV_DT %td 
		
	gen POSTP_DX_PREECSEV_TIMING = 3 if POSTP_DX_PREECSEV==1
	
	tab POSTP_DX_PREECSEV, m 
	 

	// Diagnosis: postpartum eclampsia (from PNC OR Hospitalization)
	gen POSTP_DX_PPEC = POSTPART_DX_PPEC1
	
	gen POSTP_DX_PPEC_DT = VISIT_DATE1 if POSTPART_DX_PPEC1 == 1 
	format POSTP_DX_PPEC_DT %td 
	
	gen POSTP_DX_PPEC_TIMING = 2 if POSTPART_DX_PPEC1 == 1
	
		// review subsequent PNC forms: 
	foreach num of numlist 2/6 {
		
	replace POSTP_DX_PPEC = 0 if POSTPART_DX_PPEC`num' == 0 & ///
		(POSTP_DX_PPEC == 55 | POSTP_DX_PPEC == 77 | POSTP_DX_PPEC ==.)
		
	replace POSTP_DX_PPEC = 1 if POSTPART_DX_PPEC`num' == 1
		
	replace POSTP_DX_PPEC_DT = VISIT_DATE`num' if POSTPART_DX_PPEC`num' == 1 & ///
		POSTP_DX_PPEC_DT == .
		
	replace POSTP_DX_PPEC_TIMING = 2 if POSTPART_DX_PPEC`num' == 1 & ///
		POSTP_DX_PPEC_TIMING == . 
	
	}
	
		//Hospitalizations: 
	replace POSTP_DX_PPEC = 1 if PRIMARY_DX_PPEC1 == 1 | PRIMARY_DX_PPEC2 == 1 
	
	replace POSTP_DX_PPEC_TIMING = 3 if (PRIMARY_DX_PPEC1 == 1 | PRIMARY_DX_PPEC2 == 1) & ///
		POSTP_DX_PPEC_TIMING == . 
	
	replace POSTP_DX_PPEC_DT = HOSP_DATE1 if PRIMARY_DX_PPEC1 == 1 & ///
		POSTP_DX_PPEC_DT == . 
		
	replace POSTP_DX_PPEC_DT = HOSP_DATE2 if PRIMARY_DX_PPEC2 == 1 & ///
		POSTP_DX_PPEC_DT == . 

		tab POSTP_DX_PPEC, m 
		tab POSTP_DX_PPEC_TIMING, m 

	// Severe features: Seizures in the postpartum period 
	gen POSTP_SEVERE_FEAT_SEIZURE = SYMPTOM_SEIZURE_PLD
	
	gen POSTP_SEVERE_FEAT_SEIZURE_TIMING = 1 if SYMPTOM_SEIZURE_PLD == 1 
	
	gen POSTP_SEVERE_FEAT_SEIZURE_DT = VISIT_DATE_PLD if SYMPTOM_SEIZURE_PLD == 1 
	format POSTP_SEVERE_FEAT_SEIZURE_DT %td
	
		// during PNC visit(s): 
	foreach num of numlist 1/6 {
	
	replace POSTP_SEVERE_FEAT_SEIZURE = 0 if SYMPTOM_SEIZURE_PNC`num' == 0 & ///
		(POSTP_SEVERE_FEAT_SEIZURE == . | POSTP_SEVERE_FEAT_SEIZURE == 55 | ///
		 POSTP_SEVERE_FEAT_SEIZURE == 77) 
		 
	replace POSTP_SEVERE_FEAT_SEIZURE = 1 if SYMPTOM_SEIZURE_PNC`num' == 1 
	
	replace POSTP_SEVERE_FEAT_SEIZURE_TIMING = 2 if SYMPTOM_SEIZURE_PNC`num' == 1 & ///
		POSTP_SEVERE_FEAT_SEIZURE_TIMING == . 
	
	replace POSTP_SEVERE_FEAT_SEIZURE_DT = VISIT_DATE`num' if SYMPTOM_SEIZURE_PNC`num' == 1 & ///
		POSTP_SEVERE_FEAT_SEIZURE_DT == . 
	
	}
	
		// during hospitalization: 
	foreach num of numlist 1/2 {
		
	replace POSTP_SEVERE_FEAT_SEIZURE = 1 if SYMPTOM_SEIZURE`num' == 1 | ///
		SEVERE_SEIZURE`num' == 1 | PRIMARY_DX_SEIZURES`num' == 1 
		
	replace POSTP_SEVERE_FEAT_SEIZURE_TIMING = 3 if (SYMPTOM_SEIZURE`num' == 1 | ///
		SEVERE_SEIZURE`num' == 1 | PRIMARY_DX_SEIZURES`num' == 1) & ///
		POSTP_SEVERE_FEAT_SEIZURE_TIMING == . 
		
	replace POSTP_SEVERE_FEAT_SEIZURE_DT = HOSP_DATE`num' if ///
		(SYMPTOM_SEIZURE`num' == 1 | ///
		SEVERE_SEIZURE`num' == 1 | PRIMARY_DX_SEIZURES`num' == 1) & ///
		POSTP_SEVERE_FEAT_SEIZURE_DT == .  
	
	}
	
	tab POSTP_SEVERE_FEAT_SEIZURE, m 
	tab POSTP_SEVERE_FEAT_SEIZURE_TIMING, m 
	
	
	foreach var of varlist POSTP_DX_CHTN POSTP_DX_GHTN POSTP_DX_PREEC ///
		POSTP_DX_PPEC POSTP_SEVERE_FEAT_SEIZURE POSTP_DX_HELLP ///
		POSTP_DX_PREECSEV POSTP_DX_SEVHYP {
	
	gen `var'_PP = `var'_DT - PREG_END_DATE if `var'_DT != . & ///
		PREG_END_DATE != . & `var' == 1 
		
	tab `var'_PP `var', m 
	
		}
	
			
	* Create a constructed outcome - severe dx in the postpartum period: 
	
		// postpartum HELLP dx 
	gen HDP_GROUP_POSTP = 6 if POSTP_DX_HELLP == 1 & ///
		POSTP_DX_HELLP_PP > 0 & POSTP_DX_HELLP_PP <=42
	
		// postpartum eclampsia dx
	replace HDP_GROUP_POSTP = 5 if POSTP_DX_PPEC == 1 & ///
		POSTP_DX_PPEC_PP > 0 & POSTP_DX_PPEC_PP <=42
	
		// postpartum seizures 
	replace HDP_GROUP_POSTP = 4 if POSTP_SEVERE_FEAT_SEIZURE == 1  & ///
		HDP_GROUP_POSTP == . & ///
		POSTP_SEVERE_FEAT_SEIZURE_PP > 0 & POSTP_SEVERE_FEAT_SEIZURE_PP <=42
		
		// PE with severe features (related to HTN) AND preelcampsia dx
	replace HDP_GROUP_POSTP = 3 if POSTP_DX_PREECSEV == 1 & ///
		POSTP_DX_PREECSEV_PP >0 & POSTP_DX_PREECSEV_PP <=42
	
		// dx of severe HTN
	replace HDP_GROUP_POSTP = 2 if POSTP_DX_SEVHYP == 1 & HDP_GROUP_POSTP == . & ///
		POSTP_DX_SEVHYP_PP >0 & POSTP_DX_SEVHYP_PP <=42
		
		// postpartum preeclampsia dx only
	replace HDP_GROUP_POSTP = 1 if POSTP_DX_PREEC == 1 & HDP_GROUP_POSTP == . & ///
		POSTP_DX_PREEC_PP >0 & POSTP_DX_PREEC_PP <=42
		
	tab HDP_GROUP_POSTP, m 
	
	label define hdppost 1 "1-Preeclampsia (postpartum dx)" ///
		2 "2-Severe HTN (dx)" 3 "3-Preeclampsia with severe features (postpartum dx)" ///
		4 "4-Postpartum seizures" 5 "5-Postpartum eclampsia (dx)" ///
		6 "6-HELLP (postpartum dx)"
		
	label var HDP_GROUP_POSTP "Severe HTN diagnoses in the postpartum period (1-42 days)"
		
	label values HDP_GROUP_POSTP hdppost
	
	tab HDP_GROUP_POSTP, m 
	
	
	keep MOMID PREGID HDP_GROUP_POSTP POSTP_DX_HELLP POSTP_DX_HELLP_PP ///
		POSTP_DX_HELLP_DT POSTP_DX_HELLP_TIMING ///
		POSTP_DX_PPEC POSTP_DX_PPEC_PP POSTP_DX_PPEC_DT POSTP_DX_PPEC_TIMING ///
		POSTP_SEVERE_FEAT_SEIZURE POSTP_SEVERE_FEAT_SEIZURE_PP ///
		POSTP_SEVERE_FEAT_SEIZURE_DT POSTP_SEVERE_FEAT_SEIZURE_TIMING ///
		POSTP_DX_PREECSEV POSTP_DX_PREECSEV_PP POSTP_DX_PREECSEV_DT ///
		POSTP_DX_PREECSEV_TIMING POSTP_DX_SEVHYP POSTP_DX_SEVHYP_PP ///
		POSTP_DX_SEVHYP_DT POSTP_DX_SEVHYP_TIMING POSTP_DX_PREEC ///
		POSTP_DX_PREEC_PP POSTP_DX_PREEC_DT POSTP_DX_PREEC_TIMING
		
	label define timingpp 3 "Hospital form" 2 "PNC form" 1 "Post-L&D form"
	label values *_TIMING timingpp
	
	foreach var of varlist *_TIMING {
	tab `var', m 
	}
		
	save "$wrk/mat_postpartum_HTN_dx", replace 
	
	restore 
	
/////////////////////////////////////////////////////////////

	* * * Return to main file & merge in postpartum dx * * * 
	
////////////////////////////////////////////////////////////

	merge 1:1 MOMID PREGID using "$wrk/mat_postpartum_HTN_dx"
	
	drop if _merge == 2 
	
	drop _merge 
	
	tab HDP_GROUP_POSTP HDP_GROUP, m 
	
		*review cases to be reassigned: 
		list SITE HDP_GROUP HDP_GROUP_POSTP ///
			POSTP_DX_PPEC POSTP_DX_PPEC_PP POSTP_DX_PPEC_TIMING ///
			POSTP_SEVERE_FEAT_SEIZURE POSTP_SEVERE_FEAT_SEIZURE_PP ///
			POSTP_SEVERE_FEAT_SEIZURE_TIMING if ///
			HDP_GROUP==0 & HDP_GROUP_POSTP != . 
			
		* UNSURE IF WE SHOULD REASSIGN FOR PP SEIZURES. TO REVIEW. FOR NOW, 
		* WE WILL STICK WITH POSTPARTUM ECLAMPSIA DXes DURING 42 DAY POSTPARTUM 
		* PERIOD.
		
		* UPDATE: As of October 29, 2024, we will re-assign for postpartum 
		* seizures within the first week after pregnancy outcome (1-7 days
		* postpartum). 

	
	// Assign to preeclampsia with severe features if postpartum eclampsia: 
	replace HDP_GROUP = 5 if POSTP_DX_PPEC ==1 & ///
		(POSTP_DX_PPEC_PP > 0 & POSTP_DX_PPEC_PP <= 42)
		
	replace HDP_GROUP_MISS = . if HDP_GROUP==5 & POSTP_DX_PPEC ==1 & ///
		(POSTP_DX_PPEC_PP > 0 & POSTP_DX_PPEC_PP <= 42)
		
	// Assign to preeclampsia with severe features if seizures reported within 7 days of pregnancy outcome: 
	replace HDP_GROUP = 5 if POSTP_SEVERE_FEAT_SEIZURE ==1 & ///
		(POSTP_SEVERE_FEAT_SEIZURE_PP > 0 & POSTP_SEVERE_FEAT_SEIZURE_PP <= 7)
		
	replace HDP_GROUP_MISS = . if HDP_GROUP==5 & POSTP_SEVERE_FEAT_SEIZURE ==1 & ///
		(POSTP_SEVERE_FEAT_SEIZURE_PP > 0 & POSTP_SEVERE_FEAT_SEIZURE_PP <= 7)
	
	tab POSTP_DX_PPEC SITE, m 
	
	tab POSTP_SEVERE_FEAT_SEIZURE SITE, m 
	
	*indicator for postpartum eclampsia: 
	gen SEVERE_FEAT_POSTP_ECLAMPSIA = 0 if HDP_GROUP == 5 
	replace SEVERE_FEAT_POSTP_ECLAMPSIA = 1 if HDP_GROUP == 5 & POSTP_DX_PPEC == 1 
	
	label var SEVERE_FEAT_POSTP_ECLAMPSIA "Severe features: Postpartum eclampsia"
	
	*indicator for postpartum seizures: 
	gen SEVERE_FEAT_POSTP_SEIZURES = 0 if HDP_GROUP == 5 
	replace SEVERE_FEAT_POSTP_SEIZURES = 1 if HDP_GROUP == 5 & ///
		POSTP_SEVERE_FEAT_SEIZURE ==1 & ///
		(POSTP_SEVERE_FEAT_SEIZURE_PP > 0 & POSTP_SEVERE_FEAT_SEIZURE_PP <= 7)
	
	label var SEVERE_FEAT_POSTP_SEIZURES "Severe features: Postpartum seizures (within 1 week of delivery)"

	*set indications (mutually exclusive) for new cases identified as PE with severe features: 
	replace SEVERE_ANY = 1 if SEVERE_FEAT_POSTP_ECLAMPSIA == 1 
	replace PREEC_SEV_IND = 10 if SEVERE_FEAT_POSTP_ECLAMPSIA == 1 & PREEC_SEV_IND == .
	
	replace SEVERE_ANY = 1 if POSTP_SEVERE_FEAT_SEIZURE ==1 & ///
		(POSTP_SEVERE_FEAT_SEIZURE_PP > 0 & POSTP_SEVERE_FEAT_SEIZURE_PP <= 7) 
	replace PREEC_SEV_IND = 11 if PREEC_SEV_IND == . & POSTP_SEVERE_FEAT_SEIZURE ==1 & ///
		(POSTP_SEVERE_FEAT_SEIZURE_PP > 0 & POSTP_SEVERE_FEAT_SEIZURE_PP <= 7)
	
	*update indication label: 
	label define severeind2 1 "1-Dx Eclampsia/Seizures" 2 "2-Dx HELLP" ///
			3 "3-GH/PE w/ organ dysf" 4 "4-Severe BP w/ Prot" ///
			5 "5-Dx severe BP w/ Prot" 6 "6-PE w/ pulm edema" ///
			7 "7-PE w/ visual" 8 "8-PE w/ epig pain" ///
			9 "9-PE w/ severe headache" ///
			10 "10-Postpartum eclampsia" ///
			11 "11-Seizures within 7 days postpartum"
			
	label values PREEC_SEV_IND severeind2
	
	tab PREEC_SEV_IND, m 
	
		// update indicators for preeclampsia & severe preeclampsia to include 
		// postpartum eclampsia / postpartum seizure cases: 
		
	replace PREECLAMPSIA = 1 if  SEVERE_FEAT_POSTP_ECLAMPSIA==1
	replace PREECLAMPSIA_SEV = 1 if SEVERE_FEAT_POSTP_ECLAMPSIA==1
	
	replace PREECLAMPSIA = 1 if POSTP_SEVERE_FEAT_SEIZURE ==1 & ///
		(POSTP_SEVERE_FEAT_SEIZURE_PP > 0 & POSTP_SEVERE_FEAT_SEIZURE_PP <= 7) 
	replace PREECLAMPSIA_SEV = 1 if POSTP_SEVERE_FEAT_SEIZURE ==1 & ///
		(POSTP_SEVERE_FEAT_SEIZURE_PP > 0 & POSTP_SEVERE_FEAT_SEIZURE_PP <= 7)
	
	
	*finalize dataset: 
	
	label var HDP_GROUP "Final status: HDPs at ANC through delivery & postpartum eclampsia"
	
		label var GHYP_IND "Qualification for gestational hypertension"
		label var PREEC_IND "Qualification for preeclampsia"
		label var PREEC_SUP_IND "Qualification for preeclampsia superimposed on HTN"
		label var PREEC_SEV_IND "Qualification for preeclampsia with severe features"
		
	* * * RENAMING FOR VARIABLE CONVENTION: 
	rename PREG_END HDP_DENOM 
	label var HDP_DENOM "Denominator for HDP outcomes: Completed pregnancies (with MNH09 completed OR loss in MNH04/19)"
	
	rename	SEVERE_FEAT_SEIZURES	PREEC_SEV_FEAT_SEIZURES
	rename	SEVERE_FEAT_HELLP	PREEC_SEV_FEAT_HELLP
	rename	SEVERE_FEAT_ORGAN	PREEC_SEV_FEAT_ORGAN
	rename	SEVERE_FEAT_SEVHIGH	PREEC_SEV_FEAT_SEVHIGH
	rename	SEVERE_FEAT_SEVHYP	PREEC_SEV_FEAT_SEVHYP
	rename	SEVERE_FEAT_PULMED	PREEC_SEV_FEAT_PULMED
	rename	SEVERE_FEAT_VISUAL	PREEC_SEV_FEAT_VISUAL
	rename	SEVERE_FEAT_EPIPAIN	PREEC_SEV_FEAT_EPIPAIN
	rename	SEVERE_FEAT_HEAD	PREEC_SEV_FEAT_HEAD
	rename	SEVERE_FEAT_POSTP_ECLAMPSIA	PREEC_SEV_FEAT_POSTP_ECLAMPSIA
	rename	SEVERE_FEAT_POSTP_SEIZURES	PREEC_SEV_FEAT_POSTP_SEIZURES
	
	* * * Create denominators for GHTN; PREECLAMPSIA; PREECLAMPSIA_SEV: 
	gen GHTN_DENOM = 0 if HTN_ANY == 1 | HDP_DENOM==0 | HDP_DENOM==. | ///
		(PREG_END_GA >=0 & PREG_END_GA < 140 & GHTN!=1)
	
	replace GHTN_DENOM = 1 if GHTN_DENOM != 0 & HDP_DENOM == 1 
	
	tab GHTN_DENOM GHTN, m 
	
	gen PREECLAMPSIA_DENOM = 0 if HDP_DENOM==0 | HDP_DENOM==. | ///
		(PREG_END_GA >=0 & PREG_END_GA < 140 & PREECLAMPSIA !=1)
		
	replace PREECLAMPSIA_DENOM = 1 if PREECLAMPSIA_DENOM != 0 & HDP_DENOM==1
	
	tab PREECLAMPSIA_DENOM PREECLAMPSIA, m 
	
	gen PREECLAMPSIA_SEV_DENOM = 0 if HDP_DENOM==0 | HDP_DENOM==. | ///
		(PREG_END_GA >=0 & PREG_END_GA < 140 & PREECLAMPSIA_SEV!=1) 
		
	replace PREECLAMPSIA_SEV_DENOM = 1 if PREECLAMPSIA_SEV_DENOM != 0 & HDP_DENOM==1
	
	tab PREECLAMPSIA_SEV_DENOM PREECLAMPSIA_SEV, m 
	
	
	save "$wrk/HDP_final", replace 
	
	tab HDP_GROUP HTN_ANY, m 
	tab HDP_GROUP GHTN, m 
	tab HDP_GROUP PREECLAMPSIA, m 
	tab HDP_GROUP PREECLAMPSIA_SEV, m 
	

	
	* save for report outcomes:
	keep SITE MOMID PREGID HTN_ANY HTN_ANY_IND ///
		ENTRY_TOTAL HIGH_BP_COUNT HIGH_BP_SEV_COUNT ///
		BP_COUNT_PRISMA BP_COUNT_IPC BP_COUNT_HOSP UA_PROT_LBORRES UA_PROT_TESTTYPE ///
		UA_PROT_DATE UA_PROT_GA UA_PROT_PRIOR20_COUNT UA_PROT_PRIOR20_LBORRES ///
		UA_PROT_PRIOR20_TESTTYPE UA_PROT_PRIOR20_DATE UA_PROT_PRIOR20_GA ///
		HDP_GROUP GHYP_IND PREEC_IND PREEC_SUP_IND PREEC_SEV_IND ///
		HDP_GROUP_MISS PREEC_SEV_FEAT_* HDP_DENOM HIGH_BP_SEVERE* ///
		GHTN PREECLAMPSIA PREECLAMPSIA_SEV GHTN_DENOM PREECLAMPSIA_DENOM ///
		PREECLAMPSIA_SEV_DENOM 
		
	tab 
	
	*review missing issue: 
	tab HDP_GROUP HDP_GROUP_MISS if SITE == "Ghana", m 
	
	save "$OUT/MAT_HDP", replace 
	
		foreach var of varlist GHTN PREECLAMPSIA PREECLAMPSIA_SEV {
		
		tab HDP_GROUP `var', m 
			
		}
	
	/*for codebook: 
	preserve 
		describe, replace clear
		list
		export excel using "$wrk/hdp_vars.xls", replace first(var)
	restore
	*/
	
	
	
