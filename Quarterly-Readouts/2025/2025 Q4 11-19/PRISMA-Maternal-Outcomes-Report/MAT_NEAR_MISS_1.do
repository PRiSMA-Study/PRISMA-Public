**PRISMA maternal outcome: Near miss**
***Adapted maternal near-miss

**By Savannah O'Malley (savannah.omalley@gwu.edu)
**Begun May 17, 2024

**# Directories and paths

	*Change the below based on data date
	global datadate "2025-11-14"
		*must format as CCYY-MM-DD to match stacked dataset format

	local date: di %td_CCYY_Mon_DD daily("`c(current_date)'", "DMY")
	global today = subinstr(strltrim("`date'"), " ", "-", .)
	disp "$today"
		*format today as CCYY-Mon-DD
	
	global da "Z:\Stacked Data/$datadate"
	global data "D:\Users\savannah.omalley\Documents\data/$datadate"
	global outcomes "Z:\Outcome Data/$datadate"
	
	
	global wrk "D:\Users\savannah.omalley\Documents\near_miss/$datadate"
		cap mkdir "$wrk"
	cd "$wrk"
	
	global queries "$wrk/queries"
		cap mkdir "$queries"
	
	global runquery 1 //logical: run query codes?


/*
**Stacked data needed: MNH04, MNH09, MNH10, MNH12, MNH19

**!This file also assumes you have imported the CSVs and converted to stata format
**file 'dataimport.ado' will do this for you with specified data upload date

**Note: this do file needs the following constructed files:
"$outcomes/MAT_ENROLL.dta" 
"$outcomes/MAT_ENDPOINTS.dta"
"$outcomes/MAT_UTERINERUP.dta"
"$outcomes/MAT_HEMORRHAGE.csv"
"$outcomes/MAT_INFECTION.csv"
"$outcomes/MAT_LABOR.dta"
"$outcomes/MAT_ANEMIA.dta"
*/

**#Outcomes included	
/*
*******Near miss definition*******
1. A woman who is pregnant, in labor, or within 42 days postpartum 

	AND

2. Experienced the PRISMA-near-miss criteria: 
	- organ dysfunction [MNH09, MNH12, MNH19]
	- blood transfusion of any volume [MNH19; or transfusion to treat PPH] 
		AND the woman experienced any of the following: 
		- antepartum hemorrhage [MAT_HEMORRHAGE]
		- severe postpartum hemorrhage [MAT_HEMORRHAGE]
		- severe anemia [MAT_ANEMIA]
		- placental abruption [MNH09]
	- preeclampsia with severe features/eclampsia [MAT_HDP]
	- uterine rupture [MAT_UTER_RUP]
	- sepsis [under construction]
	- hysterectomy [MNH09, MNH12, MNH19]
	- laparotomy other than C-section [MNH19, or hysterectomy],
	- admission to ICU [MNH10]

	AND

3. Survives the experience


******PLTC********

This file also constructs potentially life-threatening conditions (PLTC)
	PLTC definition:
	- Any near miss (above)
	- Any hospitalization [recorded in MNH19, reported in MNH04 & MNH12]
	- Postpartum hemorrhage [MAT_HEMORRHAGE]
	- Severe hypertension [MAT_HDP]
	- Pulmonary edema [MNH09, MNH12, MNH19]
	- Blood transfusion of any volume [needed - MNH09, received - MNH19]
	- Abortion complications [MNH19]
	- Obstructured labor [MAT_LABOR]
	- Prolonged labor [MAT_LABOR]
	- (suspected) Placenta accrete [MNH09]
	- (suspected) Placenta abruption [MNH09]
	- Endometritis [MNH10, MNH19]
	- Malaria [MAT_INFECTION]
	- Tuberculosis [MAT_INFECTION]
	

*/

**# MNH04 - ANC clinical status
	
	*This form collects maternal report of hospitalization
	*generate a long and wide file of those who reported hospitalization
	use "$data/mnh04",  clear

	rename M04_* *
	

	**#Hospitalization - maternal recall
	tab HOSP_OHOOCCUR, m // admitted to a health facility since last visit
	gen HOSPITALIZED = 1 if HOSP_OHOOCCUR == 1
	tab HOSP_OHOREAS //specify reason 
	gen HOSP_REASON = HOSP_OHOREAS
	replace HOSP_REASON = strtrim(HOSP_REASON)
		*trim leading and trailing spaces
	replace HOSP_REASON = "" if ///
	regexm(HOSP_OHOREAS, "n/a") | regexm(HOSP_OHOREAS,"55") | regexm(HOSP_OHOREAS,"77")
	
	str2date HOSP_OHOSTDAT

	*keep only if woman reports hospitalization
	keep if HOSPITALIZED == 1
	
	*Query 1: no estimated hospital date
	gen query_note = "Please provide hospital date" if HOSP_OHOSTDAT==. & HOSPITALIZED == 1 
	
	*Query 2: hospitalization date before preg start date
	merge m:1 SITE MOMID PREGID using "$outcomes/MAT_ENROLL", ///
		keepusing(PREG_START_DATE)
	drop if _merge == 2 //enroll only without a hospitalization report
	
	str2date PREG_START_DATE	
	replace query_note = "Hosp before preg start date" if ///
	HOSP_OHOSTDAT<PREG_START_DATE & HOSP_OHOSTDAT!=. & PREG_START_DATE!=.
	
	
	*Export site-specific hospital date queries:
	if $runquery == 1 {
		
		levelsof(SITE) if query_note!="" , local(sitelev) clean
			*which sites have queries?
		foreach site of local sitelev {
			*export site-specific queries
		
		export excel SITE MOMID PREGID query_note TYPE_VISIT ANC_OBSSTDAT HOSP_OHOOCCUR HOSP_OHOSTDAT PREG_START_DATE using "$queries/`site'-Near-Miss-$datadate.xlsx" if SITE=="`site'" & query_note!="" , sheet("MNH04-hosp-queries", modify) firstrow(variables)  datestring("%tdDD-Mon-CCYY")
			*datestring specifies date format for export
		
		disp as result "`site' Completed"
		
		}
		
	}
	
	
	
	*Save a long data set:
	keep SITE MOMID PREGID HOSPITALIZED HOSP_OHOOCCUR HOSP_REASON HOSP_OHOSTDAT  ANC_OBSSTDAT PREG_START_DATE  
	sort SITE MOMID PREGID ANC_OBSSTDAT
	
	
	
	save "$wrk/mnh04_long.dta", replace
	
	
	*save a dataset without duplicates (for later queries)
	preserve 
		
		gen HOSP_DATE = HOSP_OHOSTDAT
		drop if HOSP_DATE == . 
		drop if HOSP_DATE < PREG_START_DATE
		
		keep SITE MOMID PREGID HOSP_DATE HOSP_OHOSTDAT ANC_OBSSTDAT
		rename HOSP_OHOSTDAT M04_HOSP_OHOSTDAT
		duplicates drop 
		*if the only difference is observation date, drop the second observation date
		bysort SITE MOMID PREGID HOSP_DATE ( ANC_OBSSTDAT): gen totalhosp = _N
		bysort SITE MOMID PREGID HOSP_DATE ( ANC_OBSSTDAT): gen hospnum = _n
		keep if hospnum == 1
		drop totalhosp hospnum
		isid SITE MOMID PREGID HOSP_DATE
		save "$wrk/mnh04_nodups.dta", replace
	restore
	
	*Next: save a wide dataset 
		*to calculate yes/no hospitalized, by PREGID
	
	*generate # visits to the hospital
	by MOMID PREGID (HOSPITALIZED ANC_OBSSTDAT) , sort: gen hospnum = _n 
	tab hospnum, m
	
	*reshape wide based on # visits to the hospital
	reshape wide HOSPITALIZED HOSP_REASON HOSP_OHOSTDAT  ANC_OBSSTDAT , i(SITE MOMID PREGID) j(hospnum)
		*Note that HOSP_OHOENDAT1 will always be the earliest recorded hospitalization date
	
	egen hospnum=rowtotal(HOSPITALIZED*)
	tab hospnum,m
	
	rename * M04_*
	rename (M04_SITE M04_MOMID M04_PREGID) (SITE MOMID PREGID)
	*Generate final indicator variable that this participant visited hospital
	*Used to construct the "unplanned hospitalization" variable (PLTC)
	gen M04_HOSP = 1
	save "$wrk/mnh04_wide.dta", replace
	

	

**# MNH09 - Labor and Delivery		
	/*
	This form used to calculate several outcomes:
	- organ dysfunction/failure
	- pulmonary edema
	- placenta accrete
	- placental abruption
	- hysterectomy
	- indicator that a woman has an MNH09 (for denominator)
	
	Note:should be only one MNH09 per participant, 
		no need for collapse or reshape
	*/
	use "$data/mnh09",  clear

	rename M09_* *
	
	**#Organ dysfunction/failure
	tab1 ORG_FAIL_MHOCCUR_1 ORG_FAIL_MHOCCUR_2 ORG_FAIL_MHOCCUR_3 ORG_FAIL_MHOCCUR_4 ORG_FAIL_MHOCCUR_5 ORG_FAIL_MHOCCUR_6 ORG_FAIL_MHOCCUR_7 ORG_FAIL_MHOCCUR_77 ORG_FAIL_MHOCCUR_88 ORG_FAIL_MHOCCUR_99 ORG_FAIL_SPFY_MHTERM
	
	*Rename for interpretability:
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
	
	tab1 ORG_FAIL_HRT_M09 ORG_FAIL_RESP_M09 ORG_FAIL_RENAL_M09 ORG_FAIL_LIVER_M09 ORG_FAIL_NEUR_M09 ORG_FAIL_UTER_M09 ORG_FAIL_HEM_M09

	*Indicator for if any organ dysfunction/failure was reported in MNH09
	gen ORG_FAIL_M09 = 0 
	replace ORG_FAIL_M09 = 1 if ///
		ORG_FAIL_MHOCCUR_1 == 1 | ORG_FAIL_MHOCCUR_2 == 1 | ///
		ORG_FAIL_MHOCCUR_3 == 1 | ORG_FAIL_MHOCCUR_4 == 1 | ///
		ORG_FAIL_MHOCCUR_5 == 1 | ORG_FAIL_MHOCCUR_6 == 1 | ///
		ORG_FAIL_MHOCCUR_7 == 1 | ORG_FAIL_MHOCCUR_88 == 1 
	label var ORG_FAIL_M09 "Organ dysfunction recorded MNH09"
	
	list ORG_FAIL_MHOCCUR_1 ORG_FAIL_MHOCCUR_2 ORG_FAIL_MHOCCUR_3 ORG_FAIL_MHOCCUR_4 ORG_FAIL_MHOCCUR_5 ORG_FAIL_MHOCCUR_6 ORG_FAIL_MHOCCUR_7  ORG_FAIL_MHOCCUR_88 ORG_FAIL_MHOCCUR_99 if ORG_FAIL_M09 ==1	
	
	*Examine the organ failure: specify
	gen CHK_ORG_FAIL_SPFY = 1 if ///
	ORG_FAIL_MHOCCUR_88 == 1 | ORG_FAIL_SPFY_MHTERM!="n/a"
	label var CHK_ORG_FAIL_SPFY ///
	"Organ failure specified, examine responses"
	list ORG_FAIL_MHOCCUR_1 ORG_FAIL_MHOCCUR_2 ORG_FAIL_MHOCCUR_3 ORG_FAIL_MHOCCUR_4 ORG_FAIL_MHOCCUR_5 ORG_FAIL_MHOCCUR_6 ORG_FAIL_MHOCCUR_7  ORG_FAIL_MHOCCUR_88 ORG_FAIL_SPFY_MHTERM  if CHK_ORG_FAIL_SPFY ==1
	**check these responses to see if they are captured elsewhere
	egen check_other_orgfail = anymatch(ORG_FAIL_MHOCCUR_1 ORG_FAIL_MHOCCUR_2 ORG_FAIL_MHOCCUR_3 ORG_FAIL_MHOCCUR_4 ORG_FAIL_MHOCCUR_5 ORG_FAIL_MHOCCUR_6 ORG_FAIL_MHOCCUR_7),v(1)
	assert check_other_orgfail == 1 if CHK_ORG_FAIL_SPFY == 1
	//This will stop the do file if  CHK_ORG_FAIL_SPFY is not captured elsewhere
	drop check_other_orgfail
	
	
	*Query: missing source of organ failure
	label var ORG_FAIL_SRCE_1 "Maternal recall"
	label var ORG_FAIL_SRCE_2 "Facility or participant record"
	label var ORG_FAIL_SRCE_3 "Study assessment"

	gen MISS_SOURCE = 1 if ORG_FAIL_M09 == 1 & ///
	(ORG_FAIL_SRCE_1!=1 & ORG_FAIL_SRCE_2!=1 & ORG_FAIL_SRCE_3!=1)
	label var MISS_SOURCE "Missing info source organ failure"
	tab MISS_SOURCE SITE
	**creates a missing indicator if:
		**(recorded that woman experienced organ failure) AND 
		**(there is no information source listed)
	
	if $runquery == 1 {
		levelsof(SITE) if MISS_SOURCE == 1 , local(sitelev) clean
		foreach site of local sitelev {
			disp as result "`site'"
			export excel SITE MOMID PREGID  ORG_FAIL_MHOCCUR_1 ORG_FAIL_MHOCCUR_2 ORG_FAIL_MHOCCUR_3 ORG_FAIL_MHOCCUR_4 ORG_FAIL_MHOCCUR_5 ORG_FAIL_MHOCCUR_6 ORG_FAIL_MHOCCUR_7 ORG_FAIL_MHOCCUR_77 ORG_FAIL_MHOCCUR_88 ORG_FAIL_SRCE_1 ORG_FAIL_SRCE_2 ORG_FAIL_SRCE_3 MISS_SOURCE using "$queries/`site'-Near-Miss-$datadate.xlsx" if SITE=="`site'" & MISS_SOURCE==1, sheet("Miss-Org-Fail-Source", modify) firstrow(variables) 
			disp as result "`site' Completed"
		}
	}
	
	
	**#Pulmonary edema
	tab PREECLAMPSIA_CEOCCUR_4, m  
	//pulmonary edema is a severe feature of preeclampsia
	gen PULMONARY_EDEMA_M09 = 1 if 	PREECLAMPSIA_CEOCCUR_4 == 1
	
	**#Placenta accrete
	tab CES_PRINDC_INF1_17 CES_PRINDC_INF2_17
	gen PLACENTA_ACCRETE = 1 if ///
		(CES_PRINDC_INF1_17 == 1 | ///
		 CES_PRINDC_INF2_17 == 1 | ///
		 CES_PRINDC_INF3_17 == 1 | ///
		 CES_PRINDC_INF4_17 == 1)	
	**if c-section was done because of placenta accrete
	
	**#Placenta abruption
	gen PLACENTA_ABRUPTION = 1 if ///
		APH_FAORRES_1 ==1 | 	 /// placental abruption --> APH
		CES_PRINDC_INF1_6 == 1 | /// placental abruption --> C-section
		CES_PRINDC_INF2_6 == 1 | /// placental abruption --> C-section
		CES_PRINDC_INF3_6 == 1 | /// placental abruption --> C-section
		CES_PRINDC_INF4_6 == 1   // placental abruption --> C-section
	
	tab PLACENTA_ABRUPTION SITE
	
	**#Hysterectomy
	gen HYSTERECTOMY_M09 = 1 if PPH_FAORRES_5 == 1
	tab HYSTERECTOMY_M09 SITE
	
	
	**#Indicator woman completed MNH09
	gen 		M09 = 1 if MAT_VISIT_MNH09 <= 2
	label var 	M09 "Completed M09 form"
	//note that we are currently excluding those whose visit was not completed for various reasons
	
	keep SITE MOMID PREGID MAT_VITAL_MNH09 MAT_VISIT_MNH09 MAT_VISIT_OTHR_MNH09 MAT_LD_OHOSTDAT DELIV_DSSTDAT_INF1 MAT_DEATH_DTHDAT ORG_FAIL_HRT ORG_FAIL_RESP ORG_FAIL_RENAL ORG_FAIL_LIVER ORG_FAIL_NEUR ORG_FAIL_UTER ORG_FAIL_HEM ORG_FAIL_MHOCCUR_88 ORG_FAIL_SPFY_MHTERM ORG_FAIL_M09 CHK_ORG_FAIL_SPFY MISS_SOURCE PPH_TRNSFSN_PROCCUR PLACENTA_ACCRETE PLACENTA_ABRUPTION M09 HYSTERECTOMY_M09 PULMONARY_EDEMA_M09
	order SITE MOMID PREGID MAT_VITAL_MNH09 MAT_VISIT_MNH09 MAT_VISIT_OTHR_MNH09 MAT_LD_OHOSTDAT DELIV_DSSTDAT_INF1 MAT_DEATH_DTHDAT ORG_FAIL_HRT ORG_FAIL_RESP ORG_FAIL_RENAL ORG_FAIL_LIVER ORG_FAIL_NEUR ORG_FAIL_UTER ORG_FAIL_HEM ORG_FAIL_MHOCCUR_88 ORG_FAIL_SPFY_MHTERM ORG_FAIL_M09 CHK_ORG_FAIL_SPFY MISS_SOURCE PLACENTA_ACCRETE PLACENTA_ABRUPTION HYSTERECTOMY_M09 PULMONARY_EDEMA_M09
	
	gen site = SITE
	gen momid = MOMID
	gen pregid = PREGID
	merge 1:1  MOMID PREGID using "$outcomes/MAT_ENROLL.dta" , ///
	keepusing(SITE ENROLL REMAPP_ENROLL PREG_START_DATE)
	keep if ENROLL == 1

	save "$wrk/mnh09.dta", replace

**complete the following after mat_endpoints has been created	
	**Calculate the 42 day post partum window
	use "$outcomes/MAT_ENDPOINTS.dta", clear
	merge 1:1 MOMID PREGID  using  "$wrk/mnh09.dta", nogen
	keep if ENROLL == 1
	save "$wrk/mnh09.dta", replace
	
	gen uploaddate="$datadate"
	gen UploadDate=date(uploaddate, "YMD")
	format UploadDate %td
	drop uploaddate

**#Calculate nearmiss window (pregnancy-42 days PP)
	*Sometimes we don't know the woman's preg end date, but at some point we know that she is no longer pregnant for near-miss denominator purpose
	merge 1:1 SITE MOMID PREGID  using  "$outcomes/MAT_ENROLL.dta", nogen keepusing(EDD_BOE )
	str2date EDD_BOE
	 
	label var PREG_END_PP42_DT "PP42 days according to PREG_END_DATE or EDD"
	gen MISS_ENDPREG = 1 if PREG_END_DATE ==.

	gen PP42_PASS =0 if PREG_END_PP42_DT !=.
	replace PP42_PASS = 1 if PREG_END_PP42_DT<UploadDate & PREG_END_PP42_DT!=.
	replace PP42_PASS = 1 if (EDD_BOE+42)<UploadDate & PREG_END_PP42_DT == .
	gen PP42_PASS_edd = 1 if (EDD_BOE+42)<UploadDate & PREG_END_PP42_DT == .
	label var PP42_PASS_edd "EDD_BOE used to estimate 42 day window"
 	replace PP42_PASS = . if PREG_END_DATE==. & EDD_BOE ==. 
	
	save "$wrk/mnh09.dta", replace
	

**# MNH10 - Postdelivery maternal outcome
	/*
	**Referral to ICU
	**Endometritis
	Note: only one MNH10 per participant, no need for reshape/collapse
	*/
	use "$data/mnh10.dta",  clear
	rename M10_* *
	
	keep if MAT_VISIT_MNH10 <= 2 //keep completed
	
	
	
	*Query 1: MNH10 completed, but no pregnancy end date
	merge m:1 MOMID PREGID using "$outcomes/MAT_ENDPOINTS.dta"
	
	gen QUERY_NOTE = "Completed M10 but no preg end point recorded" if _merge == 1
	if $runquery == 1 {
		levelsof(SITE) if _merge==1 , local(sitelev) clean
		foreach site of local sitelev {
			disp as result "`site'"
			export excel SITE MOMID PREGID MAT_VISIT_MNH10 VISIT_OBSSTDAT PREG_END QUERY_NOTE using "$queries/`site'-Near-Miss-$datadate.xlsx" if SITE=="`site'" & _merge==1, sheet("M10-without-pregend", modify) firstrow(variables)  datestring("%tdDD-Mon-CCYY")
			disp as result "`site' Completed"
		}
	}
	
	*Query 2: MNH10 was completed before pregnancy end date
	str2date VISIT_OBSSTDAT
	gen M10_PP= VISIT_OBSSTDAT-PREG_END_DATE if !missing(PREG_END_DATE)
	
	replace QUERY_NOTE = "Completed M10 before preg end point recorded" if M10_PP<0
	
	if $runquery == 1 {
	levelsof(SITE) if M10_PP<0 , local(sitelev) clean
		foreach site of local sitelev {
			disp as result "`site'"
			export excel SITE MOMID PREGID MAT_VISIT_MNH10 VISIT_OBSSTDAT QUERY_NOTE  using "$queries/`site'-Near-Miss-$datadate.xlsx" if SITE=="`site'" & M10_PP<0, sheet("M10-before-pregend", modify) firstrow(variables)  datestring("%tdDD-Mon-CCYY")
			disp as result "`site' Completed"
		}
	}
	
	
	**#Referall to ICU
	gen MAT_ICU = 1 if ///
		TRANSFER_OHOLOC==1 | /// transferred to ICU in delivery facility
		TRANSFER_OHOLOC==2 	 // transferred to ICU in another facility
	label var MAT_ICU ///
	"Patient transferred to ICU in delivery or other facility"
	tab MAT_ICU SITE
	gen MAT_ICU_DT = PREG_END_DATE if MAT_ICU == 1
		*Date will equal pregnancy end date
		*Because MNH10 refers to events <=24 hours of delivery
		*Date of MNH10 may be weeks-months later
	
	gen COMPL_M10 = 1 if MAT_DSTERM == 2
	label var COMPL_M10 ///
	"Mother had complication requiring higher level care"
	
	**#Endometritis
	gen ENDOMETRITIS_M10 = 1 if POST_DEL_INF_CEOCCUR_1 == 1
	gen ENDOMETRITIS_M10_DT = PREG_END_DATE if ENDOMETRITIS_M10 == 1
	
	rename VISIT_OBSSTDAT M10_VISIT_OBSSTDAT

	*Clean up file:
	keep SITE MOMID PREGID M10_PP MAT_ICU MAT_ICU_DT TRANSFER_OHOLOC ENDOMETRITIS_M10 COMPL_M10 MAT_VITAL_MNH10 MAT_VISIT_MNH10 M10_VISIT_OBSSTDAT  ENDOMETRITIS_M10_DT
	save "$wrk/mnh10.dta" , replace	

**# MNH19 Hospitalization
	/*
	Organ failure/dysfunction
	Transfusion -  mother received
	Laparotomy (includes bowel resection and hysterectomy)
	Abortion complications
	Endometritis
	Indicator for ever hospitalized
	*/
	use "$data/mnh19",  clear
	
	rename M19_* *
	
	gen HOSP_DATE_RECORD = OHOSTDAT
	replace HOSP_DATE_RECORD = . if HOSP_DATE_RECORD < 0
	
	gen HOSP_DATE_EST = MAT_EST_OHOSTDAT
	replace HOSP_DATE_EST = . if HOSP_DATE_EST < 0 
	
	gen HOSP_DATE = HOSP_DATE_RECORD
	replace HOSP_DATE = HOSP_DATE_EST if HOSP_DATE_RECORD == . 
	format HOSP_DATE_RECORD HOSP_DATE_EST HOSP_DATE %td
	
	**# Organ failure
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

	
	gen ORG_FAIL_M19 = 0
	replace ORG_FAIL_M19 = 1 if ///
	ORGAN_FAIL_MHTERM_1 == 1 | /// heart failure
	ORGAN_FAIL_MHTERM_2 == 1 | /// respiratory
	ORGAN_FAIL_MHTERM_3 == 1 | /// renal/kidney
	ORGAN_FAIL_MHTERM_4 == 1 | /// liver/hepatic
	ORGAN_FAIL_MHTERM_5 == 1 | /// neurological
	ORGAN_FAIL_MHTERM_6 == 1 | /// uterine
	ORGAN_FAIL_MHTERM_7 == 1 | /// coagulation/hematologic
	ORGAN_FAIL_MHTERM_88 == 1 // other/specify
	label var ORG_FAIL_M19 "Organ dysfunction MNH19"
	
	gen CHK_ORG_FAIL_SPFY = 1  if ///
	ORGAN_FAIL_MHTERM_88 == 1 | ORGAN_FAIL_SPFY_MHTERM != "n/a"
	label var CHK_ORG_FAIL_SPFY "Manually check if failure is specified"
	
	gen ORG_FAIL_M19_DT = HOSP_DATE if ORG_FAIL_M19 == 1
	
	**#Transfusion 
	gen TRANSFUSION = 1 if TX_PROCCUR_1 == 1 //mother received transfusion
	gen TRANSFUSION_M19_DT = HOSP_DATE if TRANSFUSION == 1
	
	**#Laparotomy & hysterectomy
	gen LAPAROTOMY = 1 if ///
	TX_PROCCUR_2==1 | TX_PROCCUR_3 == 1 | TX_PROCCUR_5 == 1
	label var LAPAROTOMY ///
	"Laparotomy incl. hysterectomy & bowel resection"
	gen LAPAROTOMY_M19_DT = HOSP_DATE if LAPAROTOMY==1
	
	gen HYSTERECTOMY_M19 = 1 if TX_PROCCUR_5 == 1 
	gen HYSTERECTOMY_M19_DT = HOSP_DATE if HYSTERECTOMY_M19 ==1
	
	
	**#Pulmonary edema
	gen PULM_EDEMA_M19 = 1 if ///
	PREECLAMPSIA_CEOCCUR_4 == 1 | ///severe feature of PE
	HTN_MHTERM_8 == 1 | /// type of HDP
	DX_OTHR_MHTERM_4 == 1 // "other medical condition"
	gen PULM_EDEMA_M19_DT = HOSP_DATE if PULM_EDEMA_M19==1
	
	**#Abortion complications
	gen ABORT_COMPL_M19 = 1 if EARLY_LOSS_MHTERM==6
	gen ABORT_COMPL_M19_DT = HOSP_DATE if ABORT_COMPL_M19==1
	
	**#Endometritis
	gen ENDOMETRITIS_M19 = 1 if INFECTION_MHTERM_16 == 1
	gen ENDOMETRITIS_M19_DT = HOSP_DATE if ENDOMETRITIS_M19==1
	
	***Other variables around care received 
	gen WOUNDCARE = 1 if TX_PROCCUR_4 == 1
	gen VENTILATION = 1 if TX_PROCCUR_6 == 1
	
	gen CHK_INTERVENTION_ORGFAIL  = 1 if ///
	(TRANSFUSION == 1 | LAPAROTOMY == 1 | VENTILATION == 1 ) & ///
	ORG_FAIL_M19!=1 
	label var CHK_INTERVENTION_ORGFAIL ///
	"Critical intervention but no organ failure reported"
	
	gen ADMIT_INPATIENT_M19 = 1 if ///
	VISIT_FAORRES == 3 // patient admitted at this location for in-patient care
	gen REFERRED_M19 = 1 if ///
	VISIT_FAORRES == 4 | /// patient referred
	ADMIT_DSTERM == 2 //patient admitted then referred
	
	keep SITE MOMID PREGID HOSP_DATE_RECORD HOSP_DATE_EST  HOSP_DATE OBSSTDAT VISIT_OBSLOC VISIT_FAC_SPFY_OBSLOC VISIT_OTHR_SPFY_OBSLOC TIMING_OHOCAT OHOSTDAT_YN OHOSTDAT MAT_EST_OHOSTDAT CARE_OHOLOC CARE_HOSP_OHOLOC MAT_ARRIVAL_DSDECOD ORG_FAIL_HRT_M19 ORG_FAIL_RESP_M19 ORG_FAIL_RENAL_M19 ORG_FAIL_LIVER_M19 ORG_FAIL_NEUR_M19 ORG_FAIL_UTER_M19 ORG_FAIL_HEM_M19 ORG_FAIL_OTHR_M19 ORG_FAIL_SPFY_M19 ORG_FAIL_M19 ORG_FAIL_M19_DT CHK_ORG_FAIL_SPFY CHK_INTERVENTION_ORGFAIL ORG_FAIL_M19 TX_PROCCUR_1 TX_PROCCUR_2 TX_PROCCUR_3 TX_PROCCUR_4 TX_PROCCUR_5 TX_PROCCUR_6 TX_PROCCUR_77 TX_PROCCUR_88 TX_PROCCUR_99 TX_PRTRT LAPAROTOMY  LAPAROTOMY_M19_DT TRANSFUSION TRANSFUSION_M19_DT VENTILATION WOUNDCARE PRIMARY_MHTERM PREG_DSTERM PREG_FAORRES VISIT_FAORRES ADMIT_DSTERM DTHDAT ADMIT_INPATIENT_M19 REFERRED_M19 PULM_EDEMA_M19 PULM_EDEMA_M19_DT ENDOMETRITIS_M19 ENDOMETRITIS_M19_DT ABORT_COMPL_M19 ABORT_COMPL_M19_DT HYSTERECTOMY_M19 HYSTERECTOMY_M19_DT FORMCOMPLDAT_MNH19
	
	      
	
	**#Indicator for ever hospitalized
	*Used to construct the "unplanned hospitalization" variable (PLTC)
	gen M19 = 1 
	label var M19 "M19 form available"
	
	gen M19_dt = HOSP_DATE
	format M19_dt %td
	

///Merge MNH19 to MNH09 to create near-miss.dta

	merge m:1 MOMID PREGID using "$wrk/mnh09.dta", ///
	keepusing(PREG_END_DATE EDD_BOE PREG_END_PP42_DT) gen(mergewindow)
	keep if mergewindow == 3
	
	merge m:1 MOMID PREGID using "$outcomes/MAT_ENROLL.dta", ///
	keepusing(PREG_START_DATE) 
	keep if _merge == 3
	drop _merge
	
	**Generate dates of hospitalization events
	str2date PREG_START_DATE
	gen 	M19_GA = HOSP_DATE - PREG_START_DATE 
	replace M19_GA=. if HOSP_DATE>PREG_END_DATE & PREG_END_DATE!=.
	
	gen 	M19_PP = HOSP_DATE - PREG_END_DATE
	replace M19_PP = . if HOSP_DATE < PREG_END_DATE & PREG_END_DATE!=.
	replace M19_PP = . if PREG_END_DATE==.
	
	**For near miss, we will only consider if the hospitalization events took place within pregnancy up to 42 days of postpartum
	gen within42 = 1 if ///
	HOSP_DATE<= PREG_END_PP42_DT & HOSP_DATE!=. & PREG_END_PP42_DT!=.
	//keep if hospitalization date less than PP42 window
	replace within42 = 1 if ///
	OBSSTDAT <= PREG_END_PP42_DT & OBSSTDAT !=. & PREG_END_PP42_DT!=.
	//keep if date of data collection less than PP42 window
	replace within42 = 1 if ///
	HOSP_DATE==. &  TIMING_OHOCAT==1
	//if hospitalization date is missing but timing variable indicates antenatal period
	replace within42 = 1 if PREG_END_DATE==. & TIMING_OHOCAT==1
	*No preg end date but timing indicates it is antenatal

	**For denominator: indicator for woman was seen alive 42+ days postpartum
	gen after42 = 1 if ///
	HOSP_DATE>= PREG_END_PP42_DT & HOSP_DATE!=. & PREG_END_PP42_DT!=.
	//=1 if hospitalization date was after 42 day period 
	replace after42 = 1 if ///
	after42==. & /// after42 is unknown
	OBSSTDAT >= PREG_END_PP42_DT & OBSSTDAT !=. & PREG_END_PP42_DT!=. & ///
	(VISIT_FAORRES != 5 & /// woman did not die
	VISIT_FAORRES != 99 & /// status not unknown
	ADMIT_DSTERM !=3 & /// did not die while hospitalized
	ADMIT_DSTERM != 99) //status not unknown
	//=1 if FORM date was completed after the 42 day period and the woman had not died/status not unknown
	
	order SITE MOMID PREGID PREG_END_DATE EDD PREG_END_PP42_DT HOSP_DATE OBSSTDAT within42 after42

	save "$wrk/mnh19_long.dta" , replace // the long data set
	
	
	
	***************************************************
	**#Create a wide dataset for hospitalizations postpartum
	preserve 
		keep if inrange( M19_PP,0,.) //only if PP
		
		foreach var in M19 ORG_FAIL_M19 HYSTERECTOMY_M19 {
			gen `var'_STR = "`var'" if `var' == 1
		}
		egen CONDITIONS = concat( M19_STR ORG_FAIL_M19_STR HYSTERECTOMY_M19_STR), punct(" ")
		
		keep SITE MOMID PREGID CONDITIONS HOSP_DATE
		duplicates drop
		
		rename CONDITIONS M19_CONDITIONS  
		rename HOSP_DATE  M19_HOSP_DATE 
		
		bysort PREGID (M19_HOSP_DATE) : gen visnum = _n
		sum visnum
		return list 
		global max_pp = `r(max)'
		
		reshape wide M19_HOSP_DATE M19_CONDITIONS, i(SITE MOMID PREGID) j(visnum)
		
		save "$wrk/mnh19_wide_pp", replace
	restore
	***************************************************
	
	***************************************************
	**#run query for mismatched MNH04/MNH19
		*Instances where MNH04 reports hospitalization
		* & no MNH19 during ANC
		
	if $runquery == 1 {
		
		preserve
		
		duplicates tag SITE MOMID PREGID HOSP_DATE , gen(dup)
		levelsof(SITE) if dup > 0 , clean local(sitelev)
		foreach site of local sitelev {
			disp as result "`site'"
			export excel SITE MOMID PREGID HOSP_DATE OBSSTDAT VISIT_FAORRES ADMIT_DSTERM CARE_OHOLOC CARE_HOSP_OHOLOC  using "$queries/`site'-Near-Miss-$datadate.xlsx" if SITE=="`site'" & dup>0 , sheet("duplicate-mnh19", modify) firstrow(variables)  datestring("%tdDD-Mon-CCYY")
			disp as result "`site' Completed"
		}
		
		*until fixed, we will just pick the first one in the dataset:
		bysort PREGID HOSP_DATE : gen dupvisit = _n
		
		keep if dupvisit == 1
		keep if M19_GA !=. //ANC only
			
		*how many visits per pregid?
		bysort PREGID : gen numvisit = _n
		sum numvisit
		return list 
		global max =  `r(max)'
		
		gen M19_OHOSTDAT = OHOSTDAT
		gen M19_MAT_EST_OHOSTDAT = MAT_EST_OHOSTDAT
		str2date M19_OHOSTDAT M19_MAT_EST_OHOSTDAT
		format M19_OHOSTDAT M19_MAT_EST_OHOSTDAT %td
		keep SITE MOMID PREGID HOSP_DATE FORMCOMPLDAT_MNH19
		rename HOSP_DATE M19_HOSP_DATE
		bysort PREGID ( M19_HOSP_DATE): gen visnum = _n
		rename FORMCOMPLDAT_MNH19 FORMCOMPLDAT_HOSP
		reshape wide M19_HOSP_DATE FORMCOMPLDAT_HOSP, i( SITE MOMID PREGID) j(visnum)
		merge 1:m SITE MOMID PREGID using "$wrk/mnh04_nodups.dta"
		drop if _merge == 1 
		**these have only MNH19 (no query)
		
		* _merge == 2 are definite queries
		* _merge == 3 are possible queries
		
		format M19_HOSP_DATE1 M19_HOSP_DATE2 M19_HOSP_DATE3 M19_HOSP_DATE4 M19_HOSP_DATE5 M04_HOSP_OHOSTDAT HOSP_DATE %tdDDmonYY
		drop HOSP_DATE
		
		
		gen merge_note = "Hospitalization reported in MNH04 but no MNH19" if _merge == 2
		
		
		preserve
			import excel "D:\Users\savannah.omalley\Downloads\Pakistan-Near-Miss-2025-04-18_Hospitalization.xlsx", sheet("mnh04-missing-mnh19") firstrow case(upper)
			rename ANC_OBSSTDAT ANC_OBSSTDAT_STR
			rename M04_HOSP_OHOSTDAT M04_HOSP_OHOSTDAT_STR
			gen ANC_OBSSTDAT = date( ANC_OBSSTDAT_STR,"DMY")
			format ANC_OBSSTDAT %td
			gen M04_HOSP_OHOSTDAT = date( M04_HOSP_OHOSTDAT_STR ,"DMY")
			format M04_HOSP_OHOSTDAT %td
			save "D:\Users\savannah.omalley\Documents\near_miss\queries\Pakistan-Near-Miss-2025-04-18_Hospitalization.dta"
		restore
		
		merge 1:1 SITE MOMID PREGID ANC_OBSSTDAT M04_HOSP_OHOSTDAT using "D:\Users\savannah.omalley\Documents\near_miss\queries\Pakistan-Near-Miss-2025-04-18_Hospitalization.dta",gen(pakmerge)
		
		replace merge_note = "" if regexm(PAK_COMMENTS, "not hospitalized for more than 12 hours")
		
		levelsof(SITE) if merge_note == "Hospitalization reported in MNH04 but no MNH19" , clean local(sitelev)
		foreach site of local sitelev {
			
			export excel SITE MOMID PREGID merge_note ANC_OBSSTDAT M04_HOSP_OHOSTDAT using "$queries/`site'-Near-Miss-$datadate.xlsx" if SITE=="`site'" & merge_note == "Hospitalization reported in MNH04 but no MNH19" , sheet("mnh04-missing-mnh19", modify) firstrow(variables) datestring("%tdDD-Mon-CCYY")
			disp as result "`site' Completed"
		}
		
		gen query_flag = 1 if _merge == 3
		gen date_differ = .
		
		*Now check if merged entries should be a query:
		foreach num of numlist 1/$max {
			gen date_differ`num' = ///
			abs(M04_HOSP_OHOSTDAT - M19_HOSP_DATE`num')
			replace query_flag = . if inrange(date_differ`num' , 0,7)
			replace date_differ = date_differ`num' if date_differ`num' < date_differ 
		}
		
		replace merge_note ="MNH19 exists but >7 days difference" if _merge == 3 & query_flag == 1
		
		
		levelsof(SITE) if query_flag==1 & _merge == 3, clean local(sitelev)
		foreach site of local sitelev {
			export excel SITE MOMID PREGID merge_note ANC_OBSSTDAT M04_HOSP_OHOSTDAT M19_HOSP_DATE* date_differ using "$queries/`site'-Near-Miss-$datadate.xlsx" if SITE=="`site'" & query_flag == 1 & _merge == 3 , sheet("mnh04-with-mnh19-outside-range", modify) firstrow(variables)  datestring("%tdDD-Mon-CCYY")
			disp as result "`site' Completed"
		}
		
		restore
	}
	***************************************************
	

	*We only want to keep these outcomes if they occured within 42 days PP
	foreach var in ORG_FAIL_M19 ORG_FAIL_HRT_M19 ORG_FAIL_RESP_M19 ORG_FAIL_RENAL_M19 ORG_FAIL_LIVER_M19 ORG_FAIL_NEUR_M19 ORG_FAIL_UTER_M19 ORG_FAIL_HEM_M19 ORG_FAIL_OTHR_M19 TRANSFUSION LAPAROTOMY WOUNDCARE VENTILATION CHK_INTERVENTION_ORGFAIL M19 ADMIT_INPATIENT_M19 REFERRED_M19 HYSTERECTOMY_M19 ABORT_COMPL_M19 M19 {
		replace `var' = . if within42 != 1
	}
	replace ORG_FAIL_SPFY_M19 = "" if within42 != 1

	
	rename TRANSFUSION TRANSFUSION_M19
	rename LAPAROTOMY LAPAROTOMY_M19
	rename WOUNDCARE WOUNDCARE_M19
	rename VENTILATION VENTILATION_M19
	rename CHK_INTERVENTION_ORGFAIL CHK_INTERVENTION_ORGFAIL_M19

	 assert ORG_FAIL_M19_DT		!=. if ORG_FAIL_M19==1
	 assert TRANSFUSION_M19_DT 	!=. if TRANSFUSION_M19==1
	 assert LAPAROTOMY_M19_DT 	!=. if LAPAROTOMY_M19==1
	 assert PULM_EDEMA_M19_DT	!=. if PULM_EDEMA_M19==1
	 assert HYSTERECTOMY_M19_DT	!=. if HYSTERECTOMY_M19==1
	 assert ABORT_COMPL_M19_DT	!=. if ABORT_COMPL_M19==1
	 assert ENDOMETRITIS_M19_DT	!=. if ENDOMETRITIS_M19==1

	 rename *_DT *_dt
	 
	 *in case of multiple near-miss events, pick the first:	 
	 foreach var in M19 ORG_FAIL_M19 TRANSFUSION_M19 LAPAROTOMY_M19 PULM_EDEMA_M19 HYSTERECTOMY_M19 ABORT_COMPL_M19 ENDOMETRITIS_M19 {
	 	bysort PREGID : egen `var'_DT = min(`var'_dt)
	 }
	 
	 drop *_dt
	///COLLAPSE
	collapse (max) ORG_FAIL_M19 ORG_FAIL_HRT_M19 ORG_FAIL_RESP_M19 ORG_FAIL_RENAL_M19 ORG_FAIL_LIVER_M19 ORG_FAIL_NEUR_M19 ORG_FAIL_UTER_M19 ORG_FAIL_HEM_M19 ORG_FAIL_OTHR_M19 TRANSFUSION_M19 LAPAROTOMY_M19 WOUNDCARE_M19 VENTILATION_M19 CHK_INTERVENTION_ORGFAIL_M19 ADMIT_INPATIENT_M19 REFERRED_M19 M19 within42 after42 PULM_EDEMA_M19 ENDOMETRITIS_M19 ABORT_COMPL_M19 HYSTERECTOMY_M19 (firstnm) ORG_FAIL_SPFY_M19 (min) M19_DT ORG_FAIL_M19_DT TRANSFUSION_M19_DT LAPAROTOMY_M19_DT PULM_EDEMA_M19_DT HYSTERECTOMY_M19_DT ABORT_COMPL_M19_DT ENDOMETRITIS_M19_DT , by(SITE MOMID PREGID)
	
	label var ADMIT_INPATIENT_M19 "Admitted for in patient care"
	label var REFERRED_M19 "Referred to another facility"
	
	merge 1:1 MOMID PREGID using "$wrk/mnh09.dta" , gen(HOSP_merge)
	
	order ORG_FAIL_M19-M19, after( HOSP_merge)
	
	save "$wrk/near-miss.dta", replace
	
**# MNH12 Maternal PNC Clinical Status
	/*
	- Organ failure
	- Infection & sepsis
	- Hysterectomy (diagnosed)
	- Pulmonary edema
	- Mother was hospitalized
	- For denominator: woman seen > 42 days PP
	*/
	use "$data/mnh12", clear
	
	merge m:1 MOMID PREGID using "$wrk/mnh09.dta", ///
	keepusing(ENROLL PREG_END_DATE PREG_END_PP42_DT)
	keep if _merge==3
	**keep those enrolled and have an mnh12 form
	
	keep if M12_MAT_VISIT_MNH12 <= 2
			*keep only completed visits
	
	*Indicator if MNH12 form was filled out:
	gen M12 = 1 if M12_MAT_VISIT_MNH12 <=2 // =1 if visit completed
	
	**Dates of MNH12:
	str2date M12_VISIT_OBSSTDAT
	gen within42_M12 = 1 if ///
	M12_VISIT_OBSSTDAT <= PREG_END_PP42_DT & M12_MAT_VISIT_MNH12<=2
	label var within42_M12 "Observed <= 42 days of preg end"
	
	gen after42_M12 = 1 if  ///
	M12_VISIT_OBSSTDAT >= PREG_END_PP42_DT  & M12_MAT_VISIT_MNH12<=2
	replace after42_M12 = . if M12_VISIT_OBSSTDAT==. | PREG_END_PP42_DT == . 
	label var after42_M12 "Seen >= 42 days after preg end"
	*note that both the after 42 and within 42 include 42 because some sites will close out a women exactly 42 days after her miscarriage
	*we want to capture that we saw her on day 42+ because of how near-miss-denominator is calculated
	
	gen INFAGE_M12 = M12_VISIT_OBSSTDAT - PREG_END_DATE
	
	
	**# Organ failure 	
	gen ORG_FAIL_HRT_M12 = 1 if M12_ORG_FAIL_MHOCCUR_1 == 1 
	gen ORG_FAIL_RESP_M12 = 1 if M12_ORG_FAIL_MHOCCUR_2==1
	gen ORG_FAIL_RENAL_M12 = 1 if M12_ORG_FAIL_MHOCCUR_3==1
	gen ORG_FAIL_LIVER_M12 = 1 if M12_ORG_FAIL_MHOCCUR_4==1
	gen ORG_FAIL_OTHR_M12 = 1 if M12_ORG_FAIL_MHOCCUR_88==1
	
	gen ORG_FAIL_SPFY_M12 = M12_ORG_FAIL_MHTERM 
	replace ORG_FAIL_SPFY_M12 = "" if M12_ORG_FAIL_MHTERM!="77" | ///
	M12_ORG_FAIL_MHTERM !="missing" | M12_ORG_FAIL_MHTERM!="n/a"

	gen ORG_FAIL_M12 = 1 if ///
	ORG_FAIL_HRT_M12 == 1 | ORG_FAIL_RESP_M12 == 1 | ///
	ORG_FAIL_RENAL_M12 == 1 | ORG_FAIL_LIVER_M12 == 1 | ///
	ORG_FAIL_OTHR_M12 == 1 | ORG_FAIL_SPFY_M12!=""
	label var ORG_FAIL_M12 "Organ failure M12"
	**!! to discuss: should we include ORG_FAIL_SPFY in the calculation?
		**Does not affect the outcome right now
	list M12_ORG_FAIL_MHOCCUR_1 M12_ORG_FAIL_MHOCCUR_2 M12_ORG_FAIL_MHOCCUR_3 M12_ORG_FAIL_MHOCCUR_4  M12_ORG_FAIL_MHOCCUR_88 ORG_FAIL_SPFY_M12 if ORG_FAIL_M12==1
	
	
	**# Hysterectomy
	gen HYSTERECTOMY_M12 = 1 if M12_BIRTH_COMPL_MHTERM_8 == 1 
	**birth complication == hysterectomy
	*No date associated with this; use MNH12 visit date?
	
	
	**# Pulmonary edema
	gen PULMONARY_EDEMA_M12 = 1 if M12_PULM_EDEMA_MHOCCUR == 1
	gen PULMONARY_EDEMA_M12_DT = M12_PULM_EDEMA_MHSTDAT if PULMONARY_EDEMA_M12 ==1
	
	
	gen HOSPITALIZED_M12 = 1 if M12_HOSP_LAST_VISIT_OHOOCCUR==1 
		*Mother reported being hospitalized
		*Note!! Date of hospitalization is not recorded in mnh12

	**Clean up file:	
	order   SITE MOMID PREGID M12_VISIT_OBSSTDAT PREG_END_DATE INFAGE_M12 within42_M12 after42_M12 M12_MAT_VISIT_MNH12 ORG_FAIL_M12 ORG_FAIL_HRT_M12 ORG_FAIL_RESP_M12 ORG_FAIL_RENAL_M12 ORG_FAIL_LIVER_M12 ORG_FAIL_OTHR_M12 ORG_FAIL_SPFY_M12 M12_ORG_FAIL_MHOCCUR_* HYSTERECTOMY_M12 PULMONARY_EDEMA_M12 PULMONARY_EDEMA_M12_DT HOSPITALIZED_M12 M12
	
	keep SITE MOMID PREGID M12_VISIT_OBSSTDAT PREG_END_DATE INFAGE_M12 ORG_FAIL_M12 HYSTERECTOMY_M12 HOSPITALIZED_M12 ORG_FAIL_HRT_M12 ORG_FAIL_RESP_M12 ORG_FAIL_RENAL_M12 ORG_FAIL_LIVER_M12 ORG_FAIL_OTHR_M12 ORG_FAIL_M12 M12_ORG_FAIL_MHOCCUR_* within42_M12 after42_M12 HYSTERECTOMY_M12 PULMONARY_EDEMA_M12 PULMONARY_EDEMA_M12_DT HOSPITALIZED_M12 M12 ORG_FAIL_SPFY_M12  
		
	bysort PREGID ( M12_VISIT_OBSSTDAT) : gen visnum = _n
		
	**#Determine date of organ failure, hysterectomy, and hospitalization	
		*Step 1: calculate minimum and maximum dates for the events:
		
		foreach event in ORG_FAIL HYSTERECTOMY HOSPITALIZED {
			
		bysort PREGID : egen `event'_EVER = max( `event'_M12)
		gen `event'_MAX = M12_VISIT_OBSSTDAT if `event'_M12 ==1
		*"Max" date the hospitalization could have happened
		bysort PREGID (visnum) : gen `event'_MIN = M12_VISIT_OBSSTDAT[_n-1] if `event'_M12 ==1
		*"min" date it could have happened (date from previous visit)
		format `event'_MAX `event'_MIN %td
		*If the event is reported on her first PNC visit, it happened at some point between L&D and her first PNC visit. use PREG_END_DATE as the minimum:
		bysort PREGID : replace `event'_MIN = PREG_END_DATE if `event'_M12 ==1 & visnum==1
		bysort PREGID : gen `event'_MIN_PP =  `event'_MIN- PREG_END_DATE if `event'_M12 == 1
		
		}
		 
		drop  ORG_FAIL_EVER HYSTERECTOMY_EVER HOSPITALIZED_EVER
		 
		merge m:1 SITE MOMID PREGID using "$wrk/mnh19_wide_pp"
		*this dataset only has MNH19 from 0-42 days postpartum
		drop if _merge == 2
			*if only MNH19, this is not a query
			tab _merge SITE, col
	

		**#Query: Organ failure w/o MNH19
		gen merge_note = "MNH12 organ fail without MNH19" if _merge == 1 & ORG_FAIL_M12 ==1 

		levelsof(SITE) if _merge==1 & ORG_FAIL_M12 ==1 & visnum>1 , clean local(sitelev)
		foreach site of local sitelev {
			disp as result "`site'"
			export excel SITE MOMID PREGID merge_note M12_VISIT_OBSSTDAT visnum M12_ORG_FAIL_MHOCCUR_* using "$queries/`site'-SHARE-Near-Miss-$datadate.xlsx" if SITE=="`site'" & _merge == 1 & ORG_FAIL_M12 ==1 &visnum > 1, sheet("mnh12-orgfail-missing-mnh19", modify) firstrow(variables)  datestring("%tdDD-Mon-CCYY")
			disp as result "`site' Completed"
		}
		
		
		
	
		
		
		*Query 2: check for hospitalization dates within the range
		gen query_hosprange = 1 if _merge == 3 &  ///
		(ORG_FAIL_M12 ==1 | HYSTERECTOMY_M12 == 1 | HOSPITALIZED_M12 == 1 )	
		gen date_differ = . if _merge == 3 & ///
		(ORG_FAIL_M12 ==1 | HYSTERECTOMY_M12 == 1 | HOSPITALIZED_M12 == 1 )
		gen closest_hosp = M19_HOSP_DATE1 if ///
		(ORG_FAIL_M12 ==1 | HYSTERECTOMY_M12 == 1 | HOSPITALIZED_M12 == 1 )
		foreach num of numlist 1/$max_pp {
			
			gen date_differ`num' = M12_VISIT_OBSSTDAT - M19_HOSP_DATE`num' if HOSPITALIZED_M12==1
			replace query_hosprange = 0 if inrange(M19_HOSP_DATE`num',HOSPITALIZED_MIN, HOSPITALIZED_MAX )
			
			replace date_differ = date_differ`num' if date_differ`num' < date_differ & date_differ`num' > 0 & date_differ`num' !=. 
			replace closest_hosp = M19_HOSP_DATE`num' if date_differ`num' < date_differ & date_differ`num' > 0 & date_differ`num' !=.
					
		}
		
		label define query_hosprange ///
			0"hosp within min/max" 1"no hosp within range",replace
		label val 	query_hosprange query_hosprange
		
		gen HOSPITALIZED_M12_dt = closest_hosp if HOSPITALIZED_M12 == 1
		replace HOSPITALIZED_M12_dt = ///
		((HOSPITALIZED_MAX - HOSPITALIZED_MIN)/2) + HOSPITALIZED_MIN if ///
		(query_hosprange == 1 | _merge == 1) & HOSPITALIZED_M12 == 1
		label var HOSPITALIZED_M12_dt "Closest if within range, or midpoint"
		
		gen HOSPITALIZED_M12_pp = HOSPITALIZED_M12_dt-PREG_END_DATE
		
		format closest_hosp HOSPITALIZED_M12_dt %td
		
		**Query # 3: MNH19 exists but outside min/max range
		replace merge_note ="MNH19 outside range" if _merge == 3 & query_hosprange == 1
	levelsof(SITE) if _merge==3 & query_hosprange == 1 , clean local(sitelev)
		foreach site of local sitelev {
			disp as result "`site'"
			export excel SITE MOMID PREGID merge_note M12_VISIT_OBSSTDAT ORG_FAIL_M12 HYSTERECTOMY_M12 HOSPITALIZED_M12 M19_HOSP_DATE* date_differ using "$queries/`site'-Near-Miss-$datadate.xlsx" if SITE=="`site'" & _merge == 3 & query_hosprange == 1 , sheet("mnh12-mnh19-outside-range", modify) firstrow(variables)  datestring("%tdDD-Mon-CCYY")
			disp as result "`site' Completed"
		}	
			

	/*
	*next step: 
	*no longer needed, we will code all events and later restrict to <=42 days pp
	foreach var in ORG_FAIL_HRT_M12 ORG_FAIL_RESP_M12 ORG_FAIL_RENAL_M12 ORG_FAIL_LIVER_M12 ORG_FAIL_OTHR_M12 ORG_FAIL_M12  HYSTERECTOMY_M12 PULMONARY_EDEMA_M12 HOSPITALIZED_M12 M12 {
	replace `var' = . if within42_M12 != 1	
	}
	replace ORG_FAIL_SPFY_M12 = "" if within42_M12 != 1
	*/
	

	*Next: calculate midpoints for organ failure & hysterectomy
	*As of 2025-04-18, these events are not in MNH19
	   
		gen ORG_FAIL_M12_dt = ///
		((ORG_FAIL_MAX - ORG_FAIL_MIN)/2) + ORG_FAIL_MIN if ///
		ORG_FAIL_M12 == 1
		label var ORG_FAIL_M12_dt "Organ fail date - midpoint"
		bysort PREGID: egen ORG_FAIL_M12_DT=min(ORG_FAIL_M12_dt)
		
		gen HYSTERECTOMY_M12_dt = ///
		((HYSTERECTOMY_MAX - HYSTERECTOMY_MIN)/2) + HYSTERECTOMY_MIN if ///
		HYSTERECTOMY_M12 == 1
		label var HYSTERECTOMY_M12_dt "Hysterectomy date - midpoint"
		bysort PREGID: egen HYSTERECTOMY_M12_DT = min(HYSTERECTOMY_M12_dt)
	
	
		bysort PREGID: egen HOSPITALIZED_M12_DT = min(HOSPITALIZED_M12_dt)
	
		drop M12_ORG_FAIL_MHOCCUR_*
	
	
 save "$wrk/mnh12_long.dta", replace //long data set
 
 //collapse
 collapse (max) ORG_FAIL_HRT_M12 ORG_FAIL_RESP_M12 ORG_FAIL_RENAL_M12 ORG_FAIL_LIVER_M12 ORG_FAIL_OTHR_M12 ORG_FAIL_M12 within42_M12 after42_M12 HYSTERECTOMY_M12 PULMONARY_EDEMA_M12 HOSPITALIZED_M12 M12 (firstnm) ORG_FAIL_SPFY_M12 (min) ORG_FAIL_M12_DT HYSTERECTOMY_M12_DT PULMONARY_EDEMA_M12_DT HOSPITALIZED_M12_DT, by(SITE MOMID PREGID)
 
 label var HOSPITALIZED_M12_DT "First PP hospitalization from MNH12/19 if recorded, or midpoint based on MNH12"
 
 save "$wrk/mnh12_wide.dta",replace 
	
**# Merge MNH12 with other near-miss files
	merge 1:1 MOMID PREGID using "$wrk/near-miss.dta", gen(M12_merge)
	merge 1:1 MOMID PREGID using "$wrk/mnh10.dta", gen(mnh10merge)
	merge 1:1 MOMID PREGID using "$wrk/mnh04_wide.dta", gen(m04_merge)

	save "$wrk/near-miss.dta",replace

	
