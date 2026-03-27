**PRISMA maternal outcome: Near miss**
***Adapted maternal near-miss

**By Savannah O'Malley (savannah.omalley@gwu.edu)
**Begun May 17, 2024

**# Directories and paths

	*Change the below based on data date
	global datadate "2025-04-18"	

	local date: di %td_CCYY_NN_DD daily("`c(current_date)'", "DMY")
	global today = subinstr(strltrim("`date'"), " ", "-", .)
	disp "$today"
	global da "Z:\Stacked Data/$datadate"
	global outcomes "Z:\Outcome Data/$datadate"
	
	cap mkdir "D:\Users\savannah.omalley\Documents\near_miss/$datadate"
	global wrk "D:\Users\savannah.omalley\Documents\near_miss/$datadate"
	cap mkdir "$wrk/queries"
	global queries "$wrk/queries"
	cd "$wrk"

	global runquery 1
/*
**Forms needed: BOE, MAT_ENDPOINTS, MNH04, MNH09, MNH12, MNH19, MNH23

**Note: this do file needs the following constructed files:
"$outcomes/MAT_ENROLL.dta" 
"$outcomes/MAT_ENDPOINTS.dta"
"$outcomes/MAT_UTERINERUP.dta"
"$outcomes/MAT_HEMORRHAGE.csv"
"$outcomes/MAT_INFECTION.csv"
"$outcomes/MAT_LABOR.dta"
"$outcomes/MAT_ANEMIA.dta"

*/

**# MNH04 - ANC clinical status
		
**we only need hospitalization information
	*keep hospitalization variables
	*generate a short and wide file of those who reported hospitalization
	import delimited "$da/mnh04_merged.csv", ///
	bindquote(strict) case(upper) clear

	rename M04_* *
	

	**JUST NEED HOSPITALIZATION
	tab HOSP_OHOOCCUR, m // admitted to a health facility since last visit
	gen HOSPITALIZED = 1 if HOSP_OHOOCCUR == 1
	tab HOSP_OHOREAS //specify reason 
	gen HOSP_REASON = HOSP_OHOREAS
	replace HOSP_REASON = "" if ///
	regexm(HOSP_OHOREAS, "n/a") | HOSP_OHOREAS=="55" | HOSP_OHOREAS=="77"

	*keep only if woman reports hospitalization
	keep if HOSPITALIZED == 1
	keep SITE MOMID PREGID HOSPITALIZED HOSP_REASON HOSP_OHOSTDAT HOSP_OHOENDAT ANC_OBSSTDAT 
	sort SITE MOMID PREGID ANC_OBSSTDAT
	
	*generate # visits to the hospital
	by MOMID PREGID (HOSPITALIZED ANC_OBSSTDAT) , sort: gen hospnum = _n 
	tab hospnum, m
	return list
	local hospnum = `r(N)'
	
	*reshape wide based on # visits to the hospital
	reshape wide HOSPITALIZED HOSP_REASON HOSP_OHOSTDAT HOSP_OHOENDAT ANC_OBSSTDAT, i(SITE MOMID PREGID) j(hospnum)
	
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
	several outcomes:
	- organ dysfunction/failure
	- sepsis (organ dysfunction resulting from infection)
	- pulmonary edema
	- placenta accrete
	- placental abruption
	- hysterectomy
	- indicator that a woman has an MNH09
	
	Note:should be only one MNH09 per participant, 
		no need for collapse or reshape
	*/
	import delimited "$da/mnh09_merged.csv", ///
	bindquote(strict) case(upper) clear

	rename M09_* *
	
	tab1 ORG_FAIL_MHOCCUR_1 ORG_FAIL_MHOCCUR_2 ORG_FAIL_MHOCCUR_3 ORG_FAIL_MHOCCUR_4 ORG_FAIL_MHOCCUR_5 ORG_FAIL_MHOCCUR_6 ORG_FAIL_MHOCCUR_7 ORG_FAIL_MHOCCUR_77 ORG_FAIL_MHOCCUR_88 ORG_FAIL_MHOCCUR_99 ORG_FAIL_SPFY_MHTERM
	
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
	
	gen CHK_ORG_FAIL_SPFY = 1 if ///
	ORG_FAIL_MHOCCUR_88 == 1 | ORG_FAIL_SPFY_MHTERM!="n/a"
	label var CHK_ORG_FAIL_SPFY ///
	"Organ failure specified, examine responses"
	list ORG_FAIL_MHOCCUR_1 ORG_FAIL_MHOCCUR_2 ORG_FAIL_MHOCCUR_3 ORG_FAIL_MHOCCUR_4 ORG_FAIL_MHOCCUR_5 ORG_FAIL_MHOCCUR_6 ORG_FAIL_MHOCCUR_7  ORG_FAIL_MHOCCUR_88 ORG_FAIL_SPFY_MHTERM  if CHK_ORG_FAIL_SPFY ==1
	**check these responses to see if they are captured elsewhere
	egen check_other_orgfail = anymatch(ORG_FAIL_MHOCCUR_1 ORG_FAIL_MHOCCUR_2 ORG_FAIL_MHOCCUR_3 ORG_FAIL_MHOCCUR_4 ORG_FAIL_MHOCCUR_5 ORG_FAIL_MHOCCUR_6 ORG_FAIL_MHOCCUR_7),v(1)
	assert check_other_orgfail == 1 if CHK_ORG_FAIL_SPFY == 1
	//This will stop the do file if  CHK_ORG_FAIL_SPFY is not captured elsehwere
	drop check_other_orgfail
	
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
	
	
	*Pulmonary edema
	tab PREECLAMPSIA_CEOCCUR_4, m  
	//pulmonary edema is a severe feature of preeclampsia
	gen PULMONARY_EDEMA_M09 = 1 if 	PREECLAMPSIA_CEOCCUR_4 == 1
	
	*Placenta accrete
	tab CES_PRINDC_INF1_17 CES_PRINDC_INF2_17
	gen PLACENTA_ACCRETE = 1 if ///
	(CES_PRINDC_INF1_17 == 1 | CES_PRINDC_INF2_17 == 1 | ///
	CES_PRINDC_INF3_17 == 1 | CES_PRINDC_INF4_17 == 1)	
	**if c-section was done because of placenta accrete
	
	*Placenta abruption
	gen PLACENTA_ABRUPTION = 1 if ///
	APH_FAORRES_1 ==1 | /// placental abruption leading to APH
	CES_PRINDC_INF1_6 == 1 | /// placental abruption leading to C-section
	CES_PRINDC_INF2_6 == 1 | /// placental abruption leading to C-section
	CES_PRINDC_INF3_6 == 1 | /// placental abruption leading to C-section
	CES_PRINDC_INF4_6 == 1  // placental abruption leading to C-section
	
	tab PLACENTA_ABRUPTION SITE
	
	gen HYSTERECTOMY_M09 = 1 if PPH_FAORRES_5 == 1
	**note that "did mother have ruptured uterus (repaired/hysterectomy)?" is not specific enough to include in this variable
	tab HYSTERECTOMY_M09 SITE
	
	gen M09 =1 if MAT_VISIT_MNH09 <= 2
	label var M09 "Completed M09 form for this participant"
	//note that we are currently excluding those whose visit was not completed for various reasons
	
	keep SITE MOMID PREGID MAT_VITAL_MNH09 MAT_VISIT_MNH09 MAT_VISIT_OTHR_MNH09 MAT_LD_OHOSTDAT DELIV_DSSTDAT_INF1 MAT_DEATH_DTHDAT ORG_FAIL_HRT ORG_FAIL_RESP ORG_FAIL_RENAL ORG_FAIL_LIVER ORG_FAIL_NEUR ORG_FAIL_UTER ORG_FAIL_HEM ORG_FAIL_MHOCCUR_88 ORG_FAIL_SPFY_MHTERM ORG_FAIL_M09 CHK_ORG_FAIL_SPFY MISS_SOURCE PPH_TRNSFSN_PROCCUR PLACENTA_ACCRETE PLACENTA_ABRUPTION M09 HYSTERECTOMY_M09 PULMONARY_EDEMA_M09
	order SITE MOMID PREGID MAT_VITAL_MNH09 MAT_VISIT_MNH09 MAT_VISIT_OTHR_MNH09 MAT_LD_OHOSTDAT DELIV_DSSTDAT_INF1 MAT_DEATH_DTHDAT ORG_FAIL_HRT ORG_FAIL_RESP ORG_FAIL_RENAL ORG_FAIL_LIVER ORG_FAIL_NEUR ORG_FAIL_UTER ORG_FAIL_HEM ORG_FAIL_MHOCCUR_88 ORG_FAIL_SPFY_MHTERM ORG_FAIL_M09 CHK_ORG_FAIL_SPFY MISS_SOURCE PLACENTA_ACCRETE PLACENTA_ABRUPTION HYSTERECTOMY_M09 PULMONARY_EDEMA_M09
	
	gen site= SITE
	gen momid=MOMID
	gen pregid=PREGID
	merge 1:1  MOMID PREGID using "$outcomes/MAT_ENROLL.dta" , ///
	keepusing(SITE ENROLL REMAPP PREG_START_DATE)
	keep if ENROLL == 1

	save "$wrk/mnh09.dta", replace

**complete the following after mat_endpoints has been created	
	**Calculate the 42 day post partum window
	use "$outcomes/MAT_ENDPOINTS.dta", clear
	merge 1:1 MOMID PREGID  using  "$wrk/mnh09.dta", nogen
	keep if ENROLL == 1
	*merge 1:1 MOMID PREGID using "$outcomes/MAT_UTERINERUP", nogen
	save "$wrk/mnh09.dta", replace
	
	gen uploaddate="$datadate"
	gen UploadDate=date(uploaddate, "YMD")
	format UploadDate %td
	drop uploaddate
	
/*
	gen EDD = date(EST_CONCEP_DATE, "YMD") + 280
	format EDD %td
*/
	
	*gen PP42 = PREG_END_DATE + 42 if PREG_END_DATE!=.
	*replace PP42 = EDD + 42 if PREG_END_DATE ==. & EDD!=.
	*Sometimes we don't know the woman's preg end date, but at some point we know that she is no longer pregnant for near-miss time purpose
	merge 1:1 SITE MOMID PREGID  using  "$outcomes/MAT_ENROLL.dta", nogen keepusing(EDD_BOE )
	str2date EDD_BOE
	 
	label var PREG_END_PP42_DT "PP42 days according to PREG_END_DATE or EDD"
	gen MISS_ENDPREG = 1 if PREG_END_DATE ==.
	
/*
	foreach var in  PP42   {
 	format `var' %td 
	replace `var'=. if PREG_END_DATE==. & EDD == . 
 }
*/
	gen PP42_PASS =0 if PREG_END_PP42_DT !=.
	replace PP42_PASS = 1 if PREG_END_PP42_DT<UploadDate & PREG_END_PP42_DT!=.
	replace PP42_PASS = 1 if (EDD_BOE+42)<UploadDate & PREG_END_PP42_DT == .
	gen PP42_PASS_edd = 1 if (EDD_BOE+42)<UploadDate & PREG_END_PP42_DT == .
	label var PP42_PASS_edd "EDD_BOE used to estimate 42 day window"
 	replace PP42_PASS = . if PREG_END_DATE==. & EDD_BOE ==. 
	
/*
	*Code uterine rupture
	gen UTERINE_RUP = 1 if  ///
	MAT_UTER_RUP==1 & ///
	(MAT_UTER_RUP_PREGEND==1 | MAT_UTER_RUP_PNC_DT<=PP42)
	label var UTERINE_RUP "Uterine rupture within 42 days PP"
*/
	//=1 if uterine rupture happened AND 
	//rupture occured at either L&D or within PNC6 window
	
	save "$wrk/mnh09.dta", replace
	

**# MNH10 - Postdelivery maternal outcome
	/*
	**Referral to ICU
	**Endometritis
	Note: only one MNH10 per participant, no need for reshape/collapse
	*/
	import delimited "$da/mnh10_merged.csv", ///
	bindquote(strict) case(upper) clear
	rename M10_* *
	
	gen MAT_ICU = 1 if ///
	TRANSFER_OHOLOC==1 | /// transferred to ICU in delivery facility
	TRANSFER_OHOLOC==2 // transferred to ICU in another facility
	label var MAT_ICU ///
	"Patient transferred to ICU in delivery or other facility"
	tab MAT_ICU SITE
	
	gen COMPL_M10 = 1 if MAT_DSTERM == 2
	label var COMPL_M10 ///
	"Mother had complication requiring higher level care"
	
	gen ENDOMETRITIS_M10 = 1 if POST_DEL_INF_CEOCCUR_1 == 1
	


	keep SITE MOMID PREGID MAT_ICU TRANSFER_OHOLOC ENDOMETRITIS_M10 COMPL_M10 MAT_VITAL_MNH10 MAT_VISIT_MNH10 VISIT_OBSSTDAT
	save "$wrk/mnh10.dta" , replace	

**# MNH19 Hospitalization
	/*
	Organ failure/dysfunction
	Transfusion -  mother received
	Laparotomy (includes bowel resection and hysterectomy)
	Hysterectomy
	Abortion complications
	Endometritis
	*/
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
	
	

	
	gen TRANSFUSION = 1 if TX_PROCCUR_1 == 1 //mother received transfusion
	gen LAPAROTOMY = 1 if ///
	TX_PROCCUR_2==1 | TX_PROCCUR_3 == 1 | TX_PROCCUR_5 == 1
	label var LAPAROTOMY ///
	"Laparotomy incl. hysterectomy & bowel resection"
	gen WOUNDCARE = 1 if TX_PROCCUR_4 == 1
	gen VENTILATION = 1 if TX_PROCCUR_6 == 1
	
	gen CHK_INTERVENTION_ORGFAIL  = 1 if ///
	(TRANSFUSION == 1 | LAPAROTOMY == 1 | VENTILATION == 1 ) & ///
	ORG_FAIL_M19!=1 
	label var CHK_INTERVENTION_ORGFAIL ///
	"Critical intervention but no organ failure reported"
	
	gen HOSP_DATE_RECORD = date( OHOSTDAT, "YMD")
	replace HOSP_DATE_RECORD = . if HOSP_DATE_RECORD < 0
	
	gen HOSP_DATE_EST = date(MAT_EST_OHOSTDAT, "YMD")
	replace HOSP_DATE_EST = . if HOSP_DATE_EST < 0 
	
	gen HOSP_DATE = HOSP_DATE_RECORD
	replace HOSP_DATE = HOSP_DATE_EST if HOSP_DATE_RECORD == . 
	
	gen ADMIT_INPATIENT_M19 = 1 if ///
	VISIT_FAORRES == 3 // patient admitted at this location for in-patient care
	gen REFERRED_M19 = 1 if ///
	VISIT_FAORRES == 4 | /// patient referred
	ADMIT_DSTERM == 2 //patient admitted then referred
	
	gen PULM_EDEMA_M19 = 1 if ///
	PREECLAMPSIA_CEOCCUR_4 == 1 | ///severe feature of PE
	HTN_MHTERM_8 == 1 | /// type of HDP
	DX_OTHR_MHTERM_4 == 1 // "other medical condition"
	
	gen HYSTERECTOMY_M19 = 1 if TX_PROCCUR_5 == 1 
	gen ABORT_COMPL_M19 = 1 if EARLY_LOSS_MHTERM==6
	gen ENDOMETRITIS_M19 = 1 if INFECTION_MHTERM_16 == 1
	
	keep SITE MOMID PREGID HOSP_DATE_RECORD HOSP_DATE_EST  HOSP_DATE OBSSTDAT VISIT_OBSLOC VISIT_FAC_SPFY_OBSLOC VISIT_OTHR_SPFY_OBSLOC TIMING_OHOCAT OHOSTDAT_YN OHOSTDAT MAT_EST_OHOSTDAT CARE_OHOLOC CARE_HOSP_OHOLOC MAT_ARRIVAL_DSDECOD ORG_FAIL_HRT_M19 ORG_FAIL_RESP_M19 ORG_FAIL_RENAL_M19 ORG_FAIL_LIVER_M19 ORG_FAIL_NEUR_M19 ORG_FAIL_UTER_M19 ORG_FAIL_HEM_M19 ORG_FAIL_OTHR_M19 ORG_FAIL_SPFY_M19 ORG_FAIL_M19 CHK_ORG_FAIL_SPFY CHK_INTERVENTION_ORGFAIL ORG_FAIL_M19 TX_PROCCUR_1 TX_PROCCUR_2 TX_PROCCUR_3 TX_PROCCUR_4 TX_PROCCUR_5 TX_PROCCUR_6 TX_PROCCUR_77 TX_PROCCUR_88 TX_PROCCUR_99 TX_PRTRT LAPAROTOMY TRANSFUSION VENTILATION WOUNDCARE PRIMARY_MHTERM PREG_DSTERM PREG_FAORRES VISIT_FAORRES ADMIT_DSTERM DTHDAT ADMIT_INPATIENT_M19 REFERRED_M19 PULM_EDEMA_M19 ENDOMETRITIS_M19 ABORT_COMPL_M19 HYSTERECTOMY_M19
	
	*Indicator variable that an MNH19 was filled out
	*Used to construct the "unplanned hospitalization" variable (PLTC)
	gen M19 = 1 
	label var M19 "M19 form available"
	
	

	

///Merge MNH19 to MNH09 to create near-miss.dta

	merge m:1 MOMID PREGID using "$wrk/mnh09.dta", ///
	keepusing(PREG_END_DATE EDD_BOE PREG_END_PP42_DT) gen(mergewindow)
	keep if mergewindow == 3
	
	**Only consider if the hopsitalization events took place within pregnancy up to 42 days of postpartum
	gen within42 = 1 if ///
	HOSP_DATE<= PREG_END_PP42_DT & HOSP_DATE!=. & PREG_END_PP42_DT!=.
	//keep if hospitalization date less than PP42 window
	replace within42 = 1 if ///
	date(OBSSTDAT, "YMD") <= PREG_END_PP42_DT & OBSSTDAT !="" & PREG_END_PP42_DT!=.
	//keep if date of data collection less than PP42 window
	replace within42 = 1 if ///
	HOSP_DATE==. &  TIMING_OHOCAT==1
	//if hospitalization date is missing but timing variable indicates antenatal period
	

	
	
	**indicator for woman was seen alive 42+ days postpartum
	gen after42 = 1 if ///
	HOSP_DATE>= PREG_END_PP42_DT & HOSP_DATE!=. & PREG_END_PP42_DT!=.
	//=1 if hospitalization date was after 42 day period 
	replace after42 = 1 if ///
	after42==. & /// after42 is unknown
	date(OBSSTDAT, "YMD") >= PREG_END_PP42_DT & OBSSTDAT !="" & PREG_END_PP42_DT!=. & ///
	(VISIT_FAORRES != 5 & /// woman did not die
	VISIT_FAORRES != 99 & /// status not unknown
	ADMIT_DSTERM !=3 & /// did not die while hospitalized
	ADMIT_DSTERM != 99) //status not unknown
	//=1 if FORM date was completed after the 42 day period and the woman had not died/status not unknown
	
	order SITE MOMID PREGID PREG_END_DATE EDD PREG_END_PP42_DT HOSP_DATE OBSSTDAT within42 after42

	save "$wrk/mnh19_long.dta" , replace // the long data set
	
	*We only want to keep these outcomes if they occured within 42 days after pregnancy end
	foreach var in ORG_FAIL_M19 ORG_FAIL_HRT_M19 ORG_FAIL_RESP_M19 ORG_FAIL_RENAL_M19 ORG_FAIL_LIVER_M19 ORG_FAIL_NEUR_M19 ORG_FAIL_UTER_M19 ORG_FAIL_HEM_M19 ORG_FAIL_OTHR_M19 TRANSFUSION LAPAROTOMY WOUNDCARE VENTILATION CHK_INTERVENTION_ORGFAIL M19 ADMIT_INPATIENT_M19 REFERRED_M19 HYSTERECTOMY_M19 ABORT_COMPL_M19 M19 {
		replace `var' = . if within42 != 1
	}
	replace ORG_FAIL_SPFY_M19 = "" if within42 != 1

	///COLLAPSE
	collapse (max) ORG_FAIL_M19 ORG_FAIL_HRT_M19 ORG_FAIL_RESP_M19 ORG_FAIL_RENAL_M19 ORG_FAIL_LIVER_M19 ORG_FAIL_NEUR_M19 ORG_FAIL_UTER_M19 ORG_FAIL_HEM_M19 ORG_FAIL_OTHR_M19 TRANSFUSION LAPAROTOMY WOUNDCARE VENTILATION CHK_INTERVENTION_ORGFAIL ADMIT_INPATIENT_M19 REFERRED_M19 M19 within42 after42 PULM_EDEMA_M19 ENDOMETRITIS_M19 ABORT_COMPL_M19 HYSTERECTOMY_M19 (firstnm) ORG_FAIL_SPFY_M19, by(SITE MOMID PREGID)
	rename TRANSFUSION TRANSFUSION_M19
	rename LAPAROTOMY LAPAROTOMY_M19
	rename WOUNDCARE WOUNDCARE_M19
	rename VENTILATION VENTILATION_M19
	rename CHK_INTERVENTION_ORGFAIL CHK_INTERVENTION_ORGFAIL_M19
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
	*/
	import delimited "$da/mnh12_merged.csv", ///
	bindquote(strict) case(upper) clear
	
	merge m:1 MOMID PREGID using "$wrk/mnh09.dta", ///
	keepusing(ENROLL PREG_END_DATE PREG_END_PP42_DT)
	keep if _merge==3
	**keep those enrolled and have an mnh12 form
	gen within42_M12 = 1 if ///
	date(M12_VISIT_OBSSTDAT, "YMD") <= PREG_END_PP42_DT & M12_MAT_VISIT_MNH12<=2
	label var within42_M12 "Observed <= 42 days of preg end"
	
	gen after42_M12 = 1 if  ///
	date(M12_VISIT_OBSSTDAT, "YMD") >= PREG_END_PP42_DT  & M12_MAT_VISIT_MNH12<=2
	replace after42_M12 = . if M12_VISIT_OBSSTDAT=="" | PREG_END_PP42_DT == . 
	label var after42_M12 "Seen >= 42 days after preg end"
	*note that both the after 42 and within 42 include 42 because some sites will close out a women exactly 42 days after her miscarriage
	*we want to capture that we saw her on day 42+ because of how near-miss-denominator is calculated
tab1 M12_ORG_FAIL_MHOCCUR_1 M12_ORG_FAIL_MHOCCUR_2 M12_ORG_FAIL_MHOCCUR_3 M12_ORG_FAIL_MHOCCUR_4  M12_ORG_FAIL_MHOCCUR_88
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
	
	
	gen HYSTERECTOMY_M12 = 1 if M12_BIRTH_COMPL_MHTERM_8 == 1 
	**birth complication == hysterectomy
	
	gen PULMONARY_EDEMA_M12 = 1 if M12_PULM_EDEMA_MHOCCUR == 1
	gen HOSPITALIZED_M12 = 1 if M12_HOSP_LAST_VISIT_OHOOCCUR==1 //mother hospitalized
	gen M12 = 1 if M12_MAT_VISIT_MNH12 <=2 // =1 if visit completed
	
	foreach var in ORG_FAIL_HRT_M12 ORG_FAIL_RESP_M12 ORG_FAIL_RENAL_M12 ORG_FAIL_LIVER_M12 ORG_FAIL_OTHR_M12 ORG_FAIL_M12  HYSTERECTOMY_M12 PULMONARY_EDEMA_M12 HOSPITALIZED_M12 M12 {
	replace `var' = . if within42_M12 != 1	
	}
	replace ORG_FAIL_SPFY_M12 = "" if within42_M12 != 1

	//gen Infant age
	gen INFAGE_M12 = date(M12_VISIT_OBSSTDAT, "YMD") - PREG_END_DATE
	
	keep SITE MOMID PREGID M12_VISIT_OBSSTDAT INFAGE_M12 ORG_FAIL_HRT_M12 ORG_FAIL_RESP_M12 ORG_FAIL_RENAL_M12 ORG_FAIL_LIVER_M12 ORG_FAIL_OTHR_M12 ORG_FAIL_SPFY_M12 ORG_FAIL_M12 within42_M12 after42_M12 HYSTERECTOMY_M12 PULMONARY_EDEMA_M12 HOSPITALIZED_M12 M12 
	
	order   SITE MOMID PREGID M12_VISIT_OBSSTDAT INFAGE_M12 within42_M12 after42_M12 ORG_FAIL_M12 ORG_FAIL_HRT_M12 ORG_FAIL_RESP_M12 ORG_FAIL_RENAL_M12 ORG_FAIL_LIVER_M12 ORG_FAIL_OTHR_M12 ORG_FAIL_SPFY_M12 HYSTERECTOMY_M12 PULMONARY_EDEMA_M12 HOSPITALIZED_M12 M12


 save "$wrk/mnh12_long.dta", replace //long data set
 
 //collapse
 collapse (max) ORG_FAIL_HRT_M12 ORG_FAIL_RESP_M12 ORG_FAIL_RENAL_M12 ORG_FAIL_LIVER_M12 ORG_FAIL_OTHR_M12 ORG_FAIL_M12 within42_M12 after42_M12 HYSTERECTOMY_M12 PULMONARY_EDEMA_M12 HOSPITALIZED_M12 M12 (firstnm) ORG_FAIL_SPFY_M12 , by(SITE MOMID PREGID)
 
 
	
**# Merge MNH12 with other near-miss files
	merge 1:1 MOMID PREGID using "$wrk/near-miss.dta", gen(M12_merge)
	merge 1:1 MOMID PREGID using "$wrk/mnh10.dta", gen(mnh10merge)
	merge 1:1 MOMID PREGID using "$wrk/mnh04_wide.dta", gen(m04_merge)


**# Generate summary Near-Miss variables
	gen ORG_FAIL = 1 if ORG_FAIL_M12==1 | ORG_FAIL_M09==1 | ORG_FAIL_M19==1
	label var ORG_FAIL "Maternal organ failure at L&D, PNC, Hosp"

	gen ORG_FAIL_HRT = 1 if ORG_FAIL_HRT_M09==1 | ///
	ORG_FAIL_HRT_M12==1 | ORG_FAIL_HRT_M19==1
	
	gen ORG_FAIL_RESP = 1 if ORG_FAIL_RESP_M12 ==1 | ///
	ORG_FAIL_RESP_M09==1 | ORG_FAIL_RESP_M19==1
	
	gen ORG_FAIL_RENAL = 1 if ORG_FAIL_RENAL_M12==1 | ///
	ORG_FAIL_RENAL_M09==1 | ORG_FAIL_RENAL_M19==1
	
	gen ORG_FAIL_LIVER = 1 if ORG_FAIL_LIVER_M12==1 | ///
	ORG_FAIL_LIVER_M09==1 |  ORG_FAIL_LIVER_M19==1
	
	gen ORG_FAIL_NEUR = 1 if ///
	ORG_FAIL_NEUR_M09==1 | ORG_FAIL_NEUR_M19==1
	*not recorded at MNH12
	
	gen ORG_FAIL_UTER = 1 if ///
	ORG_FAIL_UTER_M09==1 |  ORG_FAIL_UTER_M19==1
	*not recorded at MNH12
	
	gen ORG_FAIL_HEM =1 if ///
	ORG_FAIL_HEM_M09 == 1 | ORG_FAIL_HEM_M19 == 1
	*not recorded at MNH12
	
	gen ORG_FAIL_OTHR = 1 if ORG_FAIL_OTHR_M12 ==1 | ///
	ORG_FAIL_MHOCCUR_88 ==1 | ORG_FAIL_OTHR_M19==1
	
	
	
	drop SITE
	merge 1:1 MOMID PREGID using "$outcomes/MAT_ENROLL.dta", ///
	keepusing (SITE) nogen
	keep if ENROLL == 1
	order SITE MOMID PREGID
	
	save "$wrk/near-miss.dta", replace
	
/*
	**EXPORT A SHORT FILE FOR ERIN'S HDP FILE
	keep SITE MOMID PREGID ORG_FAIL ORG_FAIL_HRT ORG_FAIL_RESP ORG_FAIL_RENAL ORG_FAIL_LIVER ORG_FAIL_NEUR ORG_FAIL_UTER ORG_FAIL_HEM ORG_FAIL_OTHR ORG_FAIL_M12 ORG_FAIL_M09 ORG_FAIL_M19
	save "$outcomes/MAT_ORG_FAIL.dta" , replace
*/
	**!!  discuss with Emily + Erin:
	*all organ dysfunction for HDP, or only those that occur during pregnancy + L&D?
	*any organ dysfunction, or only some organs? what is relevant for HDP?

	**#Stop here 2025-01-14 SFO
	
**Maternal infection
	*Malaria & TB (both PLTC), add HIV for context
	import delimited "$outcomes/MAT_INFECTION.csv", ///
	bindquote(strict) case(upper) clear 
	
	gen MALARIA = 1 if MAL_EVER_PREG == 1
	/// MAL_POSITIVE_ENROLL==1 | MAL_POSITIVE_ANY_ANC== 1
		*removed for April 18 2025 data set
	
	gen TB = 1 if TB_SPUTUM_EVER_PREG == 1 
	///TB_SPUTUM_POSITIVE_ENROLL == 1  | 	///TB_SPUTUM_POSITIVE_ANY_ANC == 1
		*removed for April 18 2025 data set	
	
	gen HIV = 1 if HIV_EVER_PREG == 1 
	/// HIV_POSITIVE_ENROLL==1 | HIV_POSITIVE_ANY_ANC==1
			*removed for April 18 2025 data set	
	
	keep SITE MOMID PREGID MALARIA TB HIV
	save "$wrk/MAT_INF_SHORT.dta", replace
	
**Maternal hemorrhage	
	import delimited "$outcomes/MAT_HEMORRHAGE.csv", ///
	bindquote(strict) case(upper) clear
	keep SITE MOMID PREGID HEM_PPH HEM_PPH_SEV HEM_APH
	duplicates drop
	isid MOMID PREGID
	save "$wrk/hemorrhage.dta", replace
	
	use "$wrk/near-miss.dta", clear
	merge 1:1 MOMID PREGID using "$wrk/hemorrhage.dta", nogen force
	merge 1:1 MOMID PREGID using "$outcomes/MAT_LABOR.dta", nogen
	merge 1:1 MOMID PREGID using "$wrk/MAT_INF_SHORT.dta", nogen
	merge 1:1 MOMID PREGID using "$outcomes/MAT_UTERINERUP", nogen

	replace momid= MOMID if momid==""
	replace pregid=PREGID if pregid==""
	
	save "$wrk/MAT_NEAR_MISS.dta", replace
	
	use "$wrk/MAT_NEAR_MISS.dta", clear
	
**Anemia	
	merge 1:1 SITE MOMID PREGID using "$outcomes/MAT_ANEMIA.dta", gen(anemia_merge) keepusing(ANEMIA_T1 ANEMIA_T2 ANEMIA_T3 ANEMIA_ANC)
	keep if ENROLL == 1
	
	gen ANEMIA_SEV_ANC = 0 if ANEMIA_ANC<55
	replace ANEMIA_SEV_ANC=1 if ANEMIA_ANC==3
	//need a binary indicator for PLTC calculation
	// 1 = severe anemia, 0 = otherwise (exclude missing)

	
	*Code uterine rupture
	gen UTERINE_RUP = 1 if  ///
	MAT_UTER_RUP_IPC==1 | ///
	(MAT_UTER_RUP_PNC==1 & MAT_UTER_RUP_PNC_DT<=PREG_END_PP42_DT)
	label var UTERINE_RUP "Uterine rupture within 42 days PP"

	//=1 if uterine rupture happened AND 
	//rupture occured at either L&D or within PNC6 window

	
	gen ENDOMETRITIS = 1  if ///
	ENDOMETRITIS_M19 ==1 | ENDOMETRITIS_M10 == 1 
	
	gen PULMONARY_EDEMA = 1 if ///
	PULM_EDEMA_M19 == 1 | ///context of HDP & "other medical condition"
	PULMONARY_EDEMA_M09 == 1 | ///in the context of HDP
	PULMONARY_EDEMA_M12 ==1 //in any context

	gen HOSPITALIZED = 1 if ///
	M19==1 | /// M19 filled out within 42 days after pregnancy end
	M04_HOSP==1 | /// reported admitted to hospital in MNH04
	HOSPITALIZED_M12==1
	**if hospitalization was indicated in M19, M04, or M12
	
	gen ABORT_COMPL = ABORT_COMPL_M19
	
	gen HYSTERECTOMY = 1 if ///
	HYSTERECTOMY_M12 == 1 | ///
	HYSTERECTOMY_M19 == 1 | ///
	HYSTERECTOMY_M09 == 1
	
	gen LAPAROTOMY = 1 if ///
	LAPAROTOMY_M19 == 1 | HYSTERECTOMY == 1
	
	*Transfusion and PRISMA Near-Miss transfusion
	*Near-miss transfusion: transfusion plus at least one other:
		*severe anemia, postpartum hemorrhage, antepartum hemorrhage, or placental abruption
	gen TRANSFUSION = 1 if ///
	PPH_TRNSFSN_PROCCUR==1 | /// did the mother NEED a transfusion? (M09)
	TRANSFUSION_M19==1 // did the mother RECEIVE a transfusion? (M19)
	label var TRANSFUSION "Transfusion indicated in M09 or M19"
	
	gen TRANSFUSION_NEARMISS = 1 if TRANSFUSION ==1 & ///
	(ANEMIA_SEV_ANC==1 | HEM_PPH_SEV == 1 | ///
	HEM_APH ==1 | PLACENTA_ABRUPTION == 1)
	label var TRANSFUSION_NEARMISS ///
	"Blood transfusion + hemorrhage, anemia, or placental abruption"
	*Note that this was previously any PPH, difference = 1 participant	
	*assert TRANSFUSION_NEARMISS== TRANSFUSION
	sort SITE
	browse SITE TRANSFUSION_M19 TRANSFUSION TRANSFUSION_NEARMISS ANEMIA_T1 ANEMIA_T2 ANEMIA_T3 HEM_PPH HEM_PPH_SEV MAT_ICU HOSPITALIZED MALARIA  ORG_FAIL  ENDOMETRITIS PULMONARY_EDEMA  HYSTERECTOMY LAPAROTOMY  TB HIV  if TRANSFUSION_NEARMISS==. & TRANSFUSION==1
	
	*NOTE: "SITE" var gets dropped sometimes; pull in from MAT_ENROLL

	assert SITE != ""
	assert MOMID != ""
	assert PREGID != ""
	
	/*
		drop SITE site
		merge 1:1 MOMID PREGID using "$outcomes/MAT_ENROLL.dta", ///
		keepusing (SITE) nogen
		keep if ENROLL == 1
		order SITE MOMID PREGID
	*/
	save "$wrk/near-miss.dta", replace
	
