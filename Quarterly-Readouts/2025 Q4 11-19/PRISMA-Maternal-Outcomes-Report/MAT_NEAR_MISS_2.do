**PRISMA MATERNAL NEAR-MISS
**OUTCOME CONSTRUCTION
/*
**NOTE: this file must be run after running '1.Near-Miss-Vars.do'

*NOTE: this file needs the following files:

"$wrk/near-miss.dta" (Part 1)
"$outcomes/MAT_HDP"

*/

**#Set paths
	*Change the below based on data date
	global datadate "2025-10-31"
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
	
	
	
**#Import maternal infection
	*Malaria & TB (both PLTC), add HIV for context
	import excel "$outcomes/MAT_INFECTION.xlsx", ///
		sheet("Sheet 1") firstrow case(upper) clear
	
	gen MALARIA = 1 if MAL_POSITIVE_EVER_PREG == 1
	/// MAL_POSITIVE_ENROLL==1 | MAL_POSITIVE_ANY_ANC== 1
		*removed for April 18 2025 data set
	
	gen TB = 1 if TB_CULT_POSITIVE_EVER_PREG == 1 
	///TB_SPUTUM_POSITIVE_ENROLL == 1  | 	///TB_SPUTUM_POSITIVE_ANY_ANC == 1
		*removed for April 18 2025 data set	
	
	gen HIV = 1 if HIV_POSITIVE_EVER_PREG == 1 
	/// HIV_POSITIVE_ENROLL==1 | HIV_POSITIVE_ANY_ANC==1
			*removed for April 18 2025 data set	
	str2date HIV_DATE_POSITIVE MAL_POSITIVE_DATE TB_CULT_POSITIVE_DATE
	
	keep SITE MOMID PREGID MALARIA TB HIV MAL_POSITIVE_DATE TB_CULT_POSITIVE_DATE HIV_DATE_POSITIVE
	
	save "$wrk/MAT_INF_SHORT.dta", replace
	
**#Import Maternal hemorrhage file
	import delimited "$outcomes/MAT_HEMORRHAGE.csv", ///
	bindquote(strict) case(upper) clear
	keep SITE MOMID PREGID HEM_PPH HEM_PPH_SEV HEM_APH HEM_APH_DATE HEM_APH_GESTAGE_DAYS HEM_PPH_DATE HEM_PPH_AGE_PP_DAYS HEM_PPH_SEV_DATE HEM_PPH_SEV_AGE_PP_DAYS
	duplicates drop
	isid MOMID PREGID
	save "$wrk/hemorrhage.dta", replace
	
	
**#Maternal anemia	
use "Z:\Outcome Data/$datadate/ANEMIA_all_long.dta", clear
	
	global recastlength = strlen(PREGID) + 1
	recast str$recastlength PREGID, force
	
	*Keep one hemoglobin measurement per mother x date combination
	bysort MOMID PREGID TEST_DATE (TEST_TYPE HB_LBORRES) : gen hb_num=_n
		*sorts alphabetically by TEST_TYPE (CBC will come first)
		*sorts by HB_LBORRES with the lowest HB coming first
	keep if hb_num == 1
		*keep only the first
	
	*Keep only ANC tests:
	keep if inrange(TEST_GA, 28,310) & inlist( TYPE_VISIT,1,2,3,4,5,13)
	
	gen ANEMIA_SEV_ANC = 0 if !missing(HB_LBORRES)
	replace ANEMIA_SEV_ANC = 1 if HB_LBORRES<7
	
	*How many severe anemia measurements per PREGID?
	gen severe = 1 if ANEMIA_SEV_ANC==1
	bysort PREGID severe : gen totalsevere= _N if severe == 1
	
	*date of first severe anemia?
	gen anemia_sev_anc_dt = TEST_DATE if ANEMIA_SEV_ANC==1
	bysort PREGID : egen ANEMIA_SEV_ANC_DT = min(anemia_sev_anc_dt)
	
	*Keep short file, one row per PREGID
	keep SITE MOMID PREGID ANEMIA_SEV_ANC ANEMIA_SEV_ANC_DT
	collapse (max) ANEMIA_SEV_ANC (min) ANEMIA_SEV_ANC_DT, by(SITE MOMID PREGID)
	save "$wrk/ANEMIA-SEVERE", replace
	
/*
	**EXPORT A SHORT FILE FOR ERIN'S HDP FILE
	keep SITE MOMID PREGID ORG_FAIL ORG_FAIL_HRT ORG_FAIL_RESP ORG_FAIL_RENAL ORG_FAIL_LIVER ORG_FAIL_NEUR ORG_FAIL_UTER ORG_FAIL_HEM ORG_FAIL_OTHR ORG_FAIL_M12 ORG_FAIL_M09 ORG_FAIL_M19
	save "$outcomes/MAT_ORG_FAIL.dta" , replace
*/
	**!!  discuss with Emily + Erin:
	*all organ dysfunction for HDP, or only those that occur during pregnancy + L&D?
	*any organ dysfunction, or only some organs? what is relevant for HDP?


	
	
	use "$wrk/near-miss.dta", clear
	merge 1:1 MOMID PREGID using "$wrk/hemorrhage.dta", nogen force
	merge 1:1 MOMID PREGID using "$outcomes/MAT_LABOR.dta", nogen
	merge 1:1 MOMID PREGID using "$wrk/MAT_INF_SHORT.dta", nogen
	merge 1:1 MOMID PREGID using "$outcomes/MAT_UTERINERUP", nogen

	replace momid= MOMID if momid==""
	replace pregid=PREGID if pregid==""

	merge 1:1 SITE MOMID PREGID using "$wrk/ANEMIA-SEVERE", gen(anemia_merge) 
	keep if ENROLL == 1
	
	
**# Generate summary near-miss variables:

	**#Organ failure - summary
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
	
	gen ORG_FAIL_M09_DT = PREG_END_DATE if ORG_FAIL_M09==1
	gen ORG_FAIL_DT = min(ORG_FAIL_M09_DT, ORG_FAIL_M12_DT, ORG_FAIL_M19_DT) if ORG_FAIL == 1
	
	drop SITE
	merge 1:1 MOMID PREGID using "$outcomes/MAT_ENROLL.dta", ///
	keepusing (SITE) nogen
	keep if ENROLL == 1
	order SITE MOMID PREGID
	
	
	**#Uterine rupture - summary variable
	gen UTERINE_RUP = 1 if  ///
	MAT_UTER_RUP_IPC==1 | ///
	MAT_UTER_RUP_PNC==1
	label var UTERINE_RUP "Uterine rupture any time"
	*old - we now want to capture uterine rupture any time and filter later
	*(MAT_UTER_RUP_PNC==1 & MAT_UTER_RUP_PNC_DT<=PREG_END_PP42_DT)
	*label var UTERINE_RUP "Uterine rupture within 42 days PP"
	//=1 if uterine rupture happened AND 
	//rupture occured at either L&D or within PNC6 window
	
	*Date of first uterine rupture:
	gen MAT_UTER_RUP_IPC_DT = PREG_END_DATE if MAT_UTER_RUP_IPC==1
	gen UTERINE_RUP_DT = min(MAT_UTER_RUP_IPC_DT, MAT_UTER_RUP_PNC_DT) if UTERINE_RUP == 1
	
	
	**#Endometritis - summary variable
	gen ENDOMETRITIS = 1  if ///
	ENDOMETRITIS_M19 ==1 | ENDOMETRITIS_M10 == 1 
	gen ENDOMETRITIS_DT = min( ENDOMETRITIS_M10_DT, ENDOMETRITIS_M19_DT) if ENDOMETRITIS==1
	
	
	**#Pulmonary edema - summary variable
	gen PULMONARY_EDEMA = 1 if ///
	PULM_EDEMA_M19 == 1 | ///context of HDP & "other medical condition"
	PULMONARY_EDEMA_M09 == 1 | ///in the context of HDP
	PULMONARY_EDEMA_M12 ==1 //in any context
	
	*Date of first pulmonary edema:
	gen PULMONARY_EDEMA_M09_DT = PREG_END_DATE if PULMONARY_EDEMA_M09==1
	gen PULMONARY_EDEMA_DT = min(PULMONARY_EDEMA_M09_DT, PULM_EDEMA_M19_DT, PULMONARY_EDEMA_M12_DT) if PULMONARY_EDEMA==1
		
	
	**#Ever hospitalized - summary variable
	gen HOSPITALIZED = 1 if ///
	M19==1 | /// M19 filled out within 42 days after pregnancy end
	M04_HOSP==1 | /// reported admitted to hospital in MNH04
	HOSPITALIZED_M12==1
	**if hospitalization was indicated in M19, M04, or M12
	*Date of first hospitalization:
	gen HOSPITALIZED_DT = min( M04_HOSP_OHOSTDAT1, M19_DT,HOSPITALIZED_M12_DT)
		*note that HOSPITALIZED_M12_DT will match M19_DT if recorded, or will be the midpoint based on window from MNH12
	
	
	**#Abortion complication - summary variable
	gen ABORT_COMPL = ABORT_COMPL_M19
	rename ABORT_COMPL_M19_DT ABORT_COMPL_DT
	
	**#Hysterectomy - summary variable
	gen HYSTERECTOMY = 1 if ///
	HYSTERECTOMY_M12 == 1 | ///
	HYSTERECTOMY_M19 == 1 | ///
	HYSTERECTOMY_M09 == 1	
	
	*Date of hysterectomy:
	gen HYSTERECTOMY_M09_DT = PREG_END_DATE if HYSTERECTOMY_M09 == 1
	gen HYSTERECTOMY_DT = min(HYSTERECTOMY_M09_DT,HYSTERECTOMY_M12_DT, HYSTERECTOMY_M19_DT ) if HYSTERECTOMY == 1
	
	**#Laparotomy - summary variable
	gen LAPAROTOMY = 1 if ///
	LAPAROTOMY_M19 == 1 | HYSTERECTOMY == 1
	gen LAPAROTOMY_DT = min(HYSTERECTOMY_DT, LAPAROTOMY_M19_DT)	if LAPAROTOMY == 1 
	
	
	**#Transfusion - summary variables
	gen TRANSFUSION = 1 if ///
	PPH_TRNSFSN_PROCCUR==1 | /// did the mother NEED a transfusion? (M09)
	TRANSFUSION_M19==1 // did the mother RECEIVE a transfusion? (M19)
	label var TRANSFUSION "Transfusion indicated in M09 or M19"
	gen TRANSFUSION_M09_DT = PREG_END_DATE if PPH_TRNSFSN_PROCCUR==1	
	gen TRANSFUSION_DT = min(TRANSFUSION_M09_DT, TRANSFUSION_M19_DT)
	
	
	*Near-miss transfusion: transfusion plus at least one other:
		*severe anemia
		*postpartum hemorrhage
		*antepartum hemorrhage
		*placental abruption
	gen TRANSFUSION_NEARMISS = 1 if TRANSFUSION ==1 & ///
	(ANEMIA_SEV_ANC==1 | HEM_PPH_SEV == 1 | ///
	HEM_APH ==1 | PLACENTA_ABRUPTION == 1)
	label var TRANSFUSION_NEARMISS ///
	"Blood transfusion + hemorrhage, anemia, or placental abruption"
	*Note that this was previously any PPH, difference = 1 participant	
	*assert TRANSFUSION_NEARMISS== TRANSFUSION
	
	
	
	
	sort SITE
	*browse SITE TRANSFUSION_M19 TRANSFUSION TRANSFUSION_NEARMISS ANEMIA_* HEM_PPH HEM_PPH_SEV MAT_ICU HOSPITALIZED MALARIA  ORG_FAIL  ENDOMETRITIS PULMONARY_EDEMA  HYSTERECTOMY LAPAROTOMY  TB HIV  if TRANSFUSION_NEARMISS==. & TRANSFUSION==1
	
	
	for var *_DT : format X %td
	
	
	*NOTE: "SITE" var gets dropped sometimes; pull in from MAT_ENROLL

	assert SITE != ""
	assert MOMID != ""
	assert PREGID != ""
	
	save "$wrk/near-miss.dta", replace
	
	/*
	**#Query: MAT_ICU but no hospitalization forms
	preserve
		keep if MAT_ICU == 1
		cap drop _merge
		merge 1:1 SITE MOMID PREGID using "$wrk/mnh19_wide_pp"
		keep if MAT_ICU == 1
			

		*because mnh19_wide_pp is sorted by hosp date, HOSP_DATE1 is earliest hospitalization date
		gen ICU_QUERYNOTE = "No Hospitalization form" if _merge == 1
		
		replace ICU_QUERYNOTE = "Hosp form > window" if _merge == 3 & M19_HOSP_DATE1 > (MAT_ICU_DT + 7)
		
		levelsof(SITE) if ICU_QUERYNOTE!="" , local(sitelev) clean
		foreach site of local sitelev {
		export excel SITE MOMID PREGID M10_VISIT_OBSSTDAT TRANSFER_OHOLOC ICU_QUERYNOTE using "$queries/`site'-SHARE-Near-Miss-$datadate.xlsx" if ///
	ICU_QUERYNOTE!="" &  SITE == "`site'", ///
	sheet("ICU-Missing-MNH19", modify) firstrow(variables)  datestring("%tdDD-Mon-CCYY")
	}
		
		
	
	restore
	*/
	
use "$wrk/near-miss.dta" , clear

merge 1:1 MOMID PREGID using "$outcomes/MAT_HDP", nogen

/*
gen PE = 1 if HDP_GROUP>=3 & HDP_GROUP<=4
label var PE "Pre eclampsia without severe features"
gen PE_SEV=1 if HDP_GROUP == 5
label var PE_SEV "Preeclampsia with severe features/eclampsia"
*/


**#Generate variable PLTC ever
global PLTCs  HEM_PPH ENDOMETRITIS PLACENTA_ACCRETE PLACENTA_ABRUPTION UTERINE_RUP OBS_LABOR PRO_LABOR ABORT_COMPL PULMONARY_EDEMA LAPAROTOMY MAT_ICU TRANSFUSION HOSPITALIZED ANEMIA_SEV_ANC PREECLAMPSIA PREECLAMPSIA_SEV HIGH_BP_SEVERE_ANY MALARIA TB

gen PLTC = 1 if 					///
	HEM_PPH == 1 | 					/// any postpartum hemorrage
	ENDOMETRITIS == 1 | 			///	
	PLACENTA_ACCRETE == 1 | 		///
	PLACENTA_ABRUPTION == 1 | 		///	
	UTERINE_RUP == 1 |				///
	OBS_LABOR == 1 | 				///
	PRO_LABOR == 1 | 				///
	ABORT_COMPL == 1 | 				///
	PULMONARY_EDEMA == 1 | 			///	
	LAPAROTOMY == 1 | 				///
	MAT_ICU == 1 | 					///
	TRANSFUSION == 1 | 				///
	HOSPITALIZED == 1  |			/// hospitalization
	ANEMIA_SEV_ANC==1   |        	///
	PREECLAMPSIA_NONSEV == 1 |		/// PE w/o severe features
	PREECLAMPSIA_SEV ==1 |			/// PE w/severe features
	HIGH_BP_SEVERE_ANY ==1 | 		///
	MALARIA == 1 | 					///
	TB == 1
**note: update this code to include:
	**sepsis/severe infection, once available 
	
	gen PLTC_CRITERIA = PLTC
	label var PLTC_CRITERIA "Experienced 1+ PLTC"
	*We are creating this second variable which will not be set to zero for those not in the near-miss denominator
	
	*Create date variables for placental events & labor events:	
	*Clean up dates of events:
	
	for var PLACENTA_ACCRETE PLACENTA_ABRUPTION PRO_LABOR OBS_LABOR : ///
		gen X_DT = PREG_END_DATE if X == 1
		*create a "_DT" variable equal to pregnancy end date
			*these are only reported on MNH09
	
	*set to missing if event happens before preganncy start date
	str2date PREG_START_DATE HEM_APH_DATE HEM_PPH_DATE HEM_PPH_SEV_DATE

	for var PREG_END_DATE ENDOMETRITIS_DT PLACENTA_ACCRETE_DT PLACENTA_ABRUPTION_DT UTERINE_RUP_DT PRO_LABOR_DT OBS_LABOR_DT ABORT_COMPL_DT PULMONARY_EDEMA_DT  LAPAROTOMY_DT MAT_ICU_DT TRANSFUSION_DT   HOSPITALIZED_DT ANEMIA_SEV_ANC_DT PREECLAMPSIA_DATE  PREECLAMPSIA_SEV_DATE HEM_APH_DATE HEM_PPH_DATE HEM_PPH_SEV_DATE MAL_POSITIVE_DATE TB_CULT_POSITIVE_DATE HIV_DATE_POSITIVE: replace X = . if X < PREG_START_DATE
		*set to missing if date is before pregnancy start date
	
	for var PREG_END_DATE ENDOMETRITIS_DT PLACENTA_ACCRETE_DT PLACENTA_ABRUPTION_DT UTERINE_RUP_DT PRO_LABOR_DT OBS_LABOR_DT ABORT_COMPL_DT PULMONARY_EDEMA_DT  LAPAROTOMY_DT MAT_ICU_DT TRANSFUSION_DT   HOSPITALIZED_DT ANEMIA_SEV_ANC_DT PREECLAMPSIA_DATE  PREECLAMPSIA_SEV_DATE HEM_APH_DATE HEM_PPH_DATE HEM_PPH_SEV_DATE MAL_POSITIVE_DATE TB_CULT_POSITIVE_DATE HIV_DATE_POSITIVE: format X %td
	
	/*
	*set to missing if it happens > 42 days PP
	for var PREG_END_DATE ENDOMETRITIS_DT PLACENTA_ACCRETE_DT PLACENTA_ABRUPTION_DT UTERINE_RUP_DT PRO_LABOR_DT OBS_LABOR_DT ABORT_COMPL_DT PULMONARY_EDEMA_DT  LAPAROTOMY_DT MAT_ICU_DT TRANSFUSION_DT   HOSPITALIZED_DT ANEMIA_SEV_ANC_DT PREECLAMPSIA_DATE  PREECLAMPSIA_SEV_DATE: replace X = . if X > (PREG_END_DATE + 42)
	*set to missing if it happens > 42 days after EDD AND pregnancy end date not known	
	for var PREG_END_DATE ENDOMETRITIS_DT PLACENTA_ACCRETE_DT PLACENTA_ABRUPTION_DT UTERINE_RUP_DT PRO_LABOR_DT OBS_LABOR_DT ABORT_COMPL_DT PULMONARY_EDEMA_DT  LAPAROTOMY_DT MAT_ICU_DT TRANSFUSION_DT   HOSPITALIZED_DT ANEMIA_SEV_ANC_DT PREECLAMPSIA_DATE  PREECLAMPSIA_SEV_DATE: replace X = . if X > (EDD_BOE + 42) & PREG_END_DATE==.
	*these events are outside the near-miss window
	*/
	
	
	
	gen PLTC_DT = min( 		/// 
	HEM_APH_DATE ,			///
	HEM_PPH_DATE,  			///
	MAL_POSITIVE_DATE, 		///
	TB_CULT_POSITIVE_DATE,	///
	HIV_DATE_POSITIVE,		///
	ENDOMETRITIS_DT , 		///
	PLACENTA_ACCRETE_DT, 	///
	PLACENTA_ABRUPTION_DT , /// PLACENTAL DISORDERS
	UTERINE_RUP_DT, 		///
	PRO_LABOR_DT, 			///
	OBS_LABOR_DT, 			///
	ABORT_COMPL_DT, 		///
	PULMONARY_EDEMA_DT , 	///
	LAPAROTOMY_DT, 			///
	MAT_ICU_DT, 			///
	TRANSFUSION_DT , 		///
	HOSPITALIZED_DT, 		///
	ANEMIA_SEV_ANC_DT, 		///
	PREECLAMPSIA_DATE, 		/// 
	PREECLAMPSIA_SEV_DATE)  ///
	if PLTC==1  
		
	
	gen PLTC_GA = PLTC_DT - PREG_START_DATE if PLTC== 1
	gen PLTC_PP = PLTC_DT - PREG_END_DATE if PREG_END==1 & PLTC==1
	sum PLTC_GA PLTC_PP
	
	tabstat PLTC_PP, by(SITE) stats(min max)
	
	**!! temp: a second indicator of PLTC which does not consider prolonged labor or malaria
	gen PLTC2 = 1 if 				///
	HEM_PPH == 1 | 					///
	ENDOMETRITIS == 1 | 			///	
	PLACENTA_ACCRETE == 1 | 		///
	PLACENTA_ABRUPTION == 1 | 		///	
	UTERINE_RUP == 1 |				///
	OBS_LABOR == 1 | 				///
	ABORT_COMPL == 1 | 				///
	PULMONARY_EDEMA == 1 | 			///	
	LAPAROTOMY == 1 | 				///
	MAT_ICU == 1 | 					///
	TRANSFUSION == 1 | 				///
	HOSPITALIZED == 1  |			///
	ANEMIA_SEV_ANC==1   |        	///
	PREECLAMPSIA_NONSEV == 1 |				///
	PREECLAMPSIA_SEV ==1 |			///
	HIGH_BP_SEVERE_ANY ==1 | 		///
	TB == 1
	*does not include prolonged labor or malaria

	
**#Maternal near miss
	gen MAT_MNM_CRIT = 1 if 	///
	ORG_FAIL ==1 | 				/// organ dysfunction
	HYSTERECTOMY == 1 | 		/// 
	UTERINE_RUP == 1 | 			///
	LAPAROTOMY == 1 | 			///
	MAT_ICU==1 | 				///
	HEM_PPH_SEV==1 | 			/// severe postpartum hemorrhage
	TRANSFUSION_NEARMISS==1 | 	/// near-miss definition of transfusion
	PREECLAMPSIA_SEV == 1
	**NOTE:sepsis when available
	label var MAT_MNM_CRIT "Meets PRISMA near-miss criteria"
	global MNM_CRIT ORG_FAIL HYSTERECTOMY UTERINE_RUP LAPAROTOMY MAT_ICU HEM_PPH_SEV TRANSFUSION_NEARMISS PREECLAMPSIA_SEV

	
**#Construct denominator
	**Will need to be revisited & updated
	
	**near miss window has passed (42 days postpartum) MINUS:
		*"healthy"  women who closed out < 42 days postpartum
		*"healthy" women who dont have a form
		*"healthy" women who were not seen 42+ days postpartum
		*note: "healthy" means women with no PLTC, no NMC, & no death
	
	gen NEARMISS_WINDOW = 0
	replace NEARMISS_WINDOW = 1 if PP42_PASS == 1  
	//if 42 days have passed
	label var NEARMISS_WINDOW "1= 42 days PP passed"
	//this is the expected denominator
	
	*Next we will exclude "healthy" women who closed out early
	**Exclude only non-deaths
	gen		STOP_DATE_NODEATH = STOP_DATE
	replace STOP_DATE_NODEATH = . if MAT_DEATH == 1
	label var STOP_DATE_NODEATH "Stop date for reason other than mom died"
	
	gen NEARMISS_MISS_CLOSEOUT = 1 if NEARMISS_WINDOW == 1  & ///
	(STOP_DATE_NODEATH < PREG_END_PP42_DT) & /// closed out before PP42
	(PLTC != 1 & MAT_MNM_CRIT !=1 & MAT_DEATH!=1) //"healthy"
	label var NEARMISS_MISS_CLOSEOUT ///
	"Missing because healthy + closed out during window"
	
	gen NEARMISS_MISS_FORMS = 1 if NEARMISS_WINDOW == 1 & ///
	NEARMISS_MISS_CLOSEOUT != 1 & 		/// did not close out early
	(M09 !=1 & M12 !=1 & M19!=1) & 	/// no M09/M12/M19 forms
	(PLTC!=1 & MAT_MNM_CRIT !=1 & MAT_DEATH!=1) //"healthy"
	label var NEARMISS_MISS_FORMS "Missing because no M09, 12, or 19 forms"
	*note: currently M12 is only == 1 if within 42 days of pregnancy end
	
	gen NEARMISS_MISS_NOTSEEN42 = 1 if NEARMISS_WINDOW ==1 & ///
	NEARMISS_MISS_CLOSEOUT != 1  & /// did not closeout early
	NEARMISS_MISS_FORMS != 1 &  /// not missing forms
	(after42_M12 != 1 & after42 != 1)  & ///not seen 42+ days pp
	PLTC != 1 & MAT_MNM_CRIT !=1 & MAT_DEATH !=1 //"healthy"
	label var NEARMISS_MISS_NOTSEEN42 "Missing because not seen after 42 days postpartum"
	
	gen NEARMISS_DENOM = 1 if NEARMISS_WINDOW == 1 & 	///
	NEARMISS_MISS_CLOSEOUT !=1 & 		/// did not closeout early
	NEARMISS_MISS_FORMS != 1 & 		/// not missing forms
	NEARMISS_MISS_NOTSEEN42 != 1 		// seen 42+ days postpartum
	//near miss window has passed (42 days postpartum) MINUS:
		*"healthy"  women who closed out < 42 days postpartum
		*"healthy" women who dont have a form
		*"healthy" women who were not seen 42+ days postpartum
		*note: "healthy" means women with no PLTC, no NMC, & no death

	
	*egen NUM_MNM_CRIT = rowtotal(ORG_FAIL HYSTERECTOMY UTERINE_RUP LAPAROTOMY MAT_ICU HEM_PPH_SEV TRANSFUSION_NEARMISS PREECLAMPSIA_SEV) if NEARMISS_DENOM==1
	egen NUM_MNM_CRIT = anycount(ORG_FAIL  UTERINE_RUP LAPAROTOMY MAT_ICU HEM_PPH_SEV TRANSFUSION_NEARMISS PREECLAMPSIA_SEV),v(1) 
	replace NUM_MNM_CRIT = . if NEARMISS_DENOM!=1
	label var NUM_MNM_CRIT "Number of near-miss criteria (0-8)"
		*note: laparotomy includes hysterectomy
	
	*for case review purposes, do not query if:
		*A woman has exactly 2 near miss AND
		*the 2 are transfusion + PPH_SEV
	gen MNM_TRANSFUSION_PPH = 1 if ///
	NUM_MNM_CRIT == 2 & (HEM_PPH_SEV ==1 &  TRANSFUSION_NEARMISS==1)
	
	gen 		REVIEW_MULTI_NMC = 1 if ///
				inrange(NUM_MNM_CRIT,2,9) & MNM_TRANSFUSION_PPH!=1
	label var 	REVIEW_MULTI_NMC "Review: 2+ near-miss criteria"
	
	******************************************
	**CHECK multiple near miss combinations
	
	if $runquery == 1 {
		
		
		preserve
	
		for var HEM_PPH_SEV TRANSFUSION_NEARMISS PREECLAMPSIA_SEV MAT_ICU UTERINE_RUP ORG_FAIL   LAPAROTOMY HYSTERECTOMY  : replace X = . if X !=1
		 groups HEM_PPH_SEV TRANSFUSION_NEARMISS PREECLAMPSIA_SEV MAT_ICU UTERINE_RUP ORG_FAIL   LAPAROTOMY HYSTERECTOMY   if REVIEW_MULTI_NMC==1 , missing show(freq percent ) order(high)  abbrev(20) sep(10) saving(multi-nearmiss-combinations, replace)
		restore
		
	}
	
	**#If only one near-miss criteria, check the checkbox outcomes:
	*organ failure (any)
	if $runquery == 1 {
		levelsof(SITE) if NUM_MNM_CRIT == 1 & ORG_FAIL == 1, local(sitelev) clean
		foreach site of local sitelev {
			disp as result "`site'"
			export excel SITE MOMID PREGID  ORG_FAIL_M09 ORG_FAIL_M12  ORG_FAIL_M19 using "$queries/`site'-SHARE-Near-Miss-$datadate.xlsx" if SITE=="`site'" & NUM_MNM_CRIT == 1 & ORG_FAIL == 1, sheet("Check-Org-Fail", modify) firstrow(variables) 
			disp as result "`site' Completed"
		}
	}
	
	
	
		**#If only one near-miss criteria, check the checkbox outcomes:
	*organ failure (any)
	if $runquery == 1 {
		levelsof(SITE) if NUM_MNM_CRIT == 1 & LAPAROTOMY == 1, local(sitelev) clean
		foreach site of local sitelev {
			
			export excel SITE MOMID PREGID   LAPAROTOMY_M19  HYSTERECTOMY_M09 HYSTERECTOMY_M12 HYSTERECTOMY_M19 using "$queries/`site'-SHARE-Near-Miss-$datadate.xlsx" if SITE=="`site'" & NUM_MNM_CRIT == 1 & LAPAROTOMY == 1, sheet("Check-Laparotomy", modify) firstrow(variables) 
			disp as result "`site' Completed"
		}
	}
	
	******************************************
	
	
	
	*list of PLTCs: HEM_APH HEM_PPH ENDOMETRITIS_M19 ENDOMETRITIS_M10 PLACENTA_ACCRETE PLACENTA_ABRUPTION MAT_UTER_RUP_IPC MAT_UTER_RUP_PNC MAT_UTER_RUP_HOSP OBS_LABOR PRO_LABOR ABORT_COMPL_M19 PULMONARY_EDEMA_M12 PULM_EDEMA_M19 PULMONARY_EDEMA_M09 LAPAROTOMY_M19 MAT_ICU TRANSFUSION_M19 PPH_TRNSFSN_PROCCUR HOSPITALIZED_M12 M04_HOSP M19 ANEMIA_SEV_ANC  PREECLAMPSIA HIGH_BP_SEVERE_ANY MALARIA TB ORG_FAIL_M12 ORG_FAIL_M09 ORG_FAIL_M19
	
	**Are there multiple of same PLTC?
	gen 		REVIEW_DUPLICATE_PLTC = 1 if ///
				(ENDOMETRITIS_M19 == 1 & ENDOMETRITIS_M10 == 1) | ///
				(MAT_UTER_RUP_IPC == 1 & MAT_UTER_RUP_PNC == 1 ) | ///
				(PULMONARY_EDEMA_M12 == 1 & PULM_EDEMA_M19 ==1 | /// 12+19
				PULMONARY_EDEMA_M12 == 1 & PULMONARY_EDEMA_M09 == 1 | /// 09+12
				PULM_EDEMA_M19 ==1 & PULMONARY_EDEMA_M09 == 1 ) | /// 09+19
				(TRANSFUSION_M19 == 1 & PPH_TRNSFSN_PROCCUR == 1 ) & ///
				(TRANSFUSION_M19_DT!=TRANSFUSION_M09_DT) | ///
				(ORG_FAIL_M12 == 1 & ORG_FAIL_M09 == 1 | /// 09+12
				ORG_FAIL_M12 == 1 & ORG_FAIL_M19 == 1 |  /// 12+19
				ORG_FAIL_M09 == 1 & ORG_FAIL_M19 == 1 ) //   09+19
				
				*do not flag multiple hospitalizations:
				*(HOSPITALIZED_M12 ==1 & M04_HOSP == 1 | HOSPITALIZED_M12 == 1 & M19 == 1 | M04_HOSP == 1 & M19 == 1) | ///
				*do not flag APH+PPH combo:
				*(HEM_APH == 1 & HEM_PPH == 1) | ///

	label var 	REVIEW_DUPLICATE_PLTC "Review: duplicate same near-miss criteria"
	
	gen NEARMISS = 1 if ///
	MAT_MNM_CRIT == 1 & MAT_DEATH != 1 & NEARMISS_DENOM==1
	**Experienced near-miss criteria, and did not die
	label var NEARMISS "Maternal near miss"

	*Calculate GA of event:
	for var TRANSFUSION  MAT_ICU UTERINE_RUP ORG_FAIL LAPAROTOMY HYSTERECTOMY: gen X_GA = X_DT - PREG_START_DATE if X == 1
	replace TRANSFUSION_GA = . if TRANSFUSION_NEARMISS !=1
	
	*Calculate days PP for event
	for var TRANSFUSION  MAT_ICU UTERINE_RUP ORG_FAIL LAPAROTOMY HYSTERECTOMY: gen X_PP = X_DT - PREG_END_DATE if PREG_END_DATE!=. & X == 1
	replace TRANSFUSION_PP = . if TRANSFUSION_NEARMISS !=1
	
	
	*For multiple near-miss, what was the GA of first event?
	gen NEARMISS_GA_MIN = min(TRANSFUSION_GA,  /// 
		HEM_PPH_SEV_DATE, ///
		MAT_ICU_GA, PREECLAMPSIA_SEV_GA , ///
		UTERINE_RUP_GA, ORG_FAIL_GA, ///
		LAPAROTOMY_GA,  HYSTERECTOMY_GA)
			*!!WE DONT YET HAVE GA FOR TRANSFUSION_NEAMISS,
	
	
	*For multiple near miss, what was the min and max of events?
	gen PREECLAMPSIA_SEV_PP = PREECLAMPSIA_DATE-PREG_END_DATE if PREG_END_DATE!=. & PREECLAMPSIA_SEV == 1
	gen NEARMISS_PP_MIN = min(TRANSFUSION_PP,  MAT_ICU_PP, PREECLAMPSIA_SEV_PP ,UTERINE_RUP_PP, ORG_FAIL_PP, LAPAROTOMY_PP,  HYSTERECTOMY_PP)
	gen NEARMISS_PP_MAX = max(TRANSFUSION_PP,  MAT_ICU_PP, PREECLAMPSIA_SEV_PP ,UTERINE_RUP_PP, ORG_FAIL_PP, LAPAROTOMY_PP,  HYSTERECTOMY_PP)
	
	tabstat NEARMISS_PP_MAX, by(SITE) stats(n min max)	
		*Ghana, CMC & Zambia have some events > 42 days postpartum; how to handle?
	
	gen PP_TIMING_DIFFER = NEARMISS_PP_MAX - NEARMISS_PP_MIN
	
	
	***********************************************
	**CHECK: all near miss combinations
	
	if $runquery == 1 {
		
		preserve
	
		for var HEM_PPH_SEV TRANSFUSION_NEARMISS PREECLAMPSIA_SEV MAT_ICU UTERINE_RUP ORG_FAIL   LAPAROTOMY HYSTERECTOMY  : replace X = . if X !=1
		 groups HEM_PPH_SEV TRANSFUSION_NEARMISS PREECLAMPSIA_SEV MAT_ICU UTERINE_RUP ORG_FAIL   LAPAROTOMY HYSTERECTOMY   if MAT_MNM_CRIT==1 & NEARMISS_DENOM==1 , missing show(freq percent ) order(high)  abbrev(20) sep(10) saving(nearmiss-combinations,replace)
		restore
		
	}
	
	***********************************************
	
	
	
//////ADJUDICATION////////
	*Note: we expect this progression:
		*PLTC --> near-miss criteria --> death
	*If there is a near-miss but not PLTC, review these cases
	*If there is a death but not PLTC/near-miss, review these cases
	
	gen MISS_PLTC = 1 if 			///
	NEARMISS_DENOM==1 & MAT_MNM_CRIT == 1 & PLTC != 1 
	label var MISS_PLTC "Near miss criteria without PLTC"
	**ADJUDICATION IF NEAR-MISS OCCURED BUT PLTC DID NOT OCCUR


	**LOOK at women who had NMC but not PLTC
	*Note: the only criteria in NMC but not PLTC is organ fail
	*vars ORG_FAIL_M##
		
	if $runquery == 1	{
		
		levelsof(SITE) if MISS_PLTC == 1 , local(sitelev) clean
		foreach site of local sitelev {
		export excel SITE MOMID  MISS_PLTC ORG_FAIL_M09 ORG_FAIL_M12 ORG_FAIL_M19  using "$queries/`site'-Near-Miss-$datadate.xlsx" if ///
	MISS_PLTC == 1 &  SITE == "`site'", ///
	sheet("Missing-PLTC", modify)  firstrow(variables)   datestring("%tdDD-Mon-CCYY")
	}
	}
	


*Scenario 2: the woman died but no PLTC/near-miss on record for her	
	merge 1:1 MOMID PREGID using "$outcomes/MAT_MORTALITY.dta", ///
	nogenerate 
	

	*We only consider  maternal mortality, not deaths due to incidental causes
	gen MISS_MNM_DIED = 1 if ///
	(MAT_MORTALITY ==1) &  ///
	(PLTC!=1 | MAT_MNM_CRIT!=1)
	//woman died (excluding incidental causes) but she is missing PLTC and/or MNM_CRIT

	if $runquery == 1	{
		
		levelsof(SITE) if MISS_MNM_DIED == 1 , local(sitelev) clean
		foreach site of local sitelev {
		export excel SITE  PLTC MAT_MNM_CRIT ENDOMETRITIS ABORT_COMPL PLACENTA_ACCRETE PLACENTA_ABRUPTION UTERINE_RUP MAT_ICU ORG_FAIL ORG_FAIL_HRT ORG_FAIL_RESP ORG_FAIL_RENAL ORG_FAIL_LIVER ORG_FAIL_NEUR ORG_FAIL_UTER ORG_FAIL_HEM ORG_FAIL_OTHR HEM_PPH PRO_LABOR OBS_LABOR MALARIA TB ANEMIA_SEV_ANC TRANSFUSION TRANSFUSION_NEARMISS PULMONARY_EDEMA HOSPITALIZED HYSTERECTOMY LAPAROTOMY HIGH_BP_SEVERE_ANY PREECLAMPSIA PREECLAMPSIA_SEV using "$queries/`site'-Near-Miss-$datadate.xlsx" if ///
	MISS_MNM_DIED == 1 &  SITE == "`site'", ///
	sheet("Died-Missing-PLTC-NMC", modify)  firstrow(variables)   datestring("%tdDD-Mon-CCYY")
	}
	}
	

	replace MOMID = momid if MOMID==""
	

	save "$wrk/near-miss.dta", replace
	
	
	*********************************************************
	**#Query regarding uterine ruptures reported late:
	
	preserve
	
	
	use "$data/mnh12.dta" ,clear
	merge m:1 SITE MOMID PREGID using "$outcomes/MAT_ENDPOINTS.dta", keepusing(PREG_END_DATE)
	drop _merge
	keep if PREG_END_DATE!=.
	
	gen uterine_rup = 1 if M12_BIRTH_COMPL_MHTERM_3==1
	bysort PREGID : egen UTERINE_RUP = max(uterine_rup)
	keep if UTERINE_RUP == 1
	keep if M12_MAT_VISIT_MNH12<=2
	bysort PREGID ( M12_VISIT_OBSSTDAT): gen visnum = _n
	
	*since we don't have date of uterine rupture, calculate the midpoint:
	
	
		gen UTER_RUP_MAX = M12_VISIT_OBSSTDAT if uterine_rup == 1
		*"Max" date the hospitalization could have happened
		bysort PREGID (visnum) : gen UTER_RUP_MIN = M12_VISIT_OBSSTDAT[_n-1]  if uterine_rup == 1
		*"min" date it could have happened (date from previous visit)
		format UTER_RUP_MAX UTER_RUP_MIN %td
		*If the rupture is reported on her first PNC visit, it happened at some point between L&D and her first PNC visit. use PREG_END_DATE as the minimum:
		bysort PREGID : replace UTER_RUP_MIN = PREG_END_DATE if  visnum==1 & uterine_rup == 1
		bysort PREGID : gen UTER_RUP_MIN_PP =  UTER_RUP_MIN- PREG_END_DATE  if uterine_rup == 1
	
		gen UTERINE_RUP_dt = ///
		((UTER_RUP_MAX - UTER_RUP_MIN)/2) + UTER_RUP_MIN if uterine_rup == 1
	format UTERINE_RUP_dt %td
	
	keep if uterine_rup == 1
	
	gen PP_DAYS = UTERINE_RUP_dt-PREG_END_DATE
	
	merge m:1 SITE MOMID PREGID using "$outcomes/MAT_LABOR.dta", keepusing(MAT_CES_ANY PRO_LABOR OBS_LABOR)
	keep if _merge == 3
	keep if uterine_rup == 1
	drop _merge
	merge m:1 SITE MOMID PREGID using "$outcomes/MAT_PREVPREG_COMPLICATIONS.dta", keepusing(MAT_PREVPREG_CES_UNPLAN MAT_PREVPREG_CES_PLAN MAT_PREVPREG_CES_ANY)
	keep if _merge == 3
	drop _merge
	for var MAT_CES_ANY PRO_LABOR OBS_LABOR MAT_PREVPREG_CES_UNPLAN MAT_PREVPREG_CES_PLAN MAT_PREVPREG_CES_ANY: replace X = . if X > 1
	list SITE visnum PP MAT_CES_ANY PRO_LABOR OBS_LABOR  MAT_PREVPREG_CES_UNPLAN, abbrev(25) sepby(SITE)
	gen any_compl = 1 if (MAT_CES_ANY ==1 |   PRO_LABOR ==1 |  OBS_LABOR ==1 |  MAT_PREVPREG_CES_UNPLAN ==1)

	keep SITE MOMID PREGID M12_VISIT_OBSSTDAT PREG_END_DATE PP uterine_rup visnum MAT_CES_ANY PRO_LABOR OBS_LABOR MAT_PREVPREG_CES_UNPLAN MAT_PREVPREG_CES_PLAN MAT_PREVPREG_CES_ANY any_compl
	
	
	merge 1:1 SITE MOMID PREGID using "$wrk/near-miss", keepusing(NUM_MNM_CRIT HOSPITALIZED)
	keep if uterine_rup == 1
	gen query_note = "Only near miss criteria" if NUM_MNM_CRIT == 1 
	replace query_note = "Only near miss criteria" if NUM_MNM_CRIT <2 & PP<=42 

	levelsof(SITE) if visnum>1, clean local(sitelev)
	foreach site of local sitelev {
			export excel SITE MOMID PREGID M12_VISIT_OBSSTDAT visnum query_note using "$queries/`site'-SHARE-Near-Miss-$datadate.xlsx" if SITE=="`site'" & visnum>1, sheet("Late-Uterine-Rup", modify) firstrow(variables)  datestring("%tdDD-Mon-CCYY")
				disp as result "`site' Completed"
			}
			
	restore
	*End late uterine rupture query code
	*********************************************************
	
	
**#Clean dataset for outcomes folder
	
	keep SITE  MOMID PREGID NEARMISS_WINDOW NEARMISS_MISS_CLOSEOUT NEARMISS_MISS_FORMS NEARMISS_MISS_NOTSEEN42 NEARMISS_DENOM MISS_PLTC MISS_MNM_DIED PLTC HEM_PPH ENDOMETRITIS PLACENTA_ACCRETE PLACENTA_ABRUPTION UTERINE_RUP OBS_LABOR PRO_LABOR ABORT_COMPL  PULMONARY_EDEMA TRANSFUSION TRANSFUSION_NEARMISS MAT_ICU HOSPITALIZED HYSTERECTOMY UTERINE_RUP LAPAROTOMY  ORG_FAIL ORG_FAIL_HRT ORG_FAIL_RESP ORG_FAIL_RENAL ORG_FAIL_LIVER ORG_FAIL_NEUR ORG_FAIL_UTER ORG_FAIL_HEM ORG_FAIL_OTHR NEARMISS  MAT_DEATH MAT_MNM_CRIT ANEMIA_SEV_ANC HDP_GROUP HEM_PPH_SEV PREECLAMPSIA_NONSEV PREECLAMPSIA_SEV HIGH_BP_SEVERE_ANY MALARIA TB HIV NUM_MNM_CRIT PLTC_DT PLTC_PP ENDOMETRITIS_DT PLACENTA_ACCRETE_DT PLACENTA_ABRUPTION_DT UTERINE_RUP_DT UTERINE_RUP_PP OBS_LABOR_DT PRO_LABOR_DT ABORT_COMPL_DT PULMONARY_EDEMA_DT TRANSFUSION_DT TRANSFUSION_PP MAT_ICU_DT MAT_ICU_PP HOSPITALIZED_DT HYSTERECTOMY_DT HYSTERECTOMY_PP LAPAROTOMY_DT LAPAROTOMY_PP UTERINE_RUP_DT UTERINE_RUP_PP ORG_FAIL_DT ORG_FAIL_PP MAT_DEATH_DATE MAT_DEATH_INFAGE ANEMIA_SEV_ANC_DT PREECLAMPSIA_DATE PREECLAMPSIA_SEV_DATE PREECLAMPSIA_SEV_PP
	
	**set to zero if not in near-miss-denominator
	foreach var in PLTC HEM_PPH ENDOMETRITIS PLACENTA_ACCRETE PLACENTA_ABRUPTION UTERINE_RUP OBS_LABOR PRO_LABOR ABORT_COMPL  PULMONARY_EDEMA TRANSFUSION TRANSFUSION_NEARMISS MAT_ICU HOSPITALIZED HYSTERECTOMY UTERINE_RUP LAPAROTOMY  ORG_FAIL ORG_FAIL_HRT ORG_FAIL_RESP ORG_FAIL_RENAL ORG_FAIL_LIVER ORG_FAIL_NEUR ORG_FAIL_UTER ORG_FAIL_HEM ORG_FAIL_OTHR NEARMISS MAT_MNM_CRIT ANEMIA_SEV_ANC  HEM_PPH_SEV PREECLAMPSIA_NONSEV PREECLAMPSIA_SEV  HIGH_BP_SEVERE_ANY MALARIA TB HIV  {
		replace `var' = 0 if NEARMISS_DENOM != 1
		replace `var' = 0 if NEARMISS_DENOM == 1 & `var' != 1
	}

	**Note: would appreciate feedback on this, the point is to make sure we are  only counting participants who are in the denominator (have passed the window without loss to follow up etc.)
		*However we are currently losing information about the participants who did withdraw or who died, we are setting everything to zero for these participants
		* this info is kept in near-miss.dta
	
	gen MGMT_BASED = 1 if ///
	HYSTERECTOMY==1 | MAT_ICU==1 | TRANSFUSION_NEARMISS==1 | LAPAROTOMY==1
	label var MGMT_BASED "MNM according to management-based criteria"
	gen DIS_BASED = 1 if ///
	HEM_PPH_SEV==1 | PREECLAMPSIA_SEV==1 | UTERINE_RUP==1
	label var DIS_BASED "MNM according to disease-based criteria"
	
	egen MAT_MNM_CRITERIA=rowtotal(ORG_FAIL MGMT_BASED DIS_BASED)
	label var MAT_MNM_CRITERIA "No. criteria: organ, management, or disease based"
	
	*"Healthy" indicator
	gen HEALTHY = 0 if NEARMISS_DENOM == 1
	replace HEALTHY = 1 if ///
	PLTC!=1 & MAT_MNM_CRIT!=1 & MAT_DEATH!=1 & NEARMISS_DENOM == 1
	label define HEALTHY 1"Healthy" 0"Not healthy"
	label val HEALTHY HEALTHY
	label var HEALTHY "No PLTC, NMC, or death"	
	
/////SITE NEAR-MISS ADJUDICATION ////	
	**replace MNM based on site adjudication
	**Below based on ZH email July-12-2024
	**These are IDs where the data is correct, woman experienced 1+   near-miss criteria but when cases were reviewed, the medical team determined it was not a true near-miss
	replace NEARMISS = 0 if ///
	(SITE=="Pakistan"  & MISS_PLTC==1 & MOMID == "BD-2791") | ///
	(SITE=="Pakistan"  & MISS_PLTC==1 & MOMID == "BE-9702") | ///
	(SITE=="Pakistan"  & MISS_PLTC==1 & MOMID == "BX-3861")
	
	cap drop uploaddate UploadDate
	
	

		
	save "$wrk/MAT_NEAR_MISS-$datadate.dta", replace
	
	merge 1:1 SITE MOMID PREGID  using  "$outcomes/MAT_ENROLL.dta", nogen keepusing(REMAPP_ENROLL )
	
	drop  NUM_MNM_CRIT MISS_PLTC MISS_MNM_DIED MGMT_BASED DIS_BASED MAT_MNM_CRITERIA
	*save "$outcomes/MAT_NEAR_MISS.dta", replace

	/*
///NUMBERS FOR ANNUAL MEETING SLIDES, CONTRIBUTING CRITERIA
	
	tab ORG_FAIL if NEARMISS==1 //organ failure among  near-miss cases
	tab MGMT_BASED  if NEARMISS==1 ,miss // management criteria among near miss
	tab DIS_BASED  if NEARMISS==1 , miss //disease criteria among near miss
	
	tab MAT_MNM_CRITERIA if NEARMISS==1, miss //number criteria among near miss
	
	
//NUMBERS FOR ANNUAL MEETING, UNDERLYING CONDITIONS
	
	**Prolonged labor
	tab PRO_LABOR if HEALTHY==1,miss //prolonged labor among healthy
	tab PRO_LABOR if PLTC==1,miss //prolonged labor among PLTC
	tab PRO_LABOR if NEARMISS==1,miss //prolonged labor among near-miss
	 
	**Malaria
	tab MALARIA if HEALTHY==1,miss //malaria among healthy
	tab MALARIA if PLTC==1,miss // malaria among PLTC
	tab MALARIA if NEARMISS==1,miss //malaria among near miss
	
	**HIV
	tab HIV if HEALTHY==1,miss //HIV among healthy
	tab HIV if PLTC==1,miss // HIV among PLTC
	tab HIV if NEARMISS==1,miss //HIV among near miss
	
	**Anemia
	gen ANEMIA_ANC_ANY = 0 if ANEMIA_ANC!=.
	replace ANEMIA_ANC_ANY = 1 if ANEMIA_ANC>=1 & ANEMIA_ANC<=3
	label define ANEMIA_ANC_ANY 0"No anemia or missing" 1"Any anemia"
	label val ANEMIA_ANC_ANY ANEMIA_ANC_ANY
	
	tab ANEMIA_ANC_ANY if HEALTHY==1,miss //anemia among healthy
	tab ANEMIA_ANC_ANY if PLTC==1,miss // anemia among PLTC
	tab ANEMIA_ANC_ANY if NEARMISS==1,miss //anemia among near miss
	
	*chronic hypertension
	merge 1:1 MOMID PREGID using "$outcomes/MAT_CHRONIC_HTN_DIAB.dta", nogenerate
	tab CHTN if HEALTHY==1,miss //HYPERTENSION among healthy
	tab CHTN if PLTC==1,miss // HYPERTENSION among PLTC
	tab CHTN if NEARMISS==1,miss //HYPERTENSION among near miss
	
	*GDM
	merge 1:1 MOMID PREGID using "$outcomes/MAT_GDM.dta", nogenerate
	tab DIAB_GEST_ANY if HEALTHY==1,miss //HYPERTENSION among healthy
	tab DIAB_GEST_ANY if PLTC==1,miss // HYPERTENSION among PLTC
	tab DIAB_GEST_ANY if NEARMISS==1,miss //HYPERTENSION among near miss
	
	*Ferritin
	merge 1:1 MOMID PREGID using "$outcomes/MAT_NUTR.dta", nogen
	gen FERRITIN70_ANC_ANY = 0 if ///
	FERRITIN70_ANC20!=. | FERRITIN70_ANC32!=.
	replace FERRITIN70_ANC_ANY = 1 if ///
	FERRITIN70_ANC20==1 | FERRITIN70_ANC32==1
	
	tab FERRITIN70_ANC_ANY if HEALTHY==1,miss // ferritin among healthy
	tab FERRITIN70_ANC_ANY if PLTC==1,miss // ferritin among PLTC
	tab FERRITIN70_ANC_ANY if NEARMISS==1,miss // ferritin among near miss
	
	
