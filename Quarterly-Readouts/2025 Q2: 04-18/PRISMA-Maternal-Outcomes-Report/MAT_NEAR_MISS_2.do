**PRISMA MATERNAL NEAR-MISS
**OUTCOME CONSTRUCTION
/*
**NOTE: this file must be run after running '1.Near-Miss-Vars.do'

*NOTE: this file needs the following files:

"$wrk/near-miss.dta" (Part 1)
"$outcomes/MAT_HDP"

*/

	global datadate "2025-04-18"	
	
	global da "Z:\Stacked Data/$datadate-updated"
	global outcomes "Z:\Outcome Data/$datadate"
	global wrk "Z:\Savannah_working_files\Near-Miss\data/$datadate"
	global output "Z:\Savannah_working_files\Near-Miss\output"	
	cd "$wrk"
	
	cap mkdir "Z:\Savannah_working_files\Near-Miss\data/$datadate/queries"
	global queries "Z:\Savannah_working_files\Near-Miss\data/$datadate/queries"
	global runquery = 1 //do you want to export queries for sites?
	
use "$wrk/near-miss.dta" , clear

merge 1:1 MOMID PREGID using "$outcomes/MAT_HDP", nogen

/*
gen PE = 1 if HDP_GROUP>=3 & HDP_GROUP<=4
label var PE "Pre eclampsia without severe features"
gen PE_SEV=1 if HDP_GROUP == 5
label var PE_SEV "Preeclampsia with severe features/eclampsia"
*/

gen PLTC = 1 if 					///
	HEM_PPH == 1 | 					/// any postpartum hemorrage
	ENDOMETRITIS == 1 | 			///	
	PLACENTA_ACCRETE == 1 | 	///
	PLACENTA_ABRUPTION == 1 | 	///	
	UTERINE_RUP == 1 |				///
	OBS_LABOR == 1 | 				///
	PRO_LABOR == 1 | 				///
	ABORT_COMPL == 1 | 			///
	PULMONARY_EDEMA == 1 | 			///	
	LAPAROTOMY == 1 | 				///
	MAT_ICU == 1 | 						///
	TRANSFUSION == 1 | 				///
	HOSPITALIZED == 1  |					/// hospitalization
	ANEMIA_SEV_ANC==1   |        	///
	PREECLAMPSIA == 1 |						/// PE w/o severe features
	PREECLAMPSIA_SEV ==1 |					/// PE w/severe features
	HIGH_BP_SEVERE_ANY ==1 | 		///
	MALARIA == 1 | 					///
	TB == 1
**note: update this code to include:
	**sepsis/severe infection, once available 
	
	gen PLTC_CRITERIA = PLTC
	label var PLTC_CRITERIA "Experienced 1+ PLTC"
	*We are creating this second variable which will not be set to zero for those not in the near-miss denominator
	
	
	**!! temp: a second indicator of PLTC which does not consider prolonged labor or malaria
	gen PLTC2 = 1 if 				///
	HEM_PPH == 1 | 					///
	ENDOMETRITIS == 1 | 			///	
	PLACENTA_ACCRETE == 1 | 	///
	PLACENTA_ABRUPTION == 1 | 	///	
	UTERINE_RUP == 1 |				///
	OBS_LABOR == 1 | 				///
	ABORT_COMPL == 1 | 			///
	PULMONARY_EDEMA == 1 | 			///	
	LAPAROTOMY == 1 | 				///
	MAT_ICU == 1 | 						///
	TRANSFUSION == 1 | 				///
	HOSPITALIZED == 1  |					///
	ANEMIA_SEV_ANC==1   |        	///
	PREECLAMPSIA == 1 |						///
	PREECLAMPSIA_SEV ==1 |					///
	HIGH_BP_SEVERE_ANY ==1 | 		///
	TB == 1
	*does not include prolonged labor or malaria

	gen MAT_MNM_CRIT = 1 if ///
	ORG_FAIL ==1 | 			/// organ dysfunction; pull dates from MNH09/12/19
	HYSTERECTOMY == 1 | 	/// pull dates from MNH09/12/19
	UTERINE_RUP == 1 | 		///
	LAPAROTOMY == 1 | 		///
	MAT_ICU==1 | 				///
	HEM_PPH_SEV==1 | 		/// severe postpartum hemorrhage
	TRANSFUSION_NEARMISS==1 | /// near-miss definition of transfusion
	PREECLAMPSIA_SEV == 1
	**NOTE:sepsis when available
	label var MAT_MNM_CRIT "Meets PRISMA near-miss criteria"
	

	
	***CONSTRUCT THE DENOMINATORS***
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
	gen STOP_DATE_NODEATH = STOP_DATE
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

	
	egen NUM_MNM_CRIT = rowtotal(ORG_FAIL HYSTERECTOMY UTERINE_RUP LAPAROTOMY MAT_ICU HEM_PPH_SEV TRANSFUSION_NEARMISS PREECLAMPSIA_SEV) if NEARMISS_DENOM==1
	label var NUM_MNM_CRIT "Number of near-miss criteria (0-8)"
	
	gen NEARMISS = 1 if ///
	MAT_MNM_CRIT == 1 & MAT_DEATH != 1 & NEARMISS_DENOM==1
	**Experienced near-miss criteria, and did not die
	label var NEARMISS "Maternal near miss"

	
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
		export excel SITE MOMID  MISS_PLTC ORG_FAIL_M09 ORG_FAIL_M12 ORG_FAIL_M19  using "$queries/`site'-queries-$datadate.xlsx" if ///
	MISS_PLTC == 1 &  SITE == "`site'", ///
	sheet("Missing-PLTC") replace firstrow(variables)
	}
	}
	


*Scenario 2: the woman died but no PLTC/near-miss on record for her	
	**NOTE cause of death needed!!
	merge 1:1 MOMID PREGID using "$outcomes/MAT_MORTALITY.dta", ///
	nogenerate 
	
	
	
	*We only consider  maternal mortality, not death due to incidental causes
	gen MISS_MNM_DIED = 1 if ///
	(MAT_MORTALITY ==1) &  ///
	(PLTC!=1 | MAT_MNM_CRIT!=1)
	//woman died (excluding incidental causes) but she is missing PLTC and/or MNM_CRIT

	if $runquery == 1	{
		
		levelsof(SITE) if MISS_MNM_DIED == 1 , local(sitelev) clean
		foreach site of local sitelev {
		export excel SITE  PLTC MAT_MNM_CRIT ENDOMETRITIS ABORT_COMPL PLACENTA_ACCRETE PLACENTA_ABRUPTION UTERINE_RUP MAT_ICU ORG_FAIL ORG_FAIL_HRT ORG_FAIL_RESP ORG_FAIL_RENAL ORG_FAIL_LIVER ORG_FAIL_NEUR ORG_FAIL_UTER ORG_FAIL_HEM ORG_FAIL_OTHR HEM_PPH PRO_LABOR OBS_LABOR MALARIA TB ANEMIA_SEV_ANC TRANSFUSION TRANSFUSION_NEARMISS PULMONARY_EDEMA HOSPITALIZED HYSTERECTOMY LAPAROTOMY HIGH_BP_SEVERE_ANY PREECLAMPSIA PREECLAMPSIA_SEV using "$queries/`site'-queries-$datadate.xlsx" if ///
	MISS_MNM_DIED == 1 &  SITE == "`site'", ///
	sheet("Died-Missing-PLTC-NMC") replace firstrow(variables)
	}
	}
	

	
	
	replace MOMID = momid if MOMID==""
	
	*query: woman was closed out too early
	//update for Jan 10 2025 data: 
		//these queries are captured in MAT_ENDPOINTS workflow
	gen 		closeout_daypp = CLOSEOUT_DT- PREG_END_DATE
	replace 	closeout_daypp = CLOSEOUT_DT- EDD_BOE if ///
				PREG_END_DATE==.
	label var 	closeout_daypp "days postpartum at closeout"
	
	gen 	QUERY_CLOSEOUTEARLY = "Closed out too soon" if ///
			closeout_daypp < 40 & CLOSEOUT_TYPE<=2 
	replace QUERY_CLOSEOUTEARLY = "Closed out too soon" if ///
			closeout_daypp < 300 & CLOSEOUT_TYPE==1
	
	save "$wrk/near-miss.dta", replace
	
***MAKE DATA SET FOR MATERNAL OUTCOMES REPORT***	
	
	keep SITE  MOMID PREGID NEARMISS_WINDOW NEARMISS_MISS_CLOSEOUT NEARMISS_MISS_FORMS NEARMISS_MISS_NOTSEEN42 NEARMISS_DENOM MISS_PLTC MISS_MNM_DIED PLTC HEM_PPH ENDOMETRITIS PLACENTA_ACCRETE PLACENTA_ABRUPTION UTERINE_RUP OBS_LABOR PRO_LABOR ABORT_COMPL  PULMONARY_EDEMA TRANSFUSION TRANSFUSION_NEARMISS MAT_ICU HOSPITALIZED HYSTERECTOMY UTERINE_RUP LAPAROTOMY  ORG_FAIL ORG_FAIL_HRT ORG_FAIL_RESP ORG_FAIL_RENAL ORG_FAIL_LIVER ORG_FAIL_NEUR ORG_FAIL_UTER ORG_FAIL_HEM ORG_FAIL_OTHR NEARMISS  MAT_DEATH MAT_MNM_CRIT ANEMIA_SEV_ANC ANEMIA_ANC HDP_GROUP HEM_PPH_SEV PREECLAMPSIA PREECLAMPSIA_SEV HIGH_BP_SEVERE_ANY MALARIA TB HIV NUM_MNM_CRIT 
	
	**set to zero if no in near-miss-denominator
	foreach var in PLTC HEM_PPH ENDOMETRITIS PLACENTA_ACCRETE PLACENTA_ABRUPTION UTERINE_RUP OBS_LABOR PRO_LABOR ABORT_COMPL  PULMONARY_EDEMA TRANSFUSION TRANSFUSION_NEARMISS MAT_ICU HOSPITALIZED HYSTERECTOMY UTERINE_RUP LAPAROTOMY  ORG_FAIL ORG_FAIL_HRT ORG_FAIL_RESP ORG_FAIL_RENAL ORG_FAIL_LIVER ORG_FAIL_NEUR ORG_FAIL_UTER ORG_FAIL_HEM ORG_FAIL_OTHR NEARMISS MAT_MNM_CRIT ANEMIA_SEV_ANC  HEM_PPH_SEV PREECLAMPSIA PREECLAMPSIA_SEV  HIGH_BP_SEVERE_ANY MALARIA TB HIV  {
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
	
	**check if SITE / MOMID / PREGID is missing for any participants
	foreach var in SITE MOMID PREGID {
		qui tab `var' if `var' =="", miss
		disp as text "Missing `var' = " as result _col(20) r(N)
	}
	
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
	
	
