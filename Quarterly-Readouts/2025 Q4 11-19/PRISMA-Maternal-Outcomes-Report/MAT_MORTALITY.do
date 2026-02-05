*Savannah O'Malley (savannah.omalley@gwu.edu)

*Output: MAT_MORTALITY.dta 

/*this file requires the following:
MAT_COD.csv
MAT_ENDPOINTS.dta
*/

*!! WARNING: this file requires frequent review of text data

**Set the paths

global datadate "2025-10-31"
// change date based on date of data import you're working with

global wrk "Z:\Savannah_working_files\maternal-mortality/$datadate"
cap mkdir "$wrk" //make this directory if it doesn't exist

cd "$wrk"
global output "Z:\Savannah_working_files\maternal-mortality\output"

global outcomes "Z:\Outcome Data/$datadate"
global da "Z:\Stacked Data/$datadate"
	
//CREATE VARIABLES FOR MATERNAL MORTALITY //

	import delimited "$outcomes/MAT_COD.csv", ///
	bindquote(strict) case(upper) clear
	
	**Look at cause of death:
	tab COD,m
	label define COD ///
					 1 "Road traffic accident" ///
					 2 "Obstetric haemorrhage" ///
					 3 "Pregnancy-induced hypertension" ///
					 4 "Pregnancy-related sepsis" ///
					 5 "ARI" ///
					 6 "HIV/AIDS related" ///
					 7 "Reproductive neoplasms" ///
					 8 "Pulmonary TB" ///
					 9 "Malaria" ///
					 10 "Meningitis" ///
					 11 "Diarrheal diseases" ///
					 12 "Abortion-related death" ///
					 13 "Ectopic pregnancy" ///
					 14 "Other and unspecified cardiac diseases" ///
					 15 "Other infectious diseases" ///
					 16 "Other NCD" ///
					 17 "Other (not listed) specify" ///
					 18 "Indeterminate" ///
					 77 "NA", replace
	label val COD COD
	tab COD SITE,m
	gen MAT_DEATH_CAUSE = 1 if /// these appear to be obstetric-related deaths
	inlist(COD, 2,3,4,7)
	
	replace MAT_DEATH_CAUSE = 2 if /// these appear to be not obstetric related
	inlist(COD, 1,8) 
	
	replace MAT_DEATH_CAUSE = 3 if /// these are not known/unclear if obstetric cause
	inlist(COD, 14,17,18,77)
	//review option #77
	
	
	replace MAT_DEATH_CAUSE = 55 if /// these are pending data
	COD==. 
	
	label define CAUSE 1"Obstetric" 2"Not obstetric" 3"Unknown/unclear" 55"Missing"
	label val MAT_DEATH_CAUSE CAUSE
	bigtab MAT_DEATH_CAUSE COD
	assert MAT_DEATH_CAUSE != . //ensure all causes got coded
	assert inlist(MAT_DEATH_CAUSE,3,55) if COD_MISS == 1
	assert inlist(COD_MISS,1,77)  if MAT_DEATH_CAUSE == 55 
		*sometimes COD_MISS is blank (".")
	
	merge 1:1 MOMID PREGID using "$outcomes/MAT_ENDPOINTS.dta",gen(mergeCOD)
	
	//maternal mortality is within 42 days AND due to pregnancy or birth
	gen MAT_MORTALITY=1 if ///
	MAT_DEATH==1 & /// death occured
	MAT_DEATH_42==1 &  /// occured during pregnancy-pp42
	MAT_DEATH_CAUSE==1 //obstetric-related
	
	replace MAT_MORTALITY=2 if ///
	MAT_DEATH==1 & /// death occurred
	MAT_DEATH_42==1 & /// occured during pregnancy-pp42
	MAT_DEATH_CAUSE == 2 //due to non-obstetric causes
	
	replace MAT_MORTALITY=3 if ///
	MAT_DEATH==1 & ///death occurred
	MAT_DEATH_42==1 & /// occured during pregnancy-pp42
	MAT_DEATH_CAUSE == 3 //unknown
	
	replace MAT_MORTALITY=55 if ///
	MAT_DEATH==1 & /// death occurred
	MAT_DEATH_42==1 & /// occured during pregnancy-pp42
	MAT_DEATH_CAUSE == 55 //Missing/pending
	
	
	label var 		MAT_MORTALITY ///
					"Maternal mortality (obstetric cause <= 42 days pp)"
	label define 	MAT_MORTALITY ///
					1"Mat mortality" ///
					2"Not mat mortality" ///
					3"Unknown CoD" ///
					55"Missing",replace
	label val 		MAT_MORTALITY MAT_MORTALITY
	
	bigtab  MAT_DEATH_42 MAT_DEATH_CAUSE MAT_MORTALITY if MAT_DEATH==1

	gen MAT_MORTALITY_LATE = 1 if ///
	MAT_DEATH==1 & /// death occurred
	MAT_DEATH_42==2 & /// occured 42+ days pp
	MAT_DEATH_CAUSE == 1 /// obstetric-related
	
	replace MAT_MORTALITY_LATE=2 if ///
	MAT_DEATH==1 & /// death occurred
	MAT_DEATH_42==2 & /// occurred 42+ days pp
	MAT_DEATH_CAUSE == 2 //not obstetric related
	
	replace MAT_MORTALITY_LATE=3 if ///
	MAT_DEATH==1 & /// death occurred
	MAT_DEATH_42==2 & ///occurred 42+ days pp
	MAT_DEATH_CAUSE == 3 //unknown
	
	replace MAT_MORTALITY_LATE=55 if ///
	MAT_DEATH==1 & /// death occurred
	MAT_DEATH_42==2 & ///occurred 42+ days pp
	MAT_DEATH_CAUSE == 55 //missing	
	
	label var 	MAT_MORTALITY_LATE ///
				"Late maternal mortality (obstetric cause 42-365 days pp)"
	
	label define MAT_MORTALITY_LATE ///
				 1"Late mat mortality" ///
				 2"Not late mat mortality" ///
				 3"Unknown CoD" ///
				 55"Missing", replace
	label val 	 MAT_MORTALITY_LATE MAT_MORTALITY_LATE

	sort MAT_DEATH_42 MAT_DEATH_CAUSE
	list MAT_DEATH_42 COD MAT_DEATH_CAUSE MAT_MORTALITY MAT_MORTALITY_LATE ///
	if MAT_DEATH==1, sepby(MAT_DEATH_42) 
	
	//what was the cause of death? (update as more VAs become available)
	gen MAT_DEATH_MISSCAUSE =1 if MAT_DEATH==1 & MAT_DEATH_CAUSE==55
	
	bigtab COD MAT_DEATH_MISSCAUSE if MAT_DEATH==1
	
/*
foreach site in  "Pakistan"   {
	export excel  SITE MOMID DEATH_GA using ///
	"$output/Deliveries without MNH09 `site'.xlsx" if ///
	 MAT_DEATH == 1 & DEATH_GA>198 & DEATH_INFAGE==0 & site=="`site'" , ///
	sheet("Without MNH09") replace firstrow(variables)
} 

*/
	
//make short file for maternal outcomes

	keep SITE MOMID PREGID PREG_END MAT_DEATH MAT_DEATH_DATE MAT_DEATH_MISSCAUSE MAT_DEATH_MISSDATE  MAT_DEATH_42 MAT_MORTALITY MAT_MORTALITY_LATE 


	gen 		PREG_DEATH = 1 if MAT_DEATH_42 == 1
	replace 	PREG_DEATH = 0 if MAT_DEATH!=1
	replace 	PREG_DEATH = 55 if MAT_DEATH_MISSDATE==1
	label var 	PREG_DEATH ///
	"Pregnancy related death (<= 42 days pp, any cause)"


	gen 		PREG_DEATH_LATE = 1 if MAT_DEATH_42 == 2
	replace 	PREG_DEATH_LATE = 0 if MAT_DEATH !=1
	replace 	PREG_DEATH_LATE = 55 if MAT_DEATH_MISSDATE==1
	label var 	PREG_DEATH_LATE ///
	"Late pregnancy-related death (42-365 days pp, any cause)"

	save "$wrk/MAT_MORTALITY.dta", replace
	
	*Review and save to outcomes folder:
	*save "$outcomes/MAT_MORTALITY.dta", replace
	
