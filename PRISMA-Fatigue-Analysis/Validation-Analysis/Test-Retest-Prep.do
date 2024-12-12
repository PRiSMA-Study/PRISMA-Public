**Fatigue - specifically for test-retest reliability


**Part 1: Set paths & read in the data (MNH 26)
	
	global datadate "2024-11-15"
	global da "Z:\Stacked Data/$datadate"
	global wrk "Z:\Savannah_working_files\Fatigue"
	global outcomes "Z:\Outcome Data/$datadate"
	global output "Z:\Savannah_working_files\Fatigue\output"
	global ids "Z:\Savannah_working_files\Fatigue\Test_Retest_IDs"
		
**# Create the list of test-retest IDs
	
	**Pakistan	
	import excel "$ids/PK_FACIT_Test-Retest_IDs.xlsx", ///
	sheet("ANC") firstrow clear
	drop E-Z
	drop SNo
	save "$ids/PK_ANC",replace
	import excel "$ids/PK_FACIT_Test-Retest_IDs.xlsx", ///
	sheet("PNC ") cellrange(A1:D1000) firstrow clear
	drop Sno
	append using "$ids/PK_ANC"
	keep if Date !=.
	rename VRID MOMID 
	*test and retest
	bysort MOMID (Date) : gen TEST_RETEST=_n
	*days between test and retest
	bysort MOMID (Date) : gen RETEST_TIME = round(Date-Date[1])
	replace RETEST_TIME =. if TEST_RETEST==1
	
	bysort MOMID Visittype : gen RETEST_AVAILABLE = 1 if _N==2
	gen TYPE_VISIT = 2 if Visittype == "ANC 1 (20 weeks)"
	replace TYPE_VISIT = 4 if Visittype == "ANC 3 (32 weeks)"
	replace TYPE_VISIT = 10 if Visittype == "PNC 6 (6 weeks)"
	drop Visittype
	save "$ids/PK", replace
	
	*CMC
	import excel "$ids/CMC_FINAL_FACIT_Test-Retest_17JUN2024.xlsx", ///
	sheet("MNH26-ANC") firstrow clear
	drop PREGID
	rename Dateoftest Date1
	rename Dateofretest Date2
	reshape long Date , i(MOMID)
	rename _j TEST_RETEST
	bysort MOMID (Date) : gen RETEST_TIME = round(Date-Date[1])
	replace RETEST_TIME =. if TEST_RETEST==1
	
	save "$ids/CMC_ANC",replace
	
	import excel "$ids/CMC_FINAL_FACIT_Test-Retest_17JUN2024.xlsx", ///
	sheet("MNH26-PNC") firstrow clear
	drop PREGID
	rename Dateoftest Date1
	rename Dateofretest Date2
	reshape long Date , i(MOMID)
	rename _j TEST_RETEST
	bysort MOMID (Date) : gen RETEST_TIME = round(Date-Date[1])
	replace RETEST_TIME =. if TEST_RETEST==1
	append using "$ids/CMC_ANC"
	
	bysort MOMID TYPE_VISIT : gen RETEST_AVAILABLE = 1 if _N==2
	
	save "$ids/CMC", replace
	
	
	*SAS
	import excel "$ids/SAS_FINAL_Test-retest_20SEP2024_NEW.xlsx", ///
	sheet("ANC") firstrow clear
	keep if MOMID !=""
	rename FTGE_OBSTDAT Date
	gen TYPE="ANC"
	save "$ids/SAS_ANC", replace
	
	import excel "$ids/SAS_FINAL_Test-retest_20SEP2024_NEW.xlsx", ///
	sheet("PNC") firstrow clear
	gen TYPE="PNC"
	keep if MOMID !=""
	rename FTGE_OBSTDAT Date
	append using "$ids/SAS_ANC"
	
	bysort MOMID TYPE (Date) : gen TEST_RETEST=_n
	tab TEST_RETEST TYPE
	
	bysort MOMID TYPE : gen RETEST_AVAILABLE = 1 if _N==2
	
	*check time between test and retest
	bysort MOMID TYPE (Date) : gen RETEST_TIME = round(Date-Date[1])
	replace RETEST_TIME =. if TEST_RETEST==1
	tab RETEST_TIME 
	
	save "$ids/SAS", replace
	
	**#Kenya
	import excel "$ids/Kenya-FACIT Validation IDs -updated.xlsx", ///
	sheet("ALL") firstrow case(upper) clear
	rename LANGUAGE LANGUAGE_KY
	rename FTGE_OBSTDAT Date
	
	rename TYPE_VISIT VISIT_TYPE2
	gen VISIT_TYPE = 2 if VISIT_TYPE2=="ANC-20"
	replace VISIT_TYPE= 4 if VISIT_TYPE2=="ANC-32"
	replace VISIT_TYPE = 13 if ///
	VISIT_TYPE2=="Non-scheduled visit for routine care"
	replace VISIT_TYPE = 14 if ///
	regexm(VISIT_TYPE2, "Non-scheduled PNC visit")
	replace VISIT_TYPE= 10 if VISIT_TYPE2=="PNC-6"
	bigtab VISIT_TYPE VISIT_TYPE2
	drop VISIT_TYPE2
	label define VISIT_TYPE ///
	1"Enrolment" 2"ANC-20" 3"ANC-28" 4"ANC-32" 5"ANC-36" ///
	6 "IPC" 7"PNC-0" 8"PNC-1" 9"PNC-4" 10"PNC-6" 11"PNC-26" 12"PNC-52" ///
	13"Unscheduled ANC" 14"Unscheduled PNC"
	label val VISIT_TYPE VISIT_TYPE
	
	**!!temporary fix until I hear back from Kenya
	*One woman is listed as both Kiswahili & Luo
	*I think kiswahili is incorrect
	* drop if MOMID=="KEARC02017" & LANGUAGE_KY=="KISWAHILI"
	
	bysort MOMID  (Date) : gen TEST_RETEST=_n
	tab TEST_RETEST 
	
	*check time between test and retest
	bysort MOMID (Date) : gen RETEST_TIME = round(Date-Date[1])
	replace RETEST_TIME =. if TEST_RETEST==1
	
	bysort MOMID : gen RETEST_AVAILABLE = 1 if _N>1 & _N<.
	save "$ids/KY", replace
	
	**#Zambia
	*NOTE: the excel required a little processing before import
	
	import excel "$ids/ZM_FACIT_Test-Retest_IDs.xlsx", ///
	sheet("Sheet1") firstrow clear
	keep if PTID!=""
	drop L-Z
	drop AA- AX
	
	rename PTID MOMID
	rename Site SITE_ZM
	rename Visit VISIT_TYPE
	rename Language LANG_ZM
	
	drop EPDS FATIGUEFACIT AcceptedRetest FACIT
	drop TargetRetestDate
	
	
	**two duplicates
	duplicates drop
	*reshape to long
	reshape long Date ,i(MOMID)
	drop _j
	*indicate test and retest
	bysort MOMID VISIT_TYPE (Date) : gen TEST_RETEST=_n
	tab TEST_RETEST VISIT_TYPE
	sort MOMID VISIT_TYPE Date
	
	*check time between test and retest
	bysort MOMID (Date) : gen RETEST_TIME = round(Date-Date[1])
	replace RETEST_TIME =. if TEST_RETEST==1
	
	*some with empty retest dates
	drop if Date==.
	
	bysort MOMID VISIT_TYPE: gen RETEST_AVAILABLE = 1 if _N==2
	
	save "$ids/ZM",replace
	
	
**#Stack all the test-retest files
	use "$ids/CMC", clear
	gen SITE="India-CMC"
	append using "$ids/SAS", gen(SAS)
	replace SITE ="India-SAS" if SAS==1
	drop SAS
	append using "$ids/KY", gen(KY)
	replace SITE ="Kenya" if KY==1
	drop KY
	append using "$ids/PK", gen (PK)
	replace SITE="Pakistan" if PK==1
	drop PK
	append using "$ids/ZM", gen(ZM)
	replace SITE="Zambia" if ZM==1
	drop ZM
	
	
	tab SITE RETEST_AVAILABLE,miss
	
	drop SITE_ZM
	drop RETEST_TIME
	
	sort SITE MOMID TEST_RETEST
	order SITE MOMID TEST_RETEST Date RETEST_AVAILABLE
	replace Date = floor(Date)
	
	save "$ids/RETEST_IDS", replace
	
	*get a list of all IDs
	bysort MOMID (Date) : gen NUM = _n
	keep if NUM == 1 & RETEST_AVAILABLE==1 
	merge 1:m MOMID using "$wrk/MAT_DEMOGRAPHIC.dta"
	keep if _merge==3
	
	drop _merge
	
	*merge with the larger set
	merge 1:m MOMID PREGID using "$wrk/FATIGUE_250_FINAL_WIDE.dta"
	recode _merge (3=1)
	replace RETEST_AVAILABLE =0 if RETEST_AVAILABLE==.
	
	
	foreach site in "India-CMC" "India-SAS" "Kenya" "Pakistan" "Zambia" {
		qui ttest AGE  if SITE=="`site'",by(RETEST_AVAILABLE)
		qui return list
		disp as text "`site' - AGE (ttest):" _col(35) "mean overall:  "  as result %5.1f r(mu_1) as text ", mean retests =" as result %5.1f r(mu_2) _newline _col(35) as text "Ha diff<0 : "  as result %5.3f r(p_l) as text ", Ha diff!=0 : " as result %5.4f r(p) as text ", Ha diff>0 : " as result %5.3f r(p_u)
		
		qui ttest SCHOOL_YRS  if SITE=="`site'",by(RETEST_AVAILABLE)
		qui return list
		disp as text "`site' - SCHOOL_YRS (ttest):" _col(35) "mean overall:  "  as result %5.1f r(mu_1) as text ", mean retests =" as result %5.1f r(mu_2) _newline _col(35) as text "Ha diff<0 : "  as result %5.3f r(p_l) as text ", Ha diff!=0 : " as result %5.4f r(p) as text ", Ha diff>0 : " as result %5.3f r(p_u)
		
		qui tab PARITY_CAT RETEST_AVAILABLE if SITE=="`site'", chi
		qui return list
		disp as text "`site' PARITY (Chi^2)  = " _col(35) "p = " as result %5.3f  r(p)
		disp as text "______________________________________________"
	}
	*slightly more educated in CMC, slightly less educated in Zambia
	
	**#Merge all fatigue data and retest IDs
	
	clear
	use "$wrk/mnh26.dta" 	
	merge m:1 MOMID Date using "$ids/RETEST_IDS"
	tab _merge SITE
	
	
	levelsof(MOMID) if _merge==2, local(moms)
	
	levelsof(MOMID) if _merge==2,local(moms) clean 
	*list SITE MOMID TEST_RETEST RETEST_AVAILABLE Date if inrange(MOMID,`moms')
	foreach l of local moms {
		list SITE MOMID  FTGE_OBSTDAT  Date _merge if MOMID=="`l'" & RETEST_AVAILABLE==1, clean 
	}
	
	keep if _merge==3

	*only keep RETEST_AVAILABLE = 1 if merged	
	replace RETEST_AVAILABLE = 0 if _merge !=3
	replace RETEST_AVAILABLE = 0 if RETEST_AVAILABLE!=1
	bysort MOMID PREGID: gen RETEST_CHECK=_N if RETEST_AVAILABLE==1
	replace RETEST_AVAILABLE=0 if RETEST_CHECK!=2
	
	drop _merge
	
	label define TEST_RETEST 1"Test" 2"Retest"
	label val TEST_RETEST TEST_RETEST
	
	**temporary: Stata will not allow the ICC to be run because one ID is present 4 times (she was test/retest at both ANC and PNC)
	*we will drop the PNC test-retest data
	bysort MOMID: gen VISTOTAL=_N
	tab VISTOTAL
	levelsof(MOMID)  if VISTOTAL == 4
	*we will give this mom a unique id for PNC visits
	replace MOMID = "REM-42-00026B" if MOMID== "REM-42-00026" & TYPE=="PNC"
	*drop if VISTOTAL == 4 & TYPE=="PNC"
	replace TYPE = "ANC" if inlist(TYPE_VISIT,1,2,4,5,13)
	replace TYPE = "PNC" if inlist(TYPE_VISIT,10,14)
	replace TYPE = "PNC" if inlist(TYPE_VISIT,13) & PREG_END_DATE<DATE & SITE=="Zambia"
	
	
	save "$wrk/Fatigue-Retests", replace


preserve
table1_mc , by(SITE) ///
vars(MARRIED cat \ AGE conts \ PARITY_CAT cat \ EDUCATED cat \ PAID_WORK cat \ CHEW_TOBACCO cat \ DRINK cat  ) ///
format(%2.0f) extraspace clear total(before) percformat(%5.1f)  percsign("") iqrmiddle(",") sdleft(" [±") sdright("]") gsdleft(" [×/") gsdright("]") onecol 
table1_mc_dta2docx using "All_PRISMA_Demographics.docx", replace
restore
