*Savannah O'Malley (savannah.omalley@gwu.edu)
*This file generates an indicator for whether each participant is "expected" at each type visit

**CHANGE THE BELOW BASED ON WHICH DATA YOU ARE WORKING WITH

global datadate "2026-01-30"
global outcomes "Z:\Outcome Data/$datadate"

cap use "$outcomes/MAT_ENROLL.dta"
disp _rc
if _rc != 0 {
	import excel "$outcomes/MAT_ENROLL.xlsx", sheet("Sheet 1") firstrow clear
	isid PREGID
	save "$outcomes/MAT_ENROLL.dta" , replace
}

keep if ENROLL == 1


*if the above variable are strings:
local varlist  ENROLL_SCRN_DATE M01_US_OHOSTDAT PREG_START_DATE EDD_BOE ENROLL_ONTIME_WINDOW ENROLL_LATE_WINDOW ANC20_ONTIME_WINDOW ANC20_LATE_WINDOW ANC28_ONTIME_WINDOW ANC28_LATE_WINDOW ANC32_ONTIME_WINDOW ANC32_LATE_WINDOW ANC36_ONTIME_WINDOW ANC36_LATE_WINDOW 
str2date `varlist'

merge 1:1 MOMID PREGID using "$outcomes/MAT_ENDPOINTS.dta", nogen force
keep if ENROLL == 1
gen uploaddate="$datadate"
gen UploadDate=date(uploaddate, "YMD")
format UploadDate %td

gen ANC20_LATE = ANC20_LATE_WINDOW
gen ANC36_LATE = ANC36_LATE_WINDOW
**generate expected observations at each time point

**#Enroll/ANC20
gen ANC20_EXP = 0 
replace ANC20_EXP = 1 if ///
ANC20_LATE < UploadDate & ///
(PREG_END_GA>181 | PREG_END_GA ==.) & ///
((STOP_DATE>ANC20_LATE) | (STOP_DATE == . )) 
label var ANC20_EXP "Expected number (passed ANC20 late window without closeout or end preg)"

**#ANC32/36
gen ANC32_EXP = 0 
replace ANC32_EXP = 1 if ///
ANC36_PASS_LATE ==1 & /// the late window for ANC36 has passed
(PREG_END_GA>237 | PREG_END_GA ==.) & /// she delivered at 34+ weeks
((STOP_DATE>ANC36_LATE) | (STOP_DATE == . )) 
**note this is from monitoring report code
label var ANC32_EXP "Expected number (passed ANC36 late window without closeout or end preg)"


**#Trimester 1
	gen TRI1_WINDOW = PREG_START_DATE + 97 //end of 1st tri
	gen TRI2_WINDOW = PREG_START_DATE + 195 //end of 2nd tri
	gen TRI3_WINDOW = PREG_START_DATE + 237 //34w6d
			*if she delivered before end of ANC32 window, we will not "Expect" her for trimester 3 (most T3 assessments done at ANC32)
	format TRI1_WINDOW %td
	
	foreach num of numlist 1/3 { 
		
	gen 	TRI_`num'_EXP = 1 if ///
				date("$datadate", "YMD") >= TRI`num'_WINDOW 
				*woman passed trimester by date of upload	
	replace TRI_`num'_EXP = 0 if ///
				date("$datadate", "YMD") < TRI`num'_WINDOW  	
	replace TRI_`num'_EXP = 0 if ///
				(STOP_DATE < TRI`num'_WINDOW ) & CLOSEOUT == 1
				*stop date is before end of tri, not expected
	replace TRI_`num'_EXP = 0 if ///
				(PREG_END_DATE < TRI`num'_WINDOW ) 
				*delivered before end of tri, not expected
	}
	
	replace TRI_1_EXP = 0 if ENROLL_SCRN_DATE > TRI1_WINDOW
		*not expected if enrolled after T1
				
gen PNC6_EXP = 0 
replace PNC6_EXP = 1 if ///
((PREG_END_DATE + 104) < UploadDate)  & ///
((STOP_DATE > (PREG_END_DATE + 104)) | STOP_DATE==.)
label var PNC6_EXP "Expected number (passed PNC6 window without closeout)"

save "Z:\Savannah_working_files\Expected-obs/Expected_obs-$datadate.dta" , replace
