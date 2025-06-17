*PRISMA Variable Construction - Fetal Anomaly
*Purpose: This code constructs the Fetal Anomaly outcome, as requested by IHME
*Original Version: October 24, 2024
*Update: For 1-10-2025 data 

clear
set more off
cap log close

*Directory structure:

	// Erin's folders: 
global dir  "D:\Users\emoakley\Documents\Maternal Outcome Construction" 
global log "$dir/logs"
global do "$dir/do"
global output "$dir/output"

	// Stacked Data Folders (TNT Drive) - raw data 
	
global dadate "2025-04-18"
global da "Z:/Stacked Data/$dadate" // change date here as needed

	// Working Files Folder (TNT-Drive)
global wrk "Z:/Erin_working_files/data" // set pathway here 

global OUT "Z:\Outcome Data/$dadate"

global date "250428" // today's date

log using "$log/construct_fetal-anom_$date", replace

/*************************************************************************

*This file constructs the following variables: 
	
	INF_ANOMALY 
		Fetal anomaly as reported in CRF 11: CON_MALFORM_MHOCCUR
		
	INF_ANOMALY_TYPE
		Type of fetal anomaly as reported in CRF 11:
			1=Cleft lip/pallet
			2-Neural tube defect
			3=Abdominal wall defect
			4=Bladder exstrophy 
			5=Kidney malformation
			6=Anorectal malformation 
			7=Congenital heart defect 
			8=Bowel obstruction 
			9=Tracheoesophageal fistula 
			10=Hydrocephalus 
			11=Developmental dysplasia of hip 
			12=Skeletal dysplasia
			13=Club foot 
			66=Multiple anomalies 
			88=Other 
			99=Unknown 
			
*/
	
	/////////////////////////////////////////
	
	* Import data: 
	
	import delimited "$da/mnh11_merged", varn(1) case(preserve) bindquote(strict)
		
	gen INF_ANOMALY = M11_CON_MALFORM_MHOCCUR 
	replace INF_ANOMALY = 55 if M11_CON_MALFORM_MHOCCUR == 77 | M11_CON_MALFORM_MHOCCUR == 99 
	
	label var INF_ANOMALY "Fetal anomaly noted at time of delivery"
	
	gen INF_ANOMALY_COUNT = 0 if INF_ANOMALY == 1 

	foreach num of numlist 1/13 88  {
		
	replace INF_ANOMALY_COUNT = (INF_ANOMALY_COUNT + 1) if ///
		M11_CON_MALFORM_MHTERM_`num' == 1 
		
	}
	
	tab INF_ANOMALY_COUNT, m 
	
	* review other-specify anomalies: 
	list SITE M11_CON_MALFORM_SPFY_MHTERM INF_ANOMALY_COUNT ///
		if M11_CON_MALFORM_MHTERM_88==1
	
	list SITE M11_CON_MALFORM_SPFY_MHTERM if INF_ANOMALY_COUNT==2 & ///
		M11_CON_MALFORM_MHTERM_88==1
		
	* Reassign "Other" that actually falls in a listed category: 
		* Imperforate  anus - recategorize as 6=Anorectal malformation 
	replace M11_CON_MALFORM_MHTERM_6 = 1 if M11_CON_MALFORM_SPFY_MHTERM == ///
		"Imperforate  anus" 
	replace M11_CON_MALFORM_MHTERM_88 = 0 if M11_CON_MALFORM_MHTERM_6 == 1 & ///
		M11_CON_MALFORM_SPFY_MHTERM == "Imperforate  anus"
	
		
	replace M11_CON_MALFORM_MHTERM_88=0 if M11_CON_MALFORM_SPFY_MHTERM=="n/a" & ///
		INF_ANOMALY_COUNT==2 & M11_CON_MALFORM_MHTERM_88==1 
		
	*create separate indicator for extra digits:
	gen INF_ANOMALY_FINGERS = INF_ANOMALY if INF_ANOMALY !=1
	replace INF_ANOMALY_FINGERS = 0 if INF_ANOMALY==1 
	replace INF_ANOMALY_FINGERS = 1 if ///
		M11_CON_MALFORM_SPFY_MHTERM == "Extra digits on both upper arms" | ///
		M11_CON_MALFORM_SPFY_MHTERM == "Extra  digit" | ///
		M11_CON_MALFORM_SPFY_MHTERM == "Xtra digits " | ///
		M11_CON_MALFORM_SPFY_MHTERM == "Extra finger digits on both sides " | ///
		M11_CON_MALFORM_SPFY_MHTERM == "Extra fingers in both hands" | ///
		M11_CON_MALFORM_SPFY_MHTERM == ///
		"Extra finger digits- tied with nylon after consent of the mother. " | ///
		M11_CON_MALFORM_SPFY_MHTERM == "Extra figure digits" | ///
		M11_CON_MALFORM_SPFY_MHTERM == "Extra finger digit on left hand." | ///
		M11_CON_MALFORM_SPFY_MHTERM ==  "Extra digits on both hands" | ///
		M11_CON_MALFORM_SPFY_MHTERM == "ACC--Dysmorphism-polydactyly-cisterna " | ///
		M11_CON_MALFORM_SPFY_MHTERM == "Extra digit left hand " | ///
		M11_CON_MALFORM_SPFY_MHTERM == "Extra digits " | ///
		M11_CON_MALFORM_SPFY_MHTERM == "Polydactyl"
		
		
	list SITE M11_CON_MALFORM_SPFY_MHTERM INF_ANOMALY_FINGERS INF_ANOMALY_COUNT ///
		if M11_CON_MALFORM_MHTERM_88==1
		
	list SITE M11_CON_MALFORM_SPFY_MHTERM INF_ANOMALY_FINGERS INF_ANOMALY_COUNT ///
		if M11_CON_MALFORM_MHTERM_88==1 & INF_ANOMALY_FINGERS==1
	
	*replace "other" to "0" for extra digits anomalies only
	replace M11_CON_MALFORM_MHTERM_88=0 if M11_CON_MALFORM_MHTERM_88==1 & ///
		INF_ANOMALY_FINGERS==1 & ///
		M11_CON_MALFORM_SPFY_MHTERM != "ACC--Dysmorphism-polydactyly-cisterna "
		
	*re-run the count variable: 
	drop INF_ANOMALY_COUNT
	
	gen INF_ANOMALY_COUNT = 0 if INF_ANOMALY == 1 

	foreach num of numlist 1/13 88  {
		
	replace INF_ANOMALY_COUNT = (INF_ANOMALY_COUNT + 1) if ///
		M11_CON_MALFORM_MHTERM_`num' == 1 
		
	}
	
	replace INF_ANOMALY_COUNT = INF_ANOMALY_COUNT +1 if INF_ANOMALY_FINGERS==1
	
	tab INF_ANOMALY_COUNT, m 
	
	
	*type variable: 
	gen INF_ANOMALY_TYPE = .
	
	foreach num of numlist 1/13 88 {
	
	replace INF_ANOMALY_TYPE = `num' if M11_CON_MALFORM_MHTERM_`num' == 1 & ///
		INF_ANOMALY_COUNT ==1 
		
	}
	
	replace INF_ANOMALY_TYPE = 14 if INF_ANOMALY_FINGERS==1 & INF_ANOMALY_COUNT==1
	
	replace INF_ANOMALY_TYPE = 99 if INF_ANOMALY_COUNT == 0
	
	replace INF_ANOMALY_TYPE = 66 if INF_ANOMALY_COUNT >= 2 & INF_ANOMALY_COUNT!=.
	
	tab INF_ANOMALY_TYPE, m 
	
	label define anomalies 1 "1-Cleft lip/pallet" 2 "2-Neural tube defect" ///
		3 "3-Abdominal wall defect" 4 "4-Bladder exstrophy" ///
		5 "5-Kidney malformation" 6 "6-Anorectal malformation" ///
		7 "7-Congenital heart defect" 8 "8-Bowel obstruction" ///
		9 "9-Tracheoesophageal fistula" 10 "10-Hydrocephalus" ///
		11 "11-Developmental dysplasia of hip" 12 "12-Skeletal dysplasia" ///
		13 "13-Club foot" 14 "14-Extra digits (fingers/toes)" ///
		66 "66-Multiple anomalies" 88 "88-Other" ///
		99 "99-Unknown anomaly"
		
	label values INF_ANOMALY_TYPE anomalies
	
	tab INF_ANOMALY_TYPE, m 
	tab INF_ANOMALY_TYPE INF_ANOMALY, m 
	
	*individual indicator variables: 
	rename M11_CON_MALFORM_MHTERM_1 INF_ANOMALY_CLEFTLIP
	rename M11_CON_MALFORM_MHTERM_2 INF_ANOMALY_NEURAL
	rename M11_CON_MALFORM_MHTERM_3 INF_ANOMALY_ABDOMINAL
	rename M11_CON_MALFORM_MHTERM_4 INF_ANOMALY_BLADDER
	rename M11_CON_MALFORM_MHTERM_5 INF_ANOMALY_KIDNEY
	rename M11_CON_MALFORM_MHTERM_6 INF_ANOMALY_ANORECTAL
	rename M11_CON_MALFORM_MHTERM_7 INF_ANOMALY_HEART
	rename M11_CON_MALFORM_MHTERM_8 INF_ANOMALY_BOWEL
	rename M11_CON_MALFORM_MHTERM_9 INF_ANOMALY_TRACH
	rename M11_CON_MALFORM_MHTERM_10 INF_ANOMALY_HYDROCEPH
	rename M11_CON_MALFORM_MHTERM_11 INF_ANOMALY_HIPDYSP
	rename M11_CON_MALFORM_MHTERM_12 INF_ANOMALY_SKELDYSP
	rename M11_CON_MALFORM_MHTERM_13 INF_ANOMALY_CLUBFOOT
	rename M11_CON_MALFORM_MHTERM_88 INF_ANOMALY_OTHER
	rename M11_CON_MALFORM_MHTERM_99 INF_ANOMALY_UNKNOWN
	
	foreach var of varlist INF_ANOMALY_CLEFTLIP  ///
		INF_ANOMALY_ABDOMINAL INF_ANOMALY_BLADDER INF_ANOMALY_KIDNEY ///
		INF_ANOMALY_ANORECTAL INF_ANOMALY_HEART INF_ANOMALY_BOWEL ///
		INF_ANOMALY_TRACH INF_ANOMALY_HYDROCEPH INF_ANOMALY_HIPDYSP ///
		INF_ANOMALY_SKELDYSP INF_ANOMALY_CLUBFOOT INF_ANOMALY_OTHER ///
		INF_ANOMALY_FINGERS INF_ANOMALY_UNKNOWN {
			
	replace `var' = 77 if INF_ANOMALY == 0 
	replace `var' = 55 if INF_ANOMALY == 55
	replace `var' = 0 if INF_ANOMALY == 1 & `var' == 77
	
	tab `var' INF_ANOMALY, m 
			
		}
		
	replace INF_ANOMALY_UNKNOWN=1 if INF_ANOMALY_COUNT == 0 & INF_ANOMALY==1
	

	keep MOMID PREGID INFANTID SITE INF_ANOMALY INF_ANOMALY_TYPE ///
		INF_ANOMALY_*
		
	order INF_ANOMALY INF_ANOMALY_TYPE INF_ANOMALY_COUNT, after(INFANTID)
	
	order INF_ANOMALY_OTHER, after(INF_ANOMALY_TRACH)
	order INF_ANOMALY_FINGERS, after(INF_ANOMALY_TRACH)
		
	*checks:
	list if INF_ANOMALY_COUNT==2
	
	*labels: 
	label var INF_ANOMALY_TYPE "Type of fetal anomaly (categorical)"
	label var INF_ANOMALY_COUNT "Number of fetal anomalies reported"
	label var INF_ANOMALY_CLEFTLIP "Fetal anomaly: cleft lip/pallet"
	label var INF_ANOMALY_HYDROCEPH "Fetal anomaly: hydrocephalus"
	label var INF_ANOMALY_HIPDYSP "Fetal anomaly: Developmental hip dysplasia"
	label var INF_ANOMALY_SKELDYSP "Fetal anomaly: Skeletal dysplasia"
	label var INF_ANOMALY_CLUBFOOT "Fetal anomaly: Club foot"
	label var INF_ANOMALY_NEURAL "Fetal anomaly: Neural tube defect"
	label var INF_ANOMALY_ABDOMINAL "Fetal anomaly: Abdominal wall defects"
	label var INF_ANOMALY_BLADDER "Fetal anomaly: Bladder exstrophy"
	label var INF_ANOMALY_KIDNEY "Fetal anomaly: Kidney malformation"
	label var INF_ANOMALY_ANORECTAL "Fetal anomaly: Anorectal malformation"
	label var INF_ANOMALY_HEART "Fetal anomaly: Congenital heart defect"
	label var INF_ANOMALY_BOWEL "Fetal anomaly: Bowel obstruction"
	label var INF_ANOMALY_TRACH "Fetal anomaly: Tracheoesophageal fistula"
	label var INF_ANOMALY_FINGERS "Fetal anomaly: Extra digits (fingers or toes)"
	label var INF_ANOMALY_OTHER "Fetal anomaly: Other"
	label var INF_ANOMALY_UNKNOWN "Fetal anomaly: Unknown"
	
	
	* Save file to main folder: 
	
	save "$OUT/INF_ANOMALY", replace 
	
	
	
	
	
	
	