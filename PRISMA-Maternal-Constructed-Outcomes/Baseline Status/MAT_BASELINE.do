*PRISMA Maternal Variable Construction Code
*Purpose: This code constructs baseline variables for the PRISMA study. Combines prior MAT_DEMOGRAPHIC, MAT_PREVPREG_COMPLICATIONS, and MAT_RISKS datasets. 
*Original Versions: 
	*MAT_DEMOGRAPHIC: Xiaoyan xyh@gwu.edu and Precious Williams
	*MAT_PREVPREG_COMPLICATIONS: Erin Oakley - November 2025
	*MAT_RISKS: Erin Oakley - August 2025
*Current version: EKC (emmacook@gwu.edu) - October 1, 2026
	*Translated MAT_DEMOGRAPHIC from R to Stata
		*Original R Code: https://github.com/PRiSMA-Study/PRISMA-Public/blob/8fe6c0dd2b60a1ceb961bbbd80066251e7cfd729/PRISMA-Demographic-Report/1.%20Data%20prep.R
	*Combined 3 codes (as noted above)
	*Added repeat enrollment variables (ENR_PREG_NUM, ENR_PREG_NUM_ORDER).
	
	
/* Notes:
Shared M03 OE vars with sites for recoding
	M03_HEAD_HH_SPFY_FCORRES
	M03_JOB_OTHR_SPFY_SCORRES
	M03_TOILET_SHARE_FCORRES
	M03_H2O_SPFY_FCORRES
	M03_PD_BIRTH_OTHR_SPFY_OHOLOC
	M03_STOVE_SPFY_FCORRES M03_STOVE_FUEL_SPFY_FCORRES_88
	
TO DO:
	Add STOVE_FUEL
	Improve wealth quintile construction (in progress)
*/

/*
Inputs:
	MAT_STUDYSTATUS (previously MAT_ENROLL)
	MNH00
	MNH01
	MNH03
	MNH04
	MNH05
	MNH06
	
This code uses the user written command str2date
*/

**# Step 0: Copy stacked data from TNT to local AWS environment and convert to 
* 			.dta fomat using `Import Data.dta'

clear
set more off

*Update Data Date
	gl datadate "2026-09-18" // data date

*Get today's date:
local today: di %td_CCYY_NN_DD daily("`c(current_date)'", "DMY")
global date = subinstr(strltrim("`today'"), " ", "-", .)

			
**# Directory structure:
	*Emma's local folders in AWS
		gl ldata "D:\Users\emmacook\Documents/data/$datadate"
	*Data folders in TNT Drive
		gl da "Z:/Stacked Data/$datadate"
		*gl out "Z:/Outcome Data/$datadate" // for dates prior to 9/18/2026
		gl out "Z:/Constructed Data/$datadate"
	*Working files folder (TNT Drive)	
		gl emma "Z:/Emma_working_files/Constructed Data_working files/MAT_BASELINE"
		gl wrk "$emma/$datadate"
			cap mkdir "$wrk" // creates directory if it doesn't exist	
		gl temp "$wrk/temp"
			cap mkdir "$temp"	
		gl output "$wrk/output"
			cap mkdir "$output"

			
**###################### DATA SETUP ############################

**# MNH forms: 00, 01, 03, 04, 05, 06

tempfile mnh01 mnh04 mnh05 mnh06

*mnh01 - keep enrollment visit only
	use "$ldata/mnh01.dta",clear
	keep if M01_TYPE_VISIT == 1
	bysort SITE SCRNID MOMID PREGID: keep if _N == 1 // does not drop anyone
	keep SITE SCRNID M01_US_EDD_BRTHDAT_FTS* M01_FETUS_CT_PERES_US
	save `mnh01'

* mnh04 - keep enrollment visit only
	use "$ldata/mnh04.dta",clear
	keep if M04_TYPE_VISIT == 1
	save `mnh04'

* mnh05 - keep enrollment visit only
	use "$ldata/mnh05.dta",clear
	keep if M05_TYPE_VISIT == 1
	keep SITE MOMID PREGID M05_WEIGHT_PERES M05_HEIGHT_PERES M05_MUAC_PERES
	save `mnh05'

* mnh06 - keep enrollment visit only
	use "$ldata/mnh06.dta",clear
	keep if M06_TYPE_VISIT == 1
	keep SITE MOMID PREGID M06_SINGLETON_PERES
	save `mnh06'


* Start with MAT_ENROLL, merge in MNH00-01, 03-06
use "$out/MAT_STUDYSTATUS.dta", clear 
merge 1:1 SITE SCRNID using "$ldata/mnh00.dta", ///
	keepusing(*KNOWN_DOBYN_SCORRES M00_BRTHDAT M00_ESTIMATED_AGE ///
	M00_SCHOOL_YRS_SCORRES M00_SCHOOL_SCORRES) keep(master match) nogen
merge 1:1 SITE SCRNID using `mnh01', keep(master match) nogen
merge 1:1 SITE MOMID PREGID using "$ldata/mnh03.dta", keep(master match) nogen
merge 1:1 SITE MOMID PREGID using `mnh04', keep(master match) nogen
merge 1:1 SITE MOMID PREGID using `mnh05', keep(master match) nogen
merge 1:1 SITE MOMID PREGID using `mnh06', keep(master match) nogen
  
**###################### CLEANING ############################

* Set negatives to missing
	quietly ds, has(type numeric)
	foreach v of varlist `r(varlist)' {
		display "`v'"
		replace `v' = . if `v' < 0
	}
 
* Set invalid dates to .
	foreach x in M01_US_OHOSTDAT PREG_START_DATE EST_CONCEP_DATE_US ///
				 EST_CONCEP_DATE_LMP M00_BRTHDAT M01_US_EDD_BRTHDAT_FTS1 ///
				 M01_US_EDD_BRTHDAT_FTS2 M01_US_EDD_BRTHDAT_FTS3 ///
				 M01_US_EDD_BRTHDAT_FTS4 ENROLL_SCRN_DATE {
		
		*convert strings to dates
		cap str2date `x'
		
		*check lowest dates
		sort `x'
		list `x' in 1/20 if `x'!=.
		
		*replace any invalid dates with .
		replace `x' = . if inlist(`x', daily("1907-07-07", "YMD"), ///
			daily("1905-05-05","YMD"))
	}  
		 
**###################### WEALTH QUINTILES ############################	

*Clean asset variables		
	* Durable assets
		gl assets RADIO TV FRIDGE COMPUTER WATCH BIKE MOTORCYCLE CAR BOAT ///
		   CART PLOUGH FOAM_MATT STRAW_MATT SPRING_MATT SOFA LANTERN SEW ///
		   WASH BLENDER MOSQUITO_NET TRICYCLES TABLES CABINETS SAT_DISH ///
		   DVD_CD AIRCON TRACTOR
		   
	*Additional assets to consider for inclusion
		gl assets2 LAND LIVESTOCK CATTLE GOAT SHEEP POULTRY PIG DONKEY ///
			HORSE ANIMAL_OTHR
		 
		foreach a in $assets $assets2 {
			gen `a'_ind = M03_`a'_FCORRES if inlist(M03_`a'_FCORRES,0,1)
			la var `a'_ind "`a'_ind"
			drop M03_`a'_FCORRES
		}
		   
	* Other variables
		local H2O "watersrc"
		local TOILET "toilet"
		local EXT_WALL "wall"
		local FLOOR "floor"
		local ROOF "roof"
		 
		foreach x in H2O TOILET EXT_WALL FLOOR ROOF {	
			gen ``x''_ind = M03_`x'_FCORRES
			replace ``x''_ind = . if inlist(``x'',55,77,99)
			qui tab ``x''_ind, gen(``x''_)
			
			/*graph categories by site 
			catplot, over(`x'_FCORRES) over(SITE) percent(SITE) ///
				asyvars stack recast(bar) title(`x'_FCORRES) ///
				name(`x', replace) 
			graph export "$output/`x'.png", replace*/
			
		}  
		
		gen rooms_ind = M03_HOUSE_ROOMS_FCORRES if ///
			M03_HOUSE_ROOMS_FCORRES>=0
		replace rooms_ind = . if M03_HOUSE_ROOMS_FCORRES<0
		la var rooms_ind "HOUSE_ROOMS_ind"
		drop M03_HOUSE_ROOMS_FCORRES
	
	gen site_loop = SITE 
	replace site_loop = "IndiaCMC" if SITE == "India-CMC"
	replace site_loop = "IndiaSAS" if SITE == "India-SAS"
	gl sites Ghana IndiaCMC IndiaSAS Kenya Pakistan Zambia

*Set up asset lists for PCA
	*Count assets by site
		
		foreach s in $sites {
			gen ind_num_`s' = 0
			
			foreach a in $assets {
				qui sum `a'_ind if site_loop == "`s'"
				gen `a'_ind_`s' = r(max) if site_loop=="`s'"
				replace ind_num_`s' = ind_num_`s' + 1 if `a'_ind_`s' ==1
			}
		}
		
	*Create local lists of assets by site
		foreach s in $sites {
			local `s' // create blank locals by site
			
			display "`s'"
			
			foreach a in $assets {
				qui sum `a'_ind_`s'
				if r(max)==1 {
					local `s' ``s'' `a'_ind 
					
					*sum `a'_ind if site_loop=="`s'" // check vars
				}
			}
			 
			qui su ind_num_`s' if site_loop=="`s'", meanonly
				// get # assets to display
			
			*Display number of assets and asset list
			di _newline as text "`s' (" r(max) " assets)"
			di "`s': ``s''"
		}
		     
		**UPDATE ZAMBIA LIST - additional vars collected starting March 2024
			* have too much missingness, need to drop
			local dropvars FOAM_MATT_ind SPRING_MATT_ind LANTERN_ind ///
				SEW_ind MOSQUITO_NET_ind SAT_DISH_ind
			local Zambia : list Zambia - dropvars
			disp "`Zambia'"
			
		**UPDATE INDIA-CMC LIST
			local IndiaCMC_assets `IndiaCMC' watersrc_* toilet_* wall_* ///
				floor_* roof_*
			local IndiaCMC `IndiaCMC_assets'
				*Check that it contains the intended variables
				unab IndiaCMC_vars : `IndiaCMC'
				display "`IndiaCMC_vars'"
					
**# PROPOSED UPDATES
	*Need to drop assets (within site) owned by nearly all or nearly zero households
	*Add water, toilet, building materials for all sites - directional coding or indicators for improved
		  
* PCA
	foreach s in $sites {
		pca ``s'' if site_loop=="`s'"
		predict ASSET_INDEX_`s' if site_loop=="`s'", score
		xtile WEALTH_QUINT_`s' = ASSET_INDEX_`s'  if site_loop=="`s'", nq(5)
		la var ASSET_INDEX_`s' "PCA Asset Score, `s'"
		la var WEALTH_QUINT_`s' "Asset Quintile, `s'"
	} 
		
* Overall wealth quintiles
	gen WEALTH_QUINT = . 
	
	foreach s in $sites {
		replace WEALTH_QUINT = WEALTH_QUINT_`s' if site_loop == "`s'"
		replace WEALTH_QUINT = 55 if WEALTH_QUINT_`s' == . & ///
			site_loop == "`s'"
	}
	tab WEALTH_QUINT, m 
	tab WEALTH_QUINT site, m col
	label var WEALTH_QUINT "Asset Quintile (site-specific)"
  
**################ CONSTRUCT DEMOGRAPHIC VARIABLES ################

* Female household head
	gen hh_head_female = .
	replace hh_head_female = 1 if inlist(M03_HEAD_HH_FCORRES,1,3)
	replace hh_head_female = 0 if inlist(M03_HEAD_HH_FCORRES,2,4)

	*Clean free text responses
	gen hh_head_clean = itrim(strtrim(strlower(M03_HEAD_HH_SPFY_FCORRES)))
		// itrim replaces multiple spaces with single space
		// strtrim removes leading and trailing spaces
		// strlower makes string lowercase
	tab hh_head_clean SITE if M03_HEAD_HH_FCORRES==88 & ///
		hh_head_clean!="" & hh_head_clean!="missing" ///
		& hh_head_clean!="n/a"
	tab hh_head_female,m
	
	replace hh_head_female = 1 if inlist(hh_head_clean, ///
		"aunt","auntie","aunty","babhi")
	replace hh_head_female = 1 if inlist(hh_head_clean, ///
		"biological mother","daadi saas","dadi saas")
	replace hh_head_female = 1 if inlist(hh_head_clean, ///
		"grand mother","grandmother","grandmother in law")
	replace hh_head_female = 1 if inlist(hh_head_clean, ///
		"grandmother's of the participant","grandmothet")
	replace hh_head_female = 1 if inlist(hh_head_clean, ///
		"her mother","husband grandmother")
	replace hh_head_female = 1 if inlist(hh_head_clean, ///
		"mother","mother of participant")
	replace hh_head_female = 1 if inlist(hh_head_clean, ///
		"mother of pregnant woman","mother of the pregnant woman")
	replace hh_head_female = 1 if inlist(hh_head_clean, ///
		"mother, husband","participant grandmother")
	replace hh_head_female = 1 if inlist(hh_head_clean, ///
		"participant mother","participant's mother")
	replace hh_head_female = 1 if inlist(hh_head_clean, ///
		"pregnant woman's mother","sister")
	replace hh_head_female = 1 if inlist(hh_head_clean, ///
		"sister in-law","sister- in- law","step mother")
		
	replace hh_head_female = 0 if inlist(hh_head_clean, ///
		"biological father","boy friend","boyfriend","brother", ///
		"brother in law","brother in-law","brother inlaw")
	replace hh_head_female = 0 if inlist(hh_head_clean, ///
		"brother-in -law","brother-in-law","brothers", ///
		"brought in law","dada sasur","fathe to a participant")
	replace hh_head_female = 0 if inlist(hh_head_clean, ///
		"father","father of participant","father of pregnant woman", ///
		"father of pregnant women 9","father of the pregnant woman")
	replace hh_head_female = 0 if inlist(hh_head_clean, ///
		"fiance","fiancy","grand father","grand father in law", ///
		"grandfather","her sister husband")
	replace hh_head_female = 0 if inlist(hh_head_clean, ///
		"husband -brother","husband brother","husband elder brother", ///
		"husband of pregnant woman's aunt")
	replace hh_head_female = 0 if inlist(hh_head_clean, ///
		"husband to participant's sister","husband to sister", ///
		"jeth","jeth g","jeth ji")
	replace hh_head_female = 0 if inlist(hh_head_clean, ///
		"jeth( brother in law)","jhet ji","participant father", ///
		"pregnant woman's father","pw k jeth g")
	replace hh_head_female = 0 if inlist(hh_head_clean, ///
		"sister husband","spouse","spouse to aunt","uncle")
	
	replace hh_head_female = 88 if inlist(hh_head_clean, ///
		"both","both husband and wife","both spouse and participant", ///
		"cousin","friend","landlord")
	replace hh_head_female = 88 if inlist(hh_head_clean, ///
		"luo","parent","parent of the participant","parents", ///
		"rent","renting")
		
	*Check for new responses
	tab hh_head_clean if hh_head_female==. & M03_HEAD_HH_FCORRES==88 & ///
		hh_head_clean!="" & hh_head_clean!="missing" ///
		& hh_head_clean!="n/a", m 	
		
		*If new responses - break code - code new responses
		count if hh_head_female==. & M03_HEAD_HH_FCORRES==88 & ///
			hh_head_clean!="" & hh_head_clean!="missing" ///
			& hh_head_clean!="n/a"
		
		if r(N) > 0 {
			di as error ///
				"`r(N)' new M03_HEAD_HH_SPFY_FCORRES responses require coding"
			tab hh_head_clean if hh_head_female==. & ///
				M03_HEAD_HH_FCORRES==88 & ///
				hh_head_clean!="" & hh_head_clean!="missing" ///
				& hh_head_clean!="n/a"
			exit 9
		}

	 
	*update hh_head_female
		replace hh_head_female=88 if hh_head_female==. & M03_HEAD_HH_FCORRES==88
		replace hh_head_female=55 if hh_head_female==.
		tab M03_HEAD_HH_FCORRES hh_head_female,m
	 
* Household size
	gen hh_size = M03_HOUSE_OCC_TOT_FCORRES if ///
		inrange(M03_HOUSE_OCC_TOT_FCORRES,1,54)
  
*Household has improved toilet facility
	gen toilet_improved = . 
	replace toilet_improved = 1 if inlist(M03_TOILET_FCORRES,1,2,3,5,6,7,9)
	replace toilet_improved = 0 if inlist(M03_TOILET_FCORRES,4,8,10,11,12)

	*Clean free text responses
	gen toilet_clean = itrim(strtrim(strlower(M03_TOILET_SPFY_FCORRES)))
	tab toilet_clean SITE if toilet_clean!="n/a" & toilet_clean!="na"
	 
	replace toilet_improved = 0 if ///
		inlist(toilet_clean,"bush","forest","open field")
	
	replace toilet_improved = 88 if toilet_improved==. & M03_TOILET_FCORRES==88
	replace toilet_improved = 55 if toilet_improved==.
	tab toilet_improved,m 

*Household toilet facility is shared
	gen toilet_shared = M03_TOILET_SHARE_FCORRES if ///
		inlist(M03_TOILET_SHARE_FCORRES,0,1)
	replace toilet_shared = 0 if toilet_shared==. & M03_TOILET_FCORRES==12

*Household has an improved water source
	gen water_improved = .
	replace water_improved = 1 if ///
		inlist(M03_H2O_FCORRES,1,2,3,4,5,7,9,10,11,13,14)
	replace water_improved = 0 if inlist(M03_H2O_FCORRES,6,8,12)
	
	gen water_clean = itrim(strtrim(strlower(M03_H2O_SPFY_FCORRES)))
	tab water_clean SITE if water_clean!="n/a"
	 
	tab water_clean if water_clean!="n/a"

	replace water_improved = 1 if inlist(water_clean, ///
		"bole hole","bore hole","borehole","bow hole", ///
		"public borehole","mini tank","ro plant", ///
		"ro water purifier","water purifier")
	replace water_improved = 1 if inlist(water_clean, ///
		"piped,water in another compound")
		
	replace water_improved = 0 if inlist(water_clean, ///
		"black volta","dam","damp","lake", ///
		"river","river water","stream water", ///
		"water from rock")
	
	replace water_improved = 88 if water_improved==. & M03_H2O_FCORRES==88
	replace water_improved = 55 if water_improved==.
	 
*Polluted cooking fuel
**# REVISIT DEFINITION
/*
	gen STOVE_FUEL = .
	replace STOVE_FUEL = 0 if inlist(M03_STOVE_FCORRES,1,2,3,4,5,10)
	replace STOVE_FUEL = 1 if inlist(M03_STOVE_FCORRES,7,8,9,11)
	replace STOVE_FUEL = 0 if M03_STOVE_FCORRES == 6 & ///
		(M03_STOVE_FUEL_FCORRES_1 == 1 | ///
		 M03_STOVE_FUEL_FCORRES_2 == 1 | ///
		 M03_STOVE_FUEL_FCORRES_3 == 1 | ///
		 M03_STOVE_FUEL_FCORRES_15 == 1)
	replace STOVE_FUEL = 1 if M03_STOVE_FCORRES == 6 & ///
		(M03_STOVE_FUEL_FCORRES_4  == 1 | ///
		 M03_STOVE_FUEL_FCORRES_5  == 1 | ///
		 M03_STOVE_FUEL_FCORRES_6  == 1 | ///
		 M03_STOVE_FUEL_FCORRES_7  == 1 | ///
		 M03_STOVE_FUEL_FCORRES_8  == 1 | ///
		 M03_STOVE_FUEL_FCORRES_9  == 1 | ///
		 M03_STOVE_FUEL_FCORRES_10 == 1 | ///
		 M03_STOVE_FUEL_FCORRES_11 == 1 | ///
		 M03_STOVE_FUEL_FCORRES_12 == 1 | ///
		 M03_STOVE_FUEL_FCORRES_13 == 1 | ///
		 M03_STOVE_FUEL_FCORRES_14 == 1)
	tab STOVE_FUEL,m
	*/
	
*Participant's access to a mobile phone
	gen phone_access = M03_MOBILE_ACCESS_FCORRES
	replace phone_access = 3 if phone_access==0
	replace phone_access = . if inlist(phone_access,55,77)
	la define phone_access 1 "Own mobile phone" 2 "Share mobile phone" ///
		3 "No access to mobile phone"
	la values phone_access phone_access
	
*Someone in the participant's household smokes
	gen hh_smoke = M03_SMOKE_HHOLD_OECOCCUR if ///
		inlist(M03_SMOKE_HHOLD_OECOCCUR,0,1)
		
*Maternal age
	*Calculate age from date of birth and enrollment date
	gen age_temp = floor((ENROLL_SCRN_DATE-M00_BRTHDAT)/365) if ///
		ENROLL_SCRN_DATE!=. & M00_BRTHDAT!=.

	*Maternal age at enrollment using calculated age or estimated age
		* Plausible range: 
			*15-77 in Ghana, Pakistan, Zambia; 18-77  in India, Kenya
		gen min_age = 15 // Ghana, Pakistan, Zambia
		replace min_age = 18 if inlist(SITE, "India-CMC", "India-SAS", "Kenya")
		
		gen mat_age = age_temp if inrange(age_temp,min_age,77)
		replace mat_age = M00_ESTIMATED_AGE if mat_age==. & ///
			inrange(M00_ESTIMATED_AGE, min_age, 77)
			 
		*These ages are confirmed to be correct
		*Kenya allows enrollment of emancipated minors <18
		replace mat_age = M00_ESTIMATED_AGE if mat_age==. & ///
			inlist(MOMID,"KEARC01918","KEARC01278","KEARC01765", ///
			"KEARC02166","KEARC01980","KEARC01798") 		
		
		tab SITE M00_ESTIMATED_AGE if M00_ESTIMATED_AGE<min_age
		gen enr_bel_min_age = 1 if M00_ESTIMATED_AGE<min_age & ENROLL==1 
		*browse if enr_bel_min_age==1
		drop age_temp min_age enr_bel_min_age
		
	*Participant is younger than 18 years at enrollment
		gen mat_age_under18 = (mat_age < 18) if mat_age!=.	
		
	*Age group
		gen AGE_GROUP = 1 if mat_age <20 & mat_age > 0 
		replace AGE_GROUP = 2 if mat_age >= 20 & mat_age <= 34 
		replace AGE_GROUP = 3 if mat_age >=35 & mat_age != . 
		replace AGE_GROUP = 55 if mat_age == . | mat_age <=0
	
 
* Anthropometry
	*BMI
	gen bmi_enroll = M05_WEIGHT_PERES / ((M05_HEIGHT_PERES / 100)^2) ///
		if M05_WEIGHT_PERES > 0 & M05_HEIGHT_PERES > 0
	
	gen bmi_level_enroll = .
	replace bmi_level_enroll = 1 if bmi_enroll<18.5
	replace bmi_level_enroll = 2 if bmi_enroll>=18.5 & bmi_enroll<25
	replace bmi_level_enroll = 3 if bmi_enroll>=25 & bmi_enroll<30
	replace bmi_level_enroll = 4 if bmi_enroll>=30 & bmi_enroll!=.
	la define bmi_level 1 "<18.5" 2 ">=18.5 & <25" 3 ">=25 & <30" 4 ">=30"
	la values bmi_level_enroll bmi_level
	
	*Height
	gen height_group = .
	replace height_group = 1 if M05_HEIGHT_PERES>0 & M05_HEIGHT_PERES<145
	replace height_group = 2 if M05_HEIGHT_PERES>=145 & M05_HEIGHT_PERES<150
	replace height_group = 3 if M05_HEIGHT_PERES>=150 & M05_HEIGHT_PERES<155
	replace height_group = 4 if M05_HEIGHT_PERES>=155 & M05_HEIGHT_PERES!=.
	la define height_group 1 "<145" 2 ">=145 & <150" 3 ">=150 & <155" 4 ">=155"
	la values height_group height_group
	
	*MUAC
	gen muac_enroll = M05_MUAC_PERES if M05_MUAC_PERES>=0

* Education
	*Years of schooling with age validity check
	gen school_yrs = M00_SCHOOL_YRS_SCORRES if M00_SCHOOL_YRS_SCORRES>=0
	replace school_yrs = 0 if school_yrs==. & M00_SCHOOL_SCORRES == 0
	replace school_yrs = . if school_yrs > mat_age & school_yrs!=. & mat_age!=.
	
	*Participant has attended school
	gen educated = M00_SCHOOL_SCORRES if inlist(M00_SCHOOL_SCORRES,0,1)
	replace educated = 1 if school_yrs>=1 & school_yrs!=.	
	
	*Schooling More/Less than 10: 
	gen SCHOOL_MORE10 = 0 if school_yrs >=0 & school_yrs <=10
	replace SCHOOL_MORE10 = 1 if school_yrs > 10 & school_yrs != . 
	replace SCHOOL_MORE10 = 55 if school_yrs == . 
	
	*Schooling More/Less than 12: 
	gen SCHOOL_MORE12 = 0 if school_yrs >=0 & school_yrs <=12
	replace SCHOOL_MORE12 = 1 if school_yrs > 12 & school_yrs != . 
	replace SCHOOL_MORE12 = 55 if school_yrs == .  
	
* Marital status
	*Participant is married or cohabiting
	recode M03_MARITAL_SCORRES (1 2 = 1) (3/5 = 0) (55 77 = .), gen(married)
	
	*Detailed marital status
	gen marry_status = M03_MARITAL_SCORRES if inrange(M03_MARITAL_SCORRES,1,5)
	la define marry_status 1 "Married" 2 "Cohabitating" 3 "Divorced" ///
		4 "Widowed" 5 "Single, never married"
	la values marry_status marry_status
		
	*Age at marriage
	gen marry_age = M03_MARITAL_AGE if M03_MARITAL_AGE > 0
		*check if marriage age > participant age
		gen marry_age_flag = 1 if marry_age > mat_age & marry_age!=. & mat_age!=.
		*browse SITE MOMID PREGID M03_MARITAL_AGE ENROLL_SCRN_DATE M00_KNOWN_DOBYN_SCORRES M00_BRTHDAT M00_ESTIMATED_AGE age_temp min_age mat_age marry_age if marry_age_flag==1
		gen marry_age_diff = marry_age-mat_age if marry_age > mat_age & marry_age!=. & mat_age!=.
		tab marry_age_diff
		drop marry_age_flag marry_age_diff
		
	*Married under age 18
	gen married_18 = 1 if inrange(marry_age,1,18)
	replace married_18 = 0 if marry_age>=18 & marry_age!=.
	replace married_18 = 77 if (marry_age < 0 | marry_age == .) & ///
		inlist(marry_status,2,5) 
	replace married_18 = 55 if (marry_age < 0 | marry_age == .) & ///
		inlist(marry_status,1,3,4,.) 

*Gestational age in weeks
	gen ga_wks_enroll = BOE_GA_DAYS_ENROLL / 7
		 
*Singleton
	gen num_fetus = M01_FETUS_CT_PERES_US if M01_FETUS_CT_PERES_US > 0
	
	gen singleton = 1 if num_fetus==1
	replace singleton = 0 if num_fetus>1 & num_fetus!=.
	
*Multiple pregnancy 
	gen MULTIPLE = 1 if num_fetus>1 & num_fetus!=.
	replace MULTIPLE = 0 if num_fetus == 1 
	replace MULTIPLE = 55 if num_fetus == . 
	 
*Pregnancy order - Order of pregnancies enrolled in PRISMA MNH by mother
	preserve
	 
	keep PREG_START_DATE CLOSEOUT_DAYS_PP MOMID PREGID CLOSEOUT
	duplicates drop 
	isid MOMID PREGID

	sort MOMID PREG_START_DATE PREGID

	by MOMID: gen ENR_PREG_NUM_ORDER = _n // pregnancy order
	by MOMID: gen ENR_PREG_NUM = _N // number of pregnancies

	keep MOMID PREGID ENR_PREG_NUM_ORDER ENR_PREG_NUM
	save "$temp/pregorder.dta", replace

	restore 

	merge m:1 MOMID PREGID using "$temp/pregorder.dta", nogen
		
* Paid work: coded occupation first, then the free-text "other" occupation
	gen paid_work = .
	replace paid_work = 1 if inrange(M03_JOB_SCORRES,1,8)
	replace paid_work = 0  if paid_work==. & inlist(M03_JOB_SCORRES, 9, 77)	
	
	*Clean free text responses
	gen job_clean = itrim(strtrim(strlower(M03_JOB_OTHR_SPFY_SCORRES)))
		// itrim replaces multiple spaces with single space
		// strtrim removes leading and trailing spaces
		// strlower makes string lowercase
	tab job_clean SITE if  job_clean!="" ///
		& job_clean!="n/a"
	
	replace paid_work = 1 if inlist(job_clean, ///
		"apprentice","apprentices","apprenticeship","aprintish", ///
		"beautician","beauticin","beaution","bread baker","bread seller")
	replace paid_work = 1 if inlist(job_clean, ///
		"buetition","carpenter","casual worker","charcoal burning", ///
		"cleaner","community health worker","community police", ///
		"company worker","daily wages")
	replace paid_work = 1 if inlist(job_clean, ///
		"data collector","do hand embroidery","dress maker", ///
		"embroidery work","farm","farmer","farming", ///
		"fish monger","food counter")
	replace paid_work = 1 if inlist(job_clean, ///
		"food seller","food vendor","gold mining","hair dresser", ///
		"hair dressing","hair dressing and beauty therapy", ///
		"hairdresser","hairdressers","hairdressers apprentices")
	replace paid_work = 1 if inlist(job_clean, ///
		"hairdressing","hand embroidery","headreser","health worker", ///
		"helper","home maed","home maid","home teacher","homemade")
	replace paid_work = 1 if inlist(job_clean, ///
		"hotel owner","house keeper","house maed","house maide", ///
		"houses made","intern","jua kali","koko seler","lab technician")
	replace paid_work = 1 if inlist(job_clean, ///
		"lady health vaccinator","made","maid","mama mboga", ///
		"mentoring","midwife","nurse","online writing","pecking")
	replace paid_work = 1 if inlist(job_clean, ///
		"petrol seller","pety trader","polio worker","preacher", ///
		"private surver","receptionist","receptionist (pia)", ///
		"sales and market","sales person")
	replace paid_work = 1 if inlist(job_clean, ///
		"saleslady","salonist","saloonist","sarvent", ///
		"seamstress","seamtress","selling cereals", ///
		"selling shoes","selling vegetables")
	replace paid_work = 1 if inlist(job_clean, ///
		"sells clothes","service personnel","shopper pealing", ///
		"sign language interpreter","stitching", ///
		"supports mother in business","table banking", ///
		"tader","tailor")
	replace paid_work = 1 if inlist(job_clean, ///
		"tailoring","teacher","teaching","trader", ///
		"traders","trading","un skilled person", ///
		"waitress","welding")
	replace paid_work = 1 if inlist(job_clean,"worker","company worker (helper)")

	replace paid_work = 0 if inlist(job_clean, ///
		"collage student","college student","diploma nursing students", ///
		"gnm student","health volunteer","it student", ///
		"parents","student","students")
	replace paid_work = 0 if inlist(job_clean, ///
		"studing bba","studing ca","studing dmlt", ///
		"study","studying", ///
		"volunteer as medical laboratory technician", ///
		"volunteer at jijenge","volutery teaching")
	replace paid_work = 0 if job_clean=="advocate study"
		
	*check for new responses
	tab job_clean if paid_work==. & M03_JOB_SCORRES==88 & job_clean!="" ///
		& job_clean!="n/a",m
		
		*If new responses - break code - code new responses
		count if paid_work==. & M03_JOB_SCORRES==88 & job_clean!="" ///
		& job_clean!="n/a"
		
		if r(N) > 0 {
			di as error ///
				"`r(N)' new M03_JOB_OTHR_SPFY_SCORRES responses require coding"
			tab job_clean if paid_work==. & M03_JOB_SCORRES==88 & ///
				job_clean!="" & job_clean!="n/a"
			exit 9
		}
	
	*update paid_work
	 replace paid_work = 88 if paid_work==. & M03_JOB_SCORRES==88
	 replace paid_work = 55 if paid_work==.
	 tab paid_work,m
	  
* Substance use
	gen smoke = M03_SMOKE_OECOCCUR if inlist(M03_SMOKE_OECOCCUR, 0, 1)
	gen chew_tobacco = M03_CHEW_OECOCCUR if inlist(M03_CHEW_OECOCCUR, 0, 1)
	gen chew_betelnut = M03_CHEW_BNUT_OECOCCUR if ///
		inlist(M03_CHEW_BNUT_OECOCCUR, 0, 1)
	gen drink = M03_DRINK_OECOCCUR if inlist(M03_DRINK_OECOCCUR, 0, 1)

* Participant received folic acid or iron-folic acid supplementation
	gen folic_suppl_enroll = 1 if M04_FOLIC_ACID_CMOCCUR == 1 | ///
		M04_IFA_CMOCCUR == 1
	replace folic_suppl_enroll = 0 if M04_FOLIC_ACID_CMOCCUR == 0 & ///
		M04_IFA_CMOCCUR == 0

*Participant slept under an insecticide-treated net the previous night
	gen sleep_under_net = M04_INSECT_LSTNIGHT_OBSOCCUR if ///
		inlist(M04_INSECT_LSTNIGHT_OBSOCCUR, 0, 1)

*Planned delivery location is a health facility
	recode M03_PD_BIRTH_OHOLOC (1 = 1) (2 = 0) (88 = 88) (else = .), ///
		generate(birth_facility)
		
*Person responsible for deciding the planned birth location
	gen birth_loc_decision_maker = M03_PD_DM_SCORRES ///
		if inrange(M03_PD_DM_SCORRES, 1, 5) | M03_PD_DM_SCORRES == 88
	la define birth_loc_decision_maker 1 "Participant" 2 "Husband/partner" ///
		3 "Mother-in-law" 4 "Father-in-law" ///
		5 "Participant and husband/partner jointly"
	la values birth_loc_decision_maker birth_loc_decision_maker

**################ OBSTETRIC HISTORY ################
*Cleaning
	replace M04_PH_PREV_RPORRES = . if M04_PH_PREV_RPORRES > 20
	replace M04_PH_LIVE_RPORRES = . if inlist(M04_PH_LIVE_RPORRES, 55, 77) ///
		| M04_PH_LIVE_RPORRES<0
	replace M04_STILLBIRTH_CT_RPORRES = . if ///
		inlist(M04_STILLBIRTH_CT_RPORRES, 55, 77) | ///
		M04_STILLBIRTH_CT_RPORRES<0

*Total number of pregnancies including current pregnancy
	gen gravidity = M04_PH_PREVN_RPORRES + 1 if M04_PH_PREVN_RPORRES > 0 & ///
		M04_PH_PREVN_RPORRES!=. & num_fetus!=.
	replace gravidity = 1 if gravidity==. & num_fetus!=. ///
		& (M04_PH_PREV_RPORRES == 0 | M04_PH_PREVN_RPORRES == 0)
		
*Participant is in her first pregnancy
	gen primigravida = 1 if gravidity==1
	replace primigravida = 0 if gravidity>1 & gravidity!=.
	
*Parity: live births + stillbirths 
	gen parity = .
	replace parity = 0 if M04_PH_PREV_RPORRES==0 // no prev pregnancies
	replace parity = M04_PH_LIVE_RPORRES + M04_STILLBIRTH_CT_RPORRES ///
		if parity==. & ///
		M04_PH_LIVE_RPORRES!=. & M04_STILLBIRTH_CT_RPORRES!=.
	replace parity = M04_PH_LIVE_RPORRES if parity==. & ///
		M04_PH_LIVE_RPORRES!=. & M04_STILLBIRTH_CT_RPORRES==.
	replace parity = M04_STILLBIRTH_CT_RPORRES if parity==. & ///
		M04_STILLBIRTH_CT_RPORRES!=. & M04_PH_LIVE_RPORRES==.
	replace parity = 0 if parity==. & ///
		M04_PH_OTH_RPORRES>0 & M04_PH_OTH_RPORRES!=.

		
	*Parity - categorical
		gen parity_cat = 0 if parity == 0 
		replace parity_cat = 1 if parity == 1
		replace parity_cat = 2 if parity>=2 & parity!=.
		label define parity_cat 0 "0" 1 "1" 2 "2+"
		la values parity_cat parity_cat
		 
	*Grand parity
		gen gparity = 0 if parity==0
		replace gparity = 1 if inrange(parity,1,4)
		replace gparity = 2 if parity>=5 & parity!=.
		label define gparity 0 "0" 1 "1-4" 2 "5+"
		la values gparity gparity
		
*Participant has had no previous births
	gen nulliparous = 1 if parity==0
	replace nulliparous = 0 if parity>0 & parity!=.
	
*Previous Miscarriage
	gen miscarriage =  M04_MISCARRIAGE_RPORRES if ///
		inlist(M04_MISCARRIAGE_RPORRES,0,1)
	replace miscarriage = 0 if miscarriage==. & ///
		(M04_PH_PREV_RPORRES == 0 | M04_PH_OTH_RPORRES == 0)
	
	gen num_miscarriage = M04_MISCARRIAGE_CT_RPORRES if ///
		M04_MISCARRIAGE_CT_RPORRES >= 0 & M04_MISCARRIAGE_CT_RPORRES!=.
	replace num_miscarriage = 0 if num_miscarriage==. & ///
		(M04_MISCARRIAGE_RPORRES == 0 | M04_PH_OTH_RPORRES == 0)
	replace num_miscarriage = 1 if num_miscarriage==. & ///
		M04_PH_OTH_RPORRES == 1 & M04_MISCARRIAGE_RPORRES == 1
	replace num_miscarriage = 0 if num_miscarriage==. & ///
		M04_PH_OTH_RPORRES == 1 & ///
		(M04_STILLBIRTH_CT_RPORRES == 1 | M04_STILLBIRTH_RPORRES == 1)
	replace num_miscarriage = 0 if num_miscarriage==. & ///
		M04_PH_OTH_RPORRES > 1 & M04_PH_OTH_RPORRES!=. & ///
		M04_PH_OTH_RPORRES == M04_STILLBIRTH_CT_RPORRES
	replace num_miscarriage = . if num_miscarriage==. & ///
    M04_PH_PREV_RPORRES == 0 
	
*Previous pregnancy complications 
		*PH_PREV_RPORRES - ever pregnant
	foreach let in PPH APH PRETERM POSTTERM GEST_HTN PREECLAMPSIA ///
				   GEST_DIAB PREMATURE_RUPTURE OBSTR_LABOR ///
				   INTERUTER_GROWTH LOWBIRTHWT MACROSOMIA ///
				   OLIGOHYDRAMNIOS {
		gen MAT_PREVPREG_`let' = 77 if M04_PH_PREV_RPORRES == 0 
		replace MAT_PREVPREG_`let' = 0 if M04_`let'_RPORRES == 0 & ///
			M04_PH_PREV_RPORRES==1 
		replace MAT_PREVPREG_`let' = 1 if M04_`let'_RPORRES == 1 & ///
			M04_PH_PREV_RPORRES==1
		replace MAT_PREVPREG_`let' = 55 if ///
			inlist(M04_`let'_RPORRES,55,77,99) & M04_PH_PREV_RPORRES ==1
		}
		 
	rename MAT_PREVPREG_GEST_HTN MAT_PREVPREG_GHTN 
	rename MAT_PREVPREG_PREECLAMPSIA MAT_PREVPREG_PREEC 
	rename MAT_PREVPREG_GEST_DIAB MAT_PREVPREG_GDM
	rename MAT_PREVPREG_PREMATURE_RUPTURE MAT_PREVPREG_PROM
	rename MAT_PREVPREG_OBSTR_LABOR MAT_PREVPREG_PROLABOR
	rename MAT_PREVPREG_INTERUTER_GROWTH MAT_PREVPREG_IUGR
	rename MAT_PREVPREG_LOWBIRTHWT MAT_PREVPREG_LBW
	rename MAT_PREVPREG_MACROSOMIA MAT_PREVPREG_MACRO
	rename MAT_PREVPREG_OLIGOHYDRAMNIOS MAT_PREVPREG_OLIGO

*fetal anomalies: 	
	gen MAT_PREVPREG_ANOMALY = 77 if M04_PH_PREV_RPORRES == 0 
	replace MAT_PREVPREG_ANOMALY = 0 if M04_MALFORM_MHOCCUR==0 & ///
		M04_PH_PREV_RPORRES==1
	replace MAT_PREVPREG_ANOMALY = 1 if M04_MALFORM_MHOCCUR==1 & ///
		M04_PH_PREV_RPORRES==1
	replace MAT_PREVPREG_ANOMALY = 55 if M04_PH_PREV_RPORRES == 1 & ///
		inlist(M04_MALFORM_MHOCCUR,55,77,99)
		
*history of unplanned c-section: 	
	gen MAT_PREVPREG_CES_UNPLAN = 77 if M04_PH_PREV_RPORRES == 0 
	replace MAT_PREVPREG_CES_UNPLAN = 0 if M04_UNPL_CESARIAN_PROCCUR == 0 ///
		& M04_PH_PREV_RPORRES==1
	replace MAT_PREVPREG_CES_UNPLAN  = 1 if M04_UNPL_CESARIAN_PROCCUR == 1 ///
		& M04_PH_PREV_RPORRES==1 
	replace MAT_PREVPREG_CES_UNPLAN = 55 if M04_PH_PREV_RPORRES == 1 & ///
		inlist(M04_UNPL_CESARIAN_PROCCUR,55,77,99,.)

*history of planned c-section: 
	gen MAT_PREVPREG_CES_PLAN = 77 if M04_PH_PREV_RPORRES == 0 
	replace MAT_PREVPREG_CES_PLAN = 0 if M04_PL_CESARIAN_PROCCUR == 0 & ///
		M04_PH_PREV_RPORRES==1
	replace MAT_PREVPREG_CES_PLAN = 1 if M04_PL_CESARIAN_PROCCUR == 1 & ///
		M04_PH_PREV_RPORRES==1
	replace MAT_PREVPREG_CES_PLAN  = 55 if M04_PH_PREV_RPORRES == 1 & ///
		inlist(M04_PL_CESARIAN_PROCCUR,55,77,99,.)

*history of ANY c-section: 	
	tab M04_CESARIAN_RPORRES, m 
	gen MAT_PREVPREG_CES_ANY = 77 if M04_PH_PREV_RPORRES == 0 
	replace MAT_PREVPREG_CES_ANY = 0 if M04_CESARIAN_RPORRES == 0 & ///
		M04_PH_PREV_RPORRES==1
	replace MAT_PREVPREG_CES_ANY = 1 if M04_CESARIAN_RPORRES == 1 & ///
		M04_PH_PREV_RPORRES==1
	replace MAT_PREVPREG_CES_ANY = 55 if M04_PH_PREV_RPORRES == 1 & ///
		inlist(M04_CESARIAN_RPORRES,55,77,99,.)
	replace MAT_PREVPREG_CES_ANY = 1 if MAT_PREVPREG_CES_UNPLAN == 1 & ///
		inlist(MAT_PREVPREG_CES_ANY,0,55)
	replace MAT_PREVPREG_CES_ANY = 1 if MAT_PREVPREG_CES_PLAN == 1 & ///
		inlist(MAT_PREVPREG_CES_ANY,0,55)		
	tab MAT_PREVPREG_CES_ANY,m

*Update c-section variables 	
	* if ANY ces = 0, replace unplanned = 0 if currently missing:
	replace MAT_PREVPREG_CES_UNPLAN = 0 if MAT_PREVPREG_CES_ANY == 0 & ///
		MAT_PREVPREG_CES_UNPLAN == 55 
		
	* if ANY ces = 1 and unplanned = 0 and planned = 0, set to missing for 
	* unplanned (since we don't know if it's planned or unplanned)
	replace MAT_PREVPREG_CES_UNPLAN = 55 if ///
		MAT_PREVPREG_CES_UNPLAN == 0 & ///
		MAT_PREVPREG_CES_PLAN == 0 & MAT_PREVPREG_CES_ANY == 1 

  
**################ VARIABLE LABELING & FORMATTING ################

gl vars SITE SCRNID MOMID PREGID ENROLL_SCRN_DATE                           ///
    hh_head_female hh_size toilet_improved toilet_shared water_improved     ///
    phone_access hh_smoke mat_age mat_age_under18 bmi_enroll                ///
    bmi_level_enroll ga_wks_enroll school_yrs married marry_status          ///
    marry_age height_group singleton educated gravidity primigravida        ///
    parity nulliparous paid_work smoke chew_tobacco chew_betelnut drink     ///
    folic_suppl_enroll sleep_under_net birth_facility                       ///
    birth_loc_decision_maker num_fetus miscarriage num_miscarriage          ///
    muac_enroll WEALTH_QUINT parity_cat gparity married_18 AGE_GROUP		///
	SCHOOL_MORE10 SCHOOL_MORE12 MULTIPLE MAT_PREVPREG_PPH MAT_PREVPREG_APH  ///
	MAT_PREVPREG_PRETERM MAT_PREVPREG_POSTTERM MAT_PREVPREG_GHTN 			///
	MAT_PREVPREG_PREEC MAT_PREVPREG_GDM MAT_PREVPREG_PROM 					///
	MAT_PREVPREG_PROLABOR MAT_PREVPREG_IUGR MAT_PREVPREG_LBW 				///
	MAT_PREVPREG_MACRO MAT_PREVPREG_OLIGO MAT_PREVPREG_ANOMALY 				///
	MAT_PREVPREG_CES_UNPLAN MAT_PREVPREG_CES_PLAN MAT_PREVPREG_CES_ANY 		///
	ENR_PREG_NUM_ORDER ENR_PREG_NUM

	
keep $vars ASSET_INDEX_*

rename $vars, upper

la var SITE                     "Study site"
la var SCRNID                   "Screening ID"
la var MOMID                    "Mom ID"
la var PREGID                   "Pregnancy ID"
la var ENROLL_SCRN_DATE 	 	"Date of enrollment screening"
la var HH_HEAD_FEMALE           "Female-headed household"
la var HH_SIZE                  "Number of people living in household"
la var TOILET_IMPROVED          "Household has improved toilet facility"
la var TOILET_SHARED            "Household toilet facility is shared"
la var WATER_IMPROVED           "Household has improved water source"
la var PHONE_ACCESS             "Mobile phone access"
la var HH_SMOKE                 "Someone in household smokes"
la var MAT_AGE                  "Maternal age at enrollment"
la var MAT_AGE_UNDER18          "Maternal age under 18 years"
la var BMI_ENROLL               "Body mass index at enrollment"
la var BMI_LEVEL_ENROLL         "BMI category at enrollment"
la var GA_WKS_ENROLL            "Gestational age in weeks at enrollment"
la var SCHOOL_YRS               "Completed years of schooling"
la var MARRIED                  "Married or cohabiting"
la var MARRY_STATUS             "Marital status"
la var MARRY_AGE                "Age at marriage"
la var HEIGHT_GROUP             "Maternal height category"
la var SINGLETON                "Singleton pregnancy"
la var EDUCATED                 "Attended school"
la var GRAVIDITY                "Total number of pregnancies including current pregnancy"
la var PRIMIGRAVIDA             "First pregnancy"
la var PARITY                   "Number of previous births"
la var NULLIPAROUS              "No previous births"
la var PAID_WORK                "Performs paid work"
la var SMOKE                    "Currently smokes"
la var CHEW_TOBACCO             "Currently chews tobacco"
la var CHEW_BETELNUT            "Currently chews betel nut"
la var DRINK                    "Currently drinks alcohol"
la var FOLIC_SUPPL_ENROLL       "Folic acid or iron-folic acid supplementation at enrollment"
la var SLEEP_UNDER_NET          "Slept under insecticide-treated net previous night"
la var BIRTH_FACILITY           "Planned delivery location is a health facility"
la var BIRTH_LOC_DECISION_MAKER "Person responsible for deciding planned birth location"
la var NUM_FETUS                "Number of fetuses at enrollment ultrasound"
la var MISCARRIAGE              "History of miscarriage"
la var NUM_MISCARRIAGE          "Number of previous miscarriages"
la var MUAC_ENROLL              "Mid-upper arm circumference at enrollment"

*NEW VARIABLES FROM MAT_RISKS 10/9/26
la var PARITY_CAT 				"Parity (live+stillbirths; 0/1/2+)"
la var GPARITY 					"Grand parity (live+stillbirths; 0/1-4/5+)"	
la var MARRIED_18				"Married under age 18"	
la var AGE_GROUP 				"Age Group (1=<20, 2=20-34, 3=35+)"
la var SCHOOL_MORE10 			"Attended more than 10 years of school"
la var SCHOOL_MORE12 			"Attended more than 12 years of school"
la var MULTIPLE 				"Multiple pregnancy (1=twin/trip, 0=single)"
 
*NEW VARIABLES FROM MAT_PREVPREG_COMPLICATIONS 10/9/26
la var MAT_PREVPREG_APH 		"Reported APH in previous pregnancy"
la var MAT_PREVPREG_PPH 		"Reported PPH in previous pregnancy"
la var MAT_PREVPREG_PRETERM 	"Preterm delivery in previous pregnancy"
la var MAT_PREVPREG_POSTTERM 	"Postterm delivery in previous pregnancy"
la var MAT_PREVPREG_GHTN 		"GHTN in previous pregnancy"
la var MAT_PREVPREG_PREEC 		"Preeclampsia/eclampsia in previous pregnancy"
la var MAT_PREVPREG_GDM 		"Gestational diabetes in previous pregnancy"
la var MAT_PREVPREG_PROM 		"PROM in previous pregnancy"
la var MAT_PREVPREG_PROLABOR 	"Prolonged or obstructed labor in previous pregnancy"
la var MAT_PREVPREG_IUGR 		"Intrauterine growth restriction in previous pregnancy"
la var MAT_PREVPREG_LBW 		"Low birthweight baby in previous pregnancy"
la var MAT_PREVPREG_MACRO 		"Macrosomia in previous pregnancy"
la var MAT_PREVPREG_OLIGO 		"Oligohydramnios in previous pregnancy"
la var MAT_PREVPREG_ANOMALY 	"Baby with fetal anomaly in previous pregnancy"
la var MAT_PREVPREG_CES_UNPLAN 	"History of unplanned c-section(s)"	
la var MAT_PREVPREG_CES_ANY 	"History of ANY c-section(s)"
la var MAT_PREVPREG_CES_PLAN 	"History of planned c-section"
 
*NEW VARIABLES NOT PREVIOUSLY CONSTRUCTED 10/14/26
la var ENR_PREG_NUM "Number of times (pregnancies) mother has been enrolled in PRISMA MNH"
la var ENR_PREG_NUM_ORDER "Order of pregnancies enrolled in PRISMA MNH by mother"
 
**################ VARIABLE FORMATTING ################

*Dummy variables
	foreach x in AGE_GROUP MARRIED_18 EDUCATED SCHOOL_MORE10 SCHOOL_MORE12 ///
		GPARITY PARITY_CAT WEALTH_QUINT {
			recode `x' (.=55), gen(`x'_t)
			qui tab `x'_t, gen(`x'_)
			drop `x'_t
		}
	rename AGE_GROUP_4 AGE_GROUP_55
	rename WEALTH_QUINT_6 WEALTH_QUINT_55
 
	foreach x in MARRIED_18 EDUCATED SCHOOL_MORE10 SCHOOL_MORE12 {
		rename `x'_1 `x'_0
		rename `x'_2 `x'_1
		rename `x'_3 `x'_55
	}
	rename MARRIED_18_4 MARRIED_18_77

	foreach x in GPARITY PARITY_CAT {
		
		rename `x'_1 `x'_0
		rename `x'_2 `x'_1
		rename `x'_3 `x'_2
		rename `x'_4 `x'_55
	} 

	label var AGE_GROUP_1 "Mother age <20 at enrollment"
	label var AGE_GROUP_2 "Mother age 20-34 at enrollment"
	label var AGE_GROUP_3 "Mother age 35+ at enrollment"
	label var AGE_GROUP_55 "Mother age unknown at enrollment" 
 
	label var EDUCATED_0 "Mother never attended school"
	label var EDUCATED_1 "Mother attended any school"
	label var EDUCATED_55 "School status missing"
	
	label var SCHOOL_MORE10_0 "Mother attended 10 or fewer years of school"
	label var SCHOOL_MORE10_1 "Mother attended more than 10 years of school"
	label var SCHOOL_MORE10_55 "School status/length of enrollment missing"
	
	label var SCHOOL_MORE12_0 "Mother attended 12 or fewer years of school"
	label var SCHOOL_MORE12_1 "Mother attended more than 12 years of school"
	label var SCHOOL_MORE12_55 "School status/length of enrollment missing"

	label var PARITY_CAT_0 "Nulliparous"
	label var PARITY_CAT_1 "Parity 1"
	label var PARITY_CAT_2 "Parity 2+"
	label var PARITY_CAT_55 "Parity Unknown"

	label var GPARITY_0 "Nulliparous"
	label var GPARITY_1 "Multi-parity (1-4)"
	label var GPARITY_2 "Grand-parity (5+)"
	label var GPARITY_55 "Parity unknown"
	
	label var MARRIED_18_0 "Married at age 18 or older"
	label var MARRIED_18_1 "Married under age 18"
	label var MARRIED_18_55 "Unknown/missing"
	label var MARRIED_18_77 "Unmarried"
	
	forvalues i=1/5 {
		la var WEALTH_QUINT_`i' "Quintile `i'"
	}
	la var WEALTH_QUINT_55 "Missing wealth quintile"
	
foreach x in AGE_GROUP MARRIED_18 EDUCATED SCHOOL_MORE10 SCHOOL_MORE12 GPARITY ///
	PARITY_CAT WEALTH_QUINT {
		order `x'_*, after(`x')
	}

 
*------------------------------------------------------------------------------
* QC
*------------------------------------------------------------------------------
 
sum ENROLL_SCRN_DATE, format

gl cont HH_SIZE MAT_AGE BMI_ENROLL MUAC_ENROLL SCHOOL_YRS MARRY_AGE GA_WKS_ENROLL NUM_FETUS GRAVIDITY PARITY NUM_MISCARRIAGE ENR_PREG_NUM ENR_PREG_NUM_ORDER

bysort SITE: sum $cont

gl cat WEALTH_QUINT HH_HEAD_FEMALE TOILET_IMPROVED TOILET_SHARED WATER_IMPROVED PHONE_ACCESS HH_SMOKE MAT_AGE_UNDER18 AGE_GROUP BMI_LEVEL_ENROLL HEIGHT_GROUP EDUCATED SCHOOL_MORE10 SCHOOL_MORE12 MARRIED MARRY_STATUS MARRIED_18 SINGLETON MULTIPLE PAID_WORK SMOKE CHEW_TOBACCO CHEW_BETELNUT DRINK FOLIC_SUPPL_ENROLL SLEEP_UNDER_NET BIRTH_FACILITY BIRTH_LOC_DECISION_MAKER PRIMIGRAVIDA PARITY_CAT GPARITY NULLIPAROUS MISCARRIAGE

foreach x in $cat {
	replace `x' = 55 if `x'==.
	tab SITE `x',m
}

foreach x of varlist MAT_PREVPREG_* {
	tab SITE `x',m
}


	 
*------------------------------------------------------------------------------
* Save
*------------------------------------------------------------------------------
label data "Data date: $datadate; `c(username)' modified `c(current_date)'"
local datalabel: data label

compress
save "$wrk/MAT_BASELINE.dta", replace
save "$out/MAT_BASELINE.dta", replace
export delimited using "$out/MAT_BASELINE.csv", replace
export excel using "$out/MAT_BASELINE.xlsx", firstrow(variables) replace
