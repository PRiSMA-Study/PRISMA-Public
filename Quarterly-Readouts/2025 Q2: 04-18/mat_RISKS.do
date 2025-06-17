*PRISMA Maternal Variable Construction Code
*Purpose: This code drafts variable construction code for maternal demographics
	*variables for the PRISMA study - Demographics for RFA
*Original Version: April 15, 2024 by E Oakley (emoakley@gwu.edu)
*Update: July 9, 2024 by E Oakley (remove variables duplicated with mat outcomes report)
*Update: July 12, 2024 by E Oakley (save a clean copy of de-duplicated vars to shared folder)
*Update: January 17, 2024 by E Oakley (update for January 10 data and naming conventions; remove BMI variables, folic acid variables)

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

global OUT "Z:/Outcome Data/2025-04-18"

	// Working Files Folder (TNT-Drive)
global wrk "Z:/Erin_working_files/data" // set pathway here for where you want to save output data files (i.e., constructed analysis variables)

global date "250428" // today's date

log using "$log/mat_outcome_construct_DEMO_$date", replace

/*************************************************************************
	*Variables constructed in this do file:
	
	Maternal Demographic Characteristics:
	
	Wealth quintile (construct from asset index - MNH03)
		Quintile 1
		Quintile 2
		Quintile 3
		Quintile 4
		Quintile 5

	Parity (construct from: PH_LIVE_RPORRES & (maybe) STILLBIRTH_RPORRES) - MNH04)
		'0
		'1
		'2+
		
	Additions: 
		GPARITY -- Parity where categories are 0; 1-4; 5+
		MARRIED_18 -- 1=married under age 18
		
*/

	*Variables NOT already created in Xiaoyan's dataset - Wealth quintile: 
	
	
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////

*MNH03 
	
	*Wealth quintile (construct from asset index - MNH03)
	
	*Pull in demographic variables from MNH03:
	import delimited "$da/mnh03_merged", bindquote(strict)
	
	rename momid MOMID 
	rename pregid PREGID 
	
	drop if MOMID == "" | PREGID == ""

	*Wealth Quintile 
		//Prep asset index: 
		
	foreach l in radio tv fridge computer watch bike motorcycle car boat ///
		cart plough foam_matt straw_matt spring_matt sofa lantern sew ///
		wash blender mosquito_net tricycles tables cabinets sat_dish dvd_cd ///
		aircon tractor {
	gen `l'_ind = m03_`l'_fcorres if m03_`l'_fcorres == 0 | m03_`l'_fcorres == 1
	
	tab `l'_ind site, m 
	
		}
		
	*NOTE: asset index is different for all sites, so we need site-specific 
	*indicators for asset is included: 
	
	gen site_loop = site 
	replace site_loop = "IndiaCMC" if site == "India-CMC"
	replace site_loop = "IndiaSAS" if site == "India-SAS"
	
	foreach s in Ghana IndiaCMC IndiaSAS Kenya Pakistan Zambia {
		
	gen ind_num_`s' = 0
		
	foreach l in radio tv fridge computer watch bike motorcycle car boat ///
		cart plough foam_matt straw_matt spring_matt sofa lantern sew ///
		wash blender mosquito_net tricycles tables cabinets sat_dish dvd_cd ///
		aircon tractor {
	
	sum `l'_ind if site_loop == "`s'"
	gen `l'_ind_`s' = r(max) if site_loop == "`s'" 
	
	replace ind_num_`s' = ind_num_`s' + 1 if `l'_ind_`s' != . 
	
	}		
	}
	
	tab ind_num_Ghana, m // all 27
	tab ind_num_IndiaCMC, m // 6: fridge computer motorcyle car wash aircon
		tabstat *_ind_IndiaCMC 
	tab ind_num_IndiaSAS, m // all 27 
	tab ind_num_Kenya, m // all 27 
	tab ind_num_Pakistan, m // 12: radio tv fridge computer watch bike motorcycle car boat cart wash aircon
		tabstat *_ind_Pakistan
	tab ind_num_Zambia, m // 12: radio tv fridge computer watch bike motorcycle car sofa table cabinets dvd_cd
		tabstat *_ind_Zambia	
	
	// Ghana: 
	pca radio_ind tv_ind fridge_ind computer_ind watch_ind bike_ind ///
		motorcycle_ind car_ind boat_ind cart_ind plough_ind foam_matt_ind ///
		straw_matt_ind spring_matt_ind sofa_ind lantern_ind sew_ind ///
		wash_ind blender_ind mosquito_net_ind tricycles_ind tables_ind ///
		cabinets_ind sat_dish_ind dvd_cd_ind aircon_ind tractor_ind if ///
		site == "Ghana"
	predict ASSET_INDEX_Ghana if site == "Ghana", score
	xtile WEALTH_QUINT_Ghana = ASSET_INDEX_Ghana if site == "Ghana", nq(5)
	label var ASSET_INDEX_Ghana "PCA Asset Score-Ghana"
	label var WEALTH_QUINT_Ghana "Asset Quintile-Ghana"
	sum ASSET_INDEX_Ghana  
	tab WEALTH_QUINT_Ghana site, m 
	
	// India-SAS: 
	pca radio_ind tv_ind fridge_ind computer_ind watch_ind bike_ind ///
		motorcycle_ind car_ind boat_ind cart_ind plough_ind foam_matt_ind ///
		straw_matt_ind spring_matt_ind sofa_ind lantern_ind sew_ind ///
		wash_ind blender_ind mosquito_net_ind tricycles_ind tables_ind ///
		cabinets_ind sat_dish_ind dvd_cd_ind aircon_ind tractor_ind if ///
		site == "India-SAS"
	predict ASSET_INDEX_IndiaSAS if site == "India-SAS", score
	xtile WEALTH_QUINT_IndiaSAS = ASSET_INDEX_IndiaSAS if site == "India-SAS", nq(5)
	label var ASSET_INDEX_IndiaSAS "PCA Asset Score-India-SAS"
	label var WEALTH_QUINT_IndiaSAS "Asset Quintile-India-SAS"
	sum ASSET_INDEX_IndiaSAS  
	tab WEALTH_QUINT_IndiaSAS site, m 
	
	// Kenya: 
	pca radio_ind tv_ind fridge_ind computer_ind watch_ind bike_ind ///
		motorcycle_ind car_ind boat_ind cart_ind plough_ind foam_matt_ind ///
		straw_matt_ind spring_matt_ind sofa_ind lantern_ind sew_ind ///
		wash_ind blender_ind mosquito_net_ind tricycles_ind tables_ind ///
		cabinets_ind sat_dish_ind dvd_cd_ind aircon_ind tractor_ind if ///
		site == "Kenya"
	predict ASSET_INDEX_Kenya if site == "Kenya", score
	xtile WEALTH_QUINT_Kenya = ASSET_INDEX_Kenya if site == "Kenya", nq(5)
	label var ASSET_INDEX_Kenya "PCA Asset Score-Kenya"
	label var WEALTH_QUINT_Kenya "Asset Quintile-Kenya"
	sum ASSET_INDEX_Kenya  
	tab WEALTH_QUINT_Kenya site, m 
	
	// Pakistan: 
	pca radio_ind tv_ind fridge_ind computer_ind watch_ind bike_ind ///
		motorcycle_ind car_ind boat_ind cart_ind wash_ind aircon_ind if ///
		site == "Pakistan"
	predict ASSET_INDEX_Pakistan if site == "Pakistan", score
	xtile WEALTH_QUINT_Pakistan = ASSET_INDEX_Pakistan if site == "Pakistan", nq(5)
	label var ASSET_INDEX_Pakistan "PCA Asset Score-Pakistan"
	label var WEALTH_QUINT_Pakistan "Asset Quintile-Pakistan"
	sum ASSET_INDEX_Pakistan  
	tab WEALTH_QUINT_Pakistan site, m 
	
	// Zambia: 
	pca radio_ind tv_ind fridge_ind computer_ind watch_ind bike_ind ///
		motorcycle_ind car_ind sofa_ind tables_ind cabinets_ind dvd_cd_ind if ///
		site == "Zambia"
	predict ASSET_INDEX_Zambia if site == "Zambia", score
	xtile WEALTH_QUINT_Zambia = ASSET_INDEX_Zambia if site == "Zambia", nq(5)
	label var ASSET_INDEX_Zambia "PCA Asset Score-Zambia"
	label var WEALTH_QUINT_Zambia "Asset Quintile-Zambia"
	sum ASSET_INDEX_Zambia  
	tab WEALTH_QUINT_Zambia site, m 
	
	//////////////////////////////////////////////////////////////
	* India-CMC Update - from Savannah's code; added to main code on 4-28-25:
	
		
	*water source: m03_h2o_fcorres //added March 2025
	foreach num of numlist 1 2 3 4 5 6 7 8 9 10 11 12 13 14 88 {
		gen watersrc_`num' = 0 if m03_h2o_fcorres<=88
		replace watersrc_`num' = 1 if m03_h2o_fcorres == `num'
	}


	*toilet: m03_toilet_fcorres //added March 2025
	foreach num of numlist 1 2 3 4 5 6 7 8 9 10 11 12 88 {
		gen toilet_`num' = 0 if m03_toilet_fcorres<=88
		replace toilet_`num' = 1 if m03_toilet_fcorres == `num'
	}

	*Walls: M03_EXT_WALL_FCORRES //added March 2025
	foreach num of numlist 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 88 {
		gen wall_`num' = 0 if m03_ext_wall_fcorres<=88
		replace wall_`num' = 1 if m03_ext_wall_fcorres == `num'
	}
	*floor:M03_FLOOR_FCORRES //added March 2025
	foreach num of numlist 1 2 3 4 5 6 7 8 9 10 11 88 {
		gen floor_`num' = 0 if m03_floor_fcorres<=88
		replace floor_`num' = 1 if m03_floor_fcorres == `num'
	}

	*roof:M03_ROOF_FCORRES //added March 2025
	foreach num of numlist 1 2 3 4 5 6 7 8 9 10 11 12 13 14 88 {
		gen roof_`num' = 0 if m03_roof_fcorres<=88
		replace roof_`num' = 1 if m03_roof_fcorres == `num'
	}
		
		// India-CMC:  //updated March 2025
		pca fridge_ind computer_ind motorcycle_ind car_ind wash_ind aircon_ind 	toilet_1 toilet_2 toilet_3 toilet_4 toilet_5 toilet_6 toilet_7 toilet_8 toilet_9 toilet_10 toilet_11 toilet_12 toilet_88 watersrc_1 watersrc_2 watersrc_3 watersrc_4 watersrc_5 watersrc_6 watersrc_7 watersrc_8 watersrc_9 watersrc_10 watersrc_11 watersrc_12 watersrc_13 watersrc_14 watersrc_88 roof_1 roof_2 roof_3 roof_4 roof_5 roof_6 roof_7 roof_8 roof_9 roof_10 roof_11 roof_12 roof_13 roof_14 wall_1 wall_2 wall_3 wall_4 wall_5 wall_6 wall_7 wall_8 wall_9 wall_10 wall_11 wall_12 wall_13 wall_14 wall_15 wall_16 wall_17 wall_88  floor_1 floor_2 floor_3 floor_4 floor_5 floor_6 floor_7 floor_8 floor_9 floor_10 floor_11 floor_88   if site == "India-CMC"
		predict ASSET_INDEX_IndiaCMC if site == "India-CMC", score
		xtile WEALTH_QUINT_IndiaCMC = ASSET_INDEX_IndiaCMC if site == "India-CMC", nq(5)
		label var ASSET_INDEX_IndiaCMC "PCA Asset Score-India-CMC"
		label var WEALTH_QUINT_IndiaCMC "Asset Quintile-India-CMC"
		sum ASSET_INDEX_IndiaCMC  
		tab WEALTH_QUINT_IndiaCMC if site == "India-CMC", m 
		label define m03_ptr_scorres 1"Salaried" 2"Small business" 3"Business owner" 4"Skilled labor" 5"Unskilled labor"
		label val m03_ptr_scorres m03_ptr_scorres
		tab m03_ptr_scorres WEALTH_QUINT_IndiaCMC if site == "India-CMC", col
	
	
	gen WEALTH_QUINT = . 
	
	foreach s in Ghana IndiaCMC IndiaSAS Kenya Pakistan Zambia {
	
	replace WEALTH_QUINT = WEALTH_QUINT_`s' if site_loop == "`s'"
	replace WEALTH_QUINT = 55 if WEALTH_QUINT_`s' == . & site_loop == "`s'"
	
	}
	
	tab WEALTH_QUINT, m 
	tab WEALTH_QUINT site, m 
	label var WEALTH_QUINT "Asset Quintile (site-specific)"

	
	*Check on India-CMC: 
	list *_ind WEALTH_QUINT if site == "India-CMC"
	*Note: India CMC has only 6 items included in their asset index, which 
	*is why the wealth quintiles look very strange in comparison to other 
	xtile WEALTH_QUART = ASSET_INDEX_IndiaCMC if site == "India-CMC", nq(4)
	tab WEALTH_QUART, m 	
	
	keep MOMID PREGID WEALTH_QUINT ASSET_INDEX_*
	
	save "$wrk/demographics_mnh03", replace 
	clear 	
	
	
	////////////////////////////////////////////
	*Pull ANC data to collect pregnancy history: 
	import delimited "$da/mnh04_merged", bindquote(strict)
	
	*Visit type 
	gen TYPE_VISIT = m04_type_visit
	tab TYPE_VISIT, m 
	
	*** Create visit type label: 
	label define vistype 1 "1-Enrollment" 2 "2-ANC-20" 3 "3-ANC-28" ///
		4 "4-ANC-32" 5 "5-ANC-36" 6 "6-IPC" 7 "7-PNC-0" 8 "8-PNC-1" ///
		9 "9-PNC-4" 10 "10-PNC-6" 11 "11-PNC-26" 12 "12-PNC-52" ///
		13 "13-ANC-Unsched" 14 "14-PNC-Unsched" 
	
	label var TYPE_VISIT "MNH04 Visit Type"
	label values TYPE_VISIT vistype
	tab TYPE_VISIT, m 
	
	*generate visit date: 
	gen VISIT_DATE = date(m04_anc_obsstdat, "YMD") if ///
		m04_anc_obsstdat != "1907-07-07"
	format VISIT_DATE %td 
	sum VISIT_DATE, format
	
	
	/*
	Parity (construct from: PH_LIVE_RPORRES & STILLBIRTH_RPORRES) - MNH04)
		'0
		'1
		'2+
		
	Note: R code is as follows: 
	nulliparous = case_when(
      M04_PH_PREV_RPORRES_1 == 0 | M04_PH_PREVN_RPORRES_1 == 0 | M04_PH_LIVE_RPORRES_1 == 0 ~ 1, 
	 #never pregnant or pregnancy =0 or live birth =0
      M04_PH_PREV_RPORRES_1 == 1 ~ 0, 
      TRUE ~ NA_real_
    ), 
	*/	
	
	*Parity: 
	tab m04_ph_prev_rporres, m // ever pregnant
	tab m04_ph_live_rporres, m // number of livebirths 
	tab m04_ph_oth_rporres, m // ever loss of pregnancy (including stillbirth)
	tab m04_stillbirth_rporres, m // ever stillbirth
	tab m04_stillbirth_ct_rporres, m // count of stillbirth 
	
	gen livebirth = 0 if m04_ph_prev_rporres==0 
	gen stillbirth = 0 if m04_ph_prev_rporres==0
	
	replace stillbirth = 0 if m04_stillbirth_rporres == 0 | m04_ph_oth_rporres == 0
	
	replace livebirth = m04_ph_live_rporres if m04_ph_live_rporres >= 0 & ///
		m04_ph_live_rporres != . 
		
	replace stillbirth = m04_stillbirth_ct_rporres if ///
		m04_stillbirth_rporres == 1 & ///
		m04_stillbirth_ct_rporres >= 0 & m04_stillbirth_ct_rporres != . 
		
	tab livebirth TYPE_VISIT, m 
	tab stillbirth TYPE_VISIT, m 
	
	gen PARITY = 0 if m04_ph_prev_rporres==0 | (livebirth == 0 & stillbirth == 0)
		
	replace PARITY = 1 if (livebirth + stillbirth == 1)
	
	replace PARITY = 2 if (livebirth + stillbirth >= 2 & livebirth != . & ///
		stillbirth != . )
		
	label var PARITY "Parity (livebirths+stillbirths; 0/1/2+)"
		
	
	gen GPARITY = 0 if m04_ph_prev_rporres==0 | (livebirth == 0 & stillbirth == 0)
		
	replace GPARITY = 1 if (livebirth + stillbirth >=1 & ///
		livebirth + stillbirth <= 4)
	
	replace GPARITY = 2 if (livebirth + stillbirth >= 5 & livebirth != . & ///
		stillbirth != . )		
		
	label var GPARITY "Grand parity (livebirths+stillbirths; 0/1-4/5+)"	
	
	tab PARITY GPARITY, m  
	
	*prep the data: 
	gen parity_info = 1 if PARITY != .
	
	keep if parity_info == 1 
	
	*reshape to wide: 
	*create an indicator of total duplicates per person: 
	sort momid pregid VISIT_DATE TYPE_VISIT
	quietly by momid pregid :  gen ENTRY_NUM = cond(_N==1,0,_n)
	tab ENTRY_NUM, m 	
	
	replace ENTRY_NUM = 1 if ENTRY_NUM == 0 
	label var ENTRY_NUM "Total entries per person with parity information"
	
	keep momid pregid TYPE_VISIT VISIT_DATE PARITY GPARITY ENTRY_NUM 
	
	rename * *_
	
	// fix variable formats:
	rename momid_ momid_old
	gen momid = ustrtrim(momid_old)
	
	rename pregid_ pregid_old
	gen pregid = ustrtrim(pregid_old)
	
	drop momid_old pregid_old
	
	// reshape data:
	rename ENTRY_NUM_ ENTRY_NUM 
		
	reshape wide TYPE_VISIT VISIT_DATE PARITY GPARITY, ///
		i(momid pregid) j(ENTRY_NUM)  
		
	*review parity data for those with multiple entries:
	list TYPE_VISIT_1 VISIT_DATE_1 PARITY_1 GPARITY_1 ///
		 TYPE_VISIT_2 VISIT_DATE_2 PARITY_2 GPARITY_2 ///
		 if GPARITY_2 != . 
		 
	*many observations have parity information more than once; we will take the
	*first recorded parity information: 
	
	keep momid pregid TYPE_VISIT_1 VISIT_DATE_1 PARITY_1 GPARITY_1 
	
	rename *_1 *
	
	label var TYPE_VISIT "MNH04 Visit Type"
	label var VISIT_DATE "Date of visit"
	label var PARITY "Parity (livebirths+stillbirths; 0/1/2+)"
	label var GPARITY "Grand parity (livebirths+stillbirths; 0/1-4/5+)"	
	
	rename momid MOMID 
	rename pregid PREGID 
	
	keep MOMID PREGID PARITY GPARITY 
	
	save "$wrk/parity_mnh04", replace 
	clear 	
	
	
///////////////////////////////////////////////////////////////////////////////
	
	*Pull from Xiaoyan's file: 
	
	*NOTE: For the January 10 upload, the MAT_DEMOGRAPHIC csv file was 
	*corrupted and could not be read in. Instead, I edited the code below 
	*to pull from the Excel file version (now available). Irrelevant parts 
	*are commented out (in case they are ever needed again). 
	
	import excel "$OUT/MAT_DEMOGRAPHIC", first case(lower)
	
	drop ga_wks_enroll enroll_scrn_date
	
	rename momid MOMID 
	rename pregid PREGID 
	
	*Also on the Jan 10 upload, there are duplicates: 
	drop if MOMID == "" // not resolving the issue
	
	*Review of duplicates: 
	duplicates tag MOMID PREGID, gen(dup)
	
	tab dup, m 
	
	list if dup >0
	
	* Only 1 duplicate entry: check if duplicate is exact or different:
	duplicates tag *, gen(exact)
	
	tab exact, m 
	
	* exact match on all variables; we will drop 1 entry: 
	sort PREGID
	
	by PREGID: gen number = _n 
	
	gsort -exact 
	
	drop if number == 2 & exact == 1 
	
	drop dup exact number 
	
	duplicates tag MOMID PREGID, gen(dup)
	
	tab dup 
	
	drop dup
	
	/*
	foreach var of varlist * {
		
	replace `var' = "." if `var' == "NA"
	
	destring `var', replace 
	
	*tab `var', m 
		
	}
	*/
	
	////////////////////////////
	* merge in other variables:
	
	merge 1:1 MOMID PREGID using "$wrk/demographics_mnh03"
	
	drop _merge 
	
	merge 1:1 MOMID PREGID using "$wrk/parity_mnh04"
	
		*review parity:
		tab nulliparous PARITY, m 
		tab nulliparous GPARITY, m 
		 
		
		*some differences; to discuss. 
		
	drop _merge 
	
	merge 1:1 MOMID PREGID using "$wrk/SMOKE"
	
	drop _merge 
	
	
	////////////////////////////
	* merge in enrollment variables:
	merge 1:1 MOMID PREGID using "$OUT/MAT_ENROLL"
	
	*restrict to enrolled only: 
	keep if ENROLL==1
	
	
		*Fix constructed variables for enrolled indicator:
		foreach var of varlist WEALTH_QUINT PARITY GPARITY SMOKE {
			
		replace `var' = 55 if `var' == . 
		
		}
	
	
	* Prep variables for RFA: 
	*Marital status: 
	tab marry_status, m  
	
	/*Categories requested:
	Married (1)
    Cohabitating (2)
    Divored/Permanently separated/Widowed (3)
    Single/never married (4)
	*/
	
	gen MARSTATUS = marry_status 
	replace MARSTATUS = 3 if marry_status == 3 | marry_status == 4 
	replace MARSTATUS = 4 if marry_status == 5 
	replace MARSTATUS = 55 if marry_status == . 
	
	label var MARSTATUS "Marital status (1=married;2=cohabit;3=div/sep/wid;4=single)"
	
	tab MARSTATUS, m 
	
	
	*Married under age 18: 
	tab marry_age, m 
	
	gen MARRIED_18 = 1 if marry_age < 18 & marry_age > 0 
	replace MARRIED_18 = 0 if marry_age >=18 & marry_age !=.
	replace MARRIED_18 = 77 if (marry_age < 0 | marry_age == .) & ///
		(MARSTATUS == 2 | MARSTATUS == 4)
	replace MARRIED_18 = 55 if (marry_age < 0 | marry_age == .) & ///
		(MARSTATUS == 1 | MARSTATUS == 3 | MARSTATUS == 55)

		
	tab marry_age MARRIED_18, m
	tab MARSTATUS MARRIED_18, m 
	
	label var MARRIED_18 "Married under age 18"	
	
	
	*Mobile phone access (construct from: MOBILE_ACCESS_FCORRES - MNH03)
	tab phone_access, m 
	
	/*Categories requested: 
	Personally own
    Share with others
    No access
	*/
	
	gen PHONE_ACCESS = phone_access
	replace PHONE_ACCESS = 55 if phone_access == . 
		
	label var PHONE_ACCESS "Mobile phone (1=own;2=shared;3=none)"

	tab PHONE_ACCESS, m 
	
	
	*Marternal age categories: 
	gen AGE_GROUP = 1 if mat_age <20 & mat_age > 0 
	replace AGE_GROUP = 2 if mat_age >= 20 & mat_age <= 34 
	replace AGE_GROUP = 3 if mat_age >=35 & mat_age != . 
	replace AGE_GROUP = 55 if mat_age == . | mat_age <=0
	
	label var AGE_GROUP "Age Group (1=<20, 2=20-34, 3=35+)"
	tab mat_age AGE_GROUP, m 
	
	rename mat_age MAT_AGE
	
	
	*Schooling: 
	gen SCHOOL_ANY = 0 if school_yrs == 0 
	replace SCHOOL_ANY = 1 if school_yrs >=1 & school_yrs != . 
	replace SCHOOL_ANY = 55 if school_yrs == . 
	
	label var SCHOOL_ANY "Ever attended school"
	
	tab school_yrs SCHOOL_ANY, m 
	
	
	*Schooling More/Less than 10: 
	gen SCHOOL_MORE10 = 0 if school_yrs >=0 & school_yrs <=10
	replace SCHOOL_MORE10 = 1 if school_yrs > 10 & school_yrs != . 
	replace SCHOOL_MORE10 = 55 if school_yrs == . 
	
	label var SCHOOL_MORE10 "Attended more than 10 years of school"
	
	tab school_yrs SCHOOL_MORE10, m 	
	
	
	*Schooling More/Less than 12: 
	gen SCHOOL_MORE12 = 0 if school_yrs >=0 & school_yrs <=12
	replace SCHOOL_MORE12 = 1 if school_yrs > 12 & school_yrs != . 
	replace SCHOOL_MORE12 = 55 if school_yrs == . 
	
	label var SCHOOL_MORE12 "Attended more than 12 years of school"
	
	tab school_yrs SCHOOL_MORE12, m 	
	
	
	*Paid work 
	tab paid_work, m 
	
	replace paid_work = 55 if paid_work == 88 | paid_work == . 
	
	rename paid_work PAID_WORK 
	label var PAID_WORK "Paid work (0=no;1=yes;55=unknown)"
	
	
	*WASH: 
	rename water_improved WATER_IMPROVED 
	
	replace WATER_IMPROVED = 55 if WATER_IMPROVED == . | WATER_IMPROVED == 88 
	
	label var WATER_IMPROVED "Improved water source (0=no;1=yes;55=unknown)"
	
	rename toilet_improved TOILET_IMPROVED 
	
	replace TOILET_IMPROVED = 55 if TOILET_IMPROVED == . | TOILET_IMPROVED == 88 
	
	label var TOILET_IMPROVED "Improved sanitation (0=no;1=yes;55=unknown)"	
	
	rename toilet_shared TOILET_SHARED 
	
	replace TOILET_SHARED = 55 if TOILET_SHARED == . | TOILET_SHARED == 88 
	
	label var TOILET_SHARED "Shared toilet facilities (0=no;1=yes;55=unknown)"	
	
	*chew_tobacco
	
	rename chew_tobacco CHEW_TOBACCO
	
	replace CHEW_TOBACCO = 55 if CHEW_TOBACCO == . 
	
	label var CHEW_TOBACCO "Chewing tobacco (0=no;1=yes;55=unknown)"
	
	*chew_betelnut
	
	rename chew_betelnut CHEW_BETELNUT
	
	replace CHEW_BETELNUT = 55 if CHEW_BETELNUT == . 
	
	label var CHEW_BETELNUT "Chewing betelnut (0=no;1=yes;55=unknown)"	
	
	
	*drink alcohol 
	rename drink DRINK 
	
	replace DRINK = 55 if DRINK == . | DRINK == 88 | DRINK == 66 
	
	label var DRINK "Drank alcohol in the last month (0=no;1=yes;55=unknown)"
	
	
	*primigravida 
	
	rename primigravida PRIMIGRAVIDA 
	
	replace PRIMIGRAVIDA = 55 if PRIMIGRAVIDA == . 
	
	label var PRIMIGRAVIDA "First pregnancy (0=no;1=yes;55=unknown)"
	
	
	*Household members smoke 
	rename hh_smoke HH_SMOKE 
	
	replace HH_SMOKE = 55 if HH_SMOKE == . 
	
	label var HH_SMOKE "Members of household smoke (0=no;1=yes;55=unknown)"

	
	
	*under_net 
	rename sleep_under_net UNDER_NET 
	
	replace UNDER_NET = 55 if UNDER_NET == . 
	
	label var UNDER_NET "Sleeps under a bednet"
	
	
	*Multiple pregnancy 
	
	tab num_fetus, m 
	
	gen MULTIPLE = 1 if num_fetus == 2 | num_fetus == 3 
	
	replace MULTIPLE = 0 if num_fetus == 1 
	
	replace MULTIPLE = 55 if num_fetus == . 
	
	label var MULTIPLE "Multiple pregnancy (1=twin/trip, 0=single)"
	
	
	*Miscarriage 
	tab miscarriage PRIMIGRAVIDA, m 
	replace miscarriage = 0 if PRIMIGRAVIDA == 1 
	
	rename miscarriage MISCARRIAGE 
	
	replace MISCARRIAGE = 55 if MISCARRIAGE == . 
	
	label var MISCARRIAGE "History of miscarriage"
	
	

///////////////////////////////////////////
 * * * * DUMMY VARIABLES: * * * * 
//////////////////////////////////////////
	
	*Create binary indicators for age categories:
	
	foreach num of numlist 1/3 55 {
	gen AGE_GROUP_`num' = 0
	replace AGE_GROUP_`num' = 1 if AGE_GROUP == `num'
	
	tab AGE_GROUP AGE_GROUP_`num', m 
	tab MAT_AGE if AGE_GROUP_`num' == 1, m 
	}
	
	label var AGE_GROUP_1 "Mother age <20 at enrollment"
	label var AGE_GROUP_2 "Mother age 20-34 at enrollment"
	label var AGE_GROUP_3 "Mother age 35+ at enrollment"
	label var AGE_GROUP_55 "Mother age unknown at enrollment" 
	
	foreach var of varlist SCHOOL_ANY SCHOOL_MORE10 SCHOOL_MORE12 {
		
	foreach num of numlist 0/1 55 {
	gen `var'_`num' = 0
	replace `var'_`num' = 1 if `var' == `num'
	
	tab `var' `var'_`num', m 
	tab `var' if `var'_`num' == 1, m 
	}
	}
	
	label var SCHOOL_ANY_0 "Mother never attended school"
	label var SCHOOL_ANY_1 "Mother attended any school"
	label var SCHOOL_ANY_55 "School status missing"
	
	label var SCHOOL_MORE10_0 "Mother attended 10 or fewer years of school"
	label var SCHOOL_MORE10_1 "Mother attended more than 10 years of school"
	label var SCHOOL_MORE10_55 "School status/length of enrollment missing"
	
	label var SCHOOL_MORE12_0 "Mother attended 12 or fewer years of school"
	label var SCHOOL_MORE12_1 "Mother attended more than 12 years of school"
	label var SCHOOL_MORE12_55 "School status/length of enrollment missing"
	
	
	foreach var of varlist WATER_IMPROVED TOILET_IMPROVED TOILET_SHARED ///
		DRINK MARRIED_18 PAID_WORK HH_SMOKE SMOKE PRIMIGRAVIDA ///
		CHEW_TOBACCO CHEW_BETELNUT UNDER_NET ///
		MISCARRIAGE MULTIPLE {
	
	foreach num of numlist 0/1 55 {
	gen `var'_`num' = 0
	replace `var'_`num' = 1 if `var' == `num'
	
	tab `var' `var'_`num', m 
	}
	
	}
	
	label var WATER_IMPROVED_0 "No improved water source"
	label var WATER_IMPROVED_1 "Improved water source"
	label var WATER_IMPROVED_55 "Unknown water source"
	
	label var TOILET_IMPROVED_0 "No improved toilet/sanitation"
	label var TOILET_IMPROVED_1 "Improved toilet/sanitation"
	label var TOILET_IMPROVED_55 "Unknown sanitation"
	
	label var TOILET_SHARED_0 "No shared toilet/latrine"
	label var TOILET_SHARED_1 "Shared toilet/latrine"
	label var TOILET_SHARED_55 "Unknown if shared toilet/latrine"
	
	label var DRINK_0 "Did not drink in the last month"
	label var DRINK_1 "Drank in the last month"
	label var DRINK_55 "Unknown"
	
	label var MARRIED_18_0 "Married at age 18 or older"
	label var MARRIED_18_1 "Married under age 18"
	label var MARRIED_18_55 "Unknown/missing"
	
	gen MARRIED_18_77 = 0
	replace MARRIED_18_77 = 1 if MARRIED_18 == 77 
	
	label var MARRIED_18_77 "Unmarried"
	
	label var PAID_WORK_0 "No paid work"
	label var PAID_WORK_1 "Paid work"
	label var PAID_WORK_55 "Unknown if paid work"
	
	label var SMOKE_0 "Mother is not a current smoker"
	label var SMOKE_1 "Mother is a current smoker"
	label var SMOKE_55 "Unknown"
	
	label var HH_SMOKE_0 "Household member(s) do not smoke at home"
	label var HH_SMOKE_1 "Household member(s) smoke at home"
	label var HH_SMOKE_55 "Unknown"
	
	label var PRIMIGRAVIDA_0 "Not first pregnancy"
	label var PRIMIGRAVIDA_1 "First pregnancy"
	label var PRIMIGRAVIDA_55 "Unknown"
	
	label var CHEW_TOBACCO_0 "Mother did not chew tobacco in last month"
	label var CHEW_TOBACCO_1 "Mother chewed tobacco in last month"
	label var CHEW_TOBACCO_55 "Unknown"
	
	label var CHEW_BETELNUT_0 "Mother did not chew Betelnut in last month"
	label var CHEW_BETELNUT_1 "Mother chewed Betelnut in last month"
	label var CHEW_BETELNUT_55 "Unknown"
	
	label var UNDER_NET_0 "Mother does not sleep under bed net"
	label var UNDER_NET_1 "Mother sleeps under bed net"
	label var UNDER_NET_55 "Unknown"	
	
	label var MISCARRIAGE_0 "No history of miscarriage"
	label var MISCARRIAGE_1 "History of miscarriage(s)"
	label var MISCARRIAGE_55 "Unknown"
	
	label var MULTIPLE_0 "Singleton pregnancy"
	label var MULTIPLE_1 "Multiple pregnancy (twins, triplets)"
	label var MULTIPLE_55 "Unknown"
	
	
	*Indicators for phone access:
	foreach var of varlist PHONE_ACCESS {
	
	foreach num of numlist 1/3 55 {
	gen `var'_`num' = 0
	replace `var'_`num' = 1 if `var' == `num'
	
	tab `var' `var'_`num', m 
	}
	
	}
	
	label var PHONE_ACCESS_1 "Has own mobile phone"
	label var PHONE_ACCESS_2 "Shared mobile phone"
	label var PHONE_ACCESS_3 "No access to mobile phone"
	label var PHONE_ACCESS_55 "Missing phone access"
	
	*Indicators for marital status
	foreach var of varlist MARSTATUS {
	
	foreach num of numlist 1/4 55 {
	gen `var'_`num' = 0
	replace `var'_`num' = 1 if `var' == `num'
	
	tab `var' `var'_`num', m 
	}
	
	}
	
	label var MARSTATUS_1 "Married"
	label var MARSTATUS_2 "Cohabitating"
	label var MARSTATUS_3 "Divorced/separated/widowed"
	label var MARSTATUS_4 "Single/never married"
	label var MARSTATUS_55 "Missing marital status"
	
	*Indicators for wealth quintile
	foreach var of varlist WEALTH_QUINT {
	
	foreach num of numlist 1/5 55 {
	gen `var'_`num' = 0
	replace `var'_`num' = 1 if `var' == `num'
	
	tab `var' `var'_`num', m 
	
	label var `var'_`num' "Quintile `num'"
	}
	
	}
	
	label var WEALTH_QUINT_55 "Missing wealth quintile"
	
	
	
	*Dummy indicators for parity
	foreach var of varlist PARITY GPARITY {
	
	foreach num of numlist 0/2 55 {
	gen `var'_`num' = 0
	replace `var'_`num' = 1 if `var' == `num'
	
	tab `var' `var'_`num', m 
	}
	
	}
	
	label var PARITY_0 "Nulliparous"
	label var PARITY_1 "Parity 1"
	label var PARITY_2 "Parity 2+"
	label var PARITY_55 "Parity Unknown"

	label var GPARITY_0 "Nulliparous"
	label var GPARITY_1 "Multi-parity (1-4)"
	label var GPARITY_2 "Grand-parity (5+)"
	label var GPARITY_55 "Parity unknown"
	
	

	keep MOMID PREGID SITE ///
		MAT_AGE AGE_GROUP* ///
			MARRIED_18* MARSTATUS* SCHOOL_ANY* SCHOOL_MORE10* ///
			SCHOOL_MORE12* PAID_WORK* PHONE_ACCESS* WEALTH_QUINT* ///
			CHEW_BETELNUT* CHEW_TOBACCO* DRINK* SMOKE* HH_SMOKE* ///
			UNDER_NET* TOILET_IMPROVED* TOILET_SHARED* ///
			WATER_IMPROVED* GPARITY* PARITY* PRIMIGRAVIDA* MISCARRIAGE* ///
			MULTIPLE* ASSET_INDEX_*
			
	order MOMID PREGID SITE ///
		MAT_AGE AGE_GROUP* ///
			MARRIED_18* MARSTATUS* SCHOOL_ANY* SCHOOL_MORE10* ///
			SCHOOL_MORE12* PAID_WORK* PHONE_ACCESS* WEALTH_QUINT* ///
			CHEW_BETELNUT* CHEW_TOBACCO* DRINK* SMOKE* HH_SMOKE* ///
			UNDER_NET* TOILET_IMPROVED* TOILET_SHARED* ///
			WATER_IMPROVED* GPARITY* PARITY* PRIMIGRAVIDA* MISCARRIAGE* ///
			MULTIPLE* ASSET_INDEX_*
		
	save "$wrk/MAT_RISKFACTORS_COMPILED", replace 
	
	
	*Prep for shared folder: 
	
	keep MOMID PREGID SITE AGE_GROUP* MARRIED_18* SCHOOL_ANY* SCHOOL_MORE10* ///
		SCHOOL_MORE12* WEALTH_QUINT* PARITY* GPARITY* ASSET_INDEX_*
		
	
	save "$OUT/MAT_RISKS", replace 	
	
	
	
