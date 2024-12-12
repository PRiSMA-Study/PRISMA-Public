
use "$da/prisma_fp_data-raw.dta", replace

*************************
*Generating Crosstabs*
*************************

*Married/ unmarried
gen married = 1 if M03_MARITAL_SCORRES==1
replace married = 0 if M03_MARITAL_SCORRES > 1 & M03_MARITAL_SCORRES!=.
la var married "=1 if Married"

*Early marriage
gen early_marriage = 1 if M03_MARITAL_AGE<18 & M03_MARITAL_AGE>0
replace early_marriage = 0 if M03_MARITAL_AGE>=18 & M03_MARITAL_AGE!=.
la var early_marriage "=1 if married before age 18 (among married)"

*Wealth


*Age group indicators
local age "M00_ESTIMATED_AGE"
gen age_grp = 1 if `age'>=15 & `age'<=19
replace age_grp = 2 if `age'>=20 & `age'<=24
replace age_grp = 3 if `age'>=25 & `age'<=29
replace age_grp = 4 if `age'>=30 & `age'<=34
replace age_grp = 5 if `age'>=35 & `age'<=39

la var age_grp "Five year age groups"

*Ever attended school
gen ever_school = 0 if M00_SCHOOL_SCORRES==0
replace ever_school = 1 if M00_SCHOOL_SCORRES==1
la var ever_school "=1 if ever attended school"

*Above median years of schooling (by site)
encode SITE, gen(sites)

gen school_abovemed = 0 if M00_SCHOOL_SCORRES!=. & M00_SCHOOL_SCORRES!=77
la var school_abovemed "=1 if above median schooling (by country)"

foreach s in 1 2 3 4 5 {
	
	sum M00_SCHOOL_YRS_SCORRES if sites==`s' & M00_SCHOOL_YRS_SCORRES>0, detail
	
	replace school_abovemed = 1 ///
		if M00_SCHOOL_YRS_SCORRES>r(p50) & M00_SCHOOL_YRS_SCORRES!=. & sites==`s'
	
}

*Have own mobile phone (Ghana, India-CMC, Kenya, Pakistan, Zambia)
local ph M03_MOBILE_ACCESS_FCORRES
gen own_phone = 0 if `ph'==0 | `ph'==2
replace own_phone = 1 if `ph'==1
la var own_phone "=1 if owns own cell phone"

*************************
*Generating FP variables*
*************************
*use "$da/mnh12_processed.dta", clear
rename *, lower

//generating base indicators

local 1 "condom"
local 2 "pills"
local 3 "pills"
local 4 "inj"
local 5 "inj"
local 6 "iud"
local 7 "impl"
local 8 "sdm"
local 9 "lam"
local 10 "steril"
local 11 "steril"
local 12 "withdrawal"

local condom "condoms"
local pills "pills"
local inj "injectables"
local iud "an IUD"
local impl "an implant"
local sdm "standard days method"
local lam "LAM"
local steril "sterilization"
local withdrawal "withdrawal"

foreach i in 1 2 4 6 7 8 9 10 12 {
	
	gen use_``i''=0 if m12_method_fp_faorres==1 | m12_method_fp_faorres==0
	
	la var use_``i'' "Mother has used ```i''' since giving birth"
}


forvalues i=1/12 {

	replace use_``i''=1 if m12_method_fp_spfy_faorres==`i'

}

gen use_anyFP = 0 if m12_method_fp_faorres==1 | m12_method_fp_faorres==0
replace use_anyFP = 1 if  m12_method_fp_faorres==1
la var use_anyFP "Mother has used any method of family planning since giving birth"

gen use_modern=0 if use_anyFP!=.
foreach x in condom pill inj iud impl lam steril {
replace use_modern = 1 if use_`x' ==1
}
la var use_modern "Mother has used a modern method of FP (condom, pill, inj, iud, impl, lam)"

gen use_medical = 0
foreach x in condom pill inj iud impl steril {
	replace use_medical = 1 if use_`x' ==1
	
}
la var use_medical "Mother has used a medical method of FP (pill, inj, iud, impl, steril)"



drop if m12_method_fp_faorres==77 | m12_method_fp_faorres==99
la var m12_method_fp_faorres "=1 if adopted any method of family planning"

*Reason for non-use
/*
1 Not interested
2 Haven't resumed sex after pregnancy
3 Fear of side effects
4 Partner disapproval
5 Methods weren't available
6 Not worried about pregnancy
7 No awareness of family planning options
8 Menstruation has not yet restarted after pregnancy
55 Missing
66 Refused to answer
77 Not applicable
88 Other
99 Don't know
*/

keep site momid pregid m12_type_visit married early_marriage age_grp ever_school ///
		  school_abovemed own_phone first_preg use_condom use_pills use_inj ///
		  m12_method_fp_faorres use_iud use_impl use_sdm use_lam use_steril ///
		  use_withdrawal use_anyFP use_modern use_medical

save "$da/prisma_fp_data-constructed.dta", replace
