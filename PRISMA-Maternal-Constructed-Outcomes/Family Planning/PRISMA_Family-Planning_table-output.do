

gl date 240428

//Creating the output table
use "$da/prisma_fp_data-constructed.dta", clear

****************************
*Ever adopt (across visits)*
****************************
preserve 
gl fp use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical

foreach var in $fp {
	
	local l_`var' : variable label `var'
	
}

collapse (max) m12_method_fp_faorres use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical , by(site momid pregid)

foreach var in $fp {
	la var `var' "`l_`var''"
}

gl fp use_anyFP use_modern use_medical use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal 

tabstat $fp , stat(sum mean) col(statistics) long save
matrix overall=r(StatTotal)'

foreach x in Ghana Kenya Pakistan Zambia {
	tabstat $fp if site=="`x'", stat(sum mean) col(statistics) long save
	matrix `x'=r(StatTotal)' 
}

tab m12_method_fp_faorres
matrix N=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	tab m12_method_fp_faorres if site=="`x'"
	matrix N_`x'=r(N)
}

tab m12_method_fp_faorres if (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
matrix Nd=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	tab m12_method_fp_faorres if site=="`x'" & (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
	matrix N_`x'd=r(N)
}

putexcel set "$output/Family_Planning_Report_$date.xlsx", sheet(Since Birth)replace

putexcel A1 = "Postpartum Family planning utilization: Ever Use"

putexcel B2 = "All Sites"
putexcel D2 = "Ghana"
putexcel F2 = "Kenya"
putexcel H2 = "Pakistan"
putexcel J2 = "Zambia"

putexcel A3 = matrix(overall), names nformat(number)
putexcel D4 = matrix(Ghana), nformat(number)
putexcel F4 = matrix(Kenya), nformat(number)
putexcel H4 = matrix(Pakistan), nformat(number)
putexcel J4 = matrix(Zambia), nformat(number)

putexcel A16 = "Denominator"
putexcel B16 = matrix(Nd), nformat(number)
putexcel D16 = matrix(N_Ghanad), nformat(number)
putexcel F16 = matrix(N_Kenyad), nformat(number)
putexcel H16 = matrix(N_Pakistand), nformat(number)
putexcel J16 = matrix(N_Zambiad), nformat(number)
/*
putexcel A17 = "Total Sample"
putexcel B17 = matrix(N), nformat(number)
putexcel D17 = matrix(N_Ghana), nformat(number)
putexcel F17 = matrix(N_Kenya), nformat(number)
putexcel H17 = matrix(N_Pakistan), nformat(number)
putexcel J17 = matrix(N_Zambia), nformat(number)
*/
foreach l in B D F H J {
putexcel `l'3 = "N"
}

foreach l in C E G I K {
putexcel `l'3 = "%"
}

local i = 4
foreach v of global fp {
	local ColVar`i' = "`v'"
	local ColVarLabel`i' : variable label `ColVar`i''
	putexcel A`i' = "`ColVarLabel`i''"
	macro drop ColVar`i' ColVarLabel`i'
		local i = `i' + 1
	}

	foreach l in C E G I K {
	
	putexcel `l'4:`l'100, nformat("0.00%") overwritefmt
	
}

restore
exit
****************************
***PNC Timing of adoption***
****************************

local 7 "PNC-0"
local 8 "PNC-1"
local 9 "PNC-4"
local 10 "PNC-6"
local 11 "PNC-26"

local j = 19

foreach tv in 7 8 9 10 11 {
	
preserve 
gl fp use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical
foreach var in $fp {
	
	local l_`var' : variable label `var'
	
}
egen temp0 = max(m12_type_visit) if m12_type_visit<14, by(momid pregid)
egen temp = max(temp0),  by(momid pregid)
keep if  m12_type_visit<=`tv' & temp>=`tv' & temp!=.
collapse (max) m12_method_fp_faorres use_condom use_pill use_inj use_iud ///
			   use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP ///
			   use_modern use_medical , ///
			   by(site momid pregid)

foreach var in $fp {
	la var `var' "`l_`var''"
}

gl fp use_anyFP use_modern use_medical use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal 

cap tabstat $fp , stat(sum mean) col(statistics) long save
matrix overall=r(StatTotal)'

foreach x in Ghana Kenya Pakistan Zambia {
cap	tabstat $fp if site=="`x'", stat(sum mean) col(statistics) long save
	matrix `x'=r(StatTotal)' 
}

tab m12_method_fp_faorres
matrix N=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
cap	tab m12_method_fp_faorres if site=="`x'"
	matrix N_`x'=r(N)
}

tab m12_method_fp_faorres if (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
matrix Nd=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	tab m12_method_fp_faorres if site=="`x'" & (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
	matrix N_`x'd=r(N)
}

putexcel set "$output/Family_Planning_Report_$date.xlsx", sheet(Since Birth) modify

putexcel A`j' = "Postpartum Family planning utilization: As of ``tv''"

local jj=`j'+1
local jjj=`j'+2
local jjjj = `j'+3
local jjjjj = `j'+15

putexcel B`jj' = "All Sites"
putexcel D`jj' = "Ghana"
putexcel F`jj' = "Kenya"
putexcel H`jj' = "Pakistan"
putexcel J`jj' = "Zambia"

putexcel A`jjj' = matrix(overall), names nformat(number)
putexcel D`jjjj' = matrix(Ghana), nformat(number)
putexcel F`jjjj' = matrix(Kenya), nformat(number)
putexcel H`jjjj' = matrix(Pakistan), nformat(number)
putexcel J`jjjj' = matrix(Zambia), nformat(number)

putexcel A`jjjjj' = "Denominator"
putexcel B`jjjjj' = matrix(Nd), nformat(number)
putexcel D`jjjjj' = matrix(N_Ghanad), nformat(number)
putexcel F`jjjjj' = matrix(N_Kenyad), nformat(number)
putexcel H`jjjjj' = matrix(N_Pakistand), nformat(number)
putexcel J`jjjjj' = matrix(N_Zambiad), nformat(number)

foreach l in B D F H J {
putexcel `l'`jjj' = "N"
}

foreach l in C E G I K {
putexcel `l'`jjj' = "%"
}

local i = 3+`j'
foreach v of global fp {
	local ColVar`i' = "`v'"
	local ColVarLabel`i' : variable label `ColVar`i''
	putexcel A`i' = "`ColVarLabel`i''"
	macro drop ColVar`i' ColVarLabel`i'
		local i = `i' + 1
	}

	foreach l in C E G I K {
	
	putexcel `l'4:`l'200, nformat("0.00%") overwritefmt
	
}
	local j= `j'+19
	restore
}

****************************
***PPFP adoption by parity**
****************************

local 0 "First Pregnancy"
local 1 "Second Pregnancy"
local 2 "Third Pregnancy"
local 3 "Higher Order Pregnancy"

local j=1

foreach p in 0 1 2 3 {
preserve 

keep if first_preg==`p'

gl fp use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical
foreach var in $fp {
	
	local l_`var' : variable label `var'
	
}

collapse (max) m12_method_fp_faorres use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical , by(site momid pregid)

foreach var in $fp {
	la var `var' "`l_`var''"
}

gl fp use_anyFP use_modern use_medical use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal 

tabstat $fp , stat(sum mean) col(statistics) long save
matrix overall=r(StatTotal)'

foreach x in Ghana Kenya Pakistan Zambia {
	tabstat $fp if site=="`x'", stat(sum mean) col(statistics) long save
	matrix `x'=r(StatTotal)' 
}

tab m12_method_fp_faorres
matrix N=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	tab m12_method_fp_faorres if site=="`x'"
	matrix N_`x'=r(N)
}

tab m12_method_fp_faorres if (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
matrix Nd=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	tab m12_method_fp_faorres if site=="`x'" & (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
	matrix N_`x'd=r(N)
}

putexcel set "$output/Family_Planning_Report_$date.xlsx", sheet(Parity) modify

putexcel A`j' = "Postpartum Family planning utilization: ``p''"

local jj=`j'+1
local jjj=`j'+2
local jjjj = `j'+3
local jjjjj = `j'+15

putexcel B`jj' = "All Sites"
putexcel D`jj' = "Ghana"
putexcel F`jj' = "Kenya"
putexcel H`jj' = "Pakistan"
putexcel J`jj' = "Zambia"

putexcel A`jjj' = matrix(overall), names nformat(number)
putexcel D`jjjj' = matrix(Ghana), nformat(number)
putexcel F`jjjj' = matrix(Kenya), nformat(number)
putexcel H`jjjj' = matrix(Pakistan), nformat(number)
putexcel J`jjjj' = matrix(Zambia), nformat(number)

putexcel A`jjjjj' = "Denominator"
putexcel B`jjjjj' = matrix(Nd), nformat(number)
putexcel D`jjjjj' = matrix(N_Ghanad), nformat(number)
putexcel F`jjjjj' = matrix(N_Kenyad), nformat(number)
putexcel H`jjjjj' = matrix(N_Pakistand), nformat(number)
putexcel J`jjjjj' = matrix(N_Zambiad), nformat(number)

foreach l in B D F H J {
putexcel `l'`jjj' = "N"
}

foreach l in C E G I K {
putexcel `l'`jjj' = "%"
}

local i = 3+`j'
foreach v of global fp {
	local ColVar`i' = "`v'"
	local ColVarLabel`i' : variable label `ColVar`i''
	putexcel A`i' = "`ColVarLabel`i''"
	macro drop ColVar`i' ColVarLabel`i'
		local i = `i' + 1
	}

	foreach l in C E G I K {
	
	putexcel `l'4:`l'200, nformat("0.00%") overwritefmt
	
}
	local j= `j'+19
restore
}



****************************
***PPFP adoption by AGe**
****************************

local 1 "15-19 years old"
local 2 "20-24 years old"
local 3 "25-29 years old"
local 4 "30-34 years old"
local 5 "35-39 years old"

local j=1

foreach p in 1 2 3 4 5 {
preserve 

keep if age_grp==`p'

gl fp use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical
foreach var in $fp {
	
	local l_`var' : variable label `var'
	
}

collapse (max) m12_method_fp_faorres use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical , by(site momid pregid)

foreach var in $fp {
	la var `var' "`l_`var''"
}

gl fp use_anyFP use_modern use_medical use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal 

tabstat $fp , stat(sum mean) col(statistics) long save
matrix overall=r(StatTotal)'

foreach x in Ghana Kenya Pakistan Zambia {
	cap tabstat $fp if site=="`x'", stat(sum mean) col(statistics) long save
	matrix `x'=r(StatTotal)' 
}

tab m12_method_fp_faorres
matrix N=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	cap tab m12_method_fp_faorres if site=="`x'"
	matrix N_`x'=r(N)
}

tab m12_method_fp_faorres if (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
matrix Nd=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	cap tab m12_method_fp_faorres if site=="`x'" & (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
	matrix N_`x'd=r(N)
}

putexcel set "$output/Family_Planning_Report_$date.xlsx", sheet(Age) modify

putexcel A`j' = "Postpartum Family planning utilization: Ages ``p''"

local jj=`j'+1
local jjj=`j'+2
local jjjj = `j'+3
local jjjjj = `j'+15

putexcel B`jj' = "All Sites"
putexcel D`jj' = "Ghana"
putexcel F`jj' = "Kenya"
putexcel H`jj' = "Pakistan"
putexcel J`jj' = "Zambia"

putexcel A`jjj' = matrix(overall), names nformat(number)
putexcel D`jjjj' = matrix(Ghana), nformat(number)
putexcel F`jjjj' = matrix(Kenya), nformat(number)
putexcel H`jjjj' = matrix(Pakistan), nformat(number)
putexcel J`jjjj' = matrix(Zambia), nformat(number)

putexcel A`jjjjj' = "Denominator"
putexcel B`jjjjj' = matrix(Nd), nformat(number)
putexcel D`jjjjj' = matrix(N_Ghanad), nformat(number)
putexcel F`jjjjj' = matrix(N_Kenyad), nformat(number)
putexcel H`jjjjj' = matrix(N_Pakistand), nformat(number)
putexcel J`jjjjj' = matrix(N_Zambiad), nformat(number)

foreach l in B D F H J {
putexcel `l'`jjj' = "N"
}

foreach l in C E G I K {
putexcel `l'`jjj' = "%"
}

local i = 3+`j'
foreach v of global fp {
	local ColVar`i' = "`v'"
	local ColVarLabel`i' : variable label `ColVar`i''
	putexcel A`i' = "`ColVarLabel`i''"
	macro drop ColVar`i' ColVarLabel`i'
		local i = `i' + 1
	}

	foreach l in C E G I K {
	
	putexcel `l'4:`l'200, nformat("0.00%") overwritefmt
	
}
	local j= `j'+19
restore
}


****************************
***PPFP adoption by Marital Status**
****************************

local 0 "Unmarried"
local 1 "Married"


local j=1

foreach p in 0 1 {
preserve 

keep if married==`p'

gl fp use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical
foreach var in $fp {
	
	local l_`var' : variable label `var'
	
}

collapse (max) m12_method_fp_faorres use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical , by(site momid pregid)

foreach var in $fp {
	la var `var' "`l_`var''"
}

gl fp use_anyFP use_modern use_medical use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal 

tabstat $fp , stat(sum mean) col(statistics) long save
matrix overall=r(StatTotal)'

foreach x in Ghana Kenya Pakistan Zambia {
	cap tabstat $fp if site=="`x'", stat(sum mean) col(statistics) long save
	matrix `x'=r(StatTotal)' 
}

tab m12_method_fp_faorres
matrix N=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	cap tab m12_method_fp_faorres if site=="`x'"
	matrix N_`x'=r(N)
}

tab m12_method_fp_faorres if (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
matrix Nd=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	cap tab m12_method_fp_faorres if site=="`x'" & (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
	matrix N_`x'd=r(N)
}

putexcel set "$output/Family_Planning_Report_$date.xlsx", sheet("Marital Status") modify

putexcel A`j' = "Postpartum Family planning utilization: ``p''"

local jj=`j'+1
local jjj=`j'+2
local jjjj = `j'+3
local jjjjj = `j'+15

putexcel B`jj' = "All Sites"
putexcel D`jj' = "Ghana"
putexcel F`jj' = "Kenya"
putexcel H`jj' = "Pakistan"
putexcel J`jj' = "Zambia"

putexcel A`jjj' = matrix(overall), names nformat(number)
putexcel D`jjjj' = matrix(Ghana), nformat(number)
putexcel F`jjjj' = matrix(Kenya), nformat(number)
putexcel H`jjjj' = matrix(Pakistan), nformat(number)
putexcel J`jjjj' = matrix(Zambia), nformat(number)

putexcel A`jjjjj' = "Denominator"
putexcel B`jjjjj' = matrix(Nd), nformat(number)
putexcel D`jjjjj' = matrix(N_Ghanad), nformat(number)
putexcel F`jjjjj' = matrix(N_Kenyad), nformat(number)
putexcel H`jjjjj' = matrix(N_Pakistand), nformat(number)
putexcel J`jjjjj' = matrix(N_Zambiad), nformat(number)

foreach l in B D F H J {
putexcel `l'`jjj' = "N"
}

foreach l in C E G I K {
putexcel `l'`jjj' = "%"
}

local i = 3+`j'
foreach v of global fp {
	local ColVar`i' = "`v'"
	local ColVarLabel`i' : variable label `ColVar`i''
	putexcel A`i' = "`ColVarLabel`i''"
	macro drop ColVar`i' ColVarLabel`i'
		local i = `i' + 1
	}

	foreach l in C E G I K {
	
	putexcel `l'4:`l'200, nformat("0.00%") overwritefmt
	
}
	local j= `j'+19
restore
}


****************************
***PPFP adoption by School Enrollment **
****************************

local 0 "Never Enrolled in School"
local 1 "Ever Enrolled in School"


local j=1

foreach p in 0 1 {
preserve 

keep if ever_school==`p'

gl fp use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical
foreach var in $fp {
	
	local l_`var' : variable label `var'
	
}

collapse (max) m12_method_fp_faorres use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical , by(site momid pregid)

foreach var in $fp {
	la var `var' "`l_`var''"
}

gl fp use_anyFP use_modern use_medical use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal 

tabstat $fp , stat(sum mean) col(statistics) long save
matrix overall=r(StatTotal)'

foreach x in Ghana Kenya Pakistan Zambia {
	cap tabstat $fp if site=="`x'", stat(sum mean) col(statistics) long save
	matrix `x'=r(StatTotal)' 
}

tab m12_method_fp_faorres
matrix N=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	cap tab m12_method_fp_faorres if site=="`x'"
	matrix N_`x'=r(N)
}

tab m12_method_fp_faorres if (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
matrix Nd=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	cap tab m12_method_fp_faorres if site=="`x'" & (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
	matrix N_`x'd=r(N)
}

putexcel set "$output/Family_Planning_Report_$date.xlsx", sheet("School enrollment") modify

putexcel A`j' = "Postpartum Family planning utilization: ``p''"

local jj=`j'+1
local jjj=`j'+2
local jjjj = `j'+3
local jjjjj = `j'+15

putexcel B`jj' = "All Sites"
putexcel D`jj' = "Ghana"
putexcel F`jj' = "Kenya"
putexcel H`jj' = "Pakistan"
putexcel J`jj' = "Zambia"

putexcel A`jjj' = matrix(overall), names nformat(number)
putexcel D`jjjj' = matrix(Ghana), nformat(number)
putexcel F`jjjj' = matrix(Kenya), nformat(number)
putexcel H`jjjj' = matrix(Pakistan), nformat(number)
putexcel J`jjjj' = matrix(Zambia), nformat(number)

putexcel A`jjjjj' = "Denominator"
putexcel B`jjjjj' = matrix(Nd), nformat(number)
putexcel D`jjjjj' = matrix(N_Ghanad), nformat(number)
putexcel F`jjjjj' = matrix(N_Kenyad), nformat(number)
putexcel H`jjjjj' = matrix(N_Pakistand), nformat(number)
putexcel J`jjjjj' = matrix(N_Zambiad), nformat(number)

foreach l in B D F H J {
putexcel `l'`jjj' = "N"
}

foreach l in C E G I K {
putexcel `l'`jjj' = "%"
}

local i = 3+`j'
foreach v of global fp {
	local ColVar`i' = "`v'"
	local ColVarLabel`i' : variable label `ColVar`i''
	putexcel A`i' = "`ColVarLabel`i''"
	macro drop ColVar`i' ColVarLabel`i'
		local i = `i' + 1
	}

	foreach l in C E G I K {
	
	putexcel `l'4:`l'200, nformat("0.00%") overwritefmt
	
}
	local j= `j'+19
restore
}


****************************
***PPFP adoption by Above median years of schooling**
****************************

local 0 "Years of schooling is Median or below (within site)"
local 1 "Above median years of schooling (within site)"


local j=1

foreach p in 0 1 {
preserve 

keep if school_abovemed==`p'

gl fp use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical
foreach var in $fp {
	
	local l_`var' : variable label `var'
	
}

collapse (max) m12_method_fp_faorres use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical , by(site momid pregid)

foreach var in $fp {
	la var `var' "`l_`var''"
}

gl fp use_anyFP use_modern use_medical use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal 

tabstat $fp , stat(sum mean) col(statistics) long save
matrix overall=r(StatTotal)'

foreach x in Ghana Kenya Pakistan Zambia {
	cap tabstat $fp if site=="`x'", stat(sum mean) col(statistics) long save
	matrix `x'=r(StatTotal)' 
}

tab m12_method_fp_faorres
matrix N=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	cap tab m12_method_fp_faorres if site=="`x'"
	matrix N_`x'=r(N)
}

tab m12_method_fp_faorres if (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
matrix Nd=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	cap tab m12_method_fp_faorres if site=="`x'" & (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
	matrix N_`x'd=r(N)
}

putexcel set "$output/Family_Planning_Report_$date.xlsx", sheet("Years of Schooling") modify

putexcel A`j' = "Postpartum Family planning utilization: ``p''"

local jj=`j'+1
local jjj=`j'+2
local jjjj = `j'+3
local jjjjj = `j'+15

putexcel B`jj' = "All Sites"
putexcel D`jj' = "Ghana"
putexcel F`jj' = "Kenya"
putexcel H`jj' = "Pakistan"
putexcel J`jj' = "Zambia"

putexcel A`jjj' = matrix(overall), names nformat(number)
putexcel D`jjjj' = matrix(Ghana), nformat(number)
putexcel F`jjjj' = matrix(Kenya), nformat(number)
putexcel H`jjjj' = matrix(Pakistan), nformat(number)
putexcel J`jjjj' = matrix(Zambia), nformat(number)

putexcel A`jjjjj' = "Denominator"
putexcel B`jjjjj' = matrix(Nd), nformat(number)
putexcel D`jjjjj' = matrix(N_Ghanad), nformat(number)
putexcel F`jjjjj' = matrix(N_Kenyad), nformat(number)
putexcel H`jjjjj' = matrix(N_Pakistand), nformat(number)
putexcel J`jjjjj' = matrix(N_Zambiad), nformat(number)

foreach l in B D F H J {
putexcel `l'`jjj' = "N"
}

foreach l in C E G I K {
putexcel `l'`jjj' = "%"
}

local i = 3+`j'
foreach v of global fp {
	local ColVar`i' = "`v'"
	local ColVarLabel`i' : variable label `ColVar`i''
	putexcel A`i' = "`ColVarLabel`i''"
	macro drop ColVar`i' ColVarLabel`i'
		local i = `i' + 1
	}

	foreach l in C E G I K {
	
	putexcel `l'4:`l'200, nformat("0.00%") overwritefmt
	
}
	local j= `j'+19
restore
}


****************************
***PPFP adoption by cell phone ownership**
****************************

local 0 "Does not have personal cell phone"
local 1 "Has personal cell phone"


local j=1

foreach p in 0 1 {
preserve 

keep if own_phone==`p'

gl fp use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical
foreach var in $fp {
	
	local l_`var' : variable label `var'
	
}

collapse (max) m12_method_fp_faorres use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical , by(site momid pregid)

foreach var in $fp {
	la var `var' "`l_`var''"
}

gl fp use_anyFP use_modern use_medical use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal 

tabstat $fp , stat(sum mean) col(statistics) long save
matrix overall=r(StatTotal)'

foreach x in Ghana Kenya Pakistan Zambia {
	cap tabstat $fp if site=="`x'", stat(sum mean) col(statistics) long save
	matrix `x'=r(StatTotal)' 
}

tab m12_method_fp_faorres
matrix N=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	cap tab m12_method_fp_faorres if site=="`x'"
	matrix N_`x'=r(N)
}

tab m12_method_fp_faorres if (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
matrix Nd=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	cap tab m12_method_fp_faorres if site=="`x'" & (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
	matrix N_`x'd=r(N)
}

putexcel set "$output/Family_Planning_Report_$date.xlsx", sheet("Cell Phone") modify

putexcel A`j' = "Postpartum Family planning utilization: ``p''"

local jj=`j'+1
local jjj=`j'+2
local jjjj = `j'+3
local jjjjj = `j'+15

putexcel B`jj' = "All Sites"
putexcel D`jj' = "Ghana"
putexcel F`jj' = "Kenya"
putexcel H`jj' = "Pakistan"
putexcel J`jj' = "Zambia"

putexcel A`jjj' = matrix(overall), names nformat(number)
putexcel D`jjjj' = matrix(Ghana), nformat(number)
putexcel F`jjjj' = matrix(Kenya), nformat(number)
putexcel H`jjjj' = matrix(Pakistan), nformat(number)
putexcel J`jjjj' = matrix(Zambia), nformat(number)

putexcel A`jjjjj' = "Denominator"
putexcel B`jjjjj' = matrix(Nd), nformat(number)
putexcel D`jjjjj' = matrix(N_Ghanad), nformat(number)
putexcel F`jjjjj' = matrix(N_Kenyad), nformat(number)
putexcel H`jjjjj' = matrix(N_Pakistand), nformat(number)
putexcel J`jjjjj' = matrix(N_Zambiad), nformat(number)

foreach l in B D F H J {
putexcel `l'`jjj' = "N"
}

foreach l in C E G I K {
putexcel `l'`jjj' = "%"
}

local i = 3+`j'
foreach v of global fp {
	local ColVar`i' = "`v'"
	local ColVarLabel`i' : variable label `ColVar`i''
	putexcel A`i' = "`ColVarLabel`i''"
	macro drop ColVar`i' ColVarLabel`i'
		local i = `i' + 1
	}

	foreach l in C E G I K {
	
	putexcel `l'4:`l'200, nformat("0.00%") overwritefmt
	
}
	local j= `j'+19
restore
}


****************************
***PPFP adoption by AGe - two groups**
****************************

gen young = 1 if age_grp == 1 | age_grp==2
replace young = 0 if age_grp==3 | age_grp==4 | age_grp==5

local 1 "15-24 years old"
local 0 "25-39 years old"

local j=1

foreach p in 0 1{
preserve 

keep if young==`p'

gl fp use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical
foreach var in $fp {
	
	local l_`var' : variable label `var'
	
}

collapse (max) m12_method_fp_faorres use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal use_anyFP use_modern use_medical , by(site momid pregid)

foreach var in $fp {
	la var `var' "`l_`var''"
}

gl fp use_anyFP use_modern use_medical use_condom use_pill use_inj use_iud use_impl use_sdm use_lam use_steril use_withdrawal 

tabstat $fp , stat(sum mean) col(statistics) long save
matrix overall=r(StatTotal)'

foreach x in Ghana Kenya Pakistan Zambia {
	cap tabstat $fp if site=="`x'", stat(sum mean) col(statistics) long save
	matrix `x'=r(StatTotal)' 
}

tab m12_method_fp_faorres
matrix N=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	cap tab m12_method_fp_faorres if site=="`x'"
	matrix N_`x'=r(N)
}

tab m12_method_fp_faorres if (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
matrix Nd=r(N)
foreach x in Ghana Kenya Pakistan Zambia {
	cap tab m12_method_fp_faorres if site=="`x'" & (m12_method_fp_faorres==1 | m12_method_fp_faorres==0)
	matrix N_`x'd=r(N)
}

putexcel set "$output/Family_Planning_Report_$date.xlsx", sheet("Age-young-old") modify

putexcel A`j' = "Postpartum Family planning utilization: Ages ``p''"

local jj=`j'+1
local jjj=`j'+2
local jjjj = `j'+3
local jjjjj = `j'+15

putexcel B`jj' = "All Sites"
putexcel D`jj' = "Ghana"
putexcel F`jj' = "Kenya"
putexcel H`jj' = "Pakistan"
putexcel J`jj' = "Zambia"

putexcel A`jjj' = matrix(overall), names nformat(number)
putexcel D`jjjj' = matrix(Ghana), nformat(number)
putexcel F`jjjj' = matrix(Kenya), nformat(number)
putexcel H`jjjj' = matrix(Pakistan), nformat(number)
putexcel J`jjjj' = matrix(Zambia), nformat(number)

putexcel A`jjjjj' = "Denominator"
putexcel B`jjjjj' = matrix(Nd), nformat(number)
putexcel D`jjjjj' = matrix(N_Ghanad), nformat(number)
putexcel F`jjjjj' = matrix(N_Kenyad), nformat(number)
putexcel H`jjjjj' = matrix(N_Pakistand), nformat(number)
putexcel J`jjjjj' = matrix(N_Zambiad), nformat(number)

foreach l in B D F H J {
putexcel `l'`jjj' = "N"
}

foreach l in C E G I K {
putexcel `l'`jjj' = "%"
}

local i = 3+`j'
foreach v of global fp {
	local ColVar`i' = "`v'"
	local ColVarLabel`i' : variable label `ColVar`i''
	putexcel A`i' = "`ColVarLabel`i''"
	macro drop ColVar`i' ColVarLabel`i'
		local i = `i' + 1
	}

	foreach l in C E G I K {
	
	putexcel `l'4:`l'200, nformat("0.00%") overwritefmt
	
}
	local j= `j'+19
restore
}

