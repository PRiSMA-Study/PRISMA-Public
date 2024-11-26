*FATIGUE - ANALYSIS
**Author: Savannah F. O'Malley (savannah.omalley@gwu.edu)
**Begun April 22, 2024

**Purpose: validate the use of the FACIT-fatigue tool in a 
	***pregnant and postpartum population in 5 LMICs: 
	***Ghana, India (2 sites), Kenya, Pakistan, and Zambia

	**Set paths
	global wrk "Z:\Savannah_working_files\Fatigue"
	global output "Z:\Savannah_working_files\Fatigue\output"
	global today "2024-Nov-19"
	
	
	cd "$output"
	
	**Multiple steps:
	*1. Content Validity 
		*(qualitative assessment; not in this do file)
	*2. Internal reliability 
		*(cronbach's alpha; inter-item correlation)
	*3. Test-retest reliability 
		*(ICC between test and re-test data)
	*4. Convergent validity
		*(Spearman's correlation between FACIT-fatigue and EPDS)
	*5. Differentiation by known groups
		*(Fatigue scores between groups with suspected differences in fatigue such as anemia)

**Confirmatory factor analysis?
* sem (FATIGUE -> FTGE_AN2_R-FTGE_AN16_R) if CONTINENT==1 & ENROLL250==1, latent(FATIGUE) method(adf)


	
**RUNNING TABLES*

	//note: make sure to install table1_mc
	//ssc install table1_mc


**#TABLE 1: DEMOGRAPHICS


use "$wrk/FATIGUE_250.dta" , clear

preserve
table1_mc if  VISITNUM_ALL==1, by(SITE) ///
vars(FATIGUE_GA conts \ MARRIED cat \ AGE conts \ EDUCATED cat \ PARITY_CAT cat \ GPARITY cat \ MISCARRIAGE_M04 cat \ STILLBIRTH_M04 cat \ BMI_LEVEL cat \ PAID_WORK cat \ CHEW_ANY cat \ DRINK cat \ CURREPREGN_DESIRE_YN cat \ HB10 cat \  BEDREST cat \ CES_ANY cat \ HEM_PPH cat \ HEM_PPH_SEV cat ) ///
format(%2.0f) extraspace clear total(before) percformat(%5.1f)  percsign("") iqrmiddle(",") sdleft(" [±") sdright("]") gsdleft(" [×/") gsdright("]") onecol 
drop pvalue
table1_mc_dta2docx using "$output/Table1_Demographics-$today.docx", replace land tablenum("Table 1") tabletitle("Characteristics by site")
restore


**# Supplementary table A1: Descriptive responses (first assessment)	
	label define FTGE 0"Not at all" 1"A little bit" 2"Somewhat" 3"Quite a bit" 4"Very much"
	label val FTGE_AN2 FTGE_HI7 FTGE_HI12 FTGE_AN1V FTGE_AN3 FTGE_AN4 ///
	FTGE_AN8 FTGE_AN12 FTGE_AN14 FTGE_AN15 FTGE_AN16 FTGE_AN5 FTGE_AN7 ///
	FTGE
	
preserve
table1_mc if ENROLL250 == 1 & VISITNUM_ALL==1, by(SITE) ///
vars(FTGE_AN2 cat \ FTGE_HI7 cat \ FTGE_HI12 cat \ FTGE_AN1V cat \ FTGE_AN3 cat \ FTGE_AN4 cat \ FTGE_AN5 cat \ FTGE_AN7 cat \ FTGE_AN8 cat \ FTGE_AN12 cat \ FTGE_AN14 cat \  FTGE_AN15 cat \ FTGE_AN16 cat \ FATIGUE conts)  ///
format(%2.0f) extraspace clear total(before) percformat(%5.1f) percent percsign("") iqrmiddle(",") sdleft(" [±") sdright("]") gsdleft(" [×/") gsdright("]") onecol   
table1_mc_dta2docx using "Responses_by_SITE-$today.docx", replace land tablenum("Table A1") tabletitle("Responses by site (first administration)")
restore

****TABLE 2: Internal reliability (Cronbach's alpha)


*start the excel
putexcel set "$output/Table2_Alphas-$today.xlsx" , modify sheet("Ghana")
*putexcel  A3="An2, tired" A4="HI7, fatigued" A5="HI12, weak" A6="An1V, washed out" A7="An3, trouble starting things" A8="An4, trouble finishing things" A9="An5, have energy" A10="An7, able to do usual activities" A11="An8, need sleep during day" A12="An12, too tired to eat" A13="An14, need help usual activities" A14="An15, frustrated" A15="An16, limit social" A16="Test scale" A17="N=" B1="ENROL/ANC20" B2="Item-test correlation" B19="`site'" A20="Alpha (ANC20)" A21="Alpha (ANC32)" A22="Alpha (PNC6)" A23="Alpha (all)" C2="alpha if removed" E1="ANC32/36" E2="Item-test correlation" F2="alpha if removed" H1="PNC6" H2="Item-rest correlation" I2="alpha if removed"  K1="Overall" K2="Item-rest correlation" L2="alpha if removed"

*For each site and time point, compute Cronbach's alpha
foreach site in "Ghana" "India-CMC" "India-SAS" "Kenya" "Pakistan" "Zambia" {
	putexcel set "$output/Table2_Alphas-$today.xlsx", modify sheet("`site'")
	putexcel  A3="An2, tired" A4="HI7, fatigued" A5="HI12, weak" A6="An1V, washed out" A7="An3, trouble starting things" A8="An4, trouble finishing things" A9="An5, have energy" A10="An7, able to do usual activities" A11="An8, need sleep during day" A12="An12, too tired to eat" A13="An14, need help usual activities" A14="An15, frustrated" A15="An16, limit social" A16="Test scale" A17="N=" B1="ENROL/ANC20" B2="Item-test correlation" B19="`site'" A20="Alpha (ANC20)" A21="Alpha (ANC32)" A22="Alpha (PNC6)" A23="Alpha (all)" C2="alpha if removed" E1="ANC32/36" E2="Item-test correlation" F2="alpha if removed" H1="PNC6" H2="Item-rest correlation" I2="alpha if removed"  K1="Overall" K2="Item-rest correlation" L2="alpha if removed"
	*ENROL/ANC20 (TYPE_VISIT == 1, 2)
	qui alpha  FTGE_AN2_R FTGE_HI7_R FTGE_HI12_R FTGE_AN1V_R FTGE_AN3_R FTGE_AN4_R FTGE_AN5 FTGE_AN7 FTGE_AN8_R FTGE_AN12_R FTGE_AN14_R FTGE_AN15_R FTGE_AN16_R  if ENROLL250==1 & inlist(TYPE_VISIT,1,2) & SITE=="`site'", item asis
	qui return list
	matrix corrs=r(ItemRestCorr)'
	matrix alphas=r(Alpha)'
	putexcel B3=matrix(corrs), nformat(#.000) 
	putexcel C3=matrix(alphas),  nformat(number_d2) 
	putexcel C16=`r(alpha)', nformat(number_d2) 
	putexcel B20=`r(alpha)', nformat(number_d2) 
	foreach num of numlist 3/15 {
		putexcel D`num'=formula(IF(C`num'>`r(alpha)',"*",""))
		*if alpha increases when removed, add an asterisk
	}
	qui sum FATIGUE if ENROLL250==1 &  inlist(TYPE_VISIT,1,2) & SITE=="`site'" & FATIGUE>=0 & FATIGUE!=.
	putexcel B17=`r(N)' C20=`r(N)' 
	
	* ANC32/36 (TYPE_VISIT == 4 and 5)
	qui alpha  FTGE_AN2_R FTGE_HI7_R FTGE_HI12_R FTGE_AN1V_R FTGE_AN3_R FTGE_AN4_R FTGE_AN5 FTGE_AN7 FTGE_AN8_R FTGE_AN12_R FTGE_AN14_R FTGE_AN15_R FTGE_AN16_R  if ENROLL250==1 & inlist(TYPE_VISIT,4,5) & SITE=="`site'", item asis
	qui return list
	matrix corrs=r(ItemTestCorr)'
	matrix alphas=r(Alpha)'
	putexcel E3=matrix(corrs), nformat(#.000) 
	putexcel F3=matrix(alphas),  nformat(number_d2) 
	putexcel F16=`r(alpha)', nformat(number_d2)
	putexcel B21=`r(alpha)', nformat(number_d2)
	foreach num of numlist 3/15 {
		putexcel G`num'=formula(IF(F`num'>`r(alpha)',"*",""))
	}
	qui sum FATIGUE if ENROLL250==1 & SITE=="`site'" & inlist(TYPE_VISIT,4,5) & FATIGUE>=0 & FATIGUE!=.
	putexcel E17=`r(N)' C21=`r(N)'
	
	*PNC6 (TYPE_VISIT==10)
	qui alpha  FTGE_AN2_R FTGE_HI7_R FTGE_HI12_R FTGE_AN1V_R FTGE_AN3_R FTGE_AN4_R FTGE_AN5 FTGE_AN7 FTGE_AN8_R FTGE_AN12_R FTGE_AN14_R FTGE_AN15_R FTGE_AN16_R  if ENROLL250==1 & TYPE_VISIT==10 & SITE=="`site'", item asis
	qui return list
	matrix corrs=r(ItemTestCorr)'
	matrix alphas=r(Alpha)'
	putexcel H3=matrix(corrs), nformat(#.000) 
	putexcel I3=matrix(alphas),  nformat(number_d2) 
	putexcel I16=`r(alpha)', nformat(number_d2)
	putexcel B22=`r(alpha)', nformat(number_d2)
	foreach num of numlist 3/15 {
		putexcel J`num'=formula(IF(I`num'>`r(alpha)',"*",""))
	}
	qui sum FATIGUE if ENROLL250==1 & SITE=="`site'" & TYPE_VISIT==10 & FATIGUE>=0 & FATIGUE!=.
	putexcel H17=`r(N)' C22=`r(N)'
	
	
	*All visits
	qui alpha  FTGE_AN2_R FTGE_HI7_R FTGE_HI12_R FTGE_AN1V_R FTGE_AN3_R FTGE_AN4_R FTGE_AN5 FTGE_AN7 FTGE_AN8_R FTGE_AN12_R FTGE_AN14_R FTGE_AN15_R FTGE_AN16_R  if ENROLL250==1  & SITE=="`site'", item asis
	qui return list
	matrix corrs=r(ItemTestCorr)'
	matrix alphas=r(Alpha)'
	putexcel K3=matrix(corrs), nformat(#.000) 
	putexcel L3=matrix(alphas),  nformat(number_d2) 
	putexcel L16=`r(alpha)', nformat(number_d2)
	putexcel B23=`r(alpha)', nformat(number_d2)
	foreach num of numlist 3/15 {
		putexcel M`num'=formula(IF(L`num'>`r(alpha)',"*",""))
	}
	qui sum FATIGUE if ENROLL250==1 & SITE=="`site'"  & FATIGUE>=0 & FATIGUE!=.
	putexcel K17=`r(N)' C23=`r(N)'
}


**#TABLE 3: Test-retest reliability (ICC)
	//This is adapted from Erin's code
	**also see: https://www.statalist.org/forums/forum/general-stata-discussion/general/1749715-syntax-for-icc-for-test-retest-situation
	** https://www.stata.com/meeting/sandiego12/materials/sd12_huber.pdf
	**slides 38-39

use "$wrk/Fatigue-Retests.dta", clear

**Note: as of November 19, 2024:
	*Only 5 sites have given me their test-retest data.
	*Once Ghana uploads their files, 
	*the below code can easily be modified to include their data
foreach site in "India-SAS" "India-CMC" "Kenya" "Pakistan" "Zambia" {
	local j=2
	putexcel set "$output/Table3_ICC-$today", modify sheet("`site'")
	putexcel A1="Item" A2="An2, tired" A3="HI7, fatigued" A4="HI12, weak" A5="An1V, washed out" A6="An3, trouble starting things" A7="An4, trouble finishing things" A8="An5, have energy" A9="An7, able to do usual activities" A10="An8, need sleep during day" A11="An12, too tired to eat" A12="An14, need help usual activities" A13="An15, frustrated" A14="An16, limit social" A15="Fatigue score" B1="`site'"
	foreach var in FTGE_AN2 FTGE_HI7 FTGE_HI12 FTGE_AN1V FTGE_AN3 FTGE_AN4 FTGE_AN5 FTGE_AN7 FTGE_AN8 FTGE_AN12 FTGE_AN14 FTGE_AN15 FTGE_AN16 FATIGUE {
		cap quietly icc `var' MOMID TEST_RETEST if RETEST_AVAILABLE==1 & SITE=="`site'" & TYPE=="ANC"
		cap qui return list
		cap qui putexcel B1="`site' ANC" B`j'=`r(icc_i)' B16="N= `r(N_target)'"
		
		cap quietly icc `var' MOMID TEST_RETEST if RETEST_AVAILABLE==1 & SITE=="`site'" & TYPE=="PNC"
		cap qui return list	
		cap qui putexcel C1="`site' PNC" C`j'=`r(icc_i)' C16="N= `r(N_target)'"
		
		cap quietly icc `var' MOMID TEST_RETEST if RETEST_AVAILABLE==1 & SITE=="`site'" 
		cap qui return list	
		cap qui putexcel D1="`site' Overall" D`j'=`r(icc_i)' D16="N= `r(N_target)'"
			
		local j=`j'+1
	}
	disp as result "finish `site'"
	
}


*******TABLE 4*****
putexcel set "Table4_Differentiation", modify sheet("Ghana")


foreach site in "Ghana" "India-CMC" "India-SAS" "Kenya" "Pakistan" "Zambia"{
	putexcel set "Table4_Differentiation", modify sheet("`site'")
	putexcel A1="Item" A2="Parity" A3="    Nulliparous" A4="    1" A5="    2+" A6="Grand parity" A7="    Nulliparous" A8="    1-4" A9="    5+" A10="Anemia" A11="    No anemia" A12="    Mild anemia" A13= "    Moderate anemia" A14="    Severe anemia" A15="Timing" A16="    ANC20" A17="    ANC32" A18="    PNC6" A19="Depression (site-specific cutoff)" A20="    Not depressed" A21="    Screening for depression" A22="Severe postpartum hemorrhage" A23="    No" A24="    Yes" B1="Median" C1="Q1" D1="Q3" E1="N" G1="Concatenated"
	local j=3
	*Parity
	sum FATIGUE if PARITY == 0 & SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT <=2,d
	return list
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+1
	sum FATIGUE if PARITY == 1 & SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT <=2,d
	return list
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+1
	
	sum FATIGUE if PARITY == 2 & SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT <=2,d
	return list
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+2
	
	*Grand parity
	sum FATIGUE if GPARITY == 0 & SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT <=2,d
	return list
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+1
	
	sum FATIGUE if GPARITY == 1 & SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT <=2,d
	return list
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+1
	
	sum FATIGUE if GPARITY == 2 & SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT <=2,d
	return list
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+2
	
	**Anemia
	sum FATIGUE if ANEMIA_T2 == 0 & SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT <=2 ,d 
	return list 
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+1
	
	sum FATIGUE if ANEMIA_T2 == 1 &  SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT <=2,d
	return list 
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+1
	
	sum FATIGUE if ANEMIA_T2 == 2 &  SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT <=2,d
	return list 
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+1
	
	sum FATIGUE if ANEMIA_T2 == 3 &  SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT <=2,d
	return list 
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+2
	
	**Timing
	sum FATIGUE if TYPE_VISIT<=2 &  SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1,d
	return list 
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+1
	
	sum FATIGUE if TYPE_VISIT==4 &  SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1,d
	return list 
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+1
	
	sum FATIGUE if TYPE_VISIT==10 &  SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1,d
	return list 
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+2
	
	*Depression
	sum FATIGUE if DEPR_ANC20_SITE == 0 &  SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT <=2,d
	return list 
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+1
	sum FATIGUE if DEPR_ANC20_SITE == 1 &  SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT <=2,d
	return list 
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+2
	
	*Pospartum hemorrhage
	sum FATIGUE if HEM_PPH_SEV == 0 &  SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT ==10,d
	return list 
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))
	local j=`j'+1
	sum FATIGUE if HEM_PPH_SEV == 1 &  SITE=="`site'" & FATIGUE>=0 & FATIGUE !=. & ENROLL250 == 1 & TYPE_VISIT ==10,d
	return list 
	cap putexcel  B`j'=`r(p50)' C`j'=`r(p25)' D`j'=`r(p75)' E`j'=`r(N)' G`j'=formula(CONCATENATE(ROUND(B`j',1), " (",ROUND(C`j',1), ", ",ROUND(D`j',1), ") ", "n=",E`j'))

}

	**Correlations between known groups
	gen SITE_COL ="B" if SITE == "Ghana"
	replace SITE_COL = "C" if SITE =="India-CMC"
	replace SITE_COL = "D" if SITE == "India-SAS"
	replace SITE_COL = "E" if SITE =="Kenya"
	replace SITE_COL = "F" if SITE == "Pakistan"
	replace SITE_COL = "G" if SITE == "Zambia"
	
	putexcel set "Table4_Differentiation", modify sheet("Correlates")
	putexcel A1="Item" 							///
	A2="Parity, Rho"  A3="N" A4="p-value"      	///
	A5="Grand parity, Rho" A6="N" A7="p-value" 	///
	A8="Anemia, Rho" A9="N" A10="p-value"      	///
	A11="Timing, Rho" A12="N" A13="p-value"    	///
	A14="Depression, Rho" A15="N" A16="p-value" ///
	A17="PPH_SEV, Rho" A18="N" A19="p-value"    //// 
	B1="Ghana" C1="India-CMC" D1="India-SAS" E1="Kenya" F1="Pakistan" G1="Zambia"
	
local sites "B C D E F G"
foreach x of local sites {
	putexcel set "Table4_Differentiation", modify sheet("Correlates")
	qui corr FATIGUE PARITY if PARITY<55 & FATIGUE >=0  & TYPE_VISIT<=2 &  ENROLL250 == 1 & SITE_COL=="`x'"
	qui return list
	cap putexcel `x'2=`r(rho)' , nformat(#.00)
	cap putexcel `x'3=`r(N)'
	
	qui corr FATIGUE GPARITY if GPARITY<55 & FATIGUE >=0 & TYPE_VISIT<=2  & ENROLL250 == 1 & SITE_COL=="`x'"
	qui return list
	cap putexcel `x'5=`r(rho)' , nformat(#.00)
	cap putexcel `x'6=`r(N)'
	
	qui corr FATIGUE ANEMIA_T2 if ANEMIA_T2<55 & FATIGUE >=0 & ENROLL250 == 1 & TYPE_VISIT<=2 & SITE_COL=="`x'"
	qui return list
	cap putexcel `x'8=`r(rho)' , nformat(#.00)
	cap putexcel `x'9=`r(N)'
	
	qui corr FATIGUE TYPE_VISIT if  (TYPE_VISIT<=2 | TYPE_VISIT==4 | TYPE_VISIT==10) & FATIGUE >=0 & ENROLL250 == 1 &  SITE_COL=="`x'"
	qui return list
	cap putexcel `x'11=`r(rho)' , nformat(#.00)
	cap putexcel `x'12=`r(N)'
	
	qui corr FATIGUE DEPR_ANC20_SITE if DEPR_ANC20_SITE<55 & FATIGUE >=0 & ENROLL250 == 1 & TYPE_VISIT<=2 & SITE_COL=="`x'"
	qui return list
	cap putexcel `x'14=`r(rho)' , nformat(#.00)
	cap putexcel `x'15=`r(N)'
	
	cap corr FATIGUE HEM_PPH_SEV if HEM_PPH_SEV!=. & FATIGUE >=0 & ENROLL250 == 1 & TYPE_VISIT==10 & SITE_COL=="`x'"
	qui return list
	cap putexcel `x'17=`r(rho)' , nformat(#.00)
	cap putexcel `x'18=`r(N)'

}

putexcel set "Table5_Item_Correlations", modify sheet("Ghana")
putexcel B2="An2" B3="HI7" B4="HI12" B5="An1V" B6="An3" B7="An4" B8="An5" B9="An7" B10="An8" B11="An12" B12="An14" B13="An15" B14="An16" C1="An2" D1="HI7" E1="HI12" F1="An1V" G1="An3" H1="An4" I1="An5" J1="An7" K1="An8" L1="An12" M1="An14" N1="An15" O1="An16"
foreach site in "Ghana" "India-CMC" "India-SAS" "Kenya" "Pakistan" "Zambia" {
	putexcel set "Table5_Item_Correlations", modify sheet("`site'")
	putexcel B1="`site'" B2="An2" B3="HI7" B4="HI12" B5="An1V" B6="An3" B7="An4" B8="An5" B9="An7" B10="An8" B11="An12" B12="An14" B13="An15" B14="An16" B16="N=" C1="An2" D1="HI7" E1="HI12" F1="An1V" G1="An3" H1="An4" I1="An5" J1="An7" K1="An8" L1="An12" M1="An14" N1="An15" O1="An16"
	 corr FTGE_AN2_R FTGE_HI7_R FTGE_HI12_R FTGE_AN1V_R FTGE_AN3_R FTGE_AN4_R FTGE_AN5 FTGE_AN7 FTGE_AN8_R FTGE_AN12_R FTGE_AN14_R FTGE_AN15_R FTGE_AN16_R if SITE=="`site'" & ENROLL250==1
	 qui return list
	 matrix corrs=r(C)
	 putexcel C2=matrix(corrs), nformat(#.000) 
	 putexcel C16=`r(N)'
	 
}
