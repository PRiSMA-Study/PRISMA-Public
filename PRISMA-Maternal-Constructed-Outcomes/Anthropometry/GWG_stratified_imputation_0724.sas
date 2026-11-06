options nofmterr;
libname GWG "D:\Users\xinyili\Documents\Analysis\Gestational Weight Gain\data_out";

/* data GWG.ANALYSIS_2025_01_10    ;*/
/*       %let _EFIERR_ = 0; /* set the ERROR detection macro variable */*/
/*       infile 'D:\Users\xinyili\Documents\Analysis\Gestational Weight Gain\data_out\GWG_IMPUTE_LONG_2025-01-10.csv'*/
/*  delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;*/
/*		  informat ID $18. ;*/
/*          informat SITE $8. ;*/
/*          informat SCRNID $20. ;*/
/*          informat MOMID $50. ;*/
/*          informat PREGID $50. ;*/
/*		  informat TYPE_VISIT best32. ;*/
/*		  informat EST_CONCEP_DATE $12. ;*/
/*		  informat PREG_END $2. ;*/
/*		  informat M05_HEIGHT_PERES $5. ;*/
/*		  informat M05_WEIGHT_PERES best32. ;*/
/*		  informat WEIGHT_ENROLL best32. ;*/
/*          informat GA_WKS best32. ;*/
/**/
/*		  format ID $18. ;*/
/*          format SCRNID $20. ;*/
/*          format SITE $8. ;*/
/*          format PREGID $50. ;*/
/*          format MOMID $50. ;*/
/*		  format TYPE_VISIT best12. ;*/
/*		  format EST_CONCEP_DATE $12. ;*/
/*		  format PREG_END $2. ;*/
/*		  format M05_HEIGHT_PERES $5. ;*/
/*		  format M05_WEIGHT_PERES best12. ;*/
/*		  format WEIGHT_ENROLL best12. ;*/
/*          format GA_WKS best12. ;*/
/**/
/*       input*/
/*				   ID  $*/
/*				   SITE  $*/
/*                   SCRNID  $*/
/*                   MOMID  $*/
/*                   PREGID  $*/
/*                   TYPE_VISIT*/
/*				   EST_CONCEP_DATE $	*/
/*				   PREG_END $	*/
/*				   M05_HEIGHT_PERES $	*/
/*				   M05_WEIGHT_PERES 	*/
/*				   WEIGHT_ENROLL	*/
/*				   GA_WKS*/
/*      ;*/
/*      if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */*/
/*  run;*/

  *SITE $ SCRNID $	MOMID $	PREGID $	TYPE_VISIT	EST_CONCEP_DATE $	PREG_END $	M05_HEIGHT_PERES $	M05_WEIGHT_PERES 	WEIGHT_ENROLL	GA_WKS;


proc import datafile="D:\Users\xinyili\Documents\Analysis\Gestational Weight Gain\data_out\GWG_IMPUTE_LONG_2026-05-01.csv"
out=analysis
dbms=csv
replace;
GUESSINGROWS=MAX;
getnames=yes;
run;

/*data GWG.ANALYSIS_2025_01_10;*/
/*set mydata;*/
/*run;*/
/**/
/*data analysis;*/
/*set GWG.ANALYSIS_2025_01_10;*/
/*run;*/

/* Tabulations of GBD regions and 10-year calendar year periods */
proc freq data = analysis;
table site;
run;
proc contents data=analysis;run;
data analysis; 
set analysis; 
rename M05_WEIGHT_PERES=mweight;
GA_week=GA_WKS*1; 
drop GA_WKS;
run;
proc sort data=analysis; by site pregid ga_week; run;
/************* Dividing the combined dataset into sub datasets by GBD super region *************/

data region_ghana;
set analysis;
where site eq "Ghana";
run;

data region_cmc;
set analysis;
where site eq "India-CMC";
run;

data region_sas;
set analysis;
where site eq "India-SAS";
run;

data region_kenya;
set analysis;
where site eq "Kenya";
run;

data region_pakistan;
set analysis;
where site eq "Pakistan";
run;

data region_zambia;
set analysis;
where site eq "Zambia";
run;

/************************************************************************************************************/
/************ Stratified imputation(by GBD super region) using restricted cubic splines *********************/
/************************************************************************************************************/

/*************************************************/
/********** Ghana **********/
/*************************************************/
proc univariate data = region_ghana;		
var ga_week;	
where ga_week lt 28;	
output out = temp pctlpre=P_ pctlpts=25,50,75;		
run;	
proc print data = temp; run;* get three values to put in the formula later, this should be run everytime;
* Qing's codes;
/*proc hpmixed data= region_ghana;*/
/*class momid; 	*/
/*where ga_week lt 28;	*/
/*effect spl = spline(ga_week / details naturalcubic basis=tpf(noint) knotmethod=percentiles(3)); 		*/
/*model mweight = spl / ddfm=residual solution;		*/
/*random int spl/ type=un subject=momid solution; 		*/
/*ods output ParameterEstimates=sf_1(keep=effect estimate 		*/
/*rename=(estimate=overall));		*/
/*ods output solutionr=sr_1(keep=effect spl Subject estimate 		*/
/*                             rename=(estimate=ssdev));		*/
/*run; */


proc hpmixed data= region_ghana;
class pregid; 	
where ga_week lt 28;	
effect spl = spline(ga_week / details naturalcubic basis=tpf(noint) knotmethod=percentiles(3)); 		
model mweight = spl / ddfm=residual solution;		
random int spl/ type=un subject=pregid solution; 		
ods output ParameterEstimates=sf_1(keep=effect estimate 		
rename=(estimate=overall));		
ods output solutionr=sr_1(keep=effect spl Subject estimate 		
                             rename=(estimate=ssdev));		
run; 
* Lili's question: should I change momid to pregid here? What is mweight?

*preparing to merge datasets with solutions for fixed and random effects into one dataset; 		
data sf;		
	set sf_1;	
	splnum=_n_-1;	
	if effect='Intercept' then Param='Intercept';	
	else if effect='spl' then Param=cats(effect,splnum); 	
	drop effect;	
run;		
		
data sr;		
	set sr_1;	
	if effect='Intercept' then Param='Intercept'; 	
	else if effect='spl' then Param=cats(effect,spl);	
	pregid = Subject; 	
	drop effect;	
run; 		
		
proc sort data=sf; by Param; run;		
proc sort data=sr; by Param; run;		
		
*merging the datasets and calculating subject specific coefficients; 		
data final;		
merge sf sr;		
by Param;		
sscoeff = overall + ssdev; 		
run; 		
		
proc sort data=final; by pregid Param; run; 		
		
*transposing data to put subjects in rows; 		
proc transpose data=final out=splcoeff prefix=_; 		
by pregid;		
id Param;		
var sscoeff; 		
run; 		

data splcoeff_9;
set splcoeff;

drop _name_;

*k_n, k_n_1 k_i2_1 and k_1 should be updated everytime;
k_n = 22;		
k_n_1 = 19;		
k_i2_1 = 15;	
k_1 = 15;	

ga_impute = 9;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);				

weight_imputed_9 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep pregid weight_imputed_9;
label weight_imputed_9 = "Imputed gestational weight at 9 completed weeks";
run;

data splcoeff_14;
set splcoeff;

drop _name_;

k_n = 22;		
k_n_1 = 19;		
k_i2_1 = 15;	
k_1 = 15;

ga_impute = 13.86;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);		

weight_imputed_14 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep pregid weight_imputed_14;
label weight_imputed_14 = "Imputed gestational weight at 13.86 weeks";
run;	

/* Merging the imputed weights */			
proc sort data = splcoeff_9; by pregid; run;
proc sort data = splcoeff_14; by pregid; run;
data gwg.region_ghana_imputation;
merge splcoeff_9 splcoeff_14;				
by pregid;	
keep pregid weight_imputed_9 weight_imputed_14;
run;	


/*************************************************/
/********** India-CMC **********/
/*************************************************/
proc univariate data = region_cmc;		
var ga_week;	
where ga_week lt 28;	
output out = temp pctlpre=P_ pctlpts=25,50,75;		
run;	
proc print data = temp; run;

proc hpmixed data= region_cmc;
class pregid; 	
where ga_week lt 28;	
effect spl = spline(ga_week / details naturalcubic basis=tpf(noint) knotmethod=percentiles(3)); 		
model mweight = spl / ddfm=residual solution;		
random int spl/ type=un subject=pregid solution; 		
ods output ParameterEstimates=sf_1(keep=effect estimate 		
rename=(estimate=overall));		
ods output solutionr=sr_1(keep=effect spl Subject estimate 		
                             rename=(estimate=ssdev));		
run; 


*preparing to merge datasets with solutions for fixed and random effects into one dataset; 		
data sf;		
	set sf_1;	
	splnum=_n_-1;	
	if effect='Intercept' then Param='Intercept';	
	else if effect='spl' then Param=cats(effect,splnum); 	
	drop effect;	
run;		
		
data sr;		
	set sr_1;	
	if effect='Intercept' then Param='Intercept'; 	
	else if effect='spl' then Param=cats(effect,spl);	
	pregid = Subject; 	
	drop effect;	
run; 		
		
proc sort data=sf; by Param; run;		
proc sort data=sr; by Param; run;		
		
*merging the datasets and calculating subject specific coefficients; 		
data final;		
merge sf sr;		
by Param;		
sscoeff = overall + ssdev; 		
run; 		
		
proc sort data=final; by pregid Param; run; 		
		
*transposing data to put subjects in rows; 		
proc transpose data=final out=splcoeff prefix=_; 		
by pregid;		
id Param;		
var sscoeff; 		
run; 		

data splcoeff_9;
set splcoeff;

drop _name_;

k_n = 26;		
k_n_1 = 20;		
k_i2_1 = 14;	
k_1 = 14;	

ga_impute = 9;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);				

weight_imputed_9 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep pregid weight_imputed_9;
label weight_imputed_9 = "Imputed gestational weight at 9 completed weeks";
run;

data splcoeff_14;
set splcoeff;

drop _name_;

k_n = 26;		
k_n_1 = 20;		
k_i2_1 = 14;	
k_1 = 14;	

ga_impute = 13.86;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);		

weight_imputed_14 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep pregid weight_imputed_14;
label weight_imputed_14 = "Imputed gestational weight at 13.86 weeks";
run;	

/* Merging the imputed weights */			
proc sort data = splcoeff_9; by pregid; run;
proc sort data = splcoeff_14; by pregid; run;
data gwg.region_cmc_imputation;
merge splcoeff_9 splcoeff_14;				
by pregid;	
keep pregid weight_imputed_9 weight_imputed_14;
run;	


/*************************************************/
/********** India-SAS **********/
/*************************************************/
proc univariate data = region_sas;		
var ga_week;	
where ga_week lt 28;	
output out = temp pctlpre=P_ pctlpts=25,50,75;		
run;	
proc print data = temp; run;

proc hpmixed data= region_sas;
class pregid; 	
where ga_week lt 28;	
effect spl = spline(ga_week / details naturalcubic basis=tpf(noint) knotmethod=percentiles(3)); 		
model mweight = spl / ddfm=residual solution;		
random int spl/ type=un subject=pregid solution; 		
ods output ParameterEstimates=sf_1(keep=effect estimate 		
rename=(estimate=overall));		
ods output solutionr=sr_1(keep=effect spl Subject estimate 		
                             rename=(estimate=ssdev));		
run; 


*preparing to merge datasets with solutions for fixed and random effects into one dataset; 		
data sf;		
	set sf_1;	
	splnum=_n_-1;	
	if effect='Intercept' then Param='Intercept';	
	else if effect='spl' then Param=cats(effect,splnum); 	
	drop effect;	
run;		
		
data sr;		
	set sr_1;	
	if effect='Intercept' then Param='Intercept'; 	
	else if effect='spl' then Param=cats(effect,spl);	
	pregid = Subject; 	
	drop effect;	
run; 		
		
proc sort data=sf; by Param; run;		
proc sort data=sr; by Param; run;		
		
*merging the datasets and calculating subject specific coefficients; 		
data final;		
merge sf sr;		
by Param;		
sscoeff = overall + ssdev; 		
run; 		
		
proc sort data=final; by pregid Param; run; 		
		
*transposing data to put subjects in rows; 		
proc transpose data=final out=splcoeff prefix=_; 		
by pregid;		
id Param;		
var sscoeff; 		
run; 		

data splcoeff_9;
set splcoeff;

drop _name_;

k_n = 25;		
k_n_1 = 18;		
k_i2_1 = 11;	
k_1 = 11;	

ga_impute = 9;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);				

weight_imputed_9 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep pregid weight_imputed_9;
label weight_imputed_9 = "Imputed gestational weight at 9 completed weeks";
run;

data splcoeff_14;
set splcoeff;

drop _name_;

k_n = 25;		
k_n_1 = 18;		
k_i2_1 = 11;	
k_1 = 11;

ga_impute = 13.86;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);		

weight_imputed_14 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep pregid weight_imputed_14;
label weight_imputed_14 = "Imputed gestational weight at 13.86 weeks";
run;	

/* Merging the imputed weights */			
proc sort data = splcoeff_9; by pregid; run;
proc sort data = splcoeff_14; by pregid; run;
data gwg.region_sa_imputation;
merge splcoeff_9 splcoeff_14;				
by pregid;	
keep pregid weight_imputed_9 weight_imputed_14;
run;	


/*************************************************/
/********** Kenya **********/
/*************************************************/
proc univariate data = region_kenya;		
var ga_week;	
where ga_week lt 28;	
output out = temp pctlpre=P_ pctlpts=25,50,75;		
run;	
proc print data = temp; run;

proc hpmixed data= region_kenya;
class pregid; 	
where ga_week lt 28;	
effect spl = spline(ga_week / details naturalcubic basis=tpf(noint) knotmethod=percentiles(3)); 		
model mweight = spl / ddfm=residual solution;		
random int spl/ type=un subject=pregid solution; 		
ods output ParameterEstimates=sf_1(keep=effect estimate 		
rename=(estimate=overall));		
ods output solutionr=sr_1(keep=effect spl Subject estimate 		
                             rename=(estimate=ssdev));		
run; 


*preparing to merge datasets with solutions for fixed and random effects into one dataset; 		
data sf;		
	set sf_1;	
	splnum=_n_-1;	
	if effect='Intercept' then Param='Intercept';	
	else if effect='spl' then Param=cats(effect,splnum); 	
	drop effect;	
run;		
		
data sr;		
	set sr_1;	
	if effect='Intercept' then Param='Intercept'; 	
	else if effect='spl' then Param=cats(effect,spl);	
	pregid = Subject; 	
	drop effect;	
run; 		
		
proc sort data=sf; by Param; run;		
proc sort data=sr; by Param; run;		
		
*merging the datasets and calculating subject specific coefficients; 		
data final;		
merge sf sr;		
by Param;		
sscoeff = overall + ssdev; 		
run; 		
		
proc sort data=final; by pregid Param; run; 		
		
*transposing data to put subjects in rows; 		
proc transpose data=final out=splcoeff prefix=_; 		
by pregid;		
id Param;		
var sscoeff; 		
run; 		

data splcoeff_9;
set splcoeff;

drop _name_;

k_n = 22;		
k_n_1 = 19;		
k_i2_1 = 15;	
k_1 = 15;	

ga_impute = 9;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);				

weight_imputed_9 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep pregid weight_imputed_9;
label weight_imputed_9 = "Imputed gestational weight at 9 completed weeks";
run;

data splcoeff_14;
set splcoeff;

drop _name_;

k_n = 22;		
k_n_1 = 19;		
k_i2_1 = 15;	
k_1 = 15;	

ga_impute = 13.86;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);		

weight_imputed_14 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep pregid weight_imputed_14;
label weight_imputed_14 = "Imputed gestational weight at 13.86 weeks";
run;	

/* Merging the imputed weights */			
proc sort data = splcoeff_9; by pregid; run;
proc sort data = splcoeff_14; by pregid; run;
data gwg.region_kenya_imputation;
merge splcoeff_9 splcoeff_14;				
by pregid;	
keep pregid weight_imputed_9 weight_imputed_14;
run;	


/*************************************************/
/********** Pakistan **********/
/*************************************************/
proc univariate data = region_pakistan;		
var ga_week;	
where ga_week lt 28;	
output out = temp pctlpre=P_ pctlpts=25,50,75;		
run;	
proc print data = temp; run;

proc hpmixed data= region_pakistan;
class pregid; 	
where ga_week lt 28;	
effect spl = spline(ga_week / details naturalcubic basis=tpf(noint) knotmethod=percentiles(3)); 		
model mweight = spl / ddfm=residual solution;		
random int spl/ type=un subject=pregid solution; 		
ods output ParameterEstimates=sf_1(keep=effect estimate 		
rename=(estimate=overall));		
ods output solutionr=sr_1(keep=effect spl Subject estimate 		
                             rename=(estimate=ssdev));		
run; 


*preparing to merge datasets with solutions for fixed and random effects into one dataset; 		
data sf;		
	set sf_1;	
	splnum=_n_-1;	
	if effect='Intercept' then Param='Intercept';	
	else if effect='spl' then Param=cats(effect,splnum); 	
	drop effect;	
run;		
		
data sr;		
	set sr_1;	
	if effect='Intercept' then Param='Intercept'; 	
	else if effect='spl' then Param=cats(effect,spl);	
	pregid = Subject; 	
	drop effect;	
run; 		
		
proc sort data=sf; by Param; run;		
proc sort data=sr; by Param; run;		
		
*merging the datasets and calculating subject specific coefficients; 		
data final;		
merge sf sr;		
by Param;		
sscoeff = overall + ssdev; 		
run; 		
		
proc sort data=final; by pregid Param; run; 		
		
*transposing data to put subjects in rows; 		
proc transpose data=final out=splcoeff prefix=_; 		
by pregid;		
id Param;		
var sscoeff; 		
run; 		

data splcoeff_9;
set splcoeff;

drop _name_;

k_n = 21;		
k_n_1 = 18;		
k_i2_1 = 13;	
k_1 = 13;	

ga_impute = 9;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);				

weight_imputed_9 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep pregid weight_imputed_9;
label weight_imputed_9 = "Imputed gestational weight at 9 completed weeks";
run;

data splcoeff_14;
set splcoeff;

drop _name_;

k_n = 21;		
k_n_1 = 18;		
k_i2_1 = 13;	
k_1 = 13;
ga_impute = 13.86;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);		

weight_imputed_14 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep pregid weight_imputed_14;
label weight_imputed_14 = "Imputed gestational weight at 13.86 weeks";
run;	

/* Merging the imputed weights */			
proc sort data = splcoeff_9; by pregid; run;
proc sort data = splcoeff_14; by pregid; run;
data gwg.region_pakistan_imputation;
merge splcoeff_9 splcoeff_14;				
by pregid;	
keep pregid weight_imputed_9 weight_imputed_14;
run;	


/*************************************************/
/********** Zambia **********/
/*************************************************/
proc univariate data = region_zambia;		
var ga_week;	
where ga_week lt 28;	
output out = temp pctlpre=P_ pctlpts=25,50,75;		
run;	
proc print data = temp; run;

proc hpmixed data= region_zambia;
class pregid; 	
where ga_week lt 28;	
effect spl = spline(ga_week / details naturalcubic basis=tpf(noint) knotmethod=percentiles(3)); 		
model mweight = spl / ddfm=residual solution;		
random int spl/ type=un subject=pregid solution; 		
ods output ParameterEstimates=sf_1(keep=effect estimate 		
rename=(estimate=overall));		
ods output solutionr=sr_1(keep=effect spl Subject estimate 		
                             rename=(estimate=ssdev));		
run; 


*preparing to merge datasets with solutions for fixed and random effects into one dataset; 		
data sf;		
	set sf_1;	
	splnum=_n_-1;	
	if effect='Intercept' then Param='Intercept';	
	else if effect='spl' then Param=cats(effect,splnum); 	
	drop effect;	
run;		
		
data sr;		
	set sr_1;	
	if effect='Intercept' then Param='Intercept'; 	
	else if effect='spl' then Param=cats(effect,spl);	
	pregid = Subject; 	
	drop effect;	
run; 		
		
proc sort data=sf; by Param; run;		
proc sort data=sr; by Param; run;		
		
*merging the datasets and calculating subject specific coefficients; 		
data final;		
merge sf sr;		
by Param;		
sscoeff = overall + ssdev; 		
run; 		
		
proc sort data=final; by pregid Param; run; 		
		
*transposing data to put subjects in rows; 		
proc transpose data=final out=splcoeff prefix=_; 		
by pregid;		
id Param;		
var sscoeff; 		
run; 		

data splcoeff_9;
set splcoeff;

drop _name_;

k_n = 20;		
k_n_1 = 18;		
k_i2_1 = 14;	
k_1 = 14;	

ga_impute = 9;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);				

weight_imputed_9 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep pregid weight_imputed_9;
label weight_imputed_9 = "Imputed gestational weight at 9 completed weeks";
run;

data splcoeff_14;
set splcoeff;

drop _name_;

k_n = 20;		
k_n_1 = 18;		
k_i2_1 = 14;	
k_1 = 14;

ga_impute = 13.86;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);		

weight_imputed_14 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep pregid weight_imputed_14;
label weight_imputed_14 = "Imputed gestational weight at 13.86 weeks";
run;	

/* Merging the imputed weights */			
proc sort data = splcoeff_9; by pregid; run;
proc sort data = splcoeff_14; by pregid; run;
data gwg.region_zambia_imputation;
merge splcoeff_9 splcoeff_14;				
by pregid;	
keep pregid weight_imputed_9 weight_imputed_14;
run;		


/* Combine the imputed weights */
data region_cmc_imputation;  set gwg.region_cmc_imputation; pregid=Substr(pregid,7);run;
data region_cmc_imputation; length site $ 9 pregid $ 50;  set region_cmc_imputation;site="India-CMC";run;

data region_ghana_imputation; set gwg.region_ghana_imputation; site="Ghana";pregid=Substr(pregid,7);run; 
data region_ghana_imputation; length site $ 9 pregid $ 50;  set region_ghana_imputation;run;

data region_sa_imputation; set gwg.region_sa_imputation; site="India-SAS"; pregid=Substr(pregid,7); run;
data region_sa_imputation; length site $ 9 pregid $ 50;  set region_sa_imputation;site="India-SAS";run;

data region_kenya_imputation;  set gwg.region_kenya_imputation; pregid=Substr(pregid,7); run; 
data region_kenya_imputation;  length site $ 9 pregid $ 50; set region_kenya_imputation;site="Kenya";run;

data region_pakistan_imputation;  set gwg.region_pakistan_imputation; pregid=Substr(pregid,7);run; 
data region_pakistan_imputation;  length site $ 9 pregid $ 50; set region_pakistan_imputation;site="Pakistan";run;

data region_zambia_imputation;  set gwg.region_zambia_imputation;  pregid=Substr(pregid,7);run;
data region_zambia_imputation;  length site $ 9 pregid $ 50; set region_zambia_imputation;site="Zambia";run;


data gwg.imputation_final;
set  region_ghana_imputation region_cmc_imputation region_sa_imputation
    region_kenya_imputation region_pakistan_imputation region_zambia_imputation;
run;

proc export data=gwg.imputation_final
    outfile="D:/Users/xinyili/Documents/Analysis/Gestational Weight Gain/data_out/GWG_ImputeObs_wk9_wk14.csv"
    dbms=csv replace;
    putnames=yes;  /* Includes column headers */
run;

/*proc sort data=gwg.imputation_final; by site momid; run;*/
/*proc sort data=gwg.analysis; by site momid; run;*/
/*data gwg.analysis; length site $ 9 id $ 40;set gwg.analysis;  run;*/
/*data gwg.all;   merge gwg.analysis gwg.imputation_final; by site momid; run;*/
/**/
/*data temp; set gwg.all; where weight_imputed_9 = . | weight_imputed_14 = . ; run;*/
