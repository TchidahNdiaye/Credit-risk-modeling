libname mydata "D:\Documents\My SAS Files\Credit Risk Modeling\00 creditriskmodeling Data";

proc contents data=mydata.hmeq;
run;

data hmeq;
	set mydata.hmeq;
run;

proc freq data=hmeq;
	tables bad reason job;
run;

************ 1. EXPLORATORY DATA ANALYSIS FOR CREDIT SCORING ********************;

data mortgage;
	set mydata.mortgage;
run;

proc contents data=mortgage;
run;

*********** 1.1. Analyse unidimentionnelle ************************;
proc freq data=mortgage;
	tables default_time;
	title1 "Fréquences relative et absolue du défaut"; 
	title2 "(0 : Pas de défaut et 1 : Client en défaut de paiement)";
run;

proc means data=mortgage n nmiss mean std min max;
	var gdp_time;
run;

ods graphics on;
proc univariate data=mortgage;
	var fico_orig_time ltv_orig_time;
	cdfplot fico_orig_time ltv_orig_time;
	histogram fico_orig_time ltv_orig_time;
	title "La fonction de distribution cummulative
			des variables FICO & LTV";
run;
ods graphics off;

proc means data=mortgage
		n mean median mode p1 p99 maxdec=4;
	var default_time fico_orig_time ltv_orig_time;
	title "Mesures de la location";
run;

ods graphics on;
proc univariate data=mortgage noprint;
	qqplot fico_orig_time ltv_orig_time
		   / Normal(mu=est sigma=est color=ltgrey);
	title "Mesure de la dispersion des variables FICO & LTV";
run;
ods graphics off;

proc means data=mortgage
		n min max range qrange var std cv maxdec=4;
	var default_time fico_orig_time ltv_orig_time;
	title "Mesures de la dispersion de nos variables";
run;

proc means data=mortgage
		n skew kurt maxdec=4;
	var default_time fico_orig_time ltv_orig_time;
	title "Mesures du Skewness et du Kurtosis";
run;


*************** 1.2. Analyse bidimentionnelle ************;

data mortgage1;
	set mortgage;
run;

proc sort data=mortgage1;
	by id time;
run;

*	proc rank permet de regrouper les obsersations en cinq groupes afin de constituer 
	les distributions conditionnelles*;
proc rank data=mortgage1 groups=5
			   out=quint(keep=id time fico_orig_time);
	var fico_orig_time;
run;

data new;
	merge mortgage1 quint;
	by id time;
run;

proc freq data=new;
	tables default_time*fico_orig_time;
	title1 "Analyse bidimentionnelle";
	title2 "Tableau de contingence";
run;

*******************************************************************************
*******************************************************************************
* Contruction d'un tableau de contingence pour trois groupes chez fico 		***
proc rank data=mortgage1 groups=3											***
*			   out=qui2(keep=id time fico_orig_time);						***
*	var fico_orig_time;														***
*run;																		***
*																			***
*data new2;																	***
*	merge mortgage1 qui2;													***
*	by id time;																***
*run;																		***
*																			***
*proc freq data=new2;														***
*	tables default_time*fico_orig_time;										***
*	title "Tableau de contingence pour trois groupes";						***
*run; 																		***
*******************************************************************************
*******************************************************************************



