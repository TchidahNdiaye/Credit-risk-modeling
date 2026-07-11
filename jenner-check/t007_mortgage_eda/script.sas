************ 1. EXPLORATORY DATA ANALYSIS FOR CREDIT SCORING ********************;

proc contents data=mortgage;
run;

*********** 1.1. Analyse unidimentionnelle ************************;
proc freq data=mortgage;
	tables default_time;
	title1 "Frequences relative et absolue du defaut";
	title2 "(0 : Pas de defaut et 1 : Client en defaut de paiement)";
run;

proc means data=mortgage n nmiss mean std min max;
	var gdp_time;
run;

ods graphics on;
proc univariate data=mortgage;
	var fico_orig_time ltv_orig_time;
	cdfplot fico_orig_time;
	cdfplot ltv_orig_time;
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
