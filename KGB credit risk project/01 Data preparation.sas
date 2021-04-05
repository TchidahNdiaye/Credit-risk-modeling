**************** 01 Preparing the Data for Credit risk modeling********;

**************** 1.1 SAS Programming to Explore Your Data******;
libname mydata "C:\ECBB4C13\Data";

proc univariate data=mydata.applicants;
   var age;
run;

proc univariate data=mydata.applicants;
   var age;
   histogram;
run;

proc univariate data=mydata.applicants;
   var age;
   class good_bad;
   histogram;
run;

proc gchart data=mydata.applicants;
   pie purpose;
run;

proc gplot data=mydata.applicants;
   plot age * amount;
run;


**************** 3.2 Impute Missing Values ************************;

proc standard data=credit replace out=creditnomissing;
run;

**************** 3.3 Box Plot to Detect and Treat Outliers *********;

proc univariate data=mydata.applicants plot;
   var age;
run;

************** 3.4.  Programming to Standardize Data **************;

proc standard data=credit mean=0 std=1 out=creditstand;
run;

proc means data=creditstand;
run;

proc freq data=coarse1;
   weight count;
   tables default*resstatus / chisq;
run;

proc freq data=coarse2;
   weight count;
   tables default*resstatus / chisq;
run;



*Data visualization in SAS;
libname mydata "/home/u44791576/credit_risk/Data";

data applicants;
	set mydata.applicants;
run;

proc print data=applicants (obs=20);
	var good_bad age amount duration checking savings;
run;


********************DATA SET EXPLORATION********************;

proc univariate data=applicants;
	var age;
run;

proc univariate data=applicants;
	var age;
	histogram;
run;

proc univariate data=applicants;
	class good_bad;
	var age;
	histogram;
run;

proc gchart data=applicants;
	pie purpose;
run;

proc gplot data=applicants;
	plot age * amount;
run;



*Missing value imputation;



proc standard data=credit replace out=creditnomissing;
run;
