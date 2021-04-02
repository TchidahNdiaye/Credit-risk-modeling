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

*We create a small dataset;
data credit;
	input income savings;
	datalines;
1300 400
1000 .
2000 800
. 200
1700 600
2500 1000
2200 900
. 1000
1500 .
;
run;

proc standard data=credit replace out=creditnomissing;
run;

*We use replace in proc standard to replace the missing values
by the mean of the variable, And the result of the proc will
be stored in a new data set call creditnomissing;

*********************STANDARDIZING DATA************************;
*SAS Database creation;
data credit;
	input income saving;
	datalines;
1300 400
1000 700
2000 800
1800 200
1700 600
2500 1000
2200 900
1800 1000
1500 700
;
run;

proc standard data=credit mean=0 std=1 out=creditstand;
run;

proc means data=creditstand;
run;

*********************COARSE CLASSIFICATION************************;
*SAS Database creation for this demo;

data residence;
	input default$ resstatus$ count;
	datalines;
good owner 6000
good rentunf 1600
good rentfurn 350
good withpar 950
good other 90
good noanswer 10
bad owner 300
bad rentunf 400
bad rentfurn 140
bad withpar 100
bad other 50
bad noanswer 10
;

data coarse1;
	input default$ resstatus$ count;
	datalines;
good owner 6000
good renter 1950
good other 1050
bad owner 300
bad renter 540
bad other 160
;

data coarse2;
	input default$ resstatus$ count;
	datalines;
good owner 6000
good withpar 950
good other 2050
bad owner 300
bad withpar 100
bad other 600
;

proc freq data=coarse1;
	weight count;
	tables default*resstatus / chisq;
run;

proc freq data=coarse2;
	weight count;
	tables default*resstatus / chisq;
run;

***********************************************************;
proc freq data=residence;
	weight count;
	tables default * resstatus / chisq;
run;

proc freq data=coarse1;
	table resstatus;
run;

proc freq data=coarse2;
	table resstatus;
run;
