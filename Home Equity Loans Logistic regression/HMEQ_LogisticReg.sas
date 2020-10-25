libname mydata "/home/u44791576/credit_risk/Data";

/* Create the Data set in work library. */

data hmeq;
	set mydata.hmeq;
run;

ods excel file="/home/u44791576/thesis/HMEQ_LogisticReg.xlsx" 
          options(sheet_interval="proc"
          embedded_titles="yes");
          
proc contents data=hmeq; run;

/* Hmeq dataset exploration */

proc means data=hmeq n nmiss mean median std min max; run;

proc freq data=hmeq;
	tables BAD REASON JOB;
run;

proc univariate data=hmeq;
	var YOJ;
	class BAD;
	histogram;
run;

proc gchart data=hmeq;
	pie REASON;
run;	

proc gplot data=hmeq;
	plot VALUE * LOAN;
run;

/* Credit scoring modeling with logistic regression */

proc logistic data=hmeq;
	class BAD REASON JOB / param=glm;
	model BAD =  CLAGE  CLNO DEBTINC DELINQ DEROG JOB 
				 LOAN MORTDUE NINQ REASON VALUE YOJ /
	selection=stepwise slentry=0.05 slstay=0.01;
run;

proc logistic data=hmeq plots=all;
	class BAD REASON JOB / param=glm;
	model BAD =  CLAGE  CLNO DEBTINC DELINQ DEROG JOB 
				 LOAN MORTDUE NINQ REASON VALUE YOJ /
	selection=stepwise slentry=0.05 slstay=0.01;
run;

proc logistic data=hmeq plots=all;
	class BAD REASON JOB / param=glm;
	model BAD(event="1") =  CLAGE  CLNO DEBTINC DELINQ DEROG JOB 
				 LOAN MORTDUE NINQ REASON VALUE YOJ /
	selection=stepwise slentry=0.05 slstay=0.01;
run;
ods excel close;
ods proctitle;