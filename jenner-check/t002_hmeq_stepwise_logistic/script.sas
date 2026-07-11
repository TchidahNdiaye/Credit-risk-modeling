proc contents data=hmeq; run;

/* Hmeq dataset exploration */

proc means data=hmeq n nmiss mean median std min max; run;

proc freq data=hmeq;
	tables BAD REASON JOB;
run;

/* Credit scoring modeling with logistic regression */

proc logistic data=hmeq;
	class BAD REASON JOB / param=glm;
	model BAD =  CLAGE  CLNO DEBTINC DELINQ DEROG JOB
				 LOAN MORTDUE NINQ REASON VALUE YOJ /
	selection=stepwise slentry=0.05 slstay=0.01;
run;
