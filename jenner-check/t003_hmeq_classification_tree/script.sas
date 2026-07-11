proc contents data=hmeq; run;
proc means data=hmeq n nmiss mean std min max; run;

proc freq data=hmeq;
	tables BAD JOB REASON;
	title "Categorical freq in data set";
run;

ods graphics on;
proc hpsplit data=hmeq seed=12345 nodes;
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE  CLNO DEBTINC DELINQ DEROG
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow entropy;
	prune costcomplexity;
	title "Decision tree without missing values";
run;

proc hpsplit data=hmeq seed=12345 nodes;
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE  CLNO DEBTINC DELINQ DEROG
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow gini;
	prune costcomplexity;
	title "Decision tree with Gini criterion";
run;
