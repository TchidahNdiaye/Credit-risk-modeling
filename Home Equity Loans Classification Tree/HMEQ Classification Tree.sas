proc import datafile="/home/u44791576/credit_risk/Data/hmeq.csv"
					out=hmeq
					dbms=csv replace;

ods excel file="/home/u44791576/thesis/HMEQ_DTree.xlsx" 
          options(sheet_interval="proc"
          embedded_titles="yes");

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
	partition fraction(valid=.3 seed=12345);
	code file='/home/u44791576/thesis/hmeq_score_entropy.sas';
    rules file='/home/u44791576/thesis/hmeq_rules_entropy.txt';
	title "Decision tree without missing values";
run;

proc hpsplit data=hmeq seed=12345 nodes;
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE  CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow gini;
	prune costcomplexity;
	partition fraction(valid=.3 seed=12345);
	code file='/home/u44791576/thesis/hmeq_score_gini.sas';
    rules file='/home/u44791576/thesis/hmeq_rules_gini.txt';
	title "Decision tree with Gini criterion";
run;

proc hpsplit data=hmeq seed=12345 nodes;
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE  CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow chaid;
	prune costcomplexity;
	partition fraction(valid=.3 seed=12345);
	code file='/home/u44791576/thesis/hmeq_score_chaid.sas';
    rules file='/home/u44791576/thesis/hmeq_rules_chaid.txt';
	title "Decision tree with CHAID criterion";
run;

proc hpsplit data=hmeq seed=12345 nodes;
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE  CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow chisquare;
	prune costcomplexity;
	partition fraction(valid=.3 seed=12345);
	code file='/home/u44791576/thesis/hmeq_score_chisquare.sas';
    rules file='/home/u44791576/thesis/hmeq_rules_chisquare.txt';
	title "Decision tree with CHISQUARE criterion";
run;

proc hpsplit data=hmeq seed=12345 nodes;
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE  CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow fastchaid;
	prune costcomplexity;
	partition fraction(valid=.3 seed=12345);
	code file='/home/u44791576/thesis/hmeq_score_fastchaid.sas';
    rules file='/home/u44791576/thesis/hmeq_rules_fastchaid.txt';
	title "Decision tree with FASTCHAID criterion";
run;



proc hpsplit data=hmeq seed=12345 nodes;
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE  CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow entropy;
	prune costcomplexity;
	partition fraction(valid=.2 test=.2 seed=12345);
	title "Decision tree with partition of 60% in Training sample,
			20% in test sample and 20% in validation sample";
run;

proc hpsplit data=hmeq seed=12345 nodes assignmissing=similar;
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE  CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow entropy;
	prune costcomplexity;
	partition fraction(valid=.2 test=.2 seed=12345);
	title1 "Decision tree with missing values";
	title2 "With a partition of 60% in Training sample,
			20% in test sample and 20% in validation sample";
run;

proc hpsplit data=hmeq seed=12345 nodes assignmissing=similar;
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow chisquare;
	prune costcomplexity;
	partition fraction(valid=.3 seed=12345);
	title1 "Decision tree with a partition of 70% of data in
			training sample and 30% in validation sample";
	title2 "Tree with all values";
run;


proc hpsplit data=hmeq seed=12345 nodes assignmissing=similar
						plots=zoomedtree(nodes=('3') depth=4);
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow chisquare;
	prune costcomplexity;
	partition fraction(valid=.3 seed=12345);
	title "View the subtree starting at Node 3";
run;

proc hpsplit data=hmeq seed=12345 nodes assignmissing=similar
						plots=zoomedtree(nodes=('4') depth=3);
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow chisquare;
	prune costcomplexity;
	partition fraction(valid=.3 seed=12345);
	title "View the subtree starting at Node 4";
run;


proc hpsplit data=hmeq seed=12345 nodes assignmissing=similar
						plots=zoomedtree(nodes=('5') depth=3);
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow chisquare;
	prune costcomplexity;
	partition fraction(valid=.3 seed=12345);
	title "View the subtree starting at Node 5";
run;


proc hpsplit data=hmeq seed=12345 nodes assignmissing=similar
						plots=zoomedtree(nodes=('J') depth=6);
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow chisquare;
	prune costcomplexity;
	partition fraction(valid=.3 seed=12345);
	title "View the subtree starting at Node J";
run;

proc hpsplit data=hmeq seed=12345 nodes assignmissing=similar
						plots=zoomedtree(nodes=('C') depth=4);
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow chisquare;
	prune costcomplexity;
	partition fraction(valid=.3 seed=12345);
	title "View the subtree starting at Node C";
run;


proc hpsplit data=hmeq seed=12345 nodes assignmissing=similar
						plots=zoomedtree(nodes=('6') depth=5);
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow chisquare;
	prune costcomplexity;
	partition fraction(valid=.3 seed=12345);
	title "View the subtree starting at Node 6";
run;


proc hpsplit data=hmeq seed=12345 nodes maxdepth=7 cvmodelfit;
	class  BAD  JOB  REASON;
	model BAD(event="1") =  CLAGE CLNO DEBTINC DELINQ DEROG 
				 			JOB LOAN MORTDUE NINQ REASON VALUE YOJ;
	grow chisquare;
	prune costcomplexity;
	title "Decision tree with cross validation on the training sample";
run;
ods excel close;
ods proctitle;
title;footnote;
















*********************************************************************;
*********************************************************************;
ods graphics on;												*****;
proc hpsplit data=hmeqtest seed=12345 nodes;					*****;
	class  BAD  JOB  REASON;									*****;
	model BAD =  CLAGE  CLNO DEBTINC DELINQ DEROG 				*****;
				 JOB LOAN MORTDUE NINQ REASON VALUE YOJ;		*****;
	grow entropy;												*****;
	prune costcomplexity;										*****;
run;															*****;
*********************************************************************;
*********************************************************************;