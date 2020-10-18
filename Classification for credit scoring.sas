******************* CLASSIFICATION FOR CREDIT SCORING*************;

******************* 1. LOGISTIC REGRESSION ******************;

proc logistic data=mydata.applicants;
   class checking savings/param=glm;
   model good_bad=age amount duration checking savings/ctable;
run;

proc logistic data=mydata.applicants;
   class checking savings/param=glm;
   model good_bad=age amount duration checking savings/link=probit ctable;
run;

proc logistic data=mydata.applicants;
   class checking savings/param=glm;
   model good_bad=age amount duration checking savings/link=cloglog ctable;
run;


************** DECISION TREE WITH SAS STUDIO *********************;
***************DESCRIPTION DE LA BASE DE DONNEES*********************;
data applicants;
	set mydata.applicants;
run;

proc contents data=applicants;
run;

proc freq data=applicants;
	tables good_bad;
run;

*************TRAINING & VALIDATION DATA SET CREATION*****************;
proc sort data=applicants out=applicants_sort;
	by good_bad;
run;

proc surveyselect noprint data=applicants_sort
				  samprate=0.70 out=applicants_sample
				  seed=4444 outall;
	strata good_bad;
run;

data train(drop=selected SelectionProb  SamplingWeight)
	 valid(drop=selected SelectionProb  SamplingWeight);
	set applicants_sample;
	if selected then output train;
	else output valid;
run;

*************************DECISION TREES*****************************;
ods graphics on;
proc hpsplit data=train;
	class good_bad checking savings;
	model good_bad=age amount duration checking savings;
	grow entropy;
	prune costcomplexity;
run;



****************** 2. Cumulative Logistic Regression**********;

data bondrate2;
   set bondrate;
   if rating='AAA' then rating=1;
   if rating='AA' then rating=2;
   if rating='A' then rating=3;
   if rating='BAA' then rating=4;
   if rating='BA' then rating=5;
   if rating='B' then rating=6;
   if rating='C' then rating=7;
run;

proc logistic data=bondrate2;
   model rating=  LOPMAR     LFIXCHAR     LGEARRAT     LTDCAP     LLEVER  
                  LCASHRAT     LACIDRAT     LCURRAT LRECTURN LASSLTD;
   output out=bondpreds predprobs=individual;
run;


proc contents data=mydata.ratings;
run;

data ratings;
	set mydata.ratings;
run;

proc means data=ratings n nmiss mean std min max;
run;


proc corr data=ratings plots=matrix(histogram);
run;

*CUMULATIVE LOGISTIC REGRESSION;
proc logistic data=ratings;
	model rating = spid COMMEQTA LLPLOANS 
				   COSTTOINCOME ROE LIQASSTA SIZE;
	output out=ratingpreds predprobs=individual;
run;



************CLASSIFICATION PERFORMANCE STATISTICS*************;

proc sql noprint;
	select count(*) into :goods from perf where actual_gb = "g";
	select count(*) into :bads from perf where actual_gb = "b";
quit;

%let all = %eval(&goods + &bads);

data discrimstat;
	set perf;
	retain g b 0;
	if actual_gb = "b" then b+1;
	else g + 1;
	sens = (&goods - g) / &goods;
	spec = (b / &bads);
	neg_spec = 1 - spec;
	neg_sens = 1 - sens;
	KS = spec - neg_sens;
	percentile = _N_ / 25;
run;

goptions reset = all hsize=14cm vsize=10cm;
symbol i = join ci = red v = dot cv = blue;
proc gplot data=discrimstat;
	plot sens * neg_spec;
	label sens = "Sensitivity" neg_spec = "1 - Specificity";
	title "ROC plot";
run;
quit;

symbol1 i = join ci = red v = dot cv = blue;
symbol2 i = joint ci = green v = dot cv = blue;
proc gplot data=discrimstat;
	plot neg_sens *score spec*score / overlay;
	title "Kolmogorov Smirov Plot";
run;
quit;

symbol i = joint ci = red v = dot cv = blue;
proc gplot data=discrimstat;
	plot spec*percentile;
	title "CAP plot";
run;
quit;
