********* 8. LGD MODELING *************;

********* 8.1. Beta Linear Regression ***************;

proc import out= work.lgddata
            datafile= "/home/u44791576/credit_risk/Data/LGD.txt"
            dbms=tab replace;
   getnames=yes;
   datarow=2;
run;

ods excel file="/home/u44791576/thesis/Applicants_LGDModeling.xlsx" 
          options(sheet_interval="proc"
          embedded_titles="yes");

proc univariate data=LGDdata;
   var LGD;
   histogram;
run;

data training holdout;
   set lgddata;
   if 1 < = _N_ < = 337 then output training;
   else output holdout;
run;

proc reg data=training outest=outests;
   LGDOLS: model LGD= ratio1-ratio13;
run;

proc score data=holdout score=outests out=preds type=parms;
   var ratio1-ratio13;
run;

proc corr data=preds;
   var LGD LGDOLS;
run;

data MSEtemp;
   set preds;
   MSEterm=(LGD-LGDOLS)**2;
run;

proc means data=MSEtemp;
   var MSEterm;
run;

proc means data=training;
   var LGD;
   output out=meanstats mean=mu var=sigmasq;
run;

data betaparams;
   set meanstats;
   alpha=(mu*mu*(1-mu)/sigmasq)-mu;
   beta=alpha*(1/mu -1);
run;

data transformedtraining;
   if _N_=1 then set betaparams;
   set training;
   newLGD=probit(cdf('BETA',LGD,alpha,beta));
run;

proc univariate data=transformedtraining;
   var newLGD;
   histogram;
run;

proc reg data=transformedtraining outest=outests2;
   LGDBETA: model newLGD= ratio1-ratio13;
run;

proc score data=holdout score=outests2 out=transpreds type=parms;
   var ratio1-ratio13;
run;

data preds2;
   if _N_=1 then set betaparams;
   set transpreds;
   LGDpred=betainv(cdf('NORMAL',LGDBETA),alpha,beta);
run;

proc corr data=preds2;
   var LGD LGDpred;
run;

data MSEtemp2;
   set preds2;
   MSEterm=(LGD-LGDpred)**2;
run;

proc means data=MSEtemp2;
   var MSEterm;
run;

*********************************************************************************;


proc univariate data=lgddata;
	var lgd;
	histogram;
run;

data training holdout;
	set lgddata;
	if 1 < _N_ < = 337 then output training;
	else output holdout;
run;

proc reg data=training outest=outests;
	LGDOLS: model lgd = ratio1-ratio13;
run;

proc score data=holdout score=outests out=preds type=parms;
	var ratio1-ratio13;
run;

proc corr data=preds;
	var LGD LGDOLS;
run;

data MSEtemp;
	set preds;
	MSEterm=(LGD-LGDOLS)**2;
run;

proc means data=MSEtemp;
	var MSEterm;
run;

proc means data=training;
	var lgd;
	output out=meanstats mean=mu var=sigmasq;
run;

data betaparams;
	set meanstats;
	alpha=(mu*mu*(1-mu)/sigmasq)-mu;
	beta=alpha* (1/mu -1);
run;

data transformetraining;
	if _N_=1 then set betaparams;
	set training;
	newLGD=probit (cdf('BETA',LGD,alpha,beta));
run;

proc univariate data=transformetraining;
	var newLGD;
	histogram;
run;

proc reg data=transformetraining outest=outests2;
	LGDBETA: model newLGD= ratio1-ratio13;
run;

proc score data=holdout score=outests2 out=transpreds type=parms;
	var ratio1-ratio13;
run;

data preds2;
	if _N_=1 then set betaparams;
	set transpreds;
	LGDpred=betainv (cdf('NORMAL',LGDBETA),alpha,beta);
run;

proc corr data=preds2;
	var LGD LGDpred;
run;

data MSEtemp2;
	set preds2;
	MSEterm= (LGD-LGDpred)**2;
run;

proc means data=MSEtemp2;
	var MSEterm;
run;
ods excel close;