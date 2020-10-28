libname mydata "/home/u44791576/credit_risk/Data";
data applicants;
	set mydata.applicants;
run;	

ods excel file="/home/u44791576/thesis/Applicants_VarSelection.xlsx" 
          options(sheet_interval="proc"
          embedded_titles="yes");

*********************CHI-SQUARE ANALYSIS TO FILTER VARIABLES************;

proc freq data=applicants;
	table marital*good_bad / chisq;
run;

*******************STEPWISE LOGISTIC REG. TO FILTER VARIABLES************;

proc logistic data=work.applicants;
	class  checking history purpose savings age
		   employed marital coapp resident
		   other housing property / param=glm;
	model good_bad = checking duration history 
					 purpose amount savings 
					 employed installp marital 
					 coapp resident property other
					 / selection=stepwise slentry=0.10 slstay=0.05;
run;



proc logistic data=mydata.applicants;
   class checking history purpose savings 
         employed marital coapp
         resident property age 
         other housing / param=glm;
   model good_bad=checking duration history 
         purpose amount savings
         employed installp marital 
         coapp resident property other 
        / selection=stepwise slentry=0.10 slstay=0.05;
run;


**************************** LOGISTIC REGRESSION **********************************;

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

ods excel close;