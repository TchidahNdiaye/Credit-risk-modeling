******************* CLASSIFICATION FOR CREDIT SCORING*************;

******************* 1. LOGISTIC REGRESSION ******************;

proc logistic data=mydata_applicants;
   class checking savings/param=glm;
   model good_bad=age amount duration checking savings/ctable;
run;

proc logistic data=mydata_applicants;
   class checking savings/param=glm;
   model good_bad=age amount duration checking savings/link=probit ctable;
run;

proc logistic data=mydata_applicants;
   class checking savings/param=glm;
   model good_bad=age amount duration checking savings/link=cloglog ctable;
run;
