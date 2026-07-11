*********************CHI-SQUARE ANALYSIS TO FILTER VARIABLES************;

proc freq data=applicants;
	table marital*good_bad / chisq;
run;

*******************STEPWISE LOGISTIC REG. TO FILTER VARIABLES************;

proc logistic data=applicants;
	class  checking history purpose savings age
		   employed marital coapp resident
		   other housing property / param=glm;
	model good_bad = checking duration history
					 purpose amount savings
					 employed installp marital
					 coapp resident property other
					 / selection=stepwise slentry=0.10 slstay=0.05;
run;
