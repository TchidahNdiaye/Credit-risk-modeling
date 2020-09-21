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
data bondrate;
   input
   OBS     RATING $    LOPMAR     LFIXCHAR     LGEARRAT     LTDCAP     LLEVER    LCASHRAT     LACIDRAT     LCURRAT LRECTURN LASSLTD;
   lines;
1     AAA     -1.663     0.749     -0.491     0.378     0.160     -1.225     0.433     1.120     1.629     1.277
2     AAA     -2.382     0.814     0.147     0.534     1.188     -1.552     -1.008     0.553     2.415     1.357
3     AAA     -1.401     2.561     -1.797     0.142     -0.531     0.496     0.314     1.014     1.728     2.273
4     AAA     -2.040     2.514     -1.528     0.178     -0.325     0.019     0.149     0.773     2.612     2.070
5     AAA     -1.360     2.432     -1.118     0.246     -0.085     -0.083     0.033     0.344     1.854     1.772
6     AAA     -1.687     2.891     -1.637     0.162     0.025     0.183     -0.051     0.328     2.197     2.361
7     AAA     -1.694     0.499     0.054     0.513     0.474     -1.539     0.745     0.897     1.949     0.907
8     AAA     -1.323     0.998     -0.936     0.281     -0.042     -0.187     0.001     0.863     1.349     1.704
9     AAA     -2.100     1.516     -1.654     0.159     0.251     0.342     -0.077     0.347     1.762     2.515
10     AAA     -1.888     2.484     -1.015     0.265     -0.099     -0.393     0.274     0.926     1.727     1.662
11     AAA     -1.633     1.589     -0.966     0.275     -0.105     -0.724     -0.287     0.204     2.128     1.610
12     AA     -2.041     2.636     -1.714     0.152     -0.240     0.537     0.393     0.634     1.911     2.296
13     AA     -2.434     2.193     -0.779     0.314     0.113     -0.798     -0.215     0.686     2.324     1.592
14     AA     -2.473     1.155     -0.925     0.283     -0.177     -0.732     0.074     0.737     2.257     1.541
15     AA     -2.632     2.342     -1.387     0.199     -0.182     -0.233     -0.189     0.824     2.728     2.022
16     AA     -2.285     1.767     -1.493     0.183     -0.049     -0.154     -0.321     0.642     2.335     2.222
17     AA     -2.363     1.807     -0.770     0.316     0.007     -0.707     -0.072     0.644     2.778     1.498
18     AA     -2.108     1.783     -0.829     0.303     -0.004     -0.414     0.030     0.494     2.227     1.540
19     AA     -2.064     1.742     -0.598     0.354     0.089     -0.837     0.011     0.628     3.252     1.337
20     AA     -1.884     2.291     -1.339     0.206     -0.235     -0.102     -0.164     0.525     2.182     1.941
21     AA     -1.740     1.606     -0.832     0.303     -0.147     -0.636     0.502     0.953     1.637     1.466
22     AA     -1.684     1.354     -1.368     0.202     -0.364     0.137     0.205     0.883     1.886     1.912
23     AA     -1.743     1.626     -1.207     0.230     -0.066     -0.266     -0.229     0.543     1.718     1.917
24     AA     -1.776     1.153     -0.450     0.389     0.171     -0.898     -0.073     0.440     2.227     1.251
25     AA     -2.371     2.451     -1.491     0.183     -0.024     -0.010     0.040     0.268     1.758     2.173
26     AA     -1.545     2.560     -0.820     0.302     0.306     -0.334     0.135     0.395     2.016     1.693
27     A     -1.720     1.239     -0.586     0.357     0.151     -0.935     0.110     0.876     1.758     1.377
28     A     -2.429     2.254     -1.221     0.226     -0.240     -0.297     0.061     0.751     2.325     1.812
29     A     -2.841     1.600     -1.235     0.225     -0.116     -0.413     0.070     0.932     2.724     1.915
30     A     -2.114     1.587     -0.833     0.302     -0.138     -0.776     -0.248     0.552     2.365     1.470
31     A     -1.416     1.353     -0.287     0.428     0.379     -0.750     -0.098     0.402     1.504     1.235
32     A     -1.466     1.679     -0.589     0.356     0.372     -0.487     -0.054     0.291     1.643     1.490
33     A     -1.850     1.490     -0.540     0.364     0.287     -0.546     0.156     0.619     1.899     1.40
34     A     -2.499     2.004     -0.492     0.378     0.525     -0.450     0.035     0.412     2.069     1.48
35     A     -2.388     1.248     -0.390     0.403     0.635     -0.664     -0.389     0.185     2.712     1.46
36     A     -2.014     1.736     -1.429     0.193     -0.417     -0.130     0.375     0.866     1.968     1.95
37     A     -1.704     3.691     -3.155     0.040     -0.936     1.573     0.122     0.998     2.033     3.49
38     A     -1.774     0.887     -0.532     0.369     0.013     -0.929     0.070     0.781     1.891     1.23
39     A     -2.219     1.776     -1.760     0.146     0.099     0.231     -0.003     0.818     1.801     2.53
40     A     -1.999     1.580     -1.059     0.257     0.122     -1.487     0.328     0.691     0.897     1.86
41     BAA     -3.323     1.021     -0.912     0.286     -0.049     -0.863     0.110     0.934     2.827     1.67
42     BAA     -2.147     1.373     -0.861     0.297     -0.233     -0.803     0.508     1.087     1.706     1.45
43     BAA     -1.844     2.238     -1.391     0.199     -0.450     -0.171     0.239     1.006     1.662     1.89
44     BAA     -2.145     1.834     -1.857     0.134     -0.300     0.219     0.182     0.808     1.675     2.42
45     BAA     -2.443     0.505     -0.622     0.348     0.136     -1.243     0.154     0.828     1.860     1.40
46     BAA     -2.195     1.546     -1.122     0.244     -0.057     -0.492     0.038     0.583     1.833     1.79
47     BAA     -2.353     0.816     -0.884     0.292     -0.047     -0.791     -0.034     0.716     2.086     1.56
48     BAA     -2.296     1.283     -0.695     0.332     -0.020     -0.984     -0.083     0.588     2.281     1.38
49     BAA     -2.403     1.597     -1.130     0.243     0.441     -0.148     -0.730     0.499     2.545     2.09
50     BAA     -2.194     1.601     -0.790     0.311     0.272     -0.675     -0.224     0.612     1.923     1.64
51     BAA     -1.288     1.727     -0.734     0.324     0.177     -0.502     -0.048     0.957     1.575     1.54
52     BAA     -2.163     1.097     -1.099     0.249     -0.101     -0.453     -0.709     0.787     3.011     1.76
53     BAA     -1.987     0.528     -1.059     0.257     -0.049     -0.531     -0.225     0.476     1.433     1.74
54     BAA     -1.494     2.986     -1.429     0.193     -0.316     -0.007     0.539     1.074     1.351     1.97
55     BAA     -1.308     0.526     0.427     0.605     0.898     -1.990     -0.244     0.230     2.385     0.84
56     BA     -1.595     0.936     -0.620     0.349     0.130     -1.301     -0.315     0.784     1.625     1.41
57     BA     -2.024     2.363     -1.897     0.130     -0.318     0.506     0.317     0.662     1.992     2.45
58     BA     -1.282     1.293     -0.169     0.457     0.398     -0.980     0.563     0.770     1.557     1.08
59     BA     -1.629     2.101     -0.418     0.395     0.703     -0.743     -0.084     0.233     2.047     1.53
60     BA     -2.297     2.629     -1.387     0.199     -0.452     -0.330     0.678     1.243     1.738     1.88
61     BA     -2.486     1.719     -0.739     0.323     -0.154     -1.383     0.590     1.224     1.556     1.35
62     BA     -1.674     2.229     -0.605     0.353     0.355     -0.539     -0.713     0.299     2.438     1.49
63     BA     -2.205     1.385     -0.744     0.322     -0.077     -1.057     0.306     0.884     1.883     1.41
64     BA     -2.451     1.290     -0.330     0.418     0.345     -1.252     0.204     0.730     1.898     1.21
65     BA     -1.552     2.089     -0.405     0.399     0.176     -0.760     0.216     0.759     1.935     1.202
66     BA     -2.281     1.055     -0.452     0.388     0.128     -1.138     0.004     0.830     2.295     1.233
67     BA     -1.901     2.523     -1.187     0.233     -0.244     -0.166     0.022     0.726     2.083     1.767
68     BA     -2.387     0.583     -0.901     0.288     -0.024     -0.952     -0.006     0.569     2.303     1.581
69     BA     -2.201     0.659     -0.136     0.465     0.396     -1.583     -0.005     0.899     2.009     1.052
70     BA     -1.936     1.642     -0.310     0.422     0.451     -1.068     0.156     0.809     1.177     1.290
71     B     -0.661     0.177     0.101     0.525     0.638     -1.473     -0.205     0.189     0.586     0.960
72     B     -0.486     1.345     0.480     0.617     0.733     -1.242     0.683     1.186     0.900     0.727
73     B     -3.399     0.518     -0.149     0.462     0.096     -1.709     -0.009     0.756     3.025     0.925
74     B     -2.475     0.847     -0.480     0.382     0.189     -1.892     0.265     0.960     1.092     1.409
75     B     -2.116     1.390     -0.218     0.445     0.499     -0.983     0.233     0.649     1.735     1.253
76     B     -2.102     0.705     0.328     0.581     0.394     -1.739     -0.039     0.789     2.294     0.673
77     B     -2.757     0.494     0.757     0.680     1.386     -1.568     -0.487     0.152     2.569     0.922
78     B     -2.287     1.233     -1.057     0.257     0.192     -0.719     -0.492     0.425     2.022     1.952
79     B     -1.841     1.939     -0.553     0.365     0.030     -0.992     0.174     1.078     1.806     1.286
80     B     -2.305     0.655     -0.100     0.474     0.164     -1.715     -0.299     0.705     1.960     0.930
81     B     -2.435     1.179     0.034     0.506     0.626     -1.482     -0.189     0.846     2.011     1.098
82     B     -3.467     0.811     -1.300     0.214     -0.372     -1.398     0.234     1.075     1.554     1.826
83     B     -2.013     1.535     -0.148     0.462     0.591     -0.985     0.193     0.833     1.519     1.200
84     C     -2.274     0.631     -0.414     0.396     0.324     -1.343     -0.261     0.763     2.047     1.336
85     C     -0.384     0.373     0.248     0.561     0.750     -1.385     0.247     0.481     -0.134     0.898
86     C     -1.201     0.412     0.949     0.721     1.274     -1.909     -0.020     0.301     1.446     0.582
87     C     -1.394     1.392     -0.107     0.473     0.587     -0.748     -0.133     0.654     1.629     1.160
88     C     -2.412     0.277     0.227     0.556     0.815     -2.327     0.041     0.726     0.984     0.958
89     C     -2.395     0.498     0.298     0.574     0.938     -1.560     -0.368     0.484     1.627     1.089
90     C     -2.053     1.723     -0.174     0.456     0.540     -0.790     -0.091     0.192     2.418     1.173
91     C     -2.098     0.004     0.134     0.533     0.538     -1.961     0.571     0.661     2.221     0.863
92     C     -1.147     0.000     0.024     0.503     0.381     -1.484     -0.348     0.088     1.903     0.886
93     C     -2.478     0.563     -0.099     0.475     0.290     -1.213     0.046     0.292     2.359     0.992
94     C     -1.698     1.535     -0.394     0.402     0.463     -0.244     0.545     0.848     1.219     1.373
95     C     -2.397     1.021     0.010     0.502     0.662     -0.933     -0.145     0.328     2.361     1.127
 ;

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

data perf;
	input score actual_gb $;
	cards;
100 b
110 b
120 g
130 b
140 b
150 g
160 b
170 g
180 g
190 b
200 g
210 g
220 b
230 g
240 g
250 b
260 b
270 g
280 g
290 b
300 g
310 b
320 g
330 g
340 g
;

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
