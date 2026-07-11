/* cap input rows for the captured run */
options obs=100;

/* Original script reads libname mydata "..."; data mydata.applicants.
   That external path does not exist outside the author's machine, so this
   bundle substitutes a small mock sample (same column shape: good_bad, age,
   amount, duration, checking, savings) built from the German-credit-style
   fields the script's CLASS/MODEL statements reference. */
data mydata_applicants;
	length checking $4 savings $4;
	input good_bad age amount duration checking $ savings $;
	datalines;
1 22 1049 6 A11 A61
1 49 2799 12 A12 A61
0 23 841 12 A14 A61
1 45 2122 24 A11 A61
1 53 2171 24 A11 A61
1 35 2241 24 A14 A62
1 53 3398 24 A11 A61
1 35 1361 24 A11 A61
1 61 1098 6 A14 A65
1 28 3758 24 A12 A61
0 25 3905 12 A11 A61
0 24 1935 15 A12 A61
1 22 1928 24 A13 A61
1 60 2384 12 A14 A63
1 28 8858 30 A11 A61
0 32 4780 18 A11 A61
1 53 6468 36 A12 A64
1 25 6350 24 A12 A61
1 44 1225 10 A14 A65
0 31 1478 12 A11 A61
1 48 2515 15 A14 A61
1 39 4020 24 A13 A62
0 26 2745 9 A11 A61
1 36 3161 18 A12 A61
0 20 1360 8 A11 A61
0 27 1610 9 A12 A61
0 34 2320 15 A11 A61
0 41 5150 24 A11 A62
0 29 990 6 A13 A61
0 46 3340 18 A12 A63
0 38 4210 24 A14 A61
0 33 1870 12 A11 A61
0 51 6020 30 A12 A61
0 24 1225 9 A14 A61
0 42 2980 15 A11 A64
1 30 1540 9 A13 A61
1 47 2760 12 A14 A62
1 26 980 6 A12 A61
1 55 4130 18 A13 A63
1 37 2050 12 A14 A61
1 29 1330 9 A11 A61
;
run;
