libname cluster "C:\Users\Dwaipayan\Desktop\DD\SAS";

PROC IMPORT OUT= cluster.Cluster 
            DATAFILE= "C:\Users\Dwaipayan\Desktop\DD\SAS\Cluster.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

data cluster.clustering;
set cluster;
drop VAR9;
run;

Proc contents data = cluster.Clustering;
run;

/*************************************************
Outlier treatment

**************************************************/

%macro pctlcap(input=, output=, class=none, vars=, pctl=1 95);
%if &output = %then %let output = &input;
%let varL=;
%let varH=;
%let xn=1;

%do %until (%scan(&vars,&xn)= );
%let token = %scan(&vars,&xn);
%let varL = &varL &token.L;
%let varH = &varH &token.H;
%let xn=%EVAL(&xn + 1);
%end;

%let xn=%eval(&xn-1);
data xtemp;
set &input;
run;
%if &class = none %then %do;
data xtemp;
set xtemp;
xclass = 1;
run;
%let class = xclass;
%end;
proc sort data = xtemp;
by &class;
run;
proc univariate data = xtemp noprint;
by &class;
var &vars;
output out = xtemp_pctl PCTLPTS = &pctl PCTLPRE = &vars PCTLNAME = L H;
run;
data &output;
merge xtemp xtemp_pctl;
by &class;
array trimvars{&xn} &vars;
array trimvarl{&xn} &varL;
array trimvarh{&xn} &varH;

do xi = 1 to dim(trimvars);
if not missing(trimvars{xi}) then do;
if (trimvars{xi} < trimvarl{xi}) then trimvars{xi} = trimvarl{xi};
if (trimvars{xi} > trimvarh{xi}) then trimvars{xi} = trimvarh{xi};
end;
end;
drop &varL &varH xclass xi;
run;

%mend pctlcap;

%pctlcap(input=cluster.clustering, output=result, class=none, vars = Cat1 Cat2 Cat3 Cat4, pctl=1 95);

data result1;
set result;
SPSQFS= sale/size;
run;
proc univariate data= result1;
var Cat1 Cat2 Cat3 Cat4 SPSQFS;
run;


%macro kmean(K);
proc fastclus data=result1 out=outdata&K. maxclusters= &K. outstat=cluststat&K. maxiter=100 converge=0;
var cat1 cat2 cat3 cat4 size SPSQFS;
run;
%mend;
%kmean(1);
%kmean(2);
%kmean(3);
%kmean(4);
%kmean(5);
%kmean(6);
%kmean(7);
%kmean(8);
%kmean(9);

%macro create_ds(A);
data aa&A.;
set cluststat&A.;
nclus = &A.;
if _type_ = "RSQ";
keep nclus OVER_ALL;
run;
%mend;
%create_ds (1);
%create_ds (2);
%create_ds (3);
%create_ds (4);
%create_ds (5);
%create_ds (6);
%create_ds (7);
%create_ds (8);
%create_ds (9);


data plotin;
set Aa1 Aa2 Aa3 aa4 aa5 aa6 aa7 aa8 aa9;
run;

data plotin1;
set plotin;
x = 1 - over_all;
run;

* plot elbow curve using r-square values;
symbol1 color=blue interpol=join;
proc gplot data=plotin1;
plot x*nclus;
run;




/*Three cluster is the optimum cluster because post that The pseudo-F statistic that is*/
/*intended to capture the 'tightness' of clusters, and is in essence a ratio of the mean */
/*sum of squares between groups to the mean sum of squares within group goes down.*/

/*CCC to increase to a maximum as we increment the number of clusters to 3 and it is at -4.119 post that */
/*there is a decrease so we take the number of clusters at 3 based on (local) maximum. */
