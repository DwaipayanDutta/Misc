PROC IMPORT OUT= WORK.CLUSTER 
            DATAFILE= "C:\Users\Dwaipayan\Desktop\DD\SAS\Cluster.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc print data=WORK.CLUSTER ;


proc freq data = cluster ;
tables State * storenum /noCol norow ;
run;
proc univariate data = cluster;
var storenum Cat1;
run;
