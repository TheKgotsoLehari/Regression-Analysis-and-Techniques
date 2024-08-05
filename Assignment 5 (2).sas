title1 'Cereal Dataset'; 
options validvarname= v7;
FILENAME REFILE '/home/u60987316/sasuser.v94/STK 310 2024/Section A/Assignment 5/cereal.csv' ENCODING='wlatin1';

proc import datafile=REFILE
DBMS=CSV
OUT=WORK.Yes;
getnames=YES;
run;
 
data pap; 
set WORK.Yes; 
y=Sales_of_paul_se_pap; 
x2=Price_of_paul_se_pap; 
x3=Price_of_Three_Bears_Bran; 
x4=Sales_of_Three_Bears_Bran; 
keep y x2 x3 x4; 
run; 

** Question 1**;
title1 'Multiple regression using proc reg: select model using adjusted R-square'; 
proc reg data=pap plot=none; 
model y=x2 x3 x4 / selection=adjrsq; 
run;


**Question 2 **;
title1 'Partial correlation between Y & X2'; 
proc corr data=pap; 
var y x2; 
partial x3; 
run; 


title1 'Partial correlation between Y & X3'; 
proc corr data=pap; 
var y x3; 
partial x2; 
run;


**Question 3**;
data logs; 
set pap; 
lny=log(y); 
lnx2=log(x2); 
lnx3=log(x3); 
run; 


title1 'Multiple regression using proc reg: inference & prediction'; 
proc reg data=logs plot=none; 
model lny=lnx2 lnx3 / alpha=0.01 clb clm; 
id x2 x3; 
test lnx2=lnx3; 
output out=reg_out p=lnyhat lclm=lnmeanlo uclm=lnmeanup r=residual; 
run; 


title1 'Verifying normality assumption'; 
proc univariate data=reg_out normal; 
var residual; 
run; 


title1 'Mean prediction'; 
proc iml; 
use reg_out; 
read all var{lnyhat lnmeanlo lnmeanup}; 
i = 12; 
yhat36 = exp(lnyhat[i]); 
meanlo36 = exp(lnmeanlo[i]); 
meanup36 = exp(lnmeanup[i]); 
print 'Predicted mean number of boxes of paul-se-pap sold:'; 
print yhat36[label=none]; 
print '99% confidence interval for mean number of boxes of paul-se-pap sold:'; 
print meanlo36[label=none] meanup36[label=none];


** Question 4**;
data onlylogs; 
set logs; 
keep lny lnx2 lnx3; 
run; 
goptions reset=all; 
title1 'Multiple regression using proc iml'; 
proc iml; 
use onlylogs; 
read all into matrix; 
n=nrow(matrix); 
y=matrix[,1]; 
x=j(n,1,1)||matrix[,2:3]; 
print 'y:' y[label=none] 'X:' x[label=none]; 
bhat=inv(x`*x)*x`*y; 
print 'Vector of parameter estimates:' bhat[label=none]; 
k=ncol(x); 
uhat=y-x*bhat; 
mse=uhat`*uhat/(n-k); 
print 'Mean square error:' mse[label=none]; 
varcovb=mse#inv(x`*x); 
print 'Covariance matrix for estimates:' varcovb[label=none]; 
stderr=sqrt(vecdiag(varcovb)); 
t=bhat/stderr; 
t_pvalue=2#(1-probt(abs(t),n-k)); 
print 'Standard errors of estimates:' stderr[label=none]; 
print 'Test statistic values for t-tests:' t[label=none]; 
print 'p-values for t-tests:' t_pvalue[label=none]; 
ybar=sum(y)/n; 
ess=bhat`*x`*y-n#ybar##2; 
tss=y`*y-n#ybar##2; 
rsquare=ess/tss; 
print 'Coefficient of determination:' rsquare[label=none]; 
adjrsq=1-(1-rsquare)#(n-1)/(n-k); 
print 'Adjusted coefficient of determination:' adjrsq[label=none]; 
f=(rsquare/(k-1))/((1-rsquare)/(n-k)); 
f_pvalue=1-probf(f,k-1,n-k); 
print 'Test statistic value for F-test:' f[label=none]; 
print 'p-value for F-test:' f_pvalue[label=none]; 
quit;
