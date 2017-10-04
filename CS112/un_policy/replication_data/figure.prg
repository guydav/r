new;

dat=loada("figure.dat",5);  @y, p1,p2,counterp1,counterp2@
obs=seqa(1,1,rows(dat));
y=dat[.,1];
p1=dat[.,2];
p2=dat[.,3];

library pgraph; 

#include graphon.g;
graphon;                 

begwind;
window(2,2,0);
graphset;
title("In Sample Fit");
xlabel("Probabilities from modified model");
ylabel("Probabilities from original model"); 
ytics(0,1,0.25,0);
xtics(0,1.0,0.25,0);
_plctrl=-1;
_pstype=8; @ solid circle@
/* _pltype={2,6}; */
dat=sortc(dat,3); @sorted by p2@
_plinet=4;
pline(0,0,1,1);
_pltype={6,3};
_plotsiz={5,5};  @ so the figure is square @
_psym={
  0.9971 0.9528   1 8 15 1 0,   
  0.9999 0.9310   1 8 15 1 0,
  0.9884 0.8849   1 8 15 1 0,
  0.8753 0.9863   2 8 15 1 0,
  0.9892 0.8395   1 8 15 1 0,
  0.1500 0.4213   1 8 15 1 0};
xy(dat[.,3],dat[.,2]);

nextwind;
graphset;
title("Counterfactual Prediction");
xlabel("Probabilities from modified model");
ylabel("Probabilities from original model"); 
ytics(0,1,0.25,0);
xtics(0,1.0,0.25,0);
_plctrl=-1;
_pstype=1; @ empty circle@
/* _pltype={2,6}; */
dat=sortc(dat,5); @sorted by counterp2@
_plinet=4;
pline(0,0,1,1);
_pltype={6,3};
_plotsiz={5,5};  @ so the figure is square @
xy(dat[.,5],dat[.,4]);
  
endwind;
WinPrintPQG(_graphon_pqg,"-t EPS  -o P -f figure.ps");




