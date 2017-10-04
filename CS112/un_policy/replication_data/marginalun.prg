new;
rndseed 1234;

dat=loada("wp.dat",13);
@ call dstat(0,dat); @
y=dat[.,1];
x=dat[.,2:11 13];                 @ 122x11, x1-x10,un@

beta0=loada("beta0.dat",12);      

/* 1000x12, logit coeffs. 1000 simulations with mean at the point estimates. const, x1-x10, un. from stata/clarify: estsimp logit y x1-x10 un, robust cluster(cluster) antisim; outfile b12 b1-b11 using beta0.dat. similar below. */

beta0=meanc(beta0); @12 by 1 @

beta1=loada("beta1.dat",13);      @ 1000x13, logit coeffs. 1000 simulations with mean at the point estimates. const, x1-x10, un, xu3@

beta1=meanc(beta1); @13 by 1 @

/* Original model */

x0=reshape(meanc(x),122,11);
x1=x0;
x0[.,3]=x[.,3];
x1[.,3]=x[.,3];
x0[.,11]=zeros(rows(x0),1);
x0=ones(rows(x0),1)~x0;
x1[.,11]=ones(rows(x1),1);
x1=ones(rows(x1),1)~x1;

p0=logiti(x0*beta0);
p1=logiti(x1*beta0);
dp=p1-p0;  @nx1@

/* Model with added interactions */

x0=reshape(meanc(x),122,11);
x1=x0;
x0[.,3]=x[.,3];
x1[.,3]=x[.,3];
x0[.,11]=zeros(rows(x0),1);
xu3=x0[.,11].*x0[.,3];
x0=ones(rows(x0),1)~x0~xu3;

x1[.,11]=ones(rows(x1),1);
xu3=x1[.,11].*x1[.,3];
x1=ones(rows(x1),1)~x1~xu3;

p0=logiti(x0*beta1);
p1=logiti(x1*beta1);
dpx=p1-p0;  @nx1@

effects=x[.,3]~dp~dpx;

effects=sortc(effects,1);  @sorted by x[.,3], wardur@

library pgraph;

#include graphon.g;
graphon;                

begwind;
window(2,2,0);
graphset;
ylabel("Marginal effects of UN peacekeeping operations");
xlabel("Duration of wars");
xtics(0,315,5,1); 
ytics(0,0.8,.1,1); 
_pltype={2,6};

_pmsgstr="Dotted: Original model\000Model with interaction term";
_pmsgctl={160 .4 .15 0 1 15 0,
          35 .2 .15 0 1 15 0};     

xy(effects[.,1],effects[.,2:3]);
endwind;
WinPrintPQG(_graphon_pqg,"-t EPS  -o P -f marginalun.ps");   












