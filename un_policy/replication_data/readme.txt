Replication information for the re-analysis of the UN peacekeeping data:

--files for Table 2: 

replication.do, replication.log 

(takes peace.dta (original dta from DS), replicates DS's model A8,
create wp.dat and wpx.dta (useful variables), run the original and
modified models (reported in table 2 in our paper), simulate the
coefficients and write them to beta0.data and beta1.dat for later
use.)

--Checking convex hull membership of counterfactuals and computing
Gower distances: Step-by-step instructions on replicating this part of
the analysis are given at the WhatIf website (in the "Examples"
section of the documentation for WhatIf):
http://gking.harvard.edu/whatif/docs/Counterfactuals_about.html

--files for figure 4: 

figure.do: --> figure.dat---> used by figure.prg --> figure.eps

(figure.do generates the data file for the figure, containing y, p1 (pr
on observed data, original model, p2 (pr on observed data, modified
model), countp1 (pr on counterfactuals, orginal model), countp2 (pr on
counterfactuals, modified model). figure.prg plots figure.eps.)

--files for figure 7: 
fig7.do, fig7.eps

--files for figure 8: 
marginalun.prg, marginalun.ps 