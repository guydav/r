set seed 1979

use peace
log using replication.log, replace

/* table 3, model A8 */

logistic pbs2s3 wartype logcost wardur factnum factnum2 trnsfcap untype4 treaty develop exp decade,robust cluster(cluster)

gen un2=un2cint 
 
label var un2 "un2cint" 
label var develop "electric. cons."    

keep pbs2s3 wartype logcost wardur factnum factnum2 trnsfcap develop exp decade treaty un2 untype4 cluster

/* wp.dat: write out Y x1-x9 treaty un_table2A un_table3A8 cluster, excluding the md's */

keep if trnsfcap!=. & logcost!=.
outfile pbs2s3 wartype logcost wardur factnum factnum2 trnsfcap develop exp decade treaty un2 untype4 using wp.dat, replace wide

keep cluster
save cluster,replace
clear

infile y x1-x10 un1 un using wp.dat
merge using cluster
drop _merge

save wpx,replace

estsimp logit y x1-x10 un, antisim robust cluster(cluster)
outfile b12 b1-b11 using beta0.dat, w replace
drop b*

gen xu3=x3*un  

estsimp logit y x1-x10 un xu3, antisim robust cluster(cluster)
outfile b13 b1-b12 using beta1.dat, w replace

log close





