clear
use wpx

replace un=1-un /* counterfactual */

drop un1 

save counter11,replace /* counterfactual data for model 1 */

replace un=1-un /* the original un */
logit y x1-x10 un
predict p1
sum p1
save p1,replace

use counter11, clear
predict countp1

label var countp1 "predicted probabilities using counterfactual data with un=1-un */
save countp1,replace

clear
 
use wpx

replace un=1-un /* counter factuals */

gen xu3=x3*un  /* counterfactual data */

drop un1

save counter22,replace

replace un=1-un /* back to original un */

replace xu3=x3*un 

logit y x1-x10 un xu3  
predict p2
sum p2
save p2,replace

use counter22,clear
predict countp2

sum

label var countp2 "predicted probabilities using counterfactual data with un=1-un, in model with interactions */

merge using countp1
drop _merge
merge using p1
drop _merge
merge using p2
drop _merge
merge using countp1

keep y p1 p2 countp1 countp2
outfile y p1 p2 countp1 countp2 using figure.dat, replace

