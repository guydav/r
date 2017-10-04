clear
use wpx

replace x10=.97 if un==1 /* so the entire black bar can show */
  
twoway (histogram x10 if un==1, discrete density gap(90) bfcolor(gs2)) (histogram x10 if un==0, discrete density gap(90) bfcolor(gs15)), xtitle(Treaty) xscale(range(-.3 1.3)) xlabel(0(1)1) legend(off) scheme(s1manual)

graph save "/home/lzeng/Everest0/papers/Gary/causal/WP/New/Nicholas/x10.gph"

twoway (kdensity x7 if un==1, epan) (kdensity x7 if un==0, epan), ytitle(Density) xtitle(Development (energy consumption)) legend(off) scheme(s1manual)

graph save "/home/lzeng/Everest0/papers/Gary/causal/WP/New/Nicholas/x7.gph", replace

graph combine x10.gph x7.gph, scheme(s1manual) ysize(2.5) xsize(6) graphregion(margin(horiz_bargraph)) plotregion(margin(horiz_bargraph))

graph export "/home/lzeng/Everest0/papers/Gary/causal/WP/New/Nicholas/fig7.eps", as(eps) replace
