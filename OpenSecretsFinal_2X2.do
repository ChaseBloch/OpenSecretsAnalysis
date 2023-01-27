clear all

cd "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\Figures\"

import delimited "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\2X2Data_Final.csv"
destring *, ignore("NA") replace

*Tables with dispositional controls
eststo: reg esca_scaled i.denial##i.adversary ma_scaled nc_scaled govtrust newstrust inttrust readfp 
estimates store m1
margins, dydx(denial) at(adversary = (0 1))
marginsplot, recast(bar) title("") xtitle("") xlabel(0 "Qatar" 1 "Iran") yscale(range(-25 0)) ylabel(0 -5 -10 -15 -20 -25)
graph export ME_Escalation.jpg, replace width(4000) height(3000)

eststo: reg reputation_scaled denial##adversary ma_scaled nc_scaled govtrust newstrust inttrust readfp  
eststo: ologit ambiguity i.denial##i.adversary ma_scaled nc_scaled govtrust newstrust inttrust readfp
estimates store m2
margins adversary, dydx(denial) predict(xb)
marginsplot, recast(bar) title("") xtitle("") xlabel(0 "Qatar" 1 "Iran") yscale(range(-4 0)) ylabel(0 -1 -2 -3 -4)
graph export ME_Certainty.jpg, replace width(4000) height(3000) 
  
eststo: ologit insulting denial##adversary ma_scaled nc_scaled govtrust newstrust inttrust readfp  

esttab using OS_2X2.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Escalation Preference and Mediators with Adversary Interaction") ///
	coeflabels(denial "Denial" ma_scaled "Military Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust "International Trust" readfp "Foreign Policy Interest") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insult")
eststo clear


