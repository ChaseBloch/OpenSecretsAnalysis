clear all

cd "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\Figures\"

import delimited "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\2X2Data_Final.csv"
destring *, ignore("NA") replace

* Run adversary interaction model with escalation DV
eststo: reg esca_scaled i.denial##i.adversary ma_scaled nc_scaled govtrust newstrust inttrust readfp 

* Create a margins plot of the adversary interaction
margins, dydx(denial) at(adversary = (0 1))
marginsplot, recast(line) title("") xtitle("") xlabel(-.25 " " 0 "Qatar" 1 "Iran" 1.25 " ", notick) 
graph export ME_Escalation.jpg, replace width(4000) height(3000)

* Run adversary interaction model with reputation DV
eststo: reg reputation_scaled denial##adversary ma_scaled nc_scaled govtrust newstrust inttrust readfp  

* Run adversary interaction model with ambiguity DV
eststo: ologit ambiguity i.denial##i.adversary ma_scaled nc_scaled govtrust newstrust inttrust readfp

* Create a margins plot of the adversary interaction
margins adversary, dydx(denial) predict(xb)
marginsplot, recast(line) title("") xtitle("") xlabel(-.25 " " 0 "Qatar" 1 "Iran" 1.25 " ", notick)
graph export ME_Certainty.jpg, replace width(4000) height(3000) 

* Run adversary interaction model with insult DV  
eststo: ologit insulting denial##adversary ma_scaled nc_scaled govtrust newstrust inttrust readfp  

* Create results table of adversary interaction model
esttab using OS_2X2.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Escalation Preference and Mediators with Adversary Interaction") ///
	coeflabels(denial "Denial" ma_scaled "Military Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust "International Trust" readfp "Foreign Policy Interest") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insult")
eststo clear


