clear all

cd "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\Figures\"

import delimited "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\2X2Data_Final.csv"
destring *, ignore("NA") replace

*Tables with dispositional controls
eststo: reg esca_scaled denial##adversary ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
eststo: reg reputation_scaled denial##adversary ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
eststo: ologit ambiguity denial##adversary ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
eststo: ologit insulting denial##adversary ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  

esttab using OS_2X2.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Escalation Preference and Mediators with Adversary Interaction") ///
	coeflabels(denial "Denial" ma_scaled "Military Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" militaryservice "Military Service" readfp "Reads News") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear


