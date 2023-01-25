clear all

cd "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\Figures\"

import delimited "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\2X2Data_Final.csv"
destring *, ignore("NA") replace

***Creating Tables for Qatar***

drop if adversary == 1

*Tables with dispositional controls
eststo: reg esca_scaled i.denial ma_scaled nc_scaled govtrust newstrust inttrust readfp  
margins denial, atmeans
marginsplot, recast(bar) xtitle("Denial") title("Qatar: Adjusted predictions of denial with 95% CIs")
graph export denial_pp_qatar.jpg, replace width(4000) height(3000)

eststo: reg reputation_scaled denial ma_scaled nc_scaled govtrust newstrust inttrust readfp  
eststo: ologit ambiguity denial ma_scaled nc_scaled govtrust newstrust inttrust readfp  
eststo: ologit insulting denial ma_scaled nc_scaled govtrust newstrust inttrust readfp  

esttab using OS_MainModel_Qatar.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Qatar: Escalation Preference and Mediators with Dispositional Controls") ///
	coeflabels(denial "Denial" ma_scaled "Militant Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust "International Trust" readfp "Foreign Policy Interest") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insult")
eststo clear


*Tables with no controls
eststo: reg esca_scaled denial 
eststo: reg reputation_scaled denial 
eststo: ologit ambiguity denial 
eststo: ologit insulting denial 

esttab using OS_MainModel_NoControls_Qatar.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Qatar: Escalation Preference and Mediators with No Controls") ///
	coeflabels(denial "Denial") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear

*Tables with demographic controls
eststo: reg esca_scaled denial age male hhi white education republican democrat
eststo: reg reputation_scaled denial age male hhi white education republican democrat
eststo: ologit ambiguity denial age male hhi white education republican democrat
eststo: ologit insulting denial age male hhi white education republican democrat

esttab using OS_MainModel_DemControls_Qatar.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Qatar: Escalation Preference and Mediators with Demographic Controls") ///
	coeflabels(denial "Denial" age "Age" male "Male" hhi "Household Income" white "White" education "Education" republican "Republican" democrat "Democrat") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear

*interaction model
eststo: reg esca_scaled denial##c.ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
eststo: reg reputation_scaled denial##c.ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
eststo: ologit ambiguity denial##c.ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
eststo: ologit insulting denial##c.ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  

esttab using OS_InteractionModel_Qatar.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Qatar: Escalation Preference and Mediators with Military Assertiveness Interaction") ///
	coeflabels(denial "Denial" ma_scaled "Military Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" militaryservice "Military Service" readfp "Reads News") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear