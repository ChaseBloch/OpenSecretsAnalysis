clear all

cd "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\Figures\"

import delimited "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\2X2Data_Final.csv"
destring *, ignore("NA") replace

***Creating Tables for Iran***

drop if adversary == 0

*Tables with dispositional controls
eststo: reg esca_scaled i.denial ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
margins denial, atmeans
marginsplot, recast(bar) xtitle("Denial") title("Iran: Adjusted predictions of denial with 95% CIs")
graph export denial_pp_iran.jpg, replace width(4000) height(3000)

eststo: reg reputation_scaled denial ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp
estimates store m1 

coefplot m1, nolabel drop(_cons) keep(*:) xline(0) coeflabels(reputation_scaled = "Reputation Scaled" denial = "Denial" ma_scaled = "Military Assertiveness" nc_scaled = "National Chauvinism" govtrust = "Government Trust" newstrust = "Newstrust" inttrust = "International Trust" militaryservice = "Military Service" readfp = "Read Foreign Policy") title("Treatment and Escalation Preferences") 

graph export OS_MainModel.jpg, replace width(4000) height(3000)

eststo: ologit ambiguity denial ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
eststo: ologit insulting denial ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  

esttab using OS_MainModel.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Iran: Escalation Preference and Mediators with Dispositional Controls") ///
	coeflabels(denial "Denial" ma_scaled "Military Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" militaryservice "Military Service" readfp "Reads News") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear


*Tables with no controls
eststo: reg esca_scaled denial 
eststo: reg reputation_scaled denial 
eststo: ologit ambiguity denial 
eststo: ologit insulting denial 

esttab using OS_MainModel_NoControls.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Iran: Escalation Preference and Mediators without Controls") ///
	coeflabels(denial "Denial") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear

*Tables with demographic controls
eststo: reg esca_scaled denial age male hhi white education republican democrat
eststo: reg reputation_scaled denial age male hhi white education republican democrat
eststo: ologit ambiguity denial age male hhi white education republican democrat
eststo: ologit insulting denial age male hhi white education republican democrat

esttab using OS_MainModel_DemControls.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Iran: Escalation Preference and Mediators with Demographic Controls") ///
	coeflabels(denial "Denial" age "Age" male "Male" hhi "Household Income" white "White" education "Education" republican "Republican" democrat "Democrat") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear

*interaction model
eststo: reg esca_scaled denial##c.ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
eststo: reg reputation_scaled denial##c.ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
eststo: ologit ambiguity denial##c.ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
eststo: ologit insulting denial##c.ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  

esttab using OS_InteractionModel.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Iran: Escalation Preference and Mediators with Military Assertiveness Interaction") ///
	coeflabels(denial "Denial" ma_scaled "Military Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" militaryservice "Military Service" readfp "Reads News") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear