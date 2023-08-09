clear all

cd "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\Figures2\"

import delimited "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\IranMoreCertainty_Final.csv"
destring *, ignore("NA") replace

***Creating Tables for Iran***

* Remove Qatar observations
drop if adversary == 0

* Run regression models for main DV and mediators with dispositional controls
eststo: reg esca_scaled i.denial ma_scaled nc_scaled govtrust newstrust inttrust readfp  
eststo: reg reputation_scaled denial ma_scaled nc_scaled govtrust newstrust inttrust readfp
estimates store m1 
eststo: ologit ambiguity denial ma_scaled nc_scaled govtrust newstrust inttrust readfp  
eststo: ologit insulting denial ma_scaled nc_scaled govtrust newstrust inttrust readfp  

* Create a coefficient plot of results with dispositional controls
coefplot m1, nolabel drop(_cons) keep(*:) xline(0) coeflabels(reputation_scaled = "Reputation Scaled" denial = "Denial" ma_scaled = "Military Assertiveness" nc_scaled = "National Chauvinism" govtrust = "Government Trust" newstrust = "Newstrust" inttrust = "International Trust" readfp = "Foreign Policy Interest") title("Treatment and Escalation Preferences") 
graph export OS_MainModel.jpg, replace width(4000) height(3000)

* Create results table for models with dispositional controls
esttab using OS_MainModel.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Iran: Escalation Preference and Mediators with Dispositional Controls") ///
	coeflabels(denial "Denial" ma_scaled "Military Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" readfp "Foreign Policy Interest") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insult")
eststo clear

* Run regression models for main DV and mediators with no controls
eststo: reg esca_scaled denial 
eststo: reg reputation_scaled denial 
eststo: ologit ambiguity denial 
eststo: ologit insulting denial 

* Create results table for models with no controls
esttab using OS_MainModel_NoControls.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Iran: Escalation Preference and Mediators without Controls") ///
	coeflabels(denial "Denial") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear

* Run regression models for main DV and mediators with demographic controls
eststo: reg esca_scaled denial age male hhi white education republican democrat
eststo: reg reputation_scaled denial age male hhi white education republican democrat
eststo: ologit ambiguity denial age male hhi white education republican democrat
eststo: ologit insulting denial age male hhi white education republican democrat

* Create results table for models with demographic controls
esttab using OS_MainModel_DemControls.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Iran: Escalation Preference and Mediators with Demographic Controls") ///
	coeflabels(denial "Denial" age "Age" male "Male" hhi "Household Income" white "White" education "Education" republican "Republican" democrat "Democrat") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear

* Run models with militant assertiveness interaction and dispositional controls
eststo: reg esca_scaled c.denial##c.ma_scaled nc_scaled govtrust newstrust inttrust readfp  
eststo: reg reputation_scaled c.denial##c.ma_scaled nc_scaled govtrust newstrust inttrust readfp
eststo: ologit insulting c.denial##c.ma_scaled nc_scaled govtrust newstrust inttrust readfp  
eststo: ologit ambiguity c.denial##c.ma_scaled nc_scaled govtrust newstrust inttrust readfp  

* Create results table for militant assertiveness interaction
esttab using Iran_MA_Interaction.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	coeflabels(denial "Denial" ma_scaled "Militant Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" readfp "Foreign Policy Interest") ///
	mtitles("Escalation" "Reputation" "Insult" "Certainty")
eststo clear

* Run models with international trust interaction and dispositional controls
eststo: reg esca_scaled c.denial##c.inttrust ma_scaled nc_scaled govtrust newstrust readfp 

* Create margins plot for the international trust interaction in the escalation model
margins, dydx(denial) at(inttrust = (1 2 3 4 5))
marginsplot, recast(line) title("") xtitle("International Trust")
graph export Iran_ME_IntTrust_Escalation.jpg, replace width(4000) height(3000) 

eststo: reg reputation_scaled c.denial##c.inttrust ma_scaled nc_scaled govtrust newstrust readfp
eststo: ologit insulting c.denial##c.inttrust ma_scaled nc_scaled govtrust newstrust readfp 
eststo: ologit ambiguity c.denial##c.inttrust ma_scaled nc_scaled govtrust newstrust readfp  

* Create results table for international trust interaction
esttab using Iran_IT_Interaction.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	coeflabels(denial "Denial" ma_scaled "Militant Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" readfp "Foreign Policy Interest") ///
	mtitles("Escalation" "Reputation" "Insult" "Certainty")
eststo clear

* Run models with news trust interaction and dispositional controls
eststo: reg esca_scaled c.denial##c.newstrust ma_scaled nc_scaled govtrust  inttrust readfp  
eststo: reg reputation_scaled c.denial##c.newstrust ma_scaled nc_scaled govtrust  inttrust readfp
eststo: ologit insulting c.denial##c.newstrust ma_scaled nc_scaled govtrust inttrust readfp  
eststo: ologit ambiguity c.denial##c.newstrust ma_scaled nc_scaled govtrust inttrust readfp 

* Create margins plot for news trust interaction in the escalation model
margins, dydx(denial) at(newstrust = (1 2 3 4 5)) predict(xb)
marginsplot, recast(line) title("") xtitle("Trust in News ")
graph export Iran_ME_NewsTrust_Escalation.jpg, replace width(4000) height(3000) 

* Create results table for news trust interaction
esttab using Iran_NT_Interaction.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	coeflabels(denial "Denial" ma_scaled "Militant Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" readfp "Foreign Policy Interest") ///
	mtitles("Escalation" "Reputation" "Insult" "Certainty")
eststo clear

* Run models with government trust interaction and dispositional controls
eststo: reg esca_scaled c.denial##c.govtrust ma_scaled nc_scaled newstrust inttrust readfp  
eststo: reg reputation_scaled c.denial##c.govtrust  ma_scaled nc_scaled newstrust inttrust readfp
eststo: ologit insulting c.denial##c.govtrust ma_scaled nc_scaled newstrust inttrust readfp  
eststo: ologit ambiguity c.denial##c.govtrust ma_scaled nc_scaled newstrust inttrust readfp  

* Create results table for government trust interaction
esttab using Iran_Gov_Interaction.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	coeflabels(denial "Denial" ma_scaled "Militant Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" readfp "Foreign Policy Interest") ///
	mtitles("Escalation" "Reputation" "Insult" "Certainty")
eststo clear

*Demographic model with party ID interaction
eststo: reg esca_scaled denial##republican age male hhi white education
eststo: reg reputation_scaled denial##republican age male hhi white education
eststo: ologit ambiguity denial##republican age male hhi white education
eststo: ologit insulting denial##republican age male hhi white education

esttab using Iran_Party_Interaction.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Iran: Escalation Preference and Mediators with Demographic Controls") ///
	coeflabels(denial "Denial" age "Age" male "Male" hhi "Household Income" white "White" education "Education" republican "Republican" democrat "Democrat") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear

