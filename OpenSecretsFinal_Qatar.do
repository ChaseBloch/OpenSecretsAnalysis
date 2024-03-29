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

*Main model with militant assertiveness interaction
eststo: reg esca_scaled c.denial##c.ma_scaled nc_scaled govtrust newstrust inttrust readfp  
eststo: reg reputation_scaled c.denial##c.ma_scaled nc_scaled govtrust newstrust inttrust readfp

estimates store m1
margins, dydx(denial) at(ma_scaled=(-4.5(.1)3.5))
marginsplot, recast(line) recastci(rarea) ciopt(color(%30)) title("") xtitle("Militant Assertiveness")
graph export Qatar_ME_MA_Rep.jpg, replace width(4000) height(3000) 

eststo: ologit insulting c.denial##c.ma_scaled nc_scaled govtrust newstrust inttrust readfp

estimates store m2
margins, dydx(denial) at(ma_scaled=(-4.5(.1)3.5)) predict(xb)
marginsplot, recast(line) recastci(rarea) ciopt(color(%30)) title("") xtitle("Militant Assertiveness")
graph export Qatar_ME_MA_Insult.jpg, replace width(4000) height(3000) 

eststo: ologit ambiguity c.denial##c.ma_scaled nc_scaled govtrust newstrust inttrust readfp  

esttab using Qatar_MA_Interaction.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	coeflabels(denial "Denial" ma_scaled "Militant Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" readfp "Foreign Policy Interest") ///
	mtitles("Escalation" "Reputation" "Insult" "Certainty")
eststo clear

*Main model with international trust interaction
eststo: reg esca_scaled c.denial##c.inttrust ma_scaled nc_scaled govtrust newstrust readfp  
eststo: reg reputation_scaled c.denial##c.inttrust ma_scaled nc_scaled govtrust newstrust readfp

estimates store m3
margins, dydx(denial) at(inttrust=(1 2 3 4 5))
marginsplot, recast(line) xtitle("International Trust") title("")
graph export Qatar_ME_IntTrust_Rep.jpg, replace width(4000) height(3000) 

eststo: ologit insulting c.denial##c.inttrust ma_scaled nc_scaled govtrust newstrust readfp 

estimates store m4
margins, dydx(denial) at(inttrust=(1 2 3 4 5)) predict(xb)
marginsplot, recast(line) xtitle("International Trust") title("")
graph export Qatar_ME_IntTrust_Ins.jpg, replace width(4000) height(3000) 

eststo: ologit ambiguity c.denial##c.inttrust ma_scaled nc_scaled govtrust newstrust readfp  

esttab using Qatar_IT_Interaction.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	coeflabels(denial "Denial" ma_scaled "Militant Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" readfp "Foreign Policy Interest") ///
	mtitles("Escalation" "Reputation" "Insult" "Certainty")
eststo clear
*Reputation and insult significant

*Main model with news trust interaction
eststo: reg esca_scaled c.denial##c.newstrust ma_scaled nc_scaled govtrust  inttrust readfp  
eststo: reg reputation_scaled c.denial##c.newstrust ma_scaled nc_scaled govtrust  inttrust readfp
eststo: ologit insulting c.denial##c.newstrust ma_scaled nc_scaled govtrust inttrust readfp  
eststo: ologit ambiguity c.denial##c.newstrust ma_scaled nc_scaled govtrust inttrust readfp 

esttab using Qatar_NT_Interaction.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	coeflabels(denial "Denial" ma_scaled "Militant Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" readfp "Foreign Policy Interest") ///
	mtitles("Escalation" "Reputation" "Insult" "Certainty")
eststo clear

*Main model with government trust interaction
eststo: reg esca_scaled c.denial##c.govtrust ma_scaled nc_scaled newstrust inttrust readfp  
eststo: reg reputation_scaled c.denial##c.govtrust  ma_scaled nc_scaled newstrust inttrust readfp
eststo: ologit insulting c.denial##c.govtrust ma_scaled nc_scaled newstrust inttrust readfp  
eststo: ologit ambiguity c.denial##c.govtrust ma_scaled nc_scaled newstrust inttrust readfp  

esttab using Qatar_Gov_Interaction.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	coeflabels(denial "Denial" ma_scaled "Militant Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" readfp "Foreign Policy Interest") ///
	mtitles("Escalation" "Reputation" "Insult" "Certainty")
eststo clear

*Demographic model with party ID interaction
eststo: reg esca_scaled denial##democrat denial##republican age male hhi white education
eststo: reg reputation_scaled denial##democrat denial##republican age male hhi white education 
eststo: ologit ambiguity denial##democrat denial##republican age male hhi white education 
eststo: ologit insulting denial##democrat denial##republican age male hhi white education 

esttab using Qatar_Party_Interaction.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Qatar: Escalation Preference and Mediators with Demographic Controls") ///
	coeflabels(denial "Denial" age "Age" male "Male" hhi "Household Income" white "White" education "Education" republican "Republican" democrat "Democrat") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear