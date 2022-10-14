clear all

cd "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\"

import delimited "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\2X2Data_Final.csv"
destring *, ignore("NA") replace

***Cleaning Demographic variables***
*Gender
gen male=gender
drop gender
replace male=0 if male==2 

* income
recode hhi (-3105=.) //these are respondents who preferred not to answer

*hispanic
gen Hispanic=hispanic
recode Hispanic (15=.) //these are respondents who preferred not to answer
replace Hispanic=0 if Hispanic==1
replace Hispanic=1 if Hispanic>1
drop hispanic
ren Hispanic hispanic

*ethnicity
gen Ethnicity=ethnicity
drop ethnicity
ren Ethnicity ethnicity
gen white=(ethnicity==1 & hispanic==0) 
gen black=(ethnicity==2) 

*education
gen Education=education
replace Education=0 if Education==-3105 //this is none of the above category. probably respondents who did not receive any formal education
drop education 
ren Education education

*political party
gen Political_party=political_party
drop political_party
ren Political_party political_party
gen republican=1 if political_party==9 | political_party==10 | political_party==5 | political_party==8
recode republican (.=0)

gen democrat=1 if political_party==1 | political_party==2 | political_party==3 | political_party==6
recode democrat (.=0) 

//when we add these two political party variables, then the baseline is others and independents

***Creating Tables for Iran***

drop if adversary == 0

*Tables with dispositional controls
eststo: reg esca_scaled i.denial ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
margins denial, atmeans
marginsplot, recast(bar) xtitle("Denial")
graph export denial_pp.jpg, replace width(4000) height(3000)

eststo: reg reputation_scaled denial ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
eststo: ologit ambiguity denial ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  
eststo: ologit insulting denial ma_scaled nc_scaled govtrust newstrust inttrust militaryservice readfp  

esttab using OS_MainModel.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Table 1: Escalation Preference and Mediators with Dispositional Controls") ///
	coeflabels(denial "Denial" ma_scaled "Military Assertiveness" nc_scaled "National Chauvinism" govtrust "Trust in Gov." newstrust "Trust in News" inttrust 		"International Trust" militaryservice "Military Service" readfp "Reads News") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear


*Tables with no controls
eststo: reg esca_scaled denial 
eststo: reg reputation_scaled denial 
eststo: ologit ambiguity denial 
eststo: ologit insulting denial 

esttab using OS_MainModel_NoControls.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Table 1: Escalation Preference and Mediators with Dispositional Controls") ///
	coeflabels(denial "Denial") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear

*Tables with demographic controls
eststo: reg esca_scaled denial age male hhi white education republican democrat
eststo: reg reputation_scaled denial age male hhi white education republican democrat
eststo: ologit ambiguity denial age male hhi white education republican democrat
eststo: ologit insulting denial age male hhi white education republican democrat

esttab using OS_MainModel_DemControls.rtf, replace b(3) se(3) noconstant  star(* .10 ** .05 *** .01)  ///
	title("Table 1: Escalation Preference and Mediators with Dispositional Controls") ///
	coeflabels(denial "Denial" age "Age" male "Male" hhi "Household Income" white "White" education "Education" republican "Republican" democrat "Democrat") ///
	mtitles("Escalation" "Reputation" "Certainty" "Insulting")
eststo clear