library(qualtRics)
library(stargazer)
library(ggplot2)
library(dplyr)
library(glm2)
library(httr)
library(mediation)
library(MASS)
library(data.table)



###Importing and Processing Survey Data###

#Register API credentials
qualtrics_api_credentials(api_key = "8BhFgrQIJ9YIDgPHwx78h4Sid2VI2tKLDYXQEULF", 
                          base_url = "pennstate.ca1.qualtrics.com",
                          install = TRUE,
                          overwrite = TRUE)

setwd("C:/Users/chase/GDrive/GD_Work/Dissertation/JointPaper/OpenSecretsAnalysis")

#Import Survey and correct variable names
surveys <- all_surveys()
df <- fetch_survey(surveyID = surveys$id[10],verbose =TRUE,force_request = TRUE)
df2 <- fetch_survey(surveyID = surveys$id[2],verbose =TRUE,force_request = TRUE)
df <- rbindlist(list(df, df2), fill = TRUE)
names(df) <- gsub(":", "", names(df))
names(df) <- make.names(names(df), unique=TRUE)


###Attention Checks###



#Drop based on first attention check
df <- df[!is.na(df$AC1_1) & !is.na(df$AC1_2) & df$Status != "Survey Preview" & df$Finished == "TRUE",]
df[] <- lapply(df, function(x) {
  is.na(levels(x)) <- levels(x) == "NA"
  x
})

#Drop based on second attention check
df = df[df$i.AC2 == "The United States and Iran" | df$q.AC2 == "The United States and Qatar",]

#Keep only respondents who finished and were from Lucid (no previews)
df <- df[df$Status == "IP Address",]
df <- df[!is.na(df$Status),]

#Creating denial and adversary variables
df$denial = ifelse(!is.na(df$q.denial) | !is.na(df$i.denial), 1,0)
df$adversary = ifelse(!is.na(df$i.AC2), 1,0)

###Combining Variables###
df = df %>% mutate(escalation_1 = coalesce(q.escalation_1, i.escalation_1))
df = df %>% mutate(escalation_2 = coalesce(q.escalation_2, i.escalation_2))
df = df %>% mutate(escalation_3 = coalesce(q.escalation_3, i.escalation_3))
df = df %>% mutate(escalation_4 = coalesce(q.escalation_4, i.escalation_4))

df = df %>% mutate(reputation_1 = coalesce(q.reputation_1, i.reputation_1))
df = df %>% mutate(reputation_2 = coalesce(q.reputation_2, i.reputation_2))
df = df %>% mutate(reputation_3 = coalesce(q.reputation_3, i.reputation_3))

df = df %>% mutate(insulting = coalesce(q.emotions, i.emotions))

df = df %>% mutate(ambiguity = coalesce(q.ambiguity, i.ambiguity))

###Creating Indices for Variables###


#Factor escalation variables
df$escalation_1 = factor(df$escalation_1, levels = c(
  "Oppose strongly",
  "Oppose somewhat",
  "Neither favor nor oppose",
  "Favor somewhat",
  "Favor strongly"
))

df$escalation_2 = factor(df$escalation_2, levels = c(
  "Oppose strongly",
  "Oppose somewhat",
  "Neither favor nor oppose",
  "Favor somewhat",
  "Favor strongly"
))

df$escalation_3 = factor(df$escalation_3, levels = c(
  "Oppose strongly",
  "Oppose somewhat",
  "Neither favor nor oppose",
  "Favor somewhat",
  "Favor strongly"
))

df$escalation_4 = factor(df$escalation_4, levels = c(
  "Oppose strongly",
  "Oppose somewhat",
  "Neither favor nor oppose",
  "Favor somewhat",
  "Favor strongly"
))

#Create weighted escalation variable

df$esca1 = as.numeric(df$escalation_1)*1
df$esca2 = as.numeric(df$escalation_2)*2
df$esca3 = as.numeric(df$escalation_3)*3
df$esca4 = as.numeric(df$escalation_4)*4

df$esca_dv = (as.numeric(df$esca1) + as.numeric(df$esca2) + as.numeric(df$esca3) + as.numeric(df$esca4)) - 5
df$esca_dv = df$esca_dv/45*100

#Created scaled escalation variable
df$esca_scaled = (scale(as.numeric(df$escalation_1)) + scale(as.numeric(df$escalation_2)) + scale(as.numeric(df$escalation_3)) + scale(as.numeric(df$escalation_4)))/4

#Creating individual escalation variables for each answer
df$war[as.numeric(df$escalation_4) == 4 | as.numeric(df$escalation_4) == 5 ] <- 1
df$war[as.numeric(df$escalation_4) == 1 | as.numeric(df$escalation_4) == 2 | as.numeric(df$escalation_4) == 3] <- 0

df$airstrike[as.numeric(df$escalation_3) == 4 | as.numeric(df$escalation_3) == 5 ] <- 1
df$airstrike[as.numeric(df$escalation_3) == 1 | as.numeric(df$escalation_3) == 2 | as.numeric(df$escalation_3) == 3] <- 0

df$sanctions[as.numeric(df$escalation_2) == 4 | as.numeric(df$escalation_2) == 5 ] <- 1
df$sanctions[as.numeric(df$escalation_2) == 1 | as.numeric(df$escalation_2) == 2 | as.numeric(df$escalation_2) == 3 ] <- 0

df$diplomacy[as.numeric(df$escalation_1) == 4 | as.numeric(df$escalation_1) == 5 ] <- 1
df$diplomacy[as.numeric(df$escalation_1) == 1 | as.numeric(df$escalation_1) == 2 | as.numeric(df$escalation_1) == 3 ] <- 0


#Military Assertiveness
df$MA = (6 - as.numeric(df$MA1)) + as.numeric(df$MA2) + (6 - as.numeric(df$MA3))
df$MA_scaled = (scale(6 - as.numeric(df$MA1))) + scale(as.numeric(df$MA2)) + scale((6 - as.numeric(df$MA3)))/3

#National Chauvinism
df$NC = (as.numeric(df$NC1) + as.numeric(df$NC2))
df$NC_scaled = scale(as.numeric(df$NC1) + scale(as.numeric(df$NC2)))/2

#Reputation
df$reputation_1 = factor(df$reputation_1, levels = c(
  "Almost no chance",
  "25% change",
  "50-50 chance",
  "75% chance",
  "Nearly 100% certain"
))

df$reputation_2 = factor(df$reputation_2, levels = c(
  "Almost no chance",
  "25% change",
  "50-50 chance",
  "75% chance",
  "Nearly 100% certain"
))

df$reputation_3 = factor(df$reputation_3, levels = c(
  "Almost no chance",
  "25% change",
  "50-50 chance",
  "75% chance",
  "Nearly 100% certain"
))

df$reputation_scaled = (scale(as.numeric(df$reputation_1)) + scale(as.numeric(df$reputation_2)) + scale(as.numeric(df$reputation_3)))/3

df$reputation = as.numeric(df$reputation_1) + as.numeric(df$reputation_2) + as.numeric(df$reputation_3)

df$GovTrust = 6-as.numeric(df$GovTrust)
df$NewsTrust = 6-as.numeric(df$NewsTrust)
df$IntTrust = 6-as.numeric(df$IntTrust)
df$Military.Service = 2-as.numeric(df$Military.Service)
df$Military.Service[df$Military.Service ==-1] = NA
df$Read.FP = 6-as.numeric(df$Read.FP)

###Analysis###

#Linear Models
df_res = df %>% dplyr::select(denial, adversary, esca_dv, esca_scaled, MA_scaled, GovTrust, 
                              NewsTrust, IntTrust, NC_scaled, Military.Service, Read.FP, reputation, 
                              reputation_scaled, ambiguity, insulting, war, airstrike, sanctions, diplomacy, age, gender, hhi, ethnicity, hispanic, education, political_party,region =)
df_res[] <- lapply(df_res, as.numeric)
write.csv(df_res, "2X2Data_Final.csv")

