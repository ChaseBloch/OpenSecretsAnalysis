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

setwd("C:/Users/chase/GDrive/GD_Work/Dissertation/JointPaper/Data")

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




#Converting treatment to ordinal variable 
df$treat[!is.na(df$q.denial)] <- 0
df$treat[!is.na(df$i.denial)] <- 1
df$treat[!is.na(df$q.overt)] <- 2
df$treat[!is.na(df$i.overt)] <- 3

#Creating denial and adversary variables
df$denial_2 = ifelse(!is.na(df$q.denial) | !is.na(df$i.denial), 1,0)
df$adversary_2 = ifelse(!is.na(df$i.AC2), 1,0)



#Creating treatment dummy varaibles
df$overt_nonadversary = ifelse(df$treat == 2, 1, 0)
df$adversary = ifelse(df$treat == 3, 1, 0)
df$denial = ifelse(df$treat == 0, 1, 0)
df$denial_adversary = ifelse(df$treat == 1, 1, 0)

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



###Analysis###

#Linear Models
df_res = df %>% dplyr::select(denial_2, adversary_2, esca_dv, esca_scaled, escalation_1, escalation_2, escalation_3, escalation_4, treat, denial, MA, MA_scaled, GovTrust, NewsTrust, IntTrust, NC, NC_scaled, Military.Service, Read.FP, reputation, reputation_scaled, ambiguity, insulting, war, airstrike, sanctions, diplomacy, reputation_1, reputation_2, reputation_3, adversary, denial_adversary, overt_nonadversary)
df_res[] <- lapply(df_res, as.numeric)
df_res = df_res[!is.na(df_res$esca_scaled),]
df_res = df_res[complete.cases(df_res),]
write.csv(df_res, "2X2Data_Final.csv")

med_ma = median(df_res$MA_scaled)
df_res_dove = df_res[df_res$MA_scaled < med_ma,]
df_res_hawk = df_res[df_res$MA_scaled > med_ma,]

df_res$denial_adversary_2 = df_res$denial_2*df_res$adversary_2

#Continuous DV with and without controls
m_den = lm(esca_scaled ~ denial_2 + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)
m_adv= lm(esca_scaled ~ denial_2 + adversary_2 + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)
m_denXadv= lm(esca_scaled ~ denial_2 + adversary_2 + denial_adversary_2 + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)

stargazer(m_den,m_adv,m_denXadv, type = "html", out = "MainContDV.html", title = "Continuous DV", omit.stat = c("ll","rsq","adj.rsq","f","aic","ser"))
BROWSE("MainContDv.html")

#Estimates of Mediators
df_res$ambiguity = as.numeric(df_res$ambiguity)
df_res$insulting = as.numeric(df_res$insulting)

m_rep = lm(reputation_scaled ~ denial_2 + adversary_2 + denial_adversary_2 +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)
m_amb = lm(ambiguity ~ denial_2 + adversary_2 + denial_adversary_2 +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res) #method = "probit", Hess = TRUE)
m_ins = lm(insulting ~ denial_2 + adversary_2 + denial_adversary_2 +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)



stargazer(m_rep, m_amb, m_ins, type = 'html', out = "OnTheMeds.html",dep.var.labels = 
            (c("Reputation", "Certainty", "Anxious", "Insulted", "Angry")),
          omit.stat = c("ll","rsq","adj.rsq","f","aic","ser"),
          title = "Testing Treatments on the Mediators")
BROWSE("OnTheMeds.html")

#Effects of Mediators on Escalation
m2_rep = lm(esca_scaled ~ reputation_scaled + denial_2 + adversary_2 + denial_adversary_2 +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)
m2_amb = lm(esca_scaled ~ ambiguity + denial_2 + adversary_2 + denial_adversary_2 +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)
m2_ins = lm(esca_scaled ~ insulting + denial_2 + adversary_2 + denial_adversary_2 +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)

m2_all = lm(esca_scaled ~ reputation_scaled + ambiguity + insulting + denial_2 + adversary_2 + denial_adversary_2 +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)

stargazer(m2_rep, m2_amb, m2_ins, m2_all, type = 'html',
          out = "MedsOnEsca.html", covariate.labels = 
            (c("Reputation", "Certainty", "Insulted")),
          title = "Testing Mediators on the DV",
          omit.stat = c("ll","rsq","adj.rsq","f","aic","ser"))
BROWSE("MedsOnEsca.html")


###Mediation Analysis###
m_rep = lm(reputation_scaled ~ treat +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)
m_amb = lm(ambiguity ~ treat +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res) #method = "probit", Hess = TRUE)
m_ins = lm(insulting ~ treat +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)

m2_rep = lm(esca_scaled ~ reputation_scaled + treat +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)
m2_amb = lm(esca_scaled ~ ambiguity + treat +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)
m2_ins = lm(esca_scaled ~ insulting + treat +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)



med.rep <- mediate(m_rep, m2_rep, treat = "treat", mediator = "reputation_scaled", #Need to match variables here with models above
                   robustSE = TRUE, sims = 1000)
summary(med.rep)
plot(med.rep)

med.amb <- mediate(m_amb, m2_amb, treat = "treat", mediator = "ambiguity",
                   robustSE = TRUE, sims = 1000, boot = TRUE)
summary(med.amb)
plot(med.amb)


med.ins <- mediate(m_ins, m2_ins, treat = "treat", mediator = "emotions_2",
                   robustSE = TRUE, sims = 100, control.value = 0, treat.value = 3)
summary(med.ins)
plot(med.ins)


##Moderated Mediation##
se <- function(x) sqrt(var(x) / length(x))

#MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP
m2_all = lm(esca_scaled ~ reputation_scaled + ambiguity + emotions_1 + emotions_2 + emotions_3 + denial_2 + adversary_2 + denial_adversary_2 + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res)

m_rep = lm(reputation_scaled ~ denial_2 + adversary_2 + denial_adversary_2, data = df_res)
m_amb = lm(ambiguity ~ denial_2 + adversary_2 + denial_adversary_2 + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res) #method = "probit", Hess = TRUE)
m_anx = glm(emotions_1 ~ denial_2 + adversary_2 + denial_adversary_2 + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res, family = "binomial")
m_ins = glm(emotions_2 ~ denial_2 + adversary_2 + denial_adversary_2 + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res, family = "binomial")
m_ang = glm(emotions_3 ~ denial_2 + adversary_2 + denial_adversary_2 + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_res, family = "binomial")


df_0 = df_res
df_1 = df_res
df_2 = df_res
df_3 = df_res

df_0$denial_2 = 0
df_0$adversary_2 = 0
df_0$denial_adversary_2 = df_0$denial_2*df_0$adversary_2

df_1$denial_2 = 1
df_1$adversary_2 = 0
df_1$denial_adversary_2 = df_1$denial_2*df_1$adversary_2

df_2$denial_2 = 0
df_2$adversary_2 = 1
df_2$denial_adversary_2 = df_2$denial_2*df_2$adversary_2

df_3$denial_2 = 1
df_3$adversary_2 = 1
df_3$denial_adversary_2 = df_3$denial_2*df_3$adversary_2

df_0$reputation_scaled = predict(m_rep, newdata = df_0)
df_1$reputation_scaled = predict(m_rep, newdata = df_1)
df_2$reputation_scaled = predict(m_rep, newdata = df_2)
df_3$reputation_scaled = predict(m_rep, newdata = df_3)

df_0$ambiguity = predict(m_amb, newdata = df_0)
df_1$ambiguity = predict(m_amb, newdata = df_1)
df_2$ambiguity = predict(m_amb, newdata = df_2)
df_3$ambiguity = predict(m_amb, newdata = df_3)

df_0$emotions_1 = predict(m_anx, newdata = df_0)
df_1$emotions_1 = predict(m_anx, newdata = df_1)
df_2$emotions_1 = predict(m_anx, newdata = df_2)
df_3$emotions_1 = predict(m_anx, newdata = df_3)

df_0$emotions_2 = predict(m_ins, newdata = df_0)
df_1$emotions_2 = predict(m_ins, newdata = df_1)
df_2$emotions_2 = predict(m_ins, newdata = df_2)
df_3$emotions_2 = predict(m_ins, newdata = df_3)

df_0$emotions_3 = predict(m_ang, newdata = df_0)
df_1$emotions_3 = predict(m_ang, newdata = df_1)
df_2$emotions_3 = predict(m_ang, newdata = df_2)
df_3$emotions_3 = predict(m_ang, newdata = df_3)

df_rep0 = df_2
df_rep0$reputation_scaled = df_0$reputation_scaled

#Denial = 0, Adversary = 1
rep10 = predict(m2_all, newdata = df_2)
#Denial = 0, K = 0, !K = 1
rep00 = predict(m2_all, newdata = df_rep0)

rep0 = rep10 - rep00

df_rep1 = df_3
df_rep1$reputation_scaled = df_1$reputation_scaled

#Denial = 1, Adversary = 1
rep11 = predict(m2_all, newdata = df_3)  
#Denial = 1, K = 0, !K = 1
rep01 = predict(m2_all, newdata = df_rep1)

rep1 = rep11 - rep01

mean((rep0 + rep1)/2)
se((rep0 + rep1)/2)






df_amb0 = df_2
df_amb0$ambiguity = df_0$ambiguity

#Denial = 0, Adversary = 1
amb10 = predict(m2_all, newdata = df_2)
#Denial = 0, K = 0, !K = 1
amb00 = predict(m2_all, newdata = df_amb0)

amb0 = amb10 - amb00

df_amb1 = df_3
df_amb1$ambiguity = df_1$ambiguity

#Denial = 1, Adversary = 1
amb11 = predict(m2_all, newdata = df_3)  
#Denial = 1, K = 0, !K = 1
amb01 = predict(m2_all, newdata = df_amb1)

amb1 = amb11 - amb01

mean((amb0 + amb1)/2)
se((amb0 + amb1)/2)





