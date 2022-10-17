library(glm2)
library(mediation)
library(MASS)
library(dplyr)

setwd("C:/Users/chase/GDrive/GD_Work/Dissertation/JointPaper/OpenSecretsAnalysis")
df = read.csv("2X2Data_Final.csv")

df$ambiguity = as.factor(df$ambiguity)
df$insulting = as.factor(df$insulting)


df_i = df[df$adversary == 1,]


###Mediation Analysis with Dispositional Controls###
##########################
df_dis = df_i %>% dplyr::select(denial, esca_scaled, MA_scaled, GovTrust, 
                              NewsTrust, IntTrust, NC_scaled, Military.Service, Read.FP,
                              reputation_scaled, ambiguity, insulting)
df_dis = df_dis[complete.cases(df_dis),]


m_rep = lm(reputation_scaled ~ denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_dis)
m_amb = polr(ambiguity ~ denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_dis, method = "logistic", Hess = TRUE)
m_ins = polr(insulting ~ denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_dis, method = "logistic", Hess = TRUE)

m2_rep = lm(esca_scaled ~ reputation_scaled + denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_dis)
m2_amb = lm(esca_scaled ~ ambiguity + denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_dis)
m2_ins = lm(esca_scaled ~ insulting + denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Military.Service + Read.FP, data = df_dis)



med.rep <- mediate(m_rep, m2_rep, treat = "denial", mediator = "reputation_scaled", #Need to match variables here with models above
                   robustSE = TRUE, sims = 100)
summary(med.rep)
plot(med.rep)

med.amb <- mediate(m_amb, m2_amb, treat = "denial", mediator = "ambiguity", sims = 100, boot = TRUE)
summary(med.amb)
plot(med.amb)

med.ins <- mediate(m_ins, m2_ins, treat = "denial", mediator = "insulting", sims = 100, boot = TRUE)
summary(med.ins)
plot(med.ins)
##########################


###Mediation Analysis with no Controls###
##########################
df_dis = df_i %>% dplyr::select(denial, esca_scaled, ambiguity, insulting, reputation_scaled)
df_dis = df_dis[complete.cases(df_dis),]


m_rep = lm(reputation_scaled ~ denial, data = df_dis)
m_amb = polr(ambiguity ~ denial, data = df_dis, method = "logistic", Hess = TRUE)
m_ins = polr(insulting ~ denial, data = df_dis, method = "logistic", Hess = TRUE)

m2_rep = lm(esca_scaled ~ reputation_scaled + denial, data = df_dis)
m2_amb = lm(esca_scaled ~ ambiguity + denial, data = df_dis)
m2_ins = lm(esca_scaled ~ insulting + denial, data = df_dis)



med.rep <- mediate(m_rep, m2_rep, treat = "denial", mediator = "reputation_scaled", #Need to match variables here with models above
                   robustSE = TRUE, sims = 100)
summary(med.rep)
plot(med.rep)

med.amb <- mediate(m_amb, m2_amb, treat = "denial", mediator = "ambiguity", sims = 100, boot = TRUE)
summary(med.amb)
plot(med.amb)

med.ins <- mediate(m_ins, m2_ins, treat = "denial", mediator = "insulting", sims = 100, boot =TRUE)
summary(med.ins)
plot(med.ins)
##########################