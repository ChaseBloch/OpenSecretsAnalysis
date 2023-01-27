library(qualtRics)
library(stargazer)
library(ggplot2)
library(dplyr)
library(httr)
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
df1 <- fetch_survey(surveyID = surveys$id[10],verbose =TRUE,force_request = TRUE)
df2 <- fetch_survey(surveyID = surveys$id[2],verbose =TRUE,force_request = TRUE)
df3 <- rbindlist(list(df1, df2), fill = TRUE)
names(df3) <- gsub(":", "", names(df3))
names(df3) <- make.names(names(df3), unique=TRUE)


###Attention Checks###



#Drop based on first attention check
df <- df3[!is.na(df3$AC1_1) & !is.na(df3$AC1_2) & df3$Status != "Survey Preview" & df3$Finished == "TRUE",]
df[] <- lapply(df, function(x) {
  is.na(levels(x)) <- levels(x) == "NA"
  x
})

#Drop based on second attention check
df = df[df$i.AC2 == "The United States and Iran" | df$q.AC2 == "The United States and Qatar",]

#Keep only respondents who finished and were from Lucid (no previews)
df <- df[df$Status == "IP Address",]
df <- df[!is.na(df$Status),]

#Check what percentage of respondents got the treatment check correct
iran_overt = nrow(df[df$i.AC3 == "Iran claimed responsibility for the attack." & !is.na(df$i.overt)])
iran_covert = nrow(df[df$i.AC3 == "Iran denied involvement in the attack." & !is.na(df$i.denial)])
qatar_overt = nrow(df[df$q.AC3 == "Qatar claimed responsibility for the attack." & !is.na(df$q.overt)])
qatar_covert = nrow(df[df$q.AC3 == "Qatar denied involvement in the attack." & !is.na(df$q.denial)]) 
perc3 = (iran_overt + iran_covert + qatar_overt + qatar_covert)/1431

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

#Created scaled escalation variable
df$esca_scaled = (scale(as.numeric(df$escalation_1)) + scale(as.numeric(df$escalation_2)) + scale(as.numeric(df$escalation_3)) + scale(as.numeric(df$escalation_4)))/4
df$esca_scaled = (df$esca_scaled - min(df$esca_scaled, na.rm = TRUE))
df$esca_scaled = df$esca_scaled/max(df$esca_scaled,na.rm = TRUE)*100

#Creating individual escalation variables for each answer
df$war = df$escalation_4
df$airstrike = df$escalation_3
df$sanctions = df$escalation_2
df$diplomacy = df$escalation_1



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
df$reputation_scaled = (df$reputation_scaled - min(df$reputation_scaled, na.rm = TRUE))
df$reputation_scaled = df$reputation_scaled/max(df$reputation_scaled,na.rm = TRUE)*100



df$GovTrust = 6-as.numeric(df$GovTrust)
df$NewsTrust = 6-as.numeric(df$NewsTrust)
df$IntTrust = 6-as.numeric(df$IntTrust)
df$Military.Service = 2-as.numeric(df$Military.Service)
df$Military.Service[df$Military.Service ==-1] = NA
df$Read.FP = 6-as.numeric(df$Read.FP)

###Cleaning demographic controls###
df$male = df$gender
df$male[df$male == 2] = 0

df$hhi[df$hhi == -3105] = NA

df$hispanic[df$hispanic == 15] = NA
df$hispanic = ifelse(df$hispanic > 1, 1, 0)

df$white = ifelse(df$ethnicity == 1 & df$hispanic == 0, 1, 0)
df$black = ifelse(df$ethnicity == 2, 1, 0)

df$education[df$education == -3105] = 0

df$republican = ifelse(df$political_party == 9 | df$political_party == 10 | df$political_party == 5 | df$political_party == 8, 1, 0)
df$democrat = ifelse(df$political_party == 1 | df$political_party == 2 | df$political_party == 3 | df$political_party == 6, 1, 0)

###Analysis###

#Linear Models
df_res = df %>% dplyr::select(denial, adversary, esca_scaled, MA_scaled, GovTrust, 
                              NewsTrust, IntTrust, NC_scaled, Military.Service, Read.FP,
                              reputation_scaled, ambiguity, insulting, war, airstrike, sanctions, 
                              diplomacy, age, male, hhi, white, black, education, republican, democrat)
df_res[] <- lapply(df_res, as.numeric)
#write.csv(df_res, "2X2Data_Final.csv")


###Bar Plot Iran####

df_i = df_res[df_res$adversary==1]

means = c(mean(df_i$war[df_i$denial == 1],na.rm = TRUE), 
          mean(df_i$airstrike[df_i$denial == 1],na.rm = TRUE),
          mean(df_i$sanctions[df_i$denial == 1],na.rm = TRUE), 
          mean(df_i$diplomacy[df_i$denial == 1],na.rm = TRUE),
          mean(df_i$war[df_i$denial == 0],na.rm = TRUE), 
                   mean(df_i$airstrike[df_i$denial == 0],na.rm = TRUE),
                   mean(df_i$sanctions[df_i$denial == 0],na.rm = TRUE), 
                   mean(df_i$diplomacy[df_i$denial == 0],na.rm = TRUE)
          )

sds = c(sd(df_i$war[df_i$denial == 1],na.rm = TRUE), 
          sd(df_i$airstrike[df_i$denial == 1],na.rm = TRUE),
          sd(df_i$sanctions[df_i$denial == 1],na.rm = TRUE), 
          sd(df_i$diplomacy[df_i$denial == 1],na.rm = TRUE),
          sd(df_i$war[df_i$denial == 0],na.rm = TRUE), 
          sd(df_i$airstrike[df_i$denial == 0],na.rm = TRUE),
          sd(df_i$sanctions[df_i$denial == 0],na.rm = TRUE), 
          sd(df_i$diplomacy[df_i$denial == 0],na.rm = TRUE)
)

ns = c(length(df_i$war[df_i$denial == 1]), 
        length(df_i$airstrike[df_i$denial == 1]),
        length(df_i$sanctions[df_i$denial == 1]), 
        length(df_i$diplomacy[df_i$denial == 1]),
        length(df_i$war[df_i$denial == 0]), 
        length(df_i$airstrike[df_i$denial == 0]),
        length(df_i$sanctions[df_i$denial == 0]), 
        length(df_i$diplomacy[df_i$denial == 0])
)


treat = c("Covert", "Covert","Covert","Covert", "Overt", "Overt", "Overt", "Overt")
type = c("War", "Airstrike", "Sanctions", "Diplomacy", "War", "Airstrike", "Sanctions", "Diplomacy")
type = factor(type, levels = c(
  "Diplomacy",
  "Sanctions",
  "Airstrike",
  "War"
))


forgraph = data.frame(means, treat, type, sds, ns)
forgraph$error = qt(0.975, df=ns-1)*sds/sqrt(ns)

ggplot(aes(x = type, y = means, fill = treat), data = forgraph, group = factor(type)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x=type, ymin=means-error, ymax=means+error),
                width = 0.25,
                position = position_dodge(width = 0.9)) +
  xlab("") +
  ylab("") +
#  ggtitle("Iran: Average Preference per Response Option") +
  scale_y_discrete(limits = c(1,2,3,4,5),
                   labels = c("Strongly\nOppose (1)", "Somewhat\nOppose (2)", "Neutral (3)", "Somewhat\nFavor (4)", "Strongly\nFavor (5)"),
                   expand = expansion(add = c(0,1))) +
  scale_fill_discrete(name = "", labels = c("Denial", "Overt")) +
  theme(axis.text.y = element_text(angle = 0, size = 12.5),
        axis.text.x = element_text(size = 12.5),
        legend.text = element_text(size = 12.5)) 
 

#ggsave("Figures/avg_escalation_iran.png", width = 6, height = 4, unit = "in")
####################
###Bar Plot Qatar###
df_q = df_res[df_res$adversary==0]

means = c(mean(df_q$war[df_q$denial == 1],na.rm = TRUE), 
          mean(df_q$airstrike[df_q$denial == 1],na.rm = TRUE),
          mean(df_q$sanctions[df_q$denial == 1],na.rm = TRUE), 
          mean(df_q$diplomacy[df_q$denial == 1],na.rm = TRUE),
          mean(df_q$war[df_q$denial == 0],na.rm = TRUE), 
          mean(df_q$airstrike[df_q$denial == 0],na.rm = TRUE),
          mean(df_q$sanctions[df_q$denial == 0],na.rm = TRUE), 
          mean(df_q$diplomacy[df_q$denial == 0],na.rm = TRUE)
)

sds = c(sd(df_q$war[df_q$denial == 1],na.rm = TRUE), 
        sd(df_q$airstrike[df_q$denial == 1],na.rm = TRUE),
        sd(df_q$sanctions[df_q$denial == 1],na.rm = TRUE), 
        sd(df_q$diplomacy[df_q$denial == 1],na.rm = TRUE),
        sd(df_q$war[df_q$denial == 0],na.rm = TRUE), 
        sd(df_q$airstrike[df_q$denial == 0],na.rm = TRUE),
        sd(df_q$sanctions[df_q$denial == 0],na.rm = TRUE), 
        sd(df_q$diplomacy[df_q$denial == 0],na.rm = TRUE)
)

ns = c(length(df_q$war[df_q$denial == 1]), 
       length(df_q$airstrike[df_q$denial == 1]),
       length(df_q$sanctions[df_q$denial == 1]), 
       length(df_q$diplomacy[df_q$denial == 1]),
       length(df_q$war[df_q$denial == 0]), 
       length(df_q$airstrike[df_q$denial == 0]),
       length(df_q$sanctions[df_q$denial == 0]), 
       length(df_q$diplomacy[df_q$denial == 0])
)


treat = c("Covert", "Covert","Covert","Covert", "Overt", "Overt", "Overt", "Overt")
type = c("War", "Airstrike", "Sanctions", "Diplomacy", "War", "Airstrike", "Sanctions", "Diplomacy")
type = factor(type, levels = c(
  "Diplomacy",
  "Sanctions",
  "Airstrike",
  "War"
))


forgraph = data.frame(means, treat, type, sds, ns)
forgraph$error = qt(0.975, df=ns-1)*sds/sqrt(ns)

ggplot(aes(x = type, y = means, fill = treat), data = forgraph, group = factor(type)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x=type, ymin=means-error, ymax=means+error),
                width = 0.25,
                position = position_dodge(width = 0.9)) +
  xlab("") +
  ylab("") +
  ggtitle("Qatar: Average Preference per Response Option") +
  scale_y_discrete(limits = c(1,2,3,4,5),
                   labels = c("Strongly\nOppose (1)", "Somewhat\nOppose (2)", "Neutral (3)", "Somewhat\nFavor (4)", "Strongly\nFavor (5)"),
                   expand = expansion(add = c(0,1))) +
  scale_fill_discrete(name = "", labels = c("Denial", "Overt")) +
  theme(axis.text.y = element_text(angle = 0, size = 12.5),
        axis.text.x = element_text(size = 12.5),
        legend.text = element_text(size = 12.5)) 


ggsave("avg_escalation_qatar.png", width = 6, height = 4, unit = "in")




