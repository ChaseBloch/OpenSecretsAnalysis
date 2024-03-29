library(qualtRics)
library(stargazer)
library(ggplot2)
library(dplyr)
library(httr)
library(data.table)
library(vtable)
library(stargazer)


###Importing and Processing Survey Data###

# Register API credentials
qualtrics_api_credentials(api_key = "8BhFgrQIJ9YIDgPHwx78h4Sid2VI2tKLDYXQEULF", 
                          base_url = "pennstate.ca1.qualtrics.com",
                          install = TRUE,
                          overwrite = TRUE)

setwd("C:/Users/chase/GDrive/GD_Work/Dissertation/JointPaper/OpenSecretsAnalysis")

# Import Survey and correct variable names
surveys <- all_surveys()
df <- fetch_survey(surveyID = surveys$id[1],verbose =TRUE,force_request = TRUE)
#df2 <- fetch_survey(surveyID = surveys$id[2],verbose =TRUE,force_request = TRUE)
#df <- rbindlist(list(df1, df2), fill = TRUE)
names(df) <- gsub(":", "", names(df))
names(df) <- make.names(names(df), unique=TRUE)
df = as.data.frame(df)
###Attention Checks###

# Drop based on first attention check
df <- df[
  !is.na(df$AC1_1) & 
    !is.na(df$AC1_2) & 
    df$Status != "Survey Preview" & df$Finished == "TRUE",]

# Convert string NAs to Actual NAs
df[] <- lapply(df, function(x) {
  is.na(levels(x)) <- levels(x) == "NA"
  x
})

# Drop based on second attention check
df = df[df$i.AC2 == "The United States and Iran",]

# Keep only respondents who finished and were from Lucid (no previews)
df <- df[df$Status == "IP Address",]
df <- df[!is.na(df$Status),]

# Check what percentage of respondents got the treatment check correct
#iran_overt = nrow(df[df$i.AC3 == "Iran claimed responsibility for the attack." & !is.na(df$i.overt)])
#iran_covert = nrow(df[df$i.AC3 == "Iran denied involvement in the attack." & !is.na(df$i.denial)])
#qatar_overt = nrow(df[df$q.AC3 == "Qatar claimed responsibility for the attack." & !is.na(df$q.overt)])
#qatar_covert = nrow(df[df$q.AC3 == "Qatar denied involvement in the attack." & !is.na(df$q.denial)]) 
#perc3 = (iran_overt + iran_covert)/492

# Create denial and adversary variables
df$denial = ifelse(!is.na(df$i.denial), 1,0)
df$adversary = ifelse(!is.na(df$i.AC2), 1,0)

###Combining Variables From Both Surveys###

df = df %>% mutate(escalation_1 = coalesce(i.escalation_1))
df = df %>% mutate(escalation_2 = coalesce(i.escalation_2))
df = df %>% mutate(escalation_3 = coalesce(i.escalation_3))
df = df %>% mutate(escalation_4 = coalesce(i.escalation_4))

df = df %>% mutate(reputation_1 = coalesce(i.reputation_1))
df = df %>% mutate(reputation_2 = coalesce(i.reputation_2))
df = df %>% mutate(reputation_3 = coalesce(i.reputation_3))

df = df %>% mutate(insulting = coalesce(i.emotions))

df = df %>% mutate(ambiguity = coalesce(i.ambiguity))

###Creating Indices for Variables###

# Factor escalation variables
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

# Scale and combine escalation variables
df$esca_scaled = (scale(as.numeric(df$escalation_1)) + 
                    scale(as.numeric(df$escalation_2)) + 
                    scale(as.numeric(df$escalation_3)) + 
                    scale(as.numeric(df$escalation_4)))/4
df$esca_scaled = (df$esca_scaled - min(df$esca_scaled, na.rm = TRUE))
df$esca_scaled = df$esca_scaled/max(df$esca_scaled,na.rm = TRUE)*100

# Create individual escalation variables for each answer
df$war = df$escalation_4
df$airstrike = df$escalation_3
df$sanctions = df$escalation_2
df$diplomacy = df$escalation_1

# Scale Military Assertiveness
df$MA = (6 - as.numeric(df$MA1)) + as.numeric(df$MA2) + (6 - as.numeric(df$MA3))
df$MA_scaled = (scale(6 - as.numeric(df$MA1))) + 
  scale(as.numeric(df$MA2)) + 
  scale((6 - as.numeric(df$MA3)))/3

# Scale National Chauvinism
df$NC = (as.numeric(df$NC1) + as.numeric(df$NC2))
df$NC_scaled = scale(as.numeric(df$NC1) + scale(as.numeric(df$NC2)))/2

# Factor reputation variables
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

# Scale and combine reputation variables
df$reputation_scaled = (scale(as.numeric(df$reputation_1)) + scale(as.numeric(df$reputation_2)) + scale(as.numeric(df$reputation_3)))/3
df$reputation_scaled = (df$reputation_scaled - min(df$reputation_scaled, na.rm = TRUE))
df$reputation_scaled = df$reputation_scaled/max(df$reputation_scaled,na.rm = TRUE)*100

# Convert dispositional controls to numeric values
df$GovTrust = 6-as.numeric(df$GovTrust)
df$NewsTrust = 6-as.numeric(df$NewsTrust)
df$IntTrust = 6-as.numeric(df$IntTrust)
df$Read.FP = 6-as.numeric(df$Read.FP)

###Cleaning demographic controls###

# Make male variable 0 and 1
df$male = df$gender
df$male[df$male == 2] = 0

# Change missing hhi data to NA
df$hhi[df$hhi == -3105] = NA

# Create hispanic indicator variable
df$hispanic[df$hispanic == 15] = NA
df$hispanic = ifelse(df$hispanic > 1, 1, 0)

# Create white and black indicator variables
df$white = ifelse(df$ethnicity == 1 & df$hispanic == 0, 1, 0)
df$black = ifelse(df$ethnicity == 2, 1, 0)

# Change missing education data to 0
df$education[df$education == -3105] = 0

# Create republican and democrat indicator variables (independent comparison)
df$republican = ifelse(
  df$political_party == 9 | 
    df$political_party == 10 | 
    df$political_party == 5 | 
    df$political_party == 8, 1, 0)
df$democrat = ifelse(
  df$political_party == 1 | 
    df$political_party == 2 | 
    df$political_party == 3 | 
    df$political_party == 6, 1, 0)

###Analysis###

# Summary Statistics
# Iran
df_sum = df %>% dplyr::select(esca_scaled, denial, adversary, reputation_scaled, insulting, ambiguity, MA_scaled,NC_scaled, GovTrust, 
                              NewsTrust, IntTrust, Read.FP)
df_sum$insulting = as.numeric(df$insulting)
df_sum$ambiguity = as.numeric(df$ambiguity)

stargazer(df_sum, summary = TRUE, type = 'html', out ='Figures2/Iran_Sum.html', digits = 2, 
          covariate.labels = c('Escalation Preference', 'Denial', 'Reputation', 'Insult', 
                               'Certainty', 'Militant Assertiveness', 'National Chauvinism', 
                               'Trust in Gov.', 'Trust in News', 'International Trust', 'Foreign Policy Interest'),
          summary.stat = c('n', 'mean', 'median', 'sd', 'min', 'max'))
BROWSE('Figures2/Iran_Sum.html')

# Iran Demographic Controls
df_sum_dem = df %>% dplyr::select(adversary, age, male, hhi, white, black, education, republican, democrat)

stargazer(df_sum_dem, summary = TRUE, type = 'html', out ='Figures2/Iran_Sum_Dem.html', digits = 2, 
          covariate.labels = c('Age', 'Male', 'Household Income', 'White', 'Black', 'Education','Republican', 'Democrat'),
          summary.stat = c('n', 'mean', 'median', 'sd', 'min', 'max'))
BROWSE('Figures2/Iran_Sum_Dem.html')


#Linear Models


df_res = df %>% dplyr::select(denial, adversary, esca_scaled, MA_scaled, GovTrust, 
                              NewsTrust, IntTrust, NC_scaled, Read.FP,
                              reputation_scaled, ambiguity, insulting, war, airstrike, sanctions, 
                              diplomacy, age, male, hhi, white, black, education, republican, democrat)
df_res[] <- lapply(df_res, as.numeric)
write.csv(df_res, "IranMoreCertainty_Final.csv")


###Bar Plot Iran####

# Subset for Iran
# Calculate means of relevant variables
means = c(mean(df_res$war[df_res$denial == 1],na.rm = TRUE), 
          mean(df_res$airstrike[df_res$denial == 1],na.rm = TRUE),
          mean(df_res$sanctions[df_res$denial == 1],na.rm = TRUE), 
          mean(df_res$diplomacy[df_res$denial == 1],na.rm = TRUE),
          mean(df_res$war[df_res$denial == 0],na.rm = TRUE), 
          mean(df_res$airstrike[df_res$denial == 0],na.rm = TRUE),
          mean(df_res$sanctions[df_res$denial == 0],na.rm = TRUE), 
          mean(df_res$diplomacy[df_res$denial == 0],na.rm = TRUE)
)

# Calculate standard deviations of relevant variables
sds = c(sd(df_res$war[df_res$denial == 1],na.rm = TRUE), 
        sd(df_res$airstrike[df_res$denial == 1],na.rm = TRUE),
        sd(df_res$sanctions[df_res$denial == 1],na.rm = TRUE), 
        sd(df_res$diplomacy[df_res$denial == 1],na.rm = TRUE),
        sd(df_res$war[df_res$denial == 0],na.rm = TRUE), 
        sd(df_res$airstrike[df_res$denial == 0],na.rm = TRUE),
        sd(df_res$sanctions[df_res$denial == 0],na.rm = TRUE), 
        sd(df_res$diplomacy[df_res$denial == 0],na.rm = TRUE)
)

# Calculate N of relevant variables
ns = c(length(df_res$war[df_res$denial == 1]), 
       length(df_res$airstrike[df_res$denial == 1]),
       length(df_res$sanctions[df_res$denial == 1]), 
       length(df_res$diplomacy[df_res$denial == 1]),
       length(df_res$war[df_res$denial == 0]), 
       length(df_res$airstrike[df_res$denial == 0]),
       length(df_res$sanctions[df_res$denial == 0]), 
       length(df_res$diplomacy[df_res$denial == 0])
)

# Create data frame with labels and data
treat = c("Covert", "Covert","Covert","Covert", "Overt", "Overt", "Overt", "Overt")
type = c("War", "Airstrike", "Sanctions", "Condemn", "War", "Airstrike", "Sanctions", "Condemn")
type = factor(type, levels = c(
  "Condemn",
  "Sanctions",
  "Airstrike",
  "War"
))
forgraph = data.frame(means, treat, type, sds, ns)
forgraph$error = qt(0.975, df=ns-1)*sds/sqrt(ns)

# Plot and save barplot for Iran
ggplot(aes(x = type, y = means, fill = treat), data = forgraph, group = factor(type)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x=type, ymin=means-error, ymax=means+error),
                width = 0.25,
                position = position_dodge(width = 0.9)) +
  xlab("") +
  ylab("") +
  scale_y_discrete(limits = c(1,2,3,4,5),
                   labels = c("Strongly\nOppose (1)", 
                              "Somewhat\nOppose (2)",
                              "Neutral (3)", 
                              "Somewhat\nFavor (4)", 
                              "Strongly\nFavor (5)"),
                   expand = expansion(add = c(0,1))) +
  scale_fill_discrete(name = "", labels = c("Denial", "Overt")) +
  theme(axis.text.y = element_text(angle = 0, size = 12.5),
        axis.text.x = element_text(size = 12.5),
        legend.text = element_text(size = 12.5)) 
ggsave("Figures2/avg_escalation_iran.png", width = 6, height = 4, unit = "in")





