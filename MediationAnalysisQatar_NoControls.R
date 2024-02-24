library(glm2)
library(mediation)
library(MASS)
library(dplyr)
library(stargazer)
library(httr)
library(ggplot2)

set.seed(1234)

setwd("G:/My Drive/GD_Work/Dissertation/JointPaper/OpenSecretsAnalysis")
df = read.csv("2X2Data_Final.csv")

# Factor ambiguity and insulting
df$ambiguity = as.factor(df$ambiguity)
df$insulting = as.factor(df$insulting)

# Subset Qatar data
df_i = df[df$adversary == 0,]

###Mediation Analysis with Dispositional Controls###
##########################

# Subset relevant variables and remove NA values
df_dis = df_i %>% dplyr::select(denial, esca_scaled, MA_scaled, GovTrust, 
                                NewsTrust, IntTrust, NC_scaled, Military.Service, Read.FP,
                                reputation_scaled, ambiguity, insulting)
df_dis = df_dis[complete.cases(df_dis),]

# Run models with mediators as the DV
m_rep = lm(reputation_scaled ~ denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP, data = df_dis)
m_amb = polr(ambiguity ~ denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP, data = df_dis, method = "logistic", Hess = TRUE)
m_ins = polr(insulting ~ denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP, data = df_dis, method = "logistic", Hess = TRUE)

# Run models with mediators as the IVs
m2_rep = lm(esca_scaled ~ reputation_scaled + denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP, data = df_dis)
m2_amb = lm(esca_scaled ~ ambiguity + denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP, data = df_dis)
m2_ins = lm(esca_scaled ~ insulting + denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP, data = df_dis)

# Run mediate function for reputation
med.rep_dis <- mediate(m_rep, m2_rep, treat = "denial", mediator = "reputation_scaled", sims = 500, boot = FALSE)
summary(med.rep_dis)
pdf(file = 'FinalScripts&Figures/MediationRepDisQatar.pdf')
par(mar = c(4.1, 4.4, 4.1, 1.9), xaxs="i", yaxs="i")
plot(med.rep_dis)
dev.off()

# Run mediate function for ambiguity (certainty)
med.amb_dis <- mediate(m_amb, m2_amb, treat = "denial", mediator = "ambiguity", sims = 500, boot = FALSE)
summary(med.amb_dis)
pdf(file = 'FinalScripts&Figures/MediationCertDisQatar.pdf')
par(mar = c(4.1, 4.4, 4.1, 1.9), xaxs="i", yaxs="i")
plot(med.amb_dis)
dev.off()

# Run mediate function for insult
med.ins_dis <- mediate(m_ins, m2_ins, treat = "denial", mediator = "insulting", sims = 500, boot = FALSE)
summary(med.ins_dis)
pdf(file = 'FinalScripts&Figures/MediationInsDisQatar.pdf')
par(mar = c(4.1, 4.4, 4.1, 1.9), xaxs="i", yaxs="i")
plot(med.ins_dis)
dev.off()

# Calculate estimate and confidence bounds for proportion mediated
variable = c("Reputation", "Certainty", "Insult")
estimate = c(med.rep_dis$n0, med.amb_dis$n0, med.ins_dis$n0)
lower = c(med.rep_dis$n0.ci[1], med.amb_dis$n0.ci[1], med.ins_dis$n0.ci[1])
upper = c(med.rep_dis$n0.ci[2], med.amb_dis$n0.ci[2], med.ins_dis$n0.ci[2])

med_props = data.frame(variable, estimate, lower, upper)

# Create and save plot for proportion mediated
p = ggplot(med_props, 
           aes(x = variable, y = estimate)) +
  geom_hline(yintercept = 0, 
             colour = gray(1/2), lty = 2) +
  geom_point(aes(x = variable, 
                 y = estimate)) + 
  geom_linerange(aes(x = variable, 
                     ymin = lower,
                     ymax = upper),
                 lwd = 1) +
  geom_text(aes(x = variable, 
                y = estimate,
                label = round(estimate,2)),
            vjust = 2) +
  ggtitle("") +
  xlab("") +
  ylab("")+
  theme(axis.text.y = element_text(angle = 0, size = 12.5),
        axis.text.x = element_text(size = 12.5)) +
  coord_flip()

ggsave("FinalScripts&Figures/prop_med_qatar.png", width = 6, height = 4, unit = "in")

##########################
###Mediation Analysis with demographic Controls###
##########################

# Subset relevant variables and remove NA values
df_dis = df_i %>% dplyr::select(denial, esca_scaled, ambiguity, insulting, reputation_scaled, age, male, hhi, white, education, republican, democrat)
df_dis = df_dis[complete.cases(df_dis),]

# Run models with mediators as the DV
m_rep = lm(reputation_scaled ~ denial + age + male + hhi + white + education + republican + democrat, data = df_dis)
m_amb = polr(ambiguity ~ denial + age + male + hhi + white + education + republican + democrat, data = df_dis, method = "logistic", Hess = TRUE)
m_ins = polr(insulting ~ denial + age + male + hhi + white + education + republican + democrat, data = df_dis, method = "logistic", Hess = TRUE)

# Run models with mediators as the IVs
m2_rep = lm(esca_scaled ~ reputation_scaled + denial + age + male + hhi + white + education + republican + democrat, data = df_dis)
m2_amb = lm(esca_scaled ~ ambiguity + denial + age + male + hhi + white + education + republican + democrat, data = df_dis)
m2_ins = lm(esca_scaled ~ insulting + denial + age + male + hhi + white + education + republican + democrat, data = df_dis)

# Run mediate function for reputation
med.rep_dem <- mediate(m_rep, m2_rep, treat = "denial", mediator = "reputation_scaled", sims = 500, boot = FALSE)
summary(med.rep_dem)
pdf(file = 'FinalScripts&Figures/MediationRepDemQatar.pdf')
par(mar = c(4.1, 4.4, 4.1, 1.9), xaxs="i", yaxs="i")
plot(med.rep_dem)
dev.off()

# Run mediate function for ambiguity (certainty)
med.amb_dem <- mediate(m_amb, m2_amb, treat = "denial", mediator = "ambiguity", sims = 500, boot = FALSE)
summary(med.amb_dem)
pdf(file = 'FinalScripts&Figures/MediationCertDemQatar.pdf')
par(mar = c(4.1, 4.4, 4.1, 1.9), xaxs="i", yaxs="i")
plot(med.amb_dem)
dev.off()

# Run mediate function for insult
med.ins_dem <- mediate(m_ins, m2_ins, treat = "denial", mediator = "insulting", sims = 500, boot = FALSE)
summary(med.ins_dem)
pdf(file = 'FinalScripts&Figures/MediationInsDemQatar.pdf')
par(mar = c(4.1, 4.4, 4.1, 1.9), xaxs="i", yaxs="i")
plot(med.ins_dem)
dev.off()
##########################

# Create HTML table of full mediation results
med_table = paste('<!DOCTYPE html>
  <html>
  <style>
  table
  th{text-align: left;}
</style>
  <body>
  
  <h2>Qatar Experiment: Mediation Effects</h2>
  
  <table style="width:35%">
  <tr><th colspan="6" style = "border-bottom: 1px solid black"></th></tr>
  <tr align = "left">
    <th style="width:15%"></th>
    <th style="width:15%">Mediator</th>
    <th style="width:15%">ACME</th>
    <th style="width:15%">Direct Effect</th>
    <th style="width:15%">Total Effect</th>
    <th style="width:15%">Proportion Mediated</th>
  </tr>
  <tr><th colspan="6" style = "border-bottom: 1px solid black"></th></tr>
  <tr>
    <th rowspan ="3">Dispositional Controls</th> 
    <td >Reputation</td>
    <td>', round(med.rep_dis$d0,2), '(',round(med.rep_dis$d0.ci[1],2),',',round(med.rep_dis$d0.ci[2],2),')','</td>
    <td>', round(med.rep_dis$z0,2),'(',round(med.rep_dis$z0.ci[1],2),',',round(med.rep_dis$z0.ci[2],2),')','</td>
    <td>', round(med.rep_dis$tau.coef,2),'(',round(med.rep_dis$tau.ci[1],2),',',round(med.rep_dis$tau.ci[2],2),')','</td>
    <td>', round(med.rep_dis$n0,2),' </td>
  </tr>
  <tr style="background-color: #D6EEEE">
    <td>Certainty</td>
    <td>', round(med.amb_dis$d0,2), '(',round(med.amb_dis$d0.ci[1],2),',',round(med.amb_dis$d0.ci[2],2),')','</td>
    <td>', round(med.amb_dis$z0,2),'(',round(med.amb_dis$z0.ci[1],2),',',round(med.amb_dis$z0.ci[2],2),')','</td>
    <td>', round(med.amb_dis$tau.coef,2),'(',round(med.amb_dis$tau.ci[1],2),',',round(med.amb_dis$tau.ci[2],2),')','</td>
    <td>', round(med.amb_dis$n0,2),' </td>
  </tr>
  <tr>
    <td>Insulting</td>
   <td>', round(med.ins_dis$d0,2),'(',round(med.ins_dis$d0.ci[1],2),',',round(med.ins_dis$d0.ci[2],2),')','</td>
    <td>', round(med.ins_dis$z0,2),'(',round(med.ins_dis$z0.ci[1],2),',',round(med.ins_dis$z0.ci[2],2),')','</td>
    <td>', round(med.ins_dis$tau.coef,2),'(',round(med.ins_dis$tau.ci[1],2),',',round(med.ins_dis$tau.ci[2],2),')','</td>
    <td>', round(med.ins_dis$n0,2),' </td>
  </tr>
  <tr><th colspan="6" style = "border-bottom: 1px solid black"></th></tr>
  <tr>
    <th rowspan ="3">Demographic Controls</th> 
    <td>Reputation</td>
    <td>', round(med.rep_dem$d0,2), '(',round(med.rep_dem$d0.ci[1],2),',',round(med.rep_dem$d0.ci[2],2),')','</td>
    <td>', round(med.rep_dem$z0,2),'(',round(med.rep_dem$z0.ci[1],2),',',round(med.rep_dem$z0.ci[2],2),')','</td>
    <td>', round(med.rep_dem$tau.coef,2),'(',round(med.rep_dem$tau.ci[1],2),',',round(med.rep_dem$tau.ci[2],2),')','</td>
    <td>', round(med.rep_dem$n0,2),' </td>
  </tr>
  <tr style="background-color: #D6EEEE">
    <td>Certainty</td>
    <td>', round(med.amb_dem$d0,2), '(',round(med.amb_dem$d0.ci[1],2),',',round(med.amb_dem$d0.ci[2],2),')','</td>
    <td>', round(med.amb_dem$z0,2),'(',round(med.amb_dem$z0.ci[1],2),',',round(med.amb_dem$z0.ci[2],2),')','</td>
    <td>', round(med.amb_dem$tau.coef,2),'(',round(med.amb_dem$tau.ci[1],2),',',round(med.amb_dem$tau.ci[2],2),')','</td>
    <td>', round(med.amb_dem$n0,2),' </td>
  </tr>
  <tr>
    <td>Insulting</td>
    <td>', round(med.ins_dem$d0,2), '(',round(med.ins_dem$d0.ci[1],2),',',round(med.ins_dem$d0.ci[2],2),')','</td>
    <td>', round(med.ins_dem$z0,2),'(',round(med.ins_dem$z0.ci[1],2),',',round(med.ins_dem$z0.ci[2],2),')','</td>
    <td>', round(med.ins_dem$tau.coef,2),'(',round(med.ins_dem$tau.ci[1],2),',',round(med.ins_dem$tau.ci[2],2),')','</td>
    <td>', round(med.ins_dem$n0,2),' </td>
  </tr>
  <tr><th colspan="6" style = "border-bottom: 1px solid black"></th></tr>
  </table>
  
  <p></p>
  
  </body>
  </html>', sep =" ")
write(med_table, "FinalScripts&Figures/med_table_qatar.html")
BROWSE("FinalScripts&Figures/med_table_qatar.html")


# Multi-mediation Imai and Yamamoto
# Ambiguity
df_dis = df_i %>% dplyr::select(denial, esca_scaled, MA_scaled, GovTrust, 
                                NewsTrust, IntTrust, NC_scaled, Read.FP,
                                reputation_scaled, ambiguity, insulting, age, male, 
                                hhi, white, education, republican, democrat)
df_dis = df_dis[complete.cases(df_dis),]

df_dis$ambiguity = as.numeric(df_dis$ambiguity)
df_dis$insulting = as.numeric(df_dis$insulting)

covariates = c('MA_scaled', 'GovTrust', 'NewsTrust', 'IntTrust', 'NC_scaled', 'Read.FP')
meds = c('reputation_scaled','insulting')
mm_amb <- multimed("esca_scaled", "ambiguity", meds, "denial", covariates, 
                   data=df_dis, sims=1000)
summary(mm_amb)

# Reputation
covariates = c('MA_scaled', 'GovTrust', 'NewsTrust', 'IntTrust', 'NC_scaled', 'Read.FP')
meds = c('ambiguity','insulting')
mm_rep <- multimed("esca_scaled", "reputation_scaled", meds, "denial", covariates, 
                   data=df_dis, sims=1000)
summary(mm_rep)

# Insulting
covariates = c('MA_scaled', 'GovTrust', 'NewsTrust', 'IntTrust', 'NC_scaled', 'Read.FP')
meds = c('ambiguity', 'reputation_scaled')
mm_ins <- multimed("esca_scaled", "insulting", meds, "denial", covariates, 
                   data=df_dis, sims=1000)
summary(mm_ins)

df$ambiguity = as.factor(df$ambiguity)
df$insulting = as.factor(df$insulting)

