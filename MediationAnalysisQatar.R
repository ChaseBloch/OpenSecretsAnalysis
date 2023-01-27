library(glm2)
library(mediation)
library(MASS)
library(dplyr)
library(stargazer)
library(httr)
library(ggplot2)

setwd("C:/Users/chase/GDrive/GD_Work/Dissertation/JointPaper/OpenSecretsAnalysis")
df = read.csv("2X2Data_Final.csv")

#df$ambiguity = as.factor(df$ambiguity)
df$insulting = as.factor(df$insulting)


df_i = df[df$adversary == 0,]


###Mediation Analysis with Dispositional Controls###
##########################
df_dis = df_i %>% dplyr::select(denial, esca_scaled, MA_scaled, GovTrust, 
                                NewsTrust, IntTrust, NC_scaled, Military.Service, Read.FP,
                                reputation_scaled, ambiguity, insulting)
df_dis = df_dis[complete.cases(df_dis),]


m_rep = lm(reputation_scaled ~ denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP, data = df_dis)
m_amb = lm(ambiguity ~ denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP, data = df_dis)
m_ins = polr(insulting ~ denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP, data = df_dis, method = "logistic", Hess = TRUE)

m2_rep = lm(esca_scaled ~ reputation_scaled + denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP, data = df_dis)
m2_amb = lm(esca_scaled ~ ambiguity + denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP, data = df_dis)
m2_ins = lm(esca_scaled ~ insulting + denial +  MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP, data = df_dis)



med.rep_dis <- mediate(m_rep, m2_rep, treat = "denial", mediator = "reputation_scaled", #Need to match variables here with models above
                       robustSE = TRUE, sims = 200)
summary(med.rep_dis)
plot(med.rep_dis)

med.amb_dis <- mediate(m_amb, m2_amb, treat = "denial", mediator = "ambiguity", sims = 200, robustSE = TRUE)
summary(med.amb_dis)
plot(med.amb_dis)

med.ins_dis <- mediate(m_ins, m2_ins, treat = "denial", mediator = "insulting", sims = 200, boot = TRUE)
summary(med.ins_dis)
plot(med.ins_dis)

variable = c("Reputation", "Certainty", "Insult")
estimate = c(med.rep_dis$n0, med.amb_dis$n0, med.ins_dis$n0)
lower = c(med.rep_dis$n0.ci[1], med.amb_dis$n0.ci[1], med.ins_dis$n0.ci[1])
upper = c(med.rep_dis$n0.ci[2], med.amb_dis$n0.ci[2], med.ins_dis$n0.ci[2])

med_props = data.frame(variable, estimate, lower, upper)


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

ggsave("Figures/prop_med_qatar.png", width = 6, height = 4, unit = "in")
##########################


###Mediation Analysis with no Controls###
##########################
df_dis = df_i %>% dplyr::select(denial, esca_scaled, ambiguity, insulting, reputation_scaled)
df_dis = df_dis[complete.cases(df_dis),]


m_rep = lm(reputation_scaled ~ denial, data = df_dis)
m_amb = lm(ambiguity ~ denial, data = df_dis)
m_ins = polr(insulting ~ denial, data = df_dis, method = "logistic", Hess = TRUE)

m2_rep = lm(esca_scaled ~ reputation_scaled + denial, data = df_dis)
m2_amb = lm(esca_scaled ~ ambiguity + denial, data = df_dis)
m2_ins = lm(esca_scaled ~ insulting + denial, data = df_dis)



med.rep <- mediate(m_rep, m2_rep, treat = "denial", mediator = "reputation_scaled", #Need to match variables here with models above
                   robustSE = TRUE, sims = 200)
summary(med.rep)
plot(med.rep)

med.amb <- mediate(m_amb, m2_amb, treat = "denial", mediator = "ambiguity", sims = 200, robustSE = TRUE)
summary(med.amb)
plot(med.amb)

med.ins <- mediate(m_ins, m2_ins, treat = "denial", mediator = "insulting", sims = 200, boot =TRUE)
summary(med.ins)
plot(med.ins)
##########################


###Mediation Analysis with demographic Controls###
##########################
df_dis = df_i %>% dplyr::select(denial, esca_scaled, ambiguity, insulting, reputation_scaled, age, male, hhi, white, education, republican, democrat)
df_dis = df_dis[complete.cases(df_dis),]


m_rep = lm(reputation_scaled ~ denial + age + male + hhi + white + education + republican + democrat, data = df_dis)
m_amb = lm(ambiguity ~ denial + age + male + hhi, white + education + republican + democrat, data = df_dis)
m_ins = polr(insulting ~ denial + age + male + hhi + white + education + republican + democrat, data = df_dis, method = "logistic", Hess = TRUE)

m2_rep = lm(esca_scaled ~ reputation_scaled + denial + age + male + hhi + white + education + republican + democrat, data = df_dis)
m2_amb = lm(esca_scaled ~ ambiguity + denial + age + male + hhi + white + education + republican + democrat, data = df_dis)
m2_ins = lm(esca_scaled ~ insulting + denial + age + male + hhi + white + education + republican + democrat, data = df_dis)



med.rep_dem <- mediate(m_rep, m2_rep, treat = "denial", mediator = "reputation_scaled", #Need to match variables here with models above
                       robustSE = TRUE, sims = 200)
summary(med.rep_dem)
plot(med.rep_dem)

med.amb_dem <- mediate(m_amb, m2_amb, treat = "denial", mediator = "ambiguity", sims = 200, robustSE = TRUE)
summary(med.amb_dem)
plot(med.amb_dem)

med.ins_dem <- mediate(m_ins, m2_ins, treat = "denial", mediator = "insulting", sims = 200, boot =TRUE)
summary(med.ins_dem)
plot(med.ins_dem)
##########################

med_table = paste('<!DOCTYPE html>
  <html>
  <style>
  table
  th{text-align: left;}
</style>
  <body>
  
  <h2>Qatar: Mediation Effects</h2>
  
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
    <th rowspan ="3"> Dispositional Controls</th> 
    <td >Reputation</td>
    <td>', round(med.rep_dis$d0,3),'</td>
    <td>', round(med.rep_dis$z0,3),'</td>
    <td>', round(med.rep_dis$tau.coef,3),'</td>
    <td>', round(med.rep_dis$n0,3),' </td>
  </tr>
  <tr style="background-color: #D6EEEE">
    <td>Ambiguity</td>
    <td>', round(med.amb_dis$d0,3),'</td>
    <td>', round(med.amb_dis$z0,3),'</td>
    <td>', round(med.amb_dis$tau.coef,3),'</td>
    <td>', round(med.amb_dis$n0,3),' </td>
  </tr>
  <tr>
    <td>Insulting</td>
   <td>', round(med.ins_dis$d0,3),'</td>
    <td>', round(med.ins_dis$z0,3),'</td>
    <td>', round(med.ins_dis$tau.coef,3),'</td>
    <td>', round(med.ins_dis$n0,3),' </td>
  </tr>
  <tr><th colspan="6" style = "border-bottom: 1px solid black"></th></tr>
  <tr>
    <th rowspan ="3"> No Controls</th> 
    <td>Reputation</td>
    <td>', round(med.rep$d0,3),'</td>
    <td>', round(med.rep$z0,3),'</td>
    <td>', round(med.rep$tau.coef,3),'</td>
    <td>', round(med.rep$n0,3),' </td>
  </tr>
  <tr style="background-color: #D6EEEE">
    <td>Ambiguity</td>
    <td>', round(med.amb$d0,3),'</td>
    <td>', round(med.amb$z0,3),'</td>
    <td>', round(med.amb$tau.coef,3),'</td>
    <td>', round(med.amb$n0,3),' </td>
  </tr>
  <tr>
    <td>Insulting</td>
   <td>', round(med.ins$d0,3),'</td>
    <td>', round(med.ins$z0,3),'</td>
    <td>', round(med.ins$tau.coef,3),'</td>
    <td>', round(med.ins$n0,3),' </td>
  </tr>
  <tr><th colspan="6" style = "border-bottom: 1px solid black"></th></tr>
  <tr>
    <th rowspan ="3">Demographic Controls</th> 
    <td>Reputation</td>
    <td>', round(med.rep_dem$d0,3),'</td>
    <td>', round(med.rep_dem$z0,3),'</td>
    <td>', round(med.rep_dem$tau.coef,3),'</td>
    <td>', round(med.rep_dem$n0,3),' </td>
  </tr>
  <tr style="background-color: #D6EEEE">
    <td>Ambiguity</td>
    <td>', round(med.amb_dem$d0,3),'</td>
    <td>', round(med.amb_dem$z0,3),'</td>
    <td>', round(med.amb_dem$tau.coef,3),'</td>
    <td>', round(med.amb_dem$n0,3),' </td>
  </tr>
  <tr>
    <td>Insulting</td>
    <td>', round(med.ins_dem$d0,3),'</td>
    <td>', round(med.ins_dem$z0,3),'</td>
    <td>', round(med.ins_dem$tau.coef,3),'</td>
    <td>', round(med.ins_dem$n0,3),' </td>
  </tr>
  <tr><th colspan="6" style = "border-bottom: 1px solid black"></th></tr>
  </table>
  
  <p></p>
  
  </body>
  </html>', sep =" ")

write(med_table, "med_table_qatar.html")
BROWSE("med_table_qatar.html")
