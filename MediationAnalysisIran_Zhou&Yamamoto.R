library(glm2)
library(mediation)
library(MASS)
library(dplyr)
library(stargazer)
library(httr)
library(ggplot2)
library(paths)
library(gbm)

setwd("C:/Users/chase/GDrive/GD_Work/Dissertation/JointPaper/OpenSecretsAnalysis")
df = read.csv("2X2Data_Final.csv")

# Factor ambiguity and insulting
df$ambiguity = as.factor(df$ambiguity)
df$insulting = as.factor(df$insulting)

# Subset Iran data
df_i = df[df$adversary == 1,]

###Mediation Analysis with Dispositional Controls###
##########################

# Subset relevant variables and remove NA values
df_dis = df_i %>% dplyr::select(denial, esca_scaled, MA_scaled, GovTrust, 
                              NewsTrust, IntTrust, NC_scaled, Read.FP,
                              reputation_scaled, ambiguity, insulting, age, male, 
                              hhi, white, education, republican, democrat)
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
med.rep_dis <- mediate(m_rep, m2_rep, treat = "denial", mediator = "reputation_scaled", #Need to match variables here with models above
                   robustSE = TRUE, sims = 1000)
medsens_rep <- medsens(med.rep_dis, rho.by = 0.1)
summary(med.rep_dis)
summary(medsens_rep)
plot(med.rep_dis)
plot(medsens_rep)

# Run mediate function for ambiguity (certainty)
med.amb_dis <- mediate(m_amb, m2_amb, treat = "denial", mediator = "ambiguity", sims = 175, boot = TRUE)
summary(med.amb_dis)
plot(med.amb_dis)

# Run mediate function for insult
med.ins_dis <- mediate(m_ins, m2_ins, treat = "denial", mediator = "insulting", sims = 1000, boot = TRUE)
summary(med.ins_dis)
plot(med.ins_dis)

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
ggsave("Figures/prop_med_iran.png", width = 6, height = 4, unit = "in")

# Multi-mediation Imai and Yamamoto
# Ambiguity

df_dis$ambiguity = as.numeric(df_dis$ambiguity)
df_dis$insulting = as.numeric(df_dis$insulting)

covariates = c('MA_scaled', 'GovTrust', 'NewsTrust', 'IntTrust', 'NC_scaled', 'Read.FP')
meds = c('reputation_scaled','insulting')
mm_amb <- multimed("esca_scaled", "ambiguity", meds, "denial", covariates, 
                    data=df_dis, sims=1000)
summary(mm_amb)
plot(mm_amb)

lims <- c(-20,20)
plot(0, 0, type="n", main="Point Estimates",
     xlab = "Average Causal Mediation Effects", ylab = "",
     xlim = lims, ylim = c(0.5, 4.5), yaxt="n")
segments(mm_amb$d.ave.ci[1,1], 4, mm_amb$d.ave.ci[2,1], 4, lwd=2)
segments(mm_amb$d1.ci[1,1], 3, mm_amb$d1.ci[2,1], 3, lwd=2)
segments(mm_amb$d0.ci[1,1], 2, mm_amb$d0.ci[2,1], 2, lwd=2)
segments(mm_amb$tau.ci[1], 1, mm_amb$tau.ci[2], 1, lwd=2)
points(mm_amb$d.ave.ub[1], 4, pch=18, cex=2)
points(mm_amb$d1.ub[1], 3, pch=18, cex=2)
points(mm_amb$d0.ub[1], 2, pch=18, cex=2)
points(mm_amb$tau, 1, pch=18, cex=2)
abline(v=0)
axis(side = 2, labels = c(
  expression(paste("Average (", bar(bar(delta)), ")")),
  expression(paste("Treated (", bar(delta)[1], ")")),
  expression(paste("Control (", bar(delta)[0], ")")),
  expression(paste("Total (", bar(tau), ")"))),
  at = c(4,3,2,1), las = 2)

# Reputation
covariates = c('MA_scaled', 'GovTrust', 'NewsTrust', 'IntTrust', 'NC_scaled', 'Read.FP')
meds = c('ambiguity','insulting')
mm_rep <- multimed("esca_scaled", "reputation_scaled", meds, "denial", covariates, 
                   data=df_dis, sims=1000)
summary(mm_rep)
plot(mm_rep)

lims <- c(-20,20)
plot(0, 0, type="n", main="Point Estimates",
     xlab = "Average Causal Mediation Effects", ylab = "",
     xlim = lims, ylim = c(0.5, 4.5), yaxt="n")
segments(mm_rep$d.ave.ci[1,1], 4, mm_rep$d.ave.ci[2,1], 4, lwd=2)
segments(mm_rep$d1.ci[1,1], 3, mm_rep$d1.ci[2,1], 3, lwd=2)
segments(mm_rep$d0.ci[1,1], 2, mm_rep$d0.ci[2,1], 2, lwd=2)
segments(mm_rep$tau.ci[1], 1, mm_rep$tau.ci[2], 1, lwd=2)
points(mm_rep$d.ave.ub[1], 4, pch=18, cex=2)
points(mm_rep$d1.ub[1], 3, pch=18, cex=2)
points(mm_rep$d0.ub[1], 2, pch=18, cex=2)
points(mm_rep$tau, 1, pch=18, cex=2)
abline(v=0)
axis(side = 2, labels = c(
  expression(paste("Average (", bar(bar(delta)), ")")),
  expression(paste("Treated (", bar(delta)[1], ")")),
  expression(paste("Control (", bar(delta)[0], ")")),
  expression(paste("Total (", bar(tau), ")"))),
  at = c(4,3,2,1), las = 2)

# Insulting
covariates = c('MA_scaled', 'GovTrust', 'NewsTrust', 'IntTrust', 'NC_scaled', 'Read.FP')
meds = c('ambiguity', 'reputation_scaled')
mm_ins <- multimed("esca_scaled", "insulting", meds, "denial", covariates, 
                   data=df_dis, sims=1000)
summary(mm_ins)
plot(mm_ins)

lims <- c(-20,20)
plot(0, 0, type="n", main="Point Estimates",
     xlab = "Average Causal Mediation Effects", ylab = "",
     xlim = lims, ylim = c(0.5, 4.5), yaxt="n")
segments(mm_ins$d.ave.ci[1,1], 4, mm_ins$d.ave.ci[2,1], 4, lwd=2)
segments(mm_ins$d1.ci[1,1], 3, mm_ins$d1.ci[2,1], 3, lwd=2)
segments(mm_ins$d0.ci[1,1], 2, mm_ins$d0.ci[2,1], 2, lwd=2)
segments(mm_ins$tau.ci[1], 1, mm_ins$tau.ci[2], 1, lwd=2)
points(mm_ins$d.ave.ub[1], 4, pch=18, cex=2)
points(mm_ins$d1.ub[1], 3, pch=18, cex=2)
points(mm_ins$d0.ub[1], 2, pch=18, cex=2)
points(mm_ins$tau, 1, pch=18, cex=2)
abline(v=0)
axis(side = 2, labels = c(
  expression(paste("Average (", bar(bar(delta)), ")")),
  expression(paste("Treated (", bar(delta)[1], ")")),
  expression(paste("Control (", bar(delta)[0], ")")),
  expression(paste("Total (", bar(tau), ")"))),
  at = c(4,3,2,1), las = 2)

df$ambiguity = as.factor(df$ambiguity)
df$insulting = as.factor(df$insulting)

# Multi-mediation Zhou and Yamamoto
# GBM models ambiguity --> reputation
m1 <- c("ambiguity")
m2 <- c('reputation_scaled')

mediators <- list(m1,m2)

M_0 <- gbm(esca_scaled ~ denial + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP , data = df_dis, distribution = "gaussian", interaction.depth = 3)  
M_1 <- gbm(esca_scaled ~ denial + ambiguity + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP , data = df_dis, distribution = "gaussian", interaction.depth = 3)
M_2 <- gbm(esca_scaled ~ denial + ambiguity + reputation_scaled + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP , data = df_dis, distribution = "gaussian", interaction.depth = 3)
models <- list(M_0, M_1, M_2)
os_paths <- paths(a = 'denial', y = 'esca_scaled', m = mediators, models, data = df_dis, nboot = 1000)
summary(os_paths)
plot(os_paths)

# Propensity scores check for ambiguity --> reputation 
formula_ps <- denial ~ MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP 
gbm_ps <- gbm(formula_ps, data = df_dis, distribution = "gaussian", interaction.depth = 3)
paths2 <- paths(a = 'denial', y = 'esca_scaled', m = mediators, ps_model = gbm_ps, models, data = df_dis, nboot = 500)  
summary(paths2)
plot(paths2)

# GBM models ambiguity --> insulting
m1 <- c("ambiguity")
m2 <- c('insulting')

mediators <- list(m1,m2)

M_0 <- gbm(esca_scaled ~ denial + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP , data = df_dis, distribution = "gaussian", interaction.depth = 3)  
M_1 <- gbm(esca_scaled ~ denial + ambiguity + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP , data = df_dis, distribution = "gaussian", interaction.depth = 3)
M_2 <- gbm(esca_scaled ~ denial + ambiguity + insulting + MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP , data = df_dis, distribution = "gaussian", interaction.depth = 3)
models <- list(M_0, M_1, M_2)
os_paths <- paths(a = 'denial', y = 'esca_scaled', m = mediators, models, data = df_dis, nboot = 1000)
summary(os_paths)
plot(os_paths)

# Propensity scores check for ambiguity --> insulting
formula_ps <- denial ~ MA_scaled + GovTrust + NewsTrust + IntTrust + NC_scaled + Read.FP 
gbm_ps <- gbm(formula_ps, data = df_dis, distribution = "gaussian", interaction.depth = 3)
paths2 <- paths(a = 'denial', y = 'esca_scaled', m = mediators, ps_model = gbm_ps, models, data = df_dis, nboot = 500)  
summary(paths2)
plot(paths2)
##########################
###Mediation Analysis with no Controls###
##########################

# Subset relevant variables and remove NA values
df_dis = df_i %>% dplyr::select(denial, esca_scaled, ambiguity, insulting, reputation_scaled)
df_dis = df_dis[complete.cases(df_dis),]

# Run models with mediators as the DV
m_rep = lm(reputation_scaled ~ denial, data = df_dis)
m_amb = polr(ambiguity ~ denial, data = df_dis, method = "logistic", Hess = TRUE)
m_ins = polr(insulting ~ denial, data = df_dis, method = "logistic", Hess = TRUE)

# Run models with mediators as the IVs
m2_rep = lm(esca_scaled ~ reputation_scaled + denial, data = df_dis)
m2_amb = lm(esca_scaled ~ ambiguity + denial, data = df_dis)
m2_ins = lm(esca_scaled ~ insulting + denial, data = df_dis)

# Run mediate function for reputation
med.rep <- mediate(m_rep, m2_rep, treat = "denial", mediator = "reputation_scaled", #Need to match variables here with models above
                   robustSE = TRUE, sims = 200)
summary(med.rep)
plot(med.rep)

# Run mediate function for ambiguity (certainty)
med.amb <- mediate(m_amb, m2_amb, treat = "denial", mediator = "ambiguity", sims = 100, boot = TRUE)
summary(med.amb)
plot(med.amb)

# Run mediate function for insult
med.ins <- mediate(m_ins, m2_ins, treat = "denial", mediator = "insulting", sims = 200, boot =TRUE)
summary(med.ins)
plot(med.ins)


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
med.rep_dem <- mediate(m_rep, m2_rep, treat = "denial", mediator = "reputation_scaled", #Need to match variables here with models above
                   robustSE = TRUE, sims = 200)
summary(med.rep_dem)
plot(med.rep_dem)

# Run mediate function for ambiguity (certainty)
med.amb_dem <- mediate(m_amb, m2_amb, treat = "denial", mediator = "ambiguity", sims = 100, boot = TRUE)
summary(med.amb_dem)
plot(med.amb_dem)

# Run mediate function for insult
med.ins_dem <- mediate(m_ins, m2_ins, treat = "denial", mediator = "insulting", sims = 200, boot =TRUE)
summary(med.ins_dem)
plot(med.ins_dem)
##########################

##########################

# Create HTML table of full mediation results
med_table = paste('<!DOCTYPE html>
  <html>
  <style>
  table
  th{text-align: left;}
</style>
  <body>
  
  <h2>Iran: Mediation Effects</h2>
  
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
write(med_table, "med_table_iran.html")
BROWSE("med_table_iran.html")
