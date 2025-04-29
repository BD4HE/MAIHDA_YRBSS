rm(list = ls())
library(ggeffects)   # predictions and significance testing
library(insight)     # extracting random effects variances
library(datawizard)  # data wrangling and preparation
library(parameters)  # model summaries
library(performance) # model fit indices, ICC
library(glmmTMB)     # multilevel modelling
library(marginaleffects)
library(ggplot2)
library(Amelia)
library(gtsummary)
library(gt)
library(writexl)
library(flextable)
library(tidyr)
library(forcats)
library(ggtext)
library(sandwich)
library(emmeans)
library(sjPlot)


# Define output directory
OutDir0="C:/Users/jmerch/Desktop/LGBTQ/YRBS/Combine11to21w23/results_20250220/"
setwd(OutDir0)

OutcomeN=c("SuicideIdeation","SuicideAttempt","SuicidePlan","Sad","Bullied","Ebullied","Rape")
Outcomes=c("suicide_ideation","suicideattemptbin","suicideplan","sad","bully","ebully","rape")


StateMeasures=c("M1_NoState","RegionCensus_4cat","overall20_5cat","NegRaceSent2020_4")
StateMeasName=c("M1_NoState","Region4","LGBT20_5","NegRace4")
StateMeasName2=c("National (Model 1)","4 Census Regions","2020 LGBTQ+ Policy","2020 Negative Racial Sentiment Quartiles")


Strata=c("Strata40")
StrataR=c("Race5")
StrataS=c("SexOrient4")
StrataG=c("Gender2")

OutDir="C:/Users/jmerch/Desktop/LGBTQ/YRBS/Combine11to21w23/results_20250220/results/"

o=1
s=1
# 
# 
# OutDir1=paste0(OutDir0,OutcomeN[o],"/")
# 
# 
# load(paste0(OutDir1,OutcomeN[o],"_Strata40_",StateMeasName[1],".RData"))
# M1=m1_0
# rm(m1_0,m1_0pred)
# 
# 
# load(paste0(OutDir1,OutcomeN[o],"_Strata40_",StateMeasName[2],".RData"))
# M2=m4
# rm(m4,m4pred,mparams)
# 
# load(paste0(OutDir1,OutcomeN[o],"_Strata40_",StateMeasName[3],".RData"))
# M3=m4
# rm(m4,m4pred,mparams)
# 
# load(paste0(OutDir1,OutcomeN[o],"_Strata40_",StateMeasName[4],".RData"))
# M4=m4
# rm(m4,m4pred,mparams)
# 
# TM=tab_model(M1,M2,M3,M4,show.aic=TRUE,show.aicc=TRUE,show.reflvl=TRUE,
#           show.dev=TRUE,show.loglik=TRUE,p.style="stars",dv.labels = StateMeasName2,
#           file=paste0(OutDir,OutcomeN[o],"_CoeffsjTable.html"))
# 
# rm(M1,M2,M3,M4,TM)



          # file=paste0(OutDir,OutcomeN[o],"_CoeffsjTable.html"))



for (o in 1:length(Outcomes)){
  
  print(OutcomeN[o])
OutDir1=paste0(OutDir0,OutcomeN[o],"/")


load(paste0(OutDir1,OutcomeN[o],"_Strata40_",StateMeasName[1],".RData"))
M1=m1_0
rm(m1_0,m1_0pred)


load(paste0(OutDir1,OutcomeN[o],"_Strata40_",StateMeasName[2],".RData"))
M2=m4
rm(m4,m4pred,mparams)

load(paste0(OutDir1,OutcomeN[o],"_Strata40_",StateMeasName[3],".RData"))
M3=m4
rm(m4,m4pred,mparams)

load(paste0(OutDir1,OutcomeN[o],"_Strata40_",StateMeasName[4],".RData"))
M4=m4
rm(m4,m4pred,mparams)



sink(paste0(OutDir, OutcomeN[o], "_ModelParams_20250409.txt"))
cat("------------------\n National \n------------------\n")
print(model_parameters(M1, exponentiate = TRUE,digits=6))
cat("\n \n")
cat("------------------\n Census \n------------------\n")
print(model_parameters(M2, exponentiate = TRUE,digits=6))
cat("\n \n")
cat("------------------\n LGBTQ Policy \n------------------\n")
print(model_parameters(M3, exponentiate = TRUE,digits=6))
cat("\n \n")
cat("------------------\n Racial Sentiment \n------------------\n")
print(model_parameters(M4, exponentiate = TRUE,digits=6))
cat("\n \n")
sink()



tab_model(M1, M2, M3, M4, show.aic=TRUE,show.reflvl=TRUE,show.p=FALSE,
          show.dev=TRUE,show.loglik=TRUE,dv.labels = StateMeasName2,digits.re=6,
          file=paste0(OutDir,OutcomeN[o],"_CoeffsjTable_20250409.html"))

rm(M1,M2,M3,M4)
}





