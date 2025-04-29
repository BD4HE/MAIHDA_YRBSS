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
library(dplyr)
library(ggplotgui)




OutcomeN=c("SuicideIdeation","SuicideAttempt","SuicidePlan","Sad","Bullied","Ebullied","Rape")
OutcomeN2=c("Seriously Considered Suicide","Attempted Suicide","Planned Suicide","Felt Sad (2 weeks+)","Been Bullied","Been Cyber-Bullied","Been Raped")
Outcomes=c("suicide_ideation","suicideattemptbin","suicideplan","sad","bully","ebully","rape")

StateMeasures=c("RegionCensus_4cat","overall20_5cat","overall10_5cat","NegRaceSent2020_4","NegRaceSent2020_3",
                "so20_5cat","so10_5cat","gi20_5cat","gi10_5cat")

StateMeasName=c("Region4","LGBT20_5","LGBT10_5","NegRace4","NegRace3","SexOrient20","SexOrient10","GenderID20","GenderID10")

StateMeasName2=c("4 Census Regions","2020 Anti-LGBTQ Laws (5 levels)","2010 Anti-LGBTQ Laws (4 levels)",
                 "Negitive Racial Sentiment (4 levels)","Negitive Racial Sentiment (3 levels)",
                 "2020 Sexual Orientation Laws (5 levels)","2010 Sexual Orientation Laws (4 levels)",
                 "2020 Gender Identity Laws (5 levels)","2010 Gender Identity Laws (4 levels)")

Strata=c("Strata40","Str20R5S2","Str16R2S4","Str6R2S2")


InDir="C:/Users/jmerch/Desktop/LGBTQ/YRBS/Combine11to21w23/results_20250220/"
OutDir="C:/Users/jmerch/Desktop/LGBTQ/YRBS/Combine11to21w23/results_20250220/predicted_probabilities/"


################################################################################
# Aggregate 7 outcomes with no State/Geographic divisions
################################################################################

counter=1

for (o in 1:length(Outcomes)){
  
  for (i in 1:length(Strata)){
  
    # load data
    load(paste0(InDir,"/",OutcomeN[o],"/",OutcomeN[o],"_",Strata[i],"_M1_NoState.RData"))
    # change sex orient column name
    names(m1_0pred)[7]="Sexual_Orientation"
    # define state division variable
    m1_0pred$state="46 States Combined"
    # define geographic division variable
    m1_0pred$Geographic="National"
    # define outcome variable
    m1_0pred$outcome=OutcomeN2[o]
    
    # create aggregated df. if first outcome set to all, otherwise bind to all
    if (counter==1){
      All=m1_0pred[,c(1:2,4:13)]
      rm(m1_0,m1_0pred)
    } else {
      All=rbind(All,m1_0pred[,c(1:2,4:13)])
      rm(m1_0,m1_0pred)
    }
  
    counter=counter+1
  }
  
  
}

write.csv(All,"C:/Users/jmerch/Desktop/LGBTQ/YRBS/Combine11to21w23/results_20250220/predicted_probabilities/All7Outcomes_Strata40_M1_NoState.csv",
          row.names = FALSE)

save(All,file = paste0(OutDir,"Outcomes7_M1_NoState.RData"))



# input_races=
# Test <- All |> filter(Race %in% input_races) |> filter(Sexual_Orientation %in% input_so)

################################################################################
# Aggregate 7 outcomes with State/Geographic divisions
################################################################################

for (o in 1:length(Outcomes)){
  
  for (s in 1:length(StateMeasures)){
    
    for (i in 1:length(Strata)){
      
      # load data
      load(paste0(InDir,"/",OutcomeN[o],"/",OutcomeN[o],"_",Strata[i],"_",StateMeasName[s] ,".RData"))
      # define geographic division variable
      m4pred$Geographic=StateMeasName2[s]
      # define outcome variable
      m4pred$outcome=OutcomeN2[o]
      
      
      All=rbind(All,m4pred[,c(1:2,4:11,13:14)])
      rm(m4pred,m4,mparams)
      
      
    }
  }
  
}



All$state=gsub("46 States Combined","Across 46 States",All$state)

All$Race=as.character(All$Race)
All$Race=gsub("Minority Race (Non-White)","Minority (Not White)",All$Race,fixed =TRUE)

All$Sexual_Orientation=as.character(All$Sexual_Orientation)
All$Sexual_Orientation=gsub("Sexual Minority (Non-Hetero)","Minority (Not Hetero)",All$Sexual_Orientation,fixed =TRUE)




write.csv(All,"C:/Users/jmerch/Desktop/LGBTQ/YRBS/Combine11to21w23/results_20250220/predicted_probabilities/All7Outcomes_Strata40_wStateDivisions.csv",
          row.names = FALSE)

save(All,file = paste0(OutDir,"Outcomes7_wStateDivisions.RData"))







