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
library(dplyr)
# library(ggplotgui)
library(varhandle)

load("C:\\Users\\jmerch\\Desktop\\LGBTQ\\YRBS\\Combine11to21w23\\data\\YRBSS_2015-2023_46States_DataAndFuncs_20250414.RData")


# Define output directory
OutDir0="C:/Users/jmerch/Desktop/LGBTQ/YRBS/Combine11to21w23/results_20250414/"
setwd(OutDir0)

OutcomeN=c("SuicideIdeation","SuicideAttempt","SuicidePlan","Sad","Bullied","Ebullied","Rape")
Outcomes=c("suicide_ideation","suicideattemptbin","suicideplan","sad","bully","ebully","rape")

StateMeasures=c("RegionCensus_4cat","overall20_5cat","NegRaceSent2020_4","NegRaceSent2020_3","overall10_5cat",
                "so20_5cat","so10_5cat","gi20_5cat","gi10_5cat")
StateMeasName=c("Region4","LGBT20_5","NegRace4","NegRace3","LGBT10_5",
                "SexOrient20","SexOrient10","GenderID20","GenderID10")

# StateMeasures=c("RegionCensus_4cat","overall20_5cat","NegRaceSent2020_4")
# StateMeasName=c("Region4","LGBT20_5","NegRace4")

# Strata=c("Strata40","Str20R5S2","Str16R2S4","Str6R2S2")
Strata=c("Strata40")
StrataR=c("Race5","Race5","Race2","Race2")
StrataS=c("SexOrient4","SexOrient2","SexOrient4","SexOrient2")
StrataG=c("Gender2","Gender2","Gender2","Gender2")

CurTime1=as.character(Sys.time())
CurTime=gsub(" ","_T",gsub(":","",CurTime1))
CurTime=strsplit(CurTime,"_")[[1]][1]
rm(CurTime1)
print(CurTime)

o=1
y=1
x=1
s=1

# # define color schemes
# RaceCol=c("blue3","red4","orange3","gold2","darkgreen","darkgrey")
# SoCol=c("blue3","red3","gold2","green4")



# loop through outcomes
for (o in 1:length(Outcomes)){
  
  # define current outcome and name
  curO=Outcomes[o]
  curOn=OutcomeN[o]
  
  # create temp data set with no missing outcomes for analyses 
  tmpData1 = Data[!is.na(Data[curO]), ]
  
  # create directory for outcome
  OutDir=paste0(OutDir0,curOn,"/")
  
  # check to see if directory exists, otherwise make it an change directories
  if (file.exists(OutDir)){
    setwd(OutDir)
  } else {
    dir.create(OutDir)
    setwd(OutDir)
  }
  
  # loop through strata 
  for (x in 1:length(Strata)){
    
    # define race,so,gender
    Race=StrataR[x]
    SexOrient=StrataS[x]
    Gender=StrataG[x]
    

    
    # create name for output
    Name1=paste0(curOn,"_",Strata[x])
    
    if (length(list.files(path=OutDir,pattern=paste0(Name1,"_M1_NoState.RData")))==0){
      
      Formula0=GetModelFormulas0(curO,Gender=Gender,Race=Race,SexOrient=SexOrient)
      
      sink(paste0(OutDir, Name1, "_M1_NoState.txt"))
      
      m0=CalculateModel(FormulaName=names(Formula0[[1]])[1],as.character(Formula0[[1]][names(Formula0[[1]])[1]]),Data=Data)
      print(model_parameters(m0, exponentiate = TRUE,digits=6))
      
      m1=CalculateModel(FormulaName=names(Formula0[[1]])[2],as.character(Formula0[[1]][names(Formula0[[1]])[2]]),Data=Data)
      print(model_parameters(m1, exponentiate = TRUE,digits=6))
      
      sink()
      
      save(m0,m1,file = paste0(OutDir,Name1,"_M1_NoState.RData"))
      rm(m0,m1,Formula0)
      
      
    }  
    
    
      # loop through state measures
      for (s in 1:length(StateMeasures)){
        
        # define current outcome and name
        curS=StateMeasures[s]
        curSn=StateMeasName[s]
        
        # remove observations missing state measure
        tmpData = tmpData1[!is.na(tmpData1[curS]), ]
        
        # define name2
        Name2=paste0(Name1,"_",curSn)
        

        if (length(list.files(path=OutDir,pattern=paste0(Name2,".RData")))==1){
          next
        } else {print(paste("working on",Name2))}
        
        
        Formula1 = GetModelFormulas(curO,curS,Gender=Gender,Race=Race,SexOrient=SexOrient)
        FormulaNames=names(Formula1[[1]])
        Formulas=as.character(Formula1[[1]])
        rm(Formula1)
        
        print(paste("Working on:",Name2))
        
        m1=CalculateModel(FormulaName=FormulaNames[1],Formula=Formulas[1],Data=tmpData)
        m2=CalculateModel(FormulaName=FormulaNames[2],Formula=Formulas[2],Data=tmpData)
        m3=CalculateModel(FormulaName=FormulaNames[3],Formula=Formulas[3],Data=tmpData)
        m4=CalculateModel(FormulaName=FormulaNames[4],Formula=Formulas[4],Data=tmpData)
        m5=CalculateModel(FormulaName=FormulaNames[5],Formula=Formulas[5],Data=tmpData)
        
        sink(paste0(OutDir,Name2,".txt"))
        
        ParamTable=print_params(m1,FormulaNames[1])
        
        tabletmp=print_params(m2,FormulaNames[2])
        ParamTable=rbind(ParamTable,tabletmp)
        rm(tabletmp)
        
        tabletmp=print_params(m3,FormulaNames[3])
        ParamTable=rbind(ParamTable,tabletmp)
        rm(tabletmp)
        
        tabletmp=print_params(m4,FormulaNames[4])
        ParamTable=rbind(ParamTable,tabletmp)
        rm(tabletmp)
        
        tabletmp=print_params(m5,FormulaNames[5])
        ParamTable=rbind(ParamTable,tabletmp)
        rm(tabletmp)
        
        sink()
        
        save(m1,m2,m3,m4,m5,ParamTable,file = paste0(OutDir,Name2,".RData"))

        write.csv(ParamTable,file = paste0(OutDir,Name2,"_ModelParametersTable.csv"))
        
        rm(m1,m2,m3,m4,m5,ParamTable,Formulas,FormulaNames,tmpData)
        
        
        
      }
      
      
    }

}