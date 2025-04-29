library(varhandle)
load("C:\\Users\\jmerch\\Desktop\\LGBTQ\\YRBS\\Combine11to21w23\\data\\YRBSS_1991-2023_46States_DataAndFuncs.RData")

Data=Data[as.numeric(as.character(Data$year))>2014,]

Data$year=factor(as.character(Data$year))


Data$overall20_5cat[Data$overall20_cat=="High: 75-100% of total"]="1:Best"
Data$overall20_5cat[Data$overall20_cat=="Medium: 50-74.9% of total"]="2:Medium"
Data$overall20_5cat[Data$overall20_cat=="Fair: 25-49.9% of total"]="3:Fair"
Data$overall20_5cat[Data$overall20_cat=="Low: 0-24.9% of total possible points"]="4:Bad"
Data$overall20_5cat[Data$overall20_cat=="Negative: Less than zero"]="5:Worst"
Data$overall20_5cat=as.factor(Data$overall20_5cat)

Data$overall10_5cat[Data$overall10_cat=="High: 75-100% of total"]="1:Best"
Data$overall10_5cat[Data$overall10_cat=="Medium: 50-74.9% of total"]="2:Medium"
Data$overall10_5cat[Data$overall10_cat=="Fair: 25-49.9% of total"]="3:Fair"
Data$overall10_5cat[Data$overall10_cat=="Low: 0-24.9% of total possible points"]="4:Bad"
Data$overall10_5cat[Data$overall10_cat=="Negative: Less than zero"]="5:Worst"
Data$overall10_5cat=as.factor(Data$overall10_5cat)

Data$so20_5cat[Data$so20_cat=="High: 75-100% of total"]="1:Best"
Data$so20_5cat[Data$so20_cat=="Medium: 50-74.9% of total"]="2:Medium"
Data$so20_5cat[Data$so20_cat=="Fair: 25-49.9% of total"]="3:Fair"
Data$so20_5cat[Data$so20_cat=="Low: 0-24.9% of total possible sexual orientation points"]="4:Bad"
Data$so20_5cat[Data$so20_cat=="Negative: Less than zero"]="5:Worst"
Data$so20_5cat=as.factor(Data$so20_5cat)

Data$so10_5cat[Data$so10_cat=="High: 75-100% of total"]="1:Best"
Data$so10_5cat[Data$so10_cat=="Medium: 50-74.9% of total"]="2:Medium"
Data$so10_5cat[Data$so10_cat=="Fair: 25-49.9% of total"]="3:Fair"
Data$so10_5cat[Data$so10_cat=="Low: 0-24.9% of total possible sexual orientation points"]="4:Bad"
Data$so10_5cat[Data$so10_cat=="Negative: Less than zero"]="5:Worst"
Data$so10_5cat=as.factor(Data$so10_5cat)

Data$gi20_5cat[Data$gi20_cat=="High: 75-100% of total"]="1:Best"
Data$gi20_5cat[Data$gi20_cat=="Medium: 50-74.9% of total"]="2:Medium"
Data$gi20_5cat[Data$gi20_cat=="Fair: 25-49.9% of total"]="3:Fair"
Data$gi20_5cat[Data$gi20_cat=="Low: 0-24.9% of total possible gender identity points"]="4:Bad"
Data$gi20_5cat[Data$gi20_cat=="Negative: Less than zero"]="5:Worst"
Data$gi20_5cat=as.factor(Data$gi20_5cat)

Data$gi10_5cat[Data$gi10_cat=="High: 75-100% of total"]="1:Best"
Data$gi10_5cat[Data$gi10_cat=="Medium: 50-74.9% of total"]="2:Medium"
Data$gi10_5cat[Data$gi10_cat=="Fair: 25-49.9% of total"]="3:Fair"
Data$gi10_5cat[Data$gi10_cat=="Low: 0-24.9% of total possible gender identity points"]="4:Bad"
Data$gi10_5cat[Data$gi10_cat=="Negative: Less than zero"]="5:Worst"
Data$gi10_5cat=as.factor(Data$gi10_5cat)







# Data$overall20_4cat=factor(Data$overall20_4cat)
Data$NegRaceSent2020_4=factor(Data$NegRaceSent2020_4)
Data$NegRaceSent2020_3=factor(Data$NegRaceSent2020_3)

Data$Race2=as.character(Data$Race2)
Data$Race2[Data$Race2=="RacialMinority"]="Minority Race (Non-White)"
Data$Race2[Data$Race2=="White"]="Majority Race (white)"
Data$Race2=as.factor(Data$Race2)
Data <- within(Data, Race2 <- relevel(Race2, ref = "Majority Race (white)"))

Data <- within(Data, Race5 <- relevel(Race5, ref = "White"))

Data$SexOrient2=as.character(Data$SexOrient2)
Data$SexOrient2[Data$SexOrient2=="SexualMinority"]="Sexual Minority (Non-Hetero)"
Data$SexOrient2[Data$SexOrient2=="Heterosexual"]="Sexual Majority (Hetero)"
Data$SexOrient2=as.factor(Data$SexOrient2)
Data <- within(Data, SexOrient2 <- relevel(SexOrient2, ref = "Sexual Majority (Hetero)"))

Data <- within(Data, SexOrient4 <- relevel(SexOrient4, ref = "Heterosexual"))

Data <- within(Data, Gender2 <- relevel(Gender2, ref = "Male"))

Data <- within(Data, state <- relevel(state, ref = "MD"))


Data=Data[,c("year","state","Race5","Race2","SexOrient4","SexOrient2","Gender2",
             "RegionCensus_4cat","overall20_5cat","overall10_5cat","NegRaceSent2020_4",
             "NegRaceSent2020_3","so20_5cat","so10_5cat","gi20_5cat","gi10_5cat",
             "suicide_ideation","suicideattemptbin","suicideplan","sad","bully",
             "ebully","rape")]



CalculateModel <- function(FormulaName=FormulaNames[1],Formula=Formulas[1],Data=Data){
  
  cat("\n")
  cat("__________________________________\n")
  cat(paste(FormulaName,"\n"))
  cat("__________________________________\n")
  
  # estimate null model
  model <- glmmTMB(as.formula(Formula),
                   data = Data,
                   family = binomial(link = 'logit'),
                   REML = TRUE)
  
  return(model)
}

print_params <- function(model,name) {
  cat(paste0("------------------\n",name, "\n------------------\n"))
  # summarize the model
  cat("---------\n summary: \n---------\n")
  msummary=summary(model)
  print(msummary)
  cat("\n \n")
  
  # get model parameters
  cat("------------------\n model parameters: \n------------------\n")
  mparams=model_parameters(model, exponentiate = TRUE,digits=6)
  print(mparams)

  # 
  cat("------------------\n variance \n------------------\n")
  print(get_variance(model))
  cat("\n \n")
  
  mparamsFx=mparams[mparams$Effects=="fixed",]
  mparamsRx=mparams[mparams$Effects=="random",]
  
  CoeffFx=cbind(mparamsFx[["Parameter"]],
                as.data.frame(paste0(as.character(round(mparamsFx$Coefficient,2)),
                                     " (", as.character(round(mparamsFx$SE,2)), 
                                     ") [",as.character(round(mparamsFx$CI_low,2)),
                                     ", ",as.character(round(mparamsFx$CI_high,2)),"]")
                ),
                as.data.frame(paste0(as.character(round(msummary$coefficients$cond[,1],2)),
                                     " (",
                                     as.character(round(msummary$coefficients$cond[,2],2)),")")
                ),
                as.data.frame(as.character(round(msummary$coefficients$cond[,3],2)))
                
  )
  names(CoeffFx)=c("Term","expCoeff","Coeff","Z")
  
  CoeffRx=cbind(mparamsRx[["Parameter"]],
                as.data.frame(as.character(round(mparamsRx$Coefficient,3)))
  )
  CoeffRx[c("Coeff","Z")]=""
  
  names(CoeffRx)=c("Term","expCoeff","Coeff","Z")
  
  tmpCoeffFx=CoeffRx[0,]
  tmpCoeffFx[1,]=c(name,"","","")
  tmpCoeffFx[2,]=c("Fixed Effects","","","")
  
  tmpCoeffRx=CoeffRx[0,]
  tmpCoeffRx[1,]=c("Random Effects","SD/Corr","Var/Cov","")
  
  ModelCoeffTable=rbind(tmpCoeffFx,CoeffFx,tmpCoeffRx,CoeffRx)
  
  return(ModelCoeffTable)
}

GetModelFormulas0 <- function(curO,Gender="Gender2",Race="Race5",SexOrient="SexOrient4"){
  
  FormulaNames=c("Model 0 (Null)",
                 "Model 1: Fixed Main Effects"
                 )
  
  Formulas=c(
    "CURO ~ 1 + ( 1 | GENDER:SEXORIENT:RACE:year)",
    "CURO ~ 1 + GENDER + SEXORIENT + RACE + year + ( 1 | GENDER:SEXORIENT:RACE:year)"
  )
  
  Formulas=gsub("CURO",curO,Formulas)
  Formulas=gsub("GENDER",Gender,Formulas)
  Formulas=gsub("RACE",Race,Formulas)
  Formulas=gsub("SEXORIENT",SexOrient,Formulas)
  
  Formula1=list(Formulas)
  names(Formula1[[1]])=FormulaNames
  
  return(Formula1)
  
}

GetModelFormulas <- function(curO,curS,Gender="Gender2",Race="Race5",SexOrient="SexOrient4"){
  
  FormulaNames=c("Model 0 (Null)",
                 "Model 1: Fixed Main Effects",
                 "Model 2: fixed state effect",
                 "Model 3: fixed and random state effect",
                 "Model 4: Full model")
  
  Formulas=c("CURO ~ 1 + ( 1 | GENDER:SEXORIENT:RACE:year)",
    "CURO ~ 1 + GENDER + SEXORIENT + RACE + year + ( 1 | GENDER:SEXORIENT:RACE:year)",
    "CURO ~ 1 +  CURS + ( 1 | GENDER:SEXORIENT:RACE:year)",
    "CURO ~ 1 +  CURS + ( CURS | GENDER:SEXORIENT:RACE:year)",
    "CURO ~ 1 + GENDER + SEXORIENT + RACE + year + CURS + ( CURS | GENDER:SEXORIENT:RACE:year)"
  )
  
  Formulas=gsub("CURO",curO,Formulas)
  Formulas=gsub("CURS",curS,Formulas)
  Formulas=gsub("GENDER",Gender,Formulas)
  Formulas=gsub("RACE",Race,Formulas)
  Formulas=gsub("SEXORIENT",SexOrient,Formulas)
  
  Formula1=list(Formulas)
  names(Formula1[[1]])=FormulaNames
  
  return(Formula1)
  
}


CalculateModel <- function(FormulaName=FormulaNames[1],Formula=Formulas[1],Data=Data){
  
  cat("\n")
  cat("__________________________________\n")
  cat(paste(FormulaName,"\n"))
  cat("__________________________________\n")
  
  # estimate null model
  model <- glmmTMB(as.formula(Formula),
                   data = Data,
                   family = binomial(link = 'logit'))
  
  return(model)
}

plot_m1 <- function(m1pred,curOn,OutDir,Name1){
  
  RaceCol=c("blue3","red4","orange3","gold2","darkgreen","darkgrey")
  SoCol=c("blue3","red3","gold2","green4")
  
  m1pred$predicted=round(m1pred$predicted*100,2)
  m1pred$conf.low=round(m1pred$conf.low*100,2)
  m1pred$conf.high=round(m1pred$conf.high*100,2)
  names(m1pred)=c("Year","predicted","std.error","conf.low", "conf.high","Race" ,"SexOrient","Gender")
  m1pred$strata=paste0(m1pred$Race,"-",m1pred$Gender)
  m1pred$strata2=paste0(m1pred$SexOrient,"-",m1pred$Gender)
  xMin=min(m1pred$conf.low)
  xMax=max(m1pred$conf.high)
  
  RaceColCur=RaceCol[1:length(unique(m1pred[["Race"]]))]
  SoColCur=SoCol[1:length(unique(m1pred[["SexOrient"]]))]
  
  ggplot(data = m1pred,aes(x = Year, y = predicted, color=Race)) +
    theme_light() +
    facet_grid(vars(SexOrient),vars(Gender)) +
    theme(strip.text.y = element_text(size = 12,face="bold",color="black")) +
    theme(strip.text.x = element_text(size = 12,face="bold",color="black")) +
    theme(strip.background =element_rect(fill="white"))+
    geom_line(aes(group=Race)) + geom_point(size=.8) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
    theme(axis.text.y=element_text(size=10),axis.title=element_text(size=14,face="bold")) +
    theme(axis.text.x = element_text(size=9.5,face="bold",angle=90,vjust = 1,hjust = 1)) +
    scale_shape_manual(values = c(15, 16)) +
    scale_linetype_manual(values=c( "longdash","solid"))+
    xlab("Year") +
    ylab("Predicted Probability (%)") +
    ylim(xMin, xMax) +
    scale_color_manual(values = RaceColCur) +
    ggtitle(paste0("Model1 - ",curOn)) +
    theme(plot.title = element_text( size=16, face="bold",hjust=.5))
  
  ggsave(filename = paste0(Name1,"_M1_NoState_SexORow.png"),
         path = OutDir,
         width = 8, height = 10,units="in", device='png', dpi=300,bg="white")
  
  ggplot(data = m1pred,aes(x = Year, y = predicted, color=SexOrient)) +
    theme_light() +
    facet_grid(vars(Race),vars(Gender)) +
    theme(strip.text.y = element_text(size = 12,face="bold",color="black")) +
    theme(strip.text.x = element_text(size = 12,face="bold",color="black")) +
    theme(strip.background =element_rect(fill="white"))+
    geom_line(aes(group=SexOrient)) + geom_point(size=.8) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
    theme(axis.text.y=element_text(size=10),axis.title=element_text(size=14,face="bold")) +
    theme(axis.text.x = element_text(size=9.5,face="bold",angle=90,vjust = 1,hjust = 1)) +
    scale_shape_manual(values = c(15, 16)) +
    scale_linetype_manual(values=c( "longdash","solid"))+
    xlab("Year") +
    ylab("Predicted Probability (%)") +
    ylim(xMin, xMax) +
    scale_color_manual(values = SoColCur) +
    ggtitle(paste0("Model1 - ",curOn)) +
    theme(plot.title = element_text( size=16, face="bold",hjust=.5))
  
  ggsave(filename = paste0(Name1,"_M1_NoState_RaceRow.png"),
         path = OutDir,
         width = 8, height = 10,units="in", device='png', dpi=300,bg="white")
  
  return(m1pred)
  
}


plot_m4 <- function(m4,curOn,curS,curSn,tmpData,Race,Gender,SexOrient,OutDir,Name2){
  
  RaceCol=c("blue3","red4","orange3","gold2","darkgreen","darkgrey")
  SoCol=c("blue3","red3","gold2","green4")
  
    
    Levels4=c("1:Lowest","2:Low","3:High","4:Highest")
    Levels3=c("1:Low","2:Mid","3:High")
    
    if ( typeof(unfactor(tmpData[[curS]]))=="character" ){
    # if (curS=="RegionCensus_4cat" | curS=="state" | curS=="overall20_5cat" | curS=="overall10_5cat"){
      Levels=sort(as.character(ordered(unique(tmpData[[curS]]))))
      LevelN=Levels
    } else {
      Levels=sort(as.integer(as.character(ordered(unique(tmpData[[curS]])))))
      
      
      if (length(unique(tmpData[[curS]]))==3  ){
        LevelN=Levels3
      }
      if (length(unique(tmpData[[curS]]))==4  ){
        LevelN=Levels4
      }
    }
    
    for (l in 1:length(Levels)){
      
      print(paste("working on prediction for level -",as.character(Levels[l])))
      
      PredText=paste0("predict_response(m4,terms=c('year', '",Race,"','",SexOrient,"','", Gender,"'),type = 'random', interval = 'confidence', condition=c(",curS,"=Levels[l]))")
      
      m4pred <- eval(parse(text=PredText))
      
      
      # print("cleaning/adding to df")
      m4pred$predicted=m4pred$predicted*100
      m4pred$conf.low=m4pred$conf.low*100
      m4pred$conf.high=m4pred$conf.high*100
      
      names(m4pred)=c("year","predicted","std.error","conf.low", "conf.high","race" ,"sexorient","gender")
      m4pred$strata=paste0(as.character(m4pred$race),"-",as.character(m4pred$gender))
      m4pred$strata2=paste0(as.character(m4pred$sexorient),"-",as.character(m4pred$gender))
      
      m4pred$state=LevelN[l]
      m4pred$Level=Levels[l]
      
      # print("appending to master df")
      if (l==1){
        m4pred_all=m4pred
      } else {
        m4pred_all=rbind(m4pred_all,m4pred)
      }
      
      
    }
    rm(m4pred)
    m4pred=m4pred_all
    rm(m4pred_all)
    
    xMin=min(m4pred$conf.low)
    xMax=max(m4pred$conf.high)
    
    names(m4pred)=c("Year","predicted","std.error","conf.low", "conf.high","Race" ,"Sexual_Orientation","Gender","strata","strata2","state","Level")
    
    RaceColCur=RaceCol[1:length(unique(m4pred[["Race"]]))]
    SoColCur=SoCol[1:length(unique(m4pred[["Sexual_Orientation"]]))]
    
    ggplot(data = m4pred,aes(x = Year, y = predicted, shape=Gender,color=Race)) + 
      theme_light() +
      facet_grid(vars(Sexual_Orientation), vars(state)) +
      theme(strip.text.y = element_text(size = 12,face="bold",color="black")) +
      theme(strip.text.x = element_text(size = 12,face="bold",color="black")) +
      theme(strip.background =element_rect(fill="white")) +
      geom_line(aes(group=strata, linetype=Gender)) + 
      geom_point(size=.8) +
      geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
      theme(axis.text.y=element_text(size=10)) +
      theme(axis.text.x = element_text(size=9.5,face="bold",angle=90,vjust = 1,hjust = 1)) +
      scale_shape_manual(values = c(15, 16)) +
      scale_linetype_manual(values=c( "longdash","solid"))+
      xlab("State-Level") + 
      ylab("Predicted Probability (%)") + 
      ylim(xMin, xMax) +
      scale_color_manual(values = RaceColCur) +
      ggtitle(paste0("Model4-",curOn,"-",curSn)) + 
      theme(plot.title = element_text( size=16, face="bold",hjust=.5))
    
    ggsave(filename = paste0(Name2,"_M4_SexORow.png"),
           path = OutDir,
           width = 8, height = 10,units="in", device='png', dpi=300,bg="white")
    
    
    
    ggplot(data = m4pred,aes(x = Year, y = predicted, shape=Gender,color=Sexual_Orientation)) + 
      theme_light() +
      facet_grid(vars(Race), vars(state)) +
      theme(strip.text.y = element_text(size = 12,face="bold",color="black")) +
      theme(strip.text.x = element_text(size = 12,face="bold",color="black")) +
      theme(strip.background =element_rect(fill="white")) +
      geom_line(aes(group=strata2, linetype=Gender)) + geom_point(size=.8) +
      geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
      theme(axis.text.y=element_text(size=10)) +
      theme(axis.text.x = element_text(size=9.5,face="bold",angle=90,vjust = 1,hjust = 1)) +
      scale_shape_manual(values = c(15, 16)) +
      scale_linetype_manual(values=c( "longdash","solid"))+
      xlab("State-Level") + 
      ylab("Predicted Probability (%)") + 
      ylim(xMin, xMax) +
      scale_color_manual(values = SoColCur) +
      ggtitle(paste0("Model4-",curOn,"-",curSn)) + 
      theme(plot.title = element_text( size=16, face="bold",hjust=.5))
    
    ggsave(filename = paste0(Name2,"_M4_RaceRow.png"),
           path = OutDir,
           width = 8, height = 10,units="in", device='png', dpi=300,bg="white")
  
    return(m4pred)
  
}


save.image(file = "C:\\Users\\jmerch\\Desktop\\LGBTQ\\YRBS\\Combine11to21w23\\data\\YRBSS_2015-2023_46States_DataAndFuncs_20250414.RData")
