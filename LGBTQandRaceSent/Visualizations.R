library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)


df <- readr::read_csv("https://raw.githubusercontent.com/JunaidMerchant/MAIHDA_YRBSS/refs/heads/main/Shiny/predict_probabilities/All7Outcomes_Strata40_wStateDivisions_reduced.csv")

# df=df[df$Sexual_Orientation!="Minority (Not Hetero)",]
# df=df[df$Race!="Minority (Not White)",]
# 
# 


df1=df[df$Geographic=="2020 Anti-LGBTQ Laws (5 levels)",]
df1=df1[df1$outcome=="Seriously Considered Suicide",]
df1=df1[df1$Sexual_Orientation!="Minority (Not Hetero)",]
df1=df1[df1$Race!="Minority (Not White)",]
df1$Race=gsub("MultiRace","Multi-Race",df1$Race)
df1$Race=gsub("OtherRace","Other-Race",df1$Race)
df1$Sexual_Orientation=gsub("Othersexual","Other/Questioning",df1$Sexual_Orientation)
df1$Sexual_Orientation=gsub("Homosexual","Gay/Lesbian",df1$Sexual_Orientation)
df1$Gender=gsub("Female","Girl",df1$Gender)
df1$Gender=gsub("Male","Boy",df1$Gender)
df1$strata=paste0(df1$Race,": ",df1$Gender)
df1$strata2=paste0(df1$Sexual_Orientation,": ",df1$Gender)
df1$Race=factor(df1$Race, levels=c("White","Black","Hispanic","Asian","Multi-Race","Other-Race"))
df1$Sexual_Orientation=factor(df1$Sexual_Orientation,levels=c("Heterosexual","Gay/Lesbian","Bisexual","Other/Questioning"))

df1$strata=factor(df1$strata,levels=c("White: Boy","Black: Boy","Hispanic: Boy","Asian: Boy","Multi-Race: Boy","Other-Race: Boy",
                                      "White: Girl","Black: Girl","Hispanic: Girl","Asian: Girl","Multi-Race: Girl","Other-Race: Girl"))

df1$strata2=factor(df1$strata2,levels=c("Heterosexual: Boy","Gay/Lesbian: Boy","Bisexual: Boy","Other/Questioning: Boy",
                                       "Heterosexual: Girl","Gay/Lesbian: Girl","Bisexual: Girl","Other/Questioning: Girl"))



df2=df[df$Geographic=="Negitive Racial Sentiment (4 levels)",]
df2=df2[df2$outcome=="Attempted Suicide",]
df2=df2[df2$Sexual_Orientation!="Minority (Not Hetero)",]
df2=df2[df2$Race!="Minority (Not White)",]
df2$Race=gsub("MultiRace","Multi-Race",df2$Race)
df2$Race=gsub("OtherRace","Other-Race",df2$Race)
df2$Sexual_Orientation=gsub("Othersexual","Other/Questioning",df2$Sexual_Orientation)
df2$Sexual_Orientation=gsub("Homosexual","Gay/Lesbian",df2$Sexual_Orientation)
df2$Gender=gsub("Female","Girl",df2$Gender)
df2$Gender=gsub("Male","Boy",df2$Gender)

df2$strata=paste0(df2$Race,": ",df2$Gender)
df2$strata2=paste0(df2$Sexual_Orientation,": ",df2$Gender)

df2$Race=factor(df2$Race, levels=c("White","Black","Hispanic","Asian","Multi-Race","Other-Race"))
df2$Sexual_Orientation=factor(df2$Sexual_Orientation,levels=c("Heterosexual","Gay/Lesbian","Bisexual","Other/Questioning"))

df2$strata=factor(df2$strata,levels=c("White: Boy","Black: Boy","Hispanic: Boy","Asian: Boy","Multi-Race: Boy","Other-Race: Boy",
                                      "White: Girl","Black: Girl","Hispanic: Girl","Asian: Girl","Multi-Race: Girl","Other-Race: Girl"))

df2$strata2=factor(df2$strata2,levels=c("Heterosexual: Boy","Gay/Lesbian: Boy","Bisexual: Boy","Other/Questioning: Boy",
                                       "Heterosexual: Girl","Gay/Lesbian: Girl","Bisexual: Girl","Other/Questioning: Girl"))




ggplot(df1, aes(state, predicted,color=Sexual_Orientation)) +
  theme_light() +
  facet_grid(vars(strata), vars(Year)) +
  theme(strip.text.y = element_text(size = 7,face="bold",color="black")) +
  theme(strip.text.x = element_text(size = 10,face="bold",color="black")) +
  theme(strip.background =element_rect(fill="white")) +
  geom_line(aes(group=Sexual_Orientation, linetype=Gender)) +
  geom_point(size=.8) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  theme(axis.text.y=element_text(size=10,face="bold")) +
  theme(axis.text.x = element_text(size=10,face="bold",angle=45,vjust = 1,hjust = 1)) +
  xlab("LGBTQ+ Policy Level") +
  ylab("% YES (predicted probability)") +
  scale_linetype_manual(values=c( "longdash","solid"))+
  scale_color_manual(values = c("grey2","red4","gold2","green4","skyblue3")) +
  # ggtitle(input$Outcome) + 
  theme(legend.position="top") + 
  theme(plot.title = element_text( size=16, face="bold",hjust=.5))

ggsave(filename = "SI_RaceGenderByYear_Model4_LGBTQPolicy_8x10.png",
       path = "C:/Users/jmerch/Desktop/LGBTQ/YRBS/Combine11to21w23/results_20250220/results", 
       width = 8.25, height = 10.5,units="in", device='png', dpi=300,bg="white")



ggsave(filename = "SI_RaceGenderByYear_Model4_LGBTQPolicy_8x20.png",
       path = "C:/Users/jmerch/Desktop/LGBTQ/YRBS/Combine11to21w23/results_20250220/results", 
       width = 8.25, height = 20,units="in", device='png', dpi=300,bg="white")




ggplot(df2, aes(state, predicted,color=Race)) +
  theme_light() +
  facet_grid(vars(strata2), vars(Year)) +
  theme(strip.text.y = element_text(size = 7,face="bold",color="black")) +
  theme(strip.text.x = element_text(size = 10,face="bold",color="black")) +
  theme(strip.background =element_rect(fill="white")) +
  geom_line(aes(group=Race, linetype=Gender)) +
  geom_point(size=.8) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  theme(axis.text.y=element_text(size=10,face="bold")) +
  theme(axis.text.x = element_text(size=10,face="bold",angle=45,vjust = 1,hjust = 1)) +
  xlab("LGBTQ+ Policy Level") +
  ylab("% YES (predicted probability)") +
  scale_linetype_manual(values=c( "longdash","solid"))+
  scale_color_manual(values = c("grey2","orange3","skyblue3","darkgreen","red4","gold3","purple4")) +
  # ggtitle(input$Outcome) + 
  theme(legend.position="top") + 
  theme(plot.title = element_text( size=16, face="bold",hjust=.5))

ggsave(filename = "SA_SOGenderByYear_Model4_LGBTQPolicy_8x10.png",
       path = "C:/Users/jmerch/Desktop/LGBTQ/YRBS/Combine11to21w23/results_20250220/results", 
       width = 8.25, height = 10.5,units="in", device='png', dpi=300,bg="white")

ggsave(filename = "SA_SOGenderByYear_Model4_LGBTQPolicy_8x20.png",
       path = "C:/Users/jmerch/Desktop/LGBTQ/YRBS/Combine11to21w23/results_20250220/results", 
       width = 8.25, height = 20,units="in", device='png', dpi=300,bg="white")


################################################################################
################################################################################
rm(list = ls())
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(tidyr)


df <- readr::read_csv("https://raw.githubusercontent.com/JunaidMerchant/MAIHDA_YRBSS/refs/heads/main/Shiny/predict_probabilities/All7Outcomes_Strata40_wStateDivisions_reduced.csv")


df=df[df$Sexual_Orientation!="Minority (Not Hetero)",]
df=df[df$Race!="Minority (Not White)",]
df$Race=gsub("MultiRace","Multi-Race",df$Race)
df$Race=gsub("OtherRace","Other-Race",df$Race)
df$Sexual_Orientation=gsub("Othersexual","Other/Questioning",df$Sexual_Orientation)
df$Sexual_Orientation=gsub("Homosexual","Gay/Lesbian",df$Sexual_Orientation)
df$Gender=gsub("Female","Girl",df$Gender)
df$Gender=gsub("Male","Boy",df$Gender)
df$strata3=paste0(df$Race,":",df$Sexual_Orientation,":",df$Gender)
df$PredCI=paste0(as.character(round(df$predicted,1)), " [",as.character(round(df$conf.low)),
                 ", ",as.character(round(df$conf.high)),"]")

df=df[,c(1:7,10:14)]

################################################################
# select LGTB Laws and SI
df1=df[df$Geographic=="2020 Anti-LGBTQ Laws (5 levels)",]
df1=df1[df1$outcome=="Seriously Considered Suicide",]

# Sort
df1=df1[order(df1[,1], df1[,8], -df1[,2]), ]

# get unique year, state and strata and their lengths
Yr=unique(df1$Year)
St=unique(df1$state)
S3=unique(df1$strata3)
NYr=length(unique(df1$Year))
NSt=length(unique(df1$state))
NS3=length(unique(df1$strata3))

# create empty dataframe w strata x state rows, and 
dfStYr=data.frame(matrix(NA, nrow = (NS3*NSt), ncol = (NYr+3)))
names(dfStYr)[1:3]=c("State","Rank","Stratum")
names(dfStYr)[4:(NYr+3)]=Yr



r=1

for (st in 1:NSt){
  
  
  
  for (s3 in 1:NS3){
    
    dfStYr$State[r]=St[st]
    dfStYr$Stratum[r]=S3[s3]
    
    for (y in 1:NYr){
      
      indx=(df1$Year==Yr[y]) & (df1$state==St[st]) & (df1$strata3==S3[s3])
      
      dfStYr[r,as.character(Yr[y])]=df1$PredCI[indx]
      
      
    }
    
    r=r+1

  }
}



# create empty dataframe w strata x state rows, and 
dfYrSt=data.frame(matrix(NA, nrow = (NS3*NYr), ncol = (NSt+3)))
names(dfYrSt)[1:3]=c("Year","Rank","Stratum")
names(dfYrSt)[4:(NSt+3)]=St




r=1

for (st in 1:NYr){
  
  for (s3 in 1:NS3){
    
    dfYrSt$Year[r]=Yr[y]
    dfYrSt$Stratum[r]=S3[s3]
    
    for (y in 1:NSt){
      
      indx=(df1$Year==Yr[y]) & (df1$state==St[st]) & (df1$strata3==S3[s3])
      
      dfYrSt[r,St[st]]=df1$PredCI[indx]
      
      
    }
    
    r=r+1
    
  }
}






tmp=df1[df1$Year==2015,c(2,1,8,11,12)]

Test=spread(tmp, key = state, value = PredCI)






################################################################
df2=df[df$Geographic=="Negitive Racial Sentiment (4 levels)",]
df2=df2[df2$outcome=="Attempted Suicide",]
