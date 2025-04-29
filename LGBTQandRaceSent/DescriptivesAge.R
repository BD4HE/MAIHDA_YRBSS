
Data <- read.csv("C:/Users/jmerch/Desktop/LGBTQ/YRBS/Combine11to21w23/data/YRBSS_2011-2021_45States_48vars.csv")

Data=Data[Data$year>2014,]

Data$Age=Data$age+11

summary(Data$Age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   12.00   15.00   16.00   15.79   17.00   18.00  150599 

mean(Data$Age, na.rm=TRUE)
sd(Data$Age, na.rm=TRUE)

# mean(Data$Age, na.rm=TRUE)
# [1] 15.79133
# > sd(Data$Age, na.rm=TRUE)
# [1] 1.239507

sum(!is.na(Data$Age))
# [1] 573756

sum(Data$Age==12,na.rm=TRUE)
# [1] 3082

(3082/573756)*100
# .5% are 12 or younger
# [1] 0.5371621

# sum(Data$Age==18,na.rm=TRUE)
# [1] 51182
# > (51182/573756)*100
# [1] 8.920517
# 8.9% are 18 or older