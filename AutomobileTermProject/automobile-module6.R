#Module 6: Regression
setwd("/Users/bharila/Documents/Rstuff/AutomobileTermProject")
automobile <- read.csv("automobile.csv")

#change ? to NA
library(plyr)
changetoNA <- function(x){mapvalues(x, from = c("?"), to = c(NA))}
automobile[ ,c(2,6,19,20,22,23,26)]<-sapply(automobile[ ,c(2,6,19,20,22,23,26)],changetoNA)

#remove missing data
automobile <- automobile[complete.cases(automobile), ]
#overview
str(automobile[,c(3,4,5,7,8,9,15,18)])

#change data types
automobile$drive.wheels<-as.factor(automobile$drive.wheels)
automobile$num.of.doors<-mapvalues(automobile$num.of.doors, from = c("two", "four"), to = c(2,4))
automobile$num.of.cylinders<-mapvalues(automobile$num.of.cylinders, from = c("eight", "five", "four", "six", "three", "twelve", "two"), to =c(8,5,4,6,3,12,2))
automobile[ ,c(2,6,16,19,20,22,23,26)] <- sapply(automobile[ ,c(2,6,16,19,20,22,23,26)], as.numeric)
#overview of target variable: normalised losses
summary(automobile$normalized.losses)
hist(automobile$normalized.losses)
prop.table(table(automobile$normalized.losses))*100
round(cor(automobile[,-c(3,4,5,7,8,9,15,18)]),digits=2)*100

automobile[,c(3,7,8,15,18)]<-sapply(automobile[,c(3,7,8,15,18)], droplevels)
automobile<-automobile[,-c(9)]

set.seed(123)
train_sample <- sample(159, 120) #90% training data
automobile_train<-automobile[train_sample,]
automobile_test<-automobile[-train_sample,]
auto_model22<-lm(automobile_train$normalized.losses~make+height,data=automobile_train)
summary(auto_model22)
automobile_pred<-predict(auto_model22, automobile_test)
library(gmodels)
# plot(x = automobile_test$normalized.losses, y = automobile_pred,  
#      main = "normalised losses",  xlab = "actual",  ylab = "pred")

library(ggplot2)

ggplot(automobile_test, aes(automobile_test$normalized.losses)) +                    # basic graphical object
  geom_line(aes(y=automobile_test$normalized.losses), colour="red") +  # first layer
  geom_line(aes(y=automobile_pred), colour="green")  # second layer

answerTable <- CrossTable(automobile_test$normalized.losses, automobile_pred,
                          prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
                          dnn = c('predicted losses', 'actual losses'))

auto_model<-lm(automobile$normalized.losses~.,data=automobile)
summary(auto_model)
#ifelse(automobile$make=="chevrolet",1,0)
auto_model11<-lm(automobile$normalized.losses~make+num.of.doors+drive.wheels+height,data=automobile)
summary(auto_model11)

# propTable<-function(x){prop.table(table(x))*100}
# sapply(automobile[ ,c(3,7,8,15,18)], propTable)
# meanAndVarianceByLevel <-function(x){
#   tapply(automobile$normalized.losses, x, mean)
#   #tapply(automobile$normalized.losses, automobile[ ,3], var)
# }
# 
# sapply(automobile[ ,c(3,7,8,15,18)], meanAndVarianceByLevel)
# 
# library(forcats)
# automobile$make<-fct_collapse(automobile$make, highLossMakes = c("bmw", "porsche","audi","dodge","jaguar","mitsubishi","nissan","peugot"))
# automobile$make<-fct_collapse(automobile$make, lowLossMakes = c("chevrolet","honda","mazda","mercedes-benz","plymouth","saab","subaru","toyota","volkswagen","volvo"))
# #automobile$MakeLoss<automobile$make
# automobile$body.style<-ifelse(automobile$body.style=="wagon","lowLossBodyStyle","highLossBodyStyle")#since wagon has much lower mean loss than others which have similar mean amountof loss
# automobile$engine.type<-ifelse(automobile$engine.type=="ohc" | automobile$engine.type=="ohcf","LowLossEngineType","HighLossEngineType")
# #since ohc and ohcf have lower mean normalised losses compared to others which have similar
# automobile$fuel.system <-ifelse(automobile$fuel.system=="1bbl"|automobile$fuel.system=="2bbl"|automobile$fuel.system=="idi","LowLossFuelSystem","HighLossFuelSystem")
# #because 1bl, 2bbl and idi have lower mean losses
# 
# #automobile[,c(1,2,3)]<-sapply(automobile[,c(1,2,3)],as.fac)
# #remove drive.wheels as insignificant (mean losses about similar for different categories)
# #, and remove engine locaiton since all are one type
# automobile<-automobile[,-c(8,9)]
# auto_model2<-lm(automobile$normalized.losses~.,data=automobile)
# summary(auto_model2)
