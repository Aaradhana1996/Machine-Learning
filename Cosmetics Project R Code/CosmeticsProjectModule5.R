#Module 5: Classification using Decision Trees and Rules
library(class)
setwd("/Users/bharila/Documents/Rstuff/CosmeticsTermProject")
cosmetics <- read.csv("cosmeticsCleanedWithTarget.csv", stringsAsFactors = FALSE)
#convert some columns into different types
cosmetics[1:9] <- lapply(cosmetics[1:9], as.factor)
cosmetics$IsDiscontinued <- mapvalues(cosmetics$IsDiscontinued, from=c("FALSE", "TRUE" ), to=c("no", "yes"))
cosmetics[1] <- lapply(cosmetics[1], as.factor)
write.csv(cosmetics, file = "cosmeticsCleanedWithTargetAsFactor.csv", row.names = FALSE)
cosmetics[10:15] <- lapply(cosmetics[10:15], as.Date)
cosmetics[10:15] <- lapply(cosmetics[10:15], as.numeric)
#leave out all categorical variables because they have too many levels
#except for isDiscontinued because it has only 2 levels
#isDiscontinued has been converted to a factor already because 
#for some reason the c50 doesn't work with logical type predictors
#leave out DiscontiniuedDate because it has same information as isDiscontinued
cosmetics<-cosmetics[ ,-c(2,3,4,5,7,8,9,12)]
#leave this level out because there are too few and often we end up with zero
#of this type in training
#or zero in testing which creates complications
cosmetics <- cosmetics[cosmetics$PrimaryCategory!="Baby Products", ]
cosmetics$PrimaryCategory <- droplevels(cosmetics$PrimaryCategory)
table(cosmetics$PrimaryCategory)
#convert Primary Category names to alphabets so crosstable fits into one page later
library(plyr)
cosmetics$PrimaryCategory <- mapvalues(cosmetics$PrimaryCategory, from=c("Bath Products", "Fragrances", "Hair Care Products (non-coloring)", 
                                                                         "Hair Coloring Products", "Makeup Products (non-permanent)", "Nail Products", "Oral Hygiene Products", 
                                                                         "Personal Care Products", "Shaving Products", "Skin Care Products ", "Sun-Related Products", 
                                                                         "Tattoos and Permanent Makeup"), to=c("B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"))
#overview of data
str(cosmetics)
prop.table(table(cosmetics$PrimaryCategory))*100

# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(123)
train_sample <- sample(88954, 80059) #90% training data

str(train_sample)

# split the data frames
cosmetics_train <- cosmetics[train_sample, ]
cosmetics_test  <- cosmetics[-train_sample, ]

# check the proportion of class variable
prop.table(table(cosmetics_train$PrimaryCategory))*100
prop.table(table(cosmetics_test$PrimaryCategory))*100

## Step 3: Training a model on the data ----
# build the simplest decision tree
library(C50)
cosmetics_model <- C5.0(cosmetics_train[-2], cosmetics_train$PrimaryCategory, trials = 5)
# display simple facts about the tree
cosmetics_model
# display detailed information about the tree
summary(cosmetics_model)

## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
cosmetics_pred <- predict(cosmetics_model, cosmetics_test)

sms_test_prob <- predict(cosmetics_model, cosmetics_test, type = "prob")
head(sms_test_prob)
# cross tabulation of predicted versus actual classes
library(gmodels)
answerTable <- CrossTable(cosmetics_test$PrimaryCategory, cosmetics_pred,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted primary category', 'actual primary category'))

#get overall percentage correct and error for test data
answerMatrix<-answerTable[1]$t
index=1
sumTotal=0
side = dim(answerMatrix)[1]
while(index <= side){
  sumTotal=sumTotal+answerMatrix[index,index]
  print(index)
  print(sumTotal)
  index = index + 1
}
percentageCorrect <- (sumTotal/sum(answerMatrix))*100
percentageCorrect
testErrorRate <- 100-percentageCorrect
testErrorRate

