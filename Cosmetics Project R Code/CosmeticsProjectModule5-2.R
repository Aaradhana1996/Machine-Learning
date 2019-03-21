library(class)
library(plyr)
setwd("/Users/bharila/Documents/Rstuff/CosmeticsTermProject")
cosmetics <- read.csv("cosmeticsCleanedWithTargetAsFactor.csv")
#convert some columns into different types
cosmetics[10:15] <- lapply(cosmetics[10:15], as.Date)
cosmetics[10:15] <- lapply(cosmetics[10:15], as.numeric)
#leave out all categorical variables because they have too many levels
#except for isDiscontinued because it has only 2 levels
#isDiscontinued has been converted to a factor already because 
#for some reason the c50 doesn't work with logical type predictors
#leave out DiscontiniuedDate because it has same information as isDiscontinued
cosmetics<-cosmetics[ ,-c(2,3,4,5,7,8,9,12)]
str(cosmetics)
prop.table(table(cosmetics$IsDiscontinued))*100

# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(123)
train_sample <- sample(88954, 80059) #90% training data

str(train_sample)

# split the data frames
cosmetics_train <- cosmetics[train_sample, ]
cosmetics_test  <- cosmetics[-train_sample, ]

# check the proportion of class variable
prop.table(table(cosmetics_train$IsDiscontinued))*100
prop.table(table(cosmetics_test$IsDiscontinued))*100

## Step 3: Training a model on the data ----
# build the simplest decision tree
library(C50)
cosmetics_model <- C5.0(cosmetics_train[-1], cosmetics_train$IsDiscontinued, trials=5)
# display simple facts about the tree
cosmetics_model
# display detailed information about the tree
summary(cosmetics_model)

## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
cosmetics_pred <- predict(cosmetics_model, cosmetics_test)

# cross tabulation of predicted versus actual classes
library(gmodels)
answerTable <- CrossTable(cosmetics_pred, cosmetics_test$IsDiscontinued,
                          prop.chisq = FALSE, prop.r = FALSE, prop.t = FALSE,
                          dnn = c('predicted isDiscontinued', 'actual isDiscontinued'))

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
