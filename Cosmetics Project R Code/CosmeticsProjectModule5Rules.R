#Module 5: Classification using Decision Trees and Rules
#### Part 2: Rule Learners -------------------

## Example: Identifying Poisonous Mushrooms ----
## Step 2: Exploring and preparing the data ----
setwd("/Users/bharila/Documents/Rstuff/CosmeticsTermProject")
cosmetics <- read.csv("cosmeticsCleanedWithTargetAsFactor.csv")

#convert some columns into different types
cosmetics[10:15] <- lapply(cosmetics[10:15], as.Date)
cosmetics[10:15] <- lapply(cosmetics[10:15], as.numeric)
#remove product name, csf and casid, and discontinueddate
cosmetics<-cosmetics[ ,-c(2,3,8,12)]
# examine the structure of the data frame
str(cosmetics)

# examine the class distribution
prop.table(table(cosmetics$IsDiscontinued))*100

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
library(RWeka)

# train OneR() on the data
cosmetics_1R <- OneR(IsDiscontinued ~ ., data = cosmetics_train)

## Step 4: Evaluating model performance ----
cosmetics_1R
summary(cosmetics_1R)

cosmetics_pred <- predict(cosmetics_1R, cosmetics_test)

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

## Step 5: Improving model performance ----
cosmetics_JRip <- JRip(IsDiscontinued ~ ., data = cosmetics_train)
cosmetics_JRip
summary(cosmetics_JRip)
cosmetics_pred_jrip <- predict(cosmetics_JRip, cosmetics_test)

answerTablejrip <- CrossTable(cosmetics_pred_jrip, cosmetics_test$IsDiscontinued,
                          prop.chisq = FALSE, prop.r = FALSE, prop.t = FALSE,
                          dnn = c('predicted isDiscontinued', 'actual isDiscontinued'))

#get overall percentage correct and error for test data
answerMatrixjrip<-answerTablejrip[1]$t
index=1
sumTotal=0
side = dim(answerMatrixjrip)[1]
while(index <= side){
  sumTotal=sumTotal+answerMatrixjrip[index,index]
  print(index)
  print(sumTotal)
  index = index + 1
}
percentageCorrectjrip <- (sumTotal/sum(answerMatrixjrip))*100
percentageCorrectjrip
testErrorRatejrip <- 100-percentageCorrectjrip
testErrorRatejrip

