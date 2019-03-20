setwd("/Users/bharila/Documents/Rstuff/AutomobileTermProject")
automobileWithStrings <- read.csv("automobile.csv", stringsAsFactors = FALSE)

library(plyr)
automobileWithStrings$num.of.doors<-mapvalues(automobileWithStrings$num.of.doors, from = c("two", "four"), to = c(2,4))
automobileWithStrings$num.of.cylinders<-mapvalues(automobileWithStrings$num.of.cylinders, from = c("eight", "five", "four", "six", "three", "twelve", "two"), to =c(8,5,4,6,3,12,2))
automobileWithStrings$fuel.type<-mapvalues(automobileWithStrings$fuel.type, from = c("diesel", "gas"), to = c(1,2))
automobileWithStrings$aspiration<-mapvalues(automobileWithStrings$aspiration, from = c("std", "turbo"), to = c(1,2))
automobileWithStrings$engine.location<-mapvalues(automobileWithStrings$engine.location, from = c("front", "rear"), to = c(1,2))

#removes string data and converts all to numeric
automobileWithMissingData <- automobileWithStrings[,-c(3,7,8,9,15,18)]
changetoNA <- function(x){mapvalues(x, from = c("?"), to = NA)}
automobileWithMissingData <- as.data.frame(lapply(automobileWithMissingData, changetoNA))
#removes missing data
automobile <- automobileWithMissingData[complete.cases(automobileWithMissingData), ]
#automobile <- na.omit(automobileWithMissingData)

automobile <- as.data.frame(lapply(automobile, as.character))
toint <- function(x){
  return(as.integer(as.character(x)))
}
tonum <- function(x){
  return(as.numeric(as.character(x)))
}
automobile[,c(1:5,10:12,16:20)]<-as.data.frame(lapply(automobile[,c(1:5,10:12,16:20)],toint))
automobile[,-c(1:5,10:12,16:20)] <- as.data.frame(lapply(automobile[,-c(1:5,10:12,16:20)],tonum))
str(automobile)

# custom normalization function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
automobile_norm <- as.data.frame(lapply(automobile, normalize))
str(automobile_norm)

# create a random sample for training and test data
set.seed(12345)
train_sample <- sample(159, 120) #90% training data

# split the data frames
automobile_train <- automobile_norm[train_sample, ]
automobile_test <- automobile_norm[-train_sample, ]

library(neuralnet)

# simple ANN with only a single hidden neuron
set.seed(12345) # to guarantee repeatable results
myform <- as.formula(paste0('normalized.losses ~ ', 
                            paste(names(automobile_train[!names(automobile_train) %in% 'normalized.losses']),
                                  collapse = ' + ')))
#automobile_model <- neuralnet(automobile_train$normalized.losses ~ ., data = automobile_train[,c(1,3:16)])
automobile_model <- neuralnet(myform, data = automobile_train, hidden=10)

# visualize the network topology
plot(automobile_model)

## Step 4: Evaluating model performance ----
# obtain model results
model_results <- compute(automobile_model, automobile_test[c(1,3:20)])
# obtain predicted strength values
predicted_losses <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_losses, automobile_test$normalized.losses)

#support vector
library(kernlab)

toCategory <- function(x){
  if(x < 95){
    "very low"
  }
  else if(x < 125){
    "low"
  }
  else if(x < 155){
    "medium"
  }
  else if(x < 185){
    "high"
  }
  else{
    "very high"
  }
}
automobile$normalized.losses<-sapply(automobile$normalized.losses, function(x) toCategory(x))
# create a random sample for training and test data
set.seed(12345)
train_sample <- sample(159, 120) 
# split the data frames
automobile_train2 <- automobile[train_sample, ]
automobile_test2 <- automobile[-train_sample, ]
letter_classifier <- ksvm(normalized.losses ~ ., data = automobile_train2,
                          kernel = "anovadot")

# look at basic information about the model
letter_classifier

## Step 4: Evaluating model performance ----
# predictions on testing dataset
letter_predictions <- predict(letter_classifier, automobile_test2)

head(letter_predictions)

table(letter_predictions, automobile_test2$normalized.losses)

# look only at agreement vs. non-agreement
# construct a vector of TRUE/FALSE indicating correct/incorrect predictions
agreement <- letter_predictions == automobile_test2$normalized.losses
table(agreement)
prop.table(table(agreement))
