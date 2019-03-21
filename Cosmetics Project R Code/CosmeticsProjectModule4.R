library(e1071)
library(gmodels)
library(plyr)

setwd("/Users/bharila/Documents/Rstuff/CosmeticsTermProject")
cosmetics <- read.csv("cosmeticsCleanedWithTarget.csv", stringsAsFactors = FALSE)

#only keep categorical data
cosmetics <- cosmetics[1:9] 

#convert some columns into type: int to chr
cosmetics$CasId <- as.character(cosmetics$CasId)

#convert some columns into type: chr to factor
cosmetics[2:9] <- lapply(cosmetics[2:9], as.factor)
#overview of data
str(cosmetics)
#leave out ProductName and CSF because there are too many levels and casid because same info about chemicals as chemical name
cosmetics<-cosmetics[ ,-c(2,3,8)]
#check frequencies isDiscontinued
prop.table(table(cosmetics$PrimaryCategory))*100

#training and testing
cosmetics <- cosmetics[sample(nrow(cosmetics)), ]
#overview of data
str(cosmetics)
cosmetics_train <- cosmetics[1:66715, -c(1)]
cosmetics_test  <- cosmetics[66716:88954, -c(1)]

cosmetics_train_labels <- cosmetics[1:66715, ]$IsDiscontinued
cosmetics_test_labels  <- cosmetics[66716:88954, ]$IsDiscontinued
#another
cosmetics$PrimaryCategory <- mapvalues(cosmetics$PrimaryCategory, from=c("Baby Products", "Bath Products", "Fragrances", "Hair Care Products (non-coloring)", 
                            "Hair Coloring Products", "Makeup Products (non-permanent)", "Nail Products", "Oral Hygiene Products", 
                            "Personal Care Products", "Shaving Products", "Skin Care Products ", "Sun-Related Products", 
                            "Tattoos and Permanent Makeup"), to=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"))

#cosmetics_train <- cosmetics[1:66715, -c(4)]
#cosmetics_test  <- cosmetics[66716:88954, -c(4)]

#cosmetics_train_labels <- cosmetics[1:66715, ]$PrimaryCategory
#cosmetics_test_labels  <- cosmetics[66716:88954, ]$PrimaryCategory

#check frequencies isDiscontinued training and testing
prop.table(table(cosmetics_train_labels))*100
prop.table(table(cosmetics_test_labels))*100
#training model on data
cosmetics_classifier <- naiveBayes(cosmetics_train, cosmetics_train_labels)
#evaluating model performance
cosmetics_test_pred <- predict(cosmetics_classifier, cosmetics_test)

cosmetics_test_prob <- predict(cosmetics_classifier, cosmetics_test, type = "raw")
head(cosmetics_test_prob)

cosmetics_results <- data.frame(actual_type = cosmetics_test_labels,
                          predict_type = cosmetics_test_pred,
                          prob_false = round(cosmetics_test_prob[ , 1], 5),
                          prob_true = round(cosmetics_test_prob[ , 2], 5)
                          )

library(ROCR)
pred <- prediction(predictions = cosmetics_results$prob_true,
                   labels = cosmetics_results$actual_type)

# ROC curves
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve for cosmetics discontinued true", col = "blue", lwd = 2)

# add a reference line to the graph
abline(a = 0, b = 1, lwd = 2, lty = 2)

# calculate AUC
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)

answerTable<-CrossTable(cosmetics_test_pred, cosmetics_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r=FALSE,
           dnn = c('predicted', 'actual'))

answerMatrix<-answerTable[1]$t
index=1
sumTotal=0
while(index <= 13){
  sumTotal=sumTotal+answerMatrix[index,index]
  print(index)
  print(sumTotal)
  index = index + 1
}
percentageCorrect <- sumTotal/sum(answerMatrix)
percentageCorrect
