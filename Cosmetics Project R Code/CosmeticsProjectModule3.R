#Module 3: Lazy Learning
library(class)
library(gmodels)

setwd("/Users/bharila/Documents/Rstuff/CosmeticsTermProject")
cosmetics <- read.csv("cosmeticsCleaned.csv", stringsAsFactors = FALSE)
#only keep numerical data
#cosmetics <- cosmetics[9:15] 
library(plyr)
cosmetics$PrimaryCategory <- mapvalues(cosmetics$PrimaryCategory, from=c("Baby Products", "Bath Products", "Fragrances", "Hair Care Products (non-coloring)", 
                                                                         "Hair Coloring Products", "Makeup Products (non-permanent)", "Nail Products", "Oral Hygiene Products", 
                                                                         "Personal Care Products", "Shaving Products", "Skin Care Products ", "Sun-Related Products", 
                                                                         "Tattoos and Permanent Makeup"), to=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"))
#convert some columns into type: chr to Date
cosmetics$InitialDateReported <- as.Date(cosmetics$InitialDateReported)
cosmetics$MostRecentDateReported <- as.Date(cosmetics$MostRecentDateReported)
cosmetics$DiscontinuedDate <- as.Date(cosmetics$DiscontinuedDate)
cosmetics$ChemicalCreatedAt <- as.Date(cosmetics$ChemicalCreatedAt)
cosmetics$ChemicalUpdatedAt <- as.Date(cosmetics$ChemicalUpdatedAt)
cosmetics$ChemicalDateRemoved <- as.Date(cosmetics$ChemicalDateRemoved)

#convert dates to numbers
cosmetics$InitialDateReported <- as.numeric(cosmetics$InitialDateReported)
cosmetics$MostRecentDateReported <- as.numeric(cosmetics$MostRecentDateReported)
cosmetics$DiscontinuedDate <- as.numeric(cosmetics$DiscontinuedDate)
cosmetics$ChemicalCreatedAt <- as.numeric(cosmetics$ChemicalCreatedAt)
cosmetics$ChemicalUpdatedAt <- as.numeric(cosmetics$ChemicalUpdatedAt)
cosmetics$ChemicalDateRemoved <- as.numeric(cosmetics$ChemicalDateRemoved)

#add boolean isDiscontinued column
cosmetics <- cbind(IsDiscontinued = 0, cosmetics)
falseifNA <- function(x){
  ifelse(is.na(x), FALSE, TRUE)
}
cosmetics$IsDiscontinued <- falseifNA(cosmetics$DiscontinuedDate)
#normalise
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
cosmetics_n <- cosmetics
#change missing values to ""
#cosmetics_n[is.na(cosmetics_n)] <- ""
#cosmetics_n[9:15] <- as.data.frame(lapply(cosmetics[9:15], normalize))
cosmetics_n$InitialDateReported <- normalize(cosmetics_n$InitialDateReported)
cosmetics_n$MostRecentDateReported <- normalize(cosmetics_n$MostRecentDateReported)
cosmetics_n$ChemicalCreatedAt <- normalize(cosmetics_n$ChemicalCreatedAt)
cosmetics_n$ChemicalUpdatedAt <- normalize(cosmetics_n$ChemicalUpdatedAt)
cosmetics_n$ChemicalCount <- normalize(cosmetics_n$ChemicalCount)

#remove DiscontinuedDate and ChemicalDateRemoved columns because knn cannot deal with missing values
#only keep numerical data
#target variable primary category
cosmetics_n <- cosmetics_n[ ,-c(1:5,7:9,12,15)]
prop.table(table(cosmetics$PrimaryCategory))*100
#target variable isDiscontinued
#cosmetics_n <- cosmetics_n[ ,-c(2:9,12,15)]
#prop.table(table(cosmetics$IsDiscontinued))*100

#cosmetics_n <- cosmetics_n[1:2500, ]
#divide into training and testing data
cosmetics_train <- cosmetics_n[1:7000, 2:6]
cosmetics_test <- cosmetics_n[7000:88954, 2:6]

cosmetics_train_labels <- cosmetics_n[1:7000, 1]
cosmetics_test_labels <- cosmetics_n[7000:88954, 1]

prop.table(table(cosmetics_train_labels))*100
prop.table(table(cosmetics_test_labels))*100

#k is set to sqrt of n=88954 i.e. 298
cosmetics_test_pred <- knn(train = cosmetics_train, test = cosmetics_test, cl = cosmetics_train_labels, k = 230)

answerTable <- CrossTable(x = cosmetics_test_labels, y = cosmetics_test_pred, prop.chisq=FALSE)

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




