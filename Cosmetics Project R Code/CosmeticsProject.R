#About the data
# All cosmetic companies in California are required to report their
#products (since 2005) if they could cause cancer, developmental birth defects, 
#or harm the reproductive system. This dataset contains information about them.

#load csv
setwd("/Users/bharila/Documents/Rstuff/CosmeticsTermProject")
cosmetics <- read.csv("cosmetics.csv", stringsAsFactors = FALSE)

#info about variables
str(cosmetics)

#cleaning up dataset
#1. remove CDPHId because there is no extra information; 
#some ids have been repeated where the product name etc. is the same and there are only differences in chemical name or csf,
#however there are too many categories (33353) of CDPHIds for any analysis to be useful.
#2. remove CSFId, CompanyId, PrimaryCategoryId and SubCategoryId because they contain the same information as
# CSF, CompanyName, PrimaryCategory and SubCategory respectively. 
#3. remove CasNumber because it contains the same information as CasId.
# We are keeping CasId because it is issued by CDPH instead of specific manufacturers
#4. remove ChemicalId because ChemicalId is the chemical name CDPH has assigned specific to the product so two different 
#products with the same chemcial can have two different ChemicalIds. We don't want that. 
cosmetics <- cosmetics[ ,-c(1, 3, 5, 8, 10, 13, 14)]

#convert some columns into type: int to chr
cosmetics$CasId <- as.character(cosmetics$CasId)

#convert some columns into type: chr to factor
cosmetics$ProductName <- as.factor(cosmetics$ProductName)
cosmetics$CSF <- as.factor(cosmetics$CSF)
cosmetics$CompanyName <- as.factor(cosmetics$CompanyName)
cosmetics$BrandName <- as.factor(cosmetics$BrandName)
cosmetics$PrimaryCategory <- as.factor(cosmetics$PrimaryCategory)
cosmetics$SubCategory <- as.factor(cosmetics$SubCategory)
cosmetics$CasId <- as.factor(cosmetics$CasId)
cosmetics$ChemicalName <- as.factor(cosmetics$ChemicalName)

#convert some columns into type: chr to Date
cosmetics$InitialDateReported <- as.Date(cosmetics$InitialDateReported,"%m/%d/%Y")
cosmetics$MostRecentDateReported <- as.Date(cosmetics$MostRecentDateReported,"%m/%d/%Y")
cosmetics$DiscontinuedDate <- as.Date(cosmetics$DiscontinuedDate,"%m/%d/%Y")
cosmetics$ChemicalCreatedAt <- as.Date(cosmetics$ChemicalCreatedAt,"%m/%d/%Y")
cosmetics$ChemicalUpdatedAt <- as.Date(cosmetics$ChemicalUpdatedAt,"%m/%d/%Y")
cosmetics$ChemicalDateRemoved <- as.Date(cosmetics$ChemicalDateRemoved,"%m/%d/%Y")

#overview of data
str(cosmetics)
summary(cosmetics[, 9:15])
table(cosmetics$ChemicalCount)

#get frequency percentages, sort into descending order, and plot
t <- round(prop.table(table(cosmetics$CompanyName))*100, 0)
t <- t[order(-t)]
t

plot(t)

t <- round(prop.table(table(cosmetics$BrandName))*100, 0)
t <- t[order(-t)]
t

plot(t)

t <- round(prop.table(table(cosmetics$PrimaryCategory))*100, 0)
t <- t[order(-t)]
t

plot(t)

t <- round(prop.table(table(cosmetics$SubCategory))*100, 0)
t <- t[order(-t)]
t

plot(t)

t <- round(prop.table(table(cosmetics$CasId))*100, 0)
t <- t[order(-t)]
t

plot(t)

t <- round(prop.table(table(cosmetics$ChemicalName))*100, 0)
t <- t[order(-t)]
t

plot(t)

write.csv(cosmetics, file = "cosmeticsCleaned.csv", row.names = FALSE)

#add boolean isDiscontinued column
cosmetics <- cbind(IsDiscontinued = 0, cosmetics)
falseifNA <- function(x){
  ifelse(is.na(x), FALSE, TRUE)
}
cosmetics$IsDiscontinued <- falseifNA(cosmetics$DiscontinuedDate)

write.csv(cosmetics, file = "cosmeticsCleanedWithTarget.csv", row.names = FALSE)
