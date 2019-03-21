setwd("/Users/bharila/Documents/Rstuff/CosmeticsTermProject")
cosmetics <- read.csv("cosmeticsCleanedWithTargetAsFactor.csv")
cosmetics[10:15] <- lapply(cosmetics[10:15], as.Date)
cosmetics[10:15] <- lapply(cosmetics[10:15], as.numeric)
cosmetics<-cosmetics[,-c(2:9, 12, 15)]
str(cosmetics)

# summarize the charges variable
summary(cosmetics$ChemicalCount)

# histogram of insurance charges
hist(cosmetics$ChemicalCount)

#isDiscontinued
cosmetics$IsDiscontinued <- ifelse(cosmetics$IsDiscontinued=="yes", 1, 0)

# exploring relationships among features: correlation matrix
cor(cosmetics[])

# visualing relationships among features: scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "expenses")])

# more informative scatterplot matrix
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])