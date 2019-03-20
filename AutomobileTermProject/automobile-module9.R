setwd("C:/Users/bharila/Documents/Rstuff/AutomobileTermProject")
automobileWithStrings <- read.csv("automobile.csv", stringsAsFactors = FALSE)
str(automobileWithStrings) 

library(plyr)
automobileWithStrings$num.of.cylinders<-mapvalues(automobileWithStrings$num.of.cylinders, from = c("eight", "five", "four", "six", "three", "twelve", "two"), to =c(8,5,4,6,3,12,2))
automobileWithStrings$fuel.type<-mapvalues(automobileWithStrings$fuel.type, from = c("diesel", "gas"), to = c(1,2))
automobileWithStrings$aspiration<-mapvalues(automobileWithStrings$aspiration, from = c("std", "turbo"), to = c(1,2))
automobileWithStrings$engine.location<-mapvalues(automobileWithStrings$engine.location, from = c("front", "rear"), to = c(1,2))

changetoNA <- function(x){mapvalues(x, from = c("?"), to = NA)}
automobile <- as.data.frame(lapply(automobileWithStrings, changetoNA))

automobile <- as.data.frame(lapply(automobile, as.character))
toint <- function(x){
  return(as.integer(as.character(x)))
}
tonum <- function(x){
  return(as.numeric(as.character(x)))
}
automobile[,c(1:2,4:5,9,11,14,16,17,22,23,24,25,26)]<-as.data.frame(lapply(automobile[,c(1:2,4:5,9,11,14,16,17,22,23,24,25,26)],toint))
automobile[,c(10, 12, 13,19:21)] <- as.data.frame(lapply(automobile[,c(10, 12, 13,19:21)],tonum))
#handle missing data

#dont know what to use to estimate horsepower adn peak rpm and price missing values so remove missing values
automobile <- automobile[complete.cases(automobile[,-c(2,6,19,20)]), ]
str(automobile)
#add factor level "missing" to num of doors
automobile$num.of.doors <- factor(automobile$num.of.doors, levels=c(levels(x), "missing"))
automobile$num.of.doors[is.na(automobile$num.of.doors)] <- "missing"
#estimate
ave_losses <- ave(automobile$normalized.losses, automobile$symboling,
               FUN = function(x) mean(x, na.rm = TRUE))
automobile$normalized.losses <- ifelse(is.na(automobile$normalized.losses), ave_losses, automobile$normalized.losses)
#dont need bore and stroke data
automobile<-automobile[,-c(19,20)]
#remove factor columns since cant deal with non numeric data
automobileWithoutMissing<-automobile[,-c(6,7,8,15,18)]
#Training a model on the data
automobileWIthoutMake <- automobileWithoutMissing[,-c(3)]
#automobileWIthoutBodyStyle <- automobileWithoutMissing[,-c(5)]
#normalise
automobile_z<-automobileWIthoutMake
automobile_z <- as.data.frame(lapply(automobile_z, scale))

set.seed(2345)
make_clusters <- kmeans(automobile_z, 22)

# look at the size of the clusters
make_clusters$size

# look at the cluster centers
make_clusters$centers

# apply the cluster IDs to the original data frame
automobileWithoutMissing$cluster <- make_clusters$cluster

# look at the first five records
automobileWithoutMissing[, c("make","cluster")]
average_cluster<-aggregate(data = automobileWithoutMissing, cluster ~ body.style, mean, na.rm = TRUE)

ave_cluster <- ave(automobileWithoutMissing$cluster, automobileWithoutMissing$make,
                  FUN = function(x) mean(x, na.rm = TRUE))
