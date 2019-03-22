setwd("/Users/bharila/Documents/RStuff/microsoft term project")
# load the grocery data into a sparse matrix
library(arules)
vroots <- read.transactions("msweb.csv", sep = ",")
summary(vroots)

# look at the first five transactions
inspect(vroots[1:5])

# examine the frequency of items
itemFrequency(vroots[, 1:3])

# plot the frequency of items
itemFrequencyPlot(vroots, support = 0.1)
itemFrequencyPlot(vroots, topN = 100)

# a visualization of the sparse matrix for the first five transactions
image(vroots[1:5])

# visualization of a random sample of 100 transactions
image(sample(vroots, 100))

## Step 3: Training a model on the data ----
library(arules)

# default settings result in zero rules learned
apriori(vroots)

# set better support and confidence levels to learn more rules
vrootsrules <- apriori(vroots, parameter = list(support =
                                                      0.01, confidence = 0.25, minlen = 2))
vrootsrules

## Step 4: Evaluating model performance ----
# summary of grocery association rules
summary(vrootsrules)

# look at the first three rules
inspect(vrootsrules[1:3])

## Step 5: Improving model performance ----

# sorting grocery rules by lift
inspect(sort(vrootsrules, by = "lift")[1:5])

# finding subsets of rules containing any berry items
berryrules <- subset(vrootsrules, items %in% "1001")
inspect(sort(berryrules, by = "lift"))

# writing the rules to a CSV file
write(vrootsrules, file = "vrootsrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# converting the rule set to a data frame
vrootsrules_df <- as(vrootsrules, "data.frame")
str(vrootsrules_df)