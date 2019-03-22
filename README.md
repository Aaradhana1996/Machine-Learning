# Machine-Learning

**This folder contains 10 different projects using 10 different machine learning algorithms, and one of the 3 different datasets described below in this document.**

**Cosmetics Dataset:**
All cosmetic companies in California are required to report their products (since 2005) if they could cause cancer, developmental birth defects, or harm the reproductive system. This dataset contains information about them: company names, chemical names, cosmetic types, date product was registered, date product was discontinued (if it was) etc...

CosmeticsProject.R writes the new excel file CosmeticsCleanedWithTarget.csv.
CosmeticsProjectModule2.R writes the new excel file CosmeticsCleanedWithTargetAsFactor.csv.

1. CosmeticsProjectModule3.R: k-NN nearest neighbour
2. CosmeticsProjectModule4.R: Naive Bayes
3. CosmeticsProjectModule5.R: Decision Trees with PrimaryCategory as target variable
4. CosmeticsProjectModule5-2.R: Decision Trees with IsDiscontinued as target variable
5. CosmeticsProjectModule5Rules.R: Rule Learners with IsDiscontinued as target variable
6. CosmeticsProjectModule5Rules2.R: Rule Learners with PrimaryCategory as target variable

**Automobile Insurance Dataset:**
This dataset is about automobiles and insurance information related to that. The target variable is normalized losses (relative average loss payment per insured vehicle year), and there were 25 predictors involved. Since the list is too long, I have simply attached an excel document in case anyone is interested. 

7. automobile-module6.R: Regression
8. automobile-module7.R: Neural Nets and Support Vectors
9. automobile-module9.R: Clustering with k-means

**Microdoft Website Dataset:**
More information about the microsoft dataset can be found in the module 8 description. Module 8 is the only module using this dataset. The Microsoft-Data-Csv folder contains the Java code used to convert the csv files into a format that could be used for the R code.

10. module 8.R: Association Rules

**All the above R files have a corresponding description document for them.
Finally, the ModelPerformanceEvaluation document reports the performance of the Naive Bayes implementation.**
