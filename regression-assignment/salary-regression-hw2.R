# Alex Roussos, 26 Jun 2013
# Regression Assignment (HW #2) for GADS4
# Predicting salary based on job data, based on Adzuna competition
# http://www.kaggle.com/c/job-salary-prediction

##########################################################################################
# PART 1: Split data
##########################################################################################

# Create training and test data sets
# setwd('./ga-datascience/datascience_repo/regression-assignment/')
data <- read.csv("train.csv", header=TRUE)
train.indices <- sample(1:nrow(data), 0.7 * nrow(data))
data.train <- data[train.indices,]
data.test <- data[-train.indices,]

##########################################################################################
# PART 2: Try some models
##########################################################################################

# Load helper function
mae <- function(x,y)
{
  sum( abs(x-y) ) /length(x)
}

# See what might be interesting to study
summary(data)

# Model ContractType (full/parttime) or ContractTime (temp/perm)
model <- lm(SalaryNormalized ~ ContractType + ContractTime, data.train)
mae(predict(model, data.test), data.test$SalaryNormalized) # 11284.07
summary(model) # R2 = 0.074
# Perhaps the interaction of these is meaningful
model <- lm(SalaryNormalized ~ ContractType:ContractTime, data.train)
mae(predict(model, data.test), data.test$SalaryNormalized) # 11114.51
summary(model) # R2 = 0.079

# ONLY LM has r-squared - can approx - see https://stat.ethz.ch/pipermail/r-help/2010-June/243113.html
#R2 <- cor(data.training$SalaryNormalized, predict(model))^2

# See how well Category predicts salary
# Too many Categories -- length(levels(data.train$Category)) = 28
model <- lm(SalaryNormalized ~ Category, data.train)
mae(predict(model, data.test), data.test$SalaryNormalized) # 10335
summary(model) # R2 = 0.1446
# Create an aggregate category: WhiteCollar=(Accounting, Engineering, Legal, IT, Cosultancy)
#TODO use function, pass list of job fields
data.train$WhiteCollar <- grepl('IT', data.mini$Category) | grepl('Engineer', data.mini$Category) | grepl('Finance', data.mini$Category) | grepl('Legal', data.mini$Category) | grepl('Consult', data.mini$Category)
data.test$WhiteCollar <- grepl('IT', data.mini$Category) | grepl('Engineer', data.mini$Category) | grepl('Finance', data.mini$Category) | grepl('Legal', data.mini$Category) | grepl('Consult', data.mini$Category)  
model <- lm(SalaryNormalized ~ WhiteCollar, data.train)
mae(predict(model, data.test), data.test$SalaryNormalized) # 11933
summary(model) # R2 = 2.31e-05
# Well, WhiteCollar wasn't an improvement over Category

# Also too many sources to model -- length(levels(data.train$SourceName)) = 74
# Try top 10 Sources
sources.counts <- summary(data.train$SourceName)
top.sources <- names(sources.counts[order(sources.counts, decreasing= TRUE)][1:10])
data.train$TopSource <- factor(data.train$Source, levels=top.sources)
data.train$TopSource[is.na(data.train$TopSource)] <- "Other"
data.test$TopSource <- factor(data.test$Source, levels=top.sources)
data.test$TopSource[is.na(data.test$TopSource)] <- "Other"
model <- lm(SalaryNormalized ~ TopSource, data.train)
mae(predict(model, data.test), data.test$SalaryNormalized) # "NA" - TODO why?
summary(model) # R2 = 0.171
# Source (website) looks moderately useful

# Get the top locations and remove "UK" because too coarse
locations.counts <- summary(data$LocationNormalized)
top.locations <- names(locations.counts[order(locations.counts, decreasing= TRUE)][1:10])
data.train$TopLocation <- factor(data.train$LocationNormalized, levels=top.locations)
data.train$TopLocation[is.na(data.train$TopLocation)] <- "(Other)"
data.train$TopLocation[data.train$TopLocation == "UK"] <- "(Other)"
data.test$TopLocation <- factor(data.test$LocationNormalized, levels=top.locations)
data.test$TopLocation[is.na(data.test$TopLocation)] <- "(Other)"
data.test$TopLocation[data.test$TopLocation == "UK"] <- "(Other)"
model <- lm(SalaryNormalized ~ TopLocation, data.train)
mae(predict(model, data.test), data.test$SalaryNormalized) # 11317
summary(model) # R2 = 0.089
# Not very useful -- probably too many "(Other)" locations as it is top by far

# Examine interaction - maybe ContractTime (temp/perm) applies differently depending on the Category
model <- lm(SalaryNormalized ~ Category:ContractTime, data.train)
mae(predict(model, data.test), data.test$SalaryNormalized) # 10291
summary(model) # R2 = 0.221

##########################################################################################
# PART 3: Try DAAG for cross-validation
##########################################################################################

library('DAAG')

data$TopSource <- factor(data$Source, levels=top.sources)
data$TopSource[is.na(data$TopSource)] <- "Other Source"

result <- cv.lm(df=data, form.lm = formula(SalaryNormalized ~ Category + ContractTime), m=3) # plot: dots=FALSE, plotit=TRUE

##########################################################################################
# PART 4: Merge Location_Tree.csv
##########################################################################################

read.csv('Location_Tree.csv')

# TODO Merge location_tree.csv and note effect on performance
# 1 - get list of regions from tree
# 2 - remove rows from data whose LocationNormalized is not in the list of regions
# 3 - set the Region column 
# 4 - now have location column at consistent granularity (eg region)

##########################################################################################
# PART 5: Build a GLM Model (*)
##########################################################################################

##########################################################################################
# PART 6: Add text features (**)
##########################################################################################

##########################################################################################
# PART 7: Try a larger file (**)
##########################################################################################

##########################################################################################
# PART 8: Use vowpal wabbit with largest set (***)
##########################################################################################

