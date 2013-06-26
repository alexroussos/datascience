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

# See how well Category predicts salary
# Too many Categories -- length(levels(data.train$Category)) = 28
model <- lm(SalaryNormalized ~ Category, data.train)
mae(predict(model, data.test), data.test$SalaryNormalized) # 10335
summary(model) # R2 = 0.1446
# Create an aggregate category: WhiteCollar=(Accounting, Engineering, Legal, IT, Cosultancy)
#TODO use function, pass list of job fields
data.train$WhiteCollar <- grepl('IT', data.train$Category) | grepl('Engineer', data.train$Category) | grepl('Finance', data.train$Category) | grepl('Legal', data.train$Category) | grepl('Consult', data.train$Category)
data.test$WhiteCollar <- grepl('IT', data.test$Category) | grepl('Engineer', data.test$Category) | grepl('Finance', data.test$Category) | grepl('Legal', data.test$Category) | grepl('Consult', data.test$Category)  
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

# Look at whether Title contains words like "Senior" or "Manager" or "Head"
data.train$Manager <- grepl('Senior', data.train$Title, ignore.case=TRUE) | grepl('Head', data.train$Title,ignore.case=TRUE) | grepl('Manager', data.train$Title, ignore.case=TRUE)
data.test$Manager <- grepl('Senior', data.test$Title, ignore.case=TRUE) | grepl('Head', data.test$Title,ignore.case=TRUE) | grepl('Manager', data.test$Title, ignore.case=TRUE)
model <- lm(SalaryNormalized ~ Manager, data.train)
mae(predict(model, data.test), data.test$SalaryNormalized) # 11128.17
summary(model) # R2 = 0.057

##########################################################################################
# PART 3: Try DAAG for cross-validation
##########################################################################################

library('DAAG')
cv.lm(df=data, form.lm = formula(SalaryNormalized ~ Category + ContractTime), m=3) # plot: dots=FALSE, plotit=TRUE

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

##########################################################################################
# FINAL: Process test set and export predictions
##########################################################################################
final.data.test <- read.csv("test.csv")
final.data.train <- data.train # TODO read in larger training file

# TODO adding these columns should be a function so it can be done with same code as above

# Adding level isn't working, cheating by putting it into the training set
# Why no work: levels(final.data.train$Category) <- c(levels(final.data.train$Category), "Part time Jobs")
final.data.train[1,]$Category <- "Part time Jobs"
final.data.train[2,]$Category <- "Graduate Jobs"

# Top Sources
final.sources.counts <- summary(final.data.train$SourceName)
final.top.sources <- names(final.sources.counts[order(final.sources.counts, decreasing= TRUE)][1:10])
final.data.train$TopSource <- factor(final.data.train$Source, levels=final.top.sources)
final.data.train$TopSource[is.na(final.data.train$TopSource)] <- "Other"
final.data.test$TopSource <- factor(final.data.test$Source, levels=final.top.sources)
final.data.test$TopSource[is.na(final.data.test$TopSource)] <- "Other"

# Top Locations
final.locations.counts <- summary(final.data.train$LocationNormalized)
final.top.locations <- names(final.locations.counts[order(final.locations.counts, decreasing= TRUE)][1:10])
final.data.train$TopLocation <- factor(final.data.train$LocationNormalized, levels=final.top.locations)
final.data.train$TopLocation[is.na(final.data.train$TopLocation)] <- "(Other)"
final.data.train$TopLocation[final.data.train$TopLocation == "UK"] <- "(Other)"
final.data.test$TopLocation <- factor(final.data.test$LocationNormalized, levels=final.top.locations)
final.data.test$TopLocation[is.na(final.data.test$TopLocation)] <- "(Other)"
final.data.test$TopLocation[final.data.test$TopLocation == "UK"] <- "(Other)"

# Title contains words like "Senior" or "Manager" or "Head"
final.data.train$Manager <- grepl('Senior', final.data.train$Title, ignore.case=TRUE) | grepl('Head', final.data.train$Title,ignore.case=TRUE) | grepl('Manager', final.data.train$Title, ignore.case=TRUE)
final.data.test$Manager <- grepl('Senior', final.data.test$Title, ignore.case=TRUE) | grepl('Head', final.data.test$Title,ignore.case=TRUE) | grepl('Manager', final.data.test$Title, ignore.case=TRUE)

# Build the final model
final.model <- lm(SalaryNormalized ~ Category:ContractTime + TopLocation + TopSource + Manager, final.data.train)
final.predictions <- predict(final.model, final.data.test)
final.output <- data.frame(final.data.test$Id, Salary=final.predictions)
mae(predict(final.model, final.data.train), final.data.train$SalaryNormalized) # TODO why are some predictions NA?
summary(final.model) # R2 = 0.034
write.csv(final.output, "my_submission.csv", row.names=FALSE)
