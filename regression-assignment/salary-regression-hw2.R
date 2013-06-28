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
# summary(data.test)
# hist(data.test$SalaryNormalized)
# hist(log(data.test$SalaryNormalized)) # this looks more normal, try modeling it

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
mae(predict(model, data.test), data.test$SalaryNormalized)
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

# Look at whether Title contains words like Senior, Manager, Head, Director
data.train$Manager <- grepl('Senior', data.train$Title, ignore.case=TRUE) | grepl('Head', data.train$Title,ignore.case=TRUE) | grepl('Manager', data.train$Title, ignore.case=TRUE) | grepl('Director', data.train$Title, ignore.case=TRUE)
data.test$Manager <- grepl('Senior', data.test$Title, ignore.case=TRUE) | grepl('Head', data.test$Title,ignore.case=TRUE) | grepl('Manager', data.test$Title, ignore.case=TRUE) | grepl('Director', data.test$Title, ignore.case=TRUE)
model <- lm(SalaryNormalized ~ Manager, data.train)
mae(predict(model, data.test), data.test$SalaryNormalized) # 10990
summary(model) # R2 = 0.069

# See if modeling log(salary) is an improvement
model <- lm(log(SalaryNormalized) ~ ContractType + ContractTime, data.train)
mae(exp(predict(model, data.test)), data.test$SalaryNormalized) # 10737
summary(model) # R2 = 0.431
# This is a huge improvement!

##########################################################################################
# PART 3: Try DAAG for cross-validation
##########################################################################################

library('DAAG')
cv.lm(df=data, form.lm = formula(SalaryNormalized ~ Category + ContractTime), m=3) # plot: dots=FALSE, plotit=TRUE

##########################################################################################
# PART 4: Merge Location_Tree.csv
##########################################################################################

# Most common value of LocationNormalized is a city, so merge on that.
# Column 3 of Location_Tree seems to generally be a city.
# This will drop any rows whose LocationNormalized is not a city and give us the Country, Region and Neighborhood
location.tree <- read.csv('Location_Tree2.csv', col.names=c("Country","Region","City","Neighborhood"), header=FALSE)
data.location.tree <- merge(data, location.tree, by.x="LocationNormalized", by.y="City")

# Re-split
train.indices <- sample(1:nrow(data.location.tree), 0.7 * nrow(data.location.tree))
data.location.tree.train <- data.location.tree[train.indices,]
data.location.tree.test <- data.location.tree[-train.indices,]

# Let's see if any geographic granularities are useful
# Country is always "UK" so skip that, and Neighborhood too granular
# Region is not too useful:
model <- lm(SalaryNormalized ~ Region, data.location.tree.train)
mae(predict(model, data.location.tree.test), data.location.tree.test$SalaryNormalized) # 11135
summary(model) # R2 = 0.047
# LocationNormalized is just City now
model <- lm(SalaryNormalized ~ LocationNormalized, data.location.tree.train)
mae(predict(model, data.location.tree.test), data.location.tree.test$SalaryNormalized) # 10846
summary(model) # R2 = 0.091

##########################################################################################
# PART 5: Build a GLM Model (*)
##########################################################################################
library('glmnet')

glm.model <- glmnet(as.matrix(data.train[,c('Manager')]), as.matrix(data.train$SalaryNormalized))
glm.model.cv <- cv.glmnet(as.matrix(data.train[,c('Manager')]), as.matrix(data.train$SalaryNormalized))
glm.predict <- predict(glm.model, as.matrix(data.test[,c('Manager')]))
glm.predict.cv <- predict(glm.model.cv, as.matrix(data.test[,c('Manager')]), s=glm.model.cv$lambda.min)
mae(glm.predict, data.test$SalaryNormalized) # 11362
mae(glm.predict.cv, data.test$SalaryNormalized) #11254

# GLM CV improved MAE. Would run with more columns but getting error when including them (must need cleaning up)
#   Error in elnet(x, is.sparse, ix, jx, y, weights, offset, type.gaussian,  : NA/NaN/Inf in foreign function call (arg 5)

##########################################################################################
# PART 6: Add text features (**)
##########################################################################################
library('tm')

# Add a column for descriptions containing 'bonus'
src <- DataframeSource(data.frame(data$FullDescription))
c <- Corpus(src)
dtm<- DocumentTermMatrix(c)
text.data <- cbind(data, as.matrix(dtm[,'bonus']))

# Re-split data                    
train.indices <- sample(1:nrow(text.data), 0.7 * nrow(text.data))
text.data.train <- text.data[train.indices,]
text.data.test <- text.data[-train.indices,]
                       
text.model <- lm(SalaryNormalized ~ bonus, data = text.data)
mae(predict(text.model, text.data.test), text.data.test$SalaryNormalized) # 11672
summary(text.model) # R2 = 0.003             

# TODO do something like spam problem but, instead of spam vs non-spam, find words for high-paying vs low-paying 

##########################################################################################
# PART 7: Try a larger file (**)
##########################################################################################
# Increasing training file size from 10k to 50k 
# - increased R2 from 0.315 to 0.354
# - decreased MAE from 10202 to 10503

##########################################################################################
# PART 8: Use vowpal wabbit with largest set (***)
##########################################################################################

##########################################################################################
# FINAL: Process test set and export predictions
##########################################################################################

final.data.test <- read.csv("test.csv")
final.data.train <- read.csv("train_50k.csv", header=TRUE) # TODO read in larger training file

# Top Sources
final.sources.counts <- summary(final.data.train$SourceName)
final.top.sources <- names(final.sources.counts[order(final.sources.counts, decreasing= TRUE)][1:10])
final.data.train$TopSource <- factor(final.data.train$Source, levels=final.top.sources)
levels(final.data.train$TopSource) <- c(levels(final.data.train$TopSource), "Other")
final.data.train$TopSource[is.na(final.data.train$TopSource)] <- "Other"
final.data.test$TopSource <- factor(final.data.test$Source, levels=final.top.sources)
levels(final.data.test$TopSource) <- c(levels(final.data.test$TopSource), "Other")
final.data.test$TopSource[is.na(final.data.test$TopSource)] <- "Other"

# Top Locations
final.locations.counts <- summary(final.data.train$LocationNormalized)
final.top.locations <- names(final.locations.counts[order(final.locations.counts, decreasing= TRUE)][1:10])
final.data.train$TopLocation <- factor(final.data.train$LocationNormalized, levels=final.top.locations)
levels(final.data.train$TopLocation) <- c(levels(final.data.train$TopLocation), "(Other")
final.data.train$TopLocation[is.na(final.data.train$TopLocation)] <- "(Other)"
final.data.train$TopLocation[final.data.train$TopLocation == "UK"] <- "(Other)"
final.data.test$TopLocation <- factor(final.data.test$LocationNormalized, levels=final.top.locations)
levels(final.data.test$TopLocation) <- c(levels(final.data.test$TopLocation), "(Other")
final.data.test$TopLocation[is.na(final.data.test$TopLocation)] <- "(Other)"
final.data.test$TopLocation[final.data.test$TopLocation == "UK"] <- "(Other)"

# Title contains words like "Senior" or "Manager" or "Head" or "Director"
final.data.train$Manager <- grepl('Senior', final.data.train$Title, ignore.case=TRUE) | grepl('Head', final.data.train$Title,ignore.case=TRUE) | grepl('Manager', final.data.train$Title, ignore.case=TRUE) | grepl('Director', final.data.train$Title, ignore.case=TRUE)
final.data.test$Manager <- grepl('Senior', final.data.test$Title, ignore.case=TRUE) | grepl('Head', final.data.test$Title,ignore.case=TRUE) | grepl('Manager', final.data.test$Title, ignore.case=TRUE) | grepl('Director', final.data.test$Title, ignore.case=TRUE)

# Build the final model
final.model <- lm(log(SalaryNormalized) ~ Category:ContractTime + TopLocation + TopSource + Manager, final.data.train)
final.model$xlevels$Category <- c(final.model$xlevels$Category, "Part time Jobs")
final.predictions <- exp(predict(final.model, final.data.test))
final.output <- data.frame(final.data.test$Id, Salary=final.predictions)
mae(exp(predict(final.model, final.data.train)), final.data.train$SalaryNormalized) # 10035 (10k), 11372 (50k)
summary(final.model) # R2 = 0.365 (10k), 0.428 (50k)
write.csv(final.output, "my_submission.csv", row.names=FALSE)
