'''
https://github.com/arahuja/GADS4/wiki/Logistic-Regression-and-Naive-Bayes-Assignment

@author: aroussos
'''

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
1) Split the data
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
import pandas
train = pandas.read_csv('data/train-utf8.csv')
test = pandas.read_csv('data/test-utf8.csv')

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
2) Build a Logistic Regression model
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.cross_validation import cross_val_score
from sklearn.metrics import auc_score

vectorizer = CountVectorizer()
trainX = vectorizer.fit_transform(train.Comment)
scores = cross_val_score(LogisticRegression(), trainX, train.Insult, score_func=auc_score)
print "LogisticRegression score = ", scores.mean() # 0.754

# None of these options seem to improve the score:
#     stop_words='english' - lowers score by ~ 0.03
#     lowercase=true       - no effect
#     ngram_range=(1,5)    - lowers score by ~ 0.002
#     min_df=2             - no effect
# TODO find a way to do stemming

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
3) Build a Naive Bayes model
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
from sklearn.naive_bayes import MultinomialNB

scores = cross_val_score(MultinomialNB(), trainX, train.Insult, score_func=auc_score)
print "MultinomialNB score = ", scores.mean() # 0.777

# Not fitting priors has a slight negative impact on score 
# TODO what is class_prior param?
scores = cross_val_score(MultinomialNB(fit_prior=False), trainX, train.Insult, score_func=auc_score)
print "MNB(prior=F) score = ", scores.mean() # 0.761


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
4) Use Vowpal Wabbit to build LR model
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
5) Use quadratic or cubic features in Vowpal Wabbit
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Predict using best model and output to file
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
model = MultinomialNB().fit(trainX, list(train.Insult))
testX = vectorizer.transform(test.Comment)
predictions = model.predict_proba(testX)[:,1]
submission = pandas.DataFrame({"Id": test.id, "Insult": predictions})
submission.to_csv("submission.csv", index=False)
