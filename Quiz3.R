# Quiz3

## Question 1
## or this quiz we will be using several R packages. R package versions change 
## over time, the right answers have been checked using the following versions 
## of the packages.

## AppliedPredictiveModeling: v1.1.6
## caret: v6.0.47
## ElemStatLearn: v2012.04-0
## pgmm: v1.1
## rpart: v4.1.8

## If you aren't using these versions of the packages, your answers may not 
# exactly match the right answer, but hopefully should be close. Load the cell 
# segmentation data from the AppliedPredictiveModeling package using the commands:

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)

head(segmentationOriginal)

## 1. Subset the data to a training set and testing set based on the Case 
## variable in the data set.

## 2. Set the seed to 125 and fit a CART model with the rpart method using all 
## predictor variables and default caret settings.

## 3. In the final model what would be the final model prediction for cases with 
## the following variable values:

##      a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
##      b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
##      c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
##      d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 

## 1. Subset the data to a training set and testing set based on the Case 
## variable in the data set. 
training<-segmentationOriginal[segmentationOriginal$Case=="Train",]
testing<-segmentationOriginal[segmentationOriginal$Case=="Test",]

## 2. Set the seed to 125 and fit a CART model with the rpart method using all 
## predictor variables and default caret settings. (The outcome class is 
## contained in a factor variable called Class with levels "PS" for poorly 
## segmented and "WS" for well segmented.)
set.seed(125)
model<-train(Class ~ .,
             data = training, 
             method = "rpart")
model$finalModel
plot(model$finalModel, uniform=T)
text(model$finalModel, cex=0.8)

library(ggplot2)
library(rattle)
library("pgmm", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("ElemStatLearn", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("rpart.plot", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

fancyRpartPlot(model$finalModel)
## 3. In the final model what would be the final model prediction for cases with 
## the following variable values:

##      a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
## ---> PS
##      b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
## ---> WS
##      c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
## ---> PS
##      d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 
## ---> ???

## Question 2
## If K is small in a K-fold cross validation is the bias in the estimate of 
## out-of-sample (test set) accuracy smaller or bigger? If K is small is the 
## variance in the estimate of out-of-sample (test set) accuracy smaller or 
## bigger. Is K large or small in leave one out cross validation? 

## ---> The bias is larger and the variance is smaller. Under leave one out 
## --->     cross validation K is equal to the sample size.

## Question 3
##  Load the olive oil data using the commands:
library(pgmm)
data(olive)
olive = olive[,-1]

## These data contain information on 572 different Italian olive oils from 
## multiple regions in Italy. Fit a classification tree where Area is the 
## outcome variable. 
mdlOlive <- train(Area ~ ., method = "rpart2", data = olive)
mdlOlive
## Then predict the value of area for the following data frame using the tree 
## command with all defaults
newdata = as.data.frame(t(colMeans(olive)))
predict(mdlOlive, newdata)  # 2.875
## 2.875. It is strange because Area should be a qualitative variable - but tree 
## is reporting the average value of Area as a numeric variable in the leaf 
## predicted for newdata

## Question 5
## Load the South Africa Heart Disease Data and create training and test sets 
## with the following code:
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

## Then set the seed to 13234 and fit a logistic regression model (method="glm", 
## be sure to specify family="binomial") with Coronary Heart Disease (chd) as 
## the outcome and age at onset, current alcohol consumption, obesity levels, 
## cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol 
## as predictors. 
set.seed(13234)
mdlCHD <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                data=trainSA, 
                method="glm", 
                family="binomial")
mdlCHD
## Calculate the misclassification rate for your model using this function and a 
## prediction on the "response" scale:

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(trainSA$chd, predict(mdlCHD, trainSA))
missClass(testSA$chd, predict(mdlCHD, testSA))
## --> Answer: Training set misclassification rate is 0.27 and test set 
## --> misclassification rate is 0.31

## Question 5
## Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
head(vowel.train)
head(vowel.test)
dim(vowel.train) # 528  11
dim(vowel.test) # 462  11

## Set the variable y to be a factor variable in both the training and test set. 
## Then set the seed to 33833. Fit a random forest predictor relating the factor 
## variable y to the remaining variables. Read about variable importance in 
## random forests here: 
## http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr 
## The caret package uses by default the Gini importance.

## Calculate the variable importance using the varImp function in the caret 
## package. What is the order of variable importance?

## [NOTE: Use randomForest() specifically, not caret, as there's been some 
## issues reported with that approach. 11/6/2016]

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
library("randomForest", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
modelRf <- randomForest(y ~ ., data = vowel.train, importance = FALSE)
order(varImp(modelRf), decreasing=T)
# The order of the variables is:
#  x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10
