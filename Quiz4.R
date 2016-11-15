# Quiz 4

# Quesion 1
# Load these libraries
library(AppliedPredictiveModeling)
library(caret)
library(ElemStatLearn)
library(pgmm)
library(rpart)
library(gbm)
library(lubridate)
library(forecast)
library(e1071)
# Load the vowel.train and vowel.test data sets:
data(vowel.train)
data(vowel.test) 

# Set the variable y to be a factor variable in both the training and test set. 
# Then set the seed to 33833. Fit (1) a random forest predictor relating the 
# factor variable y to the remaining variables and (2) a boosted predictor using 
# the "gbm" method. Fit these both with the train() command in the caret package.

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

# random forest predictor algorithm 
mdlRF <- train(y ~., data = vowel.train, method = "rf")

# boosting predictor algorithm 
mdlGBM <- train(y ~., data = vowel.train, method = "gbm", verbose = FALSE)

# make predictions against the vowel.test data set
predRF <- predict(mdlRF, vowel.test)
predGBM <- predict(mdlGBM, vowel.test)

# calculate the confusion matrices
cmRF <- confusionMatrix(predRF, vowel.test$y)
cmGBM <- confusionMatrix(predGBM, vowel.test$y)

# What are the accuracies for the two approaches on the test data set? 
# RF Accuracy = 
cmRF$overall[1]
# GBM Accuracy = 
cmGBM$overall[1]

# What is the accuracy among the test set samples where the two methods agree? 
pred <- data.frame(predRF, predGBM, y=vowel.test$y, agree=predRF == predGBM)
head(pred)
accuracy <- sum(predRF[pred$agree] == pred$y[pred$agree]) / sum(pred$agree)
# Agreement Accuracy = 
accuracy 

# Question 2
# Load the Alzheimer’s data using the following commands
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain, ]
testing = adData[-inTrain, ]

# Set the seed to 62433 and predict diagnosis with all the other variables 
# using a random forest ("rf"), boosted trees ("gbm") and linear discriminant 
# analysis ("lda") model. 
set.seed(62433)
mdlRF <- train(diagnosis ~ ., data = training, method = "rf")
mdlGBM <- train(diagnosis ~ ., data = training, method = "gbm")
mdlLDA <- train(diagnosis ~ ., data = training, method = "lda")

predRF <- predict(mdlRF, testing)
predGBM <- predict(mdlGBM, testing)
predLDA <- predict(mdlLDA, testing)
    
# Stack the predictions together using random forests ("rf"). 
predSTACK <- data.frame(predRF, predGBM, predLDA, diagnosis = testing$diagnosis)
combModFit <- train(diagnosis ~ ., method = "rf", data = predSTACK)
combPred <- predict(combModFit, predSTACK)

# What is the resulting accuracy on the test set? 
confusionMatrix(predRF, testing$diagnosis)$overall[1]
confusionMatrix(predGBM, testing$diagnosis)$overall[1]
confusionMatrix(predLDA, testing$diagnosis)$overall[1]
confusionMatrix(combPred, testing$diagnosis)$overall[1]
# Is it better or worse than each of the individual predictions? 
# Better

# Question 3
# Load the concrete data with the commands:
set.seed(3523)
library(AppliedPredictiveModeling)
data("concrete")
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain, ]
testing  = concrete[-inTrain, ]

# Set the seed to 233 and fit a lasso model to predict Compressive Strength
set.seed(233)
mdlLasso <- train(CompressiveStrength ~.,  data = training, method = "lasso")

mdlLasso$finalModel

# Which variable is the last coefficient to be set to zero as the penalty 
# increases? (Hint: it may be useful to look up ?plot.enet). 

plot.enet(mdlLasso$finalModel, xvar = "penalty", use.color = TRUE)

# The coefficient path shows that the variable Cement is the last coefficient to 
# be set to zero as the penalty increases.

# Question 4
# Load the data on the number of visitors to the instructors blog from here:
library(lubridate)  # For year() function below

filename_1 <- "gaData.csv"
fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
download.file(fileUrl, destfile = filename_1, method = "curl")
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012, ]
testing  = dat[year(dat$date) > 2011, ]
tstrain  = ts(training$visitsTumblr)

# Fit a model using the bats() function in the forecast package to the training 
# time series.
library(forecast)

# build a bats model based on the original time series
bats <- bats(tstrain, use.parallel = TRUE, num.cores = 4)

# construct a forecasting model using the bats function
visits.forecast <- forecast(bats)

# Then forecast this model for the remaining time points. 

# construct a forecasting model using the bats function
# predict next remaining time points
visits.forecast <- forecast(bats, nrow(testing))
plot(visits.forecast)

# extracting the 95% prediction boundaries
visits.forecast.lower95 = visits.forecast$lower[,2]
visits.forecast.upper95 = visits.forecast$upper[,2]

# see how many of the testing visit counts do actually match
table ( 
    (testing$visitsTumblr>visits.forecast.lower95) & 
    (testing$visitsTumblr<visits.forecast.upper95))

# For how many of the testing points is the true value within the 95% 
# prediction interval bounds?
226/nrow(testing)

## Alternative Q4 Answer
library(forecast)
mod_ts <- bats(tstrain)
fcast <- forecast(mod_ts, level = 95, h = dim(testing)[1])
sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
    dim(testing)[1]

# Question 5
# Load the concrete data with the commands:
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain, ]
testing = concrete[-inTrain, ]

# Set the seed to 325 and fit a support vector machine using the e1071 package 
# to predict Compressive Strength using the default settings. Predict on the 
# testing set. What is the RMSE? 
set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
inTrain <- createDataPartition(concrete$CompressiveStrength, p=3/4)[[1]]
training <- concrete[inTrain, ]
testing <- concrete[-inTrain, ]


#fit <- svm(CompressiveStrength ~., data=training)
# OR another way
fitSVM <- train(CompressiveStrength ~., data = training, method="svmRadial")
predSVM <- predict(fitSVM, testing)
acc <- accuracy(predSVM, testing$CompressiveStrength)
acc
acc[2] # RMSE 6.715009


plot(predSVM, testing$CompressiveStrength, 
     pch=20, cex=1, 
     col=testing$Age, 
     main= "Relationship between the svm forecast and actual values")
