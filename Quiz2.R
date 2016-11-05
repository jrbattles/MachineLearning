#   Quiz 2

## Question 1
##  Load the Alzheimer's disease data using the commands:
##      library(AppliedPredictiveModeling)
##      data(AlzheimerDisease)
##  Which of the following commands will create non-overlapping training and 
##  test sets with about 50% of the observations assigned to each? 

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)


adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

## Question 2
## Load the cement data using the commands:
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(dplyr)
library(gridExtra)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

## Make a plot of the outcome (CompressiveStrength) versus the index of the 
## samples. Color by each of the variables in the data set (you may find the 
## cut2() function in the Hmisc package useful for turning continuous covariates 
## into factors). What do you notice in these plots?

training <- mutate(training, index=1:nrow(training))
cutIndex <- cut2(training$index, g=10)
breaks <- 10
featurePlot(x=training[, xnames], y=training$CompressiveStrength, plot="pairs")

qplot(index, CompressiveStrength, data=training, color=cut2(training$Cement, g=breaks))

qplot(index, CompressiveStrength, data=training, color=cut2(training$BlastFurnaceSlag, g=breaks))

qplot(index, CompressiveStrength, data=training, color=cut2(training$FlyAsh, g=breaks))

qplot(index, CompressiveStrength, data=training, color=cut2(training$Water, g=breaks))

qplot(index, CompressiveStrength, data=training, color=cut2(training$Superplasticizer, g=breaks))

qplot(index, CompressiveStrength, data=training, color=cut2(training$CoarseAggregate, g=breaks))

qplot(index, CompressiveStrength, data=training, color=cut2(training$FineAggregate, g=breaks))

qplot(index, CompressiveStrength, data=training, color=cut2(training$Age, g=breaks))

## --> There is a non-random pattern in the plot of the outcome versus index 
## that does not appear to be perfectly explained by any predictor suggesting a 
## variable may be missing.

# Question 3
## Load the cement data using the commands:
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

## Make a histogram and confirm the SuperPlasticizer variable is skewed. 
## Normally you might use the log transform to try to make the data more symmetric. 
## Why would that be a poor choice for this variable?
hist(training$Superplasticizer, breaks=30)

## --> There are a large number of values that are the same and even if you took 
## the log(SuperPlasticizer + 1) they would still all be identical so the 
## distribution would not be symmetric.

# Question 4
## Load the cement data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Find all the predictor variables in the training set that begin with IL. 
# Perform principal components on these variables with the preProcess() function 
# from the caret package. Calculate the number of principal components needed to 
# capture 90% of the variance. How many are there?
predName <- names(training)
(ILpredictor <- predName[substr(predName, 1, 2) == "IL"])

ProcPCA <- preProcess(training[, ILpredictor], method = "pca", thresh = .9)
ProcPCA$numComp
ProcPCA

# Question 5
## Load the cement data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

## Create a training data set consisting of only the predictors with variable 
## names beginning with IL and the diagnosis. Build two predictive models, one 
## using the predictors as they are and one using PCA with principal components 
## explaining 80% of the variance in the predictors. Use method=“glm” in the 
## train function. What is the accuracy of each method in the test set? 
## Which is more accurate?

# This first model uses all the predictors (Non-PCA)

library(lattice)
library (ggplot2)
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p=3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(3433)
IL <- grep("^IL", colnames(training), value=TRUE)
ILpredictors <- predictors[, IL]
df <- data.frame(diagnosis, ILpredictors)
inTrain <- createDataPartition(df$diagnosis, p=3/4)[[1]]
training <- df[inTrain, ]
testing <- df[-inTrain, ]
modelFit <- train(diagnosis ~ ., method="glm", data=training)
predictions <- predict(modelFit, newdata=testing)
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1)
acc1 <- C1$overall[1]
acc1 # Non-PCA Accuracy: 0.65 


## PAY ATTENTION HERE!!!!  The "train" function was changed.  
## "You shouldn't use the data set name on the LHS of the formula. 

modelFit <- train(diagnosis ~ ., 
                  method="glm", 
                  preProcess="pca", 
                  data=training, 
                  trControl=trainControl(preProcOptions=list(thresh=0.8)))
C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
print(C2)
acc2 <- C2$overall[1]
acc2 # PCA Accuracy: 0.72
