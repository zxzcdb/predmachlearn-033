## 1.) Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
library(caret)
library(gbm)
library(mgcv)
library(nlme)
library(elasticnet)

## Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. Fit (1) a ## random forest predictor relating the factor variable y to the remaining variables and (2) a boosted predictor using ## the “gbm” method. Fit these both with the train() command in the caret package.

## What are the accuracies for the two approaches on the test data set? What is the accuracy among the test set
## samples where the two methods agree?

prf <- predict(rf, vowel.test)
pgbm <- predict(gbm, vowel.test)

cmrf <- confusionMatrix(prf, vowel.test$y)
cmgbm <- confusionMatrix(pgbm, vowel.test$y)

cmrf$overall['Accuracy']

## Accuracy 
##   0.6061

cmgbm$overall['Accuracy']

## Accuracy 
##   0.5303

### combined accuracy model
datacombined <- data.frame(prf, pgbm, y=vowel.test$y)
fit <- train(y~., datacombined)
pfit <- predict(fit, vowel.test)
cmpfit <- confusionMatrix(pfit, vowel.test$y)
cmpfit$overall['Accuracy']

## Accuracy 
##    0.697

### manual calculation for accuracy
adata <- data.frame("prf"=numeric(), "pgbm"=numeric(), "y"=numeric())
for (i in 1:nrow(datacombined)) {
    if (identical(datacombined[i,1],datacombined[i,2])){
    adata[(nrow(adata)+1),] <- datacombined[i,]
    }
}

count <- 0
for (i in 1:nrow(adata)) {
    if (identical(adata[i,2], adata[i,3])) {
        count <- count + 1
    }
}

count

## [1] 203

count / nrow(adata)

## [1] 0.657

## Accuracy for combined method is 0.657.

## RF Accuracy = .60606 GBM Accuracy = .530303 Combined Accuracy = .6622 (manual calc) or .699 (combined accuracy model) depending which way you use to calculate

## 2.) Load the Alzheimer’s data using the following commands

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

## Set the seed to 62433 and predict diagnosis with all the other variables using a random forest (“rf”), boosted
##  trees (“gbm”) and linear discriminant analysis (“lda”) model. Stack the predictions together using random forests
## (“rf”). What is the resulting accuracy on the test set? Is it better or worse than each of the individual
## predictions?

pRF <- predict(fitRF, testing)
pGBM <- predict(fitGBM, testing)
pLDA <- predict(fitLDA, testing)

combinedset <- data.frame(pRF, pGBM, pLDA, diagnosis = testing$diagnosis)
fit <- train(diagnosis~., combinedset, method="rf")

## note: only 2 unique complexity parameters in default grid. Truncating the grid to 2 .

predict <- predict(fit, testing)


cmRF <- confusionMatrix(pRF, testing$diagnosis)
cmGBF <- confusionMatrix(pGBM, testing$diagnosis)
cmLDA <- confusionMatrix(pLDA, testing$diagnosis)
cm <- confusionMatrix(predict, testing$diagnosis)

cmRF$overall['Accuracy']

## Accuracy 
##   0.7683

cmGBF$overall['Accuracy']

## Accuracy 
##   0.7927

cmLDA$overall['Accuracy']

## Accuracy 
##   0.7683

cm$overall['Accuracy']

## Accuracy 
##   0.7927

RF Accuracy = .7682927 GBM Accuracy = .7926829 LDA Accuracy = .7682927 combined Accuracy = .8049

## Stacked Accuracy: 0.79 is better than random forests and lda and the same as boosting.

## 3.) Load the concrete data with the commands:

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

## Set the seed to 233 and fit a lasso model to predict Compressive Strength. Which variable is the last coefficient
## to be set to zero as the penalty increases? (Hint: it may be useful to look up ?plot.enet).

set.seed(233)

fit <- train(CompressiveStrength ~ ., training, method="lasso")
plot.enet(fit$finalModel, xvar="penalty", use.color=TRUE)

## plot of chunk unnamed-chunk-9

## last coefficient to be set to zero is Cement.

## 4.) Load the data on the number of visitors to the instructors blog from here:
## https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv

## Using the commands:

library(forecast)
library(quantmod)
library(lubridate)  # For year() function below
dat = read.csv("~/datasciencecoursera/Courses/PracticalMachineLearning/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

## Fit a model using the bats() function in the forecast package to the training time series. Then forecast this model
## for the remaining time points. For how many of the testing points is the true value within the 95% prediction 
## interval bounds?

mod <- bats(tstrain)
fcast <- forecast.bats(mod, level=95, h=nrow(testing))
acc <- accuracy(fcast, testing$visitsTumblr)

count <- 0
for (i in 1:nrow(testing)) {
        if (testing$visitsTumblr[i] > fcast$lower[i]) {
                if(testing$visitsTumblr[i] < fcast$upper[i]) {
                count <- count + 1}
        }
}
count/nrow(testing)

## [1] 0.9617

## accuracy is .9617021

## 5.) Load the concrete data with the commands:

set.seed(3523)
library(AppliedPredictiveModeling)
library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

## Set the seed to 325 and fit a support vector machine using the e1071 package to predict Compressive Strength using ## the default settings. Predict on the testing set. What is the RMSE?

results <- c(accsvm[2], accsvmRadial[2], accsvmLinear[2], accsvmPoly[2], accsvmRadial[2], accsvmRadialCost[2])

## accsvm   |accsvmRadial | accsvmLinear|  accsvmPoly | accsvmRadial|accsvmRadialCost
## ---------|-------------|-------------|-------------|-------------|-------------
## 6.547958 |   6.177553  |   11.220626 |   5.993826  |  6.177553   |  6.106515

## correct answer is 6.72
