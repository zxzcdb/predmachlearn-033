# Q1 
## Load the cell segmentation data from the AppliedPredictiveModeling package using the commands:

## library(AppliedPredictiveModeling)
## data(segmentationOriginal)
## library(caret)

## 1. Subset the data to a training set and testing set based on the Case variable in the data set.
## 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings.
## 3. In the final model what would be the final model prediction for cases with the following variable values:
## a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
## b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
## c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
## d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
inTrain <- segmentationOriginal$Case=="Train"
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]

set.seed(125)
modelFit <- train(Class ~ ., data=trainData, method="rpart")
modelFit$finalModel
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

