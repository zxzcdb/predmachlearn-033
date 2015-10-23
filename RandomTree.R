data(iris)
library(ggplot2)
inTrain <- createDataPartition(y=iris$Species,p=0.7,list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

library(caret)
modelFit <- train(Species~.,data=training,method="rf",prox=TRUE)
modelFit

getTree(modelFit$finalModel, k=2)
