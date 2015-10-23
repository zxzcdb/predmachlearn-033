# Preprocess
data(iris)
library(ggplot2)
inTrain <- createDataPartition(y=iris$Species,p=0.7,list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

library(caret)
modelFit <- train(Species~.,data=training,method="rf",prox=TRUE)
modelFit
# Build Random Tree
getTree(modelFit$finalModel, k=2)

irisP <- classCenter(training[,c(3,4)],training$Species,modelFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species, data=training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)
# Predict testing data
pred <- predict(modelFit,testing)
testing$predRight <- pred==testing$Species
table(pred,testing$Species)
# Plot the test result
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Preditcions")
