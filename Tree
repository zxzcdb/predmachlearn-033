## Course Code for Tree
data(iris)
library(ggplot2)
names(iris)
table(iris$Species)

inTrain <- creatDataPartition(y=iris$Species,p=0.7,list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

dim(training)
dim(testing)
qplot(Petal.Width,Sepal.Width,colour=Species,data=training)
library(caret)
modelFit <- train(Species ~ ., method = "rpart",data=training)
print(modelFit$finalModel)
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

install.packages("rattle")
library(rattle)
fancyRpartPlot(modFit$finalModel)
