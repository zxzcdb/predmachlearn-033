install.packages("ISLR")
library(ggplot2)
library(caret)
library(ISLR)
data(Wage)
Wage <- subset(Wage,select=-c(logwage))
summary(Wage)

inTrain <- createDataPartition(y = Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)
featurePlot(x=training[,c("age","education","jobclass")],y = training$wage, plot = "pairs")
qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)
## qp <- qplot(age,wage,colour=jobclass,data=training)
## qp + geom_smooth(method = "lm", formular = wage ~ age)
modFit <- train(wage ~ age + jobclass + education, method = "lm", data =training)
findMod <- modFit$finalModel
print(modFit)
plot(findMod, pch=19, cex=0.5, col="#00000010")
qplot(wage, pred,colour=year,data=testing)

modFitAll <- train(wage~., data=training, method="lm")
pred <- predict(modFitAll, data=training)
qplot(wage,pred,data=testing)
