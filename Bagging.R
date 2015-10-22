install.packages("ElemStatLearn")
library(ElemStatLearn)
data(ozone,package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

## Resample every time
ll <- matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
  ss <- sample(1:dim(ozone)[1],replace=T)
  ozone0 <- ozone[ss,]
  ozone0 <- ozone0[order(opzone0$ozone),]
  loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2)
  ll[i,] <- predict(loess0,newdata=data,frame(ozone=1:155))
}

## Average the values
for(i in 1:10){
  lines(1:155,ll[i,],col="grey",lwd=2)
}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)

##Build own model
preditors = data.frame(ozone=ozone$ozone)
temperture = ozone$temperture
treebag <- bag(predictors, temperture, B=10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))
