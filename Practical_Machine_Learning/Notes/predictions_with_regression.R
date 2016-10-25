################################
#  Predictions With Regression #
################################

library(caret)
data(faithful)
set.seed(333)
intrain <- createDataPartition(y=faithful$waiting, p=0.5, list=FALSE)
trainFaith <- faithful[intrain,] 
testFaith <- faithful[-intrain,]
head(trainFaith)

plot(trainFaith$waiting, trainFaith$eruptions, pch=19, 
     col="blue", xlab="Waiting", ylab="Duration")

lm1 <- lm(eruptions~waiting, data=trainFaith)
summary(lm1)
abline(a = lm1$coefficients[[1]], b = lm1$coef[[2]], col="red", lwd=2)

# predict duration for a waiting time of 80
coef(lm1)[1]+coef(lm1)[2]*80
newdata <- data.frame(waiting=80)
predict(lm1, newdata)

# Plot predicitons - training and test
par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",
     xlab="Waiting",ylab="Duration",main="Training")
lines(trainFaith$waiting,predict(lm1),lwd=4)
abline(a=coef(lm1)[1],b=coef(lm1)[2],lwd=2,col=rgb(1,0,0,0.5))
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",
     xlab="Waiting",ylab="Duration",main="Testing")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)
abline(a=coef(lm1)[1],b=coef(lm1)[2],lwd=2,col=rgb(1,0,0,0.5))

lm2 <- lm(eruptions~waiting, data=testFaith)
summary(lm2)

# Get training and test set errors
# RMSE on training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
# RMSE on test
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))

# Prediction Intervals
pred1 <- predict(lm1, newdata=testFaith,interval = "prediction")
ord <- order(testFaith$waiting)
par(mfrow=c(1,1))
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",col=c(1,2,2),lty = c(1,1,1), lwd=3)

# Same process with caret
modFit <- train(eruptions~waiting, data=trainFaith, method="lm")
summary(modFit$finalModel)
