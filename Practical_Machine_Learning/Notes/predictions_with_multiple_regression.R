#####################################################
## Prediction with Regression, Multiple Covariates ##
#####################################################

library(ggplot2)
library(caret)
library(ISLR)
data(Wage)
Wage <- subset(Wage, select = -logwage)
summary(Wage)
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")
pairs(Wage[,c("wage","age","education","jobclass")])

qplot(age,wage,data=training, color=jobclass)
qplot(age,wage,data=training, color=education)
qplot(age,wage,data=training, color=interaction(education, jobclass))

modFit <- train(wage ~ age + jobclass + education,
                method = "lm", data=training)
finMod <- modFit$finalModel
modFit
summary(finMod)

# Diagnostics
plot(finMod, 1, pch=19, cex=0.5, col="#00000020")

qplot(finMod$fitted,finMod$residuals,colour=race,data=training,alpha=0.5)
plot(finMod$residuals,pch=19)

pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing)

# Use all covariates
modFitAll<- train(wage ~ .,data=training,method="lm")
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)