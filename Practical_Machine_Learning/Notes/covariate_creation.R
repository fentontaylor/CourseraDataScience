########################
#  Covariate Creation  #
########################

# covariates = features = predictors

# two levels of covariate creation
      # 1. From raw data to covariates
      # 2. Transforming tidy covariates

library(kernlab)
data(spam)
spam$capitalAveSq <- spam$capitalAve^2

library(ISLR)
library(caret)
data(Wage)
intrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[intrain,]; testing <- Wage[-intrain,]

table(training$jobclass)
dummies <- dummyVars(wage~jobclass, data=training)
head(predict(dummies, newdata=training))

nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv
# variable with TRUE can be thrown out. with saveMetrics = TRUE, the function 
# returns the indices of TRUE near zero variables.

library(splines)
bsBasis <- bs(training$age, df=3)
head(bsBasis)
# col 1 = age, col 2 = age^2, col 3 = age^3
# this allows to fit a curvy line
lm1 <- lm(wage ~ bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col = "red", pch=19)

# CREATE COVARIATES ON TEST DATASET USING THE EXACT SAME PROCEDURE USED ON THE 
# TRAINING SET
predict(bsBasis, age = testing$age)
