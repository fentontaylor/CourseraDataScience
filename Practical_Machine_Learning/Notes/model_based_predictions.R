#############################
## Model Based Prediction  ##
#############################

data(iris)
library(ggplot2)
library(caret)

names(iris)
table(iris$Species)

inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

modlda <- train(Species ~ ., data=training, method="lda")
modnb <- train(Species ~ ., data=training, method="nb")
plda <- predict(modlda, testing)
pnb <- predict(modnb,testing)
table(plda,pnb)

equalPredictions <- (plda==pnb)
qplot(Petal.Width,Sepal.Width,color=equalPredictions,data=testing)
