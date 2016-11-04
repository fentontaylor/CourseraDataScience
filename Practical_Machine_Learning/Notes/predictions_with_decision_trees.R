######################################
##  Predicting with Decision Trees  ##
######################################

data(iris)
library(ggplot2)
library(caret)
names(iris)
table(iris$Species)

# Separate data into training and test set
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# plot petal width vs. sepal width
qplot(Petal.Width, Sepal.Width, color=Species,data = training)
qplot(Species, Petal.Length, color=Species, geom = "boxplot", data=training)

# fit a model using "rpart" which is R's function for doing classification trees
modfit <- train(Species~.,method="rpart", data=training)
print(modfit$finalModel)

plot(modfit$finalModel, uniform = TRUE, main="Classification Tree")
text(modfit$finalModel, use.n = TRUE, all=TRUE, cex=.8)

library(rattle)
fancyRpartPlot(modfit$finalModel)

# predict on testing set using the model
test_predictions <- predict(modfit, newdata = testing)
confusionMatrix(test_predictions,testing$Species)

# can also use "party" and "rpart" packages with caret and "tree" package outside it.