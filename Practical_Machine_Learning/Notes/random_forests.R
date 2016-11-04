######################
##  Random Forests  ##
######################

data(iris)
library(ggplot2)
library(caret)

# partition the data
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

# build the model
modFit <- train(Species~., data=training,method="rf", prox=TRUE)
modFit

# get a single tree from the model
getTree(modFit$finalModel, k=2)

# plotting
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species,data=training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)

# predicting new values
pred <- predict(modFit,testing)
testing$predRight <- pred==testing$Species
table(pred,testing$Species)
View(pred)

qplot(Petal.Width, Petal.Length, color=predRight, data=testing, main="newdata Predictions")
