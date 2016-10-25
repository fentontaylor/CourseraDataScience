#####################################################
#  Preprocessing with Principal Compnents Analysis  #
#####################################################

# Often you have multiple quantitative variables and sometimes they'll be highly
# correlated with each other. In other words, they'll be very similar to being 
# the almost the exact same variable. In this case, it's not necessarily useful 
# to include every variable in the model. You might want to include some summary
# that captures most of the information in those quantitative variables. 

library(caret)
library(kernlab)
data(spam)
intrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[intrain,]
testing <- spam[-intrain,]

M <- abs(cor(training[-58]))
diag(M) <- 0
which(M > 0.8, arr.ind = T)

names(spam)[c(32,34)]
plot(spam[,32],spam[,34])

X <- 0.71*(training$num415 + training$num857)
Y <- 0.71*(training$num415 - training$num857)
plot(X,Y)

smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])

typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

preProc <- preProcess(log10(spam[,-58]+1), method="pca", pcaComp = 2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(x=trainPC, y = training$type,method="glm")
modelFit

testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit,testPC))

# alternative method
modelFit <- train(training$type ~ .,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))