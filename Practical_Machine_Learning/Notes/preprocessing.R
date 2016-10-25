library(caret)
library(kernlab)
data(spam)
intrain <- createDataPartition(y=spam$type,p=0.75,list=FALSE)
training <- spam[intrain,]
testing <- spam[-intrain,]
hist(training$capitalAve,xlab="ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)
# sd is much higher than the mean because the data is extremely skewed.
# standardize to z-scores for normal distribution
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)
# mean = 0, sd = 1
hist(trainCapAveS)

# When standardizing the test set, you have to use mean and sd from training set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)
# mean != 0, and sd != 1

# Can use the preProcess function to do various pre-processing. 
# "center" and "scale" will do the same transformation as above.
preObj <- preProcess(training[,-58], method=c("center", "scale"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)
# again, mean = 0, sd = 1

# You can use the pre-processing object from the training set to run the same
# pre-processing on the test set.
testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

# You can also pass the preprocessed commands directly to the train function 
# in caret, as an argument.
set.seed(32343)
modelfit <- train(type~., data=training, preProcess=c("center","scale"), method = "glm")
modelfit

# Can also do other kinds of transformations (BoxCox)
preObj <- preProcess(training[,-58], method = "BoxCox")
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)

# Can also impute data
set.seed(13343)

# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])
