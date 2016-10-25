library(caret)
library(kernlab)
data(spam)
intrain <- createDataPartition(y=spam$type,p=0.75,list=FALSE)
training <- spam[intrain,]
testing <- spam[-intrain,]
modelfit <- train(type~.,data=training,method="glm")
args(train.default)
args(trainControl)
library(ISLR)
library(ggplot2)
data("Wage")
summary(Wage)
intrain <- createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
training <- Wage[intrain,]
testing <- Wage[-intrain,]
dim(training); dim(testing)
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")
qplot(age,wage,data=training,color=jobclass)
qq <- qplot(age,wage,color=education,data=training)
qq + geom_smooth(method = "lm", se=F)
library(Hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)
p1 <- qplot(cutWage,age,fill=cutWage,data=training,geom="boxplot")
p2 <- qplot(cutWage,age,fill=cutWage,data=training,geom=c("boxplot","jitter"))
library(gridExtra)
grid.arrange(p1,p2,ncol=2)
t1 <- table(cutWage,training$jobclass)
t1
prop.table(t1,1)
qplot(wage,color=education,data=training,geom="density")
