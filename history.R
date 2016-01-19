#013plottingPredictors
install.packages("ISLR");
install.packages("ggplot2");
install.packages("caret");


library(ISLR); library(ggplot2); library(caret);
data(Wage)
summary(Wage)

inTrain<-createDataPartition(y=Wage$wage, p=0.7, list=FALSE); ## Isto esta mal, o qeu sai é o teste
training<-Wage[inTrain,];
testing<-Wage[-inTrain];

featurePlot(x=training[,c("age","education","jobclass")],y=training$wage, plot = "pairs")
qplot(age,wage, data=training);


qqq<-qplot(age,wage, colour=education,data=training);
qqq + geom_smooth(method="lm", formula = y~x)
install.packages("Hmisc");
library(Hmisc);
cutWage <- cut2(training$wage,g=3)
p1 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot"));
p1
p2 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot","jitter"))
library(gridExtra)
grid.arrange(p1,p2,ncol=2)
t1 <- table(cutWage,training$jobclass)
t1
prop.table(t1,1)
qplot(wage,colour=education,data=training,geom = "density")

#Pre Processing
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve,main="",xlab="ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve  - mean(trainCapAve))/sd(trainCapAve) 
mean(trainCapAveS)
sd(trainCapAveS)
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve  - mean(trainCapAve))/sd(trainCapAve) 
mean(testCapAveS)
sd(testCapAveS)
preObj <- preProcess(training[,-58],method=c("center","scale"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)
testCapAveS <- predict(preObj,testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)
set.seed(32343)
modelFit <- train(type ~.,data=training,
                  preProcess=c("center","scale"),method="glm")
modelFit
preObj <- preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)
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
#Covariate Production
library(kernlab);data(spam)
spam$capitalAveSq <- spam$capitalAve^2
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass,data=training)
head(predict(dummies,newdata=training))
nsv <- nearZeroVar(training,saveMetrics=TRUE)
nsv
library(splines)
bsBasis <- bs(training$age,df=3) 
bsBasis
lm1 <- lm(wage ~ bsBasis,data=training)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)
predict(bsBasis,age=testing$age)
#Preprocessing with Principal Components Analysis (PCA)
