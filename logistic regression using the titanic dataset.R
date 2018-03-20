test <-read.csv("~/Desktop/test.csv",header=TRUE,na.strings = c(""))
train <- read.csv("~/Desktop/train.csv",header=TRUE,na.strings = c(""))
str(train)

#transform both data set variables
train$Survived <- as.factor(train$Survived)
train$SibSp <- as.numeric(train$SibSp)
test$SibSp <- as.numeric(test$SibSp)
train$Parch <- as.numeric(train$Parch)
test$Parch <- as.numeric(test$Parch)
test$Pclass<- as.factor(test$Pclass)
train$Pclass<-as.factor(train$Pclass)
train$Name<-as.character(train$Name)
test$Name<-as.character(test$Name)

# replace NA be the meam of the Age 
train$Age[is.na(train$Age)]<-mean(train$Age,na.rm=T)
test$Age[is.na(test$Age)]<-mean(test$Age,na.rm=T)

#plot histogram to see patern in the data
library(ggplot2)
ggplot(train, aes(x = Pclass, fill = Survived))+
geom_histogram(width = 0.5,stat = "count")+
xlab("Pclass")+
ylab("Total count")+
labs(fill="Survived")
#check variable to see what variables have missing values
library(Amelia)
missmap(train, main = "observed vs Missing values")

#create new variable "title" from variable "Name"
Titlenames <- function(Name) {
  
  if (length(grep("Miss.", Name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}
title<- NULL
for (i in 1:nrow(train)) {
  title <-c(title,Titlenames(train[i,"Name"]))
}
train$title<-as.factor(title)

#Create new variable "Cabingroup" by grouping Cabin by letter
train$Cabin <- as.character(train$Cabin)
cabintype <- function(Cabin) {
  
  if (length(grep("A", Cabin)) > 0) {
    return ("A")
  } else if (length(grep("B", Cabin)) > 0) {
    return ("B")
  } else if (length(grep("C", Cabin)) > 0) {
    return ("C")
  } else if (length(grep("D", Cabin)) > 0) {
    return ("D")
  } else if (length(grep("E", Cabin)) > 0) {
    return ("E")
  } else if (length(grep("F", Cabin)) > 0) {
    return ("F")
  } else if (length(grep("G", Cabin)) > 0) {
    return ("G")
  } else {
    return ("Other")
  }
}
Cabingroup<- NULL
for (i in 1:nrow(train)) {
  Cabingroup <-c(Cabingroup,cabintype(train[i,"Cabin"]))
}
train$Cabingroup <-as.factor(Cabingroup)   

#Create new dataframe with relevant variables(replacing new variables by new created )
train1<- subset(train, select=c(2,3,5,6,7,8,10,13,14))

#Splitting train1 before creating model
training<- data.frame(train1[1:802,])
testing <- data.frame(train1[803:891,])

#fitting model
model<-glm(Survived~.,family = binomial(link = "logit"),data = training)
summary(model)
anova(model,test = "Chisq")

# Mc Fadden R square
library(pscl)
pR2(model)

#Wald Test for Cabingroup
library(survey)
regTermTest(model, "Cabingroup")

#Wald Test for title
regTermTest(model, "title")

#Wald Test for SibSp
regTermTest(model, "SibSp")

#Wald Test for Parch
regTermTest(model, "Parch")

#Variable Importance
varImp(model)

#Checking if Residual deviance and AIC decrease with "Cabingroup" remove from the model
model<-glm(Survived~Cabingroup,family = binomial(link = "logit"),data = training)
summary(model)

#Putting "Cabingroup" to the model
model<-glm(Survived~.,family = binomial(link = "logit"),data = training)

#validation of Predicted values
prediction <- predict(model,newdata=testing,type="response")
prediction <- ifelse(prediction > 0.6,1,0)
Error <- mean(prediction != testing$Survived)
print(paste("Accuracy",1-Error))


#K Fold cross validation
library(lattice)
library(ggplot2)
library(caret)
control<-trainControl(method = "cv",number = 10,savePredictions = TRUE)
model<-train(Survived~., family = binomial(link = "logit"),data = train1, method ="glm",trControl=control)
prediction <- predict(model, newdata=testing)
confusionMatrix (data=prediction, testing$Survived)

#model
model<-glm(Survived~.,family = binomial(link = "logit"),data = training)
#plot ROC curve
library(ROCR)
prob <- predict(model,newdata=testing,type="response")
pr <- prediction(prob, testing$Survived)
perf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf,print.cutoffs.at= seq(0.1, by=0.1))

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Transform test dataset to fit new model
#create new variable "title" from variable "Name"
title<- NULL
for (i in 1:nrow(test)) {
  title<-c(title,Titlenames(test[i,"Name"]))
}
test$title<-as.factor(title)

#Create new variable "Cabingroup" by grouping Cabin by letter
Cabingroup<- NULL
for (i in 1:nrow(test)) {
  Cabingroup<-c(Cabingroup,cabintype(test[i,"Cabin"]))
}
test$Cabingroup<-as.factor(Cabingroup)

#model
model<-glm(Survived~.,family = binomial(link = "logit"),data = train1)

#Fit model into test dataset
finalprediction <- predict(model,newdata=subset(test,select=c(2,4,5,6,7,9,12,13)),type="response")
finalprediction <- ifelse(finalprediction > 0.6,1,0)

#Convert final prediction as data frame
Survived<-as.data.frame(as.table(finalprediction))


                           
