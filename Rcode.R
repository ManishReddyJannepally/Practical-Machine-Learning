rm(list = ls())
getwd()
setwd("C:/Users/janne/Desktop/R - Coursera/Practical ML - Project")
train_url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(train_url,destfile = "pm1-training.csv",mode = 'wb')
train_data = read.csv("pm1-training.csv",header = TRUE, na.strings = c("NA"," ","#DIV/0!"))
download.file(test_url,destfile = "pm1-testing.csv", mode = 'wb')
testing = read.csv("pm1-testing.csv",header = TRUE,na.strings = c("NA","#DIV/0!"," "))
dim(train_data);dim(testing)
library(caret)
inTrain = createDataPartition(train_data$classe,p=0.65, list = FALSE)
training = train_data[inTrain,]
validation = train_data[-inTrain,]
dim(training);dim(validation)
#when I observed the data in excel, I can see many NA,#DIV/0!, empty cells which are of no use for the model
training = training[,!apply(training,2,function(x)any(is.na(x)))]
validation = validation[,!apply(validation,2,function(x)any(is.na(x)))]
library(randomForest)
model = randomForest(classe~.,data = training)
pred = predict(model,newdata = validation)
confusionMatrix(pred,validation$classe)
model2 = train(classe~.,data = training, method = "rf", trControl = trainControl(method = "cv", number = 3))
pred2 = predict(model2,newdata = validation)
confusionMatrix(pred2,validation$classe)
var_imp = model$importance
plot(var_imp)
varImpPlot(model, sort = FALSE)
