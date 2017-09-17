# Practical Machine Learning - WriteUp
Manish Reddy Jannepally  
September 17, 2017  

##Overview

The goal of this project is to predict the manner in which the 6 participants did the exercise using the data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. The different ways of doing exercise is classified into 5 classes. This is the "classe" variable in the training set.

##Getting and loading the data

The training data for this project are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv


The data for this project come from this source: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har


```r
rm(list = ls())
train_url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(train_url,destfile = "pm1-training.csv",mode = 'wb')
train_data = read.csv("pm1-training.csv",header = TRUE, na.strings = c("NA"," ","#DIV/0!"))
download.file(test_url,destfile = "pm1-testing.csv", mode = 'wb')
testing = read.csv("pm1-testing.csv",header = TRUE,na.strings = c("NA","#DIV/0!"," "))
dim(train_data);dim(testing)
```

```
## [1] 19622   160
```

```
## [1]  20 160
```

I am going to divide the train_data into training and validation sets using caret. I have observed a lot of data as NA, #DIV/0!, empty cells. so, considered them all as NAs while loading the data.


```r
set.seed(1839)
library(caret)
inTrain = createDataPartition(train_data$classe,p=0.65, list = FALSE)
training = train_data[inTrain,]
validation = train_data[-inTrain,]
dim(training);dim(validation)
```

```
## [1] 12757   160
```

```
## [1] 6865  160
```

#Preproccesing

As there are many NAs, #DIV/0!, empty cells we can remove them as they are of no use. If the model fails, we can add them back plan another model.


```r
training = training[,!apply(training,2,function(x)any(is.na(x)))]
validation = validation[,!apply(validation,2,function(x)any(is.na(x)))]
dim(training);dim(validation)
```

```
## [1] 12757    60
```

```
## [1] 6865   60
```

#Model building

I will try the classification with Random Forest method. Because many (me too) believe that this as the best classification model and random forest takes care of Cross Validation too. If the accuracy is not good,ll try another method.


```r
library(randomForest)
model = randomForest(classe~.,data = training,importance = TRUE)
model
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = training, importance = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3627    0    0    0    0           0
## B    0 2469    0    0    0           0
## C    0    0 2225    0    0           0
## D    0    0    0 2091    0           0
## E    0    0    0    0 2345           0
```

Seems the model is great and the cross validation (internal)error is also very very low (0.01). Lets try this model on the validation set.


```r
pred = predict(model,newdata = validation)
confusionMatrix(pred,validation$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1953    0    0    0    0
##          B    0 1328    0    0    0
##          C    0    0 1197    0    0
##          D    0    0    0 1125    0
##          E    0    0    0    0 1262
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9995, 1)
##     No Information Rate : 0.2845     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2845   0.1934   0.1744   0.1639   0.1838
## Detection Rate         0.2845   0.1934   0.1744   0.1639   0.1838
## Detection Prevalence   0.2845   0.1934   0.1744   0.1639   0.1838
## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
```

Accuracy is 100% on validation Set. The method I chose, classified the validation set 100% accurately.No need of model improvisation or any other model.
