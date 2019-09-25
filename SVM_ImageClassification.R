### Assignment to classify the handwritten digits ###

# Installing all the required Packages

install.packages("caret")

install.packages("kernlab")

install.packages("dplyr")

install.packages("readr")

install.packages("ggplot2")

install.packages("gridExtra")

library("kernlab")
library("caret")
library("dplyr")
library("readr")
library("ggplot2")
library("gridExtra")

#Reading train and Test data (As data is huge and taking lot of computation time,I have chosen 5000 records for each dataset)

trainsvm<-read.csv("mnist_train.csv",header = FALSE,nrows=5000)
testsvm<-read.csv("mnist_test.csv",header = FALSE,nrows=5000)

##Data Preparation and Data Understanding

#Exploring the data

str(trainsvm)

summary(trainsvm)

#checking missing value

sapply(trainsvm, function(x) sum(is.na(x)))

#Converting the number column to factor variable

trainsvm$V1<-factor(trainsvm$V1)
testsvm$V1<-factor(testsvm$V1)

##Data is clean and is suitable for Analysis in R.Proceeding with Model building##

# Train Dataset
trainsvmt = trainsvm

# Test Dataset
testsvmv = testsvm


#Applying Linear Model on the Dataset (taking V1 as the column name as it is the default name of the )

Model_linear <- ksvm(V1~ ., data = trainsvmt, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, testsvmv)

confusionMatrix(Eval_linear,testsvmv$V1)

### Accuracy for Linear Model is 87.7% ###


#Applying Polynomial Model on the Dataset (taking V1 as the column name as it is the default name of the )

Model_poly <- ksvm(V1~ ., data = trainsvmt, scale = FALSE, kernel = "polydot")
Eval_poly<- predict(Model_poly, testsvmv)

confusionMatrix(Eval_poly,testsvmv$V1)


### Accuracy for Polynomial Model is 87.7% which doesn't change much as compared to Linear Model ###



#Applying Radial basis Kernel Model on the Dataset (taking V1 as the column name as it is the default name of the )


Model_RBF <- ksvm(V1~ ., data = trainsvmt, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, testsvmv[-1])


confusionMatrix(Eval_RBF,testsvmv$V1)

### Accuracy for RBF Kernel Linear Model is 92.08% ###

# Looking at sigma and c values for Model_RBF
print(Model_RBF)

## values for Linear Model : c= 1 and sigma = 1.65e-07


## Trying different values for sigma and c for cross validation ##

trainControl <- trainControl(method="cv", number=5,verboseIter=TRUE)

metric <- "Accuracy"

set.seed(7)

# sigma and c values are selected comparable to the default values in Model_RBF

grid <- expand.grid(.sigma=c(0.0000001,0.0000002,0.0000003,0.0000004), .C=c(1,2,3) )

fit.svm <- train(V1~., data=trainsvmt, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)

plot(fit.svm)


#The values for which Accuracy is maximum used for the model were sigma = 4e-07 and C = 3.


## As the RBF kernel Model has more accuracy than Linear and polynomial models,
## it may overfit the train dataset,it is not suggested to use, as it is 
## highly biased towards only train dataset.
## If the requirement of client is high accuracy,we can provide the RBF kernel model, 
## with sigma = 4e-07 and c= 3 which has accuracy of 96.3%. 
## Otherwise we can recommend the Linear Model which has accuracy of 87.7% since this has 
## high variance and less bias towards train dataset.



