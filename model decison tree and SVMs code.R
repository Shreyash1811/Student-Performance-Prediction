tmp <- subset( cat, select = -c(id_student.x, X, imd_band, key))


# read data

tmp <- subset( cat, select = -c(id_student.x, X, imd_band, key))


summary(tmp)
# divide training and test data set
set.seed(100) # setting seed to reproduce results of random sampling 
split<-(.8) #choose 80:20 
trainingRowIndex2 <- sample(1:nrow(tmp),(split)*nrow(tmp)) # row indices for training data 

trainingData2 <- tmp[trainingRowIndex2, ] # model training data 
testData2 <- tmp[-trainingRowIndex2, ] # test data # Calculate prediction accuracy and error rates 
#response = predict(df_model,testData2, type = "class") 
require(caret)

# extract features and labels from training and test data set
x = trainingData2[, 1:ncol(tmp)-1] # assume the first column to last two column are features
y = trainingData2[, ncol(tmp)]     # assume the last column are the labels
x_test = testData2[, 1:ncol(tmp)-1]

# SVM
#install.packages('e1071') 
library(e1071) 
#m_svm <- svm(x, y, type = 'C-classification', kernel = 'linear')
#result <- predict(m_svm, x_test)

#Radial kernel
m_svm_r<- svm(final_result~., trainingData2, kernel = "radial", cost = 5, probability=TRUE)
r1<- predict(m_svm_r, testData2, probability=TRUE)
prob1 = attr(r1, "probabilities")
g1 = plot(roc(testData2$final_result, prob1[, 1]))

roc_obj =roc(testData2$final_result, prob1[, 1])
auc(roc_obj)

tp= 3177
fp= 361
fn= 80
recall= tp/(tp+fn)
precision = tp/(tp+fp)
f= 2*(precision*recall)/(precision+recall)
g= sqrt(recall*precision)
recall
precision
f
g

table(testData2$final_result)
table(prediction.prob)
library(pROC)
plot(roc(testData2$final_result, prediction.prob[,2]))


#poly SVM
m_svm_p <- svm(final_result~., trainingData2, kernel = 'polynomial', degree=4, gamma=1)
result = predict(m_svm_p, testData2, type="class")
confusionMatrix(result, testData2$final_result)
prediction.prob = predict(m_svm_p,testData2, type = "prob")
a= plot(roc(testData2$final_result, prediction.prob[,2]))


m_svm_p <- svm(final_result~., trainingData2, kernel = 'polynomial', degree=4, gamma=1, probability=TRUE)
result = predict(m_svm_p, testData2, type="class")
confusionMatrix(result, testData2$final_result)
r1 = predict(m_svm_p,testData2, probability=TRUE)
prob2 = attr(r1, "probabilities")
a= plot(roc(testData2$final_result, prob2[,1]))


KRB = predict(m_svm_r, testData2, probability=TRUE)
tre  = predict(my_tree,testData2, type = "prob")
g1 = plot(roc(testData2$final_result, prob1[, 1]))

one=plot(roc(Test$NA_Pass,rf_pred1[,2]))

two= lines(roc(testData2$final_result, prob1[, 1]),col="purple")
three = lines(roc(testData2$final_result, prediction.prob[,2]),col="orange")


plot(g1,gtree,col="green")





table( prediction.prob)
table(testData2$final_result)

a
library(pROC)

# model <- svm(Label ~ ., data = iris, type = 'C-classification', kernel = 'linear')

tp= 2903
fp= 335
fn= 354
recall= tp/(tp+fn)
precision = tp/(tp+fp)
f= 2*(precision*recall)/(precision+recall)
g= sqrt(recall*precision)
recall
precision
f
g

roc_obj =roc(testData2$final_result, prob1[, 1])
auc(roc_obj)


# decision tree
# Load packages
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)

# first argument: label~feature columns, need to modify according to real data set
my_tree<-rpart(final_result~., 
               data = trainingData2, method = "class")
my_tree
printcp(my_tree)
rpart.plot(my_tree)

result = predict(my_tree, testData2, type="class")
confusionMatrix(result, testData2$final_result)
library(pROC)
prediction.prob = predict(my_tree,testData2, type = "prob")
gtree =plot(roc(testData2$final_result, prediction.prob[,2]))


tp= 3157
fp= 472
fn= 100
recall= tp/(tp+fn)
precision = tp/(tp+fp)
f= 2*(precision*recall)/(precision+recall)
g= sqrt(recall*precision)
recall
precision
f
g
roc_obj =roc(testData2$final_result, prob1[, 2])
auc(roc_obj)


