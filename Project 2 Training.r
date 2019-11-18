
main_df = read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\final_data2.csv")

names(main_df)

new= main_df[,-which(names(main_df) %in% c("id_student.x","X","key"))]

new=new[!new$final_result=="Withdrawn",]
new[new$final_result=="Distinction","final_result"] = "Pass"

write.csv(new,"C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\cat2.csv", row.names = FALSE)

new = read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\cat2.csv")


## 70% of the sample size
smp_size <- floor(0.90 * nrow(new))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(new)), size = smp_size)

Train <- new[train_ind, ]
Test <- new[-train_ind, ]

row.names(Train) = NULL
row.names(Test)= NULL

library(caret)
library(ROSE)
library(pROC)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# CART
set.seed(7)
fit.cart.unbalanced <- train(final_result~., data=Train, method="rpart",metric=metric, trControl=control)

# estimate skill of CART on the validation dataset
predictions_cart1 <- predict(fit.cart.unbalanced, Test)
confusionMatrix(predictions_cart1, Test$final_result)

cart1 =  predict(fit.cart.unbalanced, Test,type="prob")
two=plot(roc(Test$final_result,cart1[,2]))

tom1= varImp(fit.cart.unbalanced)
tom1

prop.table(table(Train$final_result))
table(Train$final_result)

# Under Sampling
set.seed(222)
under_sam= ovun.sample(final_result~.,data =Train, method = "under", N=6076*2)$data
table(under_sam$final_result)

# CART
set.seed(7)
fit.cart.balanced <- train(final_result~., data=under_sam, method="rpart",metric=metric, trControl=control)

# estimate skill of CART on the validation dataset
predictions_cart2 <- predict(fit.cart.balanced, Test)
confusionMatrix(predictions_cart2, Test$final_result)

cart2 =  predict(fit.cart.balanced, Test,type="prob")
two=plot(roc(Test$final_result,cart2[,2]))

tom2= varImp(fit.cart.balanced)
tom2

names(new)

new_selected= new[,which(names(new) %in% c("weight","TMA_score","TMA_count","NA_homepage","sum_click","CMA_count","NA_quiz",
                             "code_module.x.x","code_presentation.x.x","studied_credits","final_result","age_band"))]

head(new_selected)

## 90% of the sample size
smp_size <- floor(0.90 * nrow(new_selected))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(new_selected)), size = smp_size)

Train <- new_selected[train_ind, ]
Test <- new_selected[-train_ind, ]

row.names(Train) = NULL
row.names(Test)= NULL

# Random Forest with unbalanced data
set.seed(7)
fit.cart3 <- train(final_result~., data=Train, method="rf", metric=metric, trControl=control)

# estimate skill of CART on the validation dataset
predictions_cart3 <- predict(fit.cart3, Test)
confusionMatrix(predictions_cart3, Test$final_result)

cart3 =  predict(fit.cart3, Test,type="prob")
two=  plot(roc(Test$final_result,cart3[,2]))

saveRDS(fit.cart3, "C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\models\\project2\\rf_unbalanced_model.rds")

# Under Sampling
set.seed(222)
under_sam= ovun.sample(final_result~.,data =Train, method = "under", N=6076*2)$data
table(under_sam$final_result)

head(under_sam)

# Random Forest with balanced data
set.seed(7)
fit.cart4 <- train(final_result~., data=under_sam, method="rf", metric=metric, trControl=control)

# estimate skill of rf on the validation dataset
predictions_cart4 <- predict(fit.cart4, Test)
confusionMatrix(predictions_cart4, Test$final_result)

cart4 =  predict(fit.cart3, Test,type="prob")
two=  plot(roc(Test$final_result,cart4[,2]))

saveRDS(fit.cart4, "C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\models\\project2\\rf_balanced_model.rds")

# Random Forest with balanced data
set.seed(7)
fit.LDA1 <- train(final_result~., data=under_sam, method="lda", metric=metric, trControl=control)

# estimate skill of rf on the validation dataset
predictions_lda1 <- predict(fit.LDA1, Test)
confusionMatrix(predictions_lda1, Test$final_result)

lda1 =  predict(fit.LDA1, Test,type="prob")
two=  plot(roc(Test$final_result,lda1[,2]))

saveRDS(fit.LDA1, "C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\models\\project2\\LDA1_balanced_model.rds")

# GLM with balanced data
set.seed(7)
fit.GLM1 <- train(final_result~., data=under_sam, method="glm", metric=metric, trControl=control)

# estimate skill of rf on the validation dataset
predictions_GLM1 <- predict(fit.GLM1, Test)
confusionMatrix(predictions_GLM1, Test$final_result)

GLM1 =  predict(fit.GLM1, Test,type="prob")
two=  plot(roc(Test$final_result,GLM1[,2]))

saveRDS(fit.GLM1, "C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\models\\project2\\GLM1_balanced_model.rds")

# GLM with balanced data
set.seed(7)
fit.GBM1 <- train(final_result~., data=under_sam, method="gbm", metric=metric, trControl=control)

# estimate skill of rf on the validation dataset
predictions_GBM1 <- predict(fit.GBM1, Test)
confusionMatrix(predictions_GBM1, Test$final_result)

GBM1 =  predict(fit.GBM1, Test,type="prob")
two=  plot(roc(Test$final_result,GBM1[,2]))

saveRDS(fit.GBM1, "C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\models\\project2\\GBM1_balanced_model.rds")

roc_obj <- roc(Test$final_result, GBM1[,1])
auc(roc_obj)

roc_obj <- roc(Test$final_result, lda1[,1])
auc(roc_obj)

roc_obj <- roc(Test$final_result, GLM1[,1])
auc(roc_obj)

roc_rf <- roc(Test$final_result, cart4[,1])
auc(roc_obj)

# GLM with balanced data
set.seed(7)
fit.SVM1 <- train(final_result~., data=under_sam, method="svmRadial", metric=metric, trControl=control)

# estimate skill of rf on the validation dataset
predictions_SVM1 <- predict(fit.SVM1, Test)
confusionMatrix(predictions_SVM1, Test$final_result)

saveRDS(fit.SVM1, "C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\models\\project2\\SVM1_balanced_model.rds")

SVM1 =  predict(fit.SVM1, Test,type="prob")
two=  plot(roc(Test$final_result,SVM1[,2]))

main_df = read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\final_data2.csv")

unique(main_df$final_result)

new_selected= main_df[,which(names(main_df) %in% c("weight","TMA_score","TMA_count","NA_homepage","sum_click","CMA_count","NA_quiz",
                             "code_module.x.x","code_presentation.x.x","studied_credits","age_band","final_result"))]

head(new_selected)
unique(new_selected$final_result)

 new_selected[new_selected$final_result=="Distinction","final_result"] = "Pass"

 new_selected[new_selected$final_result=="Withdrawn","final_result"] = "Fail"

write.csv(new_selected,"C:\\Users\\shrey\\OneDrive\\Desktop\\with_withdraw.csv", row.names = FALSE)

DF = read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\with_withdraw.csv", na.strings = c("","NA"))

library(dummies)
#making dummies for result:
dum = dummy(DF$final_result,sep="_")

head(dum,2)

DF = cbind(DF,dum)

DF = DF[,-which(names(DF) %in% c("NA_Fail","final_result"))]

table(DF$NA_Pass)
DF$NA_Pass = as.factor(DF$NA_Pass)


## 90% of the sample size
smp_size <- floor(0.90 * nrow(DF))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(DF)), size = smp_size)

Train <- DF[train_ind, ]
Test <- DF[-train_ind, ]

row.names(Train) = NULL
row.names(Test)= NULL

# GLM with balanced data
set.seed(7)
fit.GBM2 <- train(NA_Pass~., data=Train, method="gbm", metric=metric, trControl=control)

# estimate skill of rf on the validation dataset
predictions_GBM2 <- predict(fit.GBM2, Test)
confusionMatrix(predictions_GBM2,Test$NA_Pass)

GBM2 =  predict(fit.GBM2, Test,type="prob")
two=  plot(roc(Test$NA_Pass,GBM2[,2]))

roc_obj <- roc(Test$NA_Pass, GBM2[,1])
auc(roc_obj)

#Training with 

saveRDS(fit.GBM2, "C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\models\\project2\\GBM_with_Withdraw_model.rds")

#Load saved model:
model_GBM2 = readRDS("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\models\\project2\\GBM_with_Withdraw_model.rds")


df_target = data.frame(Test$NA_Pass)
#predicting from those models
gbm_pred1 =  predict(model_GBM2, Test)

# make a data frame for all the prediction
df_target = data.frame(Test$NA_Pass)

#add the prediction
df_target$gbm_pred1 = gbm_pred1

head(df_target)

#GBM
print("Results for GLM with 90:10 split \n")
p <- data.frame(Actual = df_target$Test.NA_Pass , Prediction = df_target$gbm_pred1)
p <- table(p)
p

accuracy <- (p[1,1] + p[2,2])/sum(p)
cat("Accuracy for GLM is =",accuracy,"\n")

precision <- (p[2,2]/(p[2,2] + p[1,2]))

cat("precision for GLM is =",precision,"\n")
recall <- (p[2,2]/(p[2,2] + p[2,1]))

cat("recall for GLM is =",recall,"\n")
f_score <- 2*((precision*recall)/(precision+recall))

cat("f_score for GLM is =",f_score,"\n")
g_score <- sqrt(precision*recall)

cat("g_score for GLM is =",g_score,"\n")

library(pROC)

gbm_pred1 =  predict(model_GBM2, Test,type="prob")

roc_obj <- roc(Test$NA_Pass, gbm_pred1[,1])
auc(roc_obj)

library(rpart)
library(rpart.plot)

model.a <- rpart(NA_Pass~.,
data=Train,
method="class")
rpart.plot(model.a)

names(Train)

library(caret)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# GLM with balanced data
set.seed(7)
fit.GLM2 <- train(NA_Pass~., data=Train, method="glm", metric=metric, trControl=control)

# estimate skill of glm on the validation dataset
predictions_GLM2 <- predict(fit.GLM2, Test)
confusionMatrix(predictions_GLM2,Test$NA_Pass)

# Prediction
GLM2 =  predict(fit.GLM2, Test,type="prob")
two=  plot(roc(Test$NA_Pass,GLM2[,2]))

glm_pred1 =  predict(fit.GLM2, Test,type="prob")

roc_obj <- roc(Test$NA_Pass, glm_pred1[,1])
auc(roc_obj)

#GLM
saveRDS(fit.GLM2, "C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\models\\project2\\GLM_with_Withdraw_model.rds")

# GLM with balanced data
set.seed(7)
fit.RF2 <- train(NA_Pass~., data=Train, method="rf", metric=metric, trControl=control)

# estimate skill of glm on the validation dataset
predictions_RF2 <- predict(fit.RF2, Test)
confusionMatrix(predictions_RF2,Test$NA_Pass)

RF_pred2 =  predict(fit.RF2, Test,type="prob")

roc_obj <- roc(Test$NA_Pass, RF_pred2[,1])
auc(roc_obj)

# Prediction
RF2 =  predict(fit.RF2, Test,type="prob")
two=  plot(roc(Test$NA_Pass,RF2[,2]))

#GLM
saveRDS(fit.RF2, "C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\models\\project2\\RF_with_Withdraw_model.rds")

df_target = data.frame(Test$NA_Pass)
#predicting from those models
glm_pred1 =  predict(fit.GLM2, Test)
rf_pred1 = predict(fit.RF2,Test)
# make a data frame for all the prediction
df_target = data.frame(Test$NA_Pass)

#add the prediction
df_target$glm_pred1 = glm_pred1
df_target$rf_pred1 = rf_pred1



head(df_target)


#GLM
print("Results for GLM with 90:10 split \n")
p <- data.frame(Actual = df_target$Test.NA_Pass , Prediction = df_target$glm_pred1)
p <- table(p)
p

accuracy <- (p[1,1] + p[2,2])/sum(p)
cat("Accuracy for GLM is =",accuracy,"\n")

precision <- (p[2,2]/(p[2,2] + p[1,2]))

cat("precision for GLM is =",precision,"\n")
recall <- (p[2,2]/(p[2,2] + p[2,1]))

cat("recall for GLM is =",recall,"\n")
f_score <- 2*((precision*recall)/(precision+recall))

cat("f_score for GLM is =",f_score,"\n")
g_score <- sqrt(precision*recall)

cat("g_score for GLM is =",g_score,"\n")


#RF
print("Results for RF with 90:10 split \n")
p <- data.frame(Actual = df_target$Test.NA_Pass , Prediction = df_target$rf_pred1)
p <- table(p)
p

accuracy <- (p[1,1] + p[2,2])/sum(p)
cat("Accuracy for RF is =",accuracy,"\n")

precision <- (p[2,2]/(p[2,2] + p[1,2]))

cat("precision for RF is =",precision,"\n")
recall <- (p[2,2]/(p[2,2] + p[2,1]))

cat("recall for RF is =",recall,"\n")
f_score <- 2*((precision*recall)/(precision+recall))

cat("f_score for RF is =",f_score,"\n")
g_score <- sqrt(precision*recall)

cat("g_score for RF is =",g_score,"\n")

#Load saved model:
fit.GBM2 = readRDS("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\models\\project2\\GBM_with_Withdraw_model.rds")


gbm_pred1 =  predict(fit.GBM2, Test,type="prob")
glm_pred1 =  predict(fit.GLM2, Test,type="prob")
rf_pred1 =  predict(fit.RF2, Test,type="prob")

one=plot(roc(Test$NA_Pass,rf_pred1[,2]))
two= lines(roc(Test$NA_Pass,gbm_pred1[,2]),col="red")
three = lines(roc(Test$NA_Pass,glm_pred1[,2]),col="BLUE")




