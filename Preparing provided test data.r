
train = read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\StudentTestFiles_tobeScored.csv", na.strings = c("","NA"))

library(caret)
library(ROSE)
library(dummies)

head(train,1)

names(train)

train= train[!is.na(train$activity_type),]
train= train[!is.na(train$assessment_type),]

#creating dummy for activity_type 
dum = dummy(train$activity_type,sep="_")

train = cbind(train,dum)

#Creating dummy for assessment_type
dum = dummy(train$assessment_type,sep = "_")

train = cbind(train,dum)

head(train)

#replacing unregistered NA with "completed"
train[is.na(train$date_unregistration),"date_unregistration"] = "Completed"
#removing unwanted columns
train= train[,-which(names(train) %in% c("week_from","week_to","date_vle","date_submitted","is_banked","date_assessment"))]
names(train)

#replacing NA score with mean of other NON NA scores
train[is.na(train$score),"score"] = mean(train[!is.na(train$score),"score"])

# Replacing mode of north region imd_band with missing imd_band
summary(train[train$region=="North Region"& !is.na(train$imd_band),"imd_band"])
train[is.na(train$imd_band),"imd_band"] ="10-20 %"

#no NA values
sapply(train, function(x) sum(is.na(x)))

#train$TMA_score = train$score

#train= train[,-which(names(train) %in% c("score"))]

#putting scores in the assessment
train$TMA_score = train$score* train$NA_TMA
train$Exam_score= train$score* train$NA_Exam
train$CMA_Score= train$score* train$NA_CMA

From_model = read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\final_data2.csv", na.strings = c("","NA"))

train= train[,-which(names(train) %in% c("NA_CMA","NA_Exam","NA_TMA","id_site","activity_type","id_assessment","score","assessment_type"))]


From_model= From_model[,-which(names(From_model) %in% c("key","X","CMA_count","Exam_count","TMA_count","NA_sharedsubpage",
                                                        "NA_dataplus","NA_dualpane","NA_folder","NA_htmlactivity","NA_ouelluminate",
                                                        "NA_repeatactivity"))]

length(names(From_model))
length(names(train))

names(From_model)
names(train)

colnames(From_model)[1] = 'code_module'
colnames(From_model)[2] = 'code_presentation'
colnames(From_model)[3] = 'id_student'

names(From_model)
names(train)

#converting distinction to pass and witdrawn as fail
From_model$final_result = as.factor(From_model$final_result )
From_model[From_model$final_result=="Distinction","final_result"] = "Pass"
From_model[From_model$final_result=="Withdrawn","final_result"] = "Fail"

unique(From_model$final_result)

write.csv(From_model,"C:\\Users\\shrey\\OneDrive\\Desktop\\Prof_train_data.csv", row.names = FALSE)

From_model = read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\Prof_train_data.csv")

#splitting the Model data
## 90% of the sample size
smp_size <- floor(0.90 * nrow(From_model))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(From_model)), size = smp_size)

Train <- From_model[train_ind, ]
Test <- From_model[-train_ind, ]

row.names(Train) = NULL
row.names(Test)= NULL

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# GLM with balanced data
set.seed(7)
fit.GBM1 <- train(final_result~., data=Train, method="gbm", metric=metric, trControl=control)

saveRDS(fit.GBM1, "C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\models\\project2\\testcasemodel.rds")

library(pROC)
GBM1 =  predict(fit.GBM1, Test,type="prob")
two=  plot(roc(Test$final_result,GBM1[,2]))
train$imd_band = FALSE

df_test_prof = data.frame(train$id_student)

df_test_prof$predicted= predict(fit.GBM1, train)

head(df_test_prof)

write.csv(df_test_prof,"C:\\Users\\shrey\\OneDrive\\Desktop\\Pro_predicted.csv", row.names = FALSE)


