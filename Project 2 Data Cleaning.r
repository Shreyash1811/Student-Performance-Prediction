
#loading all the data set 
assess= read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\assessments.csv ", na.strings = c("","NA"))
courses= read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\courses.csv ", na.strings = c("","NA"))
std_assess= read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\studentAssessment.csv ", na.strings = c("","NA"))
std_info= read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\studentInfo.csv ", na.strings = c("","NA"))
std_registration= read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\studentRegistration.csv ", na.strings = c("","NA"))
std_vle= read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\studentVle.csv ", na.strings = c("","NA"))
vle= read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\vle.csv ", na.strings = c("","NA"))

library(dplyr)
library(dummies)

#NA value here means that student didnt drop the class
head(std_registration)

#replacing those NA values with "stayed"
std_registration[is.na(std_registration$date_unregistration),"date_unregistration"] = "Completed"

# There are instances with missing date_registration
# We can replace them by grouping the data by code_presentaion and taking the 
# mean for the know days and replace on the missing places 


#Joining table student registration with student Information by id_student
std_join_info_reg = std_registration %>% inner_join(std_info, by = "id_student") 

#deleting duplicate code modeule and code presentaion columns
std_join_info_reg = std_join_info_reg[,-which(names(std_join_info_reg) %in% c("code_presentation.y","code_module.y"))]

length(unique(std_info$id_student))

length(unique(std_registration$id_student))

nrow(std_join_info_reg)

#joining vle student to vle data
vle_join = std_vle %>% left_join(vle, by = "id_site") 

#Checking NA values in the join
nrow(vle_join[!complete.cases(vle_join),])

nrow(vle_join)

# dropping week_from and week_to
vle_join = vle_join[,-which(names(vle_join) %in%  c("week_from","week_to"))]

# no NA values in vle table check 
nrow(vle_join[!complete.cases(vle_join),])
#deleting duplicate code modeule and code presentaion columns
vle_join = vle_join[,-which(names(vle_join) %in% c("code_presentation.y","code_module.y"))]

#JOINING
assess_join = std_assess %>% left_join(assess, by = "id_assessment") 

nrow(assess_join)

tom = dummy(vle_join$activity_type,sep="_")

vle_join_clean_withdummy = cbind(vle_join,tom)
vle_join_clean_withdummy = vle_join_clean_withdummy[,-which(names(vle_join_clean_withdummy) %in% c("activity_type","code_presentation.y","code_module.y","id_site"))]

length(unique(vle_join_clean_withdummy$date))
names(vle_join_clean_withdummy)

grouping_vle_join = vle_join_clean_withdummy %>% group_by(code_module.x,code_presentation.x,id_student) %>% summarise_at(c('sum_click', 'NA_dataplus', 'NA_dualpane', 
                                                                                                                  'NA_externalquiz', 'NA_folder' ,'NA_forumng'
                                                                                                                  ,'NA_glossary' ,'NA_homepage', 'NA_htmlactivity',
                                                                                                                  'NA_oucollaborate', 'NA_oucontent', 'NA_ouelluminate',
                                                                                                                  'NA_ouwiki','NA_page','NA_questionnaire','NA_quiz','NA_repeatactivity','NA_resource',
                                                                                                                  'NA_sharedsubpage','NA_subpage','NA_url'),sum, na.rm = TRUE)


#making a unique key code_module.x code_presentation.x and id_student
key = paste(grouping_vle_join$code_module.x,grouping_vle_join$code_presentation.x,grouping_vle_join$id_student)


grouping_vle_join$key = key

nrow(grouping_vle_join)

key= paste(std_join_info_reg$code_module.x,std_join_info_reg$code_presentation.x,std_join_info_reg$id_student)

std_join_info_reg$key = key

nrow(std_join_info_reg)

#joining vle join table with std join table
std_join_vle = std_join_info_reg %>% left_join(grouping_vle_join, by = "key") 

nrow(std_join_vle)

#dummy for Assessment_type
tom = dummy(assess_join$assessment_type,sep="_")

assess_join = cbind(assess_join,tom)

head(assess_join[assess_join$NA_TMA ==1,])

nrow(assess_join)

#dropping the date for the assessment since date has no impact on the scores
nrow(assess_join[!assess_join$NA_Exam ==1,])

# making key for assess_join
key = paste(assess_join$code_module,assess_join$code_presentation,assess_join$id_student)

assess_join$key = key

# weighted assessment 

assess_join$weighted_TMA = ((assess_join$NA_TMA * assess_join$weight)/100)*assess_join$score
assess_join$weighted_Exam = ((assess_join$NA_Exam * assess_join$weight)/100)*assess_join$score
assess_join$weighted_CMA = ((assess_join$NA_CMA * assess_join$weight)/100)*assess_join$score

join_ = assess_join %>% group_by(key) %>% summarise(weight= sum(weight),CMA_count=sum(NA_CMA),Exam_count=sum(NA_Exam),TMA_count=sum(NA_TMA),
                                                    TMA_score = sum(weighted_TMA),Exam_score=sum(weighted_Exam), CMA_Score=sum(weighted_CMA))

nrow(assess_join)
#after grouping
nrow(join_)

# joining assessment_join_grouped with main_table
main_table = std_join_vle


#joining assessment table with main_table
main_table_join= main_table %>% left_join(join_, by = "key")

nrow(main_table_join)

#Making key for Main table and course to join later
key1 = paste(courses$code_module,courses$code_presentation)
key2 = paste(main_table_join$code_module.x.x,main_table_join$code_presentation.x.x)

main_table_join$key= key2
courses$key = key1

finaltable_joined = main_table_join %>% left_join(courses, by = "key")

nrow(finaltable_joined)

nrow(finaltable_joined[!complete.cases(finaltable_joined),])

names(finaltable_joined)

#Deleting duplicate columns from final dataframe
finaltable_joined = finaltable_joined[,-which(names(finaltable_joined) %in% c("code_module.x.y","code_presentation.x.y",
                                                                              "code_module","code_presentation","id_student.y"))]

finaltable_joined$imd_band = is.character(finaltable_joined$imd_band)

finaltable_joined[which(finaltable_joined$imd_band =="10-20"),"imd_band"] = "10-20%"

finaltable_joined$key = as.factor(finaltable_joined$key)
lev = levels(finaltable_joined$key)

for (a in 1:length(lev)){
   finaltable_joined[is.na(finaltable_joined$date_registration) ,"date_registration"] = 
    mean(finaltable_joined[!is.na(finaltable_joined$date_registration) & finaltable_joined$key == lev[a],"date_registration"])
}

write.csv(finaltable_joined,"C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\final_data1.1.csv", row.names = FALSE)

head(finaltable_joined[complete.cases(finaltable_joined),which(names(finaltable_joined) %in% c("final_result","TMA_score","Exam_score",
                                                                                         "CMA_Score"))])

nrow(finaltable_joined[!complete.cases(finaltable_joined),])

names(finaltable_joined)

nrow(finaltable_joined)
finaltable_joined[is.na(finaltable_joined$sum_click),"sum_click"] = 0
finaltable_joined[is.na(finaltable_joined$NA_dataplus),"NA_dataplus"] = 0
finaltable_joined[is.na(finaltable_joined$NA_dualpane),"NA_dualpane"] = 0
finaltable_joined[is.na(finaltable_joined$NA_externalquiz),"NA_externalquiz"] = 0
finaltable_joined[is.na(finaltable_joined$NA_folder),"NA_folder"] = 0
finaltable_joined[is.na(finaltable_joined$NA_forumng),"NA_forumng"] = 0
finaltable_joined[is.na(finaltable_joined$NA_glossary),"NA_glossary"] = 0
finaltable_joined[is.na(finaltable_joined$NA_homepage),"NA_homepage"] = 0
finaltable_joined[is.na(finaltable_joined$NA_htmlactivity),"NA_htmlactivity"] = 0
finaltable_joined[is.na(finaltable_joined$NA_oucollaborate),"NA_oucollaborate"] = 0
finaltable_joined[is.na(finaltable_joined$NA_oucontent),"NA_oucontent"] = 0
finaltable_joined[is.na(finaltable_joined$NA_ouelluminate),"NA_ouelluminate"] = 0
finaltable_joined[is.na(finaltable_joined$NA_ouwiki),"NA_ouwiki"] = 0
finaltable_joined[is.na(finaltable_joined$NA_page),"NA_page"] = 0
finaltable_joined[is.na(finaltable_joined$NA_quiz),"NA_quiz"] = 0
finaltable_joined[is.na(finaltable_joined$NA_questionnaire),"NA_questionnaire"] = 0
finaltable_joined[is.na(finaltable_joined$NA_repeatactivity),"NA_repeatactivity"] = 0
finaltable_joined[is.na(finaltable_joined$NA_resource),"NA_resource"] = 0
finaltable_joined[is.na(finaltable_joined$NA_sharedsubpage),"NA_sharedsubpage"] = 0
finaltable_joined[is.na(finaltable_joined$NA_subpage),"NA_subpage"] = 0
finaltable_joined[is.na(finaltable_joined$NA_url),"NA_url"] = 0
finaltable_joined[is.na(finaltable_joined$weight),"weight"] = 0
finaltable_joined[is.na(finaltable_joined$CMA_count),"CMA_count"] = 0
finaltable_joined[is.na(finaltable_joined$Exam_count),"Exam_count"] = 0
finaltable_joined[is.na(finaltable_joined$TMA_count),"TMA_count"] = 0
finaltable_joined[is.na(finaltable_joined$TMA_score),"TMA_score"] = 0
finaltable_joined[is.na(finaltable_joined$Exam_score),"Exam_score"] = 0
finaltable_joined[is.na(finaltable_joined$CMA_Score),"CMA_Score"] = 0

nrow(finaltable_joined[!complete.cases(finaltable_joined),])

sapply(finaltable_joined, function(x) sum(is.na(x)))

write.csv(finaltable_joined,"C:\\Users\\shrey\\OneDrive\\Desktop\\BUSINESS ANALYTICS\\Datasets\\Project2\\final_data2.csv", row.names = FALSE)


