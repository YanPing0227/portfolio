setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')


#Build regression tree using ranger
library(ranger)
set.seed(1031)
forest_ranger = ranger(price~.,
                       data = data, 
                       num.trees = 1000)


#Predict price using the model
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_scoringData_preprocessing.RData')
pred = predict(forest_ranger, newdata = scoringData, num.trees = 1000)


#Output outcome in excel
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'PAC_Regression tree_ranger.csv',row.names = F)


################################################################################

setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')


#Build regression tree using tuned ranger
trControl=trainControl(method="cv",number=5)
tuneGrid = expand.grid(mtry=1:ncol(data)-1, 
                       splitrule = c('variance','extratrees','maxstat'), 
                       min.node.size = c(2,5,10,15,20,25))
set.seed(1031)
cvModel = train(price~.,
                data=data,
                method="ranger",
                num.trees=1000,
                trControl=trControl,
                tuneGrid=tuneGrid)
cvModel$bestTune



set.seed(1031)
cv_forest_ranger = ranger(price~.,
                          data=data,
                          num.trees = 1000, 
                          mtry=cvModel$bestTune$mtry, 
                          min.node.size = cvModel$bestTune$min.node.size, 
                          splitrule = cvModel$bestTune$splitrule)



#Predict price using the model
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_scoringData_preprocessing.RData')
pred = predict(cv_forest_ranger, newdata = scoringData, num.trees = 1000)


#Output outcome in excel
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'PAC_Regression tree_tuned ranger.csv',row.names = F)



