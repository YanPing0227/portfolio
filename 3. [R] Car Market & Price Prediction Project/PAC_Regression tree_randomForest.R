setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')


#Build regression tree using Bag_randomForest
library(randomForest)
set.seed(1031)
bag = randomForest(price~., 
                   data, 
                   mtry = ncol(data)-1,
                   ntree = 1000)


#Predict price using the model
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_scoringData_preprocessing.RData')
pred = predict(bag, newdata = scoringData)


#Output outcome in excel
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'PAC_Regression tree_Bag_randomForest.csv',row.names = F)


##########################################################################################

setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')


#Build regression tree using randomForest
library(randomForest)
set.seed(1031)
forest = randomForest(price~., 
                      data, 
                      ntree = 1000)



#Predict price using the model
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_scoringData_preprocessing.RData')
pred = predict(forest, newdata = scoringData)


#Output outcome in excel
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'PAC_Regression tree_randomForest.csv',row.names = F)




##########################################################################################
##########################################################################################



setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')


library(randomForest)
trControl = trainControl(method = 'cv', number = 5)
tuneGrid = expand.grid(mtry = 1:ncol(data)-1)
set.seed(1031)
forest_cv = train(price~., 
                  data = data, 
                  method = 'rf', 
                  trControl = trControl, 
                  tuneGrid = tuneGrid, 
                  ntree = 1000)
forest_cv$bestTune$mtry


set.seed(1031)
cvforest = randomForest(price~., 
                        data, 
                        mtry = forest_cv$bestTune$mtry, 
                        ntree = 1000)



#Predict price using the model
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_scoringData_preprocessing.RData')
pred = predict(cvforest, newdata = scoringData)


#Output outcome in excel
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'PAC_Regression tree_Tuned randomForest.csv',row.names = F)






