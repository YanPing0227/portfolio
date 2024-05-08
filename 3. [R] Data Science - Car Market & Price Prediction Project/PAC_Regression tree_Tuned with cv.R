setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')


#Build regression tuned tree 
library(caret)
trControl = trainControl(method = 'cv',number = 5)
tuneGrid = expand.grid(cp = seq(0,0.1,0.0001))

set.seed(1031)
tree_cv = train(price~.,
                data = data,
                method = 'rpart',
                trControl = trControl, 
                tuneGrid = tuneGrid)

plot(tree_cv)
tree_cv$bestTune

cvtree = rpart(price~., 
               data = data, 
               method = 'anova', 
               cp = tree_cv$bestTune)




#Predict price using the model
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_scoringData_preprocessing.RData')
pred = predict(cvtree, newdata=scoringData)



#Output outcome in excel
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'PAC_Regression tree_Tuned with cv .csv',row.names = F)



