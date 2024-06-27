setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')


#Build regression tree using boost_gbm
installed.packages("gbm")
library(gbm)
set.seed(617)
boost = gbm(price~.,
            data=data,
            distribution="gaussian",
            n.trees = 500,
            interaction.depth = 2,
            shrinkage = 0.01)
pred_train = predict(boost, n.trees=500)


#Predict price using the model
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_scoringData_preprocessing.RData')
pred = predict(boost, newdata = scoringData, n.trees = 500)


#Output outcome in excel
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'PAC_Regression tree_Boost_bgm.csv',row.names = F)



#######################################################################################



setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')


#Build regression tree using boost_tuned gbm
library(caret)
set.seed(1031)
trControl = trainControl(method="cv",number=5)
tuneGrid = expand.grid(n.trees = 500, 
                       interaction.depth = c(1,2,3),
                       shrinkage = (1:100)*0.001,
                       n.minobsinnode=c(5,10,15))
garbage = capture.output(cvModel <- train(price~.,
                                          data=data,
                                          method="gbm",
                                          trControl=trControl, 
                                          tuneGrid=tuneGrid))
set.seed(1031)
cvboost = gbm(price~.,
              data=data,
              distribution="gaussian",
              n.trees=500,
              interaction.depth=cvModel$bestTune$interaction.depth,
              shrinkage=cvModel$bestTune$shrinkage,
              n.minobsinnode = cvModel$bestTune$n.minobsinnode)


#Predict price using the model
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_scoringData_preprocessing.RData')
pred = predict(cvboost, newdata = scoringData, n.trees = 500)


#Output outcome in excel
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'PAC_Regression tree_Boost_tuned bgm.csv',row.names = F)




#######################################################################################
#######################################################################################
setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')


#Build regression tree using xgboost
library("vtreat")
trt = designTreatmentsZ(dframe = data,
                        varlist = names(data)[1:33])

newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']

data_input = prepare(treatmentplan = trt, 
                      dframe = data,
                      varRestriction = newvars)

library(xgboost)
xgboost = xgboost(data=as.matrix(data_input), 
                  label = data$price,
                  nrounds=10000,
                  verbose = 0,
                  early_stopping_rounds = 100)
xgboost$best_iteration



#Predict price using the model
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_scoringData_preprocessing.RData')
scoringData_input = prepare(treatmentplan = trt, 
                     dframe = scoringData,
                     varRestriction = newvars)
pred = predict(xgboost, newdata=as.matrix(scoringData_input))


#Output outcome in excel
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'PAC_Regression tree_Boost_xgboost.csv',row.names = F)


pred = predict(xgboost, newdata=as.matrix(data_input))
train1000_rmse <- sqrt(mean((data$price - train_predictions)^2))
print(train1000_rmse)
