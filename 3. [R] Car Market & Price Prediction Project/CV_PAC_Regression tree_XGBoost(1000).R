###用XGboost(1000)調整參數###

setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')


#Build regression tree using xgboost with k-fold cv
library(xgboost)
library(caret)

# 准备数据
labels <- data$price # 目标变量
data_no_price <- data[, -which(names(data) == "price")] # 特征集


# 使用 designTreatmentsZ 处理数据
library(vtreat)
trt = designTreatmentsZ(dframe = data, varlist = names(data_no_price))
newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']


# 5折交叉验证
folds <- createFolds(labels, k = 5, list = TRUE, returnTrain = TRUE)
results_train <- c()
results <- c()

for(i in 1:5) {
  # 分割数据
  trainIndexes <- folds[[i]]
  trainData <- data_no_price[trainIndexes, ]
  trainLabel <- labels[trainIndexes]
  testData <- data_no_price[-trainIndexes, ]
  testLabel <- labels[-trainIndexes]
  
  # 应用处理到训练数据和测试数据
  trainData_treated <- prepare(trt, dframe = trainData, varRestriction = newvars)
  testData_treated <- prepare(trt, dframe = testData, varRestriction = newvars)
  
  
  #eta学习率控制每个树对最终预测的贡献。较小的学习率需要更多的树（nrounds）来构建模型
  #打印额外的消息
  #param <- list(max_depth = 3, eta = 0.3, colsample_bytree = 0.7, 
                #lambda = 1, alpha = 0)
  bst <- xgboost(data = as.matrix(trainData_treated),
                 label = trainLabel,
                 nrounds=10000, #Boot4也是10000
                 early_stopping_rounds = 100)
                 #param
  
  # 在验证集上进行预测
  pred_train <- predict(bst, newdata= as.matrix(trainData_treated))
  pred <- predict(bst, newdata= as.matrix(testData_treated))
  
  # 计算并记录 RMSE
  rmse <- sqrt(mean((pred - testLabel)^2))
  rmse_train <- sqrt(mean((pred_train - trainLabel)^2))
  results <- c(results, rmse)
  results_train<- c(results_train, rmse_train)
}

# 计算平均 RMSE
Boost5_meanRMSE <- mean(results)
print(Boost5_meanRMSE)

train_Boost5_meanRMSE = mean(results_train)
print(train_Boost5_meanRMSE)
