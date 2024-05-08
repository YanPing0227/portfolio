library(vtreat)
library(xgboost)
library(lightgbm)
library(keras)
setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')

data <- as.data.frame(data)
labels <- data$price
features <- data[, -which(names(data) == "price"), drop = FALSE]

# XGBoost 预测
trt = designTreatmentsZ(dframe = data,
                        varlist = names(data)[1:33])

newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']

data_input = prepare(treatmentplan = trt, 
                     dframe = data,
                     varRestriction = newvars)

xgboost = xgboost(data=as.matrix(data_input), 
                  label = data$price,
                  nrounds=10000,
                  early_stopping_rounds = 100)      
xgb_pred <- predict(xgboost, as.matrix(data_input))

save(xgboost, xgb_pred,file = 'PAC_xg.RData')

# LightGBM 预测
dtrain <- lgb.Dataset(data = as.matrix(data_input), label = labels)


# 训练模型
model_lgb <- lgb.train(
  objective = "regression",
  metric = "rmse",
  data = dtrain,
  nrounds = 10000,
  num_leaves = 20,
  learning_rate = 0.01,
  max_depth = 5,
  bagging_fraction = 0.7,
  early_stopping_rounds = 100
)   

lgb_pred <- predict(model_lgb, as.matrix(data_input))





# 合并这些预测作为新的特征集
stacked_features <- data.frame(xgb_pred, lgb_pred)





library(keras)

# 构建 Keras 模型
meta_model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = 'relu', input_shape = ncol(stacked_features)) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 1)

# 编译模型
meta_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = 'adam',
  metrics = c('mean_absolute_error')
)

# 训练元模型
history <- meta_model %>% keras::fit(
  as.matrix(stacked_features),
  labels,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)



# 加载测试数据并应用 vtreat 处理
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_scoringData_preprocessing.RData')
test_data_processed <- prepare(trt, scoringData, varRestriction = newvars)

# 使用基础模型对处理过的测试数据进行预测
xgb_test_pred <- predict(xgboost, as.matrix(test_data_processed))
lgb_test_pred <- predict(model_lgb, as.matrix(test_data_processed))


# 合并测试预测结果
stacked_test_features <- data.frame(xgb_test_pred, lgb_test_pred)

# 使用元模型进行最终预测
final_predictions <- predict(meta_model, as.matrix(stacked_test_features))

# 输出结果到 Excel 文件
submissionFile <- data.frame(id = scoringData$id, price = final_predictions)
write.csv(submissionFile, 'PAC_Regression_stacked_xg_lightgbm_keras.csv', row.names = F)