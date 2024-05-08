setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')


#Build linear regression model with all predictors
library(leaps)
model4 = lm(price~horsepower + mileage + width_inches + is_new + back_legroom_inches + franchise_dealer + owner_count, data = data)


#Predict price using the model
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_scoringData_preprocessing.RData')
pred = predict(model4, newdata=scoringData)



#Output outcome in excel
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'Linear regression_r bigger than 0.3.csv',row.names = F)








#######tts
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_train.RData')
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_test.RData')


library(leaps)
model4_train = lm(price~horsepower + mileage + width_inches + is_new + back_legroom_inches + franchise_dealer + owner_count, data = train)
pred_test = predict(model4_train, newdata=test)


#Calculate rmse
df = data.frame(true = test$price, 
                pred = pred_test, 
                e = pred_test - test$price)
model4_tts_rmse = sqrt(mean(df$e^2))
model4_tts_rmse

