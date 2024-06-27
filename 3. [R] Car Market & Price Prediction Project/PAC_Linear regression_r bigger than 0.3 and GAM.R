setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')



#Build linear regression model with all predictors
library(leaps)
library(mgcv)
model5 = gam(price~s(horsepower) + s(mileage) + s(width_inches) + is_new + s(back_legroom_inches) + franchise_dealer + s(owner_count), data = data)


#Predict price using the model
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_scoringData_preprocessing.RData')
pred = predict(model5, newdata=scoringData)



#Output outcome in excel
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'Linear regression_r bigger than 0.3 and GAM.csv',row.names = F)








#######tts
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_train.RData')
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_test.RData')



#install.packages("mgcv")
library(mgcv)
model5_train = gam(price~s(horsepower) + s(mileage) + s(width_inches) + is_new + s(back_legroom_inches) + franchise_dealer + s(owner_count), method = 'REML', data = train)
pred_test = predict(model5_train, newdata=test)


#Calculate rmse
df = data.frame(true = test$price, 
                pred = pred_test, 
                e = pred_test - test$price)
model5_tts_rmse = sqrt(mean(df$e^2))
model5_tts_rmse




#Poly lm
model_horsepower = lm(price~poly(horsepower, 2) ,data = data)
summary(model_horsepower)

model_mileage = lm(price~poly(mileage, 2) ,data = data)
summary(model_mileage)

model_width_inches = lm(price~poly(width_inches, 2) ,data = data)
summary(model_width_inches)

model_back_legroom_inches = lm(price~poly(back_legroom_inches, 2) ,data = data)
summary(model_back_legroom_inches)

model_owner_count = lm(price~poly(owner_count, 2) ,data = data)
summary(model_owner_count)


library(leaps)
model5_train = lm(price~poly(horsepower, 2) + poly(mileage, 2) + poly(width_inches, 2) + is_new + poly(back_legroom_inches, 2) + franchise_dealer + poly(owner_count, 2), data = data)
pred_test = predict(model5_train, newdata=test)


#Calculate rmse
df = data.frame(true = test$price, 
                pred = pred_test, 
                e = pred_test - test$price)
model5_tts_rmse = sqrt(mean(df$e^2))
model5_tts_rmse
