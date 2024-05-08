setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_data_preprocessing_FeatureExtraction.RData')


#Build regression tree using Bag
library(ipred)
set.seed(1031) 
bag = bagging(price~.,
              data = data, 
              nbagg = 1000)


#Predict price using the model
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_scoringData_preprocessing.RData')
pred = predict(bag, newdata = scoringData)


#Output outcome in excel
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'PAC_Regression tree_Bag.csv',row.names = F)






