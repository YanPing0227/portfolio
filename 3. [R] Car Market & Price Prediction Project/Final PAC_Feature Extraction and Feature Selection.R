#Load preprocessing code from PAC_preprocessing.RData
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_preprocessing.RData')


#####Filter#####
###Bivariate Filter###
#High cor with price, low cor with other predictors
##ONLY NUMERIC variables correlation##
numeric_vars <- data[, sapply(data, is.numeric)]
numeric_vars

#Show each Numeric variable's correlation and p-value with price 
library(tidyr)
library(dplyr)
numeric_vars |>
  pivot_longer(1:25,names_to = 'var',values_to = 'values')|>
  group_by(var)|>
  summarize(r = round(cor(price, values),2), p = cor.test(price, values)$p.value)|>
  arrange(desc(abs(r))) %>% 
  print(n=Inf)


#Show Numeric variable's correlation, excluding price
corMatrix = as.data.frame(cor(numeric_vars[,-26], use="complete.obs"))
corMatrix$var1 = rownames(corMatrix)

corMatrix |>
  gather(key=var2,value=r,1:25)|>  #var2為原本各變量名
  arrange(var1,desc(var2))|>   #var1本身就是corMatrix的最左邊訊息那欄(一般狀況下會是index:1, 2, 3, 4, 5...)
  ggplot(aes(x=var1,y=reorder(var2, order(var2,decreasing=F)),fill=r))+ #order只會返回排序的數字向量(也就是index:1, 2, 3, 4, 5...)，reorder會按照數字向量對應的變量名作y軸排序
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'))+
  theme(axis.text.x=element_text(angle=75,hjust = 1))+xlab('')+ylab('')


#Show Numeric variable's correlation, including price
library(ggcorrplot)
ggcorrplot(cor(numeric_vars),
           method = 'square',
           type = 'lower',
           show.diag = F,
           colors = c('#e9a3c9', '#f7f7f7', '#a1d76a'))




#Drop id(small r and big p_value)
data = select(data, -id)



####Feature Extraction####
library(caret)
nzv = nearZeroVar(data, saveMetrics = TRUE)
lapply(data[c("frame_damaged", "salvage", "fuel_type")], table)
#Although these three variables are nzv, these three are significant


library(forcats)
fct_count(data$fuel_type)
lm_fuel_type <- lm(price ~ fuel_type, data = data)
summary(lm_fuel_type)
data$fuel_type = fct_collapse(data$fuel_type, "Diesel and Biodiesel" = c("Diesel", "Biodiesel"))


fct_count(data$body_type)
lm_body_type <- lm(price ~ body_type, data = data)
summary(lm_body_type)
data$body_type = fct_lump(f = data$body_type, prop = 0.02)



data$maximum_seating = as.factor(data$maximum_seating)
fct_count(data$maximum_seating)
lm_maximum_seating <- lm(price ~ maximum_seating, data = data)
summary(lm_maximum_seating)
data$maximum_seating = fct_lump(f = data$maximum_seating, prop = 0.003)
ggplot(data=data,aes(x= maximum_seating,y=price))+
  geom_point()+
  theme_bw()


fct_count(data$listing_color)
lm_listing_color <- lm(price ~ listing_color, data = data)
summary(lm_listing_color)
data$listing_color = fct_lump(f = data$listing_color, prop = 0.008)


fct_count(data$engine_type)
print(fct_count(data$engine_type), n = Inf)
lm_engine_type <- lm(price ~ engine_type, data = data)
summary(lm_engine_type)
data$engine_type = fct_lump(f = data$engine_type, prop = 0.0025)


fct_count(data$transmission_display)
print(fct_count(data$transmission_display), n = Inf)
lm_transmission_display <- lm(price ~ transmission_display, data = data)
summary(lm_transmission_display)
data$transmission_display = fct_lump(f = data$transmission_display, prop = 0.0025)




##Check coeffiecient and significance between rest each variable and price## 
print(fct_count(factor(data$interior_color)), n = Inf)
lm_interior_color <- lm(price ~ interior_color, data = data)
summary(lm_interior_color)


library(forcats)
print(fct_count(data$make_name), n = Inf)
lm_make_name <- lm(price ~ make_name, data = data)
summary(lm_make_name)


data$model_name <- as.factor(data$model_name)
print(fct_count(data$model_name), n = Inf)
lm_model_name <- lm(price ~ model_name, data = data)
summary(lm_model_name)


print(fct_count(data$trim_name), n = Inf)
lm_trim_name <- lm(price ~ trim_name, data = data)
summary(lm_trim_name)


print(fct_count(data$power), n = Inf)
lm_power <- lm(price ~ power, data = data)
summary(lm_power)


print(fct_count(data$torque), n = Inf)
lm_torque <- lm(price ~ torque, data = data)
summary(lm_torque)


print(fct_count(data$transmission), n = Inf)
lm_transmission <- lm(price ~ transmission, data = data)
summary(lm_transmission)


print(fct_count(data$wheel_system), n = Inf)
lm_wheel_system <- lm(price ~ wheel_system, data = data)
summary(lm_wheel_system)


print(fct_count(data$wheel_system_display), n = Inf)
lm_wheel_system_display <- lm(price ~ wheel_system_display, data = data)
summary(lm_wheel_system_display)


#顯著性比其他變數小(*少)
print(fct_count(data$major_options), n = Inf)
lm_major_options <- lm(price ~ major_options, data = data)
summary(lm_major_options)


print(fct_count(data$franchise_make), n = Inf)
lm_franchise_make <- lm(price ~ franchise_make, data = data)
summary(lm_franchise_make)

#顯著性比其他變數小(*少)，相關係數也較小
print(fct_count(data$listed_date), n = Inf)
lm_listed_date <- lm(price ~ listed_date, data = data)
summary(lm_listed_date)





###Multivariate Filter###
library(tidyr)
library(dplyr)
numeric_vars <- data[, sapply(data, is.numeric)]
numeric_vars
#No id, and no maximum seating(convert to chr)
model_multivar_num = lm(price~., data = numeric_vars)
summary(model_multivar_num) #no sig: height_inches, fleet
vif(model_multivar_num)
data.frame(Predictor = names(vif(model_multivar_num)), VIF = vif(model_multivar_num)) |>
  ggplot(aes(x=VIF, y = reorder(Predictor, VIF), fill=VIF))+
  geom_col()+
  geom_vline(xintercept=5, color = 'gray', size = 1.5)+
  geom_vline(xintercept = 10, color = 'red', size = 1.5)+
  scale_fill_gradient(low = '#fff7bc', high = '#d95f0e')+
  scale_y_discrete(name = "Predictor")+
  scale_x_continuous(breaks = seq(5,30,5))+
  theme_classic()
#VIF>10: length_inches, wheelbase_inches, highway_fuel_economy


#remove variables VIF>10
data = select(data, -c("length_inches", "wheelbase_inches", "highway_fuel_economy"))



#remove variables that have new observations in scoringData
data = select(data, -c("model_name", "trim_name", "power", "torque",
                       "engine_type", "exterior_color", "interior_color",
                       "major_options", "listed_date"))


# 在data_preprocessing.R中
# 在data_preprocessing.R中
save(data, file = 'PAC_data_preprocessing_FeatureExtraction.RData')
save(numeric_vars, file = 'PAC_numeric_vars_preprocessing_FeatureExtraction.RData')

# 在模型分析script中
#load('path/to/PAC_preprocessing_FeatureExtraction.RData')


################################################################################
################################################################################
################################################################################



#Only numeric vars using Lasso
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_numeric_vars_preprocessing_FeatureExtraction.RData')
library(glmnet)
x = model.matrix(price~.-1,data=numeric_vars)
y = numeric_vars$price
set.seed(617)
cv_lasso = cv.glmnet(x = x, 
                     y = y, 
                     alpha = 1,
                     type.measure = 'mse')
cv_lasso

coef(cv_lasso, s = cv_lasso$lambda.1se) |>
  round(4)


page(coef(cv_lasso, s = "lambda.1se"), method = "print")

cv_lasso_numchoice = data[, c("fuel_tank_volume_gallons", "city_fuel_economy", 
                              "front_legroom_inches", "length_inches", "height_inches", 
                              "engine_displacement", "horsepower", "daysonmarket",
                              "year", "franchise_dealer", "has_accidents", "isCab",
                              "is_new", "mileage", "owner_count", "seller_rating", "price")]
categorical_vars <- data[, sapply(data, function(x) !is.numeric(x))]
lasso_combined_data = cbind(cv_lasso_numchoice, categorical_vars)

setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
save(lasso_combined_data, file = 'PAC_lasso_combined_data.RData')
