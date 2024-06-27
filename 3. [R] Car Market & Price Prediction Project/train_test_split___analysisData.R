data_for_tts = read.csv('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/analysisData.csv')

library(caret)
set.seed(1031)
split = createDataPartition(y=data_for_tts$price,p = 0.7,list = F,groups = 20)
train = data_for_tts[split,]
test = data_for_tts[-split,]

unique_count = sapply(train, function(x) length(unique(x)))
unique_ratio = unique_count / nrow(train)
unique_df = data.frame(
  UniqueValues = unique_count,
  UniqueRatio = unique_ratio
)

unique_df_order = unique_df[order(-unique_df$UniqueRatio),]
unique_df_order


train = select(train, -description)


train[train == ""] = NA


na_count = sapply(train, function(column){
  sum(is.na(column))
})


na_percentage = na_count / nrow(train) * 100

na_df = data.frame(
  NA_count = na_count,
  NA_percentage = na_percentage
)


na_df[order(-na_df$NA_percentage),]


#Handle Missing values
train = select(train, -is_cpo)

ggplot(train, aes(x = owner_count)) +
  geom_bar() +
  xlab("Owner_count") +
  ylab("Frequency") +
  ggtitle("Distribution of Owner count") +
  theme(plot.title = element_text(hjust = 0.5))

median_owner_count = median(train$owner_count, na.rm = TRUE)
train$owner_count[is.na(train$owner_count)] = median_owner_count
unique(train$owner_count)

columns_to_dummy <- c("fleet", "frame_damaged", "has_accidents", "isCab", "salvage", "franchise_dealer", "is_new")

train = train %>%
  mutate(across(all_of(columns_to_dummy), ~ifelse(.x == "True", 1, ifelse(.x == "False", 0, NA))))

#mice
library(mice)
train = mice::complete(mice(train, method = "rf", seed = 666))


calculate_mode = function(x) {
  valid_values = na.omit(x)
  ux = names(which.max(table(valid_values)))
}

train = train %>%
  mutate(
    exterior_color = if_else(is.na(exterior_color), calculate_mode(exterior_color[!is.na(exterior_color)]), exterior_color),
    trim_name = if_else(is.na(trim_name), calculate_mode(trim_name[!is.na(trim_name)]), trim_name),
    wheel_system_display = if_else(is.na(wheel_system_display), calculate_mode(wheel_system_display[!is.na(wheel_system_display)]), wheel_system_display),
    wheel_system = if_else(is.na(wheel_system), calculate_mode(wheel_system[!is.na(wheel_system)]), wheel_system),
    fuel_type = if_else(is.na(fuel_type), calculate_mode(fuel_type[!is.na(fuel_type)]), fuel_type),
    engine_type = if_else(is.na(engine_type), calculate_mode(engine_type[!is.na(engine_type)]), engine_type),
    transmission_display = if_else(is.na(transmission_display), calculate_mode(transmission_display[!is.na(transmission_display)]), transmission_display),
    transmission = if_else(is.na(transmission), calculate_mode(transmission[!is.na(transmission)]), transmission),
    major_options = if_else(is.na(major_options), calculate_mode(major_options[!is.na(major_options)]), major_options)
  )


fill_na_with_probability = function(column) {
  freq = table(column[!is.na(column)])
  probs = freq / sum(freq)
  na_indices = which(is.na(column))
  column[na_indices] = sample(names(probs), length(na_indices), replace = TRUE, prob = probs)
  return(column)
}

train$torque = fill_na_with_probability(train$torque)
train$power = fill_na_with_probability(train$power)
train$franchise_make = fill_na_with_probability(train$franchise_make)


train = select(train, -id)


train$fuel_type = fct_collapse(train$fuel_type, "Diesel and Biodiesel" = c("Diesel", "Biodiesel"))
train$body_type = fct_lump(f = train$body_type, prop = 0.02)
train$maximum_seating = as.factor(train$maximum_seating)
train$maximum_seating = fct_lump(f = train$maximum_seating, prop = 0.003)
train$listing_color = fct_lump(f = train$listing_color, prop = 0.008)
train$engine_type = fct_lump(f = train$engine_type, prop = 0.0025)
train$transmission_display = fct_lump(f = train$transmission_display, prop = 0.0025)

save(train, file = 'PAC_train.RData')
load('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/PAC_train.RData')



# install.packages('leaps')
library(leaps)
subsets_train = regsubsets(price~.,data=train, nvmax=42)
summary(subsets_train)






###########test############
unique_count = sapply(test, function(x) length(unique(x)))
unique_ratio = unique_count / nrow(test)
unique_df = data.frame(
  UniqueValues = unique_count,
  UniqueRatio = unique_ratio
)

unique_df_order = unique_df[order(-unique_df$UniqueRatio),]
unique_df_order

test = select(test, -description)


test[test == ""] = NA


na_count = sapply(test, function(column){
  sum(is.na(column))
})


na_percentage = na_count / nrow(test) * 100

na_df = data.frame(
  NA_count = na_count,
  NA_percentage = na_percentage
)


na_df[order(-na_df$NA_percentage),]


test = select(test, -is_cpo)


ggplot(test, aes(x = owner_count)) +
  geom_bar() +
  xlab("Owner_count") +
  ylab("Frequency") +
  ggtitle("Distribution of Owner count") +
  theme(plot.title = element_text(hjust = 0.5))


median_owner_count = median(test$owner_count, na.rm = TRUE)
test$owner_count[is.na(test$owner_count)] = median_owner_count
unique(test$owner_count)


columns_to_dummy <- c("fleet", "frame_damaged", "has_accidents", "isCab", "salvage", "franchise_dealer", "is_new")

test = test %>%
  mutate(across(all_of(columns_to_dummy), ~ifelse(.x == "True", 1, ifelse(.x == "False", 0, NA))))


library(mice)
test = mice::complete(mice(test, method = "rf", seed = 666))


calculate_mode = function(x) {
  valid_values = na.omit(x)
  ux = names(which.max(table(valid_values)))
}

test = test %>%
  mutate(
    exterior_color = if_else(is.na(exterior_color), calculate_mode(exterior_color[!is.na(exterior_color)]), exterior_color),
    trim_name = if_else(is.na(trim_name), calculate_mode(trim_name[!is.na(trim_name)]), trim_name),
    wheel_system_display = if_else(is.na(wheel_system_display), calculate_mode(wheel_system_display[!is.na(wheel_system_display)]), wheel_system_display),
    wheel_system = if_else(is.na(wheel_system), calculate_mode(wheel_system[!is.na(wheel_system)]), wheel_system),
    fuel_type = if_else(is.na(fuel_type), calculate_mode(fuel_type[!is.na(fuel_type)]), fuel_type),
    engine_type = if_else(is.na(engine_type), calculate_mode(engine_type[!is.na(engine_type)]), engine_type),
    transmission_display = if_else(is.na(transmission_display), calculate_mode(transmission_display[!is.na(transmission_display)]), transmission_display),
    transmission = if_else(is.na(transmission), calculate_mode(transmission[!is.na(transmission)]), transmission),
    major_options = if_else(is.na(major_options), calculate_mode(major_options[!is.na(major_options)]), major_options)
  )


fill_na_with_probability = function(column) {
  freq = table(column[!is.na(column)])
  probs = freq / sum(freq)
  na_indices = which(is.na(column))
  column[na_indices] = sample(names(probs), length(na_indices), replace = TRUE, prob = probs)
  return(column)
}

test$torque = fill_na_with_probability(test$torque)
test$power = fill_na_with_probability(test$power)
test$franchise_make = fill_na_with_probability(test$franchise_make)


test$fuel_type = fct_collapse(test$fuel_type, "Diesel and Biodiesel" = c("Diesel", "Biodiesel"))
test$body_type = fct_lump(f = test$body_type, prop = 0.02)
test$maximum_seating = as.factor(test$maximum_seating)
test$maximum_seating = fct_lump(f = test$maximum_seating, prop = 0.003)
test$listing_color = fct_lump(f = test$listing_color, prop = 0.008)
test$engine_type = fct_lump(f = test$engine_type, prop = 0.0025)
test$transmission_display = fct_lump(f = test$transmission_display, prop = 0.0025)

save(test, file = 'PAC_test.RData')



#calculate rmse
df = data.frame(true = test$price, 
                pred = pred, 
                e = pred-test$price)
df
sqrt(mean(df$e^2))


