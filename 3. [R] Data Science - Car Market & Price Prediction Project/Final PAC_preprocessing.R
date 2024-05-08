library(dplyr)
library(ggplot2)
setwd("C:/Users/User/Desktop/Columbia/Courses/5200/PAC")
data = read.csv('C:/Users/User/Desktop/Columbia/Courses/5200/PAC/analysisData.csv')

str(data)

#Check unique values for each column
unique_count = sapply(data, function(x) length(unique(x)))
unique_ratio = unique_count / nrow(data)
unique_df = data.frame(
  UniqueValues = unique_count,
  UniqueRatio = unique_ratio
)

unique_df_order = unique_df[order(-unique_df$UniqueRatio),]
unique_df_order


#Drop "description"
data = select(data, -description)


#####Calculate missing values#####

#Convert empty string to NA
data[data == ""] = NA

#Calculate NA values in each column
na_count = sapply(data, function(column){
  sum(is.na(column))
})


#Calculate the percentage of NA in each column
na_percentage = na_count/nrow(data)*100

na_df = data.frame(
  "NA_count" = na_count,
  "NA_percentage" = na_percentage
)

#Sort by the "Percentage" in descending order
na_df[order(-na_df$NA_percentage),]





#####Deal with missing values#####

#Drop "is_cpo" column due to high NA percentage
data = select(data, -is_cpo)

#Check distribution of "owner_count"
ggplot(data, aes(x = owner_count)) +
  geom_bar() + 
  xlab("Owner_count") +
  ylab("Frequency") +
  ggtitle("Distribution of Owner count") +
  theme(plot.title = element_text(hjust = 0.5))

#Fill NA with median for "owner_count"
median_owner_count = median(data$owner_count, na.rm = TRUE)
data$owner_count[is.na(data$owner_count)] = median_owner_count
unique(data$owner_count)


#Transform column types to dummy variables
columns_to_dummy <- c("fleet", "frame_damaged", "has_accidents", "isCab", "salvage",
                      "franchise_dealer", "is_new")

data = data %>%                         #~ and . ==> function(x)
  mutate(across(all_of(columns_to_dummy), ~ifelse(.x == "True", 1, ifelse(.x == "False", 0, NA))))


#Use mice ranger of Random Forest method to fill NA (since our dataset is large and its computation ability faster)
library(mice)
data = mice::complete(mice(data, method = "rf", seed = 666))




###Fill NA for left categorical variables
#Fill NA with mode if na_percentage < 10%
calculate_mode = function(x) {
  valid_values = na.omit(x)
  ux = names(which.max(table(valid_values)))
}


data = data %>%
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



#Fill NA with probability if 10% < na_percentage <  40%

fill_na_with_probability = function(column) {
  freq = table(column[!is.na(column)])
  probs = freq / sum(freq)
  na_indices = which(is.na(column))
  column[na_indices] = sample(names(probs), length(na_indices), replace = TRUE, prob = probs)
  return(column)
}

data$torque = fill_na_with_probability(data$torque)
data$power = fill_na_with_probability(data$power)
data$franchise_make = fill_na_with_probability(data$franchise_make)




# 在PAC_preprocessing.R中
save(data, file = 'PAC_preprocessing.RData')

# 在模型分析script中
#load('path/to/PAC_preprocessing.RData')


