library(pwr)
#Research Question 1
#human mean = 43200 sec
#bot mean = 1.7 sec
#diff mean = -43190.7
#sd = 10000
effect_size <- -4.31907  # Cohen's d
alpha <- 0.05           
power <- 0.8        

pwr.t.test(d = effect_size, power = power, sig.level = alpha, 
           type = "two.sample", alternative = "less")



#Research Question 2
#human mean = 0.3198
#bot mean = 0.2762
#sd = 0.79
effect_size <- -0.055
alpha <- 0.05      
power <- 0.8      

#Test sample size
pwr.t.test(d = effect_size, power = power, sig.level = alpha, 
           type = "two.sample", alternative = "less")



#Research Question 3
#computer mean 57
#human 82
#sd 100
effect_size = -0.25
alpha = 0.05
power = 0.8 

pwr.t.test(d = effect_size, power= power, sig.level = alpha, type = 'two.sample', alternative = 'less')

