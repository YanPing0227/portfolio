#Research Question3
#No effect
library(data.table)
library(DT)

set.seed(329)

#Set parameters
B <- 1000  #times
n <- 199    #sample size
mean_handling_time <- 0  
sd_handling_time <- 1   

#Create simulation data
simulation.data <- data.table(Experiment = rep(1:B, each = n),
                              Group = rep(c("Bot", "Human"), each = n / 2))

simulation.data[, HandlingTime := rnorm(n, mean_handling_time, sd_handling_time), by = Experiment]

#Define function
analyze.experiment <- function(the.dat){
  the.test <- t.test(HandlingTime ~ Group, data = the.dat, alternative = "two.sided")
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  p <- the.test$p.value
  
  # ????????????????????????????????????
  false_positive <- (the.effect > 0 & p < 0.05)
  false_negative <- (the.effect < 0 & p >= 0.05)
  
  data.table(effect = the.effect, p = p, 
             false_positive = false_positive, 
             false_negative = false_negative)
}

#Analysis for simulated data
exp.results <- simulation.data[, analyze.experiment(.SD), by = "Experiment"]

#Calculate results
mean_effect <- mean(exp.results$effect)
ci <- quantile(exp.results$effect, probs = c(0.025, 0.975))
percentage_false_positives <- mean(exp.results$false_positive)
percentage_true_negatives <- mean(exp.results$effect <= 0 & exp.results$p >= 0.05)

#Create a table to show results
results_table <- data.table(
  MeanEffect = mean_effect,
  LowerCI = ci[1],
  UpperCI = ci[2],
  PercentageFalsePositives = percentage_false_positives,
  PercentageTrueNegatives = percentage_true_negatives
)


DT::datatable(results_table, rownames = FALSE)















#Expected effect
n = 199
library(data.table)
library(DT)
set.seed(329)

rr.dat = data.table(Group = c(rep.int(x = "Treatment", time = n/2),
                              rep.int(x = "Control", times = n/2)))

rr.dat[Group == "Control", RR := round(x = rnorm(n = .N, mean = 82, sd = 100), digits = 1)]
rr.dat[Group == "Treatment", RR :=round(x = rnorm(n = .N, mean = 57, sd = 100), digits = 1)]
datatable(data = rr.dat)

analyze.experiment <- function(the.dat){
  require(data.table)
  setDT(the.dat)
  
  the.test <- t.test(x = the.dat[Group == "Treatment",
                                 RR], y = the.dat[Group == "Control", RR], alternative = "less")
  
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, p = p)
  
  return(result)
}

analyze.experiment(the.dat = rr.dat)


B <- 1000
n = 199
RNGversion(vstr = 3.6)
set.seed(seed = 4172)
Experiment <- 1:B
Group = c(rep.int(x = "Treatment", time = n/2),
          rep.int(x = "Control", times = n/2))

sim.dat = as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Control", RR := round(x = rnorm(n = .N, mean = 82, sd = 100), digits = 1)]
sim.dat[Group == "Treatment", RR :=round(x = rnorm(n = .N, mean = 57, sd = 100), digits = 1)]
dim(sim.dat)

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD),
                       keyby = "Experiment"]
DT::datatable(data = round(x = exp.results[1:100,], digits = 3),
              rownames = F)


mean_effect <- mean(exp.results$effect)
mean_effect
ci <- quantile(exp.results$effect, probs = c(0.025, 0.975))
ci


# Assuming that the null is true when there is no difference or the difference is not greater than the true effect size
true_effect_size = -25
false_negatives <- sum(exp.results$p >= 0.05 & exp.results$effect > true_effect_size) / B
true_positives <- sum(exp.results$p < 0.05 & exp.results$effect > true_effect_size) / B
false_negatives
true_positives