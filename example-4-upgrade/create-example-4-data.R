library(yaml)
set.seed(6011)

n <- 1500
call_rate <- 0.3

sim_w <- function(N, shape, scale, phi){
  
  # Now simulate data
  time <- rweibull(N, shape, scale) + abs(dnorm(N, 0, 0.5))
  # Keep times of proportion converting
  time_ <- ifelse(runif(N) < phi, time, NA) 
  # duration in the experiment
  duration <- runif(N, 30, 60) 
  # outcome censored if event exceeds duration
  time_ <- ifelse(time <= duration, time_, NA) 
  # censored indicator
  censored <- ifelse(is.na(time_), 1, 0) 
  # observed times in experiment
  time_diff <- ifelse(is.na(time_), duration, time_) 
  
  par(mfrow = c(2,2))
  hist(time, breaks = 30)
  hist(time_, breaks = 30)
  hist(duration, breaks = 30)
  hist(time_diff, breaks = 30)
  
  print(prop.table(table(censored)))
  
  group <- cbind(time_diff, censored)
  
  return(group)
  
}

group1 <- data.frame(sim_w(N = n, shape = 2, scale = 40, phi = 0.08))
group1$call <- 0
group1$group <- 1

is_called <- rbinom(n, 1, call_rate)
group2_called <- sim_w(N = n, shape = 1.5, scale = 30, phi = 0.15)
group2_not_called <- sim_w(N = n, shape = 2, scale = 40, phi = 0.08)

x1 <- cbind(group2_called[is_called == 1,], call = 1, group = 2)
x2 <- cbind(group2_not_called[is_called == 0,], call = 0, group = 2)

final <- rbind(group1, data.frame(rbind(x1, x2)))

prop.table(table(final$censored, final$group),2)

write_yaml(final, "example-5-upgrade/example-5-data.yaml")
