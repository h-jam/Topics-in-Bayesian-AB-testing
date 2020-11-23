library(yaml)

N <- 1000
open_rate <- 0.25

sim_w <- function(N, shape, scale, phi, s){
  
  set.seed(s)
  
  # Now simulate data
  time <- rweibull(N, shape, scale) + abs(dnorm(N, 0, 0.5))
  # Keep times of proportion converting
  time_ <- ifelse(runif(N) < phi, time, NA) 
  # proportioned that opened
  open <- rbinom(N, 1, open_rate)
  
  # conversion rate action rates for not opening email
  time_[open == 0] <-  ifelse(rbinom(sum(open == 0), 1, 0.2) == 1, time_, NA)

  # outcome censored if event exceeds duration
  time_ <- ifelse(time_ <= 30, time_, NA) 
  # censored indicator
  censored <- ifelse(is.na(time_), 1, 0) 
  # observed times in experiment
  time_diff <- ifelse(is.na(time_), 30, time_) 
  
  event <- (censored-1)*-1
  
  par(mfrow = c(2,2))
  hist(time, breaks = 30)
  hist(time_, breaks = 30)
  hist(time_diff, breaks = 30)
  
  print(prop.table(table(censored)))
  
  group <- cbind(time_diff, censored, open, event)
  
  return(group)
  
}

group1 <- data.frame(sim_w(N = N, shape = 1.8, scale = 15, phi = 0.70, s = 7654))
group2 <- data.frame(sim_w(N = N, shape = 1.5, scale = 10, phi = 0.80, s = 8643))

params <- list(nu_a = 1.8, nu_b = 1.5, w_a = 15, w_b = 10, phi_a = 0.7, phi_b = 0.8)

final <- rbind(group1, group2)
final$group <- rep(c(1,2), each = N)

df <- list(params = params, data = final)

write_yaml(df, "example-3-edm-promotion/example-3-data.yaml")
