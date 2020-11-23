library(rjags)
library(yaml)

source('functions/jags-functions.R') # run jags per model
source('functions/posterior-plot.R') # plot posterior distributions
source('functions/decision-functions.R')

# A data set
data <- yaml.load_file("example-3-edm-promotion/example-3-data.yaml")
df <- data.frame(data$data)

data <- list(
  N1 = table(df$group), # total sample in each group 
  N2 = table(df$open, df$group)[2,], # opened in each group
  x1 = table(df$event, df$group)[2,], # total did event
  x2 = table(df$event[df$open == 1], df$group[df$open == 1])[2,] # total did event that opened
)

model = "model {

  # Priors
  logits[1] ~ dnorm(0, 1)
  logits[2] ~ dt(logits[1], (1/0.1)^2, 1)
  
  logits_[1] ~ dnorm(0, 1)
  logits_[2] ~ dt(logits[1], (1/0.1)^2, 1)
  
  for(i in 1:2){
  
    # Transform from logit to success prob.
    theta[i] <- 1.0/(1.0 + exp(-logits[i]))
    psi[i] <- 1.0/(1.0 + exp(-logits_[i]))
    
    x1[i] ~ dbin(theta[i], N1[i])
    x2[i] ~ dbin(psi[i], N2[i])
  }
  
  loss_theta[1] <- step(theta[2] - theta[1])*(theta[2] - theta[1]) 
  loss_theta[2] <- step(theta[1] - theta[2])*(theta[1] - theta[2])
  
  loss_psi[1] <- step(psi[2] - psi[1])*(theta[2] - psi[1]) 
  loss_psi[2] <- step(psi[1] - psi[2])*(psi[1] - psi[2])
    
}
"
variable_names = c("theta", "psi", "loss_theta", "loss_psi")
jm <-  do_jags(data, variable_names, model = model)
ps <- jm$results
p_summary(ps)

png('Report/plots/example-3-joint.png', width = 600, height = 300)
par(mfrow = c(1,2), mar = c(3,3,2,1))
plot(ps$theta_a, ps$theta_b, col = rgb(0.5, 0, 0.5, 0.5), pch = 19)
title(main = expression("Joint Posterior of "*theta[A]*" and "*theta[B]))
title(xlab = expression(theta[A]), line = 2)
title(ylab = expression(theta[B]), line = 2)
plot(ps$psi_a, ps$psi_b, col = rgb(0, 0.5, 0.5, 0.5), pch = 19)
title(main = expression("Joint Posterior of "*psi[A]*" and "*psi[B]))
title(xlab = expression(psi[A]), line = 2)
title(ylab = expression(psi[B]), line = 2)
dev.off()


png("Report/plots/example-3-ps-jags-binomial.png", width = 600, height = 300)
posterior_plot(ps, pt = "Example 3 Binomial Model Posterior Distributions from MCMC")
dev.off()

# Which one is better ? 
ptbb(ps$theta_a, ps$theta_b)

png("Report/plots/example-3-expected-loss.png", width = 600, height = 300)
plot_loss(ps$loss_theta_a, ps$loss_theta_b, pt = expression("Example 3 Expected Loss "*theta))
dev.off()

png("Report/plots/example-3-rope.png", width = 600, height = 300)
par(mfrow = c(1,2))
rope(ps$theta_a, ps$theta_b, xl = expression(theta[B]~"-"~theta[A]))
rope(ps$psi_a, ps$psi_b, xl = expression(psi[B]~"-"~psi[A]))
dev.off()

# end