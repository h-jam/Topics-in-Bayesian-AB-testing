library(rjags)
library(yaml)

source('functions/jags-functions.R') # run jags per model
source('functions/posterior-plot.R') # plot posterior distributions
source('functions/decision-functions.R')

# A data set
data <- yaml.load_file("example-1-landing-page/example-1-data.yaml")
df <- data.frame(data)

model = "model {

  # Priors
  logits[1] ~ dnorm(0, 1)
  logits[2] ~ dt(logits[1], (1/0.1)^2, 1)
  
  logits_[1] ~ dnorm(0, 1)
  logits_[2] ~ dt(logits_[1], (1/0.1)^2, 1)
  
  for(i in 1:2){
  
    # Transform from logit to success prob.
    theta[i] <- 1.0/(1.0 + exp(-logits[i]))
    psi[i] <- 1.0/(1.0 + exp(-logits_[i]))
    
    x1[i] ~ dbin(theta[i], N[i])
    x2[i] ~ dbin(psi[i], N[i])
  }
  
  loss[1] <- step(psi[2] - psi[1])*(psi[2] - psi[1]) 
  loss[2] <- step(psi[1] - psi[2])*(psi[1] - psi[2])
    
}
"
variable_names = c("theta", "psi", "loss")
jm <-  do_jags(data, variable_names, model = model)
ps <- jm$results

png('Report/plots/example-1-joint.png', width = 600, height = 300)
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

png("Report/plots/example-1-ps.png", width = 600, height = 300)
posterior_plot(ps, pt = "Example 1 Binomial Model Marginal Posterior Distributions from MCMC")
dev.off()

# Which one is better ? 
ptbb(ps$theta_a, ps$theta_b)
ptbb(ps$psi_a, ps$psi_b)

png("Report/plots/example-1-expected-loss.png", width = 600, height = 300)
plot_loss(ps$loss_a, ps$loss_b, pt = expression("Example 1 Expected Loss "*psi))
dev.off()

png("Report/plots/example-1-psi-rope.png", width = 600, height = 300)
rope(ps$psi_a, ps$psi_b)
dev.off()

# end