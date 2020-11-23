library(rjags)
library(yaml)

source('functions/jags-functions.R') # run jags per model
source('functions/posterior-plot.R') # plot posterior distributions
source('functions/decision-functions.R')

# A data set
data <- yaml.load_file("example-2-first-action/example-2-data.yaml")
df <- data.frame(data$data)

data <- list(
  N = table(df$group), 
  x = table(df$group, df$event)[,2]
)

model = "model {
  # Priors
  logits[1] ~ dnorm(0, 1)
  logits[2] ~ dt(logits[1], (1/0.1)^2, 1)
  
  for(i in 1:2){
  
    # Transform from logit to success prob.
    theta[i] <- 1.0/(1.0 + exp(-logits[i]))
    x[i] ~ dbin(theta[i], N[i])
  }
  
  loss_theta[1] <- step(theta[2] - theta[1])*(theta[2] - theta[1]) 
  loss_theta[2] <- step(theta[1] - theta[2])*(theta[1] - theta[2])
    
}
"
variable_names = c("theta", "loss_theta")
jm <-  do_jags(data, variable_names, model = model)
ps <- jm$results
p_summary(ps)

png("Report/plots/example-2-joint.png", width = 600, height = 300)
p1 <- ggplot(ps, aes(x = theta_a, y = theta_b)) + 
  geom_point(colour = rgb(0.5, 0, 0.5, 0.5))+
  xlab(expression(theta[A])) + ylab(expression(theta[B])) +
ggtitle(expression("Joint Posterior of "*theta[A]*" and "*theta[B])) + 
  theme_classic()
p2 <- posterior_plot(ps, pt = "Example 2 Posterior Distributions from MCMC")
grid.arrange(p1, p2, ncol = 2)
dev.off()

# Which one is better ? 
png("Report/plots/example-2-expected-loss.png", width = 600, height = 300)
plot_loss(ps$loss_theta_a, ps$loss_theta_b)
dev.off()

png("Report/plots/example-2-theta-jags-rope.png", width = 600, height = 300)
rope(ps$theta_a, ps$theta_b)
dev.off()

# end