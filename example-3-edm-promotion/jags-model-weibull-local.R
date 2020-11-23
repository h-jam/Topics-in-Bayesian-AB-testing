library(rjags)
library(yaml)
library(survival)

source('functions/jags-functions.R') # run jags per model
source('functions/posterior-plot.R') # plot posterior distributions
source('functions/survival-functions.R') # plot posterior survival 
source('functions/decision-functions.R')

# A data set
data <- yaml.load_file("example-3-edm-promotion/example-3-data.yaml")
params <- data$params
df <- data.frame(data$data)

# only include converters (non censored data) for model 1
converters = df[df$censored == 0,]
data = list(
  t = converters$time_diff, # observed time of event
  group =  converters$group,
  t_star = 30, # total time in experiment
  x =  table(df$censored, df$group)[1,], # conversions in each group
  N =  table(df$group) # sample in each group
)

model = "model {

  # Priors
  logits[1] ~ dnorm(0, 1)
  logits[2] ~ dt(logits[1], 1/0.1^2, 1)
  
  # Two groups
  for(i in 1:2){
  
    # Transform from logit to success prob.
    theta[i] <- 1.0/(1.0 + exp(-logits[i]))
    
    w[i] ~ dunif(0, 20)
    nu[i] ~ dunif(0, 3)
    
    rate[i] <- (1/w[i])^nu[i]
    
    # non conversion
    phi[i] <- theta[i]/(1-exp(-(t_star/w[i])^nu[i]))
    x[i] ~ dbin(phi[i], N[i])
    
  }

  for(i in 1:length(t)){
      t[i] ~ dweib(nu[group[i]], rate[group[i]])T(0, t_star)
  }
  
}
"

variable_names = c("theta", "w", "nu", "phi")
# jm <-  do_jags(data, variable_names, model = model)
save(jm, file = "example-3-edm-promotion/ps-example-3-jags-weibull.Rdata")
ps <- jm$results

posterior_plot(ps, pt = "Example 3 Posterior Distributions for Survival Model MCMC")
plot(jm$out)
apply(jm$results, 2, mean)

plot_surv(ps, model = "local")
# KM estimate of data for plotting
fit <- survfit(Surv(time_diff, event)~group, data = df)
lines(fit, col = c('blue', 'red'))


