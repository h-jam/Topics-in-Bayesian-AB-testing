library(rjags)

model = "model
{
    ell[1] ~ dnorm(0, 1)
    ell[2] ~ dt(ell[1], 1/0.1^2, 1)
    for(i in 1:2)
    {
        theta[i] <- 1.0/(1.0 + exp(-ell[i]))
        x[i] ~ dbin(theta[i], N[i])
    }
}
"

bad_model = "model
{
    for(i in 1:2)
    {
        theta[i] ~ dunif(0, 1)
        x[i] ~ dbin(theta[i], N[i])
    }
}
"

# Generate parameters and data. Yes, duplicates code...
gen_pair = function(N=c(10, 10))
{
    x = rep(NA, 2)
    theta = rep(NA, 2)
    ell = rep(NA, 2)
    ell[1] = rnorm(1)
    ell[2] = ell[1] + 0.1*rt(1, df=1)
    for(i in 1:length(N))
    {
        theta[i] = 1.0/(1.0 + exp(-ell[i]))
        x[i] = rbinom(1, size=N[i], prob=theta[i])
    }
    return(list(theta=theta, x=x))
}



# Variables to monitor
variable_names = c("theta")

# How many proper steps?
steps = 100000

# Thinning?
thin = 10

# Import the rjags library
library("rjags")


sample_size = c(500, 500)
sims = 1000
expected_losses = rep(NA, sims)
expected_losses_bad_prior = rep(NA, sims)
for(i in 1:sims)
{
    # A dataset
    N = rep(sample_size, 2)
    pair = gen_pair(N)
    data = list(x=pair$x, N=N)
    true_diff = abs(pair$theta[1] - pair$theta[2])

    # Create a JAGS model object
    jm = jags.model(textConnection(model), data)

    # Do some MCMC
    results = coda.samples(jm, variable_names, steps, thin=thin)

    # Extract chains as data frame
    results = as.data.frame(results[[1]])

    # Difference of thetas
    diff = results$"theta[1]" - results$"theta[2]"

    # Expected loss
    expected_losses[i] = mean(abs(diff - true_diff))

    # Now do the inference with the bad model
    jm = jags.model(textConnection(bad_model), data)
    results = coda.samples(jm, variable_names, steps, thin=thin)
    results = as.data.frame(results[[1]])
    diff = results$"theta[1]" - results$"theta[2]"

    # Expected loss
    expected_losses_bad_prior[i] = mean(abs(diff - true_diff))

    i = i + 1
}

png("Report/plots/example-2-badness.png", height = 300, width = 600)
par(mar = c(3.5,2.5,3.5,1))
hist(expected_losses, breaks = 100, col = rgb(1,0,0,0.5), main = "", xlab = "", 
     border = "white", xlim = c(0, 0.5), freq = F)
hist(expected_losses_bad_prior, breaks = 100, col = rgb(0,0,1,0.5), add =T, 
     border = "white", freq = F)
title(main = "Prior Expectation of Posterior Expected loss N = 500", 
      xlab = "Expected Loss", lin = 1.8)
abline(v = c(mean(expected_losses), mean(expected_losses_bad_prior)), 
       lty = 5, lwd = 2, col = c('red', "blue"))
abline(v = c(median(expected_losses), median(expected_losses_bad_prior)), 
       lwd = 2, lty = 3, col = c('red', "blue"))
legend("topright", c("Good Prior", "Bad Prior", "Mean", "Median"), 
       col = c("red",  "blue", "Black", "Black"), 
       lwd = 3, bty = "n", lty = c(1,1,5,3), seg.len = 4)
dev.off()
