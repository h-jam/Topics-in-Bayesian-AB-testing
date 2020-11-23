library(yaml)
library(survival)
source('functions/posterior-plot.R') # plot posterior distributions
source('functions/decision-functions.R')
source('functions/survival-functions.R')

# A data set
data <- yaml.load_file("example-3-edm-promotion/example-3-data.yaml")
df <- data.frame(data$data)

# split into groups for likelihood
group1 <- df[df$group == 1,]
group2 <- df[df$group == 2,]

# Weibull Survival Model
times1 <- group1$time_diff[group1$event == 1]
times2 <- group2$time_diff[group2$event == 1]
censored1 <- sum(group1$censored)
censored2 <- sum(group2$censored)

# source("nested-sampling/ns-model-weibull-local.R")
# source("nested-sampling/nested-sampling.R") # 6.8min
# save(ns, file = "example-3-edm-promotion/ps-example-3-ns-weibull.Rdata")
load("example-3-edm-promotion/ps-example-3-ns-weibull.Rdata")
ps <- ns$posterior_samples

# Marginal likelihood: ln(Z) = -3162.171 +- 0.4094842.
# Information: H = 16.76773 nats.
# Effective posterior sample size = 755.
# Posterior samples saved in ns-posterior-samples.csv.
# Time difference of 5.071825 mins

png("Report/plots/example-3-ps-ns-weibull.png", width = 600, height = 600)
posterior_plot(ps, pt = "Example 3 Local Survival Model Marginal Posterior Distributions from Nested Sampling")
dev.off()

png("Report/plots/example-3-ps-pairs.png", width = 600, height = 400)
pairs(ps[,-7], col = rgb(0.5, 0, 0.5, 0.5), pch = 19, upper.panel = NULL, 
      labels = c(expression(theta[A]), expression(theta[b]), 
                 expression(omega[A]), expression(omega[b]),
                 expression(nu[A]), expression(nu[b])))
title(main = "Example 3 Pairs Plot of Joint Distributions", line =1)
dev.off()

png("Report/plots/example-3-density.png", width = 600, height = 250)
par(mfrow = c(1,2), mar  = c(3,3,2,1))
plot_density(ps, model = 'local')
plot_hazard(ps)
dev.off()

# KM estimate to plot the data
fit <- survfit(Surv(time_diff, event)~group, data = df[df$event == 1,])

png("Report/plots/example-3-ns-survival.png", width = 600, height = 250)
plot_surv(ps, model = 'local')
lines(fit, col = c('red', 'blue'), lwd = 2, lty = 2)
dev.off()

p_summary(ps)

q_surv(ps, q = 0.25) # lower quartile survival
q_surv(ps, q = 0.5) # median survival

# end