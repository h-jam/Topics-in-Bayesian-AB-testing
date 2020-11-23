library(yaml)
library(survival)
source('functions/posterior-plot.R') # plot posterior distributions
source('functions/decision-functions.R')
source('functions/survival-functions.R')

# A data set
data <- yaml.load_file("example-2-first-action/example-2-data.yaml")
df <- data.frame(data$data)

# split into groups for likelihood
group1 <- df[df$group == 1,]
group2 <- df[df$group == 2,]

# split into converted and non converted per group
converted1 <- group1$time_diff[group1$event == 1]
converted2 <- group2$time_diff[group2$event == 1]

non_converted1 <- group1$time_diff[group1$is_censored == 1]
non_converted2<- group2$time_diff[group2$is_censored == 1]

# Weibull Survival Model
# source("nested-sampling/ns-model-weibull-global.R")
# source("nested-sampling/nested-sampling.R") # 6.8min
# save(ns, file = "example-2-first-action/ps-example-2-ns-weibull.Rdata")
load("example-2-first-action/ps-example-2-ns-weibull.Rdata")
ps <- ns$posterior_samples

png("Report/plots/example-2-ps-ns-weibull.png", width = 600, height = 400)
posterior_plot(ps, pt = "Example 2 Global Survival Model Marginal Posterior Distributions from Nested Sampling")
dev.off()

png("Report/plots/example-2-ps-pairs.png", width = 600, height = 400)
pairs(ps[,-7], col = rgb(0.5, 0, 0.5, 0.5), pch = 19, upper.panel = NULL, 
      labels = c(expression(phi[A]), expression(phi[b]), 
                 expression(omega[A]), expression(omega[b]),
                 expression(nu[A]), expression(nu[b])))
title(main = "Example 2 Pairs Plot of Joint Distributions", line =1)
dev.off()

# KM estimate to plot the data
fit <- survfit(Surv(time_diff, event)~group, data = df)

png("Report/plots/example-2-ns-survival.png", width = 600, height = 250)
plot_surv(ps, n = 100)
lines(fit, col = c('red', 'blue'), lwd = 2, lty = 2)
dev.off()

# survival estimates
q_surv(ps, q = 0.25) # lower quartile survival
q_surv(ps, q = 0.5) # median survival

# conversion/cummulative events estimate
predict_t(ps, 30)
predict_t(ps, 60)
predict_t(ps, 100)

p_summary(ps)

# Marginal likelihood: ln(Z) = 343.7612 +- 0.3889589.
# Information: H = 15.12891 nats.
# Effective posterior sample size = 695.
# Posterior samples saved in ns-posterior-samples.csv.
# Time difference of 55.10029 mins

# end