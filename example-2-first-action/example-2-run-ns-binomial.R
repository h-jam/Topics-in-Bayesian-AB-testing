setwd("~/Topics-in-Bayesian-Statistics/")

library(yaml)
source('functions/posterior-plot.R') # plot posterior distributions
source('functions/decision-functions.R')
source('functions/survival-functions.R')

# A data set
data <- yaml.load_file("example-2-first-action/example-2-data.yaml")
df <- data.frame(data$data)

# split into groups for likelihood
group1 <- df[df$group == 1,]
group2 <- df[df$group == 2,]

# Binomial Model
x_a <- sum(group1$event)
x_b <- sum(group2$event)
N_a <- nrow(group1)
N_b <- nrow(group2)
source("nested-sampling/ns-model-binomial.R")
source("nested-sampling/nested-sampling.R") # 6.8min
save(ns, file = "example-2-first-action/ps-example-2-ns-binomial.Rdata")
load(file = "example-2-first-action/ps-example-2-ns-binomial.Rdata")
ps <- ns$posterior_samples

jpeg("Report/plots/example-2-ps-ns-binomial.jpg", width = 600, height = 300)
posterior_plot(ps, pt = "Example 2 Binomial Model Posterior Distributions from Nested Sampling")
dev.off()

# Weibull Survival Model
source("nested-sampling/ns-model-weibull-global.R")
source("nested-sampling/nested-sampling.R") # 6.8min
save(ns, file = "example-2-first-action/ps-example-2-ns-weibull.Rdata")
load("example-2-first-action/ps-example-2-ns-weibull.Rdata")
ps <- ns$posterior_samples

jpeg("Report/plots/example-2-ps-ns-weibull.jpg", width = 600, height = 300)
posterior_plot(ps, pt = "Example 2 Global Survival Model Posterior Distributions from Nested Sampling")
dev.off()

pairs(ps)

p_summary(ps)

# end