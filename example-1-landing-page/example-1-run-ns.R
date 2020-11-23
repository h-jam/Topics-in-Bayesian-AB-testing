library(yaml)
source("nested-sampling/utils.R") # ns functions
source('functions/posterior-plot.R') # plot posterior distributions
source('functions/decision-functions.R')

# A data set
data <- yaml.load_file("example-1-landing-page/example-1-data.yaml")
df <- data.frame(data)

# Binomial Model
x_a <- df$x1[1]
x_b <- df$x1[2]
N_a <- df$N[1]
N_b <- df$N[2]
source("nested-sampling/ns-model-binomial.R")
source("nested-sampling/nested-sampling.R") # 6.8min
save(ps, file = "example-1-landing-page/ps-example-1-ns-binomial.Rdata")
#load(file = "example-1-first-action/ps-example-1-ns-binomial.Rdata")
ps <- data.frame(posterior_samples)

jpeg("Report/plots/example-2-ps-ns-binomial.jpg", width = 600, height = 300)
posterior_plot(ps, pt = "Example 1 Binomial Model Posterior Distributions from Nested Sampling")
dev.off()

# end