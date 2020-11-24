# Topics-in-Bayesian-AB-testing
Priors, Binomial and Survival models for Bayesian AB Testing using JAGS and nested sampling in R.

## About 
Bayesian AB testing is becoming a popular alternative to Frequentest methods and p-values. Current methods of Bayesian AB testing in industry under utilise the ability to fully model prior distributions and likelihoods to the experiment problem. This repository we use a dependent prior for conversion parameters between treatment groups and develop likelihoods to model time to event outcomes. Additionally, we demonstrate how using the prior expectation of the posterior expected loss can be used to estimate the “cost” of picking a poor prior distribution in small sample sizes.

We bypass analytic methods and use two simulation methods to perform our Bayesian analysis: 

### JAGS 

[JAGS](http://mcmc-jags.sourceforge.net) is Just Another Gibbs Sampler.  It is a program for analysis of Bayesian models using Markov Chain Monte Carlo (MCMC). 

You can download JAGS [here](https://sourceforge.net/projects/mcmc-jags/files) and use in in R with the [Rjags](https://cran.r-project.org/web/packages/rjags/index.html) package.

### Nested Sampling

Nested sampling by physicist John Skilling[[1]](#1) is another algorithm which we can use for analysis of Bayesian models. The actual goal of nested sampling is the marginal likelihood or evidence. 

We provide code to run nested sampling courtesy of [Brendon J. Brewer](https://github.com/eggplantbren/NSwMCMC) [[2]](#1)

## How to use this repo

### Notebooks
Clone the repo and follow the tutorials in the notebook folder. Everything you need to try out Bayesian AB testing! 

### Download the Code
Clone the repo and run the code in each example folder for yourself. Try your own datatsets! 

### Read the Report
Read the full report for the project in the pdf. 


## Acknowledgements

I would like to thank Brendon J. Brewer for his contributions to this project. 

## Disclaimer 

This repo is a public version of the research project I did as fulfillment of my Master of Science in Statistics. This is not a package, rather a collection of code to run the methods. I hope to maintain an improve the code overtime, but I'm not a software developer. All examples are completely made up, but the data reflects experience with AB testing in industry -so should be somewhat useful. All work is entirely my own in conjunction with the University of Auckland, and in no way represents the data, interests or opinions of my employers past and present. 

## References
<a id="1">[1]</a> 
[Skilling, J. (2004, November). Nested sampling. In AIP Conference Proceedings (Vol. 735, No. 1, pp. 395-405). American Institute of Physics.](https://projecteuclid.org/euclid.ba/1340370944)

<a id="2">[2]</a> 
[Bayesian Inference and Computation: A Beginner’s Guide. Brendon J. Brewer, in Bayesian Astrophysics 26, 1, Cambridge University Press](https://odysee.com/@BrendonBrewer:3/wsbook:f)

[Kruschke, J. K. (2013). Bayesian estimation supersedes the t test. Journal of Experimental Psychology: General, 142(2), 573.](https://mran.microsoft.com/snapshot/2017-04-30/web/packages/BEST/vignettes/BEST.pdf)

[O'Hagan, A., & Forster, J. J. (2004). Kendall's advanced theory of statistics, volume 2B: Bayesian inference (Vol. 2). Arnold.](https://www.amazon.com/Advanced-Theory-Statistics-Vol-Inference/dp/0340807520/)

[Stucchio, C. (2015). Bayesian A/B Testing at VWO. Whitepaper, Visual Website Optimizer.](https://cdn2.hubspot.net/hubfs/310840/VWO_SmartStats_technical_whitepaper.pdf)



