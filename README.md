# Topics-in-Bayesian-AB-testing
Priors, Binomial and Survival models for Bayiesan AB Testing using JAGS and Nested sampling in R.

## About 
Bayesian AB testing is becoming a popular alternative to Frequentist methods and
p-values. Current methods of Bayesian AB testing in industry underutilise the ability to
fully model prior distributions and likelihoods to the experiment problem. This repository we
use a dependent prior for conversion parameters between treatment groups
and develop likelihoods to model time to event outcomes. Additionally, we demonstrate
how using the prior expectation of the posterior expected loss can be used to estimate
the “cost” of picking a poor prior distribution in small sample sizes.

### JAGS 

JAGS[http://mcmc-jags.sourceforge.net] is Just Another Gibbs Sampler.  It is a program for analysis of Bayesian models using Markov Chain Monte Carlo (MCMC). 

You can download JAGS here[https://sourceforge.net/projects/mcmc-jags/files] and use in in R with the Rjags[https://cran.r-project.org/web/packages/rjags/index.html] package.

### Nested Sampling

Nested sampling by physicist John Skilling[[1]](#1) is another algorithm which we can use for analysis of Bayesian models. The actual goal of nested sampling is the marginal likelihood or evidence. 

We provide code to run nested sampling courtesey of Brendon J. Brewer[https://github.com/eggplantbren/NSwMCMC] [[2]](#1)

## Notebooks
Clone the repo and follow the tutorials in the notebook folder. Everything you need to try out Bayesian AB testing! 

## Download the Code
Clone the repo and run the code in each example folder for youself. Try your own datatsets! 

## Read the Report
Read the full report for the project in the pdf. 

## References 

<a id="1">[1]</a> 
Bayesian Inference and Computation: A Beginner’s Guide
Brendon J. Brewer, in Bayesian Astrophysics 26, 1, Cambridge University Press

## Acknowledgements

I would like to thank Brendon J. Brewer for his contributions to this project. 

## Disclaimer 

This repo is a public version of the research project I did as fulfillemnt of my Master of Science in Statistics. This is not a package, rather a collection of code to run the methods. I hope to maintain an improve the code overtime, but I'm not a software developer. All examples are compltely made up, but the data reflects experience with AB testing in industry --so should be somewhat useful. All work is entirely my own in conjucntion with the University of Auckland, and in no way represents the data, interests or opinions of my employers past and present. 
