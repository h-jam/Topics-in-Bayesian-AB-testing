names = c("theta_a", "theta_b")
num_params = 2

us_to_params = function(us){
  
  # Vector to be returned as the result of the function
  params = rep(NA, num_params)
  
  # Apply the names
  names(params) = names
  
  logit_thetaa =  qnorm(us[1], mean=0, sd=1) 
  logit_thetab =  logit_thetaa + qt(us[2], df = 1)*0.1
  
  # transform back
  params["theta_a"] = exp(logit_thetaa)/(1 + exp(logit_thetaa))
  params["theta_b"] = exp(logit_thetab)/(1 + exp(logit_thetab))
  
  return(params)
}

log_likelihood <- function(params){
  
  a <- dbinom(x_a, N_a, params["theta_a"], log = T)
  b <- dbinom(x_b, N_b, params["theta_b"], log = T)
  
  logL <- a + b 

  if(is.na(logL)){
    logL = -Inf
  }
  
  return(logL)
  
}
