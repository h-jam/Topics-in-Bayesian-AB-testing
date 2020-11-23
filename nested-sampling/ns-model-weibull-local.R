names = c("theta_a", "theta_b", "w_a", "w_b", "nu_a", "nu_b")
num_params = 6

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
  
  params["w_a"] = qunif(us[3], min = 0, max = 20)
  params["w_b"] = qunif(us[4], min = 0, max = 20)
  
  params["nu_a"] = qunif(us[5], min = 0, max = 3)
  params["nu_b"] = qunif(us[6], min = 0, max = 3)
  
  return(params)
}

log_likelihood <- function(params){
  
  ll <- function(x, censored, theta, scale, shape){
    
    log_integral <- log(1 - exp(-(30/scale)^shape))
    times <- log(shape/scale) + (shape - 1.0)*log(x/scale) - (x/scale)^shape
    
    l1 <- sum(log(theta) + times - log_integral)
    l2 <- log(1 - theta)*censored 
    
    logL <- l1 + l2
  
    return(logL)
  }
  
  a <- ll(x = times1, censored = censored1,
          theta = params['theta_a'], scale = params['w_a'], shape = params['nu_a'])
  b <- ll(x = times2, censored = censored2,
          theta = params['theta_b'], scale = params['w_b'], shape = params['nu_b'])
  
  logL <- a + b 

  if(is.na(logL)){
    logL = -Inf
  }
  
  return(logL)
  
}
