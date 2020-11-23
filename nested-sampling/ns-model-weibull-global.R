names = c("phi_a", "phi_b", "w_a", "w_b", "nu_a", "nu_b")
num_params = 6

us_to_params = function(us){
  
  # Vector to be returned as the result of the function
  params = rep(NA, num_params)
  
  # Apply the names
  names(params) = names

  logit1 = qnorm(us[1])
  logit2 =  logit1 + qt(us[2], df = 1)*0.1
  params["phi_a"] = 1.0/(1.0 + exp(-logit1))
  params["phi_b"] = 1.0/(1.0 + exp(-logit2))
  params["w_a"] = qunif(us[3], min = 0, max = 10)
  params["w_b"] = qunif(us[4], min = 0, max = 10)
  params["nu_a"] = qunif(us[5])
  params["nu_b"] = qunif(us[6])
  
  return(params)
}

log_likelihood <- function(params){
  
  ll <- function(x1, x2, phi, scale, shape){
    
    l1 <- sum(log(phi) + log(shape/scale) + (shape - 1.0)*log(x1/scale) - (x1/scale)^shape)
    l2 <- sum(log(1 - phi*(1-exp(-(x2/scale)^shape))))
    
    logL <- l1 + l2
  
    return(logL)
  }
  
  a <- ll(x1 = converted1, x2 = non_converted1, 
          phi = params['phi_a'], scale = params['w_a'], shape = params['nu_a'])
  b <- ll(x1 = converted2, x2 = non_converted2,
          phi = params['phi_b'], scale = params['w_b'], shape = params['nu_b'])
  
  logL <- a + b 

  if(is.na(logL)){
    logL = -Inf
  }
  
  return(logL)
  
}
