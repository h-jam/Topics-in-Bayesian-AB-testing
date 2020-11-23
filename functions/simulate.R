simulate<- function(shape, scale, phi, N, day, d ="fixed", seed = NA){
  
  if(!is.na(seed)){
    set.seed(seed)
  }
  
  # Now simulate data
  time <- rweibull(N, shape, scale) + abs(dnorm(N, 0, 0.5))
  time <- ifelse(runif(N) < phi, time, NA)
  
  if(d == "fixed"){
    
    censored <- ifelse(time >= day|is.na(time), 1, 0)
    duration <- ifelse(censored == 1, day, time)

  }else{
    if(d == "var"){
      
      duration <- runif(N, 0, day)
      time <- ifelse(time <= duration, time, NA)
      censored <- ifelse(is.na(time), 1, 0)
      
    }
  }
  
  
  
  print(prop.table(table(censored)))
  
  hist(time, breaks = 5)

  df <- data.frame(cbind(time, duration, censored))
  
  return(df)
}

# example 1 - 
x1 <- simulate(2, 50, 0.05, 1000, day = 60, d = 'var')
x1 <- simulate(1.5, 20, 0.05, 1000, day = 60, d = 'var')

