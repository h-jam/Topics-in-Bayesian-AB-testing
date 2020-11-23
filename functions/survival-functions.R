plot_surv <- function(df, n = 100, tn = "", model = 'global'){
  
  par(mar  = c(3,3,2,1))
  
  if(tn == ""){
    tn = "Posterior Predictive Survival from Posterior Samples"
  }
  
  # set up plot
  t <- 0:300*0.1
  plot(t, 0:300/(300), type = "n", main = tn, xlab = "", ylab = "")
  
  if(model == "local"){
    
    for(i in sample(nrow(df), n)){
      a <- pweibull(t, shape = df$nu_a[i], scale = df$w_a[i], lower.tail = F)
      b <- pweibull(t, shape = df$nu_b[i], scale = df$w_b[i], lower.tail = F)
      
      lines(t, a, col=rgb(1,0,0,0.05))
      lines(t, b, col=rgb(0,0,1,0.05))
    }
    
  }else{
    
    for(i in sample(nrow(df), n)){
      
      a <- 1 - df$phi_a[i]*(1- exp(-(t/(df$w_a[i]))^df$nu_a[i]))
      b <- 1 - df$phi_b[i]*(1 - exp(-(t/(df$w_b[i]))^df$nu_b[i]))
      
      lines(t, a, col=rgb(1,0,0,0.05))
      lines(t, b, col=rgb(0,0,1,0.05))
    }

    
  }
  
  legend("topright", c("Group A", "Group B"), col = c("red", "blue"), lwd = 2,
         bty = "n")
  title(xlab = "Time (Days)", ylab = "Survival Probability", line = 2)
  
}

plot_surv_2 <- function(df, model = "global"){
  
  par(mar  = c(3,3,2,1))
  
  # set up plot
  t <- 0:300*0.1
  plot(t, 0:300/(300), type = "n", main = "95% CI for Posterior Predictive Survival", 
       xlab = "", ylab = "")
  
  upper <- 0.975
  lower <- 0.025
  
  p <- c(lower, 0.5, upper)
  
  shape_a <- quantile(df$nu_a, probs = p)
  scale_a <- quantile(df$w_a, probs = p)
  
  shape_b <- quantile(df$nu_b, probs = p)
  scale_b <- quantile(df$w_b, probs = p)
  
  if(model == 'local'){
  
    for(i in 1:3){
      
      a <- pweibull(t, shape = shape_a[i], scale = scale_a[i], lower.tail = F)
      b <- pweibull(t, shape = shape_b[i], scale = scale_b[i], lower.tail = F)
      
      lines(t, a, col=rgb(1,0,0,0.7), lty = c(2,1,2)[i], lwd = 2)
      lines(t, b, col=rgb(0,0,1,0.7), lty = c(2,1,2)[i], lwd = 2)
      
    }
    
  }else{
    
    phi_a <- quantile(df$phi_a, probs = p)
    phi_b <- quantile(df$phi_b, probs = p)
    
    for(i in 1:3){
      
      a <- 1 - phi_a[i]*(1- exp(-(t/(scale_a[i]))^shape_a[i]))
      b <- 1 - phi_b[i]*(1 -exp(-(t/(scale_b[i]))^shape_b[i]))
      
      lines(t, a, col=rgb(1,0,0,0.7), lty = c(2,1,2)[i], lwd = 2)
      lines(t, b, col=rgb(0,0,1,0.7), lty = c(2,1,2)[i], lwd = 2)
    }
    
    
  }
  
  legend("topright", c("Group A", "Group B"), col = c("red", "blue"), lwd = 2,
         bty = "n")
  title(xlab = "Time (Days)", ylab = "Survival Probability", line = 2)
  
}

q_surv <- function(df, q = 0.5){
  
  med_a <- rep(0, nrow(df))
  med_b <- rep(0, nrow(df))
  
  for(i in 1:nrow(df)){
    med_a[i] <- qweibull(q, df$nu_a[i], df$w_a[i])
    med_b[i] <- qweibull(q, df$nu_b[i], df$w_b[i])
  }
  
  p <- c(0.025, 0.5, 0.975)
  t <- list(group_a = quantile(med_a, p), group_b = quantile(med_b, p))
  
  return(t)
}


plot_hazard <- function(df, n = 100, tn = ""){
  
  # set up plot
  t <- 0:300*0.1
  plot(t, 0:300/(600), type = "n", main = "")
  title(main = "Hazard Function from Posterior Samples", line = 1)
  title(xlab = "Time (Days)", ylab = "Hazard", line = 2)
  
  for(i in sample(nrow(df), n)){
    a <- dweibull(t, df$nu_a[i], df$w_a[i])/pweibull(t, df$nu_a[i], df$w_a[i], lower.tail = F)
    b <- dweibull(t, df$nu_b[i], df$w_b[i])/pweibull(t, df$nu_b[i], df$w_b[i], lower.tail = F)
    
    lines(t, a, col=rgb(0,0,1,0.05))
    lines(t, b, col=rgb(1,0,0,0.05))
  }
  
}
  
  
plot_density <- function(df, n = 100, tn = "", model = "global"){
    
    if(model == "local"){
      
      t <- 0:300*0.1
      plot(t, 0:300/(3000), type = "n", main = "")
      
      for(i in sample(nrow(df), n)){
        a <- dweibull(t, shape = df$nu_a[i], scale = df$w_a[i])
        b <- dweibull(t, shape = df$nu_b[i], scale = df$w_b[i])
        
        lines(t, a, col=rgb(1,0,0,0.05))
        lines(t, b, col=rgb(0,0,1,0.05))
        
      }
        
      }else{
        
        t <- 0:300*0.1
        plot(t, 0:300/(300), type = "n", main = "")
        
        for(i in sample(nrow(df), n)){
          
          a <- dweibull(t, shape = df$nu_a[i], scale = df$w_a[i])*df$phi_a[i]
          b <- dweibull(t, shape = df$nu_b[i], scale = df$w_b[i])*df$phi_b[i]

          
          lines(t, a, col=rgb(1,0,0,0.05))
          lines(t, b, col=rgb(0,0,1,0.05))
      }
      
      }
    
    title(main = "Density Function from Posterior Samples", line = 1)
    title(xlab = "Time (Days)", ylab = "Probability Density", line = 2)

    legend("topright", c("Group A", "Group B"), col = c("red", "blue"), lwd = 2,
           bty = "n")
    
}

predict_t <- function(df, t){
  
  a <- rep(0, nrow(df))
  b <- rep(0, nrow(df))
  
  for(i in 1:nrow(df)){
    a[i] <- (1- exp(-(t/(df$w_a[i]))^df$nu_a[i]))*df$phi_a[i]
    b[i] <- (1 - exp(-(t/(df$w_b[i]))^df$nu_b[i]))*df$phi_b[i]
  }
  
  p <- c(0.025, 0.5, 0.975)
  t <- list(group_a = list(q = quantile(a, p), mean = c(mean = mean(a), sd = sd(a))), 
            group_b = list(q = quantile(b, p), mean = c(mean = mean(b), sd = sd(b))))
  
  return(t)
  
}
