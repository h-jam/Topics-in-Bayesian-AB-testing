# source dependencies
require(dplyr)
require(ggplot2)
require(gridExtra)
require(grid)

# function to plot posterior samples
posterior_plot <- function(df, pt = "Posterior Plot"){
  
  th <- theme_classic() +     
    theme(legend.position = "top", legend.direction = "horizontal")
  
  if(length(df$theta_a) > 0){
  p <- df %>% 
    ggplot() + th+ 
    labs(x = expression(Conversion~Rate~theta)) +
    geom_histogram(aes(x = theta_a, fill = "theta_a"), alpha = 0.3) + 
    geom_histogram(aes(x = theta_b, fill = "theta_b"), alpha = 0.3) +
    scale_fill_manual(name = "Group",
                       values = c( "theta_a" = "red", "theta_b" = "Blue"),
                       labels = c("Group A", "Group B")) + 
    scale_y_continuous(expand = c(0,0))
  }
  
  if(length(df$psi_a) > 0){
    
    psi_ <- df %>% 
      ggplot() + th+ 
      labs(x = expression(Conversion~Rate~psi)) +
      geom_histogram(aes(x = psi_a, fill = 'psi_a'), alpha = 0.3) + 
      geom_histogram(aes(x = psi_b, fill = 'psi_b'), alpha = 0.3) + 
      scale_fill_manual(name = "Group",
                        values = c( "psi_a" = "red", "psi_b" = "Blue"),
                        labels = c("Group A", "Group B")) + 
      scale_y_continuous(expand = c(0,0))
    
  }
  
  if(length(df$nu_a) > 0 ){
    nu <- df %>% 
      ggplot() + th + 
      labs(x = expression(Shape~nu))+
      geom_histogram(aes(x = nu_a, fill = 'nu_a'), alpha = 0.3) + 
      geom_histogram(aes(x = nu_b, fill = "nu_b"), alpha = 0.3) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_manual(name = "Group",
                        values = c( "nu_a" = "red", "nu_b" = "Blue"),
                        labels = c("Group A", "Group B"))
    
    w <- df %>% 
      ggplot() + th+ labs(x = expression(Scale~omega))+
      geom_histogram(aes(x = w_a, fill = 'w_a'), alpha = 0.3) + 
      geom_histogram(aes(x = w_b, fill = "w_b"), alpha = 0.3) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_manual(name = "Group",
                        values = c( "w_a" = "red", "w_b" = "Blue"),
                        labels = c("Group A", "Group B"))
  }
    
  if(length(df$phi_a) > 0){
      phi <- df %>% 
        ggplot() + th+ labs(x = expression(Conversion~Rate~phi))+
        geom_histogram(aes(x = phi_a, fill = 'phi_a'), alpha = 0.3) + 
        geom_histogram(aes(x = phi_b, fill = 'phi_b'), alpha = 0.3) +
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_manual(name = "Group",
                          values = c( "phi_a" = "red", "phi_b" = "Blue"),
                          labels = c("Group A", "Group B"))
    }
      
  
  y <- c(exists("p"), exists("psi_"), exists("phi"), exists("nu"), exists("w"))
  x <- list()
  if(y[1] == T){x[[5]] <- p}
  if(y[2] == T){x[[4]] <- psi_}  
  if(y[3] == T){x[[3]] <- phi} 
  if(y[4] == T){x[[2]] <- nu} 
  if(y[5] == T){x[[1]] <- w} 
  
  z <- -which(sapply(x, is.null))
  if(length(z) > 0){
    x <- x[z]
  }
  x <- rev(x)
  nc <- ifelse(length(x) > 1, 2, 1)
  suppressMessages(do.call(grid.arrange, c(x, list(top = pt, ncol = nc))))

}