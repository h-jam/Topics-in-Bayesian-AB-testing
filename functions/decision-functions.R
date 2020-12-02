library(HDInterval)

# Highest density interval
hdi_ab <- function(posteriorA, posteriorB, cr = 0.95, xl = NULL){
  
  d <- density(posteriorB - posteriorA)
  
  if(is.null(xl)){
    xl = "posteriorB - posteriorA"
  }
  
  par(mar = c(3,3,2,0.5))
  
  t <- paste0(cr*100,"% HDI")
  hist(posteriorB - posteriorA, freq = F, col = rgb(0.53, 0.81, 0.98, 0.4), border = "white",
       main = "", xlab = "")
  title(main = t, line = 1)
  title(xlab = xl, line = 2)
  lines(d, lwd = 3, col = 'skyblue')
  
  hdiD1 <- hdi(d, credMass = cr)
  ht <- attr(hdiD1, "height")
  segments(hdiD1[1], ht, hdiD1[2], ht, col='mediumblue', lwd = 3)
  points(x = c(hdiD1[1], hdiD1[2]), y = c(ht, ht), col = "red", pch = 19)
  abline(h = ht, lty = 3, col = 'mediumblue')
  
  text(median(d$x), ht-5, labels = paste("k = ", round(ht,2)), col = "blue")
  text(hdiD1[1], ht+5, labels = paste(round(hdiD1[1],3)), col = "red")
  text(hdiD1[2], ht+5, labels = paste(round(hdiD1[2],3)), col = "red")
  
  return(hdiD1)
  
}

rope <- function(posteriorA, posteriorB, cr = 0.95, rope = 0.01, xl = NULL){
  
  hdi_ab(posteriorA, posteriorB, cr, xl)
  abline(v = c(-rope, rope), lty = 2, lwd = 2, col = "darkgreen")
  abline(v = 0, lwd = 5, col = rgb(0,1,0,0.5))
  
}

library(pracma)

# expected loss
plot_loss <- function(lossA, lossB, pt = "Expected Loss"){
  
  par(mar = c(3,3,2,1))
  
  lossA_ <- lossA[lossA > 0]
  lossB_ <- lossB[lossB > 0]
  
  la <- mean(lossA)
  lb <- mean(lossB)
  
  h <- hist(c(lossB_, -lossA_), freq = F, breaks = 100)
  clr <- ifelse(h$breaks > 0, "blue3", "red3")[-length(h$breaks)]
  plot(h, col = clr, border = "white", main = "")
  title(main = pt, line = 1)
  legend("topright", legend = c(paste("L(A) = ", round(la,4)), 
                                paste("L(B) = ", round(lb,4))), 
         col = c('red3', "blue3"), lty = c(1,1),
         lwd = 5, bty = "n")

  
  #return(list(loss.a = la, loss.b = lb))
  
}

# probability to be the best
ptbb <- function(posteriorA, posteriorB){
  
  n <- length(posteriorA)
  
  ptbb_a <- round(sum(posteriorA - posteriorB > 0)/n, 3)
  ptbb_b <- round(sum(posteriorB - posteriorA > 0)/n, 3)
  
  return(list(ptbb_a = ptbb_a, ptbb_b = ptbb_b))
  
}

p_summary <- function(df){
  
  s <- function(x){
    c(round(mean(x),5), round(sd(x),5))
  }
  
  a <- apply(df, 2, s)
  
  for(i in 1:ncol(a)){
    cat(colnames(a)[i], a[1,i], "+-", a[2,i], "\n")
  }
  
}
