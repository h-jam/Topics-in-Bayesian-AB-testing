
do_jags <- function(data, var_names, priors = NA, likelihood = NA, model = NA){
  
  if(!is.na(priors)){
    model = paste("model {", priors, likelihood, "}")
  }
  
  jm = jags.model(textConnection(model), data)
  out = coda.samples(jm, variable.names = var_names, n.iter = 100000, thin=10)
  results = as.data.frame(out[[1]])
    
  names <- paste0(rep(sort(var_names), each = 2), c("_a","_b"))
  colnames(results) <- names
  results <- results[names]
  
  return(list(results = results, out = out))
}