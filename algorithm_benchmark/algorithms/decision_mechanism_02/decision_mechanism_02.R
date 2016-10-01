decision_mechanism_02 <- function (inputs) {
  
  debugSource("algorithms/decision_mechanism_02/compute_pkj.R")
  debugSource("algorithms/decision_mechanism_02/compute_post.R")
  
  votes <- inputs[["votes"]]
  pps <- inputs[["pps"]]
  pps_tot <- inputs[["pps_tot"]]
  n <- inputs[["n_votes_tot"]]
  
  # paramaters
  eps <- 0.05
  l <- 0.5
  a <- 1
  b <- 1
  
  s <- sum(votes*pps)
  n <- pps_tot
  k <- sum(pps)
  
  pkj <- compute_pkj(n,k,s,l,a,b)
  p_est <- s/k

  if(p_est >= l) {
    verdict_estimated <- TRUE
  } else {
    verdict_estimated <- FALSE
  }
    
  if((pkj <= eps) | (pkj >= (1-eps))) {
    verdict_reached <- TRUE
  } else{
    verdict_reached <- FALSE
  }
    
  
  # process results to fit output interface
  output <- list()
  output[["verdict_reached"]] <- verdict_reached
  output[["verdict_estimated"]] <- verdict_estimated 
  
  debug_data <- list()
  debug_data[["pkj"]] <- pkj
  
  output[["debug_data"]] <- debug_data
  
  return(output)
  
}