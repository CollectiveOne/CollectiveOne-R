decision_stability <- function (inputs,clarity_result,pars) {
  
  votes <- inputs[["votes"]]
  pps <- inputs[["pps"]]
  n_votes_now <- length(votes)
  
  # compares two estimates of p from two batches of votes
  if(n_votes_now >= 4) {
    ix_stab <- round(n_votes_now*(1-pars[["stab_ratio"]]))
    p_est_base <- 1/sum(pps[1:ix_stab])*sum(votes[1:ix_stab]*pps[1:ix_stab])
    p_est_check <- 1/sum(pps[-(1:ix_stab)])*sum(votes[-(1:ix_stab)]*pps[-(1:ix_stab)])
    
    trend <- p_est_check - p_est_base
    
  } else {
    trend <- 0
  }
  
  stab_data <- list()
  stab_data[["trend"]] <- trend
  
  return(stab_data)
  
}