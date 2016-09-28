decision_stability <- function (pars) {
  votes <- pars[["votes"]]
  pps <- pars[["pps"]]
  n <- length(votes)
  
  # based on stability of p over two batches of votes
  if(n >= 2) {
    ix_stab <- round(n*(1-pars[["stab_ratio"]]))
    
    dec_pars <- list()
    dec_pars[["votes"]] <- votes[1:ix_stab]
    dec_pars[["pps"]] <- pps[1:ix_stab]
    res_comp_base <- decision_complexity(dec_pars)
    
    dec_pars <- list()
    dec_pars[["votes"]] <- votes[-(1:ix_stab)]
    dec_pars[["pps"]] <- pps[-(1:ix_stab)]
    res_comp_check <- decision_complexity(dec_pars)
    
    ss1 <- res_comp_check[["pc"]] - res_comp_base[["pc"]]
  } else {
    ss1 <- 1
  }
  
  # based on stability of p over two batches of votes
  rrkm1 <- 1/(n-1)*sum(votes[1:n-1])
  rrk <- 1/n*sum(votes)
  
  varkm1 <- rrkm1*(1-rrkm1)/(n-1)
  vark <- rrk*(1-rrk)/n
  
  ss2 <- abs(vark - varkm1)
  
  # store data
  stab_data <- list()
  stab_data[["ss1"]] <- ss1
  stab_data[["ss2"]] <- ss2
  stab_data[["vark"]] <- vark
  
  return(stab_data)
  
}