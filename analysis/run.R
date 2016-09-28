run <- function (pars) {
  
  # The true correct outcome of this decision
  y_true <- pars[["p"]] > 0.5;
  
  # The distribution of ppoints among voters 
  # p_ratio <- rpareto(pars[["N"]], pars[["alpha"]], pars[["scale"]])
  p_ratio <- pars[["Ptot"]]/pars[["N"]]*rep(1, pars[["N"]])
  
  pps <- p_ratio/sum(p_ratio)*pars[["Ptot"]]
  
  # The votes of all voters
  votes_base <- runif(pars[["N"]],0,1)
  votes <- votes_base < pars[["p"]]
  
  # simulate each vote and store the evolution
  # of key variables
  rr <- vector("numeric",pars[["N"]])
  pc <- vector("numeric",pars[["N"]])
  cc <- vector("numeric",pars[["N"]])
  
  vv1 <- vector("numeric",pars[["N"]])
  ss1 <- vector("numeric",pars[["N"]])
  pc_rest_flip <- vector("numeric",pars[["N"]])
  pc_ci_low <- vector("numeric",pars[["N"]])
  pc_ci_high <- vector("numeric",pars[["N"]])
  pc_ci_low_ext <- vector("numeric",pars[["N"]])
  pc_ci_high_ext <- vector("numeric",pars[["N"]])
  
  vv2 <- vector("numeric",pars[["N"]])
  ss2 <- vector("numeric",pars[["N"]])
  vark <- vector("numeric",pars[["N"]])
  
  for (ix in 1:pars[["N"]]) {
    
    # decision complexity
    dec_pars <- list()
    dec_pars[["votes"]] <- votes[1:ix]
    dec_pars[["pps"]] <- pps[1:ix]
    dec_pars[["pps_tot"]] <- sum(pps)
    
    comp_data <- decision_complexity(dec_pars)
    
    rr[ix] <- comp_data[["rr"]]
    pc[ix] <- comp_data[["pc"]]
    cc[ix] <- comp_data[["cc"]]
    
    # verdict
    dec_pars[["rr"]] <- rr[ix]
    dec_pars[["pc"]] <- pc[ix]
    dec_pars[["cc"]] <- cc[ix]
    dec_pars[["N"]] <- pars[["N"]]
    dec_pars[["stab_ratio"]] <- pars[["stab_ratio"]]
    dec_pars[["ci"]] <- pars[["ci"]]
    verdict_data <- verdict(dec_pars)
    
    vv1[ix] <- verdict_data[["vv1"]]
    ss1[ix] <- verdict_data[["ss1"]]
    
    pc_rest_flip[ix] <-  verdict_data[["pc_rest_flip"]]
    pc_ci_low[ix] <- verdict_data[["pc_ci_low"]]
    pc_ci_high[ix] <- verdict_data[["pc_ci_high"]]
    pc_ci_low_ext[ix] <- verdict_data[["pc_ci_low_ext"]]
    pc_ci_high_ext[ix] <- verdict_data[["pc_ci_high_ext"]]
    
    vv2[ix] <- verdict_data[["vv2"]]
    ss2[ix] <- verdict_data[["ss2"]]
    vark[ix] <- verdict_data[["vark"]]
  }
  
  res <- list()
  res[["y_true"]] <- y_true
  res[["votes"]] <- votes
  res[["rr"]] <- rr
  res[["pc"]] <- pc
  res[["cc"]] <- cc
  
  res[["vv1"]] <- vv1
  res[["ss1"]] <- ss1
  res[["pc_rest_flip"]] <- pc_rest_flip
  res[["pc_ci_low"]] <- pc_ci_low
  res[["pc_ci_high"]] <- pc_ci_high
  res[["pc_ci_low_ext"]] <- pc_ci_low_ext
  res[["pc_ci_high_ext"]] <- pc_ci_high_ext
  
  res[["vv2"]] <- vv2
  res[["ss2"]] <- ss2
  res[["vark"]] <- vark
  
  return(res)
}