verdict <- function (pars) {
  votes <- pars[["votes"]]
  pps <- pars[["pps"]]
  cc <- pars[["cc"]]
  pps_tot <- pars[["pps_tot"]]
  N_tot <- pars[["N"]]
  
  n <- length(votes)
  pps_cum <- sum(pps)
  
  
  # verdict based on maximum likelihood
  
  p0 <- 0.5*(1+pps/pps_cum)
  p <- 0.5 + cc*(p0-0.5)
  
  # likelihood of reject and accept
  ix_rej <- which(votes %in% 0)
  ix_acc <- which(votes %in% 1)
  
  l0 <- prod(p[ix_rej])*prod(1-p[ix_acc])
  l1 <- prod(1-p[ix_rej])*prod(p[ix_acc])
  
  # find the verdict by chosing the one with larger 
  # likelihood
  if(l1 > l0) {
    vv1 <- 1
  } else {
    vv1 <- 0
  }
  
  # verdict based on symmetric vote weight
  if(pars[["rr"]] > 0.5) {
    vv2 <- 1
  } else {
    vv2 <- 0
  }
  
  
  # determine voting stability
  stab_data <- decision_stability(pars)
  ss1 <- stab_data[["ss1"]]
  ss2 <- stab_data[["ss2"]]
  vark <- stab_data[["vark"]]
  
  # determine fliping probabilty 
  pc <- pars[["pc"]]
  pps_left <- pars[["pps_tot"]] - pps_cum;
  N_left <- N_tot - n
  
  if(abs(pps_left) > 1) {
    pc_rest_flip <- (0.5 - pc*pps_cum/pps_tot)*pps_tot/pps_left
  } else {
    pc_rest_flip <- -0.1
  }
    
  
  # mark unfeasible cases with +1.5 and -0.5
  if(pc_rest_flip > 1) {
    pc_rest_flip = 1.1
  }
  
  if(pc_rest_flip < 0) {
    pc_rest_flip = -0.1
  }
    
    
  # confidence interval for pc if the rest of voters have pc as mean vote
  # TODO: not fully consistent as this assumes  they all have the 
  # same number of ppoints
  a <- 1-pars[["ci"]]
  pc_ci_low <- pc - qnorm(1-a/2)*sqrt(pc*(1-pc)/n)
  pc_ci_high <- pc - qnorm(a-a/2)*sqrt(pc*(1-pc)/n)
  
  # protection for pc = 1 or pc = 0
  if(pc_ci_low == pc_ci_high) {
    pc_ci_low <- 0
    pc_ci_high <- 1
  }
    
  
  # extend the confidence intervals based on the instability
  # of the votes
  pc_ci_low_ext <- pc_ci_low - pc_ci_low*abs(ss1)
  pc_ci_high_ext <- pc_ci_high + (1-pc_ci_high)*abs(ss1)
  
  # store data for postprocessing
  verdict_data <- list()
  verdict_data[["vv1"]] <- vv1
  verdict_data[["ss1"]] <- ss1
  verdict_data[["pc_rest_flip"]] <- pc_rest_flip
  verdict_data[["pc_ci_low"]] <- pc_ci_low
  verdict_data[["pc_ci_high"]] <- pc_ci_high
  verdict_data[["pc_ci_low_ext"]] <- pc_ci_low_ext
  verdict_data[["pc_ci_high_ext"]] <- pc_ci_high_ext
  
  verdict_data[["vv2"]] <- vv2
  verdict_data[["ss2"]] <- ss2
  verdict_data[["vark"]] <- vark
  
  return(verdict_data)
  
}