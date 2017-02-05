decision_verdict <- function (inputs, clarity_data) {
  pps <- inputs[["pps"]]
  votes <- inputs[["votes"]]
  
  res <-list()
  if(length(votes) > 0) {
    pps_cum  <- sum(pps)
    
    p0 <- 0.5*(1+pps/pps_cum)
    p <- 0.5 + clarity_data[["c_est"]]*(p0-0.5)
    
    # likelihood of reject and accept
    ix_rej <- which(votes %in% 0)
    ix_acc <- which(votes %in% 1)
    
    l0 <- prod(p[ix_rej])*prod(1-p[ix_acc])
    l1 <- prod(1-p[ix_rej])*prod(p[ix_acc])
    
    # find the verdict by chosing the one with larger 
    # likelihood
    if(l1 > l0) {
      verdict <- 1
    } else {
      verdict <- 0
    }
    
    res[["verdict"]] <- verdict
  } else {
    res[["verdict"]] <- NaN
  }
  
  return(res)
  
}