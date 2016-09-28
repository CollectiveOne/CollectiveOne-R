decision_complexity <- function (pars) {
  votes <- pars[["votes"]]
  pps <- pars[["pps"]]
  
  rr <- 1/length(votes)*sum(votes)
  pc <- 1/sum(pps)*sum(votes*pps)
  # cc <- 2*abs(pc - 0.5)
  cc <- 1-4*pc*(1-pc)
  
  res <- list()
  res[["rr"]] <- rr
  res[["pc"]] <- pc
  res[["cc"]] <- cc
  
  return(res)
  
}