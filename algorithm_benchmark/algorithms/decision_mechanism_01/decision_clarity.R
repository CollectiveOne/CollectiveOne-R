decision_clarity <- function (inputs) {
  votes <- inputs[["votes"]]
  pps <- inputs[["pps"]]
  
  # r is the ratio of votes in favor
  r_est <- 1/length(votes)*sum(votes)
  
  # p is the weighted ratio of votes in favor (using pps as weight)
  p_est <- 1/sum(pps)*sum(votes*pps)
  
  # c is the clarity of the decision
  # inverse and scaled function of the variance, 
  # 0 when variance is max p = 0.5 and var = 0.25
  # 1 when variance is min p = 1 or p = 0 and var = 0 
  c_est <- 1-4*p_est*(1-p_est)
  
  res <- list()
  res[["r_est"]] <- r_est
  res[["p_est"]] <- p_est
  res[["c_est"]] <- c_est
  
  return(res)
  
}