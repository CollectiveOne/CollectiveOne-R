decision_is_valid <- function (inputs,clarity_result,stab_data,pars) {
  
  votes <- inputs[["votes"]]
  pps <- inputs[["pps"]]
  pps_tot <- inputs[["pps_tot"]] 
  
  pps_cum  <- sum(pps)
  n_votes_now <- length(votes)
  
  # determine the value of p_est of the rest of votes that would
  # result in flipping the current outcome
  p_est <- clarity_result[["p_est"]]
  pps_left <- pps_tot - pps_cum;
  
  valid_data <- list()
  
  if(abs(pps_left) > 1) {
    pc_rest_flip <- (0.5 - p_est*pps_cum/pps_tot)*pps_tot/pps_left
    
    # saturate unfeasible cases for plot purposes
    if(pc_rest_flip > 1) pc_rest_flip = 1.1
    if(pc_rest_flip < 0) pc_rest_flip = -0.1
    
    # confidence interval for p_est
    # TODO: not fully consistent as this assumes  they all have the 
    # same number of ppoints
    a <- 1-pars[["ci"]]
    pc_ci_low <- p_est - qnorm(1-a/2)*sqrt(p_est*(1-p_est)/n_votes_now)
    pc_ci_high <- p_est - qnorm(a-a/2)*sqrt(p_est*(1-p_est)/n_votes_now)
    
    # protection for p_est = 1 or p_est = 0
    if(pc_ci_low == pc_ci_high) {
      pc_ci_low <- 0
      pc_ci_high <- 1
    }
    
    # extend the confidence intervals based on the instability
    # of the votes
    pc_ci_low_ext <- pc_ci_low - pc_ci_low*abs(stab_data[["trend"]])
    pc_ci_high_ext <- pc_ci_high + (1-pc_ci_high)*abs(stab_data[["trend"]])
    
    # extend the confidence intervals based on time elapsed
    elapsedFactor <- inputs[["time"]]/pars[["max_duration"]]
    # extFactor goes from 1 to -1, passing through 0 when elapsedFactor is
    # 0.5 which is when elapsedHours are half the verdictHours
    extFactor = 2 * (1 - elapsedFactor) - 1;
    
    if (extFactor > 0) {
      # expand towards the [0,1] borders
      pc_ci_low_ext_time <-  pc_ci_low_ext - pc_ci_low_ext * extFactor;
      pc_ci_high_ext_time <-pc_ci_high_ext + (1 - pc_ci_high_ext)*extFactor;
    } else {
      # contract towards the center of the interval 
      ci_mean <-(pc_ci_low_ext + pc_ci_high_ext) / 2;
      low <- (ci_mean - pc_ci_low_ext) * (1 + extFactor);
      high <- (pc_ci_high_ext - ci_mean) * (1 + extFactor);
      pc_ci_low_ext_time <- ci_mean - low;
      pc_ci_high_ext_time <- ci_mean + high;
    }
  
    pc_ci_low_use <- pc_ci_low_ext
    pc_ci_high_use <- pc_ci_high_ext
    
    # check if the probability of outcome flipping is too low
    if((pc_rest_flip < pc_ci_low_use) | (pc_rest_flip > pc_ci_high_use)) {
      is_valid <- TRUE 
    } else {
      is_valid <- FALSE
    }
    
    # store for debug
    
    valid_data[["pc_rest_flip"]] <- pc_rest_flip
    valid_data[["pc_ci_low"]] <- pc_ci_low
    valid_data[["pc_ci_high"]] <- pc_ci_high
    valid_data[["pc_ci_low_ext"]] <- pc_ci_low_ext
    valid_data[["pc_ci_high_ext"]] <- pc_ci_high_ext
    valid_data[["pc_ci_low_ext_time"]] <- pc_ci_low_ext_time
    valid_data[["pc_ci_high_ext_time"]] <- pc_ci_high_ext_time
    
  } else {
    is_valid <- TRUE
    
    valid_data[["pc_rest_flip"]] <- 0
    valid_data[["pc_ci_low"]] <- 0
    valid_data[["pc_ci_high"]] <- 1
    valid_data[["pc_ci_low_ext"]] <- 0
    valid_data[["pc_ci_high_ext"]] <- 1
    valid_data[["pc_ci_low_ext_time"]] <- 0
    valid_data[["pc_ci_high_ext_time"]] <- 1
  }

  valid_data[["is_valid"]] <- is_valid
  
  return(valid_data)
  
}