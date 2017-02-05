decision_mechanism_01_pp <- function (result) {
  
  n_mc <- length(results)
 
  for (ix_mc in 1:n_mc) {
    output <- results[[ix_mc]][["debug_store"]]
    n_runs <- length(output)
    
    votes <- vector("numeric",n_runs)
    verdict <- vector("numeric",n_runs)
    p_est <- vector("numeric",n_runs)
    pc_rest_flip <- vector("numeric",n_runs)
    pc_ci_low <- vector("numeric",n_runs)
    pc_ci_high <- vector("numeric",n_runs)
    pc_ci_low_ext <- vector("numeric",n_runs)
    pc_ci_high_ext <- vector("numeric",n_runs)
    pc_ci_low_ext_time <- vector("numeric",n_runs)
    pc_ci_high_ext_time <- vector("numeric",n_runs)
    
    trend <- vector("numeric",n_runs)
    
    for (ix in 1:n_runs) {
      if(!is.nan(output[[ix]][["verdict_result"]][["verdict"]])) {
        votes[ix] <- output[[ix]][["vote"]]
        verdict[ix] <- output[[ix]][["verdict_result"]][["verdict"]]
        p_est[ix] <- output[[ix]][["clarity_result"]][["p_est"]]
        pc_rest_flip[ix] <- output[[ix]][["verdict_validity"]][["is_valid_data"]][["pc_rest_flip"]]
        pc_ci_low[ix] <- output[[ix]][["verdict_validity"]][["is_valid_data"]][["pc_ci_low"]]
        pc_ci_high[ix] <- output[[ix]][["verdict_validity"]][["is_valid_data"]][["pc_ci_high"]]
        pc_ci_low_ext[ix] <- output[[ix]][["verdict_validity"]][["is_valid_data"]][["pc_ci_low_ext"]]
        pc_ci_high_ext[ix] <- output[[ix]][["verdict_validity"]][["is_valid_data"]][["pc_ci_high_ext"]]
        pc_ci_low_ext_time[ix] <- output[[ix]][["verdict_validity"]][["is_valid_data"]][["pc_ci_low_ext_time"]]
        pc_ci_high_ext_time[ix] <- output[[ix]][["verdict_validity"]][["is_valid_data"]][["pc_ci_high_ext_time"]]
        trend[ix] <- output[[ix]][["verdict_validity"]][["stab_data"]][["trend"]]
      } else {
        votes[ix] <- NaN
        verdict[ix] <- NaN
        p_est[ix] <- NaN
        pc_rest_flip[ix] <- NaN
        pc_ci_low[ix] <- NaN
        pc_ci_high[ix] <- NaN
        pc_ci_low_ext[ix] <- NaN
        pc_ci_high_ext[ix] <- NaN
        pc_ci_low_ext_time[ix] <- NaN
        pc_ci_high_ext_time[ix] <- NaN
        trend[ix] <- NaN
      }
      
    }
    
    png(file=file.path(getwd(), "output/decision_mechanism_01", paste("internal_signals_",sprintf("%4.2f", ix_mc),".png",sep="")))
      plot(verdict,col="darkgreen",ylim=c(-0.1, 1.1))
      points(votes,col="red",pch=3)
      lines(p_est,col="darkgreen", type="s")
      lines(pc_rest_flip,col="black", type="s")
      lines(pc_ci_low,col="red", type="s")
      lines(pc_ci_high,col="red", type="s")
      lines(pc_ci_low_ext,col="blue", type="s")
      lines(pc_ci_high_ext,col="blue", type="s")
      lines(pc_ci_low_ext_time,col="magenta", type="s")
      lines(pc_ci_high_ext_time,col="magenta", type="s")
      lines(trend,col="gray", type="s")
      points(verdict_at_time[ix_mc],pc_rest_flip[verdict_at_time[ix_mc]],"p",col="red",pch=11)
      grid()
    dev.off()
  }
}