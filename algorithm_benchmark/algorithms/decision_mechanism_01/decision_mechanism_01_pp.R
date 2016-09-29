decision_mechanism_01_pp <- function (result) {
  
  n_mc <- length(results)
 
  for (ix_mc in 1:n_mc) {
    output <- results[[ix_mc]][["debug_store"]]
    n_runs <- length(output)
    
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
    }
    
    png(file=file.path(getwd(), "output/decision_mechanism_01", paste("internal_signals_",sprintf("%4.2f", ix_mc),".png",sep="")))
      plot(verdict,col="darkgreen",ylim=c(-0.1, 1.1))
      lines(p_est,col="darkgreen")
      lines(pc_rest_flip,col="black")
      lines(pc_ci_low,col="red")
      lines(pc_ci_high,col="red")
      lines(pc_ci_low_ext,col="blue")
      lines(pc_ci_high_ext,col="blue")
      lines(pc_ci_low_ext_time,col="magenta")
      lines(pc_ci_high_ext_time,col="magenta")
      lines(trend,col="gray")
      points(verdict_at_time[ix_mc],pc_rest_flip[verdict_at_time[ix_mc]],"p",col="red")
      grid()
    dev.off()
  }
}