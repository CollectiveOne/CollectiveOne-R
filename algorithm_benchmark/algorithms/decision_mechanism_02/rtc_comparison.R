rtc_comparison <- function (results,rtc_function) {
  
  rtc <- rtc_function()
  pkj_ref_matrix <- rtc[["pkj"]]
  
  n_mc <- length(results)
  
  for (ix_mc in 1:n_mc) {
    output <- results[[ix_mc]][["debug_store"]]
    n_runs <- length(output)
    
    pkj_test <- vector("numeric",n_runs)
    
    for (ix in 1:n_runs) {
      pkj_test[ix] <- output[[ix]][["pkj"]]
    }
    
    pkj_ref <- pkj_ref_matrix[ ix_mc , ]
    
    png(file=file.path(getwd(), "output/decision_mechanism_02", paste("pkj_test_vs_ref",sprintf("%4.2f", ix_mc),".png",sep="")))
      plot(pkj_test,type="l",col="darkred")
      par(new=T)
      plot(pkj_ref,type="l",axes="FALSE",col="darkgreen")
      par(new=F)
      grid()
    dev.off()
  }

}