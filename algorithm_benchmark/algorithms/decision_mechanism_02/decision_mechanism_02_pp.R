decision_mechanism_02_pp <- function (result) {
  
  n_mc <- length(results)
 
  for (ix_mc in 1:n_mc) {
    output <- results[[ix_mc]][["debug_store"]]
    n_runs <- length(output)
    
    pkj <- vector("numeric",n_runs)
    
    for (ix in 1:n_runs) {
      pkj[ix] <- output[[ix]][["pkj"]]
    }
    
    png(file=file.path(getwd(), "output/decision_mechanism_02", paste("pkj_",sprintf("%4.2f", ix_mc),".png",sep="")))
      plot(pkj,col="darkgreen",ylim=c(-0.1, 1.1))
      grid()
    dev.off()
  }
}