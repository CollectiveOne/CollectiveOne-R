compute_pkj <- function (n,k,s,l,a,b) {
  pkj <- 0
  for(t in 1:(l*n-1)) {
    pkj <- pkj + compute_post(t,n,k,s,a,b)
  }
  return(pkj)
}
  