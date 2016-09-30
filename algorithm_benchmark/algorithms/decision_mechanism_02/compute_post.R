compute_post <- function (t,n,k,s,a,b) {
  return(choose(n-k,t-s) * beta(a+t,b+n-t) / beta(a+s,b+k-s))
}
  