rm(list = ls())
# dev.off(dev.list()["RStudioGD"])

library(actuar)
source("run.r")
source("decision_complexity.r")
source("verdict.r")
source("decision_stability.r")

pars <- list()
pars[["p"]] <- 0.6
pars[["N"]] <- 150
pars[["Ptot"]] <- 1000

pars[["alpha"]] <- 10
pars[["scale"]] <- 1

pars[["ci"]] <- 0.9
pars[["stab_ratio"]] <- 0.5

pars[["n_mc"]] <- 100

#
if(pars[["p"]] > 0.5) {
  vv_ok <- 1 
} else {
  vv_ok <- 0 
}

#
set.seed(1) 

seeds <- round(10000*runif(pars[["n_mc"]],0,1))

ix_ver1 <- vector("numeric",pars[["n_mc"]])
vv_ok_ver1 <- vector("numeric",pars[["n_mc"]])

for(ix_mc in 1:pars[["n_mc"]]) {
  set.seed(seeds[ix_mc])
  res <- run(pars)
  
  # plot(res[["vv1"]],ylim=c(0, 1))
  # grid()
  # 
  # plot(res[["pc"]],ylim=c(0, 1))
  # grid()
  # 
  # plot(res[["ss1"]])
  # grid()
  # 
  # png(file=file.path(getwd(), "output", paste("ml_",sprintf("%03d", ix_mc),".png",sep="")))
  #   plot(res[["vv1"]],col="darkgreen",ylim=c(-0.1, 1.1))
  #   lines(res[["pc_rest_flip"]],col="black")
  #   lines(res[["pc_ci_low"]],col="blue")
  #   lines(res[["pc_ci_high"]],col="blue")
  #   lines(res[["pc_ci_low_ext"]],col="red")
  #   lines(res[["pc_ci_high_ext"]],col="red")
  #   grid()
  # dev.off()
  # 
  # png(file=file.path(getwd(), "output", paste("var_vv2_",sprintf("%03d", ix_mc),".png",sep="")))
  #   plot(res[["vv2"]],ylim=c(0, 1))
  #   grid()
  # dev.off()
  # 
  # png(file=file.path(getwd(), "output", paste("var_ss2_",sprintf("%03d", ix_mc),".png",sep="")))
  #   plot(res[["ss2"]],ylim=c(-0.01, +0.01))
  #   grid()
  # dev.off()
  # 
  # png(file=file.path(getwd(), "output", paste("var_vark_",sprintf("%03d", ix_mc),".png",sep="")))
  #   plot(res[["vark"]],ylim=c(-0.01, +0.01))
  #   grid()
  # dev.off()
  
  print(ix_mc)
  
  # verdict1 
  ixs_ver1 <- which((res[["pc_rest_flip"]] < res[["pc_ci_low_ext"]]) | (res[["pc_rest_flip"]] > res[["pc_ci_high_ext"]]))
  if(length(ixs_ver1) > 0) {
    ix_ver1[ix_mc] <- ixs_ver1[1]
  } else {
    ix_ver1[ix_mc] <- length(pars[["N"]])
  }
  
  if(vv_ok == res[["vv2"]][ix_mc]) {
    vv_ok_ver1[ix_mc] <- 1 
  } else {
    vv_ok_ver1[ix_mc] <- 0 
  }

}

png(file=file.path(getwd(), "output", paste("verdict_ix_ver1_hist",sprintf("%4.2f", pars[["p"]]),".png",sep="")))
  hist(ix_ver1)
dev.off()

png(file=file.path(getwd(), "output", paste("verdict_correct_ver1_hist",sprintf("%4.2f", pars[["p"]]),".png",sep="")))
  hist(vv_ok_ver1)
dev.off()



