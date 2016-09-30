# set working folder
# dev.off(dev.list()["RStudioGD"])
setwd("~/workspace/CoProjects-R/algorithm_benchmark")
rm(list = ls())

set.seed(5) 
algo_ix <- 1
# configure decision algorithm function
if(algo_ix == 1) {
  debugSource("algorithms/decision_mechanism_01/decision_mechanism_01.R")
  algorithm_function <- decision_mechanism_01
  debugSource("algorithms/decision_mechanism_01/decision_mechanism_01_pp.R")
  algorithm_postprocessing <- decision_mechanism_01_pp  
}

if(algo_ix == 2) {
  debugSource("algorithms/decision_mechanism_02/decision_mechanism_02.R")
  algorithm_function <- decision_mechanism_02
  debugSource("algorithms/decision_mechanism_02/decision_mechanism_02_pp.R")
  algorithm_postprocessing <- decision_mechanism_02_pp
}

# prepare test
debugSource("main/sim_voting.R")

# configure the test
sim_conf <- list()
sim_conf[["p_true"]] <- 0.7       # probability of voting accept
sim_conf[["n_votes_tot"]] <- 20  # number of possible votes
sim_conf[["n_mc"]] <- 100         # number of montecarlo runs

sim_conf[["pps_concentration_case"]] <- "homogeneous"
sim_conf[["pps_tot"]] <- 150

# prepare the correct verdict ()
if(sim_conf[["p_true"]] > 0.5) {
  verdict_true <- TRUE 
} else {
  verdict_true <- FALSE 
}

# prepare the seeds for each montecarlo run
seeds <- round(10000*runif(sim_conf[["n_mc"]],0,1))

# prepare vector to store the outputs of each run
verdict_at_time <- vector("numeric",sim_conf[["n_mc"]])
verdict_was_correct <- vector("numeric",sim_conf[["n_mc"]])

results <- vector("list",sim_conf[["n_mc"]])

for(ix_mc in 1:sim_conf[["n_mc"]]) {
  
  print(ix_mc)
  set.seed(seeds[ix_mc])
  
  # -------- simulate n_votes votes ---------
  # store the output of the simulation in res
  results[[ix_mc]] <- sim_voting(sim_conf,algorithm_function)
  # -----------------------------------------
  
  # store the results of this simulation
  verdict_at_time[ix_mc] = results[[ix_mc]][["verdict_at_time"]]
  
  if(verdict_true == results[[ix_mc]][["verdict_estimated"]]) {
    verdict_was_correct[ix_mc] = TRUE
  } else  {
    verdict_was_correct[ix_mc] = FALSE
  }
}

algorithm_postprocessing(result)

hist(verdict_at_time)
print(paste("correct verdict",sum(verdict_was_correct)/length(verdict_was_correct)*100,"% of times"))
print(paste("verdict taken at vote",mean(verdict_at_time),"in mean with std",sd(verdict_at_time) ))

