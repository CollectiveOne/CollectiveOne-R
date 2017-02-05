# set working folder
# dev.off(dev.list()["RStudioGD"])
setwd("~/workspace-sts/CollectiveOne-R/algorithm_benchmark")
rm(list = ls())

sim_conf <- list()
set.seed(5) 
algo_ix <- 1

# configure the test
sim_conf[["p_true"]] <- 0.7          # probability of voting accept
sim_conf[["n_votes_tot"]] <- 20      # number of possible votes
sim_conf[["test_duration"]] <- 100   # test duration in hours. The algorithm is called once every hour

sim_conf[["vote_patter"]] <- "periodic" # rule to decide how votes are casted as a funcition of time
# homogeneous: one vote every fixed (period) number of hours
sim_conf[["period"]] <- 5

sim_conf[["n_mc"]] <- 10         # number of montecarlo runs

# pareto homogeneous
sim_conf[["pps_concentration_case"]] <- "homogeneous" 
sim_conf[["pps_tot"]] <- 1000

# configure decision algorithm function
if(algo_ix == 1) {
  debugSource("algorithms/decision_mechanism_01/decision_mechanism_01.R")
  algorithm_function <- decision_mechanism_01
  debugSource("algorithms/decision_mechanism_01/decision_mechanism_01_pp.R")
  algorithm_postprocessing <- decision_mechanism_01_pp  
  
  sim_conf[["use_rtc"]] <- FALSE
}

if(algo_ix == 2) {
  debugSource("algorithms/decision_mechanism_02/decision_mechanism_02.R")
  algorithm_function <- decision_mechanism_02
  debugSource("algorithms/decision_mechanism_02/decision_mechanism_02_pp.R")
  algorithm_postprocessing <- decision_mechanism_02_pp
  
  # enable to run votes extracted from RTC
  sim_conf[["use_rtc"]] <- FALSE
  debugSource("algorithms/decision_mechanism_02/rtc_01.R")
  debugSource("algorithms/decision_mechanism_02/rtc_comparison.R")
  rtc_function <- rct_01
  
}

# prepare test
debugSource("main/sim_voting.R")

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
  sim_conf[["ix_mc"]] <- ix_mc
  sim_conf[["seed"]] <- seeds[ix_mc]
  
  # -------- simulate n_votes votes ---------
  # store the output of the simulation in res
  results[[ix_mc]] <- sim_voting(sim_conf,algorithm_function,rtc_function)
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

if(sim_conf[["use_rtc"]]) {
  rtc_comparison(results,rtc_function)
}
  

