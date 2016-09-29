sim_voting <- function (sim_conf,algorithm_function) {

  # The distribution of ppoints among voters
  switch(sim_conf[["pps_concentration_case"]],
    homogeneous = { p_ratio <- sim_conf[["pps_tot"]]/sim_conf[["n_votes_tot"]]*rep(1, sim_conf[["n_votes_tot"]])  },
    pareto = { p_ratio <- rpareto(sim_conf[["n_votes_tot"]], sim_conf[["alpha"]], sim_conf[["scale"]]) }
  )
  pps <- p_ratio/sum(p_ratio)*sim_conf[["pps_tot"]]
  
  # generate the votes (all of them)
  votes_base <- runif(sim_conf[["n_votes_tot"]],0,1)
  votes <- votes_base < sim_conf[["p_true"]]
  
  # simulate each vote and store data
  debug_store = vector("list",sim_conf[["n_votes_tot"]])
  
  # flag to mark when the algorithm reached a verdict
  result <- list()
  verdict_reached <- FALSE
  
  for (ix in 1:sim_conf[["n_votes_tot"]]) {
    
    # prepare the inputs to the decision algorithm
    step_inputs <- list()
    step_inputs[["votes"]] <- votes[1:ix]
    step_inputs[["pps"]] <- pps[1:ix]
    step_inputs[["pps_tot"]] <- sum(pps)
    step_inputs[["time"]] <- ix
    step_inputs[["n_votes_tot"]] <- sim_conf[["n_votes_tot"]]
    
    # ------- CALL THE DECISION ALGORITHM ----------
    this_step_output <- algorithm_function(step_inputs)
    # ----------------------------------------------
    
    # store debug variables generated at each step
    debug_store[[ix]] <- this_step_output[["debug_data"]]
    
    # check if verdict is reached and store the time and the verdict
    if(!verdict_reached) {
      if(this_step_output[["verdict_reached"]]) {
        verdict_reached <- TRUE
        result[["verdict_at_time"]] <- step_inputs[["time"]]
        result[["verdict_estimated"]] <- this_step_output[["verdict_estimated"]]
      }
    }
  }

  result[["debug_store"]] <- debug_store
  
  return(result)
}