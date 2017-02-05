sim_voting <- function (sim_conf,algorithm_function,rtc_function) {

  set.seed(sim_conf[["seed"]])
  
  # The distribution of ppoints among voters
  switch(sim_conf[["pps_concentration_case"]],
    homogeneous = { p_ratio <- sim_conf[["pps_tot"]]/sim_conf[["n_votes_tot"]]*rep(1, sim_conf[["n_votes_tot"]])  },
    pareto = { p_ratio <- rpareto(sim_conf[["n_votes_tot"]], sim_conf[["alpha"]], sim_conf[["scale"]]) }
  )
  pps <- p_ratio/sum(p_ratio)*sim_conf[["pps_tot"]]
  
  # generate the votes (all of them)
  if(!sim_conf[["use_rtc"]]) {
    votes_base <- runif(sim_conf[["n_votes_tot"]],0,1)
    votes <- votes_base < sim_conf[["p_true"]]  
  } else {
    # rtc <- sim_conf[["rct_fun"]]()
    rtc <- rtc_function()
    votes_mat <- rtc[["votes"]]
    votes <- votes_mat[ sim_conf[["ix_mc"]], ]
  }
  
  # simulate each vote and store data
  debug_store = vector("list",sim_conf[["n_votes_tot"]])
  
  # flag to mark when the algorithm reached a verdict
  result <- list()
  verdict_reached <- FALSE

  n_hours_no_vote <- 0
  ix_vote <- 0
  
  for (ix_hour in 1:sim_conf[["test_duration"]]) {
    
    # prepare the inputs to the decision algorithm
    step_inputs <- list()
    
    # determine the number of votes that are casted at this time
    switch(sim_conf[["vote_patter"]],
      periodic = { 
          if(n_hours_no_vote >= sim_conf[["period"]] - 1) {
            n_votes_now <- 1
            n_hours_no_vote <- 0
          } else {
            n_votes_now <- 0
          }
        }
    )
    
    if(n_votes_now > 0) {
      ix_vote <- ix_vote + n_votes_now
      # ehh, what if more than one vote???
      this_vote <- votes[ix_vote]  
    } else {
      n_hours_no_vote <- n_hours_no_vote + 1;
      this_vote <- NaN
    }
    
    if(ix_vote > 1) {
      # the algorithm receives as input all the votes received so far
      step_inputs[["votes"]] <- votes[1:ix_vote]
      step_inputs[["pps"]] <- pps[1:ix_vote]
    } else {
      step_inputs[["votes"]] <- vector()
      step_inputs[["pps"]] <- vector()
    }
    
    step_inputs[["pps_tot"]] <- sum(pps)
    step_inputs[["n_votes_tot"]] <- sim_conf[["n_votes_tot"]]
    step_inputs[["time"]] <- ix_hour
    step_inputs[["test_duration"]] <- sim_conf[["test_duration"]]
    
    # ------- CALL THE DECISION ALGORITHM ----------
    this_step_output <- algorithm_function(step_inputs)
    # ----------------------------------------------
    
    this_step_output[["debug_data"]][["vote"]] <- this_vote
    
    # store debug variables generated at each step
    debug_store[[ix_hour]] <- this_step_output[["debug_data"]]
    
    # check if verdict is reached and store the time and the verdict
    if(!verdict_reached) {
      if(this_step_output[["verdict_reached"]]) {
        verdict_reached <- TRUE
        result[["verdict_at_time"]] <- step_inputs[["time"]]
        result[["verdict_estimated"]] <- this_step_output[["verdict_estimated"]]
      }
    }
  }
  
  if(!verdict_reached) {
    result[["verdict_at_time"]] <- sim_conf[["n_votes_tot"]]
    result[["verdict_estimated"]] <- this_step_output[["verdict_estimated"]]
  }

  result[["debug_store"]] <- debug_store
  
  return(result)
}