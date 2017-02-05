decision_mechanism_01 <- function (inputs) {
  
  debugSource("algorithms/decision_mechanism_01/decision_clarity.R")
  debugSource("algorithms/decision_mechanism_01/decision_verdict.R")
  debugSource("algorithms/decision_mechanism_01/decision_validity.R")
  
  # paramaters
  pars <- list()
  pars[["ci"]] <- 0.9
  pars[["stab_ratio"]] <- 0.5
  # time is not being considered, time window coincides with the number of votes
  # so elapsedFactor is equal to the voters ratio
  pars[["max_duration"]] <- inputs[["test_duration"]]
  
  # analize vote clarity
  clarity_result <- decision_clarity(inputs)
  
  # current verdict
  verdict_result <- decision_verdict(inputs, clarity_result)
  
  # determine if verdict is valid
  verdict_validity <- decision_validity(inputs,clarity_result,verdict_result,pars)
  
  # process results to fit output interface
  output <- list()
  if(!is.nan(verdict_result[["verdict"]])) {
    output[["verdict_reached"]] <- verdict_validity[["is_valid_data"]][["is_valid"]]
  } else {
    output[["verdict_reached"]] <- FALSE;
  }
  
  output[["verdict_estimated"]] <- verdict_result[["verdict"]]  
  
  debug_data <- list()
  debug_data[["clarity_result"]] <- clarity_result
  debug_data[["verdict_result"]] <- verdict_result
  debug_data[["verdict_validity"]] <- verdict_validity
  
  output[["debug_data"]] <- debug_data
  
  return(output)
  
}