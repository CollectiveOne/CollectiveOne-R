decision_validity <- function (inputs,clarity_result,verdict_result,pars) {

  debugSource("algorithms/decision_mechanism_01/decision_stability.R")
  debugSource("algorithms/decision_mechanism_01/decision_is_valid.R")
    
  # determine voting stability
  stab_data <- decision_stability(inputs,clarity_result,pars)
  is_valid_data <- decision_is_valid(inputs,clarity_result,stab_data,pars)
  
  validity_data <- list()
  validity_data[["is_valid_data"]] <- is_valid_data
  validity_data[["stab_data"]] <- stab_data
  
  return(validity_data)
  
}