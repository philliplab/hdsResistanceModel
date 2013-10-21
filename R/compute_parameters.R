#' This function transforms the input parameters into a format that the equations can use
#' 
#' @param constants The parameters that are unlikely to change from scenario to scenario, 
#' as produced by get_de_defaults 
#' @param scenario_parameters The parameters that specify the scenario
#' @export

compute_parameters <- function(constants, scenario_parameters){
  params <- constants
  for (par_name in names(scenario_parameters)){
    params[[par_name]] <- scenario_parameters[[par_name]]
  }
  
  new_params <- within(params, {
    N_d <- N_S - length(offStrains)
    Epow <- matrix(Epow, nrow = N_S)
    er <- er*mutationAcceleration # adjustment to make the timescales reasonable
    baseRate <- (mu_T*mu_P)/(f * (1 - Td) * S_T) # Modifier for invasion rates
    stopifnot(treatments[[1]]$t == 0)
    fitnessAdjustment <- Pf * (1 - treatments[[1]]$Ts * treatments[[1]]$Te)
    k <- baseRate * fitnessAdjustment # effective per strain invasion rates
    treatments[[1]] <- NULL
    E <- f*(er^Epow)
    stochasticEventThreshold <- stochasticEventThresholdSource()
    
    mutateCont <- toggle_mutation_matrix(E, offStrains, type = 'continuous', N_S)
    mutateDisc <- toggle_mutation_matrix(E, offStrains, type = 'discrete', N_S)
    
    stopifnot(length(k) == length(Pf))
    stopifnot(length(k) == N_S)
    stopifnot(N_d > -1)
    stopifnot(nrow(E) == ncol(E))
    stopifnot(!all(is.na(E)))
    stopifnot(!all(is.null(E)))
    stopifnot(dim(mutateCont) == dim(E))
    stopifnot(dim(mutateDisc) == dim(E))
  })
  return(new_params)
}

