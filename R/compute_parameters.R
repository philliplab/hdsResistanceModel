#' This function computes the fitnessAdjustment
#' 
#' @param kBase Fitnesses of the different strains.
#' @param treatments The treatment specification. A list of lists. Each inner list is the details of a single treatment regine. When did it start (t), How how does the patient adhere (A) and how suceptible is each strain to this treatment? (Ts) (vector with susceptibility for each strain). The outer list loops over each regime change.
#' @param treatment_num The number of the treatment to use for the computation

compute_fitnessAdjustment <- function(kBase, treatments, treatment_num = 1){
  Ts <- treatments[[treatment_num]]$Ts
  A <- treatments[[treatment_num]]$A
  fitnessAdjustment <- kBase * (1 - Ts * A)
}

#' This function transforms the input parameters into a format that the equations can use
#' 
#' @param scenario_parameters The parameters that specify the scenario
#' @export

compute_parameters <- function(scenario_parameters){
  params <- scenario_parameters

  new_params <- within(params, {
    N_d <- N_S - length(offStrains)
    mutMat <- matrix(mutMat, nrow = N_S)
    er <- er*mutationAcceleration # adjustment to make the timescales reasonable
    baseRate <- (mu_T*mu_P)/(f * (1 - Td) * S_T) # Modifier for invasion rates
    fitnessAdjustment <- compute_fitnessAdjustment(kBase, treatments)
#    fitnessAdjustment <- kBase * (1 - treatments[[1]]$Ts * treatments[[1]]$A)
    k <- baseRate * fitnessAdjustment # effective per strain invasion rates
    treatments[[1]] <- NULL
    E <- f*(er^mutMat)
    stochasticEventThreshold <- stochasticEventThresholdSource()
    
    mutateCont <- toggle_mutation_matrix(E, offStrains, type = 'continuous', N_S)
    mutateDisc <- toggle_mutation_matrix(E, offStrains, type = 'discrete', N_S)
    
    stopifnot(length(k) == length(kBase))
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

