#' Checks input list for problems and throw an error or returns a valid scenario specification
#' 
#' The main goal of this function is as a placeholder for an expansion that will allow the
#' scenarios to be saved in a database.
#' 
#' @param timeStep The size of the steps in the output data
#' @param timeStop Run the system until this time
#' @param systemName A name for the system
#' @param systemDescription A description for the system
#' @param Pf Fitnesses of the different strains
#' @param treatments The treatment specification. A list of lists. Each inner list is the details of a single treatment regine. When did it start (t), What is its effect (Te) and how suceptible is each strain to this treatment? (Ts) (vector with susceptibility for each strain). The outer list loops over each regime change.
#' @param mutationAcceleration A factor that accellerates the rate of mutaton. Needed to get the timescales for when mutations arises right
#' @param Td Tcell depletion - ratio of pre-infected to post-infected equilibria. It also sets the scale for the acceptible range of the relative fitnesses of the strains.
#' @param N_S Number of strains in system
#' @param offStrains Strains not present in the initial system
#' @param stochasticEventThresholdSource A function that returns the threshold used to determine if a mutation occurred
#' @param Epow The mutation matrix.
#' @export

scenario <- function(timeStep, timeStop, systemName, systemDescription, 
                     Pf, treatments, mutationAcceleration, Td, N_S, 
                     offStrains, stochasticEventThreshold, Epow){
  
  # check that numeric variables are numeric
  numeric_variables <- c("Epow", "mutationAcceleration", "N_S", "offStrains", "Pf", "Td", "timeStep", "timeStop")
  for (numeric_variable in numeric_variables){
    numeric_variable_value <- get(numeric_variable)
    if(!is.numeric(numeric_variable_value)) {stop(str_c(numeric_variable, ' is not numeric'))}
  }
  
  # check timeStop / timeStep relationship
  if (timeStep > timeStop / 10){stop("timeStep must be < timeStop/10")}
  params <- list(
    timeStep = timeStep,
    timeStop = timeStop,
    systemName = systemName,
    systemDescription = systemDescription,
    Pf = Pf,
    treatments = treatments,
    mutationAcceleration = mutationAcceleration,
    Td = Td,
    N_S = N_S,
    offStrains = offStrains,
    stochasticEventThreshold = stochasticEventThreshold,
    Epow = Epow
    )
}
