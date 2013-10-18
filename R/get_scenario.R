#' Retrieves one of the pre-specified scenario specifications
#' 
#' This function is a poor way of storing pre-specified scenarios. This function is mainly a placeholder
#' for a mechanism that will store specified scenarios in a DB
#' 
#' @param scenario_name The name of the scenario to retrieve
#' @export

get_scenario <- function(scenario_name){
  userParsLib <- list()
  userParsLib[['Simple_1_2']] <- list(
    timeStep = 1,
    timeStop = 1000,
    systemName = "Simple_1_2",
    systemDescription = "One wild type virus that can stochastically mutate into 2 target strains. 
  This allows for 2 different stochastic events and 2 different 'paths' through the system. 
  No treatment effect but strain 2 is more fit than strain 1",
    Pf = c(0.95, 1, 0.96), # Fitnesses of the different strains
    treatments = list(list(t = 0, Te = 0.0, Ts = c(0.8, 0.80, 0.75))),
    mutationAcceleration = 1.5*(10^(-1)), # adjustment to make the timescales reasonable
    Td = 0.2, # Tcell depletion - ratio of pre-infected to post-infected equilibria
    N_S = 3, # Number of strains in system
    offStrains = c(2,3), # Strains not present in the initial system
    stochasticEventThresholdSource = function(){return(runif(1))}, # draw this number randomly in production situation
    Epow = c(0, 2, 2,
             2, 0, 2,
             2, 2, 0)
  )
  if (scenario_name %in% names(userParsLib)){
    return(do.call(scenario, userParsLib[[scenario_name]]))
  } else {
    stop("Scenario does not exist")
  }
}