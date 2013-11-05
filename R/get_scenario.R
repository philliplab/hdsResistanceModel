#' Returns all the pre-specified scenarios
#'
#' This function is a poor way of storing pre-specified scenarios. This function is mainly a placeholder
#' for a mechanism that will store specified scenarios in a DB
#' 
#' @export

get_all_scenarios <- function(){
  userParsLib <- list()
  userParsLib[['Simple_1_0']] <- list(
    timeStep = 1,
    timeStop = 1000,
    systemName = "Simple_1_0",
    systemDescription = "One wild type virus with no mutations possible. Primarily used in test cases",
    Pf = c(0.95), # Fitnesses of the different strains
    treatments = list(list(t = 0, Te = 0.0, Ts = c(0))),
    mutationAcceleration = 0.15, # adjustment to make the timescales reasonable
    Td = 0.2, # Tcell depletion - ratio of pre-infected to post-infected equilibria
    N_S = 1, # Number of strains in system
    offStrains = numeric(0), # Strains not present in the initial system
    stochasticEventThresholdSource = function(){return(runif(1))}, # draw this number randomly in production situation
    Epow = c(0)
  )
  
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
  
  userParsLib[['AccuTams_1_2']] <- list(
    timeStep = 1,
    timeStop = 1500,
    systemName = "AccuTams_1_2",
    systemDescription = "One wild strain is present initially. Two other strains can evolve - 2 point mutations to get
    get one strain and from this strain another 2 point mutations to get to a third possible strain.
    Strains have increasing fitness.",
    Pf = c(1,1,1), # Fitnesses of the different strains
    treatments = list(list(t = 0, Te = 1, Ts = 1-c(0.807442176870748, 0.857971014492754, 0.95))),
    mutationAcceleration = 1, #1.5*(10^(-1)), # adjustment to make the timescales reasonable
    Td = 0.2, # Tcell depletion - ratio of pre-infected to post-infected equilibria
    N_S = 3, # Number of strains in system
    offStrains = c(2,3), # Strains not present in the initial system
    stochasticEventThresholdSource = function(){return(runif(1))}, # draw this number randomly in production situation
    Epow = c(0, 2, 4,
             2, 0, 2,
             4, 2, 0)
  )
  
  userParsLib[['Simple_2_1']] <- list(
   timeStep = 0.1,
   timeStop = 1500,
   systemName = "Simple_2_1",
   systemDescription = "Two wild type viruses that can stochastically mutate into 1 target strain. 
   This allows for 1 stochastic events and 1 'path' through the system. 
   No treatment effect but strain 3 is more fit than strains 1 and 2",
   Pf = c(0.95, 0.949, 1), # Fitnesses of the different strains
   treatments = list(list(t = 0, Te = 0.0, Ts = c(0.8, 0.80, 0.75))),
   mutationAcceleration = 1.5*(10^(-1)), # adjustment to make the timescales reasonable
   Td = 0.2, # Tcell depletion - ratio of pre-infected to post-infected equilibria
   N_S = 3, # Number of strains in system
   offStrains = c(3), # Strains not present in the initial system
   stochasticEventThresholdSource = function(){return(runif(1))}, # draw this number randomly in production situation
   Epow = c(0, 2, 2,
            2, 0, 2,
            2, 2, 0)
  )
  
  userParsLib[['Simple_2_2']] <- list(
   timeStep = 0.5,
   timeStop = 2500,
   systemName = "Simple_2_2",
   systemDescription = "Two wild type viruses that can stochastically mutate into a different a target strain. 
   This allows for 2 stochastic events and 2 'paths' through the system. 
   No treatment effect but strains 2 and 4 are more fit than strains 1 and 3",
   Pf = c(0.95, 1, 0.951, 1), # Fitnesses of the different strains
   treatments = list(list(t = 0, Te = 0.0, Ts = c(0.8, 0.80, 0.75, 0.8))),
   mutationAcceleration = 2*(10^(-1)), # adjustment to make the timescales reasonable
   Td = 0.2, # Tcell depletion - ratio of pre-infected to post-infected equilibria
   N_S = 4, # Number of strains in system
   offStrains = c(2,4), # Strains not present in the initial system
   stochasticEventThresholdSource = function(){return(runif(1))}, # draw this number randomly in production situation
   Epow = c(0,   2,   Inf, Inf,
            2,   0,   Inf, Inf,
            Inf, Inf, 0,   2,
            Inf, Inf, 2,   0)
  )
  
  userParsLib[['RepRes_1_1']] <- list(
   timeStep = 0.5,
   timeStop = 2500,
   systemName = "RepRes_1_1",
   systemDescription = "Two strains that can mutate into each other. An attempt to show repeated ressurection",
   Pf = c(0.85, 0.95), # Fitnesses of the different strains
   treatments = list(list(t=0, Te = 0.0, Ts = c(0.8, 0.80))),
   mutationAcceleration = 2*(10^(-1)), # adjustment to make the timescales reasonable
   Td = 0.2, # Tcell depletion - ratio of pre-infected to post-infected equilibria
   N_S = 2, # Number of strains in system
   offStrains = c(2), # Strains not present in the initial system
   stochasticEventThresholdSource = function(){return(runif(1))}, # draw this number randomly in production situation
   Epow = c(0,   2,
            2,   0)
  )
  
  userParsLib[['Treatment_1_0']] <- list(
   timeStep = 0.5,
   timeStop = 2500,
   systemName = "Treatment_1_0",
   systemDescription = "One strain that cannot mutate. An attempt to illustrate varying levels of treatment",
   Pf = c(0.85), # Fitnesses of the different strains
   treatments = list(list(t=0, Te = 0, Ts = c(0.8)),
                     list(t=500, Te = 0.1, Ts = c(0.8)),
                     list(t=1000, Te = 0.0, Ts = c(0.8)),
                     list(t=1500, Te = 0.2, Ts = c(0.8))
                     ),
   mutationAcceleration = 2*(10^(-1)), # adjustment to make the timescales reasonable
   Td = 0.2, # Tcell depletion - ratio of pre-infected to post-infected equilibria
   N_S = 1, # Number of strains in system
   offStrains = 0[0], # Strains not present in the initial system
   stochasticEventThresholdSource = function(){return (0.4)}, # draw this number randomly in production situation
   Epow = c(0)
  )
  
  userParsLib[['BadStart_1_1']] <- list(
   timeStep = 0.5,
   timeStop = 2500,
   systemName = "BadStart_1_1",
   systemDescription = "Initial infection with resistant strain. Untreated until wild type pushes resistant strain to very low population levels. Treatment is initiated causing the initial resistance to arise again",
   Pf = c(0.85, 1), # Fitnesses of the different strains
   treatments = list(list(t=0, Te = 0.0, Ts = c(0.8, 0.8)),
                     list(t=1985, Te = 0.3, Ts = c(0, 1))
                     ),
   mutationAcceleration = 2*(10^(-1)), # adjustment to make the timescales reasonable
   Td = 0.2, # Tcell depletion - ratio of pre-infected to post-infected equilibria
   N_S = 2, # Number of strains in system
   offStrains = c(2), # Strains not present in the initial system
   stochasticEventThresholdSource = function(){return(runif(1))}, # draw this number randomly in production situation
   Epow = c(0,   2,
            2,   0)
  )
  
  userParsLib[['PoorTreat_1_1']] <- list(
   timeStep = 0.5,
   timeStop = 3500,
   systemName = "PoorTreat_1_1",
   systemDescription = "FAILURE: Initial infection with wildType Strain. Resistant mutations occur, but are driven to extinction by dominance of wildType. Poor treatment is initiated allowing drug resistant strain to flourish",
   Pf = c(1, 0.90), # Fitnesses of the different strains
   treatments = list(list(t=0, Te = 0.0, Ts = c(0.8, 0.8)),
                     list(t=1000, Te = 0.18, Ts = c(1,0))
                     ),
   mutationAcceleration = 2.5*(10^(-1)), # adjustment to make the timescales reasonable
   Td = 0.2, # Tcell depletion - ratio of pre-infected to post-infected equilibria
   N_S = 2, # Number of strains in system
   offStrains = c(2), # Strains not present in the initial system
   stochasticEventThresholdSource = function(){return(runif(1))}, # draw this number randomly in production situation
   Epow = c(0,   1,
            1,   0)
  )
  
  userParsLib[['AccuRes_1_2']] <- list(
   timeStep = 0.5,
   timeStop = 3500,
   systemName = "AccuRes_1_2",
   systemDescription = "3 strain system. Strain 1 very fit, but susceptible to treatment; strain 2 med fit, med susceptibility; strain 3: low fit los suscept",
   Pf = c(1, 0.95, 0.94), # Fitnesses of the different strains
   treatments = list(list(t=0, Te = 0.0, Ts = c(0.8, 0.8, 0.8)),
                     list(t=1000, Te = 0.1, Ts = c(1, 0.3, 0.0))
                     ),
   mutationAcceleration = 2.5*(10^(-1)), # adjustment to make the timescales reasonable
   Td = 0.2, # Tcell depletion - ratio of pre-infected to post-infected equilibria
   N_S = 3, # Number of strains in system
   offStrains = c(2,3), # Strains not present in the initial system
   stochasticEventThresholdSource = function(){return(runif(1))}, # draw this number randomly in production situation
   Epow = c(0,   2,   3,
            2,   0,   1,
            3,   1,   0)
  )
  
  userParsLib[['AccuTams_1_4']] <- list(
   timeStep = 0.5,
   timeStop = 3000,
   systemName = "AccTams_1_4",
   systemDescription = "5 strain system. Going from very fit but susceptible to treatment to (roughly) moderate fitness but high resistance to treatment",
   Pf = c(0.99, 0.96, 0.92, 0.91, 1), # Fitnesses of the different strains
   treatments = list(list(t=0, Te = 0.19, Ts = c(1, 0.75, 0.5, 0.1, 0))
                     ),
   mutationAcceleration = 1/200, # adjustment to make the timescales reasonable
   Td = 0.2, # Tcell depletion - ratio of pre-infected to post-infected equilibria
   N_S = 5, # Number of strains in system
   offStrains = c(2,3,4,5), # Strains not present in the initial system
   stochasticEventThresholdSource = function(){return(runif(1))}, # draw this number randomly in production situation
   Epow = c(0, 1, 2, 3, 4,
            1, 0, 1, 2, 3,
            2, 1, 0, 1, 2,
            3, 2, 1, 0, 1,
            4, 3, 2, 1, 0)
  )
  
  userParsLib[['AccuTams_1_3']] <- list(
   timeStep = 0.5,
   timeStop = 3000,
   systemName = "AccTams_1_3",
   systemDescription = "4 strain system. Going from very fit but susceptible to treatment to (roughly) moderate fitness but high resistance to treatment",
   Pf = c(0.99, 0.95, 0.93, 0.99), # Fitnesses of the different strains
   treatments = list(list(t=0, Te = 0.19, Ts = c(1, 0.75, 0.5, 0))
                     ),
   mutationAcceleration = 1/200, # adjustment to make the timescales reasonable
   Td = 0.2, # Tcell depletion - ratio of pre-infected to post-infected equilibria
   N_S = 4, # Number of strains in system
   offStrains = c(2,3,4), # Strains not present in the initial system
   stochasticEventThresholdSource = function(){return(runif(1))}, # draw this number randomly in production situation
   Epow = c(0, 1, 2, 3,
            1, 0, 1, 2,
            2, 1, 0, 1,
            3, 2, 1, 0)
  )
  
  userParsLib[['Systematic_Buildup']] <- list(
   timeStep = 1,
   timeStop = 10000,
   systemName = "Systematic_Buildup",
   systemDescription = "A Systematic build-up starting with one strain and adding ones slowly",
   Pf = c(0.5000003, 0.5000034), # Fitnesses of the different strains
   treatments = list(list(t=0, Te = 0, Ts = c(0))
                     ),
   mutationAcceleration = 1, # adjustment to make the timescales reasonable
   Td = 0.5, # Tcell depletion - ratio of pre-infected to post-infected equilibria
   N_S = 2, # Number of strains in system
   offStrains = c(2), # Strains not present in the initial system
   stochasticEventThresholdSource = function(){runif(1)}, # draw this number randomly in production situation
   Epow = c(0, 1,
            1, 0)
  )
  return(userParsLib)
}

#' Retrieves one of the pre-specified scenario specifications
#' 
#' This function is a poor way of storing pre-specified scenarios. This function is mainly a placeholder
#' for a mechanism that will store specified scenarios in a DB
#' 
#' @param scenario_name The name of the scenario to retrieve
#' @param modified_parameters A list of parameters to change. This will override the values from the stored scenario
#' @export

get_scenario <- function(scenario_name, modified_parameters = list()){
  userParsLib <- get_all_scenarios()
  if (scenario_name %in% names(userParsLib)){
    scenarioPars <- userParsLib[[scenario_name]]
    for (parameter_name in names(modified_parameters)){
      scenarioPars[[parameter_name]] <- modified_parameters[[parameter_name]]
    }
    return(do.call(scenario, scenarioPars))
  } else {
    stop("Scenario does not exist")
  }
}

#' Retrives the names of all the stored scenarios
#' 
#' @export

get_scenario_names <- function(){
  userParsLib <- get_all_scenarios()
  return(names(userParsLib))
}
