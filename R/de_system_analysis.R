#' Solves the system for the steady state
#' 
#' Needs a scenario to be loaded in the environment in which it is executed
#' 
#' @export
#' @return A vector with the state variable in the steady state
#' @examples
#' simple_1_2 <- get_scenario('tc_Simple_1_2')
#' simple_1_2_params <- compute_parameters(simple_1_2)
#' initial_values <- with(simple_1_2_params,{
#'   environment(findSteadyState) <- environment()
#'   findSteadyState()
#' })
#' print(initial_values)
#' # [1] 2.336842e+07 9.990010e-03 9.990010e-03 8.421053e+09 1.000000e+00

findSteadyState <- function(){
  P <- ((S_T * f) / (mu_P)) - (mu_T / k)
  P <- P/(N_S - length(offStrains))
  P[offStrains] <- deathThreshold/deathModifier
  Tc <- mu_P / (f * k[1])
  LAMBDA <- 1
  initVec <- c(P, Tc, LAMBDA)
  return(initVec)
}

#' Express the relationship steady state = f(Pf) under specific circumstances
#' 
#' The specific circumstances are:
#' 1) There is only 1 strain in the system
#' 2) There is no treatment
#' 
#' @param Pf The relative fitness of the strain
#' @param scenario The parameters that specify the scenario
#' @export
#' @examples
#' Pf_steady_state_relationship(0.95, get_scenario('tc_Simple_1_2'))
#' # [1]   23368421 8421052632          1

Pf_steady_state_relationship <- function(Pf, scenario){
  scenario$Pf <- Pf
  scenario$treatments <- list(list(t=0, A = 0, Ts = 0))
  scenario$N_S <- 1
  scenario$offStrains <- numeric(0)
  scenario$Epow <- 0
  params <- compute_parameters(scenario)
  initial_values <- with(params,{
    environment(findSteadyState) <- environment()
    findSteadyState()
 })
  return(initial_values)
}

#' Express the relationship Pf = f(steady state) under specific circumstances
#' 
#' The specific circumstances are:
#' 1) There is only 1 strain in the system
#' 2) There is no treatment
#' 
#' @param P0s the steady state level(s) of the strain. If length(P0s) > 1, a vector of fitnesses will be returned, one for each steady state level.
#' @param scenario The parameters that specify the scenario
#' @export
#' 
#' @examples
#' as.character(steady_state_Pf_relationship(1000, get_scenario('tc_Simple_1_2')))
#' # [1] "0.800005405441929"
#' 
#' plot(steady_state_Pf_relationship(seq(100000, 10000000, 100000), get_scenario('tc_Simple_1_2')))

steady_state_Pf_relationship <- function(P0s, scenario){
  solution <- numeric(0)
  for (steady_state in P0s){
    foo <- function(x, scenario){
      Pf_steady_state_relationship(x, scenario)[1] - steady_state
    }
    solution <- c(solution, uniroot(foo, c(1-scenario$Td, 1), scenario = scenario, tol = 0.1^20)$root)
  }
  return(solution)
}

#' Computes the time it takes a strain to grow from a lower threshold to an upper threshold
#' 
#' Given a solved system, a strain number and a lower and upper threshold, compute the time it takes
#' to grow from the one to the other. Returns NA if that growth did not occur in the system. 
#' 
#' @param ss A solved system as produced by run_system
#' @param strain_id The number of strain whose growth rates are of interest
#' @param lower The lower threshold - the number where the growth that needs to be tracked started
#' @param upper The upper threshold - the growth is tracked until this threshold is reached
#' @export
#' @examples
#' ss <- run_system(get_scenario('tc_Simple_1_2'),1)
#' calc_growth_time(ss, 2, 2, 100)
#' #147
#' calc_growth_time(ss, 2, 2, 1000)
#' #235
#' calc_growth_time(ss, 3, 2, 1000)
#' #NA
#' calc_growth_time(ss, 3, 2, 5)
#' #150

calc_growth_time <- function(ss, strain_id, lower, upper){
  if(upper <= lower){
    stop("Lower threshold is above the upper threshold")
  }
  strain_pop <- ss[,strain_id+1] # First column is time
  time <- ss$time
  suppressWarnings({
    first_row_over_lower <- min(which(strain_pop > lower))
    first_row_over_upper <- min(which(strain_pop > upper))
  })
  
  if((first_row_over_lower == Inf)|(first_row_over_lower == Inf)){
    return (NA)
  } 
  
  time_taken <- time[first_row_over_upper] - time[first_row_over_lower]
  return(time_taken)
}

#' Computes the stable populations that exists in a solved system
#'
#' Mostly for testing purposes
#'
#' @param ss A solved system as produced by run_system
#' @param strains A vector of the strain numbers to use in the computation. Can also be 'all' in which case all strains will be used. Strains are labelled from 1 to n.
#' @param timeAfter Only time points after the value for this variable will be used.
#' @param timeBefore If it is greater than 0, then only time points before this value will be used.
#' @param comparison_magnitude If two subsequent values of the state variables are within 10^-comparison_magnitude of each other, they will be seen as equal.
#' @export

compute_stable_populations <- function(ss, strains = 'all', timeAfter = 0, timeBefore = 0,
                                       comparison_magnitude = 4){
  if (timeBefore > 0){
    ss <- ss[ss$time < timeBefore,]
  }
  ss <- ss[ss$time > timeAfter,]
  if (strains == 'all'){
    strains <- 1:(ncol(ss)-3) # first col time; last col not_mutate; 2nd last col = healthy Tcells
  }
  strains <- strains + 1
  nrows <- nrow(ss)
  stable_values <- list()
  for (strain in strains){
    derivative <- ss[1:(nrows-1), strain] - ss[2:nrows, strain]
    zero_derivatives <- ss[which(abs(derivative) < 10^(-comparison_magnitude)), strain]
    stable_pops <- table(round(zero_derivatives, comparison_magnitude))
    stable_values[[str_c('Strain ', strain-1)]] <- stable_pops
  }
  return(stable_values)
}

#' Compute the approximate offThreshold - the value below which strains are set to extinct
#'
#' Mostly for testing
#'
#' @param ss A solved system as produced by run_system
#' @param scenario_spec The specification of the scenario
#' @export

compute_offThreshold <- function(ss, scenario_spec){
  deathValue <- with(scenario_spec, deathThreshold / deathModifier)
  strains <- 2:(ncol(ss)-2) # first col time; last col not_mutate; 2nd last col = healthy Tcells
  extinction_points <- NULL
  for (strain in strains){
    stepped_strain <- data.frame(no_lag = ss[1:(nrow(ss)-1), strain],
                                 lagged = ss[2:nrow(ss), strain])
    extinction <- stepped_strain[(stepped_strain$no_lag > deathValue) & (abs(stepped_strain$lagged - deathValue) < 0.0001),]
    if (nrow(extinction) > 0){
      extinction_points <- c(extinction_points, extinction$no_lag)
    }
  }
  return (extinction_points)
}

#' Compute the newStrainLevel
#'
#' Mostly for testing
#'
#' @param ss A solved system as produced by run_system
#' @param scenario_spec The specification of the scenario
#' @export

compute_newStrainLevel <- function(ss, scenario_spec){
  deathValue <- with(scenario_spec, deathThreshold / deathModifier)
  strains <- 2:(ncol(ss)-2) # first col time; last col not_mutate; 2nd last col = healthy Tcells
  new_strain_levels <- NULL
  for (strain in strains){
    stepped_strain <- data.frame(no_lag = ss[1:(nrow(ss)-1), strain],
                                 lagged = ss[2:nrow(ss), strain])
    new_strain_level <- stepped_strain[(stepped_strain$lagged > 1.1*deathValue) & (abs(stepped_strain$no_lag - deathValue) < 0.0001),]
    if (nrow(new_strain_level) > 0){
      new_strain_levels <- c(new_strain_levels, new_strain_level$lagged)
    }
  }
  return (new_strain_levels)
}


