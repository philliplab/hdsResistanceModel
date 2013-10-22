#' Solves the system for the steady state
#' 
#' Needs a scenario to be loaded in the environment in which it is executed
#' 
#' @export
#' @return A vector with the state variable in the steady state
#' @examples
#' simple_1_2 <- get_scenario('Simple_1_2')
#' simple_1_2_params <- compute_parameters(get_de_defaults(), simple_1_2)
#' initial_values <- with(simple_1_2_params,{
#'   environment(findSteadyState) <- environment()
#'   findSteadyState()
#' })
#' print(initial_values)
#' # [1] 2.336842e+07 9.990010e-03 9.990010e-03 8.421053e+09 1.000000e+00

findSteadyState <- function(){
  P <- ((S_T * f) / (mu_P)) - (mu_T / k)
  P <- P/(N_S - length(offStrains))
  P[offStrains] <- deathThreshold/1.001
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
#' @param de_defaults The parameters that are unlikely to change from scenario to scenario, as produced by get_de_defaults
#' @param scenario The parameters that specify the scenario
#' @export
#' @examples
#' Pf_steady_state_relationship(0.95, get_de_defaults(), get_scenario('Simple_1_2'))
#' # [1]   23368421 8421052632          1

Pf_steady_state_relationship <- function(Pf, de_defaults, scenario){
  scenario$Pf <- Pf
  scenario$treatments <- list(list(t=0, Te = 0, Ts = 0))
  scenario$N_S <- 1
  scenario$offStrains <- numeric(0)
  scenario$Epow <- 0
  params <- compute_parameters(de_defaults, scenario)
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
#' @param de_defaults The parameters that are unlikely to change from scenario to scenario, as produced by get_de_defaults
#' @param scenario The parameters that specify the scenario
#' @export
#' 
#' @examples
#' as.character(steady_state_Pf_relationship(1000, get_de_defaults(), get_scenario('Simple_1_2')))
#' # [1] "0.800005405441929"
#' 
#' plot(steady_state_Pf_relationship(seq(100000, 10000000, 100000), get_de_defaults(), get_scenario('Simple_1_2')))

steady_state_Pf_relationship <- function(P0s, de_defaults, scenario){
  solution <- numeric(0)
  for (steady_state in P0s){
    foo <- function(x, de_defaults, scenario){
      Pf_steady_state_relationship(x, de_defaults, scenario)[1] - steady_state
    }
    solution <- c(solution, uniroot(foo, c(1-scenario$Td, 1), de_defaults = de_defaults, scenario = scenario, tol = 0.1^20)$root)
  }
  return(solution)
}