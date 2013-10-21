#' Solves the system for the steady state
#' 
#' Needs a scenario to be loaded in the environment in which it is executed
#' 
#' @export

findSteadyState <- function(){
  P <- ((S_T * f) / (mu_P)) - (mu_T / k)
  P <- P/(N_S - length(offStrains))
  P[offStrains] <- deathThreshold/1.001
  Tc <- mu_P / (f * k[1])
  LAMBDA <- 1
  initVec <- c(P, Tc, LAMBDA)
  return(initVec)
}