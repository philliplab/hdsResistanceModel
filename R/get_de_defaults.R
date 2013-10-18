#' Returns the default values for some parameters of the model.
#' 
#' Defaults can be overwritten by setting the desired value in the call.
#' 
#' @param er general error rate / substitution rate
#' @param mu_T Deathrate of healthy cells
#' @param mu_P Deathrate of infected cells
#' @param S_T Tcell replenishment rate
#' @param f faithful replication rate
#' @param deathThreshold switch off dynamics if state variable below this
#' @param offThreshold If a strain goes below this, put it in the offStrains
#' @export

get_de_defaults <- function(er = 10^(-4), mu_T = 0.02, mu_P = 0.5, 
                            S_T = 2 * 10^8, f = 0.37, 
                            deathThreshold = 0.01, offThreshold = 0.1){
  constants <- list(
    er = er, # general error rate / substitution rate
    mu_T = mu_T, # Deathrate of healthy cells
    mu_P = mu_P, # Deathrate of infected cells
    S_T = S_T, # Tcell replenishment rate
    f = f, # faithful replication rate
    deathThreshold = deathThreshold, # switch off dynamics if state variable below this
    offThreshold = offThreshold # If a strain goes below this, put it in the offStrains
  )
  return(constants)  
}