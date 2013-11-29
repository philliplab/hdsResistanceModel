#' This function produces the shiny components that can be rendered as a UI
#' @param scenario The scenario to produce the ui from
#' @export
#scenario <- function(timeStep, timeStop, systemName, systemDescription, 
#                     kBase, treatments, N_S, 
#                     offStrains, stochasticEventThresholdSource, mutMat,
#                     mutationAcceleration = 1, Td = 0.5,
#                     er = 10^(-4), mu_T = 0.02, mu_P = 0.5, 
#                     S_T = 2 * 10^8, f = 0.37, 
#                     deathThreshold = 0.01, offThreshold = 0.1,
#                     deathModifier = 1.001, newStrainLevel = 1){

make_scenario_ui <- function(scenario){
  kBase <- scenario$kBase
  names(kBase) <- paste("Strain", 1:scenario$N_S, sep = "_")
  mutMat <- scenario$mutMat
  mutMat <- matrix(mutMat, nrow = sqrt(length(mutMat)), 
                   ncol = sqrt(length(mutMat)))
  mutMat <- data.frame(mutMat)
  names(mutMat) <- paste("Strain", 1:scenario$N_S, sep = "_")
  c(
    numericInput('timeStep', 'timeStep', scenario$timeStep),
    numericInput('timeStop', 'timeStop', scenario$timeStop),
    matrixInput('kBase', 'kBase', data.frame(as.list(kBase))),
    matrixInput('mutMat', 'mutMat', mutMat)
    )
}
