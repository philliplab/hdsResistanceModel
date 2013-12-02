#' Constructs the treatment part of the ui from a scenario
#'
#' @param scenario The scenario to produce the ui from
#' @export

treatment_ui_from_scenario <- function(scenario){
  treatment_ui <- NULL
  i <- 0
  for (treatment in scenario$treatments){
    i <- i + 1
    treatment_ui <- c(treatment_ui, 
      numericInput(str_c('treatment_t', i),
                   str_c('Start Time for Treatment Regimen ', i),
                   treatment$t),
      numericInput(str_c('treatment_A', i),
                   str_c('Adherence to Treatment Regimen ', i),
                   treatment$A),
      matrixInput(str_c('treatment_Ts',i),
                  str_c('Susceptibilities to Regimen ', i),
                  data.frame(as.list(treatment$Ts)))
      )
  }
  return(treatment_ui)
}
