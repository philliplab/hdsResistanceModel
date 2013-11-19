#' This function produces the shiny components that can be rendered as a UI
#' @param scenario The scenario to produce the ui from
#' @export

make_scenario_ui <- function(scenario){
  numericInput('timeStop', 'timeStop', scenario$timeStop)
}
