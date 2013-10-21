#' Names and reshapes the data so that it is easy to plot
#' 
#' @param solved_system The output from ode that needs to be formatted
#' @export

format_data <- function(solved_system){
  names(solved_system) <- c("time", paste("Strain", 1:(ncol(solved_system)-3), sep = " "), "tCells", "notMutation")
  
  plotVars <- melt(solved_system, id = "time")
  
  return(plotVars)
}