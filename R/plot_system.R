#' Plots a component of the system
#' 
#' @param plot_vars A solved system that has been formatted with \link{format_data}
#' @param component A character vector that can take on any of the values (Strain, tCells or notMutation)
#' @export

plot_component <- function(plot_vars, component = 'Strain'){
  stopifnot(all(component %in% c('Strain', 'tCells', 'notMutation')))
  stopifnot(length(component) == 1)
  component_data <- plot_vars[grep(component, plot_vars[,2]),]
  if (component == 'notMutation'){
    return(
      ggplot(data = component_data,
             aes(x = time,
                 y = value,
                 group = variable,
                 color = variable)) +
        geom_line())
  } else {
    return(
      ggplot(data = component_data,
             aes(x = time,
                 y = log(value, base=10),
                 group = variable,
                 color = variable)) +
        geom_line())
  }
}

#' Plots the system or an component of the system
#' 
#' Three different components can be plotted:
#' 1) The viral strains
#' 2) The healthy Tcell populations
#' 3) The probability of not mutating
#' 
#' @param plot_vars A solved system that has been formatted with \link{format_data}
#' @param components A character vector that can take on any (or any combination) of the values (Strain, tCells or notMutation, All)
#' @param file_name The name of the file into which the plots will be saved. If NULL, then the default plotting device will be used 
#' @export

plot_system <- function(plot_vars, components = 'All', file_name = NULL){
  stopifnot(all(components %in% c('Strain', 'tCells', 'notMutation', 'All')))
  if (!is.null(file_name)){
    pdf(file_name, onefile = TRUE)
  }
  if ('All' %in% components){
    components <- c('Strain', 'tCells', 'notMutation')
  }
  for (component in components){
    print(plot_component(plot_vars, component))      
  }
  if (!is.null(file_name)){
    dev.off()
  }
}
