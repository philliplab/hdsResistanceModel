#' Runs the shiny app for this package
#' 
#' @export

run_hdsResistanceModel_app <- function(){
  packageDir <- find.package('hdsResistanceModel')
  shinyAppDir <- file.path(packageDir, "www") 
  runApp(shinyAppDir)
}
