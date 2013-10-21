\name{plot_system}
\alias{plot_system}
\title{Plots the system or an component of the system}
\usage{
  plot_system(plot_vars, components = "All",
    file_name = NULL)
}
\arguments{
  \item{plot_vars}{A solved system that has been formatted
  with \link{format_data}}

  \item{components}{A character vector that can take on any
  (or any combination) of the values (Strain, tCells or
  notMutation, All)}

  \item{file_name}{The name of the file into which the
  plots will be saved. If NULL, then the default plotting
  device will be used}
}
\description{
  Three different components can be plotted: 1) The viral
  strains 2) The healthy Tcell populations 3) The
  probability of not mutating
}

