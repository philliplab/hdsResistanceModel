\name{run_system}
\alias{run_system}
\title{Run the whole system}
\usage{
  run_system(scenario, seed)
}
\arguments{
  \item{scenario}{A list of parameter values specifying the
  system. As produced by \link{scenario}}

  \item{seed}{The seed for the randomizer}
}
\description{
  Runs the differential equations using the defaults and
  the scehario
}
\examples{
ss <- run_system(get_scenario('Simple_1_2'), 1)
pv <- format_data(ss)
plot_component(pv, "Strain")
}

