\name{steady_state_kBase_relationship}
\alias{steady_state_kBase_relationship}
\title{Express the relationship kBase = f(steady state) under specific circumstances}
\usage{
  steady_state_kBase_relationship(P0s, scenario)
}
\arguments{
  \item{P0s}{the steady state level(s) of the strain. If
  length(P0s) > 1, a vector of fitnesses will be returned,
  one for each steady state level.}

  \item{scenario}{The parameters that specify the scenario}
}
\description{
  The specific circumstances are: 1) There is only 1 strain
  in the system 2) There is no treatment
}
\examples{
as.character(steady_state_kBase_relationship(1000, get_scenario('tc_Simple_1_2')))
# [1] "0.800005405441929"

plot(steady_state_kBase_relationship(seq(100000, 10000000, 100000), get_scenario('tc_Simple_1_2')))
}

