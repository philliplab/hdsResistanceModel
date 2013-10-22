\name{steady_state_Pf_relationship}
\alias{steady_state_Pf_relationship}
\title{Express the relationship Pf = f(steady state) under specific circumstances}
\usage{
  steady_state_Pf_relationship(P0s, de_defaults, scenario)
}
\arguments{
  \item{P0s}{the steady state level(s) of the strain. If
  length(P0s) > 1, a vector of fitnesses will be returned,
  one for each steady state level.}

  \item{de_defaults}{The parameters that are unlikely to
  change from scenario to scenario, as produced by
  get_de_defaults}

  \item{scenario}{The parameters that specify the scenario}
}
\description{
  The specific circumstances are: 1) There is only 1 strain
  in the system 2) There is no treatment
}
\examples{
as.character(steady_state_Pf_relationship(1000, get_de_defaults(), get_scenario('Simple_1_2')))
# [1] "0.800005405441929"

plot(steady_state_Pf_relationship(seq(100000, 10000000, 100000), get_de_defaults(), get_scenario('Simple_1_2')))
}

