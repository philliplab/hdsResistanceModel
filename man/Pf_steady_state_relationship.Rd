\name{Pf_steady_state_relationship}
\alias{Pf_steady_state_relationship}
\title{Express the relationship steady state = f(Pf) under specific circumstances}
\usage{
  Pf_steady_state_relationship(Pf, de_defaults, scenario)
}
\arguments{
  \item{Pf}{The relative fitness of the strain}

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
Pf_steady_state_relationship(0.95, get_de_defaults(), get_scenario('Simple_1_2'))
# [1]   23368421 8421052632          1
}

