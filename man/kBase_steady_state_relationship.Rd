\name{kBase_steady_state_relationship}
\alias{kBase_steady_state_relationship}
\title{Express the relationship steady state = f(kBase) under specific circumstances}
\usage{
  kBase_steady_state_relationship(kBase, scenario)
}
\arguments{
  \item{kBase}{The relative fitness of the strain}

  \item{scenario}{The parameters that specify the scenario}
}
\description{
  The specific circumstances are: 1) There is only 1 strain
  in the system 2) There is no treatment
}
\examples{
kBase_steady_state_relationship(0.95, get_scenario('tc_Simple_1_2'))
# [1]   23368421 8421052632          1
}

