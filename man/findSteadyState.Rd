\name{findSteadyState}
\alias{findSteadyState}
\title{Solves the system for the steady state}
\usage{
  findSteadyState()
}
\value{
  A vector with the state variable in the steady state
}
\description{
  Needs a scenario to be loaded in the environment in which
  it is executed
}
\examples{
simple_1_2 <- get_scenario('Simple_1_2')
simple_1_2_params <- compute_parameters(get_de_defaults(), simple_1_2)
initial_values <- with(simple_1_2_params,{
  environment(findSteadyState) <- environment()
  findSteadyState()
})
print(initial_values)
# [1] 2.336842e+07 9.990010e-03 9.990010e-03 8.421053e+09 1.000000e+00
}

