\name{compute_stable_populations}
\alias{compute_stable_populations}
\title{Computes the stable populations that exists in a solved system}
\usage{
  compute_stable_populations(ss, strains = "all",
    timeAfter = 0, timeBefore = 0,
    comparison_magnitude = 4)
}
\arguments{
  \item{ss}{A solved system as produced by run_system}

  \item{strains}{A vector of the strain numbers to use in
  the computation. Can also be 'all' in which case all
  strains will be used. Strains are labelled from 1 to n.}

  \item{timeAfter}{Only time points after the value for
  this variable will be used.}

  \item{timeBefore}{If it is greater than 0, then only time
  points before this value will be used.}

  \item{comparison_magnitude}{If two subsequent values of
  the state variables are within 10^-comparison_magnitude
  of each other, they will be seen as equal.}
}
\description{
  Mostly for testing purposes
}

