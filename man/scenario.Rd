\name{scenario}
\alias{scenario}
\title{Checks input list for problems and throw an error or returns a valid scenario specification}
\usage{
  scenario(timeStep, timeStop, systemName,
    systemDescription, Pf, treatments,
    mutationAcceleration, Td, N_S, offStrains,
    stochasticEventThresholdSource, Epow, er = 10^(-4),
    mu_T = 0.02, mu_P = 0.5, S_T = 2 * 10^8, f = 0.37,
    deathThreshold = 0.01, offThreshold = 0.1)
}
\arguments{
  \item{timeStep}{The size of the steps in the output data}

  \item{timeStop}{Run the system until this time}

  \item{systemName}{A name for the system}

  \item{systemDescription}{A description for the system}

  \item{Pf}{Fitnesses of the different strains}

  \item{treatments}{The treatment specification. A list of
  lists. Each inner list is the details of a single
  treatment regine. When did it start (t), What is its
  effect (Te) and how suceptible is each strain to this
  treatment? (Ts) (vector with susceptibility for each
  strain). The outer list loops over each regime change.}

  \item{mutationAcceleration}{A factor that accellerates
  the rate of mutaton. Needed to get the timescales for
  when mutations arises right}

  \item{Td}{Tcell depletion - ratio of pre-infected to
  post-infected equilibria. It also sets the scale for the
  acceptible range of the relative fitnesses of the
  strains.}

  \item{N_S}{Number of strains in system}

  \item{offStrains}{Strains not present in the initial
  system}

  \item{stochasticEventThresholdSource}{A function that
  returns the threshold used to determine if a mutation
  occurred}

  \item{Epow}{The mutation matrix.}
}
\description{
  The main goal of this function is to perform sanity
  checks on a scenario specification and to provide
  documentation for the input parametes
}

