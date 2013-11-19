\name{scenario}
\alias{scenario}
\title{Checks input list for problems and throw an error or returns a valid scenario specification}
\usage{
  scenario(timeStep, timeStop, systemName,
    systemDescription, Pf, treatments, N_S, offStrains,
    stochasticEventThresholdSource, Epow,
    mutationAcceleration = 1, Td = 0.5, er = 10^(-4),
    mu_T = 0.02, mu_P = 0.5, S_T = 2 * 10^8, f = 0.37,
    deathThreshold = 0.01, offThreshold = 0.1,
    deathModifier = 1.001, newStrainLevel = 1)
}
\arguments{
  \item{timeStep}{The size of the steps in the output data}

  \item{timeStop}{Run the system until this time}

  \item{systemName}{A name for the system}

  \item{systemDescription}{A description for the system}

  \item{Pf}{Fitnesses of the different strains.}

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

  \item{er}{The error rate when transcription occurs}

  \item{mu_T}{The death rate for healthy T Cells}

  \item{mu_P}{The death rate of infected T Cells}

  \item{S_T}{The replenishment rate of T Cells}

  \item{f}{The faithful transcription probability for a
  single virus replication}

  \item{deathThreshold}{Any strain with a population below
  this will be treated as extinct}

  \item{offThreshold}{Any strain that decreases below this
  level will be made extinct (by triggering an event) in
  the next time step.}

  \item{deathModifier}{When a strain is made extinct, it is
  population is set to deathThreshold / deathModifier}

  \item{newStrainLevel}{When a strain arises from a
  mutation, its initial population will be set to this
  value}
}
\value{
  A list with validated parameter settings
}
\description{
  The main goal of this function is to perform sanity
  checks on a scenario specification and to provide
  documentation for the input parameters
}
\details{
  When the effective invasion rates are computed Pf*(1 -
  Ts*Te), then the effective invasion rates must be greater
  than Td and smaller than 1. Where Ts is the treatment
  suceptibility and Te is the treatment effect

  The Epow mutation matrix contains the number of mutation
  event that must occur to mutate from one strain to
  another. Epow = [ E_ij ]. E_ii = 0 since no mutation is
  required if the exact same strain is replicated. E_ij = 1
  if a single mutation is required to mutate from strain i
  to strain j. Likewise E_ij = 2 of two mutations are
  required to mutate from strain i to j and so forth. E_ij
  = E_ji since mutating from strain i to j requires the
  same number of mutations as mutating from strain j to i.
  Infinite values are allow to indicate impossible
  mutations

  mutationAcceleration and Td will both be retired. Setting
  them away from default values will cause warnings.
}

