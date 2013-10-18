\name{toggle_mutation_matrix}
\alias{toggle_mutation_matrix}
\title{Toggles the mutation matrix to reflect mutation and extinction events.}
\usage{
  toggle_mutation_matrix(E, strains, type, N_S)
}
\arguments{
  \item{E}{The mutation matrix}

  \item{strains}{The indices of the strains that must be
  switched off}

  \item{type}{The type of the matrix to return continuous
  or discrete}

  \item{N_S}{The number of strains in the matrix not
  strictly required - should this parameter be removed?}
}
\description{
  It takes a full matrix and switches off specific strains
}
\details{
  The mutation matrix contains the probabilities that each
  strain will mutate into another strain in a single
  replication. The continuous version of the matrix is used
  in the equations for the dynamical part of the system.
  The discrete version of the matrix is used in the
  equation that computes the probability of a mutation not
  arising.

  If a strain is extinct (or off) then it is not part of
  the dynamical system, so all elements in both the rows
  and columns of the extinct strains are set to zero.

  If a strain is extinct, then it does not reproduce and as
  such does not produce mutations. Thus the elements in the
  columns of the extinct strains are set to zero. If a
  strain is not extinct, then the probability of it
  mutating from another strain is tracked in the dynamic
  portion of the equations and not in the stochastic
  portions of the equations. Thus all elements in the rows
  of strains that are NOT extinct is set to zero.
}

