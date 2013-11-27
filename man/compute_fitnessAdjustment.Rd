\name{compute_fitnessAdjustment}
\alias{compute_fitnessAdjustment}
\title{This function computes the fitnessAdjustment}
\usage{
  compute_fitnessAdjustment(kBase, treatments,
    treatment_num = 1)
}
\arguments{
  \item{kBase}{Fitnesses of the different strains.}

  \item{treatments}{The treatment specification. A list of
  lists. Each inner list is the details of a single
  treatment regine. When did it start (t), How how does the
  patient adhere (A) and how suceptible is each strain to
  this treatment? (Ts) (vector with susceptibility for each
  strain). The outer list loops over each regime change.}

  \item{treatment_num}{The number of the treatment to use
  for the computation}
}
\description{
  This function computes the fitnessAdjustment
}

