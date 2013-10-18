\name{get_de_defaults}
\alias{get_de_defaults}
\title{Returns the default values for some parameters of the model.}
\usage{
  get_de_defaults(er = 10^(-4), mu_T = 0.02, mu_P = 0.5,
    S_T = 2 * 10^8, f = 0.37, deathThreshold = 0.01,
    offThreshold = 0.1)
}
\arguments{
  \item{er}{general error rate / substitution rate}

  \item{mu_T}{Deathrate of healthy cells}

  \item{mu_P}{Deathrate of infected cells}

  \item{S_T}{Tcell replenishment rate}

  \item{f}{faithful replication rate}

  \item{deathThreshold}{switch off dynamics if state
  variable below this}

  \item{offThreshold}{If a strain goes below this, put it
  in the offStrains}
}
\description{
  Defaults can be overwritten by setting the desired value
  in the call.
}

