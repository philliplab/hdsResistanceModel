\name{stateFunc}
\alias{stateFunc}
\title{The state function of the system.}
\usage{
  stateFunc(t, y, parms)
}
\arguments{
  \item{t}{time in the system}

  \item{y}{state variables}

  \item{parms}{The extra system parameters}
}
\description{
  It computes the deriviatives of each viral strain, i, as
  dPi = ki * Pi * Tc + Eij * kj * Pj * Tc - mu_P * Pi where
  j does not equal i
}
\details{
  I computes the derivative of the healthy Tcell
  populations and the derivative of not having a stochastic
  event
}

