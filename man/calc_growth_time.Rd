\name{calc_growth_time}
\alias{calc_growth_time}
\title{Computes the time it takes a strain to grow from a lower threshold to an upper threshold}
\usage{
  calc_growth_time(ss, strain_id, lower, upper)
}
\arguments{
  \item{ss}{A solved system as produced by run_system}

  \item{strain_id}{The number of strain whose growth rates
  are of interest}

  \item{lower}{The lower threshold - the number where the
  growth that needs to be tracked started}

  \item{upper}{The upper threshold - the growth is tracked
  until this threshold is reached}
}
\description{
  Given a solved system, a strain number and a lower and
  upper threshold, compute the time it takes to grow from
  the one to the other. Returns NA if that growth did not
  occur in the system.
}
\examples{
ss <- run_system(get_scenario('tc_Simple_1_2'),1)
calc_growth_time(ss, 2, 2, 100)
#147
calc_growth_time(ss, 2, 2, 1000)
#235
calc_growth_time(ss, 3, 2, 1000)
#NA
calc_growth_time(ss, 3, 2, 5)
#150
}

