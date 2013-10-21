hdsResistanceModel
==================

A Hybrid Dynamic/Stochastic Model of HIV Resistance Evolution

## Installation

This is an R project, so within an R session, enter the following commands:

```r
install.packages('devtools') # Install the devtools package for the install_github function

install_github('hdsResistanceModel', 'philliplab')

library(hdsResistanceModel)
```

The package and all its dependancies should not be installed.

## Basic Examples

```r
help(package = 'hdsResistanceModel')
```

This will list all the functions exposed by this package. The documentation is minimal at the moment, but each function has a brief description and all the parameters used by each function is also described.

```r
solved_system <- run_system(get_scenario('AccuTams_1_3'), 3)

plotVars <- format_data(solved_system)

plot_system(plotVars, 'All', 'plots.pdf')
```

The three commands above will run the 'AccuTams_1_3' system and plot it in a file called plots.pdf in the current working directory.

All example scenarios can be listed using the `get_scenario_names` function.