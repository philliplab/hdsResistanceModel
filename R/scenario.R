#' Checks input list for problems and throw an error or returns a valid scenario specification
#' 
#' The main goal of this function is to perform sanity checks on a scenario specification and to provide
#' documentation for the input parameters
#'
#' When the effective invasion rates are computed kBase*(1 - Ts*A), then the effective invasion rates must
#' be greater than Td and smaller than 1.
#' Where Ts is the treatment suceptibility and A is the treatment effect
#' 
#' The mutMat mutation matrix contains the number of mutation event that must occur to mutate from
#' one strain to another. mutMat = [ E_ij ]. E_ii = 0 since no mutation is required if the exact same strain
#' is replicated. E_ij = 1 if a single mutation is required to mutate from strain i to strain j. Likewise
#' E_ij = 2 of two mutations are required to mutate from strain i to j and so forth. E_ij = E_ji since
#' mutating from strain i to j requires the same number of mutations as mutating from strain j to i.
#' Infinite values are allow to indicate impossible mutations
#'
#' mutationAcceleration and Td will both be retired. Setting them away from default values will cause
#' warnings.
#' 
#' @param timeStep The size of the steps in the output data
#' @param timeStop Run the system until this time
#' @param systemName A name for the system
#' @param systemDescription A description for the system
#' @param kBase Fitnesses of the different strains.
#' @param treatments The treatment specification. A list of lists. Each inner list is the details of a single treatment regine. When did it start (t), How how does the patient adhere (A) and how suceptible is each strain to this treatment? (Ts) (vector with susceptibility for each strain). The outer list loops over each regime change.
#' @param mutationAcceleration A factor that accellerates the rate of mutaton. Needed to get the timescales for when mutations arises right
#' @param Td Tcell depletion - ratio of pre-infected to post-infected equilibria. It also sets the scale for the acceptible range of the relative fitnesses of the strains.
#' @param N_S Number of strains in system
#' @param offStrains Strains not present in the initial system
#' @param stochasticEventThresholdSource A function that returns the threshold used to determine if a mutation occurred
#' @param mutMat The mutation matrix.
#' @param er The error rate when transcription occurs
#' @param mu_T The death rate for healthy T Cells
#' @param mu_P The death rate of infected T Cells
#' @param S_T The replenishment rate of T Cells
#' @param f The faithful transcription probability for a single virus replication
#' @param deathThreshold Any strain with a population below this will be treated as extinct
#' @param offThreshold Any strain that decreases below this level will be made extinct (by triggering an event) in the next time step.
#' @param deathModifier When a strain is made extinct, it is population is set to deathThreshold / deathModifier
#' @param newStrainLevel When a strain arises from a mutation, its initial population will be set to this value
#' @return A list with validated parameter settings
#' @export

scenario <- function(timeStep, timeStop, systemName, systemDescription, 
                     kBase, treatments, N_S, 
                     offStrains, stochasticEventThresholdSource, mutMat,
                     mutationAcceleration = 1, Td = 0.5,
                     er = 10^(-4), mu_T = 0.02, mu_P = 0.5, 
                     S_T = 2 * 10^8, f = 0.37, 
                     deathThreshold = 0.01, offThreshold = 0.1,
                     deathModifier = 1.001, newStrainLevel = 1){
  
  # check that numeric variables are numeric
  numeric_variables <- c("timeStep", "timeStop", "kBase", "N_S", "offStrains", 
                         "mutMat", "mutationAcceleration", "Td", "er", "mu_T",
                         "mu_P", "S_T", "f", "deathThreshold", "offThreshold",
                         "deathModifier", "newStrainLevel")
  for (numeric_variable in numeric_variables){
    numeric_variable_value <- get(numeric_variable)
    if(!is.numeric(numeric_variable_value)) {stop(str_c(numeric_variable, ' is not numeric'))}
  }

  if (deathThreshold > offThreshold) stop("deathThreshold > offThreshold")
  if (deathThreshold > newStrainLevel) stop("deathThreshold > newStrainLevel")
  if (offThreshold > newStrainLevel) stop("offThreshold > newStrainLevel")

  x <- mutMat[mutMat != Inf]
  if (!all(x == as.integer(x))) stop("Non-integer entries in mutation matrix")
  rm(x)

  if (mutationAcceleration != 1) warning("It is strongly recommended that mutationAcceleration be set to 1")
  if (Td != 0.5) warning("It is strongly recommended that Td be set to 0.5")
  
  # check timeStop / timeStep relationship
  if (timeStep > timeStop / 10){stop("timeStep must be < timeStop/10")}
  params <- list(
    timeStep = timeStep,
    timeStop = timeStop,
    systemName = systemName,
    systemDescription = systemDescription,
    kBase = kBase,
    treatments = treatments,
    mutationAcceleration = mutationAcceleration,
    Td = Td,
    N_S = N_S,
    offStrains = offStrains,
    stochasticEventThresholdSource = stochasticEventThresholdSource,
    mutMat = mutMat,
    er = er, # general error rate / substitution rate
    mu_T = mu_T, # Deathrate of healthy cells
    mu_P = mu_P, # Deathrate of infected cells
    S_T = S_T, # Tcell replenishment rate
    f = f, # faithful replication rate
    deathThreshold = deathThreshold, # switch off dynamics if state variable below this
    offThreshold = offThreshold, # If a strain goes below this, put it in the offStrains
    deathModifier = deathModifier, # A strain is set to deathThreshold / deathModifier when it is extinct
    newStrainLevel = newStrainLevel
    )
}
