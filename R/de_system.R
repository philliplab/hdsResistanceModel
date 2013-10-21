#' The state function of the system.
#' 
#' It computes the deriviatives of each viral strain, i, as 
#' dPi = ki * Pi * Tc + Eij * kj * Pj * Tc - mu_P * Pi
#' where j does not equal i
#' 
#' I computes the derivative of the healthy Tcell populations and the derivative of not having a
#' stochastic event
#' 
#' @param t time in the system
#' @param y state variables
#' @param parms The extra system parameters
#' @export

stateFunc <- function(t, y, parms){
  state <- y
  # parse arguments
  P <- matrix(state[1:N_S], ncol = 1)
  Tc <- state[N_S+1]
  LAMBDA <- state[(N_S+2)]
  
  # specify differential equations
  dP <- mutateCont %*% (k * P * Tc) - mu_P * P
  dTc <- S_T - Tc * sum(k * P) - mu_T*Tc
  dLAMBDA <- -(sum(mutateDisc %*% (P * k *Tc)) * LAMBDA)
  
  # apply strain extinction
  dP[offStrains] <- 0
  
  return(list(c(dP, dTc, dLAMBDA)))
}

#' This function searched for roots in the system
#' 
#' There are three types of roots:
#' 1) Treatment was changed
#' 2) A strain goes extinct
#' 3) A mutation event happens
#' 
#' @param t time in the system
#' @param y state variables
#' @param parms The extra system parameters
#' @export

rootFunc <- function(t, y, parms){
  state <- y
  
  # Does treatment change?
  treatmentChanges <- lapply(treatments, `[[`, 't')
  if (length(treatmentChanges)!=0){
    if (any(t > unlist(treatmentChanges))){
      return(0)
    }
  }
  
  # Does a strain go extinct?
  if (any((state > deathThreshold) & (state < offThreshold))){
    return(0)
  } 
  
  # Finally, does a mutation event happen?
  mutationEvent <- state[N_S+2]-stochasticEventThreshold
  return(mutationEvent)
}