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

#' The event function. Manipulates the state and parameter functions of thee system
#' 
#' @param t time in the system
#' @param y state variables
#' @param parms The extra system parameters
#' @export

eventFunc <- function(t, y, parms){
  state <- y
  find_event_type <- function(t, state){
    event_type <- 'treatment_change_event'
    # I have no idea why there is an event at t==0
    if (t ==0) {event_type <- 'weird_first_event'}
    if (abs(state[N_S+2]-stochasticEventThreshold) < 0.001){
      event_type <- 'mutation_event'
    }
    if (any((state > deathThreshold) & (state < offThreshold))){
      event_type <- 'extinction_event'
    }
    return(event_type)
  }
  
  mutation_event <- function(){
    # parse arguments
    P <- matrix(state[1:N_S], nrow = N_S)
    Tc <- state[N_S+1]
    LAMBDA <- state[(N_S+2)]
    
    # find which strain appears
    mutationContributions <- (mutateDisc %*% (P * k *Tc))
    newStrain = sample(x = 1:N_S,
                       size = 1,
                       replace = FALSE,
                       prob = mutationContributions)
    stopifnot(newStrain %in% offStrains)
    
    # update the offStrains - remove the strain which appeared
    offStrains <<- offStrains[-match(newStrain, offStrains)]
    
    # rebuild the mutation rate matrices
    mutateCont <<- toggle_mutation_matrix(E, offStrains, type = 'continuous', N_S)
    mutateDisc <<- toggle_mutation_matrix(E, offStrains, type = 'discrete', N_S)
    stochasticEventThreshold <<- stochasticEventThresholdSource()
    
    # update and return the state variables with the new strain present
    state[newStrain] <- 1 # init the new strain
    state[N_S+2] <- 1 # reset the mutation counter
    return(state)
  }
  
  extinction_event <- function(){
    extinctStrain <- which((state > deathThreshold) & (state < offThreshold))
    state[extinctStrain] <- deathThreshold/deathModifier
    offStrains <<- c(offStrains, extinctStrain)
    mutateCont <<- toggle_mutation_matrix(E, offStrains, type = 'continuous', N_S)
    mutateDisc <<- toggle_mutation_matrix(E, offStrains, type = 'discrete', N_S)
    return (state)
  }
  
  treatment_change_event <- function(){
    treatmentChanges <- lapply(treatments, `[[`, 't')
    treatmentTriggered <- which(t > unlist(treatmentChanges))
    k <<- baseRate * Pf * (1 - treatments[[treatmentTriggered]]$Ts * treatments[[treatmentTriggered]]$Te) # effective per strain invasion rates
    treatments[[treatmentTriggered]] <<- NULL
    return (state)
  }
  weird_first_event <- function(){
    return(state)
  }
  
  event_type <- find_event_type(t, state)
  #print (c(t, event_type))
  
  if (event_type == 'weird_first_event'){
    state <- state
  }
  if (event_type == 'mutation_event'){
    state <- mutation_event()
  }
  if (event_type == 'extinction_event'){
    state <- extinction_event()
  }
  if (event_type == 'treatment_change'){
    state <- treatment_change_event()
  }
  # I cant get the switch to work - stick with the if's for now
#   switch(event_type,
#          'weird_first_event' = weird_first_event(),
#          'mutation_event' = mutation_event(),
#          'extinction_event' = extinction_event(),
#          'treatment_change_event' = treatment_change_event())
  return(state)
}

#' Run the whole system
#' 
#' Runs the differential equations using the scehario specification
#' 
#' @param scenario A list of parameter values specifying the system. As produced by \link{scenario}
#' @param seed The seed for the randomizer
#' @export
#' @examples
#' ss <- run_system(get_scenario('Simple_1_2'), 1)
#' pv <- format_data(ss)
#' plot_component(pv, "Strain")

run_system <- function(scenario, seed){
  set.seed(seed)
  
  params <- compute_parameters(scenario)
  
  with(params, {
    
    # I need to force the de_system's equations to be executed in the
    # current environment. This is because of how the event functions
    # are implemented in deSolve. In order to manipulate the PARAMETERS
    # of the system, you need to modify them in the parent environment.
    # They are not returned from the event function, so modifying them
    # in the parent environment is the only way I could think off to
    # have the event function manipulate them
    # In other words, deSolve's parameters functionality needs to be
    # completely side-stepped and scoping tricks are used instead.
    environment(stateFunc) <- environment()
    environment(rootFunc) <- environment()  
    environment(eventFunc) <- environment()
    environment(findSteadyState) <- environment()
    
    ss <- ode(times = seq(0, timeStop, timeStep),
              y = findSteadyState(),
              func = stateFunc,
              parms = NULL,
              rootfun = rootFunc,
              events = list(func = eventFunc, root = TRUE),
              method = 'lsoda'
    )
    ss <- data.frame(ss)
    return (ss)
  })
}
