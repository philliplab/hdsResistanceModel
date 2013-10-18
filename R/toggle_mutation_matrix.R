#' Toggles the mutation matrix to reflect mutation and extinction events.
#' 
#' It takes a full matrix and switches off specific strains
#' 
#' The mutation matrix contains the probabilities that each strain will mutate into another strain in
#' a single replication. The continuous version of the matrix is used in the equations for the dynamical
#' part of the system. The discrete version of the matrix is used in the equation that computes
#' the probability of a mutation not arising.
#' 
#' If a strain is extinct (or off) then it is not part of the dynamical system, so all elements in both
#' the rows and columns of the extinct strains are set to zero.
#' 
#' If a strain is extinct, then it does not reproduce and as such does not produce mutations. Thus the
#' elements in the columns of the extinct strains are set to zero. If a strain is not extinct, then the
#' probability of it mutating from another strain is tracked in the dynamic portion of the equations and
#' not in the stochastic portions of the equations. Thus all elements in the rows of strains that are
#' NOT extinct is set to zero.
#' 
#' @param E The mutation matrix
#' @param strains The indices of the strains that must be switched off
#' @param type The type of the matrix to return continuous or discrete
#' @param N_S The number of strains in the matrix not strictly required - should this parameter be removed?
#' @export

toggle_mutation_matrix <- function(E, strains, type, N_S){
  if(!(type %in% c('continuous', 'discrete'))){
    stop("Invalid value for type")
  }
  if(class(E)!="matrix"){
    stop("E is not a matrix")
  }
  if(!((nrow(E) == ncol(E)) &
        nrow(E) == N_S)){
    stop("Dimensions implied by E, strains and N_S does not match1")
  }
  if(!length(strains)==0){
    if (min(strains)<1 | max(strains) > N_S) {
      stop("Dimensions implied by E, strains and N_S does not match2")
    }
  }
  newMat <- E
  if (type == 'continuous'){
    newMat[,strains] <- 0
    newMat[strains,] <- 0
  }
  if (type == 'discrete'){
    newMat[,strains] <- 0
    newMat[!(1:N_S %in% strains),] <- 0
  }
  return (newMat)
}
