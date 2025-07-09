uniform.mutation <- function(func, lb, ub, pop, pop.size, dimension, pm){
  r <- matrix(runif(pop.size * dimension), nrow = pop.size)
  fitness <- rep(NA, pop.size)
  idx <- which(r < pm, arr.ind = TRUE)
  
  if(nrow(idx) > 0) {
    pop[idx] <- lb[idx[,2]] + runif(nrow(idx)) * (ub[idx[,2]] - lb[idx[,2]])
  }
  
  fitness <- apply(pop, 1, func)
  return(list(pop = pop, fit = fitness))
}