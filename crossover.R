simple.crossover <- function(lb, ub, pop.size, dimension, pop, pc){
  new.pop <- matrix(rep(NA, pop.size * dimension), nrow = pop.size)
  r <- runif(pop.size)
  idx <- (r < pc)
  selected_pop <- pop[idx, , drop = FALSE]
  tmp.pop.size <- nrow(selected_pop)
  
  if(tmp.pop.size < 2) return(pop)
  
  offspring_count <- 1
  for(i in seq(1, tmp.pop.size - 1, by = 2)){  
    if(offspring_count > pop.size) break
    
    cross.point <- sample(2:(dimension-1), 1)
    offspring1 <- c(selected_pop[i, 1:cross.point], selected_pop[i+1, (cross.point+1):dimension])
    if(offspring_count + 1 <= pop.size) {
      offspring2 <- c(selected_pop[i+1, 1:cross.point], selected_pop[i, (cross.point+1):dimension])
      new.pop[offspring_count + 1, ] <- offspring2
    }
    new.pop[offspring_count, ] <- offspring1
    offspring_count <- offspring_count + 2
  }
  
  # Preenche posições restantes com a população original
  for(i in offspring_count:pop.size) {
    if(i <= pop.size) new.pop[i, ] <- pop[i, ]
  }
  
  return(new.pop)
}

# Crossover Aritmético
arith.crossover <- function(lb, ub, pop.size, dimension, pop, pc){
  new.pop <- matrix(rep(NA, pop.size * dimension), nrow = pop.size)
  r <- runif(pop.size)
  idx <- (r < pc)
  selected_pop <- pop[idx, , drop = FALSE]
  tmp.pop.size <- nrow(selected_pop)
  
  if(tmp.pop.size < 2) return(pop)
  
  offspring_count <- 1
  for(i in seq(1, tmp.pop.size - 1, by = 2)){  
    if(offspring_count > pop.size) break
    
    l <- runif(1)
    offspring1 <- l * selected_pop[i, ] + (1-l) * selected_pop[i+1, ]
    if(offspring_count + 1 <= pop.size) {
      offspring2 <- (1-l) * selected_pop[i, ] + l * selected_pop[i+1, ]
      new.pop[offspring_count + 1, ] <- offspring2
    }
    new.pop[offspring_count, ] <- offspring1
    offspring_count <- offspring_count + 2
  }
  
  # Preenche posições restantes
  for(i in offspring_count:pop.size) {
    if(i <= pop.size) new.pop[i, ] <- pop[i, ]
  }
  
  return(new.pop)
}
