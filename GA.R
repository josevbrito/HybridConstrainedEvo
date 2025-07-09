# GA Padrão
GA_standard <- function(func, lb, ub, pop.size = 50, dimension = 10, max.it = 100, 
                       pc = 0.6, pm = 0.01, sel = 1, t.size = 4, elitism = TRUE){
  
  init <- init.population(func, lb, ub, pop.size, dimension)
  pop <- init$pop
  fitness <- init$fit
  
  best_history <- rep(NA, max.it)
  
  for(it in 1:max.it){
    # Seleção
    tmp.pop <- selection(pop, pop.size, dimension, fitness, sel, t.size)
    
    # Crossover
    tmp.pop <- arith.crossover(lb, ub, pop.size, dimension, tmp.pop, pc)
    
    # Mutação
    mutation_result <- uniform.mutation(func, lb, ub, tmp.pop, pop.size, dimension, pm)
    tmp.pop <- mutation_result$pop
    tmp.fitness <- mutation_result$fit
    
    # Elitismo
    if (elitism == TRUE){
      best.tmp <- min(tmp.fitness)
      best.old <- min(fitness)
      if (best.old < best.tmp){
        idx <- which.min(fitness)
        idx.worst <- which.max(tmp.fitness)
        tmp.pop[idx.worst,] <- pop[idx,]
        tmp.fitness[idx.worst] <- fitness[idx]
      }
    } 
    
    fitness <- tmp.fitness
    pop <- tmp.pop
    best_history[it] <- min(fitness)
  }
  
  return(list(pop = pop, fit = fitness, best_history = best_history))
}

# GA com agrupamento K-means
GA_kmeans <- function(func, lb, ub, pop.size = 50, dimension = 10, max.it = 100, 
                     pc = 0.6, pm = 0.01, sel = 1, t.size = 4, elitism = TRUE, 
                     cluster_freq = 10, n_clusters = 5){
  
  init <- init.population(func, lb, ub, pop.size, dimension)
  pop <- init$pop
  fitness <- init$fit
  
  best_history <- rep(NA, max.it)
  
  for(it in 1:max.it){
    # Agrupamento a cada cluster_freq gerações
    if(it %% cluster_freq == 0 && it > 1) {
      cluster_result <- apply_kmeans_diversity(pop, fitness, n_clusters)
      
      # Aqui substitui os piores indivíduos por indivíduos diversos
      n_replace <- min(nrow(cluster_result$pop), pop.size)
      worst_idx <- order(fitness, decreasing = TRUE)[1:n_replace]
      
      pop[worst_idx, ] <- cluster_result$pop[1:n_replace, ]
      fitness[worst_idx] <- cluster_result$fit[1:n_replace]
    }
    
    # Operações GA padrão
    tmp.pop <- selection(pop, pop.size, dimension, fitness, sel, t.size)
    tmp.pop <- arith.crossover(lb, ub, pop.size, dimension, tmp.pop, pc)
    
    mutation_result <- uniform.mutation(func, lb, ub, tmp.pop, pop.size, dimension, pm)
    tmp.pop <- mutation_result$pop
    tmp.fitness <- mutation_result$fit
    
    if (elitism == TRUE){
      best.tmp <- min(tmp.fitness)
      best.old <- min(fitness)
      if (best.old < best.tmp){
        idx <- which.min(fitness)
        idx.worst <- which.max(tmp.fitness)
        tmp.pop[idx.worst,] <- pop[idx,]
        tmp.fitness[idx.worst] <- fitness[idx]
      }
    } 
    
    fitness <- tmp.fitness
    pop <- tmp.pop
    best_history[it] <- min(fitness)
  }
  
  return(list(pop = pop, fit = fitness, best_history = best_history))
}

# GA com agrupamento Hierárquico
GA_hierarchical <- function(func, lb, ub, pop.size = 50, dimension = 10, max.it = 100, 
                           pc = 0.6, pm = 0.01, sel = 1, t.size = 4, elitism = TRUE, 
                           cluster_freq = 10, n_clusters = 5){
  
  init <- init.population(func, lb, ub, pop.size, dimension)
  pop <- init$pop
  fitness <- init$fit
  
  best_history <- rep(NA, max.it)
  
  for(it in 1:max.it){
    if(it %% cluster_freq == 0 && it > 1) {
      cluster_result <- apply_hierarchical_diversity(pop, fitness, n_clusters)
      
      n_replace <- min(nrow(cluster_result$pop), pop.size)
      worst_idx <- order(fitness, decreasing = TRUE)[1:n_replace]
      
      pop[worst_idx, ] <- cluster_result$pop[1:n_replace, ]
      fitness[worst_idx] <- cluster_result$fit[1:n_replace]
    }
    
    tmp.pop <- selection(pop, pop.size, dimension, fitness, sel, t.size)
    tmp.pop <- arith.crossover(lb, ub, pop.size, dimension, tmp.pop, pc)
    
    mutation_result <- uniform.mutation(func, lb, ub, tmp.pop, pop.size, dimension, pm)
    tmp.pop <- mutation_result$pop
    tmp.fitness <- mutation_result$fit
    
    if (elitism == TRUE){
      best.tmp <- min(tmp.fitness)
      best.old <- min(fitness)
      if (best.old < best.tmp){
        idx <- which.min(fitness)
        idx.worst <- which.max(tmp.fitness)
        tmp.pop[idx.worst,] <- pop[idx,]
        tmp.fitness[idx.worst] <- fitness[idx]
      }
    } 
    
    fitness <- tmp.fitness
    pop <- tmp.pop
    best_history[it] <- min(fitness)
  }
  
  return(list(pop = pop, fit = fitness, best_history = best_history))
}

# GA com agrupamento DBSCAN
GA_dbscan <- function(func, lb, ub, pop.size = 50, dimension = 10, max.it = 100, 
                     pc = 0.6, pm = 0.01, sel = 1, t.size = 4, elitism = TRUE, 
                     cluster_freq = 10, eps = 0.5, min_pts = 3){
  
  init <- init.population(func, lb, ub, pop.size, dimension)
  pop <- init$pop
  fitness <- init$fit
  
  best_history <- rep(NA, max.it)
  
  for(it in 1:max.it){
    if(it %% cluster_freq == 0 && it > 1) {
      cluster_result <- apply_dbscan_diversity(pop, fitness, eps, min_pts)
      
      n_replace <- min(nrow(cluster_result$pop), pop.size)
      worst_idx <- order(fitness, decreasing = TRUE)[1:n_replace]
      
      pop[worst_idx, ] <- cluster_result$pop[1:n_replace, ]
      fitness[worst_idx] <- cluster_result$fit[1:n_replace]
    }
    
    tmp.pop <- selection(pop, pop.size, dimension, fitness, sel, t.size)
    tmp.pop <- arith.crossover(lb, ub, pop.size, dimension, tmp.pop, pc)
    
    mutation_result <- uniform.mutation(func, lb, ub, tmp.pop, pop.size, dimension, pm)
    tmp.pop <- mutation_result$pop
    tmp.fitness <- mutation_result$fit
    
    if (elitism == TRUE){
      best.tmp <- min(tmp.fitness)
      best.old <- min(fitness)
      if (best.old < best.tmp){
        idx <- which.min(fitness)
        idx.worst <- which.max(tmp.fitness)
        tmp.pop[idx.worst,] <- pop[idx,]
        tmp.fitness[idx.worst] <- fitness[idx]
      }
    } 
    
    fitness <- tmp.fitness
    pop <- tmp.pop
    best_history[it] <- min(fitness)
  }
  
  return(list(pop = pop, fit = fitness, best_history = best_history))
}