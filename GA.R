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

GA_Penalized <- function(func, lb, ub, pop.size = 50, dimension = 10, max.it = 100, 
                        pc = 0.6, pm = 0.01, sel = 1, t.size = 4, elitism = TRUE,
                        k_penalty = 0.1,
                        p_penalty = 1.0,
                        violation_func) {

  init <- init.population(func, lb, ub, pop.size, dimension)
  pop <- init$pop
  
  # Avaliação inicial do fitness bruto (sem penalidade)
  fitness_raw <- apply(pop, 1, func)
  
  # Calcula violações e fitness penalizado para a primeira geração
  violations <- apply(pop, 1, violation_func)
  average_fitness_generation <- mean(fitness_raw)
  
  # Termo de penalidade dinâmico
  # P = k * (t/T)^p * f_bar * sum(d_i)
  current_penalty_factor <- k_penalty * (1 / max.it)^p_penalty
  penalty_term <- current_penalty_factor * average_fitness_generation * violations
  fitness <- fitness_raw + penalty_term

  best_history <- rep(NA, max.it
  
  for(it in 1:max.it){
    # Seleção
    tmp.pop <- selection(pop, pop.size, dimension, fitness, sel, t.size)
    
    # Crossover
    tmp.pop <- arith.crossover(lb, ub, pop.size, dimension, tmp.pop, pc)
    
    # Mutação
    mutation_result <- uniform.mutation(func, lb, ub, tmp.pop, pop.size, dimension, pm)
    tmp.pop <- mutation_result$pop
    
    # Recalcula fitness bruto e violações para a nova população
    fitness_raw_tmp <- apply(tmp.pop, 1, func)
    violations_tmp <- apply(tmp.pop, 1, violation_func)
    
    # Calcula o termo de penalidade dinâmico para a iteração atual
    average_fitness_generation <- mean(fitness_raw_tmp)
    current_penalty_factor <- k_penalty * (it / max.it)^p_penalty
    penalty_term_tmp <- current_penalty_factor * average_fitness_generation * violations_tmp
    fitness_tmp <- fitness_raw_tmp + penalty_term_tmp

    # Elitismo (compara fitness penalizado)
    if (elitism == TRUE){
      best.tmp <- min(fitness_tmp)
      best.old <- min(fitness)
      
      if (best.old < best.tmp){
        idx <- which.min(fitness)
        idx.worst <- which.max(fitness_tmp)
        tmp.pop[idx.worst,] <- pop[idx,]
        fitness_tmp[idx.worst] <- fitness[idx]
      }
    } 
    
    fitness <- fitness_tmp
    pop <- tmp.pop
    
    best_history[it] <- min(fitness)
  }
  
  # No final, encontrar a melhor solução *viável* (se houver) ou a menos inviável
  final_violations <- apply(pop, 1, violation_func)
  feasible_indices <- which(final_violations < 1e-6)
  
  best_solution_final <- NA
  best_fitness_final <- NA

  if (length(feasible_indices) > 0) {
    # Se encontrou soluções viáveis, pega a melhor entre elas (fitness bruto)
    best_idx_feasible <- feasible_indices[which.min(fitness_raw[feasible_indices])]
    best_solution_final <- pop[best_idx_feasible, ]
    best_fitness_final <- fitness_raw[best_idx_feasible]
  } else {
    # Se não houver solução viável, retorna a solução com menor fitness penalizado
    # e seu fitness bruto (para avaliação do objetivo, mesmo que inviável)
    best_idx_overall <- which.min(fitness)
    best_solution_final <- pop[best_idx_overall, ]
    best_fitness_final <- fitness_raw[best_idx_overall]
  }

  return(list(pop = pop, fit = fitness, best_history = best_history,
              best_solution_final = best_solution_final, best_fitness_final = best_fitness_final,
              final_violations = final_violations[which(fitness == min(fitness))][1]))
}