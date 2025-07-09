BatAlgorithm <- function(func, lb, ub, pop.size = 50, dimension = 10, max.it = 100,
                        A = 0.5, r = 0.5, Qmin = 0, Qmax = 2, alpha = 0.9, gamma = 0.9) {
  
  # População de morcegos
  bats <- matrix(runif(pop.size * dimension), nrow = pop.size)
  bats <- lb + bats * (ub - lb)
  
  # Velocidades
  velocity <- matrix(0, nrow = pop.size, ncol = dimension)
  
  # Frequências
  frequency <- rep(0, pop.size)
  
  # Avaliação inicial
  fitness <- apply(bats, 1, func)
  
  # Melhor solução
  best_idx <- which.min(fitness)
  best_bat <- bats[best_idx, ]
  best_fitness <- fitness[best_idx]
  
  # Parâmetros dos morcegos
  loudness <- rep(A, pop.size)
  pulse_rate <- rep(r, pop.size)
  
  # Histórico de convergência
  best_history <- rep(NA, max.it)
  
  for(it in 1:max.it) {
    for(i in 1:pop.size) {
      # Atualização da frequência
      frequency[i] <- Qmin + (Qmax - Qmin) * runif(1)
      
      # Atualização da velocidade
      velocity[i, ] <- velocity[i, ] + (bats[i, ] - best_bat) * frequency[i]
      
      # Nova solução
      new_bat <- bats[i, ] + velocity[i, ]
      
      # Busca local
      if(runif(1) > pulse_rate[i]) {
        new_bat <- best_bat + loudness[i] * runif(dimension, -1, 1)
      }
      
      # Aplicação dos limites
      new_bat <- pmax(pmin(new_bat, ub), lb)
      
      # Avaliação
      new_fitness <- func(new_bat)
      
      # Aceitação da nova solução
      if(new_fitness < fitness[i] && runif(1) < loudness[i]) {
        bats[i, ] <- new_bat
        fitness[i] <- new_fitness
        
        # Atualização dos parâmetros
        loudness[i] <- alpha * loudness[i]
        pulse_rate[i] <- r * (1 - exp(-gamma * it))
        
        # Atualização do melhor
        if(new_fitness < best_fitness) {
          best_bat <- new_bat
          best_fitness <- new_fitness
        }
      }
    }
    
    best_history[it] <- best_fitness
  }
  
  return(list(pop = bats, fit = fitness, best_history = best_history,
              best_bat = best_bat, best_fitness = best_fitness))
}