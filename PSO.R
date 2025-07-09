PSO <- function(func, lb, ub, pop.size = 50, dimension = 10, max.it = 100, 
                w = 0.9, c1 = 2.0, c2 = 2.0, w_damp = 0.99) {
  
  # Partículas
  particles <- matrix(runif(pop.size * dimension), nrow = pop.size)
  particles <- lb + particles * (ub - lb)
  
  # Velocidades
  velocity <- matrix(runif(pop.size * dimension, -1, 1), nrow = pop.size)
  velocity <- velocity * abs(ub - lb) * 0.1
  
  # Avaliação inicial
  fitness <- apply(particles, 1, func)
  
  # Melhor posição pessoal
  pbest <- particles
  pbest_fitness <- fitness
  
  # Melhor posição global
  gbest_idx <- which.min(fitness)
  gbest <- particles[gbest_idx, ]
  gbest_fitness <- fitness[gbest_idx]
  
  # Histórico de convergência
  best_history <- rep(NA, max.it)
  
  for(it in 1:max.it) {
    for(i in 1:pop.size) {
      r1 <- runif(dimension)
      r2 <- runif(dimension)
      
      velocity[i, ] <- w * velocity[i, ] + 
                       c1 * r1 * (pbest[i, ] - particles[i, ]) +
                       c2 * r2 * (gbest - particles[i, ])
      
      max_vel <- abs(ub - lb) * 0.5
      velocity[i, ] <- pmax(pmin(velocity[i, ], max_vel), -max_vel)
      
      particles[i, ] <- particles[i, ] + velocity[i, ]
      particles[i, ] <- pmax(pmin(particles[i, ], ub), lb)
      
      # Avaliação
      current_fitness <- func(particles[i, ])
      
      # Atualização do melhor pessoal
      if(current_fitness < pbest_fitness[i]) {
        pbest[i, ] <- particles[i, ]
        pbest_fitness[i] <- current_fitness
        
        # Atualização do melhor global
        if(current_fitness < gbest_fitness) {
          gbest <- particles[i, ]
          gbest_fitness <- current_fitness
        }
      }
    }
    
    # Redução do peso da inércia
    w <- w * w_damp
    
    # Registro do melhor fitness
    best_history[it] <- gbest_fitness
  }
  
  return(list(pop = particles, fit = pbest_fitness, best_history = best_history,
              gbest = gbest, gbest_fitness = gbest_fitness))
}