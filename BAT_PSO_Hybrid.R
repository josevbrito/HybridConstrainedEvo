BAT_PSO_Hybrid <- function(func, lb, ub, pop.size = 50, dimension = 10, max.it = 100,
                           # Parâmetros do Bat Algorithm
                           A = 0.2422741,
                           r = 0.8776998,
                           Qmin = 0, Qmax = 2,
                           alpha = 0.9295661,
                           gamma = 0.8822333,
                           # Parâmetros do PSO
                           w = 0.8339623,
                           c1 = 0.8904942,
                           c2 = 2.2953754,
                           w_damp = 0.99,
                           # Parâmetro Híbrido
                           prob_bat = 0.3877847) {

  # Inicialização da população (morcegos/partículas)
  bats <- matrix(runif(pop.size * dimension), nrow = pop.size)
  bats <- lb + bats * (ub - lb)

  # Velocidades
  velocity <- matrix(0, nrow = pop.size, ncol = dimension)

  # Frequências (para Bat Algorithm)
  frequency <- rep(0, pop.size)

  # Avaliação inicial
  fitness <- apply(bats, 1, func)

  # Melhor posição pessoal (para PSO)
  pbest <- bats
  pbest_fitness <- fitness

  # Melhor posição global (para PSO e Bat Algorithm)
  gbest_idx <- which.min(fitness)
  gbest <- bats[gbest_idx, ]
  gbest_fitness <- fitness[gbest_idx]

  # Parâmetros dos morcegos
  loudness <- rep(A, pop.size)
  pulse_rate <- rep(r, pop.size)

  # Histórico de convergência
  best_history <- rep(NA, max.it)

  current_w <- w # Peso de inércia atual

  for(it in 1:max.it) {
    for(i in 1:pop.size) {
      # Decisão probabilística
      if (runif(1) < prob_bat) {
        # === Atualização inspirada no Bat Algorithm ===
        frequency[i] <- Qmin + (Qmax - Qmin) * runif(1)
        velocity[i, ] <- velocity[i, ] + (bats[i, ] - gbest) * frequency[i] 
        
        # Limita a velocidade
        max_vel <- abs(ub - lb) * 0.5
        velocity[i, ] <- pmax(pmin(velocity[i, ], max_vel), -max_vel)
        
        # Nova solução candidata
        new_position <- bats[i, ] + velocity[i, ]

        if(runif(1) > pulse_rate[i]) {
          # Perturbação em torno da melhor solução global
          new_position <- gbest + loudness[i] * runif(dimension, -1, 1)
        }
        
      } else {
        # === Atualização inspirada no PSO ===
        r1 <- runif(dimension)
        r2 <- runif(dimension)

        velocity[i, ] <- current_w * velocity[i, ] +
                         c1 * r1 * (pbest[i, ] - bats[i, ]) +
                         c2 * r2 * (gbest - bats[i, ])

        # Limita a velocidade
        max_vel <- abs(ub - lb) * 0.5
        velocity[i, ] <- pmax(pmin(velocity[i, ], max_vel), -max_vel)
        
        # Nova solução candidata
        new_position <- bats[i, ] + velocity[i, ]
      }
      
      # Aplicação dos limites à nova posição
      new_position <- pmax(pmin(new_position, ub), lb) 
      
      # Avalia a nova posição
      current_fitness <- func(new_position)

      # === Decisão de aceitação ===
      # Aceita a nova posição se ela for melhor que a atual do indivíduo
      if (current_fitness < fitness[i]) {
        bats[i, ] <- new_position
        fitness[i] <- current_fitness
        
        if (runif(1) < prob_bat) {
            loudness[i] <- alpha * loudness[i]
            pulse_rate[i] <- r * (1 - exp(-gamma * it))
        }
      }

      # Atualização do melhor pessoal (pbest)
      if (current_fitness < pbest_fitness[i]) {
        pbest[i, ] <- new_position # Atualiza pbest com a nova posição melhor
        pbest_fitness[i] <- current_fitness
      }

      # Atualização do melhor global (gbest)
      if (current_fitness < gbest_fitness) {
        gbest <- new_position
        gbest_fitness <- current_fitness
      }
    }

    # Redução do peso da inércia para PSO
    current_w <- current_w * w_damp

    # Registro do melhor fitness na iteração
    best_history[it] <- gbest_fitness
  }

  return(list(pop = bats, fit = fitness, best_history = best_history,
              best_bat = gbest, best_fitness = gbest_fitness))
}