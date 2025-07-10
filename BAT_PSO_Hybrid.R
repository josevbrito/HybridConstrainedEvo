BAT_PSO_Hybrid <- function(func, lb, ub, pop.size = 50, dimension = 10, max.it = 100,
                           A = 0.5, r = 0.5, Qmin = 0, Qmax = 2, alpha = 0.9, gamma = 0.9,
                           w = 0.9, c1 = 2.0, c2 = 2.0, w_damp = 0.99) {

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
      # === Atualização inspirada no Bat Algorithm ===
      frequency[i] <- Qmin + (Qmax - Qmin) * runif(1)
      velocity[i, ] <- velocity[i, ] + (bats[i, ] - gbest) * frequency[i] # Usa gbest aqui

      # Busca local (para Bat Algorithm)
      if(runif(1) > pulse_rate[i]) {
        # Perturbação em torno da melhor solução global
        new_bat_local_search <- gbest + loudness[i] * runif(dimension, -1, 1)
        new_bat_local_search <- pmax(pmin(new_bat_local_search, ub), lb) # Limites
        
        # Avalia a solução da busca local antes de possivelmente usá-la
        local_search_fitness <- func(new_bat_local_search)
        
        # Só aceita a solução da busca local se for melhor que a atual e dentro da probabilidade
        if (local_search_fitness < fitness[i] && runif(1) < loudness[i]) {
            bats[i, ] <- new_bat_local_search
            fitness[i] <- local_search_fitness
            
            # Atualiza parâmetros de loudness e pulse_rate se a busca local foi bem sucedida
            loudness[i] <- alpha * loudness[i]
            pulse_rate[i] <- r * (1 - exp(-gamma * it))
        }
      }

      # === Atualização inspirada no PSO ===
      r1 <- runif(dimension)
      r2 <- runif(dimension)

      velocity[i, ] <- current_w * velocity[i, ] +
                       c1 * r1 * (pbest[i, ] - bats[i, ]) +
                       c2 * r2 * (gbest - bats[i, ])

      # Limita a velocidade (opcional, mas recomendado para PSO)
      max_vel <- abs(ub - lb) * 0.5
      velocity[i, ] <- pmax(pmin(velocity[i, ], max_vel), -max_vel)

      # Atualiza a posição da partícula/morcego
      new_position <- bats[i, ] + velocity[i, ]
      new_position <- pmax(pmin(new_position, ub), lb) # Aplica limites

      # Avalia a nova posição
      current_fitness <- func(new_position)

      # === Decisão de aceitação (híbrido) ===
      # Uma estratégia é aceitar a nova posição se ela for melhor que a atual,
      # combinando a lógica de aceitação de ambas as meta-heurísticas.
      # Aqui, priorizamos a melhoria do fitness.
      if (current_fitness < fitness[i]) {
        bats[i, ] <- new_position
        fitness[i] <- current_fitness
      }

      # Atualização do melhor pessoal (pbest)
      if (current_fitness < pbest_fitness[i]) {
        pbest[i, ] <- new_position # Atualiza pbest com a nova posição melhor
        pbest_fitness[i] <- current_fitness
      }

      # Atualização do melhor global (gbest)
      if (current_fitness < gbest_fitness) {
        gbest <- new_position # Atualiza gbest com a nova posição melhor
        gbest_fitness <- current_fitness
      }
    }

    # Redução do peso da inércia para PSO
    current_w <- w * w_damp

    # Registro do melhor fitness na iteração
    best_history[it] <- gbest_fitness
  }

  return(list(pop = bats, fit = fitness, best_history = best_history,
              best_bat = gbest, best_fitness = gbest_fitness))
}