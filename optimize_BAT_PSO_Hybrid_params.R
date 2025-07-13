source('BAT_PSO_Hybrid.R')
source('Benchmarks.R')

param_ranges <- list(
  A = c(0.1, 1.0),
  r = c(0.0, 1.0),
  alpha = c(0.8, 0.99),
  gamma = c(0.8, 0.99),
  w = c(0.4, 0.9),
  c1 = c(0.5, 2.5),
  c2 = c(0.5, 2.5),
  prob_bat = c(0.1, 0.9)
)

target_function <- Schwefel
target_lb <- rep(-500, 10)
target_ub <- rep(500, 10)
target_dimension <- 10

evaluate_params <- function(params_vector, num_runs = 5) {
  set.seed(42)
  
  A_val <- params_vector["A"]
  r_val <- params_vector["r"]
  alpha_val <- params_vector["alpha"]
  gamma_val <- params_vector["gamma"]
  w_val <- params_vector["w"]
  c1_val <- params_vector["c1"]
  c2_val <- params_vector["c2"]
  prob_bat_val <- params_vector["prob_bat"]
  
  fitness_results <- rep(NA, num_runs)
  
  for (i in 1:num_runs) {
    result <- BAT_PSO_Hybrid(
      func = target_function,
      lb = target_lb,
      ub = target_ub,
      dimension = target_dimension,
      max.it = 200,
      A = A_val,
      r = r_val,
      alpha = alpha_val,
      gamma = gamma_val,
      w = w_val,
      c1 = c1_val,
      c2 = c2_val,
      prob_bat = prob_bat_val
    )
    fitness_results[i] <- result$best_fitness
  }
  
  return(mean(fitness_results))
}

num_iterations_tuning <- 50
best_overall_params <- NULL
best_overall_fitness <- Inf

cat("Iniciando otimização de parâmetros para BAT_PSO_Hybrid na função Schwefel...\n")

for (it in 1:num_iterations_tuning) {
  current_params_vector <- sapply(param_ranges, function(range) {
    runif(1, min = range[1], max = range[2])
  })
  
  cat(paste0("  Iteração de Tuning ", it, "/", num_iterations_tuning, " - Testando parâmetros: \n"))
  print(current_params_vector)
  
  current_fitness_mean <- evaluate_params(current_params_vector)
  
  cat(paste0("    Fitness médio resultante: ", current_fitness_mean, "\n"))
  
  if (current_fitness_mean < best_overall_fitness) {
    best_overall_fitness <- current_fitness_mean
    best_overall_params <- current_params_vector
    cat(paste0("    NOVO MELHOR ENCONTRADO! Fitness: ", best_overall_fitness, "\n"))
  }
}

cat("\n--- Otimização de Parâmetros Concluída ---\n")
cat("Melhores Parâmetros Encontrados:\n")
print(best_overall_params)
cat(paste0("Melhor Fitness Médio com esses Parâmetros: ", best_overall_fitness, "\n"))
