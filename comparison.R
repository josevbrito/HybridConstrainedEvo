run_comparison <- function() {
  set.seed(123)
  
  # Parâmetros experimentais
  execs <- 30
  pop.size <- 50
  max.it <- 500
  dimension <- 10
  
  # Funções de teste
  functions <- list(
    Sphere = list(func = Sphere, lb = -5.12, ub = 5.12, name = "Esfera (Monomodal)"),
    Schwefel = list(func = Schwefel, lb = -500, ub = 500, name = "Schwefel (Multimodal)")
  )
  
  # Algoritmos para comparação
  algorithms <- c("GA_Padrão", "GA_KMeans", "GA_Hierárquico", "GA_DBSCAN", "PSO", "Bat_Algorithm", "BAT_PSO_Hybrid")

  # Armazenamento de resultados
  results <- array(NA, dim = c(length(functions), length(algorithms), execs))
  convergence_data <- list()

  cat("Iniciando experimento de comparação (GA vs PSO vs Bat Algorithm vs BAT-PSO Híbrido)...\n")
  
  for(f_idx in 1:length(functions)) {
    func_info <- functions[[f_idx]]
    func <- func_info$func
    lb <- rep(func_info$lb, dimension)
    ub <- rep(func_info$ub, dimension)
    
    cat("Testando função:", func_info$name, "\n")
    
    for(alg_idx in 1:length(algorithms)) {
      algorithm <- algorithms[alg_idx]
      cat("  Algoritmo:", algorithm, "\n")
      
      for(exec in 1:execs) {
        if(exec %% 10 == 0) cat("    Execução:", exec, "\n")
        
        # Execução dos algoritmos
        if(algorithm == "GA_Padrão") {
          result <- GA_standard(func, lb, ub, pop.size, dimension, max.it)
        } else if(algorithm == "GA_KMeans") {
          result <- GA_kmeans(func, lb, ub, pop.size, dimension, max.it)
        } else if(algorithm == "GA_Hierárquico") {
          result <- GA_hierarchical(func, lb, ub, pop.size, dimension, max.it)
        } else if(algorithm == "GA_DBSCAN") {
          result <- GA_dbscan(func, lb, ub, pop.size, dimension, max.it)
        } else if(algorithm == "PSO") {
          result <- PSO(func, lb, ub, pop.size, dimension, max.it)
        } else if(algorithm == "Bat_Algorithm") {
          result <- BatAlgorithm(func, lb, ub, pop.size, dimension, max.it)
        } else if(algorithm == "BAT_PSO_Hybrid") {
          result <- BAT_PSO_Hybrid(func, lb, ub, pop.size, dimension, max.it)
        }
        
        # Armazenamento do melhor resultado
        if(algorithm == "PSO") {
          results[f_idx, alg_idx, exec] <- result$gbest_fitness
        } else if (algorithm %in% c("Bat_Algorithm", "BAT_PSO_Hybrid")) {
          results[f_idx, alg_idx, exec] <- result$best_fitness
        } else { # For GA algorithms
          results[f_idx, alg_idx, exec] <- min(result$fit)
        }
        
        # Armazenamento dos dados de convergência para a primeira execução
        if(exec == 1) {
          convergence_key <- paste(func_info$name, algorithm, sep = "_")
          convergence_data[[convergence_key]] <- result$best_history
        }
      }
    }
  }
  
  return(list(results = results, convergence_data = convergence_data, 
              functions = functions, algorithms = algorithms))
}

perform_statistical_analysis <- function(experiment_results) {
  results <- experiment_results$results
  functions <- experiment_results$functions
  algorithms <- experiment_results$algorithms
  
  cat("\n============================================================================\n")
  cat("ANÁLISE ESTATÍSTICA - GA vs PSO vs BAT ALGORITHM vs BAT-PSO HÍBRIDO\n")
  cat("============================================================================\n")
  
  for(f_idx in 1:length(functions)) {
    func_name <- names(functions)[f_idx]
    cat("\nFunção:", functions[[f_idx]]$name, "\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    
    # Extração dos resultados para essa função
    func_results <- results[f_idx, , ]
    
    # Estatísticas de resumo
    cat("\nEstatísticas de Resumo:\n")
    cat(sprintf("%-15s %12s %12s %12s %12s %12s\n", 
                "Algoritmo", "Média", "Desvio Padrão", "Mínimo", "Máximo", "Mediana"))
    cat(paste(rep("-", 80), collapse = ""), "\n")
    
    for(alg_idx in 1:length(algorithms)) {
      alg_results <- func_results[alg_idx, ]
      cat(sprintf("%-15s %12.6f %12.6f %12.6f %12.6f %12.6f\n", 
                  algorithms[alg_idx], mean(alg_results), sd(alg_results), 
                  min(alg_results), max(alg_results), median(alg_results)))
    }
    
    # Ranking médio
    cat("\nRanking Médio (1 = melhor):\n")
    rankings <- matrix(NA, nrow = length(algorithms), ncol = ncol(func_results))
    for(exec in 1:ncol(func_results)) {
      rankings[, exec] <- rank(func_results[, exec])
    }
    avg_rankings <- rowMeans(rankings)
    ranking_order <- order(avg_rankings)
    
    cat(sprintf("%-15s %12s\n", "Algoritmo", "Ranking Médio"))
    cat(paste(rep("-", 30), collapse = ""), "\n")
    for(i in ranking_order) {
      cat(sprintf("%-15s %12.2f\n", algorithms[i], avg_rankings[i]))
    }
    
    # Teste ANOVA
    cat("\nTeste ANOVA:\n")
    data_for_anova <- data.frame(
      value = as.vector(func_results),
      algorithm = rep(algorithms, each = ncol(func_results))
    )
    
    anova_result <- aov(value ~ algorithm, data = data_for_anova)
    anova_summary <- summary(anova_result)
    print(anova_summary)
    
    # Teste HSD de Tukey se ANOVA for significativo
    p_value <- anova_summary[[1]][["Pr(>F)"]][1]
    if(!is.na(p_value) && p_value < 0.05) {
      cat("\nTeste HSD de Tukey (comparações pareadas):\n")
      tukey_result <- TukeyHSD(anova_result)
      print(tukey_result)
      
      # Análise de significância
      cat("\nResumo das Diferenças Significativas (p < 0.05):\n")
      tukey_data <- tukey_result$algorithm
      significant <- tukey_data[tukey_data[, "p adj"] < 0.05, ]
      if(nrow(significant) > 0) {
        for(i in 1:nrow(significant)) {
          comparison <- rownames(significant)[i]
          p_val <- significant[i, "p adj"]
          diff <- significant[i, "diff"]
          cat(sprintf("  %s: diferença = %.6f, p = %.6f\n", comparison, diff, p_val))
        }
      } else {
        cat("  Nenhuma diferença significativa encontrada.\n")
      }
    } else {
      cat("\nANOVA não significativa (p >= 0.05), ignorando o teste de Tukey.\n")
    }
  }
  
  # Análise geral de desempenho
  cat("\n============================================================================\n")
  cat("ANÁLISE GERAL DE DESEMPENHO\n")
  cat("============================================================================\n")
  
  overall_rankings <- matrix(NA, nrow = length(algorithms), ncol = length(functions))
  for(f_idx in 1:length(functions)) {
    func_results <- results[f_idx, , ]
    rankings <- matrix(NA, nrow = length(algorithms), ncol = ncol(func_results))
    for(exec in 1:ncol(func_results)) {
      rankings[, exec] <- rank(func_results[, exec])
    }
    overall_rankings[, f_idx] <- rowMeans(rankings)
  }
  
  final_rankings <- rowMeans(overall_rankings)
  final_order <- order(final_rankings)
  
  cat("\nRanking Geral Consolidado:\n")
  cat(sprintf("%-15s %12s\n", "Algoritmo", "Ranking Médio"))
  cat(paste(rep("-", 30), collapse = ""), "\n")
  for(i in final_order) {
    cat(sprintf("%-15s %12.2f\n", algorithms[i], final_rankings[i]))
  }
}


create_visualization <- function(experiment_results) {
  convergence_data <- experiment_results$convergence_data
  functions <- experiment_results$functions
  algorithms <- experiment_results$algorithms
  results <- experiment_results$results
  
  # Configuração para os gráficos
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  
  # Gráficos de convergência
  colors <- c("black", "red", "blue", "green", "purple", "orange", "darkgreen")
  
  for(f_idx in 1:length(functions)) {
    func_name <- names(functions)[f_idx]
    
    # Extração dos dados de convergência
    func_convergence <- list()
    for(alg in algorithms) {
      key <- paste(functions[[func_name]]$name, alg, sep = "_")
      if(key %in% names(convergence_data)) {
        func_convergence[[alg]] <- convergence_data[[key]]
      }
    }
    
    if(length(func_convergence) > 0) {
      max_length <- max(sapply(func_convergence, length))
      
      # Gráfico de convergência
      plot(1:max_length, func_convergence[[1]], type = "l", col = colors[1], 
           xlab = "Geração", ylab = "Melhor Fitness", 
           main = paste("Convergência -", functions[[func_name]]$name),
           ylim = range(unlist(func_convergence), na.rm = TRUE), lwd = 2)
      
      for(i in 2:length(func_convergence)) {
        if(length(func_convergence[[i]]) > 0) {
          lines(1:length(func_convergence[[i]]), func_convergence[[i]], 
                col = colors[i], lwd = 2)
        }
      }
      
      legend("topright", legend = names(func_convergence), 
             col = colors[1:length(func_convergence)], lwd = 2, cex = 0.8)
    }
    
    # Box plot dos resultados finais
    func_results <- results[f_idx, , ]
    boxplot(t(func_results), names = algorithms, 
            main = paste("Distribuição dos Resultados -", functions[[func_name]]$name),
            xlab = "Algoritmo", ylab = "Fitness Final",
            las = 2, cex.axis = 0.8)
  }
}