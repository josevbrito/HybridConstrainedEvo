run_comparison <- function() {
  set.seed(123)
  
  # Parâmetros experimentais
  execs <- 30
  pop.size <- 50
  max.it <- 500
  dimension_unconstrained <- 10
  dimension_constrained <- 6 

  # Funções de teste
  functions <- list(
    Sphere = list(func = Sphere, lb = rep(-5.12, dimension_unconstrained), ub = rep(5.12, dimension_unconstrained), 
                  name = "Esfera (Monomodal)", dimension = dimension_unconstrained, type = "unconstrained"),
    Schwefel = list(func = Schwefel, lb = rep(-500, dimension_unconstrained), ub = rep(500, dimension_unconstrained), 
                    name = "Schwefel (Multimodal)", dimension = dimension_unconstrained, type = "unconstrained"),
    Michalewicz_Ex1 = list(func = func_Exemplo1_Michalewicz, lb = rep(0, dimension_constrained), ub = rep(5, dimension_constrained), 
                         name = "Michalewicz (Restrito)", dimension = dimension_constrained, type = "constrained",
                         violation_func = calculate_violations_Exemplo1)
  )
  
  # Algoritmos para comparação
  algorithms <- c("GA_Padrão", "GA_KMeans", "GA_Hierárquico", "GA_DBSCAN", "PSO", "Bat_Algorithm", "BAT_PSO_Hybrid", "GA_Penalizado")

  # Armazenamento de resultados (ajuste a dimensão do array se o número de funções mudar)
  results <- array(NA, dim = c(length(functions), length(algorithms), execs))
  convergence_data <- list()
  
  cat("Iniciando experimento de comparação (Algoritmos com e sem restrições)...\n")
  
  for(f_idx in 1:length(functions)) {
    func_info <- functions[[f_idx]]
    func <- func_info$func
    lb <- func_info$lb
    ub <- func_info$ub
    dimension_current <- func_info$dimension
    
    cat("Testando função:", func_info$name, "\n")
    
    for(alg_idx in 1:length(algorithms)) {
      algorithm <- algorithms[alg_idx]
      cat("  Algoritmo:", algorithm, "\n")
      
      for(exec in 1:execs) {
        if(exec %% 10 == 0) cat("    Execução:", exec, "\n")
        
        # Execução dos algoritmos
        result <- NULL 
        if(algorithm == "GA_Padrão") {
          result <- GA_standard(func, lb, ub, pop.size, dimension_current, max.it)
        } else if(algorithm == "GA_KMeans") {
          result <- GA_kmeans(func, lb, ub, pop.size, dimension_current, max.it)
        } else if(algorithm == "GA_Hierárquico") {
          result <- GA_hierarchical(func, lb, ub, pop.size, dimension_current, max.it)
        } else if(algorithm == "GA_DBSCAN") {
          result <- GA_dbscan(func, lb, ub, pop.size, dimension_current, max.it)
        } else if(algorithm == "PSO") {
          result <- PSO(func, lb, ub, pop.size, dimension_current, max.it)
        } else if(algorithm == "Bat_Algorithm") {
          result <- BatAlgorithm(func, lb, ub, pop.size, dimension_current, max.it)
        } else if(algorithm == "BAT_PSO_Hybrid") {
          result <- BAT_PSO_Hybrid(func, lb, ub, pop.size, dimension_current, max.it)
        } else if(algorithm == "GA_Penalizado") {
            if (func_info$type == "constrained") {
                result <- GA_Penalized(func, lb, ub, pop.size, dimension_current, max.it,
                                       violation_func = func_info$violation_func)
            } else {
                results[f_idx, alg_idx, exec] <- NA
                convergence_key <- paste(func_info$name, algorithm, sep = "_")
                convergence_data[[convergence_key]] <- rep(NA, max.it)
                next
            }
        }
        
        # Armazenamento do melhor resultado
        current_final_fitness <- NA
        
        if(algorithm == "PSO") {
          current_final_fitness <- result$gbest_fitness
        } else if (algorithm %in% c("Bat_Algorithm", "BAT_PSO_Hybrid")) {
          current_final_fitness <- result$best_fitness
        } else if (algorithm == "GA_Penalizado") {
          current_final_fitness <- result$best_fitness_final
        } else {
          current_final_fitness <- min(result$fit)
        }
        if (func_info$type == "constrained" && !(algorithm == "GA_Penalizado")) {
            sol_to_check <- NULL
            if (algorithm == "PSO") {
              sol_to_check <- result$gbest
            } else if (algorithm %in% c("Bat_Algorithm", "BAT_PSO_Hybrid")) {
              sol_to_check <- result$best_bat 
            } else {
              sol_to_check <- result$pop[which.min(result$fit),] 
            }
            
            # Garante que sol_to_check não é NULL antes de chamar violation_func
            if (!is.null(sol_to_check)) {
                current_violations <- func_info$violation_func(sol_to_check)
                if (current_violations > 1e-6) { # Se a violação é significativa (epsilon pequeno)
                    current_final_fitness <- 1e9 # Penaliza severamente com fitness alto (valor grande)
                }
            } else {
                current_final_fitness <- 1e9 # Penaliza se a solução não foi encontrada por algum motivo
            }
        }
        results[f_idx, alg_idx, exec] <- current_final_fitness
        
        # Armazenamento dos dados de convergência para a primeira execução
        if(exec == 1) {
          convergence_key <- paste(func_info$name, algorithm, sep = "_")
          # Para GA_Penalized, a convergência é do fitness penalizado
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
                  algorithms[alg_idx], 
                  mean(alg_results, na.rm = TRUE), 
                  sd(alg_results, na.rm = TRUE), 
                  min(alg_results, na.rm = TRUE), 
                  max(alg_results, na.rm = TRUE), 
                  median(alg_results, na.rm = TRUE)))
    }
    
    # Ranking médio
    cat("\nRanking Médio (1 = melhor):\n")
    rankings <- matrix(NA, nrow = length(algorithms), ncol = ncol(func_results))
    for(exec in 1:ncol(func_results)) {
      current_exec_results <- func_results[, exec]
      current_exec_results[is.na(current_exec_results)] <- Inf
      rankings[, exec] <- rank(current_exec_results, ties.method = "average")
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
    data_for_anova <- na.omit(data_for_anova)
    
    if (nrow(data_for_anova) > 0 && length(unique(data_for_anova$algorithm)) > 1) {
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
    } else {
        cat("\nNão há dados suficientes ou variação para realizar a ANOVA.\n")
    }
  }
  
  # Análise geral de desempenho
  cat("\n============================================================================\n")
  cat("ANÁLISE GERAL DE DESEMPENHO\n")
  cat("============================================================================\n")
  
  overall_rankings <- matrix(NA, nrow = length(algorithms), ncol = length(functions))
  for(f_idx in 1:length(functions)) {
    func_results <- results[f_idx, , ]
    rankings_for_function <- matrix(NA, nrow = length(algorithms), ncol = ncol(func_results))
    for(exec in 1:ncol(func_results)) {
      current_exec_results <- func_results[, exec]
      current_exec_results[is.na(current_exec_results)] <- Inf
      rankings_for_function[, exec] <- rank(current_exec_results, ties.method = "average")
    }
    overall_rankings[, f_idx] <- rowMeans(rankings_for_function)
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
  
  # Gráficos de convergência
  colors <- c("black", "red", "blue", "green", "purple", "orange", "darkgreen", "brown") 
  
  for(f_idx in 1:length(functions)) {
    func_name <- functions[[f_idx]]$name
    func_results <- results[f_idx, , ] 
    
    # Extração dos dados de convergência
    func_convergence <- list()
    alg_names_for_legend <- c() 
    
    for(i in 1:length(algorithms)) {
      alg <- algorithms[i]
      key <- paste(functions[[f_idx]]$name, alg, sep = "_") 
      if(key %in% names(convergence_data)) {
        if (!all(is.na(convergence_data[[key]]))) {
          func_convergence[[alg]] <- convergence_data[[key]]
          alg_names_for_legend <- c(alg_names_for_legend, alg)
        }
      }
    }
    
    # Gráfico de convergência em PNG
    png(filename = paste0("convergencia_", gsub(" ", "_", func_name), ".png"), width = 800, height = 600)
    par(mar = c(4, 4, 3, 1))
    
    if(length(func_convergence) > 0) {
      max_length <- max(sapply(func_convergence, length))
      
      plot_ylim <- range(unlist(func_convergence), na.rm = TRUE)
      if (length(plot_ylim) == 0 || is.infinite(plot_ylim[1]) || is.infinite(plot_ylim[2])) {
          plot_ylim <- c(0, 1) 
          if (func_name == "Esfera (Monomodal)") plot_ylim <- c(0, 0.1) 
          if (func_name == "Schwefel (Multimodal)") plot_ylim <- c(0, 2500) 
          if (func_name == "Michalewicz (Restrito)") plot_ylim <- c(0, 150) 
      }

      first_alg_data <- NULL
      first_alg_name <- NULL
      first_alg_color_idx <- NULL

      for (i in 1:length(algorithms)) {
          alg_current <- algorithms[i]
          if (alg_current %in% names(func_convergence) && !all(is.na(func_convergence[[alg_current]]))) {
              first_alg_data <- func_convergence[[alg_current]]
              first_alg_name <- alg_current
              first_alg_color_idx <- i
              break
          }
      }

      if (!is.null(first_alg_data)) {
          plot(1:max_length, first_alg_data, type = "l", col = colors[first_alg_color_idx], 
               xlab = "Geração", ylab = "Melhor Fitness", 
               main = paste("Convergência -", functions[[f_idx]]$name), 
               ylim = plot_ylim, lwd = 2)
          
          for(i in 1:length(algorithms)) { 
              alg <- algorithms[i]
              if(alg %in% names(func_convergence) && i != first_alg_color_idx) { 
                  if (!all(is.na(func_convergence[[alg]]))) { 
                      lines(1:length(func_convergence[[alg]]), func_convergence[[alg]], 
                            col = colors[i], lwd = 2)
                  }
              }
          }
          
          legend("topright", legend = alg_names_for_legend, 
                 col = colors[match(alg_names_for_legend, algorithms)], lwd = 2, cex = 0.8)
      } else {
          plot(1, type="n", main=paste("Convergência -", functions[[f_idx]]$name), 
               xlab="Geração", ylab="Melhor Fitness", xlim=c(0, max.it), ylim=c(0,1))
          text(max.it/2, 0.5, "Dados insuficientes para este gráfico")
      }
    } else {
        plot(1, type="n", main=paste("Convergência -", functions[[f_idx]]$name), 
             xlab="Geração", ylab="Melhor Fitness", xlim=c(0, max.it), ylim=c(0,1))
        text(max.it/2, 0.5, "Nenhum dado de convergência disponível")
    }
    dev.off()

    png(filename = paste0("boxplot_", gsub(" ", "_", func_name), ".png"), width = 800, height = 600)
    par(mar = c(8, 4, 3, 1))
    
    data_for_boxplot_list <- list()
    alg_names_for_boxplot <- c()

    for(i in 1:length(algorithms)) {
        alg_results_current <- func_results[i, ]
        valid_results <- alg_results_current[!is.na(alg_results_current)]

        if (length(valid_results) > 0) {
            data_for_boxplot_list[[algorithms[i]]] <- valid_results
            alg_names_for_boxplot <- c(alg_names_for_boxplot, algorithms[i])
        }
    }

    if (length(data_for_boxplot_list) > 0) {
        # Ajuste ylim para o boxplot, ignorando valores penalizados (1e9)
        boxplot_ylim_data <- unlist(data_for_boxplot_list)
        boxplot_ylim_data <- boxplot_ylim_data[is.finite(boxplot_ylim_data) & boxplot_ylim_data < 1e8] 
        
        boxplot_ylim <- range(boxplot_ylim_data, na.rm = TRUE)
        if (length(boxplot_ylim) == 0 || is.infinite(boxplot_ylim[1]) || is.infinite(boxplot_ylim[2])) {
            boxplot_ylim <- c(0, 1) 
            if (func_name == "Esfera (Monomodal)") boxplot_ylim <- c(0, 0.1)
            if (func_name == "Schwefel (Multimodal)") boxplot_ylim <- c(0, 1500)
            if (func_name == "Michalewicz (Restrito)") boxplot_ylim <- c(0, 100)
        }

        boxplot(data_for_boxplot_list,
                names = alg_names_for_boxplot,
                main = paste("Distribuição dos Resultados -", functions[[f_idx]]$name),
                xlab = "Algoritmo", ylab = "Fitness Final",
                las = 2, cex.axis = 0.8,
                ylim = boxplot_ylim) 
    } else {
        plot(1, type="n", main=paste("Distribuição dos Resultados -", functions[[f_idx]]$name),
             xlab="Algoritmo", ylab="Fitness Final", xlim=c(0,1), ylim=c(0,1))
        text(0.5, 0.5, "Nenhum dado válido disponível para boxplot")
    }
    dev.off()
  }
}