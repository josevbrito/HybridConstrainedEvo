# ============================================================================
# COMPARAÇÃO ENTRE ALGORITMOS EVOLUTIVOS HÍBRIDOS E NÃO HÍBRIDOS
# ATIVIDADE III: HIBRIDIZAÇÃO DE ALGORITMOS META-HEURÍSTICOS
# ============================================================================

# Carregando a biblioteca necessária
if (!require(dbscan)) {
  install.packages("dbscan")
  library(dbscan)
} else {
    library(dbscan)
}

# Arquivos da Atividade I
source('GA.R')
source('init.population.R')
source('crossover.R')
source('mutation.R')
source('selection.R')
source('Benchmarks.R')
source('clustering.R')

# Arquivos da Atividade II
source('PSO.R')
source('BAT.R')
source('comparison.R')

# Arquivos da Atividade III
source('BAT_PSO_Hybrid.R')

# Função main
main <- function() {
  cat("============================================================================\n")
  cat("ATIVIDADE III: COMPARAÇÃO HÍBRIDA (GA vs PSO vs BAT vs BAT-PSO Híbrido)\n")
  cat("============================================================================\n")
  
  # Execução do experimento
  cat("Iniciando experimento de hibridização e comparação...\n")
  experiment_results <- run_comparison()
  
  # Análise estatística
  perform_statistical_analysis(experiment_results)
  
  # Visualizações
  cat("\nGerando visualizações...\n")
  create_visualization(experiment_results)
  
  cat("\n============================================================================\n")
  cat("EXPERIMENTO DA ATIVIDADE III CONCLUÍDO\n")
  cat("============================================================================\n")
  cat("Resumo dos Resultados:\n")
  cat("- Testadas 2 funções de benchmark (Esfera - monomodal, Schwefel - multimodal)\n")
  cat("- Comparados 7 algoritmos, incluindo o híbrido:\n")
  cat("  1. GA Padrão\n")
  cat("  2. GA com agrupamento K-means\n")
  cat("  3. GA com agrupamento Hierárquico\n") 
  cat("  4. GA com agrupamento DBSCAN\n")
  cat("  5. PSO (Particle Swarm Optimization)\n")
  cat("  6. Bat Algorithm\n")
  cat("  7. BAT-PSO Híbrido\n")
  cat("- 30 execuções independentes por combinação algoritmo-função\n")
  cat("- Significância estatística testada com ANOVA e Tukey HSD\n")
  cat("- Gráficos de convergência e box plots gerados\n")
  cat("- Ranking geral consolidado calculado\n")
  cat("============================================================================\n")
  
  return(experiment_results)
}

# Execução do experimento
experiment_results <- main()