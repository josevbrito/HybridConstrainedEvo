# Agrupamento K-means para diversidade da população
apply_kmeans_diversity <- function(pop, fitness, n_clusters = 5) {
  if(nrow(pop) < n_clusters) n_clusters <- nrow(pop)
  
  tryCatch({
    kmeans_result <- kmeans(pop, centers = n_clusters, nstart = 10)
    clusters <- kmeans_result$cluster
    
    # Aqui é selecionado o melhor indivíduo de cada cluster
    diverse_pop <- matrix(NA, nrow = n_clusters, ncol = ncol(pop))
    diverse_fitness <- rep(NA, n_clusters)
    
    for(i in 1:n_clusters) {
      cluster_idx <- which(clusters == i)
      if(length(cluster_idx) > 0) {
        best_in_cluster <- which.min(fitness[cluster_idx])
        diverse_pop[i, ] <- pop[cluster_idx[best_in_cluster], ]
        diverse_fitness[i] <- fitness[cluster_idx[best_in_cluster]]
      }
    }
    
    return(list(pop = diverse_pop, fit = diverse_fitness, clusters = clusters))
  }, error = function(e) {
    return(list(pop = pop[1:min(n_clusters, nrow(pop)), ], 
                fit = fitness[1:min(n_clusters, nrow(pop))], 
                clusters = 1:min(n_clusters, nrow(pop))))
  })
}

# Agrupamento hierárquico para diversidade
apply_hierarchical_diversity <- function(pop, fitness, n_clusters = 5) {
  if(nrow(pop) < n_clusters) n_clusters <- nrow(pop)
  
  tryCatch({
    dist_matrix <- dist(pop)
    hc_result <- hclust(dist_matrix, method = "ward.D2")
    clusters <- cutree(hc_result, k = n_clusters)
    
    # Aqui é selecionado o melhor indivíduo de cada cluster
    diverse_pop <- matrix(NA, nrow = n_clusters, ncol = ncol(pop))
    diverse_fitness <- rep(NA, n_clusters)
    
    for(i in 1:n_clusters) {
      cluster_idx <- which(clusters == i)
      if(length(cluster_idx) > 0) {
        best_in_cluster <- which.min(fitness[cluster_idx])
        diverse_pop[i, ] <- pop[cluster_idx[best_in_cluster], ]
        diverse_fitness[i] <- fitness[cluster_idx[best_in_cluster]]
      }
    }
    
    return(list(pop = diverse_pop, fit = diverse_fitness, clusters = clusters))
  }, error = function(e) {
    return(list(pop = pop[1:min(n_clusters, nrow(pop)), ], 
                fit = fitness[1:min(n_clusters, nrow(pop))], 
                clusters = 1:min(n_clusters, nrow(pop))))
  })
}

# Agrupamento DBSCAN para diversidade
apply_dbscan_diversity <- function(pop, fitness, eps = 0.5, min_pts = 3) {
  tryCatch({
    # Normalização da população para DBSCAN
    pop_scaled <- scale(pop)
    dbscan_result <- dbscan(pop_scaled, eps = eps, minPts = min_pts)
    clusters <- dbscan_result$cluster
    
    # Lida com pontos de ruído (cluster 0)
    unique_clusters <- unique(clusters[clusters > 0])
    if(length(unique_clusters) == 0) {
      # Se nenhum cluster for encontrado, retorna os melhores indivíduos
      n_select <- min(5, nrow(pop))
      best_idx <- order(fitness)[1:n_select]
      return(list(pop = pop[best_idx, ], fit = fitness[best_idx], clusters = 1:n_select))
    }
    
    # Seleciona o melhor indivíduo de cada cluster
    diverse_pop <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(pop))
    diverse_fitness <- rep(NA, length(unique_clusters))
    
    for(i in seq_along(unique_clusters)) {
      cluster_idx <- which(clusters == unique_clusters[i])
      if(length(cluster_idx) > 0) {
        best_in_cluster <- which.min(fitness[cluster_idx])
        diverse_pop[i, ] <- pop[cluster_idx[best_in_cluster], ]
        diverse_fitness[i] <- fitness[cluster_idx[best_in_cluster]]
      }
    }
    
    return(list(pop = diverse_pop, fit = diverse_fitness, clusters = clusters))
  }, error = function(e) {
    n_select <- min(5, nrow(pop))
    best_idx <- order(fitness)[1:n_select]
    return(list(pop = pop[best_idx, ], fit = fitness[best_idx], clusters = 1:n_select))
  })
}