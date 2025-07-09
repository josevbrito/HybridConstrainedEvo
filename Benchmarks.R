# Função monomodal
Sphere <- function(x) {
  return(sum(x^2))
}

# Funções de benchmark
Rosenbrock <- function(x){
  x1 <- x[1:(length(x)-1)]
  x2 <- x[2:length(x)]
  return(sum(100 * (x2 - x1^2)^2 + (x1-1)^2))
}

Griewank <- function(x){
  i <- sqrt(1:length(x))
  return(1/4000 * sum(x^2) - prod(cos(x/i)) + 1)
}

# Função Ackley
Ackley <- function(x){
  n <- length(x)
  return(-20 * exp(-0.2 * sqrt(sum(x^2)/n)) - exp(sum(cos(2*pi*x))/n) + 20 + exp(1))
}

# Função multimodal
Schwefel <- function(x){
  return(418.9829 * length(x) - sum(x * sin(sqrt(abs(x)))))  
}

# Função Alpine
Alpine <- function(x){
  return(sum(abs(x * sin(x) + 0.1 * x))) 
}