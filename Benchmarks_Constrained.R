func_Exemplo1_Michalewicz <- function(x) {
  if (length(x) != 6) {
    stop("A função func_Exemplo1_Michalewicz requer um vetor de 6 dimensões.")
  }
  return(sum(x^2))
}

calculate_violations_Exemplo1 <- function(x) {
  violations <- 0

  violations <- violations + abs(x[1] + x[2] + x[3] - 5)
  violations <- violations + abs(x[4] + x[5] + x[6] - 10)
  violations <- violations + abs(x[1] + x[4] - 3)
  violations <- violations + abs(x[2] + x[5] - 4)

  for (i in 1:6) {
    violations <- violations + max(0, -x[i])
  }

  return(violations)
}