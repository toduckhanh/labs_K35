# include all kernel functions
kernel_gauss <- function(x){
  kern <- exp(-x^2/2)/sqrt(2*pi)
  return(kern)
}

kernel_epan <- function(u){
  0.75*(1 - u^2)*(abs(u) <= 1)
}

kernel_triangle <- function(u){
  (1 - abs(u)) * (abs(u) <= 1)
}

kernel_quartic <- function(u){
  15 * (1 - u^2)^2 * (abs(u) <= 1)/16
}

kernel_triweighted <- function(u){
  35 * (1 - u^2)^3 * (abs(u) <= 1)/32
}

kernel_tricube <- function(u){
  70 * (1 - abs(u)^3)^3 * (abs(u) <= 1)/81
}

kernel_cosine <- function(u){
  pi * cos(pi*u/2) * (abs(u) <= 1)/4
}

kernel_uniform <- function(u){
  0.5 * (abs(u) <= 1)
}
