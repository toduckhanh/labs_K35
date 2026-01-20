source("R/kernel_functions.R")

loclin_reg <- function(x, y, x_eval, h, kernel = "gauss"){
  u <- x - x_eval
  uh <- u/h
  w <- kernel_gauss(uh)
  s0 <- mean(w)
  s1 <- mean(w * (x - x_eval))
  s2 <- mean(w * (x - x_eval)^2)
  ww <- ((s2 - s1 * (x - x_eval)) * w)/(s2 * s0 - s1^2)
  y_hat <- mean(ww * y)
  return(y_hat)
}

loclin_reg <- Vectorize(FUN = loclin_reg, vectorize.args = "x_eval")

h_ll_RT <- function(x, y, kernel = "gauss") {
  if(kernel == "gauss"){
    R_k <- 0.5/sqrt(pi)
    sigma2_k <- 1
  }
  # Quartic fit
  mod_Q <- lm(y ~ poly(x, raw = TRUE, degree = 4))
  # Estimates of unknown quantities
  int_sigma2_hat <- diff(range(x))*sum(mod_Q$residuals^2)/mod_Q$df.residual
  m_2prime <- (2 * mod_Q$coefficients[3] + 6 * mod_Q$coefficients[4] * x +
    12 * mod_Q$coefficients[5] * x^2)^2
  a_hat <- mean(m_2prime)
  # h_RT
  h_RT <- ((R_k*int_sigma2_hat)/(a_hat*sigma2_k*length(x)))^(1/5)
  return(h_RT)
}

## Cross-validation
CV_loclin <- function(x, y, h, kernel = "gauss") {
  n <- length(y)
  S_diag <- numeric(n)
  for (i in 1:n) {
    u <- x - x[i]
    w <- kernel_gauss(u/h)
    a0 <- mean(w)
    a1 <- mean(w * u)
    a2 <- mean(w * u^2)
    S_diag[i] <- (a2 * w[i])/(a2 * a0 - a1^2)/n
  }
  y_hat <- loclin_reg(x = x, y = y, x_eval = x, h = h, kernel = kernel)
  loocv <- mean(((y - y_hat)/(1 - S_diag))^2)
  return(loocv)
}

CV_loclin <- Vectorize(FUN = CV_loclin, vectorize.args = "h")


GCV_loclin <- function(x, y, h, kernel = "gauss") {
  n <- length(y)
  S_diag <- numeric(n)
  for (i in 1:n) {
    u <- x - x[i]
    w <- kernel_gauss(u/h)
    a0 <- mean(w)
    a1 <- mean(w * u)
    a2 <- mean(w * u^2)
    S_diag[i] <- (a2 * w[i])/(a2 * a0 - a1^2)/n
  }
  y_hat <- loclin_reg(x = x, y = y, x_eval = x, h = h, kernel = kernel)
  gcv <- sum((y - y_hat)^2)/(n - sum(S_diag))^2
  return(gcv)
}

GCV_loclin <- Vectorize(FUN = GCV_loclin, vectorize.args = "h")
