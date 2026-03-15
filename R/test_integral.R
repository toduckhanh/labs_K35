source("R/integ_trap.R")

# f(x) = x*sin(x) ----
x <- seq(0, pi, length.out = 101)
f1 <- function(x) x * sin(x)
f1(x)

integ_trap(x = x, f = f1)
integrate(f = f1, lower = 0, upper = pi)

## Monte Carlo ----
x_uniform <- runif(n = 2500, 0, pi)
f1_uni <- pi*f1(x_uniform)
mean(f1_uni)

# f(x) = (1/(sqrt(2*pi)*sigma))*exp(-0.5*simga^2*(x - mu)^2) ----
f2 <- function(x, mu, sigma){
  t <- 1 - x
  res <- exp(-0.5*(1/sigma^2)*(log(x/t) - mu)^2)/(sqrt(2*pi)*sigma*x*t)
  return(res)
}

a <- 0
x2 <- seq(10^(-9), 1/(1 + exp(-a)), length.out = 101)

integ_trap(x = x2, f = f2, mu = 2, sigma = 1.2)
pnorm(0, mean = 2, sd = 1.2)

x2_uniform <- runif(n = 1000, 0, 1/(1 + exp(-a)))
f2_1 <- (1/(1 + exp(-a))) * f2(x = x2_uniform, mu = 2, sigma = 1.2)
mean(f2_1)

# xấp xỉ pi ----
pi_est <- function(n, seed = 153) {
  set.seed(seed)
  x1 <- runif(n, -1, 1)
  x2 <- runif(n, -1, 1)
  res <- 4*mean(x1^2 + x2^2 <= 1)
  return(res)
}
pi1 <- pi_est(n = 100)
pi1

pi2 <- pi_est(n = 200)
pi2

pi3 <- pi_est(n = 400)
pi3

pi_est_n <- sapply(c(100, 200, 500, 1000), function(x) pi_est(x))
pi_est_n

# Importance sampling ----
x3 <- runif(1000, 0, 1)
I1 <- mean(exp(-x3)/(1 + x3^2))

p3 <- runif(1000, 0, 1)
x3_IS1 <- -log(1 - p3*(1 - exp(-1)))
I2 <- mean((1 - exp(-1))/(1 + x3^2))

I_MC_simu <- sapply(1:5000, function(i){
  x3 <- runif(50, 0, 1)
  I1 <- mean(exp(-x3)/(1 + x3^2))
  ##
  p3 <- runif(50, 0, 1)
  x3_IS1 <- -log(1 - p3*(1 - exp(-1)))
  I2 <- mean((1 - exp(-1))/(1 + x3_IS1^2))
  return(c(I1, I2))
})

apply(I_MC_simu, 1, mean)
integrate(f = function(x) exp(-x)/(1 + x^2), lower = 0, upper = 1)

apply(I_MC_simu, 1, sd)

hist(I_MC_simu[1, ])
hist(I_MC_simu[2, ])

boxplot(I_MC_simu[1, ], I_MC_simu[2, ])

curve(expr = exp(-x), from = 0, to = 1, n = 101, ylim = c(0, 2))
curve(expr = exp(-x)/(1 - exp(-1)), n = 101, add = TRUE, col = "blue")
