source("R/bootstrap.R")

x <- rnorm(10)
my_boot(data = x, R = 5)

y <- data.frame(x1 = c(1:10), x2 = rnorm(10))
my_boot(data = y, R = 5)

## Gamma distribution ----

x <- rgamma(n = 20, shape = 3, rate = 0.5)
x_bts <- my_boot(data = x, R = 500)
x_bts
x_mean_bts <- sapply(x_bts, FUN = mean)

hist(x_mean_bts)
mean(x)
mean(x_mean_bts)

x_mean_bts_s <- sort(x_mean_bts)
x_mean_bts_s[c(floor(500*0.05/2), floor(500*(1 - 0.05/2)))]

quantile(x = x_mean_bts, probs = c(0.05/2, 1 - 0.05/2), type = 3)


## BCa ----
x_mean <- mean(x)
b_est <- qnorm(mean(x_mean_bts <= x_mean))
b_est

# x_mean_jack <- numeric(length(x))
# for(i in 1:length(x)) {
#   x_mean_jack[i] <- mean(x[-i])
# }

x_mean_jack <- sapply(1:length(x), function(i){
  mean(x[-i])
})

x_mean_jack2 <- mean(x_mean_jack)
u <- x_mean_jack2 - x_mean_jack

a_est <- ((1/6) * sum(u^3)) * (sum(u^2))^(-3/2)
a_est

alpha <- 0.05
z_alp <- qnorm(alpha/2)

beta1_est <- pnorm(b_est + 1/(1/(b_est + z_alp) - a_est))
beta2_est <- pnorm(b_est + 1/(1/(b_est - z_alp) - a_est))

x_mean_bts_s <- sort(x_mean_bts)
x_mean_bts_s[c(floor(500*beta1_est), floor(500*beta2_est))]

test1 <- function(fun, x){
  sapply(1:length(x), FUN = function(i) fun(x[-i]))
}
test1(fun = mean, x = x)
test1(fun = median, x = x)

my_ci_boot(x_bts, fun = mean)


