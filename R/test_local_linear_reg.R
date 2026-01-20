source(file = "R/local_linear_reg.R")

# mcycle data ----
data(mcycle, package = "MASS")
head(mcycle)

range(mcycle$times)
x_plot <- seq(2, 60, length.out = 201)
y_ll_hat <- loclin_reg(x = mcycle$times, y = mcycle$accel, x_eval = x_plot, 
                       h = 0.5)

plot(x = mcycle$times, y = mcycle$accel, pch = 16, cex = 0.7)
lines(x = x_plot, y = y_ll_hat, col = "blue")

h_times_RT <- h_ll_RT(x = mcycle$times, y = mcycle$accel)


y_ll_hat_RT <- loclin_reg(x = mcycle$times, y = mcycle$accel, x_eval = x_plot, 
                          h = h_times_RT)

plot(x = mcycle$times, y = mcycle$accel, pch = 16, cex = 0.7)
lines(x = x_plot, y = y_ll_hat, col = "blue")
lines(x = x_plot, y = y_ll_hat_RT, col = "forestgreen")

h_plot <- seq(0.1, 4, length.out = 41)
cv_ll_est <- CV_loclin(x = mcycle$times, y = mcycle$accel, h = h_plot)

plot(x = h_plot, y = cv_ll_est, type = "b", pch = 16)
h_times_CV <- h_plot[which.min(cv_ll_est)]

system.time({
  cv_ll_est <- CV_loclin(x = mcycle$times, y = mcycle$accel, h = h_plot)
})

y_ll_hat_CV <- loclin_reg(x = mcycle$times, y = mcycle$accel, x_eval = x_plot, 
                         h = h_times_CV)

plot(x = mcycle$times, y = mcycle$accel, pch = 16, cex = 0.7)
lines(x = x_plot, y = y_ll_hat_RT, col = "blue")
lines(x = x_plot, y = y_ll_hat_CV, col = "red")


system.time({
  gcv_ll_est <- GCV_loclin(x = mcycle$times, y = mcycle$accel, h = h_plot)
})

plot(x = h_plot, y = gcv_ll_est, type = "b", pch = 16)

h_times_GCV <- h_plot[which.min(gcv_ll_est)]


y_ll_hat_GCV <- loclin_reg(x = mcycle$times, y = mcycle$accel, x_eval = x_plot, 
                           h = h_times_GCV)

plot(x = mcycle$times, y = mcycle$accel, pch = 16, cex = 0.7)
lines(x = x_plot, y = y_ll_hat_RT, col = "blue")
lines(x = x_plot, y = y_ll_hat_CV, col = "red")
lines(x = x_plot, y = y_ll_hat_GCV, col = "black")

## benchmark in 100 times
res_banchmark <- microbenchmark::microbenchmark(
  cv = CV_loclin(x = mcycle$times, y = mcycle$accel, h = h_plot),
  gcv = GCV_loclin(x = mcycle$times, y = mcycle$accel, h = h_plot)
)

ggplot2::autoplot(res_banchmark)


