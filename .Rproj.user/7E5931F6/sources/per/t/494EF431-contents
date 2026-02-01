# source code ----
source(file = "R/kernel_reg.R")

# mcycle data ----
data(mcycle, package = "MASS")
head(mcycle)

accel_kr_1 <- kernel_reg(x = mcycle$times, y = mcycle$accel, x_eval = 30, 
                         h = 0.5)

plot(x = mcycle$times, y = mcycle$accel, pch = 16)
points(x = 30, y = accel_kr_1, col = "blue", pch = 4)

## obtain the estimate at several x's.
range(mcycle$times)
x_plot <- seq(0, 60, length.out = 201)
y_hat <- kernel_reg(x = mcycle$times, y = mcycle$accel, x_eval = x_plot,
                    h = 0.5)

plot(x = mcycle$times, y = mcycle$accel, pch = 16, cex = 0.7)
lines(x = x_plot, y = y_hat, col = "blue")

## CV ----
cv1_h(x = mcycle$times, y = mcycle$accel, h = 0.5)
cv1_h(x = mcycle$times, y = mcycle$accel, h = 1.5)
cv1_h(x = mcycle$times, y = mcycle$accel, h = 2.5)

h_plot <- seq(0.1, 4, length.out = 41)
cv1_est <- cv1_h(x = mcycle$times, y = mcycle$accel, h = h_plot)

plot(x = h_plot, y = cv1_est, type = "b", pch = 16)

system.time({
  cv1_est <- cv1_h(x = mcycle$times, y = mcycle$accel, h = h_plot)
})
h_plot[which.min(cv1_est)]

system.time({
  cv2_est <- cv2_h(x = mcycle$times, y = mcycle$accel, h = h_plot)
})

h_plot[which.min(cv2_est)]
all.equal(cv1_est[-(1:2)], cv2_est[-(1:2)])

## GCV ----

system.time({
  gcv_est <- gcv_h(x = mcycle$times, y = mcycle$accel, h = h_plot)
})
h_plot[which.min(gcv_est)]

## benchmark in 100 times
res_banchmark <- microbenchmark::microbenchmark(
  cv1 = cv1_h(x = mcycle$times, y = mcycle$accel, h = h_plot),
  cv2 = cv2_h(x = mcycle$times, y = mcycle$accel, h = h_plot),
  gcv = gcv_h(x = mcycle$times, y = mcycle$accel, h = h_plot)
)

ggplot2::autoplot(res_banchmark)

