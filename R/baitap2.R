set.seed(123)

# Mô phỏng Monte Carlo
out_sim_is <- sapply(1:100000, function(i, nsim) {
  # Tạo mẫu ngẫu nhiên x_sim với kích thước n_sim
  u_sim <- runif(n_sim)
  x_sim <- 1 - log(1-u_sim) 
  # Hàm tính giá trị bên trong kỳ vọng
  g_x <- (x_sim^2 / sqrt(2 * pi)) * exp(-x_sim^2 / 2 + x_sim -1)
  # Ước lượng tích phân Monte Carlo Importance Sampling
  I_est <- mean(g_x)
  return(I_est)
}, nsim = 50)
mean(out_sim_is)

