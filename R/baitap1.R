set.seed(123)
x <-  c(1.2, 0.8, 1.5, 0.9, 1.1, 1.3, 0.7, 1.0, 1.4, 1.6) # Ví dụ 5 quan sát

log_target <- function(mu, x) {
  # f(x|mu) tỷ lệ với (3 + (x-mu)^2)^-2 ]
  sum(-2 * log(3 + (x - mu)^2))
}
#Importance Sampling
m <- 10000  
mu_samples <- rnorm(m, mean = mean(x), sd = sd(x)) 

# Tính log
log_weights <- sapply(mu_samples, function(m_val) {
  log_target(m_val, x) - dnorm(m_val, mean = mean(x), sd = sd(x), log = TRUE)
})

# Chuẩn hóa log để tính đơn giản
weights <- exp(log_weights - max(log_weights))

#Kết quả kỳ vọng hậu nghiệm E
expected_mu <- sum(mu_samples * weights) / sum(weights)
cat("Kỳ vọng hậu nghiệm xấp xỉ là:", expected_mu)  

