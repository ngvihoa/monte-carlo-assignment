set.seed(42)
S0 <- 50 # Giá cổ phiếu ban đầu
K <- 52 # Giá thực hiện (strike price)
sigma <- 0.5 # Độ biến động
r <- 0.05 # Lãi suất phi rủi ro
T <- 30 # Số ngày đến đáo hạn
n <- 100000 # Số mô phỏng Monte Carlo

Z <- rnorm(n)
e_hat <- exp(-r * T / 365)

# Câu 4a:
# Giá cổ phiếu tại ngày T
ST <- S0 * exp((r - sigma^2 / 2) * T / 365 + sigma * Z * sqrt(T / 365))
Ci <- e_hat * pmax(0, ST - K)
C_bar <- mean(Ci)

cat(sprintf("Giá hợp lý - ECO: %.4f\n", C_bar))

# Câu 4b:
# Mô phỏng đường giá từng ngày t = 1..T
asian_payoff <- numeric(n)

for (i in 1:n) {
  S <- S0
  for (t in 1:T) {
    Z_t <- rnorm(1)
    S <- S * exp((r - sigma^2 / 2) / 365 + sigma * Z_t / sqrt(365))
  }
  asian_payoff[i] <- e_hat * max(0, S - K)
}

A_bar <- mean(asian_payoff)

cat(sprintf("Giá hợp lý - ACO - Mô phỏng 100,000 lần: %.4f\n", A_bar))
