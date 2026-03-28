# Setup thông tin chung
set.seed(234)
n_sample <- 1000   # Số mẫu cho mỗi lần ước lượng
n_reps <- 1000     # Số lần lặp lại mô phỏng để tính phương sai

# CÂU 3A:
## Hàm h(x) mục tiêu
h_target <- function(x) { exp(-abs(x)^3 / 3) }

## Hàm thực hiện Importance Sampling + Guassian
calc_is <- function() {
  samp <- rnorm(n_sample, 0, 1)
  h_prop <- dnorm(samp, 0, 1)
  
  w <- h_target(samp) / h_prop
  return(sum(samp^2 * w) / sum(w))
}

## Ước lượng Monte Carlo
est_gaussian  <- replicate(n_reps, calc_is())

cat("Ước lượng theo Gaussian: ", mean(est_gaussian), "\n")

# CÂU 3B:
## sắp xếp x
x <- rnorm(n_sample, 0, 1)
x_sorted <- sort(x)
## Sigma^2 theo công thức rienmann
top <- sum(x_sorted[-n_sample]^2 * diff(x_sorted) * h_target(x_sorted[-n_sample]))
bottom <- sum(diff(x_sorted) * h_target(x_sorted[-n_sample]))
sigma_hat <- top / bottom

cat("Xấp xỉ sigma^2 theo Rienmann = ", sigma_hat)

# CÂU 3C:
result_compare <- sapply(1:10000, function(i, n_sample){
  set.seed(i+345)
  
  x <- rnorm(n_sample, mean = 0, sd = 1)
  ## Importance sampling
  w <- h_target(x) / dnorm(x, 0, 1)
  sigma2_is <- sum(x^2 * w) / sum(w)
  
  ## Rienmann
  x_sorted <- sort(x)
  top <- sum(x_sorted[-n_sample]^2 * diff(x_sorted) * h_target(x_sorted[-n_sample]))
  bottom <- sum(diff(x_sorted) * h_target(x_sorted[-n_sample]))
  sigma2_R <- top / bottom
  
  return(c(IS = sigma2_is, R = sigma2_R))
}, n_sample)

## Tính giá trị tích phân thật để so sánh
### Tích phân mẫu số
Z <- integrate(function(x) h_target(x), -Inf, Inf)$value
### Tích phân tử số
M2 <- integrate(function(x) x^2 * h_target(x), -Inf, Inf)$value
### Giá trị thật của sigma^2
sigma_true <- M2 / Z
cat("Giá trị thật sigma^2 = ", sigma_true)

## So sánh mean giá trị sigma^2 giữa 2 phương pháp
apply(result_compare, 1, mean)

## So sánh độ lệch chuẩn giữa 2 phương pháp
apply(result_compare, 1, sd)


## Visualization
### Histogram
par(mfrow = c(1,2))
hist(result_compare[1, ], main = "Histogram IS", xlab = "Ước lượng IS",
     col = "lightblue", border = "white")
hist(result_compare[2, ], main = "Histogram Riemann", xlab = "Ước lượng Riemann",
     col = "lightgreen", border = "white")

### Boxplot
par(mfrow = c(1,1))
boxplot(result_compare[1, ], result_compare[2, ],
        names = c("IS", "Riemann"),
        col = c("lightblue", "lightgreen"),
        main = "So sánh IS vs Riemann")
