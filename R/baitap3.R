# setup các thông tin chung
set.seed(234)
n_sample <- 10000 ## 1_000
## Hàm h(.) mục tiêu
h_target <- function(x){
  exp(-abs(x)^3 / 3)
}
## Tạo mẫu dữ liệu từ phân phối chuẩn N(0,1)
x <- rnorm(n_sample, mean = 0, sd = 1)

# CÂU 3A:
## Hàm mật độ h(.) đề xuất theo N(0, 1)
h <- function(x){
  dnorm(x, mean = 0, sd = 1)
}
##Trọng số h_target(.) / h(.)
w = h_target(x) / h(x)
## Xấp xỉ sigma^2
sigma_hat <- sum(x^2 * w) / sum(w)

cat("Xấp xỉ sigma^2 theo IS = ", sigma_hat)

# CÂU 3B:
## sắp xếp x
x_sorted <- sort(x)
## Sigma^2 theo công thức rienmann
top <- sum(x_sorted[-n_sample]^2 * diff(x_sorted) * h_target(x_sorted[-n_sample]))
bottom <- sum(diff(x_sorted) * h_target(x_sorted[-n_sample]))
sigma_hat <- top / bottom

cat("Xấp xỉ sigma^2 theo Rienmann = ", sigma_hat)

# CÂU 3C:
result_compare <- sapply(1:100, function(i, n_sample){
  set.seed(i+345)
  
  x <- rnorm(n_sample, mean = 0, sd = 1)
  ## Importance sampling
  w <- h_target(x) / h(x)
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

### Vẽ đường estimator theo số lần mô phỏng
curve(h_target(x), from = -3, to = 3, col = "red", lwd = 2,
      main = "So sánh h_target và h đề xuất", ylab = "Giá trị")
curve(h(x), from = -3, to = 3, col = "blue", lwd = 2, add = TRUE)
legend("topright", legend = c("h_target", "h đề xuất"),
       col = c("red", "blue"), lty = 1, lwd = 2)






