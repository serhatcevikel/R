# gpuR benchmark

library(gpuR)

ORDER <- 3

A = matrix(rnorm(ORDER^2), nrow=ORDER)
B = matrix(rnorm(ORDER^2), nrow=ORDER)
gpuA = gpuMatrix(A, type="double")
gpuB = gpuMatrix(B, type="double")

matmult1 <- function() A %*% B
matmult2 <- function() gpuA %*% gpuB
