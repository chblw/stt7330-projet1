library(kernlab)
library(readr)
library(dplyr)

set.seed(25032018)

mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)

instances_to_use <- 2000

mnist_data <- mnist_raw[1:instances_to_use, -1]
mnist_label <- mnist_raw[1:instances_to_use, 1] %>% unlist %>% as.vector + 1

noise <- matrix(rnorm(prod(dim(mnist_data)), 0, 150), ncol = dim(mnist_data)[2])

mnist_data_noise <- mnist_data %>% as.matrix + noise
mnist_data <- mnist_data %>% as.matrix

for(i in 1:instances_to_use) {
  mnist_data_noise[i, ] <- pmin(pmax(mnist_data_noise[i, ], 0), 255)
}

kpc_linear <- kpca(mnist_data, kernel = "vanilladot", kpar = list(), features = 2)
kpc_rbf1 <- kpca(mnist_data, kernel = "rbfdot", kpar = list(sigma = 0.0000004), features = 2)
kpc_rbf2 <- kpca(mnist_data, kernel = "rbfdot", kpar = list(sigma = 0.000005), features = 2)
kpc_bessel <- kpca(mnist_data, kernel = "besseldot", kpar = list(sigma = 0.01, order = 10), features = 2)
kpc_tanh <- kpca(mnist_data, kernel = "tanhdot", kpar = list(scale = 0.000005), features = 2)
kpc_laplace <- kpca(mnist_data, kernel = "laplacedot", kpar = list(sigma = 0.0025), features = 2)
kpc_poly <- kpca(mnist_data, kernel = "polydot", kpar = list(degree = 3), features = 2)

pc <- prcomp(mnist_data, rank. = 2)

par(mfrow = c(1, 2))
plot(rotated(kpc_linear), col = mnist_label, pch = 19, main = "kpca linéaire", xlab = "CP1", ylab = "CP2")
plot(pc$x %*% matrix(c(-1, 0, 0, -1), 2), col = mnist_label, pch = 19, main = "pca", xlab = "CP1", ylab = "CP2")


par(mfrow = c(2, 3))
plot(rotated(kpc_rbf1), col = mnist_label, pch = 19, main = "kpca rbf 1", xlab = "CP1", ylab = "CP2")
plot(rotated(kpc_rbf2), col = mnist_label, pch = 19, main = "kpca rbf 2", xlab = "CP1", ylab = "CP2")
plot(rotated(kpc_bessel), col = mnist_label, pch = 19, main = "kpca Bessel", xlab = "CP1", ylab = "CP2")
plot(rotated(kpc_tanh), col = mnist_label, pch = 19, main = "kpca tanh", xlab = "CP1", ylab = "CP2")
plot(rotated(kpc_laplace), col = mnist_label, pch = 19, main = "kpca Laplace", xlab = "CP1", ylab = "CP2")
plot(rotated(kpc_poly), col = mnist_label, pch = 19, main = "kpca polynomial", xlab = "CP1", ylab = "CP2")


# noisy -------------------------------------------------------------------

kpc_rbf1_noise <- kpca(mnist_data_noise, kernel = "rbfdot", kpar = list(sigma = 0.0000004), features = 2)
kpc_rbf2_noise <- kpca(mnist_data_noise, kernel = "rbfdot", kpar = list(sigma = 0.0000033), features = 2)
kpc_tanh1_noise <- kpca(mnist_data_noise, kernel = "tanhdot", kpar = list(scale = 0.00000005), features = 2)
kpc_tanh2_noise <- kpca(mnist_data_noise, kernel = "tanhdot", kpar = list(scale = 0.000002), features = 2)
kpc_poly_noise <- kpca(mnist_data_noise, kernel = "polydot", kpar = list(degree = 3), features = 2)
pc_noise <- prcomp(mnist_data_noise, rank. = 2)

par(mfrow = c(2, 3))
plot(rotated(kpc_rbf1_noise), col = mnist_label, pch = 19, main = "kpca rbf 1", xlab = "CP1", ylab = "CP2")
plot(rotated(kpc_rbf2_noise), col = mnist_label, pch = 19, main = "kpca rbf 2", xlab = "CP1", ylab = "CP2")
plot(rotated(kpc_tanh1_noise), col = mnist_label, pch = 19, main = "kpca tanh 1", xlab = "CP1", ylab = "CP2")
plot(rotated(kpc_tanh2_noise), col = mnist_label, pch = 19, main = "kpca tanh 2", xlab = "CP1", ylab = "CP2")
plot(rotated(kpc_poly_noise), col = mnist_label, pch = 19, main = "kpca polynomial", xlab = "CP1", ylab = "CP2")
plot(pc_noise$x %*% matrix(c(-1, 0, 0, -1), 2), col = mnist_label, pch = 19, main = "pca", xlab = "CP1", ylab = "CP2")


par(mfrow = c(1, 4))
for(i in 1:4) {
  mat <- matrix(mnist_data[i, ], 28, 28, byrow = TRUE)
  mat <- apply(mat, 1, rev)
  mat <- apply(mat, 2, rev)
  mat <- apply(mat, 1, rev)
  image(t(mat), col = gray((0:255)/255))
}

par(mfrow = c(1, 4))
for(i in 1:4) {
  mat <- matrix(mnist_data_noise[i, ], 28, 28, byrow = TRUE)
  mat <- apply(mat, 1, rev)
  mat <- apply(mat, 2, rev)
  mat <- apply(mat, 1, rev)
  image(t(mat), col = gray((0:255)/255))
}

par(mfrow = c(1, 4))
for(i in 1:4) {
  mat <- matrix(mnist_data[i, ], 28, 28, byrow = TRUE)
  mat <- apply(mat, 1, rev)
  mat <- apply(mat, 2, rev)
  mat <- apply(mat, 1, rev)
  image(t(mat), col = gray((0:255)/255))
}

pc <- prcomp(mnist_data, rank. = 10)
img <- pc$x %*% t(pc$rotation)
img <- scale(img, center = -colMeans(mnist_data), scale = FALSE)

par(mfrow = c(1, 4))
for(i in 1:4) {
  mat <- matrix(img[i, ], 28, 28, byrow = TRUE)
  mat <- apply(mat, 1, rev)
  mat <- apply(mat, 2, rev)
  mat <- apply(mat, 1, rev)
  image(t(mat), col = gray((0:255)/255))
}
