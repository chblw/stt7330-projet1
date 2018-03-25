library(kernlab)
library(readr)
library(dplyr)
require(grDevices) # for colours

mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)

instances_to_use <- 1000

mnist_data <- mnist_raw[1:instances_to_use, -1]
mnist_label <- mnist_raw[1:instances_to_use, 1] %>% unlist %>% as.vector + 1

noise <- matrix(rnorm(prod(dim(mnist_data)), 0, 100), ncol = dim(mnist_data)[2])

mnist_data_noise <- mnist_data %>% as.matrix + noise
mnist_data <- mnist_data %>% as.matrix

for(i in 1:instances_to_use) {
  mnist_data_noise[i, ] <- pmin(pmax(mnist_data_noise[i, ], 0), 255)
}

kpc_rbf1 <- kpca(mnist_data, kernel = "rbfdot", kpar = list(sigma = 0.0000004), features = 2)
kpc_rbf2 <- kpca(mnist_data, kernel = "rbfdot", kpar = list(sigma = 0.000005), features = 2)
kpc_bessel <- kpca(mnist_data, kernel = "besseldot", kpar = list(sigma = 0.01, order = 10), features = 2)
kpc_tanh <- kpca(mnist_data, kernel = "tanhdot", kpar = list(scale = 0.000005), features = 2)
kpc_laplace <- kpca(mnist_data, kernel = "laplacedot", kpar = list(sigma = 0.0025), features = 2)
kpc_poly <- kpca(mnist_data, kernel = "polydot", kpar = list(degree = 3), features = 2)

pc <- prcomp(mnist_data, rank. = 2)

par(mfrow = c(1, 2))
plot(rotated(kpc), col = mnist_label, pch = 19, main = "kpca")
plot(pc$x %*% matrix(c(-1, 0, 0, -1), 2), col = mnist_label, pch = 19, main = "pca")




