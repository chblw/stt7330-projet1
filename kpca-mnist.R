library(kernlab)
library(readr)
library(dplyr)
require(grDevices) # for colours

mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)

mnist_data <- mnist_raw[1:1000, -1]
mnist_label <- mnist_raw[1:1000, 1] %>% unlist %>% as.vector + 1

noise <- matrix(rnorm(prod(dim(mnist_data)), 0, 50), ncol = dim(mnist_data)[2])

mnist_data_noise <- mnist_data + noise

mnist_data_noise <- pmin(mnist_data_noise, 255)


mat <- matrix(rev(mnist_data[1, ]), 28, 28)

image(1:28, 1:28, mat, col=gray((0:255)/255))

image(as.matrix(mat), col=gray((0:255)/255))


# kpc <- kpca(~., data = mnist_data, kernel = "rbfdot", kpar = list(sigma = 0.00000005), features = 2)
# kpc <- kpca(~., data = mnist_data, kernel = "polydot", kpar = list(degree = 10, offset = 0), features = 2)
kpc <- kpca(~., data = mnist_data, kernel = "tanhdot", kpar = list(scale = 0.000001, offset = 0), features = 2)
pc <- prcomp(~., data = mnist_data, rank. = 2)

par(mfrow = c(1, 2))
plot(rotated(kpc), col = mnist_label, pch = 19, main = "kpca")
plot(pc$x, col = mnist_label, pch = 19, main = "pca")
