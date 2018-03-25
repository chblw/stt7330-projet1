library(kernlab)
library(readr)
library(dplyr)

angles <- runif(300, 0, 2 * pi)
radius <- runif(300, c(1, 5, 10), c(2, 7, 13))
circle_class <- rep(2:4, 100)

x <- radius * cos(angles)
y <- radius * sin(angles)

circle_data <- cbind(x, y)

plot(circle_data, col = circle_class, asp = 1)

kpc_rbf <- kpca(circle_data, kernel = "rbfdot", kpar = list(sigma = 0.05), features = 2)
kpc_bessel <- kpca(circle_data, kernel = "besseldot", kpar = list(sigma = 0.6, order = 3), features = 2)

par(mfrow = c(1, 3))
plot(circle_data, col = circle_class, asp = 1, main = "Données originales", pch = 19)
plot(rotated(kpc_rbf), col = circle_class, pch = 19, main = "kpca rbf", asp = 1, xlab = "Première composante", ylab = "Deuxième composante")
plot(rotated(kpc_bessel), col = circle_class, pch = 19, main = "kpca bessel", asp = 1, xlab = "Première composante", ylab = "Deuxième composante")

pc <- prcomp(circle_data, rank. = 1)
kpc_rbf <- kpca(circle_data, kernel = "rbfdot", kpar = list(sigma = 0.05), features = 1)
kpc_bessel <- kpca(circle_data, kernel = "besseldot", kpar = list(sigma = 0.6, order = 3), features = 1)

plot(pc$x, rep(0, 300), col = circle_class, pch = 19, main = "pca", xlab = "Première composante")
plot(rotated(kpc_rbf), rep(0, 300), col = circle_class, pch = 19, main = "kpca rbf", xlab = "Première composante")
plot(rotated(kpc_bessel), rep(0, 300), col = circle_class, pch = 19, main = "kpca bessel", xlab = "Première composante")
