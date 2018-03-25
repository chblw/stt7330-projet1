library(kernlab)
library(readr)
library(dplyr)

mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)



mnist_data <- mnist_raw[1:1000, -1]
mnist_label <- mnist_raw[1:1000, 1] %>% unlist %>% as.vector

kpc <- kpca(~., data = mnist_data, kernel = "rbfdot", kpar = list(sigma = 0.00000005), features = 2)

plot(kpc, col = mnist_label, pch = 19)


pc <- prcomp(~., data = mnist_data, rank. = 2)

plot(pc$x, col = mnist_label, pch = 19)
