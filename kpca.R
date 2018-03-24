library(kernlab)
library(kpca)
library(readr)
library(dplyr)

mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)
