library(kernlab)
library(readr)
library(dplyr)
library(plotly)
angles <- runif(200, 0, 2 * pi)
radius <- runif(200, c(0, 7), c(2, 9))
circle_class <- rep(c(2,4), 100)

x <- radius * cos(angles)
y <- radius * sin(angles)

circle_data <- cbind(x, y)

plot(circle_data, col = circle_class, asp = 1)

phi_data = function(x,y) {
  return(c(x^2,sqrt(2)*x*y,y^2))
}
projected_data =numeric(600)
for(index_to_project in 1:200){
  point_to_project = circle_data[index_to_project,]
  projected_point = phi_data(point_to_project[1],point_to_project[2])
  
  projected_data[(3*(index_to_project-1)+1):(3*(index_to_project))] = projected_point
}
projected_data = matrix(projected_data, nrow=200, ncol=3, byrow = TRUE)
projected_data_as_dataframe = as.data.frame(projected_data)
colnames(projected_data_as_dataframe) = c("x", "y", "z")
pca <- princomp(projected_data_as_dataframe)
plot(pca$score[,1],pca$scores[,2], col=circle_class,main="Données transformées selon les composantes principales", xlab="Première composante principale", ylab="Deuxième composante principale")

plotly::plot_ly(projected_data_as_dataframe, x=~x, y=~y, z=~z, color=circle_class) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'x'),
                      yaxis = list(title = 'y'),
                      zaxis = list(title = 'x*y')))

kerneled_data = (circle_data[,1] * circle_data[,2] +1)^2
plot(cbind(rep(1,200),kerneled_data), col=circle_class, ylab="Valeur de la fonction noyau", main="Nouvelle position en utilisant l'information polynomiale")

kpc_rbf <- kpca(circle_data, kernel = "rbfdot", kpar = list(sigma = 0.05), features = 2)
kpc_bessel <- kpca(circle_data, kernel = "besseldot", kpar = list(sigma = 0.6, order = 3), features = 2)

par(mfrow = c(1, 1))
plot(rotated(kpc_rbf), col = circle_class, pch = 19, main = "kpca rbf", asp = 1, xlab = "Première composante", ylab = "Deuxième composante")
plot(rotated(kpc_bessel), col = circle_class, pch = 19, main = "kpca bessel", asp = 1, xlab = "Première composante", ylab = "Deuxième composante")

pc <- prcomp(circle_data, rank. = 1)
kpc_rbf <- kpca(circle_data, kernel = "rbfdot", kpar = list(sigma = 0.05), features = 1)
kpc_bessel <- kpca(circle_data, kernel = "besseldot", kpar = list(sigma = 0.6, order = 3), features = 1)

plot(pc$x, rep(0, 300), col = circle_class, pch = 19, main = "pca", xlab = "Première composante")
plot(rotated(kpc_rbf), rep(0, 300), col = circle_class, pch = 19, main = "kpca rbf", xlab = "Première composante")
plot(rotated(kpc_bessel), rep(0, 300), col = circle_class, pch = 19, main = "kpca bessel", xlab = "Première composante")
