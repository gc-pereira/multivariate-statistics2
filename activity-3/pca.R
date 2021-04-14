library(readxl)
library(dplyr)
library(factoextra)
library(FactoMineR)
library(ggfortify)

d1 <- read_excel("Desktop/multivariate-statistics2/d1.xlsx")
data = d1 %>% select(X1,X2,X7,X8)

data$X7 <- as.numeric(data$X7)
data$X8 <- as.numeric(data$X8)

eigen(cov(data))
eigen(cov(data))$vectors%*%diag(eigen(cov(data))$values)%*%solve(eigen(cov(data))$vectors)

percent_variance <- function(eigenvalue){
  sum_ <- sum(eigenvalue)
  m = matrix(NA, nrow = 1, ncol = length(eigenvalue))
  for(i in 1:length(eigenvalue)){
    m[1,i] = eigenvalue[i]/sum_
  }
  return(m)
}

percent_variance(
  eigenvalue = eigen(cov(data))$values
  )


pca_res <- prcomp(data)
autoplot(pca_res, loadings = T, loadings.colour = 'blue', loadings.label = TRUE)

PCA(data, graph = T, scale.unit = F)$var

eigenvalues = as.matrix(eigen(cov(data))$values)
eigenvalues

for(i in 1:length(eigenvalues)){
  ei = eigen(cov(data))$vectors[,i]
  print(t(ei)%*%cov(data)%*%ei)
}
