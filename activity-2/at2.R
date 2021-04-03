library(ggcorrplot)
library(tidyverse)
library(dplyr)
library(doBy)
library(readxl)

d1 <- read_excel("~/Desktop/multivariate-statistics2/d1.xlsx")

d1$X3 = as.numeric(d1$X3)
d1$X4 = as.numeric(d1$X4)
d1$X5 = as.numeric(d1$X5)
d1$X6 = as.numeric(d1$X6)
d1$X7 = as.numeric(d1$X7)
d1$X8 = as.numeric(d1$X8)

corr <- cor(d1[-c(1,2)])

ggcorrplot(
           corr, 
           type = "lower", 
           outline.col = "white", 
           colors = c("#6D9EC1", "white", "#E46726"), 
           lab = T
           )

selected_data <- d1 %>% select(X1,X2)

summary(selected_data)

p <- ggplot(selected_data, aes(x=X1, y=X2)) +
  geom_point() + 
  theme_classic()+
  labs(x = "X1 (Comprimento palatal)" , y = "X2 (Comprimento pós palatal)")
p

#medias e variancias das variaveis originais
media_x1 = mean(selected_data$X1)
media_x2 = mean(selected_data$X2)
var_x1 = var(selected_data$X1)
var_x2 = var(selected_data$X2)
var_total = var_x1 + var_x2

#porcentagem da variancia total explicada por cada variavel
var_x1_pct = var_x1/var_total
var_x2_pct = var_x2/var_total

#vetor para encontrar theta que maximiza a vari?ncia da transforma??o
vec_theta = seq(0,90,0.1)
vec_pct = c()
for (num in 1:length(vec_theta)) {
  k = selected_data$X1 * cos(vec_theta[num]*pi/180) + selected_data$X2 *
    sin(vec_theta[num]*pi/180)
  vec_pct[num] = var(k)/var_total
}

#gráfico dos thetas
plot(vec_pct)

ggplot(data.frame(y = vec_pct, x = vec_theta), aes(x = vec_theta, y = vec_pct)) + 
  theme_bw() + geom_point(color = "darkred", size = 0.1) + 
  ylim(.3,1) + 
  xlim(0,110) +
  labs(x = "Angulo", y = "Porcentagem da variância total explicada por X1*")

#theta que maximiza
max(vec_pct)ected_data$X1 * cos(vec_theta1[num]*pi/180) + selected_data$X2 *
    sin(vec_theta1[num]*pi/180)
  vec_pct1[num] = var(k)/var_total

#gr?fico dos thetas
plot(vec_pct1)

#theta que maximiza
max(vec_pct1)
theta_pct1 = cbind(vec_theta1,vec_pct1)
theta_pct1
theta_max = 39.134 
var_max = 0.9451951

#x1*
x1_t = selected_data$X1 * cos(39.134*pi/180) + selected_data$X2 * 
  sin(39.134*pi/180)
x1_t

var(x1_t)
var(x1_t)/var_total

#x2*
x2_t =  -1*selected_data$X1 * sin(39.134*pi/180) + selected_data$X2 * 
  cos(39.134*pi/180)
var(x2_t)
var(x2_t)/var_total

cor(x1_t, x2_t)

transformed_data = cbind(x1_t, x2_t)
transformed_data
