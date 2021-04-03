library(readxl)
library(tidyverse)
library(dplyr)
library(MASS)
library(caret)
library(MVN)
library(heplots)

original_data <- read_excel("Desktop/multivariate-statistics2/activity-1/d1.xlsx")

data = original_data %>% dplyr::select(L, X0, X1)

p <- ggplot(data, aes(x=X0, y=X1, color=L, shape=L)) +
  geom_point(size = 3) + 
  theme_classic()+
  labs(title="",
       x="Largura zigomática", y = "Comprimento palatal")+
  scale_colour_discrete(name  ="Localização",
                        breaks=c("rm", "ar"),
                        labels=c("Montanhas Rochosas", "Ártico")) +
  scale_shape_discrete(name  ="Localização", 
                       breaks=c("rm", "ar"),
                       labels=c("Montanhas Rochosas", "Ártico"))

p

data_rm <- data %>% filter(L == 'rm')
data_ar <- data %>% filter(L == 'ar')

mvn(data = data_rm[-1], mvnTest = 'mardia', multivariatePlot = 'qq')$multivariateNormality
mvn(data = data_rm[-1], mvnTest = 'royston', multivariatePlot = 'qq')$multivariateNormality

mvn(data = data_ar[-1], mvnTest = 'mardia', multivariatePlot = 'qq')$multivariateNormality
mvn(data = data_ar[-1], mvnTest = 'royston', multivariatePlot = 'qq')$multivariateNormality

boxM(data[-1], data$L)

qqnorm(data$X0, pch = 1, frame = FALSE)
qqline(data$X0, col = "steelblue", lwd = 2)

s1 = cov(data_ar[-1])
s2 = cov(data_rm[-1])
n1 = 16
n2 = 9

spooled = ((n1-1)*s1 + (n2-1)*s2)/((n1-1) + (n2-2))
spooled

mean_vector <- function(df){
  p <- length(colnames(df))
  v <- matrix(0, ncol = 1, nrow = p)
  i = 1
  while(i <= p){
    v[i] = colMeans(df)[i]
    i = i + 1
  }
  return(v)
}

xbar1 <- mean_vector(data_ar[-1])
xbar2 <- mean_vector(data_rm[-1])

dif <- xbar1 - xbar2
soma <- xbar1 + xbar2
a = t(dif)%*%solve(spooled)
y1 = a%*%xbar1
y2 = a%*%xbar2
m = 0.5*(y1+y2)
x0 = matrix(c(145, 123), nrow = 2)
x01
y = a %*% x0
p2 = 0.36
p1 = 0.64

w = y - m
wc = log(p2/p1)
w < wc

data_with_test <- data


ggplot(data, aes(x=X0, y=X1, color=L, shape=L)) +
  geom_point(size = 3) + 
  theme_classic()+
  labs(title="",
       x="Largura zigomática", y = "Comprimento palatal")+
  scale_colour_discrete(name  ="Localização",
                        breaks=c("rm", "ar"),
                        labels=c("Montanhas Rochosas", "Ártico")) +
  scale_shape_discrete(name  ="Localização", 
                       breaks=c("rm", "ar"),
                       labels=c("Montanhas Rochosas", "Ártico"))+
  geom_text(aes(label=ifelse(X0 == 150 & X1 == 100, 'x01','')),hjust=0,vjust=0)

# função de discriminante linear de fisher

AD1 = lda(L ~ X0 + X1, data = data); AD1

AD1$prior
AD1$counts
AD1$means
AD1$scaling
AD1$lev
AD1$svd
AD1$N
AD1$call

# Fazendo a previsão para a própria base de dados

P1 = AD1 %>% predict(data)

#Resultados

P1$class
P1$posterior
P1$x

# Alternativa

head(P1$posterior)

# Classificando as observações em teste
teste <- data_frame(X0 = c(150, 145), X1 = c(110, 123))
teste

P1T = AD1 %>% predict(teste)

P1T$class
P1T$posterior
P1T$x

#Avaliando a qualidade do modelo com base na propria amostra
P1$class == data$L
Av = mean(P1$class == data$L)
print(Av)

#Avaliando a Classificação por grupo
table(data$L,P1$class, dnn=c("Real","Classificação"))

table(data$L,P1$class, dnn=c("Real","Classificação")) %>%
  prop.table(1) %>% round(3)

Pr = P1$class
Obs = as.factor(data$L)

C1 = confusionMatrix(Pr,Obs)
print(C1)