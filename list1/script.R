library(readxl)
library(tidyverse)
library(dplyr)
library(MASS)
library(caret)
library(MVN)
library(heplots)
library(corrplot)

################# QUESTAO 2 #####################
f1 <- function(x) {
  (1/2)*(1-abs(x))
}
f2 <- function(x) {(1/2)*(1-abs(x-0.5))}

dom_f1 <- seq(-1,1,0.01)
dom_f2 <- seq(-0.5,1.5,0.01)

f1(dom_f1)

df.points <- data.frame(xs = c(dom_f1, dom_f2), 
                        ys = c(f1(dom_f1), f2(dom_f2)), 
                        ids = c(rep('f1', length(dom_f1)), rep('f2', length(dom_f2)))
                        )
p2 <- ggplot(data.frame(x = seq(-5, 3)), aes(x = x))

p2 + geom_point(data = df.points, aes(xs, ys, color = ids), size=1) +
  scale_color_manual(values = c("blue", "orange"))+theme_bw() + labs(x = 'x', y = "y")

x0 <- -0.4

f1(x0)/f2(x0)

################# QUESTAO 4 #####################
dados <- read_excel("Desktop/multivariate-statistics2/dados-lista1.xlsx")
head(dados)

#filtrando os dados por autor
dados_1 <- dados %>% filter(AUTOR == 1)
dados_2 <- dados %>% filter(AUTOR == 2)
dados_3 <- dados %>% filter(AUTOR == 3)

summary(dados)
summary(dados_1)
summary(dados_2)
summary(dados_3[-1])

var(dados[-1])
var(dados_1[-1])
var(dados_2[-1])
var(dados_3[-1])

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(dados[-1]), method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

#tornando a coluna autor um fator
dados$AUTOR = as.factor(dados$AUTOR)

#teste de homogeneidade de matrizes de variancias
boxM(dados[-1], dados$AUTOR)

#graus de liberdade
g = length(unique(dados$AUTOR))
p = nrow(var(dados[-1]))
v = (p*(p+1)*(g-1))/2
v

#funcao para criar vetor de medias
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

#funcao para criar o escore
dqi <- function(dados_separados, priori, xbar){
  xbar_i = mean_vector(dados_separados)
  s <- cov(dados_separados)
  di <- (-1/2)*log(det(s))-(1/2)%*%t(xbar-xbar_i)%*%solve(s)%*%(xbar-xbar_i) + log(priori)
  return(di)
}

#criando uma tabela dos resultados e a classificacao
dqi_data <- data.frame(dq1 = numeric(nrow(dados)), dq2 = numeric(nrow(dados)), dq3 = numeric(nrow(dados)), classif = numeric(nrow(dados)))
for(i in 1:nrow(dados)){
  dqi_data$dq1[i] <- dqi(dados_1[-1], 0.3333333, mean_vector(slice(dados[-1], i)))
  dqi_data$dq2[i] <- dqi(dados_2[-1], 0.3333333, mean_vector(slice(dados[-1], i)))
  dqi_data$dq3[i] <- dqi(dados_3[-1], 0.3333333, mean_vector(slice(dados[-1], i)))
  bigest <- max(c(dqi_data$dq1[i], dqi_data$dq2[i], dqi_data$dq3[i]))
  
  if(bigest == dqi_data$dq1[i]){
    dqi_data$classif[i] = 1
  }else if(bigest == dqi_data$dq2[i]){
    dqi_data$classif[i] = 2
  }else{
    dqi_data$classif[i] = 3
  }
}
dqi_data

#taxa de acerto
mean(dqi_data$classif == dados[1])

# Calculando a função de discriminate quadratica (Var diferentes)
DQ2 = qda(AUTOR ~ PALAVRA + CARACTERES + FACILIDADE + LEGIBILIDADE, data=dados)

# Resultados
print(DQ2)

# Predizendo (classificando observações da base)
PQ2 = DQ2 %>% predict(dados)

PQ2$class
PQ2$posterior

PQ2

# Média dos acertos
MA2 = mean(PQ2$class == dados$AUTOR)
MA2

table(dados$AUTOR,PQ2$class, dnn=c("Real","Classificação")) %>%
  prop.table(1) %>% round(3)

table(dados$AUTOR,PQ2$class, dnn=c("Real","Classificação")) 