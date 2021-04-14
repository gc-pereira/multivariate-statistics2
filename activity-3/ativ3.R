library(readxl)
library(dplyr)
library(factoextra)
library(FactoMineR)
library(ggfortify)

d1 <- read_excel("Desktop/multivariate-statistics2/d1.xlsx")
View(d1)
data = d1 %>% select(L,X1,X2,X7,X8)

data$X7 <- as.numeric(data$X7)
data$X8 <- as.numeric(data$X8)

# Executando a ACP

pca_res <- prcomp(data)
pca_res
B1=PCA(data, graph = T, scale.unit = F)
             
# Autovalores

B1$eig

# Autovetores

B1$var$cor

# Escores Fatoriais

B1$ind$coord

library (factoextra)

# Autovalores : Alternativa

B1.val=get_eigenvalue(B1)
B1.val

# Scree Plot

fviz_eig(B1, addlabels=TRUE)

# Gr?fico das Vari?veis

fviz_pca_var(B1,col.var="black")

# Gr?fico das Observa??es

fviz_pca_ind(B1)

# Contribui??o das Vari?veis no 1o CP

fviz_contrib(B1, choice="var", axes=1)

# Contribui??o das Observa??es no 1o Plano

fviz_contrib(B1, choice="ind", axes=1:2)

# Circulo de correla??o

fviz_pca_var(data,col.var="black",repet=TRUE,
             title= "Circulo de Correla??o: Vari?veis x CP")

#Qualidade de representa??o

install.packages("corrplot")
library ("corrplot")

corrplot(B1$var$cos2, is.corr=FALSE)

fviz_cos2(B1, choice = "var", axes = 2:3, title = "Grafico para Cos2")

fviz_pca_var(B1, col.var="cos2", gradient.cols= c("4","5","6"),
             repel= TRUE )

# Contribui??o das Vari?veis para os CP 

head(data$var$contrib)

corrplot(B1$var$contrib, is.corr= FALSE)

# Contribui??o por CP

fviz_contrib(B1, choice= "var", axes=1, top=10, 
             title="Contribui??o da Vari?veis para a CP1")

# Gr?fico de de Vari?veis e Observa??es

fviz_pca_biplot(B1)

fviz_pca_biplot(data, geom.ind="point", 
                pointshape=21,
                pointsize=4)
fviz_pca_biplot(B1,pointshape=21,
                pointsize=2)+
  theme_bw()+
  labs(title = " Grafico do Primeiro Plano", x = "CP 1", y = "CP2")

fviz_pca_biplot(B1,pointshape=21,
                pointsize=2,
                fill.ind=data$L)+
  theme_bw()+
  labs(title = " Grafico do Primeiro Plano",
       x = "CP 1", y = "CP2")
216/40