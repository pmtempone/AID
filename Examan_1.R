library(FactoMineR)
library(foreign)
library(psych)
library(knitr)
library(xtable)
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(dendextendRcpp))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggfortify))
library(corrplot)
#library(Stuff)
suppressPackageStartupMessages(library(dplyr))
library(broom)

#Ejercicio 1: 
milk <- read.csv2("C:/Users/Pablo/Google Drive/Maestria/AID/Segundo Examen/milk.csv")

#a- Realizar un análisis de cluster jerárquico explicando la clasificación obtenida y el
#método elegido.

matmilk <- as.matrix(milk[,-1])
animales <- milk$animal
rownames(matmilk) <- animales


d2 = dist(matmilk,method = "euclidean")
city.clust.sin = as.dendrogram(hclust(d2, method = "single")) %>% set("branches_lwd", 2)
city.clust.com = as.dendrogram(hclust(d2, method = "complete")) %>% set("branches_lwd", 2)
city.clust.avg = as.dendrogram(hclust(d2, method = "average")) %>% set("branches_lwd", 2)
city.clust.ward = as.dendrogram(hclust(d2, method = "ward.D")) %>% set("branches_lwd", 2)
city.dend = dendlist("Cercano" = city.clust.sin, "Lejano" = city.clust.com, "Promedio" = city.clust.avg,"Ward"=city.clust.ward)

corrplot(cor.dendlist(city.dend), "pie", "lower")

plot(city.clust.com %>% set("branches_k_color", k=3) %>% set("branches_lwd", 2), main = "Complete")
milk3 <- cutree(city.clust.com,3)
milk$clust <- factor(milk3)

Tabla <- describeBy(milk[,-c(1,7)], group = milk$clust, mat = T)[,c("group1", "mean")]
Tabla <- cbind(Tabla,"promedio gral"=rep(colMeans(milk[,-c(1,7)]), each = 3))

Tabla

#b- Idem para un cluster no jerárquico.

pca.pizzas <- PCA(milk[,-c(1,7)])
PCA.1.2 = pca.pizzas$ind$coord[,c(1:2)]

kclusts <- data.frame(k=1:5) %>% group_by(k) %>% do(kclust=kmeans(milk[,-c(1,7)], .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))


assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], PCA.1.2))

clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

ggplot(clusterings, aes(k, tot.withinss)) + geom_line() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw()

#2 grupos parece ser lo mejor por outliers

k4 = ggplot(PCA.1.2, aes(x = Dim.1, y = Dim.2), theme = NULL) + geom_point(size = 3, color = subset(assignments, k == 3)$.cluster) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw() + ggtitle("Kmeans")
plot(k4)


#sin deer
pca.pizzas <- PCA(milk[milk$animal!='deer',-c(1,7)])
PCA.1.2 = pca.pizzas$ind$coord[,c(1:2)]

kclusts <- data.frame(k=1:5) %>% group_by(k) %>% do(kclust=kmeans(milk[milk$animal!='deer',-c(1,7)], .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))


assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], PCA.1.2))

clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

ggplot(clusterings, aes(k, tot.withinss)) + geom_line() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw()

#2 grupos parece ser lo mejor por outliers

k4 = ggplot(PCA.1.2, aes(x = Dim.1, y = Dim.2), theme = NULL) + geom_point(size = 3, color = subset(assignments, k == 3)$.cluster) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw() + ggtitle("Kmeans")
plot(k4)
