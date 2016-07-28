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


#Ejercicio 1

X = matrix(c(1,2,5,6,3,4,3,4,0,1,2,1), ncol = 2)
rownames(X) = c(1:nrow(X))
colnames(X) = paste("X", 1:2, sep = "")
kable(X, align = c("c", "c"))


#i. Grafique en R2 y construya el dendrograma correspondiente utilizando el criterio del vecino más lejano (utilizar la distancia euclídea).

'Vecino más lejano:'

X.clust.com1 = as.dendrogram(hclust(dist(X, "euclidean"), method = "complete")) %>% set("branches_lwd", 2)
plot(X.clust.com)


#ii. Igual que el anterior, utilizando el criterio de vecino más cercano y distancia
#Manhattan

X.clust.com = as.dendrogram(hclust(dist(X, "manhattan"), method = "single")) %>% set("branches_lwd", 2)
plot(X.clust.com)

#iii. Repita el item anterior pero aplicando el criterio del promedio y distancia
#Chebyshev.

X.clust.avg = as.dendrogram(hclust(dist(X, "euclidean"), method = "average")) %>% set("branches_lwd", 2)
plot(X.clust.avg)


X.clust.avg = as.dendrogram(hclust(dist(X, "chebyshev"), method = "average")) %>% set("branches_lwd", 2)
plot(X.clust.avg)


#iv. Repita el ejercicio utilizando las variables estandarizadas. Compare los resultados.
X.scale = scale(X)
X.clust.scale.com = as.dendrogram(hclust(dist(X.scale, "euclidean"), method = "complete")) %>% set("branches_lwd", 2)
tanglegram(dendlist("Normalizado" = X.clust.scale.com, "Original" = X.clust.com))


X.clust.scale.sin = as.dendrogram(hclust(dist(X.scale, "manhattan"), method = "single")) %>% set("branches_lwd", 2)
tanglegram(dendlist("Normalizado" = X.clust.scale.sin, "Original" = X.clust.com))

X.clust.scale.avg = as.dendrogram(hclust(dist(X.scale, "euclidean"), method = "average")) %>% set("branches_lwd", 2)
tanglegram(dendlist("Normalizado" = X.clust.scale.avg, "Original" = X.clust.avg))


#Ejercicio 2

d2 = matrix(c(0, 4, 18, 20, 18, 4, 0, 10, 15, 20, 18, 10, 0, 24, 8, 20, 15, 24, 0, 6, 18, 20, 8, 6, 0), ncol = 5)
colnames(d2) = paste("E", 1:ncol(d2), sep = "")
rownames(d2) = paste("E", 1:ncol(d2), sep = "")
kable(d2, align = "c")


d2 = as.dist(d2)
d2

'Vecino más cercano:'

d2.clust.sin = as.dendrogram(hclust(d2, method = "single")) %>% set("branches_lwd", 2)
d2.clust.com = as.dendrogram(hclust(d2, method = "complete")) %>% set("branches_lwd", 2)
d2.clust.avg = as.dendrogram(hclust(d2, method = "average")) %>% set("branches_lwd", 2)
d2.dend = dendlist("Cercano" = d2.clust.sin, "Lejano" = d2.clust.com, "Promedio" = d2.clust.avg)


#diferencias

corrplot(cor.dendlist(d2.dend), "pie", "lower")
#El gráfico anterior permite observar que el dendrograma cercano es diferente de los otros dos dendrogramas (vecino más lejano y promedio) los cuales son iguales. Para observar las diferencias entre los dendrogramas se seleccionaron el dendrograma del vecino más lejano y el del vecino más cercano

tanglegram(d2.dend, which = c(1, 2))


'Las diferencias entre los dos dendrogramas están en la unión del elemento 3 con los grupos formados por 1-2 y 4-5, en el caso del vecino más cercano, se ve que este elemento se une primero a 4-5, mientras que en el dendrograma del vecino más lejano, este se une primero a 1-2.'

#Ejercicio 3

pizzas <- read.csv2("C:/Users/Pablo/Google Drive/Maestria/AID/clase 10/pizzas.csv")

pizzas <- pizzas[,-c(1:2)]

'i. Realice un Análisis en Componentes Principales. ¿Qué proporción de la variabilidad
total en las variables medidas explican las dos primeras componentes? Utilizando un
gráfico de individuos determinar grupos en los datos. ¿Cuántos grupos hay? ¿Cuáles
pizzas pertenecen a cuáles agrupamientos? Comparar con el ítem anterior.'

princomp(pizzas)
pca.pizzas <- PCA(pizzas)
kable(head(pca.pizzas$eig, 2), align = "c")


ggplot(data = pca.pizzas$ind$coord, aes(x = Dim.1, y = Dim.2)) + geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw()
#separa en 5 grupos aprox

grupos = as.data.frame(cutree(as.dendrogram(hclust(dist(pca.pizzas$ind$coord[,c(1:2)]), "complete")) %>% set("branches_lwd", 2), 5))
colnames(grupos) = "Grupo"
as.numeric(rownames(subset(grupos, Grupo == 1)))
as.numeric(rownames(subset(grupos, Grupo == 2)))
as.numeric(rownames(subset(grupos, Grupo == 3)))
as.numeric(rownames(subset(grupos, Grupo == 4)))
as.numeric(rownames(subset(grupos, Grupo == 5)))


'ii. Aplique un método de agrupamiento a los resultados del ítem anterior (valores de
los casos sobre las componentes).'

PCA.pizzas.clust.sin = as.dendrogram(hclust(dist(pca.pizzas$ind$coord[,c(1:2)]), "single")) %>% set("branches_lwd", 2)
PCA.pizzas.clust.comp = as.dendrogram(hclust(dist(pca.pizzas$ind$coord[,c(1:2)]), "complete")) %>% set("branches_lwd", 2)
PCA.pizzas.clust.avg = as.dendrogram(hclust(dist(pca.pizzas$ind$coord[,c(1:2)]), "average")) %>% set("branches_lwd", 2)
PCA.pizzas.clust = dendlist("Cercano" = PCA.pizzas.clust.sin, "Lejano" = PCA.pizzas.clust.comp, "Promedio" =
                              PCA.pizzas.clust.avg)
par(mar = c(5, 4, 2, 2), mfrow = c(3, 1))
plot(PCA.pizzas.clust.comp %>% set("branches_k_color", k=5) %>% set("branches_lwd", 2), main = "Lejano")
plot(PCA.pizzas.clust.sin %>% set("branches_k_color", k=5) %>% set("branches_lwd", 2), main = "Cercano")
plot(PCA.pizzas.clust.avg %>% set("branches_k_color", k=5) %>% set("branches_lwd", 2), main = "Promedio")
par(mfrow = c(1,1))


#iii. Aplique el método de K-Medias a los datos de manera de obtener 5 grupos.Compare con los resultados anteriores.

# cálculos de diferentes tamaños de grupos del algoritmo k means
PCA.1.2 = pca.pizzas$ind$coord[,c(1:2)]

#completo faltantes

pizzas2 <- complete(mice(pizzas))
pizzas <- pizzas2

kclusts <- data.frame(k=1:9) %>% group_by(k) %>% do(kclust=kmeans(pizzas, .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))


assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], PCA.1.2))

clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

# Será usado el de 5 para este ejercicio, el resto serán usandos en el literal iv.
set.seed(NULL)
pizzas.clust.sin = as.factor(cutree(as.dendrogram(hclust(dist(pizzas), "single")), 5))
pizzas.clust.comp = as.factor(cutree(as.dendrogram(hclust(dist(pizzas), "complete")), 5))
pizzas.clust.avg = as.factor(cutree(as.dendrogram(hclust(dist(pizzas), "average")), 5))
k4 = ggplot(PCA.1.2, aes(x = Dim.1, y = Dim.2), theme = NULL) + geom_point(size = 3, color = subset(assignments, k == 5)$.cluster) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw() + ggtitle("Kmeans")
k1 = ggplot(PCA.1.2, aes(x = Dim.1, y = Dim.2), theme = NULL) + geom_point(size = 3, color = pizzas.clust.sin) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw() + ggtitle("Vecino más cercano")
k2 = ggplot(PCA.1.2, aes(x = Dim.1, y = Dim.2), theme = NULL) + geom_point(size = 3, color = pizzas.clust.comp) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw() + ggtitle("Vecino más lejano")
k3 = ggplot(PCA.1.2, aes(x = Dim.1, y = Dim.2), theme = NULL) + geom_point(size = 3, color = pizzas.clust.avg) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw() + ggtitle("Promedio")
multiplot(k4, k1, k2, k3, cols = 2)

#iv. Resuma los resultados: ¿tienen los datos una estructura como para agruparlos? En el caso de que su respuesta sea afirmativa: ¿en cuántos grupos le parece más conveniente? Justifique.

p1 <- ggplot(assignments, aes(Dim.1, Dim.2)) + geom_point(aes(color=.cluster), size = 4) + facet_wrap(~ k) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw()
p1


ggplot(clusterings, aes(k, tot.withinss)) + geom_line() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw() #Este gráfico ratifica una vez más que el número adecuado de grupos es 5.


'Las componentes principales mostradas en el literal i. muestran una partición de entre 5 y 6 grupos, sin embargo, para tener certeza sobre el número de grupos adecuado, se exploran diferentes tamaños de grupos:'

#Ejercicio 6

proteinas <- read.csv2("C:/Users/Pablo/Google Drive/Maestria/AID/clase 10/proteinas.csv", row.names=1)

#a- Utilizando el método de Ward y la distancia euclídea particionar en dos
#clusters. ¿Cómo llamaría a cada uno de ellos?

Consumo.clust = as.dendrogram(hclust(dist(proteinas, "euclidean"), method = "ward.D"))
plot(Consumo.clust %>% set("branches_k_color", k=2) %>% set("branches_lwd", 2), main = "Ward 2 Grupos")
Consumo.clust.2 = cutree(Consumo.clust, k = 2)
Consumo2g = cbind(proteinas, Consumo.clust.2)
Consumo2g[, "Consumo.clust.2"] = as.factor(Consumo2g[, "Consumo.clust.2"])
caracterizacion2g = cbind(describeBy(Consumo2g, "Consumo.clust.2", mat = T)[1:18, c("group1", "mean", "sd")], rep(colMeans(proteinas), each = 2))
colnames(caracterizacion2g)[4] = "Promedio Gen"
kable(caracterizacion2g, align = "c")


#b- Idem a- pero en cuatro clusters. Utilizando el dendograma, ¿con cuál de las
#clasificaciones se quedaría?

plot(Consumo.clust %>% set("branches_k_color", k=4) %>% set("branches_lwd", 2), main = "Ward 4 Grupos")
Consumo.clust.4 = cutree(Consumo.clust, k = 4)
Consumo4g = cbind(proteinas, Consumo.clust.4)
Consumo4g[, "Consumo.clust.4"] = as.factor(Consumo4g[, "Consumo.clust.4"])
Caracterizacion4g = cbind(describeBy(Consumo4g, "Consumo.clust.4", mat = T)[, c("group1", "mean", "sd")][1:36,], rep(colMeans(proteinas), each = 4))
colnames(Caracterizacion4g)[4] = "Promedio Gen"
kable(Caracterizacion4g, align = "c")


'
c- Realice una caracterización de las variables.

Los grupos están caracterizados de la siguiente manera:

Grupo 1: alto consumo de cereal, frutos secos y frutas y vegetales,
Grupo 2: alto comsumo de pescados y carnes, consumo promedio de embutidos y leche, consumo bajo de frutos secos y frutas y vegetales.
Grupo 3: alto consumo de cereal, consumo promedio de frutos secos, consumo bajo de frutas y vegetales.
Grupo 4: alto consumo de frutos secos, frutas y vegetales, pescado y embutidos.

'

'd- Compare los resultados obtenidos con el de componentes principales.'
proteinas.pca <- prcomp(proteinas)
#Consumo.PCA = PCA(proteinas, graph = F)
#kable(head(Consumo.PCA$eig), align = "c")
'Para la comparación se toman las 3 primeras componentes principales, las cuales explican el 74% de la varianza.'

P1 <- biplot(proteinas.pca,choices = c(1,2))
P2 <- biplot(proteinas.pca,choices = c(1,3))

multiplot(P1, P2)


