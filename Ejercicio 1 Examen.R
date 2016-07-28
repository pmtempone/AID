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
city <- read.csv2("C:/Users/Pablo/Google Drive/Maestria/AID/AID parcial 2/city.csv")

#a- Realizar un análisis de cluster jerárquico explicando la clasificación obtenida y el
#método elegido.

matcity <- as.matrix(city[,-1])
ciudades <- city$city
rownames(matcity) <- ciudades

d2 = dist(matcity,method = "euclidean")
city.clust.sin = as.dendrogram(hclust(d2, method = "single")) %>% set("branches_lwd", 2)
city.clust.com = as.dendrogram(hclust(d2, method = "complete")) %>% set("branches_lwd", 2)
city.clust.avg = as.dendrogram(hclust(d2, method = "average")) %>% set("branches_lwd", 2)
city.clust.ward = as.dendrogram(hclust(d2, method = "ward.D")) %>% set("branches_lwd", 2)
city.dend = dendlist("Cercano" = city.clust.sin, "Lejano" = city.clust.com, "Promedio" = city.clust.avg,"Ward"=city.clust.ward)


#diferencias

corrplot(cor.dendlist(city.dend), "pie", "lower")
'

Como se puede ver en gráfico, todos los métodos tienen correlación muy alta, sin embargo los métodos de vecino más lejano, promedio y ward tienen correlación 1, es decir, que se puede tomar cualquiera de estos métodos para hacer la clasificación pues van a generar el mismo resultado. Se elige el vecino más lejano, el cual calcula la distanca entre los elementos más lejanos de dos grupos y los une cuando esta distancia es mínima con respecto a todos los grupos.'

plot(city.clust.com %>% set("branches_k_color", k=3) %>% set("branches_lwd", 2), main = "Complete")
city3 <- cutree(city.clust.com,3)
city$clust <- factor(city3)

Tabla <- describeBy(city[,-c(1,9)], group = city$clust, mat = T)[,c("group1", "mean")]
Tabla <- cbind(Tabla,"promedio gral"=rep(colMeans(city[,-c(1,9)]), each = 3))

Tabla

#b- Idem para un clúster no jerárquico.

pca.pizzas <- PCA(city[,-c(1,9)])
PCA.1.2 = pca.pizzas$ind$coord[,c(1:2)]

kclusts <- data.frame(k=1:9) %>% group_by(k) %>% do(kclust=kmeans(city[,-c(1,9)], .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))


assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], PCA.1.2))

clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

ggplot(clusterings, aes(k, tot.withinss)) + geom_line() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw()

#2 grupos parece ser lo mejor

k4 = ggplot(PCA.1.2, aes(x = Dim.1, y = Dim.2), theme = NULL) + geom_point(size = 3, color = subset(assignments, k == 3)$.cluster) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw() + ggtitle("Kmeans")
plot(k4)

#c- Explique la diferencia entre los métodos aglomératelos y divisivos. Ejemplifique.
'Los métodos aglomerativos son métodos de unión, es decir, estos crean grupos mediante la unión de dos grupos cercanos partiendo del hecho de que cada individuo es un grupo, mientras que los métodos divisivos separan grupos existentes, parten del hecho de que el conjunto completo de datos es un grupo y van separando los grupos más lejanos.'

#d- Indique dos maneras distintas de decidir el número de conglomerados.
'Unos de los criterios para elegir el número de conglomerados cuando se están usando métodos jerárquicos es tener en cuenta la distancia más larga en el dendrograma, ésta va a determinar la cantidad de grupos. En cuanto a el método de kmeans, una forma de estimar el número de grupos es usar el gráfico de codo de las sumas de los cuadrados dentro de los grupos, el gráfico determina la cantidad de grupos cuando la reducción en la cantidad de grupos se estabiliza.'

