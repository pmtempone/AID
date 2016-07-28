library(readxl)
suppressPackageStartupMessages(library(xlsx))
library(ggplot2)
#library(Stuff)
library(knitr)
library(reshape)
library(biotools)
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(caret))
library(MVN)
library(klaR)
library(Rmisc)
library(Hotelling)
library(profileR)
library(MASS)
library(lattice)

#Ejercicio 1

Gorriones <- read.csv2("C:/Users/Pablo/Google Drive/Maestria/AID/clase 10/Gorriones.txt")

Gorriones = Gorriones[, -1]
Gorriones$Sobrevida <- factor(Gorriones$Sobrevida)
  bbiokable(head(Gorriones), align = "c")


#i Compare las medias de cada una de las variables entre los grupos. Realice una exploración grafica.

g1 = ggplot(data = Gorriones[,c("Largo", "Sobrevida")], aes(x = Sobrevida, y = Largo, fill = Sobrevida)) + geom_boxplot()
g2 = ggplot(data = Gorriones[,c("Alas", "Sobrevida")], aes(x = Sobrevida, y = Alas, fill = Sobrevida)) + geom_boxplot()
g3 = ggplot(data = Gorriones[,c("PicoCabeza", "Sobrevida")], aes(x = Sobrevida, y = PicoCabeza, fill = Sobrevida)) + geom_boxplot()
g4 = ggplot(data = Gorriones[,c("Humero", "Sobrevida")], aes(x = Sobrevida, y = Humero, fill = Sobrevida)) + geom_boxplot()
g5 = ggplot(data = Gorriones[,c("QuillaEsternon", "Sobrevida")], aes(x = Sobrevida, y = QuillaEsternon, fill = Sobrevida)) + geom_boxplot()
multiplot(g1, g2, g3, g4, g5, layout = matrix(c(1,1,2,2,3,3,4,4,4,5,5,5), byrow = T, ncol = 6))


#hay diferencia en el Largo y en las Alas

#ii Compare los vectores medios de ambos grupos. Tiene sentido realizar un análisis discriminante?

describeBy(Gorriones, group = "Sobrevida", mat = T)[1:10,c("group1", "mean")]
#No tiene sentido hacer un análisis discriminante (ninguna media discrimina bien)

#iii Realice el análisis discriminante a partir de las variables que considere adecuada. incluir.

'Las variables consideradas para el modelo son Largo y Alas, pues estas muestran diferencias en sus medias.'

modelo1 = lda(Sobrevida ~ Largo + Alas, data = Gorriones, CV = T)
confusionMatrix(Gorriones$Sobrevida,modelo1$class)

#iv. Satisface los supuestos del modelo?. Resulta una buena clasificación?.

fit <- hotelling.test(.~Sobrevida,data = Gorriones,perm =  TRUE)
fit #p valor grande no discrimina
plot(fit)

roystonTest(Gorriones[, -ncol(Gorriones)])
#En este caso, no se rechaza la hipótesis de normalidad multivariada, entonces es correcto asumirla.

boxM(Gorriones[, -ncol(Gorriones)], Gorriones[, ncol(Gorriones)])
#Como el valor p obtenido es mayor a 0.05, no se rechaza la hipótesis nula, entonces se puede asumir la homogeneidad de las matrices de varianzas y covarianzas para cada uno de los grupos de gorriones.


#ejercicio 2

hemofilia <- read.csv2("C:/Users/Pablo/Google Drive/Maestria/AID/Clase 09/hemofilia.csv")
hemofilia[,1] = as.factor(hemofilia[,1])
head(hemofilia)

#a. Considera que ambas variables pueden ser de ayuda para esta clasificación?

a1 <- ggplot(data = hemofilia,aes(x=Portador,y=ActividadAHF,fill=Portador))+geom_boxplot()
a2 <- ggplot(data = hemofilia,aes(x=Portador,y=AntigenoAHF,fill=Portador))+geom_boxplot()
multiplot(a1,a2,cols = 2)

#hay diferencias entre ambas variables

#b. Realice un gráfico bivariado de ambos grupos conjuntamente.

ggplot(data = hemofilia, aes(x = ActividadAHF, y = AntigenoAHF, col = Portador)) + geom_point(size = 2)

#c. Le parece a partir de este grafico que una función discriminante lineal es adecuada.
'SI'

#d. Realice un análisis discriminante con 50 registros elegidos al azar.

set.seed(12345)

muestra <- sample(1:nrow(hemofilia),size = 50)

Hemofilia.Entrenamiento = hemofilia[muestra, ]
Hemofilia.Test = hemofilia[-muestra,]

fit.hemofilia <- lda(Portador ~.,data = Hemofilia.Entrenamiento)
fit.hemofilia

#e. Utilice los restantes registros para estimar la calidad de la regla discriminante.

pred.hemofilia <- predict(fit.hemofilia,Hemofilia.Test)

confusionmatrix(Hemofilia.Test$Portador,pred.hemofilia$class)

#Ejercicio 3

pulso <- read.csv("C:/Users/Pablo/Google Drive/Maestria/AID/Clase 09/pulso.csv", sep=";")
pulso$sexo <- factor(pulso$sexo)
pulso$fuma <- factor(pulso$fuma)
pulso <- pulso[!is.na(pulso$sexo),]

head(pulso)

#a- Interesa saber si la información del pulso antes y después de correr permite discriminar el sexo.

ggplot(data = pulso,aes(x=Pulso.previo,y=Pulso.posterior,col=sexo))+geom_point()

hot.sex <- hotelling.test(Pulso.previo+Pulso.posterior~sexo,data = pulso)
hot.sex

set.seed(12345)

intrain <- createDataPartition(y=pulso$sexo,p=0.60,list = FALSE)
sexo_train <- pulso[intrain,]
sexo_test <- pulso[-intrain,]

fit.sexo <- lda(sexo ~Pulso.previo+Pulso.posterior,data = sexo_train)
fit.sexo

pred.sexo <- predict(fit.sexo,sexo_test)

confusionMatrix(sexo_test$sexo,pred.sexo$class)

#b- Ídem con la categoría de fumador
ggplot(data = pulso,aes(x=Pulso.previo,y=Pulso.posterior,col=fuma))+geom_point() #pareciera no discriminar bien por sexo

hot.fuma <- hotelling.test(Pulso.previo+Pulso.posterior~fuma,data = pulso)
hot.fuma #p valor alto, no discriminaria bien


set.seed(12345)

intrain <- createDataPartition(y=pulso$fuma,p=0.60,list = FALSE)
fuma_train <- pulso[intrain,]
fuma_test <- pulso[-intrain,]

fit.fuma <- lda(fuma ~Pulso.previo+Pulso.posterior,data = fuma_train)
fit.fuma

pred.fuma <- predict(fit.fuma,fuma_test)

confusionMatrix(fuma_test$fuma,pred.fuma$class)

#no discrimina tan bien como el anterior

#c- En cuál de los dos casos discrimina mejor?.
'Discrimina mejor sobre sexo'

#Ejercicio 4

iris <- iris

#i. analice cuales valores medios son diferentes en las especies.

iris_i <- describeBy(iris, group = iris$Species, mat = T)[,c("group1", "mean", "sd")]
iris_i


names <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")

mylist <- list(iris$Sepal.Length,iris$Sepal.Width,iris$Petal.Length,iris$Petal.Width)

makeProfilePlot(mylist,names)


parallelplot(~iris[1:4], iris, groups = Species,
             horizontal.axis = FALSE, scales = list(x = list(rot = 90))) #diagrama de lineas similar a perfiles


#ii. aplique alguna regla de clasificación para discriminar los 3 grupos.

set.seed(12345)

intrain <- createDataPartition(y=iris$Species,p=0.70,list = FALSE)
iris_train <- iris[intrain,]
iris_test <- iris[-intrain,]

fit.iris <- lda(Species ~.,data = iris_train)
fit.iris #LD1 (eje 1) explica el 99%



#iii. ¿Cuál es el porcentaje de bien clasificados? ¿Y los porcentajes de bien clasificados para cada
#especie?


pred.iris <- predict(fit.iris,iris_test)

confusionMatrix(iris_test$Species,pred.iris$class)

pred.tot <- predict(fit.iris,iris)

confusionMatrix(iris$Species,pred.tot$class)

#Accuracy : 0.98   



