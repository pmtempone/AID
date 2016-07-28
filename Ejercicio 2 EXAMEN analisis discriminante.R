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
anticuerpos <- read.csv2("C:/Users/Pablo/Google Drive/Maestria/AID/AID parcial 2/anticuerpos.csv")

#a- En caso de rechazar realice un análisis discriminante lineal o cuadrático según
#corresponda y explique cuál ha elegido y por qué.

'
    Hipótesis nula:

Los vectores de medias de colesterol total, albumina, calcio y áurico son iguales para los diferentes grupos.

Hipótesis alternativa:

Existe al menos un vector de medias de colesterol total, albumina, calcio y áurico que es diferente de los demás grupos.

Para realizar el contraste se usa la prueba de Hotelling - Lawley:'

fit <- hotelling.test(.~Grupo,data = anticuerpos)
fit
'Este contraste rechaza la hipótesis de igualdad del vector de medias, para verificar que el análisis es correcto, se procede a verificar las hipótesis:

Normalidad multivariada (Se usa el test de Royston):'

roystonTest(anticuerpos[, -1])


'No se satisface el supuesto de normalidad multivariada, por lo tanto no es posible hacer un análisis discriminante a menos que se encuentre una transformación que permita normalizar los datos.

Homocedasticidad multivariada (Se usa la prueba M de Box):'

boxM(anticuerpos[, -1], anticuerpos[,1])
#Como el valor p obtenido es mayor a 0.05, no se rechaza la hipótesis nula, entonces se puede asumir la homogeneidad de las matrices de varianzas y covarianzas para cada uno de los grupos

#b- Analice de dos maneras el poder discriminante de la regla.

'Como se rechazó la hipótesis de igualdad de vectores de medias, aunque no se satisface el supuesto de normalidad multivariada (se asume esta en el límite), se propone una análisis discriminante lineal pues se satisface la hipótesis de homocedasticidad.

La regla discriminante se construye con el 70% de los datos, el 30% restante se usará para validación en el literal siguiente.

Regla discriminante y coeficientes:'

set.seed(12345)

intrain <- createDataPartition(y=anticuerpos$Grupo,p=0.70,list = FALSE)
anticuerpos_train <- anticuerpos[intrain,]
anticuerpos_test <- anticuerpos[-intrain,]

fit.anticuerpos <- lda(Grupo ~.,data = anticuerpos_train)
fit.anticuerpos

'Call:
lda(Grupo ~ ., data = anticuerpos_train)

Prior probabilities of groups:
Grupo 1 Grupo 2 
0.5     0.5 

Group means:
Colesterol Albúmina    Calcio   Aurico
Grupo 1   237.2273 42.07182 100.39576 48.43364
Grupo 2   245.9697 40.42424  99.85606 46.66667

Coefficients of linear discriminants:
LD1
Colesterol  0.007812125
Albúmina   -0.267635247
Calcio      0.020840310
Aurico     -0.042774003'


#c- Analice de dos maneras el poder discriminante de la regla.

pred.anticuerpos_test <- predict(fit.anticuerpos,anticuerpos_test)

confusionMatrix(anticuerpos_test$Grupo,pred.anticuerpos_test$class)

pred.anticuerpos_train <- predict(fit.anticuerpos,anticuerpos_train)
confusionMatrix(anticuerpos_train$Grupo,pred.anticuerpos_train$class)


#c- A qué grupo asignaría un paciente con: Colesterol=240 ,Albumnia=39, Calcio=101 y
#aurico=49.

pred_c <- predict(fit.anticuerpos,data.frame("Colesterol"=240,"Albúmina"=39,"Calcio"=101,"Aurico"=49))
pred_c

#Grupo 2