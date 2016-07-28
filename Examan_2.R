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

anticuerpos <- read.csv2("C:/Users/Pablo/Google Drive/Maestria/AID/Segundo Examen/anticuerpos.csv")
anticuerpos <- anticuerpos[,-1]
anticuerpos$Grupo <- factor(anticuerpos$Grupo)
#a- Establezca las hipótesis de interés. Realice el contraste correspondiente.

fit <- hotelling.test(.~Grupo,data = anticuerpos)
fit

'Este contraste rechaza la hipótesis de igualdad del vector de medias, para verificar que el análisis es correcto, se procede a verificar las hipótesis:

Normalidad multivariada (Se usa el test de Royston):'

roystonTest(anticuerpos[, -1])


'No se satisface el supuesto de normalidad multivariada, por lo tanto no es posible hacer un análisis discriminante a menos que se encuentre una transformación que permita normalizar los datos.

Homocedasticidad multivariada (Se usa la prueba M de Box):'

boxM(anticuerpos[, -1], anticuerpos[,1])
#Como el valor p obtenido es mayor a 0.05, no se rechaza la hipótesis nula, entonces se puede asumir la homogeneidad de las matrices de varianzas y covarianzas para cada uno de los grupos

set.seed(12345)

intrain <- createDataPartition(y=anticuerpos$Grupo,p=0.67,list = FALSE)
anticuerpos_train <- anticuerpos[intrain,]
anticuerpos_test <- anticuerpos[-intrain,]

fit.anticuerpos <- lda(Grupo ~.,data = anticuerpos_train)
fit.anticuerpos

'Call:
lda(Grupo ~ ., data = anticuerpos_train)

Prior probabilities of groups:
1   2 
0.5 0.5 

Group means:
Colesterol Albúmina    Calcio   Aurico
1   237.4444 42.17048 100.16063 48.74000
2   243.9841 40.17460  99.13492 47.57143

Coefficients of linear discriminants:
LD1
Colesterol  0.005915743
Albúmina   -0.271777595
Calcio     -0.019302855
Aurico     -0.021523867'


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
