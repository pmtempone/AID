library(readxl)   #Para leer los archivos de excel
library(psych)    #Para calcular las estadísticas por grupo
library(ggplot2)  #Para graficar
library(nortest)  #Para hacer las pruebas de hipótesis de normalidad
library(lawstat)  #Prueba de Levene
library(reshape)  #Manejo de datos
library(foreign)
library(dplyr)
library(reshape2)
library(MASS)
library(Rcmdr)

#Ejercicio 2

cervezas <- read.csv("C:/Users/Pablo/Google Drive/Maestria/AID/Clase 08/cervezas.csv", sep=";")

names(cervezas) = c("Sodio", "Marca", "Promedio_Marca", "Res.med")

#Ejercicio 3

dataset = read.spss("C:/Users/Pablo/Google Drive/Maestria/AID/Clase 08/suplementosdieta.sav", to.data.frame=TRUE)
dataset$Suplementos <- factor(dataset$Suplementos)
dieta <- dataset

ggplot(data = dieta, aes(x = Suplementos, y = EfCon, fill = Suplementos)) + geom_boxplot()
#Los suplementos S1, S2 y S4 parecen ser similares entre ellos, pero es posible que no haya igualdad de medias en este caso porque el suplemento 3 difiere mucho del resto.


#Cálculo de los promedios y las desviaciones estandar para cada uno de los tratamientos:
describeBy(dieta$EfCon, group = dieta$Suplementos, mat = T)[,c("group1", "mean", "sd")]
#Las desviaciones estandar presentadas verifican lo dicho anteriormente sobre los suplementos S1, S2 y S4. Se sigue notando que el suplemento S4 es diferente de los demás.

'b) Establezca las hipótesis de interés del problema y explicite los supuestos necesarios.

Hipótesis nula:
  
  Los promedios de eficiencia de conversión para los suplementos son iguales.

H0:μ1=μ2=⋯=μn=μ

Hipótesis alternativa:
  
  Existe al menos un promedio de eficiencia de conversión que es diferente de los demás.

Ha:∃μi≠μ
, i=1,…,n'

#c) Testee las hipótesis al 5%.

dieta.AOV = aov(data = dieta, EfCon ~ Suplementos)
dieta.AOV
summary(dieta.AOV)
'El análisis de varianza rechaza la hipótesis de igualdad en el promedio de eficiencia de conversión para cada uno de los suplementos, falta verificar que se cumplen los supuestos para poder concluir los resultados.'

#d) Analice el cumplimiento de los supuestos del modelo.

dieta = cbind(dieta, residuals(dieta.AOV))
colnames(dieta)[3] = "Anova.Residuales"

Anderson <- ad.test(dieta$Anova.Residuales)
Kolmogorov <- lillie.test(dieta$Anova.Residuales)
ShapiroWilk <- shapiro.test(dieta$Anova.Residuales)
Cramer <- cvm.test(dieta$Anova.Residuales)

rbind(cbind(Anderson$method,'p valor' = Anderson$p.value),
      cbind(Kolmogorov$method,'p valor' = Kolmogorov$p.value),
      cbind(ShapiroWilk$method,'p valor' = ShapiroWilk$p.value),
      cbind(Cramer$method,'p valor' = Cramer$p.value))

#Ninguna de las pruebas de normalidad rechaza la hipótesis nula, es decir, se puede asumir que los residuales del modelo tienen distribución normal.

qqnorm(dieta$Anova.Residuales)
qqline(dieta$Anova.Residuales)

#Para verificar la hipótesis de homocedasticidad de los datos, se usan los siguientes test:

Levene <- levene.test(dieta$Anova.Residuales, dieta$Suplementos)
Bartlett <- bartlett.test(dieta$Anova.Residuales, dieta$Suplementos)

rbind(cbind("Levene",'p valor' = Levene$p.value),
      cbind("Bartlett",'p valor' = Bartlett$p.value))
#Las pruebas de homocedasticidad tampoco rechazan la hipótesis nula en este caso, por lo tanto se puede asumir que las varianzas de los residuales son iguales para cada uno de los suplementos.

#e) Concluya en términos del problema y si rechazó H0
#, indique cuales medias son diferentes. Utilice para ello las comparaciones a posteriori de Tuckey.

#Como las hipótesis de la anova se satisfacen, se concluye que existen diferencias en las medias de los datos presentados.

TukeyHSD(dieta.AOV)
#Se conluye que hay diferencias entre los promedios de eficiencia de conversión en los suplementos. La prueba de Tukey nos permite concluir que las diferencias están dadas por el grano partido y el grano entero, pues los suplementos S1 y S2 que están constituidos por grano partido no presentan diferencias entre ellos al igual que los suplementos S3 y S4 que están constituidos por grano entero.

#Ejercicio 4

drogadolor = read.spss("C:/Users/Pablo/Google Drive/Maestria/AID/Clase 08/drogadolor.sav", to.data.frame=TRUE)
drogadolor$Trat_num <- factor(drogadolor$Trat_num)


#Cálculo de los promedios y las desviaciones estandar para cada uno de los tratamientos:
describeBy(drogadolor$Dolor, group = drogadolor$Tratamiento, mat = T)[,c("group1", "mean", "sd")]


#Ejercicio 5

coccion <- read.csv("C:/Users/Pablo/Google Drive/Maestria/AID/Clase 08/coccion.csv")

Grupo_A <- coccion$tiempo[coccion$Grupo=='a']
Grupo_B <- coccion$tiempo[coccion$Grupo=='b']
Grupo_C <- coccion$tiempo[coccion$Grupo=='c']
Grupo_D <- coccion$tiempo[coccion$Grupo=='d']

cbind(Grupo_A,Grupo_B,Grupo_C,Grupo_D)

#Grafique los tiempos de cocción por tratamiento. Calcule las medidas resumen de los mismos.
ggplot(data = coccion, aes(x = Grupo, y = tiempo, fill = Grupo)) + geom_boxplot()
describeBy(coccion$tiempo, group = coccion$Grupo, mat = T)[,c("group1", "mean", "sd")]

'b) Establezca las hipótesis de interés, escriba el modelo detallando los supuestos.

Hipótesis nula:

Los promedios de los tiempos de cocción para cada uno de los grupos son iguales:

H0:μ1=μ2=⋯=μn=μ

Hipótesis alternativa:

Existe al menos un promedio de tiempo de cocción que es diferente de los demás:

Ha:∃μi≠μ
, i=1,…,n

Sean:

Yij:

Representa la j-ésima observación en el tratamiento i

μ:

Intercepto del modelo

τi:
Tratamiento i

ϵij:
Residual del individuo j en el tratamiento i

El modelo general está dado por Yij=μ+τi+ϵij

En este ejercicio, Yij
es el tiempo de cocción de cada alimento en cada grupo, y τi

representa cada uno de los grupos.

Además, la distribución los residuales ϵij
es normal y con varianza igual para todos los grupos (Homocedástico).'

#punto c

coccion.AOV = aov(data = coccion, tiempo ~ Grupo)
summary(coccion.AOV)

coccion = cbind(coccion, residuals(coccion.AOV))
colnames(coccion)[3] = "Anova.Residuales"

Anderson <- ad.test(coccion$Anova.Residuales)
Kolmogorov <- lillie.test(coccion$Anova.Residuales)
ShapiroWilk <- shapiro.test(coccion$Anova.Residuales)
Cramer <- cvm.test(coccion$Anova.Residuales)

rbind(cbind(Anderson$method,'p valor' = Anderson$p.value),
      cbind(Kolmogorov$method,'p valor' = Kolmogorov$p.value),
      cbind(ShapiroWilk$method,'p valor' = ShapiroWilk$p.value),
      cbind(Cramer$method,'p valor' = Cramer$p.value))

#Para verificar la hipótesis de homocedasticidad de los datos, se usan los siguientes test:

Levene <- levene.test(coccion$Anova.Residuales, coccion$Grupo)
Bartlett <- bartlett.test(coccion$Anova.Residuales, coccion$Grupo)

rbind(cbind("Levene",'p valor' = Levene$p.value),
      cbind("Bartlett",'p valor' = Bartlett$p.value))
#En este caso se rechaza la hipótesis de igualdad de medias, es decir, los promedios de los tiempos de cocción en los grupos no son iguales, sin embargo, no se satisfacen las hipótesis del análisis de varianza, por lo tanto el test no es conclusivo.
#Pero nada se puede concluir pues no se cumplen los supuestos del test ANOVA: ni la normalidad ni la homocedasticidad.

#ejercicio d. Hacer transformacion porque se concluyo negativamente.
resumen.4 <- describeBy(coccion$tiempo, group = coccion$Grupo, mat = T)[,c("group1", "mean", "sd")]
ggplot(resumen.4, aes(x=mean, y=sd)) +geom_point()+geom_line()

Levene

ShapiroWilk
#grafico boxcox para ver donde transformar
boxcox(coccion$tiempo~1,plotit=T)

coccion$tiempotransf <- coccion$tiempo**0.5 #con 0.25 como en el resuelt no me da pvalor menor
#ejercicio e (realizar de nuevo el diagnostico)
coccion.AOV = aov(data = coccion, tiempotransf ~ Grupo)
summary(coccion.AOV)
coccion = cbind(coccion, residuals(coccion.AOV))
colnames(coccion)[5] = "Anova.Residualestransf"

Levene <- leveneTest(coccion$Anova.Residualestransf, coccion$Grupo)
Levene #Se rechaza la homocedasticidad

Levene <- leveneTest(coccion$Anova.Residualestransf, coccion$Grupo)
Bartlett <- bartlett.test(coccion$Anova.Residualestransf, coccion$Grupo)

rbind(cbind("Levene",'p valor' = Levene$`Pr(>F)`),
      cbind("Bartlett",'p valor' = Bartlett$p.value))

#Normalidad
Anderson <- ad.test(coccion$Anova.Residualestransf)
Kolmogorov <- lillie.test(coccion$Anova.Residualestransf)
ShapiroWilk <- shapiro.test(coccion$Anova.Residualestransf)
Cramer <- cvm.test(coccion$Anova.Residualestransf)

rbind(cbind(Anderson$method,'p valor' = as.numeric(Anderson$p.value)),
      cbind(Kolmogorov$method,'p valor' = as.numeric(Kolmogorov$p.value)),
      cbind(ShapiroWilk$method,'p valor' = as.numeric(ShapiroWilk$p.value)),
      cbind(Cramer$method,'p valor' = as.numeric(Cramer$p.value)))

#Se rechaza la normalidad
#En todos los casos se rechazan las hipótesis normalidad y homocedasticidad en los residuales del modelo, lo cual invalida el test presentado anteriormente. Se propone hacer una prueba no paramétrica de igualdad de medias.

#punto f (comparar con test no parametrico - kruskal wallis)

Kruskal <- kruskal.test(coccion$tiempo ~ coccion$Grupo)
Kruskal


'El valor p obtenido es menor al valor de alfa establecido, por lo tanto, se rechaza la hipótesis de igualdad de medias.

En los análisis de varianzas realizados, se rechazó la hipótesis nula de igualdad para los datos originales y para los datos transformados, sin embargo, los test nunca fueron concluyentes porque en ninguno de los casos se cumplieron los supuestos de normalidad y homocedasticidad. El test no paramétrico logra mostrar que existen diferencias en las medias de los grupos.'


#Ejercicio 6