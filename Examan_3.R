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

ratas <- read.csv2("C:/Users/Pablo/Google Drive/Maestria/AID/Segundo Examen/ratas.csv")

#a- Plantee los supuestos del modelo para comparar los valores medios de glóbulos rojos
#de las ratas de los cuatro grupos.


'Hipótesis nula:

Los promedios de conteos de globulos rojos en los grupos de ratas son iguales.

H0:μ1=μ2=⋯=μn=μ

Hipótesis alternativa:

Existe al menos un promedio de conteos de globulos rojos en los grupos de ratas que es diferente de los demás.

Ha:∃μi≠μ
, i=1,…,n

Análisis discriminante:'

describeBy(ratas$globulos, group = ratas$tratamiento, mat = T)[,c("group1", "mean", "sd")]
ratas$tratamiento <- factor(ratas$tratamiento)

ggplot(data = ratas, aes(x = tratamiento, y = globulos, fill = tratamiento)) + geom_boxplot()



ratas.AOV = aov(data = ratas, globulos ~ tratamiento)
ratas.AOV
summary(ratas.AOV)
#se rechaza supuesto de igualdad de medias

#b- Realice la prueba y el análisis diagnostico(supuestos)
ratas = cbind(ratas, residuals(ratas.AOV))
colnames(ratas)[3] = "Anova.Residuales"

#se prueba normalidad

Anderson <- ad.test(ratas$Anova.Residuales)
Kolmogorov <- lillie.test(ratas$Anova.Residuales)
ShapiroWilk <- shapiro.test(ratas$Anova.Residuales)
Cramer <- cvm.test(ratas$Anova.Residuales)

rbind(cbind(Anderson$method,'p valor' = Anderson$p.value),
      cbind(Kolmogorov$method,'p valor' = Kolmogorov$p.value),
      cbind(ShapiroWilk$method,'p valor' = ShapiroWilk$p.value),
      cbind(Cramer$method,'p valor' = Cramer$p.value))

#Ninguna de las pruebas de normalidad rechaza la hipótesis nula, es decir, se puede asumir que los residuales del modelo tienen distribución normal.

#Pruebas de homocedasticidad:

Levene <- leveneTest(ratas$Anova.Residuales, ratas$tratamiento)
Bartlett <- bartlett.test(ratas$Anova.Residuales, ratas$tratamiento)

rbind(cbind("Levene",'p valor' = Levene$`Pr(>F)`),
      cbind("Bartlett",'p valor' = Bartlett$p.value))

#Se satisface la hipótesis de homocedasticidad entre los grupos.

#c- Si es válida, concluya, si no lo es, utilice otra prueba y concluya.

'Se puede afirmar que hay diferencias significativas entre los grupos dado que se satisfacen los supuestos.'

#d- Explique en qué casos realizaría transformaciones de las variables.

#Se hacen transformaciones de las variables cuando no se satisfacen los supuestos de normalidad y/o homocedasticidad. Para ello, se suele hacer un gráfico de medias vs desviaciones estandar para determinar el tipo de transformación a usar (Caso de homocedasticidad).
boxcox(ratas$globulos~1,plotit=T) #como se haria
