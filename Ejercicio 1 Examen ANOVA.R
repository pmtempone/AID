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

anticuerpos <- read.csv("C:/Users/Pablo/Google Drive/Maestria/AID/AID parcial 2/anticuerpos.csv", sep=";")


