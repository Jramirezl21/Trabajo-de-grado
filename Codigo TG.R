#Librerias ----
library(Gifi)
library(whomds)
library(EFAtools)
library(psych)
library(haven)
library(polycor)
library(ggcorrplot)
library(GPArotation)
library(psych)
library(lavaan)
library(polycor)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(ade4)
library(FactoMineR)
library(missMDA)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(xtable)
Datos <- read.csv("datos.csv")
Data <- read.csv("DatosCat.csv")
xtable(round(table(df_filtrado$region)/4754,3))
xtable(round(table(df_filtrado$region)))
df_filtrado <- Data[!complete.cases(Data), ]

Datos.num <- read.csv("datos.csv")
DatosCap <- Datos[,c(5:26)]
#################################

##############################
# Crear una función para calcular la cantidad de varianza explicada por el PCA
calcular_pca <- function(data) {
  pca <-  PCA(data, scale.unit = TRUE,graph = F)
  
  # Extraer la cantidad de varianza explicada por cada componente principal
  var_exp <-  pca$eig[,"percentage of variance"]
  cargas <-  pca$var$coord

  vl_p<- pca$eig[1] #Valor Propio
  vt_p<-cargas/sqrt(vl_p) #Vector Propio
  Pesos<-(vt_p/sum(vt_p)) # Pesos relativos
  resultpca <- list(var_exp=var_exp,Pesos=Pesos)
  return(resultpca)
}

fviz_pca_var(PCA( Datos[,c(5:26)]))
fviz_pca_var(PCA( Datos[,c(29:73)]))
# Número de submuestras
num_submuestras <- 1000

# Tamaño de cada submuestra
tam_submuestra <- 3000

muestras1Cap<- vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  muestras1Cap[[i]] <- sample(rownames(DatosCap), tam_submuestra, replace =F)
}

varExp_Cap1 <- vector("list", num_submuestras)
Pesos_Cap1<-  vector("list", num_submuestras)


for (i in 1:num_submuestras) {
  
  submuestra <- DatosCap[muestras1Cap[[i]], ]
  
  
  varExp_Cap1[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_Cap1[[i]] <- calcular_pca(submuestra)$Pesos

}


tam_submuestra2 <- 1750

muestras2Cap<- vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  muestras2Cap[[i]] <- sample(rownames(DatosCap), tam_submuestra2, replace = F)
}

varExp_Cap2 <- vector("list", num_submuestras)
Pesos_Cap2<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DatosCap[muestras2Cap[[i]], ]
  
  
  varExp_Cap2[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_Cap2[[i]] <- calcular_pca(submuestra)$Pesos
  
}

indice_maximo <- which.max(sapply(varExp_Cap1, `[`, 1))
varExp_Cap1[[indice_maximo ]]
indice_maximo2 <- which.max(sapply(varExp_Cap2, `[`, 1))
varExp_Cap2[[indice_maximo2]]
### Tercera y cuarta submuestra
rm(tamano_muestra3)
# Tamaño de la muestra y número de submuestras
tam_submuestra3 <- 2377

muestras3Cap<- vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  muestras3Cap[[i]] <- sample(rownames(DatosCap), tam_submuestra3, replace = F)
}

varExp_Cap3 <- vector("list", num_submuestras)
Pesos_Cap3<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DatosCap[muestras3Cap[[i]], ]
  
  
  varExp_Cap3[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_Cap3[[i]] <- calcular_pca(submuestra)$Pesos
  
}

indice_maximo3 <- which.max(sapply(varExp_Cap3, `[`, 1))
varExp_Cap3[[indice_maximo3]]
individuos_seleccionados <- as.numeric(muestras3Cap[[indice_maximo3]])

individuos_faltantes <- setdiff(1:4754, individuos_seleccionados)
nueva_muestra <- DatosCap[individuos_faltantes, ]
acp4 <- calcular_pca(nueva_muestra)
varExp_Cap4 <- acp4$var_exp
Pesos_Cap4 <- acp4$Pesos
##### Comparacion varianzas explicadas
varExp_Cap1[[indice_maximo]]
varExp_Cap2[[indice_maximo2]]
varExp_Cap3[[indice_maximo3]]
varExp_Cap4
#Seleccionar el de mayor varianza explicada, la muestra 2
Pesos1 <- Pesos_Cap1[[indice_maximo]][,1]
Pesos2 <- Pesos_Cap2[[indice_maximo2]][,1]
Pesos3 <- Pesos_Cap2[[indice_maximo3]][,1]
Pesos4 <- acp4$Pesos[,1]

puntuaciones1 <- rowSums(Pesos1* DatosCap)
puntuaciones2<- rowSums(Pesos2* DatosCap)
puntuaciones3 <- rowSums(Pesos3* DatosCap)
puntuaciones4 <- rowSums(Pesos4* DatosCap)

#Se selecciona el intervalo mas grande en puntuaciones
vector1 <- c(min(puntuaciones1),max(puntuaciones1));max(puntuaciones1)-min(puntuaciones1)
vector2 <- c(min(puntuaciones2),max(puntuaciones2));max(puntuaciones2)-min(puntuaciones2)
vector3 <- c(min(puntuaciones3),max(puntuaciones3));max(puntuaciones3)-min(puntuaciones3)
vector4 <- c(min(puntuaciones4),max(puntuaciones4));max(puntuaciones4)-min(puntuaciones4)
#Se selecciona el indicador o puntuaciones 2
DatosCap$PuntajeCap <- puntuaciones4
DatosCap$PunCapEst <- PunCapEst
Datos$PunCapEst <- PunCapEst
Datos$PuntajeCap <- puntuaciones4

PunCapEst <- round((puntuaciones4  - min(puntuaciones4 ))*100/(max(puntuaciones4 )-min(puntuaciones4 )),2)
set.seed(1234)


kmeansPunCap <- kmeans(PunCapEst , 4, iter.max = 1000, nstart = 10)
fviz_cluster(kmeansPunCap,DatosCap)
Datos$clustercap <- kmeansPunCap$cluster
kmeansPunCap$centers; tapply(PunCapEst, kmeansPunCap$cluster, summary)
baremos <- as.data.frame(tapply(PunCapEst, kmeansPunCap$cluster, function(x){c(max(x),min(x))}))
colnames(baremos) <- c("Baremos")

fviz_cluster(kmeansPun2 ,data=DatosCap)
hist(puntuaciones4)
#Transformacion del puntaje
#################################
#Puntaje heuristico

Puntaje_heuristico <- rowSums(DatosCap[,c(1:22)])
hist(Puntaje_heuristico )
cor_spearman <- cor(PunCapEst,Puntaje_heuristico , method = "spearman")
plot(puntuaciones2~Puntaje_euristico)


kmeansHeuris <- kmeans(Puntaje_heuristico,4, iter.max = 1000, nstart = 1)
fviz_cluster(kmeansHeuris,data=DatosCap[,c(1:22)])
clustersHeuris<- data.frame(cbind(kmeansHeuris$cluster,Puntaje_euristico))


kmeansHeuris$centers; tapply( Puntaje_heuristico,
                              kmeansHeuris$cluster, summary)
Datos$PunCapHeuris <- Puntaje_heuristico
Datos$clustercapH <- kmeansHeuris$cluster
