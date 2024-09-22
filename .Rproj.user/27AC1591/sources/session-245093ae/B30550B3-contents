
##Sub muestras de cada nivel de capacidad
##remuestreo por acp
lista_clasifACP <- split(Datos, Datos$clustercap)
DesACPLEVE <- (lista_clasifACP$`4`)[,c(29:73)]
DesACPMODERADA <- (lista_clasifACP$`2`)[,c(29:73)]
DesACPSEVERA <- (lista_clasifACP$`3`)[,c(29:73)]
DesACPNINGUNA <- (lista_clasifACP$`1`)[,c(29:73)]

#################################################################
# DESEMPEÑO LEVE ####
## PRIMERA MUESTRA----


muestras1leveD <- vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  muestras1leveD[[i]] <- sample(rownames(DesACPLEVE), tam_submuestra, replace = TRUE)
}

varExp_LeveD <- vector("list", num_submuestras)
Pesos_LeveD<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DesACPLEVE[muestras1leveD[[i]],]
  
  
  varExp_LeveD[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_LeveD[[i]] <- calcular_pca(submuestra)$Pesos
  
}
##SEGUNDA MUESTRA ----
muestras2leveD <- vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  muestras2leveD[[i]] <- sample(rownames(DesACPLEVE), tam_submuestra2, replace = TRUE)
}

varExp_LeveD2 <- vector("list", num_submuestras)
Pesos_LeveD2<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DesACPLEVE[muestras2leveD[[i]],]
  
  
  varExp_LeveD2[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_LeveD2[[i]] <- calcular_pca(submuestra)$Pesos
  
}

#
##TERCERA Y CUARTA MUESTRA ####



muestras3leveD <- vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  muestras3leveD[[i]] <- sample(rownames(DesACPLEVE), tam_submuestra3, replace = TRUE)
}

varExp_LeveD3 <- vector("list", num_submuestras)
Pesos_LeveD3<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DesACPLEVE[muestras3leveD[[i]],]
  
  
  varExp_LeveD3[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_LeveD3[[i]] <- calcular_pca(submuestra)$Pesos
  
}


indice_max_varDL.3 <- which.max(sapply(varExp_LeveD3 , `[`, 1))


ind_selecDL.4 <- as.numeric(muestras3leveD[[indice_max_varDL.3]])
ind_faltDL.4 <- setdiff(rownames(DesACPLEVE), ind_selecDL.4)
nueva_muestraDL.4 <- DesACPLEVE[ind_faltDL.4, ]
varExp_LeveD4 <- calcular_pca(nueva_muestraDL.4)$var_exp
Pesos_LeveD4 <- calcular_pca(nueva_muestraDL.4)$Pesos
############
indice_max_varDL.1 <- which.max(sapply(varExp_LeveD, `[`, 1))
varExp_LeveD[[indice_max_varDL.1]]

indice_max_varDL.2 <- which.max(sapply(varExp_LeveD2, `[`, 1))
varExp_LeveD2[[indice_max_varDL.2]]

indice_max_varDL.3 <- which.max(sapply(varExp_LeveD3, `[`, 1))
varExp_LeveD3[[indice_max_varDL.3]]

varExp_LeveD4
#-------------------------------------------#
#DESEMPEÑO MODERADO ####



## PRIMERA MUESTRA


muestras1ModerD <- vector("list", num_submuestras)


for (i in 1:num_submuestras) {
  muestras1ModerD[[i]] <- sample(rownames(DesACPMODERADA), tam_submuestra, replace = TRUE)
}

varExp_ModerD <- vector("list", num_submuestras)
Pesos_ModerD<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DesACPMODERADA[muestras1ModerD[[i]],]
  
  
  varExp_ModerD[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_ModerD[[i]] <- calcular_pca(submuestra)$Pesos
  
}

## SEGUNDA MUESTRA ----

muestras2ModerD <- vector("list", num_submuestras)


for (i in 1:num_submuestras) {
  muestras2ModerD[[i]] <- sample(rownames(DesACPMODERADA), tam_submuestra2, replace = TRUE)
}


varExp_ModerD2<- vector("list", num_submuestras)
Pesos_ModerD2<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DesACPMODERADA[muestras2ModerD[[i]],]
  
  
  varExp_ModerD2[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_ModerD2[[i]] <- calcular_pca(submuestra)$Pesos
  
}

## TERCERA Y CUARTA MUESTRA ----

muestras3ModerD <- vector("list", num_submuestras)


for (i in 1:num_submuestras) {
  muestras3ModerD[[i]] <- sample(rownames(DesACPMODERADA), tam_submuestra3, replace = TRUE)
}


varExp_ModerD3<- vector("list", num_submuestras)
Pesos_ModerD3<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DesACPMODERADA[muestras3ModerD[[i]],]
  
  
  varExp_ModerD3[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_ModerD3[[i]] <- calcular_pca(submuestra)$Pesos
}


indice_max_varDM.3 <- which.max(sapply(varExp_ModerD3 , `[`, 1))

ind_selecDM.4 <- as.character(muestras3ModerD[[indice_max_varDL.3]])
ind_faltDM.4 <- setdiff(rownames(DesACPMODERADA), ind_selecDM.4)
nueva_muestraDM.4 <- DesACPMODERADA[ind_faltDM.4, ]
varExp_ModerD4 <- calcular_pca(nueva_muestraDM.4)$var_exp
Pesos_ModerD4 <- calcular_pca(nueva_muestraDM.4)$Pesos


#DESEMPEÑO SERVERO ----
## PRIMERA MUESTRA ----

muestras1SevD <- vector("list", num_submuestras)


for (i in 1:num_submuestras) {
  muestras1SevD[[i]] <- sample(rownames(DesACPSEVERA), tam_submuestra, replace = TRUE)
}



varExp_SevD<- vector("list", num_submuestras)
Pesos_SevD<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DesACPSEVERA[muestras1SevD[[i]],]
  
  
  varExp_SevD[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_SevD[[i]] <- calcular_pca(submuestra)$Pesos
}

##SEGUNDA MUESTRA ----

muestras2SevD <- vector("list", num_submuestras)


for (i in 1:num_submuestras) {
  muestras2SevD[[i]] <- sample(rownames(DesACPSEVERA), tam_submuestra2, replace = TRUE)
}



varExp_SevD2<- vector("list", num_submuestras)
Pesos_SevD2<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DesACPSEVERA[muestras2SevD[[i]],]
  
  
  varExp_SevD2[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_SevD2[[i]] <- calcular_pca(submuestra)$Pesos
}

##TERCERA Y CUARTA MUESTRA ----
muestras3SevD <- vector("list", num_submuestras)


for (i in 1:num_submuestras) {
  muestras3SevD[[i]] <- sample(rownames(DesACPSEVERA), tam_submuestra3, replace = TRUE)
}



varExp_SevD3<- vector("list", num_submuestras)
Pesos_SevD3<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DesACPSEVERA[muestras3SevD[[i]],]
  
  
  varExp_SevD3[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_SevD3[[i]] <- calcular_pca(submuestra)$Pesos
}

indice_max_varDS.3 <- which.max(sapply(varExp_SevD3 , `[`, 1))

ind_selecDS.4 <- as.numeric(muestras3SevD[[indice_max_varDS.3]])
ind_faltDS.4 <- setdiff(as.numeric(rownames(DesACPSEVERA)), ind_selecDS.4)
nueva_muestraDS.4 <- DesACPSEVERA[ind_faltDS.4, ]
varExp_SevD4 <- calcular_pca(nueva_muestraDS.4)$var_exp
Pesos_SevD4 <- calcular_pca(nueva_muestraDS.4)$Pesos

# NINGUNA DIF DES ----

## PRIMERA MUESTRA ----

muestras1NinD <- vector("list", num_submuestras)


for (i in 1:num_submuestras) {
  muestras1NinD [[i]] <- sample(rownames(DesACPNINGUNA), tam_submuestra, replace = TRUE)
}



varExp_NinD<- vector("list", num_submuestras)
Pesos_NinD<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DesACPNINGUNA[muestras1NinD[[i]],]
  
  
  varExp_NinD[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_NinD[[i]] <- calcular_pca(submuestra)$Pesos
}

##SEGUNDA MUESTRA ----

muestras2NinD <- vector("list", num_submuestras)


for (i in 1:num_submuestras) {
  muestras2NinD [[i]] <- sample(rownames(DesACPNINGUNA), tam_submuestra2, replace = TRUE)
}



varExp_NinD2<- vector("list", num_submuestras)
Pesos_NinD2<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DesACPNINGUNA[muestras2NinD[[i]],]
  
  
  varExp_NinD2[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_NinD2[[i]] <- calcular_pca(submuestra)$Pesos
}

##TERCERA Y CUARTA MUESTRA ----

muestras3NinD <- vector("list", num_submuestras)


for (i in 1:num_submuestras) {
  muestras3NinD [[i]] <- sample(rownames(DesACPNINGUNA), tam_submuestra3, replace = TRUE)
}



varExp_NinD3<- vector("list", num_submuestras)
Pesos_NinD3<-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  
  submuestra <- DesACPNINGUNA[muestras3NinD[[i]],]
  
  
  varExp_NinD3[[i]] <- calcular_pca(submuestra)$var_exp
  Pesos_NinD3[[i]] <- calcular_pca(submuestra)$Pesos
}



indice_max_varDN.3 <- which.max(sapply(varExp_NinD3 , `[`, 1))

ind_selecDN.4 <- as.numeric(muestras3NinD[[indice_max_varDN.3]])
ind_faltDN.4 <- setdiff(as.numeric(rownames(DesACPNINGUNA)), ind_selecDN.4)
nueva_muestraDN.4 <- DesACPNINGUNA[ind_faltDN.4, ]
varExp_NinD4 <- calcular_pca(nueva_muestraDN.4)$var_exp
Pesos_NinD4 <- calcular_pca(nueva_muestraDN.4)$Pesos

#SELECCION MEDIDAS----
## MEDIDAS DES LEVE----
indice_max_varDL.1 <- which.max(sapply(varExp_LeveD, `[`, 1))
varExp_LeveD[[indice_max_varDL.1]][1:2]
indice_max_varDL.2 <- which.max(sapply(varExp_LeveD2, `[`, 1))
varExp_LeveD2[[indice_max_varDL.2]][1:2]#
indice_max_varDL.3 <- which.max(sapply(varExp_LeveD3, `[`, 1))
varExp_LeveD3[[indice_max_varDL.3]][1:2]
varExp_LeveD4[1:2]

PesosL1 <- Pesos_LeveD[[indice_max_varDL.1]][,1]
PesosL2<- Pesos_LeveD2[[indice_max_varDL.2]][,1]
PesosL3 <- Pesos_LeveD3[[indice_max_varDL.3]][,1]
PesosL4<- Pesos_LeveD4[,1]

rowSums(PesosL1*DesACPLEVE)
max(rowSums(PesosL1*DesACPLEVE));min(rowSums(PesosL1*DesACPLEVE))
(max(rowSums(PesosL1*DesACPLEVE))-min(rowSums(PesosL1*DesACPLEVE)))
rowSums(PesosL2*DesACPLEVE)#
max(rowSums(PesosL2*DesACPLEVE));min(rowSums(PesosL2*DesACPLEVE))
(max(rowSums(PesosL2*DesACPLEVE))-min(rowSums(PesosL2*DesACPLEVE)))
rowSums(PesosL3*DesACPLEVE)
max(rowSums(PesosL3*DesACPLEVE));min(rowSums(PesosL3*DesACPLEVE))
(max(rowSums(PesosL3*DesACPLEVE))-min(rowSums(PesosL3*DesACPLEVE)))
rowSums(PesosL4*DesACPLEVE)
max(rowSums(PesosL4*DesACPLEVE));min(rowSums(PesosL4*DesACPLEVE))
(max(rowSums(PesosL4*DesACPLEVE))-min(rowSums(PesosL4*DesACPLEVE)))


## MEDIDAS DES MODERADO----
indice_max_varDM.1 <- which.max(sapply(varExp_ModerD, `[`, 1))
varExp_ModerD[[indice_max_varDM.1]][1:2]
indice_max_varDM.2 <- which.max(sapply(varExp_ModerD2, `[`, 1))
varExp_ModerD2[[indice_max_varDM.2]][1:2]
indice_max_varDM.3 <- which.max(sapply(varExp_ModerD3, `[`, 1))
varExp_ModerD3[[indice_max_varDM.3]][1:2]
varExp_ModerD4

PesosM1 <- Pesos_ModerD[[indice_max_varDM.1]][,1]
PesosM2<- Pesos_ModerD2[[indice_max_varDM.2]][,1]
PesosM3 <- Pesos_ModerD3[[indice_max_varDM.3]][,1]


rowSums(PesosM1*DesACPMODERADA)
max(rowSums(PesosM1*DesACPMODERADA));min(rowSums(PesosM1*DesACPMODERADA))
(max(rowSums(PesosM1*DesACPMODERADA))-min(rowSums(PesosM1*DesACPMODERADA)))
rowSums(PesosM2*DesACPMODERADA)
max(rowSums(PesosM2*DesACPMODERADA));min(rowSums(PesosM2*DesACPMODERADA))
(max(rowSums(PesosM2*DesACPMODERADA))-min(rowSums(PesosM2*DesACPMODERADA)))
rowSums(PesosM3*DesACPMODERADA)#
max(rowSums(PesosM3*DesACPMODERADA));min(rowSums(PesosM3*DesACPMODERADA))
(max(rowSums(PesosM3*DesACPMODERADA))-min(rowSums(PesosM3*DesACPMODERADA)))


## MEDIDAS DES SEVERO----
varExp_SevD
indice_max_varDS.1 <- which.max(sapply(varExp_SevD, `[`, 1))
varExp_SevD[[indice_max_varDS.1]][1:2]
indice_max_varDS.2 <- which.max(sapply(varExp_SevD2, `[`, 1))
varExp_SevD2[[indice_max_varDM.2]][1:2]
indice_max_varDS.3 <- which.max(sapply(varExp_SevD3, `[`, 1))
varExp_SevD3[[indice_max_varDM.3]][1:2]


PesosS1 <- Pesos_SevD[[indice_max_varDS.1]][,1]
PesosS2<- Pesos_SevD2[[indice_max_varDS.2]][,1]
PesosS3 <- Pesos_SevD3[[indice_max_varDS.3]][,1]


rowSums(PesosS1*DesACPSEVERA)
max(rowSums(PesosS1*DesACPSEVERA));min(rowSums(PesosS1*DesACPSEVERA))
(max(rowSums(PesosS1*DesACPSEVERA))-min(rowSums(PesosS1*DesACPSEVERA)))
rowSums(PesosS2*DesACPSEVERA)
max(rowSums(PesosS2*DesACPSEVERA));min(rowSums(PesosS2*DesACPSEVERA))
(max(rowSums(PesosS2*DesACPSEVERA))-min(rowSums(PesosS2*DesACPSEVERA)))
rowSums(PesosS3*DesACPSEVERA)#
max(rowSums(PesosS3*DesACPSEVERA));min(rowSums(PesosS3*DesACPSEVERA))
(max(rowSums(PesosS3*DesACPSEVERA))-min(rowSums(PesosS3*DesACPSEVERA)))
## MEDIDA NINGUNA DIF
indice_max_varDN.1 <- which.max(sapply(varExp_NinD, `[`, 1))
varExp_SevD[[indice_max_varDN.1]][1:2]
indice_max_varDN.2 <- which.max(sapply(varExp_NinD2, `[`, 1))
varExp_SevD2[[indice_max_varDN.2]][1:2]
indice_max_varDN.3 <- which.max(sapply(varExp_NinD3, `[`, 1))
varExp_SevD3[[indice_max_varDN.3]][1:2]


PesosN1 <- Pesos_NinD[[indice_max_varDN.1]][,1]
PesosN2<- Pesos_NinD2[[indice_max_varDN.2]][,1]
PesosN3 <- Pesos_NinD3[[indice_max_varDN.3]][,1]


rowSums(PesosN1*DesACPNINGUNA)#
max(rowSums(PesosN1*DesACPNINGUNA));min(rowSums(PesosN1*DesACPNINGUNA))
(max(rowSums(PesosN1*DesACPNINGUNA))-min(rowSums(PesosN1*DesACPNINGUNA)))
rowSums(PesosN2*DesACPNINGUNA)
max(rowSums(PesosN2*DesACPNINGUNA));min(rowSums(PesosN2*DesACPNINGUNA))
(max(rowSums(PesosN2*DesACPNINGUNA))-min(rowSums(PesosN2*DesACPNINGUNA)))
rowSums(PesosN2*DesACPNINGUNA)
max(rowSums(PesosN2*DesACPNINGUNA));min(rowSums(PesosN2*DesACPNINGUNA))
(max(rowSums(PesosN2*DesACPNINGUNA))-min(rowSums(PesosN2*DesACPNINGUNA)))
####### Puntajes seleccionados en cada caso
PuntajeDL <- data.frame(rowSums(PesosL2*DesACPLEVE))
colnames(PuntajeDL)=c("PuntajeD.ACP")
mean(PuntajeDL)
PuntajeDM <- data.frame(rowSums(PesosM3*DesACPMODERADA))
colnames(PuntajeDM)=c("PuntajeD.ACP")
mean(PuntajeDM)
PuntajeDS <- data.frame(rowSums(PesosS3*DesACPSEVERA))
colnames(PuntajeDS)=c("PuntajeD.ACP")
mean(PuntajeDS)
PuntajeDN <-data.frame(rowSums(PesosN1*DesACPNINGUNA))
colnames(PuntajeDN)=c("PuntajeD.ACP")

####### Puntajes Heuristicos
Puntaje_heuristicoDL <- data.frame(rowSums(DesACPLEVE))
colnames(Puntaje_heuristicoDL)=c("PuntajeDH")
mean(Puntaje_heuristicoDL)
Puntaje_heuristicoDM <- data.frame( rowSums(DesACPMODERADA))
colnames(Puntaje_heuristicoDM)=c("PuntajeDH")
mean(Puntaje_heuristicoDM)
Puntaje_heuristicoDS <- data.frame(rowSums(DesACPSEVERA))
colnames(Puntaje_heuristicoDS)=c("PuntajeDH")
mean(Puntaje_heuristicoDS)
Puntaje_heuristicoDN <- data.frame(rowSums(DesACPNINGUNA))
colnames(Puntaje_heuristicoDN)=c("PuntajeDH")
mean(Puntaje_heuristicoDN)

####
PuntajesHunidos <- rbind(Puntaje_heuristicoDL,
                         Puntaje_heuristicoDM,
                         Puntaje_heuristicoDS,Puntaje_heuristicoDN)
PuntajesHunidos <- PuntajesHunidos[order(as.numeric(rownames(PuntajesHunidos))), ]
Datos$PuntajesHunidosDes <- PuntajesHunidos
#####
PuntajesACPunidos <- rbind(PuntajeDL,
                           PuntajeDM,
                           PuntajeDS,PuntajeDN)
PuntajesACPunidos <- PuntajesACPunidos[order(as.numeric(rownames(PuntajesACPunidos))), ]
Datos$PuntajesACPunidosDes <- PunDesEst

##### Clusters

PunDesEst <- round((PuntajesACPunidos - min(PuntajesACPunidos))*100/(max(PuntajesACPunidos)-min(PuntajesACPunidos)),2)
set.seed(1234)

kmeansPunDes <- kmeans(PunDesEst , 4, iter.max = 1000, nstart = 10)
fviz_cluster(kmeansPunDes,Datos[,c(29:73)],geom = c("point"))

kmeansPunDes$centers; tapply(PunDesEst, kmeansPunDes$cluster, summary)

tapply(PunDesEst, kmeansPunDes$cluster, function(x){c(min(x),max(x))})

kmeansPunDesH <- kmeans(PuntajesHunidos , 4, iter.max = 1000, nstart = 10)
fviz_cluster(kmeansPunDesH,Datos[,c(29:73)],geom = c("point"))
tapply(PuntajesHunidos, kmeansPunDesH$cluster, function(x){c(min(x),max(x))})
