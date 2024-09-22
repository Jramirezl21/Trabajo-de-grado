xtable(as.data.frame(Datos %>%
  group_by(sexo) %>%
  summarise(
    N = n(),
    Min = min(edad, na.rm = TRUE),
    Q1 = quantile(edad, 0.25, na.rm = TRUE),
    Median = median(edad, na.rm = TRUE),
    Mean = mean(edad, na.rm = TRUE),
    Q3 = quantile(edad, 0.75, na.rm = TRUE),
    Max = max(edad, na.rm = TRUE),
    SD = sd(edad, na.rm = TRUE)
  )))
##### Comparacion varianzas explicadas


xtable(rbind(varExp_Cap1[[indice_maximo]]*100/sum(varExp_Cap1[[indice_maximo]]),
             varExp_Cap2[[indice_maximo2]]*100/sum( varExp_Cap2[[indice_maximo2]]),
             varExp_Cap3[[indice_maximo3]]*100/sum(varExp_Cap3[[indice_maximo3]]),
             varExp_Cap4*100/sum(varExp_Cap4))[,1:2])
#Seleccionar el de mayor varianza explicada, la muestra 2

Pesoscap <- cbind(Pesos_Cap1[[indice_maximo]][,1],
                  Pesos_Cap2[[indice_maximo2]][,1],
                  Pesos_Cap3[[indice_maximo3]][,1],
                  Pesos_Cap4[,1])

xtable(Pesoscap)
#Se selecciona el intervalo mas grande en puntuaciones
vector1 ;max(puntuaciones1)-min(puntuaciones1)
vector2 ;max(puntuaciones2)-min(puntuaciones2)
vector3 ;max(puntuaciones3)-min(puntuaciones3)
vector4 ;max(puntuaciones4)-min(puntuaciones4)


kmeansPunCap <- kmeans(PunCapEst , 4, iter.max = 1000, nstart = 10)
fviz_cluster(kmeansPunCap,DatosCap)
Datos$clustercap <- kmeansPunCap$cluster
kmeansPunCap$centers; tapply(PunCapEst, kmeansPunCap$cluster, summary)
baremos <- as.data.frame(tapply(PunCapEst, kmeansPunCap$cluster, function(x){c(max(x),min(x))}))
colnames(baremos) <- c("Baremos")
tapply(PunCapEst, kmeansPunCap$cluster, function(x){c(min(x),max(x))})
#Correlacion medida heuris

summary(PunCapEst)
summary(Puntaje_heuristico)
nbreaks1 <- pretty(range(PunCapEst), n = nclass.Sturges(PunCapEst),
                  min.n = 1)
nbreaks2 <- pretty(range(Puntaje_heuristico), n = nclass.Sturges(Puntaje_heuristico),
                   min.n = 1)
hist1 <- ggplot(data.frame(PunCapEst), aes(x = PunCapEst)) +xlab("Puntaje capacidad reescalado")+
  geom_histogram(aes(y = ..density..), breaks = nbreaks1,
                 color = "gray", fill = "skyblue") +
  geom_density(fill = "black", alpha = 0.2)
max(Puntaje_heuristico)
min(Puntaje_heuristico)

hist2 <- ggplot(data.frame(Puntaje_heuristico), aes(x = Puntaje_heuristico)) +xlab("Puntaje Heuristico")+
  geom_histogram(aes(y = ..density..), breaks = nbreaks2,
                 color = "gray", fill = "skyblue") +
  geom_density(fill = "black", alpha = 0.2)+xlim(20,110)
library(gridExtra)

grid.arrange(
  hist1,
  hist2,
  ncol = 2,
  top = paste("Correlación de Spearman: ", round(cor_spearman, 2))
)
          
cor_spearman <- cor(PunCapEst,Puntaje_heuristico , method = "spearman")

xtable::xtable(rbind(summary(PunCapEst),
                     summary(Puntaje_heuristico)))

kmeansHeuris <- kmeans(Puntaje_heuristico,4, iter.max = 1000, nstart = 1)
fviz_cluster(kmeansHeuris,data=DatosCap[,c(1:22)],geom = ("point"))
clustersHeuris<- data.frame(cbind(kmeansHeuris$cluster,Puntaje_euristico))
DatosPun$clustercapH<- kmeansHeuris$cluster;summary(DatosPun)
kmeansHeuris$centers; tapply( Puntaje_heuristico,
                              kmeansHeuris$cluster, function(x){c(min(x),max(x))})

xtable((table(kmeansPunCap$cluster)/length(kmeansPunCap$cluster))*100)
xtable((table(kmeansHeuris$cluster)/length(kmeansHeuris$cluster))*100)
#OBJETIVO 2

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
#####
PuntajesACPunidos <- rbind(PuntajeDL,
                           PuntajeDM,
                           PuntajeDS,PuntajeDN)
PuntajesACPunidos <- PuntajesACPunidos[order(as.numeric(rownames(PuntajesACPunidos))), ]
##### Clusters

PunDesEst <- round((PuntajesACPunidos - min(PuntajesACPunidos))*100/(max(PuntajesACPunidos)-min(PuntajesACPunidos)),2)
set.seed(1234)
rm(DatosPun)
DatosPun <- select(Datos,-c24,-c24,-fa11,-fa12,-d46,-d47)
DatosPun <- cbind(DatosPun,PunDesEst)
kmeansPunDes <- kmeans(PunDesEst , 4, iter.max = 1000, nstart = 10)
fviz_cluster(kmeansPunDes,DatosPun[,c(28:72)],axes = c(1, 2))
DatosPun$clusterDes <- kmeansPunDes$cluster;summary(DatosPun)
kmeansPunDes$centers; tapply(PunDesEst, kmeansPunDes$cluster, summary)
xtable::xtable((table(kmeansPunDes$cluster)/length(kmeansPunDes$cluster))*100)

#Desempeño condicionado ACP

ggplot(Datos, aes(x =PuntajesACPunidosDes)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  facet_wrap(~ dif_cap_acp, scales = "fixed") +
  labs(title = "Puntajes sintéticos de desempeño dados la dificultad en capacidad",
       x = "Puntajes",
       y = "Densidad") +
  theme_minimal()

Desc_des_dado_acp <- Datos %>%
  group_by(dif_cap_acp) %>%
  summarise(
    N = n(),
    Min = min(PuntajesACPunidosDes, na.rm = TRUE),
    Q1 = quantile(PuntajesACPunidosDes, 0.25, na.rm = TRUE),
    Median = median(PuntajesACPunidosDes, na.rm = TRUE),
    Mean = mean(PuntajesACPunidosDes, na.rm = TRUE),
    Q3 = quantile(PuntajesACPunidosDes, 0.75, na.rm = TRUE),
    Max = max(PuntajesACPunidosDes, na.rm = TRUE),
    SD = sd(PuntajesACPunidosDes, na.rm = TRUE)
  )
xtable(as.data.frame(Desc_des_dado_acp ))
# Desemepño condicionado heuris

ggplot(Datos, aes(x =PuntajesHunidosDes)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  facet_wrap(~ dif_cap_heuris, scales = "fixed") +
  labs(title = "Puntajes heuristicos de desempeño dados la dificultad en capacidad",
       x = "Puntajes",
       y = "Densidad") +
  theme_minimal()

Desc_des_dado_h<- Datos %>%
  group_by(dif_cap_heuris) %>%
  summarise(
    N = n(),
    Min = min(PuntajesHunidosDes, na.rm = TRUE),
    Q1 = quantile(PuntajesHunidosDes, 0.25, na.rm = TRUE),
    Median = median(PuntajesHunidosDes, na.rm = TRUE),
    Mean = mean(PuntajesHunidosDes, na.rm = TRUE),
    Q3 = quantile(PuntajesHunidosDes, 0.75, na.rm = TRUE),
    Max = max(PuntajesHunidosDes, na.rm = TRUE),
    SD = sd(PuntajesHunidosDes, na.rm = TRUE)
  )
xtable(as.data.frame(Desc_des_dado_h))
#############

table()
xtable::xtable(rbind(summary(PunDesEst),
summary(PuntajesHunidos)))

par(mfrow=c(1,2))
hist(PuntajesACPunidos,main="Puntuaciones ACP",xlab="Puntaje Desempeño reescalado")
hist(PuntajesHunidos ,main="Puntaje Heuristico",xlab="Puntaje Heuristico")
legend(x = "topright",         # Posición
       legend = round(cor_spearmanobj2,3 ),title = "Cor Spearman",bty = "n") 
xtable::xtable((table(kmeansPunDesH$cluster)/length(kmeansPunDesH$cluster))*100)
