#PECM
#Para capacidad
library(moments)
hist(PunCapEst)
NfinalMedia<-c() #cada repeticion bootstrap
for (i in 1:1000) {

  NfinalMedia[i]=median(sample(PunCapEst,replace = TRUE))
}
skewness(NfinalMedia)

hist(NfinalMedia)
I_media=round(mean(NfinalMedia,3))
sum(NfinalMedia)/1000
#Asumiendo insesgadez
EE<-var(NfinalMedia)+(mean(NfinalMedia)-mean(NfinalMedia))
EE

#MEDIDA HEURISTICA CAP
hist(Puntaje_heuristico)
NfinalMedia_h<-c() #cada repeticion bootstrap
for (i in 1:1000) {
  
  NfinalMedia_h[i]=median(sample(Puntaje_heuristico,replace = TRUE))
}
skewness(NfinalMedia_h)

hist(NfinalMedia_h)

#Asumiendo insesgadez
EE_H<-var(NfinalMedia_h)+(median(NfinalMedia_h)-median(NfinalMedia_h))
EE_H

########## DESEMPEÑO ACP

hist(Datos[Datos$dif_cap_acp== "Ninguna", ]$PuntajesACPunidosDes)
NfinalMedia_des_n<-c()
for (i in 1:1000) {
  
  NfinalMedia_des_n[i]=median(sample(Datos[Datos$dif_cap_acp == "Ninguna", ]$PuntajesACPunidosDes,replace = TRUE))
}
hist(NfinalMedia_des_n)
skewness(NfinalMedia_des_n)
EE_DES_N<-var(NfinalMedia_des_n)+(mean(NfinalMedia_des_n)-mean(NfinalMedia_des_n))


hist(Datos[Datos$dif_cap_acp == "Leve", ]$PuntajesACPunidosDes)
NfinalMedia_des_l<-c()
for (i in 1:1000) {
  
  NfinalMedia_des_l[i]=median(sample(Datos[Datos$dif_cap_acp == "Leve", ]$PuntajesACPunidosDes,replace = TRUE))
}
hist(NfinalMedia_des_l)
skewness(NfinalMedia_des_l)
EE_DES_L<-var(NfinalMedia_des_l)+(mean(NfinalMedia_des_l)-mean(NfinalMedia_des_l))


hist(Datos[Datos$dif_cap_acp == "Moderada", ]$PuntajesACPunidosDes)
skewness(Datos[Datos$dif_cap_acp == "Moderada", ]$PuntajesACPunidosDes)
NfinalMedia_des_m<-c()
for (i in 1:1000) {
  
  NfinalMedia_des_m[i]=mean(sample(Datos[Datos$dif_cap_acp == "Moderada", ]$PuntajesACPunidosDes,replace = TRUE))
}
hist(NfinalMedia_des_m)
skewness(NfinalMedia_des_m)
EE_DES_M<-var(NfinalMedia_des_m)+(mean(NfinalMedia_des_m)-mean(NfinalMedia_des_m))


hist(Datos[Datos$dif_cap_acp == "Severa", ]$PuntajesACPunidosDes)
skewness(Datos[Datos$dif_cap_acp == "Severa", ]$PuntajesACPunidosDes)
NfinalMedia_des_s<-c()
for (i in 1:1000) {
  
  NfinalMedia_des_s[i]=mean(sample(Datos[Datos$dif_cap_acp == "Severa", ]$PuntajesACPunidosDes,replace = TRUE))
}
hist(NfinalMedia_des_s)
skewness(NfinalMedia_des_s)
EE_DES_S<-var(NfinalMedia_des_s)+(mean(NfinalMedia_des_s)-mean(NfinalMedia_des_s))

######### DESEMPEÑO HEURISTICA

hist(Datos[Datos$dif_cap_heuris == "Ninguna", ]$PuntajesHunidosDes)
NfinalMedia_des_hn<-c()
for (i in 1:1000) {
  
  NfinalMedia_des_hn[i]=median(sample(Datos[Datos$dif_cap_heuris == "Ninguna", ]$PuntajesHunidosDes,replace = TRUE))
}
hist(NfinalMedia_des_hn)
skewness(NfinalMedia_des_hn)
EE_DES_HN<-var(NfinalMedia_des_hn)+(median(NfinalMedia_des_hn)-median(NfinalMedia_des_hn))


hist(Datos[Datos$dif_cap_heuris == "Leve", ]$PuntajesHunidosDes)
NfinalMedia_des_hl<-c()
for (i in 1:1000) {
  
  NfinalMedia_des_hl[i]=median(sample(Datos[Datos$dif_cap_heuris == "Leve", ]$PuntajesHunidosDes,replace = TRUE))
}
hist(NfinalMedia_des_hl)
skewness(NfinalMedia_des_hl)
EE_DES_HL<-var(NfinalMedia_des_hl)+(mean(NfinalMedia_des_hl)-mean(NfinalMedia_des_hl))


hist(Datos[Datos$dif_cap_heuris == "Moderada", ]$PuntajesHunidosDes)
NfinalMedia_des_hm<-c()
for (i in 1:1000) {
  
  NfinalMedia_des_hm[i]=mean(sample(Datos[Datos$dif_cap_heuris == "Moderada", ]$PuntajesHunidosDes,replace = TRUE))
}
hist(NfinalMedia_des_hm)
skewness(NfinalMedia_des_hm)
EE_DES_HM<-var(NfinalMedia_des_hm)+(mean(NfinalMedia_des_hm)-mean(NfinalMedia_des_hm))


hist(Datos[Datos$dif_cap_heuris == "Severa", ]$PuntajesHunidosDes)
NfinalMedia_des_hs<-c()
for (i in 1:1000) {
  
  NfinalMedia_des_hs[i]=mean(sample(Datos[Datos$dif_cap_heuris == "Severa", ]$PuntajesHunidosDes,replace = TRUE))
}
hist(NfinalMedia_des_hs)
skewness(NfinalMedia_des_hs)
EE_DES_HS<-var(NfinalMedia_des_hs)+(mean(NfinalMedia_des_hs)-mean(NfinalMedia_des_hs))
