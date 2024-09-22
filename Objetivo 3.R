#OBJETIVO 3
#DIVISION POR SEXO
##Puntaje de capacidad ACP
######################################################################################
Datos$sexo_cat <- factor(Datos$sexo, levels = c(1, 2), labels = c("Hombre", "Mujer"))

Datos$sexo <- Datos$sexo_cat
Datos$sexo_cat <- NULL #
######################################################################################
Datos$dif_cap_acp <- factor(Datos$clustercap, levels = c(1,4,2,3),
                            labels = c("Ninguna", "Leve","Moderada","Severa"))
Datos$dif_cap_heuris <- factor(Datos$clustercapH, levels = c(1,4,3,2),
                            labels = c("Ninguna", "Leve","Moderada","Severa"))

######################################################################################
df_hombres <- subset(Datos, sexo == "Hombre")



df_mujeres <- subset(Datos, sexo == "Mujer")



summary(df_hombres$PunCapEst)
summary(df_mujeres$PunCapEst)

# Crear los breaks para los histogramas
nbreaks1 <- pretty(range(df_hombres$PunCapEst),
                   n = nclass.Sturges(df_hombres$PunCapEst),
                   min.n = 1)

nbreaks2 <- pretty(range(df_mujeres$PunCapEst),
                   n = nclass.Sturges(df_mujeres$PunCapEst),
                   min.n = 1)

# Histograma y curva de densidad para hombres
hist_cap_h1 <- ggplot(data.frame(df_hombres$PunCapEst), 
                      aes(x = df_hombres$PunCapEst)) +
  geom_histogram(aes(y = ..density..), breaks = nbreaks1, 
                 color = "gray", fill = "skyblue") +
  geom_density(color = "black", size = 1) +
  xlab("Puntaje capacidad reescalado: Hombres") +
  ylab("Densidad") +
  ggtitle("Curva de Densidad: Hombres")

# Histograma y curva de densidad para mujeres
hist_cap_m1 <- ggplot(data.frame(df_mujeres$PunCapEst), 
                      aes(x = df_mujeres$PunCapEst)) +
  geom_histogram(aes(y = ..density..), breaks = nbreaks2, 
                 color = "gray", fill = "#B57FA4") +
  geom_density(color = "black", size = 1) +
  xlab("Puntaje capacidad reescalado: Mujeres") +
  ylab("Densidad") +
  ggtitle("Curva de Densidad: Mujeres")

library(gridExtra)

grid.arrange(
  hist_cap_h1,
  hist_cap_m1,
  ncol = 2
)


xtable(as.data.frame(Datos %>%
  group_by(sexo) %>%
  summarise(
    N = n(),
    Min = min(PunCapEst, na.rm = TRUE),
    Q1 = quantile(PunCapEst, 0.25, na.rm = TRUE),
    Median = median(PunCapEst, na.rm = TRUE),
    Mean = mean(PunCapEst, na.rm = TRUE),
    Q3 = quantile(PunCapEst, 0.75, na.rm = TRUE),
    Max = max(PunCapEst, na.rm = TRUE),
    SD = sd(PunCapEst, na.rm = TRUE)
  )))

##Puntaje de capacidad Heuristico
Datos$PunCapHeuris

summary(df_hombres$PunCapHeuris)
summary(df_mujeres$PunCapHeuris)

# Crear los breaks para los histogramas
nbreaks1Heuris <- pretty(range(df_hombres$PunCapHeuris),
                         n = nclass.Sturges(df_hombres$PunCapHeuris),
                         min.n = 1)

nbreaks2Heuris <- pretty(range(df_mujeres$PunCapHeuris),
                         n = nclass.Sturges(df_mujeres$PunCapHeuris),
                         min.n = 1)

# Histograma y curva de densidad para hombres
hist_cap_heuris_h1 <- ggplot(data.frame(df_hombres$PunCapHeuris), 
                             aes(x = df_hombres$PunCapHeuris)) +
  geom_histogram(aes(y = ..density..), breaks = nbreaks1Heuris, 
                 color = "gray", fill = "skyblue") +
  geom_density(color = "black", size = 1) +
  xlab("Puntaje capacidad heurístico: Hombres") +
  ylab("Densidad") +
  ggtitle("Curva de Densidad: Hombres")

# Histograma y curva de densidad para mujeres
hist_cap_heuris_m1 <- ggplot(data.frame(df_mujeres$PunCapHeuris), 
                             aes(x = df_mujeres$PunCapHeuris)) +
  geom_histogram(aes(y = ..density..), breaks = nbreaks2Heuris, 
                 color = "gray", fill = "#B57FA4") +
  geom_density(color = "black", size = 1) +
  xlab("Puntaje capacidad heurístico: Mujeres") +
  ylab("Densidad") +
  ggtitle("Curva de Densidad: Mujeres")



library(gridExtra)

grid.arrange(
  hist_cap_heuris_h1,
  hist_cap_heuris_m1,
  ncol = 2
)

xtable(as.data.frame(Datos %>%
                       group_by(sexo) %>%
                       summarise(
                         N = n(),
                         Min = min(PunCapHeuris, na.rm = TRUE),
                         Q1 = quantile(PunCapHeuris, 0.25, na.rm = TRUE),
                         Median = median(PunCapHeuris, na.rm = TRUE),
                         Mean = mean(PunCapHeuris, na.rm = TRUE),
                         Q3 = quantile(PunCapHeuris, 0.75, na.rm = TRUE),
                         Max = max(PunCapHeuris, na.rm = TRUE),
                         SD = sd(PunCapHeuris, na.rm = TRUE)
                       )))
####DESEMPEÑO
##MEDIDA CON ACP

ggplot(df_hombres, aes(x =PuntajesACPunidosDes)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  facet_wrap(~ dif_cap_acp, scales = "fixed") +
  labs(title = "Puntajes sinteticos de desempeño dados la dificultad en capacidad: Hombres",
       x = "Puntajes",
       y = "Densidad") +
  theme_minimal()

xtable(as.data.frame(df_hombres %>%
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
  )))

ggplot(df_hombres, aes(x =PuntajesHunidosDes)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  facet_wrap(~ dif_cap_heuris, scales = "fixed") +
  labs(title = "Puntajes heuristicos de desempeño dados la dificultad en capacidad: Hombres",
       x = "Puntajes",
       y = "Densidad") +
  theme_minimal()

xtable(as.data.frame(df_hombres %>%
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
  )))



#####################################

ggplot(df_mujeres, aes(x =PuntajesACPunidosDes)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  facet_wrap(~ dif_cap_acp, scales = "fixed") +
  labs(title = "Puntajes sintéticos de desempeño dados la dificultad en capacidad: Mujeres",
       x = "Puntajes",
       y = "Densidad") +
  theme_minimal()

xtable(as.data.frame(df_mujeres %>%
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
  )))

ggplot(df_mujeres, aes(x =PuntajesHunidosDes)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  facet_wrap(~ dif_cap_heuris, scales = "fixed") +
  labs(title = "Puntajes heuristicos de desempeño dados la dificultad en capacidad: Mujeres",
       x = "Puntajes",
       y = "Densidad") +
  theme_minimal()

xtable(as.data.frame(df_mujeres %>%
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
  )))


################################

#### Por edad
table(df_adults$age_cat)
# Categorias de edad 18-24  25-39  40-64 64-100
Datos$categoria_edad <- cut(Datos$edad,
                         breaks = c(18, 24, 39, 64, 100),
                         labels = c("18-24", "25-39", "40-64", "64-100"),
                         right = FALSE)

ggplot(Datos, aes(x = categoria_edad, y = PunCapEst,fill =  categoria_edad)) + 
  geom_boxplot()+xlab("Categorias de edad") +
  ylab("Puntaje dificultad capacidad reescalado")

# Función para obtener las estadísticas descriptivas
summary_stats <- function(x) {
  stats <- summary(x)
  return(as.data.frame(as.list(stats)))
}

# Aplicar la función a cada grupo

xtable(as.data.frame(Datos %>%
                       group_by(categoria_edad) %>%
                       summarise(
                         N = n(),
                         Min = min(PunCapEst, na.rm = TRUE),
                         Q1 = quantile(PunCapEst, 0.25, na.rm = TRUE),
                         Median = median(PunCapEst, na.rm = TRUE),
                         Mean = mean(PunCapEst, na.rm = TRUE),
                         Q3 = quantile(PunCapEst, 0.75, na.rm = TRUE),
                         Max = max(PunCapEst, na.rm = TRUE),
                         SD = sd(PunCapEst, na.rm = TRUE)
                       )))


############################# CAPACIDAD HEURISTICO

ggplot(Datos, aes(x = categoria_edad, y = PunCapHeuris,fill =  categoria_edad)) + 
  geom_boxplot()+xlab("Categorias de edad") +
  ylab("Puntaje dificultad capacidad heuristico")


# Aplicar la función a cada grupo
xtable(as.data.frame(Datos %>%
                       group_by(categoria_edad) %>%
                       summarise(
                         N = n(),
                         Min = min(PunCapHeuris, na.rm = TRUE),
                         Q1 = quantile(PunCapHeuris, 0.25, na.rm = TRUE),
                         Median = median(PunCapHeuris, na.rm = TRUE),
                         Mean = mean(PunCapHeuris, na.rm = TRUE),
                         Q3 = quantile(PunCapHeuris, 0.75, na.rm = TRUE),
                         Max = max(PunCapHeuris, na.rm = TRUE),
                         SD = sd(PunCapHeuris, na.rm = TRUE)
                       )))

############################# DESEMPEÑO
table(Datos$categoria_edad)
df_18_24 <- subset(Datos, categoria_edad == "18-24")

# Crear el boxplot ACP
ggplot(df_18_24, aes(x = dif_cap_acp, y = PuntajesACPunidosDes, fill =dif_cap_acp)) +
  geom_boxplot() +
  labs(title = "Boxplot de puntajes ACP por dificultad en capacidad: 18-24 Años",
       x = "Clasificación",
       y = "Puntajes") +
  theme_minimal() +
  theme(legend.position = "none")

xtable(as.data.frame(df_18_24 %>%
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
                )))


# Crear el boxplot HEURIS
ggplot(df_18_24, aes(x = dif_cap_heuris, y = PuntajesHunidosDes, fill =dif_cap_heuris)) +
  geom_boxplot() +
  labs(title = "Boxplot de puntajes HEURISTICOS por dificultad en capacidad: 18-24 Años",
       x = "Clasificación",
       y = "Puntajes") +
  theme_minimal() +
  theme(legend.position = "none")

xtable(as.data.frame(df_18_24 %>%
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
                )))


df_25_39 <- subset(Datos, categoria_edad == "25-39")

# Crear el boxplot ACP
ggplot(df_25_39, aes(x = dif_cap_acp, y = PuntajesACPunidosDes, fill =dif_cap_acp)) +
  geom_boxplot() +
  labs(title = "Boxplot de puntajes ACP por dificultad en capacidad: 25-39 Años",
       x = "Clasificación",
       y = "Puntajes") +
  theme_minimal() +
  theme(legend.position = "none")

xtable(as.data.frame(df_25_39 %>%
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
                )))

# Crear el boxplot HEURIS
ggplot(df_25_39, aes(x = dif_cap_heuris, y = PuntajesHunidosDes, fill =dif_cap_heuris)) +
  geom_boxplot() +
  labs(title = "Boxplot de puntajes HEURISTICOS por dificultad en capacidad: 25-39 Años",
       x = "Clasificación",
       y = "Puntajes") +
  theme_minimal() +
  theme(legend.position = "none")

xtable(as.data.frame(df_25_39%>%
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
                )))


df_40_64 <- subset(Datos, categoria_edad == "40-64")
# Crear el boxplot ACP
ggplot(df_40_64, aes(x = dif_cap_acp, y = PuntajesACPunidosDes, fill =dif_cap_acp)) +
  geom_boxplot() +
  labs(title = "Boxplot de puntajes ACP por dificultad en capacidad: 40-64 Años",
       x = "Clasificación",
       y = "Puntajes") +
  theme_minimal() +
  theme(legend.position = "none")

xtable(as.data.frame(df_40_64 %>%
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
                )))

# Crear el boxplot HEURIS
ggplot(df_40_64, aes(x = dif_cap_heuris, y = PuntajesHunidosDes, fill =dif_cap_heuris)) +
  geom_boxplot() +
  labs(title = "Boxplot de puntajes HEURISTICOS por dificultad en capacidad: 40-64 Años",
       x = "Clasificación",
       y = "Puntajes") +
  theme_minimal() +
  theme(legend.position = "none")

xtable(as.data.frame(df_40_64%>%
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
                )))

df_64_100 <- subset(Datos, categoria_edad == "64-100")
# Crear el boxplot ACP
ggplot(df_64_100, aes(x = dif_cap_acp, y = PuntajesACPunidosDes, fill =dif_cap_acp)) +
  geom_boxplot() +
  labs(title = "Boxplot de puntajes ACP por dificultad en capacidad: 64-100 Años",
       x = "Clasificación",
       y = "Puntajes") +
  theme_minimal() +
  theme(legend.position = "none")

xtable(as.data.frame(df_64_100 %>%
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
                )))




# Crear el boxplot HEURIS
ggplot(df_64_100, aes(x = dif_cap_heuris, y = PuntajesHunidosDes, fill =dif_cap_heuris)) +
  geom_boxplot() +
  labs(title = "Boxplot de puntajes HEURISTICOS por dificultad en capacidad: 64-100 Años",
       x = "Clasificación",
       y = "Puntajes") +
  theme_minimal() +
  theme(legend.position = "none")

xtable(as.data.frame(df_64_100%>%
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
                )))
