pun_lev_des_acp_res <- round((PuntajeDL$PuntajeD.ACP- min(PuntajeDL$PuntajeD.ACP))*
                               100/(max(PuntajeDL$PuntajeD.ACP)-min(PuntajeDL$PuntajeD.ACP)),2)

pun_mod_des_acp_res <- round((PuntajeDM$PuntajeD.ACP- min(PuntajeDM$PuntajeD.ACP))*
                               100/(max(PuntajeDM$PuntajeD.ACP)-min(PuntajeDM$PuntajeD.ACP)),2)

pun_sev_des_acp_res <- round((PuntajeDS$PuntajeD.ACP- min(PuntajeDS$PuntajeD.ACP))*
                               100/(max(PuntajeDS$PuntajeD.ACP)-min(PuntajeDS$PuntajeD.ACP)),2)

pun_nin_des_acp_res <- round((PuntajeDN$PuntajeD.ACP- min(PuntajeDN$PuntajeD.ACP))*
                               100/(max(PuntajeDN$PuntajeD.ACP)-min(PuntajeDN$PuntajeD.ACP)),2)

nbreaks_des_leve_acp <- pretty(range(pun_lev_des_acp_res), n = nclass.Sturges(pun_lev_des_acp_res),
                               min.n = 1)
nbreaks_des_mod_acp <- pretty(range(pun_mod_des_acp_res ), n = nclass.Sturges(pun_mod_des_acp_res ),
                              min.n = 1)
nbreaks_des_sev_acp <- pretty(range(pun_sev_des_acp_res), n = nclass.Sturges(pun_sev_des_acp_res),
                              min.n = 1)
nbreaks_des_nin_acp <- pretty(range(pun_nin_des_acp_res), n = nclass.Sturges(pun_nin_des_acp_res),
                              min.n = 1)


# Desemepño condicionado heuris

pun_lev_des_h_res <-Puntaje_heuristicoDL$PuntajeDH
pun_mod_des_h_res <-Puntaje_heuristicoDM$PuntajeDH
pun_sev_des_h_res <-Puntaje_heuristicoDS$PuntajeDH
pun_nin_des_h_res <-Puntaje_heuristicoDN$PuntajeDH


nbreaks_des_leve_h <- pretty(range(pun_lev_des_h_res), n = nclass.Sturges(pun_lev_des_h_res),
                             min.n = 1)
nbreaks_des_mod_h <- pretty(range(pun_mod_des_h_res), n = nclass.Sturges(pun_mod_des_h_res),
                            min.n = 1)
nbreaks_des_sev_h <- pretty(range(pun_sev_des_h_res), n = nclass.Sturges(pun_sev_des_h_res),
                            min.n = 1)
nbreaks_des_nin_h <- pretty(range(pun_nin_des_h_res), n = nclass.Sturges(pun_nin_des_h_res),
                            min.n = 1)



hist_des_leve_h <- ggplot(data.frame(pun_lev_des_h_res), aes(x = pun_lev_des_h_res)) +xlab("Desempeño dado cap lev")+
  geom_histogram(aes(y = ..density..), breaks = nbreaks_des_leve_h,
                 color = "gray", fill = "skyblue") +
  geom_density(fill = "black", alpha = 0.2)
hist_des_mod_h <- ggplot(data.frame(pun_mod_des_h_res), aes(x = pun_mod_des_h_res)) +xlab("Desempeño dado cap mod")+
  geom_histogram(aes(y = ..density..), breaks = nbreaks_des_mod_h,
                 color = "gray", fill = "skyblue") +
  geom_density(fill = "black", alpha = 0.2)
hist_des_sev_h <- ggplot(data.frame(pun_sev_des_h_res), aes(x = pun_sev_des_h_res)) +xlab("Desempeño dado cap sev")+
  geom_histogram(aes(y = ..density..), breaks = nbreaks_des_sev_h,
                 color = "gray", fill = "skyblue") +
  geom_density(fill = "black", alpha = 0.2)
hist_des_nin_h <- ggplot(data.frame(pun_nin_des_h_res), aes(x = pun_nin_des_h_res)) +xlab("Desempeño dado cap nin")+
  geom_histogram(aes(y = ..density..), breaks = nbreaks_des_nin_h,
                 color = "gray", fill = "skyblue") +
  geom_density(fill = "black", alpha = 0.2)




grid.arrange(
  hist_des_leve_h ,hist_des_mod_h ,
  hist_des_sev_h ,hist_des_nin_h,
  ncol = 2
)


hist_des_leve_acp <- ggplot(data.frame(pun_lev_des_acp_res ), aes(x = pun_lev_des_acp_res )) +xlab("Desempeño dado cap lev")+
  geom_histogram(aes(y = ..density..), breaks = nbreaks_des_leve_acp,
                 color = "gray", fill = "skyblue") +
  geom_density(fill = "black", alpha = 0.2)

hist_des_mod_acp <- ggplot(data.frame(pun_mod_des_acp_res), aes(x = pun_mod_des_acp_res )) +xlab("Desempeño dado cap mod")+
  geom_histogram(aes(y = ..density..), breaks = nbreaks_des_mod_acp,
                 color = "gray", fill = "skyblue") +
  geom_density(fill = "black", alpha = 0.2)

hist_des_sev_acp <- ggplot(data.frame(pun_sev_des_acp_res ), aes(x = pun_sev_des_acp_res )) +xlab("Desempeño dado cap sev")+
  geom_histogram(aes(y = ..density..), breaks = nbreaks_des_sev_acp,
                 color = "gray", fill = "skyblue") +
  geom_density(fill = "black", alpha = 0.2)

hist_des_nin_acp <- ggplot(data.frame(pun_nin_des_acp_res ), aes(x = pun_nin_des_acp_res)) +xlab("Desempeño dado cap nin")+
  geom_histogram(aes(y = ..density..), breaks = nbreaks_des_nin_acp,
                 color = "gray", fill = "skyblue") +
  geom_density(fill = "black", alpha = 0.2)




grid.arrange(
  hist_des_leve_acp ,hist_des_mod_acp ,
  hist_des_sev_acp,hist_des_nin_acp,
  ncol = 2
)
