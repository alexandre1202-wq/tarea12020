# Evolucion de calificaciones

Meses_calificados <- c("2022-03-31",
                       "2022-04-30",
                       "2022-05-31")
Grupos <- list(c("Bancos","b_Grandes"),
               c("Bancos","b_Medianos"),
               c("Bancos","b_Pequeños"),
               c("Cooperativas","S1"),
               c("Cooperativas","S2"),
               c("Aseguradoras","Aseguradoras"))


Bancos_Grandes <- Mod.Final(f2=Meses_calificados[1]) %>%
  left_join(Mod.Final(f2=Meses_calificados[2]),by="Nombre") %>%
  left_join(Mod.Final(f2=Meses_calificados[3]),by="Nombre") %>%
  rename("Estatico_Marzo"=2,
         "Dinamico_Marzo"=3,
         "Final_Marzo"=4,
         "Estatico_Abril"=5,
         "Dinamico_Abril"=6,
         "Final_Abril"=7,
         "Estatico_Mayo"=8,
         "Dinamico_Mayo"=9,
         "Final_Mayo"=10) %>%
  select(1,4,7,10) %>%
  mutate(P1 = puntaje_parcial(Final_Marzo),
         P2 = puntaje_parcial(Final_Abril),
         P3 = puntaje_parcial(Final_Mayo)) %>%
  mutate(V1 = P2-P1,
         V2 = P3-P2) %>%
  mutate(C1 = ifelse(V1 !=0,ifelse(V1 <0,"BAJA","SUBE"),"NO CAMBIA"),
         C2 = ifelse(V2 !=0,ifelse(V2 <0,"BAJA","SUBE"),"NO CAMBIA")) %>%
  select(c(1:4),C1,C2) %>%
  rename("Evolucion_Marzo_Abril"=C1,
         "Evolucion_Abril_Mayo"=C2)



Resumen <- list()

for (gsim in Grupos) {
  Resumen[[gsim[2]]] <- Mod.Final(Grupo = gsim, f2=Meses_calificados[1]) %>%
    left_join(Mod.Final(Grupo = gsim,f2=Meses_calificados[2]),by="Nombre") %>%
    left_join(Mod.Final(Grupo = gsim,f2=Meses_calificados[3]),by="Nombre") %>%
    rename("Estatico_Marzo"=2,
           "Dinamico_Marzo"=3,
           "Final_Marzo"=4,
           "Estatico_Abril"=5,
           "Dinamico_Abril"=6,
           "Final_Abril"=7,
           "Estatico_Mayo"=8,
           "Dinamico_Mayo"=9,
           "Final_Mayo"=10) %>%
    select(1,4,7,10) %>%
    mutate(P1 = puntaje_parcial(Final_Marzo),
           P2 = puntaje_parcial(Final_Abril),
           P3 = puntaje_parcial(Final_Mayo)) %>%
    mutate(V1 = P2-P1,
           V2 = P3-P2) %>%
    mutate(C1 = ifelse(V1 !=0,ifelse(V1 <0,"BAJA","SUBE"),"NO CAMBIA"),
           C2 = ifelse(V2 !=0,ifelse(V2 <0,"BAJA","SUBE"),"NO CAMBIA")) %>%
    select(c(1:4),C1,C2) %>%
    rename("Evolucion_Marzo_Abril"=C1,
           "Evolucion_Abril_Mayo"=C2)
}


Mod.Final(c("Cooperativas","S2"),f2="2022-04-30") %>% View()

Resumen %>% openxlsx::write.xlsx("Resumen2_v3.xlsx",overwrite = T)





