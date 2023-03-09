#######################################################################
# LECTURA DE LA BASE DE DATOS
#######################################################################

source("Librerias.R")

lectura <- function(x="Bancos",y="Grandes"){
  # x: Nombre del Grupo
  # y: Nombre del Sub Grupo
  
  BD <- read_excel(sprintf("BDD/%s/%s.xlsx",x,y))
  nombres <- colnames(BD) 
  colnames(BD) <- c("Fecha","Nombre",
                    "X1","X2","X3","X4","X5",
                    "X6","X7","X8","X9","X10")
  BD <- BD %>% mutate(Fecha = as.Date(Fecha),
                      Nombre = factor(Nombre),
                      X4 = -X4,
                      X6 = -X6,
                      X8 = -X8)
  
  colnames(BD) <- nombres 
  return(BD)
}

#######################################################################
# MODELO ESTATICO
#######################################################################

estadisticas <- function(x){
  # x: Vector a evaluar
  return(c(mean(ind_depurado(x)),sd(ind_depurado(x))))
}


cal_estatico <- function(x,LC = c(0.2,0.4,0.6,0.8)){
  # x: Observacion a evaluar
  ifelse(x<qnorm(LC[1]),2,
         ifelse(x<qnorm(LC[2]),4,
                ifelse(x<qnorm(LC[3]),6,
                       ifelse(x<qnorm(LC[4]),8,10))))
}

ind_depurado <- function(x){
  # x: Vector a evaluar de tamaño mayor o igual a 4
  x[x > quantile(x,0.05) & x < quantile(x,0.95)]
}

cal_estatico_global <- function(x){
  # x: Observacion a Evaluar
  ifelse(x<20+9,"E",
         ifelse(x<20+2*9,"D",
                ifelse(x<20+3*9,"C",
                       ifelse(x<20+4*9,"B",
                              ifelse(x<20+5*9,"BB",
                                     ifelse(x<20+6*9,"BBB",
                                            ifelse(x<20+7*9,"A",
                                                   ifelse(x<20+8*9,"AA","AAA"))))))))
}


estandarizar_valores <- function(x,y){
  # x: Matriz de valores a estandarizar
  # y: Vector con Media y Varianza para estandarizar a x
  for (esim in (ncol(x)-9):ncol(x)){
    x[,esim] = (x[,esim]-as.numeric(y[1, esim-(ncol(x)-10)]))/as.numeric(y[2,esim-(ncol(x)-10)])
  }
  return(x)
}


calificar_valores <- function(x,LC=c(0.2,0.4,0.6,0.4)){
  # x: Matriz de valores a calificar
  for (esim in (ncol(x)-9):ncol(x)){
    x[,esim] = cal_estatico(x[,esim],LC)
  }
  x$Total = rowSums(x[,(ncol(x)-9):ncol(x)])
  x$Calificacion = cal_estatico_global(x$Total)
  
  return(x)
}


Mod.Estatico <- function(Grupo=c("Bancos","Grandes"), i_evaluado="2022-07-31", LC=c(0.2,0.4,0.6,0.8)){
  # DF: Base de Datos del Grupo a Evaluar
  # punto_corte: Punto de Corte
  # f2: Instante de tiempo a Evaluar
  punto_corte =floor_date(as.Date(i_evaluado),"months")-1
  DF <- lectura(Grupo[1],Grupo[2]) 
  colnames(DF)[2]<- "Nombre"
  H <- DF %>% filter(Fecha <= punto_corte)
  N <- DF %>% filter(Fecha == i_evaluado)
  
  
  
  EH <- H[,3:ncol(H)] %>% summarise_if(is.numeric,estadisticas)
  VE <- estandarizar_valores(N,EH)
  CP_est <- calificar_valores(VE, LC)
  CP_est <- CP_est %>% mutate(Nombre = as.character(Nombre)) %>% select(-Fecha)
  print(sprintf("(Estatico) Calificaciones emitidas a fecha: %s, con punto de corte a fecha: %s",
                i_evaluado,punto_corte))
  return(list("Calificaciones"=CP_est,
              "Transformados"=VE %>% select(-Fecha), 
              "Estadisticas"=EH,
              "Originales"=N %>% select(-Fecha)))
}


#######################################################################
# MODELO DINAMICO
#######################################################################


calculo_pendientes <- function(x){
  # x: Vector
  res.lm <- lm(x ~c(1:length(x)))
  return(unname(coefficients(res.lm)[2]))
}





generar_pendientes <- function(x){
  # x: Base de Datos
  nombres <- colnames(x)
  colnames(x) <- c("Fecha","Nombre",
                   "X1","X2","X3","X4","X5",
                   "X6","X7","X8","X9","X10")
  XP <- x %>% 
    group_by(Nombre) %>%
    summarise_if(is.numeric,calculo_pendientes)
  colnames(XP) <- nombres[2:length(nombres)]
  return(XP)
  
}

pendientes_ponderadas <- function(XP,i_evaluado="2022-03-31"){
  pend_pond <- XP %>% rename("Nombre"=2) %>% 
    filter(Fecha <=i_evaluado) %>% 
    filter(time_length(difftime(i_evaluado,Fecha), "months") <=50) %>%
    mutate(Intervalo = ifelse(time_length(difftime(i_evaluado,Fecha),"months")<=12,"Intervalo_1",
                              ifelse(time_length(difftime(i_evaluado,Fecha),"months")<=24,"Intervalo_2",
                                     ifelse(time_length(difftime(i_evaluado,Fecha),"months")<=36,"Intervalo_3","Intervalo_4")))) %>%
    group_by(Nombre, Intervalo) %>%
    summarise_if(is.numeric,calculo_pendientes) %>%
    melt() %>% 
    dcast(Nombre+variable~Intervalo, value.var = "value") %>%
    mutate(Intervalo_1=replace(Intervalo_1,is.na(Intervalo_1),0),
           Intervalo_2=replace(Intervalo_1,is.na(Intervalo_2),0),
           Intervalo_3=replace(Intervalo_1,is.na(Intervalo_3),0),
           Intervalo_4=replace(Intervalo_1,is.na(Intervalo_4),0)) %>%
    mutate(Pendiente_ponderada = 0.4*Intervalo_1 + 0.3*Intervalo_2 + 0.15*Intervalo_3+0.15*Intervalo_4) %>%
    select(Nombre,variable,Pendiente_ponderada) %>%
    dcast(Nombre~variable, value.var = "Pendiente_ponderada")
  return(pend_pond)
}


pendientes_ponderadas_2  <- function(XP,i_evaluado="2022-03-31"){
  pend_pond <- XP %>% rename("Nombre"=2) %>% 
    filter(Fecha <=i_evaluado) %>% 
    filter(time_length(difftime(i_evaluado,Fecha), "months") <=50) %>%
    mutate(Intervalo = ifelse(time_length(difftime(i_evaluado,Fecha),"months")<=17,"Intervalo_1",
                              ifelse(time_length(difftime(i_evaluado,Fecha),"months")<=34,"Intervalo_2","Intervalo_3"))) %>%
    group_by(Nombre, Intervalo) %>%
    summarise_if(is.numeric,calculo_pendientes) %>%
    melt(id.vars=c("Nombre","Intervalo")) %>% 
    dcast(Nombre+variable~Intervalo, value.var = "value") %>%
    mutate(Intervalo_1=replace(Intervalo_1,is.na(Intervalo_1),0),
           Intervalo_2=replace(Intervalo_2,is.na(Intervalo_2),0),
           Intervalo_3=replace(Intervalo_3,is.na(Intervalo_3),0)) %>%
    mutate(Pendiente_ponderada = 0.5*Intervalo_1 + 0.3*Intervalo_2 + 0.2*Intervalo_3) %>%
    select(Nombre,variable,Pendiente_ponderada) %>%
    dcast(Nombre~variable, value.var = "Pendiente_ponderada")
  return(pend_pond)
}


calculo_limites_out <- function(x){
  # x: Base de datos
  x = ind_depurado(x)
  maximo = max(x)
  minimo = min(x)
  rango = maximo-minimo
  amplitud = rango/9
  return(t(data.frame("Maximo"=maximo,
                      "Minimo"=minimo,
                      "K" = 9,
                      "Rango"=rango,
                      "Amplitud"=amplitud,
                      "AAA"=maximo,
                      "AA"=maximo-amplitud,
                      "A"=maximo-(2*amplitud),
                      "BBB"=maximo-(3*amplitud),
                      "BB"=maximo-(4*amplitud),
                      "B"=maximo-(5*amplitud),
                      "C"=maximo-(6*amplitud),
                      "D"=maximo-(7*amplitud),
                      "E"=maximo-(8*amplitud))))
}

calculo_limites <- function(x){
  # x: Base de Datos
  maximo = max(x)
  minimo = min(x)
  rango = maximo-minimo
  amplitud = rango/9
  return(t(data.frame("Maximo"=maximo,
                      "Minimo"=minimo,
                      "K" = 9,
                      "Rango"=rango,
                      "Amplitud"=amplitud,
                      "AAA"=maximo,
                      "AA"=maximo-amplitud,
                      "A"=maximo-(2*amplitud),
                      "BBB"=maximo-(3*amplitud),
                      "BB"=maximo-(4*amplitud),
                      "B"=maximo-(5*amplitud),
                      "C"=maximo-(6*amplitud),
                      "D"=maximo-(7*amplitud),
                      "E"=maximo-(8*amplitud))))
}



generar_limites <- function(x,s="No depurar"){
  # Base de Datos 
  ifelse(s=="Depurar",
         Limites <- x %>%
           summarise_if(is.numeric,calculo_limites_out) %>%
           as.data.frame()%>%
           round(7),
         Limites <- x %>%
           summarise_if(is.numeric,calculo_limites) %>%
           as.data.frame()%>%
           round(7))
  
  
  Limites$Id <- c("Maximo",
                  "Minimo",
                  "K",
                  "Rango",
                  "Amplitud",
                  "AAA",
                  "AA",
                  "A",
                  "BBB",
                  "BB",
                  "B",
                  "C",
                  "D",
                  "E")
  Limites <- Limites %>% select(ncol(Limites),c(1:ncol(Limites)-1))
  return(Limites)
}



cal_dinamico <- function(z,x,y){
  ifelse(z<=x[y=="E"],"E",
         ifelse(z<=x[y=="D"],"D",
                ifelse(z<=x[y=="C"],"C",
                       ifelse(z<=x[y=="B"],"B",
                              ifelse(z<=x[y=="BB"],"BB",
                                     ifelse(z<=x[y=="BBB"],"BBB",
                                            ifelse(z<=x[y=="A"],"A",
                                                   ifelse(z<=x[y=="AA"],"AA","AAA"))))))))
  
}



cal_dinamico_particular <- function(x){
  ifelse(x=="AAA",9,
         ifelse(x=="AA",8,
                ifelse(x=="A",7,
                       ifelse(x=="BBB",6,
                              ifelse(x=="BB",5,
                                     ifelse(x=="B",4,
                                            ifelse(x=="C",3,
                                                   ifelse(x=="D",2,1))))))))
}


cal_dinamico_global <- function(x){
  ifelse(x<=10,"E",
         ifelse(x<=20,"D",
                ifelse(x<=30,"C",
                       ifelse(x<=40,"B",
                              ifelse(x<=50,"BB",
                                     ifelse(x<=60,"BBB",
                                            ifelse(x<=70,"A",
                                                   ifelse(x<=80,"AA","AAA"))))))))
  
}



calificar_pendientes_dinamico <- function(x,y){
  for (esim in (ncol(x)-9):ncol(x)){
    x[,esim] = cal_dinamico(x[,esim], y[,esim],y[,1])
  }
  return(x)
}


calificar_valores_dinamico <- function(x){
  for (esim in (ncol(x)-9):ncol(x)){
    x[,esim] = cal_dinamico_particular(x[,esim])
  }
  x$Total = rowSums(x[,(ncol(x)-9):ncol(x)])
  x$Calificacion = cal_dinamico_global(x$Total)
  
  return(x)
}


Mod.Dinamico <- function(Grupo=c("Bancos","Grandes"),s="No depurar", i_evaluado="2022-03-31"){
  # DF: Base de datos
  # s: Binaria Depurar o No depurar
  # N: Numero de meses para el analisis
  # i_evaluado: instante a evaluar 
  
  DF <- lectura(Grupo[1],Grupo[2]) ##leemos normalmente 
  
  colnames(DF)[2]<- "Nombre" 
  N <- DF %>% 
    filter(Fecha ==i_evaluado) %>% 
    select(Nombre) %>% t() %>% as.character() %>% unique()
  DF <- DF %>% filter(Nombre %in% N)
  
  if(Grupo[1]=="Cooperativas"){
    Aux <- DF %>% 
      rename("Nombre"=2) %>% 
      group_by(Nombre) %>%
      summarise(n=n()) %>%
      filter(n<51) %>%
      select(Nombre) %>% 
      t() %>% 
      as.character() %>% 
      unique()  
    for(i in c("S1","S2","S3")){
      if(Grupo[2]!=i){
        A<-lectura("Cooperativas",i)%>%filter(Nombre %in% Aux)
        DF<-rbind(DF,A)
      }
    }
    rm(Aux,A)
  }
  
  
  P <- DF %>% pendientes_ponderadas_2(i_evaluado)
  L <- P %>% generar_limites(s)
  cal.01 <- calificar_pendientes_dinamico(P,L)
  cal.02 <- cal.01 %>% calificar_valores_dinamico()
  cal.02 <- cal.02 %>% filter(Nombre %in% N)
  cal.02 <- cal.02 %>% mutate(Nombre = as.character(Nombre))
  print(sprintf("(Dinamico) Calificaciones emitidas a fecha: %s,Se selecciono: %s",
                i_evaluado,s))
  return(list("Puntajes"=cal.01,"Calificaciones"=cal.02,"Pendientes"=P,"Limites"=L))
}




#######################################################################
# MODELO Externa
#######################################################################

Cal.Externa <- function(x="Bancos",y="Grandes"){
  if(x=="Cooperativas"){CE <- read_excel("BDD/CALIFICACIONES EXTERNAS.xlsx",sheet = x)
  }
  else{CE<-read_excel("BDD/CALIFICACIONES EXTERNAS.xlsx", sheet=y)}
  
  return(CE)
}



#######################################################################
# MODELO FINAL
#######################################################################

puntaje_parcial <- function(x){
  ifelse(x=="AAA",9,
         ifelse(x=="AA",8,
                ifelse(x=="A",7,
                       ifelse(x=="BBB",6,
                              ifelse(x=="BB",5,
                                     ifelse(x=="B",4,
                                            ifelse(x=="C",3,
                                                   ifelse(x=="D",2,1))))))))
}



final.pd <- function(x){
  ifelse(x<=1,"E",
         ifelse(x<=2,"D",
                ifelse(x<=3,"C",
                       ifelse(x<=4,"B",
                              ifelse(x<=5,"BB",
                                     ifelse(x<=6,"BBB",
                                            ifelse(x<=7,"A",
                                                   ifelse(x<=8,"AA","AAA"))))))))
}



Mod.Final <- function(Grupo=c("Bancos","Grandes"),i_evaluado="2022-03-31", LC=c(0.2,0.4,0.6,0.8)){
  #i_evaluado Instante a Evaluar
  #punto_corte Punto de Corte
  punto_corte=floor_date(as.Date(i_evaluado),"months")-1
  ifelse(Grupo[1]=="Cooperativas",s <- "Depurar",s<-"No Depurar")
  C <- Cal.Externa(Grupo[1],Grupo[2])
  fecha_inmediata<-unique(C$Fecha[C$Fecha<i_evaluado])%>%max()###esta y la siguiente fila sirven para leer la calif externa desde la base
  C<-C%>%filter(Fecha==fecha_inmediata)%>%select(Nombre,Calificacion)
  
  A<-select(Mod.Estatico(Grupo,i_evaluado, LC)$Calificaciones, c(Nombre,Calificacion))
  B<-select(Mod.Dinamico(Grupo,s,i_evaluado)$Calificaciones, c(Nombre,Calificacion))
  CF <- left_join(left_join(A,B, by="Nombre"),C,by="Nombre")%>% 
    rename("Estatico"="Calificacion.x",
           "Dinamico"="Calificacion.y",
           "Externa"="Calificacion") %>%
    mutate("Puntaje_Estatico"=puntaje_parcial(Estatico),
           "Puntaje_Dinamico"=puntaje_parcial(Dinamico),
           "Puntaje_Externa"=puntaje_parcial(Externa))%>%
    mutate(Puntaje_Externa = replace(Puntaje_Externa,is.na(Puntaje_Externa),0),
           Puntaje_Dinamico = replace(Puntaje_Dinamico,is.na(Puntaje_Dinamico),0),
           Puntaje_Estatico = replace(Puntaje_Estatico,is.na(Puntaje_Estatico),0),
           Parcial_Estatico = 0.5*Puntaje_Estatico,
           Parcial_Dinamico = 0.35*Puntaje_Dinamico,
           Parcial_Externa = 0.15*Puntaje_Externa,
           "SUMA"=Parcial_Estatico+Parcial_Dinamico+Parcial_Externa,
           "Calificacion_Final"=final.pd(SUMA)) %>% 
    arrange(match(Calificacion_Final,c("AAA","AA","A","BBB","BB","B","C","D","E")))%>%
    select(-c(Puntaje_Estatico,Puntaje_Dinamico,Puntaje_Externa))%>%
    select(-c(Parcial_Estatico,Parcial_Dinamico,Parcial_Externa,Externa,SUMA))
  return(CF)
}

