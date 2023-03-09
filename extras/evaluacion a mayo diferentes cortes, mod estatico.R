source("Funciones.R")
source("Funciones2.R") 

calificaciones<- c("E","D","C","B","BB","BBB","A","AA","AAA")
comparaciones<-c("Empeora","Se mantiene","Mejora")
comparar_calificaciones<-function(calif1,calif2){
  califs<-match(calif1,calificaciones)-match(calif2,calificaciones) 
  
  for(i in 1:length(califs)){
    ifelse(califs[i]==0,califs[i]<-comparaciones[2],
           ifelse(califs[i]>0,califs[i]<-comparaciones[1],califs[i]<-comparaciones[3]))
  }
  
  return(califs)
}

##BANCOS  

Bg<-Mod.Estatico(c("Bancos","b_Grandes"),"2022-02-28","2022-05-31")$Calificaciones[,c(1,13)]
colnames(Bg)[2]<-"Corte feb22" 
Bg<-mutate(Bg,Mod.Estatico(c("Bancos","b_Grandes"),"2021-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bg)[3]="Corte feb 21"
Bg<-mutate(Bg,Mod.Estatico(c("Bancos","b_Grandes"),"2020-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bg)[4]="Corte feb 20"
Bg<-mutate(Bg,Mod.Estatico(c("Bancos","b_Grandes"),"2019-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bg)[5]="Corte feb 19" 

### Bancos medianos 

Bm<-Mod.Estatico(c("Bancos","b_Medianos"),"2022-02-28","2022-05-31")$Calificaciones[,c(1,13)]
colnames(Bm)[2]<-"Corte feb22" 
Bm<-mutate(Bm,Mod.Estatico(c("Bancos","b_Medianos"),"2021-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bm)[3]="Corte feb 21"
Bm<-mutate(Bm,Mod.Estatico(c("Bancos","b_Medianos"),"2020-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bm)[4]="Corte feb 20"
Bm<-mutate(Bm,Mod.Estatico(c("Bancos","b_Medianos"),"2019-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bm)[5]="Corte feb 19"



### Bancos pequeños

Bp<-Mod.Estatico(c("Bancos","b_Pequeños"),"2022-02-28","2022-05-31")$Calificaciones[,c(1,13)]
colnames(Bp)[2]<-"Corte feb22" 
Bp<-mutate(Bp,Mod.Estatico(c("Bancos","b_Pequeños"),"2021-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bp)[3]="Corte feb 21"
Bp<-mutate(Bp,Mod.Estatico(c("Bancos","b_Pequeños"),"2020-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bp)[4]="Corte feb 20"
Bp<-mutate(Bp,Mod.Estatico(c("Bancos","b_Pequeños"),"2019-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bp)[5]="Corte feb 19"


## Aseguradoras 

A<-Mod.Estatico("Aseguradoras","2022-02-28","2022-05-31")$Calificaciones[,c(1,13)]

colnames(A)[2]<-"Corte feb22" 
A<-mutate(A,Mod.Estatico(c("Aseguradoras"),"2021-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(A)[3]="Corte feb 21"
A<-mutate(A,Mod.Estatico(c("Aseguradoras"),"2020-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(A)[4]="Corte feb 20"
A<-mutate(A,Mod.Estatico(c("Aseguradoras"),"2019-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(A)[5]="Corte feb 19"



## Cooperativas 
### Segmento 1 

CS1<-Mod.Estatico(c("Cooperativas","S1"),"2022-02-28","2022-05-31")$Calificaciones[,c(1,13)]
colnames(CS1)[2]<-"Corte feb22" 
CS1<-mutate(CS1,Mod.Estatico(c("Cooperativas","S1"),"2021-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(CS1)[3]="Corte feb 21"
CS1<-mutate(CS1,Mod.Estatico(c("Cooperativas","S1"),"2020-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(CS1)[4]="Corte feb 20"
CS1<-mutate(CS1,Mod.Estatico(c("Cooperativas","S1"),"2019-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(CS1)[5]="Corte feb 19"

#tablascomparaciones<-list(Bg,Bm,Bp,A,CS1)
#write_xlsx(tablascomparaciones,"comparaciones a mayo2022.xlsx")

