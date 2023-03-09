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
colnames(Bg)[2]<-"Modelo Original" 
Bg<-mutate(Bg,Mod.Estatico2(c("Bancos","b_Grandes"),"2022-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bg)[3]="Modelo Nuevo"
Bg<-mutate(Bg,"Comparaciones"=comparar_calificaciones(Bg$`Modelo Original`,Bg$`Modelo Nuevo`)) 

### Bancos medianos 

Bm<-Mod.Estatico(c("Bancos","b_Medianos"),"2022-02-28","2022-05-31")$Calificaciones[,c(1,13)]
colnames(Bm)[2]<-"Modelo Original" 
Bm<-mutate(Bm,Mod.Estatico2(c("Bancos","b_Medianos"),"2022-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bm)[3]="Modelo Nuevo" 
Bm<-mutate(Bm,"Comparaciones"=comparar_calificaciones(Bm$`Modelo Original`,Bm$`Modelo Nuevo`)) 



### Bancos pequeños

Bp<-Mod.Estatico(c("Bancos","b_Pequeños"),"2022-02-28","2022-05-31")$Calificaciones[,c(1,13)]
colnames(Bp)[2]<-"Modelo Original" 
Bp<-mutate(Bp,Mod.Estatico2(c("Bancos","b_Pequeños"),"2022-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bp)[3]="Modelo Nuevo" 
Bp<-mutate(Bp,"Comparaciones"=comparar_calificaciones(Bp$`Modelo Original`,Bp$`Modelo Nuevo`)) 



## Aseguradoras 

A<-Mod.Estatico("Aseguradoras","2022-02-28","2022-05-31")$Calificaciones[,c(1,13)]

colnames(A)[2]<-"Modelo Original" 
A<-mutate(A,Mod.Estatico2("Aseguradoras","2022-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(A)[3]="Modelo Nuevo" 
A<-mutate(A,"Comparaciones"=comparar_calificaciones(A$`Modelo Original`,A$`Modelo Nuevo`)) 



## Cooperativas 
### Segmento 1 

CS1<-Mod.Estatico(c("Cooperativas","S1"),"2022-02-28","2022-05-31")$Calificaciones[,c(1,13)]
colnames(CS1)[2]<-"Modelo Original" 
CS1<-mutate(CS1,Mod.Estatico2(c("Cooperativas","S1"),"2022-02-28","2022-05-31")$Calificaciones[,c(1,13)])
colnames(CS1)[3]="Modelo Nuevo" 
CS1<-mutate(CS1,"Comparaciones"=comparar_calificaciones(CS1$`Modelo Original`,CS1$`Modelo Nuevo`))  


tablascomparaciones<-list(Bg,Bm,Bp,A,CS1)
write_xlsx(tablascomparaciones,"comparaciones a mayo2022.xlsx")

