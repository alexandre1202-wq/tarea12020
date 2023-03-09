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

Bg<-Mod.Estatico(c("Bancos","b_Grandes"),"2022-04-30","2022-05-31")$Calificaciones[,c(1,13)]
colnames(Bg)[2]<-"Modelo estático(corte mes anterior)" 
Bg<-mutate(Bg,Mod.Final(c("Bancos","b_Grandes"),f3="2022-04-30",f2="2022-05-31")$Calificacion_Final)
colnames(Bg)[3]="Modelo final(corte mes anterior)"
Bg<-mutate(Bg,Mod.Estatico(c("Bancos","b_Grandes"),f2="2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bg)[4]<-"Modelo estático(original)" 
Bg<-mutate(Bg,Mod.Final(c("Bancos","b_Grandes"),f2="2022-05-31")$Calificacion_Final)
colnames(Bg)[5]="Modelo final(original)" 
Bg<-mutate(Bg, "Comparacion modelo final" =comparar_calificaciones(Bg$`Modelo final(corte mes anterior)`,Bg$`Modelo final(original)`))

### Bancos medianos 

Bm<-Mod.Estatico(c("Bancos","b_Medianos"),"2022-04-30","2022-05-31")$Calificaciones[,c(1,13)]
colnames(Bm)[2]<-"Modelo estático(corte mes anterior)" 
Bm<-mutate(Bm,Mod.Final(c("Bancos","b_Medianos"),f3="2022-04-30",f2="2022-05-31")$Calificacion_Final)
colnames(Bm)[3]="Modelo final(corte mes anterior)"
Bm<-mutate(Bm,Mod.Estatico(c("Bancos","b_Medianos"),f2="2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bm)[4]<-"Modelo estático(original)" 
Bm<-mutate(Bm,Mod.Final(c("Bancos","b_Medianos"),f2="2022-05-31")$Calificacion_Final)
colnames(Bm)[5]="Modelo final(original)" 
Bm<-mutate(Bm, "Comparacion modelo final" =comparar_calificaciones(Bm$`Modelo final(corte mes anterior)`,Bm$`Modelo final(original)`))



### Bancos pequeños

Bp<-Mod.Estatico(c("Bancos","b_Pequeños"),"2022-04-30","2022-05-31")$Calificaciones[,c(1,13)]
colnames(Bp)[2]<-"Modelo estático(corte mes anterior)" 
Bp<-mutate(Bp,Mod.Final(c("Bancos","b_Pequeños"),f3="2022-04-30",f2="2022-05-31")$Calificacion_Final)
colnames(Bp)[3]="Modelo final(corte mes anterior)"
Bp<-mutate(Bp,Mod.Estatico(c("Bancos","b_Pequeños"),f2="2022-05-31")$Calificaciones[,c(1,13)])
colnames(Bp)[4]<-"Modelo estático(original)" 
Bp<-mutate(Bp,Mod.Final(c("Bancos","b_Pequeños"),f2="2022-05-31")$Calificacion_Final)
colnames(Bp)[5]="Modelo final(original)" 
Bp<-mutate(Bp, "Comparacion modelo final" =comparar_calificaciones(Bp$`Modelo final(corte mes anterior)`,Bp$`Modelo final(original)`))


## Aseguradoras 

A<-Mod.Estatico(c("Aseguradoras","Aseguradora"),"2022-04-30","2022-05-31")$Calificaciones[,c(1,13)]
colnames(A)[2]<-"Modelo estático(corte mes anterior)" 
A<-mutate(A,Mod.Final(c("Aseguradoras","Aseguradora"),f3="2022-04-30",f2="2022-05-31")$Calificacion_Final)
colnames(A)[3]="Modelo final(corte mes anterior)"
A<-mutate(A,Mod.Estatico(c("Aseguradoras","Aseguradora"),f2="2022-05-31")$Calificaciones[,c(1,13)])
colnames(A)[4]<-"Modelo estático(original)" 
A<-mutate(A,Mod.Final(c("Aseguradoras","Aseguradora"),f2="2022-05-31")$Calificacion_Final)
colnames(A)[5]="Modelo final(original)" 
A<-mutate(A, "Comparacion modelo final" =comparar_calificaciones(A$`Modelo final(corte mes anterior)`,A$`Modelo final(original)`))



## Cooperativas 
### Segmento 1 

CS1<-Mod.Estatico(c("Cooperativas","S1"),"2022-04-30","2022-05-31")$Calificaciones[,c(1,13)]
colnames(CS1)[2]<-"Modelo estático(corte mes anterior)" 
CS1<-mutate(CS1,Mod.Final(c("Cooperativas","S1"),f3="2022-04-30",f2="2022-05-31")$Calificacion_Final)
colnames(CS1)[3]="Modelo final(corte mes anterior)"
CS1<-mutate(CS1,Mod.Estatico(c("Cooperativas","S1"),f2="2022-05-31")$Calificaciones[,c(1,13)])
colnames(CS1)[4]<-"Modelo estático(original)" 
CS1<-mutate(CS1,Mod.Final(c("Cooperativas","S1"),f2="2022-05-31")$Calificacion_Final)
colnames(CS1)[5]="Modelo final(original)" 
CS1<-mutate(CS1, "Comparacion modelo final" =comparar_calificaciones(CS1$`Modelo final(corte mes anterior)`,CS1$`Modelo final(original)`))



## Cooperativas 
### Segmento 2 

CS2<-Mod.Estatico(c("Cooperativas","S2"),"2022-04-30","2022-05-31")$Calificaciones[,c(1,13)]
colnames(CS2)[2]<-"Modelo estático(corte mes anterior)" 
CS2<-mutate(CS2,Mod.Final(c("Cooperativas","S2"),f3="2022-04-30",f2="2022-05-31")$Calificacion_Final)
colnames(CS2)[3]="Modelo final(corte mes anterior)"
CS2<-mutate(CS2,Mod.Estatico(c("Cooperativas","S2"),f2="2022-05-31")$Calificaciones[,c(1,13)])
colnames(CS2)[4]<-"Modelo estático(original)" 
CS2<-mutate(CS2,Mod.Final(c("Cooperativas","S2"),f2="2022-05-31")$Calificacion_Final)
colnames(CS2)[5]="Modelo final(original)" 
CS2<-mutate(CS2, "Comparacion modelo final" =comparar_calificaciones(CS2$`Modelo final(corte mes anterior)`,CS2$`Modelo final(original)`))

tablascomparaciones<-list(Bg,Bm,Bp,A,CS1,CS2)
tablas<-createWorkbook()
addWorksheet(tablas,"Hoja 1")
fila_inicio<-1
salto_fila<-3 
for(i in tablascomparaciones){
  writeData(tablas,i,startRow = fila_inicio, sheet=1,)
  fila_inicio<-fila_inicio+nrow(i)+salto_fila
}

saveWorkbook(tablas,"comparacion original con corte mes anterior.xlsx",overwrite = T)

