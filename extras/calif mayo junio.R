Grupos<-list(bg=c("Bancos","Grandes"),bm=c("Bancos","Medianos"),bp=c("Bancos","Pequeños")
             ,cs1=c("Cooperativas","S1"),cs2=c("Cooperativas","S2"),aseg=c("Aseguradoras","Aseguradoras"))


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



tablagrupocomparacion<-function(Grupo){
  x<-Mod.Final(Grupo,i_evaluado = "2022-05-31")
  colnames(x)[2]<-"Calificacion mayo"
  y<-Mod.Final(Grupo,i_evaluado = "2022-06-30")
  colnames(y)[2]<-"Calificacion junio"
  y<-filter(y,Nombre %in% intersect(y$Nombre,x$Nombre))
  x<-merge(x,y)
  comparacion<-comparar_calificaciones(x$`Calificacion mayo`,x$`Calificacion junio`)
  x<-mutate(x,"Comparacion"=comparacion)
  
} 


mayojunio<-createWorkbook()
addWorksheet(mayojunio,"Bancos")

fila_inicio<-1
salto_fila<-3 
for(i in Grupos[1:3]){
  writeDataTable(mayojunio,tablagrupocomparacion(i),startRow = fila_inicio, sheet="Bancos")
  fila_inicio<-fila_inicio+ nrow(tablagrupocomparacion(i)) +salto_fila
}


addWorksheet(mayojunio,"Cooperativas")

fila_inicio<-1

for(i in Grupos[4:5]){
  writeDataTable(mayojunio,tablagrupocomparacion(i),startRow = fila_inicio, sheet="Cooperativas")
  fila_inicio<-fila_inicio+nrow(tablagrupocomparacion(i))+salto_fila
}

addWorksheet(mayojunio,"Aseguradoras")

fila_inicio<-1
i=Grupos$aseg
  writeDataTable(mayojunio,tablagrupocomparacion(i),startRow = fila_inicio, sheet="Aseguradoras")
  fila_inicio<-fila_inicio+nrow(tablagrupocomparacion(i))+salto_fila



saveWorkbook(mayojunio,"modelo final mayo junio3 con mod dinam.xlsx",overwrite = T)



