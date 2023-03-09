Grupos<-list(bg=c("Bancos","Grandes"),bm=c("Bancos","Medianos"),bp=c("Bancos","Pequeños")
             ,cs1=c("Cooperativas","S1"),aseg=c("Aseguradoras","Aseguradora"))


Nombresmeses<-c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

Gruposfinales<-c("Bancos Grandes", "Bancos Medianos","Bancos Pequeños", "S1", "S2", "Aseguradoras")



mesporfecha<-function(x){
  #ej x="2022-04-30"
  a<-as.numeric(substring(x,6,7))
  return(Nombresmeses[a])  
  
}

calificaciones<- c("E","D","C","B","BB","BBB","A","AA","AAA")
comparaciones<-c("Empeora","Se mantiene","Mejora")

empatarsegmento<-function(x){
  y<-""
  ifelse(x=="Grandes",y<-Gruposfinales[1],
         ifelse(x=="Medianos",y<-Gruposfinales[2],
                ifelse(x=="Pequeños",y<-Gruposfinales[3],
                       ifelse(x=="S1",y<-Gruposfinales[4],
                              ifelse(x=="S2",y<-Gruposfinales[5],y<-Gruposfinales[6])))))
  return(y)
}


comparar_califcuant<-function(calif1,calif2){
  califs<-match(calif1,calificaciones)-match(calif2,calificaciones) 
  return(califs)
}   


comparar_calificaciones<-function(calif1,calif2){
  califs<-match(calif1,calificaciones)-match(calif2,calificaciones) 
  
  for(i in 1:length(califs)){
    ifelse(califs[i]==0,califs[i]<-comparaciones[2],
           ifelse(califs[i]>0,califs[i]<-comparaciones[1],califs[i]<-comparaciones[3]))
  }
  
  return(califs)
}

####################### lECTURA CALIFICACIONES MOD FINAL POR MESES Y GRUPO #############################

lecturacalifsxgrupo<-function(Grupo){
  if(Grupo[1]=="Cooperativas"){
    datos<-read_excel("base calificaciones/Cooperativas.xlsx")%>%
      group_by(Nombre)%>%mutate("Ultima fecha"=max(Fecha))%>%  #creamos una columna indicando la última fecha de evaluacion
      group_by(Nombre,`Ultima fecha`)%>%mutate("Segmento Final"=min(Segmento), .after=Segmento) %>%
      #y agrupando por nombre y la ultima fecha evaluada obtenemos el minimo del segmento pues S2>S1  
      group_by(`Segmento Final`,Nombre,Fecha)%>%summarise(`Calificación Modelo Final`)%>%
      reshape2::dcast(`Segmento Final`+ Nombre~Fecha)%>%rename("Segmento"=1)

      
      
  }
  else{
    datos<-read_excel(sprintf("base calificaciones/%s.xlsx",Grupo[1]),sheet = Grupo[2])%>%
      group_by(Nombre,Fecha)%>%summarise(`Calificación Modelo Final`)%>%
      reshape2::dcast(Nombre~Fecha)%>%mutate("Segmento"=empatarsegmento(Grupo[2]), .before=Nombre)   
    
  }
  colnames(datos)[3:6]<-mesporfecha(colnames(datos)[3:6])
  
return(datos) 
}

######### COMPARACIONES CUALITATIVAS Y CUANTITATIVAS ####### 
crear_comparaciones<-function(Grupo){
  datos<-lecturacalifsxgrupo(Grupo)
  l<-ncol(datos)
  for(i in 3:(l-1)){
    nombre<-sprintf("Comparación cualitativa %s-%s",colnames(datos)[i],colnames(datos)[i+1])
    datos<-mutate(datos, Comparacion=comparar_calificaciones(datos[[i]],datos[[i+1]]))
    colnames(datos)[ncol(datos)]=nombre 
  }
  for(i in k:(l-1)){
    nombre<-sprintf("Comparación cuantitativa %s-%s",colnames(datos)[i],colnames(datos)[i+1])
    datos<-mutate(datos, Comparacion=comparar_califcuant(datos[[i+1]],datos[[i]]))
    colnames(datos)[ncol(datos)]=nombre 
  }
  return(datos)
  
}




modelofinalajunio<-createWorkbook()
addWorksheet(modelofinalajunio,sheetName = "Hoja 1")

tablajunio<-list()
for(i in Grupos[1:6]){
tablajunio<-rbind(tablajunio,crear_comparaciones(i))
  
}

writeData(modelofinalajunio,tablajunio,sheet=1)
saveWorkbook(modelofinalajunio,"modelo final a junio.xlsx",overwrite = T) 










