source("Funciones.R")




calificaciones<-function(i_evaluado,Grupo){
 calif<-Mod.Final(Grupo,i_evaluado) 
 colnames(calif)[2:4]<-c("Calificación Modelo estático","Calificación Modelo Dinamico","Calificación Modelo Final")
  calif<-calif%>%mutate(Fecha=i_evaluado, .before=Nombre) 
  if(Grupo[1]=="Cooperativas"){ calif<-mutate(calif, Segmento=Grupo[2], .before = Fecha)} 
  return(calif)
}


calificacionesporgrupo<-function(Grupo,fechas=c("2022-03-31","2022-04-30","2022-05-31","2022-06-30","2022-07-31")){
  califxgrupo<-list()
  for(i in fechas){
    califxgrupo<-rbind(califxgrupo, calificaciones(i,Grupo))
    
  }
  return(califxgrupo)
}

