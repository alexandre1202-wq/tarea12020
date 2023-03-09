###Bases hasta mayo
source("Basecalificaciones.R")
######----

# ###BANCOS
# bancos<-list(Grandes=c("Bancos","Grandes"),
#              Medianos=c("Bancos","Medianos"),
#              Pequeños=c("Bancos","Pequeños"))
# ###CREACION DE LA BASE DE CALIFS EMITIDAS (OMITIR SI YA ESTAN CREADAS LAS BASES Y SOLO ENFOCARSE EN LA
# #FUNCIÓN ACTUALIZAR_BASE_CALIFS)
# bc<-createWorkbook()
# addWorksheet(bc,"Grandes")
# addWorksheet(bc,"Medianos")
# addWorksheet(bc,"Pequeños")
# hoja=1
# for(i in bancos){
#   writeDataTable(bc,sheet=hoja,x=calificacionesporgrupo(i))
#   hoja<-hoja+1
# 
# }
# 
# saveWorkbook(bc,file="Calificaciones Emitidas/Bancos.xlsx",overwrite = T)
# 
# 
# ###Cooperativas
# 
# 
# coops<-createWorkbook()
# addWorksheet(coops,"Cooperativas")
# hoja=1
# 
# califcoops<-rbind(calificacionesporgrupo(c("Cooperativas","S1")),calificacionesporgrupo(c("Cooperativas","S2")))
# califcoops<-arrange(califcoops,Fecha)
# writeDataTable(coops,sheet="Cooperativas",x=califcoops)
# 
# saveWorkbook(coops,file="Calificaciones Emitidas/Cooperativas.xlsx",overwrite = T)
# 
# 
# 
# ###Aseguradoras
# 
# aseg<-createWorkbook()
# addWorksheet(aseg,"Aseguradora")
# writeDataTable(aseg,calificacionesporgrupo(c("Aseguradoras","Aseguradoras")),sheet=1)
# saveWorkbook(aseg,file="Calificaciones Emitidas/Aseguradoras.xlsx",overwrite = T)









################################# ACTUALIZAR BASES CALIFS ################
######OJO!!! ACTUALIZA PARA TODOS LOS GRUPOS FINANCIEROS ASI QUE HAY QUE ASEGURAR QUE YA ESTEN
#ACTUALIZADAS TODAS LAS BASES DE INDICADORES DE CADA GRUPO FINANCIERO, 
#PARA LA FECHA A LA QUE SE QUIERE ACTUALIZAR

actualizar_bases_califs<-function(fecha){
  #ej fecha="2022-08-31" 
  
  ##BANCOS ##
  bancos<-list(Grandes=c("Bancos","Grandes"),
               Medianos=c("Bancos","Medianos"),
               Pequeños=c("Bancos","Pequeños"))
  bc<-createWorkbook()
  addWorksheet(bc,"Grandes")
  addWorksheet(bc,"Medianos")
  addWorksheet(bc,"Pequeños")
  
  hoja=1
  for(i in bancos){
    base_existente<-read_excel("Calificaciones Emitidas/Bancos.xlsx",sheet=i[2])
    x<-rbind(base_existente,calificacionesporgrupo(i,fecha))
    writeDataTable(bc,sheet=hoja,x=x)
    hoja<-hoja+1
    
  }
  
  saveWorkbook(bc,file="Calificaciones Emitidas/Bancos.xlsx",overwrite = TRUE)
  
  
  
  ##Cooperativas
  coops<-createWorkbook()
  addWorksheet(coops,"Cooperativas")
  hoja=1 
  
  base_existente<-read_excel("Calificaciones Emitidas/Cooperativas.xlsx")
  
  x1<-rbind(base_existente,calificacionesporgrupo(c("Cooperativas","S1"),fecha))
  califcoops<-rbind(x1,calificacionesporgrupo(c("Cooperativas","S2"),fecha))

  
  califcoops<-arrange(califcoops,Fecha)
  writeDataTable(coops,sheet="Cooperativas",x=califcoops)

  saveWorkbook(coops,file="Calificaciones Emitidas/Cooperativas.xlsx",overwrite = TRUE)

  
  
  ###Aseguradoras

  aseg<-createWorkbook()
  addWorksheet(aseg,"Aseguradora")
  base_existente<-read_excel("Calificaciones Emitidas/Aseguradoras.xlsx")
  x=rbind(base_existente,calificacionesporgrupo(c("Aseguradoras","Aseguradoras"),fecha))
  writeDataTable(aseg,x=x,sheet=1)
  saveWorkbook(aseg,file="Calificaciones Emitidas/Aseguradoras.xlsx",overwrite = TRUE)
  
  
}



