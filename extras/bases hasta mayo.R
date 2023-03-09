###Bases hasta mayo
source("Basecalificaciones.R")


###BANCOS
bancos<-list(Grandes=c("Bancos","Grandes"),
             Medianos=c("Bancos","Medianos"),
             Pequeños=c("Bancos","Pequeños"))

bc<-createWorkbook()
addWorksheet(bc,"Grandes")
addWorksheet(bc,"Medianos")
addWorksheet(bc,"Pequeños")
hoja=1
for(i in bancos){
  writeDataTable(bc,calificacionesporgrupo(i),sheet=hoja)
  hoja<-hoja+1
  
}

saveWorkbook(bc,file="Bancos.xlsx",overwrite = T) 


###Cooperativas

coops<-createWorkbook()
addWorksheet(coops,"Cooperativas")

hoja=1
califcoops<-rbind(calificacionesporgrupo("Cooperativas","S1"),calificacionesporgrupo("Cooperativas","S2"))
  writeData(coops,califcoops,sheet=hoja)


saveWorkbook(coops,file="Cooperativas.xlsx",overwrite = T)



###Aseguradoras

aseg<-createWorkbook()
addWorksheet(aseg,"Aseguradora")
writeData(aseg,calificacionesporgrupo(c("Aseguradoras","Aseguradora")),sheet=1)
saveWorkbook(aseg,file="Aseguradoras.xlsx",overwrite = T)



