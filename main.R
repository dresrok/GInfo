source.with.encoding('D:/2015/Ut/ReportesLabSIG/config.R', encoding='UTF-8')
source('D:/2015/Ut/ReportesLabSIG/helper.R');
source('D:/2015/Ut/ReportesLabSIG/informesGenerales.R');
source('D:/2015/Ut/ReportesLabSIG/informesEspecificos.R');

configurarEntorno();
main <- function(){
  comuna <<- leerArchivo();
  conflictoGeneral(comuna);
  #conflictoEspecifico(comuna);
  #general(comuna);
  #densidadFollajeGeneral(comuna);
  #densidadFollajeEspecifico(comuna);
  #emplazamientoGeneral(comuna);
  #emplazamientoEspecifico(comuna);
  #estadoFisicoGeneral(comuna);
  #estadoFisicoEspecifico(comuna);
  #estadoHojaGeneral(comuna);
  #estadoHojaEspecifico(comuna);
  #estadoSanitarioGeneral(comuna);
  #estadoSanitarioEspecifico(comuna);
  #valorEsteticoGeneral(comuna);
  #valorEsteticoEspecifico(comuna);
  #especiesEspecifico(comuna);
  #procedenciaGeneral(comuna);
  #procedenciaEspecifico(comuna);
  #tipoPlantaGeneral(comuna);
  #tipoPlantaEspecifico(comuna);
  #Conflictos, Familias
}
main();