source.with.encoding('C:/Users/Labsig/Documents/GitHub/ReportesLabSIG/config.R', encoding='UTF-8')
source('C:/Users/Labsig/Documents/GitHub/ReportesLabSIG/helper.R');
source('C:/Users/Labsig/Documents/GitHub/ReportesLabSIG/informesGenerales.R');
source('C:/Users/Labsig/Documents/GitHub/ReportesLabSIG/informesEspecificos.R');

configurarEntorno();
main <- function(){
  comuna <<- leerArchivo();
  #alturas(comuna);
  general(comuna);
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
  #conflictoGeneral(comuna);
  #conflictoEspecifico(comuna);
}
main();