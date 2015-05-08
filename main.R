source.with.encoding('~/GitHub/ReportesLabSIG/config.R', encoding='UTF-8')
source('~/GitHub/ReportesLabSIG/helper.R');
source('~/GitHub/ReportesLabSIG/informesGenerales.R');
source('~/GitHub/ReportesLabSIG/informesEspecificos.R');

configurarEntorno();
main <- function(){
  comuna <<- leerArchivo();
  general(comuna);
  densidadFollajeGeneral(comuna);
  emplazamientoGeneral(comuna);
  estadoFisicoGeneral(comuna);
  estadoHojaGeneral(comuna);
  estadoSanitarioGeneral(comuna);
  valorEsteticoGeneral(comuna);
  procedenciaGeneral(comuna);
  habitoGeneral(comuna);
  conflictoGeneral(comuna);
  alturas(comuna);
  diametros(comuna);
  volumen(comuna);
  propiedadesFisicas(comuna);
  propiedadesSanitarias(comuna);
  riesgos(comuna);


  densidadFollajeEspecifico(comuna);
  emplazamientoEspecifico(comuna);
  estadoFisicoEspecifico(comuna);
  estadoHojaEspecifico(comuna);
  estadoSanitarioEspecifico(comuna);
  valorEsteticoEspecifico(comuna);
  #especiesEspecifico(comuna);
  #procedenciaEspecifico(comuna);
  #tipoPlantaEspecifico(comuna);
  #conflictoEspecifico(comuna);

  
}
main();