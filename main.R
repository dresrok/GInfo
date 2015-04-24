source.with.encoding('C:/Users/Labsig/Documents/GitHub/ReportesLabSIG/config.R', encoding='UTF-8')
source('C:/Users/Labsig/Documents/GitHub/ReportesLabSIG/helper.R');
source('C:/Users/Labsig/Documents/GitHub/ReportesLabSIG/informesGenerales.R');
source('C:/Users/Labsig/Documents/GitHub/ReportesLabSIG/informesEspecificos.R');

configurarEntorno();
main <- function(){
  comuna <<- leerArchivo();

  #general(comuna);
  #densidadFollajeGeneral(comuna);
  #emplazamientoGeneral(comuna);
  #estadoFisicoGeneral(comuna);
  #estadoHojaGeneral(comuna);
  #estadoSanitarioGeneral(comuna);
  #valorEsteticoGeneral(comuna);
  #procedenciaGeneral(comuna);
  #habitoGeneral(comuna);
  #conflictoGeneral(comuna);
  #alturas(comuna);
  #diametros(comuna);
  #volumen(comuna);
  #propiedadesFisicas(comuna);
  #propiedadesSanitarias(comuna);
  riesgos(comuna);


  #densidadFollajeEspecifico(comuna);
  #emplazamientoEspecifico(comuna);
  #estadoFisicoEspecifico(comuna);
  
  #estadoHojaEspecifico(comuna);
  #estadoSanitarioEspecifico(comuna);
  #valorEsteticoEspecifico(comuna);
  #especiesEspecifico(comuna);
  #procedenciaEspecifico(comuna);
  #tipoPlantaEspecifico(comuna);
  #conflictoGeneral(comuna);
  #conflictoEspecifico(comuna);

  
}
main();