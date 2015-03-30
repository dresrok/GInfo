source.with.encoding('D:/2015/Ut/ReportesLabSIG/config.R', encoding='UTF-8')
source('D:/2015/Ut/ReportesLabSIG/helper.R');
source('D:/2015/Ut/ReportesLabSIG/reportes.R');

configurarEntorno();
main <- function(){
  comuna <- leerArchivo();
  densidadFollajeGeneral(comuna);
  densidadFollajeEspecifico(comuna);
  emplazamientoEspecifico(comuna);
  emplazamientoGeneral(comuna);
  estadoFisicoEspecifico(comuna);
  estadoFisicoGeneral(comuna);
  estadoHojaEspecifico(comuna);
  estadoSanitarioEspecifico(comuna);
  estadoSanitarioGeneral(comuna);
}
main();