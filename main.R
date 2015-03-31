source.with.encoding('D:/2015/Ut/ReportesLabSIG/config.R', encoding='UTF-8')
source('D:/2015/Ut/ReportesLabSIG/helper.R');
source('D:/2015/Ut/ReportesLabSIG/reportes.R');

configurarEntorno();
main <- function(){
  comuna <- leerArchivo();
  densidadFollajeGeneral(comuna);
  densidadFollajeEspecifico(comuna);
  emplazamientoGeneral(comuna);
  emplazamientoEspecifico(comuna);
  estadoFisicoGeneral(comuna);
  estadoFisicoEspecifico(comuna);
  estadoHojaGeneral(comuna);
  estadoHojaEspecifico(comuna);
  estadoSanitarioGeneral(comuna);
  estadoSanitarioEspecifico(comuna);
}
main();