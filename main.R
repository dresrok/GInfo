source('D:/2015/Ut/ReportesLabSIG/config.R');
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
  estadoSanitarioEspecifico(comuna);
}
main();