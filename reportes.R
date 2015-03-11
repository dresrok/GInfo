leerArchivo <- function(){
  suppressMessages(require(gdata));
  comuna <- read.xls("data/comuna-10.xls");
  densidadFollajeEspecifico(comuna);
}