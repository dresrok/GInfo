configurarEntorno <- function(){
  suppressMessages(require(gdata));
  Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre8');
  require(xlsx, quietly = TRUE);  
  configurarDominios();
}
configurarDominios <- function(){
	CHECK <<- 1;
  SUM <<- 2;

  densidad <<- "densidad";
  densidad.denso <<- 1;
  densidad.medio <<- 2
  densidad.ralo <<- 3;

	emplazamiento <<- "emplazamiento";
	emplazamiento.pr <<- 1;
	emplazamiento.gl <<- 2;
	emplazamiento.an <<- 3;
	emplazamiento.al <<- 4;
	emplazamiento.sp <<- 5;
	emplazamiento.ant <<- 6;
	emplazamiento.zb <<- 7;
}