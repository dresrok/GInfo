configurarEntorno <- function(){
  suppressMessages(require(gdata));
  Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre8');
  require(xlsx, quietly = TRUE);  
  configurarDominios();
}
configurarDominios <- function(){
	CHECK <<- 1;
  SUM <<- 2;

  densidad <- list(
    dominio = "densidad",
    encabezado = c("Denso", "Medio", "Ralo"),
    denso = 1,
    medio = 2,
    ralo = 3
  );

  emplazamiento <- list(
    dominio = "emplazamiento",
    encabezado = c("Parque", "Glorieta", "Andén", "Alcorque", "Separador Vial", "Ante Jardín", "Zona Blanda"),
    pr = 1,
    gl = 2,
    an = 3,
    al = 4,
    sp = 5,
    ant = 6,
    zb = 7
  );
}