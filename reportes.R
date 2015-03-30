densidadFollajeEspecifico <- function(comuna){
  tmpFollajeComuna <- as.data.frame.matrix(
    table(comuna$barrio, comuna$densidad_follaje)
  );
  follajeComuna <- data.frame(
    barrio = rownames(tmpFollajeComuna),
    denso = dominio(tmpFollajeComuna, densidad$dominio, densidad$denso, CHECK),
    xd = dominio(tmpFollajeComuna, densidad$dominio, densidad$denso, SUM),
    medio = dominio(tmpFollajeComuna, densidad$dominio, densidad$medio, CHECK),
    xm = dominio(tmpFollajeComuna, densidad$dominio, densidad$medio, SUM),
    ralo = dominio(tmpFollajeComuna, densidad$dominio, densidad$ralo, CHECK),
    xr = dominio(tmpFollajeComuna, densidad$dominio, densidad$ralo, SUM)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  tmpFollajeBarrios <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$densidad_follaje)
  );
  follajeBarrios <- data.frame(
    barrio = rownames(tmpFollajeBarrios),
    denso = dominio(tmpFollajeBarrios, densidad$dominio, densidad$denso, CHECK),
    xd = dominio(tmpFollajeBarrios, densidad$dominio, densidad$denso, SUM),
    medio = dominio(tmpFollajeBarrios, densidad$dominio, densidad$medio, CHECK),
    xm = dominio(tmpFollajeBarrios, densidad$dominio, densidad$medio, SUM),
    ralo = dominio(tmpFollajeBarrios, densidad$dominio, densidad$ralo, CHECK),
    xr = dominio(tmpFollajeBarrios, densidad$dominio, densidad$ralo, SUM)
  );  
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  tmpFollajeCorredores <- as.data.frame.matrix(
    table(factor(corredores$barrio), corredores$densidad_follaje)
  );
  follajeCorredores <- data.frame(
    barrio = rownames(tmpFollajeCorredores),
    denso = dominio(tmpFollajeCorredores, densidad$dominio, densidad$denso, CHECK),
    xd = dominio(tmpFollajeCorredores, densidad$dominio, densidad$denso, SUM),
    medio = dominio(tmpFollajeCorredores, densidad$dominio, densidad$medio, CHECK),
    xm = dominio(tmpFollajeCorredores, densidad$dominio, densidad$medio, SUM),
    ralo = dominio(tmpFollajeCorredores, densidad$dominio, densidad$ralo, CHECK),
    xr = dominio(tmpFollajeCorredores, densidad$dominio, densidad$ralo, SUM)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpFollajeInstituciones <- as.data.frame.matrix(
    table(factor(instituciones$institucion), instituciones$densidad_follaje)
  );
  follajeInstituciones <- data.frame(
    barrio = rownames(tmpFollajeInstituciones),
    denso = dominio(tmpFollajeInstituciones, densidad$dominio, densidad$denso, CHECK),
    xd = dominio(tmpFollajeInstituciones, densidad$dominio, densidad$denso, SUM),
    medio = dominio(tmpFollajeInstituciones, densidad$dominio, densidad$medio, CHECK),
    xm = dominio(tmpFollajeInstituciones, densidad$dominio, densidad$medio, SUM),
    ralo = dominio(tmpFollajeInstituciones, densidad$dominio, densidad$ralo, CHECK),
    xr = dominio(tmpFollajeInstituciones, densidad$dominio, densidad$ralo, SUM)
  );
  save.xlsx(densidad$informeEspecifico, follajeComuna, follajeBarrios, follajeCorredores, follajeInstituciones);
}
densidadFollajeGeneral <- function(comuna){
  tmpFollajeComuna <- as.data.frame(
    table(comuna$densidad_follaje)
  );
  follajeComuna <- data.frame(
    densidad = encabezado(densidad$encabezado, tmpFollajeComuna$Var1),
    arboles = tmpFollajeComuna$Freq,
    xd = round(tmpFollajeComuna$Freq/sum(tmpFollajeComuna$Freq), 4)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  tmpFollajeBarrios <- as.data.frame(
    table(barrios$densidad_follaje)
  );
  follajeBarrios <- data.frame(
    densidad = encabezado(densidad$encabezado, tmpFollajeBarrios$Var1),
    arboles = tmpFollajeBarrios$Freq,
    xd = round(tmpFollajeBarrios$Freq/sum(tmpFollajeBarrios$Freq), 4)
  );
  tmpTop <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$densidad_follaje)
  );
  top <- data.frame(
    barrios = rownames(tmpTop),
    denso = dominio(tmpTop, densidad$dominio, densidad$denso, CHECK),
    medio = dominio(tmpTop, densidad$dominio, densidad$medio, CHECK),
    ralo = dominio(tmpTop, densidad$dominio, densidad$ralo, CHECK),
    stringsAsFactors=FALSE
  );
  topBarrios <- data.frame(
    barriosDenso = head(top[ order(-top$denso), 1], 10),
    arbolesDenso = head(top[ order(-top$denso), 2], 10),
    barriosMedio = head(top[ order(-top$medio), 1], 10),
    arbolesMedio = head(top[ order(-top$medio), 3], 10),
    barriosRalo = head(top[ order(-top$ralo), 1], 10),
    arbolesRalo = head(top[ order(-top$ralo), 4], 10),
    stringsAsFactors=FALSE
  );
  topBarrios <- rbind(topBarrios, "---");
  bottomBarrios <- data.frame(
    barriosDenso = head(top[ order(top$denso), 1], 5),
    arbolesDenso = head(top[ order(top$denso), 2], 5),
    barriosMedio = head(top[ order(top$medio), 1], 5),
    arbolesMedio = head(top[ order(top$medio), 3], 5),
    barriosRalo = head(top[ order(top$ralo), 1], 5),
    arbolesRalo = head(top[ order(top$ralo), 4], 5)
  );
  topBarrios <- rbind(topBarrios, bottomBarrios);
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  tmpFollajeCorredores <- as.data.frame(
    table(corredores$densidad_follaje)
  );
  follajeCorredores <- data.frame(
    densidad = encabezado(densidad$encabezado, tmpFollajeCorredores$Var1),
    arboles = tmpFollajeCorredores$Freq,
    xd = round(tmpFollajeCorredores$Freq/sum(tmpFollajeCorredores$Freq), 4)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpFollajeInstituciones <- as.data.frame(
    table(instituciones$densidad_follaje)
  );
  follajeInstituciones <- data.frame(
    densidad = encabezado(densidad$encabezado, tmpFollajeInstituciones$Var1),
    arboles = tmpFollajeInstituciones$Freq,
    xd = round(tmpFollajeInstituciones$Freq/sum(tmpFollajeInstituciones$Freq), 4)
  );
  save.xlsx(densidad$informeGeneral, follajeComuna, follajeBarrios, topBarrios, follajeCorredores, follajeInstituciones);
}
emplazamientoEspecifico <- function(comuna){
  tmpEmplazamientoComuna <- as.data.frame.matrix(
    table(comuna$barrio, comuna$emplazamiento)
  );
  emplazamientoComuna <- data.frame(
    barrio = rownames(tmpEmplazamientoComuna),
    parque = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$pr, CHECK),
    xpr = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$pr, SUM),
    glorieta = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$gl, CHECK),
    xgl = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$gl, SUM),
    anden = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$an, CHECK),
    xan = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$an, SUM),
    alcorque = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$al, CHECK),
    xal = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$al, SUM),
    separador = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$sp, CHECK),
    xsp = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$sp, SUM),
    antejardin = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$ant, CHECK),
    xant = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$ant, SUM),
    zonablanda = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$zb, CHECK),
    xzb = dominio(tmpEmplazamientoComuna, emplazamiento$dominio, emplazamiento$zb, SUM)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  tmpEmplazamientoBarrios <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$emplazamiento)
  );
  emplazamientoBarrios <- data.frame(
    barrio = rownames(tmpEmplazamientoBarrios),
    parque = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$pr, CHECK),
    xpr = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$pr, SUM),
    glorieta = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$gl, CHECK),
    xgl = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$gl, SUM),
    anden = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$an, CHECK),
    xan = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$an, SUM),
    alcorque = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$al, CHECK),
    xal = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$al, SUM),
    separador = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$sp, CHECK),
    xsp = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$sp, SUM),
    antejardin = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$ant, CHECK),
    xant = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$ant, SUM),
    zonablanda = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$zb, CHECK),
    xzb = dominio(tmpEmplazamientoBarrios, emplazamiento$dominio, emplazamiento$zb, SUM)
  );
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  tmpEmplazamientoCorredores <- as.data.frame.matrix(
    table(factor(corredores$barrio), corredores$emplazamiento)
  );
  emplazamientoCorredores <- data.frame(
    barrio = rownames(tmpEmplazamientoCorredores),
    parque = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$pr, CHECK),
    xpr = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$pr, SUM),
    glorieta = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$gl, CHECK),
    xgl = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$gl, SUM),
    anden = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$an, CHECK),
    xan = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$an, SUM),
    alcorque = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$al, CHECK),
    xal = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$al, SUM),
    separador = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$sp, CHECK),
    xsp = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$sp, SUM),
    antejardin = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$ant, CHECK),
    xant = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$ant, SUM),
    zonablanda = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$zb, CHECK),
    xzb = dominio(tmpEmplazamientoCorredores, emplazamiento$dominio, emplazamiento$zb, SUM)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpEmplazamientoInstituciones <- as.data.frame.matrix(
    table(factor(instituciones$institucion), instituciones$emplazamiento)
  );
  emplazamientoInstituciones <- data.frame(
    barrio = rownames(tmpEmplazamientoInstituciones),
    parque = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$pr, CHECK),
    xpr = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$pr, SUM),
    glorieta = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$gl, CHECK),
    xgl = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$gl, SUM),
    anden = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$an, CHECK),
    xan = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$an, SUM),
    alcorque = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$al, CHECK),
    xal = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$al, SUM),
    separador = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$sp, CHECK),
    xsp = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$sp, SUM),
    antejardin = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$ant, CHECK),
    xant = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$ant, SUM),
    zonablanda = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$zb, CHECK),
    xzb = dominio(tmpEmplazamientoInstituciones, emplazamiento$dominio, emplazamiento$zb, SUM)
  );
  save.xlsx(emplazamiento$informeEspecifico, emplazamientoComuna, emplazamientoBarrios, emplazamientoCorredores, emplazamientoInstituciones);
}
emplazamientoGeneral <- function(comuna){
  tmpEmplazamientoComuna <- as.data.frame(
    table(comuna$emplazamiento)
  );
  emplazamientoComuna <- data.frame(
    emplazamiento = encabezado(emplazamiento$encabezado, tmpEmplazamientoComuna$Var1),
    arboles = tmpEmplazamientoComuna$Freq,
    xe = round(tmpEmplazamientoComuna$Freq/sum(tmpEmplazamientoComuna$Freq), 4)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  tmpEmplazamientoBarrios <- as.data.frame(
    table(barrios$emplazamiento)
  );
  emplazamientoBarrios <- data.frame(
    densidad = encabezado(emplazamiento$encabezado, tmpEmplazamientoBarrios$Var1),
    arboles = tmpEmplazamientoBarrios$Freq,
    xe = round(tmpEmplazamientoBarrios$Freq/sum(tmpEmplazamientoBarrios$Freq), 4)
  );
  tmpTop <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$emplazamiento)
  );
  top <- data.frame(
    barrios = rownames(tmpTop),
    parque = dominio(tmpTop, emplazamiento$dominio, emplazamiento$pr, CHECK),
    glorieta = dominio(tmpTop, emplazamiento$dominio, emplazamiento$gl, CHECK),
    anden = dominio(tmpTop, emplazamiento$dominio, emplazamiento$an, CHECK),
    alcorque = dominio(tmpTop, emplazamiento$dominio, emplazamiento$al, CHECK),
    separador = dominio(tmpTop, emplazamiento$dominio, emplazamiento$sp, CHECK),
    antejardin = dominio(tmpTop, emplazamiento$dominio, emplazamiento$ant, CHECK),
    zonablanda = dominio(tmpTop, emplazamiento$dominio, emplazamiento$zb, CHECK),
    stringsAsFactors=FALSE
  );
  topBarrios <- data.frame(
    barriosParque = head(top[ order(-top$parque), 1], 10),
    arbolesParque = head(top[ order(-top$parque), 2], 10),
    barriosGlorieta = head(top[ order(-top$glorieta), 1], 10),
    arbolesGlorieta = head(top[ order(-top$glorieta), 3], 10),
    barriosAnden = head(top[ order(-top$anden), 1], 10),
    arbolesAnden = head(top[ order(-top$anden), 4], 10),
    barriosAlcorque = head(top[ order(-top$alcorque), 1], 10),
    arbolesAlcorque = head(top[ order(-top$alcorque), 5], 10),
    barriosSeparador = head(top[ order(-top$separador), 1], 10),
    arbolesSeparador = head(top[ order(-top$separador), 6], 10),
    barriosAntejardin = head(top[ order(-top$antejardin), 1], 10),
    arbolesAntejardin = head(top[ order(-top$antejardin), 7], 10),
    barriosZonaBlanda = head(top[ order(-top$zonablanda), 1], 10),
    arbolesZonaBlanda = head(top[ order(-top$zonablanda), 8], 10),
    stringsAsFactors=FALSE
  );
  topBarrios <- rbind(topBarrios, "---");
  bottomBarrios <- data.frame(
    barriosParque = head(top[ order(top$parque), 1], 5),
    arbolesParque = head(top[ order(top$parque), 2], 5),
    barriosGlorieta = head(top[ order(top$glorieta), 1], 5),
    arbolesGlorieta = head(top[ order(top$glorieta), 3], 5),
    barriosAnden = head(top[ order(top$anden), 1], 5),
    arbolesAnden = head(top[ order(top$anden), 4], 5),
    barriosAlcorque = head(top[ order(top$alcorque), 1], 5),
    arbolesAlcorque = head(top[ order(top$alcorque), 5], 5),
    barriosSeparador = head(top[ order(top$separador), 1], 5),
    arbolesSeparador = head(top[ order(top$separador), 6], 5),
    barriosAntejardin = head(top[ order(top$separador), 1], 5),
    arbolesAntejardin = head(top[ order(top$antejardin), 7], 5),
    barriosZonaBlanda = head(top[ order(top$antejardin), 1], 5),
    arbolesZonaBlanda = head(top[ order(top$antejardin), 8], 5)
  );
  topBarrios <- rbind(topBarrios, bottomBarrios);
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  tmpEmplazamientoCorredores <- as.data.frame(
    table(corredores$emplazamiento)
  );
  emplazamientoCorredores <- data.frame(
    emplazamiento = encabezado(emplazamiento$encabezado, tmpEmplazamientoCorredores$Var1),
    arboles = tmpEmplazamientoCorredores$Freq,
    xe = round(tmpEmplazamientoCorredores$Freq/sum(tmpEmplazamientoCorredores$Freq), 4)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpEmplazamientoInstituciones <- as.data.frame(
    table(instituciones$emplazamiento)
  );
  emplazamientoInstituciones <- data.frame(
    emplazamiento = encabezado(emplazamiento$encabezado, tmpEmplazamientoCorredores$Var1),
    arboles = tmpEmplazamientoInstituciones$Freq,
    xe = round(tmpEmplazamientoInstituciones$Freq/sum(tmpEmplazamientoInstituciones$Freq), 4)
  );
  save.xlsx(emplazamiento$informeGeneral, emplazamientoComuna, emplazamientoBarrios, topBarrios, emplazamientoCorredores, emplazamientoInstituciones);
}
estadoFisicoEspecifico <- function(comuna){
  tmpEstadoFisicoComuna <- as.data.frame.matrix(
    table(comuna$barrio, comuna$estado_fisico)
  );
  estadoFisicoComuna <- data.frame(
    barrio = rownames(tmpEstadoFisicoComuna),
    malo = dominio(tmpEstadoFisicoComuna, estadoFisico$dominio, estadoFisico$malo, CHECK),
    xm = dominio(tmpEstadoFisicoComuna, estadoFisico$dominio, estadoFisico$malo, SUM),
    regular = dominio(tmpEstadoFisicoComuna, estadoFisico$dominio, estadoFisico$regular, CHECK),
    xr = dominio(tmpEstadoFisicoComuna, estadoFisico$dominio, estadoFisico$regular, SUM),
    bueno = dominio(tmpEstadoFisicoComuna, estadoFisico$dominio, estadoFisico$bueno, CHECK),
    xb = dominio(tmpEstadoFisicoComuna, estadoFisico$dominio, estadoFisico$bueno, SUM)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  tmpEstadoFisicoBarrios <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$estado_fisico)
  );
  estadoFisicoBarrios <- data.frame(
    barrio = rownames(tmpEstadoFisicoBarrios),
    malo = dominio(tmpEstadoFisicoBarrios, estadoFisico$dominio, estadoFisico$malo, CHECK),
    xm = dominio(tmpEstadoFisicoBarrios, estadoFisico$dominio, estadoFisico$malo, SUM),
    regular = dominio(tmpEstadoFisicoBarrios, estadoFisico$dominio, estadoFisico$regular, CHECK),
    xr = dominio(tmpEstadoFisicoBarrios, estadoFisico$dominio, estadoFisico$regular, SUM),
    bueno = dominio(tmpEstadoFisicoBarrios, estadoFisico$dominio, estadoFisico$bueno, CHECK),
    xb = dominio(tmpEstadoFisicoBarrios, estadoFisico$dominio, estadoFisico$bueno, SUM)
  );  
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  tmpEstadoFisicoCorredores <- as.data.frame.matrix(
    table(factor(corredores$barrio), corredores$estado_fisico)
  );
  estadoFisicoCorredores <- data.frame(
    barrio = rownames(tmpEstadoFisicoCorredores),
    malo = dominio(tmpEstadoFisicoCorredores, estadoFisico$dominio, estadoFisico$malo, CHECK),
    xm = dominio(tmpEstadoFisicoCorredores, estadoFisico$dominio, estadoFisico$malo, SUM),
    regular = dominio(tmpEstadoFisicoCorredores, estadoFisico$dominio, estadoFisico$regular, CHECK),
    xr = dominio(tmpEstadoFisicoCorredores, estadoFisico$dominio, estadoFisico$regular, SUM),
    bueno = dominio(tmpEstadoFisicoCorredores, estadoFisico$dominio, estadoFisico$bueno, CHECK),
    xb = dominio(tmpEstadoFisicoCorredores, estadoFisico$dominio, estadoFisico$bueno, SUM)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpEstadoFisicoInstituciones <- as.data.frame.matrix(
    table(factor(instituciones$institucion), instituciones$estado_fisico)
  );
  estadoFisicoInstituciones <- data.frame(
    barrio = rownames(tmpEstadoFisicoInstituciones),
    malo = dominio(tmpEstadoFisicoInstituciones, estadoFisico$dominio, estadoFisico$malo, CHECK),
    xm = dominio(tmpEstadoFisicoInstituciones, estadoFisico$dominio, estadoFisico$malo, SUM),
    regular = dominio(tmpEstadoFisicoInstituciones, estadoFisico$dominio, estadoFisico$regular, CHECK),
    xr = dominio(tmpEstadoFisicoInstituciones, estadoFisico$dominio, estadoFisico$regular, SUM),
    bueno = dominio(tmpEstadoFisicoInstituciones, estadoFisico$dominio, estadoFisico$bueno, CHECK),
    xb = dominio(tmpEstadoFisicoInstituciones, estadoFisico$dominio, estadoFisico$bueno, SUM)
  );
  save.xlsx(estadoFisico$informeEspecifico, estadoFisicoComuna, estadoFisicoBarrios, estadoFisicoCorredores, estadoFisicoInstituciones);
}
estadoFisicoGeneral <- function(comuna){
  tmpEstadoFisicoComuna <- as.data.frame(
    table(comuna$estado_fisico)
  );
  estadoFisicoComuna <- data.frame(
    estadoFisico = encabezado(estadoFisico$encabezado, tmpEstadoFisicoComuna$Var1),
    arboles = tmpEstadoFisicoComuna$Freq,
    xef = round(tmpEstadoFisicoComuna$Freq/sum(tmpEstadoFisicoComuna$Freq), 4)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  tmpEstadoFisicoBarrios <- as.data.frame(
    table(barrios$estado_fisico)
  );
  estadoFisicoBarrios <- data.frame(
    estadoFisico = encabezado(estadoFisico$encabezado, tmpEstadoFisicoBarrios$Var1),
    arboles = tmpEstadoFisicoBarrios$Freq,
    xef = round(tmpEstadoFisicoBarrios$Freq/sum(tmpEstadoFisicoBarrios$Freq), 4)
  );
  tmpTop <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$estado_fisico)
  );
  top <- data.frame(
    barrios = rownames(tmpTop),
    malo = dominio(tmpTop, estadoFisico$dominio, estadoFisico$malo, CHECK),    
    regular = dominio(tmpTop, estadoFisico$dominio, estadoFisico$regular, CHECK),    
    bueno = dominio(tmpTop, estadoFisico$dominio, estadoFisico$bueno, CHECK),
    stringsAsFactors=FALSE
  );
  topBarrios <- data.frame(
    barriosMalo = head(top[ order(-top$malo), 1], 10),
    arbolesMalo = head(top[ order(-top$malo), 2], 10),
    barriosRegular = head(top[ order(-top$regular), 1], 10),
    arbolesRegular = head(top[ order(-top$regular), 3], 10),    
    barriosBueno = head(top[ order(-top$bueno), 1], 10),
    arbolesBueno = head(top[ order(-top$bueno), 4], 10),
    stringsAsFactors=FALSE
  );
  topBarrios <- rbind(topBarrios, "---");
  bottomBarrios <- data.frame(
    barriosMalo = head(top[ order(top$malo), 1], 5),
    arbolesMalo = head(top[ order(top$malo), 2], 5),
    barriosRegular = head(top[ order(top$regular), 1], 5),
    arbolesRegular = head(top[ order(top$regular), 3], 5),
    barriosBueno = head(top[ order(top$bueno), 1], 5),
    arbolesBueno = head(top[ order(top$bueno), 4], 5)
  );
  topBarrios <- rbind(topBarrios, bottomBarrios);
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  tmpEstadoFisicoCorredores <- as.data.frame(
    table(corredores$estado_fisico)
  );
  estadoFisicoCorredores <- data.frame(
    estadoFisico = encabezado(estadoFisico$encabezado, tmpEstadoFisicoCorredores$Var1),
    arboles = tmpEstadoFisicoCorredores$Freq,
    xef = round(tmpEstadoFisicoCorredores$Freq/sum(tmpEstadoFisicoCorredores$Freq), 4)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpEstadoFisicoInstituciones <- as.data.frame(
    table(instituciones$estado_fisico)
  );
  estadoFisicoInstituciones <- data.frame(
    estadoFisico = encabezado(estadoFisico$encabezado, tmpEstadoFisicoInstituciones$Var1),
    arboles = tmpEstadoFisicoInstituciones$Freq,
    xef = round(tmpEstadoFisicoInstituciones$Freq/sum(tmpEstadoFisicoInstituciones$Freq), 4)
  );
  save.xlsx(estadoFisico$informeGeneral, estadoFisicoComuna, estadoFisicoBarrios, topBarrios, estadoFisicoCorredores, estadoFisicoInstituciones);
}
estadoSanitarioEspecifico <- function(comuna){
  tmpEstadoSanitarioComuna <- as.data.frame.matrix(
    table(comuna$barrio, comuna$estado_sanitario)
  );
  estadoSanitarioComuna <- data.frame(
    barrio = rownames(tmpEstadoSanitarioComuna),
    muerto = dominio(tmpEstadoSanitarioComuna, estadoSanitario$dominio, estadoSanitario$muerto, CHECK),
    xm = dominio(tmpEstadoSanitarioComuna, estadoSanitario$dominio, estadoSanitario$muerto, SUM),
    critico = dominio(tmpEstadoSanitarioComuna, estadoSanitario$dominio, estadoSanitario$critico, CHECK),
    xc = dominio(tmpEstadoSanitarioComuna, estadoSanitario$dominio, estadoSanitario$critico, SUM),
    enfermo = dominio(tmpEstadoSanitarioComuna, estadoSanitario$dominio, estadoSanitario$enfermo, CHECK),
    xe = dominio(tmpEstadoSanitarioComuna, estadoSanitario$dominio, estadoSanitario$enfermo, SUM),
    sano = dominio(tmpEstadoSanitarioComuna, estadoSanitario$dominio, estadoSanitario$sano, CHECK),
    xs = dominio(tmpEstadoSanitarioComuna, estadoSanitario$dominio, estadoSanitario$sano, SUM)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  tmpEstadoSanitarioBarrios <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$estado_sanitario)
  );
  estadoSanitarioBarrios <- data.frame(
    barrio = rownames(tmpEstadoSanitarioBarrios),
    muerto = dominio(tmpEstadoSanitarioBarrios, estadoSanitario$dominio, estadoSanitario$muerto, CHECK),
    xm = dominio(tmpEstadoSanitarioBarrios, estadoSanitario$dominio, estadoSanitario$muerto, SUM),
    critico = dominio(tmpEstadoSanitarioBarrios, estadoSanitario$dominio, estadoSanitario$critico, CHECK),
    xc = dominio(tmpEstadoSanitarioBarrios, estadoSanitario$dominio, estadoSanitario$critico, SUM),
    enfermo = dominio(tmpEstadoSanitarioBarrios, estadoSanitario$dominio, estadoSanitario$enfermo, CHECK),
    xe = dominio(tmpEstadoSanitarioBarrios, estadoSanitario$dominio, estadoSanitario$enfermo, SUM),
    sano = dominio(tmpEstadoSanitarioBarrios, estadoSanitario$dominio, estadoSanitario$sano, CHECK),
    xs = dominio(tmpEstadoSanitarioBarrios, estadoSanitario$dominio, estadoSanitario$sano, SUM)
  );  
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  tmpEstadoSanitarioCorredores <- as.data.frame.matrix(
    table(factor(corredores$barrio), corredores$estado_sanitario)
  );
  estadoSanitarioCorredores <- data.frame(
    barrio = rownames(tmpEstadoSanitarioCorredores),
    muerto = dominio(tmpEstadoSanitarioCorredores, estadoSanitario$dominio, estadoSanitario$muerto, CHECK),
    xm = dominio(tmpEstadoSanitarioCorredores, estadoSanitario$dominio, estadoSanitario$muerto, SUM),
    critico = dominio(tmpEstadoSanitarioCorredores, estadoSanitario$dominio, estadoSanitario$critico, CHECK),
    xc = dominio(tmpEstadoSanitarioCorredores, estadoSanitario$dominio, estadoSanitario$critico, SUM),
    enfermo = dominio(tmpEstadoSanitarioCorredores, estadoSanitario$dominio, estadoSanitario$enfermo, CHECK),
    xe = dominio(tmpEstadoSanitarioCorredores, estadoSanitario$dominio, estadoSanitario$enfermo, SUM),
    sano = dominio(tmpEstadoSanitarioCorredores, estadoSanitario$dominio, estadoSanitario$sano, CHECK),
    xs = dominio(tmpEstadoSanitarioCorredores, estadoSanitario$dominio, estadoSanitario$sano, SUM)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpEstadoSanitarioInstituciones <- as.data.frame.matrix(
    table(factor(instituciones$institucion), instituciones$estado_sanitario)
  );
  estadoSanitarioInstituciones <- data.frame(
    barrio = rownames(tmpEstadoSanitarioInstituciones),
    muerto = dominio(tmpEstadoSanitarioInstituciones, estadoSanitario$dominio, estadoSanitario$muerto, CHECK),
    xm = dominio(tmpEstadoSanitarioInstituciones, estadoSanitario$dominio, estadoSanitario$muerto, SUM),
    critico = dominio(tmpEstadoSanitarioInstituciones, estadoSanitario$dominio, estadoSanitario$critico, CHECK),
    xc = dominio(tmpEstadoSanitarioInstituciones, estadoSanitario$dominio, estadoSanitario$critico, SUM),
    enfermo = dominio(tmpEstadoSanitarioInstituciones, estadoSanitario$dominio, estadoSanitario$enfermo, CHECK),
    xe = dominio(tmpEstadoSanitarioInstituciones, estadoSanitario$dominio, estadoSanitario$enfermo, SUM),
    sano = dominio(tmpEstadoSanitarioInstituciones, estadoSanitario$dominio, estadoSanitario$sano, CHECK),
    xs = dominio(tmpEstadoSanitarioInstituciones, estadoSanitario$dominio, estadoSanitario$sano, SUM)
  );
  save.xlsx(estadoSanitario$informeEspecifico, estadoSanitarioComuna, estadoSanitarioBarrios, estadoSanitarioCorredores, estadoSanitarioInstituciones);
}
estadoSanitarioGeneral <- function(comuna){
  tmpEstadoSanitarioComuna <- as.data.frame(
    table(comuna$estado_sanitario)
  );
  estadoFisicoComuna <- data.frame(
    estadoSanitario = encabezado(estadoSanitario$encabezado, tmpEstadoSanitarioComuna$Var1),
    arboles = tmpEstadoSanitarioComuna$Freq,
    xes = round(tmpEstadoSanitarioComuna$Freq/sum(tmpEstadoSanitarioComuna$Freq), 4)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  tmpEstadoFisicoBarrios <- as.data.frame(
    table(barrios$estado_sanitario)
  );
  estadoSanitarioBarrios <- data.frame(
    estadoSanitario = encabezado(estadoSanitario$encabezado, tmpEstadoFisicoBarrios$Var1),
    arboles = tmpEstadoFisicoBarrios$Freq,
    xes = round(tmpEstadoFisicoBarrios$Freq/sum(tmpEstadoFisicoBarrios$Freq), 4)
  );
  tmpTop <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$estado_sanitario)
  );
  top <- data.frame(
    barrios = rownames(tmpTop),
    muerto = dominio(tmpTop, estadoSanitario$dominio, estadoSanitario$muerto, CHECK),
    critico = dominio(tmpTop, estadoSanitario$dominio, estadoSanitario$critico, CHECK),
    enfermo = dominio(tmpTop, estadoSanitario$dominio, estadoSanitario$enfermo, CHECK),
    sano = dominio(tmpTop, estadoSanitario$dominio, estadoSanitario$sano, CHECK),
    stringsAsFactors=FALSE
  );
  topBarrios <- data.frame(
    barriosMuerto = head(top[ order(-top$muerto), 1], 10),
    arbolesMuerto = head(top[ order(-top$muerto), 2], 10),
    barriosCritico = head(top[ order(-top$critico), 1], 10),
    arbolesCritico = head(top[ order(-top$critico), 3], 10),
    barriosEnfermo = head(top[ order(-top$enfermo), 1], 10),
    arbolesEnfermo = head(top[ order(-top$enfermo), 4], 10),
    barriosSano = head(top[ order(-top$sano), 1], 10),
    arbolesSano = head(top[ order(-top$sano), 5], 10),
    stringsAsFactors=FALSE
  );
  topBarrios <- rbind(topBarrios, "---");
  bottomBarrios <- data.frame(
    barriosMuerto = head(top[ order(-top$muerto), 1], 10),
    arbolesMuerto = head(top[ order(-top$muerto), 2], 10),
    barriosCritico = head(top[ order(-top$critico), 1], 10),
    arbolesCritico = head(top[ order(-top$critico), 3], 10),
    barriosEnfermo = head(top[ order(-top$enfermo), 1], 10),
    arbolesEnfermo = head(top[ order(-top$enfermo), 4], 10),
    barriosSano = head(top[ order(-top$sano), 1], 10),
    arbolesSano = head(top[ order(-top$sano), 5], 10),
  );
  topBarrios <- rbind(topBarrios, bottomBarrios);
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  tmpEstadoSanitarioCorredores <- as.data.frame(
    table(corredores$estado_sanitario)
  );
  estadoSanitarioCorredores <- data.frame(
    estadoSanitario = encabezado(estadoSanitario$encabezado, tmpEstadoSanitarioCorredores$Var1),
    arboles = tmpEstadoSanitarioCorredores$Freq,
    xes = round(tmpEstadoSanitarioCorredores$Freq/sum(tmpEstadoSanitarioCorredores$Freq), 4)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpEstadoSanitarioInstituciones <- as.data.frame(
    table(instituciones$estado_sanitario)
  );
  estadoSanitarioInstituciones <- data.frame(
    estadoSanitario = encabezado(estadoSanitario$encabezado, tmpEstadoSanitarioInstituciones$Var1),
    arboles = tmpEstadoSanitarioInstituciones$Freq,
    xes = round(tmpEstadoSanitarioInstituciones$Freq/sum(tmpEstadoSanitarioInstituciones$Freq), 4)
  );
  save.xlsx(estadoSanitario$informeGeneral, estadoFisicoComuna, estadoSanitarioBarrios, topBarrios, estadoSanitarioCorredores, estadoSanitarioInstituciones);
}