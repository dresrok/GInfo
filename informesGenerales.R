general <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Distribución de los arboles por ubicación  
  totalIndividuosBarrio <- nrow(barrios);
  totalIndividuosCorredores <- nrow(corredores);
  totalIndividuosInstituciones <- nrow(instituciones);
  totales <- c(
    totalIndividuosBarrio-totalIndividuosInstituciones, 
    totalIndividuosInstituciones,
    totalIndividuosCorredores
  );
  comunaGeneral <- data.frame(
    ubicacion = informeComuna$encabezado,
    individuos = totales,
    xi = round(totales/sum(totales), 4),
    stringsAsFactors=FALSE
  );
  filaTotalComuna <- data.frame(
    ubicacion = "Total individuos",
    individuos = sum(comunaGeneral$individuos),
    xi = sum(comunaGeneral$xi),
    stringsAsFactors=FALSE
  );
  comunaGeneral <- rbind(comunaGeneral, filaTotalComuna);
  # Fin Distribución de los arboles por ubicación

  # Inicio Familias más abundantes registradas 
  familiasComuna <- contarFamilias(comuna);
  familiasBarrios <- contarFamilias(barrios);
  familiasCorredores <- contarFamilias(corredores);
  familiasInstituciones <- contarFamilias(instituciones);
  # Fin Familias más abundantes registradas 
  save.xlsx(informeComuna$informeGeneral, comunaGeneral, familiasComuna, familiasBarrios, familiasCorredores, familiasInstituciones);
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
    emplazamiento = encabezado(emplazamiento$encabezado, tmpEmplazamientoBarrios$Var1),
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
    emplazamiento = encabezado(emplazamiento$encabezado, tmpEmplazamientoInstituciones$Var1),
    arboles = tmpEmplazamientoInstituciones$Freq,
    xe = round(tmpEmplazamientoInstituciones$Freq/sum(tmpEmplazamientoInstituciones$Freq), 4)
  );
  save.xlsx(emplazamiento$informeGeneral, emplazamientoComuna, emplazamientoBarrios, topBarrios, emplazamientoCorredores, emplazamientoInstituciones);
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
estadoHojaGeneral <- function(comuna){
  tmpCloroticaComuna <- as.data.frame.list(
    table(comuna$hc), row.names = estadoHoja$clorotica
  );
  colnames(tmpCloroticaComuna) <- estadoHoja$encabezado;
  cloroticaComuna <- tmpCloroticaComuna;
  tmpCaducifoliaComuna <- as.data.frame.list(
    table(comuna$hcf), row.names = estadoHoja$caducifolia
  );
  colnames(tmpCaducifoliaComuna) <- estadoHoja$encabezado;
  caducifoliaComuna <- tmpCaducifoliaComuna;
  estadoHojaComuna <- rbind(cloroticaComuna, caducifoliaComuna);

  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));

  tmpCloroticaBarrios <- as.data.frame.list(
    table(barrios$hc), row.names = estadoHoja$clorotica
  );
  colnames(tmpCloroticaBarrios) <- estadoHoja$encabezado;
  cloroticaBarrios <- tmpCloroticaBarrios;
  tmpCaducifoliaBarrios <- as.data.frame.list(
    table(barrios$hcf), row.names = estadoHoja$caducifolia
  );
  colnames(tmpCaducifoliaBarrios) <- estadoHoja$encabezado  
  caducifoliaBarrios <- tmpCaducifoliaBarrios;
  estadoHojaBarrios <- rbind(cloroticaBarrios, caducifoliaBarrios);

  tmpTopClorotica <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$hc)
  );
  topClorotica <- data.frame(
    barrios = rownames(tmpTopClorotica),
    en = dominio(tmpTopClorotica, estadoHoja$dominio, estadoHoja$en, CHECK),    
    dn = dominio(tmpTopClorotica, estadoHoja$dominio, estadoHoja$dn, CHECK),    
    nr = dominio(tmpTopClorotica, estadoHoja$dominio, estadoHoja$nr, CHECK),
    stringsAsFactors=FALSE
  );
  topCloroticaBarrios <- data.frame(
    barriosEn = head(topClorotica[ order(-topClorotica$en), 1], 10),
    arbolesEn = head(topClorotica[ order(-topClorotica$en), 2], 10),
    barriosDn = head(topClorotica[ order(-topClorotica$dn), 1], 10),
    arbolesDn = head(topClorotica[ order(-topClorotica$dn), 3], 10),    
    barriosNr = head(topClorotica[ order(-topClorotica$nr), 1], 10),
    arbolesNr = head(topClorotica[ order(-topClorotica$nr), 4], 10),
    stringsAsFactors=FALSE
  );
  topCloroticaBarrios <- rbind(topCloroticaBarrios, "---");
  bottomCloroticaBarrios <- data.frame(
    barriosEn = head(topClorotica[ order(topClorotica$en), 1], 5),
    arbolesEn = head(topClorotica[ order(topClorotica$en), 2], 5),
    barriosDn = head(topClorotica[ order(topClorotica$dn), 1], 5),
    arbolesDn = head(topClorotica[ order(topClorotica$dn), 3], 5),
    barriosNr = head(topClorotica[ order(topClorotica$nr), 1], 5),
    arbolesNr = head(topClorotica[ order(topClorotica$nr), 4], 5)
  );
  topCloroticaBarrios <- rbind(topCloroticaBarrios, bottomCloroticaBarrios);
  
  tmpTopCaducifolia <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$hcf)
  );
  topCaducifolia <- data.frame(
    barrios = rownames(tmpTopCaducifolia),
    en = dominio(tmpTopCaducifolia, estadoHoja$dominio, estadoHoja$en, CHECK),    
    dn = dominio(tmpTopCaducifolia, estadoHoja$dominio, estadoHoja$dn, CHECK),    
    nr = dominio(tmpTopCaducifolia, estadoHoja$dominio, estadoHoja$nr, CHECK),
    stringsAsFactors=FALSE
  );
  topCaducifoliaBarrios <- data.frame(
    barriosEn = head(topCaducifolia[ order(-topCaducifolia$en), 1], 10),
    arbolesEn = head(topCaducifolia[ order(-topCaducifolia$en), 2], 10),
    barriosDn = head(topCaducifolia[ order(-topCaducifolia$dn), 1], 10),
    arbolesDn = head(topCaducifolia[ order(-topCaducifolia$dn), 3], 10),    
    barriosNr = head(topCaducifolia[ order(-topCaducifolia$nr), 1], 10),
    arbolesNr = head(topCaducifolia[ order(-topCaducifolia$nr), 4], 10),
    stringsAsFactors=FALSE
  );
  topCaducifoliaBarrios <- rbind(topCaducifoliaBarrios, "---");
  bottomCaducifoliaBarrios <- data.frame(
    barriosEn = head(topCaducifolia[ order(topCaducifolia$en), 1], 5),
    arbolesEn = head(topCaducifolia[ order(topCaducifolia$en), 2], 5),
    barriosDn = head(topCaducifolia[ order(topCaducifolia$dn), 1], 5),
    arbolesDn = head(topCaducifolia[ order(topCaducifolia$dn), 3], 5),
    barriosNr = head(topCaducifolia[ order(topCaducifolia$nr), 1], 5),
    arbolesNr = head(topCaducifolia[ order(topCaducifolia$nr), 4], 5)
  );
  topCaducifoliaBarrios <- rbind(topCaducifoliaBarrios, bottomCaducifoliaBarrios);
  
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));   
  tmpCloroticaCorredores <- as.data.frame.list(
    table(corredores$hc), row.names = estadoHoja$clorotica
  );
  colnames(tmpCloroticaCorredores) <- estadoHoja$encabezado;
  cloroticaCorredores <- tmpCloroticaCorredores;
  tmpCaducifoliaCorredores <- as.data.frame.list(
    table(corredores$hcf), row.names = estadoHoja$caducifolia
  );
  colnames(tmpCaducifoliaCorredores) <- estadoHoja$encabezado;
  caducifoliaCorredores <- tmpCaducifoliaCorredores;
  estadoHojaCorredores <- rbind(cloroticaCorredores, caducifoliaCorredores);

  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpCloroticaInstituciones <- as.data.frame.list(
    table(instituciones$hc), row.names = estadoHoja$clorotica
  );
  colnames(tmpCloroticaInstituciones) <- estadoHoja$encabezado;
  cloroticaInstituciones <- tmpCloroticaInstituciones;
  tmpCaducifoliaInstituciones <- as.data.frame.list(
    table(instituciones$hcf), row.names = estadoHoja$caducifolia
  );
  colnames(tmpCaducifoliaInstituciones) <- estadoHoja$encabezado;
  caducifoliaInstituciones <- tmpCaducifoliaInstituciones;
  estadoHojaInstituciones <- rbind(cloroticaInstituciones, caducifoliaInstituciones);
  save.xlsx(estadoHoja$informeGeneral, estadoHojaComuna, estadoHojaBarrios, topCloroticaBarrios, topCaducifoliaBarrios, estadoHojaCorredores, estadoHojaInstituciones);
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
    barriosMuerto = head(top[ order(top$muerto), 1], 5),
    arbolesMuerto = head(top[ order(top$muerto), 2], 5),
    barriosCritico = head(top[ order(top$critico), 1], 5),
    arbolesCritico = head(top[ order(top$critico), 3], 5),
    barriosEnfermo = head(top[ order(top$enfermo), 1], 5),
    arbolesEnfermo = head(top[ order(top$enfermo), 4], 5),
    barriosSano = head(top[ order(top$sano), 1], 5),
    arbolesSano = head(top[ order(top$sano), 5], 5)
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
valorEsteticoGeneral <- function(comuna){
  tmpValorEsteticoComuna <- as.data.frame(
    table(comuna$valor_estetico)
  );
  valorEsteticoComuna <- data.frame(
    valorEstetico = encabezado(valorEstetico$encabezado, tmpValorEsteticoComuna$Var1),
    arboles = tmpValorEsteticoComuna$Freq,
    xve = round(tmpValorEsteticoComuna$Freq/sum(tmpValorEsteticoComuna$Freq), 4)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  tmpValorEsteticoBarrios <- as.data.frame(
    table(barrios$valor_estetico)
  );
  valorEsteticoBarrios <- data.frame(
    valorEstetico = encabezado(valorEstetico$encabezado, tmpValorEsteticoBarrios$Var1),
    arboles = tmpValorEsteticoBarrios$Freq,
    xve = round(tmpValorEsteticoBarrios$Freq/sum(tmpValorEsteticoBarrios$Freq), 4)
  );
  tmpTop <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$valor_estetico)
  );
  top <- data.frame(
    barrios = rownames(tmpTop),
    emblematico = dominio(tmpTop, valorEstetico$dominio, valorEstetico$emb, CHECK),
    esencial = dominio(tmpTop, valorEstetico$dominio, valorEstetico$ese, CHECK),
    deseable = dominio(tmpTop, valorEstetico$dominio, valorEstetico$des, CHECK),
    indiferente = dominio(tmpTop, valorEstetico$dominio, valorEstetico$ind, CHECK),
    inaceptable = dominio(tmpTop, valorEstetico$dominio, valorEstetico$ina, CHECK),
    stringsAsFactors=FALSE
  );
  topBarrios <- data.frame(
    barriosEmblematico = head(top[ order(-top$emb), 1], 10),
    arbolesEmblematico = head(top[ order(-top$emb), 2], 10),
    barriosEsencial = head(top[ order(-top$ese), 1], 10),
    arbolesEsencial = head(top[ order(-top$ese), 3], 10),
    barriosDeseable = head(top[ order(-top$des), 1], 10),
    arbolesDeseable = head(top[ order(-top$des), 4], 10),
    barriosIndiferente = head(top[ order(-top$ind), 1], 10),
    arbolesIndiferente = head(top[ order(-top$ind), 5], 10),
    barriosInaceptable = head(top[ order(-top$ina), 1], 10),
    arbolesInaceptable = head(top[ order(-top$ina), 6], 10),
    stringsAsFactors=FALSE
  );
  topBarrios <- rbind(topBarrios, "---");
  bottomBarrios <- data.frame(
    barriosEmblematico = head(top[ order(-top$emb), 1], 5),
    arbolesEmblematico = head(top[ order(-top$emb), 2], 5),
    barriosEsencial = head(top[ order(-top$ese), 1], 5),
    arbolesEsencial = head(top[ order(-top$ese), 3], 5),
    barriosDeseable = head(top[ order(-top$des), 1], 5),
    arbolesDeseable = head(top[ order(-top$des), 4], 5),
    barriosIndiferente = head(top[ order(-top$ind), 1], 5),
    arbolesIndiferente = head(top[ order(-top$ind), 5], 5),
    barriosInaceptable = head(top[ order(-top$ina), 1], 5),
    arbolesInaceptable = head(top[ order(-top$ina), 6], 5)
  );
  topBarrios <- rbind(topBarrios, bottomBarrios);
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  tmpValorEsteticoCorredores <- as.data.frame(
    table(corredores$valor_estetico)
  );
  valorEsteticoCorredores <- data.frame(
    valorEstetico = encabezado(valorEstetico$encabezado, tmpValorEsteticoCorredores$Var1),
    arboles = tmpValorEsteticoCorredores$Freq,
    xve = round(tmpValorEsteticoCorredores$Freq/sum(tmpValorEsteticoCorredores$Freq), 4)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpValorEsteticoInstituciones <- as.data.frame(
    table(instituciones$valor_estetico)
  );
  valorEsteticoInstituciones <- data.frame(
    valorEstetico = encabezado(valorEstetico$encabezado, tmpValorEsteticoInstituciones$Var1),
    arboles = tmpValorEsteticoInstituciones$Freq,
    xe = round(tmpValorEsteticoInstituciones$Freq/sum(tmpValorEsteticoInstituciones$Freq), 4)
  );
  save.xlsx(valorEstetico$informeGeneral, valorEsteticoComuna, valorEsteticoBarrios, topBarrios, valorEsteticoCorredores, valorEsteticoInstituciones);
}
procedenciaGeneral <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Procedencia de las especies encontradas
  procedenciaComuna <- especiesProcedenciaHabito(comuna, procedencia$dominio);
  procedenciaBarrios <- especiesProcedenciaHabito(barrios, procedencia$dominio);
  procedenciaCorredores <- especiesProcedenciaHabito(corredores, procedencia$dominio);
  procedenciaInstituciones <- especiesProcedenciaHabito(instituciones, procedencia$dominio);
  save.xlsx(procedencia$informeGeneral, procedenciaComuna, procedenciaBarrios, procedenciaCorredores, procedenciaInstituciones);
  # Fin Procedencia de las especies encontradas
}
habitoGeneral <- function(comuna){

  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  habitoComuna <- especiesProcedenciaHabito(comuna, habito$dominio);  
  habitoBarrios <- especiesProcedenciaHabito(barrios, habito$dominio);
  habitoCorredores <- especiesProcedenciaHabito(corredores, habito$dominio);
  habitoInstituciones <- especiesProcedenciaHabito(instituciones, habito$dominio);

  save.xlsx(habito$informeGeneral, habitoComuna, habitoBarrios, habitoCorredores, habitoInstituciones);

  if(FALSE){
    barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
    tmpTipoPlantaBarrios <- as.data.frame(
      table(barrios$habito_crecimiento)
    );
    tipoPlantaBarrios <- data.frame(
      habito = encabezado(habito$encabezado, tmpTipoPlantaBarrios$Var1),
      especies = length(unique(barrios$especies)),
      individuos = tmpTipoPlantaBarrios$Freq,
      xi = round(tmpTipoPlantaBarrios$Freq/sum(tmpTipoPlantaBarrios$Freq), 4)
    );
    corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
    tmpTipoPlantaCorredores <- as.data.frame(
      table(corredores$habito_crecimiento)
    );
    tipoPlantaCorredores <- data.frame(
      habito = encabezado(habito$encabezado, tmpTipoPlantaCorredores$Var1),
      especies = length(unique(corredores$especies)),
      individuos = tmpTipoPlantaCorredores$Freq,
      xi = round(tmpTipoPlantaCorredores$Freq/sum(tmpTipoPlantaCorredores$Freq), 4)
    );
    instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
    tmpTipoPlantaInstituciones <- as.data.frame(
      table(instituciones$habito_crecimiento)
    );
    tipoPlantaInstituciones <- data.frame(
      habito = encabezado(habito$encabezado, tmpTipoPlantaInstituciones$Var1),
      especies = length(unique(instituciones$especies)),
      individuos = tmpTipoPlantaInstituciones$Freq,
      xi = round(tmpTipoPlantaInstituciones$Freq/sum(tmpTipoPlantaInstituciones$Freq), 4)
    );
    save.xlsx(habito$informeGeneral, tipoPlantaComuna, tipoPlantaBarrios, tipoPlantaCorredores, tipoPlantaInstituciones);
  }
}
conflictoGeneral <- function(comuna){  
  tmpConflictosComuna <- contarConflictos(comuna, conteo$general, darValor(comuna, conteo$limite));  
  totalComuna <- darValor(comuna, conteo$total);
  conflictosComuna <- data.frame(
    conflictos = conflictos$nombres,
    sinConflicto = tmpConflictosComuna$sinConflicto,
    xsi = round(tmpConflictosComuna$sinConflicto/totalComuna, 4),
    conConflicto = tmpConflictosComuna$conConflicto,
    xno = round(tmpConflictosComuna$conConflicto/totalComuna, 4)
  );

  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  totalBarrios <- darValor(barrios, conteo$total);
  tmpConflictosBarrios <- contarConflictos(barrios, conteo$general, darValor(comuna, conteo$limite));
  conflictosBarrios <- data.frame(
    conflictos = conflictos$nombres,
    sinConflicto = tmpConflictosBarrios$sinConflicto,
    xsi = round(tmpConflictosBarrios$sinConflicto/totalBarrios, 4),
    conConflicto = tmpConflictosBarrios$conConflicto,
    xno = round(tmpConflictosBarrios$conConflicto/totalBarrios, 4)
  );

  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  totalCorredores <- darValor(corredores, conteo$total);
  tmpConflictosCorredores <- contarConflictos(corredores, conteo$general, darValor(comuna, conteo$limite));
  conflictosCorredores <- data.frame(
    conflictos = conflictos$nombres,
    sinConflicto = tmpConflictosCorredores$sinConflicto,
    xsi = round(tmpConflictosCorredores$sinConflicto/totalCorredores, 4),
    conConflicto = tmpConflictosCorredores$conConflicto,
    xno = round(tmpConflictosCorredores$conConflicto/totalCorredores, 4)
  );

  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  totalInstituciones <- darValor(instituciones, conteo$total);
  tmpConflictosInstituciones <- contarConflictos(instituciones, conteo$general, darValor(comuna, conteo$limite));
  conflictosInstituciones <- data.frame(
    conflictos = conflictos$nombres,
    sinConflicto = tmpConflictosInstituciones$sinConflicto,
    xsi = round(tmpConflictosInstituciones$sinConflicto/totalInstituciones, 4),
    conConflicto = tmpConflictosInstituciones$conConflicto,
    xno = round(tmpConflictosInstituciones$conConflicto/totalInstituciones, 4)
  );

  save.xlsx(conflictos$informeGeneral, conflictosComuna, conflictosBarrios, conflictosCorredores, conflictosInstituciones);
}
alturas <- function(comuna){  
  alturaFusteComuna <<- calcularRangos(comuna, 1);
  alturaTotalComuna <<- calcularRangos(comuna, 2);
}