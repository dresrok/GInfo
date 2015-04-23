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
  familiasComuna <- getFamilias(comuna);
  familiasBarrios <- getFamilias(barrios);
  familiasCorredores <- getFamilias(corredores);
  familiasInstituciones <- getFamilias(instituciones);
  # Fin Familias más abundantes registradas

  save.xlsx(informeComuna$informeGeneral, comunaGeneral, familiasComuna, familiasBarrios, familiasCorredores, familiasInstituciones);
}
densidadFollajeGeneral <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Densidad de follaje
  follajeComuna <- getDensidadFollaje(comuna);
  follajeBarrios <- getDensidadFollaje(barrios);
  follajeCorredores <- getDensidadFollaje(corredores);
  follajeInstituciones <- getDensidadFollaje(instituciones);
  # Fin Densidad de follaje
  
  save.xlsx(densidad$informeGeneral, follajeComuna, follajeBarrios, follajeCorredores, follajeInstituciones);
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
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Estado físico
  estadoFisicoComuna <- getEstadoFisico(comuna);
  estadoFisicoBarrios <- getEstadoFisico(barrios);
  estadoFisicoCorredores <- getEstadoFisico(corredores);
  estadoFisicoInstituciones <- getEstadoFisico(instituciones);
  # Fin Estado físico

  save.xlsx(estadoFisico$informeGeneral, estadoFisicoComuna, estadoFisicoBarrios, estadoFisicoCorredores, estadoFisicoInstituciones);
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
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Valor estético de los individuos
  valorEsteticoComuna <- getValorEstetico(comuna);
  valorEsteticoBarrios <- getValorEstetico(barrios);
  valorEsteticoCorredores <- getValorEstetico(corredores);
  valorEsteticoInstituciones <- getValorEstetico(instituciones);
  # Fin Valor estético de los individuos

  save.xlsx(valorEstetico$informeGeneral, valorEsteticoComuna, valorEsteticoBarrios, valorEsteticoCorredores, valorEsteticoInstituciones);

  if(FALSE){
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
  }
}
procedenciaGeneral <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Procedencia de las especies encontradas
  procedenciaComuna <- getEspeciesProcedencia(comuna);
  procedenciaBarrios <- getEspeciesProcedencia(barrios);
  procedenciaCorredores <- getEspeciesProcedencia(corredores);
  procedenciaInstituciones <- getEspeciesProcedencia(instituciones);
  save.xlsx(procedencia$informeGeneral, procedenciaComuna, procedenciaBarrios, procedenciaCorredores, procedenciaInstituciones);
  # Fin Procedencia de las especies encontradas
}
habitoGeneral <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Hábito de crecimiento de las especies
  habitoComuna <- getEspeciesHabito(comuna);  
  habitoBarrios <- getEspeciesHabito(barrios);
  habitoCorredores <- getEspeciesHabito(corredores);
  habitoInstituciones <- getEspeciesHabito(instituciones);
  # Fin Hábito de crecimiento de las especies

  save.xlsx(habito$informeGeneral, habitoComuna, habitoBarrios, habitoCorredores, habitoInstituciones);
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
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Distribución de altura total
  alturaTotalComuna <- getAlturas(comuna, "1");
  alturaTotalBarrios <- getAlturas(barrios, "1");
  alturaTotalCorredores <- getAlturas(corredores, "1");
  alturaTotalInstituciones <- getAlturas(corredores, "1");
  # Fin Distribución de altura total

  # Inicio Distribución de altura de fuste
  alturaFusteComuna <- getAlturas(comuna, "2");
  alturaFusteBarrios <- getAlturas(barrios, "2");
  alturaFusteCorredores <- getAlturas(corredores, "2");
  alturaFusteInstituciones <- getAlturas(instituciones, "2");
  # Fin Distribución de altura de fuste

  save.xlsx("alturas.xlsx", alturaTotalComuna, alturaFusteComuna, 
    alturaTotalBarrios, alturaFusteBarrios, 
    alturaTotalCorredores, alturaFusteCorredores, 
    alturaTotalInstituciones, alturaFusteInstituciones);
}
diametros <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Distribución de DAP
  diametroNormalComuna <- getDiametros(comuna, "1");
  diametroNormalBarrios <- getDiametros(barrios, "1");
  diametroNormalCorredores <- getDiametros(corredores, "1");
  diametroNormalInstituciones <- getDiametros(instituciones, "1");
  # Fin Distribución de DAP

  # Inicio Distribución del diámetro de copa
  diametroCopaComuna <- getDiametros(comuna, "2");
  diametroCopaBarrios <- getDiametros(barrios, "2");
  diametroCopaCorredores <- getDiametros(corredores, "2");
  diametroCopaInstituciones <- getDiametros(instituciones, "2");
  # Fin Distribución del diámetro de copa

  save.xlsx("diametros.xlsx", diametroNormalComuna, diametroCopaComuna, 
    diametroNormalBarrios, diametroCopaBarrios, 
    diametroNormalCorredores, diametroCopaCorredores,
    diametroNormalInstituciones, diametroCopaInstituciones);
}
volumen <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Distribución del volumen
  volumenComuna <- getVolumen(comuna);
  volumenBarrios <- getVolumen(barrios);
  volumenCorredores <- getVolumen(corredores);
  volumenInstituciones <- getVolumen(instituciones);
  # Fin Distribución del volumen

  save.xlsx("volumen.xlsx", volumenComuna, volumenBarrios, volumenCorredores, volumenInstituciones);
}