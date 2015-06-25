densidadFollajeEspecifico <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  instituciones$barrio <- NULL;

  # Inicio Densidad de follaje especifico
  follajeComuna <- getDensidadFollaje(comuna, informe$especifico);
  follajeBarrios <- getDensidadFollaje(barrios, informe$especifico);
  follajeCorredores <- getDensidadFollaje(corredores, informe$especifico);
  follajeInstituciones <- getDensidadFollaje(instituciones, informe$especifico);
  # Fin Densidad de follaje especifico

  save.xlsx(densidad$informeEspecifico, follajeComuna, follajeBarrios, follajeCorredores, follajeInstituciones);
}
emplazamientoEspecifico <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  instituciones$barrio <- NULL;

  emplazamientoComuna <- getEmplazamiento(comuna, informe$especifico);
  emplazamientoBarrios <- getEmplazamiento(barrios, informe$especifico);
  emplazamientoCorredores <- getEmplazamiento(corredores, informe$especifico);
  emplazamientoInstituciones <- getEmplazamiento(instituciones, informe$especifico);

  save.xlsx(emplazamiento$informeEspecifico, emplazamientoComuna, emplazamientoBarrios, emplazamientoCorredores, emplazamientoInstituciones);
}
estadoFisicoEspecifico <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  instituciones$barrio <- NULL;

  estadoFisicoComuna <- getEstadoFisico(comuna, informe$especifico);
  estadoFisicoBarrios <- getEstadoFisico(barrios, informe$especifico);
  estadoFisicoCorredores <- getEstadoFisico(corredores, informe$especifico);
  estadoFisicoInstituciones <- getEstadoFisico(instituciones, informe$especifico);

  save.xlsx(estadoFisico$informeEspecifico, estadoFisicoComuna, estadoFisicoBarrios, estadoFisicoCorredores, estadoFisicoInstituciones);
}
estadoHojaEspecifico <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  instituciones$barrio <- NULL;

  estadoHojaComuna <- getEstadoHoja(comuna, informe$especifico);
  estadoHojaBarrios <- getEstadoHoja(barrios, informe$especifico);
  estadoHojaCorredores <- getEstadoHoja(corredores, informe$especifico);
  estadoHojaInstituciones <- getEstadoHoja(instituciones, informe$especifico);

  save.xlsx(estadoHoja$informeEspecifico, estadoHojaComuna, estadoHojaBarrios, estadoHojaCorredores, estadoHojaInstituciones);
}
estadoSanitarioEspecifico <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  instituciones$barrio <- NULL;

  estadoSanitarioComuna <- getEstadoSanitario(comuna, informe$especifico);
  estadoSanitarioBarrios <- getEstadoSanitario(barrios, informe$especifico);
  estadoSanitarioCorredores <- getEstadoSanitario(corredores, informe$especifico);
  estadoSanitarioInstituciones <- getEstadoSanitario(instituciones, informe$especifico);

  save.xlsx(estadoSanitario$informeEspecifico, estadoSanitarioComuna, estadoSanitarioBarrios, estadoSanitarioCorredores, estadoSanitarioInstituciones);
}
valorEsteticoEspecifico <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  instituciones$barrio <- NULL;

  # Inicio Valor estético especifico
  valorEsteticoComuna <- getValorEstetico(comuna, informe$especifico);
  valorEsteticoBarrios <- getValorEstetico(barrios, informe$especifico);
  valorEsteticoCorredores <- getValorEstetico(corredores, informe$especifico);
  valorEsteticoInstituciones <- getValorEstetico(instituciones, informe$especifico);
  # Fin Valor estético especifico

  save.xlsx(valorEstetico$informeEspecifico, valorEsteticoComuna, valorEsteticoBarrios, valorEsteticoCorredores, valorEsteticoInstituciones);    
}
especiesEspecifico <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  barrios$nom_cientifico <- factor(barrios$nom_cientifico);
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  corredores$nom_cientifico <- factor(corredores$nom_cientifico);
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  instituciones$nom_cientifico <- factor(instituciones$nom_cientifico);
  instituciones$barrio <- NULL;

  tmpEspeciesComuna <- as.data.frame(
    table(trimws(comuna$nom_cientifico, which = "both")), stringsAsFactors=FALSE
  );
  especiesComunas <- data.frame(
    nombreCientifico = as.character(tmpEspeciesComuna$Var1),
    abundancia = tmpEspeciesComuna$Freq,
    x = round((tmpEspeciesComuna$Freq/sum(tmpEspeciesComuna$Freq))*100, 2),
    stringsAsFactors=FALSE
  );
  especiesComunas <- contarEspecies(especiesComunas, comuna, conteo$general);

  tmpEspeciesBarrio <- as.data.frame(
    table(factor(trimws(barrios$nom_cientifico, which = "both"))), stringsAsFactors=FALSE
  );

  especiesBarrios <- data.frame(
    nombreCientifico = as.character(tmpEspeciesBarrio$Var1),
    abundancia = tmpEspeciesBarrio$Freq,
    x = round((tmpEspeciesBarrio$Freq/sum(tmpEspeciesBarrio$Freq))*100, 2),
    stringsAsFactors=FALSE
  );

  especiesBarrios <- contarEspecies(especiesBarrios, barrios, conteo$general);

  if(nrow(corredores) > 0){
    tmpEspeciesCorredor <- as.data.frame(
      table(factor(trimws(corredores$nom_cientifico, which = "both"))), stringsAsFactors=FALSE
    );

    especiesCorredores <- data.frame(
      nombreCientifico = as.character(tmpEspeciesCorredor$Var1),
      abundancia = tmpEspeciesCorredor$Freq,
      x = round((tmpEspeciesCorredor$Freq/sum(tmpEspeciesCorredor$Freq))*100, 2),
      stringsAsFactors=FALSE
    );

    especiesCorredores <- contarEspecies(especiesCorredores, corredores, conteo$general);
  } else {
    especiesCorredores <- "No existen registros";
  }
    
  if(nrow(instituciones) > 0){
    tmpEspeciesInstitucion <- as.data.frame(
      table(factor(trimws(instituciones$nom_cientifico, which = "both"))), stringsAsFactors=FALSE
    );

    especiesInstituciones <- data.frame(
      nombreCientifico = as.character(tmpEspeciesInstitucion$Var1),
      abundancia = tmpEspeciesInstitucion$Freq,
      x = round((tmpEspeciesInstitucion$Freq/sum(tmpEspeciesInstitucion$Freq))*100, 2),
      stringsAsFactors=FALSE
    );

    especiesInstituciones <- contarEspecies(especiesInstituciones, instituciones, conteo$general);
  } else {
    especiesInstituciones <- "No existen registros";
  }
    

  if(FALSE){
    nombresBarrios <- as.character(factor(unique(barrios$barrio)));
    encabezadoBarrio <- contarEspecies(sort(nombresBarrios), barrios, conteo$especifico);
    
    nombresCorredores <- as.character(factor(unique(corredores$barrio)));
    encabezadoCorredor <- contarEspecies(sort(nombresCorredores), corredores, conteo$especifico);
    
    nombresInstituciones <- as.character(factor(unique(instituciones$institucion)));
    encabezadoInstitucion <- contarEspecies(sort(nombresInstituciones), instituciones, conteo$especifico);
  }
  
  save.xlsx(especies$informeEspecifico, especiesComunas, especiesBarrios, especiesCorredores, especiesInstituciones);
}
procedenciaEspecifico <- function(comuna){
  tmpProcedenciaComuna <- as.data.frame.matrix(
    table(comuna$barrio, comuna$procedencia)
  );
  procedenciaComuna <- data.frame(
    barrio = rownames(tmpProcedenciaComuna),
    nativa = dominio(tmpProcedenciaComuna, procedencia$dominio, procedencia$nativa, CHECK),
    xn = dominio(tmpProcedenciaComuna, procedencia$dominio, procedencia$nativa, SUM),
    exotica = dominio(tmpProcedenciaComuna, procedencia$dominio, procedencia$exotica, CHECK),
    xe = dominio(tmpProcedenciaComuna, procedencia$dominio, procedencia$exotica, SUM)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  tmpProcedenciaBarrios <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$procedencia)
  );
  procedenciaBarrios <- data.frame(
    barrio = rownames(tmpProcedenciaBarrios),
    nativa = dominio(tmpProcedenciaBarrios, procedencia$dominio, procedencia$nativa, CHECK),
    xn = dominio(tmpProcedenciaBarrios, procedencia$dominio, procedencia$nativa, SUM),
    exotica = dominio(tmpProcedenciaBarrios, procedencia$dominio, procedencia$exotica, CHECK),
    xe = dominio(tmpProcedenciaBarrios, procedencia$dominio, procedencia$exotica, SUM)
  );  
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  tmpProcedenciaCorredores <- as.data.frame.matrix(
    table(factor(corredores$barrio), corredores$procedencia)
  );
  procedenciaCorredores <- data.frame(
    barrio = rownames(tmpProcedenciaCorredores),
    nativa = dominio(tmpProcedenciaCorredores, procedencia$dominio, procedencia$nativa, CHECK),
    xn = dominio(tmpProcedenciaCorredores, procedencia$dominio, procedencia$nativa, SUM),
    exotica = dominio(tmpProcedenciaCorredores, procedencia$dominio, procedencia$exotica, CHECK),
    xe = dominio(tmpProcedenciaCorredores, procedencia$dominio, procedencia$exotica, SUM)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpProcedenciaInstituciones <- as.data.frame.matrix(
    table(factor(instituciones$institucion), instituciones$procedencia)
  );
  follajeInstituciones <- data.frame(
    barrio = rownames(tmpProcedenciaInstituciones),
    nativa = dominio(tmpProcedenciaInstituciones, procedencia$dominio, procedencia$nativa, CHECK),
    xn = dominio(tmpProcedenciaInstituciones, procedencia$dominio, procedencia$nativa, SUM),
    exotica = dominio(tmpProcedenciaInstituciones, procedencia$dominio, procedencia$exotica, CHECK),
    xe = dominio(tmpProcedenciaInstituciones, procedencia$dominio, procedencia$exotica, SUM)
  );
  save.xlsx(procedencia$informeEspecifico, procedenciaComuna, procedenciaBarrios, procedenciaCorredores, follajeInstituciones);
}
tipoPlantaEspecifico <- function(comuna){
  tmpTipoPlantaComuna <- as.data.frame.matrix(
    table(comuna$barrio, comuna$habito_crecimiento)
  );
  tipoPlantaComuna <- data.frame(
    barrio = rownames(tmpTipoPlantaComuna),
    arbol = dominio(tmpTipoPlantaComuna, tipoPlanta$dominio, tipoPlanta$arbol, CHECK),
    xa = dominio(tmpTipoPlantaComuna, tipoPlanta$dominio, tipoPlanta$arbol, SUM),
    arbusto = dominio(tmpTipoPlantaComuna, tipoPlanta$dominio, tipoPlanta$arbusto, CHECK),
    xabto = dominio(tmpTipoPlantaComuna, tipoPlanta$dominio, tipoPlanta$arbusto, SUM),
    palma = dominio(tmpTipoPlantaComuna, tipoPlanta$dominio, tipoPlanta$palma, CHECK),
    xp = dominio(tmpTipoPlantaComuna, tipoPlanta$dominio, tipoPlanta$palma, SUM),
    bambu = dominio(tmpTipoPlantaComuna, tipoPlanta$dominio, tipoPlanta$bambu, CHECK),
    xb = dominio(tmpTipoPlantaComuna, tipoPlanta$dominio, tipoPlanta$bambu, SUM)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  tmpTipoPlantaBarrios <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$habito_crecimiento)
  );
  tipoPlantaBarrios <- data.frame(
    barrio = rownames(tmpTipoPlantaBarrios),
    arbol = dominio(tmpTipoPlantaBarrios, tipoPlanta$dominio, tipoPlanta$arbol, CHECK),
    xa = dominio(tmpTipoPlantaBarrios, tipoPlanta$dominio, tipoPlanta$arbol, SUM),
    arbusto = dominio(tmpTipoPlantaBarrios, tipoPlanta$dominio, tipoPlanta$arbusto, CHECK),
    xabto = dominio(tmpTipoPlantaBarrios, tipoPlanta$dominio, tipoPlanta$arbusto, SUM),
    palma = dominio(tmpTipoPlantaBarrios, tipoPlanta$dominio, tipoPlanta$palma, CHECK),
    xp = dominio(tmpTipoPlantaBarrios, tipoPlanta$dominio, tipoPlanta$palma, SUM),
    bambu = dominio(tmpTipoPlantaBarrios, tipoPlanta$dominio, tipoPlanta$bambu, CHECK),
    xb = dominio(tmpTipoPlantaBarrios, tipoPlanta$dominio, tipoPlanta$bambu, SUM)
  );  
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  tmpTipoPlantaCorredores <- as.data.frame.matrix(
    table(factor(corredores$barrio), corredores$habito_crecimiento)
  );
  tipoPlantaCorredores <- data.frame(
    barrio = rownames(tmpTipoPlantaCorredores),
    arbol = dominio(tmpTipoPlantaCorredores, tipoPlanta$dominio, tipoPlanta$arbol, CHECK),
    xa = dominio(tmpTipoPlantaCorredores, tipoPlanta$dominio, tipoPlanta$arbol, SUM),
    arbusto = dominio(tmpTipoPlantaCorredores, tipoPlanta$dominio, tipoPlanta$arbusto, CHECK),
    xabto = dominio(tmpTipoPlantaCorredores, tipoPlanta$dominio, tipoPlanta$arbusto, SUM),
    palma = dominio(tmpTipoPlantaCorredores, tipoPlanta$dominio, tipoPlanta$palma, CHECK),
    xp = dominio(tmpTipoPlantaCorredores, tipoPlanta$dominio, tipoPlanta$palma, SUM),
    bambu = dominio(tmpTipoPlantaCorredores, tipoPlanta$dominio, tipoPlanta$bambu, CHECK),
    xb = dominio(tmpTipoPlantaCorredores, tipoPlanta$dominio, tipoPlanta$bambu, SUM)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpTipoPlantaInstituciones <- as.data.frame.matrix(
    table(factor(instituciones$institucion), instituciones$habito_crecimiento)
  );
  tipoPlantaInstituciones <- data.frame(
    barrio = rownames(tmpTipoPlantaInstituciones),
    arbol = dominio(tmpTipoPlantaInstituciones, tipoPlanta$dominio, tipoPlanta$arbol, CHECK),
    xa = dominio(tmpTipoPlantaInstituciones, tipoPlanta$dominio, tipoPlanta$arbol, SUM),
    arbusto = dominio(tmpTipoPlantaInstituciones, tipoPlanta$dominio, tipoPlanta$arbusto, CHECK),
    xabto = dominio(tmpTipoPlantaInstituciones, tipoPlanta$dominio, tipoPlanta$arbusto, SUM),
    palma = dominio(tmpTipoPlantaInstituciones, tipoPlanta$dominio, tipoPlanta$palma, CHECK),
    xp = dominio(tmpTipoPlantaInstituciones, tipoPlanta$dominio, tipoPlanta$palma, SUM),
    bambu = dominio(tmpTipoPlantaInstituciones, tipoPlanta$dominio, tipoPlanta$bambu, CHECK),
    xb = dominio(tmpTipoPlantaInstituciones, tipoPlanta$dominio, tipoPlanta$bambu, SUM)
  );
  save.xlsx(tipoPlanta$informeEspecifico, tipoPlantaComuna, tipoPlantaBarrios, tipoPlantaCorredores, tipoPlantaInstituciones);
}
conflictoEspecifico <- function(comuna){
  conflictosComuna <- contarConflictos(comuna, conteo$especifico, darValor(comuna, conteo$limite));

  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  conflictosBarrios <- contarConflictos(barrios, conteo$especifico, darValor(barrios, conteo$limite));

  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  conflictosCorredores <- contarConflictos(corredores, conteo$especifico, darValor(barrios, conteo$limite));

  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  conflictosInstituciones <- contarConflictos(instituciones, conteo$especifico, darValor(barrios, conteo$limite));

  save.xlsx(conflictos$informeEspecifico, conflictosComuna, conflictosBarrios, conflictosCorredores, conflictosInstituciones);
}