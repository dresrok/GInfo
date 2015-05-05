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
    ubicacion = informe$encabezado,
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

  especiesMasAbundantesBarrio <- getMasAbundante(barrios);

  save.xlsx(informe$comuna, comunaGeneral, familiasComuna, familiasBarrios, especiesMasAbundantesBarrio, familiasCorredores, familiasInstituciones);
}
densidadFollajeGeneral <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Densidad de follaje
  follajeComuna <- getDensidadFollaje(comuna, informe$general);
  follajeBarrios <- getDensidadFollaje(barrios, informe$general);
  follajeCorredores <- getDensidadFollaje(corredores, informe$general);
  follajeInstituciones <- getDensidadFollaje(instituciones, informe$general);
  # Fin Densidad de follaje
  
  save.xlsx(densidad$informeGeneral, follajeComuna, follajeBarrios, follajeCorredores, follajeInstituciones);
}
emplazamientoGeneral <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Emplazamiento de los individuos
  emplazamientoComuna <- getEmplazamiento(comuna, informe$general);
  emplazamientoBarrios <- getEmplazamiento(barrios, informe$general);
  emplazamientoCorredores <- getEmplazamiento(corredores, informe$general);
  emplazamientoInstituciones <- getEmplazamiento(instituciones, informe$general);
  # Fin Emplazamiento de los individuos

  save.xlsx(emplazamiento$informeGeneral, emplazamientoComuna, emplazamientoBarrios, emplazamientoCorredores, emplazamientoInstituciones);
}
estadoFisicoGeneral <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Estado físico
  estadoFisicoComuna <- getEstadoFisico(comuna, informe$general);
  estadoFisicoBarrios <- getEstadoFisico(barrios, informe$general);
  estadoFisicoCorredores <- getEstadoFisico(corredores, informe$general);
  estadoFisicoInstituciones <- getEstadoFisico(instituciones, informe$general);
  # Fin Estado físico

  save.xlsx(estadoFisico$informeGeneral, estadoFisicoComuna, estadoFisicoBarrios, estadoFisicoCorredores, estadoFisicoInstituciones);
}
estadoHojaGeneral <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Estado de hoja
  estadoHojaComuna <- getEstadoHoja(comuna, informe$general);
  estadoHojaBarrios <- getEstadoHoja(barrios, informe$general);
  estadoHojaCorredores <- getEstadoHoja(corredores, informe$general);
  estadoHojaInstituciones <- getEstadoHoja(instituciones, informe$general);
  # Fin Estado de hoja

  save.xlsx(estadoHoja$informeGeneral, estadoHojaComuna, estadoHojaBarrios, estadoHojaCorredores, estadoHojaInstituciones); 
}
estadoSanitarioGeneral <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Estado sanitario
  estadoSanitarioComuna <- getEstadoSanitario(comuna, informe$general);
  estadoSanitarioBarrios <- getEstadoSanitario(barrios, informe$general);
  estadoSanitarioCorredores <- getEstadoSanitario(corredores, informe$general);
  estadoSanitarioInstituciones <- getEstadoSanitario(instituciones, informe$general);
  # Fin Estado sanitario 

  save.xlsx(estadoSanitario$informeGeneral, estadoSanitarioComuna, estadoSanitarioBarrios, estadoSanitarioCorredores, estadoSanitarioInstituciones);
}
valorEsteticoGeneral <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Valor estético de los individuos
  valorEsteticoComuna <- getValorEstetico(comuna, informe$general);
  valorEsteticoBarrios <- getValorEstetico(barrios, informe$general);
  valorEsteticoCorredores <- getValorEstetico(corredores, informe$general);
  valorEsteticoInstituciones <- getValorEstetico(instituciones, informe$general);
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
  # Fin Procedencia de las especies encontradas

  save.xlsx(procedencia$informeGeneral, procedenciaComuna, procedenciaBarrios, procedenciaCorredores, procedenciaInstituciones);
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
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Distribución porcentual del conflicto
  conflictosComuna <<- getConflictos(comuna);
  conflictosBarrios <<- getConflictos(barrios);
  conflictosCorredores <<- getConflictos(corredores);
  conflictosInstituciones <<- getConflictos(instituciones);
  # Fin Distribución porcentual del conflicto

  #save.xlsx(conflictos$informeGeneral, conflictosComuna, conflictosBarrios, conflictosCorredores, conflictosInstituciones);
}
alturas <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Distribución de altura total
  alturaTotalComuna <- getAlturas(comuna, "1");
  alturaTotalBarrios <- getAlturas(barrios, "1");
  alturaTotalCorredores <- getAlturas(corredores, "1");
  alturaTotalInstituciones <- getAlturas(instituciones, "1");
  # Fin Distribución de altura total

  # Inicio Distribución de altura de fuste
  alturaFusteComuna <- getAlturas(comuna, "2");
  alturaFusteBarrios <- getAlturas(barrios, "2");
  alturaFusteCorredores <- getAlturas(corredores, "2");
  alturaFusteInstituciones <- getAlturas(instituciones, "2");
  # Fin Distribución de altura de fuste

  save.xlsx(dasometria$alturas, alturaTotalComuna, alturaFusteComuna, 
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

  save.xlsx(dasometria$diametros, diametroNormalComuna, diametroCopaComuna, 
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

  save.xlsx(dasometria$volumen, volumenComuna, volumenBarrios, volumenCorredores, volumenInstituciones);
}
propiedadesFisicas <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Distribución de las propiedades físicas
  propiedadesFisicasComuna <- getPropiedadesFisicas(comuna);
  propiedadesFisicasBarrios <- getPropiedadesFisicas(barrios);
  propiedadesFisicasCorredores <- getPropiedadesFisicas(corredores);
  propiedadesFisicasInstituciones <- getPropiedadesFisicas(instituciones);
  # Fin Distribución de las propiedades físicas

  save.xlsx(propiedades$informeFisicas, propiedadesFisicasComuna, propiedadesFisicasBarrios, propiedadesFisicasCorredores, propiedadesFisicasInstituciones);
}
propiedadesSanitarias <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Distribución de las propiedades sanitarias
  propiedadesSanitariasComuna <- getPropiedadesSanitarias(comuna);
  propiedadesSanitariasBarrios <- getPropiedadesSanitarias(barrios);
  propiedadesSanitariasCorredores <- getPropiedadesSanitarias(corredores);
  propiedadesSanitariasInstituciones <- getPropiedadesSanitarias(instituciones);
  # Fin Distribución de las propiedades sanitarias

  save.xlsx(propiedades$informeSanitarias, propiedadesSanitariasComuna, propiedadesSanitariasBarrios, propiedadesSanitariasCorredores, propiedadesSanitariasInstituciones);
}
riesgos <- function(comuna){
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));

  # Inicio Distribución de los riesgos
  riesgosComuna <- getRiesgos(comuna);
  riesgosBarrios <- getRiesgos(barrios);
  riesgosCorredores <- getRiesgos(corredores);
  riesgosInstituciones <- getRiesgos(instituciones);
  # Fin Distribución de los riesgos

  save.xlsx(propiedades$informeRiesgos, riesgosComuna, riesgosBarrios, riesgosCorredores, riesgosInstituciones);
}