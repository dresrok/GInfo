densidadFollajeEspecifico <- function(comuna){
  tmpFollajeComuna <- as.data.frame.matrix(
    table(comuna$barrio, comuna$densidad_follaje)
  );
  follajeComuna <- data.frame(
    barrio = rownames(tmpFollajeComuna),
    denso = dominio(tmpFollajeComuna, densidad, densidad.denso, CHECK),
    xd = dominio(tmpFollajeComuna, densidad, densidad.denso, SUM),
    medio = dominio(tmpFollajeComuna, densidad, densidad.medio, CHECK),
    xm = dominio(tmpFollajeComuna, densidad, densidad.medio, SUM),
    ralo = dominio(tmpFollajeComuna, densidad, densidad.ralo, CHECK),
    xr = dominio(tmpFollajeComuna, densidad, densidad.ralo, SUM)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)))
  tmpFollajeBarrios <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$densidad_follaje)
  );
  follajeBarrios <- data.frame(
    barrio = rownames(tmpFollajeBarrios),
    denso = dominio(tmpFollajeBarrios, densidad, densidad.denso, CHECK),
    xd = dominio(tmpFollajeBarrios, densidad, densidad.denso, SUM),
    medio = dominio(tmpFollajeBarrios, densidad, densidad.medio, CHECK),
    xm = dominio(tmpFollajeBarrios, densidad, densidad.medio, SUM),
    ralo = dominio(tmpFollajeBarrios, densidad, densidad.ralo, CHECK),
    xr = dominio(tmpFollajeBarrios, densidad, densidad.ralo, SUM)
  );  
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)))
  tmpFollajeCorredores <- as.data.frame.matrix(
    table(factor(corredores$barrio), corredores$densidad_follaje)
  );
  follajeCorredores <- data.frame(
    barrio = rownames(tmpFollajeCorredores),
    denso = dominio(tmpFollajeCorredores, densidad, densidad.denso, CHECK),
    xd = dominio(tmpFollajeCorredores, densidad, densidad.denso, SUM),
    medio = dominio(tmpFollajeCorredores, densidad, densidad.medio, CHECK),
    xm = dominio(tmpFollajeCorredores, densidad, densidad.medio, SUM),
    ralo = dominio(tmpFollajeCorredores, densidad, densidad.ralo, CHECK),
    xr = dominio(tmpFollajeCorredores, densidad, densidad.ralo, SUM)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)))
  tmpFollajeInstituciones <- as.data.frame.matrix(
    table(factor(instituciones$institucion), instituciones$densidad_follaje)
  );
  follajeInstituciones <- data.frame(
    barrio = rownames(tmpFollajeInstituciones),
    denso = dominio(tmpFollajeInstituciones, densidad, densidad.denso, CHECK),
    xd = dominio(tmpFollajeInstituciones, densidad, densidad.denso, SUM),
    medio = dominio(tmpFollajeInstituciones, densidad, densidad.medio, CHECK),
    xm = dominio(tmpFollajeInstituciones, densidad, densidad.medio, SUM),
    ralo = dominio(tmpFollajeInstituciones, densidad, densidad.ralo, CHECK),
    xr = dominio(tmpFollajeInstituciones, densidad, densidad.ralo, SUM)
  );
  save.xlsx("F19_densidad_follaje.xlsx", follajeComuna, follajeBarrios, follajeCorredores, follajeInstituciones)
}
densidadFollajeGeneral <- function(comuna){
  tmpFollajeComuna <- as.data.frame(
    table(comuna$densidad_follaje)
  );
  follajeComuna <- data.frame(
    densidad = densidad.header,
    arboles = tmpFollajeComuna$Freq,
    xd = round(tmpFollajeComuna$Freq/sum(tmpFollajeComuna$Freq), 4)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)))
  tmpFollajeBarrios <- as.data.frame(
    table(barrios$densidad_follaje)
  );
  follajeBarrios <- data.frame(
    densidad = densidad.header,
    arboles = tmpFollajeBarrios$Freq,
    xd = round(tmpFollajeBarrios$Freq/sum(tmpFollajeBarrios$Freq), 4)
  );
  tmpTop <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$densidad_follaje)
  );
  top <- data.frame(
    barrios = rownames(tmpTop),
    denso = dominio(tmpTop, densidad, densidad.denso, CHECK),
    medio = dominio(tmpTop, densidad, densidad.medio, CHECK),
    ralo = dominio(tmpTop, densidad, densidad.ralo, CHECK)
  );
  topBarrios <- data.frame(
    barriosDenso = head(top[ order(-top$denso), 1], 10),
    arbolesDenso = head(top[ order(-top$denso), 2], 10),
    barriosMedio = head(top[ order(-top$medio), 1], 10),
    arbolesMedio = head(top[ order(-top$medio), 3], 10),
    barriosRalo = head(top[ order(-top$ralo), 1], 10),
    arbolesRalo = head(top[ order(-top$ralo), 4], 10)
  );
  sep <- data.frame(
    barriosDenso = "---",
    arbolesDenso = "---",
    barriosMedio = "---",
    arbolesMedio = "---",
    barriosRalo = "---",
    arbolesRalo = "---"
  );
  topBarrios <- rbind(topBarrios, sep);
  bottomBarrios <- data.frame(
    barriosDenso = head(top[ order(top$denso), 1], 5),
    arbolesDenso = head(top[ order(top$denso), 2], 5),
    barriosMedio = head(top[ order(top$medio), 1], 5),
    arbolesMedio = head(top[ order(top$medio), 3], 5),
    barriosRalo = head(top[ order(top$ralo), 1], 5),
    arbolesRalo = head(top[ order(top$ralo), 4], 5)
  );
  topBarrios <- rbind(topBarrios, bottomBarrios);
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)))
  tmpFollajeCorredores <- as.data.frame(
    table(corredores$densidad_follaje)
  );
  follajeCorredores <- data.frame(
    densidad = densidad.header,
    arboles = tmpFollajeCorredores$Freq,
    xd = round(tmpFollajeCorredores$Freq/sum(tmpFollajeCorredores$Freq), 4)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)))
  tmpFollajeInstituciones <- as.data.frame(
    table(instituciones$densidad_follaje)
  );
  follajeInstituciones <- data.frame(
    densidad = densidad.header,
    arboles = tmpFollajeInstituciones$Freq,
    xd = round(tmpFollajeInstituciones$Freq/sum(tmpFollajeInstituciones$Freq), 4)
  );
  save.xlsx("F1_densidad_follaje.xlsx", follajeComuna, follajeBarrios, topBarrios, follajeCorredores, follajeInstituciones)
}
emplazamientoEspecifico <- function(comuna){
  tmpEmplazamientoComuna <- as.data.frame.matrix(
    table(comuna$barrio, comuna$emplazamiento)
  );
  emplazamientoComuna <- data.frame(
    barrio = rownames(tmpEmplazamientoComuna),
    parque = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.pr, CHECK),
    xpr = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.pr, SUM),
    glorieta = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.gl, CHECK),
    xgl = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.gl, SUM),
    anden = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.an, CHECK),
    xan = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.an, SUM),
    alcorque = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.al, CHECK),
    xal = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.al, SUM),
    separador = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.sp, CHECK),
    xsp = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.sp, SUM),
    antejardin = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.ant, CHECK),
    xant = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.ant, SUM),
    zonablanda = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.zb, CHECK),
    xzb = dominio(tmpEmplazamientoComuna, emplazamiento, emplazamiento.zb, SUM)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)))
  tmpEmplazamientoBarrios <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$emplazamiento)
  );
  emplazamientoBarrios <- data.frame(
    barrio = rownames(tmpEmplazamientoBarrios),
    parque = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.pr, CHECK),
    xpr = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.pr, SUM),
    glorieta = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.gl, CHECK),
    xgl = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.gl, SUM),
    anden = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.an, CHECK),
    xan = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.an, SUM),
    alcorque = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.al, CHECK),
    xal = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.al, SUM),
    separador = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.sp, CHECK),
    xsp = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.sp, SUM),
    antejardin = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.ant, CHECK),
    xant = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.ant, SUM),
    zonablanda = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.zb, CHECK),
    xzb = dominio(tmpEmplazamientoBarrios, emplazamiento, emplazamiento.zb, SUM)
  );
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)))
  tmpEmplazamientoCorredores <- as.data.frame.matrix(
    table(factor(corredores$barrio), corredores$emplazamiento)
  );
  emplazamientoCorredores <- data.frame(
    barrio = rownames(tmpEmplazamientoCorredores),
    parque = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.pr, CHECK),
    xpr = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.pr, SUM),
    glorieta = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.gl, CHECK),
    xgl = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.gl, SUM),
    anden = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.an, CHECK),
    xan = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.an, SUM),
    alcorque = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.al, CHECK),
    xal = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.al, SUM),
    separador = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.sp, CHECK),
    xsp = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.sp, SUM),
    antejardin = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.ant, CHECK),
    xant = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.ant, SUM),
    zonablanda = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.zb, CHECK),
    xzb = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.zb, SUM)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)))
  tmpEmplazamientoInstituciones <- as.data.frame.matrix(
    table(factor(instituciones$institucion), instituciones$emplazamiento)
  );
  emplazamientoInstituciones <- data.frame(
    barrio = rownames(tmpEmplazamientoInstituciones),
    parque = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.pr, CHECK),
    xpr = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.pr, SUM),
    glorieta = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.gl, CHECK),
    xgl = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.gl, SUM),
    anden = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.an, CHECK),
    xan = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.an, SUM),
    alcorque = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.al, CHECK),
    xal = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.al, SUM),
    separador = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.sp, CHECK),
    xsp = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.sp, SUM),
    antejardin = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.ant, CHECK),
    xant = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.ant, SUM),
    zonablanda = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.zb, CHECK),
    xzb = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.zb, SUM)
  );
  save.xlsx("F20_emplazamientos.xlsx", emplazamientoComuna, emplazamientoBarrios, emplazamientoCorredores, emplazamientoInstituciones)
}
emplazamientoGeneral <- function(comuna){
  tmpEmplazamientoBarrios <- as.data.frame(
    table(comuna$emplazamiento)
  );
  emplazamientoComuna <- data.frame(
    emplazamiento = emplazamiento.header,
    arboles = tmpEmplazamientoBarrios$Freq,
    xe = round(tmpEmplazamientoBarrios$Freq/sum(tmpEmplazamientoBarrios$Freq), 4)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)))
  tmpEmplazamientoBarrios <- as.data.frame(
    table(barrios$emplazamiento)
  );
  emplazamientoBarrios <- data.frame(
    densidad = emplazamiento.header,
    arboles = tmpEmplazamientoBarrios$Freq,
    xe = round(tmpEmplazamientoBarrios$Freq/sum(tmpEmplazamientoBarrios$Freq), 4)
  );
  tmpTop <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$emplazamiento)
  );
  top <- data.frame(
    barrios = rownames(tmpTop),
    parque = dominio(tmpTop, emplazamiento, emplazamiento.pr, CHECK),
    glorieta = dominio(tmpTop, emplazamiento, emplazamiento.gl, CHECK),
    anden = dominio(tmpTop, emplazamiento, emplazamiento.an, CHECK),
    alcorque = dominio(tmpTop, emplazamiento, emplazamiento.al, CHECK),
    separador = dominio(tmpTop, emplazamiento, emplazamiento.sp, CHECK),
    antejardin = dominio(tmpTop, emplazamiento, emplazamiento.ant, CHECK),
    zonablanda = dominio(tmpTop, emplazamiento, emplazamiento.zb, CHECK)
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
    arbolesZonaBlanda = head(top[ order(-top$zonablanda), 8], 10)
  );
  sep <- data.frame(
    barriosParque = "---",
    arbolesParque = "---",
    barriosGlorieta = "---",
    arbolesGlorieta = "---",
    barriosAnden = "---",
    arbolesAnden = "---",
    barriosAlcorque = "---",
    arbolesAlcorque = "---",
    barriosSeparador = "---",
    arbolesSeparador = "---",
    barriosAntejardin = "---",
    arbolesAntejardin = "---",
    barriosZonaBlanda = "---",
    arbolesZonaBlanda = "---"
  );
  topBarrios <- rbind(topBarrios, sep);
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
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)))
  tmpEmplazamientoCorredores <<- as.data.frame(
    table(corredores$emplazamiento)
  );
  emplazamientoCorredores <- data.frame(
    emplazamiento = encabezado(emplazamiento.header, tmpEmplazamientoCorredores$Var1),
    arboles = tmpEmplazamientoCorredores$Freq,
    xe = round(tmpEmplazamientoCorredores$Freq/sum(tmpEmplazamientoCorredores$Freq), 4)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)))
  tmpEmplazamientoInstituciones <- as.data.frame(
    table(instituciones$emplazamiento)
  );
  emplazamientoInstituciones <- data.frame(
    emplazamiento = encabezado(emplazamiento.header, tmpEmplazamientoCorredores$Var1),
    arboles = tmpEmplazamientoInstituciones$Freq,
    xe = round(tmpEmplazamientoInstituciones$Freq/sum(tmpEmplazamientoInstituciones$Freq), 4)
  );
  save.xlsx("F2_emplazamiento.xlsx", emplazamientoComuna, emplazamientoBarrios, topBarrios, emplazamientoCorredores, emplazamientoInstituciones)
}