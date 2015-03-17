densidadFollajeEspecifico <- function(comuna){
  tmpFollajeComuna <- as.data.frame.matrix(
    table(comuna$barrio, comuna$densidad_follaje)
  );
  follajeComuna <- data.frame(
    barrio = rownames(tmpFollajeComuna),
    denso = tmpFollajeComuna[,1],
    xd = round(tmpFollajeComuna[,1]/sum(tmpFollajeComuna[,1]), 4),
    medio = tmpFollajeComuna[,2],
    xm = round(tmpFollajeComuna[,2]/sum(tmpFollajeComuna[,2]), 4),
    ralo = tmpFollajeComuna[,3],
    xr = round(tmpFollajeComuna[,3]/sum(tmpFollajeComuna[,3]), 4)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)))
  tmpFollajeBarrios <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$densidad_follaje)
  );
  follajeBarrios <- data.frame(
    barrio = rownames(tmpFollajeBarrios),
    denso = tmpFollajeBarrios[,1],
    xd = round(tmpFollajeBarrios[,1]/sum(tmpFollajeBarrios[,1]), 4),
    medio = tmpFollajeBarrios[,2],
    xm = round(tmpFollajeBarrios[,2]/sum(tmpFollajeBarrios[,2]), 4),
    ralo = tmpFollajeBarrios[,3],
    xr = round(tmpFollajeBarrios[,3]/sum(tmpFollajeBarrios[,3]), 4)
  );  
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)))
  tmpFollajeCorredores <- as.data.frame.matrix(
    table(factor(corredores$barrio), corredores$densidad_follaje)
  );
  follajeCorredores <- data.frame(
    barrio = rownames(tmpFollajeCorredores),
    denso = tmpFollajeCorredores[,1],
    xd = round(tmpFollajeCorredores[,1]/sum(tmpFollajeCorredores[,1]), 4),
    medio = tmpFollajeCorredores[,2],
    xm = round(tmpFollajeCorredores[,2]/sum(tmpFollajeCorredores[,2]), 4),
    ralo = tmpFollajeCorredores[,3],
    xr = round(tmpFollajeCorredores[,3]/sum(tmpFollajeCorredores[,3]), 4)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)))
  tmpFollajeInstituciones <- as.data.frame.matrix(
    table(factor(instituciones$institucion), instituciones$densidad_follaje)
  );
  follajeInstituciones <- data.frame(
    barrio = rownames(tmpFollajeInstituciones),
    denso = tmpFollajeInstituciones[,1],
    xd = round(tmpFollajeInstituciones[,1]/sum(tmpFollajeInstituciones[,1]), 4),
    medio = tmpFollajeInstituciones[,2],
    xm = round(tmpFollajeInstituciones[,2]/sum(tmpFollajeInstituciones[,2]), 4),
    ralo = tmpFollajeInstituciones[,3],
    xr = round(tmpFollajeInstituciones[,3]/sum(tmpFollajeInstituciones[,3]), 4)
  );
  save.xlsx("F19_densidad_follaje.xlsx", follajeComuna, follajeBarrios, follajeCorredores, follajeInstituciones)
}
densidadFollajeGeneral <- function(comuna){
  tmpFollajeComuna <- as.data.frame(
    table(comuna$densidad_follaje)
  );
  follajeComuna <- data.frame(
    densidad = c("Denso", "Medio", "Ralo"),
    arboles = tmpFollajeComuna$Freq,
    xd = round(tmpFollajeComuna$Freq/sum(tmpFollajeComuna$Freq), 4)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)))
  tmpFollajeBarrios <- as.data.frame(
    table(barrios$densidad_follaje)
  );
  follajeBarrios <- data.frame(
    densidad = c("Denso", "Medio", "Ralo"),
    arboles = tmpFollajeBarrios$Freq,
    xd = round(tmpFollajeBarrios$Freq/sum(tmpFollajeBarrios$Freq), 4)
  );
  tmpTop <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$densidad_follaje)
  );
  top <- data.frame(
    barrios = rownames(tmpTop),
    denso = tmpTop[,1],
    medio = tmpTop[,2],
    ralo = tmpTop[,3]
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
    densidad = c("Denso", "Medio", "Ralo"),
    arboles = tmpFollajeCorredores$Freq,
    xd = round(tmpFollajeCorredores$Freq/sum(tmpFollajeCorredores$Freq), 4)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)))
  tmpFollajeInstituciones <- as.data.frame(
    table(instituciones$densidad_follaje)
  );
  follajeInstituciones <- data.frame(
    densidad = c("Denso", "Medio", "Ralo"),
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
    parque = tmpEmplazamientoComuna[,1],
    xpr = round(tmpEmplazamientoComuna[,1]/sum(tmpEmplazamientoComuna[,1]), 4),
    glorieta = tmpEmplazamientoComuna[,2],
    xgl = round(tmpEmplazamientoComuna[,2]/sum(tmpEmplazamientoComuna[,2]), 4),
    anden = tmpEmplazamientoComuna[,3],
    xan = round(tmpEmplazamientoComuna[,3]/sum(tmpEmplazamientoComuna[,3]), 4),
    alcorque = tmpEmplazamientoComuna[,4],
    xal = round(tmpEmplazamientoComuna[,4]/sum(tmpEmplazamientoComuna[,4]), 4),
    separador = tmpEmplazamientoComuna[,5],
    xsp = round(tmpEmplazamientoComuna[,5]/sum(tmpEmplazamientoComuna[,5]), 4),
    antejardin = tmpEmplazamientoComuna[,6],
    xant = round(tmpEmplazamientoComuna[,6]/sum(tmpEmplazamientoComuna[,6]), 4),
    zonablanda = tmpEmplazamientoComuna[,7],
    xzb = round(tmpEmplazamientoComuna[,7]/sum(tmpEmplazamientoComuna[,7]), 4)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)))
  tmpEmplazamientoBarrios <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$emplazamiento)
  );
  emplazamientoBarrios <- data.frame(
    barrio = rownames(tmpEmplazamientoBarrios),
    parque = tmpEmplazamientoBarrios[,1],
    xpr = round(tmpEmplazamientoBarrios[,1]/sum(tmpEmplazamientoBarrios[,1]), 4),
    glorieta = tmpEmplazamientoBarrios[,2],
    xgl = round(tmpEmplazamientoBarrios[,2]/sum(tmpEmplazamientoBarrios[,2]), 4),
    anden = tmpEmplazamientoBarrios[,3],
    xan = round(tmpEmplazamientoBarrios[,3]/sum(tmpEmplazamientoBarrios[,3]), 4),
    alcorque = tmpEmplazamientoBarrios[,4],
    xal = round(tmpEmplazamientoBarrios[,4]/sum(tmpEmplazamientoBarrios[,4]), 4),
    separador = tmpEmplazamientoBarrios[,5],
    xsp = round(tmpEmplazamientoBarrios[,5]/sum(tmpEmplazamientoBarrios[,5]), 4),
    antejardin = tmpEmplazamientoBarrios[,6],
    xant = round(tmpEmplazamientoBarrios[,6]/sum(tmpEmplazamientoBarrios[,6]), 4),
    zonablanda = tmpEmplazamientoBarrios[,7],
    xzb = round(tmpEmplazamientoBarrios[,7]/sum(tmpEmplazamientoBarrios[,7]), 4)
  );
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)))
  tmpEmplazamientoCorredores <- as.data.frame.matrix(
    table(factor(corredores$barrio), corredores$emplazamiento)
  );
  emplazamientoCorredores <- data.frame(
    barrio = rownames(tmpEmplazamientoCorredores),
    parque = tmpEmplazamientoCorredores[,1],
    xpr = round(tmpEmplazamientoCorredores[,1]/sum(tmpEmplazamientoCorredores[,1])),
    glorieta = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.gl, CHECK),
    xgl = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.gl, SUM),
    anden = tmpEmplazamientoCorredores[,3],
    xan = round(tmpEmplazamientoCorredores[,3]/sum(tmpEmplazamientoCorredores[,3])),
    alcorque = tmpEmplazamientoCorredores[,4],
    xal = round(tmpEmplazamientoCorredores[,4]/sum(tmpEmplazamientoCorredores[,4])),
    separador = tmpEmplazamientoCorredores[,5],
    xsp = round(tmpEmplazamientoCorredores[,5]/sum(tmpEmplazamientoCorredores[,5])),
    antejardin = tmpEmplazamientoCorredores[,6],
    xant = round(tmpEmplazamientoCorredores[,6]/sum(tmpEmplazamientoCorredores[,6])),
    zonablanda = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.zb, CHECK),
    xzb = dominio(tmpEmplazamientoCorredores, emplazamiento, emplazamiento.zb, SUM)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)))
  tmpEmplazamientoInstituciones <- as.data.frame.matrix(
    table(factor(instituciones$institucion), instituciones$emplazamiento)
  );
  emplazamientoInstituciones <- data.frame(
    barrio = rownames(tmpEmplazamientoInstituciones),
    parque = tmpEmplazamientoInstituciones[,1],
    xpr = round(tmpEmplazamientoInstituciones[,1]/sum(tmpEmplazamientoInstituciones[,1])),
    glorieta = tmpEmplazamientoInstituciones[,2],
    xgl = round(tmpEmplazamientoInstituciones[,2]/sum(tmpEmplazamientoInstituciones[,2])),
    anden = tmpEmplazamientoInstituciones[,3],
    xan = round(tmpEmplazamientoInstituciones[,3]/sum(tmpEmplazamientoInstituciones[,3])),
    alcorque = tmpEmplazamientoInstituciones[,4],
    xal = round(tmpEmplazamientoInstituciones[,4]/sum(tmpEmplazamientoInstituciones[,4])),
    separador = tmpEmplazamientoInstituciones[,5],
    xsp = round(tmpEmplazamientoInstituciones[,5]/sum(tmpEmplazamientoInstituciones[,5])),
    antejardin = tmpEmplazamientoInstituciones[,6],
    xant = round(tmpEmplazamientoInstituciones[,6]/sum(tmpEmplazamientoInstituciones[,6])),
    zonablanda = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.zb, CHECK),
    xzb = dominio(tmpEmplazamientoInstituciones, emplazamiento, emplazamiento.zb, SUM)
  );
  save.xlsx("F20_emplazamientos.xlsx", emplazamientoComuna, emplazamientoBarrios, emplazamientoCorredores, emplazamientoInstituciones)
}