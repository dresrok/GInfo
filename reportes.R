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
  save.xlsx("F1_densidad_follaje.xlsx", follajeComuna, follajeBarrios, follajeCorredores, follajeInstituciones)
}