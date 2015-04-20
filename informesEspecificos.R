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
estadoHojaEspecifico <- function(comuna){
  tmpCloroticaComuna <- as.data.frame.matrix(
    table(comuna$barrio, comuna$hc)
  );
  cloroticaComuna <- data.frame(
    barrio = rownames(tmpCloroticaComuna),
    enHC = dominio(tmpCloroticaComuna, estadoHoja$dominio, estadoHoja$en, CHECK),
    xenHC = dominio(tmpCloroticaComuna, estadoHoja$dominio, estadoHoja$en, SUM),
    dnHC = dominio(tmpCloroticaComuna, estadoHoja$dominio, estadoHoja$dn, CHECK),
    xdnHC = dominio(tmpCloroticaComuna, estadoHoja$dominio, estadoHoja$dn, SUM),
    nrHC = dominio(tmpCloroticaComuna, estadoHoja$dominio, estadoHoja$nr, CHECK),
    xnrHC = dominio(tmpCloroticaComuna, estadoHoja$dominio, estadoHoja$nr, SUM)
  );
  tmpCaducifoliaComuna <- as.data.frame.matrix(
    table(comuna$barrio, comuna$hcf)
  );
  caducifoliaComuna <- data.frame(
    barrio = rownames(tmpCaducifoliaComuna),
    enHCF = dominio(tmpCaducifoliaComuna, estadoHoja$dominio, estadoHoja$en, CHECK),
    xenHCF = dominio(tmpCaducifoliaComuna, estadoHoja$dominio, estadoHoja$en, SUM),
    dnHCF = dominio(tmpCaducifoliaComuna, estadoHoja$dominio, estadoHoja$dn, CHECK),
    xdnHCF = dominio(tmpCaducifoliaComuna, estadoHoja$dominio, estadoHoja$dn, SUM),
    nrHCF = dominio(tmpCaducifoliaComuna, estadoHoja$dominio, estadoHoja$nr, CHECK),
    xnrHCF = dominio(tmpCaducifoliaComuna, estadoHoja$dominio, estadoHoja$nr, SUM)
  );
  estadoHojaComuna <- cbind(cloroticaComuna, caducifoliaComuna);
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  tmpCloroticaBarrios <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$hc)
  );
  cloroticaBarrios <- data.frame(
    barrio = rownames(tmpCloroticaBarrios),
    enHC = dominio(tmpCloroticaBarrios, estadoHoja$dominio, estadoHoja$en, CHECK),
    xenHC = dominio(tmpCloroticaBarrios, estadoHoja$dominio, estadoHoja$en, SUM),
    dnHC = dominio(tmpCloroticaBarrios, estadoHoja$dominio, estadoHoja$dn, CHECK),
    xdnHC = dominio(tmpCloroticaBarrios, estadoHoja$dominio, estadoHoja$dn, SUM),
    nrHC = dominio(tmpCloroticaBarrios, estadoHoja$dominio, estadoHoja$nr, CHECK),
    xnrHC = dominio(tmpCloroticaBarrios, estadoHoja$dominio, estadoHoja$nr, SUM)
  );
  tmpCaducifoliaBarrios <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$hcf)
  );
  caducifoliaBarrios <- data.frame(
    barrio = rownames(tmpCaducifoliaBarrios),
    enHCF = dominio(tmpCaducifoliaBarrios, estadoHoja$dominio, estadoHoja$en, CHECK),
    xenHCF = dominio(tmpCaducifoliaBarrios, estadoHoja$dominio, estadoHoja$en, SUM),
    dnHCF = dominio(tmpCaducifoliaBarrios, estadoHoja$dominio, estadoHoja$dn, CHECK),
    xdnHCF = dominio(tmpCaducifoliaBarrios, estadoHoja$dominio, estadoHoja$dn, SUM),
    nrHCF = dominio(tmpCaducifoliaBarrios, estadoHoja$dominio, estadoHoja$nr, CHECK),
    xnrHCF = dominio(tmpCaducifoliaBarrios, estadoHoja$dominio, estadoHoja$nr, SUM)
  );
  estadoHojaBarrios <- cbind(cloroticaBarrios, caducifoliaBarrios);

  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  tmpCloroticaCorredores <- as.data.frame.matrix(
    table(factor(corredores$barrio), corredores$hc)
  );
  cloroticaCorredores <- data.frame(
    barrio = rownames(tmpCloroticaCorredores),
    enHC = dominio(tmpCloroticaCorredores, estadoHoja$dominio, estadoHoja$en, CHECK),
    xenHC = dominio(tmpCloroticaCorredores, estadoHoja$dominio, estadoHoja$en, SUM),
    dnHC = dominio(tmpCloroticaCorredores, estadoHoja$dominio, estadoHoja$dn, CHECK),
    xdnHC = dominio(tmpCloroticaCorredores, estadoHoja$dominio, estadoHoja$dn, SUM),
    nrHC = dominio(tmpCloroticaCorredores, estadoHoja$dominio, estadoHoja$nr, CHECK),
    xnrHC = dominio(tmpCloroticaCorredores, estadoHoja$dominio, estadoHoja$nr, SUM)
  );
  tmpCaducifoliaCorredores <- as.data.frame.matrix(
    table(factor(corredores$barrio), corredores$hcf)
  );
  caducifoliaCorredores <- data.frame(
    barrio = rownames(tmpCaducifoliaCorredores),
    enHCF = dominio(tmpCaducifoliaCorredores, estadoHoja$dominio, estadoHoja$en, CHECK),
    xenHCF = dominio(tmpCaducifoliaCorredores, estadoHoja$dominio, estadoHoja$en, SUM),
    dnHCF = dominio(tmpCaducifoliaCorredores, estadoHoja$dominio, estadoHoja$dn, CHECK),
    xdnHCF = dominio(tmpCaducifoliaCorredores, estadoHoja$dominio, estadoHoja$dn, SUM),
    nrHCF = dominio(tmpCaducifoliaCorredores, estadoHoja$dominio, estadoHoja$nr, CHECK),
    xnrHCF = dominio(tmpCaducifoliaCorredores, estadoHoja$dominio, estadoHoja$nr, SUM)
  );
  estadoHojaCorredores <- cbind(cloroticaCorredores, caducifoliaCorredores);
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpCloroticaInstituciones <- as.data.frame.matrix(
    table(factor(instituciones$institucion), instituciones$hc)
  );
  cloroticaInstituciones <- data.frame(
    barrio = rownames(tmpCloroticaInstituciones),
    enHC = dominio(tmpCloroticaInstituciones, estadoHoja$dominio, estadoHoja$en, CHECK),
    xenHC = dominio(tmpCloroticaInstituciones, estadoHoja$dominio, estadoHoja$en, SUM),
    dnHC = dominio(tmpCloroticaInstituciones, estadoHoja$dominio, estadoHoja$dn, CHECK),
    xdnHC = dominio(tmpCloroticaInstituciones, estadoHoja$dominio, estadoHoja$dn, SUM),
    nrHC = dominio(tmpCloroticaInstituciones, estadoHoja$dominio, estadoHoja$nr, CHECK),
    xnrHC = dominio(tmpCloroticaInstituciones, estadoHoja$dominio, estadoHoja$nr, SUM)
  );
  tmpCaducifoliaInstituciones <- as.data.frame.matrix(
    table(factor(instituciones$institucion), instituciones$hcf)
  );
  caducifoliaInstituciones <- data.frame(
    barrio = rownames(tmpCloroticaInstituciones),
    enHCF = dominio(tmpCloroticaInstituciones, estadoHoja$dominio, estadoHoja$en, CHECK),
    xenHCF = dominio(tmpCloroticaInstituciones, estadoHoja$dominio, estadoHoja$en, SUM),
    dnHCF = dominio(tmpCloroticaInstituciones, estadoHoja$dominio, estadoHoja$dn, CHECK),
    xdnHCF = dominio(tmpCloroticaInstituciones, estadoHoja$dominio, estadoHoja$dn, SUM),
    nrHCF = dominio(tmpCloroticaInstituciones, estadoHoja$dominio, estadoHoja$nr, CHECK),
    xnrHCF = dominio(tmpCloroticaInstituciones, estadoHoja$dominio, estadoHoja$nr, SUM)
  );
  estadoHojaInstituciones <- cbind(cloroticaInstituciones, caducifoliaInstituciones);
  save.xlsx(estadoHoja$informeEspecifico, estadoHojaComuna, estadoHojaBarrios, estadoHojaCorredores, estadoHojaInstituciones);
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
valorEsteticoEspecifico <- function(comuna){
  tmpValorEsteticoComuna <- as.data.frame.matrix(
    table(comuna$barrio, comuna$valor_estetico)
  );
  valorEsteticoComuna <- data.frame(
    barrio = rownames(tmpValorEsteticoComuna),
    emblematico = dominio(tmpValorEsteticoComuna, valorEstetico$dominio, valorEstetico$emb, CHECK),
    xemb = dominio(tmpValorEsteticoComuna, valorEstetico$dominio, valorEstetico$emb, SUM),
    esencial = dominio(tmpValorEsteticoComuna, valorEstetico$dominio, valorEstetico$ese, CHECK),
    xese = dominio(tmpValorEsteticoComuna, valorEstetico$dominio, valorEstetico$ese, SUM),
    deseable = dominio(tmpValorEsteticoComuna, valorEstetico$dominio, valorEstetico$des, CHECK),
    xdes = dominio(tmpValorEsteticoComuna, valorEstetico$dominio, valorEstetico$des, SUM),
    indiferente = dominio(tmpValorEsteticoComuna, valorEstetico$dominio, valorEstetico$ind, CHECK),
    xind = dominio(tmpValorEsteticoComuna, valorEstetico$dominio, valorEstetico$ind, SUM),
    inaceptable = dominio(tmpValorEsteticoComuna, valorEstetico$dominio, valorEstetico$ina, CHECK),
    xina = dominio(tmpValorEsteticoComuna, valorEstetico$dominio, valorEstetico$ina, SUM)
  );
  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  tmpValorEstetiBarrios <- as.data.frame.matrix(
    table(factor(barrios$barrio), barrios$valor_estetico)
  );
  valorEsteticoBarrios <- data.frame(
    barrio = rownames(tmpValorEstetiBarrios),
    emblematico = dominio(tmpValorEstetiBarrios, valorEstetico$dominio, valorEstetico$emb, CHECK),
    xemb = dominio(tmpValorEstetiBarrios, valorEstetico$dominio, valorEstetico$emb, SUM),
    esencial = dominio(tmpValorEstetiBarrios, valorEstetico$dominio, valorEstetico$ese, CHECK),
    xese = dominio(tmpValorEstetiBarrios, valorEstetico$dominio, valorEstetico$ese, SUM),
    deseable = dominio(tmpValorEstetiBarrios, valorEstetico$dominio, valorEstetico$des, CHECK),
    xdes = dominio(tmpValorEstetiBarrios, valorEstetico$dominio, valorEstetico$des, SUM),
    indiferente = dominio(tmpValorEstetiBarrios, valorEstetico$dominio, valorEstetico$ind, CHECK),
    xind = dominio(tmpValorEstetiBarrios, valorEstetico$dominio, valorEstetico$ind, SUM),
    inaceptable = dominio(tmpValorEstetiBarrios, valorEstetico$dominio, valorEstetico$ina, CHECK),
    xina = dominio(tmpValorEstetiBarrios, valorEstetico$dominio, valorEstetico$ina, SUM)
  );
  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  tmpValorEsteticoCorredores <- as.data.frame.matrix(
    table(factor(corredores$barrio), corredores$valor_estetico)
  );
  valorEsteticoCorredores <- data.frame(
    barrio = rownames(tmpValorEsteticoCorredores),
    emblematico = dominio(tmpValorEsteticoCorredores, valorEstetico$dominio, valorEstetico$emb, CHECK),
    xemb = dominio(tmpValorEsteticoCorredores, valorEstetico$dominio, valorEstetico$emb, SUM),
    esencial = dominio(tmpValorEsteticoCorredores, valorEstetico$dominio, valorEstetico$ese, CHECK),
    xese = dominio(tmpValorEsteticoCorredores, valorEstetico$dominio, valorEstetico$ese, SUM),
    deseable = dominio(tmpValorEsteticoCorredores, valorEstetico$dominio, valorEstetico$des, CHECK),
    xdes = dominio(tmpValorEsteticoCorredores, valorEstetico$dominio, valorEstetico$des, SUM),
    indiferente = dominio(tmpValorEsteticoCorredores, valorEstetico$dominio, valorEstetico$ind, CHECK),
    xind = dominio(tmpValorEsteticoCorredores, valorEstetico$dominio, valorEstetico$ind, SUM),
    inaceptable = dominio(tmpValorEsteticoCorredores, valorEstetico$dominio, valorEstetico$ina, CHECK),
    xina = dominio(tmpValorEsteticoCorredores, valorEstetico$dominio, valorEstetico$ina, SUM)
  );
  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  tmpValorEsteticoInstituciones <- as.data.frame.matrix(
    table(factor(instituciones$institucion), instituciones$valor_estetico)
  );
  valorEsteticoInstituciones <- data.frame(
    barrio = rownames(tmpValorEsteticoInstituciones),
    emblematico = dominio(tmpValorEsteticoInstituciones, valorEstetico$dominio, valorEstetico$emb, CHECK),
    xemb = dominio(tmpValorEsteticoInstituciones, valorEstetico$dominio, valorEstetico$emb, SUM),
    esencial = dominio(tmpValorEsteticoInstituciones, valorEstetico$dominio, valorEstetico$ese, CHECK),
    xese = dominio(tmpValorEsteticoInstituciones, valorEstetico$dominio, valorEstetico$ese, SUM),
    deseable = dominio(tmpValorEsteticoInstituciones, valorEstetico$dominio, valorEstetico$des, CHECK),
    xdes = dominio(tmpValorEsteticoInstituciones, valorEstetico$dominio, valorEstetico$des, SUM),
    indiferente = dominio(tmpValorEsteticoInstituciones, valorEstetico$dominio, valorEstetico$ind, CHECK),
    xind = dominio(tmpValorEsteticoInstituciones, valorEstetico$dominio, valorEstetico$ind, SUM),
    inaceptable = dominio(tmpValorEsteticoInstituciones, valorEstetico$dominio, valorEstetico$ina, CHECK),
    xina = dominio(tmpValorEsteticoInstituciones, valorEstetico$dominio, valorEstetico$ina, SUM)
  );
  save.xlsx(valorEstetico$informeEspecifico, valorEsteticoComuna, valorEsteticoBarrios, valorEsteticoCorredores, valorEsteticoInstituciones);
}
especiesEspecifico <- function(comuna){
  tmpEspeciesComuna <- as.data.frame(
    table(comuna$nom_cientifico), stringsAsFactors=FALSE
  );
  especiesComunas <- data.frame(
    nombreCientifico = as.character(tmpEspeciesComuna$Var1),
    abundancia = tmpEspeciesComuna$Freq,
    x = round(tmpEspeciesComuna$Freq/sum(tmpEspeciesComuna$Freq), 4),
    stringsAsFactors=FALSE
  );
  especiesComunas <- contarEspecies(especiesComunas, comuna, conteo$general);

  barrios <- subset(comuna, !grepl("^corredor", tolower(barrio)));
  nombresBarrios <- as.character(factor(unique(barrios$barrio)));
  encabezadoBarrio <- contarEspecies(sort(nombresBarrios), barrios, conteo$especifico);

  corredores <- subset(comuna, grepl("^corredor", tolower(barrio)));
  nombresCorredores <- as.character(factor(unique(corredores$barrio)));
  encabezadoCorredor <- contarEspecies(sort(nombresCorredores), corredores, conteo$especifico);

  instituciones <- subset(comuna, !grepl("^ninguno|estadio", tolower(institucion)));
  instituciones$barrio <- NULL;
  nombresInstituciones <- as.character(factor(unique(instituciones$institucion)));
  encabezadoInstitucion <- contarEspecies(sort(nombresInstituciones), instituciones, conteo$especifico);

  save.xlsx(especies$informeEspecifico, especiesComunas, encabezadoBarrio, encabezadoCorredor, encabezadoInstitucion);
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