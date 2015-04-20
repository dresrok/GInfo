configurarEntorno <- function(){
  suppressMessages(require(gdata));
  Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre8');
  require(xlsx, quietly = TRUE);  
  configurarVaribales();
}
configurarVaribales <- function(){
  CHECK <<- 1;
  SUM <<- 2;

  conteo <<- list(
    general = 1,
    especifico = 2,
    total = "total",
    limite = "limite"
  );
  informeComuna <<- list(
    informeGeneral= "F18_general_comuna.xlsx"
  );
  densidad <<- list(
    informeGeneral = "F1_densidad_follaje.xlsx",
    informeEspecifico = "F19_densidad_follaje.xlsx",
    dominio = "densidad",
    encabezado = c("Denso", "Medio", "Ralo"),
    denso = 1,
    medio = 2,
    ralo = 3
  );
  emplazamiento <<- list(
    informeGeneral = "F2_emplazamiento.xlsx",
    informeEspecifico = "F20_emplazamientos.xlsx",
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
  estadoFisico <<- list(
    informeGeneral = "F3_estado_fisico.xlsx",
    informeEspecifico = "F21_estado_fisico.xlsx",
    dominio = "estadoFisico",
    encabezado = c("Malo", "Regular", "Bueno"),
    malo = 1,
    regular = 2,
    bueno = 3
  );
  estadoHoja <<- list(
    clorotica = "Hoja Clorótica",
    caducifolia = "Hoja Caducifolia",
    informeGeneral = "F4_estado_hojas.xlsx",
    informeEspecifico = "F22_estado_hojas.xlsx",
    dominio = "estadoHoja",
    encabezado = c("Estado Natural", "Deficiencia Nutricional", "No Registra"),
    en = 1,
    dn = 2,
    nr = 3
  );
  estadoSanitario <<- list(
    informeGeneral = "F5_estado_sanitario.xlsx",
    informeEspecifico = "F23_estado_sanitario.xlsx",
    dominio = "estadoSanitario",
    encabezado = c("Muerto", "Crítico", "Enfermo", "Sano"),
    muerto = 1,
    critico = 2,
    enfermo = 3,
    sano = 4
  );
  valorEstetico <<- list(
    informeGeneral = "F6_valor_estetico.xlsx",
    informeEspecifico = "F24_valor_estetico.xlsx",
    dominio = "valorEstetico",
    encabezado = c("Emblemático", "Esencial", "Deseable", "Indiferente", "Inaceptable"),
    emb = 1,
    ese = 2,
    des = 3,
    ind = 4,
    ina = 5
  );
  especies <<- list(      
    informeEspecifico = "F28_especies.xlsx"
  );
  procedencia <<- list(
    informeGeneral = "F11_procedencia.xlsx",
    informeEspecifico = "F29_procedencia.xlsx",
    dominio = "procedencia",
    encabezado = c("Nativa", "Exótia"),
    nativa = 1,
    exotica = 2
  );
  tipoPlanta <<- list(
    informeGeneral = "F12_tipo_planta.xlsx",
    informeEspecifico = "F30_tipo_planta.xlsx",
    dominio = "tipoPlanta",
    encabezado = c("Árbol", "Arbusto", "Palma", "Bambu"),
    arbol = 1,
    arbusto = 2,
    palma = 3,
    bambu = 4
  );
  conflictos <<- list(
    informeGeneral = "F13_conflictos.xlsx",
    informeEspecifico = "F31_conflictos.xlsx",
    encabezado = c("sinConflicto", "conConflicto"),
    nombres = c("Redes eléctricas", "Redes hidráulicas", "Redes de alcantarillado",
              "Otros árboles", "Estructuras", "Infraestructura vial", "Redes telefónicas",
              "Redes de gas", "Alumbrado público"),
    encabezadoEspecifico = list(
      "36" = c("creSin","xCreSin", "creCon","xCreCon", "totalCre"),
      "37" = c("crhSin", "xCrhSin", "crhCon","xCrhCon", "totalCrh"),
      "38" = c("craSin", "xCraSin", "craCon","xCraCon", "totalCra"),
      "39" = c("coaSin", "xCoaSin", "coaCon","xCoaCon", "totalCoa"),
      "40" = c("ceSin", "xCeSin", "ceCon","xCeCon", "totalCe"),
      "41" = c("civSin", "xCivSin", "civCon","xCivCon", "totalCiv"),
      "42" = c("crtSin", "xCrtSin", "crtCon","xCrtCon", "totalCrt"),
      "43" = c("crgSin", "xCrgSin", "crgCon","xCrgCon", "totalCrg"),
      "44" = c("capSin", "xCapSin", "capCon","xCapCon", "totalCap")
    )
  );
}