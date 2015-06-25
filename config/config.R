configurarEntorno <- function(){
  suppressMessages(require(gdata));
  Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_45');
  require(xlsx, quietly = TRUE);
  require(tcltk);
  configurarVaribales();
}
configurarVaribales <- function(){
  CHECK <<- 1;
  SUM <<- 2;

  valoresCheckBox <<- list(
    cb1 = tclVar("1"),
    cb2 = tclVar("1"),
    cb3 = tclVar("1"),
    cb4 = tclVar("1"),
    cb5 = tclVar("1"),
    cb6 = tclVar("1"),
    cb7 = tclVar("1"),
    cb8 = tclVar("1"),
    cb9 = tclVar("1"),
    cb10 = tclVar("1"),
    cb11 = tclVar("1"),
    cb12 = tclVar("1"),
    cb13 = tclVar("1"),
    cb14 = tclVar("1"),
    cb15 = tclVar("1"),
    cb16 = tclVar("1"),
    cb17 = tclVar("1"),
    cb18 = tclVar("1"),
    cb19 = tclVar("1"),
    cb20 = tclVar("1"),
    cb21 = tclVar("1"),
    cb22 = tclVar("1"),
    cb23 = tclVar("1"),
    cb24 = tclVar("1")
  )

  conteo <<- list(
    general = 1,
    especifico = 2,
    total = "total",
    limite = "limite"
  );
  informe <<- list(
    familia = "F17_familias.xlsx",
    comuna = "F18_general_comuna.xlsx",
    familiaEncabezado = c("Comuna", "Barrios", "Corredores", "Instituciones"),
    encabezado = c("Barrios", "Instituciones", "Corredor Vial"),
    general = "general",
    especifico = "especifico"
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
    encabezado = c("estadoNatural", "deficienciaNutricional", "noRegistra"),
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
    informeEspecifico = "F10_especies.xlsx",
    encabezado = "Barrios"
  );
  procedencia <<- list(
    informeGeneral = "F11_procedencia.xlsx",
    informeEspecifico = "F29_procedencia.xlsx",
    dominio = "procedencia",
    encabezado = c("Nativa", "Exótica"),
    nativa = 1,
    exotica = 2
  );
  habito <<- list(
    informeGeneral = "F12_habito_crecimiento.xlsx",
    informeEspecifico = "F30_habito_crecimiento.xlsx",
    dominio = "habito",
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
  dasometria <<- list(
    alturas = "F14_alturas.xlsx",
    diametros = "F15_diametros.xlsx",
    volumen = "F16_volumen.xlsx"
  );
  propiedades <<- list(
    informeFisicas = "F7_propiedades_fisicas.xlsx",
    informeSanitarias = "F8_propiedades_sanitarias.xlsx",
    informeRiesgos = "F9_riesgos.xlsx",
    dominio = "porcentaje",
    fisicas = list(
      "20" = "inclinacion",
      "21" = "raizDescubierta",
      "22" = "danoMecanico",
      "23" = "bifurcacionBasal",
      "24" = "afectacionBasal"
    ),
    sanitarias = list(
      "27" = "presenciaInsectos", 
      "28" = "presenciaHongos", 
      "29" = "presenciaAgallas", 
      "32" = "pudricionLocalizada", 
      "33" = "presenciaEpifitas", 
      "34" = "presenciaParasitas", 
      "35" = "presenciaObjetos"
    ),
    riesgos = list(
      "48" = "riesgoVolcamiento",
      "49" = "riesgoRamas",
      "50" = "riesgoElementos"
    )
  );
}