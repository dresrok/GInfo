# GInfo
Esta aplicación fue desarrollada para automatizar la elaboración de los informes en formato crudo para la estructuración de los planes de silvicultura de las Comunas del Censo del Arbolado Urbano de Ibagué.

## Instalación
Descargue el repositorio, cree una carpeta llamada **`R`** dentro de **`Mis Documentos`** de tal forma que el proyecto al momento de ser extraído quede ubicado en la siguiente dirección `../Documentos/R/GInfo`

## Dependencias
Versión de R: `>= 3.2.0`
<br>
Versión de RStudio: `>= 0.99.441`
<br>
Versión librería tcltk: `>= 8.5.8`

Debe instalar los siguientes paquetes:

```R
install.packages("gdata")
install.packages("xlsx")
```

Si no tiene instalado **JDK (Java Development Kit)**, debe hacerlo. Posteriormente debe configurar la variable global de Java en la aplicación, en la carpeta `GInfo/config` modificando el archivo `config.R`

```R
configurarEntorno <- function(){
  suppressMessages(require(gdata));
  Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_45');
  require(xlsx, quietly = TRUE);
  require(tcltk);
  configurarVaribales();
}
```

Cambiando la variable `JAVA_HOME` por la dirección de instalación de su equipo.

#Ejecución
Para abrir la aplicación con **RStudio** por favor de doble clic sobre el archivo **`GInfo.Rproj`** y ejecute en la interfaz del **IDE** el archivo **`app.R`**

Licencia
-------
[MIT license](http://opensource.org/licenses/MIT)