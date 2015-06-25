cargarArchivo <- function(){
	rutaArchivo <- tclvalue(tkgetOpenFile())
	if(validarArchivo(rutaArchivo)){
		tclvalue(nombreArchivo) <- archivo
		tclvalue(estadoCarga) <- "  Archivo cargado"
		tkconfigure(btnGenerarReportes, state="normal")
		tkconfigure(btnCerrar, state="normal")
	}
}
validarArchivo <- function(rutaArchivo){
	validacion = FALSE
	tclvalue(estadoCarga) <- "  Cargando archivo..."
	if(!nchar(rutaArchivo)){
		tclvalue(estadoCarga) <- "  Archivo sin seleccionar"
		tkmessageBox(title = "Advertencia", message = "No seleccionó ningún archivo. \nPor favor intente de nuevo!", icon = "warning", type = "ok") 
		return(validacion)
	} else {
		archivo <<- basename(rutaArchivo)
		extensiones <- grepl("*xlsx|xls", archivo)
		if(extensiones){			
			tkconfigure(btnCargarArchivo, state="disabled")
			tkconfigure(btnCerrar, state="disabled")
			comuna <<- leerArchivo(rutaArchivo)
			tkmessageBox(title = "Mensaje", message = "El archivo fue cargado exitosamente!", type = "ok")
			validacion = TRUE
			return(validacion)
		} else{
			tclvalue(estadoCarga) <- "  Archivo no compatible"
			rutaArchivo <- NULL
			tkmessageBox(title = "Error", message = "Tipo de archivo no válido.\nIntente de nuevo con un archivo Excel!", icon = "error", type = "ok")  
			return(validacion)
		}
	}
}

generarInformes <- function(){
	contador <- 0
	if(tclvalue(valoresCheckBox$cb1) == "1"){
		densidadFollajeGeneral(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb2) == "1"){
		emplazamientoGeneral(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb3) == "1"){
		estadoFisicoGeneral(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb4) == "1"){
		estadoHojaGeneral(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb5) == "1"){
		estadoSanitarioGeneral(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb6) == "1"){
		valorEsteticoGeneral(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb7) == "1"){
		propiedadesFisicas(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb8) == "1"){
		propiedadesSanitarias(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb9) == "1"){
		riesgos(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb10) == "1"){
		especiesEspecifico(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb11) == "1"){
		procedenciaGeneral(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb12) == "1"){
		habitoGeneral(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb13) == "1"){
		conflictoGeneral(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb14) == "1"){
		alturas(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb15) == "1"){
		diametros(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb16) == "1"){
		volumen(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb17) == "1"){
		familiasGeneral(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb18) == "1"){
		general(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb19) == "1"){
		densidadFollajeEspecifico(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb20) == "1"){
		emplazamientoEspecifico(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb21) == "1"){
		estadoFisicoEspecifico(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb22) == "1"){
		estadoHojaEspecifico(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb23) == "1"){
		estadoSanitarioEspecifico(comuna)
		contador <- contador+1
	}
	if(tclvalue(valoresCheckBox$cb24) == "1"){
		valorEsteticoEspecifico(comuna)
		contador <- contador+1
	}
	mensajeInformes <- paste("Se han generado un total de ", contador, " informe(s), los cuales\nse encuentran en la carpeta 'data' de la aplicación", sep="")
	tkmessageBox(title = "Mensaje", message = mensajeInformes, type = "ok")
}