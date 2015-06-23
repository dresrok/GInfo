cargarArchivo <- function(){
	rutaArchivo <- tclvalue(tkgetOpenFile())
	if(validarArchivo(rutaArchivo)){
		tclvalue(nombreArchivo) <- archivo
		tclvalue(estadoCarga) <- "Estado: Archivo cargado"
		tkconfigure(btnGenerarReportes, state="normal")
	}
}
validarArchivo <- function(rutaArchivo){
	validacion = FALSE
	if(!nchar(rutaArchivo)){
		tclvalue(estadoCarga) <- "Estado: Archivo no seleccionado"
		tkmessageBox(title = "Advertencia", message = "No seleccionó ningún archivo. \nPor favor intente de nuevo!", icon = "warning", type = "ok") 
		return(validacion)
	} else {
		archivo <<- basename(rutaArchivo)
		extensiones <- grepl("*xlsx|xls", archivo)
		if(extensiones){
			tclvalue(estadoCarga) <- "Estado: Cargando archivo..."
			tkconfigure(btnCargarArchivo, state="disabled")			
			comuna <<- leerArchivo(rutaArchivo)
			tkmessageBox(title = "Mensaje", message = "El archivo fue cargado exitosamente!", type = "ok")
			validacion = TRUE
			return(validacion)
		} else{
			tclvalue(estadoCarga) <- "Estado: Archivo no compatible"
			rutaArchivo <- NULL
			tkmessageBox(title = "Error", message = "Tipo de archivo no válido.\nIntente de nuevo con un archivo Excel!", icon = "error", type = "ok")  
			return(validacion)
		}
	}
}

generarInformes <- function(){
	if(tclvalue(valoresCheckBox$cb1) == "1"){
		densidadFollajeGeneral(comuna);
	}
	if(tclvalue(valoresCheckBox$cb2) == "1"){
		emplazamientoGeneral(comuna);
	}
	if(tclvalue(valoresCheckBox$cb3) == "1"){
		estadoFisicoGeneral(comuna);
	}
	if(tclvalue(valoresCheckBox$cb4) == "1"){
		estadoHojaGeneral(comuna);
	}
	if(tclvalue(valoresCheckBox$cb5) == "1"){
		estadoSanitarioGeneral(comuna);
	}
	if(tclvalue(valoresCheckBox$cb6) == "1"){
		valorEsteticoGeneral(comuna);
	}
	if(tclvalue(valoresCheckBox$cb7) == "1"){
		propiedadesFisicas(comuna);
	}
	if(tclvalue(valoresCheckBox$cb8) == "1"){
		propiedadesSanitarias(comuna);
	}
	if(tclvalue(valoresCheckBox$cb9) == "1"){
		riesgos(comuna);
	}
	if(tclvalue(valoresCheckBox$cb11) == "1"){
		procedenciaGeneral(comuna);
	}
	if(tclvalue(valoresCheckBox$cb12) == "1"){
		habitoGeneral(comuna);
	}
	if(tclvalue(valoresCheckBox$cb13) == "1"){
		conflictoGeneral(comuna);
	}
	if(tclvalue(valoresCheckBox$cb14) == "1"){
		alturas(comuna);
	}
	if(tclvalue(valoresCheckBox$cb15) == "1"){
		diametros(comuna);
	}
	if(tclvalue(valoresCheckBox$cb16) == "1"){
		volumen(comuna);
	}
	if(tclvalue(valoresCheckBox$cb17) == "1"){
		familiasGeneral(comuna);
	}
	if(tclvalue(valoresCheckBox$cb18) == "1"){
		general(comuna);
	}
	if(tclvalue(valoresCheckBox$cb19) == "1"){
		densidadFollajeEspecifico(comuna);
	}
	if(tclvalue(valoresCheckBox$cb20) == "1"){
		emplazamientoEspecifico(comuna);
	}
	if(tclvalue(valoresCheckBox$cb21) == "1"){
		estadoFisicoEspecifico(comuna);
	}
	if(tclvalue(valoresCheckBox$cb22) == "1"){
		estadoHojaEspecifico(comuna);
	}
	if(tclvalue(valoresCheckBox$cb23) == "1"){
		estadoSanitarioEspecifico(comuna);
	}
	if(tclvalue(valoresCheckBox$cb24) == "1"){
		valorEsteticoEspecifico(comuna);
	}
}