require(tcltk)

guiMenuPrincipal <- function(){
	ventanaMenuPrincipal <<- tktoplevel(bg="white", width=400, height=300)
	tkwm.title(ventanaMenuPrincipal, "GInfo")
	tkwm.maxsize(ventanaMenuPrincipal, 400, 300)
	tkwm.minsize(ventanaMenuPrincipal, 400, 300)
	barraMenu <- tkmenu(ventanaMenuPrincipal)
	tkconfigure(ventanaMenuPrincipal, menu=barraMenu)
	menuArchivo <- tkmenu(barraMenu, tearoff=FALSE)
	menuComuna <- tkmenu(barraMenu, tearoff=FALSE)
	menuAyuda <- tkmenu(barraMenu, tearoff=FALSE)

	# Menú Archivo
	tkadd(
		menuArchivo, "command",
		label = "Salir",
		command = function() tkdestroy(ventanaMenuPrincipal)
	)
	tkadd(
		barraMenu, "cascade",
		label = "Archivo",
		menu = menuArchivo
	)
	# Menú Comuna
	tkadd(
		menuComuna, "command",
		label = "Generar informes",
		command = function() tkdestroy(ventanaMenuPrincipal)
	)
	tkadd(
		barraMenu, "cascade",
		label = "Comuna",
		menu = menuComuna
	)
	# Menú Ayuda
	tkadd(
		menuAyuda, "command",
		label = "Acerca de GInfo",
		command = function() guiAcercaDe()
	)
	tkadd(
		barraMenu, "cascade",
		label = "Ayuda",
		menu = menuAyuda
	)
	tkfocus(ventanaMenuPrincipal)
}

guiGenerarInformes <- function(){
	ventanaGenerarInformes <- tktoplevel(parent=ventanaMenuPrincipal, bg="white", width=400, height=300)
	tkwm.title(ventanaGenerarInformes, "Generar informes")	
}

guiAcercaDe <- function(){
	ventanaAcercaDe <- tktoplevel(parent=ventanaMenuPrincipal, bg="white", width=285, height=280)
	tkwm.title(ventanaAcercaDe, "Acerca de GInfo")
	tkwm.maxsize(ventanaAcercaDe, 285, 275)
	tkwm.minsize(ventanaAcercaDe, 285, 275)	

	estiloTitulo 	<- tkfont.create(family="times", size=22, weight="bold")
	estiloVersionAutor <- tkfont.create(family="times", size=10, slant="italic")	
	estiloRepositorio <- tkfont.create(family="times", size=10, weight="bold", slant="italic")

	lblTitulo		<- tklabel(ventanaAcercaDe, bg="white", text="GInfo", font=estiloTitulo)
	lblVersion 	<- tklabel(ventanaAcercaDe, bg="white", text="Versión 0.5.9 - 2015 MIT License", font=estiloVersionAutor)
	lblRepositorio <- tklabel(ventanaAcercaDe, bg="white", text="https://github.com/paleox/GInfo", font=estiloRepositorio)
	lblAutor 		<- tklabel(ventanaAcercaDe, bg="white", text="Andrés Felipe Santos - andresfesantos@gmail.com", font=estiloVersionAutor)	

	cuerpo <- "Esta aplicación fue desarrollada para automatizar la\nelaboración de los informes en formato crudo para la\nestructuración de los planes de silvicultura de las\ncomunas de Ibagué."
	lblCuerpo 	<- tklabel(ventanaAcercaDe, bg="white", text=cuerpo)	

	btnAceptar <- tkbutton(
		ventanaAcercaDe,
		text="Aceptar",
		command = function() tkdestroy(ventanaAcercaDe)
	)	

	tkgrid(lblTitulo)
	tkgrid(tklabel(ventanaAcercaDe,bg="white", text="    "))
	tkgrid(lblVersion, sticky="w")
	tkgrid(lblRepositorio, sticky="w")
	tkgrid(lblAutor, sticky="w")	
	tkgrid(tklabel(ventanaAcercaDe,bg="white", text="    "))
	tkgrid(lblCuerpo)
	tkgrid(tklabel(ventanaAcercaDe,bg="white", text="    "))
	tkgrid(btnAceptar)
	tkfocus(ventanaAcercaDe)
}