guiMenuPrincipal <- function(){
	ventanaMenuPrincipal <<- tktoplevel(bg="white", width=500, height=500)
	tkwm.title(ventanaMenuPrincipal, "GInfo")
	tkwm.maxsize(ventanaMenuPrincipal, 500, 500)
	tkwm.minsize(ventanaMenuPrincipal, 500, 500)
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
		command = function() guiGenerarInformes()
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