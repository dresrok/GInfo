options(guiToolkit = "RGtk2")
library(gWidgets)

guiMenuPrincipal <- function(){
	ventanaMenuPrincipal <- gwindow("GInfo", visible=FALSE)
	menu <- list(
		# Menú Archivo
		Archivo=list(
			itemMenuSalir = gaction(
				label = "Salir",
				icon	= "quit",
				handler = function(h,...) dispose(ventanaMenuPrincipal)
			)
		),
		# Menú Comuna
		Comuna=list(
			itemGenerarInformes = gaction(
				label = "Generar Informes",
				icon = "file"
			)
		),
		# Menú Ayuda
		Ayuda=list(
			itemMenuAyuda = gaction(
				label = "Ayuda",
				icon	= "info",
				handler = function(h,...) dispose(ventanaMenuPrincipal)
			)
		)
	)	
	barraMenu <- gmenu(menu, cont=ventanaMenuPrincipal)
	barraEstado <- gstatusbar("status", cont=ventanaMenuPrincipal)
	visible(ventanaMenuPrincipal) <- TRUE
}

guiAcercaDe <- function(){
	ventanaAcercaDe <- gwindow("Acerca de GInfo", visible=FALSE, parent=ventanaMenuPrincipal)
}
	