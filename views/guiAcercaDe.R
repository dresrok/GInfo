guiAcercaDe <- function(){
	ventanaAcercaDe <- tktoplevel(parent=ventanaMenuPrincipal, bg="white", width=300, height=280)
	tkwm.title(ventanaAcercaDe, "Acerca de GInfo")
	tkwm.maxsize(ventanaAcercaDe, 300, 275)
	tkwm.minsize(ventanaAcercaDe, 300, 275)

	estiloTitulo 	<- tkfont.create(family="times", size=22, weight="bold")
	estiloVersionAutor <- tkfont.create(family="times", size=10, slant="italic")
	estiloRepositorio <- tkfont.create(family="times", size=10, weight="bold", slant="italic")
	estiloCuerpo <- tkfont.create(family="times", size=10, slant="italic")

	lblTitulo		<- tklabel(ventanaAcercaDe, bg="white", text="GInfo", font=estiloTitulo)
	lblVersion 	<- tklabel(ventanaAcercaDe, bg="white", text="Versión 0.5.9 - 2015 MIT License", font=estiloVersionAutor)
	lblRepositorio <- tklabel(ventanaAcercaDe, bg="white", text="https://github.com/paleox/GInfo", font=estiloRepositorio)
	lblAutor 		<- tklabel(ventanaAcercaDe, bg="white", text="Andrés Felipe Santos - andresfesantos@gmail.com", font=estiloVersionAutor)

	cuerpo <- "Esta aplicación fue desarrollada para automatizar la\nelaboración de los informes en formato crudo para la\nestructuración de los planes de silvicultura de las\ncomunas de Ibagué."
	lblCuerpo 	<- tklabel(ventanaAcercaDe, bg="white", text=cuerpo, font=estiloCuerpo)

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