cargarMenu <- function(){
	paquetes <- comprobarDependencias();
	if(length(paquetes) > 0){
		
	}
}
require(tcltk);
tkmessageBox(message = "Do you want to save before quitting?",
    icon = "question", type = "yesnocancel", default = "yes")