source('~/R/GInfo/config/config.R', encoding = 'UTF-8')
source('~/R/GInfo/helpers/helperInformes.R');
source('~/R/GInfo/helpers/helperGui.R', encoding = 'UTF-8');
source('~/R/GInfo/utils/informesGenerales.R');
source('~/R/GInfo/utils/informesEspecificos.R');
source('~/R/GInfo/views/guiMenuPrincipal.R');
source('~/R/GInfo/views/guiGenerarInformes.R');
source('~/R/GInfo/views/guiAcercaDe.R', encoding = 'UTF-8');

app <- function(){
  configurarEntorno();
  guiMenuPrincipal();
}

app()