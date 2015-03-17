leerArchivo <- function(){
  configurarEntorno();
  comuna <- read.xls("data/comuna-10.xls");
  densidadFollajeGeneral(comuna);
  densidadFollajeEspecifico(comuna);
  emplazamientoEspecifico(comuna);
}
dominio <- function(dataFrame, dominio, indice, operacion = NULL){
  if(operacion == 1){
    switch(dominio, 
      densidad={    
        if(indice %in% colnames(dataFrame)){
          return(dataFrame[[as.character(indice)]]);
        }
        return(0);            
      },
      emplazamiento={      
        if(indice %in% colnames(dataFrame)){
          return(dataFrame[[as.character(indice)]]);
        }
        return(0);     
      }
    );
  } else if(operacion == 2){
    switch(dominio, 
      densidad={    
        if(indice %in% colnames(dataFrame)){
          return(round(dataFrame[[as.character(indice)]]/sum(dataFrame[[as.character(indice)]]), 4));
        }
        return(0);
      },
      emplazamiento={      
        if(indice %in% colnames(dataFrame)){
          return(round(dataFrame[[as.character(indice)]]/sum(dataFrame[[as.character(indice)]]), 4));
        }
        return(0);
      }
    );
  }  
}
save.xlsx <- function (file, ...){  
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
  print(paste("El archivo", file, "tiene", nobjects, "subhojas."))
}