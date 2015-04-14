leerArchivo <- function(){  
  comuna <- read.xls("data/comuna-10.xls");
  return(comuna);
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
      },
      estadoFisico={
        if(indice %in% colnames(dataFrame)){
          return(dataFrame[[as.character(indice)]]);
        }
        return(0); 
      },
      estadoHoja={
        if(indice %in% colnames(dataFrame)){
          return(dataFrame[[as.character(indice)]]);
        }
        return(0); 
      },
      estadoSanitario={
        if(indice %in% colnames(dataFrame)){
          return(dataFrame[[as.character(indice)]]);
        }
        return(0);
      },
      valorEstetico={
        if(indice %in% colnames(dataFrame)){
          return(dataFrame[[as.character(indice)]]);
        }
        return(0);
      },
      procedencia={
        if(indice %in% colnames(dataFrame)){
          return(dataFrame[[as.character(indice)]]);
        }
        return(0);
      },
      tipoPlanta={
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
      },
      estadoFisico={
        if(indice %in% colnames(dataFrame)){
          return(round(dataFrame[[as.character(indice)]]/sum(dataFrame[[as.character(indice)]]), 4));
        }
        return(0);
      },
      estadoHoja={
        if(indice %in% colnames(dataFrame)){
          return(round(dataFrame[[as.character(indice)]]/sum(dataFrame[[as.character(indice)]]), 4));
        }
        return(0);
      },
      estadoSanitario={
        if(indice %in% colnames(dataFrame)){
          return(round(dataFrame[[as.character(indice)]]/sum(dataFrame[[as.character(indice)]]), 4));
        }
        return(0);
      },
      valorEstetico={
        if(indice %in% colnames(dataFrame)){
          return(round(dataFrame[[as.character(indice)]]/sum(dataFrame[[as.character(indice)]]), 4));
        }
        return(0);
      },
      procedencia={
        if(indice %in% colnames(dataFrame)){
          return(round(dataFrame[[as.character(indice)]]/sum(dataFrame[[as.character(indice)]]), 4));
        }
        return(0);
      },
      tipoPlanta={
        if(indice %in% colnames(dataFrame)){
          return(round(dataFrame[[as.character(indice)]]/sum(dataFrame[[as.character(indice)]]), 4));
        }
        return(0);
      }
    );
  }
}
encabezado <- function(encabezado, data){
  nuevoEncabezado = character(0);
  for(i in 1:length(encabezado)){
    if(i %in% data){
      nuevoEncabezado <- c(nuevoEncabezado, encabezado[i]);
    }
  }
  return(nuevoEncabezado);
}
contarEspecies <- function(sector, dataFrame, conteo){
  switch(conteo,
    "1"={
      for (i in 1:length(sector$nombreCientifico)){
        sector$familia[i] <- as.character(
          dataFrame$familia[ which( dataFrame$nom_cientifico == sector$nombreCientifico[i] )[1] ]
        );
        sector$nombreComun[i] <- as.character(
          dataFrame$nom_comun[ which( dataFrame$nom_cientifico == sector$nombreCientifico[i] )[1] ]
        );
      }      
      return(sector[c(4,1,5,2,3)]);
    },
    "2"={
      tmpEspeciesSector <- data.frame();
      encabezadoSector <- data.frame();
      for (i in 1:length(sector)){
        tmpEspeciesSector <- data.frame(
          nombreCientifico = sector[i],
          abundancia = "---",
          x = "---",
          familia = "---",
          nombreComun = "---",
          stringsAsFactors=FALSE
        );
        encabezadoSector <- rbind(encabezadoSector, tmpEspeciesSector);
        if("barrio" %in% colnames(dataFrame)){
          tmpEspeciesSector <- as.data.frame(
            table(
              factor( dataFrame$nom_cientifico[ which( dataFrame$barrio == sector[i] ) ] )
            )
          );
        } else{
          tmpEspeciesSector <- as.data.frame(
            table(
              factor( dataFrame$nom_cientifico[ which( dataFrame$institucion == sector[i] ) ] )
            )
          );
        }
        especiesSector <- data.frame(
          nombreCientifico = as.character(tmpEspeciesSector$Var1),
          abundancia = tmpEspeciesSector$Freq,
          x = round(tmpEspeciesSector$Freq/sum(tmpEspeciesSector$Freq), 4),      
          stringsAsFactors=FALSE
        );    
        for (j in 1:length(especiesSector$nombreCientifico)){
          especiesSector$familia[j] <- as.character(
            dataFrame$familia[ which( dataFrame$nom_cientifico == especiesSector$nombreCientifico[j] )[1] ]
          );
          especiesSector$nombreComun[j] <- as.character(
            dataFrame$nom_comun[ which( dataFrame$nom_cientifico == especiesSector$nombreCientifico[j] )[1] ]
          );
        }
        encabezadoSector <- rbind(encabezadoSector, especiesSector);
      }
      return(encabezadoSector[c(4,1,5,2,3)])     
    }
  );
}
contarConflictos <- function(dataFrame, conteo, limite){
  switch(conteo,
    "1"={          
      tmpConflictos <- data.frame();
      for (i in limite$inicio:limite$fin){      
        tmp <- as.data.frame.list(
          table(dataFrame[i])
        );
        if(!"X2" %in% colnames(tmp)){
          tmp$X2 <- 0;
        }
        tmpConflictos <- rbind(tmpConflictos, tmp)
      }
      colnames(tmpConflictos) <- conflictos$encabezado;
      return(tmpConflictos);
    },
    "2"={
      tmpConflictos <- data.frame(0);
      for (i in limite$inicio:limite$fin){      
        tmp <- as.data.frame.matrix(
          table(dataFrame$barrio, dataFrame[,i])
        );
        if(!"2" %in% colnames(tmp)){
          tmp$'2' <- 0;
        }
        colnames(tmp) <- conflictos$encabezadoEspecifico[[as.character(i)]]
        tmpConflictos <- cbind(tmpConflictos, tmp)
        if("X0" %in% colnames(tmpConflictos)){
          tmpConflictos$X0 <- NULL;
        }
      }      
      return(tmpConflictos);
    }
  );
}
darValor <- function(dataFrame, opcion){
  switch(opcion,
    total={
      total <- nrow(dataFrame);
      return(total);
    },
    limite={
      limite <- list(
        inicio = which(colnames(dataFrame)=="cre"),
        fin = which(colnames(dataFrame)=="cap")
      );
      return(limite);
    }
  );  
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