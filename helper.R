leerArchivo <- function(){  
  comuna <- read.xls("data/comuna11.xls", perl = 'C:\\Perl64\\bin\\perl.exe');
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
#Nuevo 1
getFamilias <- function(dataFrame){
  tmpFamilias <- as.data.frame(
    table(dataFrame$familia)
  );
  tmpFamilias <- tmpFamilias[order(-tmpFamilias$Freq),];
  familias <- data.frame(
    familia = tmpFamilias$Var1[1:10],
    individuos = tmpFamilias$Freq[1:10],
    xi = round(tmpFamilias$Freq[1:10]/sum(tmpFamilias$Freq), 4)
  );
  totalEspecies <- length(unique(dataFrame$nom_cientifico));
  for (i in 1:length(familias$familia)){
    especies <- unique(dataFrame$nom_cientifico[ dataFrame$familia == familias$familia[i] ]);
    numeroEspecies <- length(especies);
    familias$especies[i] <- numeroEspecies;
    familias$xe[i] = round(familias$especies[i]/totalEspecies, 4);
  }
  familias <- familias[c(1,4,5,2,3)];
  filaTotal <- data.frame(
    familia = "Total",
    especies = totalEspecies,
    xe = 100,
    individuos = nrow(dataFrame),
    xi = 100
  );
  familias <- rbind(familias, filaTotal);
  return(familias);
}
#Fin nuevo 1
#Nuevo 2
getEspeciesProcedencia <- function(dataFrame){
  tmpProcedencia <- as.data.frame.matrix(
    table(dataFrame$procedencia, dataFrame$habito_crecimiento)
  );
  tmpProcedencia$procedencia <- procedencia$encabezado;
  procedencia <- data.frame(
    procedencia = tmpProcedencia$procedencia,
    arboles = tmpProcedencia$"1",
    arbustos = tmpProcedencia$"2",
    palmas = tmpProcedencia$"3"
  );
  maxProcedencia <- max(dataFrame$procedencia);
  maxHabito <- max(dataFrame$habito_crecimiento);
  especies <- data.frame(0);
  for (i in 1:maxProcedencia){
    for (j in 1:maxHabito){
      tmpEspecies <- length(unique(subset(dataFrame$nom_cientifico, dataFrame$procedencia == i & dataFrame$habito_crecimiento == j)));
      especies[i,as.character(j)] <- tmpEspecies;
    }
  }
  #angelica moncaleano
  procedencia$especiesArboles <- especies$"1";
  procedencia$especiesArbustos <- especies$"2";
  procedencia$especiesPalmas <- especies$"3";
  procedencia <- procedencia[c(1,2,5,3,6,4,7)];
  procedencia$totalIndividuos <- procedencia$arboles + procedencia$arbustos + procedencia$palmas;
  procedencia$totalEspecies <- procedencia$especiesArboles + procedencia$especiesArbustos + procedencia$especiesPalmas;
  return(procedencia);
}
#Fin Nuevo 2
#Nuevo 3
getEspeciesHabito <- function(dataFrame){
  tmpHabito <- as.data.frame(
    table(dataFrame$habito_crecimiento)
  );
  habito <- data.frame(
    habito = encabezado(habito$encabezado, tmpHabito$Var1),
    individuos = tmpHabito$Freq,
    xi = round(tmpHabito$Freq/sum(tmpHabito$Freq), 4)
  );
  maxHabito <- max(dataFrame$habito_crecimiento);
  especies <- data.frame(0);
  for (i in 1:maxHabito){
    tmpEspecies <- length(unique(subset(dataFrame$nom_cientifico, dataFrame$habito_crecimiento == i)));
    especies[as.character(i),1] <- tmpEspecies;
  }
  habito$especies <- especies$X0;
  habito$xe <- round(habito$especies/sum(habito$especies), 4)
  habito <- habito[c(1,4,5,2,3)];
  filaTotal <- data.frame(
    habito = "Total",
    especies = sum(habito$especies),
    xe = sum(habito$xe),
    individuos = sum(habito$individuos),
    xi = sum(habito$xi)
  );
  habito <- rbind(habito, filaTotal);
  return(habito);
}
#Fin nuevo 3
getValorEstetico <- function(dataFrame){
  tmpValorEstetico <- as.data.frame.matrix(
    table(dataFrame$valor_estetico, dataFrame$habito_crecimiento)
  );
  tmpValorEstetico$valorEstetico <- encabezado(valorEstetico$encabezado, row.names(tmpValorEstetico));
  valorEstetico <- data.frame(
    valorEstetico = tmpValorEstetico$valorEstetico,
    arboles = tmpValorEstetico$"1",
    arbustos = tmpValorEstetico$"2",
    palmas = tmpValorEstetico$"3"
  );
  valorEstetico$totalIndividuos <- valorEstetico$arboles + valorEstetico$arbustos + valorEstetico$palmas;
  return(valorEstetico);
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
      str <- character(0);
      for (i in limite$inicio:limite$fin){      
        tmp <- as.data.frame.matrix(
          table(factor(dataFrame$barrio), dataFrame[,i])
        );
        if(!"2" %in% colnames(tmp)){
          tmp$'2' <- 0;
        }
        totalConflicto <- tmp$"1" + tmp$"2";
        tmp$"3" <- round(tmp$"1"/totalConflicto, 4);
        tmp$"4" <- round(tmp$"2"/totalConflicto, 4);
        tmp$"5" <- tmp$"1" + tmp$"2";
        tmp <- tmp[c(1,3,2,4,5)];
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
getAlturas <- function(dataFrame, opcion){
  switch(opcion,
    "1"={
      maxHabito <- max(dataFrame$habito_crecimiento);
      alturaTotal <- data.frame();
      for (i in 1:maxHabito) {
        sector <- subset(dataFrame, habito_crecimiento == i);
        inferior <- min(sector$altura_total);
        superior <- max(sector$altura_total);
        sturges <- (1 + 3.322) * log10(nrow(sector));
        clase <- (superior-inferior)/sturges
        rangos <- cut(
          sector$altura_total, 
          breaks = seq(inferior, superior+clase, by = clase), 
          include.lowest = TRUE
        );
        tmpAlturaTotal <- as.data.frame(
          table(rangos),
          stringsAsFactors=FALSE
        );
        colnames(tmpAlturaTotal) <- c("rangos", "individuos");
        for (i in 1:nrow(tmpAlturaTotal)){
          cadena <- tmpAlturaTotal$rangos[i];
          cadenaRango <- substr(cadena, 2, nchar(cadena)-1);
          listaRango <- strsplit(cadenaRango, ",");
          r1 <- as.integer(listaRango[[1]][1]);
          r2 <- as.integer(listaRango[[1]][2]);
          especies <- subset(sector$nom_cientifico, sector$altura_total >= r1 & sector$altura_total <= r2);
          dfEspecies <- as.data.frame(table(especies));
          especieMasComun <- as.character(
            dfEspecies[order(-dfEspecies$Freq),1][1]
          );
          tmpAlturaTotal$nombreComun[i] <- as.character(
            sector$nom_comun[ sector$nom_cientifico == especieMasComun ][1]
          );
        }
        sep <- data.frame(
          rangos = "Total",
          individuos = sum(as.integer(tmpAlturaTotal$individuos)),
          nombreComun = "---",
          stringsAsFactors=FALSE
        );
        tmpAlturaTotal <- rbind(tmpAlturaTotal, sep);
        alturaTotal <- rbind(alturaTotal, tmpAlturaTotal);
      }
      return(alturaTotal);
    },
    "2"={
      alturaFuste <- data.frame();  
      inferior <- min(dataFrame$altura_fuste);
      superior <- max(dataFrame$altura_fuste);
      sturges <- (1 + 3.322) * log10(nrow(dataFrame));
      clase <- (superior-inferior)/sturges
      rangos <- cut(
        dataFrame$altura_fuste, 
        breaks = seq(inferior, superior+clase, by = clase), 
        include.lowest = TRUE
      );
      tmpAlturaFuste <- as.data.frame(
        table(rangos),
        stringsAsFactors=FALSE
      );
      colnames(tmpAlturaFuste) <- c("rangos", "individuos");
      for (i in 1:nrow(tmpAlturaFuste)){
        cadena <- tmpAlturaFuste$rangos[i];
        cadenaRango <- substr(cadena, 2, nchar(cadena)-1);
        listaRango <- strsplit(cadenaRango, ",");
        r1 <- as.integer(listaRango[[1]][1]);
        r2 <- as.integer(listaRango[[1]][2]);
        especies <- subset(dataFrame$nom_cientifico, dataFrame$altura_total >= r1 & dataFrame$altura_total <= r2);
        dfEspecies <- as.data.frame(table(especies));
        especieMasComun <- as.character(
          dfEspecies[order(-dfEspecies$Freq),1][1]
        );
        tmpAlturaFuste$nombreComun[i] <- as.character(
          dataFrame$nom_comun[ dataFrame$nom_cientifico == especieMasComun ][1]
        );
      }
      sep <- data.frame(
        rangos = "Total",
        individuos = sum(as.integer(tmpAlturaFuste$individuos)),
        nombreComun = "---",
        stringsAsFactors=FALSE
      );
      tmpAlturaFuste <- rbind(tmpAlturaFuste, sep);
      alturaFuste <- rbind(alturaFuste, tmpAlturaFuste);
      return(alturaFuste);
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