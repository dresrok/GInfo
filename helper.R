leerArchivo <- function(){  
  comuna <- read.xls("data/comuna4.xls", perl = 'C:\\Perl64\\bin\\perl.exe');
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
#Inicio Nuevos
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
getMasAbundante <- function(dataFrame){
  topBarrio <- as.data.frame(
    unique(sort(dataFrame$barrio)),
    stringsAsFactors=FALSE
  );
  colnames(topBarrio) <- "Var1";
  especiesBarrio <- as.data.frame(
    table(dataFrame$nom_cientifico),
    row.names = NULL,
    stringsAsFactors=FALSE
  );
  especiesMasAbundantes <- especiesBarrio[order(-especiesBarrio$Freq),];
  especiesMasAbundantes <- especiesMasAbundantes[1:15,];

  for (i in 1:nrow(especiesMasAbundantes)){
    for (j in 1:nrow(topBarrio)){
      numIndividuos <- subset(dataFrame, barrio == topBarrio$Var1[j] & nom_cientifico == especiesMasAbundantes$Var1[i])
      topBarrio[j,i+1] = nrow(numIndividuos);
    }
  }
  encabezado <- c(especies$encabezado, especiesMasAbundantes$Var1);
  colnames(topBarrio) <- encabezado;
  return(topBarrio);
}
getDensidadFollaje <- function(dataFrame){
  tmpDensidadFollaje <- as.data.frame.matrix(
    table(dataFrame$densidad_follaje, dataFrame$habito_crecimiento)
  );
  tmpDensidadFollaje$densidadFollaje <- encabezado(densidad$encabezado, row.names(tmpDensidadFollaje));
  densidadFollaje <- data.frame(
    densidadFollaje = tmpDensidadFollaje$densidadFollaje,
    arboles = tmpDensidadFollaje$"1",
    arbustos = tmpDensidadFollaje$"2",
    palmas = tmpDensidadFollaje$"3"
  );
  densidadFollaje$totalIndividuos <- densidadFollaje$arboles + densidadFollaje$arbustos + densidadFollaje$palmas;
  return(densidadFollaje);
}
getEmplazamiento <- function(dataFrame){
  tmpEmplazamiento <- as.data.frame.matrix(
    table(dataFrame$emplazamiento, dataFrame$habito_crecimiento)
  );
  tmpEmplazamiento$emplazamiento <- encabezado(emplazamiento$encabezado, row.names(tmpEmplazamiento));
  emplazamientoIndividuos <- data.frame(
    emplazamiento = tmpEmplazamiento$emplazamiento,
    arboles = tmpEmplazamiento$"1",
    arbustos = tmpEmplazamiento$"2",
    palmas = tmpEmplazamiento$"3"
  );
  emplazamientoIndividuos$totalIndividuos <- emplazamientoIndividuos$arboles + emplazamientoIndividuos$arbustos + emplazamientoIndividuos$palmas;
  return(emplazamientoIndividuos);
}
getEstadoFisico <- function(dataFrame){
  tmpEstadoFisico <- as.data.frame.matrix(
    table(dataFrame$estado_fisico, dataFrame$habito_crecimiento)
  );
  tmpEstadoFisico$estadoFisico <- encabezado(estadoFisico$encabezado, row.names(tmpEstadoFisico));
  estadoFisico <- data.frame(
    estadoFisico = tmpEstadoFisico$estadoFisico,
    arboles = tmpEstadoFisico$"1",
    arbustos = tmpEstadoFisico$"2",
    palmas = tmpEstadoFisico$"3"
  );
  estadoFisico$totalIndividuos <- estadoFisico$arboles + estadoFisico$arbustos + estadoFisico$palmas;
  return(estadoFisico);
}
getEstadoHoja <- function(dataFrame){
  tmpClorotica <- as.data.frame.list(
    table(dataFrame$hc), row.names = estadoHoja$clorotica
  );
  tmpClorotica <- checkEstadoHoja(tmpClorotica);
  colnames(tmpClorotica) <- estadoHoja$encabezado;
  clorotica <- tmpClorotica;

  tmpCaducifolia <- as.data.frame.list(
    table(dataFrame$hcf), row.names = estadoHoja$caducifolia
  );
  tmpCaducifolia <- checkEstadoHoja(tmpCaducifolia);
  colnames(tmpCaducifolia) <- estadoHoja$encabezado;
  caducifolia <- tmpCaducifolia;

  estadoHoja <- rbind(clorotica, caducifolia);
  estadoHoja$individuos <- estadoHoja$estadoNatural + estadoHoja$deficienciaNutricional + estadoHoja$noRegistra;
  estadoHoja$noRegistra <- NULL
  return(estadoHoja);
}
checkEstadoHoja <- function(dataFrame){
  encabezado <- c("X1", "X2", "X3");
  columnas <- colnames(dataFrame);
  indice <- which(!encabezado %in% columnas);
  if(length(indice) > 0){
    for (i in 1:length(indice)){
      nuevaColumna <- encabezado[indice[i]];
      dataFrame[[nuevaColumna]] <- 0;
    }    
  }
  dataFrame <- dataFrame[,order(names(dataFrame))];
  return(dataFrame);
}
getEstadoSanitario <- function(dataFrame){
  tmpEstadoSanitario <- as.data.frame.matrix(
    table(dataFrame$estado_sanitario, dataFrame$habito_crecimiento)
  );
  tmpEstadoSanitario$estadoSanitario <- encabezado(estadoSanitario$encabezado, row.names(tmpEstadoSanitario));
  estadoSanitario <- data.frame(
    estadoSanitario = tmpEstadoSanitario$estadoSanitario,
    arboles = tmpEstadoSanitario$"1",
    arbustos = tmpEstadoSanitario$"2",
    palmas = tmpEstadoSanitario$"3"
  );
  estadoSanitario$totalIndividuos <- estadoSanitario$arboles + estadoSanitario$arbustos + estadoSanitario$palmas;
  return(estadoSanitario);
}
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
        for (j in 1:nrow(tmpAlturaTotal)){
          cadena <- tmpAlturaTotal$rangos[j];
          cadenaRango <- substr(cadena, 2, nchar(cadena)-1);
          listaRango <- strsplit(cadenaRango, ",");
          r1 <- as.double(listaRango[[1]][1]);
          r2 <- as.double(listaRango[[1]][2]);
          if(j == 1){
            especies <- factor(subset(sector$nom_cientifico, sector$altura_total >= r1 & sector$altura_total <= r2));
          } else{
            especies <- factor(subset(sector$nom_cientifico, sector$altura_total > r1 & sector$altura_total <= r2));
          }
          dfEspecies <- as.data.frame(table(especies));
          especieMasComun <- as.character(
            dfEspecies[order(-dfEspecies$Freq),1][1]
          );
          tmpAlturaTotal$nombreComun[j] <- as.character(
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
        r1 <- as.double(listaRango[[1]][1]);
        r2 <- as.double(listaRango[[1]][2]);
        if(i == 1){
          especies <- factor(subset(dataFrame$nom_cientifico, dataFrame$altura_fuste >= r1 & dataFrame$altura_fuste <= r2));
        } else {
          especies <- factor(subset(dataFrame$nom_cientifico, dataFrame$altura_fuste > r1 & dataFrame$altura_fuste <= r2));
        }
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
getDiametros <- function(dataFrame, opcion){
  switch(opcion,
    "1"={
      maxHabito <- max(dataFrame$habito_crecimiento);
      diametroNormal <- data.frame();
      for (i in 1:maxHabito) {
        sector <- subset(dataFrame, habito_crecimiento == i);
        inferior <- min(sector$diametro_normal);
        superior <- max(sector$diametro_normal);
        sturges <- (1 + 3.322) * log10(nrow(sector));
        clase <- (superior-inferior)/sturges
        rangos <- cut(
          sector$diametro_normal, 
          breaks = seq(inferior, superior+clase, by = clase), 
          include.lowest = TRUE
        );
        tmpDiametroNormal <- as.data.frame(
          table(rangos),
          stringsAsFactors=FALSE
        );
        colnames(tmpDiametroNormal) <- c("rangos", "individuos");
        for (j in 1:nrow(tmpDiametroNormal)){
          cadena <- tmpDiametroNormal$rangos[j];
          cadenaRango <- substr(cadena, 2, nchar(cadena)-1);
          listaRango <- strsplit(cadenaRango, ",");
          r1 <- as.double(listaRango[[1]][1]);
          r2 <- as.double(listaRango[[1]][2]);
          if(j == 1){
            especies <- factor(subset(sector$nom_cientifico, sector$diametro_normal >= r1 & sector$diametro_normal <= r2));
          } else{
            especies <- factor(subset(sector$nom_cientifico, sector$diametro_normal > r1 & sector$diametro_normal <= r2));
          }
          dfEspecies <- as.data.frame(table(especies));
          especieMasComun <- as.character(
            dfEspecies[order(-dfEspecies$Freq),1][1]
          );
          tmpDiametroNormal$nombreComun[j] <- as.character(
            sector$nom_comun[ sector$nom_cientifico == especieMasComun ][1]
          );
        }
        sep <- data.frame(
          rangos = "Total",
          individuos = sum(as.integer(tmpDiametroNormal$individuos)),
          nombreComun = "---",
          stringsAsFactors=FALSE
        );
        tmpDiametroNormal <- rbind(tmpDiametroNormal, sep);
        diametroNormal <- rbind(diametroNormal, tmpDiametroNormal);
      }
      return(diametroNormal);
    },
    "2"={
      diametroCopa <- data.frame();  
      inferior <- min(dataFrame$diametro_normal);
      superior <- max(dataFrame$diametro_normal);
      sturges <- (1 + 3.322) * log10(nrow(dataFrame));
      clase <- (superior-inferior)/sturges
      rangos <- cut(
        dataFrame$diametro_normal, 
        breaks = seq(inferior, superior+clase, by = clase), 
        include.lowest = TRUE
      );
      tmpDiametroCopa <- as.data.frame(
        table(rangos),
        stringsAsFactors=FALSE
      );
      colnames(tmpDiametroCopa) <- c("rangos", "individuos");
      for (i in 1:nrow(tmpDiametroCopa)){
        cadena <- tmpDiametroCopa$rangos[i];
        cadenaRango <- substr(cadena, 2, nchar(cadena)-1);
        listaRango <- strsplit(cadenaRango, ",");
        r1 <- as.double(listaRango[[1]][1]);
        r2 <- as.double(listaRango[[1]][2]);
        if(i == 1){
          especies <- factor(subset(dataFrame$nom_cientifico, dataFrame$diametro_normal >= r1 & dataFrame$diametro_normal <= r2));
        } else {
          especies <- factor(subset(dataFrame$nom_cientifico, dataFrame$diametro_normal > r1 & dataFrame$diametro_normal <= r2));
        }
        dfEspecies <- as.data.frame(table(especies));
        especieMasComun <- as.character(
          dfEspecies[order(-dfEspecies$Freq),1][1]
        );
        tmpDiametroCopa$nombreComun[i] <- as.character(
          dataFrame$nom_comun[ dataFrame$nom_cientifico == especieMasComun ][1]
        );
      }
      sep <- data.frame(
        rangos = "Total",
        individuos = sum(as.integer(tmpDiametroCopa$individuos)),
        nombreComun = "---",
        stringsAsFactors=FALSE
      );
      tmpDiametroCopa <- rbind(tmpDiametroCopa, sep);
      diametroCopa <- rbind(diametroCopa, tmpDiametroCopa);
      return(diametroCopa);
    }
  );
}
getVolumen <- function(dataFrame){
  maxHabito <- max(dataFrame$habito_crecimiento);
  volumen <- data.frame();
  for (i in 1:maxHabito) {
    sector <- subset(dataFrame, habito_crecimiento == i);
    inferior <- min(sector$volumen);
    superior <- max(sector$volumen);
    sturges <- (1 + 3.322) * log10(nrow(sector));
    clase <- (superior-inferior)/sturges
    rangos <- cut(
      sector$volumen, 
      breaks = seq(inferior, superior+clase, by = clase), 
      include.lowest = TRUE
    );
    tmpVolumen <- as.data.frame(
      table(rangos),
      stringsAsFactors=FALSE
    );
    colnames(tmpVolumen) <- c("rangos", "individuos");
    for (j in 1:nrow(tmpVolumen)){
      cadena <- tmpVolumen$rangos[j];
      cadenaRango <- substr(cadena, 2, nchar(cadena)-1);
      listaRango <- strsplit(cadenaRango, ",");
      r1 <- as.double(listaRango[[1]][1]);
      r2 <- as.double(listaRango[[1]][2]);
      if(j == 1){
        especies <- factor(subset(sector$nom_cientifico, sector$volumen >= r1 & sector$volumen <= r2));
      } else{
        especies <- factor(subset(sector$nom_cientifico, sector$volumen > r1 & sector$volumen <= r2));
      }
      dfEspecies <- as.data.frame(table(especies));
      especieMasComun <- as.character(
        dfEspecies[order(-dfEspecies$Freq),1][1]
      );
      tmpVolumen$nombreComun[j] <- as.character(
        sector$nom_comun[ sector$nom_cientifico == especieMasComun ][1]
      );
    }
    sep <- data.frame(
      rangos = "Total",
      individuos = sum(as.integer(tmpVolumen$individuos)),
      nombreComun = "---",
      stringsAsFactors=FALSE
    );
    tmpVolumen <- rbind(tmpVolumen, sep);
    volumen <- rbind(volumen, tmpVolumen);
  }
  return(volumen);
}
getPropiedadesFisicas <- function(dataFrame){
  propiedadesFisicas <- data.frame(0);
  for (i in 20:24) {
    tmpPropiedadesFisicas <- as.data.frame(
      table(dataFrame[[i]]),
      stringsAsFactors=FALSE
    );    
    for (j in 0:5){
      if(!j %in% tmpPropiedadesFisicas$Var1){
        fila <- data.frame(
          Var1 = j,
          Freq = 0,
          stringsAsFactors=FALSE
        );
        tmpPropiedadesFisicas <- rbind(tmpPropiedadesFisicas, fila);
        tmpPropiedadesFisicas <- tmpPropiedadesFisicas[order(tmpPropiedadesFisicas$Var1),];        
      }
    }
    colnames(tmpPropiedadesFisicas)[2] <- propiedades$fisicas[[as.character(i)]];
    if(i != 20){
      tmpPropiedadesFisicas$Var1 <- NULL;
    }   
    propiedadesFisicas <- cbind(propiedadesFisicas, tmpPropiedadesFisicas);
  }
  propiedadesFisicas$X0 <- NULL;
  colnames(propiedadesFisicas)[1] <- propiedades$dominio;
  filaTotal <- data.frame(
    porcentaje = "Total",
    inclinacion = sum(propiedadesFisicas$inclinacion),
    raizDescubierta = sum(propiedadesFisicas$raizDescubierta),
    danoMecanico = sum(propiedadesFisicas$danoMecanico),
    bifurcacionBasal = sum(propiedadesFisicas$bifurcacionBasal),
    afectacionBasal = sum(propiedadesFisicas$afectacionBasal)
  );
  propiedadesFisicas <- rbind(propiedadesFisicas, filaTotal);
  return(propiedadesFisicas[2:7,]);
}
getPropiedadesSanitarias <- function(dataFrame){
  propiedadesSanitarias <- data.frame(0);
  for (i in c(27, 28, 29, 32, 33, 34, 35)) {
    tmpPropiedadesSanitarias <- as.data.frame(
      table(dataFrame[[i]]),
      stringsAsFactors=FALSE
    );    
    for (j in 0:5){
      if(!j %in% tmpPropiedadesSanitarias$Var1){
        fila <- data.frame(
          Var1 = j,
          Freq = 0,
          stringsAsFactors=FALSE
        );
        tmpPropiedadesSanitarias <- rbind(tmpPropiedadesSanitarias, fila);
        tmpPropiedadesSanitarias <- tmpPropiedadesSanitarias[order(tmpPropiedadesSanitarias$Var1),];        
      }
    }
    colnames(tmpPropiedadesSanitarias)[2] <- propiedades$sanitarias[[as.character(i)]];
    if(i != 27){
      tmpPropiedadesSanitarias$Var1 <- NULL;
    }   
    propiedadesSanitarias <- cbind(propiedadesSanitarias, tmpPropiedadesSanitarias);
  }
  propiedadesSanitarias$X0 <- NULL;
  colnames(propiedadesSanitarias)[1] <- propiedades$dominio;
  filaTotal <- data.frame(
    porcentaje = "Total",
    presenciaInsectos = sum(propiedadesSanitarias$presenciaInsectos),
    presenciaHongos = sum(propiedadesSanitarias$presenciaHongos),
    presenciaAgallas = sum(propiedadesSanitarias$presenciaAgallas),
    pudricionLocalizada = sum(propiedadesSanitarias$pudricionLocalizada),
    presenciaEpifitas = sum(propiedadesSanitarias$presenciaEpifitas),
    presenciaParasitas = sum(propiedadesSanitarias$presenciaParasitas),
    presenciaObjetos = sum(propiedadesSanitarias$presenciaObjetos)
  );
  propiedadesSanitarias <- rbind(propiedadesSanitarias, filaTotal);
  return(propiedadesSanitarias[2:7,]);
}
getConflictos <- function(dataFrame){
  tmpConflictos <- contarConflictos(dataFrame, conteo$general, darValor(dataFrame, conteo$limite));  
  total <- darValor(dataFrame, conteo$total);
  conflictos <- data.frame(
    conflictos = conflictos$nombres,
    sinConflicto = tmpConflictos$sinConflicto,
    xsi = round(tmpConflictos$sinConflicto/total, 4),
    conConflicto = tmpConflictos$conConflicto,
    xno = round(tmpConflictos$conConflicto/total, 4),
    individuos = tmpConflictos$sinConflicto + tmpConflictos$conConflicto
  );
  conflictos$sinConflicto <- NULL;
  conflictos$xsi <- NULL;
  return(conflictos);
}
getRiesgos <- function(dataFrame){
  riesgos <- data.frame(0);
  for (i in 48:50) {
    tmpRiesgos <- as.data.frame(
      table(dataFrame[[i]]),
      stringsAsFactors=FALSE
    );    
    for (j in 0:5){
      if(!j %in% tmpRiesgos$Var1){
        fila <- data.frame(
          Var1 = j,
          Freq = 0,
          stringsAsFactors=FALSE
        );
        tmpRiesgos <- rbind(tmpRiesgos, fila);
        tmpRiesgos <- tmpRiesgos[order(tmpRiesgos$Var1),];        
      }
    }
    colnames(tmpRiesgos)[2] <- propiedades$riesgos[[as.character(i)]];
    if(i != 48){
      tmpRiesgos$Var1 <- NULL;
    }   
    riesgos <- cbind(riesgos, tmpRiesgos);
  }
  riesgos$X0 <- NULL;
  colnames(riesgos)[1] <- propiedades$dominio;
  filaTotal <- data.frame(
    porcentaje = "Total",
    riesgoVolcamiento = sum(riesgos$riesgoVolcamiento),
    riesgoRamas = sum(riesgos$riesgoRamas),
    riesgoElementos = sum(riesgos$riesgoElementos)
  );
  riesgos <- rbind(riesgos, filaTotal);
  return(riesgos[2:7,]);
}
#Fin Nuevos
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