

# _____________________ Bajar librerias cada vez que corra el modelo
cargar_librerias <- function() {
  library(MASS)
  library(reshape)
  library(reshape2)
  library(dplyr)
  library(tidyr)
  library(writexl)
}

# _____________________ Union de Modelos de Federico
Combinacion <- function(MarcasOff,MarcasOn) {

  ListOff   <- c("Daypart","Agency","Advertiser","Product","Medium","Vehicle")
  MarcasOff <- MarcasOff[ , !(names(MarcasOff) %in% ListOff)]
  ListOn    <- c("Conca","Medium","Impressions")
  MarcasOn  <- MarcasOn[ , !(names(MarcasOn) %in% ListOn)]

  # Convertir la columna "BRAND" a mayúsculas
  MarcasOff$Brand <- toupper(MarcasOff$Brand)
  MarcasOn$Brand  <- toupper(MarcasOn$Brand)

  # Cambiar el nombre de la columna "X.U.FEFF.PAIS" a "PAIS"
  colnames(MarcasOff)[colnames(MarcasOff) == "X.U.FEFF.Country"] <- "PAIS"
  colnames(MarcasOn)[colnames(MarcasOn) == "X.U.FEFF.Country"]   <- "PAIS"

  # Cambiar el nombre de todas las columnas que quedan en el archivo
  colnames(MarcasOff)[colnames(MarcasOff) == "Year"]  <- "ANO"
  colnames(MarcasOn)[colnames(MarcasOn) == "Year"]    <- "ANO"
  colnames(MarcasOff)[colnames(MarcasOff) == "Month"] <- "MES"
  colnames(MarcasOn)[colnames(MarcasOn) == "Month"]   <- "MES"
  colnames(MarcasOff)[colnames(MarcasOff) == "Brand"] <- "MARCA"
  colnames(MarcasOn)[colnames(MarcasOn) == "Brand"]   <- "MARCA"
  colnames(MarcasOff)[colnames(MarcasOff)== "Country"]<- "PAIS"
  colnames(MarcasOn)[colnames(MarcasOn)== "Country"]  <- "PAIS"
  colnames(MarcasOff)[colnames(MarcasOff)== "Investment"]<- "INVERSION"
  colnames(MarcasOn)[colnames(MarcasOn)== "Investment"]  <- "INVERSION"
  colnames(MarcasOff)[colnames(MarcasOff) == "Week_Year"]<- "SEMANA"

  MarcasOff <- MarcasOff %>% group_by(PAIS,ANO,MES,MARCA)  %>%  summarise(INVERSION = sum(INVERSION, na.rm = T),
                                                                          GRPS = sum(GRPS, na.rm = T))

  # Realizar uniones de ambas bases de datos, online y offline
  data      <- left_join(x=MarcasOff, y=MarcasOn, by = c("PAIS","ANO", "MES", "MARCA"))

  #Realizar la sumatoria de ambas bases de datos
  data$INVERSION <- ifelse(is.na(data$INVERSION.y),data$INVERSION.x,((data$INVERSION.x+data$INVERSION.y)))
  data$INVERSION <- ifelse((is.na(data$INVERSION.x)),data$INVERSION.y,data$INVERSION)

  # Cambiar el nombre de las columnas de Inversion
  colnames(data)[colnames(data) == "INVERSION.x"]    <- "INVERSION OFF"
  colnames(data)[colnames(data) == "INVERSION.y"]    <- "INVERSION ON"

  # Poner el formato en termino de plantilla
  data <- data[,c("ANO","MES","MARCA","INVERSION","GRPS")]
  colnames(data) <- c("Año", "Mes", "Marcas", "InversionTotal", "GRPS")
  return(data)

  writexl::write_xlsx(data, "datatransformada.xlsx")

}

# _________________________- Transformación de columnas y filas -_________________________________

Transformacion <- function(inversion,trends,grps,trendsvalue="percentages", year = "", data = "normal")
  {
             if (data == "normal") {
               datinv  <- inversion
               dattrps <- grps
               datrend <- trends
               datrend <- datrend[ , !(names(datrend) %in% "Semana")]
               datrend <- melt(datrend, id = c('Año', 'Mes'))
               datrend[is.na(datrend)] = 0
               datrend$value            <- as.numeric(datrend$value)
               datrend$Marcas           <- datrend$variable
               datrend$Trend            <- datrend$value
               datrend                  <- datrend[ , !(names(datrend) %in% c("variable","value"))]
               dattrps                  <- dattrps %>% group_by(Año,Mes,Marcas) %>% summarise(GRPS = sum(GRPS))
               datinv                   <- left_join(x=datinv, y=dattrps, by=c('Año','Mes','Marcas'))
             } else {
               datinv  <- inversion
               datrend <- trends
               datrend <- datrend[ , !(names(datrend) %in% "Semana")]
               datrend <- melt(datrend, id = c('Año', 'Mes'))
               datrend[is.na(datrend)] = 0
               datrend$value            <- as.numeric(datrend$value)
               datrend$Marcas           <- datrend$variable
               datrend$Trend            <- datrend$value
               datrend                  <- datrend[ , !(names(datrend) %in% c("variable","value"))]
             }
             if (trendsvalue == "percentages") {
                 datrend    <- datrend %>% group_by(Año,Mes,Marcas)    %>%  summarise(Trend = mean(Trend))
               if (year != "") {
                 datt       <- datrend %>% group_by(Marcas, Año, Mes)  %>%  filter(Año == year)  %>%  summarise(Trend     = round(mean(Trend),0))
                 dati       <- datinv  %>% group_by(Marcas, Año , Mes) %>%  filter(Año == year ) %>%  summarise(Inversión = sum(InversionTotal), SOV = sum(GRPS,na.rm=T))
               } else {
                 # Agruparlo por marcas no mas ?
                 datt       <- datrend %>% group_by(Marcas, Año, Mes)   %>%  summarise(Trend     = round(mean(Trend),0))
                 dati       <- datinv  %>% group_by(Marcas, Año , Mes)  %>%  summarise(Inversión = sum(InversionTotal), SOV = sum(GRPS,na.rm=T))
               }
             } else {
                 datrend    <- datrend %>% group_by(Año,Mes,Marcas)   %>% summarise(Trend = sum(Trend))
               if (year != "") {
                 datt       <- datrend %>% group_by(Marcas, Año, Mes) %>%  filter((Año == year)) %>%  summarise(Trend = round(mean(Trend),0))
                 dati       <- datinv  %>% group_by(Marcas, Año , Mes)%>%  filter(Año == year  ) %>%  summarise(Inversión = mean(InversionTotal), SOV = sum(GRPS,na.rm=T))
               } else {
                 datt       <- datrend %>% group_by(Marcas, Año, Mes)  %>%  summarise(Trend = round(sum(Trend),0))
                 dati       <- datinv %>% group_by(Marcas, Año , Mes)  %>%  summarise(Inversión = mean(InversionTotal), SOV = sum(GRPS,na.rm=T))
               }
             }
             dati$SOV <- (dati$SOV / sum(dati$SOV))*100
             dati$SOV <- round(dati$SOV,2)
             dat      <- left_join(x = dati , y = datt, by = c('Año', 'Mes','Marcas'))
             dat[is.na(dat)] = 0
             return(dat)
}

# _________________________- Unir las dos datas -_________________________________

#mirar como hacer condicion para mas opciones de Año FLEXIBLE
Union <- function(dat_new,digital ="")
  {
          dat     <- dat_new
          if (digital != ""){
            tv <- 1 - digital
            dat$Index <- (dat$SOV * tv) + (dat$Trend * digital)
          } else{
            dat$Index <- (dat$SOV * 0.5) + (dat$Trend * 0.5)
          }
          print(cor.test(dat$Inversión, dat$Index))
          return(dat)
}

# _________________________- Prueba de Correlaciones -_________________________________

Correlaciones <- function(trend, dat_new1)
{ dat    <- dat_new1
  numcol <- ncol(trend)
  col    <- (numcol - 3)
  Marca  <- vector("character", col)
  Answer <- vector("logical", col)
  Prueba <- list()
  Value  <- list()
  Resultados  <- list()
  datNo  <- data.frame(Marcas_ = character() , Correlacion = numeric())
  for (i in 4:numcol) {Marca[[i]] <- names(trend)[i]}
  Marca <- Marca[4:numcol]
  nombres_df <- paste0("df_", Marca)
  for (i in 1:col) {
      Answer[[i]] <- any(dat$Marcas == paste0( "" ,Marca[[i]], ""))
    if (!Answer[[i]]){
      next
    } else {
      df_actual  <- dat %>% filter(Marcas == paste0( "" ,Marca[i], ""))
      assign(nombres_df[i], df_actual)
      Resultados[[i]] <- cor.test(df_actual$Inversión, df_actual$Index)$estimate
      datNo           <- rbind(datNo, data.frame(Marcas_ = Marca[[i]], Correlacion = Resultados[[i]]))
    }
  }
  TablaFrecuencias <- data.frame(datNo)
  writexl::write_xlsx(TablaFrecuencias, "TablaFrecuencias.xlsx")
  }

# ___________________________________- MODELO DE PREDICCIÓN -_________________________________

Modelo <- function(dat_new1,Marcas = "" ,fecha=(060223)) {
  if (Marcas == ""){
  dati      <- dat_new1 %>% group_by(Marcas,Año,Mes) %>%  summarise(Inversión = sum(Inversión), SOV = mean(SOV,na.rm=T), Trend = mean(Trend,na.rm=T), Index = mean(Index,na.rm=T))
  dati      <- dat_new1 %>% group_by(Marcas,Año,Mes) %>%  summarise(Inversión = sum(Inversión), SOV = mean(SOV,na.rm=T), Trend = mean(Trend,na.rm=T), Index = mean(Index,na.rm=T))
  dati1     <- dat_new1 %>% group_by(Marcas,Año) %>%  summarise(Inversión = sum(Inversión), SOV = mean(SOV,na.rm=T), Trend = mean(Trend,na.rm=T), Index = mean(Index,na.rm=T))
  dati1     <- dati1 %>% arrange(Index)
  } else {
  dat_new2  <- dat_new1[dat_new1$Marcas %in% Marcas,]
  dati      <- dat_new2 %>% group_by(Marcas,Año,Mes) %>%  summarise(Inversión = sum(Inversión), SOV = mean(SOV,na.rm=T), Trend = mean(Trend,na.rm=T), Index = mean(Index,na.rm=T))
  dati1     <- dat_new2 %>% group_by(Marcas,Año) %>%  summarise(Inversión = sum(Inversión), SOV = mean(SOV,na.rm=T), Trend = mean(Trend,na.rm=T), Index = mean(Index,na.rm=T))
  dati1     <- dati1 %>% arrange(Index)
  dati2     <- dati1
  TotalSov  <- sum(dati2$SOV)
  TotalTrend<- sum(dati2$Trend)
  TotalIndex<- sum(dati2$Index)
  dati2$SOV <- (dati2$SOV/TotalSov)*100
  dati2$Trend <- (dati2$Trend/TotalTrend)*100

  # Truncar la columna "Index" a los dos primeros dígitos
  dati2$Index <- as.integer(substring(dati2$Index, 1, 2))
  }
  #dati1_1   <- dati1[,c(1,3:6)]
  #writexl::write_xlsx(dati1_1, "BubblesTable.xlsx")
  dati1_1   <- dati2[,c(1,3:6)]
  writexl::write_xlsx(dati2, "BubblesTable.xlsx")
  print("Base de datos agrupada por marca, Año y mes")
  print(dati1)
  print(dati2)
  # Data 1
  data1 <- dati
  data1 <- data1[,c(4,7)]
  # Modelo
  set.seed(fecha)
  model <- lm(Inversión ~ Index, data = data1)
  model
  return(model)

}

# ___________________________________- PREDICCIÓN MENSUAl-_________________________________

Prediccion <- function(model, Index = 1, confianza = 0.90) {
  sd = Index * 0.014
  Index   <- rnorm(12, mean =Index , sd = sd)
  Index <- data.frame(Index)
  predict <- predict(model, newdata = Index, interval = "confidence", level = confianza)
  print("A continuación se muestra los valores de Index para la PREDICCIÓN:")
  print(Index)
  print("A continuación se muestra los valores predichos:")
  print(predict)
  predict <- data.frame(predict)
  writexl::write_xlsx(predict, "PredictTable.xlsx")
  return(predict)
}





