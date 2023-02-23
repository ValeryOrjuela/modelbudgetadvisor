

# _____________________ Bajar librerias cada vez que corra el modelo
cargar_librerias <- function() {
  library(MASS)
  library(reshape)
  library(reshape2)
  library(dplyr)
  library(tidyr)
  library(writexl) 
}

# _________________________- Transformación de columnas y filas -_________________________________

Transformacion <- function(inversion,trends,grps,trendsvalue="percentages", year = "") 
  {
             datinv  <- inversion
             dattrps <- grps
             datrend <- trends
             datrend <- datrend[,c(1,2,4:11)]
             datrend                  <- melt(datrend, id = c('Año', 'Mes'))
             datrend$Marcas           <- datrend$variable
             datrend$Trend            <- datrend$value
             datrend                  <- datrend[,c(1,2,5,6)]
             dattrps                  <- dattrps[,c(1,2,4,5)]
             datinv  <- left_join(x = datinv , y = dattrps, by = c('Año', 'Mes','Marcas'))  
             if (trendsvalue == "percentages") {
               datrend <- datrend %>% group_by(Año,Mes,Marcas) %>% summarise(Trend = mean(Trend)) 
               if (year != "") {
                 datt       <- datrend %>% group_by(Marcas, Año, Mes) %>%  filter((Año == year)) %>%  summarise(Trend = round(mean(Trend),0)) 
                 dati       <- datinv %>% group_by(Marcas, Año , Mes)  %>%  filter(Año == year ) %>%  summarise(Inversión = sum(InversionTotal), SOV = sum(GRPS,na.rm=T))
               } else {
                 datt       <- datrend
                 dati       <- datinv
               }
             } else {
               datrend <- datrend %>% group_by(Año,Mes,Marcas) %>% summarise(Trend = sum(Trend))
               if (year != "") {
                 datt       <- datrend %>% group_by(Marcas, Año, Mes) %>%  filter((Año == year)) %>%  summarise(Trend = round(sum(Trend),0)) 
                 dati       <- datinv %>% group_by(Marcas, Año , Mes)  %>%  filter(Año == year  ) %>%  summarise(Inversión = sum(InversionTotal), SOV = sum(GRPS,na.rm=T))
               } else {
                 datt       <- datrend
                 dati       <- datinv
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
            dat$Index <- (dat$SOV * digital) + (dat$Trend * tv)
          } else{
            dat$Index <- (dat$SOV * 0.5) + (dat$Trend * 0.5)          
          }
          print(cor.test(dat$Inversión, dat$Index))
          return(dat)
}

# _________________________- Prueba de Correlaciones -_________________________________

Correlaciones <- function(dat_new1, correlacion = "" )
{ dat <- dat_new1
  Marca1 = names(datrend)[4]; Marca2 = names(datrend)[5]
  Marca3 = names(datrend)[6]; Marca4 = names(datrend)[7]
  Marca5 = names(datrend)[8]; Marca6 = names(datrend)[9]
  Marca7 = names(datrend)[10];Marca8 = names(datrend)[11]
  prueba1 <- dat %>% filter(Marcas == Marca1); value1  <- cor.test(prueba1$Inversión, prueba1$Index)$estimate
  dat1    <- c(prueba1$Marcas[1],value1)
  prueba2 <- dat %>% filter(Marcas == Marca2); value2  <- cor.test(prueba2$Inversión, prueba2$Index)$estimate
  dat2    <- c(prueba2$Marcas[1],value2)
  prueba3 <- dat %>% filter(Marcas == Marca3); value3  <- cor.test(prueba3$Inversión, prueba3$Index)$estimate
  dat3    <- c(prueba3$Marcas[1],value3)
  prueba4 <- dat %>% filter(Marcas == Marca4); value4  <- cor.test(prueba4$Inversión, prueba4$Index)$estimate
  dat4    <- c(prueba4$Marcas[1],value4)
  prueba5 <- dat %>% filter(Marcas == Marca5); value5  <- cor.test(prueba5$Inversión, prueba5$Index)$estimate
  dat5    <- c(prueba5$Marcas[1],value5)
  prueba6 <- dat %>% filter(Marcas == Marca6); value6  <- cor.test(prueba6$Inversión, prueba6$Index)$estimate
  dat6    <- c(prueba6$Marcas[1],value6)
  prueba7 <- dat %>% filter(Marcas == Marca7); value7  <- cor.test(prueba7$Inversión, prueba7$Index)$estimate
  dat7    <- c(prueba7$Marcas[1],value7)
  prueba8 <- dat %>% filter(Marcas == Marca8); value8  <- cor.test(prueba8$Inversión, prueba8$Index)$estimate
  dat8    <- c(prueba8$Marcas[1],value8)
  TablaFrecuencias <- data.frame(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8)
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
  }
  dati1_1   <- dati1[,c(1,3:6)]
  writexl::write_xlsx(dati1_1, "BubblesTable.xlsx")
  print("Base de datos agrupada por marca, Año y mes")
  print(dati1)
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
