#estaciones Nicoya y Liberia
#graficos comparativos
setwd("~/Google Drive/RAnalisis/ClimaHIDROCEC")
library(tidyverse)
library(stringr)
library(lubridate)

#cargar estaciones
datos1Li <- read_csv("DatosUNA_2015-oct2016Completos.csv", skip=2, col_names = FALSE, 
                   col_types = cols(.default = col_character()))
datos2Li <- read_tsv("Davis_HIDROCEC_Jun2016_Feb2017.txt", skip=2, col_names = FALSE, 
                   col_types = cols(.default = col_character()))
datosNicoya <- read_csv("CEMEDE_VW_Hourly.dat", skip=4, col_names = FALSE, 
                        col_types = cols(.default = col_character()))

#lectura de encabezados

header1 <- scan("DatosUNA_2015-oct2016Completos.csv", nlines = 1, what = character(), sep = ",") 
header2 <- scan("DatosUNA_2015-oct2016Completos.csv", skip = 1, nlines = 1, what = character(), sep = ",") 
names(datos1Li) <- paste0(header1, header2)

header10 <- scan("Davis_HIDROCEC_Jun2016_Feb2017.txt", nlines = 1, what = character(), sep = "\t")
header20 <- scan("Davis_HIDROCEC_Jun2016_Feb2017.txt", skip = 1, nlines = 1, what = character(), sep = "\t") 
names(datos2Li) <- paste0(header10, header20)

header <- scan("CEMEDE_VW_Hourly.dat", skip = 1, nlines = 1, what = character(), sep = ",")
names(datosNicoya) <- paste0(header)


#convirtiendo columnas a tipos respectivos
datos1Li[datos1Li=="---"] <- NA
datos1Li$Date <- as.Date((datos1Li$Date), format="%m/%d/%Y")
datos1Li$Time <- parse_time(datos1Li$Time, "%I:%M %p")
datos1Li_conv <- type_convert(datos1Li)

datos2Li[datos2Li=="---"] <- NA
datos2Li$Date <- as.Date((datos2Li$Date), format="%m/%d/%y")

datos2Li$Time <- gsub("a", "AM", datos2Li$Time)
datos2Li$Time <- gsub("p", "PM", datos2Li$Time)

datos2Li$Time <- parse_time(datos2Li$Time, "%I:%M %p")
datos2Li_conv <- type_convert(datos2Li)

datosNicoya$TIMESTAMP <- parse_datetime(datosNicoya$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")
datosNicoya$Rain_mm_Tot <- parse_double(datosNicoya$Rain_mm_Tot)


#Juntar tablas de Liberia eliminando repetidas y únicamente con columnas similares:
cols_com <- intersect(colnames(datos1Li), colnames(datos2Li))
datos1Li_com <- subset(datos1Li, select = cols_com)
datos2Li_com <- subset(datos2Li, select = cols_com)
datosLi <- rbind(datos1Li_com, datos2Li_com)

datos_unicos <- distinct(datosLi, Date, Time, .keep_all = TRUE)
datos_unicos$Rain <- parse_double(datos_unicos$Rain)

#Seleccionar variables y sumar a valores diarios
lluvia_diaNicoya <- datosNicoya %>% 
  group_by(year(datosNicoya$TIMESTAMP), month(datosNicoya$TIMESTAMP), day(datosNicoya$TIMESTAMP)) %>% 
  summarise(sum(Rain_mm_Tot), Date = first(TIMESTAMP))

colnames(lluvia_diaNicoya) <- c("year", "month", "day", "Rain", "Date")


lluvia_diaLiberia = datos_unicos %>% 
  group_by(year(datos_unicos$Date), month(datos_unicos$Date), day(datos_unicos$Date)) %>% 
  summarise(sum(Rain), Date = first(Date))

colnames(lluvia_diaLiberia) <- c("year", "month", "day", "Rain", "Date")

add_column(lluvia_diaLiberia, estacion = "Liberia")

