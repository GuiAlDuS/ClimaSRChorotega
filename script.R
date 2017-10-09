setwd("~/Google Drive/RAnalisis/ClimaHIDROCEC")
datos <- read.table("DatosUNA_2015-oct2016Completos.csv", skip=2, header = F, sep = ",") #leer sin header

header <- scan("DatosUNA_2015-oct2016Completos.csv", nlines = 1, what = character(), sep = ",") #header de línea 1, separado por tabs para tomar en cuenta columnas vacías
header2 <- scan("DatosUNA_2015-oct2016Completos.csv", skip = 1, nlines = 1, what = character(), sep = ",") #header de línea 2

names(datos) <- paste0(header, header2)

#hacer tabla nueva sólo con las variables de lluvia
datoslluvia = data.frame("Date" = datos$Date, "Time" = datos$Time, "Rain" = datos$Rain)

library(ggplot2)
g = ggplot(datoslluvia, aes(Date, Rain))
g + geom_point()

datoslluvia$Date <- as.POSIXct(as.character(datoslluvia$Date), format="%m/%d/%Y")

g = ggplot(datoslluvia, aes(Date, Rain))
g + geom_point()

g1 = ggplot(datoslluvia, aes(x = Rain))
g1 + geom_density() + scale_x_log10()

g2 = ggplot(datoslluvia, aes(Date, Rain))
g2 + geom_boxplot()  

#ojo con datos faltantes

#buscar cuales días no tienen 24 mediciones
tab = table(datoslluvia$Date)
tab[which(tab<24)]

#buscar cuales días no se midieron
#https://bocoup.com/weblog/padding-time-series-with-r

library(lubridate)
library(dplyr)

#lluvia diaria
lluvia_dia = datoslluvia %>% group_by(year(datoslluvia$Date), month(datoslluvia$Date), day(datoslluvia$Date)) %>% summarise(sum(Rain), Date = first(Date))

colnames(lluvia_dia) <- c("year", "month", "day", "Rain", "Date")

g3 = ggplot(lluvia_dia, aes(Date,Rain))
g3 + geom_bar(stat = "identity")

#lluvia mensual
lluvia_mes = datoslluvia %>% group_by(year(datoslluvia$Date), month(datoslluvia$Date)) %>% summarise(sum(Rain), Date = first(Date))

colnames(lluvia_mes) <- c("year", "month", "Rain", "Date")

g4 = ggplot(lluvia_mes, aes(Date,Rain))
g4 + geom_bar(stat = "identity")

#lluvia anual
lluvia_ano = datoslluvia %>% group_by(year(datoslluvia$Date)) %>% summarise(sum(Rain))

colnames(lluvia_ano) <- c("year", "Rain")

g5 = ggplot(lluvia_ano, aes(year,Rain))
g5 + geom_bar(stat = "identity")


#curva cumulativa de precipitación
datoslluvia$year = year(datoslluvia$Date)
datoslluvia = mutate(group_by(datoslluvia, year), cumRain = cumsum(Rain))

datoslluvia$juliano = yday(datoslluvia$Date) #convertir a día juliano

g6 = ggplot(datoslluvia, aes(juliano, cumRain, color = factor(year(Date))))
g6 + geom_line() + ggtitle("Estación UNA Liberia") +
  xlab("Día") + ylab("Precipitación acumulada (mm)") +
  scale_color_discrete(name="Año")


#heatmap con lluvia diaria
#basado en https://www.r-bloggers.com/ggplot2-time-series-heatmaps/

lluvia_dia$monthf<-factor(lluvia_dia$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)

lluvia_dia$weekday = as.POSIXlt(lluvia_dia$Date)$wday + 1
lluvia_dia$weekdayf <- factor(lluvia_dia$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)


library(zoo)
lluvia_dia$yearmonth <- as.yearmon(lluvia_dia$Date)
lluvia_dia$yearmonthf <- factor(lluvia_dia$yearmonth)
lluvia_dia$week <- as.numeric(format(lluvia_dia$Date,"%W"))
lluvia_dia <- lluvia_dia %>% group_by(yearmonthf) %>% mutate(monthweek = 1 + week - min(week))

P<- ggplot(lluvia_dia, aes(monthweek, weekdayf, fill = Rain)) + 
  geom_tile(colour = "white") + facet_grid(year~monthf) + 
  scale_fill_gradient(low="red", high="yellow") +
  xlab("Week of Month") + ylab("")
P

#revisar este otro
#http://blog.revolutionanalytics.com/2009/11/charting-time-series-as-calendar-heat-maps-in-r.html