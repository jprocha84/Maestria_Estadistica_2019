# setwd("")
datos=read.csv('./data/2019-03-13_datos_permisos_2005-2018_-12-31.csv')
head(datos, 10) # Permite visualizar los primeros 10 registros.
attach(datos)
(n=length(Tipo)) # Total de registros 42955

datos$fecha_pe = as.Date(datos$Fecha.PE,'%d/%m/%Y')
datos$fecha_pe_year = as.numeric(format(datos$fecha_pe, "%Y"))


# Frecuencias por tipo
(tablaFrecAbs = table(Tipo))

# Frecuencias relativas 
(tablaFrecRel = tablaFrecAbs/n)
# Frecuencias en porcentaje
(tablaFrecRelPct = (tablaFrecAbs/n)*100)

# Grafico de sectores
pie(table(Tipo))

# Grafico de sectores - con colores y etiquetas
frec_abs<-table(Tipo)
porcentajes<-(frec_abs/n)*100
# Si bien dado que n=100 la frecuencia absoluta coincide con el porcentaje, 
# se indica en forma general.  

lbls <- c("Circular 34 inc h", "Demoliciones","", "Obras Nuevas + Registro", "Obras Nuevas", "Registros")
lbls <- paste(lbls, porcentajes) # Se agregan los % a las etiquetas
lbls <- paste(lbls,"%", sep="")  # Se agrega el sìmbolo % a las etiquetas
pie(frec_abs,labels = lbls, col=c("red", "orange","yellow", "blue","green", "grey"))


# Graficos de barras
barplot(tablaFrecRelPct, xlab="Permisos de construccion", ylab="Porcentaje",
        ylim=c(0,60), col=c("red"),
        names=c("C", "D", "E", "M","ON","R"))


# Barras agrupadas
counts <- table(Tipo, fecha_pe_year)
barplot(counts, xlab="Edad gestacional", names=c("A término", "Fuera de término"),
        ylab="Número de pacientes",
        col=c("lightblue","lightgreen"), ylim=c(0,60), beside=TRUE)  

legend("topright", legend = c("No primeriza", "Primeriza"), 
       fill = c("lightblue","lightgreen"))


# Gráfico de Pareto
library(qcc)
frec_abs<-table(Tipo)
pareto.chart(frec_abs,
             xlab="Registro de obras", ylab="Cantidad registros",
             ylab2 = "Porcentaje acumulado",
             main="", col="lightblue")
