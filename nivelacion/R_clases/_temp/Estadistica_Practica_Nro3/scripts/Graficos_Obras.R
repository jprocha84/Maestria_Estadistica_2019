datos=read.csv('~/maestria_estadistica/Estadistica_Practica_Nro3/data/2019-03-13_datos_permisos_2005-2018_-12-31.csv')
head(datos, 10) # Permite visualizar los primeros 10 registros.
datos = datos[datos$Tipo != 'E',]
datos$Tipo <- factor(datos$Tipo)
attach(datos)
(n=length(Tipo)) # Total de registros 42955

datos$fecha_pe = as.Date(datos$Fecha.PE,'%d/%m/%Y')
datos$fecha_pe_year = as.numeric(format(datos$fecha_pe, "%Y"))


# Frecuencias por tipo
(tablaFrecAbs = table(datos$Tipo))


# Frecuencias relativas 
round((tablaFrecRel = tablaFrecAbs/n),digits=2)
# Frecuencias en porcentaje
round((tablaFrecRelPct = (tablaFrecAbs/n)*100),digits=2)

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
             xlab="Tipo de trámite", ylab="Número de registros",
             ylab2 = "Porcentaje acumulado",
             main="", col="lightblue")
title("Diagrama de Pareto de registros por tipo de trámite",cex.main=1)

summary(Tipo)




round(mean(Total.m.),digits=3)

Total.m.
total_m_cubiertos = Total.m.[Total.m.>0]
median(total_m_cubiertos)

# Create the function.
getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Calculate the mode using the user function.
result <- getmode(total_m_cubiertos)
round(print(result), digits = 2)
min(total_m_cubiertos)
max(total_m_cubiertos)

(rango<-max(total_m_cubiertos)-min(total_m_cubiertos))

var(total_m_cubiertos)
sd(total_m_cubiertos)
quantile(total_m_cubiertos)

summary(total_m_cubiertos)

boxplot(datos$Total.m.~datos$Tipo, 
        boxfill="lightblue", 
        horizontal=FALSE,
        main="Total metros cuadrados cubiertos",
        pch=1)


########
library(ggplot2)

ggplot(data = datos)+geom_histogram(aes(x=Total.m.))
ggplot(data = datos) + geom_density(aes(x = Total.m., y=Tipo), fill = "grey50")

ggplot(datos, aes(x=Total.m., y=Tipo)) + 
        geom_boxplot(outlier.colour="red")


ggplot(data = datos, 
       aes(x = Tipo, y = Total.m.)) +
        geom_boxplot() +
        xlab("Tipo") +
        ylab("Metros Cubiertos") +
        labs(title = "Metros por tipo")

legend_plot <- ggplot_box_legend()
chloride_plot <- ggplot(data = datos, 
                        aes(x = Tipo, y = Total.m.)) +
        boxplot_framework(upper_limit = 120000) + 
        xlab(label = "Tipo") +
        ylab(label = "Metros Cubiertos") +
        labs(title = "Metros por tipo")

plot_grid(chloride_plot, 
          legend_plot,
          nrow = 1, rel_widths = c(.6,.4))

########




hist(total_m_cubiertos, main="", xlab="Peso (en kg)",
     ylab="Número de pacientes", ylim=c(1,100))

stem(Total.m.[Total.m.>0], scale = 5)

hist(Total.m.[Total.m.>0], 
     main="Acceso a Internet", 
     xlab="Porcentaje de población con acceso a Internet", 
     ylab="Frecuencia", 
     col="lightblue")

hist(Total.m.[Total.m.>0], 
     breaks=c(min(Total.m.[Total.m.>0]), 
              seq(20,100,10), 
              max(Total.m.[Total.m.>0])))

boxplot(Total.m.[Total.m.>0], 
        boxfill="lightblue", 
        horizontal=TRUE,
        main="Duración del trabajo de parto (hs)",
        ylim = c(0, 120000),
        outline = TRUE)


res <- boxplot(Total.m.[Total.m.>0], outline=FALSE)
mean(data$a[!data$a %in% res$out])
attach(datos)
boxplot(Total.m.[Total.m.>0],
        horizontal=TRUE,
        outline=FALSE,
        boxfill="lightblue",
        main="Total metros cubiertos (en m2)")
abline(h=quantile(Total.m.[Total.m.>0], c(0.25, 0.75)), col="red")


boxplot(Total.m.~Tipo,
        data=datos, 
        main="Total metros cubiertos (en m2)",
        outline=FALSE,
        boxfill="lightblue",
        xlab="Tipos de Trámite", 
        ylab="Total metros cubiertos (en m2)")


x <- Total.m.[Total.m.<1000 ]
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
     

d <- density(Total.m.[Total.m.<1000]) # returns the density data 
plot(d,
     main="Densidad") # plots the results



