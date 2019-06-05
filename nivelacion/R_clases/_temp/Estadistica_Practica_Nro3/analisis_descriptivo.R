library(knitr)
library(latticeExtra)
library(dplyr) #for data manipulation grammar (the pipe operator)
library(tidyr) #for tidying up the data
library(ggplot2) #for data viz

# Rosario - Facturación por rama de actividad económica
dsFacturacion = read.csv("/Users/jprocha/Desktop/operativos_alcoholemia_-_emergencia_de_seguridad_2017.csv", 
                                header = TRUE,
                                sep = ";",
                                col.names = c("sector","rama","anio","mes","facturacion"))

dsFacturacion$facturacion <- as.numeric(gsub('[$]','',gsub('[,]','.',gsub('[.]', '', dsFacturacion$facturacion))))

levels(dsFacturacion$sector)
levels(dsFacturacion$rama)

summary(dsFacturacion)
head(dsFacturacion,10)
(n=length(dsFacturacion$sector))
(tablaFrecAbs = table(dsFacturacion$sector))
