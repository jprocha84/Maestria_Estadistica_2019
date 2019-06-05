library(tikzDevice)
options(tikzDefaultEngine = 'pdftex')
# Encuesta a usuarios del Sistema de Bicicletas Públicas
# Fuente: https://datos.rosario.gob.ar/dataset/encuesta-usuarios-del-sistema-de-bicicletas-públicas 

encuesta_data=read.csv('/Users/jprocha/Proyectos/3.Educacion/maestria_estadistica/nivelacion/R_clases/Estadistica_Practica_Nro3/data/encuesta_sbp_2018.csv',
               sep = ";",
               header = TRUE,
               fileEncoding="latin1")
colnames(encuesta_data)

keeps <- c("Edad", 
           "En.una.escala.de.0.a.10..Cómo.valorás.el.grado.de.utilidad.que.tiene.para.vos.el.servicio.de.bicicletas.públicas.",
           "En.una.escala.de.0.a.10..Cuál.es.tu.nivel.de.satisfacción.general.en.relación.al.servicio.")
encuesta_resumen=encuesta_data[keeps]
names(encuesta_resumen)<-c("Edad",
                           "Puntaje_Utilidad",
                           "Puntaje_Satisfaccion")

# Elimino los missing values
encuesta_resumen<-subset(encuesta_resumen, !is.na(encuesta_resumen$Puntaje_Utilidad))

summary(encuesta_resumen)
#Resumen gráfico de cada variable:
hist(encuesta_resumen$Edad, main="Edad", 
     xlab="Porcentaje por Edad", 
     ylab="Frecuencia", 
     col="lightblue")
hist(encuesta_resumen$Puntaje_Utilidad, main="Puntaje por utilidad del servicio", 
     xlab="Porcentaje de Puntajes", 
     ylab="Frecuencia", 
     col="lightblue")
hist(encuesta_resumen$Puntaje_Satisfaccion, main="Puntaje por satisfacción", 
     xlab="Porcentaje de Puntajes", 
     ylab="Frecuencia", 
     col="lightblue")

#Diagrama de dispersión entre acceso a internet y uso de facebook
plot(encuesta_resumen$Puntaje_Satisfaccion, 
     encuesta_resumen$Puntaje_Utilidad,
     main="Diagrama de dispersión", 
     xlab="Satisfaccion", 
     ylab="Utilidad", col="blue", pch=16)
abline(h=20.54, v=82.9, lty=2)

#Para añadir la recta de regresión estimada:
abline(lm(datos$Facebook ~ datos$Internet), col="red") 


# Datos sobre exportaciones en Santa FE
datos=read.csv('~/maestria_estadistica/Estadistica_Practica_Nro3/data/principales-tasas-interes.csv')
hist(datos$tasas_interes_cajas_ahorro, main="Valor en USD", 
     xlab="Valor en USD", 
     ylab="Frecuencia", 
     col="lightblue")
hist(datos$tasas_interes_plazo_fijo_30_59_dias, main="Peso en Kg", 
     xlab="Peso en Kg", 
     ylab="Frecuencia", 
     col="lightblue")

#Diagrama de dispersión entre acceso a internet y uso de facebook
plot(datos$tasas_interes_plazo_fijo_mas_60_dias, 
     datos$tasas_interes_plazo_fijo_30_59_dias,
     main="Diagrama de dispersión", 
     xlab="Valor en USD", 
     ylab="Peso en Kg", col="blue", pch=16)
abline(h=20.54, v=82.9, lty=2)

#Para añadir la recta de regresión estimada:
abline(lm(datos$tasas_interes_plazo_fijo_mas_60_dias ~ datos$tasas_interes_plazo_fijo_30_59_dias), col="red") 
(cor(datos$tasas_interes_plazo_fijo_mas_60_dias,
     datos$tasas_interes_plazo_fijo_30_59_dias))



lstTabla = c(0,7,6,2,2,1,1,0,21,42,29,26,20,17,9,4,2,1,0,0,0,0,0,0,0,0)
lstTablaB = c(0,0,0,0,0,0,0,0,0,1,2,4,9,17,20,26,29,42,21,1,1,2,2,6,7,0)
sum(lstTabla)
sum(lstTablaB)

sum(lstTabla[12:length(lstTabla)]) / sum(lstTabla)
sum(lstTabla[length(lstTabla):(length(lstTabla)-10)]) / sum(lstTabla)

sum(lstTablaB[0:15]/sum(lstTablaB))



