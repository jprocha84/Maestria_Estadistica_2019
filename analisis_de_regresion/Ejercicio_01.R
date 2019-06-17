
# EJERCICIO 1: hectareas vs producción de caña azúcar                                                                                     
                                                                                    

# creación del conjunto de datos EJER1 en el objeto data (data.frame)                                                                 
dpto<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
ha<-c(13638,6151,5828,931,12222,5302,11979,8175,13679,8296,13396,3238,16633,7244)
produccion<-c(940000,460000,440000,65000,830000,380000,860000,590000,1020000,585000,1020000,200000,1130000,570000)
dpto<-as.factor(dpto)
datos1<-data.frame(dpto,ha,produccion)
datos1

# MEDIDAS DESCRIPTIVAS 
summary(datos1)
colMeans(datos1[,-1])

# GRÁFICO DE DISPERSIÓN con plot{graphics}                                                                                                           
plot(produccion~ha,main="Hectareas cosechadas vs Produccion de caña de azucar (toneladas)", 
     ylab = "Producción total", xlab = "Hectareas cosechadas",
     col="red",cex=0.95, pch=19,xlim=c(0,17000),ylim=c(0,1200000))



# AJUSTE DEL MODELO DE REGRESIÓN LINEAL                                                  
modelo1 = lm(produccion ~ ha, data=datos1)
modelo1
coefficients(modelo1)




# GRÁFICO DE DISPERSIÓN CON RECTA AJUSTADA 

plot(produccion~ha,main="Hectareas cosechadas vs Produccion de caña de azucar (toneladas)", xlab = "Hectareas",col="red",cex=0.75)    
abline(modelo1)


# ANOVA
anova(modelo1)

# salida general

summary(modelo1)

# coeficiente de determinación
summary(modelo1)$r.squared 


# IC para coef regresion (dos opciones) 
confint(modelo1)
t(confint(modelo1))

# residuos  
residuos<-resid(modelo1)
residuos

standresid<-rstandard(modelo1)
standresid



# IC para la respuesta media donde x=5302
predict(modelo1, list(ha=5302), interval="conf")

# IC de confianza para la respuesta media  (para todos los casos)
predict(modelo1, interval="conf")


# Intervalo de predicción para x=7244
predict(modelo1,list(ha=7244) , interval="pred")

# Intervalo de predicción  (para todos los casos)
predict(modelo1,interval="pred")






##############################################################
#Comprobación de supuestos         
##############################################################


#valores predichos 
predichos<-predict(modelo1)    #otra forma : predichos<-fitted(modelo1) 

#residuos 
residuos<-resid(modelo1)

#residuos estandarizados
standresid<-rstandard(modelo1)


#Histograma de residuos
hist(residuos,ylab="Frecuencia", xlab="Residuos", main="")

#Gráfico probabilístico Normal
qqnorm(standresid, ylab="Residuos estandarizados", xlab="Normal Scores",col="red",cex=0.95, pch=19) 
qqline(standresid) 

#Gráfico de residuos vs predichos 
plot(standresid~predichos,ylab="Residuos estandarizados",xlab="Predichos",col="red",cex=0.95, pch=19)
abline(0,0) 

#Gráfico de residuos vs orden (orden: como fueron cargados en la tabla de datos) 
plot(standresid,ylab="Residuos estandarizados",xlab = "Orden",col="red",cex=0.95, pch=19)
abline(0,0) 


#los cuatro gráficos juntos

par(mfrow=c(2,2))   
hist(residuos,ylab="Frecuencia", xlab="Residuos", main="")
qqnorm(standresid, ylab="Residuos estandarizados", xlab="Normal Scores",col="red",cex=0.95, pch=19) 
qqline(standresid) 
plot(standresid~predichos,ylab="Residuos estandarizados",xlab="Predichos",col="red",cex=0.95, pch=19)
abline(0,0) 
plot(standresid,ylab="Residuos estandarizados",xlab = "Orden",col="red",cex=0.95, pch=19)
abline(0,0) 

# Test Durbin y Watson: 
install.packages("lmtest")
library(lmtest)
dyw<-dwtest(modelo1)
dyw


# Test de Normalidad AD y kS 
install.packages("nortest")
library(nortest)
ad.test(residuos) 
lillie.test(residuos)




# GRÁFICOS POR DEFECTO con plot(modelo1)
op <- par(mfrow = c(2,2))
plot(modelo1)           
par(op)

