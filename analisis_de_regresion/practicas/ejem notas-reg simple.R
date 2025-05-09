#EJEMPLO NOTAS 
 

#REGRESION LINEAL SIMPLE
#carga de datos
alumno<-c(1,2,3,4,5,6,7,8,9,10,11)
nota.parcial<-c(70,100,65,90,70,85,70,50,50,90,81)
nota.final<-c(76,99,66,92,69,80,71,51,50,80,75)
#crea base de datos
datos<-data.frame(alumno,nota.final,nota.parcial)

#an�lisis exploratorio univariado
summary(nota.parcial)
summary(nota.final)
cbind(min(nota.parcial), max(nota.parcial), mean(nota.parcial), sd(nota.parcial))
cbind(min(nota.final), max(nota.final), mean (nota.final), sd(nota.final))

split.screen(c(1,2))
boxplot(nota.parcial,ylim=c(40,100),ylab="nota.parcial")
screen(2)
boxplot(nota.final,ylim=c(40,100),ylab="nota.final")
 
#an�lisis exploratorio bivariado
plot(nota.final~nota.parcial,ylim=c(40,100),xlim=c(40,100),col="blue",cex=0.75)
cor(nota.final,nota.parcial)

#modelo de regresi�n lineal simple
modelo = lm(nota.final~ nota.parcial, data=datos)
modelo

#gr�fico de dispersi�n con curva ajustada
plot(nota.final~nota.parcial,ylim=c(40,100),xlim=c(40,100),col="blue",cex=0.75)
abline(modelo)

#CME
summary(modelo)$sigma^2

# matriz de variancias y cov de betas
vcov(modelo) 

#IC de coefs de regresi�n
confint(modelo) 

#tests de hip�tesis de coefs de regresi�n
summary(modelo)

#percentil 0.975 de la distribuci�n t con 9 gl
qt(0.975,9)
  
#estimaci�n de E(y) e IC (x=70)
predict(modelo,list(nota.parcial=70),interval="conf")

#estimaci�n de la respuesta media e IC
predict(modelo,interval="conf")

#gr�fico de E(y)estimada e IC
install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)
mpi = cbind(datos, predict(modelo, interval = "conf"))
ggplot(mpi, aes(x = nota.parcial)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray", alpha = 0.5) +
  geom_point(aes(y = nota.final)) +
  geom_line(aes(y = fit), colour = "blue", size = 1)

#predicci�n de y e IP (x=70)
predict(modelo,list(nota.parcial=70),interval="conf")

#predicci�n de y e IP
predict(modelo,interval="conf")

#gr�fico de valores predichos e IP
mpi = cbind(datos, predict(modelo, interval = "prediction"))
ggplot(mpi, aes(x = nota.parcial)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray", alpha = 0.5) +
  geom_point(aes(y = nota.final)) +
  geom_line(aes(y = fit), colour = "blue", size = 1)

#anova 
anova(modelo)
qf(0.95,1,9)

# coeficiente de determinaci�n
summary(modelo)$r.squared 

# Comprobaci�n de supuestos                                                                                                          
#valores ajustados
ajustados<-fitted(modelo) 
#residuos
residuos<-resid(modelo)
#residuos estandarizados
standresid<-rstandard(modelo)

#distribuci�n normal de los errores
# gr�fico probabil�stico Normal
qqnorm(standresid, ylab="Residuos estandarizados", xlab="Normal Scores",main="") 
qqline(standresid) 
# test de Normalidad
install.packages("nortest")
library(nortest)
ad.test(residuos) 

#media de los errores igual a 0 y variancia constante
#gr�fico residuos vs valores ajustados
plot(standresid~ajustados,ylab="Residuos estandarizados", xlab="Valores ajustados",xlim=c(40,100),ylim=c(-2,2))
abline(2,0)
abline(-2,0)

#errores correlacionados
# gr�fico residuos vs orden
plot(standresid,ylab="Residuos estandarizados", xlab="Orden",xlim=c(0,12),ylim=c(-2,2))
# estad�stica Durbin y Watson 
install.packages("lmtest")
library(lmtest)
dwtest(modelo)

#relacion propuesta para x e y
# gr�fico residuos vs X
plot(standresid~nota.parcial, ylab="Residuos estandarizados", xlab="Nota parcial",xlim=c(40,100),ylim=c(-2,2)) 
abline(0, 0)

#para ver detalles de la sentencia plot
?plot # par = parametros modificables en un grafico






