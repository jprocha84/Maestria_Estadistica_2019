library(tikzDevice) 
tikz('figs/simpleEx.tex',width=3.5,height=3.5) 
plot(1,main='Hello World!')
hist(encuesta_resumen$Edad, main="Edad", 
     xlab="Porcentaje por Edad", 
     ylab="Frecuencia", 
     col="lightblue")
dev.off()