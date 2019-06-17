library("readxl")
tas_data <- read_excel("Datos_TAS-base1.1.xls")
tas_data$id <- as.factor(tas_data$id)

summary(tas_data)
colMeans(tas_data[,2:5])

attach(tas_data)
plot(tas~edad,main="TAS vs Edad",cex.main=0.8,ylab="TAS",xlab="Edad",
     cex.lab=0.8,xlim=c(40,70),ylim=c(100,350), cex.axis=0.8,col="red",cex=0.75, pch=19)

plot(tas~peso,main="TAS vs Peso",cex.main=0.8,ylab="TAS",xlab="Peso",
     cex.lab=0.8,xlim=c(50,110),ylim=c(100,350), cex.axis=0.8,col="red",cex=0.75, pch=19)

plot(tas~colesterol,main="TAS vs Colesterol",cex.main=0.8,ylab="TAS",xlab="Colesterol",
     cex.lab=0.8,xlim=c(160,295),ylim=c(100,350), cex.axis=0.8,col="red",cex=0.75, pch=19)


