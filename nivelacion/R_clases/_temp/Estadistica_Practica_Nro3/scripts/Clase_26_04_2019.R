a= table(c(1,2),c(2,3))

smoke <- matrix(c(51,43,22,92,28,21,68,22,9),ncol=3,byrow=TRUE)
colnames(smoke) <- c("High","Low","Middle")
rownames(smoke) <- c("current","former","never")
smoke <- as.table(smoke)
smoke

personas <- matrix(c(139,230,443,502),ncol=2,byrow=TRUE)

edad_18_34 = c(5,6,174,213)
edad_35_54 = c(41,19,198,180)
edad_55_64 = c(51,40,64,81)
edad_65_mas = c(42,165,7,28)


personas <- matrix(edad_65_mas,ncol=2,byrow=TRUE)
colnames(personas) <- c("Fumador","No Fumador")
rownames(personas) <- c("No Sobrevive","Sobrevive")
personas <- as.table(personas)
personas

print(tabla.prop)
(tabla.prop<-round(prop.table(personas,1),3)) 
barplot(tabla.prop, ylim=c(0,1.2),
        beside=T, 
        main="Hábito de fumar y sobrevida a 20 años \n Edad 65 años o más", 
        legend.text=rownames(tabla.prop), 
        ylab="Proporción", 
        xlab="Hábito de Fumar")


print(xtable(tabla.prop, type = "latex"), file = "filename2.tex")

(test.chi<-chisq.test(personas, correct=F))
test.chi$expected



