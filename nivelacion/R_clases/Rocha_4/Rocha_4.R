library(xtable)
library(tikzDevice) 

#Ejercicio 1 - A
mx_estudio_tot <- matrix(c(139,230,443,502),ncol=2,byrow=TRUE)
colnames(mx_estudio_tot) <- c("Fumadora","No Fumadora")
rownames(mx_estudio_tot) <- c("No Sobrevive","Sobrevive")
ct_estudio_tot = addmargins(mx_estudio,FUN = list(Total = sum), quiet = TRUE)
ct_estudio_tot_prop = prop.table(mx_estudio_tot,margin = 1) #Para mostrar porcentajes

# Tabla de contingencia inicial
print(xtable(ct_estudio_tot, type = "latex"), file = "output.tex")

# Porcentaje de fumadoras por sobrevida
print(xtable(round(prop.table(mx_estudio_tot,margin=1)*100,2)), file = "output.tex")

# Porcentaje de sobrevida por hábito de fumar
print(xtable(round(prop.table(mx_estudio_tot,margin=2)*100,2)), file = "output.tex")

#Figura 1 - Proporción de fumadores y sobrevida a 20 años
tikz('fig1.tex',width=3.5,height=3.5) 
barplot(ct_estudio_tot_prop, ylim=c(0,1),
        beside=T, 
        main="Hábito de fumar y sobrevida a 20 años", 
        ylab="Proporción", 
        xlab="Hábito de Fumar",
        args.legend = list(x = "topright"),
        cex.main=1,
        col=c("grey","white")
)
legend("topright",legend=rownames(tabla.prop),
       fill=c("grey","white"), horiz=TRUE, cex=0.8)
dev.off()

#Test de independencia
(test.chi<-chisq.test(mx_estudio_tot, correct=F))

#Medidas de asociación y RR
139/369-443/945
230/369-502/945
RR = (139/369)/(443/945)
RR = (230/369)/(502/945)


#Ejercicio 1 - B

# 18 a 34 años
ct_estudio = c(5,6,174,213)
mx_estudio <- matrix(ct_estudio,ncol=2,byrow=TRUE)
colnames(mx_estudio) <- c("Fumadora","No Fumadora")
rownames(mx_estudio) <- c("No Sobrevive","Sobrevive")

mx_estudio_tot = addmargins(mx_estudio,FUN = list(Total = sum), quiet = TRUE)
print(xtable(mx_estudio_tot, type = "latex"), file = "output.tex")

5/11-174/387
6/11-213/387
(5/11)/(174/387)
(6/11)/(213/387)

tikz('fig2.tex',width=3.5,height=3.5) 
barplot(mx_estudio_prop, ylim=c(0,1),
        beside=T, 
        main="18 a 34 años", 
        ylab="Proporción", 
        xlab="Hábito de Fumar",
        args.legend = list(x = "topright"),
        cex.main=1,
        col=c("grey","white")
)
legend("topright",legend=rownames(tabla.prop),
       fill=c("grey","white"), horiz=TRUE, cex=0.8)
dev.off()
#Test de independencia
(test.chi<-chisq.test(mx_estudio, correct=F))

# 35 a 54 años
ct_estudio = c(41,19,198,180)
mx_estudio <- matrix(ct_estudio,ncol=2,byrow=TRUE)
colnames(mx_estudio) <- c("Fumadora","No Fumadora")
rownames(mx_estudio) <- c("No Sobrevive","Sobrevive")
mx_estudio_prop = prop.table(mx_estudio,margin = 1) #Para mostrar porcentajes

mx_estudio_tot = addmargins(mx_estudio,FUN = list(Total = sum), quiet = TRUE)
print(xtable(mx_estudio_tot, type = "latex"), file = "output.tex")

41/60-198/378
19/60-180/378
(41/60)/(198/378)
(19/60)/(180/378)

tikz('fig3.tex',width=3.5,height=3.5) 
barplot(mx_estudio_prop, ylim=c(0,1),
        beside=T, 
        main="35 a 54 años", 
        ylab="Proporción", 
        xlab="Hábito de Fumar",
        args.legend = list(x = "topright"),
        cex.main=1,
        col=c("grey","white")
)
legend("topright",legend=rownames(tabla.prop),
       fill=c("grey","white"), horiz=TRUE, cex=0.8)
dev.off()
#Test de independencia
(test.chi<-chisq.test(mx_estudio, correct=F))

# 55 a 64 años
ct_estudio = c(51,40,64,81)
mx_estudio <- matrix(ct_estudio,ncol=2,byrow=TRUE)
colnames(mx_estudio) <- c("Fumadora","No Fumadora")
rownames(mx_estudio) <- c("No Sobrevive","Sobrevive")
mx_estudio_prop = prop.table(mx_estudio,margin = 1) #Para mostrar porcentajes

mx_estudio_tot = addmargins(mx_estudio,FUN = list(Total = sum), quiet = TRUE)
print(xtable(mx_estudio_tot, type = "latex"), file = "output.tex")

51/91-64/145
40/91-81/145
(51/91)/(64/145)
(40/91)/(81/145)

tikz('fig4.tex',width=3.5,height=3.5) 
barplot(mx_estudio_prop, ylim=c(0,1),
        beside=T, 
        main="55 a 64 años", 
        ylab="Proporción", 
        xlab="Hábito de Fumar",
        args.legend = list(x = "topright"),
        cex.main=1,
        col=c("grey","white")
)
legend("topright",legend=rownames(tabla.prop),
       fill=c("grey","white"), horiz=TRUE, cex=0.8)
dev.off()
#Test de independencia
(test.chi<-chisq.test(mx_estudio, correct=F))

# 65 años o más
ct_estudio = c(42,165,7,28)
mx_estudio <- matrix(ct_estudio,ncol=2,byrow=TRUE)
colnames(mx_estudio) <- c("Fumadora","No Fumadora")
rownames(mx_estudio) <- c("No Sobrevive","Sobrevive")
mx_estudio_prop = prop.table(mx_estudio,margin = 1) #Para mostrar porcentajes

mx_estudio_tot = addmargins(mx_estudio,FUN = list(Total = sum), quiet = TRUE)
print(xtable(mx_estudio_tot, type = "latex"), file = "output.tex")

42/207-7/35
165/207-28/35
(42/207)/(7/35)
(165/207)/(28/35)

tikz('fig5.tex',width=3.5,height=3.5) 
barplot(mx_estudio_prop, ylim=c(0,1),
        beside=T, 
        main="65 años o más", 
        ylab="Proporción", 
        xlab="Hábito de Fumar",
        args.legend = list(x = "topright"),
        cex.main=1,
        col=c("grey","white")
)
legend("topright",legend=rownames(tabla.prop),
       fill=c("grey","white"), horiz=TRUE, cex=0.8)
dev.off()
#Test de independencia
(test.chi<-chisq.test(mx_estudio, correct=F))


# Ejercicio Nro 2
#Fuentes: https://datos.gob.ar/dataset/sspm-esquema-ahorro---inversion---financimmiento-administracion-publica-nacional-base-caja/archivo/sspm_372.9
#Esquema Ahorro - Inversión - Financiamiento. Administración Pública Nacional. Valores mensuales 2017. En millones de pesos.
apn_valores_mensuales = read.csv('administracion-publica-nacional-valores-mensuales-17.csv')
df_apn_valores_mensuales = apn_valores_mensuales[,c('ing_corr_2017', 'gtos_corr_2017')]

summary_table = summary(df_apn_valores_mensuales)
print(xtable(summary_table, type = "latex"), file = "output.tex")

tikz('fig6.tex',width=3.5,height=3.5) 
#Diagrama de dispersión entre acceso a internet y uso de facebook
plot(df_apn_valores_mensuales$ing_corr_2017, 
     df_apn_valores_mensuales$gtos_corr_2017, 
     main="Diagrama de dispersión", xlab="Ingresos corrientes", ylab="Gastos corrientes", col="blue", pch=16)
#Para añadir la recta de regresión estimada:
abline(lm(df_apn_valores_mensuales$ing_corr_2017 ~ df_apn_valores_mensuales$gtos_corr_2017), col="red") 
dev.off()

#Cálculo del coeficiente de correlación de Pearson:
(cor(df_apn_valores_mensuales$ing_corr_2017 , df_apn_valores_mensuales$gtos_corr_2017))

#Gráfico de dispersión entre los rangos asignados a X e Y:
tikz('fig7.tex',width=3.5,height=3.5) 
plot(rank(df_apn_valores_mensuales$ing_corr_2017), rank(df_apn_valores_mensuales$gtos_corr_2017), 
     pch=19, xlab="Rango(Ingresos corrientes)", ylab="rango(Gastos corrientes)",
     main="Gráfico de dispersión - Rangos") 
dev.off()
#coeficiente de correlación de Spearman:
cor(rank(df_apn_valores_mensuales$ing_corr_2017), rank(df_apn_valores_mensuales$gtos_corr_2017))




