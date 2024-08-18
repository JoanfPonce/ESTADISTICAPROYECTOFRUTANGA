data = read.csv(file.choose(),head = TRUE, sep = ",")
data = data.frame(data)

viernes <- na.omit(data$Viernes.1)
lunes <- na.omit(data$Lunes.1)
martes <- na.omit(data$Martes.1)
miercoles <- na.omit(data$Miercoles.1)
jueves <- na.omit(data$Jueves.1)
lunes2 <- na.omit(data$Lunes.2)
martes2 <- na.omit(data$Martes.2)

dias <- c("Lunes", "Martes", "Miércoles", "Jueves","Viernes",  "Lunes 2", "Martes 2")

medias <- c(mean(lunes), mean(martes), mean(miercoles), mean(jueves),mean(viernes), mean(lunes2), mean(martes2))
df <- data.frame(Dias = dias, Medias = medias)

#GRAFICA DE LINEA TODOS LOS DIAS

plot(df$Medias, 
     type = "o", 
     pch = 16,
     col = "#FF4500", 
     lwd = 3,  # Aumenta el grosor de la línea
     xlab = "Días", 
     ylab = "(λ) Número promedio de clientes cada 5 minutos",
     ylim = c(1,3),
     main = "Gráfico de Línea de Clientes Promedio por Día (cada 5 minutos)", 
     xaxt = "n")

axis(1, at = 1:length(dias), labels = dias)
grid()
box()



