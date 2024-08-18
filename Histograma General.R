data = read.csv(file.choose(),head = TRUE, sep = ",")
data = data.frame(data)
data


dataMiercoles = na.omit(data$Miercoles.1)
dataMiercoles
mean(dataMiercoles)
sd(dataMiercoles)
median(dataMiercoles)
moda <- as.numeric(names(table(dataMiercoles)[table(dataMiercoles) == max(table(dataMiercoles))]))
moda

#HISTOGRAMA GENERAL


data_TOTAL <- c(na.omit(data$Viernes.1),
                   na.omit(data$Lunes.1),
                   na.omit(data$Martes.1),
                   na.omit(data$Miercoles.1),
                   na.omit(data$Jueves.1),
                   na.omit(data$Lunes.2),
                   na.omit(data$Martes.2))

hist(data_TOTAL,
     breaks = seq(-0.5, 12.5, by = 1),  # Intervalos centrados para cada valor desde 0 hasta 12
     main = "Histograma de Número de Clientes Cada 5 minutos (General)",
     xlab = "Clientes",
     xlim = c(0, 12),  # Asegura que el eje X vaya de 0 a 12
     ylab = "Frecuencia Absoluta",
     ylim = c(0, 80),  # Limita el eje Y entre 0 y 120 (ajústalo según tus datos)
     axes = FALSE,  # Desactiva los ejes por defecto
     label = TRUE,
     col = "lightblue"
)
axis(1, at = 0:12, labels = 0:12)
axis(2)
grid()
box()

lambda_all <- mean(data_TOTAL)
hist(data_TOTAL,
     breaks = seq(-0.5, 12.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (General)",
     xlab = "Clientes",
     ylab = "Frecuencia Relativa",
     xlim = c(0, 12),
     axes = TRUE,
     label = TRUE,
     probability = TRUE,
     col = "lightblue",
     ylim = c(0, 0.3)
)
axis(1, at = 0:12, labels = 0:12)
axis(2)
x_vals <- c(-1, 0:max(data_TOTAL))
pois_vals <- c(0, dpois(0:max(data_TOTAL), lambda_all))
lines(x_vals, pois_vals, col = "blue", lwd = 2)  
grid()
box()





