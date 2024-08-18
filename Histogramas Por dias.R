# Lista de nombres de las columnas/días
data = read.csv(file.choose(),head = TRUE, sep = ",")
data = data.frame(data)
# Para Viernes.1
dataV = na.omit(data$Viernes.1)
hist(dataV,
     breaks = seq(-0.5, 12.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Viernes)",
     xlab = "Clientes",
     ylab = "Frecuencia Absoluta",
     ylim = c(0,20),
     axes = TRUE,
     label = TRUE,
     col = "slategray1",
)
grid()
box()
axis(1, at = 0:12, labels = 0:12)
axis(2)

lambda <- mean(dataV)
hist(dataV,
     breaks = seq(-0.5, 12.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Viernes)",
     xlab = "Clientes",
     ylab = "Frecuencia Relativa",
     axes = TRUE,
     label = TRUE,
     probability = TRUE,
     col = "slategray1",
     ylim = c(0, 0.35)  
)
axis(1, at = 0:12, labels = 0:12)
axis(2)
x_vals <- c(-1, 0:max(dataV))
pois_vals <- c(0, dpois(0:max(dataV), lambda))
lines(x_vals, pois_vals, col = "blue", lwd = 2)  
grid()
box()

# Para Lunes.1
dataV = na.omit(data$Lunes.1)
hist(dataV,
     breaks = seq(-0.5, 12.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Lunes)",
     xlab = "Clientes",
     ylab = "Frecuencia Absoluta",
     ylim = c(0,15),
     axes = TRUE,
     label = TRUE,
     col = "slategray1",
)
grid()
box()
axis(1, at = 0:12, labels = 0:12)
axis(2)

lambda <- mean(dataV)
hist(dataV,
     breaks = seq(-0.5, 12.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Lunes)",
     xlab = "Clientes",
     ylab = "Frecuencia Relativa",
     axes = TRUE,
     label = TRUE,
     probability = TRUE,
     col = "slategray1",
     ylim = c(0, 0.35)  
)
axis(1, at = 0:12, labels = 0:12)
axis(2)
x_vals <- c(-1, 0:max(dataV))
pois_vals <- c(0, dpois(0:max(dataV), lambda))
lines(x_vals, pois_vals, col = "blue", lwd = 2)  
grid()
box()

# Para Martes.1
dataV = na.omit(data$Martes.1)
hist(dataV,
     breaks = seq(-0.5, 8.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Martes)",
     xlab = "Clientes",
     ylab = "Frecuencia Absoluta",
     ylim = c(0,15),
     axes = TRUE,
     label = TRUE,
     col = "slategray1",
)
grid()
box()
axis(1, at = 0:12, labels = 0:12)
axis(2)

lambda <- mean(dataV)
hist(dataV,
     breaks = seq(-0.5, 8.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Martes)",
     xlab = "Clientes",
     ylab = "Frecuencia Relativa",
     axes = TRUE,
     label = TRUE,
     probability = TRUE,
     col = "slategray1",
     ylim = c(0, 0.35)  
)
axis(1, at = 0:12, labels = 0:12)
axis(2)
x_vals <- c(-1, 0:max(dataV))
pois_vals <- c(0, dpois(0:max(dataV), lambda))
lines(x_vals, pois_vals, col = "blue", lwd = 2)  
grid()
box()

# Para Miercoles.1
dataV = na.omit(data$Miercoles.1)
hist(dataV,
     breaks = seq(-0.5, 12.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Miercoles)",
     xlab = "Clientes",
     ylab = "Frecuencia Absoluta",
     ylim = c(0,20),
     axes = TRUE,
     label = TRUE,
     col = "slategray1",
)
grid()
box()
axis(1, at = 0:12, labels = 0:12)
axis(2)

lambda <- mean(dataV)
hist(dataV,
     breaks = seq(-0.5, 12.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Miercoles)",
     xlab = "Clientes",
     ylab = "Frecuencia Relativa",
     axes = TRUE,
     label = TRUE,
     probability = TRUE,
     col = "slategray1",
     ylim = c(0, 0.35)  
)
axis(1, at = 0:12, labels = 0:12)
axis(2)
x_vals <- c(-1, 0:max(dataV))
pois_vals <- c(0, dpois(0:max(dataV), lambda))
lines(x_vals, pois_vals, col = "blue", lwd = 2)  
grid()
box()

# Para Jueves.1
dataV = na.omit(data$Jueves.1)
hist(dataV,
     breaks = seq(-0.5, 8.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Jueves)",
     xlab = "Clientes",
     ylab = "Frecuencia Absoluta",
     ylim = c(0,20),
     axes = TRUE,
     label = TRUE,
     col = "slategray1",
)
grid()
box()
axis(1, at = 0:12, labels = 0:12)
axis(2)

lambda <- mean(dataV)
hist(dataV,
     breaks = seq(-0.5, 8.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Jueves)",
     xlab = "Clientes",
     ylab = "Frecuencia Relativa",
     axes = TRUE,
     label = TRUE,
     probability = TRUE,
     col = "slategray1",
     ylim = c(0, 0.35)  
)
axis(1, at = 0:12, labels = 0:12)
axis(2)
x_vals <- c(-1, 0:max(dataV))
pois_vals <- c(0, dpois(0:max(dataV), lambda))
lines(x_vals, pois_vals, col = "blue", lwd = 2)  
grid()
box()

# Para Lunes.2
dataV = na.omit(data$Lunes.2)
hist(dataV,
     breaks = seq(-0.5, 5.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Lunes 2)",
     xlab = "Clientes",
     ylab = "Frecuencia Absoluta",
     ylim = c(0,15),
     axes = TRUE,
     label = TRUE,
     col = "slategray1",
)
grid()
box()
axis(1, at = 0:12, labels = 0:12)
axis(2)

lambda <- mean(dataV)
hist(dataV,
     breaks = seq(-0.5, 5.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Lunes 2)",
     xlab = "Clientes",
     ylab = "Frecuencia Relativa",
     axes = TRUE,
     label = TRUE,
     probability = TRUE,
     col = "slategray1",
     ylim = c(0, 0.35)  
)
axis(1, at = 0:12, labels = 0:12)
axis(2)
x_vals <- c(-1, 0:max(dataV))
pois_vals <- c(0, dpois(0:max(dataV), lambda))
lines(x_vals, pois_vals, col = "blue", lwd = 2)  
grid()
box()

# Para Martes.2
dataV = na.omit(data$Martes.2)
hist(dataV,
     breaks = seq(-0.5, 6.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Martes 2)",
     xlab = "Clientes",
     ylab = "Frecuencia Absoluta",
     ylim = c(0,5),
     axes = TRUE,
     label = TRUE,
     col = "slategray1",
)
grid()
box()
axis(1, at = 0:12, labels = 0:12)
axis(2)


lambda <- mean(dataV)
hist(dataV,
     breaks = seq(-0.5, 6.5, by = 1),
     main = "Histograma de Número de Clientes Cada 5 minutos (Martes 2)",
     xlab = "Clientes",
     ylab = "Frecuencia Relativa",
     axes = TRUE,
     label = TRUE,
     probability = TRUE,
     col = "slategray1",
     ylim = c(0, 0.80)  
)
axis(1, at = 0:12, labels = 0:12)
axis(2)
x_vals <- c(-1, 0:max(dataV))
pois_vals <- c(0, dpois(0:max(dataV), lambda))
lines(x_vals, pois_vals, col = "blue", lwd = 2)  
grid()
box()

