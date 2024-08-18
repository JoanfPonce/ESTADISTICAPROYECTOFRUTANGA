

data = read.csv(file.choose(),head = TRUE, sep = ",")
data = data.frame(data)
data


# Supongamos que las otras variables son data_VAR1 y data_VAR2
# Crear un data frame con las variables
data_combined <- data.frame(Lunes = data$Lunes.1, 
                            Martes = data$Martes.1,
                            Miercoles = data$Miercoles.1,
                            Jueves = data$Jueves.1,
                            Viernes = data$Viernes.1,
                            Lunes2 = data$Lunes.2,
                            Martes2 = data$Martes.2
                            )
data_combined
na.omit(data_combined$Viernes)

data_combined# Generar el diagrama de cajas para todas las variables
boxplot(data_combined,
        main = "Diagramas de Cajas de Número de Clientes cada 5 Minutos por Día",
        xlab = "Dias",
        ylab = "(λ) Número de clientes cada 5 minutos",
        col = c("lightblue", "lightgreen", "lightcoral", "lightgoldenrod", "lightpink", "lightcyan", "lightyellow", "lightsalmon"), 
        border = "black",
        names = c("Lunes", "Martes", "Miercoles","Jueves","Viernes","Lunes 2","Martes 2"))

