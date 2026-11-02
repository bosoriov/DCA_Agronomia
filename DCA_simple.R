# Ejercicio 1: DCA simple

# 1. Crear el conjunto de datos de germinación
temperatura <- factor(rep(c("T20", "T25", "T30", "T35"), each = 5))
germinacion <- c(65, 68, 62, 64, 66,   # T20
                 88, 90, 85, 87, 89,   # T25
                 92, 95, 91, 94, 93,   # T30
                 50, 52, 48, 51, 49)   # T35

datos_germinacion <- data.frame(temperatura, germinacion)

# 2. Resumen descriptivo de los datos
by(datos_germinacion$germinacion, datos_germinacion$temperatura, mean)

# 3. Ajuste del modelo lineal y Análisis de Varianza
modelo_dca <- aov(germinacion ~ temperatura, data = datos_germinacion)
summary(modelo_dca)
