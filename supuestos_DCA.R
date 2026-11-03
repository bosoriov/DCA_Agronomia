# 1. Simulación de datos con asimetría típica de conteo de plagas
tratamiento <- factor(rep(c("Control", "Bio-insumo A", "Bio-insumo B", "Químico"), each = 6))
conteo_plaga <- c(2, 3, 1, 4, 2, 3,       # Control
                  10, 15, 12, 14, 11, 13,  # Bio A
                  25, 30, 28, 35, 27, 32,  # Bio B
                  1, 0, 2, 1, 2, 0)        # Químico
datos_plaga <- data.frame(tratamiento, conteo_plaga)

# 2. Ajuste del modelo inicial
modelo_plagas <- lm(conteo_plaga ~ tratamiento, data = datos_plaga)

# 3. Pruebas formales de supuestos
# Normalidad de residuos (Shapiro-Wilk)
shapiro.test(residuals(modelo_plagas))

# Homogeneidad de varianzas (Prueba de Levene)
if(!require(car)) install.packages("car")
library(car)
leveneTest(conteo_plaga ~ tratamiento, data = datos_plaga)

# 4. Gráficos de diagnóstico
par(mfrow = c(2, 2))
plot(modelo_plagas)

# 5. Aplicación de transformación Raíz Cuadrada (al ser datos de conteo)
datos_plaga$conteo_trans <- sqrt(datos_plaga$conteo_plaga)
modelo_trans <- lm(conteo_trans ~ tratamiento, data = datos_plaga)

# 6. Reevaluar normalidad y homocedasticidad con los datos transformados
shapiro.test(residuals(modelo_trans))
leveneTest(conteo_trans ~ tratamiento, data = datos_plaga)

# 7. Gráficos de diagnóstico
plot(modelo_trans)
par(mfrow = c(1, 1)) # Restablecer ventana gráfica

