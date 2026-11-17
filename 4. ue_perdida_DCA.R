# 1. Crear el dataset incluyendo un dato perdido (NA)
tratamiento <- factor(rep(c("T1", "T2", "T3"), each = 5))
rendimiento <- c(4.2, 4.5, 4.3, 4.4, 4.1,   # T1 (5 repeticiones)
                 3.8, 3.9, NA,  3.7, 4.0,   # T2 (1 dato perdido / NA)
                 5.1, 5.3, 5.0, 5.2, 4.9)   # T3 (5 repeticiones)

datos_perdidos <- data.frame(tratamiento, rendimiento)
print(datos_perdidos)

# 2. Ajustar el modelo lineal (R omite automáticamente el NA con una advertencia)
modelo_perdido <- lm(rendimiento ~ tratamiento, data = datos_perdidos)

# 3. ANOVA con Sumas de Cuadrados Tipo III (recomendado para datos desbalanceados)
if(!require(car)) install.packages("car")
library(car)
Anova(modelo_perdido, type = "III")

# 4. Obtener las medias ajustadas (Least Squares Means) correctas para datos desbalanceados
if(!require(emmeans)) install.packages("emmeans")
library(emmeans)

medias_ajustadas <- emmeans(modelo_perdido, ~ tratamiento)
print(medias_ajustadas)

# Comparación de medias con Tukey para datos desbalanceados
pairs(medias_ajustadas, adjust = "tukey")


tapply(rendimiento, tratamiento, mean, na.rm = TRUE)
