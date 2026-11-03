# 1. Simulación de datos con pendientes paralelas (Relación lineal uniforme)
tratamiento <- factor(rep(c("Testigo", "Ferti_A", "Ferti_B"), each = 7))

altura_inicial<-c(
  13.88, 14.54, 18.12, 15.14, 15.26, 18.43, 15.92,
  12.47, 13.63, 14.11, 17.45, 15.72, 15.80, 15.22,
  13.89, 18.57, 16.00, 11.07, 16.40, 14.05, 12.86
)

rendimiento_final<- c(
  25.64, 25.98, 31.59, 27.21, 26.53, 33.31, 29.00,
  26.79, 30.44, 30.50, 34.93, 33.29, 33.40, 32.48,
  33.38, 40.30, 35.94, 28.35, 36.29, 32.52, 31.13
)

datos_validos <- data.frame(tratamiento, altura_inicial, rendimiento_final)

# 2. VERIFICACIÓN DEL SUPUESTO DE PARALELISMO (Interacción)
modelo_interaccion <- lm(rendimiento_final ~ altura_inicial * tratamiento, data = datos_validos)
anova(modelo_interaccion)


# 3. Ajustar el modelo final de ANCOVA (sin la interacción)
modelo_ancova_valido <- aov(rendimiento_final ~ altura_inicial + tratamiento, data = datos_validos)
summary(modelo_ancova_valido)

# 4. Calcular las Medias Ajustadas (Least Squares Means) con emmeans
if(!require(emmeans)) install.packages("emmeans")
library(emmeans)

medias_ajustadas <- emmeans(modelo_ancova_valido, ~ tratamiento)
print(medias_ajustadas)

# 5. Comparación de medias ajustadas (Tukey)
pairs(medias_ajustadas, adjust = "tukey")

tapply(rendimiento_final, tratamiento, mean, na.rm = TRUE)



# 1. Simulación de datos con covariable (Altura Inicial vs Altura Final)
tratamiento <- factor(rep(c("Control", "Abono_A", "Abono_B"), each = 6))
altura_inicial <- c(12, 14, 13, 15, 12, 13,   # Control
                    18, 17, 19, 20, 18, 19,   # Abono A (tenían ventaja inicial)
                    10, 11, 10, 12, 11, 10)   # Abono B
altura_final   <- c(25, 27, 26, 28, 25, 26,   # Control
                    38, 37, 40, 42, 39, 40,   # Abono A
                    30, 32, 31, 33, 31, 30)   # Abono B

datos_ancova <- data.frame(tratamiento, altura_inicial, altura_final)

# 2. Verificar el supuesto de paralelismo (interacción Tratamiento x Covariable)
modelo_supuesto <- lm(altura_final ~ altura_inicial * tratamiento, data = datos_ancova)
anova(modelo_supuesto)
# Si el p-value de la interacción es > 0.05, el supuesto de paralelismo se cumple.

# 3. Ejecutar el ANCOVA definitivo
modelo_ancova <- aov(altura_final ~ altura_inicial + tratamiento, data = datos_ancova)
summary(modelo_ancova)

# 4. Obtener las medias ajustadas por la covariable con 'emmeans'
if(!require(emmeans)) install.packages("emmeans")
library(emmeans)
medias_ancova <- emmeans(modelo_ancova, ~ tratamiento)
print(medias_ancova)

modelo_2 <- aov(altura_final ~ tratamiento, data = datos_ancova)
summary(modelo_2)

tapply(altura_final, tratamiento, mean, na.rm = TRUE)