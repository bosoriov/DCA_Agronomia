# 1. Generar datos de rendimiento de arroz con un outlier artificial
variedad <- factor(rep(c("V1", "V2", "V3"), each = 5))
rendimiento <- c(4.2, 4.5, 4.3, 4.4, 4.1,   # V1
                 5.1, 5.3, 5.0, 5.2, 9.8,   # V2 (El 9.8 es un claro outlier)
                 4.8, 4.6, 4.7, 4.9, 4.5)   # V3
datos_arroz <- data.frame(variedad, rendimiento)

# 2. Ajustar modelo
mod_arroz <- lm(rendimiento ~ variedad, data = datos_arroz)

# 3. Calcular residuos estudentizados
residuos_stud <- rstudent(mod_arroz)
print(residuos_stud)

# 4. Identificar visualmente con boxplots y puntos etiquetados
boxplot(rendimiento ~ variedad, data = datos_arroz, col = "lightblue",
        main = "Identificación visual de Outliers")
points(1:3, tapply(rendimiento, variedad, mean), col = "red", pch = 18, cex = 1.5)
