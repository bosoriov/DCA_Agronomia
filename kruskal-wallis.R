# 1. Simulación de datos con escala ordinal de daño (1 = sano, 5 = daño severo)
bioinsumo <- factor(rep(c("Testigo", "Producto_1", "Producto_2"), each = 6))
severidad_daño <- c(4, 5, 4, 5, 4, 5,    # Testigo (alto daño)
                    2, 3, 2, 1, 2, 2,    # Producto 1
                    1, 1, 2, 1, 1, 2)    # Producto 2

datos_np <- data.frame(bioinsumo, severidad_daño)

# 2. Intentar ANOVA clásico y verificar su falla en normalidad
mod_temp <- aov(severidad_daño ~ bioinsumo, data = datos_np)
shapiro.test(residuals(mod_temp)) # ¡Falla la normalidad (p < 0.05)!

# 3. Aplicar la prueba no paramétrica de Kruskal-Wallis
kruskal_res <- kruskal.test(severidad_daño ~ bioinsumo, data = datos_np)
print(kruskal_res)

# 4. Comparaciones múltiples no paramétricas (Paquete agricolae)
if(!require(agricolae)) install.packages("agricolae")
library(agricolae)

kruskal_comparacion <- kruskal(datos_np$severidad_daño, datos_np$bioinsumo, alpha = 0.05, console = TRUE)