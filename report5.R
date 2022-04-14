# install.packages("tidyverse")
library("tidyverse")

# Se lee el archivo de datos y se guarda en una variable
datos_pda <- read.csv("./Data/PobrezaDesempleoAsesinatos.csv")

# Función que devuelve las primeras n filas del conjunto de datos.
head(datos_pda)
names(datos_pda)

pairs(datos_pda)
view(cor(datos_pda))

# y = Estimate Std(Intercept) + Estimate Std(Porcentaje.desempleado) x
# y = 4.5312 + 2.1902x
regresion <- lm(Porcentaje.con.ingresos.debajo.de.5000 ~ Porcentaje.desempleado, data = datos_pda)
summary(regresion)

plot(datos_pda$Porcentaje.desempleado, datos_pda$Porcentaje.con.ingresos.debajo.de.5000, 
    xlab='Desempleo', ylab='Pobreza')
abline(regresion)

nuevas.Porcentaje.desempleado <- data.frame(Porcentaje.desempleado = seq(5, 10))
view(predict(regresion, nuevas.Porcentaje.desempleado))


# y = Estimate Std(Intercept) + Estimate Std(Asesinatos.por.1000000.habitantes) x
# y = 14.05204 + 0.27555x
regresion2 <- lm(Porcentaje.con.ingresos.debajo.de.5000 ~ Asesinatos.por.1000000.habitantes, data = datos_pda)
summary(regresion2)

plot(datos_pda$Asesinatos.por.1000000.habitantes, datos_pda$Porcentaje.con.ingresos.debajo.de.5000, 
    xlab='Asesinatos', ylab='Pobreza')
abline(regresion2)

nuevas.Asesinatos.por.1000000.habitantes <- data.frame(Asesinatos.por.1000000.habitantes = seq(30, 50))
view(predict(regresion2, nuevas.Asesinatos.por.1000000.habitantes))
