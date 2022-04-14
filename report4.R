# install.packages("tidyverse")
library("tidyverse")

# Se lee el archivo de datos y se guarda en una variable
datos_presion_sanguinea <- read.csv("./Data/PresionSanguineaEdad.csv")

# Función que devuelve las primeras n filas del conjunto de datos.
head(datos_presion_sanguinea)

pairs(datos_presion_sanguinea)
view(cor(datos_presion_sanguinea))

# y = Estimate Std(Intercept) + Estimate Std(Edad) x
# y = 98.7147 + 0.9709x
regresion <- lm(Systolic.Blood.Pressure ~ Edad, data = datos_presion_sanguinea)
summary(regresion)

plot(datos_presion_sanguinea$Edad, datos_presion_sanguinea$Systolic.Blood.Pressure, 
    xlab='Edad', ylab='Presión sanguínea')
abline(regresion)

nuevas.Edad <- data.frame(Edad = seq(30, 50))
view(predict(regresion, nuevas.Edad))
