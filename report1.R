# install.packages("tidyverse")
library("tidyverse")

# Se lee el archivo de datos y se guarda en una variable
datos_ventas <- read.csv("./Data/VENTAS.csv")

# Función que devuelve las primeras n filas del conjunto de datos.
head(datos_ventas)

# Se convierte el conjunto de datos en un dataframe 
df_ventas <- data.frame(datos_ventas)

# Variable que se utiliza para filtrar los datos, para acceder a una columna del data frame se utiliza el simbolo $
filtro <- df_ventas[df_ventas$Country == "Guatemala" | df_ventas$Country == "Belice" 
    | df_ventas$Country == "El Salvador" | df_ventas$Country == "Honduras" 
    | df_ventas$Country == "Nicaragua" | df_ventas$Country == "Costa Rica" 
    | df_ventas$Country == "Panama", ]

# Frecuencia absoluta
frecuencia_absoluta <- table(filtro$Country)

# Frecuencia acumulada
frecuencia_acumulada <- cumsum(frecuencia_absoluta)

# Frecuencia relativa
frecuencia_relativa <- prop.table(frecuencia_absoluta)

# Frecuencia relativa acumulada
frecuencia_relativa_acumulada <- cumsum(frecuencia_relativa)

# Creacion de un data frame con los resultados obtenidos
tabla_frecuencias <- cbind(frecuencia_absoluta, frecuencia_acumulada, frecuencia_relativa, frecuencia_relativa_acumulada)
df_tabla_frecuencias <- data.frame(tabla_frecuencias)
view(df_tabla_frecuencias)

# Grafica de barras de la frecuencia absoluta
barplot(frecuencia_absoluta, main = "Gráfico de frecuencias absolutas", ylab = "Frecuencias", xlab = "Países")

# Histograma de frecuencia acumulada 
hist(frecuencia_acumulada, main = "Histograma de frecuencias acumuladas", ylab = "Frecuencia", xlab="")

# Calculando el número de barras como la función hist()
nbreaks <- pretty(range(frecuencia_acumulada), n = nclass.Sturges(frecuencia_acumulada), min.n = 1)
ggplot(data.frame(frecuencia_acumulada), aes(x = frecuencia_acumulada)) + 
    geom_histogram(aes(y = ..density..), breaks = nbreaks, color = "gray", fill = "white") + 
    geom_density(fill = "black", alpha = 0.2)
