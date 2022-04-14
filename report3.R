# install.packages("agricolae")
# install.packages("tidyverse")

library("agricolae")
library("tidyverse")

# Se lee el archivo de datos y se guarda en una variable
datos_decesos <- read.csv("./Data/MuertesSexoEdad.csv")

# Función que devuelve las primeras n filas del conjunto de datos.
head(datos_decesos)

# Se convierte el conjunto de datos en un dataframe 
df_decesos <- data.frame(datos_decesos)

# Variable que se utiliza para filtrar los datos, para acceder a una columna del data frame se utiliza el simbolo $
filtro <- df_decesos %>% 
    filter(Age != "Total")
df_filtro <- data.frame(filtro)

# Variable para filtar por rango de edad
filtro_edad <- df_filtro %>% 
    group_by(Age) %>%
    summarize(Conteo = sum(Count))

# Frecuencia absoluta
frecuencia_absoluta <- filtro_edad$Conteo

# Frecuencia acumulada
frecuencia_acumulada <- cumsum(filtro_edad$Conteo)

# Frecuencia relativa
frecuencia_relativa <- prop.table(frecuencia_absoluta)

# Frecuencia relativa acumulada
frecuencia_relativa_acumulada <- cumsum(frecuencia_relativa)

# Creacion de un data frame con los resultados obtenidos
tabla_frecuencias <- cbind(frecuencia_absoluta, frecuencia_acumulada, frecuencia_relativa, frecuencia_relativa_acumulada)
df_tabla_frecuencias <- data.frame(tabla_frecuencias)
row.names(df_tabla_frecuencias) <- filtro_edad$Age
view(df_tabla_frecuencias)

# Histograma de frecuencia absoluta 
h <- graph.freq(frecuencia_absoluta, col = colors()[86], 
    main = "Histograma de frecuencias absolutas", ylab = "Frecuencia", xlab=" ")

summary(h)
plot(h, col = "gray", frequency = 1, 
    main = "Poligono de frecuencias absolutas", ylab = "Frecuencia", xlab=" ")
polygon.freq(h, col = "red", frequency = 1, lwd = 2)
