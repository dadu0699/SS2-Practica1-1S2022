# install.packages("tidyverse")
library("tidyverse")

# Se lee el archivo de datos y se guarda en una variable
datos_desempenio_cpu <- read.csv("./Data/DesempenioCPU.csv")

# Función que devuelve las primeras n filas del conjunto de datos.
head(datos_desempenio_cpu)

# Se convierte el conjunto de datos en un dataframe 
df_desempenio_cpu <- data.frame(datos_desempenio_cpu)

# Variable que se utiliza para filtrar los datos, para acceder a una columna del data frame se utiliza el simbolo $
rendimiento_amdahl <- df_desempenio_cpu %>% 
    filter(Compania == "amdahl") %>%
    summarize(
        Promedio = mean(PRP), 
        Minimo = min(PRP), 
        Maximo = max(PRP))

# Creacion de un data frame con los resultados obtenidos
df_rendimiento_amdahl <- data.frame(
    Rendimiento=c("Promedio", "Maximo", "Minimo"),
    Valor=c(rendimiento_amdahl$Promedio, 
        rendimiento_amdahl$Maximo, 
        rendimiento_amdahl$Minimo))
view(df_rendimiento_amdahl)

# Grafica de barras de rendimiento
barplot(height = df_rendimiento_amdahl$Valor, names = df_rendimiento_amdahl$Rendimiento,
    main = "Gráfico de rendimiento de AMDahl", ylab = "Rendimiento", xlab = "")


# Variables que se utilizan para filtrar los datos y obtener los resultados para la grafica 2
rendimiento_por_compania <- df_desempenio_cpu %>% 
    group_by(Compania) %>% 
    summarize(
        Promedio = mean(PRP), 
        Minimo = min(PRP), 
        Maximo = max(PRP))
df_rendimiento_por_compania <- data.frame(rendimiento_por_compania)

filter_compania <- rendimiento_por_compania %>% 
    filter(Promedio == min(Promedio) | Promedio == max(Promedio))
df_filter_compania <- data.frame(filter_compania)
view(df_filter_compania)

# Grafica de barras de rendimiento
barplot(height = df_filter_compania$Promedio, names = df_filter_compania$Compania,
    main = "Gráfico de rendimiento del mejor y peor rendimiento promedio", ylab = "Rendimiento", xlab = "")