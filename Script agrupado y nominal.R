#activacion de librerias####
library(readr)
library(tidyverse)
library(dplyr)
library(knitr)
library(ggplot2)
library(readxl)
library(highcharter)
library(lubridate)
library(gridExtra) #para las tablitas de indicadores


#preparacion de base agrupada----
{
  #lectura de base y chequeo de formato dataframe
agrupada <- read.csv("NEUQUEN_CLI.CSV", sep = ";",na.strings = "")
sum(complete.cases(agrupada))

nrow(agrupada)

#selecciono hasta la SE de mi BEM
agrupada <- agrupada %>%
  filter(ANIO <= 2024 & SEMANA <= 53)


#Para ver un resumen de mi dataframe se usa la funcion str
#para leer sobre el uso de funcion STR. Son 2 modos de llegar a la misma ayuda
?str  
help("str")
str(agrupada)}

#agrego columna region----
{
#lectura de la base 
regiones <- read_excel("REGIONES.xlsx")
regiones <- as.data.frame(regiones) #cambio formato a dataframe


#cruzo base de datos (es como un buscar V en excel)
?left_join

regiones_duplicadas <- regiones %>% #esto lo hago porque habia duplicados que agregaban observaciones a "agrupadas"
  group_by(LOCALIDAD) %>%
  filter(n() > 1)

regiones <- regiones%>%
  distinct(LOCALIDAD, .keep_all = TRUE)

#agrego la columna regiones e ID regiones a agrupadas
agrupada <- agrupada %>%
  left_join(regiones, by = "LOCALIDAD")
}
#columna se-año, SE MAX, MIN----
{#armo la variable SE-año
agrupada <- agrupada %>%
  mutate(SE_ANIO= paste(SEMANA, ANIO, sep = "-"))

##SE/año del BEM-----
#las SE/año del BEM de este mes son 48,49,50,51 y 52 (esto es un ayuda memoria) del año 2024#SE maxima
SE_BEM <- c(48, 49, 50, 51, 52) #hay que cambiarlo mensualmente
SE_BEM <- as.vector(SE_BEM)

#ANIO maximo (lo voy a usar para tablas, gráficos)
ANIO_max <- agrupada %>% 
  summarise(ANIO_max = max(ANIO, na.rm = TRUE))

ANIO_max <- as.numeric(ANIO_max[[1]])  # Extrae la primera columna y lo convierte a numérico


view(ANIO_max)

#SE_min 
SE_min <- SE_BEM %>%
  min()
  
#SE_max 
SE_max <- SE_BEM %>%
  max()
}

#Diarreas----
diarreas <- agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP==11)

#set de indicadores
#tabla con el total y la variacion porcentual 

#selecciono solo las SE del BEM para hacer mi objeto del total de las DA de este BEM

DA_cantidad_SE_BEM <- diarreas %>%
  filter(ANIO == 2024, SEMANA   %in% c(48, 49, 50, 51, 52)) %>% #Es importante ir cambiando el año y las SE según corresponda al BEM
  summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE))
  



#selecciono las SE del año pasado (es para hacer la variacion porcentual del set de indicadores)
DA_cantidad_SE_BEM_anioanterior <- diarreas %>%
  filter(ANIO == 2023, SEMANA %in% c(48, 49, 50, 51, 52)) %>% #Es importante ir cambiando el año y las SE según corresponda al BEM
  summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE)) 


#variacion porcentual 
DA_variacion_porcentual <- 
  round((DA_cantidad_SE_BEM - DA_cantidad_SE_BEM_anioanterior)/DA_cantidad_SE_BEM_anioanterior*100,1)



#DA evolutivo 
{#ARMO UNA TABLA 
DA_evolutivo <- diarreas %>% 
  group_by(ANIO, SEMANA) %>% 
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>% 
  mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
  arrange(ANIO, SEMANA) %>% 
  mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
  as.data.frame()

#por qué lo convierto a factor? Mostrar imagen. Si es un factor permite el orden cronologico

#objeto total de diarreas histórico
DA_total_evolutivo <- diarreas %>% 
    summarise(Total = sum(CANTIDAD))
  

#gráfico DA_evolutivo
grafico_DA_evolutivo <- DA_evolutivo %>% 
  ggplot(aes(x = ANIO_SE, y = Total)) +
  geom_bar(stat = "identity", fill = "orange",color = "#FF8C00", width = 0.5) +
  scale_x_discrete(
    breaks = levels(DA_evolutivo$ANIO_SE)[seq(1, length(levels(DA_evolutivo$ANIO_SE)), by = 5)],
    expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0, max(DA_evolutivo$Total, na.rm = TRUE), by = 200),
    expand = c(0, 0))+
  labs(
    x = "SE-año",
    y = "Casos de diarrea aguda") +
  theme_classic () +
  theme(
    axis.title = element_text(size = 15),
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 12),  # Fuente para los textos del eje Y
    panel.border = element_blank(),  # Eliminar borde del panel
    axis.line = element_blank(),  # Eliminar líneas de los ejes
    axis.ticks = element_blank())  # Eliminar "guioncito" de los ejes
  
grafico_DA_evolutivo
}


#DA por grupos etarios acumulado
{#Armar una tabla para hacer un graf de DA por grupos etarios acumulado
  DA_tabla_grupoetario_acumulado <- diarreas %>% 
    group_by(ANIO, SEMANA, GRUPO) %>%  
    summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>%  # Sumar por grupo etario
    mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
    arrange(ANIO, SEMANA) %>% 
    mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
    as.data.frame()
  
  # Ver la tabla
  view(DA_tabla_grupoetario_acumulado) #chequeo para ver que esté ok
  
  #agrupo segun grupos etarios del BEM 
  DA_tabla_grupoetario_acumulado <- DA_tabla_grupoetario_acumulado %>%
    mutate(
      GRUPO_2 = case_when(
        GRUPO == "< 6 m"~ "< 15 años",
        GRUPO == "6 a 11 m"~ "< 15 años",
        GRUPO == "12 a 23 m"~ "< 15 años",
        GRUPO == "2 a 4"~ "< 15 años",
        GRUPO == "5 a 9"~ "< 15 años",
        GRUPO == "10 a 14"~ "< 15 años",
        GRUPO == "15 a 19"~ "15 a 24 años",
        GRUPO == "20 a 24"~ "15 a 24 años",
        GRUPO == "25 a 34"~ "25 a 44 años",
        GRUPO == "35 a 44"~ "25 a 44 años",
        GRUPO == "45 a 54"~ "45 a 64 años",
        GRUPO == "45 a 64"~ "45 a 64 años",
        GRUPO == "55 a 64"~ "45 a 64 años",
        GRUPO == "65 a 74"~ "65 años y más",
        GRUPO == ">= a 75"~ "65 años y más",
        TRUE ~ GRUPO))
  
  
  DA_tabla_grupoetario_acumulado$GRUPO_2 <- factor(DA_tabla_grupoetario_acumulado$GRUPO_2, 
                                                   levels = c("< 15 años","15 a 24 años",
                                                              "25 a 44 años","45 a 64 años", 
                                                              "65 años y más"))
  

  
  # Crear gráfico de columnas apiladas
  DA_grafico_grupoetario_acumulado <- DA_tabla_grupoetario_acumulado %>% 
    filter(!is.na(GRUPO_2)) %>%  # Eliminar filas con NA en GRUPO_2
    ggplot(aes(x = ANIO_SE, y = Total, fill = GRUPO_2)) +
    geom_bar(stat = "identity", position = "fill") + #position fill es para apilado al 100%
    scale_x_discrete(
      breaks = levels(DA_tabla_grupoetario_acumulado$ANIO_SE)[seq(1, length(levels(DA_tabla_grupoetario_acumulado$ANIO_SE)), by = 5)],
      expand = c(0, 0)) +
    scale_y_continuous(
      labels = scales::percent,  # Cambiar el formato de las etiquetas a porcentaje
      breaks = seq(0, 1, by = 0.1))+ # Poner los breaks del eje Y entre 0 y 1, con intervalos de 0.1
    scale_fill_manual(values = c(
      "< 15 años" = "#ff8c00",
      "15 a 24 años" = "#be9500",  
      "25 a 44 años" = "#7a9500",  
      "45 a 64 años" = "#218f06", 
      "65 años y más" = "#00833c")) +  
    labs(
      x = "SE-año",
      y = "% de casos de diarrea aguda",
      fill = "Grupo de edad") +
    theme_classic() +
    theme(
      axis.title = element_text(size = 20),
      axis.text.x = element_text(size = 20, angle = 90, hjust = 1),  # Rotar etiquetas en X
      axis.text.y = element_text(size = 20),  # Fuente para los textos del eje Y
      axis.title.x = element_text(size = 10),  # Cambia el tamaño del título del eje X
      axis.title.y = element_text(size = 10),
      panel.border = element_blank(),  # Eliminar borde del panel
      axis.line = element_blank(),  # Eliminar líneas de los ejes
      axis.ticks = element_blank())  # Eliminar "guioncito" de los ejes
  
  DA_grafico_grupoetario_acumulado
}


#DA por grupos etarios SE del BEM
{#Armar una tabla para hacer un graf de DA por grupos etarios SE del BEM
  DA_tabla_grupoetario <- diarreas %>%
  filter(ANIO == 2024, SEMANA %in% c(48, 49, 50, 51, 52)) %>% # Cambiar ANIO y SEMANA según corresponda
  group_by(GRUPO) %>% 
    summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>%
    mutate(
    GRUPO_2 = case_when(
      GRUPO == "< 6 m"~ "< 6 meses",
      GRUPO == "6 a 11 m"~ "6 a 11 meses",
      GRUPO == "12 a 23 m"~ "12 a 23 meses",
      GRUPO == "2 a 4"~ "2 a 4 años",
      GRUPO == "5 a 9"~ "5 a 9 años",
      GRUPO == "10 a 14"~ "10 a 14 años",
      GRUPO == "15 a 19"~ "15 a 19 años",
      GRUPO == "20 a 24"~ "20 a 24 años",
      GRUPO == "25 a 34"~ "25 a 34 años",
      GRUPO == "35 a 44"~ "35 a 44 años",
      GRUPO == "45 a 64"~ "45 a 64 años",
      GRUPO == "65 a 74"~ "65 a 74 años",
      GRUPO == ">= a 75"~ ">= a 75 años",
       TRUE ~ GRUPO)) 
      



DA_tabla_grupoetario <- as.data.frame(DA_tabla_grupoetario)

#hago la columna GRUPO2 factor para poder ordenarlo

DA_tabla_grupoetario$GRUPO_2 <- factor(DA_tabla_grupoetario$GRUPO_2, 
                                       levels = c("< 6 meses","6 a 11 meses",
                                                  "12 a 23 meses","2 a 4 años", "5 a 9 años",
                                                  "10 a 14 años", "15 a 19 años",
                                                  "20 a 24 años", "25 a 34 años", "35 a 44 años",
                                                  "45 a 64 años","65 a 74 años",">= a 75 años"))

#gráfico DA grupo de edad
DA_grafico_grupoetario <- DA_tabla_grupoetario %>% 
  ggplot(aes(x=Total, y=GRUPO_2))+
  geom_bar(stat = "identity", fill = "orange", width = 0.5)+
  labs(
    x = "Casos de diarrea aguda",
    y = "Grupos de edad") +
  scale_x_continuous(breaks = seq(0, max(DA_tabla_grupoetario$Total), by = 50)) +
  theme_classic () +
  theme(
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6), 
    axis.title.x = element_text(size = 6),  # Cambia el tamaño del título del eje X
    axis.title.y = element_text(size = 6),# Fuente para los textos del eje Y
    panel.border = element_blank(),  # Eliminar borde del panel
    axis.line = element_blank(),  # Eliminar líneas de los ejes
    axis.ticks = element_blank())  # Eliminar "guioncito" de los ejes
DA_grafico_grupoetario 
}

#DA por regiones segun grupo de edad
{
#Armar una tabla 
DA_tabla_regiones_grupoetario <- diarreas %>%
  filter(ANIO == 2024, SEMANA %in% c(48, 49, 50, 51, 52)) %>% # Cambiar ANIO y SEMANA según corresponda
  group_by(GRUPO, REGIONES) %>%
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")

#agrupo segun grupos etarios del BEM 
DA_tabla_regiones_grupoetario <- DA_tabla_regiones_grupoetario %>%
  mutate(
    GRUPO_2 = case_when(
      GRUPO == "< 6 m"~ "0 a 4 años",
      GRUPO == "6 a 11 m"~ "0 a 4 años",
      GRUPO == "12 a 23 m"~ "0 a 4 años",
      GRUPO == "2 a 4"~ "0 a 4 años",
      GRUPO == "5 a 9"~ "5 a 9 años",
      GRUPO == "10 a 14"~ "10 a 19 años",
      GRUPO == "15 a 19"~ "10 a 19 años",
      GRUPO == "20 a 24"~ "20 a 44 años",
      GRUPO == "25 a 34"~ "20 a 44 años",
      GRUPO == "35 a 44"~ "20 a 44 años",
      GRUPO == "45 a 54"~ "45 a 64 años",
      GRUPO == "45 a 64"~ "45 a 64 años",
      GRUPO == "55 a 64"~ "45 a 64 años",
      GRUPO == "65 a 74"~ "65 años y más",
      GRUPO == ">= a 75"~ "65 años y más",
      TRUE ~ GRUPO))

DA_tabla_regiones_grupoetario$GRUPO_2 <- factor(DA_tabla_regiones_grupoetario$GRUPO_2, 
                                       levels = c("0 a 4 años","5 a 9 años",
                                                  "10 a 19 años","20 a 44 años", "45 a 64 años",
                                                  "65 años y más"))

#grafico por regiones segun grupo etario

DA_grafico_regiones <- 
  DA_tabla_regiones_grupoetario %>% 
  filter(!is.na(REGIONES)) %>%  # Eliminar filas donde REGIONES es NA
  ggplot(aes(x=Total, y=GRUPO_2))+
  geom_bar(stat = "identity", fill = "orange", width = 0.5) +  
             facet_wrap(~ REGIONES, ncol=3) +  # Facetear por la columna REGIONES
             labs(
               x = "Casos de diarrea aguda",
               y = "Grupos de edad"
             ) +
             theme_classic() +
             theme(
               axis.text.x = element_text(size = 7),
               axis.text.y = element_text(size = 7),
               axis.title.x = element_text(size = 7),
               axis.title.y = element_text(size = 7))

DA_grafico_regiones 
}         


##########################FIN DA####


#Internaciones por lesiones en el hogar----
{
lesiones_hogar <- agrupada %>% 
  filter(IDEVENTOAGRUPADO==116) %>% 
  filter((ANIO > 2023) | (ANIO == 2023 & SEMANA >= 21))

#Cantidad segun SE BEM
lesiones_hogar_SE_BEM <- lesiones_hogar %>%
  filter(ANIO == 2024, SEMANA   %in% c(48, 49, 50, 51, 52)) %>% #Es importante ir cambiando el año y las SE según corresponda al BEM
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")
 



#tabla_lesiones_ evolutivo
lesiones_evolutivo <- lesiones_hogar %>%
  group_by(SEMANA,ANIO) %>%  
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")%>%   # Suma la columna cantidad, ignorando NA
  mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
  arrange(ANIO, SEMANA) %>% 
  mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
  as.data.frame()
  
  

#objeto n lesiones
lesiones_total <- sum(lesiones_evolutivo$Total, na.rm = TRUE)



#grafico evolutivo lesiones

lesiones_grafico_evolutivo <- lesiones_evolutivo %>% 
  ggplot(aes(x = ANIO_SE,  y = Total)) +
  geom_bar(stat = "identity", fill = "#21618c", width = 0.5) +
  scale_x_discrete(
    breaks = levels(lesiones_evolutivo$ANIO_SE)[seq(1, length(levels(lesiones_evolutivo$ANIO_SE)), by =3)])+
  scale_y_continuous(
    breaks = seq(0, max(lesiones_evolutivo$Total, na.rm = TRUE), by = 10),
    expand = c(0, 0)) +
  labs(
    x = "SE-año",
    y = "Lesiones en el hogar") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank())

# Mostrar el gráfico
lesiones_grafico_evolutivo 
}

##############FIN LESIONES EN EL HOGAR##########################################


#Lesiones por mordedura de perro (ambulatorias)----
{#mordeduras evolutivo
mordedura_perro <- agrupada %>% 
    filter(ID_SNVS_EVENTO_AGRP %in% c(512, 341))
  

#tabla mordedura evolutivo

mordeduras_evolutivo <- mordedura_perro %>%
  group_by(SEMANA, ANIO) %>%  
  summarise(total_cantidad = sum(CANTIDAD), .groups = "drop") %>%  # Suma la columna cantidad, ignorando NA
  arrange(ANIO, SEMANA) %>% 
  mutate(SE_ANIO = paste(ANIO, SEMANA, sep = "-")) %>%  # Crear la variable como texto: "ANIO-SEMANA"
  mutate(SE_ANIO = factor(SE_ANIO, levels = unique(SE_ANIO)))  # Convertir a factor, asegurando el orden cronológico


#objeto n mordeduras
mordeduras_total <- sum(mordeduras_evolutivo$total_cantidad, na.rm = TRUE)


#grafico evolutivo
mordeduras_grafico_evolutivo <- mordeduras_evolutivo %>% 
  ggplot(aes(x = SE_ANIO, y = total_cantidad)) +
  geom_bar(stat = "identity", fill = "#7dd473", width = 0.5) +
  scale_x_discrete(
  breaks = levels(mordeduras_evolutivo$SE_ANIO)[seq(1, length(levels(mordeduras_evolutivo$SE_ANIO)), by = 5)],
      expand = c(0, 0)) +  
      labs(
    x = "SE-año",
    y = "Casos de mordeduras de perros") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 12),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank())
mordeduras_grafico_evolutivo
}

#mordeduras incidencia
{
#calculo de incidencia
#cargo la base de poblaciones (es la misma que están usando en el BEM)
poblacion <- read_excel("Poblacion.xlsx")

#armo la tabla

mordedura_incidencia <- mordedura_perro %>%
  filter(SEMANA %in% SE_BEM) %>% #estoy usando el objeto SE_BEM asique mucha atención a poner las SE correctas del BEM
  group_by(REGIONES, ANIO) %>%
  summarise(
    total_cantidad = sum(CANTIDAD), 
    .groups = "drop")


#agrego la columna poblacion con leftjoin
mordedura_incidencia <- mordedura_incidencia %>%
  left_join(poblacion, by = c("ANIO", "REGIONES"))


#agrego el calculo de incidencia 
mordedura_incidencia <- mordedura_incidencia %>%
  mutate(
    total_cantidad = as.numeric(total_cantidad),  # cambio formato
    poblacion = as.numeric(poblacion),  # cambio formato
    incidencia = ifelse(!is.na(total_cantidad) & !is.na(poblacion), 
                        round((total_cantidad / poblacion) * 10000, 1), 
                        NA))


mordedura_incidencia <- as.data.frame(mordedura_incidencia)


#grafico incidencia
mordedura_incidencia_grafico <- mordedura_incidencia %>% 
  filter(!is.na(incidencia)) %>%  # Filtrar para excluir NA en incidencia
  ggplot(aes(x = incidencia, y = factor(ANIO))) +  # Años en el eje Y, ordenados cronológicamente
  geom_col(stat = "identity", fill = "#7dd473", width = 0.5) +  # Gráfico de barras horizontales
  geom_text(aes(label = round(incidencia, 1)),  # Etiquetas con valores redondeados
            hjust = -0.2,  # Desplaza las etiquetas a la derecha
            size = 3) +  # Tamaño del texto de las etiquetas
  facet_wrap(~ REGIONES, ncol = 3) +  # Facetear por región, 2 columnas
  labs(
    x = "Incidencia acumulada cada 10000 habitantes",
    y = "Año") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    strip.text = element_text(size = 10))  # Tamaño del texto en los facetes

mordedura_incidencia_grafico

}
##############FIN Lesiones por mordedura de perro (ambulatorias)##########################################

#Internaciones por siniestros viales----

{#sinistrios viales evolutivo
  siniestros_viales <- agrupada %>% 
    filter(IDEVENTOAGRUPADO == 115) %>% 
    filter((ANIO > 2023) | (ANIO == 2023 & SEMANA >= 21))
  
  
#armo la tabla evolutivo
siniestros_viales_evolutivo <- siniestros_viales %>%
    group_by(SEMANA, ANIO) %>%  
    summarise(total_cantidad = sum(CANTIDAD), .groups = "drop") %>%  # Suma la columna cantidad, ignorando NA
    arrange(ANIO, SEMANA) %>% 
    mutate(SE_ANIO = paste(ANIO, SEMANA, sep = "-")) %>%  # Crear la variable como texto: "ANIO-SEMANA"
    mutate(SE_ANIO = factor(SE_ANIO, levels = unique(SE_ANIO)))  # Convertir a factor, asegurando el orden cronológico 
  
#armo objeto total de siniestros viales
siniestros_viales_total <- siniestros_viales_evolutivo %>% 
summarise(total_Cantidad = sum(total_cantidad, na.rm = TRUE))

#armo  total SE BEM de siniestros viales SE BEM
siniestros_viales_SEBEM <- siniestros_viales_evolutivo %>% 
  filter((ANIO == ANIO_max & SEMANA %in% SE_BEM)) %>%
  summarise(total_Cantidad = sum(total_cantidad, na.rm = TRUE))


#grafico evolutivo
siniestros_viales_grafico_evolutivo <- siniestros_viales_evolutivo %>% 
  ggplot(aes(x = SE_ANIO, y = total_cantidad)) +
  geom_bar(stat = "identity", fill = "#e24a38", width = 0.5) +
  scale_x_discrete(
    breaks = levels(siniestros_viales_evolutivo$SE_ANIO)[seq(1, length(levels(siniestros_viales_evolutivo$SE_ANIO)), by = 3)],
    expand = c(0, 0)) +  
  labs(
    x = "SE-año",
    y = "Lesiones por causas externas") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 7),
    axis.text.x = element_text(size = 7, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 7),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank())
siniestros_viales_grafico_evolutivo
}
#####FIN INTERNACIONES POR SINIESTROS VIALES----



























#Sifilis----
{#base nominal pendiente
}

#Inmunoprevenibles----
IP <- agrupada %>% 
  filter(IDEVENTOAGRUPADO==5)

#armo tabla evolutivo 
{
IP_evolutivo <- IP %>% 
  group_by(ANIO, SEMANA, NOMBREEVENTOAGRP) %>% 
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>% 
  mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
  arrange(ANIO, SEMANA) %>% 
  mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
  as.data.frame()
  
  
#objeto sumatoria cantidad tabla evolutivo
  IP_total<- IP_evolutivo %>% 
  summarize(Total_Cantidad = sum(Total, na.rm = TRUE))
  
  }
  
#armo objeto total de varicelas SE BEM
{
varicela_SEBEM <- IP %>%
  filter(ID_SNVS_EVENTO_AGRP == 6, 
         ANIO == ANIO_max,  
         SEMANA %in% SE_BEM) %>% 
   summarize(Total_Cantidad = sum(CANTIDAD, na.rm = TRUE))
}

# Variacion porcentual
{


#selecciono las SE del año pasado (es para hacer la variacion porcentual del set de indicadores)
  IP_SEBEM_anio_anterior <- IP_evolutivo %>%
    filter((ANIO == 2023 & SEMANA %in% SE_BEM)) %>% #Es importante ir cambiando el año y las SE según corresponda al BEM
    summarise(total_Cantidad = sum(Total, na.rm = TRUE)) 

  
  
#selecciono las SE de este año
 IP_SEBEM <- IP_evolutivo %>%
    filter(ANIO == ANIO_max, SEMANA %in% SE_BEM) %>% 
    summarise(Total_Cantidad = sum(Total, na.rm = TRUE)) 
 #Puede ser que no tenga ningun sentido crear este objeto ya que desde 2023 se notifica
 #varicela y no parotiditis, por lo que el N tendria que ser igual que "Varicela_SEBEM"
 
  
#variacion porcentual
  IP_variacion_porcentual <- 
    round((IP_SEBEM - IP_SEBEM_anio_anterior)/IP_SEBEM_anio_anterior*100,1)
}
  
#grafico evolutivo 
{
  
  
  IP_grafico_evolutivo <- IP_evolutivo %>% 
    ggplot(aes(x = ANIO_SE, y = Total, fill=NOMBREEVENTOAGRP)) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_x_discrete(
      breaks = levels(IP_evolutivo$ANIO_SE)[seq(1, length(levels(IP_evolutivo$ANIO_SE)), by = 4)],
      expand = c(0, 0)) +  
    scale_fill_manual(
      values = c("Parotiditis" = "#f9f871",  
                 "Varicela" = "#63ddb0"))+
       labs(
      x = "SE-año",
      y = "Casos de infecciones inmunoprevenibles",
      fill = "Evento") +
    theme_classic() +
    theme(
      axis.title = element_text(size = 15),
      axis.text.x = element_text(size = 15, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 15),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank())
  IP_grafico_evolutivo
}

#Grafico varicela por grupos etarios
{
#armo tabla filtrando solo varicela
 
  Varicela_evolutivo <- IP %>% 
  filter(ID_SNVS_EVENTO_AGRP==6, IDEDAD %in% c(1, 2, 3, 4, 5, 6)) %>% 
  group_by(ANIO, SEMANA, GRUPO) %>% 
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>% 
  mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
  arrange(ANIO, SEMANA) %>% 
  mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
  as.data.frame()
  
 
  
#armo los grupos de edad segun publicaciones del BEM 
Varicela_evolutivo <- Varicela_evolutivo %>% 
  mutate(
    GRUPO_2 = case_when(
      GRUPO == "< 6 m"~ "Menores de 5 años",
      GRUPO == "6 a 11 m"~ "Menores de 5 años",
      GRUPO == "12 a 23 m"~ "Menores de 5 años",
      GRUPO == "2 a 4"~ "Menores de 5 años",
      GRUPO == "5 a 9"~ "5-9 años",
      GRUPO == "10 a 14"~ "10-14 años",
          TRUE ~ GRUPO))

#objeto N para usarlo en titulo
Varicela_evolutivo_total <- Varicela_evolutivo %>% 
  summarise(total_Cantidad = sum(Total, na.rm = TRUE))


#grafico grupos de edad columnas apiladas al 100%

IP_grafico_grupoetario_acumulado <- Varicela_evolutivo %>% 
    ggplot(aes(x = ANIO_SE, y = Total, fill = GRUPO_2)) +
  geom_bar(stat = "identity", position = "fill") + #position fill es para apilado al 100%
  scale_x_discrete(
    breaks = levels(Varicela_evolutivo$ANIO_SE)[seq(1, length(levels(Varicela_evolutivo$ANIO_SE)), by = 5)],
    expand = c(0, 0)) +
  scale_y_continuous(
    labels = scales::percent,  # Cambiar el formato de las etiquetas a porcentaje
    breaks = seq(0, 1, by = 0.1))+ # Poner los breaks del eje Y entre 0 y 1, con intervalos de 0.1
  scale_fill_manual(values = c(
    "Menores de 5 años" = "#63ddbd",
    "5-9 años" = "#f9f871",  
    "10-14 años" = "#008469")) +  
  labs(
    x = "SE-año",
    y = "% de casos de varicela",
    fill = "Grupo de edad") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1),  # Rotar etiquetas en X
    axis.text.y = element_text(size = 10),  # Fuente para los textos del eje Y
    axis.title.x = element_text(size = 10),  # Cambia el tamaño del título del eje X
    axis.title.y = element_text(size = 10),
    panel.border = element_blank(),  # Eliminar borde del panel
    axis.line = element_blank(),  # Eliminar líneas de los ejes
    axis.ticks = element_blank())  # Eliminar "guioncito" de los ejes

IP_grafico_grupoetario_acumulado

}  

#gráfico varicela grupo de edad SEBEM
{
#tabla para el grafico edades segun SE BEM

varicela_tabla_grupoetario <- IP %>% 
  filter(ID_SNVS_EVENTO_AGRP==6,ANIO == ANIO_max & SEMANA %in% SE_BEM ) %>% 
  group_by(ANIO, SEMANA, GRUPO) %>% 
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>% 
  mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
  arrange(ANIO, SEMANA) %>% 
  mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
  mutate(
    GRUPO_2 = case_when(
      GRUPO == "< 6 m"~ "< 6 meses",
      GRUPO == "6 a 11 m"~ "6 a 11 meses",
      GRUPO == "12 a 23 m"~ "12 a 23 meses",
      GRUPO == "2 a 4"~ "2 a 4 años",
      GRUPO == "5 a 9"~ "5 a 9 años",
      GRUPO == "10 a 14"~ "10 a 14 años",
      GRUPO == "15 a 19"~ "15 a 19 años",
      GRUPO == "20 a 24"~ "20 a 24 años",
      GRUPO == "25 a 34"~ "25 a 34 años",
      GRUPO == "35 a 44"~ "35 a 44 años",
      GRUPO == "45 a 64"~ "45 a 64 años",
      GRUPO == "65 a 74"~ "65 a 74 años",
      GRUPO == ">= a 75"~ ">= a 75 años",
      TRUE ~ GRUPO)) %>% 
  mutate(GRUPO_2 = factor(GRUPO_2, levels = c("< 6 meses", "6 a 11 meses", "12 a 23 meses",
                                              "2 a 4 años", "5 a 9 años", "10 a 14 años",
                                              "15 a 19 años", "20 a 24 años","25 a 34 años",
                                              "35 a 44 años","45 a 64 años", "65 a 74 años", ">= a 75 años"))) %>% 
as.data.frame()

#objeto total varicelas SE BEM

#gráfico varicela grupo de edad
varicela_grafico_grupoetario <- varicela_tabla_grupoetario %>% 
  ggplot(aes(x=Total, y=GRUPO_2))+
  geom_bar(stat = "identity", fill = "#63ddbd", width = 0.5)+
  labs(
    x = "Casos de varicela",
    y = "Grupos de edad") +
    theme_classic () +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10), 
    axis.title.x = element_text(size = 10),  # Cambia el tamaño del título del eje X
    axis.title.y = element_text(size = 10),# Fuente para los textos del eje Y
    panel.border = element_blank(),  # Eliminar borde del panel
    axis.line = element_blank(),  # Eliminar líneas de los ejes
    axis.ticks = element_blank())  # Eliminar "guioncito" de los ejes
varicela_grafico_grupoetario

}

#grafico incidencia por regiones
{
  varicela_incidencia <-  IP %>%
    filter(ID_SNVS_EVENTO_AGRP == 6) %>% 
    group_by(REGIONES, ANIO) %>%
    summarise(
      total_cantidad = sum(CANTIDAD), 
      .groups = "drop")
  
  
  
  #agrego la columna poblacion con leftjoin
  varicela_incidencia <- varicela_incidencia %>%
    left_join(poblacion, by = c("ANIO", "REGIONES"))
  
  
  #agrego el calculo de incidencia 
  varicela_incidencia <- varicela_incidencia %>%
    mutate(
      total_cantidad = as.numeric(total_cantidad),  # cambio formato
      poblacion = as.numeric(poblacion),  # cambio formato
      incidencia = ifelse(!is.na(total_cantidad) & !is.na(poblacion), 
                          round((total_cantidad / poblacion) * 10000, 1), 
                          NA))
  
  
  #grafico incidencia
  varicela_incidencia_grafico <- varicela_incidencia %>% 
    filter(!is.na(incidencia)) %>%  # Filtrar para excluir NA en incidencia
    ggplot(aes(x = incidencia, y = factor(ANIO))) +  # Años en el eje Y, ordenados cronológicamente
    geom_col(stat = "identity", fill = "#63ddb0", width = 0.5) +  # Gráfico de barras horizontales
    geom_text(aes(label = round(incidencia, 1)),  # Etiquetas con valores redondeados
              hjust = -0.2,  # Desplaza las etiquetas a la derecha
              size = 3) +  # Tamaño del texto de las etiquetas
    facet_wrap(~ REGIONES, ncol = 3) +  # Facetear por región, 2 columnas
    labs(
      x = "Incidencia acumulada cada 10000 habitantes",
      y = "Año") +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      strip.text = element_text(size = 10))  # Tamaño del texto en los facetes
  
  varicela_incidencia_grafico
  
  
  
  
  
  
  
  
  
}















