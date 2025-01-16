#activacion de librerias####
library(readr)
library(tidyverse)
library(dplyr)
library(knitr)
library(ggplot2)
library(readxl)
library(highcharter)
library(lubridate)
library(kableExtra)
library(gt)




#base agrupada-----------------------------
#lectura de base y chequeo de formato dataframe----
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
str(agrupada)

#agrego columna region-----
#lectura de la base 
regiones <- read_excel("REGIONES.xlsx")
regiones <- as.data.frame(regiones) #cambio formato a dataframe


#cruzo base de datos (es como un buscar V en excel)
?left_join

agrupada <- agrupada%>% left_join(regiones,by = "LOCALIDAD") 

#armo la variable SE-año
agrupada <- agrupada %>%
  mutate(SE_ANIO= paste(SEMANA, ANIO, sep = "-")) 


##SE/año del BEM-----
#las SE/año del BEM de este mes son 48,49,50,51 y 52 (esto es un ayuda memoria) del año 2024#SE maxima
SE_BEM <- c(48, 49, 50, 51, 52) #hay que cambiarlo mensualmente

#ANIO maximo
ANIO_max <- agrupada %>% 
  summarise(ANIO_max = max(ANIO, na.rm = TRUE))
view(ANIO_max)

#SE_min 
SE_min <- SE_BEM %>%
  min()
  
#SE_max 
SE_max <- SE_BEM %>%
  max()


################################
#Diarreas----
diarreas <- agrupada %>% 
  filter(ID_SNVS_EVENTO_AGRP==11)



#set de indicadores
#tabla con el total y la variacion porcentual 

#selecciono solo las SE del BEM para hacer mi objeto del total de las DA de este BEM

DA_cantidad_SE_BEM <- diarreas %>%
filter(ANIO == 2024, SEMANA   %in% c(48, 49, 50, 51, 52)) %>% #Es importante ir cambiando el año y las SE según corresponda al BEM
summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE)) %>% 
pull(total_Cantidad)  # Esto extrae el valor numérico de la columna 



#selecciono las SE del año pasado (es para hacer la variacion porcentual del set de indicadores)
DA_cantidad_SE_BEM_anioanterior <- diarreas %>%
  filter(ANIO == 2023, SEMANA %in% c(48, 49, 50, 51, 52)) %>% #Es importante ir cambiando el año y las SE según corresponda al BEM
  summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE)) %>%
  pull(total_Cantidad)  # Esto extrae el valor numérico de la columna

#variacion porcentual 
DA_variacion_porcentual <- 
round((DA_cantidad_SE_BEM - DA_cantidad_SE_BEM_anioanterior)/DA_cantidad_SE_BEM_anioanterior*100,1)


# Crear la tabla con gt
#DA_indicadores <- tibble(
#  Categoria = "Diarrea aguda",
#  Cantidad = DA_cantidad_SE_BEM,
#  Variacion = DA_variacion_porcentual)


#DA_indicadores_gt <- DA_indicadores %>% 
#  gt() %>%
#  tab_header(
#    title = md("**Diarrea aguda**")  ) %>%
#  fmt_number(
#    columns = vars(Cantidad), # Usar 'vars()' para referenciar la columna
#   decimals = 0) %>%
#  tab_style(
#    style = list(
#      cell_fill(color = "orange"),
#      cell_text(color = "white", weight = "bold")),
#    locations = cells_title(groups = "title")) %>%
#  tab_style(
#    style = cell_text(size = px(50), weight = "bold", align = "center"),
#    locations = cells_body(columns = vars(Cantidad))) %>% # Usar 'vars()' para referenciar
# tab_style(
#   style = list(
#      cell_fill(color = "lightpink"),
#     cell_text(color = "red", align = "center", weight = "bold")),
#   locations = cells_body(columns = vars(Variacion))) # Usar 'vars()' para referenciar
#DA_indicadores_gt


# Convertir la tabla gt a LaTeX
#DA_indicadores_latex <- as_latex(DA_indicadores_gt)


#DA_indicadores_latex

#tabla para DA evolutivo 
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
  group_by(ANIO) %>% 
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





#Armar una tabla para hacer un graf de DA por grupos etarios SE del BEM
DA_tabla_grupoetario <- diarreas %>%
  filter(ANIO == 2024, SEMANA %in% c(48, 49, 50, 51, 52)) %>% # Cambiar ANIO y SEMANA según corresponda
  group_by(GRUPO) %>% 
  summarise(CANTIDAD = n(), .groups = "drop") %>% 
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
      

view(DA_tabla_grupoetario)

DA_tabla_grupoetario <- as.data.frame(DA_tabla_grupoetario)

#gráfico DA grupo de edad
DA_grafico_grupoetario <- DA_tabla_grupoetario %>% 
  ggplot(aes(x=CANTIDAD, y=GRUPO_2))+
  geom_bar(stat = "identity", fill = "orange", width = 0.5)+
  labs(
    x = "Casos de diarrea aguda",
    y = "Grupos de edad") +
  scale_x_continuous(breaks = seq(0, max(DA_tabla_grupoetario$CANTIDAD), by = 20)) +
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


#Armar una tabla para hacer un graf de DA por grupos etarios acumulado

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
    GRUPO == "55 a 64"~ "45 a 64 años",
    GRUPO == "65 a 74"~ "65 años y más",
    GRUPO == ">= a 75"~ "65 años y más",
    TRUE ~ GRUPO))
    
 view(DA_tabla_grupoetario_acumulado)


# Crear gráfico de columnas apiladas
DA_grafico_grupoetario_acumulado <- 
DA_tabla_grupoetario_acumulado %>% 
  ggplot(aes(x = ANIO_SE, y = Total, fill = GRUPO_2)) +
  geom_bar(stat = "identity", position = "fill", color = "white") + #position fill es para apilado al 100%
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


#DA por regiones segun grupo de edad

#Armar una tabla 
DA_tabla_regiones_grupoetario <- diarreas %>%
  filter(ANIO == 2024, SEMANA %in% c(48, 49, 50, 51, 52)) %>% # Cambiar ANIO y SEMANA según corresponda
  group_by(GRUPO, REGIONES) %>%
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")


view(DA_tabla_regiones_grupoetario)


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
      GRUPO == "55 a 64"~ "45 a 64 años",
      GRUPO == "65 a 74"~ "65 años y más",
      GRUPO == ">= a 75"~ "65 años y más",
      TRUE ~ GRUPO))

view(DA_tabla_regiones_grupoetario)

#grafico por regiones segun grupo etario

DA_grafico_regiones <- 
  DA_tabla_regiones_grupoetario %>% 
  filter(!is.na(REGIONES)) %>%  # Eliminar filas donde REGIONES es NA
  ggplot(aes(x=Total, y=GRUPO_2))+
  geom_bar(stat = "identity", fill = "orange", width = 0.5) +  
             facet_wrap(~ REGIONES, ncol=2) +  # Facetear por la columna REGIONES
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


##########################FIN DA##############################################
#Internaciones por lesiones en el hogar----
lesiones_hogar <- agrupada %>% 
  filter(IDEVENTOAGRUPADO==116) %>% 
  filter((ANIO > 2023) | (ANIO == 2023 & SEMANA >= 21))

#Cantidad segun SE BEM
lesiones_hogar_SE_BEM <- lesiones_hogar %>%
  filter(ANIO == 2024, SEMANA   %in% c(48, 49, 50, 51, 52)) %>% #Es importante ir cambiando el año y las SE según corresponda al BEM
  summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE)) %>% 
  pull(total_Cantidad)  # Esto extrae el valor numérico de la columna 




#tabla_lesiones_ evolutivo
lesiones_evolutivo <- lesiones_hogar %>%
  group_by(SEMANA,ANIO,SE_ANIO) %>%  
  summarise(total_cantidad = sum(CANTIDAD), .groups = "drop") %>%  # Suma la columna cantidad, ignorando NA
  arrange(ANIO, SEMANA) %>% 
  mutate(SE_ANIO = factor(SE_ANIO, levels = unique(SE_ANIO)))


#objeto n lesiones
lesiones_total <- sum(lesiones_evolutivo$total_cantidad, na.rm = TRUE)



#grafico evolutivo lesiones

lesiones_grafico_evolutivo <- lesiones_evolutivo %>% 
  ggplot(aes(x = SE_ANIO, y = total_cantidad)) +
  geom_bar(stat = "identity", fill = "#528B8B", color = "#79CDCD", width = 0.5) +
  scale_x_discrete(expand = c(0, 0)) +  
  scale_y_continuous(
    breaks = seq(0, max(lesiones_evolutivo$total_cantidad, na.rm = TRUE), by = 10),
    expand = c(0, 0)) +
  labs(
    x = "SE-año",
    y = "Lesiones en el hogar") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 30),
    axis.text.x = element_text(size = 30, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 30),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank())

# Mostrar el gráfico
lesiones_grafico_evolutivo

##############FIN LESIONES##########################################
#Lesiones por mordedura de perro (ambulatorias)

mordedura_perro <- agrupada %>% 
  filter(IDEVENTOAGRUPADO==117)

#tabla mordedura evolutivo

mordeduras_evolutivo <- mordedura_perro %>%
  group_by(SEMANA,ANIO,SE_ANIO) %>%  
  summarise(total_cantidad = sum(CANTIDAD), .groups = "drop") %>%  # Suma la columna cantidad, ignorando NA
  arrange(ANIO, SEMANA) %>% 
  mutate(SE_ANIO = factor(SE_ANIO, levels = unique(SE_ANIO)))




#grafico evolutivo
mordeduras_grafico_evolutivo <- mordeduras_evolutivo %>% 
  ggplot(aes(x = SE_ANIO, y = total_cantidad)) +
  geom_bar(stat = "identity", fill = "#528B8B", color = "#79CDCD", width = 0.5) +
  scale_x_discrete(expand = c(0, 0)) +  
  labs(
    x = "SE-año",
    y = "Casos de mordeduras de perros") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 30),
    axis.text.x = element_text(size = 30, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 30),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank())
mordeduras_grafico_evolutivo















