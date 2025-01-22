#Breve apunte sobre librerias y paquetes----
#librerias: ya hay paquetes y librerias "preinstaladas"
#hay mas de 18mil 
#PAQUETES: colecciones de funciones, datos y código compilado de R. Se usan tareas específicas,
#como visualización geoespacial, gráficos, análisis psicométricos, minería de datos, etc

#LIBRERIAS:directorios donde R guarda los paquetes
#ustedes van a tener que instalar algunos paquetes para luego instalar las librerias


#activacion de librerias----
#lxs que nunca hayan usado R, van a tener que instalar los paquetes en principio

install.packages("readr")
install.packages("dplyr")


library(tidyverse)#paquetes que incluye a dplyr, tidyr, ggplot2, readr, purr, tibble, stringr y forcat. Muy, muy utilizado
?tidyverse

library(dplyr) #para manipular y transformar datos. Permite prepararlos para su análisis estadístico.
               #por ej: select, filter, mutate, arrange, count, group_by
?dplyr

library(readxl) #para importar .xlxs


library(readr)#para importar .csv


library(ggplot2)#para graficar. 
?ggplot2

#al escribir library(), se importan las funciones contenidas en el paquete al entorno de trabajo actual.

#IMPORTANTE: hay que correr las library() cada que se inicie una sesión en R.
#esto se debe a que aunque ya hayas importado las funciones de un paquete con anterioridad, las sesiones de R se inician “limpias”.


#carga de base de datos
#imp!: el camino más directo es tener el dataset en la misma carpeta del .Rproj

#nombre <- read.csv2("nombre.csv")
#nombre <- read.csv("nombre.csv", sep = ";")

arbovirus <- read.csv2("ARBO_NEUQUEN.csv")

datos <- read.csv("ARBO_NEUQUEN.csv")

#¿cuántas observaciones tiene y cuantas variables? ¿Cuál es su formato?
nrow(arbovirus)
ncol(arbovirus)
class(arbovirus)

#DATAFRAME: estructura de datos similar a una tabla. Los datos están organizados en filas y columnas. 

#modificamos el formato del dataset
arbovirus <- as.data.frame(arbovirus)



#Carguemos la base como un xlsx


#Miremos como está compuesta nuestra tabla

{str(arbovirus)
  
#hago unas tablas para ver eventos que tiene
  #funcion: table
  table(arbovirus$EVENTO)
  
}

#Preguntas a mi set de datos-----
#a)como es la distribucion temporal de las notificaciones de arbovirus?
#b)como es la distribucion temporal de las notificaciones de los eventos?
#c)distribucion por sx
#d)distribuc por edad
#e)distribucion por region
#f)
#g)

#filtros por prov de residencia----

#uso de la funcion filter. ¿Por cuál columna vamos a filtar? 
arbovirus <- arbovirus %>%  #pipe o tuberias
  filter(ID_PROV_INDEC_RESIDENCIA==58)


#SE min----
#ya sabemos que la SE_FIS no siempre está completa
arbovirus <- arbovirus %>%
  mutate(SE_MIN = pmin(SEPI_SINTOMA,SEPI_CONSULTA,SEPI_MUESTRA,SEPI_APERTURA,
                       na.rm = TRUE))

#hagamos lo mismo con el año
arbovirus <- arbovirus %>%
  mutate(ANIO_MIN = pmin(ANIO_EPI_SINTOMA,ANIO_EPI_CONSULTA,ANIO_EPI_CONSULTA,ANIO_EPI_MUESTRA,ANIO_EPI_APERTURA,
                       na.rm = TRUE))

#na.rm = remove NA Si mis valores ausentes decido ignorarlos, pongo TRUE



#Bueno vamo a grafica----
{
  #a) Distribucion temporal de las notificaciones de arbo
  
  arbovirus_evolutivo <- arbovirus %>% 
    group_by(ANIO_MIN, SE_MIN, SEXO, CLASIFICACION_MANUAL) %>% 
    summarise(casos = n(), .groups = "drop") %>% 
    mutate(ANIO_SE = paste(ANIO_MIN, SE_MIN, sep = "-"))
  
  
 
  
  
#Grafico
  #estructura de los graficos
  #nombre de mi grafico <- nombre de mis datos %>% 
  #ggplot(aes(x=, y=)) +
  #geom_bar(stat="identity") #le estoy diciendo que no sume ni calcule nada, yo ya calcule los valores previamente
  
  
 #armemos el grafico entre todxs
  
  
 #cual es la salida de la consola? 
  
 #que tal los formatos de mis columnas?
  #puedo ordenar como una variable continua una variable con formato caracter?
  

  
  #metemos un poco más de mano
  #propuesta completisima   
  arbovirus_grafico_evolutivo <-  arbovirus_evolutivo %>%  
    ggplot(aes(x=ANIO_SE, y=casos)) +
    geom_bar(stat = "identity") + 
    scale_x_discrete(
      breaks = levels(factor(arbovirus_evolutivo$ANIO_SE))[seq(1, length(levels(factor(arbovirus_evolutivo$ANIO_SE))), by = 5)],
      expand = c(0, 0)) 
  arbovirus_grafico_evolutivo 
  

  #los colores en R y los temas
  
  
  
  
  
  
  
  

#propuesta completisima   
  arbovirus_grafico_evolutivo <-  arbovirus_evolutivo %>%  
    ggplot(aes(x=ANIO_SE, y=casos)) +
    geom_bar(stat = "identity", fill="red", color="yellow") + #fill= color de relleno #color=color de linea
    scale_x_discrete(
      breaks = levels(factor(arbovirus_evolutivo$ANIO_SE))[seq(1, length(levels(factor(arbovirus_evolutivo$ANIO_SE))), by = 5)],
      expand = c(0, 0)) +
    theme_light()+
    labs(
      y="Notificaciones de arbovirus",
      x= "Año-SE")
  
  
  arbovirus_grafico_evolutivo 
  
  
  
#bizarreando
  install.packages("ggimage")
  library(ggimage)
  
  
  arbovirus_grafico_evolutivo_2 <- arbovirus_evolutivo %>%
    ggplot(aes(x = ANIO_SE, y = casos)) +
    geom_image(aes(image = C:/Users/Epidemio/Documents/R para Resis/R-residencia/Taller 2025), size = 0.1) +  
    scale_x_discrete(
      breaks = levels(factor(arbovirus_evolutivo$ANIO_SE))[seq(1, length(levels(factor(arbovirus_evolutivo$ANIO_SE))), by = 5)],
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(0, max(arbovirus_evolutivo$casos, na.rm = TRUE), by = 50),
      expand = c(0, 0)
    ) +
    theme_minimal()
  
  arbovirus_grafico_evolutivo_2
  
  
}







