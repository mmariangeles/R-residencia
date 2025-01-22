###TALLER R -------
##Encuentro 1

##1) Interfaz Rstudio####
#   Script (arriba - izquierda)
#   Consola (abajo - izquierda)
#   Ambiente global/environment (arriba - derecha)
#   Archivos, gráficos, paquetes, ayuda (abajo - derecha)


#para hacer comentarios hay que poner # al inicio
#porque sino hay errores
#para correr una línea: atajo ctr+enter o "run"


#algunos calculos más que sencillos (probamos la consola)
100+98
2+8-6

############################################################################


#Objetos
#Cuando agrego <-  estoy creando un objeto. Un objeto me permite almacenar informacion
#de distintas "características/formato" que vamos a ir viendo


#2) CREACION DE OBJETOS----
#Crear objetos: flechita de asignacion (ATAJO ALT+GUION MEDIO O SIGNO MENOS)

mi_primer_objeto <- 150


100 -> otro_num #no se usa mucho esta forma de escribir

#división
division <- mi_primer_objeto/10

#resta
otro_calculo <- division+mi_primer_objeto-50
#suma
numero+150

#palabras
mi_fruta_fav <- "durazno"
color_fav <- "verde" #qué mensaje nos tiró la consola? 



#MUY IMPORTANTE: Rstudio distingue mayúsculas y minúsculas y tambien los espacios.
#recomendacion NO espacios. Si es necesario separar, se_puede_usar_guiones_

#Creamos objetos entre todxs. Cómo sería un objeto de c/u de sus nombres y las cohortes de la resi?

R_1a <- "Lu"
R_1a_a <- "Pao"
R_1a_a_a <- "Adri"


R_2_a <- "Alma"
R_2_a_a <- "Agus"
R_2_a_a_a <- "Fiamma"

R_3_a <- "Su"
R_3_a_a <- "Vicki"
R_3_a_a_a <- "Meli"
R_3_a_a_a_a <- "Manu"



##Vamos con las funciones----
#raiz
sqrt(4) #esto es un objeto?



##4) TIPOS DE DATOS, OBJETOS Y FUNCIONES ASOCIADAS----------------------------------
###VECTORES----
####Numericos----
secuencia <- c(14,58,96,25,79654,45,1) #funcion C=concatenar


#Miren como está armado esa línea. 
secuencia <- c(10,20,30,40,50,60) #Funcionará?
secuencia

secuencia2 <- c(10, 20, 30, 40, NA, 60) #NA=dato vacio
secuencia2

class(secuencia)
class(secuencia2)
#Importantísisisimo: los vectores DEBEN tener sólo un tipo de datos

secuencia3 <- c(10,"veinte",30,"cuarenta")
class(secuencia3)


#armemos un vector con las cohortes de la resi#

#y ahora usando los objetos creados previamente





#Hagamos algunas cositas con nuestro vector numérico
mean(secuencia)

mean(secuencia2) #por qué es esa mi salida de R?
mean(secuencia2, na.rm = TRUE)

sd(secuencia, na.rm = T)
sum(secuencia, na.rm = T)

#ordeno los valores
sort(secuencia, decreasing =F)#sort sirve para ordenar



####Texto STRING CHARACTER----
palabras_random <- c("mucho", "poquito", "nada", "poquito", "mucho", "nada", "mucho", "mucho")
nombre_apellido <- c("Juan", "Perez")
class(palabras_random)
class(nombre_apellido)

#probamos cositas
paste(palabras_random, collapse = " ")
paste(nombre_apellido, collapse = " ")
paste(palabras_random, ".1")
paste0(palabras_random, ".1")
paste0("1.", palabras_random)
sort(palabras_random)

#¿cuántos hay de c/u? una tabla es lo más rápido para verlo
table(palabras_random)



#Factor ¿cómo le indico al Rstudio que mi texto tiene una cantidad finita de valores osea que es un factor?----
class(palabras_random)
palabras_random_f1 <- factor(palabras_random)
class(palabras_random_f1)

palabras_random_f2 <- factor(palabras_random, #pongo el vector
                             ordered = TRUE, 
                             levels = c("nada", "poquito", "mucho"))
palabras_random_f2
class(palabras_random_f1)
class(palabras_random_f2)

sort(palabras_random_f2)
sort(palabras_random_f2, decreasing = F)


#Vamos con un base de datos------------------------------------------------------------

#Mostrar Rdocumentation
library(readxl)


base <- read_xlsx("NEUQUEN_CLI.xlsx")
View(base)

#también puedo usar la base de datos en otros formatos, como csv
base_1 <- read.csv("NEUQUEN_CLI.csv", sep = ";")
View(base_1)
#Cuántas observaciones tiene mi base de datos? Cuántas variables?



