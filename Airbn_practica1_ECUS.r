# install.packages("readxl")
# install.packages("dplyr") # dplyr es un paquete que se utiliza para manipular/gestionar datos
library(dplyr)
# Actualizar el paquete vctrs
library(gridExtra)
options(gridExtra.show_missing_message = FALSE)
# install.packages("gtable")
library(gtable)
# install.packages("ggstance")
library(ggstance)
library(gridExtra)
library(grid)
# install.packages("vctrs")
library(readxl)
# install.packages("tidyverse")
library(ggplot2)
install.packages("pdftools")
library(pdftools)
install.packages("jpeg")
library(jpeg)

#Obtener los datos
datos <- read_excel("MO_dataset_airbnb_nyc.xlsx")
# Obtener el directorio de trabajo actual
getwd() #mirar el directorio
dim(datos) #obtener el tamaño de los datos
names(datos) #listar los atributos del dataframe o columnas
head(datos) #visualizar los datos
attach(datos)

pdf("preguntas4.pdf", onefile=TRUE, width = 8, height = 12)
# Limpieza y transformación de los datos 
# Conversión de datos de las columnas tipo caracter
print(class(datos$reviews_per_month))
datos$reviews_per_month <- as.numeric(datos$reviews_per_month)
head(datos)
imp_total_nulos = sum(is.na(datos)) #suma total de los datos NA del dataframe
imp_total_nulos
# Eliminar todas las filas que indican que no está habilitado el alojamiento con availability_365 = 0
datos <- datos[datos$availability_365 != 0 & !is.na(datos$availability_365),  ] #eliminaos los alojamientos que no están disponibles
dim(datos)
sum(is.na(datos))
imp_titulo_datos <- "Visualizaremos los datos a procesar"
imp_muestra_datos <- head(datos)
imp_muestra_datos
imp_muestra_datos <- tableGrob(imp_muestra_datos, rows=NULL)

# Crear un nuevo archivo PDF
# 1.- ¿Qué zonas de Nueva York son las que ofrecen una mayor oferta de alojamiento?
imp_preg_1 <- "Qué zonas de Nueva York son las que ofrecen una mayor oferta de alojamiento?"
imp_valor_max_noche <- which.max(datos$minimum_nights)
imp_valor_max_noche #Obtenemos el ínidice que tiene la mayor oferta del total
imp_zona_max_ny <- datos$neighbourhood_group[imp_valor_max_noche]
imp_zona_max_ny #la que más ofrece es Manhattan

tabla_oferta_sitios_zona <- as.data.frame(table(Zona = datos$neighbourhood_group))
tabla_oferta_sitios_zona <- data.frame(tabla_oferta_sitios_zona %>%
  arrange(desc(tabla_oferta_sitios_zona$Freq))) #ordenamos la tabla de oferta de mayor a menor
tabla_oferta_sitios_zona
grafico_p1 <- ggplot(data = tabla_oferta_sitios_zona, 
  aes(x = Zona, y = Freq, fill = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Zona de Nueva York") +
  ylab("Número de alojamientos") +
  ggtitle("Oferta de Alojamiento por número de establecimientos en la zona") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(plot.background = element_rect(fill = "white", size = 1),
        panel.background = element_rect(fill = "white", size = 1),
        plot.margin = unit(c(2,2,2,2), "cm"),
        plot.title = element_text(size = 14))
tabla_oferta_sitios_zona <- tableGrob(tabla_oferta_sitios_zona, rows=NULL)
grafico_p1
#Calculamos la mayor oferta por noche sumando la cantidad de noches mínimas ofrecidas por cada establecimiento
head(tabla_oferta_noche)
dim(tabla_oferta_noche)
attach(tabla_oferta_noche)
imp_titulo_1 <- "Lista de zonas de NY y oferta de alojamiento por total de establecimientos"
df_oferta <- data.frame(Zona = datos$neighbourhood_group, Num_noches = datos$minimum_nights)
tabla_oferta_noche <- df_oferta %>% 
  group_by(Zona) %>%
  summarize(Oferta_Total_noche = sum(Num_noches),
            Total_lugares = n())  #grupamos por zona y sumamos la oferta para cada zona

df_oferta <- tabla_oferta_noche %>%
  arrange(desc(tabla_oferta_noche$Oferta_Total_noche)) #ordenamos la tabla de oferta de mayor a menor
# Crear una cadena de texto para identificar la tabla
imp_titulo_2 <- textGrob("Total de plazas de alojamiento por zona según el mínimo de noches", gp = gpar(fontsize = 12, fontface = "bold"))
df_oferta #Dataframe que contiene para cada zona la oferta total por año y el número total de establecimientos.
tabla_oferta <- tableGrob(df_oferta, rows=NULL) #creamos la tabla para incluir al pdf
# grid.arrange(tabla_oferta, ncol=1)
#Graficamos la oferta por zona
grafico_p2 <- ggplot(data = df_oferta, aes(x = Zona, y = Oferta_Total_noche, fill = Oferta_Total_noche)) +
  geom_bar(stat = "identity") +
  xlab("Zona de Nueva York") +
  ylab("Oferta de Alojamiento") +
  ggtitle("Oferta de Alojamiento por Zona de Nueva York") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(plot.background = element_rect(fill = "white", size = 1),
        panel.background = element_rect(fill = "white", size = 1),
        plot.margin = unit(c(2,2,2,2), "cm"),
        plot.title = element_text(size = 14))
ggsave("mi_grafico.jpg", plot = grafico_p2, width = 15, height = 11, dpi = 800)
# Cargar la imagen
imagen <- readJPEG("mi_grafico.jpg")
# Convertir la imagen a un grob
imagen_grob <- rasterGrob(imagen, interpolate = TRUE)
# Agregar la tabla, el gráfico y la cadena de texto al PDF
grid.arrange(
  grid.text(imp_titulo_1, x=0.1, y=0.1, just=c("left", "top")),
  tabla_oferta_sitios_zona,
  grafico_p1, 
  ncol=1, nrow = 4
)


pdf("preguntas5.pdf", onefile=TRUE, width = 6, height = 8)
# Combinar los elementos en una sola página
grid.arrange((gridExtra::arrangeGrob(imp_titulo_2, tabla_oferta_sitios_zona)),
  imagen_grob,
  imp_titulo_2,
  ncol=1
)
dev.off()

# 2.- Para cada zona, ¿qué tipo de alojamiento es el más ofertado?
#Creamos una tabla de frecuencia para tipos de alojamiento por zona
imp_preg_2 <- "2.- Para cada zona, ¿qué tipo de alojamiento es el más ofertado?"
names(datos)
#generamos una tabla de frecuencia por zona y tipo de alojamiento
tabla_frecuencia_zona <- xtabs(~ datos$room_type + datos$neighbourhood_group, data = datos)
tabla_frecuencia_zona
diccionario_resultado_zona <- list()  #declaramos lel diccionario vacío tipo list

#Llenamos un diccionario con la zona y el tipo de alojamiento más ofertado
for (i in 1:ncol(tabla_frecuencia_zona)) {
  columna_actual <- tabla_frecuencia_zona[, i]
  zona_actual <- colnames(tabla_frecuencia_zona)[i]
  tipo_alojamiento_mas_ofertado_zona <- names(which.max(columna_actual))
  diccionario_resultado_zona[[zona_actual]] <- tipo_alojamiento_mas_ofertado_zona
}
diccionario_resultado_zona
#Creamos el dataFrame con los totales calculados
df_resultado_zona <- data.frame(Tipo = unlist(diccionario_resultado_zona))
df_resultado_zona
imp_titulo_2 <- "Lista de tipos de alojamientos más ofertados por zonas de NY"
tabla_max_ofertado <- tableGrob(df_resultado_zona, rows=NULL) #creamos la tabla para incluir al pdf

# Agrupar datos por zona y tipo de alojamiento, y contar el número de registros
contadores <- aggregate(id ~ neighbourhood_group + room_type, data = datos, FUN = length)
contadores
# 3.- ¿Cuáles son las zonas más caras?
names(datos)
tabla_precios <- data.frame(Zona = datos$neighbourhood_group, Precio = datos$price)
head(tabla_precios)
attach(tabla_precios)
dim(tabla_precios)
df_precios <- tabla_precios %>%
  group_by(Zona) %>%
  summarize(Precios_Max = max(Precio)) %>%
  arrange(desc(Precios_Max))
df_precios
#Graficamos el promedio de precios por zona
ggplot(data = df_precios, aes(x = Zonas, y = Precios_Max, fill = Precios_Max)) +
geom_bar(stat = "identity") +
  xlab("Zona de Nueva York") +
  ylab("Promedio de Precios de alojamiento") +
  ggtitle("Ranking de Media de los precios de Alojamiento por Zona de Nueva York") +
  scale_fill_gradient(low = "blue", high = "red")

#¿Y las más populares?  ¿Coinciden unas con otras?
# METODO 1
names(datos)
tabla_popular <- data.frame(Zona = datos$neighbourhood_group, Votos = datos$number_of_reviews)
head(tabla_popular)
attach(tabla_popular)
tabla_popular <- tabla_popular %>% filter(!is.na(Votos))
tabla_popular <- tabla_popular %>% filter(Votos != 0)
dim(tabla_popular)
df_popular <- tabla_popular %>%
  group_by(Zona) %>%
  summarize(Votos_Max = max(Votos)) %>%
  arrange(desc(Votos_Max))
df_popular

#Graficamos el promedio de precios por zona
ggplot(data = df_popular, aes(x = Zonas, y = Votos_Max, fill = Votos_Max)) +
geom_bar(stat = "identity") +
  xlab("Zona de Nueva York") +
  ylab("Promedio de Precios de alojamiento") +
  ggtitle("Ranking de Media de los precios de Alojamiento por Zona de Nueva York") +
  scale_fill_gradient(low = "blue", high = "red")

# Los dueños de más de un alojamiento tienen sus ofertas en el
#  mismo sitio o en sitios diferentes?
names(datos)
tabla_duenos <- data.frame(Nombre = datos$host_name, Zona = datos$neighbourhood_group)
head(tabla_duenos)
attach(tabla_duenos)
dim(tabla_duenos)
tabla_duenos <- tabla_duenos %>% filter(!is.na(Zona), !is.na(Nombre))
tabla_duenos <- tabla_duenos %>% filter(Zona != 0)
dim(tabla_duenos)

df_duenos <- tabla_duenos %>%
  group_by(Nombre) %>%
  summarize(Zonas = paste0(unique(Zona), collapse = ", "),
            Num_Zonas = n_distinct(Zona))

head(df_duenos)
str(df_duenos)
dim(df_duenos)

df_duenos_varios <- df_duenos %>%
  filter(nchar(Zonas) > 1) %>%
  arrange(desc(Num_Zonas))
dim(df_duenos_varios)
head(df_duenos_varios)


df_duenos_contar <- df_duenos %>%
  group_by(Cant_Zonas = Num_Zonas) %>%
  summarize(Total_Duenos = n_distinct(Nombre))
dim(df_duenos_contar)
df_duenos_contar

df_duenos_porcentaje <- df_duenos_contar %>%
  mutate(Porcentaje = (round(Total_Duenos / sum(Total_Duenos) * 100, 2)))

head(df_duenos_porcentaje)
str(df_duenos_porcentaje)
df_duenos_porcentaje$Porcentaje <- round(df_duenos_porcentaje$Porcentaje)
df_duenos_porcentaje


ggplot(df_duenos_contar, aes(x = Cant_Zonas)) +
  geom_bar(aes(y = Total_Duenos, fill = Cant_Zonas), stat = "identity") +
  xlab("Numero de Zonas") +
  ylab("Numero de Propietarios") +
  ggtitle("Distribución del Número de zonas por propietarios") +
  scale_fill_gradient(name = "Cantidad de Zonas", low = "blue", high = "red")

ggplot(df_duenos_porcentaje, aes(x = Cant_Zonas)) +
  geom_bar(aes(y = Porcentaje, fill = Cant_Zonas), stat = "identity") +
  xlab("Numero de Zonas") +
  ylab("Numero de Propietarios") +
  ggtitle("Distribución del Número de zonas por propietarios") +
  scale_fill_gradient(name = "Cantidad de Zonas", low = "blue", high = "red")

ggplot(df_duenos, aes(x = Num_Zonas)) +
  geom_histogram(binwidth = 1) +
  xlab("Número de Zonas") +
  ylab("Frecuencia") +
  ggtitle("Distribución de Propietarios por Número de Zonas")

ggplot(df_duenos, aes(x = Num_Zonas)) +
  geom_density() +
  xlab("Número de Zonas") +
  ylab("Densidad") +
  ggtitle("Distribución de Propietarios por Número de Zonas")

# ¿Cuál es el alojamiento más caro teniendo en cuenta el número
# mínimo de noches por reserva?¿Y el más barato?
names(datos)
tabla_precios_noches <- data.frame(Lugar = datos$name, Precio = datos$price, Cantidad_min = datos$minimum_nights)
dim(tabla_precios_noches)
attach(tabla_precios_noches)
tabla_precios_noches <- tabla_precios_noches %>% filter(!is.na(Lugar), !is.na(Precio), !is.na(Cantidad_min))
dim(tabla_precios_noches)
tabla_precios_noches <- tabla_precios_noches %>% filter(Precio!= 0, Cantidad_min!=0)
dim(tabla_precios_noches)
attach(tabla_precios_noches)
tabla_precios_noches$Resultado <- Precio * Cantidad_min
head(tabla_precios_noches)
tabla_precios_noches_max <- tabla_precios_noches %>%
  arrange(desc(Resultado))
head(tabla_precios_noches_max)
tabla_precios_noches_min <- tabla_precios_noches %>%
  arrange((Resultado))
head(tabla_precios_noches_min)
tabla_precios_def <- data.frame(LugarCaro = head(tabla_precios_noches_max$Lugar, 15), 
                                MasCostoso = head(tabla_precios_noches_max$Resultado, 15),
                                LugarBarato = head(tabla_precios_noches_max$Lugar, 15),
                                MasBarato = head(tabla_precios_noches_min$Resultado, 15))
tabla_precios_def
head(tabla_precios_noches_max$Lugar)

# Agregar la tabla, el gráfico y la cadena de texto al PDF
grid.arrange(grid.text(cadena_texto, x=0.1, y=0.1, just=c("left", "bottom"), gp=gpar(fontsize=12, fontface="bold")),tabla_oferta, grafico, 
             ncol=1)

print(grafico)
# Cerrar el archivo PDF
dev.off()
