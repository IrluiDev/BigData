# install.packages("readxl")
# install.packages("dplyr") # dplyr es un paquete que se utiliza para manipular/gestionar datos
# install.packages("tidyverse")
# install.packages("vctrs")
# install.packages("ggstance")
# install.packages("gtable")
# install.packages("jpeg")
library(dplyr)
library(gridExtra)
options(gridExtra.show_missing_message = FALSE)
library(gtable)
library(ggstance)
library(gridExtra)
library(grid)
library(readxl)
library(ggplot2)
install.packages("pdftools")
library(pdftools)
library(jpeg)
#Configuración de gráficos y tablas
# Crear variable de formato
colores_g <- scale_fill_gradient(low = "#00CED1", high = "#FFC0CB")
colores <- c("#00CED1", "#FFC0CB", "Green", "Pink")
colores_discrete <- scale_fill_manual(values = colores)

formato_x <- theme(
  axis.title = element_text(size = 20),
  axis.text = element_text(size = 20),
  plot.title = element_text(size = 20, hjust = 0.5),
  legend.title = element_text(size = 18, face = "bold", hjust = 0.5)
)
formato_angulo0 <- theme(axis.text.x = element_text(size = 18, hjust = 0.5))

formato_angulo45 <- theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 0.5))

formato_grafico <- theme(
  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
  text = element_text(size = 20),
  legend.text = element_text(size = 18)  
)

#Obtener los datos -----------------------------
setwd("C:/Users/itramirez.INDRA/Documents/github_irBigData.code-workspace")
datos <- read_excel("MO_dataset_airbnb_nyc.xlsx")
# Obtener el directorio de trabajo actual
getwd() #mirar el directorio
dim(datos) #obtener el tamaño de los datos
names(datos) #listar los atributos del dataframe o columnas
head(datos) #visualizar los datos
attach(datos)
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

#Crear cabecera del informe: ------------------------------------------
imp_cabecera <- "Máster en Tecnologías de Análisis de Datos Masivos:\nBig Data\nAplicaciones y casos de uso empresarial.\nElaborado por: Irlui Ramírez Hernández.\nFebrero 2023" 
imp_cabecera <- textGrob(imp_cabecera,
                      gp = gpar(fontsize = 10, fontface = "bold")) 
#------------------------------------------------------------------------
# 1.- ¿Qué zonas de Nueva York son las que ofrecen una mayor oferta de alojamiento?
imp_preg_1 <- textGrob("1.-Qué zonas de Nueva York son las que ofrecen una mayor oferta de alojamiento?",
                      gp = gpar(fontsize = 10, fontface = "bold"))
# imp_preg_1 <- "Qué zonas de Nueva York son las que ofrecen una mayor oferta de alojamiento?"
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
  colores_g +  
  formato_x +
  formato_grafico + formato_angulo0
ggsave("g_oferta_sitios.jpg", plot = grafico_p1, width = 12, height = 8, dpi = 300)
# Cargar la imagen
g_oferta_sitios <- readJPEG("g_oferta_sitios.jpg")
# Convertir la imagen a un grob
g_oferta_sitios <- rasterGrob(g_oferta_sitios, interpolate = TRUE)
# Agregar la tabla, el gráfico y la cadena de texto al PDF
tabla_sitios_zona <- tableGrob(
  tabla_oferta_sitios_zona, 
  rows = NULL, 
  theme = ttheme_default(base_size = 10, 
                         fg_params = list(fontsize = 9))
)
#Calculamos la mayor oferta por noche sumando la cantidad de noches mínimas ofrecidas por cada establecimiento
imp_titulo_1 <-textGrob("Tabla 1: Lista de zonas de NY y oferta de alojamiento\n por total de establecimientos activos", gp = gpar(fontsize = 10, fontface = "bold"))
df_oferta <- data.frame(Zona = datos$neighbourhood_group, Num_noches = datos$minimum_nights)
tabla_oferta_noche <- df_oferta %>% 
  group_by(Zona) %>%
  summarize(Noches = sum(Num_noches),
            Lugar = n())  #grupamos por zona y sumamos la oferta para cada zona

df_oferta <- tabla_oferta_noche %>%
  arrange(desc(tabla_oferta_noche$Noches)) #ordenamos la tabla de oferta de mayor a menor
# Crear una cadena de texto para identificar la tabla
imp_titulo_2 <- textGrob("Tabla 2: Total de plazas de alojamiento por\n zona según el mínimo de noches", gp = gpar(fontsize = 10, fontface = "bold"))
df_oferta #Dataframe que contiene para cada zona la oferta total por año y el número total de establecimientos.
tabla_oferta <- tableGrob(
  df_oferta, 
  rows = NULL,
  theme = ttheme_default(base_size = 10, 
                         fg_params = list(fontsize = 9))
)
#Graficamos la oferta por zona
grafico_p2 <- ggplot(data = df_oferta, aes(x = Zona, y = Noches, fill = Noches)) +
  geom_bar(stat = "identity") +
  xlab("Zona de Nueva York") +
  ylab("Oferta de Alojamiento") +
  ggtitle("Oferta mínima de noches por Zona de Nueva York") +
  colores_g +  
  formato_x +
  formato_grafico + formato_angulo0
ggsave("g_oferta_noches.jpg", plot = grafico_p2, width = 12, height = 8, dpi = 300)
# Cargar la imagen
g_oferta_noches <- readJPEG("g_oferta_noches.jpg")
# Convertir la imagen a un grob
g_oferta_noches <- rasterGrob(g_oferta_noches, interpolate = TRUE) 

#------------------------------------------------------------------------
# 2.- Para cada zona, ¿qué tipo de alojamiento es el más ofertado?
#Creamos una tabla de frecuencia para tipos de alojamiento por zona
imp_preg_2 <-textGrob("2.- Para cada zona, ¿qué tipo de alojamiento es el más ofertado?", 
              gp = gpar(fontsize = 10, fontface = "bold"))
#generamos una tabla de frecuencia por zona y tipo de alojamiento
tabla_tipo_zona <- xtabs(~ datos$room_type + datos$neighbourhood_group, data = datos)
tabla_tipo_zona
diccionario_tipo_zona <- list()  #declaramos lel diccionario vacío tipo list

# Llenamos el data frame con la zona y el tipo de alojamiento más ofertado
df_tipo_zona <- data.frame(Zona = character(), Tipo = character(), stringsAsFactors = FALSE)

for (i in 1:ncol(tabla_tipo_zona)) {
  columna_actual <- tabla_tipo_zona[, i]
  zona_actual <- colnames(tabla_tipo_zona)[i]
  tipo_mas_oferta <- names(which.max(columna_actual))
  df_tipo_zona <- rbind(df_tipo_zona, data.frame(Zona = zona_actual, Tipo = tipo_mas_oferta, stringsAsFactors = FALSE))
} #Calculamos y agregamos filas al dataframe df_tipo_zona

# Imprimimos el data frame
df_tipo_zona
imp_titulo_3 <- textGrob("Tabla 3: Tipos de alojamientos más ofertados en cada zonas de NY", 
          gp = gpar(fontsize = 10, fontface = "bold"))
#creamos la tabla para incluir al pdf
tabla_max_tipo <- tableGrob(
  df_tipo_zona, 
  rows = NULL,
  theme = ttheme_default(base_size = 10, 
                         fg_params = list(fontsize = 9))
) 
#Graficamos la oferta por tipo
grafico_p3 <- ggplot(data = df_tipo_zona, aes(x = Zona, y = Tipo, fill = Tipo)) +
  geom_bar(stat = "identity") +
  xlab("Zona de Nueva York") +
  ylab("Tipo Alojamiento") +
  ggtitle("Grafica 3: Tipo de alojamiento más ofecido según la Zona de NY") +
  colores_discrete +  
  formato_x +
  formato_grafico + formato_angulo0
ggsave("g_oferta_tipo.jpg", plot = grafico_p3, width = 12, height = 8, dpi = 300)
# Cargar la imagen
g_oferta_tipo <- readJPEG("g_oferta_tipo.jpg")
# Convertir la imagen a un grob
g_oferta_tipo <- rasterGrob(g_oferta_tipo, interpolate = TRUE)

#------------------------------------------------------------------------
# 3.- ¿Cuáles son las zonas más caras?
imp_preg_3 <-textGrob("3.- ¿Cuáles son las zonas más caras?, ¿Las más populares?\n¿Coinciden unas con otras?", 
              gp = gpar(fontsize = 10, fontface = "bold"))

tabla_precios <- data.frame(Zona = datos$neighbourhood_group, Precio = datos$price)
sum(is.na(tabla_precios))
tabla_precios <- tabla_precios[!is.na(tabla_precios$Precio), ]
sum(is.na(tabla_precios))
df_precios <- tabla_precios %>%
  group_by(Zona) %>%
  summarize(Precio = max(Precio)) %>%
  arrange(desc(Precio))
df_precios
#creamos la tabla para incluir al pdf
tabla_max_precio <- tableGrob(
  df_precios, 
  rows = NULL,
  theme = ttheme_default(base_size = 10, 
                         fg_params = list(fontsize = 9))
) 
imp_titulo_4 <- textGrob("Tabla 4: Precios más alto por zona de NY", 
          gp = gpar(fontsize = 10, fontface = "bold"))
#Graficamos la oferta por tipo
grafico_p4 <- ggplot(data = df_precios, aes(x = Zona, y = Precio, fill = Precio)) +
  geom_bar(stat = "identity") +
  xlab("Zona de Nueva York") +
  ylab("Precio") +
  ggtitle("Precios por Zona de Nueva York") +
  colores_g +  
  formato_x +
  formato_grafico + formato_angulo0
ggsave("g_oferta_precio.jpg", plot = grafico_p4, width = 12, height = 8, dpi = 300)
# Cargar la imagen
g_oferta_precio <- readJPEG("g_oferta_precio.jpg")
# Convertir la imagen a un grob
g_oferta_precio <- rasterGrob(g_oferta_precio, interpolate = TRUE)

#¿Y las más populares?  ¿Coinciden unas con otras?
# METODO 1
tabla_popular <- data.frame(Zona = datos$neighbourhood_group, Votos = datos$number_of_reviews)
tabla_popular <- tabla_popular %>% filter(!is.na(Votos))
tabla_popular <- tabla_popular %>% filter(Votos != 0)
dim(tabla_popular)
df_popular <- tabla_popular %>%
  group_by(Zona) %>%
  summarize(Votos = max(Votos)) %>%
  arrange(desc(Votos))
df_popular

#creamos la tabla para incluir al pdf
tabla_max_popular <- tableGrob(
  df_popular, 
  rows = NULL,
  theme = ttheme_default(base_size = 10, 
                         fg_params = list(fontsize = 9))
)
imp_titulo_5 <- textGrob("Tabla 5: Comparativa de la Popularidad de las Zonas de NY", 
          gp = gpar(fontsize = 10, fontface = "bold"))
#Graficamos por popularidad
grafico_p5 <- ggplot(data = df_popular, 
  aes(x = Zona, y = Votos, fill = Votos)) +
  geom_bar(stat = "identity") +
  xlab("Zona de Nueva York") +
  ylab("Votos - Popularidad") +
  ggtitle("Establecimientos más votados por zona de NY") +
  colores_g +  
  formato_x +
  formato_grafico + formato_angulo0
ggsave("g_popular.jpg", plot = grafico_p5, width = 12, height = 8, dpi = 300)
# Cargar la imagen
g_popular<- readJPEG("g_popular.jpg")
# Convertir la imagen a un grob
g_popular <- rasterGrob(g_popular, interpolate = TRUE)

#------------------------------------------------------------------------
# 4.- Los dueños de más de un alojamiento tienen sus ofertas en el
#  mismo sitio o en sitios diferentes?
imp_preg_4 <-textGrob("4.- ¿Los dueños de más de un alojamiento tienen sus ofertas en el\nmismo sitio o en sitios diferentes?", 
              gp = gpar(fontsize = 10, fontface = "bold"))
tabla_duenos <- data.frame(Nombre = datos$host_name, Zona = datos$neighbourhood_group)
sum(is.na(tabla_duenos))
tabla_duenos <- tabla_duenos[!is.na(tabla_duenos$Nombre), ]
head(tabla_duenos)
dim(tabla_duenos)
#Agrupar por dueños para obtener la lista de zonas donde tienen establecimientos
df_duenos <- tabla_duenos %>%
  group_by(Nombre) %>%
  summarize(Zonas = paste0(unique(Zona), collapse = ", "),
            Total = n_distinct(Zona))
# Filtramos por zonas para obtener los dueños con más de un establecimiento por zona
df_duenos_varios <- df_duenos %>%
  filter(nchar(Zonas) > 1) %>%
  arrange(desc(Total))
# Agrupamos para contar el total de establemcimientos por cada zona
df_duenos_contar <- df_duenos %>%
  group_by(Zonas = Total) %>%
  summarize(Propietarios = n_distinct(Nombre))
dim(df_duenos_contar)
df_duenos_contar
#Calculamos el porcentaje de dueños por cantidad de zonas de NY que abarcan
df_duenos_porcentaje <- df_duenos_contar %>%
  mutate(Porcentaje = (round(Propietarios / sum(Propietarios) * 100, 2)))
df_duenos_porcentaje$Porcentaje <- round(df_duenos_porcentaje$Porcentaje)
imp_titulo_6 <- textGrob("Tabla 5: Porcentaje de dueños con más de 1 zona", 
          gp = gpar(fontsize = 10, fontface = "bold"))
names(df_duenos_porcentaje)
df_duenos_porcentaje
#creamos la tabla para incluir al pdf
tabla_duenos_porcentaje <- tableGrob(
  df_duenos_porcentaje, 
  rows = NULL,
  theme = ttheme_default(base_size = 10, 
                         fg_params = list(fontsize = 9))
) 

#Graficamos cantidad de zonas abarcadas por propietarios
grafico_p6 <- ggplot(data = df_duenos_porcentaje, 
  aes(x = Zonas, y = Porcentaje, fill = Porcentaje)) +
  geom_bar(stat = "identity") +
  xlab("# de zonas que ocuapan en porcenaje") +
  ylab("Porcenaje de dueños") +
  ggtitle("Porcentaje sobre el total de dueños con variación en las zonas") +
  colores_g +  
  formato_x +
  formato_grafico + formato_angulo0
ggsave("g_duenos.jpg", plot = grafico_p6, width = 12, height = 8, dpi = 300)
# Cargar la imagen
g_duenos <- readJPEG("g_duenos.jpg")
# Convertir la imagen a un grob
g_duenos <- rasterGrob(g_duenos, interpolate = TRUE)

#------------------------------------------------------------------------
# 5.- ¿Cuál es el alojamiento más caro teniendo en cuenta el número
# mínimo de noches por reserva?¿Y el más barato?
imp_preg_5 <-textGrob("5.- ¿Cuál es el alojamiento más caro teniendo en cuenta el número\nmínimo de noches por reserva?¿Y el más barato?", 
              gp = gpar(fontsize = 10, fontface = "bold"))
tabla_precios_noches <- data.frame(Lugar = datos$name, Precio = datos$price, Minima = datos$minimum_nights)
tabla_precios_noches <- tabla_precios_noches %>% filter(!is.na(Lugar), !is.na(Precio), !is.na(Minima))
sum(is.na(tabla_precios_noches))
attach(tabla_precios_noches)
tabla_precios_noches$Resultado <- Precio * Minima
head(tabla_precios_noches)
tabla_precios_noches_max <- tabla_precios_noches %>%
  arrange(desc(Resultado))
head(tabla_precios_noches_max)
tabla_precios_noches_min <- tabla_precios_noches %>%
  arrange((Resultado))
head(tabla_precios_noches_min)
tabla_ranking <- data.frame(LugarCaro = head(tabla_precios_noches_max$Lugar, 10), 
                                MasCostoso = head(tabla_precios_noches_max$Resultado, 10),
                                LugarBarato = head(tabla_precios_noches_min$Lugar, 10),
                                MasBarato = head(tabla_precios_noches_min$Resultado, 10))
tabla_ranking
imp_titulo_7 <- textGrob("Tabla 6: Ranking de los 10 lugares y más caros y más baratos en precio por noche", 
          gp = gpar(fontsize = 10, fontface = "bold"))
#creamos la tabla para incluir al pdf
tabla_ranking_ <- tableGrob(
  tabla_ranking, 
  rows = NULL,
  theme = ttheme_default(base_size = 9, 
                         fg_params = list(fontsize = 8))
)
tabla_ranking[3:4]
#Graficamos los 10 primeros lugares más costosos por noche
grafico_p7 <- ggplot(data = tabla_ranking[1:2], 
  aes(x = LugarCaro, y = MasCostoso, fill = MasCostoso)) +
  geom_bar(stat = "identity") +
  xlab("Lugares") +
  ylab("Precio por noche") +
  labs(title = "Ranking de los 10 lugares más costosos por noche", 
  x = "Dueños", 
  y = "Precio", fill = "Precio") +
  colores_g +  
  formato_x +
  formato_grafico + formato_angulo45
ggsave("g_caros.jpg", plot = grafico_p7, width = 12, height = 8, dpi = 300)
# Cargar la imagen
g_caros <- readJPEG("g_caros.jpg")
# Convertir la imagen a un grob
g_caros <- rasterGrob(g_caros, interpolate = TRUE)

#Graficamos los 10 primeros lugares más baratos por noche
grafico_p8 <- ggplot(data = tabla_ranking[3:4], 
  aes(x = LugarBarato, y = MasBarato, fill = MasBarato)) +
  geom_bar(stat = "identity") +
  xlab("Lugares") +
  ylab("Precio por noche") +
  labs(title = "Ranking de los 10 lugares más baratos por noche", 
  x = "Dueños", 
  y = "Precio", fill = "Precio") +
  colores_g +  
  formato_x +
  formato_grafico + formato_angulo45
ggsave("g_baratos.jpg", plot = grafico_p8, width = 12, height = 8, dpi = 300)
# Cargar la imagen
g_baratos <- readJPEG("g_baratos.jpg")
# Convertir la imagen a un grob
g_baratos <- rasterGrob(g_baratos, interpolate = TRUE)

#Sección de Respuestas: ------------------------------------------------------
respuesta1 <- paste(
  "Cómo podemos observar, se puede responder a la pregunta\ncon dos características de los datos:\n",
  "1.- Por la cantidad de establecimientos ofertados (Tabla 1)\n",
  "2.- Por cantidad de noches mínimas ofrecidas en cada establacimiento (Tabla 2)\n",
  "En el primer caso, Brooklyn es la ciudad de NY con más lugares disponibles\n",
  "con un total de 1865 establecimientos, seguido de Manhattan con 1821 establecimientos.\n",
  "En el segundo caso, la principal zona con ofertas es Manhattan con 23754 días mínimos de reservas diario.\n",
  "Le sigue Brooklyn con 15253 y Queens con 2484. En ambos casos, Bookyn es la de menos ofertas de NY.\n"
)
# Imprimir el texto del párrafo
cat(respuesta1)
respuesta1 <- textGrob(respuesta1,
                      gp = gpar(fontsize = 10))

respuesta2 <- paste(
  "De los tres tipo ofrecidos en NY, como son Private room, Entire Home/apt y Shared Home\n",
  "Sólo los dos primero son los más ofrecidos, y el tipo de alojamiento más ofrecido en NY\n",
  "es Entire Home/apt compartido por las zonas de Bookyn, Brooklyn y Manhattan.\n"
)
# Imprimir el texto del párrafo
cat(respuesta2)
respuesta2 <- textGrob(respuesta2,
                      gp = gpar(fontsize = 10))

respuesta3 <- paste(
  "En NY hay 6 zonas que la conforman,la zona más cara en función del precio por noche es\n",
  "Brooklyn con un precio de 8,000$ por noche, seguida de Manhathan con 6,000$ por noche.\n",
  "En cambio, la más barata se encuentra en el Bronx con 250$ por noche.\n",
  "Por otro lado, la zona más popular es Manhathan con 607 votos seguido de Brooklyn con\n",
  "488 votos. Y ambos ocupan los primeros lugares en el ranking de más caros/noche.\n",
  "Igualmente, el menos popular también es el Bronk con 321 votos.\n"
)
# Imprimir el texto del párrafo
cat(respuesta3)
respuesta3 <- textGrob(respuesta3,
                      gp = gpar(fontsize = 10))

respuesta4 <- paste(
  "Sólo un 29% de los propietarios tienen alojamientos en más de una zona.\n",
  "El 81% de los propietarios prefieren tener alojamientos en la misma zona.\n"
)
# Imprimir el texto del párrafo
cat(respuesta4)
respuesta4 <- textGrob(respuesta4,
                      gp = gpar(fontsize = 10))

respuesta5 <- paste(
  "El lugar más caro es Prime W. Village location 1 bdrm, con un mínimo de reserva\n",
  "de 225,000$ seguido de Located at the heart of Manhattan con un 50% menos del precio\n",
  "noche, siendo 1 noche lo mínimo que puedes reservar.\n",
  "El 81% de los propietarios prefieren tener alojamientos en la misma zona.\n"
)
# Imprimir el texto del párrafo
cat(respuesta5)
respuesta5 <- textGrob(respuesta5,
                      gp = gpar(fontsize = 10))
#------------------------------------------------------------------------
# Imprimimos en el pdf
# pdf("Ramirez_Hernandez_p1_ACUE.pdf", onefile=TRUE, width = 9, height = 15)
pdf("Ramirez_Hernandez_p1_ACUE.pdf", onefile=TRUE, width = 8.5, height = 11)
#Agregar la cabecera al informe pdf
# Agregar la tabla, el gráfico y la cadena de texto al PDF
grid.arrange((gridExtra::arrangeGrob(
  imp_cabecera)),
  top = " -\n\n\n",
  bottom = " -\n",
  ncol=1
)

# Agregar la tabla, el gráfico y la cadena de texto al PDF
grid.arrange(
  gridExtra::arrangeGrob(imp_preg_1, imp_titulo_1, tabla_sitios_zona, g_oferta_sitios,
                        heights = c(20, 10,35,55,5), nrow=5),
  top = " -\n\n",
  bottom = " -",
  widths=c(6),
  heights=c(8, 1)
)

# Combinar los elementos en una sola página
grid.arrange((gridExtra::arrangeGrob(imp_titulo_2, tabla_oferta, 
              g_oferta_noches,respuesta1,
              heights = c(20,45,90,50), nrow=4)),
  top = " -\n\n",
  bottom = " -",
  widths=c(6),
  heights=c(8, 1)
)

  # Agregar la tabla, el gráfico y la cadena de texto al PDF
grid.arrange((gridExtra::arrangeGrob(
              imp_preg_2,
              imp_titulo_3,
              tabla_max_tipo, g_oferta_tipo, respuesta2,
              heights = c(10,15,50,70,25), nrow=5)),
  top = " -\n\n",
  bottom = " -",
  widths=c(6),
  heights=c(8, 1)
)

# Agregar la tabla, el gráfico y la cadena de texto al PDF
grid.arrange((gridExtra::arrangeGrob(
  imp_preg_3,
  imp_titulo_4,
  tabla_max_precio, g_oferta_precio,
              heights = c(10,15,40,60,15), nrow=5)),
  top = " -\n\n",
  bottom = " -\n",
  widths=c(6),
  heights=c(8, 1)
)

grid.arrange((gridExtra::arrangeGrob(
  imp_titulo_5,
  tabla_max_popular, g_popular,respuesta3,
              heights = c(20,55,90,50), nrow=4)),
  top = " -\n\n",
  bottom = " -\n",
  widths=c(6),
  heights=c(8, 1)
)

grid.arrange((gridExtra::arrangeGrob(
  imp_preg_4,
  imp_titulo_6,
  tabla_duenos_porcentaje, g_duenos,respuesta4,
          heights = c(10,15,40,60,15), nrow=5)),
  top = " -\n\n",
  bottom = " -\n",
  widths=c(6),
  heights=c(8, 1)
)

grid.arrange((gridExtra::arrangeGrob(
  imp_preg_5,
  imp_titulo_7, tabla_ranking_, g_caros, heights = c(15, 3,35,55,5), nrow=5, ncol = 1)),
  # gridExtra::arrangeGrob(g_caros, heights = c(1, 2), ncol = 1),
  widths=c(6),
  heights=c(8, 1),
  top = " - ",
  bottom = " -\n"
)

grid.arrange((gridExtra::arrangeGrob(
  g_baratos,
  respuesta5, 
  heights = c(70,40,30,10), nrow=4, ncol = 1)),
  widths=c(6),
  heights=c(25, 1),
  top = " -\n\n\n",
  bottom = " -\n"
)

dev.off()












