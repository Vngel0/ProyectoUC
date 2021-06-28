#Librarias a utilizar-----------------------------------------------------------------------------------------------
library(janitor)
library(skimr)
library(tidyverse)
library(lubridate)
?lubridate
#Importar dataset desde Github--------------------------------------------------------------------------------------

ocupacion <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto24/CamasHospital_Diario_std.csv")

casos_comuna <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto1/Covid-19_std.csv", encoding = "UTF-8" )

vacunacion_pais <- read.csv("country_vaccinations.csv")

tipo_vacuna_pais <- read.csv("country_vaccinations_by_manufacturer.csv")

read_csv("https://raw.githubusercontent.com/Vngel0/ProyectoUC/main/Proceso%20vacunacion%20hasta%2027-06-21.csv")

#Datasets-----------------------------------------------------------------------------------------------------------

##Dataset 1: ocupacion de camas

#Exploracion 1: Vista preliminar pre-limpieza-----------------------------------------------------------------------

skim(ocupacion)

#No presenta valores perdidos, por lo que la data esta completa

tail(ocupacion)

#Última actualización de datos al 26 de junio 2021

"
Cama básica : destinada a pacientes que, estando en cualquiera de las etapas de una enfermedad (evaluación, diagnóstico, tratamiento y / o recuperación)

Cama media : destinada a entregar cuidados a pacientes de mediana complejidad.

Camas de cuidados críticos (UTI, UCI) : destinada a brindar cuidados de alta complejidad.
"

#Puesta en punto del dataset (limpieza, filtros y tranasformación de tipos de datos)

skim(ocupacion)
sapply(ocupacion, class)

#Se requiere cambios de formato, filtro de camas UCI y UTI, y cambios en el titulo de variables

ocupacion_v1 <- ocupacion %>%
  filter(Tipo.de.cama == "UCI" | Tipo.de.cama == "UTI") %>%
  clean_names() %>%
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d"),
         casos_confirmados = as.numeric(casos_confirmados))

tail(ocupacion_v1)
sapply(ocupacion_v1, class)
str(ocupacion_v1)

max(ocupacion_v1$fecha)
min(ocupacion_v1$fecha)

# se puede observar que el rango de fecha está entre 20-04-16 y 2021-08-27.


##Dataset 2: casos por comuna--

skim(casos_comuna)
#Se presentan valores perdidos

sapply(casos_comuna, class)
#Se requiere el cambio en el tipo de dato de fecha

#Puesta en punto del dataset (limpieza, filtros y tranasformación de tipos de datos)

casos_comuna_v1 <- na.omit(casos_comuna) %>%
  clean_names() %>%
  mutate(fecha = as.Date(fecha))

skim(casos_comuna_v1)
sapply(casos_comuna_v1, class)

#casos_comuna_v2 <- casos_comuna_v1 %>%
#  mutate(año = format(as.Date(fecha), "%Y"),
#         mes = format(as.Date(fecha), "%B"), #"%B transforma el mes de numero a letras
#         fecha_corta = paste(año, mes),
#         id_region = case_when(
#           region == "Arica y Parinacota" ~ 1,
#           region == "Tarapacá" ~ 2,
#           region == "Antofagasta" ~ 3,
#           region == "Atacama" ~ 4,
#           region == "Coquimbo" ~ 5,
#           region == "Valparaíso" ~ 6,
#           region == "Metropolitana" ~ 7,
#           region == "O’Higgins" ~ 8,
#           region ==  "Maule" ~ 9,
#           region == "Ñuble" ~ 10,
#           region == "Biobío" ~ 11,
#           region == "Biobío" ~ 12,
#           region == "Araucanía" ~ 13,
#           region == "Los Ríos" ~ 14,
#           region == "Los Lagos" ~ 15,
#           region == "Aysén" ~ 16,
#           region == "Magallanes" ~ 17
#         ))
# unique(casos_comuna_v2$region)
#
#casos_comuna_v2$region <- factor(casos_comuna_v2$region)
#
#casos_comuna_v2  %>%
#  group_by(region) %>%
#  summarise(mean(casos_confirmados),
#            sum(casos_confirmados),
#            .groups = "keep")
#
#casos_comuna_v2 %>%
#  group_by(fecha_corta) %>%
#  summarise(sum(casos_confirmados))
#
#casos_comuna_v2 %>%
#  group_by(region) %>%
#  summarise(.groups = "rowwise")

##Dataset 3: vacunación por pais

skim(vacunacion_pais)

#Hay valores perdidos, pero primero ordenaremos el dataset de acuerdo a los paises de estudio

vacunacion_pais_v1 <- vacunacion_pais %>%
  filter(country == "Chile" |
           country == "Argentina" |
           country == "Brazil" |
           country == "Perú" |
           country == "Ecuador" |
           country == "Bolivia" |
           country == "Uruguay") %>%
  select(country, iso_code, date, total_vaccinations, daily_vaccinations)

names(vacunacion_pais_v1) = c("Pais", "Código", "Fecha", "Vacunacion_Total","Vacunacion_diaria")

sapply(vacunacion_pais_v1, class)

vacunacion_pais_v1 <- vacunacion_pais_v1 %>%
  clean_names() %>%
  mutate(fecha = as.Date(fecha))

skim(vacunacion_pais_v1)


##Dataset 4: tipo de vacuna por pais

tipo_vacuna_pais

skim(tipo_vacuna_pais)
sapply(tipo_vacuna_pais, class)

#Se requiere filtro por los paises de estudio, cambio de formato y encabezados de columna

tipo_vacuna_pais_v1 <- tipo_vacuna_pais %>%
  filter(location == "Chile" |
           location == "Argentina" |
           location == "Brazil" |
           location == "Perú" |
           location == "Ecuador" |
           location == "Bolivia" |
           location == "Uruguay") %>%
  clean_names() %>%
  mutate(date = as.Date(date))

names(tipo_vacuna_pais_v1) <- c("pais", "fecha", "tipo_vacuna", "total_vacunados")

# Datasets limpios --------------------------------------------------------

ocupacion_v1
casos_comuna_v1
vacunacion_pais_v1
tipo_vacuna_pais_v1


# Graficos ----------------------------------------------------------------

# DS1
# Evolucion de ocupacion de camas criticas

who_events <- tribble(
  ~ fecha, ~ evento,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting\nchange"
) %>%
  mutate(fecha = as.Date(fecha))

evolucion_casos_fig <- ggplot(ocupacion_v1, aes(fecha, casos_confirmados , colour = tipo_de_cama))+
  geom_line() +
  theme(
    panel.grid.major = element_line(colour = "gray0", linetype = "blank"),
    panel.grid.minor = element_line(colour = "green4",linetype = "blank"),
    axis.title = element_text(size = 15),
    axis.text = element_text(colour = "gray0"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = NA),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1)) +
  labs(
    title = "Evolución de casos confirmados",
    x = "Fecha",
    y = "Casos Confirmados",
    colour = "Tipo de cama",
    caption = "Elaboracion propia")

evolucion_casos_fig



# DS2
#casos_comuna_v1 graficos


# Grafico acumulado de casos por region
#casos_comuna_v2 %>%
#  ggplot(aes(x = region, stat = "count"))+
#  geom_bar(fill = "turquoise1", color = "black", bins = 10) +
#  theme(panel.grid.major = element_line(linetype = "blank"),
#    panel.grid.minor = element_line(size = 0.3,
#        linetype = "dashed"), axis.text = element_text(size = 7,
#        colour = "turquoise1"), axis.text.x = element_text(colour = "turquoise1"),
#    plot.title = element_text(family = "AvantGarde",
#        face = "bold", colour = "turquoise1",
#        hjust = 0.5), panel.background = element_rect(fill = "black"),
#    plot.background = element_rect(fill = "black",
#        colour = NA), strip.text = element_text(family = "serif"),
#    legend.position = "none") +labs(title = "Casos acumulado por región",
#    y = "Cantidad")

# DS3

ggplot(vacunacion_pais_v1, aes)







