#¿Cómo avanza el proceso de vacunación en los principales países de Sudamérica?
#(Argentina, Brasil, Bolivia, Chile, Ecuador, Perú, Uruguay)?

#install.packages("ggpubr")
#install.packages("ggfortify")
#install.packages("agricolae")
#install.packages(lubridate)
library(tidyverse)
library(ggpubr)
library(ggfortify)
library(ggplot2)
library(agricolae)
library(lubridate)
library(scales)


#Los datos fueron obtenidos de desde https://www.kaggle.com/gpreda/covid-world-vaccination-progress el día 26 de junio de 2021. El archivo csv contiene la Vacunación diaria y total contra COVID-19 a nivel mundial.

#Carga de base de datos ----

vacunacion_world <- read_csv("./country_vaccinations.csv")


vacunacion_world2 <- vacunacion_world %>%
    select(country, iso_code, date, total_vaccinations, daily_vaccinations, people_vaccinated, people_fully_vaccinated)

names(vacunacion_world2) = c("country", "iso_code", "fecha", "total_vaccinations", "daily_vaccinations", "people_vaccinated", "people_fully_vaccinated")

#Revisión de base de datos original ----

#Descripción de variables:
#Date: fecha para la entrada de datos; para algunas de las fechas solo tenemos las vacunas diarias, para otras, solo el total (acumulativo)
#Total number of vaccinations: este es el número absoluto de inmunizaciones totales en el país
#Total number of people vaccinated: Número total de personas vacunadas. Una persona, según el esquema de inmunización, recibirá una o más (normalmente 2) vacunas; en un momento determinado, el número de vacunaciones puede ser mayor que el número de personas;
#Total number of people fully vaccinated: Número total de personas completamente vacunadas: este es el número de personas que recibieron el conjunto completo de inmunización de acuerdo con el esquema de inmunización (normalmente 2); en un momento determinado, puede haber un cierto número de personas que recibieron una vacuna y otro número (menor) de personas que recibieron todas las vacunas del esquema;
#Daily vaccinations (raw): Vacunas diarias (crudas): para una determinada entrada de datos, el número de vacunaciones para esa fecha / país
#Daily vaccinations: Vacunas diarias: para una determinada entrada de datos, el número de vacunaciones para esa fecha / país;
#Number of vaccinations per day: número de vacunaciones diarias para ese día y país (Nro de dosis adminitradas)

#identificar dimensión de la BD, nombres de columnas y tipos de datos de cada una de ellas
#La BD completa tiene 26.881 filas y 15 columnas
glimpse(vacunacion_world2)
dim(vacunacion_worl2)
names(vacunacion_world2)

#Identificar cantidad de países y número de registros por país
grupos <- group_by(vacunacion_world2, country)
summarise(grupos,
          num = n()
)


#Filtrar la BD para quedarnos con los países de interés ----

#Filtramos para quedarnos sólo con los países de interés
vacunacion_latam <- filter(vacunacion_world2, iso_code %in% c("CHL", "ARG", "BRA", "BOL", "ECU", "PER", "URY"))

#Revisión de base de datos filtrada ----
#La BD filtrada tiene 1.070 filas y 15 columnas
#Tiene datos desde el 24-12-2020 al 24-06-2021
glimpse(vacunacion_latam)
min(vacunacion_latam$fecha)
max(vacunacion_latam$fecha)

#Fechas de iinicio de la vacunación
fechas_inicio <- vacunacion_latam %>%
  group_by(iso_code) %>%
  summarize(min_date = min(fecha))
write.csv(fechas_inicio, file="fecha_inicio_vacunacion.csv", row.names = F)

#tabla de contingencia para saber cuantos registros por país tenemos
#ARG BOL BRA CHL ECU PER URY
#178 145 160 181 154 135 117
table(vacunacion_latam$iso_code)


#Limpieza ----

#identificar valores NA
tabla_resumen <- skimr::skim(vacunacion_latam)
summary(vacunacion_latam)
#capture.output(tabla_resumen, file="tabla_resumen.doc")
#write.csv(tabla_resumen, file="tabla_resumen.csv", row.names = F)

#identificar el tipo de datos de cada variable
str(vacunacion_latam)

#dado que tenemos valos NA en nuestra columna de vacunaciones diarias
vacunacion_latam[is.na(vacunacion_latam)] <- 0
skimr::skim(vacunacion_latam)

#división de fechas
vacunacion_latam2 <- vacunacion_latam %>%
  mutate(año = year(vacunacion_latam$fecha),
         mes = month(vacunacion_latam$fecha),
         dia = day(vacunacion_latam$fecha),
         fecha_corta = paste(mes, año))

# write.csv(vacunacion_latam2, file="vacunacion_latam2.csv", row.names = F)

#Calculo de estadísticos ----

#Total
Resumen_vacuna_diaria <- summary(vacunacion_latam2$daily_vaccinations)
Resumen_vacuna_diaria
capture.output(Resumen_vacuna_diaria, file="Resumen_vacuna_diarian.doc")

#Por país
estadisticoxpais <- vacunacion_latam2 %>%
  group_by(iso_code) %>%
  summarize(media_vacunacion = mean(daily_vaccinations),
            max_vacunacion = max(daily_vaccinations),
            primer_cuartil = quantile(daily_vaccinations, prob=c(0.25)),
            tercer_cuartil = quantile(daily_vaccinations, prob=c(0.75)),
            mediana_vacunacion = median(daily_vaccinations))


#Representaciones gráficas ----

#Evolución de la vacunación diaria según el país (Dosis administradas)

#suma_por_paismes2 <- vacunacion_latam2 %>%
#  group_by(iso_code, fecha_corta) %>%
#  summarize(suma = sum(daily_vaccinations),
#            sumaM = round(suma/1000))

#suma_por_paismes3 <- vacunacion_latam2 %>%
#  group_by(iso_code, date) %>%
#  summarize(suma = sum(daily_vaccinations),
#            sumaM = round(suma/1000))

#suma_por_paismes2 %>%
#  filter(iso_code == "CHL")  %>%
#  ggplot(aes(x = fecha_corta, y = sumaM)) +
#  geom_line() +
#  geom_point()

#vacunacion_latam2_chl <- vacunacion_latam2 %>%
#  filter(vacunacion_latam2$iso_code == "CHL")

prueba <- vacunacion_latam2 %>%
  filter(año == 2021)

avance_por_pais <- ggplot(prueba, aes(fecha, daily_vaccinations, colour = iso_code))+
  geom_line() +
  theme(
    panel.grid.major = element_line(colour = "gray0", linetype = "blank"),
    panel.grid.minor = element_line(colour = "green4",linetype = "blank"),
    axis.title = element_text(size = 11),
    axis.text = element_text(colour = "gray0"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = NA),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank())+
    # legend.background = element_rect(fill="lightgrey")) +
  labs(
    title = "Vacunación diaria por país de interés",
    x = "Fecha",
    y = "Dosis administradas",
    colour = "",
    caption = "Fuente: Elaboración propia")+
  theme(
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.position = "top",
    axis.title = element_text(size = 15),
    axis.text = element_text(colour = "gray0"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))+
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1)) + theme(legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA),
    legend.direction = "horizontal") +labs(colour = NULL)

avance_por_pais
# ggsave("avance_x_pais2021.png", avance_por_pais)


#Evolución sin Brasil
prueba2 <- vacunacion_latam2 %>%
  filter(año == 2021 & iso_code != "BRA")


avance_por_pais2 <- ggplot(prueba2, aes(fecha, daily_vaccinations, colour = iso_code))+
  geom_line() +
  theme(
    panel.grid.major = element_line(colour = "gray0", linetype = "blank"),
    panel.grid.minor = element_line(colour = "green4",linetype = "blank"),
    axis.title = element_text(size = 11),
    axis.text = element_text(colour = "gray0"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = NA),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill="lightgrey")) +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
  labs(
    title = "Vacunación diaria por país de interés",
    x = "Fecha",
    y = "Dosis administradas",
    colour = "País",
    caption = "Fuente: Elaboración propia")+
  theme(
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.position = "top",
    axis.title = element_text(size = 15),
    axis.text = element_text(colour = "gray0"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))+
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1)) +
  theme(legend.key = element_rect(fill = NA),
  legend.background = element_rect(fill = NA),
  legend.direction = "horizontal") +
  labs(colour = NULL)

avance_por_pais2
# ggsave("avance_x_pais2021_sinBrasil.png", avance_por_pais2)

#Evolución 2020, para ver quién inicio primero

prueba3 <- vacunacion_latam2 %>%
  filter(año == 2020)

avance_por_pais <- ggplot(prueba3, aes(fecha, daily_vaccinations, colour = iso_code))+
  geom_line() +
  theme(
    panel.grid.major = element_line(colour = "gray0", linetype = "blank"),
    panel.grid.minor = element_line(colour = "green4",linetype = "blank"),
    axis.title = element_text(size = 11),
    axis.text = element_text(colour = "gray0"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = NA),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill="lightgrey")) +
  labs(
    title = "Vacunación diaria por país de interés",
    x = "Fecha",
    y = "Dosis administradas",
    colour = "País",
    caption = "Fuente:Elaboración propia")

avance_por_pais
# ggsave("avance_x_pais2020.png", avance_por_pais)

#Dosis adminitradas por país (Ranking)

prueba4 <- vacunacion_latam2 %>%
  group_by(iso_code) %>%
  summarize(suma = sum(daily_vaccinations))

prueba5 <- arrange(prueba4, desc(suma))
names(prueba5) = c("Países", "Dosis_Administradas")

ranking <- ggplot(prueba5, aes(Países, Dosis_Administradas, fill = Países)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
  coord_flip() +
  labs(caption = "Fuente:Elaboración propia")

ranking
# ggsave("ranking.png", ranking)

grafico_ordenado <- ggplot(prueba5, aes(Dosis_Administradas, reorder(Países, Dosis_Administradas), fill = Países))+
  stat_summary(fun = sum, geom = "bar", show.legend = F)+
  labs(y= "Países",
       x = "Dosis administradas",
       fill = "",
       caption = "Fuente: Elaboración propia")+
  scale_x_continuous(labels = function(n){format(n, scientific = F)})+
  theme_minimal()+
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 12),
    axis.text = element_text(colour = "gray0"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.25),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(hjust = 1))


grafico_ordenado
# ggsave("ranking2.png", grafico_ordenado)

#Vacunas más utilizadas
#matriz <- as.matrix(vacunacion_latam)
#vacunas <- str_split(vacunacion_latam$vaccines, ", ", simplify = #TRUE)
#write.csv(x, file="vacunas.csv", row.names = F)
#vacunas2 <- cbind(matriz, vacunas)
#vacunas3 <- as.data.frame(vacunas2)

#table(vacunas3$iso_code, vacunas3$V17)
#tot_vacuna <- table(vacunas3$iso_code, vacunas3$vaccines)
#capture.output(tot_vacuna, file="tot_vacuna.doc")



