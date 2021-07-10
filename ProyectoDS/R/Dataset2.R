# Librarias a utilizar----

library(janitor)
library(skimr)
library(tidyverse)
library(scales)
library(readxl)
library(ggrepel)
library(modeest)
library(lubridate)

#Importe Dataset 2----
ocupacion <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto24/CamasHospital_Diario_std.csv")

##Limpieza----
ocupacion_v1 <- ocupacion %>%
  clean_names() %>%
  mutate(fecha = as.Date(fecha))

##Dataset complemento----
###Casos----

casos <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales_T.csv", encoding = "UTF-8") %>% clean_names()

## Limpieza
casos[is.na(casos)] <- 0

casos <- casos %>%
  mutate(fecha = as.Date(fecha)) %>%
  arrange(fecha)

###DS1----
list.files("./dataset1")
archivos <- list.files("./dataset1")
vacunacion_chile <- data.frame()
for(archivo in archivos){
  nombre_archivo <- paste0("./dataset1/",archivo)
  df <- read_excel(nombre_archivo)
  vacunacion_chile <- rbind(vacunacion_chile,df)
}
rm(df, archivo, archivos, nombre_archivo)
## Limpieza

# limpieza de nombres y formato fecha
vacunacion_chile_v1 <- vacunacion_chile %>%
  clean_names() %>%
  mutate(fecha_inmunizacion = as.Date(fecha_inmunizacion,
                                      format="%d/%m/%Y"))

rm(ocupacion ,vacunacion_chile)

#Se requiere cambio en los nombres de los encabezados
names(vacunacion_chile_v1) = c("laboratorio",
                               "cantidad",
                               "comuna",
                               "dosis",
                               "fecha",
                               "region",
                               "pfizer",
                               "sinovac",
                               "astrazeneca",
                               "cansino",
                               "tipo_poblacion")

#Se reemplazan los NA por 0
vacunacion_chile_v1[is.na(vacunacion_chile_v1)] <-0

#La siguiente linea agrega una variable indicando solamente el nombre de la vacuna aplicada
vacunacion_chile_v1$vacuna <- ifelse(vacunacion_chile_v1$laboratorio == "Campaña SARS-CoV-2 (AstraZeneca)", "AstraZeneca",
                                     ifelse(vacunacion_chile_v1$laboratorio == "Campaña SARS-CoV-2 (CanSino)", "CanSino",
                                            ifelse(vacunacion_chile_v1$laboratorio == "Campaña SARS-CoV-2 (Pfizer)", "Pfizer",
                                                   ifelse(vacunacion_chile_v1$laboratorio == "Campaña SARS-CoV-2 (Sinovac)", "Sinovac",
                                                          0))))

# Consolidado de bases----
vac_diaria <- vacunacion_chile_v1 %>% select(fecha, cantidad) %>% arrange(fecha) %>%
  group_by(fecha) %>% summarise(cantidad = sum(cantidad))

names(vac_diaria) <- c("fecha","vacunados_diaros")

ocu_diaria <- ocupacion_v1 %>% arrange(fecha) %>% select(fecha, casos_confirmados) %>%
  group_by(fecha) %>% summarise(casos_confirmados = sum(casos_confirmados))

names(ocu_diaria) <- c("fecha","camas_utilizadas_diarias")


# union de DS 2 y ds1
consolidado <- left_join(ocu_diaria,vac_diaria, by = "fecha")

# union de consolidado y casos
consolidado_final <- left_join(casos,consolidado, by = "fecha")

consolidado_final[is.na(consolidado_final)] <- 0

consolidado_final <- consolidado_final %>% filter(fecha <= "2021-06-30")

rm(ocupacion, ocupacion_v1, vacunacion_chile_v1, casos,
   vac_diaria, ocu_diaria, consolidado)

#Comparativo en uso de camas y vacunados diarios----

consolidado_final <- consolidado_final %>% filter(fecha >= "2020-04-18")

grafico_vacunacion_diaria <- ggplot(consolidado_final, aes(x = fecha, y = vacunados_diaros, fill = c("#FF2D00")))+
  stat_summary(fun = sum, geom = "bar", show.legend = F)+
  labs(y= "Vacunas aplicadas",
       x = "",
       fill = "",
       caption = "Fuente: Elaboración propia, a partir de datos del Minsal.")+
  theme_minimal()+
  scale_y_continuous(labels = function(n){format(n, scientific = F)})+
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(colour = "gray0", size = 17),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))+
  scale_x_date(date_breaks = "10 day", date_labels =  "%d %b %Y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_y_continuous(labels = comma, breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000))

grafico_camas <- ggplot(consolidado_final, aes(x = fecha, y = camas_utilizadas_diarias))+
  stat_summary(fum = mean, geom = "bar", size = 0.9, fill = 4, alpha = 0.5)+
  labs(title = "Comparativo ocupación de camas y vacunación diaria",
       y= "Camas utilizadas",
       x = "",
       colour = "")+
  theme_minimal()+
  scale_y_continuous(labels = function(n){format(n, scientific = F)})+
  theme(
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 0),
    legend.position = "top",
    axis.title = element_text(size = 20),
    axis.text = element_text(colour = "gray0", size = 17),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))+
  scale_x_date(date_breaks = "10 day", date_labels =  "%d %b %Y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_color_manual(values=c("#1C9D52", "#FF2D00","#1C799D", "#D711A1"))+
  scale_y_continuous(labels = comma)

gridExtra::grid.arrange(grafico_camas,grafico_vacunacion_diaria, nrow = 2)
rm(grafico_camas, grafico_vacunacion_diaria)

# Relacion uso camas y vacunacion

grafico_vacunados <- ggplot(consolidado_final, aes(x = fecha, y = vacunados_diaros))+
  stat_summary(fun = mean, geom = "bar", size = 0.9, show.legend = T, color = "red", fill = 10)+
  labs(y= "Cantidad",
       x = "",
       fill = "",
       caption = "Fuente: Elaboración propia, a partir de datos del Minsal.")+
  theme_minimal()+
  scale_y_continuous(labels = function(n){format(n, scientific = F)})+
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(colour = "gray0", size = 18),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))+
  scale_x_date(date_breaks = "1 month", date_labels =  " %b %Y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_y_continuous(labels = comma)

grafico_camas <- ggplot(consolidado_final, aes(x = fecha, y = camas_utilizadas_diarias))+
  stat_summary(fun = mean, geom = "area", size = 0.9, show.legend = F, alpha=0.5, fill = 4)+
  geom_area(aes(x = fecha, y = casos_nuevos_totales), alpha = 0.5, fill = 9)+
  theme_minimal()+
  scale_fill_brewer(palette="Dark2")+
  labs(y = "Cantidad",
       x = "",
       title = "Q camas utilizadas, casos y vacunados diarios")+
  scale_x_date(date_breaks = "1 month", date_labels =  " %b %Y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_y_continuous(labels = comma)+
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(colour = "gray0", size = 18),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))

camast <- consolidado_final %>% tail(1) %>% select(camas_utilizadas_diarias)
camast <- mean(camast$camas_utilizadas_diarias)

casosdiarios <- consolidado_final %>% tail(1) %>% select(casos_nuevos_totales)
casosdiarios <- mean(casosdiarios$casos_nuevos_totales)

fechamaxima <- mean(consolidado_final$fecha)

etiquetas <- grafico_camas +
  annotate(geom='text', x=fechamaxima - 47, y=camast - 4500 , label= "Camas UCI",color = "black", size = 7)+
  annotate(geom='text', x=fechamaxima + 160, y=casosdiarios - 1000, label= "Casos diarios" ,color = "93B5B5", size = 7)

gridExtra::grid.arrange(etiquetas,grafico_vacunados,
                        nrow = 2)

rm(grafico_camas, grafico_vacunados, etiquetas,
   camast, casosdiarios, fechamaxima)
