# Librarias a utilizar----

library(janitor)
library(skimr)
library(tidyverse)
library(scales)
library(readxl)
library(ggrepel)
library(modeest)
library(lubridate)

# Importe dataset de múltiples archivo + consolidado en un archivo único + Exportación----

list.files("./dataset1")
archivos <- list.files("./dataset1")
vacunacion_chile <- data.frame()
for(archivo in archivos){
  nombre_archivo <- paste0("./dataset1/",archivo)
  df <- read_excel(nombre_archivo)
  vacunacion_chile <- rbind(vacunacion_chile,df)
}
rm(df, archivo, archivos, nombre_archivo)

# write.csv(vacunacion_chile, file="vacunacion_chile.csv", row.names = F)

#Se descargaron 16 archivos separados con la información del progreso de vacunación proveniente del Minsal, con
#el objetivo de tener los datos consistenes a las cifras que día a día se informan. Para ello se realiza un ciclo For,
#para realizar la lectura y carga del archivo, para finalmente terminar con un consolidado que reuna la información
#de los 16 archvios en 1. Adicionalmente, la última linea de comando realiza una exportación de este archivo.

vacunacion_chile

#skim(vacunacion_chile)
#sapply(vacunacion_chile, class, simplify = T)
#head(vacunacion_chile, n = 3)

#Se requiere cambio en el formato de la fecha de 01/06/2021 a 01-06-2021 y se hace limpieza de los encabezados

## Limpieza ----

# limpieza de nombres y formato fecha
vacunacion_chile_v1 <- vacunacion_chile %>%
  clean_names() %>%
  mutate(fecha_inmunizacion = as.Date(fecha_inmunizacion,
                                      format="%d/%m/%Y"))

rm(vacunacion_chile)

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

#Análisis de tópicos----

##Avance de la vacunación diaria----
grafico_vacunacion_diaria <- ggplot(vacunacion_chile_v1, aes(x = fecha,
                                                             y = cantidad,
                                                             fill = c("#FF2D00")))+
  stat_summary(fun = sum, geom = "col", show.legend = F)+
  labs(title = "Vacunación díaria",
       y= "Vacunas aplicadas",
       x = "",
       fill = "",
       caption = "Fuente: Elaboración propia, a partir de datos del Minsal.")+
  theme_minimal()+
  scale_y_continuous(labels = function(n){format(n, scientific = F)})+
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(colour = "gray0", size = 10),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))+
  scale_x_date(date_breaks = "10 day", date_labels =  "%d %b %Y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_y_continuous(labels = comma, breaks = c(0,
                                                50000,
                                                100000,
                                                150000,
                                                200000,
                                                250000,
                                                300000,
                                                350000,
                                                400000,
                                                450000))


grafico_vacunacion_diaria
# ggsave("Gráfico de vacunación diaria.jpg")
rm(grafico_vacunacion_diaria)

##Avance de las dosis aplicadas a la población----

vacunacion_chile_v2 <- vacunacion_chile_v1 %>% arrange(fecha)

vacunacion_chile_v2$una_dosis <- ifelse(vacunacion_chile_v2$dosis == "1 dosis",
                                        vacunacion_chile_v2$cantidad, 0)
vacunacion_chile_v2$dos_dosis <- ifelse(vacunacion_chile_v2$dosis == "2 dosis",
                                        vacunacion_chile_v2$cantidad, 0)
vacunacion_chile_v2$unica_dosis <- ifelse(vacunacion_chile_v2$dosis == "Unica dosis",
                                          vacunacion_chile_v2$cantidad, 0)

#Ahora sacamos una columna acumulativa para las tres categorias de dosis

acum_1 <- apply(X = vacunacion_chile_v2[,13], MARGIN = 2, FUN = cumsum)
vacunacion_chile_v3 <- cbind(vacunacion_chile_v2,"acumulado_una_dosis"= c(acum_1))

acum_2 <- apply(X = vacunacion_chile_v2[,14], MARGIN = 2, FUN = cumsum)
vacunacion_chile_v4 <- cbind(vacunacion_chile_v3,"acumulado_dos_dosis"= c(acum_2))

acum_u <- apply(X = vacunacion_chile_v2[,15], MARGIN = 2, FUN = cumsum)
vacunacion_chile_v5 <- cbind(vacunacion_chile_v4,"acumulado_unica_dosis"= c(acum_u))

#Se crea la variable acumulativa

vacunacion_chile_v5$acumulado <-
  ifelse(vacunacion_chile_v5$dosis == "1 dosis",
         vacunacion_chile_v5$acumulado_una_dosis,
         ifelse(vacunacion_chile_v5$dosis == "2 dosis",
                vacunacion_chile_v5$acumulado_dos_dosis,
                ifelse(vacunacion_chile_v5$dosis == "Unica dosis",
                       vacunacion_chile_v5$acumulado_unica_dosis,
                       0)))

vacunacion_chile_dosis <- vacunacion_chile_v5 %>% filter(acumulado !=0)

acum_dosis <- ggplot(vacunacion_chile_dosis, aes(x = fecha, y = acumulado, colour = dosis))+
  stat_summary(fun = mean,  geom = "line", size = 0.9)+
  labs(title = "Avance de vacunación en Chile por dosis aplicadas",
       y= "Vacunaciones acumuladas a la fecha",
       x = "",
       colour = "",
       caption = "Fuente: Elaboración propia, a partir de datos del Minsal.")+
  theme_minimal()+
  scale_y_continuous(labels = function(n){format(n, scientific = F)})+
  theme(
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.position = "top",
    axis.title = element_text(size = 20),
    axis.text = element_text(colour = "gray0", size = 18),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))+
  scale_x_date(date_breaks = "10 day", date_labels =  "%d %b %Y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_color_manual(values=c("#EE9700", "#1C9D52","#1C799D"))+
  scale_y_continuous(labels = comma)

una <- max(vacunacion_chile_dosis$acumulado_una_dosis)
dos <- max(vacunacion_chile_dosis$acumulado_dos_dosis)
unica <- max(vacunacion_chile_dosis$acumulado_unica_dosis)

fechamaxd <- max(vacunacion_chile_dosis$fecha)

acum_dosis_1 <- acum_dosis +
  annotate(geom='text',
           x=fechamaxd -5,
           y=una +230000,
           label= paste("12.199.528 - 1 dosis") ,
           color = "#EE9700",
           size = 7)+
  annotate(geom='text',
           x=fechamaxd -5,
           y=dos + 230000,
           label= paste("10.270.251 - 2 dosis") ,
           color = "#1C9D52" ,
           size = 7)+
  annotate(geom='text',
           x=fechamaxd -5,
           y=unica +230000,
           label= paste(" 414.027 Unica dosis") ,
           color = "#1C799D" ,
           size = 7)

acum_dosis_1
# ggsave(Avance_por_dosis.jpg")
rm(una, dos, unica, fechamaxd,
   acum_dosis, acum_dosis_1, acum_u,
   acum_1, acum_2,
   vacunacion_chile_v2, vacunacion_chile_v3,
   vacunacion_chile_v4, vacunacion_chile_v5)

###Estadísticos total vacunados----
#Se realiza una agrupación diaria para poder sacar métricas del tipo, maximo, minimo
diaria <- vacunacion_chile_v1 %>%
  group_by(fecha) %>%
  arrange(fecha) %>%
  summarize(sum_cantidad  = sum(cantidad))

rm(diaria)
##Avance de la cantidad de vacunas aplicadas por región----
resumen_regiones <- vacunacion_chile_v1 %>%
  group_by(region) %>%
  summarize(sum_cantidad = sum(cantidad, na.rm = T))

grafico_resumen_regiones <- ggplot(resumen_regiones, aes(x = sum_cantidad,
                                                 y = reorder(region, sum_cantidad),
                                                 fill = region))+
  stat_summary(fun = sum, geom = "bar", show.legend = F)+
  labs(title = "Vacunas por región",
       y= "",
       x = "Vacunas aplicas",
       fill = "",
       caption = "Fuente: Elaboración propia")+
  theme_minimal()+
  scale_x_continuous(labels = function(n){format(n, scientific = F)})+
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(colour = "gray0"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.25),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))+
  stat_summary(
    aes(label = sum_cantidad),
    fun = "mean",
    geom = "text_repel",
    min.segment.length = 0, # always draw segments
    position = position_nudge_repel(x = 100000))+
  scale_x_continuous(labels = comma)

grafico_resumen_regiones
# ggsave("Resumen por regiones.jpg")
rm(resumen_regiones,
   grafico_resumen_regiones)

##Avance vacunas aplicadas en Chile----
#Se crea un objeto que nos almacene el resumen de la cantidad de vacunados
#de acuerdo a la vacuna aplicada
#Resumen_lab, almacena la información general de la cantidad de vacunados
#Resumen tipo de vacuna + la distribución por %
resumen_lab <- vacunacion_chile_v1 %>%
  group_by(vacuna) %>%
  summarize(sum_cantidad = sum(cantidad, na.rm = T))

total <- sum(resumen_lab$sum_cantidad)

resumen_lab_v1 <- resumen_lab %>%
  mutate(porcentaje_total = round(100*(resumen_lab$sum_cantidad)/total,2))

grafico_prop_vacunas <- ggplot(resumen_lab_v1, aes(x = reorder(vacuna, -porcentaje_total),
                                           y = porcentaje_total,
                                           fill = vacuna))+
  stat_summary(fun = sum, geom = "bar", show.legend = F)+
  labs(title = "Proporción de las vacunas utilizadas en Chile",
       y= "Porcentaje (%)",
       x = "",
       caption = "Fuente: Elaboración propia, a partir de datos del Minsal.")+
  theme_minimal()+
  scale_y_continuous(labels = function(n){format(n, scientific = F)})+
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(colour = "gray0", size = 18),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))+
  scale_fill_manual(values=c("#EE9700",
                             "#1C9D52",
                             "#1C799D",
                             "#FF2D00"))+
  geom_text(aes(label = porcentaje_total), vjust=-0.5, size = 7)

grafico_prop_vacunas
# ggsave("proporcion_vacunas.jpg")
rm(grafico_prop_vacunas,
   resumen_lab_v1,
   total,
   resumen_lab)

#Avance vacunación acumulada por laboratorio----

#Construccion de los 4 acumulados por tipo de vacuna

vacunacion_chile_v2 <- vacunacion_chile_v1 %>% arrange(fecha)

#Se agrega el acumulado pfizer al df
acum_p <- apply(X = vacunacion_chile_v2[,7], MARGIN = 2, FUN = cumsum)
vacunacion_chile_v3 <- cbind(vacunacion_chile_v2,"pfizer_acum"= c(acum_p))

#Se agrega el acumulado sinovac al df
acum_s <- apply(X = vacunacion_chile_v2[,8], MARGIN = 2, FUN = cumsum)
vacunacion_chile_v4 <- cbind(vacunacion_chile_v3,"sinovac_acum"= c(acum_s))

#Se agrega el acumulado astrazeneca al df
acum_a <- apply(X = vacunacion_chile_v2[,9], MARGIN = 2, FUN = cumsum)
vacunacion_chile_v5 <- cbind(vacunacion_chile_v4,"astrazeneca_acum"= c(acum_a))

#Se agrega el acumulado cansino al df
acum_c <- apply(X = vacunacion_chile_v2[,10], MARGIN = 2, FUN = cumsum)
vacunacion_chile_v6 <- cbind(vacunacion_chile_v5,"cansino_acum"= c(acum_c))


#La siguiente linea agrega una variable indicando solamente el nombre de la vacuna aplciada

vacunacion_chile_v7 <- vacunacion_chile_v6
vacunacion_chile_v7$acumulado <-
  ifelse(vacunacion_chile_v7$vacuna == "Pfizer",
         vacunacion_chile_v7$pfizer_acum,
         ifelse(vacunacion_chile_v7$vacuna == "Sinovac",
                vacunacion_chile_v7$sinovac_acum,
                ifelse(vacunacion_chile_v7$vacuna == "AstraZeneca",
                       vacunacion_chile_v7$astrazeneca_acum,
                       ifelse(vacunacion_chile_v7$vacuna == "CanSino",
                              vacunacion_chile_v7$cansino_acum,
                              0))))

vacunacion_chile_lab <- vacunacion_chile_v7

grafico_vacunas_acumuladas <- ggplot(vacunacion_chile_lab, aes(x = fecha, y = acumulado, colour = vacuna))+
  stat_summary( geom = "line", size = 0.9)+
  labs(title = "Vacunación acumulada",
       y= "Vacunas aplicadas",
       x = "",
       colour = "",
       caption = "Fuente: Elaboración propia")+
  theme_minimal()+
  scale_y_continuous(labels = function(n){format(n, scientific = F)})+
  theme(
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.position = "top",
    axis.title = element_text(size = 15),
    axis.text = element_text(colour = "gray0"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))+
  scale_x_date(date_breaks = "10 day", date_labels =  "%d %b %Y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_color_manual(values=c("#EE9700", "#1C9D52","#1C799D","#FF2D00"))+
  scale_y_continuous(labels = comma)


a1 <- max(vacunacion_chile_lab$fecha)
as <- max(vacunacion_chile_lab$sinovac_acum)
ap <- max(vacunacion_chile_lab$pfizer_acum)
aa <- max(vacunacion_chile_lab$astrazeneca_acum)
ac <- max(vacunacion_chile_lab$cansino_acum)

acumuladas_ <- grafico_vacunas_acumuladas+
  annotate(geom='text',
           x=a1 + 12,
           y=as, label= paste(as," Sinovac") ,
           color = "#FF2D00",
           size = 3.5)+
  annotate(geom='text',
           x=a1 + 12,
           y=ap, label= paste(ap," Pfizer") ,
           color = "#1C799D" ,
           size = 3.5)+
  annotate(geom='text',
           x=a1 + 12, y=aa + 625000,
           label= paste(aa,"AstraZeneca") ,
           color = "#EE9700" ,
           size = 3.5)+
  annotate(geom='text',
           x=a1 + 12,
           y=ac - 10000,
           label= paste(ac,"CanSino") ,
           color = "#1C9D52"  ,
           size = 3.5)

acumuladas_
# ggsave("Grafico total vacunados.jpg")

rm(grafico_vacunas_acumuladas, acum_a, acum_c, acum_p, acum_s,
   acumuladas_, a1, aa, ac, ap, as,
   vacunacion_chile_v2, vacunacion_chile_v3,
   vacunacion_chile_v4, vacunacion_chile_v5,
   vacunacion_chile_v6, vacunacion_chile_v7)

##Estadisticas por laboratorio ----
lab_ <- vacunacion_chile_v1 %>%
  group_by(fecha) %>%
  arrange(fecha) %>%
  summarize(sum_vacunap = sum(pfizer),
            sum_vacunas = sum(sinovac),
            sum_vacunaa = sum(astrazeneca),
            sum_vacunac = sum(cansino))

summary(lab_)
rm(lab_)

#Avance diario según vacuna----

grafico_diario_junto <- ggplot(vacunacion_chile_v1, aes(x = fecha, y = cantidad, fill = vacuna))+
  stat_summary(fun = sum, geom = "bar", show.legend = F)+
  facet_grid(vacuna~., scales = "free")+
  theme_minimal()+
  scale_fill_manual(values=c("#EE9700", "#1C9D52","#1C799D","#FF2D00"))+
  scale_y_continuous(labels = comma)+
  scale_x_date(date_breaks = "10 day", date_labels =  "%d %b %Y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  labs(title = "Vacunación diaria",
       y= "Vacunas aplicadas",
       x = "",
       colour = "",
       caption = "Fuente: Elaboración propia")+
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(colour = "gray0"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))

grafico_diario_junto
# ggsave("Grafico_diario_junto.jpg")
rm(grafico_diario_junto)
