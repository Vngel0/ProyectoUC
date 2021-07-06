#Librarias a utilizar-----------------------------------------------------------------------------------------------
library(janitor)
library(skimr)
library(tidyverse)
library(scales)
library(readxl)
library(ggrepel)
library(modeest)
library(tables)

###Importe dataset de multiples archivo + consolidado en un archivo unico + Exportación-----
#getwd()
#list.files("./dataset")
#archivos <- list.files("./dataset")
#vacunacion_chile <- data.frame()
#for(archivo in archivos){
#  nombre_archivo <- paste0("./dataset/",archivo)
#  df <- read_excel(nombre_archivo)
#  vacunacion_chile <- rbind(vacunacion_chile,df)
#}
#rm(df, archivo, archivos, nombre_archivo)
#
## write.csv(vacunacion_chile, file="vacunacion_chile.csv", row.names = F)


#Se descargaron 16 archivos separados con la información del progreso de vacunación proveniente del Minsal, con
#el objetivo de tener los datos consistenes a las cifras que día a día se informan. Para ello se realiza un ciclo For,
#para realizar la lectura y carga del archivo, para finalmente terminar con un consolidado que reuna la información
#de los 16 archvios en 1. Adicionalmente, la última linea de comando realiza una exportación de este archivo.

load("C:/Users/Angelo/Dropbox/Mi PC (vngel0-PC)/Desktop/Diplomado DS PUC/Proyecto/vacunacion_chile.Rdata")

vacunacion_chile

skim(vacunacion_chile)
sapply(vacunacion_chile, class, simplify = T)

sapply(vacunacion_chile)

?sapply()
#Se requiere cambio en el formato de la fecha de 01/06/2021 a 01-06-2021 y se hace limpieza de los encabezados

vacunacion_chile_v1 <- vacunacion_chile %>%
  clean_names() %>%
  mutate(fecha_inmunizacion = as.Date(fecha_inmunizacion, format="%d/%m/%Y"))

#Se requiere cambio en los nombres de los encabezados
names(vacunacion_chile_v1)
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


#La siguiente linea agrega una variable indicando solamente el nombre de la vacuna aplciada

vacunacion_chile_v1$vacuna <- ifelse(vacunacion_chile_v1$laboratorio == "Campaña SARS-CoV-2 (AstraZeneca)", "AstraZeneca",
                                     ifelse(vacunacion_chile_v1$laboratorio == "Campaña SARS-CoV-2 (CanSino)", "CanSino",
                                            ifelse(vacunacion_chile_v1$laboratorio == "Campaña SARS-CoV-2 (Pfizer)", "Pfizer",
                                                   ifelse(vacunacion_chile_v1$laboratorio == "Campaña SARS-CoV-2 (Sinovac)", "Sinovac",
                                                          0))))

#Análisis al dataset----

resumen_dias <- vacunacion_chile_v1 %>% select(fecha, vacuna)

##Avance de la vacunación diaria----

grafico_vacunacion_diaria <- ggplot(vacunacion_chile_v1, aes(x = fecha, y = cantidad, fill = dosis))+
  stat_summary(fun = sum, geom = "bar")+
  labs(title = "Vacunación díaria",
       y= "Vacunas aplicadas",
       x = "",
       fill = "",
       caption = "Fuente: Elaboración propia.")+
  theme_minimal()+
  scale_y_continuous(labels = function(n){format(n, scientific = F)})+
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 0),
    legend.position = "top",
    axis.title = element_text(size = 15),
    axis.text = element_text(colour = "gray0"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))+
  scale_x_date(date_breaks = "10 day", date_labels =  "%d %b %Y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values=c("#FF2D00", "#1C799D","#1C9D52"))

grafico_vacunacion_diaria +
  geom_vline(aes(xintercept = date), data = who_events, linetype = "dashed")+
  geom_text(aes(date, label = event), data = who_events, y = 1e5)

# who_events <- tribble(
#  ~ date, ~ event,
#  ,paste(max(vacunacion_chile_v7$pfizer), "Maximo Pfizer"),
#  ,max(vacunacion_chile_v7$sinovac), "Maximo Sinovac",
#  ,max(vacunacion_chile_v7$astrazeneca), "Maximo Astrazeneca",
#  ,max(vacunacion_chile_v7$cansino), "Maximo Cansino") %>%
#  mutate(date = as.Date(date))

# who_events# who_events

ggsave("Grafico de vacunación diaria.jpg")

#Se realiza una agrupación diaria para poder sacar métricas del tipo, maximo, minimo
diaria <- vacunacion_chile_v1 %>%
  group_by(fecha) %>%
  arrange(fecha) %>%
  summarize(sum_cantidad  = sum(cantidad))

#El dia que más vacunas se aplicaron fue:
maximo <- diaria %>%
  filter(sum_cantidad == as.numeric(apply(diaria, 2, max))[2])

print(table(maximo))
#fecha: "2021-03-15"
#sum_cantidad: 430.092

#El día que menos vacunas se aplicaron
minimo <- diaria %>%
  filter(sum_cantidad == as.numeric(apply(diaria, 2, min))[2])
print(minimo)
# print(paste("El día que menos vacunas se aplicaron fue: ",minimo))
#fecha 2021-01-30 (Día sabado)
#sum_cantidad: 1

# summary
print(summary(vacunacion_chile$CANTIDAD,
              na.rm = T))

# promedio por día
print(paste("El promedio de vacunas por día es: ",
            round(mean(vacunacion_chile$CANTIDAD, digits = 2,
                       na.rm = T))))

# mediana por día
print(paste("La mediana de vacunas por día es: ",
            median(vacunacion_chile$CANTIDAD,
                   na.rm = T)))

# moda por día
print(paste("La moda de vacunas por día es: ",
            mfv(vacunacion_chile$CANTIDAD,
                na.rm = TRUE)))

# Q1
print(paste("Descripcion de frecuencia por día mediante cuartiles Q1 es: ",
            quantile(vacunacion_chile$CANTIDAD,
                     prob = 0.25,
                     na.rm = TRUE)))
# Q2
print(paste("Descripcion de frecuencia por día mediante cuartiles Q2 es: ",
            quantile(vacunacion_chile$CANTIDAD,
                     prob = 0.50,
                     na.rm = TRUE)))
# Q3
print(paste("Descripcion de frecuencia por día mediante cuartiles Q3 es: ",
            quantile(vacunacion_chile$CANTIDAD,
                     prob = 0.75,
                     na.rm = TRUE)))

# varianza
print(paste("Varianza es: ",
            round(var(vacunacion_chile$CANTIDAD,
                      na.rm = TRUE), digits = 2)))

# desviacion estandar
print(paste("Desviación estandar es: ",
            round(sd(vacunacion_chile$CANTIDAD,
                     na.rm = TRUE), digits = 2)))

# Rango de cantidad de vacunados por día
print(paste("Rango de cantidad de vacunados por día: ",
            round(range(vacunacion_chile$CANTIDAD,
                     na.rm = TRUE), digits = 2)))

# La distancia entre el tercer y primer cuantil es
print(paste("La distancia entre el tercer y primer cuantil es: ",
            round(IQR(vacunacion_chile$CANTIDAD,
                        na.rm = TRUE), digits = 2)))

# Coeficiente de variación
print(paste("Coeficiente de variación: ",
  round
  (sd(vacunacion_chile$CANTIDAD,
          na.rm = TRUE)
       /mean(vacunacion_chile$CANTIDAD,
             na.rm = T),
  digits = 2)))

tabular(minimo)

##Resumen de las vacunas aplicadas----

#Se crea un objeto que nos almacene el resumen de la cantidad de vacunados de acuerdo a la bacuna aplicada

#Resumen_lab, almacena la información general de la cantidad de vacunados por tipo de vacuna + la distribución por %

resumen_lab <- vacunacion_chile_v1 %>%
  group_by(vacuna) %>%
  summarize(sum_cantidad = sum(cantidad, na.rm = T))


total <- sum(resumen_lab$sum_cantidad)

resumen_lab_v1 <- resumen_lab %>%
  mutate(porcentaje_total = round(100*(resumen_lab$sum_cantidad)/total,2))



#vacuna      sum_cantidad porcentaje_total
#<chr>              <dbl>            <dbl>
#1 AstraZeneca       359717             1.6
#2 CanSino           357374             1.59
#3 Pfizer           4386382            19.5
#4 Sinovac         17356507            77.3

#Sinovac es la vacuna más utlizada con un 77%, seguido por pfizer con 20%

#Gráfico que representa el % de uso de la vacuna sobre el total de la población de Chile


prop_vacunas <- ggplot(resumen_lab_v1, aes(x = reorder(vacuna, -porcentaje_total), y = porcentaje_total, fill = vacuna))+
  stat_summary(fun = sum, geom = "bar", show.legend = F)+
  labs(title = "Proporción de las vacunas utilizadas en Chile",
       y= "Porcentaje (%)",
       x = "",
       caption = "Fuente: Ministerio de Salud de Chile")+
  theme_minimal()+
  scale_y_continuous(labels = function(n){format(n, scientific = F)})+
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(colour = "gray0", size = 10),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))+
  scale_fill_manual(values=c("#EE9700", "#1C9D52","#1C799D","#FF2D00"))+
  geom_text(aes(label = porcentaje_total), vjust=-1)
ggsave("proporcion_vacunas.jpg")



##Resumen de la cantidad de vacunas aplicadas por comuna----

resumen_regiones <- vacunacion_chile_v1 %>%
  group_by(region) %>%
  summarize(sum_cantidad = sum(cantidad, na.rm = T))

# resumen_regiones <- resumen_regiones[order(resumen_regiones$sum_cantidad, decreasing = T),]

resumen_regiones <- ggplot(resumen_regiones, aes(x = sum_cantidad, y = reorder(region, sum_cantidad), fill = region))+
  stat_summary(fun = sum, geom = "bar", show.legend = F)+
  labs(title = "Vacunas por región",
       y= "",
       x = "Vacunas aplicas",
       fill = "",
       caption = "Fuente: Ministerio de Salud de Chile")+
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
    position = position_nudge_repel(x = 100000)
  )

ggsave("Resumen por regiones.jpg")

##Resumen de las dosis aplicadas a la población----

resumen_dosis <- vacunacion_chile_v1 %>%
  group_by(dosis) %>%
  summarize(sum_cantidad = sum(cantidad))

grafico_dosis <- ggplot(resumen_dosis, aes(x = dosis, y = sum_cantidad, fill = dosis))+
  stat_summary(fun = sum, geom = "bar", show.legend = F)+
  labs(title = "Número de dosis aplicadas a la población",
       y= "Cantidad",
       x = "",
       fill = "",
       caption = "Fuente: Ministerio de Salud de Chile")+
  theme_minimal()+
  scale_y_continuous(labels = comma)+
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(colour = "gray0"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.background = element_rect(colour = NA,linetype = "dashed"),
    strip.text = element_text(colour = "gray0", hjust = 1))+
  scale_fill_manual(values=c("#FF2D00", "#1C799D","#1C9D52"))+
  geom_text(aes(label = sum_cantidad), vjust=-1)

ggsave("Grafico cantidad dosis.jpg")

##Comparativo vacunas----

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
  ifelse(vacunacion_chile_v7$vacuna == "Pfizer", vacunacion_chile_v7$pfizer_acum,
         ifelse(vacunacion_chile_v7$vacuna == "Sinovac", vacunacion_chile_v7$sinovac_acum,
                ifelse(vacunacion_chile_v7$vacuna == "AstraZeneca", vacunacion_chile_v7$astrazeneca_acum,
                       ifelse(vacunacion_chile_v7$vacuna == "CanSino", vacunacion_chile_v7$cansino_acum,
                              0))))


#Se eliminan las tablas intermedias

rm(vacunacion_chile_v1,
   vacunacion_chile_v2,
   vacunacion_chile_v3,
   vacunacion_chile_v4,
   vacunacion_chile_v5,
   vacunacion_chile_v6)

grafico_vacunas_acumuladas<- ggplot(vacunacion_chile_v7, aes(x = fecha, y = acumulado, colour = vacuna))+
  stat_summary( geom = "line", size = 0.9)+
  labs(title = "Vacunación acumulada",
       y= "Vacunas aplicadas",
       x = "",
       colour = "",
       caption = "Fuente: Ministerio de Salud de Chile")+
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
  scale_color_manual(values=c("#EE9700", "#1C9D52","#1C799D","#FF2D00"))


a1 <- max(vacunacion_chile_v7$fecha)
as <- max(vacunacion_chile_v7$sinovac_acum)
ap <- max(vacunacion_chile_v7$pfizer_acum)
aa <- max(vacunacion_chile_v7$astrazeneca_acum)
ac <- max(vacunacion_chile_v7$cansino_acum)

acumuladas_ <- grafico_vacunas_acumuladas+
  annotate(geom='text', x=a1 + 12, y=as, label= paste(as," de vacunas") ,color = "#FF2D00", size = 3.5)+
  annotate(geom='text', x=a1 + 12, y=ap, label= paste(ap," de vacunas") ,color = "#1C799D" ,  size = 3.5)+
  annotate(geom='text', x=a1 + 12, y=aa + 625000, label= paste(aa," de vacunas") ,color = "#EE9700" , size = 3.5)+
  annotate(geom='text', x=a1 + 12, y=ac - 10000, label= paste(ac," de vacunas") ,color = "#1C9D52"  ,  size = 3.5)

ggsave("Grafico total vacunados.jpg")

#Gráfico que representa el avance a traves del tiempo de la vacunación, segmentado por vacuna aplicada (diaria)

ggplot(vacunacion_chile_v1, aes(x = fecha, y = cantidad, fill = vacuna))+
  stat_summary(fun = sum, geom = "bar", show.legend = F)+
  facet_grid(vacuna~., scales = "free")+
  theme_minimal()+
  scale_fill_manual(values=c("#EE9700", "#1C9D52","#1C799D","#FF2D00"))
