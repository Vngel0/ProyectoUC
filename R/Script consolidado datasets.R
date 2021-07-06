
#Script para leer multiples documentos y dejarlos consolidados, con una exportación en csv.
#Los archivos que se consolidan son los datos del avance de vacunación, por cada región

#Los datos se deben actualizar a partir del siguiente link

#https://informesdeis.minsal.cl/SASVisualAnalytics/?reportUri=%2Freports%2Freports%2F9037e283-1278-422c-84c4-16e42a7026c8&sectionIndex=0&sso_guest=true&reportViewOnly=true&reportContextBar=false&sas-welcome=false

library(readxl)

list.files("./dataset")
archivos <- list.files("./dataset")
datos <- data.frame()
for(archivo in archivos){
  nombre_archivo <- paste0("./dataset/",archivo)
  df <- read_excel(nombre_archivo)
  datos <- rbind(datos,df)
}
rm(df, archivo, archivos, nombre_archivo)

datos

write.csv(datos, file="dataset_consolidado2.csv", row.names = F)

