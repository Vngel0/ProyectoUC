#  Cargamos las Librerias ----------------------------------------------------------

setwd()

library(pacman)
library(tidyverse)

pacman::p_load(raster, sf, ggplot2, openxlsx,ggspatial, cowplot, ggpubr)

# Cargammos los SHP del Chile ---------------------------------------------------------------
Chile           <- getData('GADM', country='Chile', level=1) %>% st_as_sf()  
Datos_General   <- read.xlsx("Data excel/Data_covid_chile.xlsx", sheet="Hoja1") 
Covid_chile     <- cbind(Chile,Datos_General)

summary(Covid_chile$Casos)
cortes_fallecidos <- c(816229, 15000000, 30000000, 60000000, 90000000, 120000000, 137197605)

A<-ggplot()+
  geom_sf(data=Chile)+
  geom_sf(data = Covid_chile, aes(fill=Casos))+
  scale_fill_distiller(palette   = "Reds", direction = 1,
                       na.value = 'white', breaks = cortes_fallecidos,
                       labels = c("[816229 - 15000000] ","[15000000-30000000]", "[30000000-60000000]", "[60000000-90000000]", 
                                  "[90000000-120000000]","[120000000-137197605]", "[137197605]")) +
  coord_sf(xlim = c(-80,-60), ylim = c(-60,-15),expand = FALSE)+
  theme_void()+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  guides(fill = guide_legend(title.position = "right",direction = "vertical",
                             title.theme = element_text(angle = 90, size = 10, colour = "black"),
                             barheight = .5, barwidth = .95,
                             title.hjust = 0.5, raster = FALSE,
                             title = 'Indi/Depar'))+
  theme(legend.position = c(0.90,0.20))

gg <-ggdraw(A) +
  draw_plot({A + 
      coord_sf(xlim = c(-74,-68), ylim = c(-36 ,-31),expand = FALSE)+
      theme(legend.position = "none")},
      x = 0.68, y = 0.50,width = 0.26,   height = 0.26)

W <-annotate_figure(gg ,top = text_grob("Casos activos de COVID-19 por habitante\n [Casos activos/Por Departamentos]", color = "black", face = "bold", size = 13),
                    bottom = text_grob("Fuente: https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo_std.csv", color = "black",
                                       hjust = 1, x = 1, face = "italic", size = 10),
                    left = text_grob("Mapa de Covid en Chile", color = "black", rot = 90),
                    right = "Gorky Florez Castillo",
                    fig.lab = "Figure 1", fig.lab.face = "bold")

ggsave(plot = W ,"Mapas exportados/Mpa de Covid de Chile.png", units = "cm", 
       width = 21,height = 29, dpi = 900)
#------------------------------------------------------------------------