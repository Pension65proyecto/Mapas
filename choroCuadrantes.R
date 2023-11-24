rm(list = ls())

#MapaCuadrantes2013

Mapa2013 <- "MapaCuadrantes.gpkg"
library(sf)

st_layers(Mapa2013)
Cuadrantes2013=na.omit(st_read(Mapa2013, layer = "Provincias"))
class(Cuadrantes2013$cuadrante2013)

library(tidyverse)
Cuadrantes2013 <- mutate(Cuadrantes2013, cuadrante2013 = case_when(
  cuadrante2013 == "Alto-Alto" ~ "I.Alto-T.Alta",
  cuadrante2013 == "Alto-Bajo" ~ "I.Alto-T.Baja",
  cuadrante2013 == "Bajo-Bajo" ~ "I.Bajo-T.Baja",
  cuadrante2013 == "Bajo-Alto" ~ "I.Bajo-T.Alta",
  TRUE ~ as.character(cuadrante2013)
))

base2013=Cuadrantes2013

base2013$cuadrante2013=NULL

base2013Map=ggplot() + geom_sf(data=base2013, color='grey60',fill='white',lwd = 0.1) + theme_minimal()
choro1=base2013Map + geom_sf(data=Cuadrantes2013,aes(fill=cuadrante2013),lwd = 0.1,show.legend = F) 
choro1=choro1 + facet_wrap(~cuadrante2013,ncol = 2)
choro1=choro1 + scale_fill_manual(values = c("green", "blue", "grey","red","black"))
choro1

saveRDS(choro1, file = "cuadrantes2013.rds")

#MapaCuadrantes2022

Mapa2022 <- "MapaCuadrantes.gpkg"

st_layers(Mapa2022)
Cuadrantes2022=na.omit(st_read(Mapa2022, layer = "Provincias"))
class(Cuadrantes2022$cuadrante2022)

Cuadrantes2022 <- mutate(Cuadrantes2022, cuadrante2022 = case_when(
  cuadrante2022 == "Alto-Alto" ~ "I.Alto-T.Alta",
  cuadrante2022 == "Alto-Bajo" ~ "I.Alto-T.Baja",
  cuadrante2022 == "Bajo-Bajo" ~ "I.Bajo-T.Baja",
  cuadrante2022 == "Bajo-Alto" ~ "I.Bajo-T.Alta",
  TRUE ~ as.character(cuadrante2022)
))

base2022=Cuadrantes2022

base2022$cuadrante2022=NULL

base2022Map=ggplot() + geom_sf(data=base2022, color='grey60',fill='white',lwd = 0.1) + theme_minimal()
choro2=base2022Map + geom_sf(data=Cuadrantes2022,aes(fill=cuadrante2022),lwd = 0.1,show.legend = F) 
choro2=choro2 + facet_wrap(~cuadrante2022,ncol = 2)
choro2=choro2 + scale_fill_manual(values = c("green", "blue", "grey","red","black"))
choro2

saveRDS(choro2, file = "cuadrantes2022.rds")
