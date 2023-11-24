
rm(list = ls())

#Mapa2013

Mapa2013 <- "PeruMapaColoreado2013.gpkg"
library(sf)

st_layers(Mapa2013)
Departamentos2013=st_read(Mapa2013, layer = "Departamentos")
class(Departamentos2013$Total_fj5)
Departamentos2013$Total_fj5_cat <- as.factor(Departamentos2013$Total_fj5)
class(Departamentos2013$Total_fj5_cat)

library(ggplot2)
base2013=Departamentos2013

base2013$Total_fj5_cat=NULL

base2013Map=ggplot() + geom_sf(data=base2013, color='grey60',fill='white',lwd = 0.1) + theme_minimal()
choro1=base2013Map + geom_sf(data=Departamentos2013,aes(fill=Total_fj5_cat),lwd = 0.1,show.legend = F) 
choro1=choro1 + facet_wrap(~Total_fj5_cat,ncol = 2)
choro1=choro1 + scale_fill_manual(values = c("green", "blue", "grey","red","black"))
choro1

saveRDS(choro1, file = "choro2013.rds")


#Mapa2022

Mapa2022 <- "PeruRegionesColoreado2022.gpkg"
library(sf)

st_layers(Mapa2022)
Departamentos2022=st_read(Mapa2022, layer = "Departamentos")
class(Departamentos2022$Total_fj5)
Departamentos2022$Total_fj5_cat <- as.factor(Departamentos2022$Total_fj5)
class(Departamentos2022$Total_fj5_cat)

library(ggplot2)
base2022=Departamentos2022

base2022$Total_fj5_cat=NULL

base2022Map=ggplot() + geom_sf(data=base2022, color='grey60',fill='white',lwd = 0.1) + theme_minimal()
choro3=base2022Map + geom_sf(data=Departamentos2022,aes(fill=Total_fj5_cat),lwd = 0.1,show.legend = F) 
choro3=choro3 + facet_wrap(~Total_fj5_cat,ncol = 2)
choro3=choro3 + scale_fill_manual(values = c("green", "blue", "grey","red","black"))
choro3

saveRDS(choro3, file = "choro2022.rds")

#MapaDif

MapaDif <- "PeruMapaDif.gpkg"
library(sf)

st_layers(MapaDif)
DepartamentosDif=st_read(MapaDif, layer = "Departamentos")
class(DepartamentosDif$Total_fj5)
DepartamentosDif$Total_fj5_cat <- as.factor(DepartamentosDif$Total_fj5)
class(DepartamentosDif$Total_fj5_cat)

library(tidyverse)
DepartamentosDif <- mutate(DepartamentosDif, Total_fj5_cat = case_when(
  Total_fj5_cat == 0 ~ "0 = bajo",
  Total_fj5_cat == 1 ~ "1 = medio",
  Total_fj5_cat == 2 ~ "2 = regular",
  Total_fj5_cat == 3 ~ "3 = alto",
  Total_fj5_cat == 4 ~ "4 = muy alto",
  TRUE ~ as.character(Total_fj5_cat)
))

DepartamentosDif

library(ggplot2)
baseDif=DepartamentosDif

baseDif$Total_fj5_cat=NULL

baseDifMap=ggplot() + geom_sf(data=baseDif, color='grey60',fill='white',lwd = 0.1) + theme_minimal()
choro4=baseDifMap + geom_sf(data=DepartamentosDif,aes(fill=Total_fj5_cat),lwd = 0.1,show.legend = F) 
choro4=choro4 + facet_wrap(~Total_fj5_cat,ncol = 2)
choro4=choro4 + scale_fill_manual(values = c("green", "blue", "grey","red","black"))
choro4

saveRDS(choro4, file = "choroDif.rds")

