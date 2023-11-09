
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

#Mapa2023

Mapa2023 <- "PeruRegionesColoreado2023.gpkg"
library(sf)

st_layers(Mapa2023)
Departamentos2023=st_read(Mapa2023, layer = "Departamentos")
class(Departamentos2023$Total_fj5)
Departamentos2023$Total_fj5_cat <- as.factor(Departamentos2023$Total_fj5)
class(Departamentos2023$Total_fj5_cat)

library(ggplot2)
base2023=Departamentos2023

base2023$Total_fj5_cat=NULL

base2023Map=ggplot() + geom_sf(data=base2023, color='grey60',fill='white',lwd = 0.1) + theme_minimal()
choro2=base2023Map + geom_sf(data=Departamentos2023,aes(fill=Total_fj5_cat),lwd = 0.1,show.legend = F) 
choro2=choro2 + facet_wrap(~Total_fj5_cat,ncol = 2)
choro2=choro2 + scale_fill_manual(values = c("green", "blue", "grey","red","black"))
choro2

saveRDS(choro2, file = "choro2023.rds")

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

