library(rio)
Datos <- import("Datos.xls")
library(tidyverse)
library(ggrepel)
library(scales)
library(ggthemes)

#Scatter2013
regiones2013Mayor=Datos %>%  top_n(1, Transferencia2013)
regiones2013Menor=Datos %>% top_n(-1, Transferencia2013)
MayoryMenor2013=c(regiones2013Mayor$Departamento,regiones2013Menor$Departamento)

Datos$label=ifelse(Datos$Departamento%in%MayoryMenor2013,Datos$Departamento,"")

baseScatter2013=ggplot(data=Datos,aes(x=ingreso2013,
                                      y=Transferencia2013)) 

scatter2013=baseScatter2013+geom_point(color='grey60',shape = 21)

scatter2013=scatter2013 + 
  geom_text_repel(aes(label=label,size=Transferencia2013),min.segment.length = 0.01)

scatter2013=scatter2013+
  scale_y_continuous(labels = comma)

scatter2013=scatter2013+
  theme_linedraw()

scatter2013=scatter2013+theme(legend.position = "none")

scatter2013=scatter2013+
  labs(x="Ingreso promedio de adultos mayores de 65 años",
       y="Monto transferido")

scatter2013

saveRDS(scatter2013, file = "scatter2013.rds")


#Scatter2022
regiones2022Mayor=Datos %>%  top_n(1, Transferencia2022)
regiones2022Menor=Datos %>% top_n(-1, Transferencia2022)
MayoryMenor2022=c(regiones2022Mayor$Departamento,regiones2022Menor$Departamento)

Datos$label2022=ifelse(Datos$Departamento%in%MayoryMenor2022,Datos$Departamento,"")

baseScatter2022=ggplot(data=Datos,aes(x=ingreso2022,
                                      y=Transferencia2022)) 

scatter2022=baseScatter2022+geom_point(color='grey60',shape = 21)

scatter2022=scatter2022 + 
  geom_text_repel(aes(label=label2022,size=Transferencia2022),min.segment.length = 0.01)

scatter2022=scatter2022+
  scale_y_continuous(labels = comma)

scatter2022=scatter2022+
  theme_linedraw()

scatter2022=scatter2022+theme(legend.position = "none")

scatter2022=scatter2022+
  labs(x="Ingreso promedio de adultos mayores de 65 años",
       y="Monto transferido")

scatter2022

saveRDS(scatter2022, file = "scatter2022.rds")
