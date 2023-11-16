rm(list = ls())
library(rio)
DatosProvincias <- import("DatosProvincias.xlsx")

DatosProvincias <- na.omit(DatosProvincias) 

y_mid <- median(DatosProvincias$Transferencia2022_r)
x_mid <- median(DatosProvincias$ing_tot_2022_r)

library(dplyr)
library(ggplot2)

cuadrantes2022 <- DatosProvincias %>% 
  mutate(cuadrante = case_when(ing_tot_2022_r > x_mid & Transferencia2022_r > y_mid   ~ "Alto-Alto",
                              ing_tot_2022_r <= x_mid & Transferencia2022_r > y_mid  ~ "Bajo-Alto",
                              ing_tot_2022_r <= x_mid & Transferencia2022_r <= y_mid ~ "Bajo-Bajo",
                              TRUE                                         ~ "Alto-Bajo")) %>% 
  ggplot(aes(x = ing_tot_2022_r, y = Transferencia2022_r, color = cuadrante)) +
  geom_vline(xintercept = x_mid) + # plot vertical line
  geom_hline(yintercept = y_mid) + # plot horizontal line
  geom_point()

cuadrantes2022 <- cuadrantes2022 + labs(x="Ingreso promedio de adultos mayores de 65 años",
                                        y="Monto transferido")

cuadrantes2022

saveRDS(cuadrantes2022, "cuadrantes2022.rds")

y_mid1 <- median(DatosProvincias$Transferencia2013_r)
x_mid2 <- median(DatosProvincias$ingtot_2013_r)

cuadrantes2013 <- DatosProvincias %>% 
  mutate(cuadrante = case_when(ingtot_2013_r > x_mid & Transferencia2013_r > y_mid   ~ "Alto-Alto",
                              ingtot_2013_r <= x_mid & Transferencia2013_r > y_mid  ~ "Bajo-Alto",
                              ingtot_2013_r <= x_mid & Transferencia2013_r <= y_mid ~ "Bajo-Bajo",
                              TRUE                                         ~ "Alto-Bajo")) %>% 
  ggplot(aes(x = ingtot_2013_r, y = Transferencia2013_r, color = cuadrante)) +
  geom_vline(xintercept = x_mid) + # plot vertical line
  geom_hline(yintercept = y_mid) + # plot horizontal line
  geom_point()

cuadrantes2013 <- cuadrantes2013 + labs(x="Ingreso promedio de adultos mayores de 65 años",
                                        y="Monto transferido")

cuadrantes2013

saveRDS(cuadrantes2013, "cuadrantes2013.rds")

#Creamos variables

##2022
DatosProvincias <- DatosProvincias %>% 
  mutate(cuadrante = case_when(ing_tot_2022_r > x_mid & Transferencia2022_r > y_mid   ~ "Alto-Alto",
                               ing_tot_2022_r <= x_mid & Transferencia2022_r > y_mid  ~ "Bajo-Alto",
                               ing_tot_2022_r <= x_mid & Transferencia2022_r <= y_mid ~ "Bajo-Bajo",
                               TRUE                                         ~ "Alto-Bajo"))
DatosProvincias <- DatosProvincias %>% 
  rename(cuadrante2022 = cuadrante)

##2013
DatosProvincias <- DatosProvincias %>% 
  mutate(cuadrante2013 = case_when(ingtot_2013_r > x_mid & Transferencia2013_r > y_mid   ~ "Alto-Alto",
                               ingtot_2013_r <= x_mid & Transferencia2013_r > y_mid  ~ "Bajo-Alto",
                               ingtot_2013_r <= x_mid & Transferencia2013_r <= y_mid ~ "Bajo-Bajo",
                               TRUE                                         ~ "Alto-Bajo"))

library(writexl)
write_xlsx(DatosProvincias, "DatosProvinciasv2.xlsx")
