rm(list = ls())
library(rio)
DatosProvincias <- import("DatosProvincias.xlsx")

DatosProvincias <- na.omit(DatosProvincias) 

y_mid <- median(DatosProvincias$Transferencia2022_r)
x_mid <- median(DatosProvincias$ing_tot_2022_r)

library(dplyr)
library(ggplot2)

cuadrantes2022 <- DatosProvincias %>% 
  mutate(quadrant = case_when(ing_tot_2022_r > x_mid & Transferencia2022_r > y_mid   ~ "Q1",
                              ing_tot_2022_r <= x_mid & Transferencia2022_r > y_mid  ~ "Q2",
                              ing_tot_2022_r <= x_mid & Transferencia2022_r <= y_mid ~ "Q3",
                              TRUE                                         ~ "Q4")) %>% 
  ggplot(aes(x = ing_tot_2022_r, y = Transferencia2022_r, color = quadrant)) +
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
  mutate(quadrant = case_when(ingtot_2013_r > x_mid & Transferencia2013_r > y_mid   ~ "Q1",
                              ingtot_2013_r <= x_mid & Transferencia2013_r > y_mid  ~ "Q2",
                              ingtot_2013_r <= x_mid & Transferencia2013_r <= y_mid ~ "Q3",
                              TRUE                                         ~ "Q4")) %>% 
  ggplot(aes(x = ingtot_2013_r, y = Transferencia2013_r, color = quadrant)) +
  geom_vline(xintercept = x_mid) + # plot vertical line
  geom_hline(yintercept = y_mid) + # plot horizontal line
  geom_point()

cuadrantes2013 <- cuadrantes2013 + labs(x="Ingreso promedio de adultos mayores de 65 años",
                                        y="Monto transferido")

cuadrantes2013

saveRDS(cuadrantes2013, "cuadrantes2013.rds")
