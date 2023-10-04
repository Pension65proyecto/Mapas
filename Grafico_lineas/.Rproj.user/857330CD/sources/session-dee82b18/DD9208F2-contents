library(rio)
install.packages("rio")

pension65 <- import("Beneficiarios.csv")

install.packages("tidyverse")
install.packages("ggthemes")
install.packages("scales")
install.packages("caret")

library(tidyverse)
library(ggthemes)
library(ggplot2)
library(scales)
library(caret)
library(recipes)

install.packages("recipes")

ggplot(pension65, aes(Date))+
  geom_line(aes(y=Monto, colour="Monto"), size = 1)+
  geom_line(aes(y=Beneficiarios, colour="Beneficiarios"), size = 1)+
  scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = seq(2013, 2023, 1))+
  labs(title = "Beneficiarios y monto transferido por Pensión 65 (2013-2023)",
       x='Año',
       y= "Monto/Beneficiarios")+
  theme_clean()

pension65$Montolog <- log(pension65$Monto)
pension65$Beneficiarioslog <- log(pension65$Beneficiarios)

ggplot(pension65, aes(Date))+
  geom_line(aes(y=Montolog, colour="Monto"), size = 1)+
  geom_line(aes(y=Beneficiarioslog, colour="Beneficiarios"), size = 1)+
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(2013, 2023, 1))+
  labs(title = "Beneficiarios y monto transferido por Pensión 65 (2013-2023)",
       x='Año',
       y= "Monto/Beneficiarios")+
  theme_clean()

pension65MinMax <- preProcess(pension65, method=c("range"))
pension65MinMax <- predict(pension65MinMax, as.data.frame(pension65))


ggplot(pension65MinMax1, aes(Date))+
  geom_line(aes(y=Monto, colour="Monto"), size = 1)+
  geom_line(aes(y=Beneficiarios, colour="Beneficiarios"), size = 1)+
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(2013, 2023, 1))+
  labs(title = "Beneficiarios y monto transferido por Pensión 65 (2013-2023)",
       x='Año',
       y= "Monto/Beneficiarios")+
  theme_clean()

install.packages("writexl")
library(writexl)

write_xlsx(pension65MinMax, "pension65MinMax.xlsx")

pension65MinMax1 <- import("file_show.xlsx")


#Alargando df

install.packages("reshape2")
library(reshape2)

pension65MinMax_long <- melt(pension65MinMax1, id="Date")

pension65MinMax2 <- pension65MinMax1
pension65MinMax2 <- pension65MinMax2 %>% 
  select(Date, Monto, Beneficiarios)

pension65MinMax_long <- melt(pension65MinMax2, id = "Date")

ggplot(data=pension65MinMax_long,
       aes(x=Date, y=value, colour=variable), size = 2) +
  geom_line()+
  scale_x_continuous(breaks = seq(2013, 2023, 1))+
  labs(title = "Número de beneficiarios y monto transferido por Pensión 65 (2013-2023)",
       subtitle = "(Escalamiento MinMax)",
       x='Año',
       y= "Monto/Beneficiarios")+
  theme_clean()

pension65_1 <- pension65 %>% 
  select(Date, Monto, Beneficiarios)

pension65_long <- melt(pension65_1, id = "Date")

ggplot(data=pension65_long,
       aes(x=Date, y=value, colour=variable)) +
  geom_line()+
  scale_x_continuous(breaks = seq(2013, 2023, 1))+
  labs(title = "Número de beneficiarios y monto transferido por Pensión 65 (2013-2023)",
       x='Año',
       y= "Monto/Beneficiarios")+
  theme_clean()
