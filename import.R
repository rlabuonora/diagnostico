library(readxl)
library(tidyverse)


pea_sexo <- read_xlsx("PEA y TA.xlsx", sheet="PEA", range="B6:D40")

tasa_actividad_sexo <- read_xlsx("PEA y TA.xlsx", sheet="Tasa de actividad", range="B5:D39") %>% 
  rename(year=1)

tasa_actividad_edad <- read_xlsx("PEA y TA.xlsx", sheet="Tasa de actividad", range="G5:P40") %>% 
  rename(year=1) %>% 
  pivot_longer(-year, names_to="edad", values_to="tasa")
