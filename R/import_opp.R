library(readxl)
library(tidyverse)



# Mail de Pía:
#       HOMBRE	MUJER	TOTAL
# 2020	952.707,41	805.478,04	1.758.185,45
# 2030	1.005.056,02	832.652,65	1.837.708,67
# 2040	1.022.423,98	828.717,26	1.851.141,24
# 2050	1.020.641,96	812.753,99	1.833.395,94


proyecciones_opp <- tibble::tribble(
  ~year, ~genero, ~poblacion,
  # 2020, "Hombres", 952707,
  # 2020, "Mujeres", 805478,
  2030, "Hombres", 1005056,
  2030, "Mujeres", 832652,
  2040, "Hombres", 1022423,
  2040, "Mujeres", 828717,
  2050, "Hombres", 1020641,
  2050, "Mujeres", 812753)


pea_sexo <- read_xlsx("./data/xls/PEA y TA.xlsx", sheet="PEA", range="B6:D40") %>% 
  rename(year=`año`) %>% 
  pivot_longer(-year, names_to="genero", values_to="poblacion") %>% 
  bind_rows(proyecciones_opp)

tasa_actividad_sexo <- read_xlsx("./data/xls/PEA y TA.xlsx", sheet="Tasa de actividad", range="B5:D39") %>% 
  rename(year=1, Hombres=Hombre, Mujeres=Mujer) %>% 
  pivot_longer(-year, names_to="genero", values_to="tasa")

tasa_actividad_edad <- read_xlsx("./data/xls/PEA y TA.xlsx", sheet="Tasa de actividad", range="G5:P39") %>% 
  rename(year=1) %>% 
  pivot_longer(-year, names_to="edad", values_to="tasa")


save(pea_sexo, tasa_actividad_sexo, tasa_actividad_edad, file="./data/rds/tasas_opp.RData")
