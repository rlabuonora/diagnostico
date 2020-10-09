library(tidyverse)
library(readxl)
library(tidyxl)
library(unpivotr)
library(cellranger)



# Tasa de actividad por sexo ---

cells <- xlsx_cells("./data/xls/PEA 2100.xlsx", sheet="Indicadores PEA")

header_row_1 <- dplyr::filter(cells, row == 8, col >= letter_to_num("H")) %>%
  dplyr::select(row, col, year = numeric)

header_col_1 <- dplyr::filter(cells, col == 1, row %in% 19:20) %>%
  dplyr::select(row, col, sexo = character)

datos <- dplyr::filter(cells, row %in% 19:20) %>%
  dplyr::select(row, col, tasa = numeric)

tasa_actividad_sexo <- datos %>%
  enhead(header_row_1, "up") %>%
  enhead(header_col_1, "left") %>% 
  select(-row, -col) %>% 
  mutate(sexo = str_extract(sexo, "Hombres|Mujeres"))


# Tasa de actividad por edad ---


# PEA por sexo ---

cells <- xlsx_cells("./data/xls/PEA 2100.xlsx", sheet="Poblacion econo activa")


header_row_1 <- dplyr::filter(cells, row == 8, col >= letter_to_num("H")) %>%
  dplyr::select(row, col, year = numeric)

header_col_1 <- dplyr::filter(cells, col == 1, row %in% c(12:29)) %>%
  dplyr::select(row, col, edad = character)

datos <- dplyr::filter(cells, row %in% c(12:29), col >= letter_to_num("H")) %>%
  dplyr::select(row, col, tasa_actividad = numeric)

ta_edad <- datos %>%
  enhead(header_row_1,  "N") %>%
  enhead(header_col_1, "W") %>% 
  select(-row, -col)

# TA edad y sexo

header_row_1 <- dplyr::filter(cells, row == 8, 
                              col >= letter_to_num("H")) %>%
  dplyr::select(row, col, year = numeric)

header_col_1 <- dplyr::filter(cells, col == 1, row %in% c(33:50, 54:71)) %>%
  dplyr::select(row, col, edad = character)

header_col_2 <- dplyr::filter(cells, col == 1, row %in% c(31, 52)) %>%
  dplyr::select(row, col, sexo = character)

datos <- dplyr::filter(cells, row %in% c(33:50, 54:71), 
                       col >= letter_to_num("H")) %>%
  dplyr::select(row, col, tasa_actividad = numeric)

ta_edad_sexo <- datos %>%
  enhead(header_row_1, "up") %>%
  enhead(header_col_1, "left") %>% 
  enhead(header_col_2, "up-left") %>% 
  select(-row, -col) %>% 
  mutate(sexo = str_extract(sexo, "Hombres|Mujeres"))

save(ta_edad_sexo, ta_edad, tasa_actividad_sexo, file="./data/rds/tasas_onu.RData")
