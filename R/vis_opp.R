library(tidyverse)
library(extrafont)

theme_set(
  theme_minimal(base_family="Helvetica") +
    theme(plot.title=element_text(hjust=.5, margin = margin(10, 0, 30, 0)))
  )


options(scipen = 999)

load("./data/rds/tasas_opp.RData")


pea_sexo %>% 
  ggplot(aes(year, poblacion, color=genero)) + 
  geom_line(size=1.2, aes(linetype = year > 2018)) +
  geom_text(aes(label=genero),
            nudge_y = -4e4, data = filter(pea_sexo, year == max(year))) +
  geom_point(data = filter(pea_sexo, year %in% c(2018, 2030, 2040, 2050))) +
  geom_text(aes(label = scales::number_format(big.mark=".")(poblacion)), 
            nudge_y = 4e4,
            data = filter(pea_sexo, year %in% c(2018, 2050))) +
  scale_color_manual(values = c("gray30", "#d95f0e")) +
  scale_x_continuous(breaks = seq(1985, 2050, by = 5),
                     expand = expansion(add=c(0, 10))) + 
  scale_y_continuous(labels =scales::number_format(big.mark = ".")) +
  guides(color = FALSE, linetype=FALSE) +
  labs(x="Año", y="Personas", 
       caption= "Fuente: OPP",
       title = "Población económicamente activa por sexo")

ggsave("./imgs/pea_sexo_opp.png", width=10)

pea_sexo %>% 
  filter(year <= 2018) %>% 
  ggplot(aes(year, poblacion, color=genero)) + 
  geom_line(size=1.2) +
  geom_text(aes(label=genero),
            nudge_x = 2.5, data = filter(pea_sexo, year == 2018)) +
  geom_point(data = filter(pea_sexo, year %in% c(2018))) +
  geom_text(aes(label = scales::number_format(big.mark=".")(poblacion)), 
            nudge_y = 4e4,
            data = filter(pea_sexo, year %in% c(2018))) +
  scale_color_manual(values = c("gray30", "#d95f0e")) +
  scale_x_continuous(breaks = seq(1985, 2018, by = 5),
                     expand = expansion(add=c(0, 10))) + 
  scale_y_continuous(labels =scales::number_format(big.mark = ".")) +
  guides(color = FALSE, linetype=FALSE) +
  labs(x="Año", y="Personas", 
       caption= "Fuente: OPP",
       title = "Población económicamente activa por sexo")

ggsave("./imgs/pea_sexo_opp_sin_proy.png", width=10)

df <- tasa_actividad_edad %>% 
  mutate(edad = fct_inorder(edad)) %>% 
  group_by(edad) %>% 
  mutate(max_grp = if_else(year == max(year), 
                           scales::percent_format(scale=1)(round(tasa)), ""),
         min_grp = if_else(year == min(year), 
                           scales::percent_format(scale=1)(round(tasa)), "")) %>% 
  filter(! edad %in% c("Entre 70 y 79", "Entre 80 y 89", "90 o más") ) 

df %>% 
  ggplot(aes(year, tasa)) + 
  geom_line(size = 1.2, color="#d95f0e") + 
  geom_point(color = "#d95f0e",
             size=1.8,
             data = filter(df, year == max(df$year) | year == min(df$year))) + 
  scale_y_continuous(labels=scales::percent_format(scale = 1)) +
  geom_text(aes(label = max_grp), nudge_x=-1, nudge_y=12, size=3) +
  geom_text(aes(label = min_grp), nudge_x=1, nudge_y=12, size=3) +
  facet_wrap(~edad) + 
  labs(x="Año", y="Tasa de actividad", 
       title="Evolución de la tasa de actividad por tramos de edad") +
  theme(
    plot.title = element_text(margin=margin(10, 0, 20, 0), hjust=0.5)
  )



#ggsave("./imgs/tasa_actividad_edad_opp.png", width=10)

df <- tasa_actividad_sexo %>% 
  mutate(label = if_else(
    year == max(tasa_actividad_sexo$year) |
      year == min(tasa_actividad_sexo$year), 
    scales::percent_format(scale=1, accuracy=.1)(tasa), ""
  )) %>% 
  mutate(
    y_pos = if_else(genero=="Mujeres", tasa + 3.5, tasa - 2)
  )

df %>% 
  ggplot(aes(year, tasa, color=genero)) + 
  geom_line(size=1.2) +
  geom_text(aes(label = label, y = y_pos)) +
  geom_point(data = filter(df, label != "")) +
  geom_text(aes(label=genero), nudge_x = 3, data = filter(tasa_actividad_sexo, year == max(year))) +
  scale_color_manual(values = c("gray30", "#d95f0e")) +
  scale_x_continuous(breaks = seq(1985, 2020, by = 5),
                     expand = expansion(add=c(3, 10))) + 
  scale_y_continuous(labels =scales::percent_format(scale=1)) +
  guides(color = FALSE) +
  labs(x="Año", y="Personas", title = "Tasa de actividad por sexo")

#ggsave("./imgs/tasa_actividad_sexo_opp.png", width=10)

