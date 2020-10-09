load('./data/rds/tasas_onu.RData')

library(tidyverse)

theme_set(
  theme_minimal(base_family="Helvetica") +
    theme(plot.title=element_text(hjust=.5, margin = margin(10, 0, 0, 0)),
          plot.subtitle=element_text(hjust=.5, margin = margin(0, 0, 20, 0)))
)
# Tasa de actividad por sexo ----

df <- tasa_actividad_sexo %>% 
  mutate(label = if_else(
    year %in% c(1986, 2018, 2050), 
    scales::percent_format(scale=1, accuracy=.1)(tasa), ""
  )) %>% 
  mutate(
    y_pos = if_else(sexo=="Mujeres", tasa + 3.5, tasa - 3)
  )

df %>% 
  ggplot(aes(year, tasa, color=sexo)) + 
  geom_line(size=1.2, aes(linetype=year > 2018)) +
  geom_text(aes(label = label, y = y_pos)) +
  geom_point(data = filter(df, label != "")) +
  geom_text(aes(label=sexo), nudge_y = 0, nudge_x = 10,
            data = filter(tasa_actividad_sexo, year == 2045)) +
  scale_color_manual(values = c("gray30", "#d95f0e")) +
  scale_x_continuous(breaks = seq(1985, 2050, by = 5),
                     expand = expansion(add=c(3, 10))) + 
  scale_y_continuous(labels =scales::percent_format(scale=1)) +
  guides(color = FALSE, linetype=FALSE) +
  labs(x="Año", y="Personas", 
       caption="Fuente: CELADE",
       title = "Tasa de actividad por sexo")

ggsave("./imgs/tasa_actividad_sexo_celade.png", width=8, height=4)


# Tasas de actividad por edad

df <- ta_edad %>% 
  filter(! edad %in% c(
    "70 - 74", 
    "75 - 79", "80 - 84", "85 - 89",
    "90 - 94", "95 - 99", "100 y más /  and over") ) %>% 
  mutate(edad = fct_inorder(edad)) %>% 
  group_by(edad) %>% 
  mutate(max_grp = if_else(year %in% c(1980, 2018, 2048), 
                           scales::percent_format(scale=1, accuracy=.1)(tasa_actividad), ""))

df_2 <- ta_edad %>% 
  filter(edad %in% c(
    "50 - 54", "55 - 59",  
    "60 - 64", "65 - 69") ) %>% 
  mutate(edad = fct_inorder(edad)) %>% 
  group_by(edad) %>% 
  mutate(max_grp = if_else(year %in% c(1980, 2018, 2048), 
                           scales::percent_format(scale=1, accuracy=.1)(tasa_actividad), ""))

df_3 <- ta_edad %>% 
  filter(edad %in% c(
    "15 - 19")) %>% 
  filter(year < 2019) %>% 
  mutate(edad = fct_inorder(edad)) %>% 
  group_by(edad) %>% 
  mutate(max_grp = if_else(year %in% c(1980, 2018), 
                           scales::percent_format(scale=1, accuracy=.1)(tasa_actividad), ""))

df_3 %>% 
  ggplot(aes(year, tasa_actividad)) + 
  geom_line(size = 1.2, color="#d95f0e") + 
  geom_point(color = "#d95f0e",
             size=1.8,
             data = filter(df_3, year %in% c(1980, 2018))) + 
  scale_y_continuous(labels=scales::percent_format(scale = 1)) +
  scale_x_continuous(expand = expansion(add=c(8, 6))) +
  geom_text(aes(label = max_grp), nudge_x=-1, nudge_y=8, size=3) +
  #geom_text(aes(label = min_grp), nudge_x= 1, nudge_y=12, size=3) +
  geom_hline(yintercept=0, color = "gray80") +
  # geom_vline(xintercept = min(df$year), color = "gray80") +
  labs(x="Año", y="", 
       caption="Fuente: CELADE",
       title="Tasa de actividad entre 15 y 19 años") +
  guides(linetype=FALSE) +
  theme(
    plot.title = element_text(margin=margin(10, 0, 20, 0), hjust=0.5)
  )

ggsave("./imgs/tasa_actividad_15_19.png", width=8, height=4)

df %>% 
  ggplot(aes(year, tasa_actividad)) + 
  geom_line(size = 1.2, color="#d95f0e") + 
  geom_point(color = "#d95f0e",
             size=1.8,
             data = filter(df, year %in% c(1980, 2018, 2048))) + 
  scale_y_continuous(labels=scales::percent_format(scale = 1)) +
  scale_x_continuous(expand = expansion(add=c(8, 6))) +
  geom_text(aes(label = max_grp), nudge_x=-1, nudge_y=12, size=3) +
  #geom_text(aes(label = min_grp), nudge_x= 1, nudge_y=12, size=3) +
  geom_hline(yintercept=0, color = "gray80") +
 # geom_vline(xintercept = min(df$year), color = "gray80") +
  facet_wrap(~edad) + 
  labs(x="Año", y="", 
       caption="Fuente: CELADE",
       title="Tasa de actividad por tramos de edad: evolución reciente y proyecciones") +
  guides(linetype=FALSE) +
  theme(
    plot.title = element_text(margin=margin(10, 0, 20, 0), hjust=0.5)
  )

df_2 %>% 
  ggplot(aes(year, tasa_actividad)) + 
  geom_line(aes(linetype= year > 2018), size = 1.2, color="#d95f0e") + 
  geom_point(color = "#d95f0e",
             size=1.8,
             data = filter(df_2, year %in% c(1980, 2018, 2048))) + 
  scale_y_continuous(labels=scales::percent_format(scale = 1)) +
  scale_x_continuous(expand = expansion(add=c(8, 6))) +
  geom_text(aes(label = max_grp), nudge_x=-1, nudge_y=6, size=3) +
  #geom_text(aes(label = min_grp), nudge_x= 1, nudge_y=12, size=3) +
  geom_hline(yintercept=0, color = "gray80") +
  # geom_vline(xintercept = min(df$year), color = "gray80") +
  facet_wrap(~edad, nrow=1) + 
  labs(x="Año", y="", 
       caption="Fuente: CELADE",
       title="Tasa de actividad por tramos de edad: evolución reciente y proyecciones") +
  guides(linetype=FALSE) +
  theme(
    plot.title = element_text(margin=margin(10, 0, 20, 0), hjust=0.5)
  )

ggsave("./imgs/tasa_actividad_edad_celade_tramos_altos.png", width=10)

