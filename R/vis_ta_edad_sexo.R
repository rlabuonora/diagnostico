load('./data/rds/tasas_onu.RData')

theme_set(
  theme_minimal(base_family="Helvetica") +
    theme(
      legend.position = "bottom",
      plot.title=element_text(hjust=.5, margin = margin(10, 0, 0, 0)),
      plot.subtitle=element_text(hjust=.5, margin = margin(0, 0, 20, 0)))
)


df <- ta_edad_sexo %>% 
  mutate(edad = fct_inorder(edad)) %>% 
  group_by(edad) %>% 
  mutate(max_grp = if_else(year %in% c(1980, 2018, 2048), 
                           scales::percent_format(scale=1, accuracy=.1)(tasa_actividad), "")) %>% 
  filter(! edad %in% c(
     "70 - 74", 
    "75 - 79", "80 - 84", "85 - 89",
    "90 - 94", "95 - 99", "100 y más /  and over") ) %>% 
  filter(year < 2020) %>% 
  mutate(y_pos = if_else(sexo == "Mujeres", tasa_actividad - 10, tasa_actividad + 10))


# Menos grupos de edad
df_2 <- ta_edad_sexo %>% 
  filter(edad %in% c(
    "50 - 54", "55 - 59",  
    "60 - 64", "65 - 69") ) %>% 
  group_by(edad) %>% 
  mutate(max_grp = if_else(year %in% c(1980, 2018, 2048), 
                           scales::percent_format(scale=1, accuracy=.1)(tasa_actividad), "")) %>% 

  filter(year < 2020) %>% 
  mutate(y_pos = if_else(sexo == "Mujeres", tasa_actividad - 10, tasa_actividad + 10)) %>% 
  mutate(edad = fct_inorder(edad))

df_2 %>% 
  ggplot(aes(year, tasa_actividad, color=sexo)) + 
  geom_hline(yintercept=0, color = "gray80") +
  geom_line(size = 1.2) + 
  geom_point(size=1.8,
             data = filter(df_2, year %in% c(1980, 2018))) + 
  scale_y_continuous(labels=scales::percent_format(scale = 1)) +
  scale_x_continuous(expand = expansion(add=c(8, 6))) +
  scale_color_manual("", values=c("#377eb8", "#e41a1c")) + 
  geom_text(aes(label = max_grp, y=y_pos), nudge_x=-1, size=3) +
  facet_wrap(~edad, nrow=1) + 
  labs(x="Año", y="", 
       caption="Fuente: CELADE",
       title="Tasa de actividad por tramos de edad y sexo") +
  #  guides(color=FALSE) +
  theme(
    plot.title = element_text(margin=margin(10, 0, 20, 0), hjust=0.5)
  ) +
  

ggsave("./imgs/tasa_actividad_edad_sexo_celade_tramos_altos.png", width=10)


df %>% 
  ggplot(aes(year, tasa_actividad, color=sexo)) + 
  geom_line(size = 1.2) + 
  geom_point(size=1.8,
             data = filter(df, year %in% c(1980, 2018))) + 
  scale_y_continuous(labels=scales::percent_format(scale = 1)) +
  scale_x_continuous(expand = expansion(add=c(8, 6))) +
  scale_color_manual("", values=c("#377eb8", "#e41a1c")) + 
  geom_text(aes(label = max_grp, y=y_pos), nudge_x=-1, size=3) +
  geom_hline(yintercept=0, color = "gray80") +
  # geom_vline(xintercept = min(df$year), color = "gray80") +
  facet_wrap(~edad) + 
  labs(x="Año", y="", 
       caption="Fuente: CELADE",
       title="Tasa de actividad por tramos de edad y sexo") +
#  guides(color=FALSE) +
  theme(
    plot.title = element_text(margin=margin(10, 0, 20, 0), hjust=0.5)
  )


ggsave("./imgs/tasa_actividad_edad_sexo_celade.png", width=12, height=8)
