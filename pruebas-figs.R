## Gráficos con Logo

plot_con_logo <- agregar_logo(
  plot_path = "figs/tasamun.png", 
  logo_path = "logo/logo.png", 
  posicion_logo = "superior derecha", 
  logo_scale = 15
)

magick::image_write(agregar_logo(
  plot_path = "figs/tasamun.png", 
  logo_path = "logo/logo.png", 
  posicion_logo = "superior derecha", 
  logo_scale = 15), 
  paste0(Sys.Date(), ggsave("figs/tasamun.png", width = 30, height = 20, units = "cm")))

## Gráficos animados

# transition_time(fecha_confirmacion) +  ease_aes('linear') + 
  
casospoblmun %>% filter(`Tasa.10^5Hab` > 15) %>% 
  mutate(municipio = reorder(municipio, `Tasa.10^5Hab`)) %>% 
  ggplot(aes(x = `Tasa.10^5Hab`, xend = 0,
             y = municipio, yend = municipio,
             color = municipio)) +
  geom_point(show.legend = F) +
  geom_segment(show.legend = F) +
  geom_text(aes(label = `Tasa.10^5Hab`), 
            show.legend = F, 
            hjust = -0.5,
            family = "URWGeometricW03-Light") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  transition_time(casos$fecha) +  ease_aes('linear') +
  labs(x = "Tasa por 10^5 Habitantes", y = "",
       title = "Tasa de Incidencia de Casos por Municipios",
       subtitle = "Datos de población por municipios obtenidos de la página web de la ONEI para el año 2018\n
       Datos filtrados para municipos con una tasa mayor a 15",
       caption = "Enlace a fichero de la ONEI: http://www.onei.gob.cu/sites/default/files/03series_0.rar\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19/blob/master/data/poblacion.cuba.2018.onei.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ipsum() + 
  theme(axis.text.x = element_text(angle=0, hjust = 1),
        axis.text.y = element_text(size = 10,
                                   family = "Century Gothic"),
        panel.grid.major.y = element_blank())

## lineas de casos
library(ggdark)
library(ggthemes)
library(gifski)

image_

evo.animada <- (animate(casos %>% 
  ggplot(aes(x = fecha)) + 
  geom_line(aes(y = muertos), color = "#EF233C", size = 1.5, alpha = 0.5) +
  geom_label(aes(y = muertos, label = "F"), fill = "#EF233C") + 
  geom_line(aes(y = recuperados), color = "#6BAB90", size = 1.5, alpha = 0.5) +
  geom_label(aes(y = recuperados, label = "R"), fill = "#6BAB90", size = 4) +
  geom_line(aes(y = activos), color = "#ECC30B", size = 1.5, alpha = 0.5) +
  geom_label(aes(y = activos, label = "A"), fill = "#ECC30B", size = 3,) +
  geom_line(aes(y = acumulados), color = "#EE6C4D", size = 1.5, alpha = 0.5) +
  geom_label(aes(y = acumulados, label = "C"), fill = "#EE6C4D", size = 2,) +
  labs(x = "Fecha", y ="Cantidad de Casos",
            title = "Evolución de Casos por Variables:",
            subtitle = "F - Fallecidos, R - Recuperados, A - Activos, C - Acumulados",
            caption ="''Probando los paquetes: ggdark y gganimate''\n
       Fuente de datos: https://covid19cubadata.github.io/data/covid19-cuba.json\n
       Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ipsum() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank()) +
  transition_reveal(casos$fecha) +
  ease_aes('linear'),
  width = 690, height = 480, nframes = 360, fps = 50))


image_write_gif(evo.animada, "figs/casos.evo.animada.gif")




legend(x = "toplesft", 
       legend = c("Muertos", "Recuperados", "Activos", "Acumulados"), 
       fill = c("EF233C", "E0FBFC", "3D5A80", "EE6C4D"), 
       title = "Variables")




   



