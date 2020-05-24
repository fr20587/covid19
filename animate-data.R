#========================================"GRAFICOS ANIMADOS"=======================================

# Representando los casos Cuba vs TOP - 10 
### Tema claro
casos.top.10.cu.anim <- casos.top.10.cu  %>% 
  ggplot(aes(x = dateRep, y = casos.acum)) +
  geom_line(aes(colour = countryterritoryCode), size = 1.5, alpha = 0.5, show.legend = F) +
  geom_flag(aes(country = geoId), size = 8) +
  labs(x = "Fecha", y ="Cantidad de Casos",
       title = paste0("Evolución de Casos\n TOP-10 Paises más Cuba - ",
                      "Datos cierre: ", 
                      format(Sys.Date(), "%A, %d de %B de %Y")),
       caption ="Fuente de datos: Centro europeo para la prevención y el control de enfermedades\n
       Enlace a fichero de datos: https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-05-15.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 17, hjust = 1), 
        panel.grid.major.x = element_blank()) +  
  transition_reveal(dateRep) +
  ease_aes('linear')

# animate(casos.top.10.cu.anim, width = 900, height = 506, nframes = 560, fps = 50)

image_write_gif(animate(casos.top.10.cu.anim, 
                        width = 900, 
                        height = 506, 
                        nframes = 560, 
                        fps = 50, 
                        renderer=magick_renderer()), 
                "figs/casos.top.10.cu.anim.gif")

`tasa.10^5.top.10.cu.anim` <- casos.top.10.cu  %>% 
  ggplot(aes(x = dateRep, y = `tasa.10^5.hab.acum`)) +
  geom_line(aes(colour = geoId), size = 1.5, alpha = 0.5, show.legend = F) +
  geom_flag(aes(country = geoId), size = 8) +
  labs(x = "Fecha", y ="Cantidad de Casos",
       title = paste0("Evolución de Casos\n TOP-10 Paises más Cuba - ",
                      "Datos cierre: ", 
                      format(Sys.Date(), "%A, %d de %B de %Y")),
       subtitle = "Normalizado por cien mil habitantes.",
       caption ="Fuente de datos: Centro europeo para la prevención y el control de enfermedades\n
       Enlace a fichero de datos: https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-05-15.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 17, hjust = 1), 
        panel.grid.major.x = element_blank()) +  
  transition_reveal(dateRep) +
  ease_aes('linear')

# animate(`tasa.10^5.top.10.cu.anim`, width = 900, height = 506, nframes = 560, fps = 50)


image_write_gif(animate(`tasa.10^5.top.10.cu.anim`, 
                        width = 900, 
                        height = 506, 
                        nframes = 560, 
                        fps = 50, 
                        renderer=magick_renderer()), 
                "figs/`tasa.10^5.top.10.cu.anim`.gif")

### Tema oscuro
casos.top.10.cu.anim.n <- casos.top.10.cu  %>% 
  ggplot(aes(x = dateRep, y = casos.acum)) +
  geom_line(aes(colour = geoId), size = 1.5, alpha = 0.5, show.legend = F) +
  geom_flag(aes(country = geoId), size = 8) +
  labs(x = "Fecha", y ="Cantidad de Casos",
       title = paste0("Evolución de Casos\n TOP-10 Paises más Cuba - ",
                      "Datos cierre: ", 
                      format(Sys.Date(), "%A, %d de %B de %Y")),
       caption ="Fuente de datos: Centro europeo para la prevención y el control de enfermedades\n
       Enlace a fichero de datos: https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-05-15.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#141622"),
        panel.background = element_rect(fill = "#141622", 
                                        colour = "#141622",
                                        size = 2, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, 
                                        linetype = 'solid',
                                        colour = "gray30"),
        axis.title.x = element_text(size = 13, 
                                    face = "bold", 
                                    colour = "white", 
                                    family = "Century Gothic"),
        axis.title.y = element_text(size = 13, 
                                    face = "bold", 
                                    colour = "white", 
                                    family = "Century Gothic"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.title = element_text(face = "bold", 
                                  colour = "white", 
                                  size = 14, 
                                  family = "Century Gothic"),
        plot.subtitle = element_text(colour = "white", 
                                     family = "URWGeometricW03-Light", 
                                     size = 10),
        plot.caption = element_text(colour = "white", 
                                    family = "URWGeometricW03-Light", 
                                    size = 10),
        plot.caption.position = "plot",
        legend.title = element_text(colour = "white", 
                                    family = "URWGeometricW03-Light", 
                                    size = 14),
        legend.text = element_text(colour = "white", 
                                   family = "URWGeometricW03-Light", 
                                   size = 14)) +  
  transition_reveal(dateRep) +
  ease_aes('linear')

# animate(casos.top.10.cu.anim, width = 900, height = 506, nframes = 560, fps = 50)

image_write_gif(animate(casos.top.10.cu.anim.n, 
                        width = 900, 
                        height = 506, 
                        nframes = 560, 
                        fps = 50, 
                        renderer=magick_renderer()), 
                "figs/casos.top.10.cu.anim.n.gif")

`tasa.10^5.top.10.cu.anim.n` <- casos.top.10.cu  %>% 
  ggplot(aes(x = dateRep, y = `tasa.10^5.hab.acum`)) +
  geom_line(aes(colour = geoId), size = 1.5, alpha = 0.5, show.legend = F) +
  geom_flag(aes(country = geoId), size = 8) +
  labs(x = "Fecha", y ="Cantidad de Casos",
       title = paste0("Evolución de Casos\n TOP-10 Paises más Cuba - ",
                      "Datos cierre: ", 
                      format(Sys.Date(), "%A, %d de %B de %Y")),
       subtitle = "Normalizado por cien mil habitantes.",
       caption ="Fuente de datos: Centro europeo para la prevención y el control de enfermedades\n
       Enlace a fichero de datos: https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-05-15.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  dark_mode(theme_ipsum()) +
  theme(axis.text.x = element_text(angle = 17, hjust = 1), 
        panel.grid.major.x = element_blank()) +  
  transition_reveal(dateRep) +
  ease_aes('linear')

# animate(`tasa.10^5.top.10.cu.anim.n`, width = 900, height = 506, nframes = 560, fps = 50)


image_write_gif(animate(`tasa.10^5.top.10.cu.anim.n`, 
                        width = 900, 
                        height = 506, 
                        nframes = 560, 
                        fps = 50, 
                        renderer=magick_renderer()), 
                "figs/`tasa.10^5.top.10.cu.anim.n`.gif")