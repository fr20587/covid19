#========================================"GRAFICOS ANIMADOS"=======================================

# Representando los casos Cuba vs TOP - 10 
### Tema claro
casos.top.10.cu.anim <- casos.top.10.cu  %>% 
  ggplot(aes(x = dateRep, y = casos.acum)) +
  geom_line(aes(colour = countryterritoryCode), size = 1.5, alpha = 0.5, show.legend = F) +
  geom_flag(aes(country = geoId), size = 8) +
  labs(x = "Fecha", y = "Cantidad de Casos",
       title = paste0("Evolución de Casos\n TOP-10 Paises más Cuba - ",
                      "Datos cierre: ", format(Sys.Date(), "%A, %d de %B de %Y")),
       caption ="Fuente de datos: Centro europeo para la prevención y el control de enfermedades\n
       Enlace a fichero de datos: https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-05-15.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() +
  theme(axis.text.x = element_text(hjust = 1), 
        panel.grid.major.x = element_blank()) +  
  transition_reveal(dateRep) +
  ease_aes('linear')

# animate(casos.top.10.cu.anim, width = 900, height = 506, nframes = 560, fps = 50)

image_write_gif(animate(casos.top.10.cu.anim, 
                        width = 900, 
                        height = 506, 
                        nframes = 560, 
                        fps = 50, 
                        rendere = magick_renderer()), 
                "figs/casos.top.10.cu.anim.gif")

`tasa.10^5.top.10.cu.anim` <- casos.top.10.cu  %>% 
  ggplot(aes(x = dateRep, y = `tasa.10^5.hab.acum`)) +
  geom_line(aes(colour = geoId), size = 1.5, alpha = 0.5, show.legend = F) +
  geom_flag(aes(country = geoId), size = 8) +
  labs(x = "Fecha", y = "Cantidad de Casos",
       title = paste0("Evolución de Casos\n TOP-10 Paises más Cuba - ",
                      "Datos cierre: ", 
                      format(Sys.Date(), "%A, %d de %B de %Y")),
       subtitle = "Normalizado por cien mil habitantes.",
       caption ="Fuente de datos: Centro europeo para la prevención y el control de enfermedades\n
       Enlace a fichero de datos: https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-05-15.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() +
  theme(axis.text.x = element_text(hjust = 1), 
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
  dark_mode(theme_ATHENDAT_claro()) +
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

casos.prov.tiempo.anim <- casos.prov.tiempo %>% 
  ggplot(aes(x = casos.acum.prov,
             y = casos,
             group = provincia,
             color = provincia)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  scale_y_log10() +
  scale_x_log10() +
  labs(x = "Casos Acumulados", y = "Casos Nuevos",
       title = paste0("Crecimiento de nuevos Casos por Provincias\n", "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       subtitle = ,
       caption = "Fuente de datos: https://covid19cubadata.github.io/#cuba\n
       Enlace a fichero de datos : https://covid19cubadata.github.io/data/covid19-casos.csv\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~provincia) +  
  theme_ATHENDAT_claro() +
  theme(axis.text.x = element_text(hjust = 1), 
        panel.grid.major.x = element_blank()) +
  transition_reveal(fecha_confirmacion) +
  ease_aes('linear')

image_write_gif(animate(casos.prov.tiempo.anim, 
                        width = 900, 
                        height = 900, 
                        nframes = 560, 
                        fps = 50, 
                        renderer=magick_renderer()), 
                "figs/casos.prov.tiempo.anim.gif") 

casos.tiempo.eventos.anim <- casos.tiempo.eventos %>%
  ggplot(aes(x = Fecha,
             y = Cantidad,
             group = Evento,
             color = Evento)) +
  geom_line(alpha = 0.5,
            size = 2.5) +
  geom_point(alpha = 0.5,
             size = 5) +
  scale_x_date(breaks = "2 weeks") +
  scale_color_manual(values = pal) +
  labs(x = "Fecha", 
       y = "Cantidad de Casos",
       title = "Evolución de casos por Evento en el tiempo.",
       subtitle = paste0("Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       caption = "Fuente de datos: https://covid19cubadata.github.io\n
         Enlace a fichero de datos: https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/evolution_of_cases_by_days.json\n
         Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() +
  theme(axis.text.x = element_text(angle = 17, hjust = 1)) + 
  transition_reveal(Fecha) +
  ease_aes('linear')

image_write_gif(animate(casos.tiempo.eventos.anim, 
                        width = 900, 
                        height = 600, 
                        nframes = 560, 
                        fps = 25, 
                        renderer = magick_renderer()), 
                "figs/casos.tiempo.eventos.anim.gif") 
