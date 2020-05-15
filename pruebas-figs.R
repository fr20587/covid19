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

evo.animada <- (animate(casos %>% 
  ggplot(aes(x = fecha)) + 
  geom_line(aes(y = muertos), color = "#EF233C", size = 1.5, alpha = 0.5) +
  geom_label(aes(y = muertos, label = "F"), fill = "#EF233C", size = 4, alpha = 0.5, fill = "white") + 
  geom_line(aes(y = recuperados), color = "#6BAB90", size = 1.5, alpha = 0.5) +
  geom_label(aes(y = recuperados, label = "R"), fill = "#6BAB90", size = 4, alpha = 0.5, fill = "white") +
  geom_line(aes(y = activos), color = "#ECC30B", size = 1.5, alpha = 0.5) +
  geom_label(aes(y = activos, label = "A"), fill = "#ECC30B", size = 4, alpha = 0.5, fill = "white") +
  geom_line(aes(y = acumulados), color = "#EE6C4D", size = 1.5, alpha = 0.5) +
  geom_label(aes(y = acumulados, label = "C"), fill = "#EE6C4D", size = 4, alpha = 0.5, fill = "white") +
  labs(x = "Fecha", y ="Cantidad de Casos",
            title = paste0("Evolución de Casos por Variables - ", 
                           "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
            subtitle = "F - Fallecidos, R - Recuperados, A - Activos, C - Acumulados",
            caption ="Fuente de datos: https://covid19cubadata.github.io\n
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
  width = 900, height = 506, nframes = 560, fps = 50))



casos %>% 
  ggplot(aes(x = fecha)) +
    geom_line(aes(y = muertos), color = "#EF233C", size = 1.5, alpha = 0.5) +
    geom_line(aes(y = recuperados), color = "#6BAB90", size = 1.5, alpha = 0.5) +
    geom_line(aes(y = activos), color = "#ECC30B", size = 1.5, alpha = 0.5) +
    geom_line(aes(y = acumulados), color = "#EE6C4D", size = 1.5, alpha = 0.5) +
    labs(x = "Fecha", y ="Cantidad de Casos",
         title = paste0("Evolución de Casos por Variables\n", 
          "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
          subtitle = "F - Fallecidos, R - Recuperados, A - Activos, C - Acumulados",
          caption ="Fuente de datos: https://covid19cubadata.github.io\n
          Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv\n
          Gráfico realizado por: Frank Rodríguez López") +
    dark_mode(theme_fivethirtyeight()) +
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey30", size = 0.2),
          panel.grid.minor = element_line(color = "grey30", size = 0.2),
          legend.background = element_blank(),
          axis.ticks = element_blank(),
          legend.key = element_blank())

ggsave("figs/casos.evo.n.png", width = 30, height = 20, units = "cm")

evo.animada.n <- (animate(casos %>% 
                          ggplot(aes(x = fecha)) + 
                          geom_line(aes(y = muertos), color = "#EF233C", size = 1.5, alpha = 0.5) +
                          geom_label(aes(y = muertos, label = "F"), fill = "#EF233C", size = 4, alpha = 0.5, fill = "white") + 
                          geom_line(aes(y = recuperados), color = "#6BAB90", size = 1.5, alpha = 0.5) +
                          geom_label(aes(y = recuperados, label = "R"), fill = "#6BAB90", size = 4, alpha = 0.5, fill = "white") +
                          geom_line(aes(y = activos), color = "#ECC30B", size = 1.5, alpha = 0.5) +
                          geom_label(aes(y = activos, label = "A"), fill = "#ECC30B", size = 4, alpha = 0.5, fill = "white") +
                          geom_line(aes(y = acumulados), color = "#EE6C4D", size = 1.5, alpha = 0.5) +
                          geom_label(aes(y = acumulados, label = "C"), fill = "#EE6C4D", size = 4, alpha = 0.5, fill = "white") +
                          labs(x = "Fecha", y ="Cantidad de Casos",
                               title = paste0("Evolución de Casos por Variables - ", 
                                              "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
                               subtitle = "F - Fallecidos, R - Recuperados, A - Activos, C - Acumulados",
                               caption ="Fuente de datos: https://covid19cubadata.github.io\n
                                Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv\n
                                Gráfico realizado por: Frank Rodríguez López") +
                          dark_mode(theme_fivethirtyeight()) +
                          theme(panel.background = element_blank(),
                                panel.grid.major = element_line(color = "grey30", size = 0.2),
                                panel.grid.minor = element_line(color = "grey30", size = 0.2),
                                legend.background = element_blank(),
                                axis.ticks = element_blank(),
                                legend.key = element_blank()) +
                          transition_reveal(casos$fecha) +
                          ease_aes('linear'),
                          width = 900, height = 506, nframes = 560, fps = 50))

## Animación Casos Cuba vs Top10

casos.top.10.cu <- casos.ecdc %>% 
  filter(geoId %in% c("CU", "US", "ES", "RU", "UK", "IT", "BR", "FR", "DE", "TR", "IR")) %>%
  filter(cases > 0) %>% 
  arrange(dateRep) %>% 
  arrange(geoId)

casos.cu <- casos.top.10.cu %>% filter(geoId == "CU") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.))

casos.cu$geoId[casos.cu$geoId == "CU"] <- "cu"

casos.us <- casos.top.10.cu %>% filter(geoId == "US") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.))

casos.us$geoId[casos.us$geoId == "US"] <- "us"
 
casos.es <- casos.top.10.cu %>% filter(geoId == "ES") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.))

casos.es$geoId[casos.es$geoId == "ES"] <- "es"

casos.ru <- casos.top.10.cu %>% filter(geoId == "RU") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.))

casos.ru$geoId[casos.ru$geoId == "RU"] <- "ru"

casos.uk <- casos.top.10.cu %>% filter(geoId == "UK") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.))

casos.uk$geoId[casos.uk$geoId == "UK"] <- "gb"

casos.it <- casos.top.10.cu %>% filter(geoId == "IT") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.))

casos.it$geoId[casos.it$geoId == "IT"] <- "it"

casos.br <- casos.top.10.cu %>% filter(geoId == "BR") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.))

casos.br$geoId[casos.br$geoId == "BR"] <- "br"

casos.fr <- casos.top.10.cu %>% filter(geoId == "FR") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.))

casos.fr$geoId[casos.fr$geoId == "FR"] <- "fr"

casos.tr <- casos.top.10.cu %>% filter(geoId == "TR") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.))

casos.tr$geoId[casos.tr$geoId == "TR"] <- "tr"

casos.ir <- casos.top.10.cu %>% filter(geoId == "IR") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.))

casos.ir$geoId[casos.ir$geoId == "IR"] <- "ir"

casos.top.10.cu <- rbind(casos.cu, casos.us, casos.es, casos.ru, casos.uk, casos.it, casos.br, casos.fr, casos.tr, casos.ir) %>% 
  mutate(`tasa.10^5.hab.acum` = 10^5*casos.acum/popData2018) %>% 
  mutate(geoId = as.factor(geoId))

casos.top.10.cu %>% filter(geoId == "cu") %>% 
  ggplot(aes(x = dateRep, y = cases, country = geoId)) +
  geom_flag(size = 10)

casos.top.10.cu %>% 
  ggplot(aes(x = dateRep, y = `tasa.10^5.hab.acum`, country = geoId)) +
  geom_flag(size = 10)









