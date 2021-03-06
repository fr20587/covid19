
load("rda/cubadata.rda")
load("rda/casos.rda")
load("rda/casosprov.rda")
load("rda/factores.riesgos.rda")

# Distribución de casos por edades por Provincias
cubadata %>% 
  mutate(provincia = reorder(provincia, edad, FUN = median)) %>% 
  ggplot(aes(x = provincia, y = edad, color = provincia)) + 
    geom_boxplot(show.legend = F) + 
    geom_jitter(aes(color = provincia, alpha = "0.01"), show.legend = F)  +
    geom_hline(yintercept=mean(cubadata$edad), color = "#aa4586", linetype ="dotted") +
    geom_text(x = 0, y = mean(cubadata$edad), label = paste0("Media = ", round(mean(cubadata$edad), 2)) , 
            hjust = -1, vjust = -0.5, colour = "#aa4586", size = 11 * 0.8 / .pt,) +
    labs(x = "", y = "Edad",
       title = "Distribución Estadística de Casos",
       subtitle = paste0("Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       caption = "''Provincias ordenadas por media de edad''\n 
       Fuente de datos: https://covid19cubadata.github.io/#cuba\n
       Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv\n
       Gráfico realizado por: Frank Rodríguez López") + 
    theme_ATHENDAT_claro() +
    theme(axis.text.x = element_text(angle = 17, hjust = 1),
          panel.grid.major.x = element_blank())

ggsave("figs/casos.dist.edades.prov.png", width = 30, height = 20, units = "cm")

image_write(agregar_logo(plot_path = "figs/casos.dist.edades.prov.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/casos.dist.edades.prov.png")


# Distribución de casos por rangos de edades por Provincias


rango.c <- as_tibble(table(Rango = cubadata$rango, Provincias = cubadata$provincia)) %>%
  rename(n, Cantidad = n) 

rango.c %>% 
  mutate(Rango = as_factor(Rango), Provincias = as_factor(Provincias)) %>% 
  ggplot(aes(Provincias, Rango, fill = Cantidad)) +
  geom_tile() +
  geom_text(aes(label = ifelse(Cantidad > 0, Cantidad, " ")), 
            color = ifelse(rango.c$Cantidad < 50, "#053841", "white"), size = 4) +
  scale_fill_gradient(low = "white", high = "#053841") +
  labs(x = "", y = "",
       title = paste0("Casos - ", "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       subtitle = "Rangos Etarios vs Provincias - Distribución de Rangos Etarios oficial del MINSAP",
       caption = "''Provincias ordenadas por orden alfabético''\n
       Fuente de datos: https://covid19cubadata.github.io/#cuba\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19/blob/master/rda/casos.rda\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() + 
  theme(axis.text.x=element_text(angle = 17, hjust = 1))

ggsave("figs/casos.rango.prov.png", width = 30, height = 20, units = "cm")

image_write(agregar_logo(plot_path = "figs/casos.rango.prov.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/casos.rango.prov.png")


  ## Rango 2
rango.c2 <-as_tibble(table(Rango2 = cubadata$rango2, Provincias = cubadata$provincia)) %>%
  rename(n, Cantidad = n) 

rango.c2%>% 
  mutate(Rango2 = as_factor(Rango2), Provincias = as_factor(Provincias)) %>% 
  ggplot(aes(Provincias, Rango2, fill = Cantidad)) +
  geom_tile() +
  geom_text(aes(label = ifelse(Cantidad > 0, Cantidad, " ")),color = ifelse(rango.c2$Cantidad < 50, "black", "white"), size = 4) +
  scale_fill_gradient(low="white", high="#053841") +
  labs(x = "", y = "Rango Etario",
       title = paste0("Casos - ", "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       subtitle = "Rangos Etarios vs Provincias - Rangos Etarios por Decenio",
       caption = "''Provincias ordenadas por orden alfabético''\n
       Fuente de datos: https://covid19cubadata.github.io/#cuba\n
       Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() + 
  theme(axis.text.x = element_text(angle=17, hjust = 1))

ggsave("figs/casos.rango2.prov.png", width = 30, height = 20, units = "cm")

image_write(agregar_logo(plot_path = "figs/casos.rango2.prov.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/casos.rango2.prov.png")

## Distribución de casos por Municipios según Rango Etario

rango.c2.mun <-as_tibble(table(rango2 = cubadata$rango2, municipio = cubadata$municipio)) %>%
  rename(n, Cantidad = n) 

rango.c2.mun%>% 
  mutate(rango2 = as_factor(rango2), municipio = as_factor(municipio)) %>% 
  ggplot(aes(municipio, rango2, fill = Cantidad)) +
  geom_tile() +
  geom_text(aes(label = ifelse(Cantidad > 0, Cantidad, " ")),color = ifelse(rango.c2.mun$Cantidad < max(rango.c2.mun$Cantidad)*0.50, "#053841", "white"), size = 4) +
  scale_fill_gradient(low="white", high="#053841") +
  labs(x = "", y = "",
       title = paste0("Casos - ", "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       subtitle = "Rangos Etarios vs Municipios - Rangos Etarios por Decenio",
       caption = "''Municipios ordenados por orden alfabético'' - Fuente de dato: https://covid19cubadata.github.io/#cuba - Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv - Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() +
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.45),
        legend.position = 'top')


ggsave("figs/casos.rango2.mun.png", width = 60, height = 20, units = "cm")

image_write(agregar_logo(plot_path = "figs/casos.rango2.mun.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/casos.rango2.mun.png")

# Analizando la edad media de casos y su desviación estandar

edades <- cubadata %>% 
  group_by(edad, provincia) %>% 
  summarise(Edad = mean(edad, na.rm = T),
            Cantidad = n()) %>% 
  arrange(provincia)

edades %>% ggplot(aes(x = Cantidad, y = Edad, color = provincia)) +
  geom_jitter(alpha = 0.3, show.legend = F) +
  labs(x = "Cantidad", y = "Edad",
       title = paste0("Dispersión de Casos \n", "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       subtitle = "Cantidad de casos vs Edad",
       caption = "Fuente de datos: https://covid19cubadata.github.io/#cuba\n
       Enlace a fichero de datos : https://covid19cubadata.github.io/data/covid19-casos.csv\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() +
  facet_wrap(~provincia)

ggsave("figs/casos.disp.edades.png", width = 25, height = 25, units = "cm")

image_write(agregar_logo(plot_path = "figs/casos.disp.edades.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/casos.disp.edades.png")

# Distribución de muertes por edades por Provicias

muertes %>% 
  mutate(provincia = reorder(provincia, edad, FUN = median)) %>% 
  ggplot(aes(x = provincia, y = edad, color = provincia)) + 
  geom_boxplot(show.legend = F) + 
  geom_jitter(aes(color = provincia, alpha = "0.01"),show.legend = F)  +
  geom_hline(yintercept=mean(muertes$edad), color = "#aa4586", linetype ="dotted") +
  geom_text(x = 0, 
            y = mean(muertes$edad), 
            label = paste0("Media = ", round(mean(muertes$edad), 2)), 
            hjust = -1, vjust = -0.5, 
            colour = "red", size = 11 * 0.8 / .pt,
            family = "URWGeometricW03-Light") +
  labs(x = "", y = "Edad",
       title = "Distribución Estadística de Fallecidos",
       subtitle = paste0("Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y"), "\n
       NA: provincias no informada en la publicación oficial"),
       caption = "''Provincias ordenadas por media de edad''\n 
       Fuente de datos: Reportes oficiales publicados en la página web del MINSAP - 
       Enlace a fichero de datos: https://github.com/fr20587/covid19cu/blob/master/muertes.xlsx - 
       Gráfico realizado por: Frank Rodríguez López") + 
  theme_ATHENDAT_claro() +
  theme(axis.text.x = element_text(angle = 17, hjust = 1), 
        panel.grid.major.x = element_blank())

ggsave("figs/muer.dist.edades.prov.png", width = 30, height = 20, units = "cm")

image_write(agregar_logo(plot_path = "figs/muer.dist.edades.prov.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/muer.dist.edades.prov.png")

# Distribución de muertes por rangos etrios por Provincias

## Rango
rango.m <- as_tibble(table(Rango = muertes$rango, Provincias = muertes$provincia)) %>%
  rename(n, Cantidad = n) 

rango.m %>% 
  mutate(Rango = as_factor(Rango), Provincias = as_factor(Provincias)) %>% 
  ggplot(aes(Provincias, Rango, fill = Cantidad)) +
  geom_tile() +
  geom_text(aes(label = ifelse(Cantidad > 0, Cantidad, " ")), color = ifelse(rango.m$Cantidad < 5, "black", "white"), size = 4) +
  scale_fill_gradient(low = "white", high = "#aa4586") +
  labs(x = "", y = "Rango Etario",
       title = paste0("Fallecidos - ", "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       subtitle = "Rangos Etarios vs Provincias - Distribución de Rangos Etarios oficial del MINSAP",
       caption = "''Provincias ordenadas por orden alfabético'' - Fuente de datos: Reportes oficiales publicados en la página web del MINSAP - Enlace a fichero de datos: https://github.com/fr20587/covid19cu/blob/master/muertes.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() + 
  theme(axis.text.x=element_text(angle=17, hjust = 1))

ggsave("figs/muer.rango.prov.png", width = 30, height = 20, units = "cm")

image_write(agregar_logo(plot_path = "figs/muer.rango.prov.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/muer.rango.prov.png")

## Rango 2
rango.m2 <- as_tibble(table(Rango = muertes$rango2, Provincias = muertes$provincia)) %>%
  rename(n, Cantidad = n) 

rango.m2 %>% 
  mutate(Rango = as_factor(Rango), Provincias = as_factor(Provincias)) %>% 
  ggplot(aes(Provincias, Rango, fill = Cantidad)) +
  geom_tile() +
  geom_text(aes(label = ifelse(Cantidad > 0, Cantidad, " ")), color = ifelse(rango.m2$Cantidad < 5, "black", "white"), size = 4) +
  scale_fill_gradient(low="white", high="#aa4586") +
  labs(x = "", y = "Rango Etario",
       title = paste0("Fallecidos - ", "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       subtitle = "Rangos Etarios vs Provincias - Rangos Etarios por Decenio",
       caption = "''Provincias ordenadas por orden alfabético''\n
       Fuente de datos: Reportes oficiales publicados en la página web del MINSAP\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19cu/blob/master/muertes.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() + 
  theme(axis.text.x=element_text(angle=17, hjust = 1), 
        panel.grid = element_blank())

ggsave("figs/muer.rango2.prov.png", width = 30, height = 20, units = "cm")

image_write(agregar_logo(plot_path = "figs/muer.rango2.prov.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/muer.rango2.prov.png")

## Distribución de muertes por Municipios según Rango Etario

rango.m2.mun <- as_tibble(table(rango2 = muertes$rango2, municipio = muertes$municipio)) %>%
  rename(n, Cantidad = n) 

rango.m2.mun %>% 
  mutate(rango = as_factor(rango2), minicipio = as_factor(municipio)) %>% 
  ggplot(aes(municipio, rango, fill = Cantidad)) +
  geom_tile() +
  geom_text(aes(label = ifelse(Cantidad > 0, Cantidad, " ")), color = ifelse(rango.m2.mun$Cantidad < 4, "black", "white"), size = 4) +
  scale_fill_gradient(low="white", high="#aa4586") +
  labs(x = "", y = "Rango Etario",
       title = paste0("Fallecidos - ", "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       subtitle = "Rangos Etarios vs Municipios - Rangos Etarios por Decenios",
       caption = "''Municipios ordenados por orden alfabético'' - Fuente de datos: Reportes oficiales publicados en la página web del MINSAP - Enlace a fichero de datos: https://github.com/fr20587/covid19cu/blob/master/muertes.xlsx - Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() + 
  theme(axis.text.x = element_text(angle=23, hjust = 1), 
        panel.grid = element_blank(),
        legend.position = 'top')

ggsave("figs/muer.rango2.mun.png", width = 40, height = 20, units = "cm")

image_write(agregar_logo(plot_path = "figs/muer.rango2.mun.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/muer.rango2.mun.png")

# Determinando la edad media de muertes y su desviación estandar

edad.media.muertes <- mean(muertes$edad)
desviación.estandar.edad.muertes <- sd(muertes$edad)

# Resumiendo la cantidad de muertes por provincias

edadesmuertes <- muertes %>% 
  group_by(edad, provincia) %>% 
  summarise(Edad = mean(edad, na.rm = T),
            Cantidad = n()) %>% 
  arrange(provincia)

edadesmuertes %>% ggplot(aes(x = Cantidad, y = Edad, color = provincia)) +
  geom_jitter(alpha = 0.3, show.legend = F) +
  labs(x = "Cantidad", y = "Edad",
       title = "Dispersión de Fallecidos",
       subtitle = paste0("Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y"), "\nCantidad de Fallecidos vs Edad\n
       NA: provincias no informada en la publicación oficial"),
       caption = "Fuente de datos: Reportes oficiales publicados en la página web del MINSAP\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19cu/blob/master/muertes.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() +
  facet_wrap(~provincia)

ggsave("figs/muer.disp.edades.png", width = 25, height = 25, units = "cm")

image_write(agregar_logo(plot_path = "figs/muer.disp.edades.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/muer.disp.edades.png")


# Representando la incidencia de los factores de riesgos en los fallecidos

Factores %>% filter(Total > 1) %>% 
  mutate(Factor.Riesgo = reorder(Factor.Riesgo, Total)) %>% 
  ggplot(aes(x = Total, xend = 0,
             y = Factor.Riesgo, yend = Factor.Riesgo,
             colour = Factor.Riesgo)) +
  geom_point(show.legend = F) +
  geom_segment(show.legend = F) +
  geom_text(aes(label = Total), show.legend = F, hjust = -1) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "", y = "",
       title = paste0("Factores de Riesgo en Pacientes Fallecidos \n", "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       subtitle = "Datos filtrado para factores con más de una aparición",
       caption = "''Factores con orden descendente de mayoner a menor incidencia''\n
       Fuente de datos: Reportes oficiales publicados en la página web del MINSAP\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19/blob/master/data/muertes.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() + 
  theme(axis.text.x=element_text(angle=0, hjust = 1),
        panel.grid.major.y = element_blank())
  
ggsave("figs/muer.factores.riesgos.png", width = 30, height = 20, units = "cm")

image_write(agregar_logo(plot_path = "figs/muer.factores.riesgos.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/muer.factores.riesgos.png")

# Representando la detección de casos detectados con COVID-19 con respecto a la fecha de deceso

count(class.muertes, clasificación) %>% mutate(clasificación = reorder(clasificación, n)) %>% 
  ggplot(aes(x = n, xend = 0,
             y = clasificación, yend = clasificación,
             colour = clasificación)) +
  geom_point(show.legend = F) +
  geom_segment(show.legend = F) +
  geom_text(aes(label = n), show.legend = F, hjust = -1) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "", y = "",
       title = "Casos detectados con COVID-19 con respecto a la fecha de deceso.",
       subtitle = paste0("Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y"), "\nNA: Casos que no he podido emparejar la fecha de detección con la fecha de fallecimiento al no informarse 
       la provincia y el municipio en el parte oficial o que las edades y sexos dados en el informe oficial no coincide. 
       con ninguno de los casos idetificados para esa provincia y municipio"),
       caption = "Fuente de datos: Reportes oficiales publicados en la página web del MINSAP\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19/blob/master/data/muertes.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() + 
  theme(axis.text.x=element_text(angle=0, hjust = 1),
        panel.grid.major.y = element_blank())

ggsave("figs/muer.clasificacion.png", width = 30, height = 20, units = "cm")

image_write(agregar_logo(plot_path = "figs/muer.clasificacion.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "ii",
                         logo_scale = 7), 
            "figs/muer.clasificacion.png")

# Representando los municipios por tasa de incidencia por cien mil habitantes

casospoblmun %>% filter(`Tasa.10^5Hab` > 20) %>% 
  mutate(municipio = reorder(municipio, `Tasa.10^5Hab`)) %>% 
  ggplot(aes(x = `Tasa.10^5Hab`, xend = 0,
             y = municipio, yend = municipio,
             color = provincia)) +
  geom_point(show.legend = T) +
  geom_segment(show.legend = F) +
  geom_text(aes(label = `Tasa.10^5Hab`), 
            show.legend = F, 
            hjust = -0.5,
            family = "URWGeometricW03-Light") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Tasa por 10^5 Habitantes", y = "",
       title = paste0("Tasa de Incidencia de Casos por Municipios \n", "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       subtitle = "Datos de población por municipios obtenidos de la página web de la ONEI para el año 2018\n
       Datos filtrados para municipos con una tasa mayor a 20",
       caption = "Enlace a fichero de la ONEI: http://www.onei.gob.cu/sites/default/files/03series_0.rar\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19/blob/master/data/poblacion.cuba.2018.onei.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() + 
  theme(axis.text.x = element_text(angle=0, hjust = 1),
        axis.text.y = element_text(size = 10,
                                    family = "Century Gothic"),
        panel.grid.major.y = element_blank())

ggsave("figs/muer.tasa.mun.png", width = 30, height = 20, units = "cm")

image_write(agregar_logo(plot_path = "figs/muer.tasa.mun.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/muer.tasa.mun.png")

# Representando evolución en el tiempo de los casos por provincias en un gráfico log - log

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
        panel.grid.major.x = element_blank())

ggsave("figs/casos.prov.tiempo.png", width = 30, height = 30, units = "cm")

image_write(agregar_logo(plot_path = "figs/casos.prov.tiempo.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/casos.prov.tiempo.png")

## Represaentación de Nuevos Casos por provincias en los últimos 15 días.

casos.prov.u15.dias <- casos.prov.tiempo %>% 
  filter(fecha_confirmacion >= (Sys.Date() - 15)) %>% 
  ggplot(aes(x = fecha_confirmacion,
             y = casos,
             group = provincia,
             fill = provincia)) +
  geom_col(show.legend = F) +
  scale_x_date(date_breaks = "2 days") + 
  labs(x = "Casos Acumulados", y = "Casos Nuevos",
       title = paste0("Cantidad de nuevos casos por Provincias en los últimos 15 días.\n", "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       subtitle = ,
       caption = "Fuente de datos: https://covid19cubadata.github.io/#cuba - Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv - Gráfico realizado por: Frank Rodríguez López") +
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~provincia) +  
  theme_ATHENDAT_claro() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.major.x = element_blank())

ggsave("figs/casos.prov.u15.dias.png", width = 30, height = 30, units = "cm")

image_write(agregar_logo(plot_path = "figs/casos.prov.u15.dias.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/casos.prov.u15.dias.png")

## Represaentación de Nuevos Casos por provincias en el tiempo..

casos.nuevos.prov.tiempo <- casos.prov.tiempo %>% 
  ggplot(aes(x = fecha_confirmacion,
             y = casos,
             group = provincia,
             color = provincia)) +
  geom_col(show.legend = F) +
  scale_x_date(date_breaks = "2 week") + 
  labs(x = "Casos Acumulados", y = "Casos Nuevos",
       title = paste0("Cantidad de nuevos casos por Provincias\n", "Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       subtitle = ,
       caption = "Fuente de datos: https://covid19cubadata.github.io/#cuba - Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv - Gráfico realizado por: Frank Rodríguez López") +
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~provincia) +  
  theme_ATHENDAT_claro() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.major.x = element_blank())

ggsave("figs/casos.nuevos.prov.tiempo.png", width = 30, height = 30, units = "cm")

image_write(agregar_logo(plot_path = "figs/casos.nuevos.prov.tiempo.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/casos.nuevos.prov.tiempo.png")

# Segmentando la cantidad de casos por rango etario en el tiempo

casos.rango2 <- count(cubadata, fecha_confirmacion, rango2) %>% rename(casos = n)

casos.rango2.tiempo <- casos.rango2 %>% 
  ggplot(aes(fecha_confirmacion, casos, color = rango2)) +
  geom_line(show.legend = F) +
  scale_y_continuous() +
  facet_grid(rango2~.) +
  labs(x = "", y = "Cantidad",
       title = "Evolución en el tiempo de los casos de COVID-19 por Rango Etario en Cuba",
       subtitle = paste0("Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       caption = "''Provincias ordenadas por media de edad'' - Fuente de datos: https://covid19cubadata.github.io/#cuba - Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv - Gráfico realizado por: Frank Rodríguez López") + 
  theme_ATHENDAT_claro() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        panel.grid.major.x = element_blank())

ggsave("figs/casos.rango2.tiempo.png", width = 30, height = 20, units = "cm")

image_write(agregar_logo(plot_path = "figs/casos.rango2.tiempo.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 10), 
            "figs/casos.rango2.tiempo.png")

casos.rango2.tiempo.infantes <- casos.rango2 %>% 
  filter(rango2 %in% c("0-9", "10-19")) %>% 
  ggplot(aes(fecha_confirmacion, casos, color = rango2)) +
  geom_line(show.legend = F) +
  facet_grid(rango2~.) +
  labs(x = "", y = "Cantidad",
       title = "Evolución en el tiempo de los casos de COVID-19 
Rangos Etarios de infantes en Cuba.",
       subtitle = paste0("Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       caption = "''Provincias ordenadas por media de edad'' - Fuente de datos: https://covid19cubadata.github.io/#cuba - Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv - Gráfico realizado por: Frank Rodríguez López") + 
  theme_ATHENDAT_claro() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        panel.grid.major.x = element_blank())

ggsave("figs/casos.rango2.tiempo.infantes.png", width = 30, height = 10, units = "cm")

image_write(agregar_logo(plot_path = "figs/casos.rango2.tiempo.infantes.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/casos.rango2.tiempo.infantes.png")

casos.rango2.tiempo.trabajadores <- casos.rango2 %>% 
  filter(rango2 %in% c("20-29", "30-39", "40-49", "50-59", "60-69")) %>% 
  ggplot(aes(x = fecha_confirmacion, y = casos, color = rango2)) +
  geom_line(show.legend = F) +
  facet_grid(rango2~.) +
  labs(x = "", y = "Cantidad",
       title = "Evolución en el tiempo de los casos de COVID-19 
Rangos Etarios en edad laboral en Cuba.",
       subtitle = paste0("Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       caption = "''Provincias ordenadas por media de edad'' - Fuente de datos: https://covid19cubadata.github.io/#cuba - Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv - Gráfico realizado por: Frank Rodríguez López") + 
  theme_ATHENDAT_claro() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        panel.grid.major.x = element_blank())

ggsave("figs/casos.rango2.tiempo.trabajadores.png", width = 30, height = 15, units = "cm")

image_write(agregar_logo(plot_path = "figs/casos.rango2.tiempo.trabajadores.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/casos.rango2.tiempo.trabajadores.png")

casos.rango.tiempo <- count(cubadata, fecha_confirmacion, rango) %>% 
  rename(casos = n) %>% 
  ggplot(aes(fecha_confirmacion, casos, color = rango)) +
  geom_line(show.legend = F) +
  facet_grid(rango~.) +
  labs(x = "", y = "Cantidad",
       title = "Evolución en el tiempo de los casos de COVID-19 por Rango Etario en Cuba",
       subtitle = paste0("Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       caption = "''Provincias ordenadas por media de edad'' - Fuente de datos: https://covid19cubadata.github.io/#cuba - Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv - Gráfico realizado por: Frank Rodríguez López") + 
  theme_ATHENDAT_claro() +
  theme(axis.text.x = element_text(angle = 17, hjust = 1),
        panel.grid.major.x = element_blank())

ggsave("figs/casos.rango.tiempo.png", width = 30, height = 20, units = "cm")

image_write(agregar_logo(plot_path = "figs/casos.rango.tiempo.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 10), 
            "figs/casos.rango.tiempo.png")

# Evolución de los casos activos en el tiempo.
casos.activos.tiempo <- casos.tiempo.eventos %>% 
  filter(Evento == "Casos Activos") %>% 
  ggplot(aes(x = Fecha,
             y = Cantidad,
             group = Evento,
             fill = Evento)) +
  geom_area(alpha = 0.5, size = 2.5, show.legend = F) +
  geom_line(aes(color = Evento), alpha = 0.9, size = 2.5, show.legend = F) +
  geom_hline(yintercept = casos.activos.hoy$Cantidad, color = "#aa4586", linetype ="dotted") +
 
  scale_x_date(breaks = "2 weeks") +
  scale_color_manual(values = pal) +
  labs(x = "Fecha", 
       y = "Cantidad de Casos",
       title = "Evolución de Casos Activos en el tiempo.",
       subtitle = paste0("Datos cierre: ", format(Sys.Date() - 1, "%A, %d de %B de %Y")),
       caption = "Fuente de datos: https://covid19cubadata.github.io - Enlace a fichero de datos: https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/evolution_of_cases_by_days.json - Gráfico realizado por: Frank Rodríguez López") +
  theme_ATHENDAT_claro() +
  theme(axis.text.x = element_text(angle = 17, hjust = 1))

ggsave("figs/casos.activos.tiempo.png", width = 30, height = 10, units = "cm")

image_write(agregar_logo(plot_path = "figs/casos.activos.tiempo.png",
                         logo_path = "logo/logo.png",
                         posicion_logo= "sd",
                         logo_scale = 7), 
            "figs/casos.activos.tiempo.png")
