library(tidyverse)
library(d3heatmap)
library(hrbrthemes)

load("rda/cubadata.rda")
load("rda/casos.rda")
load("rda/casosprov.rda")
load("rda/factores.riesgos.rda")

# Distribución de casos por edades por Provicias
cubadata %>% 
  mutate(provincia = reorder(provincia, edad, FUN = median)) %>% 
  ggplot(aes(x = provincia, y = edad, color = provincia)) + 
    geom_boxplot(show.legend = F) + 
    geom_jitter(aes(color = provincia, alpha = "0.01"),show.legend = F)  +
    geom_hline(yintercept=mean(cubadata$edad), color = "red", linetype ="dotted") +
    geom_text(x = 0, y = mean(cubadata$edad), label = "Media   =", 
            hjust = -1, vjust = -0.5, colour = "red", size = 11 * 0.8 / .pt,) +
    geom_text(x = 1.25, y = mean(cubadata$edad), label = round(mean(cubadata$edad), 2), 
            hjust = -1, vjust = -0.5, colour = "red", size = 11 * 0.8 / .pt,) +
    labs(x="", y="Edad",
       title="Distribución Estadística de Casos:",
       subtitle = "Edades vs Provincias",
       caption="''Provincias ordenadas por media de edad''\n 
       Fuente de datos: https://covid19cubadata.github.io/#cuba\n
       Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv\n
       Gráfico realizado por: Frank Rodríguez López") + 
    theme_ipsum() +
    theme(axis.text.x=element_text(angle = 17, hjust = 1), 
          panel.grid.major.x = element_blank())

ggsave("figs/casosedades.png", width = 30, height = 20, units = "cm")


# Distribución de casos por rangos de edades por Provincias


rango.c <- as_tibble(table(Rango = cubadata$rango, Provincias = cubadata$provincia)) %>%
  rename(n, Cantidad = n) 

rango.c %>% 
  mutate(Rango = as_factor(Rango), Provincias = as_factor(Provincias)) %>% 
  ggplot(aes(Provincias, Rango, fill = Cantidad)) +
  geom_tile() +
  geom_text(aes(label = ifelse(Cantidad > 0, Cantidad, " ")), color = ifelse(rango.c$Cantidad < 50, "black", "white"), size = 4) +
  scale_fill_gradient(low="white", high="red") +
  labs(x="", y="Rango Etario",
       title="Casos:",
       subtitle = "Rangos Etarios vs Provincias\n
       Distribución oficial del MINSAP",
       caption="''Provincias ordenadas por orden alfabético''\n
       Fuente de datos: https://covid19cubadata.github.io/#cuba\n
       Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ipsum() + 
  theme(axis.text.x=element_text(angle=17, hjust = 1))

ggsave("figs/rangoedades.png", width = 30, height = 20, units = "cm")


  ## Rango 2
rango.c2 <-as_tibble(table(Rango2 = cubadata$rango2, Provincias = cubadata$provincia)) %>%
  rename(n, Cantidad = n) 

rango.c2%>% 
  mutate(Rango2 = as_factor(Rango2), Provincias = as_factor(Provincias)) %>% 
  ggplot(aes(Provincias, Rango2, fill = Cantidad)) +
  geom_tile() +
  geom_text(aes(label = ifelse(Cantidad > 0, Cantidad, " ")),color = ifelse(rango.c2$Cantidad < 50, "black", "white"), size = 4) +
  scale_fill_gradient(low="white", high="red") +
  labs(x="", y="Rango Etario",
       title="Casos:",
       subtitle = "Rangos Etarios vs Provincias\n
       Distribución personalizada por decenio",
       caption="''Provincias ordenadas por orden alfabético''\n
       Fuente de datos: https://covid19cubadata.github.io/#cuba\n
       Enlace a fichero de datos: https://covid19cubadata.github.io/data/covid19-casos.csv\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ipsum() + 
  theme(axis.text.x=element_text(angle=17, hjust = 1))

ggsave("figs/rangoedades2.png", width = 30, height = 20, units = "cm")

# Analizando la edad media de casos y su desviación estandar

edades <- cubadata %>% 
  group_by(edad, provincia) %>% 
  summarise(Edad = mean(edad, na.rm = T),
            Cantidad = n()) %>% 
  arrange(provincia)

edades %>% ggplot(aes(x = Cantidad, y = Edad, color = provincia)) +
  geom_jitter(alpha = 0.3, show.legend = F) +
  labs(x = "Cantidad", y = "Edad",
       title = "Dispersión de Casos:",
       subtitle = "Cantidad de casos vs Edad",
       caption = "Fuente de datos: https://covid19cubadata.github.io/#cuba\n
       Enlace a fichero de datos : https://covid19cubadata.github.io/data/covid19-casos.csv\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ipsum() +
  facet_wrap(~provincia) + coord_flip()

ggsave("figs/dispersionedades.png", width = 20, height = 20, units = "cm")

# Distribución de muertes por edades por Provicias

muertes %>% 
  mutate(provincia = reorder(provincia, edad, FUN = median)) %>% 
  ggplot(aes(x = provincia, y = edad, color = provincia)) + 
  geom_boxplot(show.legend = F) + 
  geom_jitter(aes(color = provincia, alpha = "0.01"),show.legend = F)  +
  geom_hline(yintercept=mean(muertes$edad), color = "red", linetype ="dotted") +
  geom_text(x = 0, y = mean(muertes$edad), label = "Media   =", 
            hjust = -1, vjust = -0.5, colour = "red", size = 11 * 0.8 / .pt,) +
  geom_text(x = 0.8, y = mean(muertes$edad), label = round(mean(muertes$edad), 2), 
            hjust = -1, vjust = -0.5, colour = "red", size = 11 * 0.8 / .pt,) +
  labs(x="", y="Edad",
       title="Distribución Estadística de Muertes:",
       subtitle = "Edades vs Provincias\n
       NA: provincias no informada en la publicación oficial",
       caption="''Provincias ordenadas por media de edad''\n 
       Fuente de datos: Reportes oficiales publicados en la página web del MINSAP\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19cu/blob/master/muertes.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") + 
  theme_ipsum() +
  theme(axis.text.x=element_text(angle = 17, hjust = 1), 
        panel.grid.major.x = element_blank())

ggsave("figs/muertesedades.png", width = 30, height = 20, units = "cm")

# Distribución de muertes por rangos etrios por Provincias

## Rango
rango.m <- as_tibble(table(Rango = muertes$rango, Provincias = muertes$provincia)) %>%
  rename(n, Cantidad = n) 

rango.m %>% 
  mutate(Rango = as_factor(Rango), Provincias = as_factor(Provincias)) %>% 
  ggplot(aes(Provincias, Rango, fill = Cantidad)) +
  geom_tile() +
  geom_text(aes(label = ifelse(Cantidad > 0, Cantidad, " ")), color = ifelse(rango.m$Cantidad < 5, "black", "white"), size = 4) +
  scale_fill_gradient(low="white", high="red") +
  labs(x="", y="Rango Etario",
       title="Muertes:",
       subtitle = "Rangos Etarios vs Provincias\n
       Rangos Etarios oficiales del MINSAP",
       caption="''Provincias ordenadas por orden alfabético''\n
       Fuente de datos: Reportes oficiales publicados en la página web del MINSAP\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19cu/blob/master/muertes.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ipsum() + 
  theme(axis.text.x=element_text(angle=17, hjust = 1))

ggsave("figs/rangoedadesmuertes.png", width = 30, height = 20, units = "cm")


## Rango 2
rango.m2 <- as_tibble(table(Rango = muertes$rango2, Provincias = muertes$provincia)) %>%
  rename(n, Cantidad = n) 

rango.m2 %>% 
  mutate(Rango = as_factor(Rango), Provincias = as_factor(Provincias)) %>% 
  ggplot(aes(Provincias, Rango, fill = Cantidad)) +
  geom_tile() +
  geom_text(aes(label = ifelse(Cantidad > 0, Cantidad, " ")), color = ifelse(rango.m2$Cantidad < 5, "black", "white"), size = 4) +
  scale_fill_gradient(low="white", high="red") +
  labs(x="", y="Rango Etario",
       title="Muertes:",
       subtitle = "Rangos Etarios vs Provincias\n
       Rangos Etarios personalizaddos por decenios",
       caption="''Provincias ordenadas por orden alfabético''\n
       Fuente de datos: Reportes oficiales publicados en la página web del MINSAP\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19cu/blob/master/muertes.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ipsum() + 
  theme(axis.text.x=element_text(angle=17, hjust = 1), 
        panel.grid = element_blank())

ggsave("figs/rangoedades2muertes.png", width = 30, height = 20, units = "cm")

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
       title = "Dispersión de Muertes:",
       subtitle = "Cantidad de Muertes vs Edad\n
       NA: provincias no informada en la publicación oficial",
       caption = "Fuente de datos: Reportes oficiales publicados en la página web del MINSAP\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19cu/blob/master/muertes.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ipsum() +
  facet_wrap(~provincia) + coord_flip()

ggsave("figs/dispersionedades.png", width = 20, height = 20, units = "cm")

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
       title = "Factores de Riesgo en Pacientes Fallecidos",
       subtitle = "Datos filtrado para factores con más de una aparición",
       caption = "''Factores con orden descendente de mayoner a menor incidencia''\n
       Fuente de datos: Reportes oficiales publicados en la página web del MINSAP\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19/blob/master/data/muertes.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ipsum() + 
  theme(axis.text.x=element_text(angle=0, hjust = 1),
        panel.grid.major.y = element_blank())
  
ggsave("figs/factores.riesgos.png", width = 30, height = 20, units = "cm")


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
       title = "Casos detectados con COVID-19 con respecto a la fecha de deceso:",
       subtitle = "NA: Casos que no he podido emparejar la fecha de detección con la fecha de fallecimiento al no informarse 
       la provincia y el municipio en el parte oficial o que las edades y sexos dados en el informe oficial no coincide 
       con ninguno de los casos idetificados para esa provincia y municipio",
       caption = "Fuente de datos: Reportes oficiales publicados en la página web del MINSAP\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19/blob/master/data/muertes.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ipsum() + 
  theme(axis.text.x=element_text(angle=0, hjust = 1),
        panel.grid.major.y = element_blank())

ggsave("figs/class.muertes.png", width = 30, height = 20, units = "cm")

# Representando los municipios por tasa de incidencia por cien mil habitantes

casospoblmun %>% filter(`Tasa.10^5Hab` > 15) %>% 
  mutate(municipio = reorder(municipio, `Tasa.10^5Hab`)) %>% 
  ggplot(aes(x = `Tasa.10^5Hab`, xend = 0,
             y = municipio, yend = municipio,
             color = municipio)) +
  geom_point(show.legend = F) +
  geom_segment(show.legend = F) +
  geom_text(aes(label = `Tasa.10^5Hab`), show.legend = F, hjust = -1) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Tasa por 10^5 Habitantes", y = "",
       title = "Tasa de Incidencia de Casos por Municipios",
       subtitle = "Datos de población por municipios obtenidos de la página web de la ONEI para el año 2018\n
       Datos filtrados para municipos con una tasa mayor a 15",
       caption = "Enlace a fichero de la ONEI: http://www.onei.gob.cu/sites/default/files/03series_0.rar\n
       Enlace a fichero de datos: https://github.com/fr20587/covid19/blob/master/data/poblacion.cuba.2018.onei.xlsx\n
       Gráfico realizado por: Frank Rodríguez López") +
  theme_ipsum() + 
  theme(axis.text.x=element_text(angle=0, hjust = 1),
        panel.grid.major.y = element_blank())

ggsave("figs/tasamun.png", width = 30, height = 20, units = "cm")
