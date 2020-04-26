library(tidyverse)
library(d3heatmap)
library(hrbrthemes)

load("rda/cubadata.rda")
load("rda/casos.rda")
load("rda/casosprov.rda")


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


as_tibble(table(Rango = cubadata$rango, Provincias = cubadata$provincia)) %>%
  rename(n, Cantidad = n) %>% 
  mutate(Rango = as_factor(Rango), Provincias = as_factor(Provincias)) %>% 
  ggplot(aes(Provincias, Rango, fill = Cantidad)) +
  geom_tile() +
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
as_tibble(table(Rango2 = cubadata$rango2, Provincias = cubadata$provincia)) %>%
  rename(n, Cantidad = n) %>% 
  mutate(Rango2 = as_factor(Rango2), Provincias = as_factor(Provincias)) %>% 
  ggplot(aes(Provincias, Rango2, fill = Cantidad)) +
  geom_tile() +
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
as_tibble(table(Rango = muertes$rango, Provincias = muertes$provincia)) %>%
  rename(n, Cantidad = n) %>% 
  mutate(Rango = as_factor(Rango), Provincias = as_factor(Provincias)) %>% 
  ggplot(aes(Provincias, Rango, fill = Cantidad)) +
  geom_tile() +
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
as_tibble(table(Rango = muertes$rango2, Provincias = muertes$provincia)) %>%
  rename(n, Cantidad = n) %>% 
  mutate(Rango = as_factor(Rango), Provincias = as_factor(Provincias)) %>% 
  ggplot(aes(Provincias, Rango, fill = Cantidad)) +
  geom_tile() +
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
  theme(axis.text.x=element_text(angle=17, hjust = 1))

ggsave("figs/rangoedades2muertes.png", width = 30, height = 20, units = "cm")

# Analizando la edad media de muertes y su desviación estandar

edadesmuertes <- muertes %>% 
  group_by(edad, provincia) %>% 
  summarise(Edad = mean(edad, na.rm = T),
            Cantidad = n()) %>% 
  arrange(provincia)

## Por provincias
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

#probando control github
